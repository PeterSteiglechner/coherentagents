#!/usr/bin/env python
# -*- coding: utf-8 -*-
# 
"""
author: Peter Steiglechner
email: peter.steiglechner@gmail.com
last update: 11-10-2023
log:
   - cleaned version 
"""


import random
import numpy as np
import networkx as nx 
import os
import copy
import pandas as pd
import time
import logging
logging.basicConfig(level=logging.WARNING, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger()

logistic = lambda x, k: 1/(1+np.exp(-k*x))

manhattan = lambda a, b: sum(abs(val1-val2) for val1, val2 in zip(a,b))/len(a)

class Model:
    def __init__(self, **kwargs) -> None:
        
        # ---- Set parameters ----
        self.rand_seed = kwargs["rand_seed"]           
        self.init_beliefs = "from_data"                       # how do we determine initial beliefs of the agents? currently: {"random", "data"}
        self.n_attitudes = kwargs["n_attitudes"]            # in equations: m
        self.network_type = kwargs["network_type"]      # currently: {"watts", "random"} 
        self.network_params = kwargs["network_params"]  
        self.country_data = kwargs["country_data"]                # currently "DE" "PL "CZ". 
        self.grouping = kwargs["grouping"]  
        assert (self.grouping=="CCA7" or self.grouping=="OneGroup" or self.grouping=="random" or self.grouping=="CCA3")
        self.k_coherence = kwargs["k_coherence"]        # steepness of logistic curve --> importance of coherence in adaptation. 
        self.conformity_tendency = kwargs["conformity_tendency"]            # in equations: \mu
        self.variance_of_new_belief = kwargs["variance_of_new_belief"]      # in equations: \sigma
        self.prob_social_influence = kwargs["prob_social_influence"]
        self.prob_self_reflection = kwargs["prob_self_reflection"]
       
        # ---- Check if parameters are set correctly ----    
        # different parameters need to be given depending on the type of network.      
        if self.network_type == "random": 
            assert "link_prob" in self.network_params.keys() 
        if self.network_type == "watts":  
            assert ("avg_node_degree" in self.network_params.keys() and "rewiring_prob" in self.network_params.keys())
        
        # different agent groupings and cognitive models are stored in different files
        if self.grouping == "OneGroup":
            fnameItems = "items.csv"
            fnameCorrelations = "correlations.csv"
        elif self.grouping == "CCA7":
            fnameItems = "itemsCCA.csv"
            fnameCorrelations = "correlationsCCA.csv"
        elif self.grouping == "random":
            fnameItems = "items7groups.csv"
            fnameCorrelations = "correlations7groups.csv"
        elif self.grouping == "CCA3":
            fnameItems = "itemsCCA3.csv"
            fnameCorrelations = "correlationsCCA3.csv"
                
        assert os.path.exists(self.country_data +"/"+fnameItems)
        assert os.path.exists(self.country_data +"/"+fnameCorrelations)
        
        assert self.k_coherence>0
   
        # ---- Set seed ----
        np.random.seed(self.rand_seed)
        random.seed(self.rand_seed)

        # ---- Get correlation matrices from {fnameCorrelations}.csv ----
        # Load matrices for different groups from external file. The matrices are stacked on top of each other
        cogn_model_data = pd.DataFrame(pd.read_csv(self.country_data +"/"+fnameCorrelations))
        self.attitude_names = cogn_model_data["item"][:self.n_attitudes]
        assert int(len(cogn_model_data)/self.n_attitudes) == len(cogn_model_data)/self.n_attitudes
        self.n_groups = int(len(cogn_model_data)/self.n_attitudes)
        
        # store the correlation matrices
        self.cogn_model_dict = {}     
        groups = cogn_model_data["group"][::self.n_attitudes]                    
        for i, g in zip(groups.index, groups):
            # take entries from i * n_attitudes to (i+1) * n_attitudes, e.g. for n_attitudes=5: from 0 to 4, from 5 to 9, ... for n_groups times.
            matrix = cogn_model_data[self.attitude_names].iloc[range(i, i+self.n_attitudes)]
            matrix_with_zero_diag = matrix - np.diag(np.ones(self.n_attitudes))       # set diagonal to zero.
            matrix_with_zero_diag["index"] = self.attitude_names
            matrix_with_zero_diag = matrix_with_zero_diag.set_index("index")
            self.cogn_model_dict[g] = matrix_with_zero_diag
        
        # ---- Get init data from items.csv ----
        # initialise agents with beliefs, the corresponding id number, and their group
        if self.init_beliefs=="from_data":
            surveydata = pd.read_csv(self.country_data +"/"+fnameItems)
            self.groups = surveydata["group"]
            self.n_agents = len(self.groups)
            init_beliefs = surveydata[self.attitude_names].to_numpy()
            if "idno" in surveydata.keys():
                ids = surveydata["idno"]
            else:
                ids = np.arange(self.n_agents)
            if self.grouping=="CCA3":
                # only for CCA3 shuffle up all agents, because they are ordered by their group.
                np.random.shuffle(ids)
                self.groups = self.groups[ids]
                init_beliefs = init_beliefs[ids, :]
                ids = np.arange(self.n_agents)
                
        elif self.init_beliefs == "random":
            assert "n_agents" in kwargs.keys()
            self.n_agents = kwargs["n_agents"]
            self.groups = np.random.randint(1,self.n_groups+1, size=self.n_agents)
            init_beliefs = np.random.random(size=(self.n_agents, self.n_attitudes)) * 2 - 1   
            ids = np.array(range(len(init_beliefs)))

       
        # ---- Init Network ----
        # using the network_type {"watts", "random"} create a social network. depending on the type of network, different parameters need to be specified
        if self.network_type == "watts":
            self.network = nx.watts_strogatz_graph(self.n_agents, self.network_params["avg_node_degree"], self.network_params["rewiring_prob"])
            self.pos = nx.circular_layout(self.network)
        elif self.network_type == "random":
            self.network = nx.erdos_renyi_graph(self.n_agents, self.network_params["link_prob"])
            self.pos = nx.spring_layout(self.network, seed=self.rand_seed)
        nx.set_edge_attributes(self.network, 1.0, "health")
        self.edges = list(self.network.edges(data=True))
        
        # ---- Init Agents ----
        #  Create list of all agents (with model, unique_id, group, and belief):
        self.agents = [Agent(self, i, id_nr, group, belief) for i, (id_nr, group, belief) in enumerate(zip(ids, self.groups, init_beliefs))]
        self.agent_ids = np.array(list(range(self.n_agents))).astype(int)
        return 
    
    def run(self, T, times_to_track, save_network=False, save_agentstates=False) -> int:
        """
        run the model for T steps and store the agent beliefs at all values in times_to_track
        """

        ##### arrays for storing the data
        resultsM = pd.DataFrame()  
        resultsMG = pd.DataFrame()  
        resAll, resG = self.observe_macro(t=0)
        resultsM = pd.concat([resultsM, resAll])  
        resultsMG = pd.concat([resultsMG, resG]) 
        if save_agentstates:
            results = pd.DataFrame()                            # output data frame
            results = pd.concat([results, self.observe_opinions(t=0)])  # append a dataframe with initial opinions
        else:
            results = None
        if save_network:
            results_network = pd.DataFrame()                            # output data frame
            results_network = pd.concat([results_network, self.observe_network(t=0)])  # append a dataframe with initial opinions
        else:
            results_network = None
        
        ##### Simulation
        logger.info("initial state saved, start running...")
        for t in range(1,T+1):
            # advance one time step
            # do n_agents update steps (with randomly selected agents). returns 1 if everything went good, 0 else

            self.single_step()

            if t in times_to_track:
                # store values for time t:
                logger.info("t={}".format(t))
                if t in times_to_track:
                    resAll, resG = self.observe_macro(t=t)
                    resultsM = pd.concat([resultsM, resAll]) 
                    resultsMG = pd.concat([resultsMG, resG]) 
                if save_agentstates:
                    results = pd.concat([results, self.observe_opinions(t)])  # append a dataframe with current opinions
        logger.info("simulation run ...done")
        
        return results, results_network, resultsM, resultsMG 
       
    
    def single_step(self) -> int:
        # 1) social influence
        # Activate each edge/link with probability p_s (in random order) 
        np.random.shuffle(self.edges)
        for i,j,attr in self.edges:            
            if np.random.random() < self.prob_social_influence:
                active_link = (i,j) if np.random.random()<0.5 else (j, i)
                receiver, speaker = [self.agents[active_link[0]], self.agents[active_link[1]]] 
                coherence_of_suggestion = self.social_influence(receiver, speaker)     # Process 1: Soc Influence
        # 2) self-reflection
        np.random.shuffle(self.agent_ids)
        for ag_id in self.agent_ids:
            ag = self.agents[ag_id]
            if np.random.random() < self.prob_self_reflection:
                ag.self_reflection()                                      # Process 2: Self-check
        return 

    def social_influence(self, receiver, sender) -> float:
        discussion_topic = int(np.random.random() * self.n_attitudes)      # topic/dimension to be discussed
        # the focal belief is the receiver belief except for the discussed topic for which it has the value of the speaker belief
        focal_belief = copy.copy(receiver.belief)                 # note: need to copy the n_attitude-dim vector.
        focal_belief[discussion_topic] = sender.belief[discussion_topic]
        coherence_focal = receiver.adapt_or_reject(focal_belief)           
        return coherence_focal


    # ----------------------------------------------
    # ---------      UTIL FUNCTIONS    -------------
    # ----------------------------------------------

    def observe_network(self, t) -> pd.DataFrame:
        current_network = nx.to_pandas_edgelist(self.network)
        current_network["t"] = [t for _i in current_network["source"]]
        return current_network

    def observe_opinions(self, t) -> pd.DataFrame:    
        current_state = pd.DataFrame([ag.belief for ag in self.agents], columns=self.attitude_names)
        current_state["group"] = [ag.group for ag in self.agents]
        current_state["t"] = [t for _ in self.agents] 
        current_state["coherence"] = [ag.coherence for ag in self.agents]    
        return current_state

    def observe_macro(self, t) -> pd.DataFrame:
        opDictAll = dict(zip([f"mean_{att}" for att in self.attitude_names], self.calc_mean_opinion(g=None)))
        opDictAll.update(dict(zip([f"std_{att}" for att in self.attitude_names], self.calc_std_opinion(g=None))))
        vars = {
            "extremeness":self.calc_extremeness(g=None), 
            "diversity": self.calc_diversity(g=None), 
            "t":[t], 
            "stdDev_coherence": self.calc_stdDevcoherence(g=None),
            "mean_coherence": self.calc_meancoherence(g=None),
            }
        vars.update(opDictAll)
        resAll = pd.DataFrame(vars)
        groups = np.unique(self.groups)
        opDictGroup =   dict(zip([f"meanGroup_{att}" for att in self.attitude_names], np.array([self.calc_mean_opinion(g=g) for g in groups]).T))
        opDictGroup.update(
                        dict(zip([f"stdGroup_{att}"  for att in self.attitude_names], np.array([self.calc_std_opinion(g=g)  for g in groups]).T))
                        )
        vars = {
            "extremenessGroup":[self.calc_extremeness(g=g) for g in groups], 
            "diversityGroup": [self.calc_diversity(g=g) for g in groups], 
            "t":[t for g in groups], 
            "mean_coherenceGroup": [self.calc_meancoherence(g=g) for g in groups],
            "stdDev_coherenceGroup": [self.calc_stdDevcoherence(g=g) for g in groups],
        }
        vars.update(opDictGroup)
        resGroups = pd.DataFrame(vars)
        resGroups["group"] = groups
        return pd.DataFrame(resAll), pd.DataFrame(resGroups)

    def calc_mean_opinion(self, g=None):
        if g is None:
            ops = np.array([ag.belief for ag in self.agents])
        else:
            ops = np.array([ag.belief for ag in self.agents if ag.group == g])
        return np.mean(ops, axis=0)
    
    def calc_std_opinion(self, g=None):
        if g is None:
            ops = np.array([ag.belief for ag in self.agents])
        else:
            ops = np.array([ag.belief for ag in self.agents if ag.group == g])
        return np.std(ops, axis=0)
    
    def calc_meancoherence(self, g=None):
        if g is None:
            selectedCohs = np.array([ag.coherence for ag in self.agents])
        else:
            selectedCohs = np.array([ag.coherence  for ag in self.agents if ag.group == g])
        return np.mean(selectedCohs)   
    
    def calc_stdDevcoherence(self, g=None):
        if g is None:
            selectedCohs = np.array([ag.coherence for ag in self.agents])
        else:
            selectedCohs = np.array([ag.coherence  for ag in self.agents if ag.group == g])
        return np.std(selectedCohs)   
    

    def calc_extremeness(self, g=None):
        if g is None:
            selectedBeliefs = np.array([ag.belief for ag in self.agents])
        else:
            selectedBeliefs = np.array([ag.belief  for ag in self.agents if ag.group == g])
        return np.mean([manhattan(agentBelief, np.zeros(self.n_attitudes)) for agentBelief in selectedBeliefs])   
    

    def calc_diversity(self, g=None):
        if g is None:
            selectedBeliefs = np.array([ag.belief for ag in self.agents])
        else:
            selectedBeliefs = np.array([ag.belief for ag in self.agents if ag.group == g])
        sigs_per_attitude = np.std(selectedBeliefs, axis=0)
        assert( len(sigs_per_attitude) == self.n_attitudes)
        return np.mean(sigs_per_attitude)
    



class Agent:
    def __init__(self, model, unique_id, data_idno, group, belief) -> None:
        """
        An agent is a single person 
        (1) embedded in a model, 
        (2) with a unique_id, 
        (3) with an idno in the survey data (if applicable), 
        (4) with a group index, 
        (5) with a belief from which we calculate 
        (6) its coherence,  
        (7) a list of neighbours in the social network.
        """
        self.model = model                                              # model in which the agent is embedded
        self.unique_id = unique_id                                      # node_id 
        self.data_idno = data_idno                                      # corresponding id in the survey data
        self.group = group                                              # group (or cognitive model)
        self.belief = belief                                            # belief
        self.coherence = self.calc_coherence(self.belief)               # coherence of currently held belief
        self.nbs = list(self.model.network.neighbors(self.unique_id))   # (dynamic) list of neighbours
        return 


    def calc_coherence(self, belief_vec) -> float:
        """ 
        calculate the coherence of a belief_vec b given the agent's group coherence matrix CMat
        c = 0.5 * b.T * (CMat-1) * b 
        e.g. for n_attitudes=2: b = (x,y) and CMat-1 = ( (0, r); (r, 0)), then c = 0.5 * (x*r*y + y*r*x) = r*x*y
        """
        return 1/2 * np.dot(belief_vec.T, np.dot(self.model.cogn_model_dict[self.group], belief_vec))
    
    def self_reflection(self) -> None:
        discussion_topic = int(np.random.random() * self.model.n_attitudes) 
        noisy_deviation = copy.copy(self.belief)
        noisy_deviation[discussion_topic] = np.random.normal(loc=self.belief[discussion_topic], scale=self.model.variance_of_new_belief)  # = prior + Gauss Noise;
        noisy_deviation[discussion_topic] = max(-1, min(noisy_deviation[discussion_topic], 1))  #  bounded between -1 and 1
        self.adapt_or_reject(noisy_deviation)
        return 

    def adapt_or_reject(self, test_belief) -> float:
        """ calculate coherence change when adopting the test_belief, then adapt (in parts) with certain (logistic) probability. """
        test_coherence = self.calc_coherence(test_belief)
        c_diff = test_coherence - self.coherence
        prob_adapt = logistic(c_diff, self.model.k_coherence)
        if np.random.random() < prob_adapt:
            # adapt belief
            self.belief += self.model.conformity_tendency * (test_belief - self.belief)
            self.coherence = test_coherence
        return test_coherence





# ---- MAIN FUNCTION ----

def simulation(T, track_times, params, save_network=False, save_agentstates=False):
    s0 = time.time()
    m = Model(**params)
    logger.info("Setup DONE")
    results, results_network,  macroAll, macroGroups = m.run(T, track_times, save_network=save_network, save_agentstates=save_agentstates)
    s1 = time.time()
    minutes = int((s1-s0)/60)
    seconds = s1-s0 - 60 * minutes
    assert (results is None) or (type(results)==pd.DataFrame)
    return (minutes, seconds), results, results_network, macroAll, macroGroups
    


if __name__ == '__main__':
    logger.setLevel(logging.DEBUG)
    # TEST RUN 
    def test_main_func():
        track_times = np.arange(0,201, step=10) 
        T = 10
        params = dict(
            rand_seed=42, 
            network_type = "watts",
            network_params={"rewiring_prob":0.1, "avg_node_degree":10}, 
            country_data ="DE", 
            n_attitudes = 5,
            k_coherence=10,
            conformity_tendency = 0.2,
            variance_of_new_belief = 0.1,
            prob_social_influence = 0.5,
            prob_self_reflection = 0.5,
            init_beliefs="from_data",
            grouping="CCA3"
        )
        elapsed_time, results, results_network, macroAll, macroGroup= simulation(T, track_times, params, save_network=True, save_agentstates=True)
        logging.info(results)
        logging.info(results_network)
        logging.info(macroAll)
        logging.info(macroGroup)
        logging.info("elapsed time: {:} min and {:.1f} seconds".format(elapsed_time[0], elapsed_time[1])) 
    
    
    test_main_func()
