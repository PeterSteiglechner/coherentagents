#!/usr/bin/env python
# -*- coding: utf-8 -*-
# 
"""
author: Peter Steiglechner
last update: 18 August 2022
log:
    - 18-08-2022: updated the model to fit coherence_MB_v2_BE_2.2.nlogo and included the _PS version
"""


import numpy as np
import matplotlib.pyplot as plt
import networkx as nx 
import os
import copy
import pandas as pd
import time

logistic = lambda x, k: 1/(1+np.exp(-k*x))

class Model:
    def __init__(self, version, **kwargs) -> None:
        self.errorFlag = False
        self.version = version
        
        # ---- Set parameters ----
        self.rand_seed = kwargs["rand_seed"]           
        self.init_beliefs = "from_data"                       # how do we determine initial beliefs of the agents? currently: {"random", "data"}
        self.n_attitudes = kwargs["n_attitudes"]            # in equations: m
        self.network_type = kwargs["network_type"]      # currently: {"watts", "random"} 
        self.network_params = kwargs["network_params"]  
        self.country = kwargs["country"]                # currently "DE" "PL "CZ". 
        self.k_coherence = kwargs["k_coherence"]        # steepness of logistic curve --> importance of coherence in adaptation. 
        self.k_link = kwargs["k_link"]                  # steepness of logistic curve --> importance of link health in link dropping.        
        self.conformity_tendency = kwargs["conformity_tendency"]            # in equations: \mu
        self.variance_of_new_belief = kwargs["variance_of_new_belief"]      # in equations: \sigma
        self.prob_social_influence = kwargs["prob_social_influence"]
        self.prob_self_check = kwargs["prob_self_check"]
        self.link_health_change = kwargs["link_health_change"]              # in equations: \gamma
        self.max_prob_drop_bad_link = kwargs["max_prob_drop_bad_link"]
        self.prob_FoF = kwargs["prob_FoF"]
        self.link_lonely = bool(kwargs["link_lonely"])
        if self.version == "coherence_MB_v2_BE_2.2":        # not needed in 
            self.prob_add_link = kwargs["prob_add_link"]
            self.max_num_links = kwargs["max_num_links"]
        else:
            self.max_num_links = 1000
        #self.no_repeat_link = kwargs["no_repeat_link"]     # TODO not yet implemented ??? 

        # ---- Check if parameters are set correctly ----    
        # different parameters need to be given depending on the type of network.
        if  not ((self.network_type == "random" and ("link_prob" in self.network_params.keys())) or 
          (self.network_type == "watts"  and ("avg_node_degree" in self.network_params.keys()) and ("rewiring_prob" in self.network_params.keys())) ):
            print("ERROR: network parameters or network type"); self.errorFlag = True; return
        if not os.path.exists(self.country+"/items.csv") or not os.path.exists(self.country+"/correlations.csv"):
            print("ERROR: Wrong country, file structure, or missing files items.csv or correlations.csv"); self.errorFlag = True; return
        if self.k_link>0 or self.k_coherence<0:
            print("ERROR: one of the logistic k values is not correct"); self.errorFlag = True; return
        if not ((self.init_beliefs == "from_data") or (self.init_beliefs == "random" and "n_agents" in kwargs.keys())):
                print("ERROR: init_belief should be 'random' or 'from_data'. If init_beliefs is 'random', then user needs to specify 'n_agents'"); self.errorFlag = True; return
   
        # ---- Set seed ----
        np.random.seed(self.rand_seed)

        # ---- Get correlation matrices from correlations.csv ----
        # Load matrices for different groups from external file. The matrices are stacked on top of each other
        corr_matrix_data = pd.DataFrame(pd.read_csv(self.country+"/correlations.csv"))
        self.attitude_names = corr_matrix_data["item"][:self.n_attitudes]
        self.n_groups = len(corr_matrix_data)/self.n_attitudes
        if not int(self.n_groups) == self.n_groups:
            # n_groups is not an integer. 
            print("ERROR: in reading the matrix. Correlation matrices do not match nr_groups * nr_attitudes"); self.errorFlag = True; return
        self.matrix_list = []                           # store the correlation matrices
        for i in range(int(self.n_groups)):
            # take entries from i * n_attitudes to (i+1) * n_attitudes, e.g. for n_attitudes=5: from 0 to 4, from 5 to 9, ... for n_groups times.
            matrix = corr_matrix_data[self.attitude_names].iloc[range(i*self.n_attitudes, (i+1)*self.n_attitudes)]
            matrix["index"] = self.attitude_names
            matrix = matrix.set_index("index")
            matrix_with_zero_diag = matrix - np.diag(np.ones(self.n_attitudes))       # set diagonal to zero.
            self.matrix_list.append(matrix_with_zero_diag)
        
        # ---- Get init data from items.csv ----
        # initialise agents with beliefs, the corresponding id number, and their group
        if self.init_beliefs=="from_data":
            surveydata = pd.read_csv(self.country+"/items.csv")
            self.groups = surveydata["group"]
            self.n_agents = len(self.groups)
            init_beliefs = surveydata[self.attitude_names].to_numpy()
            ids = surveydata["idno"]
        elif self.init_beliefs == "random":
            self.n_agents = kwargs["n_agents"]
            self.groups = np.random.randint(0,self.n_groups, size=self.n_agents)
            init_beliefs = np.random.random(size=(self.n_agents, self.n_attitudes)) * 2 - 1   
            ids = np.array(range(len(init_beliefs)))
       
        # ---- Init Network ----
        # using the network_type {"watts", "random"} create a social network. depending on the type of network, different parameters need to be specified
        if self.network_type == "watts":
            self.network = nx.watts_strogatz_graph(self.n_agents, self.network_params["avg_node_degree"], self.network_params["rewiring_prob"])
            self.pos = nx.circular_layout(self.network)
        elif self.network_type == "random":
            self.network = nx.erdos_renyi_graph(self.n_agents, self.network_params["link_prob"])
            self.pos = nx.spring_layout(self.network)
        nx.set_edge_attributes(self.network, 1.0, "health")
        self.edges = list(self.network.edges(data=True))
        
        # ---- Init Agents ----
        #  Create list of all agents (with model, unique_id, group, and belief):
        self.agents = [Agent(self, i, id_nr, group-1, belief) for i, (id_nr, group, belief) in enumerate(zip(ids, self.groups, init_beliefs))]
        return 
    
    def run(self, T, times_to_track, verbose=False, save_network=False) -> int:
        """
        run the model for T steps and store the agent beliefs at all values in times_to_track
        """
        results = pd.DataFrame()                            # output data frame
        results = pd.concat([results, self.observe_opinions(t=0)])  # append a dataframe with initial opinions
        if save_network:
            results_network = pd.DataFrame()                            # output data frame
            results_network = pd.concat([results_network, self.observe_network(t=0)])  # append a dataframe with initial opinions
        else:
            results_network = None
        if verbose: print("initial state saved, start running")
        for t in range(1,T+1):
            # advance one time step
            # do n_agents update steps (with randomly selected agents). returns 1 if everything went good, 0 else
            self.single_step()
            self.edges = list(self.network.edges(data=True))
            if t in times_to_track:
                # store values for time t:
                if verbose: print("{}".format(t), end=", ")
                results = pd.concat([results, self.observe_opinions(t)])  # append a dataframe with current opinions
                if save_network:
                    results_network = pd.concat([results_network, self.observe_network(t)])
            if self.errorFlag:
                break
        if verbose:  print("...done")
        return results, results_network
       
    
    def single_step(self) -> int:
        # 1) SOCIAL INFLUENCE
        if self.edges:
            # Activate each edge/link with probability p_s (in random order), then update the link health, and correspondingly potentially drop them 
            np.random.shuffle(self.edges)
            for i,j,hdict in self.edges:            
                health = hdict["health"]
                if np.random.random() < self.prob_social_influence:
                    active_link = (i,j) if np.random.random()<0.5 else (j, i)
                    receiver, speaker = [self.agents[active_link[0]], self.agents[active_link[1]]] 
                    coherence_of_suggestion = self.be_socially_influenced(receiver, speaker)     # Process 1: Soc Infl
                    health = self.update_link_health(health, coherence_of_suggestion)
                    self.network[i][j]["health"] = health
                    if self.version == "coherence_MB_v2_BE_2.2_PS":
                        isLinkRemoved = self.potentially_remove_link(active_link, health)   # Process 3_PS: Rewiring
                        if isLinkRemoved: self.add_new_link(receiver)
                # 3) LINK DROPPING 
                if self.version == "coherence_MB_v2_BE_2.2":
                    self.potentially_remove_link((i,j), health)         # Process 3: Link Dropiing
        # 2) SELF-CHECKS and 4) LINK ADDING
        agent_order = copy.copy(self.agents)
        np.random.shuffle(agent_order)
        for ag in agent_order:
            if np.random.random() < self.prob_self_check:
                ag.self_check()                                      # Process 2: Self-check
            if self.version == "coherence_MB_v2_BE_2.2":
                if np.random.random() < self.prob_add_link:
                    self.add_new_link(ag)                       # Process 4: new link
            if self.link_lonely:
                if not ag.nbs:
                    self.add_new_link(ag)
        return 

    def potentially_remove_link(self, link, health) -> bool:
        prob_drop_link = self.max_prob_drop_bad_link * logistic(health, self.k_link)
        if np.random.random() < prob_drop_link:
            self.agents[link[0]].nbs.remove(link[1])  # remove agent i from agent j's neighbours
            self.agents[link[1]].nbs.remove(link[0])  # remove agent j from agent i's neighbours
            self.network.remove_edge(link[0], link[1])  # remove edge with the link_id
            return True
        return False  

    def add_new_link(self, ag) -> None:
        if len(ag.nbs) < self.max_num_links:
            if np.random.random() < self.prob_FoF and ag.nbs:
                potential_new_neighbours = list(np.unique(np.concatenate([self.agents[nb].nbs for nb in ag.nbs])))
            else:
                potential_new_neighbours = list(range(self.n_agents))
            for nb in ag.nbs:
                if nb in potential_new_neighbours: 
                    potential_new_neighbours.remove(nb)
            if ag.unique_id in potential_new_neighbours: 
                potential_new_neighbours.remove(ag.unique_id)
            if potential_new_neighbours:
                new_nb = potential_new_neighbours[int(np.random.random() * len(potential_new_neighbours))]
                ag.nbs.append(new_nb)
                self.agents[new_nb].nbs.append(ag.unique_id)
                self.network.add_edge(new_nb, ag.unique_id, health=1.0)
        return    

    def be_socially_influenced(self, receiver, sender) -> float:
        discussion_topic = int(np.random.random() * self.n_attitudes)      # topic/dimension to be discussed
        focal_belief = copy.copy(receiver.belief)                 # note: need to copy the n_attitude-dim vector.
        focal_belief[discussion_topic] = sender.belief[discussion_topic]
        coherence_focal = receiver.adapt_or_reject(focal_belief)           
        return coherence_focal

    def update_link_health(self, last_health, recent_coherency) -> None:
        return self.link_health_change * recent_coherency + (1 - self.link_health_change) * last_health

    def observe_network(self, t) -> pd.DataFrame:
        #self.network = nx.Graph()
        #self.network.add_nodes_from(range(self.n_agents))
        #self.network.add_weighted_edges_from([(i,j,h_ij) for (i,j), h_ij in zip (self.edges, self.link_health)], weight="link_health")
        current_network = nx.to_pandas_edgelist(self.network)
        current_network["t"] = [t for _i in current_network["source"]]
        return current_network

    def observe_opinions(self, t) -> pd.DataFrame:    
        current_state = pd.DataFrame([ag.belief for ag in self.agents], columns=self.attitude_names)
        current_state["group"] = self.groups 
        current_state["t"] = [t for _ in self.agents]     
        return current_state


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
        self.coherency = self.calc_coherency(self.belief)               # coherency of currently held belief
        self.nbs = list(self.model.network.neighbors(self.unique_id))   # (dynamic) list of neighbours
        return 


    def calc_coherency(self, belief_vec) -> float:
        """ 
        calculate the coherency of a belief_vec b given the agent's group coherency matrix CMat
        c = 0.5 * b.T * (CMat-1) * b 
        e.g. for n_attitudes=2: b = (x,y) and CMat-1 = ( (0, r); (r, 0)), then c = 0.5 * (x*r*y + y*r*x) = r*x*y
        """
        return 1/2 *np.dot(belief_vec.T, np.dot(self.model.matrix_list[self.group], belief_vec))
    
    def self_check(self) -> None:
        discussion_topic = int(np.random.random() * self.model.n_attitudes) 
        noisy_deviation = copy.copy(self.belief)
        noisy_deviation[discussion_topic] = np.random.normal(loc=self.belief[discussion_topic], scale=self.model.variance_of_new_belief)  # = prior + Gauss Noise;
        noisy_deviation[discussion_topic] = max(-1, min(noisy_deviation[discussion_topic], 1))  #  bounded between -1 and 1
        self.adapt_or_reject(noisy_deviation)
        return 

    def adapt_or_reject(self, test_belief) -> float:
        """ calculate coherence change when adopting the test_belief, then adapt (in parts) with certain (logistic) probability. """
        test_coherency = self.calc_coherency(test_belief)
        c_diff = test_coherency - self.coherency
        prob_adapt = logistic(c_diff, self.model.k_coherence)
        if np.random.random() < prob_adapt:
            # adapt
            self.belief += self.model.conformity_tendency * (test_belief - self.belief)
            self.coherency = test_coherency
        return test_coherency


# ---- MAIN FUNCTION ----

def simulation(T, track_times, params, verbose=False, save_network=False):
    s0 = time.time()
    m = Model(**params)
    if not m.errorFlag:
        if verbose: print("Setup DONE")
        results, results_network = m.run(T, track_times, verbose=verbose,save_network=save_network)
    if not m.errorFlag:
        s1 = time.time()
        minutes = int((s1-s0)/60)
        seconds = s1-s0 - 60 * minutes
        return (minutes, seconds), results, results_network
    else:
        print("An ERROR occurred!")
        return None



if __name__ == '__main__':
    # TEST RUN 
    def test_main_func():
        track_times = [0,1,2,3,4,5,6,7,8,9,10]  # [0,10,50,100,200,500,1000]
        T = 10
        params = dict(
            rand_seed=42, 
            network_type = "watts",
            network_params={"rewiring_prob":0.1, "avg_node_degree":10}, 
            country="DE", 
            n_attitudes = 5,
            #n_agents=2203, 
            k_coherence=10,
            k_link=-100, 
            conformity_tendency = 0.3,
            variance_of_new_belief = 0.0,
            prob_social_influence = 1.0 ,
            prob_self_check = 0.0,
            link_health_change = 0.0,
            max_prob_drop_bad_link = 0.0,
            prob_FoF = 0.0,
            version = "coherence_MB_v2_BE_2.2",
            prob_add_link = 0.0,
            link_lonely = False,
            max_num_links = 100,
            init_beliefs="from_data",
        )
        elapsed_time, results, results_network = simulation(T, track_times, params, verbose=True, save_network=True)
        print(results)
        print(results_network)
        print("elapsed time: {:} min and {:.1f} seconds".format(elapsed_time[0], elapsed_time[1])) 
    
    
    test_main_func()
