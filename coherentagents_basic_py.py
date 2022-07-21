import numpy as np
import matplotlib.pyplot as plt
import networkx as nx 
import sys
import copy
import pandas as pd
import time

class Model:
    def __init__(self, **kwargs) -> None:
        # ---- Set parameters ----
        self.seed = kwargs["seed"]                      # random seed
        np.random.seed(self.seed)
        self.country = kwargs["country"]                # country / folder
        self.n_agents = kwargs["n_agents"]              # nr of agents
        self.k = kwargs["k"]                            # steepness of logistic curve --> importance of coherence in adaptation.
        self.gamma = kwargs["gamma"]                    # prob to break link if reject
        self.beta = kwargs["beta"]                      # prob to reflect (self-coherence)
        self.alpha = kwargs["alpha"]                    # prob to be socially influenced
        self.kappa = kwargs["kappa"]                    # prob to establish a new link
        self.th_cut_link = kwargs["th_cut_link"]        # threshold of distance between message and corresponding belief to cut link if message rejected
        self.noise = kwargs["noise"]                    # noise in reflection (craziness-of-new-belief)
        self.learning_rate = kwargs["learning_rate"]    # rate at which social influence is adopted. Conformity tendency 
        self.n_attitudes = kwargs["n_attitudes"]        # number of belief items
        self.network_params = kwargs["network_params"]  # type of network. currently: {"watts", "random"} 
        #                                               # depending on the type of network, different parameters need to be chosen
        self.init_beliefs = kwargs["init_beliefs"]              # how do we determine initial beliefs of the agents? currently: {"random", "data"}
        #                                               # the "data" option requires n_agents=2203
        self.save_network = kwargs["save_network"]      # save the network data together with the beliefs?

        # ---- Get correlation matrices from .csv ----
        # Load matrices for different groups which are stacked on top of each other
        corr_matrix_data = pd.DataFrame(pd.read_csv(self.country+"/correlations.csv"))
        self.attitudenames = corr_matrix_data["item"][:self.n_attitudes]
        self.n_groups = len(corr_matrix_data)/self.n_attitudes
        if not self.n_groups%1== 0:
            # n_groups is not an integer. 
            print("ERROR: in matrix_read")
            return 0
        self.matrix_list = []                           # store the correlation matrices
        for i in range(int(self.n_groups)):
            # take entries from i*n_attitueds to (i+1) * n_attitudes, e.g. for n_attitudes=5: from 0 to 4, from 5 to 9, ... for n_groups times.
            m = corr_matrix_data[self.attitudenames].iloc[range(i*self.n_attitudes, (i+1)*self.n_attitudes)]
            m["index"] = self.attitudenames
            m = m.set_index("index")
            m_nodiag = m - np.diag(np.ones(self.n_attitudes))       # set diagonal to zero.
            self.matrix_list.append(m_nodiag)

        # ---- Init Network ----
        # using the network_type {"watts", "random"} create a social network. depending on the type of network, different parameters need to be specified
        if self.network_params["type"]=="watts":
            self.network = nx.watts_strogatz_graph(self.n_agents, self.network_params["k"], self.network_params["p"])
            self.pos = nx.circular_layout(self.network)
        elif self.network_type == "random":
            self.network = nx.erdos_renyi_graph(self.n_agents, self.network_params["p"])
            self.pos = nx.spring_layout(self.network)
        else:
            print("ERROR: specify correct network_type. ...Aborting.")
            return 
        self.edges = list(self.network.edges)
        self.n_edges = len(self.edges)
        
        # ---- Init Agents ----
        # initialise agents with beliefs, the corresponding id number, and their group
        if self.init_beliefs=="data" and not self.n_agents==2203:
            print("WARNING: nr of agents is not 2203 but you try to initialise the agent beliefs to the data. The initial beliefs are therefore drawn randomly")
            self.init_beliefs = "random"
        if self.init_beliefs == "data":
            surveydata = pd.read_csv(self.country+"/items.csv")
            self.groups = surveydata["group"]
            init_beliefs = surveydata[self.attitudenames].to_numpy()
            ids = surveydata["idno"]
        elif self.init_beliefs == "random":
            self.groups = np.random.randint(0,self.n_groups, size=self.n_agents)
            init_beliefs = np.random.random(size=(self.n_agents, self.n_attitudes))*2-1  # could also initialise as pd.DataFrame( *, columns=self.attitudenames)
            ids = np.array(range(len(init_beliefs)))
        # Create list of all agents (with model, unique_id, group, and belief):
        self.agents = [Agent(self, i, idno, group-1, belief) for i, (idno, group, belief) in enumerate(zip(ids, self.groups, init_beliefs))]

        self.times_to_track = None
        self.running = 1
        # TEST: to check how often each process took place:
        #    self.counts=dict(reflection=0, influence=0, linkbreak=0, linknew=0, samebelief_influence=0)
        return 

    def run(self, T, times_to_track, verbose=False) -> int:
        """
        run the model for T steps and store the agent beliefs at all values in times_to_track
        """
        self.times_to_track = times_to_track
        d = pd.DataFrame()                            # output data frame
        d = pd.concat([d, self.observe_opinions(t=0)])  # append a dataframe with initial opinions
        for t in range(1,T+1):
            # advance one time step
            for _ in range(self.n_agents):
                # do n_agents update steps (with randomly selected agents)
                self.running = self.single_step()
            if t in self.times_to_track:
                # store values:
                if verbose: print("{}".format(t), end=", ")
                d = pd.concat([d, self.observe_opinions(t)])  # append a dataframe with current opinions
                if self.save_network:
                    self.observe_network(t)
        if verbose: print("...done")
        return self.running, d

    def vis_network(self) -> None:
        """
        visualise the network 
        """
        fig = plt.figure()
        ax = fig.add_subplot(111)
        self.network = nx.Graph()
        self.network.add_nodes_from(range(self.n_agents))
        self.network.add_edges_from(self.edges)
        self.pos = nx.spring_layout(self.network)
        cmap = copy.copy(plt.get_cmap("Set1"))
        colors = [cmap(g) for g in self.groups]
        nx.draw(self.network, self.pos, ax=ax, node_color=colors)
        self

    def observe_network(self, t):
        self.network = nx.Graph()
        self.network.add_nodes_from(range(self.n_agents))
        self.network.add_edges_from(self.edges)
        current_network = nx.to_pandas_edgelist(self.network)
        current_network["t"] = [t for _ in current_network["source"]]
        return current_network

    def observe_opinions(self, t) -> pd.DataFrame:    
        """
        get current beliefs of all agents and return them in a dataframe with the current time t. 
        """  
        current_state = pd.DataFrame([ag.belief for ag in self.agents], columns=self.attitudenames)
        current_state["group"] = self.groups 
        current_state["t"] = [t for _ in self.agents]     

        return current_state

    def single_step(self) -> int:
        """
        do a single step for one agent.
        """
        # (1+3) Social Influence + Link Cutting
        if self.n_edges == 0:
            # network has no more edges --> no social influence/link-cutting
            if self.running==1:
                print("WARNING: network has no links")
                self.running=0
            # select a random "receiver" who might do a reflection (see 2)
            receiver = np.random.choice(len(self.n_agents))
        else:
            # select link with two nodes to be receiver and sender
            # Note: networkx is extremely slow in sampling the links as long as links keep changing. 
            # The following two lines are thus replaced by a faster list approach
            #   link_id = np.random.choice(self.n_edges)
            #   interacting_nodes = list(self.network.edges)[link_id]
            link_id = np.random.choice(self.n_edges)
            link = self.edges[link_id]
            # randomly choose sender and receiver from the chosen link ends
            receiver_id, sender_id = (link[0], link[1]) if np.random.random()<0.5 else (link[1], link[0]) 
            sender = self.agents[sender_id]
            receiver = self.agents[receiver_id]
        
            # (1) Social influence
            if np.random.random() < self.alpha:
                focal_belief = np.random.choice(self.n_attitudes)   # topic/dimension to be discussed
                testbelief = copy.copy(receiver.belief)                 # note: need to copy the n_attidude-dim vector.
                testbelief[focal_belief] = sender.belief[focal_belief]
                # TEST: count how often agents are socially influenced by the exact same opinion they already hold.
                #    if (testbelief==receiver.belief).all():
                #      self.counts["samebelief_influence"]+=1
                #    else:
                success = receiver.adapt_or_reject(testbelief, rate=self.learning_rate)
                # (3) potentially break link
                if (not success) and (np.random.random() < self.gamma) and (abs(testbelief[focal_belief] - receiver.belief[focal_belief]) > self.th_cut_link):
                    #self.network.remove_edge(interacting_nodes[0], interacting_nodes[1])
                    receiver.nbs.remove(sender_id)  # remove sender from receiver's neighbours
                    sender.nbs.remove(receiver_id)  # remove receiver from sender's neighbours
                    # TEST self.counts["linkbreak"] += 1
                    del self.edges[link_id]  # remove edge with the link_id
                    self.n_edges -= 1
                # TEST: see above
                # else:
                #    self.counts["influence"]+=1

        # (2) Reflection 
        if np.random.random() < self.beta:
            focal_belief = np.random.choice(self.n_attitudes)
            testbelief = copy.copy(receiver.belief) 
            testbelief[focal_belief] = max(-1, min(np.random.normal(loc=testbelief[focal_belief], scale=self.noise), 1))  # = prior + Gauss Noise; bounded between -1 and 1
            success = receiver.adapt_or_reject(testbelief, rate=1)
            # TEST 
            # if success:
            #    self.counts["reflection"]+=1

        # (4) Add new random link
        if (np.random.random() < self.kappa):
            # an agent considers only potential links to agents that are not already neighbour or to itself.
            potential_new_neighbours = [k for k in range(self.n_agents) if k not in receiver.nbs or k==receiver_id]
            new_neighbour = np.random.choice(potential_new_neighbours)
            # OLD: self.network.add_edge(receiver_id, new_neighbour)
            receiver.nbs.append(new_neighbour)  # add new neighbour to the neighbour list
            self.agents[new_neighbour].nbs.append(receiver_id)  # add receiver to the new_neighbour's neighbour list
            self.edges.append((receiver_id, new_neighbour))
            self.n_edges += 1
            # TEST see above:
            #    self.counts["linknew"]+=1  
            
        return self.running




class Agent:
    def __init__(self, model, unique_id, data_idno, group, belief) -> None:
        """
        An agent is a single person (1) embedded in a model, (2) with a unique_id, 
        (3) with an idno in the survey data (if applicable), (4) with a group index, 
        (5) with a belief from which we calculate (6) its coherence, and 
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
        c = 0.5 * b.T * CMat * b 
        e.g. for 2D: b = (x,y) and CMat = ( (0, r); (r, 0)), then c = 0.5 * (x*r*y + y*r*x) = r*x*y
        """
        c = np.dot(belief_vec.T, np.dot(self.model.matrix_list[self.group], belief_vec)) / 2
        return c       
    
    def adapt_or_reject(self, testbelief, rate=1) -> int:
        """ calculate coherence change when adopting the testbelief, then adapt (in parts determined by the rate) with certain (logistic) probability. """
        test_coherency = self.calc_coherency(testbelief)
        c_diff = test_coherency - self.coherency
        prob_adapt = 1 / ( 1 + np.exp( - self.model.k * c_diff))
        if np.random.random() < prob_adapt:
            # adapt with certain rate (1 for reflection and model.learning_rate for social influence)
            self.belief += rate * (testbelief-self.belief)
            self.coherency = test_coherency
            return 1
        else:
            return 0


            

def simulation(T, track_times, params, verbose=False, save_network=False):
    s0 = time.time()
    m = Model(**params, save_network=save_network)
    run_finished, results = m.run(T, track_times, verbose=verbose)
    if run_finished:
        s1 = time.time()
        minutes = int((s1-s0)/60)
        seconds = s1-s0 - 60 * minutes
        return (minutes, seconds), results
    else:
        print("An ERROR occured!")
        return None

def testmain():
    track_times = [0,1,2,3,4,5,6,7,8,9,10]  # [0,10,50,100,200,500,1000]
    T = 10
    params = dict(seed=42, 
        country="DE", 
        n_agents=2203, 
        k=10, 
        gamma=0.1, 
        beta=0.5, 
        alpha=0.5, 
        kappa=0.1, 
        th_cut_link=0.2, 
        n_attitudes=5, 
        noise = 0.1, 
        learning_rate=0.5,
        network_params={"type":"watts", "p":0.1, "k":10}, 
        init_beliefs="data" 
    )
    ellapsed_time, results = simulation(T, track_times, params)
    print(results)
    print("ellapsed time: {:} min and {:.1f} seconds".format(ellapsed_time[0], ellapsed_time[1])) 



if __name__ == '__main__':
    testmain()
