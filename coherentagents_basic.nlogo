;;;;; Model using coherence agents -- written from scratch

;; Updated: 2022-07-22 FranCesko

;; Brief idea:
;; -----------
;; We try to costruct model where agents offer other agents values of shared believes,
;; then they evaluate according the coherence matrix of their group the offered values and
;; then they receive them or refuse, according the higher consistency.
;; Agents communicate over the communication network which is stable for the first version,
;; but might be developing/changing in the future versions.
;;

;; Agents offer believes each other, then they compare the consistency of prior set of believes with the consistency of possibly updated set of believes, and proportionaly to the difference they accept new belief or not.

;; Agents operate on communication network. It means that they offers their believes and receive the believes only from nodes of the network they have an edge with. For now there is no update of the network.


;; HEAD STUFF
extensions [nw matrix csv table]

turtles-own [idno belief_vector group ]

globals [coherency_matrices]



;; SETUP
to setup
;- Clear everything: DONE!
  ca
  file-close  ; Just for sure that no files are open from the previous run of model
  file-close
  file-close

;- Setting random seed
  if set_seed [random-seed RS]

;- Initialize globals: DONE!
  initialize-globals

;- Initialize communication network: DONE!
  initialize-comm-network

;- Set agents variables: DONE!
  set-agents

;- Set links variables: NOT NEEDED NOW!
  set-links

;- Reset ticks
  reset-ticks

;- Visualization of agents
  visualize

;- Setting seed again: in case of different random numbers consumation
  if set_seed [random-seed RS]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   S E T U P   P R O C E D U R E S   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to initialize-globals
  ;; First of all, we have to set correct N according the length of 'items.csv',
  ;; BUT only in the case we use our OWN data for agents, ie., agents beliefs are not random.
  ;; If agents are initialized in random fashion, then we let N as it is.
  if set_agents = "OWN" [set N file-length (agents_name)]

  ;; Now, there is only one global to be read from file: coherency_matrices, let's initialize it as table!
  set coherency_matrices table:make

  ;; Checking existency of .csv file with coherency matrices and in case of existence, reading matrices from file.
  ifelse file-exists? coherence_name [
    file-close
    file-open coherence_name
    let consume-vars-names-out csv:from-row file-read-line
    print "TEST!!!!"
    print consume-vars-names-out
    let i 1  ;; index of group/coherence matrix
    while [not file-at-end?] [
      let j 1  ;; index of line inside the matrix we create now
      let l []
      while [j <= 5][  ;; now we know that we use 5 values, that our matrix is 5x5
        let fl (butlast (butlast (csv:from-row file-read-line)))   ;; also hardcoded: We know that we do not use for consistence matrix the first one and last two values
        let flp []

        foreach (fl) [[nx] -> set flp lput (precision (nx) 3) flp ]  ;; just rounding to 3 decimal places, to make matrices better readable
        set l lput flp l
        set j j + 1
      ]
      let m matrix:from-row-list l
      table:put coherency_matrices i m
      set i i + 1
    ]
    file-close
  ][print (word "FILE NOT FOUND! You have to put alongside the model file '" coherence_name "' describing your coherence matrices") ]

    ;; Checking how table with coherency matrices look like.
    print coherency_matrices
end

to-report file-length [file-name]
  ;; Opening the file for counting lines
  file-open file-name

  ;; Initializing counter.
  ;; Note! We initialize it as -1, because we want to know the number of agents,
  ;; not number of lines, so we do not count:
  ;;   a) the first line with variable names and
  ;;   b) the last line with just end of the file mark,
  ;; that's why we must start with -1, to omit 2 rows from counting
  ;; (the first with var names and the last almost empty just with file-end mark)
  let l -1

  ;; Main WHILE cycle going through the file and counting lines
  while [not file-at-end?][
    let just-throw-it-away file-read-line
    set l (l + 1)
  ]

  ;; closing file
  file-close

  ;; Reportin the length of file
  report l
end


to initialize-comm-network
  ;; Which kind of network we are for?
  ;; Let's start with Small-World network, but other might come later, let's also prepare for it!
  (ifelse
    network_type = "Watts" [
      resize-world (0 - round(sqrt(N))) round(sqrt(N)) (0 - round(sqrt(N))) round(sqrt(N))
      nw:generate-watts-strogatz turtles links N neis rewiring [ fd (round(sqrt(N)) - 1) ]
    ]
    network_type = "Kleinberg" [
      resize-world 0 (round(sqrt(N)) - 1) 0 (round(sqrt(N)) - 1)
      nw:generate-small-world turtles links round(sqrt(N)) round(sqrt(N)) 2.0 toroidial_Kleinberg?
      (foreach (sort turtles) (sort patches) [ [t p] -> ask t [ move-to p ] ])
    ]
    network_type = "Barabasi" [
      resize-world (0 - round(sqrt(N))) round(sqrt(N)) (0 - round(sqrt(N))) round(sqrt(N))
      nw:generate-preferential-attachment turtles links N min_degree [ fd (round(sqrt(N)) - 1) ]
    ]
    network_type = "random" [
      resize-world (0 - round(sqrt(N))) round(sqrt(N)) (0 - round(sqrt(N))) round(sqrt(N))
      nw:generate-random turtles links N rewiring [ fd (round(sqrt(N)) - 1) ]
    ]
    network_type = "Bruce" [
      crt N [setxy random-xcor random-ycor]
      ask turtles [
        repeat min_degree [
          create-link-with min-one-of (other turtles with [not link-neighbor? myself]) [distance myself]
         ]
       ]
    ]
    ; elsecommands: network_type = "OWN"
    [
      ifelse file-exists? network_name and file-exists? agents_name [
        file-open agents_name
        set N file-read
        file-close
        resize-world (0 - round(sqrt(N))) round(sqrt(N)) (0 - round(sqrt(N))) round(sqrt(N))
        nw:load-matrix network_name turtles links [ fd (round(sqrt(N)) - 1) ]
      ][print (word "FILE NOT FOUND! You have to put alongside the model these files '" network_name "' and '" agents_name "' describing your network") ]
  ])

  ;; Let's set the common size of the seen world for every type of network:
  set-patch-size 500 / world-width
end


to set-agents
  (ifelse
    set_agents = "OWN" [
      file-close
      file-open agents_name
      print (word "File describes " N " agents using these variables:\n" file-read-line)
      (foreach (shuffle sort turtles) [ [t] ->
        ask t [
          let line csv:from-row file-read-line
          let bv but-first (but-last (line))
          set belief_vector bv
          set idno first line
          set group last line
          ;show (word "Length: " length(bv) ", Group: " group ", ID: " idno ", Believes: " bv ", Min: " min(bv) ", Max: " max(bv))
        ]
      ])
      file-close
    ]
    ; elsecommands: set_agents = "random"
    [
      ;; NOT NOW!
      ;print "We are now go for representation of real respondents, no play with random agents now!"

    ask turtles [
      set group 1 + random 6
      set belief_vector n-values 5 [1 - random-float 2]]

  ])
end


to set-links
  ;; NOT NEEDED NOW!
  ask links [set hidden? hidden_links?]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   G O   P R O C E D U R E S   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to go

  ask links [
    ;; The core procedure happens only with probability ALPHA for avery connected pair of agents:
    if alpha > random-float 1 [be-socially-influenced] ; process 1 and 4 (dropping of link process inside)
    set hidden? hidden_links?
  ]

  ;; Self-coherency checks and new links creation will do agents in random order:
  ask turtles [
    ;; Self-coherency tries and checks
    if beta > random-float 1 [check-self-coherency]  ; process 2

    ;; establishing of new random link
    if kappa > random-float 1 [add-new-link] ; process 3
  ]

  visualize

  tick

end


to visualize
  ask turtles [
    setxy (round(sqrt(N)) * item (x_belief - 1) belief_vector) (round(sqrt(N)) * item (y_belief  - 1) belief_vector)
    set color (group * 10) + 5
  ]

  ask links [set hidden? hidden_links?]

end


to be-socially-influenced
  ;; Select who is SENDER and who is RECEIVER
  let ends shuffle sort both-ends  ;; SORT transforms agent set to list, SHUFFLE randomizes the order
  let sender first ends  ;; Since we randomized order in the pair of agents we might take the first as SENDER...
  let receiver last ends  ;; ...and the last as RECEIVER
  ; print (word "Is later sometimes the first? " (([who] of sender) > ([who] of receiver)) ", because " sender " sends belief to " receiver) ;; just for code-checking...

  ;; SENDER randomly picks the belief dimension and get her belief value:
  let message 0  ;; we need to initialize MESSAGE on the level of the link
  let dimension random 5  ;; same with the dimension, resp. we could directly randomly set the dimension; BTW: sorry for hard-wiring number of dimensions... but we change it later...
  ask sender [
    set message item dimension belief_vector
    ;print (word dimension "; " message "; " belief_vector)  ;; just for code-checking...
  ]
  ask receiver [
    ;; firstly, we store belief_vector for later comparison for determining whwther change happened
    let previous_belief_vector belief_vector

    ;; Main function, we changing belief her:
    change-belief (belief_vector) (replace-item dimension belief_vector message) (group) (dimension)

    ;; Checking whwther the belief_vector is still not changed:
    ;; TRUE means that belief was rejected, e.g. there is no change in previous/belief_vector:
    let belief_reject (belief_vector = previous_belief_vector)

    ; dropping link process 4
    if (abs (message - (item dimension belief_vector))) > belief_distance_treshold AND belief_reject AND gamma > random-float 1 [
      ask myself [die]
    ]
  ]
end

to-report euclid [one second]
  let l 0
  let distances []
  while [l < length(one)] [
    set distances lput (((item l one) - (item l second)) ^ 2) distances
    set l l + 1
  ]
  ;if (sqrt(sum(distances)) / sqrt(length(distances))) > belief_distance_treshold [print (sqrt(sum(distances)) / sqrt(length(distances)))]
  report (sqrt(sum(distances)) / sqrt(length(distances)))
end


to check-self-coherency
  let belief_position random 5
  let focal_belief item belief_position belief_vector
  let changed_focal_belief precision (focal_belief + random-normal 0 craziness_of_new_belief) 3
  if changed_focal_belief > 1 [set changed_focal_belief 1 ]
  if changed_focal_belief < -1 [set changed_focal_belief -1]
  change-belief (belief_vector) (replace-item belief_position belief_vector changed_focal_belief) (group) (belief_position)

end



  to add-new-link   ;; All agents create one or zero new links to a new agent. Note, multiple agents might establish a link with the same agent
     let potential_new_neighbors other turtles with [(not link-neighbor? myself)]
     create-link-with one-of potential_new_neighbors
end


to change-belief [old new group_num belief_position]
  let matrix table:get coherency_matrices [group] of self ; get group of agent
  let old_coherency coherence-function (matrix) (old)
  let new_coherency coherence-function (matrix) (new)
  let diff_coherency new_coherency - old_coherency

  let prob 1 / ( 1 + exp (- k * diff_coherency))
  if prob > random-float 1 [
      let belief_change conformity_tendency * ((item belief_position new) - (item belief_position old)) ; increase or decrease of belief
      let new_belief (item belief_position old) + belief_change
      if new_belief > 1 [set new_belief 1 print "attention"]
      if new_belief < -1 [set new_belief -1 print "attention"]
      set belief_vector replace-item belief_position belief_vector (precision new_belief 3)
   ]
end


to-report coherence-function [matrix vector]
  let i 0
  let products []
  while [i <= 3]
    [let j (i + 1)
       while [j <= 4]
          [let r matrix:get matrix i j
           set products lput (r * item i vector * item j vector) products
           set j j + 1 ]
    set i i + 1 ]
  report sum (products)
end
@#$#@#$#@
GRAPHICS-WINDOW
204
10
712
519
-1
-1
5.2631578947368425
1
10
1
1
1
0
0
0
1
-47
47
-47
47
1
1
1
ticks
30.0

BUTTON
11
24
74
57
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
74
24
137
57
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
137
24
204
57
1-step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

CHOOSER
11
57
103
102
network_type
network_type
"random" "Watts" "Kleinberg" "Barabasi" "Bruce" "OWN"
4

SLIDER
11
101
204
134
N
N
10
5000
2203.0
1
1
NIL
HORIZONTAL

SLIDER
10
133
102
166
neis
neis
1
50
5.0
1
1
NIL
HORIZONTAL

SLIDER
101
133
193
166
rewiring
rewiring
0.001
1
0.01
0.001
1
NIL
HORIZONTAL

SWITCH
11
165
153
198
toroidial_Kleinberg?
toroidial_Kleinberg?
0
1
-1000

SLIDER
11
198
157
231
clustering_exponent
clustering_exponent
0.01
10
0.02
0.01
1
NIL
HORIZONTAL

SLIDER
10
231
102
264
min_degree
min_degree
1
10
2.0
1
1
NIL
HORIZONTAL

BUTTON
103
57
204
90
save network
nw:save-matrix network_name 
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
715
21
954
81
network_name
network.txt
1
0
String

CHOOSER
12
269
150
314
set_agents
set_agents
"random" "OWN"
1

INPUTBOX
715
80
954
140
agents_name
DE/items.csv
1
0
String

INPUTBOX
715
140
954
200
coherence_name
DE/correlations.csv
1
0
String

CHOOSER
12
318
104
363
k
k
1 2 3 4 5 10 20 100
5

SLIDER
715
205
887
238
conformity_tendency
conformity_tendency
0
1
0.5
0.05
1
NIL
HORIZONTAL

SLIDER
716
245
902
278
craziness_of_new_belief
craziness_of_new_belief
0
1
0.1
0.05
1
NIL
HORIZONTAL

SLIDER
935
209
1107
242
alpha
alpha
0
1
0.1
0.05
1
NIL
HORIZONTAL

SLIDER
936
247
1108
280
beta
beta
0
1
0.9
0.05
1
NIL
HORIZONTAL

SLIDER
937
313
1109
346
kappa
kappa
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
936
279
1108
312
gamma
gamma
0
1
0.0
0.05
1
NIL
HORIZONTAL

SLIDER
970
24
1142
57
x_belief
x_belief
1
5
3.0
1
1
NIL
HORIZONTAL

SLIDER
972
63
1144
96
y_belief
y_belief
1
5
5.0
1
1
NIL
HORIZONTAL

SLIDER
973
107
1158
140
belief_distance_treshold
belief_distance_treshold
0
2
0.1
.1
1
NIL
HORIZONTAL

SWITCH
13
363
125
396
hidden_links?
hidden_links?
0
1
-1000

SWITCH
13
396
119
429
set_seed
set_seed
0
1
-1000

INPUTBOX
13
429
168
489
RS
10.0
1
0
Number

@#$#@#$#@
## Files needed for the correct run of model:
- netLogo model: main file with the model code 
- `Correlationmatrix.csv`: contains coherence matrices for 6 groups estimated from ESS 9 Germany (2018)
- `agents.csv`: contains data on 5 believes, estimated group (1--6), and ID (just for ability to connect it with used data, nothing important for the model run) from ESS 9 Germany (2018) 
- `network.txt`: file storing network structure -- by the button `save network` you might save present network structure, but you also might generate your own network structure (unweighted, just 1/0 informing of existence of links)

## The purpose of the model  
  
Test whether cognitive consistent agent might create polarized public/society. There are various consistency matrices, for each group of agents there is one. 
  
## Basics of the model  
  
Agents offer believes each other, then they compare the consistency of prior set of believes with the consistency of possibly updated set of believes, and proportionaly to the difference they accept new belief or not. 

Agents operate on communication network. It means that they offers their believes and receive the believes only from nodes of the network they have an edge with. 

## Course of procedures in the model  


## Initialization  

- Initialize communication network
- Set agents variables
- Reset ticks

 
## Parameters description  

- network_type: type of network which we initialize at the start
- N: number of agents/turtles in the simulation
- neis: parameter for Watts-Strogatz small-world network -- number of direct neighbors on the left side
- rewiring: in Watts-Strogatz small-world network probability that link to direct neighbor is re-wired to random agent/turtle; in random Erdos-Renyi network it specifies probability of creation of link between any pair of agents/turtles
- toroidial_Kleinberg?: in Kleinberg small-world network it specifies whether the initial adjacency network is toroidial or not 
- clustering_exponent: in Kleinberg small-world network it specifies behavior of creating close and long dostant links (it is said in NetLogo dictionary, that 2 is standard value)
- min_degree: in Barabasi-Albert preferential attachement network it specifies the minimum degree of nodes; in Bruce planar network it also specifies the minimum degree, i.e. how many closest non-linked nodes the connecting node conects to
- set_agents: specifies how the agents/turtles are initialized: `random` means that belief vector and group membership is randomly initialized, `OWN` means that we read in values from files `agents.csv` -- NOTE: Assure yourself you downloaded also this file!
- k: steepnes of sigmoid function
- hidden_links?: hides/shows links -- with hiddent links the simulation is faster
- set_seed & RS: if `set_seed` is TRUE/ON, random seed is set for value `RS`
- network_name: states filename where is stored network structure to which the communication network is initialized (for `network_type` 'OWN')
- agents_name: states filename where is stored information which is fed in agents during initialization (for `set_agents` 'OWN')
- coherence_name: states filename where are stored coherence matrices (NOTE: It is needed to have such a file! Model doesn't function for random initialization of these matrices!) - conformity_tendency: states speed with which `receiver` moves her value of belief toward proposed value of `sender`
- craziness_of_new_belief: states SD of random change of randomly selected belief in the self-consistency/beta phase
- x_belief: the index (numbered from 1!) of belief from belief vector according which are agents/turtles plotted on X axis
- y_belief: the index (numbered from 1!) of belief from belief vector according which are agents/turtles plotted on Y axis
- belief_distance_treshold: states treshold of absolute difference of sender's and receiver's values -- if the difference is larger than threshold and receiver rejects this value for the focal belief, thah with probability `gamma` receiver kills the communication link between sender and receiver
- alpha: states probability that link initializes communication between sender and receiver which might lead to exchange of value of focal belief
- beta: states probability that agent/turtle does self-consistency -- randomly changes value of randomly chosen focal belief and then compares whether is more consistent original or changed value of focal belief and then chooses the more consistent value
- gamma: states probability that receiver drops link with sender who offered as message too different value of focal belief ('too different' is controlled by `belief_distance_treshold`)
- kappa: states probability that agent/turtle creates communication link with not coonected random other agent/turtle


## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
