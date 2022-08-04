;;;;; Model using coherence agents -- written from scratch

;; Updated: 2022-07-12 FranCesko

;; Brucified version based on Marlene's version ("newnew" or ver2, 15/Juk/22)


;; HEADER STUFF
extensions [nw matrix csv table profiler]

turtles-own [
  idno               ;; identity number from data
  belief_vector      ;; a vector of values, each in [-1, 1] representing intensity of attituden
  group              ;; the number of the group it is in
]

links-own [
  link_health        ;; a measure of whether the relationship is consant or not, from -1 to 1
]

globals [
  coherency_matrices  ;; a table: group number -> its coherency matrix
  num_agents          ;; number of agents (usually the same as the number of subjects in survey data)
  num_items           ;; how many items in data - the dimension of the belief vectors and correlation matriccies
  item_labels         ;; a list of item labels, in order
  secs-per-tick       ;; smoothed time for each tick
  world_size          ;; visual size of world
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   S E T U P   P R O C E D U R E S   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to __SETUP_procedures end


;; SETUP
to setup
;- Clear everything
  ca
  if rand-seed = 0 [set rand-seed new-seed]
  random-seed rand-seed

;- Initialize globals
  initialize-globals

;- Initialize communication network and create agents
  initialize-comm-network

;- Initialise links in network
  initialize-links

;- Set agent variables - mostly attitudes
  initialize-agents

;- Reset ticks
  reset-ticks

;- Do visualisation
  visualize

  set secs-per-tick 0
  reset-timer

end

to initialize-globals
  set world_size 500
  ;; Now, there is only one global: coherency_matrices, let's initialize it as table!
  set coherency_matrices table:make

  ;; read in some agent globals
  ifelse file-exists? agent_data  [
    set num_agents file-length (agent_data)  ;; Counting number of rows with data on agents
    file-open agent_data
    set item_labels but-first but-last csv:from-row file-read-line
    set num_items length item_labels
    clear-output
    output-print (word "File '" agent_data "' describes " num_agents " agents using " num_items " variables:\n   " item_labels)
    file-close
  ] [
    error (word "agents_name file (" agent_data ") does not exist!")
  ]

  ;;  read in correlation matricies
  ifelse file-exists? corol_mat_file [
    file-close
    file-open corol_mat_file
    let coh_item_labels but-last but-last csv:from-row file-read-line
    if item_labels != coh_item_labels [error (word "Item labels in " agent_data  " " item_labels ", and " corol_mat_file " " coh_item_labels " are not the same!")]
    let i 1  ;; index of group/coherence matrix
    while [not file-at-end?] [
      let j 1  ;; index of line inside the matrix we create now
      let l []
      while [j <= num_items][  ;;
        let fl (but-last (but-last (csv:from-row file-read-line)))   ;; also hardcoded: We know that we do not use for consistence matrix the last two values
        let flp []
        foreach (fl) [[nx] -> set flp lput (precision (nx) 3) flp ]  ;; just rounding to 3 decimal places, to make matrices more readable
        set l lput flp l
        set j j + 1
      ]
      let m matrix:from-row-list l
      table:put coherency_matrices i m
      set i i + 1
    ]
    file-close
  ][error (word "FILE NOT FOUND! You have to put alongside the model file '" corol_mat_file "' describing your coherence matrices") ]

    ;; Checking how table with coherency matrices look like.
    foreach sort table:keys coherency_matrices [gn ->
      output-print (word "Group: " gn)
      output-print matrix:pretty-print-text (table:get coherency_matrices gn)
  ]
end

to initialize-comm-network
  ;; Which kind of network we are for?
  (ifelse
    network_type = "Watts" [
      resize-world (0 - round(sqrt(num_agents))) round(sqrt(num_agents)) (0 - round(sqrt(num_agents))) round(sqrt(num_agents))
      nw:generate-watts-strogatz turtles links num_agents neis rewiring [ fd (round(sqrt(num_agents)) - 1) ]
      ask links [set hidden? not show-new-links?]
      output-print "Initial network set to Watts"
    ]
    network_type = "Kleinberg" [
      resize-world 0 (round(sqrt(num_agents)) - 1) 0 (round(sqrt(num_agents)) - 1)
      nw:generate-small-world turtles links round(sqrt(num_agents)) round(sqrt(num_agents)) 2.0 toroidial_world?
      (foreach (sort turtles) (sort patches) [ [t p] -> ask t [ move-to p ] ])
      ask links [set hidden? not show-new-links?]
      ask turtles [set size 0.5]
      output-print "Initial network set to Kleinberg"
    ]
    network_type = "Barabasi" [
      resize-world (0 - round(sqrt(num_agents))) round(sqrt(num_agents)) (0 - round(sqrt(num_agents))) round(sqrt(num_agents))
      nw:generate-preferential-attachment turtles links num_agents min_degree [ fd (round(sqrt(num_agents)) - 1) ]
      ask links [set hidden? not show-new-links?]
      output-print "Initial network set to Barabasi"
    ]
    network_type = "Random" [
      resize-world (0 - round(sqrt(num_agents))) round(sqrt(num_agents)) (0 - round(sqrt(num_agents))) round(sqrt(num_agents))
      nw:generate-random turtles links num_agents rewiring [ fd (round(sqrt(num_agents)) - 1) ]
      ask links [set hidden? not show-new-links?]
      output-print "Initial network set to Random"
    ]
    network_type = "Planar" [
      crt num_agents [setxy random-xcor random-ycor]
      ask turtles [
        repeat min_degree [
          ifelse prob rewiring
            [create-link-with one-of other turtles with [not link-neighbor? myself] [set hidden? not show-new-links?]]
            [create-link-with (min-one-of (other turtles with [not link-neighbor? myself]) [distance myself]) [set hidden? not show-new-links?]]
        ]
      ]
      output-print "Initial network set to Planar"
    ]
    ;; this reads a previoiusly stored network from file
    network_type = "From File"   [
      ifelse file-exists? network_file and file-exists? agent_data [
        file-close
        file-open agent_data
        set num_agents file-read
        file-close
        resize-world (0 - round(sqrt(num_agents))) round(sqrt(num_agents)) (0 - round(sqrt(num_agents))) round(sqrt(num_agents))
        nw:load-matrix network_file turtles links [ fd (round(sqrt(num_agents)) - 1) ]
        ask links [set hidden? not show-new-links?]
        output-print (word "Initial network set using saved file, " network_file)
      ][error (word "FILE NOT FOUND! You have to put alongside the model these files '" network_file "' and '" agent_data "' describing your network") ]
    ]
    [error "Network type unknFrom File!"]
  )

  ;; Some of the above network methods create slightly the wrong number of agents so cull down to right number -- ***CLUDGE!***
  if count turtles > num_agents [
    ask n-of (count turtles - num_agents) turtles [die]
  ]
  ;; Let's set the common size of the seen world for every type of network:
  set-patch-size world_size / world-width
  if count turtles != num_agents [error (word "Not the right number of turtles! There are: " count turtles)]
end

to initialize-links
  ;;
  (ifelse
    Initial_link_goodness = "Random Uniform" [
      ask links [set link_health rand-unif-val]
      output-print "Initial link healths set randomly from a uniform [-1, 1] distribution"
    ]
    Initial_link_goodness = "Random Normal" [
      ask links [set link_health rand-norm-val]
      output-print "Initial link healths set randomly from a normal N(0,0.3333) distribution"
    ]
    Initial_link_goodness = "High" [
      ask links [set link_health 1]
      output-print "Initial link healths all set to 1"
    ]
    Initial_link_goodness = "Neutral" [
      ask links [set link_health 0]
      output-print "Initial link healths all set to 0"
    ]
    Initial_link_goodness = "Low" [
      ask links [set link_health -1]
      output-print "Initial link healths all set to -1"
    ]
    [error "Link initialisataion type unknown!"]
    )
end


to initialize-agents
  ;; initialise agent beliefs
    file-close
    file-open agent_data
    let line file-read-line
    ;show line
    (foreach (sort turtles) [ [t] ->
      ask t [
        set line csv:from-row file-read-line
        ;; show line
        let bv but-first (but-last (line))
        set belief_vector bv
        set idno first line
        set group last line
        if not is-number? group [error (word "Turtle " who " has a group which is not a number")]
        ;show (word "Length: " length(bv) ", Group: " group ", ID: " idno ", Believes: " bv ", Min: " min(bv) ", Max: " max(bv))
      ]
    ])
    file-close

    (ifelse
    set_agents = "From File" [
      output-print (word "Agent beliefs set from file: " agent_data)
    ]
    set_agents = "Random Normal" [
      ask turtles [set belief_vector n-values num_items [rand-norm-val]]
      output-print "Agent beliefs set using random normal values"
    ]
    set_agents = "Random Uniform" [
      ask turtles [set belief_vector n-values num_items [rand-unif-val]]
      output-print "Agent beliefs set using random uniform values"
    ]
    set_agents = "Bipolar" [
      ask turtles [set belief_vector n-values num_items [rand-bipolar-val]]
      output-print "Agent beliefs set using random bipolar values"
    ]
    set_agents = "Neutral" [
      ask turtles [set belief_vector n-values num_items [0]]
      output-print "Agent beliefs set to 0"
    ]
    ; elsecommands: set_agents = "random"
    [
      error (word "Agent initialisation method " set_agents " not implemented!")
    ])
  ;; other stuff
  ask turtles [
    set shape "circle"
    set color (group * 10) + 5
    ifelse network_type = "Kleinberg" [set size 0.5] [set size 1]
  ]
end


to-report file-length [file-name]
  ;; Opening the file for counting lines
  file-close
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

  ;; Reporting the length of file
  report l
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   G O   P R O C E D U R E S   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to __GO_procedures end

to go

  ask links [
    ;; maybe hide old links
    set hidden? not show-old-links?
    ;; The core procedure happens only with probability prob_soc_infl for every connected pair of agents:
    if prob prob_soc_infl [be-socially-influenced] ; process 1 (includes link health update)
    if prob (drop-bad-link * logistic link_health) [die] ;; process 4 using same logistic function with same k
  ]

  ;; Self-coherency checks and new links creation will do agents in random order:
  ask turtles [
    ;; Self-coherency tries and checks
    if prob prob_self_check [check-self-coherency]  ; process 2

    ;; establishing of new random link
    if prob prob_add_link [add-new-link] ; process 3
    if link_lonely? and (not any? my-links) [add-new-link]
  ]

  visualize

  if rearrange_every > 0 and (ticks mod rearrange_every = 0)
    [rearrange]

  if max_time > 0 and ticks >= max_time [stop]

  set secs-per-tick (0.01 * timer) + (0.99 * secs-per-tick)
  reset-timer

  tick

end

to be-socially-influenced
  let new_coherency 0
  ;; Select who is SENDER and who is RECEIVER
  let ends shuffle sort both-ends  ;; SORT transforms agent set to list, SHUFFLE randomizes the order
  let sender first ends  ;; Since we randomized order in the pair of agents we might take the first as SENDER...
  let receiver last ends  ;; ...and the last as RECEIVER
  ; print (word "Is later sometimes the first? " (([who] of sender) > ([who] of receiver)) ", because " sender " sends belief to " receiver) ;; just for code-checking...

  ;; SENDER randomly picks the belief dimension and get her belief value:
  let message 0  ;; we need to initialize MESSAGE on the level of the link
  let dimension random num_items  ;; randomly set the dimension
  ask sender [
    set message item dimension belief_vector
    ;print (word dimension "; " message "; " belief_vector)  ;; just for code-checking...
  ]
  ask receiver [
    ;; Main function, we are changing belief here as well as returning coherency of suggested belief
    set new_coherency change-belief (belief_vector) (replace-item dimension belief_vector message) (group) (dimension)
    ]
  ;; update link health based on coherency of suggested belief
  set link_health update_health link_health new_coherency
end

to-report update_health [last_health recent_coherency]
  report (link_health_ch * recent_coherency) + ((1 - link_health_ch) * last_health)
end

to-report euclid [one second]
  ;; work out euclidean distance between two vectors
  report (sqrt (sum (map [[f l] -> (f - l) ^ 2] one second) / num_items))
end

to check-self-coherency
  let belief_position random num_items
  let focal_belief item belief_position belief_vector
  let changed_focal_belief precision (focal_belief + random-normal 0 var_of_new_belief) 3
  if changed_focal_belief > 1 [set changed_focal_belief 1 ]
  if changed_focal_belief < -1 [set changed_focal_belief -1]
  let changed? change-belief (belief_vector) (replace-item belief_position belief_vector changed_focal_belief) (group) (belief_position)
end

to add-new-link   ;; All agents create one or zero new links to a new agent. Note, multiple agents might establish a link with the same agent
  if max_num_links = 0 or count my-links < max_num_links [  ;; max_num_links = 0 means no maximum
    let potential_new_neighbors no-turtles
    if prob prob_FoF    ;; with a probability set potential friends to friends of friends
      [set potential_new_neighbors other (turtle-set [link-neighbors] of link-neighbors)]
    if not any? potential_new_neighbors
      [set potential_new_neighbors other turtles]
    if no_repeat_link?
      [set potential_new_neighbors potential_new_neighbors with [(not link-neighbor? myself)]]
    if any? potential_new_neighbors
      [create-link-with (one-of potential_new_neighbors) [set hidden? not show-new-links?]]
  ]
end

to-report change-belief [old new group_num belief_position]
  ;; reports coherency of suggested belief
  let matrix table:get coherency_matrices [group] of self ; get correlation matrix of agent's group
  let old_coherency coherence-function (matrix) (old)
  let new_coherency coherence-function (matrix) (new)
  let diff_coherency new_coherency - old_coherency
  if prob logistic diff_coherency [
      let belief_change conformity_tendency * ((item belief_position new) - (item belief_position old)) ; increase or decrease of belief
      let new_belief (item belief_position old) + belief_change
      if new_belief > 1 [set new_belief 1 print "attention"]
      if new_belief < -1 [set new_belief -1 print "attention"]
      set belief_vector replace-item belief_position belief_vector (precision new_belief 3)
   ]
  report new_coherency
end

to-report coherence-function [matrix vector]
  ifelse Coherence_Meth = "Frant"
    [report Frant matrix vector]
    [error (word "Coherence Method, " Coherence_Meth " is not implemented!")]
end

to-report Frant [matrix vector]
  ;; use matrix algrebra?
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  VISUALISATIONS & GRAPHS      ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to __VIS+GRAPH_procedures end

to visualize
  if not visualisations? [stop]
  set-current-plot "attitude space"
  clear-plot
  set-plot-background-color 0
  ask turtles [
    set-plot-pen-color (group * 10) + 5
    plotxy (some_fuzz + item (x_belief - 1) belief_vector) (some_fuzz + item (y_belief  - 1) belief_vector)
  ]

  set-current-plot "Attitude Dynamics"
  let ts no-turtles
  ifelse group_shown < 1
    [set ts turtles]
    [set ts turtles with [group = group_shown]]
  ask ts [
    set-plot-pen-color (group * 10) + 5
    plotxy ticks item (belief_shown - 1) belief_vector
  ]
end

to-report some_fuzz
  report random-normal 0 fuzz
end

to rearrange
  let dist world_size / (4 * sqrt num_agents)
  repeat 20 [ layout-spring turtles links 0.25 dist 0.05]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;       U T I L I T I E S       ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to __UTILITY_procedures end

to-report rand-norm-val
  ;; produces a random normal in [-1, 1], av 0 sd 1/3
  report force-range random-normal 0 0.333333333
end

to-report force-range [vl]
  report min list 1 max list -1 vl
end

to-report rand-unif-val
  ;; produces a random val from [-1, 1] ;; but not 1!
  report (random-float 2) - 1
end

to-report rand-bipolar-val
  ;; uses rand-norm-val to produce values tending araound -1 and 1
  let bp rand-norm-val
  ifelse bp > 0
    [report 1 - bp]
    [report -1 - bp]
end

to-report logistic [vl]
  ;; the logistic function
  report 1 / ( 1 + exp (- k * vl))
end

to-report linear [vl]
  ;; the linear function from [-1, 1] tp [0, 1]
  report (1 + vl) / 2
end

to-report prob [vl]
  if vl = 0 [report false]
  ;; makes code slightly cleaner :-)
  report (random-float 1) < vl
end
@#$#@#$#@
GRAPHICS-WINDOW
148
10
656
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
5
10
72
43
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
78
10
143
43
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
5
44
72
77
step
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
4
161
141
206
network_type
network_type
"Random" "Watts" "Kleinberg" "Barabasi" "Planar" "From File"
4

SLIDER
4
241
142
274
neis
neis
1
50
19.0
1
1
NIL
HORIZONTAL

SLIDER
4
275
146
308
rewiring
rewiring
0
1
0.0
0.001
1
NIL
HORIZONTAL

SWITCH
3
309
146
342
toroidial_world?
toroidial_world?
1
1
-1000

SLIDER
3
343
146
376
clustering_exp
clustering_exp
0.01
10
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
4
207
143
240
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
78
45
143
78
save netw
nw:save-matrix network_file
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
658
10
817
70
network_file
network.txt
1
0
String

CHOOSER
3
471
144
516
set_agents
set_agents
"From File" "Random Normal" "Random Uniform" "Bipolar" "Neutral"
0

INPUTBOX
821
10
973
70
agent_data
DE/items.csv
1
0
String

INPUTBOX
977
10
1126
70
corol_mat_file
DE/correlations.csv
1
0
String

SLIDER
808
140
976
173
conformity_tendency
conformity_tendency
0
1
0.3
0.05
1
NIL
HORIZONTAL

SLIDER
808
175
977
208
var_of_new_belief
var_of_new_belief
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
807
210
977
243
prob_soc_infl
prob_soc_infl
0
1
0.1
0.05
1
NIL
HORIZONTAL

SLIDER
807
245
975
278
prob_self_check
prob_self_check
0
1
0.15
0.05
1
NIL
HORIZONTAL

SLIDER
979
144
1126
177
prob_add_link
prob_add_link
0
1
0.07
0.01
1
NIL
HORIZONTAL

SLIDER
978
108
1128
141
drop-bad-link
drop-bad-link
0
1
0.07
0.01
1
NIL
HORIZONTAL

SLIDER
903
650
1013
683
x_belief
x_belief
1
5
5.0
1
1
NIL
HORIZONTAL

SLIDER
1015
650
1126
683
y_belief
y_belief
1
5
4.0
1
1
NIL
HORIZONTAL

PLOT
785
685
1128
1020
attitude space
NIL
NIL
-1.05
1.05
-1.05
1.05
false
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

SWITCH
659
180
801
213
show-old-links?
show-old-links?
1
1
-1000

PLOT
148
522
308
642
Links per agent
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count links / count turtles"

TEXTBOX
7
146
157
164
Network Parameters
11
0.0
1

TEXTBOX
809
71
959
89
Process Parameters
11
0.0
1

TEXTBOX
4
453
154
471
Agent iniitialisation
11
0.0
1

SLIDER
979
248
1127
281
max_num_links
max_num_links
0
20
10.0
1
1
NIL
HORIZONTAL

SWITCH
660
214
801
247
show-new-links?
show-new-links?
1
1
-1000

SLIDER
785
649
898
682
fuzz
fuzz
0
0.025
0.002
0.001
1
NIL
HORIZONTAL

INPUTBOX
919
76
969
136
k
10.0
1
0
Number

CHOOSER
808
88
913
133
Coherence_Meth
Coherence_Meth
"Frant"
0

OUTPUT
664
323
1125
647
12

INPUTBOX
6
81
79
141
rand-seed
1.254654168E9
1
0
Number

MONITOR
661
74
711
119
s/tck
secs-per-tick
3
1
11

SLIDER
3
574
131
607
belief_shown
belief_shown
1
5
1.0
1
1
NIL
HORIZONTAL

PLOT
3
649
782
1020
Attitude Dynamics
NIL
NIL
0.0
1.0
-1.0
1.0
true
false
"clear-plot\nset-plot-background-color 0" ""
PENS
"default" 1.0 2 -16777216 true "" ""

SWITCH
2
540
130
573
visualisations?
visualisations?
1
1
-1000

PLOT
313
523
484
643
Histogram Node Arities
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "ifelse max_num_links > 0 \n  [set-plot-x-range 0 (max_num_links + 1)\n   set-histogram-num-bars (max_num_links + 1)]\n  [set-plot-x-range 0 10\n   set-histogram-num-bars 10]"
PENS
"default" 1.0 1 -16777216 true "" "histogram [count my-links] of turtles"

SLIDER
662
248
800
281
rearrange_every
rearrange_every
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
4
610
133
643
group_shown
group_shown
0
10
0.0
1
1
NIL
HORIZONTAL

BUTTON
661
283
798
316
Re-arrange Now
rearrange
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
980
214
1126
247
link_lonely?
link_lonely?
1
1
-1000

SLIDER
979
179
1126
212
prob_FoF
prob_FoF
0
1
0.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
665
165
760
183
World Viz Params
11
0.0
1

MONITOR
714
74
766
119
lnks/ag
count links / num_agents
2
1
11

TEXTBOX
7
521
157
539
Vizualisation Stuff\n
11
0.0
1

INPUTBOX
83
81
147
141
max_time
0.0
1
0
Number

TEXTBOX
10
385
104
403
Link initialisation
11
0.0
1

CHOOSER
4
402
145
447
Initial_link_goodness
Initial_link_goodness
"Random Uniform" "Random Normal" "High" "Neutral" "Low"
0

SLIDER
977
72
1126
105
link_health_ch
link_health_ch
0
1
1.0
0.01
1
NIL
HORIZONTAL

PLOT
488
524
660
646
Histogram Link Health
NIL
NIL
-1.0
1.0
0.0
10.0
true
false
"set-plot-x-range -1 1\nset-histogram-num-bars 16" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [link_health] of links"

SWITCH
981
285
1128
318
no_repeat_link?
no_repeat_link?
1
1
-1000

@#$#@#$#@
## This version (BE)

Builds upon and messes with Frantisek's version :-)

V ...2.1 adds an option to not check if a link aleady exists when making new link - speeds things up a lot! :-)

This adds parameters and visualisations, but most importantly it does link dropping differently, namely:

* Links have a variable: *link_health* ranging from -1 to 1
* During interaction the *link_health* is updated by the coherence of the suggested belief with the recipients beliefs (**not** the difference in coherence)
* Each time tick each link can die with probability linked to its health *(drop-bad-link x logistic link_health)*

This logistic uses the same *k* as the logistic used in other coherence difference -> prob calculations. To facilitate the above process the *change-belief* procedure is changed into a function, that does the same as before but returns the coherence of the suggested belief with recipient's beliefs (so we do not calculate it twice).

## Files needed for the correct run of model:
- netLogo model: main file with the model code 
- `Correlationmatrix.csv`: contains coherence matrices for 6 groups estimated from ESS 9 Germany (2018)
- `agents.csv`: contains data on 5 believes, estimated group (1--6), and ID (just for ability to connect it with used data, nothing important for the model run) from ESS 9 Germany (2018) 
- `network.txt`: file storing network structure -- by the button `save network` you might save present network structure, but you also might generate your own network structure (unweighted, just 1/0 informing of existence of links)

## The purpose of the model  
  
Test whether cognitive consistent agent might create polarized public/society. There are various consistency matrices, for each group of agents there is one. 
  
## Brief idea:

We try to costruct model where agents offer other agents values of shared believes,
then they evaluate according the coherence matrix of their group the offered values and
then they receive them or refuse, according the higher consistency.
Agents communicate over the communication network which change co-develop with the agents initeractino and beliefs.

Agents offer believes each other, then they compare the consistency of prior set of believes with the consistency of possibly updated set of believes, and proportionaly to the difference they accept new belief or not.

Agents operate on communication network. It means that they offers their believes and receive the believes only from nodes of the network they have an edge with. 

## Course of procedures in the model  

Agents operate in random order (different each tick).

Agents firstly check their extroversy whether they will offer the belief. For now it will be the individual parameter derrived from random normal distribution. Later we might bring some function like 'Fear of isolation' from Spiral of Silence.

Offering agents choose their partner for belief exchange, then they choose their belief and pass it. Note: Now we choose randomly, later we might select partner according prior success of the belief passing, believes closeness, number of successful interactions, ratio of successful interactions etc. We might later choose belief non-randomly, as well: choice might be proportional to the success ratio of passing belief, number of exchanges, or we might generate saliency function (e.g., via Markov matrix) etc.

Receiving agents decide whether they want to comunicate about the belief. For now we make it for sure (communication forced by passing agent), but later we might make the probability of communication refusal proportional to belief inconsistency, e.g. the more incosistency the belief makes in the cognitive system of the agent the more probable will the agent communicate about the belief. 

Receiving agents which accept the communication then decide upon the adoption of belief. They compare the consistency of the present value of belief with the consistency of the offered belief and they proportionaly to the difference of these consistencies decide for adoption or against it.

All agents update their believes and the next round starts.

## Initialization  

- Initialize communication network
- Set agents variables
- Set links variables
- Reset ticks

## Parameters description  

### Simulation control

* *rand-seed*: if != 0 then is the random seed for the simulation, otherwise sets it to a new (fixed) random choice
* *max_time*: the tick at which the simulation stops if > 0, otherwise no end tick

### Key files

- *agent_data*: the filename for the agent data - first row number of subjects, then field labels, then row for each subject: id, the value of each item and its group number
- *corol_mat_file*: the filename for the data file with the correlation matricies for each group 
- *network_file*: the filename to save and/or restore the network from


### Initial Network

- *network_type*: type of network which we initialize at the start
- *neis*: parameter for Watts-Strogatz small-world network -- number of direct neighbors on the left side
- *rewiring*: in Watts-Strogatz small-world network probability that link to direct neighbor is re-wired to random agent/turtle; in random Erdos-Renyi network it specifies probability of creation of link between any pair of agents/turtles
- *toroidial_world?*: in Kleinberg small-world network it specifies whether the initial adjacency network is toroidial or not 
- *clustering_exponent*: in Kleinberg small-world network it specifies behavior of creating close and long dostant links (it is said in NetLogo dictionary, that 2 is standard value)
- *min_degree*: in Barabasi-Albert preferential attachement network it specifies the minimum degree of nodes; in Bruce planar network it also specifies the minimum degree, i.e. how many closest non-linked nodes the connecting node conects to

### Link Initialisation

*Initial_link_goodness* can take one of the following values, to set the link variable, *link_health*:

* "Random Uniform" - initialises links with a uniform random float from [-1, 1]
* "Random Normal" - initialises links with a rundom normal N(0, 0.333333) constrained to be in [-1, 1]
* "High" - initialises links with the value 1
* "Neutral" - initialises links with the value 0
* "Low" - initialises links with the value -1


### Agent Initialisation

*set_agents* can take one of the following values, to set the values of its initial beliefs, *belief_vector*:

* "From File" - uses the file named in 'agent_data.csv" to set initial belief values
* "Random Normal" - sets all belief values randomly, using N(0, 0.3333)
* "Random Uniform" - sets all belief values randomly from [-1, 1]
* "Bipolar" - sets all belief values randomly N(1 0.333) 50% of time and N(-1, 0.333) otherwise
* "Neutral" - sets all belief values to 0

### Belief Process Parameters

* *Coherence_Meth*: the method used to determin coherency of beliefs
* *k*: the steepness of the logistic curve used to map [-1, 1] to probabilities. Lower numbers means more randomness
* *conformity_tendency*: how much an agents shifts its belief towards one that is successfully suggested by another agent
* *var_of_new_belief*: how big a variation of a belief is considered when an agent looks at the consistency of its beliefs
* *prob_soc_infl*: the probability that social influence is attempted along each link each tick
* *prob_self_check*: the probability that an agent evaluaates the consistency of a belief

### Link change parameters

* *link_health_ch*: how much the health of a link is upddated by the coherency of a belief attempted to be transmitted over it with respect to the recipient's beliefs
* *drop-bad-link*: the multiplicative factor applied to the mappepd health value to get the probability it will be dropped
* *prob_add_link*: the probability a new link will be added to a node 
* *prob_FoF*: if and when a new link is added the probability that a friend-of-a-friend link is chosen (if no such or otherwise it is randomly chosen)
* *link_lonely?*: whether to always add a link to a node with no links
* *max_num_links*: if > 0 then the maximum number of links allowed for a node (so no link adding for such), if = 0 then no limit

## Graphs and visualisations

### World view

The world view shows the social network, with the agents coloured by group and the links maaybe show (see options below).

* *show-old-links?*:  whether to show links (except newly created ones) - not showing them speeds up the simulation
* *show-new-links?*: whether to briefly show newly-crated links before they might be hidden next tick
* *Re-arrange Now*: button causes a spring layout algorithm to spread the network out (if possible)
* *rearrange_every*: forces the spring layout algorithm to be applied this number of ticks (if 0 then no re-arrangement)

### Visualisations

* *visualisations?*: this turns off or on the visualisations for speed

Bottom left is an 'Opinion Dynamics' like picture of the attitude of agents for one of the item dimensions (top 1, bottom -1). There is a dot for each subject's belief value at each tick (the y-axis)

* *belief_shown*: determines the item dimension that is shown
* *group_shown*: determines which group is shown (if =0 then all groups shown)

Bottom right is a 2D visualisation of the aattitude spade showing two item dimensions at the current tick of the simulation. Different groups are indicated by the colour of dots.

* *x_belief*: selects the dimensioni for the x-axis
* *y_belief*: selects the dimensioni for the y-axis
* *fuzz*: is the amount of noise in the plotting (useful if all dots are on exactly the same place)

There are three graphs below the world view. From left to right: (1) the average number links / agent over time (2) a historgram of the node arities (3) a histogram of link health values.

There are two monitors, to top right of world view: (a) the (smoothed) measure of seconds per simulation tick (b) the links per number of agents.

The text window shows summaries of the data reaad in.
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
