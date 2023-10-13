;;;;; Model using coherence agents -- version accompanying the paper

;; Updated: 2023-10-13 FranCesko


;; HEADER STUFF
extensions [nw array matrix csv table profiler time]

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
      let l [] ;; empty list for matrix processed in WHILE cycle
      let g 0 ;; group ID for dictionary -- we need to initialize variable here, but we have to set it inside WHILE cycle, and use it after WHILE cycle ends
      while [j <= num_items][  ;;
        let line csv:from-row file-read-line  ;; we have to read matrix line here, since we will use this info twice: for group ID and for a row of matrix
        if j = 1 [set g last line]            ;; setting group ID hardcoded: we know that group ID is the last column in the *.csv file; why IF? We do setting just in the first round of cycle, then it's not needed
        let fl (but-last (but-last (line)))   ;; also hardcoded: We know that we do not use for consistence matrix the last two values
        let flp []
        foreach (fl) [[nx] -> set flp lput (precision (nx) 3) flp ]  ;; just rounding to 3 decimal places, to make matrices more readable
        set l lput flp l
        set j j + 1
      ]
      let m matrix:from-row-list l
      set m (m matrix:- matrix:make-identity num_items)  ; set diagonal of correlation matrix to 0
      table:put coherency_matrices g m
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
      nw:generate-small-world turtles links round(sqrt(num_agents)) round(sqrt(num_agents)) 12.0 toroidial_world?
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
      ifelse file-exists? network_file [
        resize-world (0 - round(sqrt(num_agents))) round(sqrt(num_agents)) (0 - round(sqrt(num_agents))) round(sqrt(num_agents))
        nw:load-matrix network_file turtles links [ fd (round(sqrt(num_agents)) - 1) ]
        ask links [set hidden? not show-new-links?]
        output-print (word "Initial network set using saved file, " network_file)
      ][error (word "FILE NOT FOUND! You have to put alongside the model file '" network_file "' describing your network") ]
    ]
    [error "Network type unknown!"]
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
  ask links [
    set link_health 1
  ]

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
    set color (group * 20) + 5
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
    if prob (drop-bad-link * (logistic (link_health) (k_link)) ) [die] ;; process 4 using same logistic function with (negative) k_link
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
  ;; work out euclidean distance between two vectorse
  report (sqrt (sum (map [[f l] -> (f - l) ^ 2] one second)))
end

to-report manhattan-distance [one second]
  ;; work out manhattan distance between two vectors
  report (sum (map [[f l] -> abs (f - l) ] one second))
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
      [create-link-with (one-of potential_new_neighbors) [
        set hidden? not show-new-links?
        set link_health 1
        ]
      ]
  ]
end

to-report change-belief [old new group_num belief_position]
  ;; reports coherency of suggested belief
  let matrix table:get coherency_matrices [group] of self ; get correlation matrix of agent's group
  let old_coherency coherence-function (matrix) (old)
  let new_coherency coherence-function (matrix) (new)
  let diff_coherency new_coherency - old_coherency
  if prob (logistic (diff_coherency) (k))[
      let belief_change conformity_tendency * ((item belief_position new) - (item belief_position old)) ; increase or decrease of belief
      let new_belief (item belief_position old) + belief_change
      if new_belief > 1 [set new_belief 1 print "attention"]
      if new_belief < -1 [set new_belief -1 print "attention"]
      set belief_vector replace-item belief_position belief_vector (precision new_belief 3)
   ]
  report new_coherency
end

to-report coherence-function [matrix vector]
  ;; Coherency is calculated as  c = 0.5 * x^T * (C-1) * x = c12 x1 x2 + c13 x1 x3 + c23 x2 x3 + ...
  ;; where x=(x1, x2, x3, x4, x5) is a column-vector and x^T a row-vector (here for num_beliefs=5)
  ;; and where C-1 is the correlation matrix with 0 on the diagonal (see initialisation), i.e. no bias towards any of the belief.
  let col-vec matrix:from-column-list  ( list vector )    ; this is a (5,1)-matrix (column vector) of the agent's belief
  let row-vec  matrix:from-row-list  ( list vector )      ; this is a (1,5) matrix (row-vector) of the agent's belief
  let coherency 0.5 * matrix:get ( matrix:times row-vec matrix col-vec ) 0 0  ; do multiplication and extract coherency from (1,1)-matrix
  report coherency
end

to-report agent-coherence
  let matrix table:get coherency_matrices [group] of self ; get correlation matrix of agent's group
  report coherence-function matrix belief_vector
end

to-report extremness
  report (mean [manhattan-distance belief_vector n-values length belief_vector [0]] of turtles) / num_items
end

to-report diversity
  report mean map [x -> standard-deviation [item x belief_vector] of turtles] range num_items
end

to __GROUP_measures end

to-report group-extremness [grp]
  report ifelse-value (max table:keys coherency_matrices >= grp) [(mean [manhattan-distance belief_vector n-values length belief_vector [0]] of turtles with [group = grp]) / num_items]["NA"]
end

to-report group-diversity [grp]
  report ifelse-value (max table:keys coherency_matrices >= grp) [mean map [x -> standard-deviation [item x belief_vector] of turtles with [group = grp]] range num_items]["NA"]
end

to-report group-coherence [grp]
  report ifelse-value (max table:keys coherency_matrices >= grp) [mean [agent-coherence] of turtles with [group = grp]]["NA"]
end

to-report g1_ex
  report group-extremness (1)
end

to-report g1_dv
  report group-diversity (1)
end

to-report g1_ch
  report group-coherence (1)
end

to-report g2_ex
  report group-extremness (2)
end

to-report g2_dv
  report group-diversity (2)
end

to-report g2_ch
  report group-coherence (2)
end

to-report g3_ex
  report group-extremness (3)
end

to-report g3_dv
  report group-diversity (3)
end

to-report g3_ch
  report group-coherence (3)
end

to-report g4_ex
  report group-extremness (4)
end

to-report g4_dv
  report group-diversity (4)
end

to-report g4_ch
  report group-coherence (4)
end

to-report g5_ex
  report group-extremness (5)
end

to-report g5_dv
  report group-diversity (5)
end

to-report g5_ch
  report group-coherence (5)
end

to-report g6_ex
  report group-extremness (6)
end

to-report g6_dv
  report group-diversity (6)
end

to-report g6_ch
  report group-coherence (6)
end

to-report g7_ex
  report group-extremness (7)
end

to-report g7_dv
  report group-diversity (7)
end

to-report g7_ch
  report group-coherence (7)
end

to-report g8_ex
  report group-extremness (8)
end

to-report g8_dv
  report group-diversity (8)
end

to-report g8_ch
  report group-coherence (8)
end

to-report g9_ex
  report group-extremness (9)
end

to-report g9_dv
  report group-diversity (9)
end

to-report g9_ch
  report group-coherence (9)
end

to-report g10_ex
  report group-extremness (10)
end

to-report g10_dv
  report group-diversity (10)
end

to-report g10_ch
  report group-coherence (10)
end

to-report g11_ex
  report group-extremness (11)
end

to-report g11_dv
  report group-diversity (11)
end

to-report g11_ch
  report group-coherence (11)
end

to-report g12_ex
  report group-extremness (12)
end

to-report g12_dv
  report group-diversity (12)
end

to-report g12_ch
  report group-coherence (12)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  VISUALISATIONS & GRAPHS      ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to __VIS+GRAPH_procedures end

to visualize
  if not visualisations? [stop]
  set-current-plot "attitude space 2d"
  clear-plot
  set-plot-background-color 0
  ask turtles [
    set-plot-pen-color (group * 20) + 5
    plotxy (some_fuzz + item (x_belief - 1) belief_vector) (some_fuzz + item (y_belief  - 1) belief_vector)
  ]

  set-current-plot "attitude space 1d"
  clear-plot
  set-plot-background-color 0
  ask turtles [
    set-plot-pen-color (group * 20) + 5
    plotxy (some_fuzz + group / max [group] of turtles) (some_fuzz + item (belief_shown  - 1) belief_vector)
  ]

  set-current-plot "topics"
  clear-plot
  set-plot-background-color 0
  ask ifelse-value (only_group_shown) [turtles with [group = group_shown]] [turtles] [
    set-plot-pen-color (group * 20) + 5
    (foreach (n-values length belief_vector [ i -> (i + 0.5) / length belief_vector ]) (belief_vector) [ [x y] -> plotxy x + some_fuzz y])
    ; plotxy (some_fuzz + group / max [group] of turtles) (some_fuzz + item (y_belief  - 1) belief_vector)
  ]

  set-current-plot "Attitude Dynamics"
  let ts no-turtles
  ifelse group_shown < 1
    [set ts turtles]
    [set ts turtles with [group = group_shown]]
  ask ts [
    set-plot-pen-color (group * 20) + 5
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

to-report logistic [vl steepness-k]
  ;; the logistic function
  report 1 / ( 1 + exp (- steepness-k * vl))
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
654
517
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
1

SLIDER
4
241
142
274
neis
neis
1
50
10.0
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
0.1
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
10.0
1
1
NIL
HORIZONTAL

INPUTBOX
658
10
820
82
network_file
network.txt
1
0
String

CHOOSER
4
399
145
444
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
DE/itemsCCA.csv
1
0
String

INPUTBOX
977
10
1126
70
corol_mat_file
DE/correlationsCCA.csv
1
0
String

SLIDER
807
178
975
211
conformity_tendency
conformity_tendency
0
1
0.2
0.05
1
NIL
HORIZONTAL

SLIDER
807
213
976
246
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
806
248
976
281
prob_soc_infl
prob_soc_infl
0
1
0.5
0.05
1
NIL
HORIZONTAL

SLIDER
806
283
974
316
prob_self_check
prob_self_check
0
1
0.5
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
0.0
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
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
903
663
1013
696
x_belief
x_belief
1
5
2.0
1
1
NIL
HORIZONTAL

SLIDER
1015
663
1126
696
y_belief
y_belief
1
5
5.0
1
1
NIL
HORIZONTAL

PLOT
785
698
1128
1033
attitude space 2d
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
816
154
962
172
Process Parameters
11
0.0
1

TEXTBOX
5
381
155
399
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
20.0
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
662
898
695
fuzz
fuzz
0
0.025
0.025
0.001
1
NIL
HORIZONTAL

INPUTBOX
911
75
978
135
k
4.0
1
0
Number

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
10.0
1
0
Number

MONITOR
659
82
709
127
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
2.0
1
1
NIL
HORIZONTAL

PLOT
8
1038
1128
1409
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
0
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
2.0
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
0.3
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
712
82
764
127
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
200.0
1
0
Number

SLIDER
977
72
1126
105
link_health_ch
link_health_ch
0
1
0.3
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

INPUTBOX
830
73
896
133
k_link
-1.0
1
0
Number

TEXTBOX
833
135
983
153
k_link<0!!!
12
0.0
1

PLOT
1193
161
1436
310
All agents: Output Measures
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"extremness" 1.0 0 -16777216 true "" "plot extremness"
"diversity" 1.0 0 -7500403 true "" "plot diversity"
"coherence" 1.0 0 -2674135 true "" "plot mean [agent-coherence] of turtles"

PLOT
1193
310
1436
460
Group 1 Output Measures
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"g1_ex" 1.0 0 -16777216 true "" "plot g1_ex"
"g1_dv" 1.0 0 -7500403 true "" "plot g1_dv"
"g1_ch" 1.0 0 -2674135 true "" "plot g1_ch"

BUTTON
75
43
142
77
NIL
visualize
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
9
696
417
1031
attitude space 1d
group
belief_shown
0.05
1.05
-1.05
1.05
false
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
426
698
777
1032
topics
topic
belief
0.0
1.05
-1.05
1.05
false
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
1148
10
1438
160
extremeness topics
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"topic1" 1.0 0 -16777216 true "" "plot mean [abs item 0 belief_vector] of turtles"
"topic2" 1.0 0 -2674135 true "" "plot mean [abs item 1 belief_vector] of turtles"
"topic3" 1.0 0 -6459832 true "" "plot mean [abs item 2 belief_vector] of turtles"
"topic4" 1.0 0 -10899396 true "" "plot mean [abs item 3 belief_vector] of turtles"
"topic5" 1.0 0 -14835848 true "" "plot mean [abs item 4 belief_vector] of turtles"

PLOT
1438
10
1728
160
sd topics
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"topic1" 1.0 0 -16777216 true "" "plot standard-deviation [item 0 belief_vector] of turtles"
"topic2" 1.0 0 -2674135 true "" "plot standard-deviation [item 1 belief_vector] of turtles"
"topic3" 1.0 0 -6459832 true "" "plot standard-deviation [item 2 belief_vector] of turtles"
"topic4" 1.0 0 -8732573 true "" "plot standard-deviation [item 3 belief_vector] of turtles"
"topic5" 1.0 0 -14835848 true "" "plot standard-deviation [item 4 belief_vector] of turtles"

SWITCH
531
654
718
687
only_group_shown
only_group_shown
0
1
-1000

MONITOR
9
651
67
696
topic
belief_shown
17
1
11

MONITOR
718
653
776
698
group
ifelse-value (only_group_shown) [group_shown] [\"all\"]
17
1
11

PLOT
1193
460
1436
610
Group 2 Output Measures
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"g2_ex" 1.0 0 -16777216 true "" "plot g2_ex"
"g2_dv" 1.0 0 -7500403 true "" "plot g2_dv"
"g2_ch" 1.0 0 -2674135 true "" "plot g2_ch"

PLOT
1193
610
1436
760
Group 3 Output Measures
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"g3_ex" 1.0 0 -16777216 true "" "plot g3_ex"
"g3_dv" 1.0 0 -7500403 true "" "plot g3_dv"
"g3_ch" 1.0 0 -2674135 true "" "plot g3_ch"

PLOT
1437
161
1680
311
Group 4 Output Measures
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"g4_ex" 1.0 0 -16777216 true "" "plot g4_ex"
"g4_dv" 1.0 0 -7500403 true "" "plot g4_dv"
"g4_ch" 1.0 0 -2674135 true "" "plot g4_ch"

PLOT
1437
310
1680
460
Group 5 Output Measures
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"g5_ex" 1.0 0 -16777216 true "" "plot g5_ex"
"g5_dv" 1.0 0 -7500403 true "" "plot g5_dv"
"g5_ch" 1.0 0 -2674135 true "" "plot g5_ch"

PLOT
1437
460
1680
610
Group 6 Output Measures
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"g6_ex" 1.0 0 -16777216 true "" "plot g6_ex"
"g6_dv" 1.0 0 -7500403 true "" "plot g6_dv"
"g6_ch" 1.0 0 -2674135 true "" "plot g6_ch"

PLOT
1437
610
1680
760
Group 7 Output Measures
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"g7_ex" 1.0 0 -16777216 true "" "plot g7_ex"
"g7_dv" 1.0 0 -7500403 true "" "plot g7_dv"
"g7_ch" 1.0 0 -2674135 true "" "plot g7_ch"

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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment-dynamicNetwork" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>ticks</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="19"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-20"/>
      <value value="-100"/>
      <value value="-10"/>
      <value value="-1"/>
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-staticNetwork" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>ticks</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="19"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="conformity-staticNetwork" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>ticks</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="variability-staticNetwork" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>ticks</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="socInfluence-staticNetwork" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>ticks</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="selfCheck-staticNetwork" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>ticks</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="conformity-staticNetwork-groups" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="100"/>
      <value value="20"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="variability-staticNetwork-groups" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="socInfluence-staticNetwork-groups" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="selfCheck-staticNetwork-groups" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="socInfluence-staticNetwork-groups_WIDE" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.75"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="selfCheck-staticNetwork-groups_WIDE" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.75"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="kHunt-staticNetwork-groups" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="100"/>
      <value value="90"/>
      <value value="81"/>
      <value value="74"/>
      <value value="67"/>
      <value value="60"/>
      <value value="55"/>
      <value value="49"/>
      <value value="45"/>
      <value value="40"/>
      <value value="37"/>
      <value value="33"/>
      <value value="30"/>
      <value value="27"/>
      <value value="25"/>
      <value value="22"/>
      <value value="20"/>
      <value value="18"/>
      <value value="16"/>
      <value value="15"/>
      <value value="13"/>
      <value value="12"/>
      <value value="11"/>
      <value value="10"/>
      <value value="9"/>
      <value value="8"/>
      <value value="7"/>
      <value value="6"/>
      <value value="5"/>
      <value value="4"/>
      <value value="3"/>
      <value value="2"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="kHunt-staticNetwork-groups_MORE" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="6" step="1" last="30"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="100"/>
      <value value="90"/>
      <value value="81"/>
      <value value="74"/>
      <value value="67"/>
      <value value="60"/>
      <value value="55"/>
      <value value="49"/>
      <value value="45"/>
      <value value="40"/>
      <value value="37"/>
      <value value="33"/>
      <value value="30"/>
      <value value="27"/>
      <value value="25"/>
      <value value="22"/>
      <value value="20"/>
      <value value="18"/>
      <value value="16"/>
      <value value="15"/>
      <value value="13"/>
      <value value="12"/>
      <value value="11"/>
      <value value="10"/>
      <value value="9"/>
      <value value="8"/>
      <value value="7"/>
      <value value="6"/>
      <value value="5"/>
      <value value="4"/>
      <value value="3"/>
      <value value="2"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="conformity-staticNetwork-groups_WIDE" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="100"/>
      <value value="20"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.5"/>
      <value value="0.7"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="variability-staticNetwork-groups_WIDE" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="20"/>
      <value value="100"/>
      <value value="10"/>
      <value value="1"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="conformityVariabilityInteraction-staticNetwork-groups_WIDE_RS01-05" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="33"/>
      <value value="30"/>
      <value value="27"/>
      <value value="25"/>
      <value value="22"/>
      <value value="20"/>
      <value value="18"/>
      <value value="16"/>
      <value value="15"/>
      <value value="13"/>
      <value value="12"/>
      <value value="11"/>
      <value value="10"/>
      <value value="9"/>
      <value value="8"/>
      <value value="7"/>
      <value value="6"/>
      <value value="5"/>
      <value value="4"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.7"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="conformityVariabilityInteraction-staticNetwork-groups_WIDE_RS06-10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="201"/>
    <metric>ticks</metric>
    <metric>extremness</metric>
    <metric>diversity</metric>
    <metric>g1_ex</metric>
    <metric>g1_dv</metric>
    <metric>g2_ex</metric>
    <metric>g2_dv</metric>
    <metric>g3_ex</metric>
    <metric>g3_dv</metric>
    <metric>g4_ex</metric>
    <metric>g4_dv</metric>
    <metric>g5_ex</metric>
    <metric>g5_dv</metric>
    <metric>g6_ex</metric>
    <metric>g6_dv</metric>
    <metric>g7_ex</metric>
    <metric>g7_dv</metric>
    <metric>g8_ex</metric>
    <metric>g8_dv</metric>
    <metric>g9_ex</metric>
    <metric>g9_dv</metric>
    <metric>g10_ex</metric>
    <metric>g10_dv</metric>
    <metric>g11_ex</metric>
    <metric>g11_dv</metric>
    <metric>g12_ex</metric>
    <metric>g12_dv</metric>
    <steppedValueSet variable="rand-seed" first="6" step="1" last="10"/>
    <enumeratedValueSet variable="max_time">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k_link">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="33"/>
      <value value="30"/>
      <value value="27"/>
      <value value="25"/>
      <value value="22"/>
      <value value="20"/>
      <value value="18"/>
      <value value="16"/>
      <value value="15"/>
      <value value="13"/>
      <value value="12"/>
      <value value="11"/>
      <value value="10"/>
      <value value="9"/>
      <value value="8"/>
      <value value="7"/>
      <value value="6"/>
      <value value="5"/>
      <value value="4"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity_tendency">
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.7"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_of_new_belief">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_soc_infl">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_self_check">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set_agents">
      <value value="&quot;From File&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corol_mat_file">
      <value value="&quot;DE/correlationsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_data">
      <value value="&quot;DE/itemsCCA.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_health_ch">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drop-bad-link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_add_link">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_FoF">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_lonely?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_num_links">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_repeat_link?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rearrange_every">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-old-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-new-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only_group_shown">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_type">
      <value value="&quot;Watts&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neis">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_degree">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering_exp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toroidial_world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief_shown">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x_belief">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="y_belief">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuzz">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group_shown">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="own_stamp">
      <value value="&quot;A00001&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_file">
      <value value="&quot;network.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="main_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_agents_filename">
      <value value="&quot;agent-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network_filename">
      <value value="&quot;network-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="storing_folder">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_metadata_filename">
      <value value="&quot;meta-data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use_random_stamp?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="store_network?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
