#### Script for analysis of experiment on interaction between conformity, k and variability
#### 

## Encoding: windows-1250
## Created:  2022-11-08 FrK
## Edited:   2022-11-08 FrK
## 

## Notes:
## 1) since there are 1280 combinations of conformity, variability and K (8 x 8 x 20),
##    I'm running only 5 runs per each combination (5 x 1280 = 6400 simulations),
##    so I want to be sure that 5 runs is enough ==> 
##    I firstly randomly pick several times 5 random runs out of 20 from previous analysis (simGrps.R) and
##    I will store the averages (for whole simulation, but also for groups) and
##    then I will plot these averages -- just to check whether 5 runs is save option.
##    
## 2) I will analyze interaction between conformity and belief variability -- 
##    these two factors revealed in previous analysis as making the highest difference
##    (conformity makes the biggest deifference, then the belief variability)
##         



# Head --------------------------------------------------------------------

# Clearing environment
rm(list = ls())

# Packages
library(tidyverse)



# 2) EXPERIMENT -----------------------------------------------------------

# ad 2:Loading and processing the data -----------------------------------------

df = read_csv('Experiments/conformityVariabilityInteraction-staticNetwork-groups_WIDE_RS01-05.csv', skip = 6)%>% 
  select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
         self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74)  %>% 
  add_row(
    read_csv('Experiments/conformityVariabilityInteraction-staticNetwork-groups_WIDE_RS06-10.csv', skip = 6)%>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74)
  ) %>% 
  
  # reshaping of data:
  rowid_to_column(var = 'ID') %>%   # we have to create sims' ID for later identification in long format of data
  pivot_longer(cols = ALL_ex:g12_dv) %>%  # now we reshape all 26 variables on 2 measures to one column
  separate(name, into = c('group', "measure")) %>%  # we separate from past variable names info on group and measure
  mutate(measure = recode(measure, ex = "extremness", dv = "diversity")) %>%  # renaming measures more intuitively 
  pivot_wider(id_cols = ID:group, names_from = measure) %>%  # and finally we separate measures from one column to two variables
  
  # final filtering of non needed observations:
  filter(!is.na(extremness), !is.na(diversity)) %>% 
  
  # final factoring:
  mutate(
    conformity = factor(conformity),
    belief_variability = factor(belief_variability) %>% fct_rev())



# Graphs ------------------------------------------------------------------

df %>% 
  group_by(k, conformity, belief_variability, group) %>% 
  summarise(across(.cols = c(extremness, diversity), ~mean(.x))) %>% 
  arrange(k) %>% 
  ggplot() +
  aes(x = extremness, y = diversity, group = group,
      col = group, alpha = log(k) / 6) +
  facet_grid(cols = vars(conformity), rows = vars(belief_variability)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_alpha_identity() +
  labs(title = "Correlation of 'Extremness' and 'Diversity' for each 'Group', by 'Conformity' and 'Belief variability'") +
  theme_light()

ggsave("Experiments/ExtVsDivByConfVsVarVsK.png", units = "cm", height = 84.9, width = 57)


df %>% 
  group_by(k, conformity, belief_variability, group) %>% 
  summarise(across(.cols = c(extremness, diversity), ~mean(.x))) %>% 
  arrange(k) %>% 
  # computing the difference for each K:
  ungroup() %>% group_by(k, conformity, belief_variability) %>% 
  mutate(ex_diff = max(extremness) - min(extremness), dv_diff = max(diversity) - min(diversity)) %>% 
  ggplot() +
  aes(y = extremness, x = k, group = group,
      col = group, alpha = (diversity + 0.14)) +
  facet_grid(cols = vars(conformity), rows = vars(belief_variability)) +
  geom_point() +
  geom_line() +
  geom_line(aes(y = ex_diff), col = "black") +
  geom_point(aes(y = ex_diff), col = "black") +
  scale_color_viridis_d() +
  scale_alpha_identity() +
  scale_x_log10() +
  labs(title = "Correlation of 'Extremness' and 'k' for each 'Group', by 'Conformity' and 'Belief variability'") +
  theme_light()

ggsave("Experiments/ExtVsKByConfVsVarVsGroup.png", units = "cm", height = 40, width = 40)


df %>% 
  group_by(k, conformity, belief_variability, group) %>% 
  summarise(across(.cols = c(extremness, diversity), ~mean(.x))) %>% 
  arrange(k) %>% 
  ggplot() +
  aes(x = k, y = diversity, group = group,
      col = group, alpha = extremness) +
  facet_grid(cols = vars(conformity), rows = vars(belief_variability)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_alpha_identity() +
  scale_x_log10() +
  labs(title = "Correlation of 'k' and 'Diversity' for each 'Group', by 'Conformity' and 'Belief variability'") +
  theme_light()

ggsave("Experiments/DivVsKByConfVsVarVsGroup.png", units = "cm", height = 20, width = 40)

max(df$extremness) - min(df$extremness)
max(df$diversity) - min(df$diversity)




# 1) TESTING WHETHER 5 SEEDS IS ENOUGH ------------------------------------

# ad 1: Loading and processing data ------------------------------------------------

# loading main data, selecting needed variables and joining data:
# We keep only simulation with baseline influence and self-check values,
# so we load only experiments with conformity and belief variability, others are not important.
dfc = read_csv("Experiments/conformity-staticNetwork-groups_2022-11-04.csv", skip = 6) %>% 
  select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
         self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(file = 1) %>%  
  add_row(
    read_csv("Experiments/variability-staticNetwork-groups_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(file = 3)) %>% 
  add_row(
    read_csv("Experiments/conformity-staticNetwork-groups_WIDE_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(file = 2)) %>% 
  add_row(
    read_csv("Experiments/variability-staticNetwork-groups_WIDE_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(file = 4)) %>% 
  filter(k>=1) %>% 
  select(-c(5, 6, 23:32)) %>% # We let only varibles needed for the check.
  relocate(file, .before = 1)




# ad 1: functions for subsampling 5 seeds out of 20 and averaging results per each parameters' combination ------------------------

# rsd = random_selected_df
rsd = function(.data = dfc, .rs = 1001, .n = 5) {
  set.seed(.rs)
  .rows = sample(1:20, .n)
  .data %>% 
    group_by(k, conformity, belief_variability) %>% 
    slice(.rows, .preserve = TRUE) %>%
    # reshaping of data:
    rowid_to_column(var = 'ID') %>%   # we have to create sims' ID for later identification in long format of data
    pivot_longer(cols = ALL_ex:g7_dv) %>%  # now we reshape all 26 variables on 2 measures to one column
    separate(name, into = c('group', "measure")) %>%  # we separate from past variable names info on group and measure
    mutate(measure = recode(measure, ex = "extremness", dv = "diversity")) %>%  # renaming measures more intuitively 
    pivot_wider(id_cols = ID:group, names_from = measure) %>%  # and finally we separate measures from one column to two variables
    group_by(k, conformity, belief_variability, group) %>% 
    summarise(extremness = mean(extremness), diversity = mean(diversity)) %>%
    ungroup() %>% ungroup() %>% ungroup() %>% 
    arrange(k, conformity, belief_variability, group) 
  # %>%  # Just for code testing purposes 
  #   mutate(r_1 = .rows[1], r_2 = .rows[2], r_3 = .rows[3], r_4 = .rows[4], r_5 = .rows[5])
}


# cs = compare_selects
cs = function(.dx = df1, .dy = df2, .facts = 1:4, .vars = 5:6) {
  .match = .dx[, .facts] != .dy[, .facts]
  if(sum(.match) != 0) {
    stop("Some combinations do not match!")
    } else {
      .df = cbind(.dx[, .facts], abs(.dx[, .vars] - .dy[, .vars]))
    }
  tibble(.df)
}


# ac = add comparisons
ac = function(.dx = df1, .dy = df2, .facts = 1:4, .vars = 5:6) {
  .match = .dx[, .facts] != .dy[, .facts]
  if(sum(.match) != 0) {
    stop("Some combinations do not match!")
  } else {
    .df = cbind(.dx[, .facts], (.dx[, .vars] + .dy[, .vars]))
  }
  tibble(.df)
}


# amc = accumulate many comparisons
amc = function(.seeds = 1001:1009, .including_whole_sample = T) {
  l = length(.seeds)
  for (i in 1:(l-1)){
    for (j in (i+1):(l)){
      .temp = cs(rsd(.rs = .seeds[i]), rsd(.rs = .seeds[j]))
      if (i == 1 & j == 2) .df = .temp else .df = ac(.df, .temp)
    }
    if (.including_whole_sample) .df = ac(.df,  cs(rsd(.rs = .seeds[i]), rsd(.n = 20)))
  }
  .df
}


# aci = accumulate comparisons individually
aci = function(.seeds = 1001:1009, .including_whole_sample = T) {
  l = length(.seeds)
  for (i in 1:(l-1)){
    for (j in (i+1):(l)){
      .temp = cs(rsd(.rs = .seeds[i]), rsd(.rs = .seeds[j]))
      if (i == 1 & j == 2) .df = .temp else .df = .df %>% add_row(.temp)
      .df = mutate(.df, rs1 = .seeds[i], rs2 = .seeds[j]) 
    }
    if (.including_whole_sample) .df = .df %>% add_row(cs(rsd(.rs = .seeds[i]), rsd(.n = 20)))
  }
  .df
}




# ad 1: Analysis ----------------------------------------------------------

## Sensitivity to size of sub-sample
# Preparing 'ss' data-object
ss = tibble(n = NA_real_, rs = NA_integer_, max = NA_real_)
for (i in 1:15) {
  for (r in 1001:1100){
    ss = ss %>% 
      add_row(
        cs(rsd(.n = i, .rs = r), rsd(.n = 20))%>%
        pivot_longer(cols = 5:6) %>% 
        summarise(max = max(value)) %>% 
        mutate(n = i, rs = r)
      )
  }
}  
  
# Drawing a graph
ssa = filter(ss, !is.na(n)) %>%
  group_by(n) %>% 
  summarise(mean = mean(max), min = min(max), max = max(max))

filter(ss, !is.na(n)) %>% 
  ggplot() +
  aes(x = n, y = max) +
  geom_jitter(aes(col = factor(n)), show.legend = F, height = 0, width = 0.2) +
  geom_point(data = ssa, aes(y = mean), size = 10, show.legend = F, alpha = 0.35) +
  geom_line(data = ssa, aes(y = mean)) +
  geom_line(data = ssa, aes(y = min)) +
  geom_line(data = ssa, aes(y = max)) +
  labs(title = "Maximal differences according size of sub-sample",
       subtitle = "One point represents one comparison --\nwe take the highest difference from 960 comparisons between sub-sample and full sample",
       y = "Maximal difference found between sub-sample and full sample", x = "Sub-sample size") +
  theme_light()
ggsave("Experiments/subsampleSizeEffect.png", units = "cm", width = 24, height = 12)



## Individual differences between subsamples
da = aci(.seeds = 101:110) %>% 
  pivot_longer(cols = 5:6) %>% 
  filter(value > 0.000000001) 

da %>% filter(value > 0.0000001) %>% 
  # summarise(max = min(value)) 
  ggplot()+
  aes(fill = name, x = value, y = group, col = group)+
  geom_boxplot(alpha = 0.3) +
  geom_jitter(alpha = 0.05, position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(y = "", title = "All diferences produced for each combination of parameters for each pair of sub-samples",
       fill = "Variable:", color = "Group:") +
  theme_light()

ggsave("Experiments/subsamplesIndividualDifferences.png", units = "cm", width = 20, height = 20)


