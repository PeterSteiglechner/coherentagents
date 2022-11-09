#### Script for processing results from sensitivity experiments in groups
#### For finding the most important factors

## Encoding: windows-1250
## Created:  2022-11-04 FrK
## Edited:   2022-11-08 FrK
## 


## NOTES:  
## We have to simulate results for each factor separately.
## In NetLogo we do for each factor 5x5 combinations (5 'k' values, 5 other factor's value) and
## for each combination we do 20 simulation (random seed 1:20)
## 


# Head --------------------------------------------------------------------

# Cleaning environment
rm(list=ls())

# Packages
library(tidyverse)
library(forcats)



# Loading and processing data ------------------------------------------------

# loading main data, selecting needed variables and joining data:
df = read_csv("Experiments/conformity-staticNetwork-groups_2022-11-04.csv", skip = 6) %>% 
  select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
         self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(factor = "conformity") %>% 
  add_row(
    read_csv("Experiments/variability-staticNetwork-groups_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(factor = "belief_variability")) %>% 
  add_row(
    read_csv("Experiments/socInfluence-staticNetwork-groups_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(factor = "social_influence")) %>% 
  add_row(
    read_csv("Experiments/selfCheck-staticNetwork-groups_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(factor = "self_check")) %>% 
  add_row(
    read_csv("Experiments/conformity-staticNetwork-groups_WIDE_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(factor = "conformity")) %>% 
  add_row(
    read_csv("Experiments/variability-staticNetwork-groups_WIDE_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(factor = "belief_variability")) %>% 
  add_row(
    read_csv("Experiments/socInfluence-staticNetwork-groups_WIDE_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(factor = "social_influence")) %>% 
  add_row(
    read_csv("Experiments/selfCheck-staticNetwork-groups_WIDE_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% mutate(factor = "self_check")) %>% 
  mutate(factor = factor(factor), rs = factor(rs)) %>% 
  
  # reshaping of data:
  rowid_to_column(var = 'ID') %>%   # we have to create sims' ID for later identification in long format of data
  pivot_longer(cols = ALL_ex:g12_dv) %>%  # now we reshape all 26 variables on 2 measures to one column
  separate(name, into = c('group', "measure")) %>%  # we separate from past variable names info on group and measure
  mutate(measure = recode(measure, ex = "extremness", dv = "diversity")) %>%  # renaming measures more intuitively 
  pivot_wider(id_cols = ID:group, names_from = measure) %>%  # and finally we separate measures from one column to two variables

  # final filtering of non needed observations:
  filter(!is.na(extremness), !is.na(diversity))


# Loading data for K-hunt:
kh = read_csv("Experiments/kHunt-staticNetwork-groups_2022-11-05.csv", skip = 6) %>% 
  select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
         self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74) %>% 
  add_row(read_csv("Experiments/kHunt-staticNetwork-groups_MORE_2022-11-05.csv", skip = 6) %>% 
            select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
                   self_check = 9, ALL_ex = 49, ALL_dv = 50, 51:74)) %>% 
  
  # reshaping of data:
  rowid_to_column(var = 'ID') %>%   # we have to create sims' ID for later identification in long format of data
  pivot_longer(cols = ALL_ex:g12_dv) %>%  # now we reshape all 26 variables on 2 measures to one column
  separate(name, into = c('group', "measure")) %>%  # we separate from past variable names info on group and measure
  mutate(measure = recode(measure, ex = "extremness", dv = "diversity")) %>%  # renaming measures more intuitively 
  pivot_wider(id_cols = ID:group, names_from = measure) %>%  # and finally we separate measures from one column to two variables
  
  # final filtering of non needed observations:
  filter(!is.na(extremness), !is.na(diversity)) %>% 
  
  # summarising according 'k' and 'group':
  group_by(k, group) %>% 
  summarise(extremness = mean(extremness), diversity = mean(diversity)) %>% 
  
  # computing the difference for each K:
  ungroup() %>% group_by(k) %>% 
  mutate(ex_diff = max(extremness) - min(extremness), dv_diff = max(diversity) - min(diversity))


# K-hunt graphs -----------------------------------------------------------

kh %>% 
  ggplot() +
  aes(x = k, y = extremness, col = group, group = group) +
  geom_point(size = 3, alpha = 0.4) +
  geom_line() +
  geom_line(aes(y = ex_diff), col = "black") +
  geom_point(aes(y = ex_diff), col = "black") +
  annotate('text', x = 9, y = 0.175, label = "This line tells us what's the margin between maximum and minimum 'extremness' for the respective 'k'.\nBut it also might be a hat or a snake who ate an elephant :-)") +
  scale_color_viridis_d() +
  scale_x_log10(breaks = c(1, 10, 20, 30, 50, 100)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_light()  +
  theme(legend.position = c(0.85, 0.64))

ggsave("Experiments/ExtVsK.png", units = "cm", width = 24, height = 20)

kh %>% 
  ggplot() +
  aes(x = k, y = diversity, col = group, group = group) +
  geom_point(size = 3, alpha = 0.4) +
  geom_line() +
  geom_line(aes(y = dv_diff), col = "black") +
  geom_point(aes(y = dv_diff), col = "black") +
  annotate('text', x = 10, y = -0.025, label = "Black line tells us what's the margin between maximum and minimum 'diversity' for the respective 'k'.") +
  scale_color_viridis_d() +
  scale_x_log10(breaks = c(1, 10, 20, 30, 50, 100)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_light()  +
  theme(legend.position = c(0.15, 0.65))

ggsave("Experiments/DivVsK.png", units = "cm", width = 24, height = 20)


kh %>% 
  ggplot() +
  aes(x = extremness, y = diversity, col = group, group = group, alpha = (k + 60) / 170) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_viridis_d() +
  scale_alpha_identity() +
  # lims(x = c(0, 1), y = c(0,1)) +
  annotate("text", x = .6, y = .8, label = "'k' is associated with alpha of points -- the darker point the higher 'k'") +
  labs(title = "'extremness' correlates with 'diversity' in nonlinear fashion -- relationship differs by 'group'") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_light() +
  theme(legend.position = c(0.05, 0.8))

ggsave("Experiments/ExtVsDivKhunt.png", units = "cm", width = 24, height = 20)


# functions 'heatmap of conformity' ---------------------------------------------

heat_factor = function(.data = df, .factor = "conformity", .fun = "mean", 
                       .var = "extremness", .name = "g0001.png", .width = 27) {
  .data %>% 
    filter(factor == .factor, group == "ALL") %>% 
    pivot_longer(cols = 4:7) %>% 
    filter(factor == name) %>% 
    group_by(k, value) %>% 
    summarise(heat = do.call(.fun, args = list(as.name(.var)))) %>% 
    ungroup() %>% 
    mutate(k = factor(k), value = factor(value)) %>% 
    ggplot() +
      aes(x = value, y = k, col = heat, label = round(heat, 4)) +
      geom_point(shape = 15, size = 41) +
      geom_text(color = "#000000") +
      scale_color_viridis_c() +
      labs(title = paste0(.fun, " of '", .var, "' by  '", .factor, "' and 'k' [static]"),
           x = .factor, color = .var) +
      theme_light() +
      theme(legend.position = "top")
  ggsave(.name, units = "cm", width = .width, height = 20)
}


heat_factor_groups = function(.data = df, .factor = "conformity", .fun = "mean", .width = 28, 
                              .var = "extremness", .name = "g0101.png", .all.position = 0) {
  .data %>% 
    filter(factor == .factor) %>% 
    pivot_longer(cols = 4:7) %>% 
    filter(factor == name) %>% 
    group_by(k, value, group) %>% 
    summarise(heat = do.call(.fun, args = list(as.name(.var)))) %>% 
    ungroup() %>% 
    mutate(k = factor(k), value = factor(value), 
      group = factor(group) %>% fct_relevel("ALL", after = .all.position)) %>%
    ggplot() +
    aes(x = value, y = k, col = heat, label = round(heat, 4)) +
    facet_wrap(vars(group), nrow = 3) +
    geom_point(shape = 15, size = 14) +
    geom_text(color = "#000000", size = 2.5) +
    scale_color_viridis_c() +
    labs(title = paste0(.fun, " of '", .var, "' by  'group', '", .factor, "' and 'k' [static]"),
         x = .factor, color = .var) +
    theme_light() +
    theme(legend.position = c(0.8, 0.15))
  ggsave(.name, units = "cm", width = .width, height = 20)
}


heat_factor_diff = function(.data = df, .factor = "conformity", .fun = "mean", .width = 28, 
                              .var = "extremness", .name = "g0201.png") {
  .data %>% 
    filter(factor == .factor) %>% 
    pivot_longer(cols = 4:7) %>% 
    filter(factor == name, group != "ALL") %>% 
    group_by(k, value, group) %>% 
    summarise(heat = do.call(.fun, args = list(as.name(.var)))) %>% 
    ungroup() %>% 
    mutate(k = factor(k), value = factor(value)) %>% 
    group_by(k, value) %>% 
    mutate(dif = max(heat) - min(heat)) %>%
    ggplot() +
    aes(x = value, y = k, col = dif, label = round(dif, 4)) +
    geom_point(shape = 15, size = 45) +
    geom_text(color = "#000000", size = 5) +
    scale_color_viridis_c() +
    labs(title = paste0("Groups' max difference in ", .fun, " of '", .var, "' by  '", .factor, "' and 'k' [static]"),
         x = .factor, color = .var) +
    theme_light() +
    theme(legend.position = "top")
  ggsave(.name, units = "cm", width = .width, height = 20)
}


# Diversity and Extremeness -----------------------------------------------

df %>% 
  ggplot() +
  aes(x = extremness, y = diversity, col = group) +
  geom_point(size = 3, alpha = 0.2) +
  scale_color_viridis_d() +
  lims(x = c(0, 1), y = c(0,1)) +
  labs(title = "'extremness' allows 'diversity': 'diversity' is never higher than 'extremness'") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) +
  theme_light() +
  theme(legend.position = c(0.075, 0.7))

ggsave("Experiments/ExtVsDiv.png", units = "cm", width = 20, height = 20)


df %>% 
  ggplot() +
  aes(x = extremness, y = diversity, col = group) +
  facet_wrap(vars(factor)) +
  geom_point(size = 3, alpha = 0.2) +
  scale_color_viridis_d() +
  # lims(x = c(0, 1), y = c(0,1)) +
  labs(title = "'diversity' is never higher than 'extremness' [even by manipulated factors]") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, ))) +
  theme_light() +
  theme(legend.position = c(0.05, 0.8))

ggsave("Experiments/ExtVsDivByfactor.png", units = "cm", width = 20, height = 20)


df %>% filter(k>1) %>% mutate(k = factor(k) %>% fct_rev()) %>% 
  ggplot() +
  aes(x = extremness, y = diversity, col = group) +
  facet_grid(rows = vars(k), cols = vars(factor)) +
  geom_point(size = 3, alpha = 0.2) +
  scale_color_viridis_d() +
  # lims(x = c(0, 1), y = c(0,1)) +
  labs(title = "'diversity' is never higher than 'extremness' [even by 'factor' & 'k']") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) +
  theme_light() +
  theme(legend.position = c(0.05, 0.74))

ggsave("Experiments/ExtVsDivByFactorAndK.png", units = "cm", width = 28, height = 21)


df %>% filter(k>1) %>% mutate(k = factor(k) %>% fct_rev()) %>% 
  ggplot() +
  aes(x = extremness, y = diversity, col = factor) +
  facet_grid(rows = vars(k), cols = vars(group), scales = "fixed") +
  geom_point(size = 3, alpha = 0.2) +
  scale_color_viridis_d() +
  # lims(x = c(0, 1), y = c(0,1)) +
  labs(title = "'diversity' is never higher than 'extremness' [even by 'group' & 'k']") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) +
  theme_light() +
  theme(legend.position = c(0.035, 0.86))

ggsave("Experiments/ExtVsDivByGroupAndK.png", units = "cm", width = 56, height = 21)


df %>% filter(k>=1) %>% mutate(k = factor(k) %>% fct_rev()) %>% 
  ggplot() +
  aes(x = extremness, y = k) +
  geom_boxplot(alpha = 0.3, show.legend = F, col = "grey50") +
  geom_jitter(aes(col = factor), alpha = 0.1) +
  scale_color_viridis_d() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 5))) +
  theme_light()+
  theme(legend.position = c(0.10, 0.15))
  
ggsave("Experiments/ExtVsKByFactor.png", units = "cm", width = 32, height = 16)


df %>% filter(k>=1) %>% mutate(k = factor(k) %>% fct_rev()) %>% 
  ggplot() +
  aes(x = diversity, y = k) +
  geom_boxplot(alpha = 0.3, show.legend = F, col = "grey50") +
  geom_jitter(aes(col = factor), alpha = 0.1) +
  scale_color_viridis_d() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 5))) +
  theme_light()+
  theme(legend.position = c(0.10, 0.15))

ggsave("Experiments/DivVsKByFactor.png", units = "cm", width = 32, height = 16)


df %>% filter(k>=1) %>% mutate(k = factor(k) %>% fct_rev()) %>% 
  ggplot() +
  aes(x = extremness, y = k) +
  geom_boxplot(alpha = 0.3, show.legend = F, col = "grey50") +
  geom_jitter(aes(col = group), alpha = 0.1) +
  scale_color_viridis_d() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 5))) +
  theme_light()+
  theme(legend.position = c(0.05, 0.25))

ggsave("Experiments/ExtVsKByGroup.png", units = "cm", width = 32, height = 16)


df %>% filter(k>=1, group != "ALL") %>% mutate(k = factor(k) %>% fct_rev()) %>% 
  ggplot() +
  aes(x = diversity, y = k) +
  geom_boxplot(alpha = 0.3, show.legend = F, col = "grey50") +
  geom_jitter(aes(col = group), alpha = 0.1) +
  scale_color_viridis_d() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 5))) +
  theme_light()+
  theme(legend.position = c(0.05, 0.25))

ggsave("Experiments/DivVsKByGroup.png", units = "cm", width = 32, height = 16)


# Heat maps for groups---------------------------------------------------------------
# Means
heat_factor_groups(.factor = "conformity", .fun = "mean", .var = "extremness", .all.position = 4, .name = "Experiments/g0101.png")
heat_factor_groups(.factor = "conformity", .fun = "mean", .var = "diversity", .all.position = 4, .name = "Experiments/g0105.png")

heat_factor_groups(.factor = "belief_variability", .fun = "mean", .var = "extremness", .all.position = 4, .name = "Experiments/g0102.png")
heat_factor_groups(.factor = "belief_variability", .fun = "mean", .var = "diversity", .all.position = 4, .name = "Experiments/g0106.png")

heat_factor_groups(.factor = "social_influence", .fun = "mean", .var = "extremness", .all.position = 4, .name = "Experiments/g0103.png", .width = 30)
heat_factor_groups(.factor = "social_influence", .fun = "mean", .var = "diversity", .all.position = 4, .name = "Experiments/g0107.png", .width = 30)

heat_factor_groups(.factor = "self_check", .fun = "mean", .var = "extremness", .all.position = 4, .name = "Experiments/g0104.png", .width = 30)
heat_factor_groups(.factor = "self_check", .fun = "mean", .var = "diversity", .all.position = 4, .name = "Experiments/g0108.png", .width = 30)

# SD
heat_factor_groups(.factor = "conformity", .fun = "sd", .var = "extremness", .all.position = 4, .name = "Experiments/g0111.png")
heat_factor_groups(.factor = "conformity", .fun = "sd", .var = "diversity", .all.position = 4, .name = "Experiments/g0115.png")

heat_factor_groups(.factor = "belief_variability", .fun = "sd", .var = "extremness", .all.position = 4, .name = "Experiments/g0112.png")
heat_factor_groups(.factor = "belief_variability", .fun = "sd", .var = "diversity", .all.position = 4, .name = "Experiments/g0116.png")

heat_factor_groups(.factor = "social_influence", .fun = "sd", .var = "extremness", .all.position = 4, .name = "Experiments/g0113.png", .width = 30)
heat_factor_groups(.factor = "social_influence", .fun = "sd", .var = "diversity", .all.position = 4, .name = "Experiments/g0117.png", .width = 30)

heat_factor_groups(.factor = "self_check", .fun = "sd", .var = "extremness", .all.position = 4, .name = "Experiments/g0114.png", .width = 30)
heat_factor_groups(.factor = "self_check", .fun = "sd", .var = "diversity", .all.position = 4, .name = "Experiments/g0118.png", .width = 30)



# Heat maps ---------------------------------------------------------------
# Means
heat_factor(.factor = "conformity", .fun = "mean", .var = "extremness", .name = "Experiments/g0001.png")
heat_factor(.factor = "conformity", .fun = "mean", .var = "diversity", .name = "Experiments/g0005.png")

heat_factor(.factor = "belief_variability", .fun = "mean", .var = "extremness", .name = "Experiments/g0002.png")
heat_factor(.factor = "belief_variability", .fun = "mean", .var = "diversity", .name = "Experiments/g0006.png")

heat_factor(.factor = "social_influence", .fun = "mean", .var = "extremness", .name = "Experiments/g0003.png", .width = 30)
heat_factor(.factor = "social_influence", .fun = "mean", .var = "diversity", .name = "Experiments/g0007.png", .width = 30)

heat_factor(.factor = "self_check", .fun = "mean", .var = "extremness", .name = "Experiments/g0004.png", .width = 30)
heat_factor(.factor = "self_check", .fun = "mean", .var = "diversity", .name = "Experiments/g0008.png", .width = 30)

# SD
heat_factor(.factor = "conformity", .fun = "sd", .var = "extremness", .name = "Experiments/g0011.png")
heat_factor(.factor = "conformity", .fun = "sd", .var = "diversity", .name = "Experiments/g0015.png")

heat_factor(.factor = "belief_variability", .fun = "sd", .var = "extremness", .name = "Experiments/g0012.png")
heat_factor(.factor = "belief_variability", .fun = "sd", .var = "diversity", .name = "Experiments/g0016.png")

heat_factor(.factor = "social_influence", .fun = "sd", .var = "extremness", .name = "Experiments/g0013.png", .width = 30)
heat_factor(.factor = "social_influence", .fun = "sd", .var = "diversity", .name = "Experiments/g0017.png", .width = 30)

heat_factor(.factor = "self_check", .fun = "sd", .var = "extremness", .name = "Experiments/g0014.png", .width = 30)
heat_factor(.factor = "self_check", .fun = "sd", .var = "diversity", .name = "Experiments/g0018.png", .width = 30)



# Heat maps of differences ---------------------------------------------------------------
# Means
heat_factor_diff(.factor = "conformity", .fun = "mean", .var = "extremness", .name = "Experiments/g0201.png")
heat_factor_diff(.factor = "conformity", .fun = "mean", .var = "diversity", .name = "Experiments/g0205.png")

heat_factor_diff(.factor = "belief_variability", .fun = "mean", .var = "extremness", .name = "Experiments/g0202.png")
heat_factor_diff(.factor = "belief_variability", .fun = "mean", .var = "diversity", .name = "Experiments/g0206.png")

heat_factor_diff(.factor = "social_influence", .fun = "mean", .var = "extremness", .name = "Experiments/g0203.png", .width = 30)
heat_factor_diff(.factor = "social_influence", .fun = "mean", .var = "diversity", .name = "Experiments/g0207.png", .width = 30)

heat_factor_diff(.factor = "self_check", .fun = "mean", .var = "extremness", .name = "Experiments/g0204.png", .width = 30)
heat_factor_diff(.factor = "self_check", .fun = "mean", .var = "diversity", .name = "Experiments/g0208.png", .width = 30)



# checking random seeds ---------------------------------------------------

df %>% 
  ggplot() +
  aes(x = rs, fill = rs) +
  geom_bar(show.legend = F) +
  geom_text(size = 2, aes(label = ..count..), stat = "count", vjust = +1.5) +
  labs(x = "Random seed") +
  theme_light()



# regression model --------------------------------------------------------

m1 = lm(extremness ~ log(k), 
        data = filter(df, group != "ALL"))
summary(m1)
m3 = lm(diversity ~ log(k), 
        data = filter(df, group != "ALL"))
summary(m3)
m4 = lm(diversity ~ log(k) + extremness , 
        data = filter(df, group != "ALL"))
summary(m4)
m2 = lm(extremness ~ log(k) + group + conformity + belief_variability + social_influence + self_check, 
        data = filter(df, group != "ALL"))
summary(m2)
m5 = lm(diversity ~ log(k) + extremness + group + conformity + belief_variability + social_influence + self_check, 
        data = filter(df, group != "ALL"))
summary(m5)
stargazer::stargazer(m1, m2, m3, m4, m5, type = "text", omit.stat = c("f", "ser"))




