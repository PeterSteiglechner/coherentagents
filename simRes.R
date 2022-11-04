#### Script for processing results from sensitivity experiments
#### For finding the most important factors

## Encoding: windows-1250
## Created:  2022-11-04 FrK
## Edited:   2022-11-04 FrK
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



# Loading data ------------------------------------------------------------
# loading data, selecting needed variables and joining data:
df = read_csv("Experiments/conformity-staticNetwork_2022-11-04.csv", skip = 6) %>% 
  select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
         self_check = 9, 48:49) %>% mutate(factor = "conformity") %>% 
  add_row(
    read_csv("Experiments/variability-staticNetwork_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, 48:49) %>% mutate(factor = "belief_variability")) %>% 
  add_row(
    read_csv("Experiments/socInfluence-staticNetwork_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, 48:49) %>% mutate(factor = "social_influence")) %>% 
  add_row(
    read_csv("Experiments/selfCheck-staticNetwork_2022-11-04.csv", skip = 6) %>% 
      select(rs = 2, 5, conformity = 6, belief_variability = 7, social_influence = 8, 
             self_check = 9, 48:49) %>% mutate(factor = "self_check")) %>% 
  mutate(factor = factor(factor), rs = factor(rs))



# function 'heatmap of conformity' ---------------------------------------------

heat_factor = function(.data = df, .factor = "conformity", .fun = "mean", .var = "extremness", .name = "g0001.png") {
  .data %>% 
    filter(factor == .factor) %>% 
    pivot_longer(cols = 3:6) %>% 
    filter(factor == name) %>% 
    group_by(k, value) %>% 
    summarise(heat = do.call(.fun, args = list(as.name(.var)))) %>% 
    ungroup() %>% 
    mutate(k = factor(k), value = factor(value)) %>% 
    ggplot() +
      aes(x = value, y = k, col = heat, label = round(heat, 4)) +
      geom_point(shape = 15, size = 41) +
      geom_text(color = "#000000") +
      # scale_color_gradient(low = "blue", high = "yellow") +
      scale_color_viridis_c() +
      labs(title = paste0(.fun, " of '", .var, "' by  '", .factor, "' and 'k' [static]"),
           x = .factor, color = .var) +
      theme_light() +
      theme(legend.position = "top")
  ggsave(.name, units = "cm", width = 17.5, height = 20)
}


# Heat maps ---------------------------------------------------------------
# Means
heat_factor(.factor = "conformity", .fun = "mean", .var = "extremness", .name = "Experiments/g0001.png")
heat_factor(.factor = "conformity", .fun = "mean", .var = "diversity", .name = "Experiments/g0005.png")

heat_factor(.factor = "belief_variability", .fun = "mean", .var = "extremness", .name = "Experiments/g0002.png")
heat_factor(.factor = "belief_variability", .fun = "mean", .var = "diversity", .name = "Experiments/g0006.png")

heat_factor(.factor = "social_influence", .fun = "mean", .var = "extremness", .name = "Experiments/g0003.png")
heat_factor(.factor = "social_influence", .fun = "mean", .var = "diversity", .name = "Experiments/g0007.png")

heat_factor(.factor = "self_check", .fun = "mean", .var = "extremness", .name = "Experiments/g0004.png")
heat_factor(.factor = "self_check", .fun = "mean", .var = "diversity", .name = "Experiments/g0008.png")

# SD
heat_factor(.factor = "conformity", .fun = "sd", .var = "extremness", .name = "Experiments/g0011.png")
heat_factor(.factor = "conformity", .fun = "sd", .var = "diversity", .name = "Experiments/g0015.png")

heat_factor(.factor = "belief_variability", .fun = "sd", .var = "extremness", .name = "Experiments/g0012.png")
heat_factor(.factor = "belief_variability", .fun = "sd", .var = "diversity", .name = "Experiments/g0016.png")

heat_factor(.factor = "social_influence", .fun = "sd", .var = "extremness", .name = "Experiments/g0013.png")
heat_factor(.factor = "social_influence", .fun = "sd", .var = "diversity", .name = "Experiments/g0017.png")

heat_factor(.factor = "self_check", .fun = "sd", .var = "extremness", .name = "Experiments/g0014.png")
heat_factor(.factor = "self_check", .fun = "sd", .var = "diversity", .name = "Experiments/g0018.png")



# checking random seeds ---------------------------------------------------

df %>% 
  ggplot() +
  aes(x = rs, fill = rs) +
  geom_bar(show.legend = F) +
  geom_text(aes(label = ..count..), stat = "count", vjust = +1.5) +
  labs(x = "Random seed") +
  theme_light()



# regression model --------------------------------------------------------

m1 = lm(extremness ~ log(k) + conformity + belief_variability + social_influence + self_check, data = df)
summary(m1)
m2 = lm(diversity ~ log(k) + conformity + belief_variability + social_influence + self_check, data = df)
summary(m2)
stargazer::stargazer(m1, m2, type = "text")




