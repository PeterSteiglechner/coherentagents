#### Script for processing simulations' results
#### For comparison with Peter's Python results

## Encoding: windows-1250
## Created:  2022-11-04 FrK
## Edited:   2022-11-04 FrK
## 


## NOTES:  
## 


# Head --------------------------------------------------------------------

# Cleaning environment
rm(list=ls())

# Packages
library(tidyverse)



# Loading data ------------------------------------------------------------
# loading data and selecting needed variables:
df = read_csv("Experiments/dynamicNetwork_2022-11-04.csv", skip = 6) %>% 
  select(2, 4:5, 48:49) %>% 
  mutate(rs = (`rand-seed`))

sf = read_csv("Experiments/staticNetwork_2022-11-04.csv", skip = 6) %>% 
  select(2, 4:5, 48:49) %>% 
  mutate(rs = (`rand-seed`))

# Processing aggregated file ----------------------------------------------

dfa = df %>% 
  mutate(k_link = factor(-k_link), k = factor(k)) %>% 
  group_by(k_link, k) %>% 
  summarise(
    ex_mean = mean(extremness),
    ex_sd = sd(extremness),
    dv_mean = mean(diversity),
    dv_sd = sd(diversity)
  )

sfa = sf %>% 
  mutate(k_link = factor(-k_link), k = factor(k)) %>% 
  group_by(k_link, k) %>% 
  summarise(
    ex_mean = mean(extremness),
    ex_sd = sd(extremness),
    dv_mean = mean(diversity),
    dv_sd = sd(diversity)
  )

# Graphs ------------------------------------------------------------------

# ## Dynamic: -------------------------------------------------------------

# Extremness mean
dfa %>% 
  ggplot() +
  aes(x = k_link, y = k, col = ex_mean, label = round(ex_mean, 3)) +
  geom_point(shape = 15, size = 41) +
  geom_text(color = "#000000") +
  scale_color_gradient(low = "blue", high = "yellow") +
  labs(title = "Mean 'extremness' grows with 'k' and 'k_link' [dynamic]") +
  theme_light()

ggsave("g01.png", units = "cm", width = 20, height = 18)

# Extremness sd
dfa %>% 
  ggplot() +
  aes(x = k_link, y = k, col = ex_sd, label = round(ex_sd, 4)) +
  geom_point(shape = 15, size = 41) +
  geom_text(color = "#000000") +
  scale_color_gradient(low = "blue", high = "yellow") +
  labs(title = "SD of 'extremness' is tiny but leaps with 'k' and 'k_link' [dynamic]") +
  theme_light()

ggsave("g02.png", units = "cm", width = 20, height = 18)

# Diversity mean
dfa %>% 
  ggplot() +
  aes(x = k_link, y = k, col = dv_mean, label = round(dv_mean, 3)) +
  geom_point(shape = 15, size = 41) +
  geom_text(color = "#000000") +
  scale_color_gradient(low = "blue", high = "yellow") +
  labs(title = "Mean 'diversity' grows with 'k' and 'k_link' [dynamic]") +
  theme_light()

ggsave("g03.png", units = "cm", width = 20, height = 18)

# Diversity sd
dfa %>% 
  ggplot() +
  aes(x = k_link, y = k, col = dv_sd, label = round(dv_sd, 4)) +
  geom_point(shape = 15, size = 41) +
  geom_text(color = "#000000") +
  scale_color_gradient(low = "blue", high = "yellow") +
  labs(title = "SD of 'diversity' is tiny but leaps with 'k' and 'k_link' [dynamic]") +
  theme_light()

ggsave("g04.png", units = "cm", width = 20, height = 18)



# ## Static: --------------------------------------------------------------

# Extremness mean
sfa %>% 
  ggplot() +
  aes(x = k_link, y = k, col = ex_mean, label = round(ex_mean, 3)) +
  geom_point(shape = 15, size = 41) +
  geom_text(color = "#000000") +
  scale_color_gradient(low = "blue", high = "yellow") +
  labs(title = "Mean 'extremness' grows with 'k' and 'k_link' [static]") +
  theme_light()

ggsave("g11.png", units = "cm", width = 20, height = 18)

# Extremness sd
sfa %>% 
  ggplot() +
  aes(x = k_link, y = k, col = ex_sd, label = round(ex_sd, 4)) +
  geom_point(shape = 15, size = 41) +
  geom_text(color = "#000000") +
  scale_color_gradient(low = "blue", high = "yellow") +
  labs(title = "SD of 'extremness' is tiny but leaps with 'k' and 'k_link' [static]") +
  theme_light()

ggsave("g12.png", units = "cm", width = 20, height = 18)

# Diversity mean
sfa %>% 
  ggplot() +
  aes(x = k_link, y = k, col = dv_mean, label = round(dv_mean, 3)) +
  geom_point(shape = 15, size = 41) +
  geom_text(color = "#000000") +
  scale_color_gradient(low = "blue", high = "yellow") +
  labs(title = "Mean 'diversity' grows with 'k' and 'k_link' [static]") +
  theme_light()

ggsave("g13.png", units = "cm", width = 20, height = 18)

# Diversity sd
sfa %>% 
  ggplot() +
  aes(x = k_link, y = k, col = dv_sd, label = round(dv_sd, 4)) +
  geom_point(shape = 15, size = 41) +
  geom_text(color = "#000000") +
  scale_color_gradient(low = "blue", high = "yellow") +
  labs(title = "SD of 'diversity' is tiny but leaps with 'k' and 'k_link' [static]") +
  theme_light()

ggsave("g14.png", units = "cm", width = 20, height = 18)



# # Seeds: ----------------------------------------------------------------

df %>% 
  filter(rs < 20) %>% 
  mutate(rs = factor(rs)) %>% 
  group_by(rs) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  aes(x = rs, fill = rs, y = n, label = n) +
  geom_col(show.legend = F) +
  geom_text(vjust = +1.5) +
  labs(title = "Frequencies of random seeds are equal in dynamic experiment", x = "Random seeds", y = "Frequency") +
  theme_light()

sf %>% 
  filter(rs < 20) %>% 
  mutate(rs = factor(rs)) %>% 
  group_by(rs) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  aes(x = rs, fill = rs, y = n, label = n) +
  geom_col(show.legend = F) +
  geom_text(vjust = +1.5) +
  labs(title = "Frequencies of random seeds are equal in static experiment", x = "Random seeds", y = "Frequency") +
  theme_light()


