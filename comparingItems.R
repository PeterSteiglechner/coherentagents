#### Comparing old and new files "itemsCCA.csv"
#### one from folder "DE/", the second from "ns_DE/"

## Encoding: windows-1250
## Created:  2023-03-17 FranÈesko
## Edited:   2023-03-20 FranÈesko
 
## NOTES:
## 1) We read data in, join old and new file together and check files length
## 
## 2) Then we plot groups mapping in the old and new data file
## 



# Heading -----------------------------------------------------------------

## Clar all:
rm(list = ls())

## Needed packages:
library(tidyverse)



# Data in! ----------------------------------------------------------------

## Reading data in and joining files:
tb1 = read_csv("DE/itemsCCA.csv") %>% 
  mutate(group = recode(group, `1` = 4, `2` = 1, `3` = 2, `4` = 6, `5` = 7, `6` = 5, `7` = 3))
tb2 = read_csv("ns_DE/itemsCCA.csv") %>% select(-idno) %>% 
  mutate(group = recode(group, `1` = "1 (n=1040)", `2` = "2 (n=601)", `3` = "3 (n=562)"))
tb = right_join(tb1, tb2, by = c("freehms", "gincdif", "lrscale", "impcntr", "euftf")) %>% unique()


## Check of files length:
paste("Old and new files 'itemsCCA.csv' are of the",
      if_else(nrow(tb1) == nrow(tb2), "SAME", "DIFFERENT"), "length.") %>% print()
paste("Old file 'itemsCCA.csv' and joined file are of the",
      if_else(nrow(tb1) == nrow(tb), "SAME", "DIFFERENT"), "length.") %>% print()
paste("New file 'itemsCCA.csv' and joined file are of the",
      if_else(nrow(tb2) == nrow(tb), "SAME", "DIFFERENT"), "length.") %>% print()



# Drawing graph -----------------------------------------------------------

count(tb, group.x, group.y) %>% 
  ggplot(aes(x = group.x, y = group.y, size = n/5, label = n, col = factor(group.y))) + 
  geom_count(alpha = 0.15) +
  geom_text(size = 5) +
  scale_size_identity() +
  scale_x_continuous(breaks = 1:7, limits = c(0.5, 7.5)) +
  # scale_y_continuous(breaks = 1:3, limits = c(0.5, 3.5)) +
  guides(col = 'none') +
  labs(x = "7-CAA", y = "3-CAA") +
  theme_classic()

ggsave("groupsMapping.png", units = "px", width = 3000, height = 2000)

