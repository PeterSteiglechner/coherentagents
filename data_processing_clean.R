#### Script for reading, cleaning and preparing data for other phases of project of Group 12

# cleaning the environment:
rm(list=ls())
# Loading packages:
library(corrplot) 
library(tidyverse)
library(RCA)
library(igraph)

# Data Preparation

## Loading 
raw = read_csv('ESS9e03_1.csv') # Use this to select other context variables later

## Transform attitude items and calculate correlations
dffull <-  raw %>% 
  # Filtering the cases -- cases with missing values on believes variables deleted:
  filter(freehms <= 5, gincdif <= 5, impcntr <= 4, 
         lrscale <= 10, euftf <= 10)%>%
  mutate(
    # Scaling data to values within [-1, 1]. 
    # v' = -1 + 2 * (v-m)/(M-m), where m is the lowest, M the highest possible answer
    across(c(freehms, gincdif), ~ -1 + 2 * (.x - 1)/(5-1)), 
    across(c(lrscale, euftf), ~ -1 + 2 * (.x - 0)/(10-0)), 
    impcntr = -1 + 2* (impcntr - 1)/(4-1),
    # Flipping some scales: some questions are asked in a "negative" sense: 
    # e.g. -1 --> "more immigrants" and 1 --> "less immigrants"
    across(c(freehms, gincdif, impcntr), ~ .x * -1)
  )

appendRCAgroups <- function(df, attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf"), country_name) { 
  x_five <- df |> filter(cntry==country_name) |> select(attitudenames) |> RCA()
  df |> filter(cntry==country_name) |> select(idno, attitudenames)  |> mutate(group = x_five$membership)
}

checkGroups <- function (df, attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf")) {
  # marks all groups where sd is 0 for at least one variable
  std <- df |> group_by(group) |> summarise(across(attitudenames, .fns = sd))
  std$deviation <- std$freehms * std$gincdif * std$lrscale * std$impcntr * std$euftf
  std$cluster_exclusion <- ifelse(std$deviation == 0,T,F)
  df |> left_join(std[, c("cluster_exclusion", "group")], by="group")
}

computeCorrelationsPerGroup <- function(df, attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf")) {
  df <- df |> filter(cluster_exclusion==F)
  df_matrix <- tibble()
  for (i in unique(df$group)) {
    df_matrix_temp <- 
      df |> filter(group == i) |> select(attitudenames) |>  
      cor() |> as_tibble() |> 
      mutate(item = attitudenames, group = i)
    df_matrix <- bind_rows(df_matrix, df_matrix_temp)
   }
 df_matrix
}

writeForABM <- function(df, attitudenames, country_name) {
  items <- appendRCAgroups(df, attitudenames = attitudenames, country_name = country_name)
  items <- checkGroups(items)
  correlations <- computeCorrelationsPerGroup(items)
  if (!dir.exists(country_name)) {
    dir.create(country_name)
  } 
  write_csv(items, paste0(country_name,"/items.csv"))
  write_csv(correlations, paste0(country_name,"/correlations.csv"))
}

## Write out items.csv and correlations.csv for different countries
attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf")
writeForABM(dffull, attitudenames, country_name = "DE") # Takes time because of RCA!
writeForABM(dffull, attitudenames, country_name = "NL") # Takes time because of RCA!
writeForABM(dffull, attitudenames, country_name = "PL") # Takes time because of RCA!



# Visualization Scripts
items <- read_csv("PL/items.csv")
attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf")
## Item distributions general 
items |> pivot_longer(attitudenames) |> 
  ggplot(aes(value)) + geom_histogram(bins = 30) + facet_wrap(~name, nrow = 1) + theme_bw()
## Data Visualization of RCA Groups' item characteristics
### Groups' item distributions
items |> filter(!cluster_exclusion) |> pivot_longer(all_of(attitudenames)) |>
  ggplot(aes(value)) + geom_histogram(bins = 30) + facet_grid(name~group) + theme_bw()
### Explore the cross tabulation for one group and two items
heatmap2times <- function(items, grouplabel = 1, item1 = "freehms", item2 = "gincdif") {
  d <- items |> filter(group == grouplabel) |> select(all_of(c(item1, item2)))
  d |> group_by_all() |> 
    summarize(freq = n(), .groups = "drop") |> 
    ggplot(aes_string(item1, item2, fill = "freq", label = "freq")) + 
    geom_tile() + geom_text(color = "white") +
    labs(title = paste("Group",grouplabel, "Cor", round(cor(d[,1],d[,2]), digits = 3)))
}
heatmap2times(items, 1, "freehms", "gincdif")
heatmap2times(items, 5, "freehms", "gincdif")
heatmap2times(items, 5, "freehms", "impcntr")
heatmap2times(items, 1, "freehms", "impcntr")
heatmap2times(items, 5, "freehms", "lrscale")
heatmap2times(items, 5, "euftf", "lrscale")

## Jan: Other Correlation Exploration
library(correlation)
library(GGally)
items |> select(attitudenames) |> correlation()
items |> select(attitudenames) |> ggcorr()
items |> select(attitudenames) |> cor() |> corrplot(method='number')





#### THINGS TO DO LATER:

#PLOT:

#for (i in clust_num_list) {
# plot(x_five, module = i, heat_labels = T)
#}

