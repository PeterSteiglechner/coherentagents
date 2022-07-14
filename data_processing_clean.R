#### Script for reading, cleaning and preparing data for other phases of project of Group 12

## Encoding: windows-1250
## Edited:   2022-07-11 FranCesko


## NOTES:
# Steps taken:
# 1) Filtering missing values for the five beliefs we're interested in 
# 2) Scaling values on a -1 to +1 scale
#  'rstatix' package, or 'sjmisc' package might help here (Jan would love 'sjmisc', it mimics STATA!)
#
# 3) scaling of the values and finding correlations is done
# 
# 4) Human values are transformed into Schwartz values 


# Header ------------------------------------------------------------------

# cleaning the environment:
rm(list=ls())

# Loading packages:
library(corrplot) 
library(tidyverse)
library(RCA)
library(igraph)

####BEFORE Pushing: remove dplyr before select

# Loading and cleaning data -----------------------------------------------
#raw = read_csv('ESS9e03_1.csv')
raw = read_csv("ESS9e03_1_complete.csv")


# Transform attitude items and calculate correlations -------------------------
# Scaling data to values within [-1, 1]. 
# v' = -1 + 2 * (v-m)/(M-m), where m is the lowest, M the highest possible answer
dffull <-  raw %>% 
  # Filtering the cases -- cases with missing values on believes variables deleted:
  filter(freehms <= 5, gincdif <= 5, impcntr <= 4, 
         lrscale <= 10, euftf <= 10)%>%
  mutate(
    across(c(freehms, gincdif), ~ -1 + 2 * (.x - 1)/(5-1)), 
    across(c(lrscale, euftf), ~ -1 + 2 * (.x - 0)/(10-0)), 
    impcntr = -1 + 2* (impcntr - 1)/(4-1)
  )
# Flipping some scales:
# some questions are asked in a "negative" sense: 
# e.g. -1 --> "more immigrants" and 1 --> "less immigrants"
# we flip the sign of these questions. 
dffull$freehms <- dffull$freehms * -1
dffull$gincdif <- dffull$gincdif * -1
dffull$impcntr <- dffull$impcntr * -1

#NOTE: Below, prtclede is country specific as well
dfsel <- dffull %>%
  dplyr::select(idno,cntry, prtclede, freehms, gincdif, lrscale, impcntr, euftf, ipcrtiv:impfun)

# filter out COUNTRY 
##(ADD variable name instead of DE here)


appendRCAgroups <- function(df, attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf"), country_name = "DE") { 
  x_five <- df |> filter(cntry==country_name) |> select(attitudenames) |> RCA()
  df |> filter(cntry==country_name) |> select(idno, attitudenames)  |> mutate(group = x_five$membership)
}

checkClusters <- function (df, attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf")) {
  std <- df |> group_by(group) |> summarise(across(attitudenames, .fns = sd))
  std$deviation <- std$freehms * std$gincdif * std$lrscale * std$impcntr * std$euftf
  std$cluster_exclusion <- ifelse(std$deviation == 0,T,F)
  df |> left_join(std[, c("cluster_exclusion", "group")], by="group")
}


computeCorrelationsPerGroup <- function(df, attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf")) {
  
  df <- df |> filter(cluster_exclusion==F)
  df_matrix <- data.frame()
  
  for (i in unique(df$group)) {
    df_matrix_temp <- 
      df |> filter(group == i) |> 
      dplyr::select(attitudenames) |>  
      cor() |> 
      as.data.frame() |> 
      mutate (item = attitudenames) |>
      mutate(group = i)
    
    df_matrix <- rbind(df_matrix, df_matrix_temp, make.row.names=F)
   }
 df_matrix 
}


writeForABM <- function(df,country_name) {
  items <- appendRCAgroups(df)
  items <- checkClusters(items)
  correlations <- computeCorrelationsPerGroup(items)
  if (!dir.exists(country_name)) {
    dir.create(country_name)
  } 
  write_csv(items, paste0(country_name,"/items.csv"))
  write_csv(correlations, paste0(country_name,"/correlations.csv"))
}



writeForABM(dffull, "DE")

#### THINGS TO DO LATER:

#PLOT:

#for (i in clust_num_list) {
# plot(x_five, module = i, heat_labels = T)
#}

