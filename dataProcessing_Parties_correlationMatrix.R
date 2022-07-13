# This file is a minimum version to extract the correlation matrix from RCA groups 
# 
# 1) load ESS data
# 2) transform the data and select only 5 attitudes


# cleaning the environment:
rm(list=ls())

# Loading packages:
library(readr)
library(tidyverse)
library(RCA)
library(dplyr)
library(corrplot)

# Loading and cleaning data -----------------------------------------------
raw = read_csv('ESS9e03_1.csv')

attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf")

# Transform attitude items and calculate correlations -------------------------
# Scaling data to values within [-1, 1]. 
# v' = -1 + 2 * (v-m)/(M-m), where m is the lowest, M the highest possible answer
df <-  raw  %>% 
  filter(agea<=110)  %>% 
  dplyr::select(idno,cntry,prtclede, all_of(attitudenames)) %>% 
  # Filtering the cases -- cases with missing values on believes variables deleted:
  filter(freehms <= 5, gincdif <= 5, impcntr <= 4, 
         lrscale <= 10, euftf <= 10)%>%
  # transform values from different scales to -1 .. 1 and flipping some of the scales
  mutate(
    across(c(freehms, gincdif), ~ -1 + 2 * (.x - 1)/(5-1)), 
    across(c(lrscale, euftf), ~ -1 + 2 * (.x - 0)/(10-0)), 
    impcntr = -1 + 2* (impcntr - 1)/(4-1)
  ) %>% 
  mutate(
    freehms = freehms * (-1),
    gincdif = gincdif * (-1),
    impcntr = impcntr * (-1)
  ) %>%
  filter(
    prtclede<10
  )
  

# Filter Data for a specific country
country = "DE"
df_country = df   |> 
  filter(cntry==country)


# group according to party:
table(df_country$prtclede)

# Calculate and plot correlation matrices
parties = c("CDUCSU", "SPD", "Left", "Green", "FDP", "AfD")
par(mfrow=c(1,length(parties)))
for (j in 1:length(parties)) {
  matrix <- df_country |> filter(prtclede==j) |> 
    dplyr::select(attitudenames) |>  cor() 
  matrix |> corrplot(method='number',title=paste(parties[j]), mar=c(0,0,1,0))
}


