#### Script for reading, cleaning and preparing data for other phases of project of Group 12

## Encoding: windows-1250
## Edited:   2022-07-11 FranCesko


## NOTES:
# 1) selection and filtering is done -- we have 26 vars (21 human values, 5 believes),
#  we have 2,126 observations, i.e. respondents which answer all our 26 questions!
#
# 2) generating of frequency table is not neat -- we might rewrite in more concise way,
#  'rstatix' package, or 'sjmisc' package might help here (Jan would love 'sjmisc', it mimics STATA!)
#
# 3) scaling of the values and finding correlations is done
# 
# 4) Human values are transformed into Schwartz values 


# Header ------------------------------------------------------------------

# cleaning the environment:
rm(list=ls())


# Loading packages:
library(readr)
library(tibble)
library(dplyr)
library(forcats)
library(writexl)
library(corrplot) 
library(ggplot2)
library(tidyverse)
library(RCA)
library(igraph)
library(ggplot2)

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

country_name = "DE"

dffull_g = dffull   |> 
  filter(cntry==country_name)
dfsel_g = dfsel |> 
  filter(cntry==country_name)

#Assigning groups of values to variables
attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf")
columns = append(attitudenames, "idno", after=0)

### RCA:

# NOTE: use dfsel_g for country-specific data or dffull for whole 

df_bel <- dfsel_g[columns]

#x <- RCA(df_bel, alpha = 0.01)
#print(x)
x_five <- RCA(df_bel[attitudenames])
df_bel$group <- x_five$membership

#Removing modules/clusters with too few members:

num_mod_og <- max(x_five$membership) #total number of modules/groups

table_mod <- as.data.frame(
  table(x_five$membership)
)

remove_var <- table_mod |>
  filter(Freq < 10) #Set required member size of a group

  
remove_list = as.numeric(remove_var$Var1)

remove_list

df_bel_filt <- df_bel[ ! df_bel$group %in% remove_list, ]


num_mod <- n_distinct(df_bel_filt$group) #total number of clustered in filtered version


#Plotting heat maps and network plots of the RCA 

clust_num_list <- unique(c(as.numeric(df_bel_filt$group))) #list of unique module/cluster numbers

for (i in clust_num_list) {
  plot(x_five, module = i, heat_labels = T)
}

for (i in clust_num_list) {
  plot(x_five, module = i, heatmap=F, margin = 0.5, vertex_five_size = 40, layout= layout.circle)
} #Don't know if we even need these plots really


# Trying to generate the correlational plot:
 
df_matrix <- data.frame()

for (i in clust_num_list) {
  df_matrix_temp <- 
    df_bel_filt |> filter(group == i) |> 
    dplyr::select(attitudenames) |>  
    cor() |> 
    as.data.frame() |> 
    mutate (item = attitudenames) |>
    mutate(group = i)
  
  df_matrix <- rbind(df_matrix, df_matrix_temp, make.row.names=F)
}

write.csv (df_matrix, 'Correlationmatrix_clean.csv')

