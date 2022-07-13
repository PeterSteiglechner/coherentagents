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
library(abind)

# Loading and cleaning data -----------------------------------------------
raw = read_csv('ESS9e03_1.csv')

attitudenames = c("freehms", "gincdif", "lrscale", "impcntr", "euftf")

# Transform attitude items and calculate correlations -------------------------
# Scaling data to values within [-1, 1]. 
# v' = -1 + 2 * (v-m)/(M-m), where m is the lowest, M the highest possible answer
df <-  raw  %>% 
  filter(agea<=110)  %>% 
  dplyr::select(idno,cntry, all_of(attitudenames)) %>% 
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
  )

# Filter Data for a specific country
country = "DE"
df_country = df   |> 
  filter(cntry==country)

# group according to RCA:
columns = append(attitudenames, "idno", after=0)
df_country_bel <- df_country[columns]
rcagroups <- RCA(df_country_bel[attitudenames]) # alpha=?
# assign group membership as new column
df_country_bel$group <- rcagroups$membership

table(df_country_bel$group)

# For Germany: group 7 has two individuals only. Therefore, we remove this group.
df_country_bel_reduced <- df_country_bel |> 
  filter(group<=6)

# Calculate and plot correlation matrices
par(mfrow=c(2,3))
for (i in 1:6){
  matrix <- df_country_bel_reduced |> filter(group == i) |> 
  dplyr::select(attitudenames) |>  cor() 
  matrix |> corrplot(method='number',title=paste("RCAgroup", i), mar=c(0,0,1,0))
}


#########################################
####    Evaluate Goodness of Fit    #####
#########################################
groupname=3
group = df_country_bel_reduced |>  filter(group==groupname)
grouping_r <- group |> select(attitudenames)  |> cor()
n_group = nrow(group)
a = list()
for (i in 1:1000){
  sample_group <- df_country_bel_reduced[sample(nrow(df_country_bel_reduced), n_group), ]
  sample_r <- sample_group|> select(attitudenames)  |> cor()
  a[[i]] <- data.frame(sample_r)
}
all_rs <- abind(a, along = 3)
# now plot hist of all_rs[i,j,] and line in grouping[i,j]
# create list to access Dimensions in lapply
dims <- list(row = rep(1:nrow(all_rs),each = ncol(all_rs)),
             col = rep(1:ncol(all_rs),times = nrow(all_rs)))
xlimval=1.05
plots <- lapply(seq_along(dims$row),
                function(i){ 
                  if (dims$row[i]<dims$col[i]) {
                    randvalues = all_rs[dims$row[i],dims$col[i],]
                    randmean = mean(randvalues)
                    randsd = sd(randvalues)
                    group_r = grouping_r[dims$row[i],dims$col[i]]
                    if (abs(group_r-randmean) <= 2*randsd){col="red"}else{col="green"}
                    ggplot(data.frame(x = randvalues),aes(x = x)) +
                      geom_histogram(binwidth=0.01)+ 
                      xlim(-xlimval, xlimval) + 
                      geom_vline(xintercept=group_r, colour=col)+
                      theme_minimal()+
                      labs(title = paste(attitudenames[dims$row[i]],'-',attitudenames[dims$col[i]]))+
                      xlab(paste("r_", dims$row[i], dims$col[i], " = ", format(grouping_r[dims$row[i],dims$col[i]], digits=3), sep=""))+
                      ylab("")
                  }
                })
plot_grid(plotlist = plots, ncol = 5) 




# THINGS TO PLOT:
# plot(rcagroups, module = 1, heat_labels = T)
# plot(rcagroups, module = 2, heat_labels = T)
# plot(rcagroups, module = 3, heat_labels = T)
# plot(rcagroups, module = 4, heat_labels = T)
# plot(rcagroups, module = 5, heat_labels = T)
# plot(rcagroups, module = 6, heatmap=F, margin = 0.5, vertex_five_size = 40)
# plot(rcagroups, module = 2, heatmap=F, margin = 0.5, vertex_five_size = 40)
# summary(rcagroups)
# plot(rcagroups, module = 1, heatmap=F, margin = 0.5, vertex_five_size = 40, layout = layout.circle)
# plot(rcagroups, module = 2, heatmap=F, margin = 0.5, vertex_five_size = 40, layout = layout.circle)
# plot(rcagroups, module = 3, heatmap=F, margin = 0.5, vertex_five_size = 40, layout = layout.circle)
# plot(rcagroups, module = 4, heatmap=F, margin = 0.5, vertex_five_size = 40, layout = layout.circle)
# plot(rcagroups, module = 5, heatmap=F, margin = 0.5, vertex_five_size = 40, layout = layout.circle)
# plot(rcagroups, module = 6, heatmap=F, margin = 0.5, vertex_five_size = 40, layout = layout.circle)
# print(rcagroups)

# #beliefs
# ggplot(dfgRCA, aes(x=group, y=freehms)) + geom_boxplot()
# ggplot(dfgRCA, aes(x=group, y=gincdif)) + geom_boxplot()
# ggplot(dfgRCA, aes(x=group, y=impcntr)) + geom_boxplot()
# ggplot(dfgRCA, aes(x=group, y=lrscale)) + geom_boxplot()
# ggplot(dfgRCA, aes(x=group, y=euftf)) + geom_boxplot()

# df_bel |> mutate(group = rcagroups$membership) |> 
#   pivot_longer(attitudenames) |>
#   ggplot(aes(value)) + geom_histogram() + facet_wrap(name~group, ncol=9)

