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

valuenames <- c("ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff", "ipfrule", "ipudrst", 
                "ipmodst", "ipgdtim", "impfree", "iphlppl", "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp",
                "iprspot", "iplylfr", "impenv",  "imptrad", "impfun") 
# Transform attitude items and calculate correlations -------------------------
# Scaling data to values within [-1, 1]. 
# v' = -1 + 2 * (v-m)/(M-m), where m is the lowest, M the highest possible answer
df <-  raw  %>% 
  filter(agea<=110)  %>% 
  dplyr::select(idno,cntry, all_of(attitudenames), all_of(valuenames)) %>% 
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

# group according to values!
df_ten <- df |> dplyr::select(idno, ipcrtiv:impfun, cntry) |> #rowwise() |> 
  mutate(Conformity = (!!sym(valuenames[7]) + !!sym(valuenames[16])/2),
         Tradition = (!!sym(valuenames[9]) + !!sym(valuenames[20])/2),
         Benevolence = (!!sym(valuenames[12]) + !!sym(valuenames[18])/2),
         Universalism = (!!sym(valuenames[3]) + !!sym(valuenames[8]) + !!sym(valuenames[19])/3),
         SelfDirection = (!!sym(valuenames[1]) + !!sym(valuenames[11])/2),
         Stimulation = (!!sym(valuenames[6]) + !!sym(valuenames[15])/2),
         Hedonism = (!!sym(valuenames[10]) + !!sym(valuenames[21])/2),
         Achievement = (!!sym(valuenames[4]) + !!sym(valuenames[13])/2),
         Power = (!!sym(valuenames[2]) + !!sym(valuenames[17])/2),
         Security = (!!sym(valuenames[5]) + !!sym(valuenames[14])/2),
         mrat = (ipcrtiv + imprich + ipeqopt + ipshabt + impsafe + impdiff + ipfrule + ipudrst + 
                   ipmodst + ipgdtim + impfree + iphlppl + ipsuces + ipstrgv + ipadvnt + ipbhprp +
                   iprspot + iplylfr + impenv + imptrad + impfun)/21) |> 
  ungroup() |> 
  mutate(across(Conformity:Security, function(x) x - mrat)) |> 
  mutate(Openness = (SelfDirection + Stimulation)/2,
         SelfEnhancement = (Hedonism + Achievement + Power)/3,
         Conservation = (Security + Conformity + Tradition)/3,
         SelfTranscendence = (Benevolence + Universalism)/2)

df_four <- df_ten[sample(nrow(df_ten)),] |> 
  dplyr::select(idno, Openness:SelfTranscendence) |> 
  pivot_longer(Openness:SelfTranscendence) |> 
  group_by(idno) |> 
  mutate(rank = rank(value)) |> 
  group_by(idno) |> 
  summarize(Value1 = name[value == max(value)][1],
            howmanymaxequal= sum(value==max( value[value!=max(value)] )),
            Value2 = name[value == min(value)][1],
            Consistent =
              str_starts(Value1,"Self") & str_starts(Value2,"Self") |
              Value1 == "Openness" & Value2 == "Conservation" |
              Value2 == "Openness" & Value1 == "Conservation") |> 
  mutate(ValueType = if_else(Consistent, Value1, if_else(howmanymaxequal<2, "Erratic", "2max"))) 


df <- df |> left_join(df_four)

# Filter Data for a specific country
country = "DE"
df_country = df   |> 
  filter(cntry==country)

table(df_country$ValueType)

valuetypenames = c("SelfEnhancement", "Openness", "Conservation", "SelfTranscendence")
# Calculate and plot correlation matrices
par(mfrow=c(1,4))
for (i in 1:4){
  valuetype = valuetypenames[[i]]
  matrix <- df_country |> filter(ValueType == valuetype) |> 
    dplyr::select(attitudenames) |>  cor() 
  matrix |> corrplot(method='number',title=valuetype, mar=c(0,0,1,0))
}


#########################################
####    Evaluate Goodness of Fit    #####
#########################################
groupname=c("SelfEnhancement", "Openness", "Conservation", "SelfTranscendence")[1]
print(groupname)
group = df_country |>  filter(ValueType==groupname)
grouping_r <- group |> select(attitudenames)  |> cor()
n_group = nrow(group)
a = list()
for (i in 1:1000){
  sample_group <- df_country[sample(nrow(df_country), n_group), ]
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




# THINGS TO PLOT POTENTIALLY


# mds <- df_ten |> select(Conformity:Security) |> t() |> dist() |> 
#   cmdscale(eig = TRUE, k =2)
# plot(mds$points[,1],mds$points[,2])
# text(mds$points[,1],mds$points[,2],labels = row.names(mds$points))
# mds <- df_ten |> select(Openness:SelfTranscendence) |> t() |> dist() |> 
#   cmdscale(eig = TRUE, k =2)
# plot(mds$points[,1],mds$points[,2])
# text(mds$points[,1],mds$points[,2],labels = row.names(mds$points))