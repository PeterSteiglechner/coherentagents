library(tidyverse)
library(corclass)
library(igraph)

raw <- read_csv('ESS9e03_1.csv') # Use this to select other context variables later
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
attitudenames <- c("freehms", "gincdif", "lrscale", "impcntr", "euftf")
cntrynames <- dffull$cntry |> unique()

write_ABM_data <- function(country, attitudenames) {
  if (!dir.exists(paste0("ns_",country))) {
    dir.create(paste0("ns_",country))
  } 
  countrydf <- dffull |> select(idno, cntry, all_of(attitudenames)) |> filter(cntry == country)
  cm <- countrydf |> select(all_of(attitudenames)) |> cca(filter.significance = FALSE)
  1:length(cm$modules) |> 
    map(function(i) cm$modules[[i]]$dtf |> as_tibble() |>
          mutate(group = i)) |>
    reduce(bind_rows) |>
    mutate(idno = 1:nrow(countrydf)) |>  # Note: Our the most advanced ABM supposes that the first column of data is 'idno' which is not read in agents, so we "create" here 'idno' containing just unuseful number -1. 
    relocate(idno, .before = freehms) |>
    write_csv(paste0("ns_",country,"/itemsCCA.csv"))
  1:length(cm$modules) |> 
    map(function(i) cm$modules[[i]]$cormat |> as_tibble() |> 
          mutate(item = attitudenames, group = i)) |> 
    reduce(bind_rows) |> 
    write_csv(paste0("ns_",country,"/correlationsCCA.csv"))
}  

write_ABM_data("DE",attitudenames)
#cntrynames |> map(\(x) write_ABM_data(x,attitudenames))

