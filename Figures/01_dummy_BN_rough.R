### create dummy bayes network 

## packages
library(tidyverse)
library(lubridate)
# install.packages("bnlearn")
# install.packages("networkD3")
library(bnlearn)
library(networkD3)



# Create the DAG ----------------------------------------------------------

## Each node is specified by a pair of brackets [<var_name>]. 
## If the node has a parentset we denote it by | and specify the list of parents separated by colons :.
## We can compute the formula into a bn object with model2network.


# dummyBN <- model2network("[A][C][B|A][D|C][F|A:B:C][E|F]")
# dummyBN

dummyBN <- model2network("[Location][Urban Land Use|Location][Season][MP Conc|Season:Urban Land Use][Food Availability][MP Accumulation|MP Conc:Food Availability:Filtration Rate][Submergence][Filtration Rate|Submergence][Nutrition|Filtration Rate:MP Accumulation][DevelopmentGrowth|Nutrition][Mortality|DevelopmentGrowth][Reproductive Rate|Nutrition:DevelopmentGrowth][Adult Survival Rate|Mortality][Metapopulation][Population Decline|Adult Survival Rate:Reproductive Rate:Metapopulation]")
dummyBN


plot(dummyBN)

# Plot the DAG ------------------------------------------------------------

## background code for plot

plotD3bn <- function(bn) {
  varNames <- nodes(bn)
  # Nodes should be zero indexed!
  links <- data.frame(arcs(bn)) %>%
    mutate(from = match(from, varNames)-1, to = match(to, varNames)-1, value = 1)
  
  nodes <- data.frame(name = varNames) %>%
    mutate(group = 1, size = 30)
  
  networkD3::forceNetwork(
    Links = links,  
    Nodes = nodes,
    Source = "from",
    Target = "to",
    Value = "value",
    NodeID = "name",
    Group = "group",
    fontSize = 5,
    zoom = TRUE,
    arrows = TRUE,
    bounded = TRUE,
    opacityNoHover = 1
  )
}

## plot
plotD3bn(dummyBN)
