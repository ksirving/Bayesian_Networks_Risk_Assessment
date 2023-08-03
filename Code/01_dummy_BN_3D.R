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
class(dummyBN)
bn.net(dummyBN)

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


# Conditional Probabilities -----------------------------------------------
# [Location]
# [Urban Land Use|Location]
# [Season]
# [MP Conc|Season:Urban Land Use]
# [Food Availability]
# [MP Accumulation|MP Conc:Food Availability:Filtration Rate]
# [Submergence]
# [Filtration Rate|Submergence]
# [Nutrition|Filtration Rate:MP Accumulation]
# [DevelopmentGrowth|Nutrition]
# [Mortality|DevelopmentGrowth]
# [Reproductive Rate|Nutrition:DevelopmentGrowth]
# [Adult Survival Rate|Mortality]
# [Metapopulation]
# [Population Decline|Adult Survival Rate:Reproductive Rate:Metapopulation]

# net = model2network("[A][B][C|A:B]")
# cptA = matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
# cptB = matrix(c(0.8, 0.2), ncol = 2, dimnames = list(NULL, c("GOOD", "BAD")))
# cptC = c(0.5, 0.5, 0.4, 0.6, 0.3, 0.7, 0.2, 0.8)
# dim(cptC) = c(2, 2, 2)
# dimnames(cptC) = list("C" = c("TRUE", "FALSE"), "A" =  c("LOW", "HIGH"),
#                       "B" = c("GOOD", "BAD"))
cptC
## singles
cptLocation = matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("SiteA", "SiteB")))
# cptUrbanLandUse = matrix(c(0.8, 0.2), ncol = 2, dimnames = list(NULL, c("High", "Low")))
cptSeason = matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("Summer", "Winter")))
cptFoodAvailability = matrix(c(0.3, 0.7), ncol = 2, dimnames = list(NULL, c("High", "Low")))
cptSubmergence = matrix(c(0.7, 0.3), ncol = 2, dimnames = list(NULL, c("Long", "Short")))
cptMetapopulation = matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("Pop1", "Pop2")))

## parents - twos
## [Urban Land Use|Location]
cptUrbanLandUse = c(0.8, 0.2, 0.5, 0.5)
dim(cptUrbanLandUse) = c(2, 2)
dimnames(cptUrbanLandUse) = list("UrbanLandUse" = c("High", "Low"), "Location" =  c("SiteA", "SiteB"))
cptUrbanLandUse

# [Filtration Rate|Submergence]
cptFiltrationRate = c(0.8, 0.2, 0.7, 0.3)
dim(cptFiltrationRate) = c(2, 2)
dimnames(cptFiltrationRate) = list("FiltrationRate" = c("High", "Low"), "Submergence" =  c("Long", "Short"))
cptFiltrationRate

# [DevelopmentGrowth|Nutrition]
cptDevelopment = c(0.3, 0.7, 0.7, 0.3)
dim(cptDevelopment) = c(2, 2)
dimnames(cptDevelopment) = list("Development" = c("High", "Low"), "Nutrition" =  c("Reduced", "None"))
cptDevelopment

# [Adult Survival Rate|Mortality]

cptSurvival = c(0.6, 0.4, 0.4, 0.6)
dim(cptSurvival) = c(2, 2)
dimnames(cptSurvival) = list("Survival" = c("High", "Low"), "Mortality" =  c("High", "Low"))
cptSurvival

# [Mortality|DevelopmentGrowth]
cptDevelopment = c(0.4, 0.6, 0.3, 0.7)
dim(cptDevelopment) = c(2, 2)
dimnames(cptDevelopment) = list("Mortality" =  c("High", "Low"), "Development" = c("High", "Low"))
cptDevelopment

## parents - > two

# [MP Conc|Season:Urban Land Use]
cptMPConc = c(0.7, 0.3, 0.5, 0.5, 0.2, 0.7, 0.2, 0.8)
dim(cptMPConc) = c(2, 2, 2)
dimnames(cptMPConc) = list("MPConc" = c("High", "Low"), "Season" =  c("Summer", "Winter"),
                      "UrbanLandUse" = c("High", "Low"))
cptMPConc

# [Nutrition|Filtration Rate:MP Accumulation]

# [Reproductive Rate|Nutrition:DevelopmentGrowth]

# [MP Accumulation|MP Conc:Food Availability:Filtration Rate]
cptMPConc = c(0.7, 0.3, 0.5, 0.5, 0.2, 0.7, 0.2, 0.8)
dim(cptMPConc) = c(2, 2, 2)
dimnames(cptMPConc) = list("MPAcc" = c("High", "Low"), "MPConc" =  c("High", "Low"),
                           "Food" = c("High", "Low"), )
cptMPConc



# [Population Decline|Adult Survival Rate:Reproductive Rate:Metapopulation]