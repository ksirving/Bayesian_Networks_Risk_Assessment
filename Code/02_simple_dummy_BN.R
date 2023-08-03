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

dummyBN <- model2network("[Location][Season][MPConc|Season:Location][Nutrition|MPConc][Mortality|Nutrition]")
# [Nutrition|MPConc][Mortality|Nutrition][ReproductiveRate|Nutrition][PopulationDecline|Mortality:ReproductiveRate]
class(dummyBN)
# bn.net(dummyBN)

plot(dummyBN)

# Plot the DAG ------------------------------------------------------------
# ?graphviz.plot
graphviz.plot(dummyBN, layout = "dot") ## nicest
# graphviz.plot(dummyBN, layout = "fdp")
# graphviz.plot(dummyBN, layout = "circo")

hlight <- list(nodes = nodes(dummyBN), arcs = arcs(dummyBN),
               col = "blue", textCol = "blue")

pp1 <- graphviz.plot(dummyBN, highlight = hlight)

#The look of the arcs can be customised as follows using the edgeRenderInfo function from Rgraphviz.
# edgeRenderInfo(pp) <- list(col = c("S~E" = "black", "E~R" = "black"),
#                            lwd = c("S~E" = 3, "E~R" = 3))


nodeRenderInfo(pp1) <- list(fontsize = 20, fill = c("Location" = "grey", "Season" = "grey", "MPConc" = "orange",
                                    "Nutrition" = "pink", "Mortality" = "pink", "ReproductiveRate" = "green", "PopulationDecline" = "green"))
# ?nodeRenderInfo
# Once we have made all the desired modifications, we can plot the DAG again with the renderGraph 
# function from Rgraphviz.

renderGraph(pp1)


# Conditional Probabilities -----------------------------------------------

loc.lv <- c("SiteA", "SiteB")
seas.lv <- c("Summer", "Winter")
MP.lv <- c("high", "low")
nutr.lv <- c("high", "low")
mor.lv <- c("high", "low")
RR.lv <- c("high", "low")
# # PD.lv <- c("none", "low", "high")
# PD.lv <- c("high", "low", "none")

loc.prob <- array(c(0.5,0.5), dim = 2, dimnames = list(Location = loc.lv))
seas.prob <- array(c(0.5,0.5), dim = 2, dimnames = list(Season = seas.lv))

## if summer
## Site A: high MP = 0.4, low MP = 0.6
## Site B: high MP = 0.3, low MP = 0.7

## if winter
## Site A: high MP = 0.8, low MP = 0.2
## Site B: high MP = 0.9, low MP = 0.1


MP.prob <- array(c(0.4, 0.6, 0.3, 0.7, 0.8, 0.2, 0.9, 0.1), 
                dim = c(2,2,2), dimnames = list(MPConc = MP.lv, Location = loc.lv, Season = seas.lv))

## IF MP high: High Nutrition = 0.2, Low nutrtion = 0.8
## IF MP low: High Nutrition = 0.6, Low nutrtion = 0.4

nut.prob <- array(c(0.2,0.8, 0.6, 0.4), dim = c(2,2), dimnames = list(Nutrition = nutr.lv, MPConc = MP.lv))
# 
# ## IF nutrition high: high mortality = 0.2, Low mortality = 0.8
# ## IF nutrition low: High mortality = 0.75, Low mortality = 0.25
# 
mor.prob <- array(c(0.2,0.8,0.75,0.25), dim = c(2,2), dimnames = list(Mortality = mor.lv, Nutrition = nutr.lv))
mor.prob
# 
# ## IF nutrition high: high reproduction = 0.8, Low reproduction = 0.2
# ## IF nutrition low: High reproduction = 0.25, Low reproduction = 0.75
# 
# repro.prob <- array(c(0.8,0.2,0.25,0.75), dim = c(2,2), dimnames = list(ReproductionRate = RR.lv, 
#                                                                         Nutrition = nutr.lv))
# repro.prob
# 
# ## if high Mortality 
# ## High Reproduction: high decline = 0.5, low decline = 0.25, no decline = 0.25
# ## Low Reproduction: high decline = 0.95, low decline = 0.04, no decline = 0.01
# 
# ## if low Mortality 
# ## High Reproduction: high decline = 0.01, low decline = 0.3, no decline = 0.69
# ## Low Reproduction: high decline = 0.1, low decline = 0.5, no decline = 0.4
# 
# popdec.prob <- array(c(0.5, 0.25, 0.25, 0.95, 0.04, 0.01, 0.01, 0.3, 0.69, 0.1, 0.5, 0.4), 
#                 dim = c(3,2,2), dimnames = list(PopDecline = PD.lv, ReproductionRate = RR.lv, Mortality = mor.lv))
# popdec.prob

# ## IF mortality high: high decline = 0.8, Low decline = 0.1, no decline, 0.1
# ## IF mortality low: high decline = 0.05, Low decline = 0.45, no decline = 0.5

# popdec.prob <- array(c(0.8, 0.1, 0.1, 0.05, 0.45, 0.5), 
#                                      dim = c(3,2), dimnames = list(PopDecline = PD.lv, Mortality = mor.lv))
# popdec.prob
                     

# cpt <- list(Location = loc.prob, Season = seas.prob, MPConc = MP.prob, Nutrition = nut.prob, 
#             Mortality = mor.prob, ReproductiveRate =repro.prob, PopulationDecline = popdec.prob)
# 
# cpt

cpt <- list(Location = loc.prob, Season = seas.prob, MPConc = MP.prob, Nutrition = nut.prob, Mortality = mor.prob)

cpt


# Model -------------------------------------------------------------------


# fit cpt table to network
bn <- custom.fit(dummyBN, dist=cpt)
bn


