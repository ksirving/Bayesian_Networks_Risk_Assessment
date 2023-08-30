### create dummy bayes network 

## packages
library(tidyverse)
library(lubridate)
# install.packages("bnlearn")
# install.packages("networkD3")
# install.packages("gRain")
library(gRain)
library(bnlearn)
library(networkD3)

library(Rgraphviz)

outdir <- "Figures/"

check_prob_sum <- function(cpt) {
  col_sums <- apply(cpt, c(1, 2), sum)
  all(col_sums == 1)
}

check_prob_sum(MP.prob)
col_sums <- apply(MP.prob, c(1, 2), sum)
col_sums
# Population info ---------------------------------------------------------

pops <- read.csv("output_data/02_growth_extinction_Rates.csv")
pops
# Create the DAG ----------------------------------------------------------

## Each node is specified by a pair of brackets [<var_name>]. 
## If the node has a parentset we denote it by | and specify the list of parents separated by colons :.
## We can compute the formula into a bn object with model2network.


# dummyBN <- model2network("[A][C][B|A][D|C][F|A:B:C][E|F]")
# dummyBN

# dummyBN <- model2network("[Location][Season][Metapopulation][MPConc|Season:Location][Mortality|MPConc][ReproductiveRate|MPConc][PopulationDecline|Mortality:Metapopulation:ReproductiveRate]")
dummyBN <- model2network("[Season][Metapopulation][MPConc|Season][Mortality|MPConc][ReproductiveRate|MPConc][PopulationDecline|Mortality:Metapopulation:ReproductiveRate]")


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
                                     "Mortality" = "green", "ReproductiveRate" = "green", "PopulationDecline" = "green", "Metapopulation" = "grey"))
# ?nodeRenderInfo
# Once we have made all the desired modifications, we can plot the DAG again with the renderGraph 
# function from Rgraphviz.

renderGraph(pp1)


# Conditional Probabilities -----------------------------------------------
## low = <100, high == >100
## NOEC - no effect, LOEC = effect
# loc.lv <- c("Site1", "Site2")
seas.lv <- c("Summer", "Winter")
MP.lv <- c("NOEC", "LOEC")
mor.lv <- c("Effect", "No Effect")
repr.lv <- c("Effect", "No Effect")
pop.lv <- c("none", "Reduced")
met.lv <- c("Treasure Island", "Shaws Cove", "Crystal Cove")

# cpt_location <- matrix(c(
#   0.5, 0.5  # Location = Site1, Site2
# ), ncol = 2, dimnames = list(NULL, loc.lv))
# 
# cpt_season <- matrix(c(
#   0.5, 0.5  # Location = Site1, Site2
# ), ncol = 2, dimnames = list(NULL, seas.lv))
# 
# cpt_metapopulation <- matrix(c(
#   0.33,0.33, 0.34  # Location = Site1, Site2
# ), ncol = 3, dimnames = list(NULL, met.lv))

# 
# loc.prob <- array(c(0.5,0.5), dim = 2, dimnames = list(Location = loc.lv))
seas.prob <- array(c(0.5,0.5), dim = 2, dimnames = list(Season = seas.lv))
meta.prob <- array(c(0.33,0.33, 0.34), dim = 3, dimnames = list(Metapopulation = met.lv))
# MP.prob <- array(c(0.3, 0.7), dim = 2, dimnames = list(MPConc = MP.lv))
# MP.prob
## we need a probability that environment concentration is LOEC or higher, and NOEC or lower

## if summer
## Site A: NOEC threshold = 0.4, LOEC threshold = 0.6,
## Site B: NOEC threshold = 0.4, LOEC threshold = 0.6

## if winter
## Site A: NOEC threshold = 0.6, LOEC threshold = 0.4
## Site B: NOEC threshold = 0.6, LOEC threshold = 0.4

MP.prob <- array(c(1, 0, ## Site A: summer (NOEC, LOEC)
                   # 0.5, 0.5, ## Site B: summer (NOEC, LOEC)
                   1, 0), ## Site A: Winter (NOEC, LOEC)
                   # 0.5, 0.5), ## Site B: Winter (NOEC, LOEC)
                dim = c(2,2), dimnames = list(MPConc = MP.lv,  Season = seas.lv))

MP.prob
# # Location = loc.lv,

## here is the probability that if LOEC threshold is met, then there's an effect on reproduction and 
## if NOEC is met there's NO effect on reproduction

## IF MP NOEC: Effect = 0.1, no effect = 0.9
## IF MP LOEC: Effect = 0.9, no effect = 0.1

repr.prob <- array(c(0.1,0.9, ## NOEC (effect, no effect)
                     0.9, 0.1), ## LOEC (effect, no effect)
                   dim = c(2,2), dimnames = list(ReproductiveRate = repr.lv, MPConc = MP.lv))
repr.prob

## IF MP NOEC: Effect = 0.5, no effect = 0.5
## IF MP LOEC: Effect = 0.7, no effect = 0.3

mor.prob <- array(c(0.5,0.5, ## NOEC (effect, no effect)
                    0.9,0.1), ## LOEC (effect, no effect)
                  dim = c(2,2), dimnames = list(Mortality = mor.lv, MPConc = MP.lv))
mor.prob


# we need several things here: 1) probability of mortality effect and reproduction effect at treasure island, 
# 2) probability of ReproductionRate effect and mortality effect at Shaws Cove,
# 3) probability of ReproductionRate effect and mortality effect at Crystal Cove,
# 4) probability of ReproductionRate, given effect of mortality at all sites - assume an additive effect and add them together
 

popdec.prob <- array(c(
  (1-0.295), 0.295,  ## Mortality = Effect, ReproductiveRate = Effect, Metapopulation = Treasure Island (none, Reduced)
  (1-0.004), 0.004,  ## Mortality = No Effect, ReproductiveRate = Effect, Metapopulation = Treasure Island (none, Reduced)
  (1-0.292), 0.292,  ## Mortality = Effect, ReproductiveRate = No Effect, Metapopulation = Treasure Island (none, Reduced)
  1, 0,  ## Mortality = No Effect, ReproductiveRate = No Effect, Metapopulation = Treasure Island (none, Reduced)
  
  (1-0.295), 0.295,  ## Mortality = Effect, ReproductiveRate = Effect, Metapopulation = Shaws Cove (none, Reduced)
  (1-0.004), 0.004, ## Mortality = No Effect, ReproductiveRate = Effect, Metapopulation = Shaws Cove (none, Reduced)
  (1-0.29), 0.29,  ## Mortality = Effect, ReproductiveRate = No Effect, Metapopulation = Shaws Cove (none, Reduced)
  1, 0,  ## Mortality = No Effect, ReproductiveRate = No Effect, Metapopulation = Shaws Cove (none, Reduced)
  
  (1-0.236), 0.236,  ## Mortality = Effect, ReproductiveRate = Effect, Metapopulation = Crystal Cove (none, Reduced)
  (1-0.093), 0.093,  ## Mortality = No Effect, ReproductiveRate = Effect, Metapopulation = Crystal Cove (none, Reduced)
  (1-0.142), 0.142,  ## Mortality = Effect, ReproductiveRate = No Effect, Metapopulation = Crystal Cove (none, Reduced)
  1, 0  ## Mortality = No Effect, ReproductiveRate = No Effect, Metapopulation = Crystal Cove (none, Reduced)
), dim = c(2, 2, 2, 3), dimnames = list(
  PopulationDecline = pop.lv,
  Mortality = mor.lv,
  ReproductiveRate = repr.lv,
  Metapopulation = met.lv
))
popdec.prob
# ReproductionRate = repr.lv,
#### put all together in conditional probs list

cpt <- list(Season = seas.prob, MPConc = MP.prob, Mortality = mor.prob, ReproductiveRate = repr.prob, PopulationDecline = popdec.prob, Metapopulation = meta_prob)

cpt
#  Location = loc.prob,

# Model -------------------------------------------------------------------

# fit cpt table to network
bn <- custom.fit(dummyBN, dist=cpt, debug = T)

pdf(paste0(outdir, "simpleBBN_NoMP.pdf"), width=15, height=10)

graphviz.chart(bn, type = "barprob", grid = TRUE, bar.col = "darkgreen",
               strip.bg = "lightskyblue")

dev.off()



# test --------------------------------------------------------------------

library(bnlearn)
cptA = matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
cptA
# LOW HIGH
# [1,] 0.4  0.6
cptB = matrix(c(0.8, 0.2), ncol = 2, dimnames = list(NULL, c("GOOD", "BAD")))
cptB
# GOOD BAD
# [1,]  0.8 0.2
cptC = c(0.5, 0.5, 0.4, 0.6, 0.3, 0.7, 0.2, 0.8)
dim(cptC) = c(2, 2, 2)
dimnames(cptC) = list("C" = c("TRUE", "FALSE"), "A" =  c("LOW", "HIGH"),
                        "B" = c("GOOD", "BAD"))
cptC

cpt.bad = matrix(c(0.4, 0.5, 0.1), ncol = 3, dimnames = list(NULL, c("LOW", "AVERAGE", "HIGH")))
cpt.bad


# Influence Diagram -------------------------------------------------------
install.packages("DiagrammeR")
library(DiagrammeR)
library(igraph)
library(Rgraphviz)

influence_code <- "
  diagram: [ 
    -> Metapopulation -> PopulationDecline
    -> Decision -> Mortality
               -> ReproductiveRate
               -> Utility
    -> Metapopulation -> Mortality
               -> MPConc
    -> MPConc -> ReproductiveRate -> Value
  ]
"
render_graph(code = influence_code)

influence_diagram <- graph.empty(directed = TRUE)

# Add nodes to the graph
nodes <- c("Metapopulation", "MPConc", "Mortality", "ReproductiveRate", "PopulationDecline", "Decision", "Utility", "Value")
influence_diagram <- add.vertices(influence_diagram, v = nodes, nv = length(nodes))

# Add edges to indicate relationships
edges <- matrix(c(
  "Decision", "Mortality",
  "Decision", "PopulationDecline",
  "Decision", "ReproductiveRate",
  "Decision", "Utility",
  "Mortality", "PopulationDecline",
  "Mortality", "Utility",
  "Metapopulation", "PopulationDecline",
  "ReproductiveRate", "Value",
  "Metapopulation", "Mortality",
  "MPConc", "Metapopulation"
), ncol = 2, byrow = TRUE)

influence_diagram <- addEdgeGraph(influence_diagram, edges)
add.edges(influence_diagram, edges)
?add.edges
# Plot the influence diagram
plot(influence_diagram,
     main = "Influence Diagram",
     layout = layout.reingold.tilford,
     vertex.label = nodes,
     edge.arrow.size = 0.5,
     vertex.color = "lightblue",
     vertex.shape = "rectangle",
     edge.color = "black")


# Chat GBT ----------------------------------------------------------------
loc.lv <- c("Site1", "Site2")
seas.lv <- c("Summer", "Winter")
MP.lv <- c("NOEC", "LOEC")
mor.lv <- c("Effect", "No Effect")
repr.lv <- c("Effect", "No Effect")
pop.lv <- c("none", "Reduced")
met.lv <- c("Treasure Island", "Shaws Cove", "Crystal Cove")
# Define conditional probability distributions

# Define CPD for Location
cpt_location <- matrix(c(
  0.2, # Location = Site1
  0.5 # Location = Site2
), ncol = 2, dimnames = list(NULL, loc.lv))

# Define CPD for Season
cpt_season <- matrix(c(
  0.4, # Season = Summer
  0.3 # Season = Winter
), ncol = 2, dimnames = list(NULL, seas.lv))

# Define CPD for Metapopulation
cpt_metapopulation <- matrix(c(
  0.7, # Metapopulation = Treasure Island
  0.3,
  0# Metapopulation = Shaws Cove
), ncol = 3, dimnames = list(NULL, met.lv))

# Define CPD for MPConc given Season and Location
# (Assuming a 3x2x2 matrix for simplicity, adjust if needed)
cpt_mpconc <- array(c(
  # Site1 - Summer
  0.1, 0.2,  # NOEC
  0.3, 0.4,  # LOEC
  # Site1 - Winter
  0.5, 0.6,  # NOEC
  0.7, 0.8,  # LOEC
  # Site2 - Summer
  0.9, 0.1,  # NOEC
  0.2, 0.3,  # LOEC
  # Site2 - Winter
  0.4, 0.5,  # NOEC
  0.6, 0.7   # LOEC
), dim = c(2, 2, 2), dimnames = list(NULL, MP.lv, seas.lv, loc.lv))

# Define CPD for Mortality given MPConc
cpt_mortality <- matrix(c(
  0.1, # Mortality = Effect | MPConc = NOEC
  0.3, # Mortality = No Effect | MPConc = NOEC
  0.5, # Mortality = Effect | MPConc = LOEC
  0.7  # Mortality = No Effect | MPConc = LOEC
), ncol = 2, dimnames = list(NULL, Mortality))

# Define CPD for ReproductiveRate given MPConc
cpt_reproductive_rate <- matrix(c(
  0.2, # ReproductiveRate = Effect | MPConc = NOEC
  0.8, # ReproductiveRate = No Effect | MPConc = NOEC
  0.5, # ReproductiveRate = Effect | MPConc = LOEC
  0.5  # ReproductiveRate = No Effect | MPConc = LOEC
), ncol = 2, dimnames = list(NULL, ReproductiveRate))

# Define CPD for PopulationDecline given Mortality, Metapopulation, and ReproductiveRate
# (Assuming a 2x3x2x2 matrix for simplicity, adjust if needed)
cpt_population_decline <- array(c(
  # Effect - Treasure Island
  0.1, 0.2,  # none
  0.3, 0.4,  # Reduced
  # No Effect - Treasure Island
  0.5, 0.6,  # none
  0.7, 0.8,  # Reduced
  # Effect - Shaws Cove
  0.9, 0.1,  # none
  0.2, 0.3,  # Reduced
  # No Effect - Shaws Cove
  0.4, 0.5,  # none
  0.6, 0.7   # Reduced
), dim = c(2, 2, 3, 2), dimnames = list(NULL, PopulationDecline, metapopulation, Mortality, ReproductiveRate))

# Set the conditional probability distributions for the nodes
set.cpt(bayesian_network, "Location", cpt_location)
set.cpt(bayesian_network, "Season", cpt_season)
set.cpt(bayesian_network, "Metapopulation", cpt_metapopulation)
set.cpt(bayesian_network, "MPConc", cpt_mpconc)
set.cpt(bayesian_network, "Mortality", cpt_mortality)
set.cpt(bayesian_network, "ReproductiveRate", cpt_reproductive_rate)
set.cpt(bayesian_network, "PopulationDecline", cpt_population_decline)

# Print the network structure
print(bayesian_network)

# Plot the network graph
graphviz.plot(bayesian_network)
