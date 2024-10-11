### create dummy bayes network 

## packages
library(tidyverse)
library(lubridate)
# install.packages("bnlearn")
# install.packages("networkD3")
# install.packages("gRain")
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("Rgraphviz")
library(gRain)
library(bnlearn)
library(networkD3)

library(Rgraphviz)




outdir <- "Figures/"

# check_prob_sum <- function(cpt) {
#   col_sums <- apply(cpt, c(1, 2), sum)
#   all(col_sums == 1)
# }
# 
# check_prob_sum(MP.prob)
# col_sums <- apply(MP.prob, c(1, 2), sum)
# col_sums


# data(alarm)
# bn.cv(alarm, bn = "hc", algorithm.args = list(score = "bde", iss = 1))
# ?variable.importance

# Population info ---------------------------------------------------------

pops <- read.csv("output_data/02_growth_extinction_Rates.csv")
pops
# Create the DAG ----------------------------------------------------------

## Each node is specified by a pair of brackets [<var_name>]. 
## If the node has a parentset we denote it by | and specify the list of parents separated by colons :.
## We can compute the formula into a bn object with model2network.

# dummyBN <- model2network("[Location][Season][Metapopulation][MPConc|Season:Location][Mortality|MPConc][ReproductiveRate|MPConc][PopulationDecline|Mortality:Metapopulation:ReproductiveRate]")
dummyBN <- model2network("[Season][Metapopulation][LifeStage][MPConc][Growth|MPConc:LifeStage][AirTemperature|Season][SeaSurfaceTemp|Season][Mortality|MPConc:AirTemperature:SeaSurfaceTemp:LifeStage:Growth][ReproductiveRate|MPConc:AirTemperature:SeaSurfaceTemp:Growth:LifeStage][PopulationDecline|Mortality:ReproductiveRate:Metapopulation:MPConc]")

#[SeaSurfaceTemp]
# [Nutrition|MPConc][Mortality|Nutrition][ReproductiveRate|Nutrition][PopulationDecline|Mortality:ReproductiveRate]
class(dummyBN)
# bn.net(dummyBN)

plot(dummyBN)

# test <- bn(dummBN)

# Plot the DAG ------------------------------------------------------------
dev.off()
graphviz.plot(dummyBN, layout = "dot") ## nicest
graphviz.plot(dummyBN, layout = "fdp")
graphviz.plot(dummyBN, layout = "circo")

hlight <- list(nodes = nodes(dummyBN), arcs = arcs(dummyBN),
               col = "black", textCol = "black")

pp1 <- graphviz.plot(dummyBN, highlight = hlight, layout = "dot") +graphviz.plot(dummyBN, shape = "ellipse", highlight = list(nodes = children(dummyBN, "Growth"), fill = "orange"))
pp1+ graphviz.plot(dummyBN, highlight = list(nodes = "LifeStage", fill = "green"))
pp1+ graphviz.plot(dummyBN, highlight = list(nodes = descendants(dummyBN, "Growth"), fill = "green"),
                   highlight = list(nodes = "LifeStage", fill = "green"))

pp1 + graphviz.plot(dummyBN, shape = "ellipse", highlight = list(nodes = children(dummyBN, "Growth"), fill = "orange"))



#The look of the arcs can be customised as follows using the edgeRenderInfo function from Rgraphviz.
edgeRenderInfo(pp1) <- list(col = c( "LifeStage~Mortality" = "red"), 
                           lwd = c( "LifeStage~Mortality" = 3))
edgeRenderInfo
# "LifeStage~Mortality" = "red",
# "LifeStage~Mortality" = 3,
# "ReproductionRate~PopulationDecline" = "red"
# "Growth~ReproductionRate" = 3, "ReproductionRate~PopulationDecline" = 3
nodeRenderInfo(pp1) <- list(fontsize = 20, fill = c("Metapopulation" = "green", "Season" = "green", LifeStage = "green", ## spatial/temporal
                                                    "MPConc" = "orange", "AirTemperature" = "green", "SeaSurfaceTemp" ="red", ## stressors
                                                    "Mortality" = "orange", "ReproductiveRate" = "orange", Growth  = "orange", ## vital rates
                                                    "PopulationDecline" = "green")) ## end points
# ?nodeRenderInfo
# Once we have made all the desired modifications, we can plot the DAG again with the renderGraph 
# function from Rgraphviz.

renderGraph(pp1)

g <- randomGraph(letters[1:4], 1:3, p=0.8)
nodeRenderInfo(g) <- list(fill=c("a"="red", "b"="green"))
edgeRenderInfo(g) <- list(lwd=3)
edgeRenderInfo(g) <- list(lty=3, col="red")
parRenderInfo(g) <- list(edges=list(lwd=2, lty="dashed"),
                         nodes=list(col="gray", fill="gray"))
nodeRenderInfo(g)
edgeRenderInfo(g, "lwd")
edgeRenderInfo(g, c("lwd", "col"))
parRenderInfo(g)

layoutGraph(g)
# Conditional Probabilities -----------------------------------------------
## chances of effect at MP concs
mpprobs <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/SCCWRP Risk Assessment - General/PopulationModel/riskProbs.csv")
mpprobs

## vital rates - pop decline - mag of effect
difs <- read.csv("output_data/03_lambda_diffs_mort_reproduction_growth_development.csv")

# loc.lv <- c("Site1", "Site2")
seas.lv <- c("Spring", "Fall")
MP.lv <- c("Up to 69", "70-358", "Over 358") ## change to percentiles of ambient MP conc
AT.lv <- c("Effect", "No Effect") ## <62, 62-64, >64 - UPDATE THESE BACK TO LOW/MED/HIGH
sst.lv <- c("Low", "Medium", "High")
gro.lv <-c("Effect", "No Effect")
mor.lv <- c("Effect", "No Effect")
repr.lv <- c("Effect", "No Effect")
ls.lv <- c("Early", "Juvenile", "Adult")
pop.lv <- c("Increased", "none", "Reduced")
met.lv <- c("Treasure Island", "Shaws Cove", "Crystal Cove")

## Parent nodes

seas.prob <- array(c(0.5, 0.5), dim = 2, dimnames = list(Season = seas.lv))
ls.prob <- array(c(0.33,0.33, 0.34), dim = 3, dimnames = list(LifeStage = ls.lv))
meta.prob <- array(c(0.33,0.33, 0.34), dim = 3, dimnames = list(Metapopulation = met.lv))
MP.prob <- array(c(0.5,0.25, 0.25), dim = 3, dimnames = list(MPConc = MP.lv))

## one parent
## if spring/fall at each site - SST

sst.prob <- array(c(0.8, 0.2, 0,## Spring: Low, medium, High
                   0, 0.2, 0.8), ## Fall:Low, medium, High
                   dim = c(3,2), dimnames = list(SeaSurfaceTemp = sst.lv, Season = seas.lv))

sst.prob

# ## if spring/fall at each site - Air temp
AT.prob <- array(c( 0.25, 0.75, ## Spring: effect, no effect
                    0.75, 0.25), ## Fall: effect, no effect
                 dim = c(2,2), dimnames = list( AirTemperature= AT.lv, Season = seas.lv))

AT.prob

### vital rates effects


# Growth ------------------------------------------------------------------

grprobsJ <- mpprobs %>%
  filter(Endpoint == "Growth", Life.Stage == "Juvenile")

groJuvEff <- grprobsJ$Probability.of.Effect
groJuvNoEff <- grprobsJ$Probability.of.No.Effect

grprobsE <- mpprobs %>%
  filter(Endpoint == "Growth", Life.Stage == "Early")
groEarlyEff <- grprobsE$Probability.of.Effect
groEarlyNoEff <- grprobsE$Probability.of.No.Effect

Gr.prob <- array(c( # 0-69 conc
                  groEarlyEff[1],groEarlyNoEff[1], ## (effect, no effect) - Early
                  groJuvEff[1], groJuvNoEff[1], ##  (effect, no effect) - Juvenile
                   0,1, ##  (effect, no effect) - Adult - no data
                  ## 69-358
                  groEarlyEff[2],groEarlyNoEff[2], ## (effect, no effect) - Early
                  groJuvEff[2], groJuvNoEff[2], ##  (effect, no effect) - Juvenile
                  0,1, ##  (effect, no effect) - Adult - no data
                  ## >358
                  groEarlyEff[3],groEarlyNoEff[3], ## (effect, no effect) - Early
                  groJuvEff[3], groJuvNoEff[3], ##  (effect, no effect) - Juvenile
                  0,1), ##  (effect, no effect) - Adult - no data
                 dim = c(2,3,3), dimnames = list(Growth = gro.lv, LifeStage = ls.lv, MPConc = MP.lv))

Gr.prob
ls.lv


# Reproduction ------------------------------------------------------------

### effcts of temp and growth 

##SST

groJuvEffssteff <- groJuvEff*0.7
groearEffssteff <- groEarlyEff*0.7

## effect of AT 

groJuvEffATeff <- groJuvEff*0.7
groearEffATeff <- groEarlyEff*0.7

## both temps effect
groJuvEffTemp <- groJuvEffATeff*groJuvEffssteff
groearEffTemp <- groearEffssteff*groearEffATeff

## reproduction
repprobsA <- mpprobs %>%
  filter(Endpoint == "Reproduction", Life.Stage == "Adult")

repAdEff <- repprobsA$Probability.of.Effect
repAdNoEff <- repprobsA$Probability.of.No.Effect

## effect of temp on repro

## effect
repssteff <- repAdEff*0.7 ## sst
repATeff <- repAdEff*0.7 ##at
repTempeff <- repssteff*repATeff


repr.prob <- array(c( ## effect of growth, effect sst, effect AT
                      ## 0-69
                      groearEffTemp[1], 1-(groearEffTemp[1]), ## (effect, no effect) - Early
                      groJuvEffTemp[1], 1-(groJuvEffTemp[1]), #  (effect, no effect) - Juvenile
                      repTempeff[1], 1-(repTempeff[1]), ##  (effect, no effect) - Adult
                                      ## effect of growth, low sst, low AT
                                      ## 69-358
                      groearEffTemp[2], 1-(groearEffTemp[2]), ## (effect, no effect) - Early
                      groJuvEffTemp[2], 1-(groJuvEffTemp[2]), #  (effect, no effect) - Juvenile
                      repTempeff[2], 1-(repTempeff[2]), ##  (effect, no effect) - Adult
                                                 ## effect of growth, low sst, low AT
                                                 ## >358
                      groearEffTemp[3], 1-(groearEffTemp[3]), ## (effect, no effect) - Early
                      groJuvEffTemp[3], 1-(groJuvEffTemp[3]), #  (effect, no effect) - Juvenile
                      repTempeff[3], 1-(repTempeff[3]), ##  (effect, no effect) - Adult
                        
                      ## no effect of growth, effect sst, effect AT
                        ## 0-69
                        0.3, 0.7, ## (effect, no effect) - Early
                        0.3, 0.7, #  (effect, no effect) - Juvenile
                      repTempeff[1], 1-(repTempeff[1]), ##  (effect, no effect) - Adult
                                     ## effect of growth, low sst, low AT
                                     ## 69-358
                      0.3, 0.7, ## (effect, no effect) - Early
                      0.3, 0.7, #  (effect, no effect) - Juvenile
                      repTempeff[2], 1-(repTempeff[2]), ##  (effect, no effect) - Adult
                                                  ## effect of growth, low sst, low AT
                                                  ## >358
                      0.3, 0.7, ## (effect, no effect) - Early
                      0.3, 0.7, #  (effect, no effect) - Juvenile
                      repTempeff[3], 1-(repTempeff[3]), ##  (effect, no effect) - Adult
                      
                      ## effect of growth, no effect sst, effect AT
                      ## 0-69
                      groearEffATeff[1], 1-(groearEffATeff[1]), ## (effect, no effect) - Early
                      groJuvEffATeff[1], 1-(groJuvEffATeff[1]), #  (effect, no effect) - Juvenile
                      repATeff[1], 1-(repATeff[1]), ##  (effect, no effect) - Adult
                                    ## effect of growth, low sst, low AT
                                    ## 69-358
                      groearEffATeff[2], 1-(groearEffATeff[2]), ## (effect, no effect) - Early
                      groJuvEffATeff[2], 1-(groJuvEffATeff[2]), #  (effect, no effect) - Juvenile
                      repATeff[2], 1-(repATeff[2]), ##  (effect, no effect) - Adult
                                                  ## effect of growth, low sst, low AT
                                                  ## >358
                      groearEffATeff[3], 1-(groearEffATeff[3]), ## (effect, no effect) - Early
                      groJuvEffATeff[3], 1-(groJuvEffATeff[3]), #  (effect, no effect) - Juvenile
                      repATeff[3], 1-(repATeff[3]), ##  (effect, no effect) - Adult
                   
                   ## no effect of growth, no effect sst, effect AT
                   ## 0-69
                   0.3, 0.7, ## (effect, no effect) - Early
                   0.3, 0.7, #  (effect, no effect) - Juvenile
                   repATeff[1], 1-(repATeff[1]), ##  (effect, no effect) - Adult
                   ## effect of growth, low sst, low AT
                   ## 69-358
                   0.3, 0.7, ## (effect, no effect) - Early
                   0.3, 0.7, #  (effect, no effect) - Juvenile
                   repATeff[2], 1-(repATeff[2]), ##  (effect, no effect) - Adult
                   ## effect of growth, low sst, low AT
                   ## >358
                   0.3, 0.7, ## (effect, no effect) - Early
                   0.3, 0.7, #  (effect, no effect) - Juvenile
                   repATeff[3], 1-(repATeff[3]), ##  (effect, no effect) - Adult
                   
                   ## effect of growth, effect sst, no effect AT
                   ## 0-69
                   groearEffssteff[1], 1-(groearEffssteff[1]), ## (effect, no effect) - Early
                   groJuvEffssteff[1], 1-(groJuvEffssteff[1]), #  (effect, no effect) - Juvenile
                   repssteff[1], 1-(repssteff[1]), ##  (effect, no effect) - Adult
                   ## effect of growth, low sst, low AT
                   ## 69-358
                   groearEffssteff[2], 1-(groearEffssteff[2]), ## (effect, no effect) - Early
                   groJuvEffssteff[2], 1-(groJuvEffssteff[2]), #  (effect, no effect) - Juvenile
                   repssteff[2], 1-(repssteff[2]), ##  (effect, no effect) - Adult
                   ## effect of growth, low sst, low AT
                   ## >358
                   groearEffssteff[3], 1-(groearEffssteff[3]), ## (effect, no effect) - Early
                   groJuvEffssteff[3], 1-(groJuvEffssteff[3]), #  (effect, no effect) - Juvenile
                   repssteff[3], 1-(repssteff[3]), ##  (effect, no effect) - Adult
                   
                   ## no effect of growth, effect sst, no effect AT
                   ## 0-69
                   0.3, 0.7, ## (effect, no effect) - Early
                   0.3, 0.7, #  (effect, no effect) - Juvenile
                   repssteff[1], 1-(repssteff[1]), ##  (effect, no effect) - Adult
                   ## effect of growth, low sst, low AT
                   ## 69-358
                   0.3, 0.7, ## (effect, no effect) - Early
                   0.3, 0.7, #  (effect, no effect) - Juvenile
                   repssteff[2], 1-(repssteff[2]), ##  (effect, no effect) - Adult
                   ## effect of growth, low sst, low AT
                   ## >358
                   0.3, 0.7, ## (effect, no effect) - Early
                   0.3, 0.7, #  (effect, no effect) - Juvenile
                   repssteff[3], 1-(repssteff[3]), ##  (effect, no effect) - Adult

                  ## effect of growth, no effect sst, no effect AT
                  ## 0-69
                  groEarlyEff[1],groEarlyNoEff[1], ## (effect, no effect) - Early
                  groJuvEff[1], groJuvNoEff[1], ##  (effect, no effect) - Juvenile
                  repAdEff[1], repAdNoEff[1], ##  (effect, no effect) - Adult
                  ## effect of growth, low sst, low AT
                  ## 69-358
                  groEarlyEff[2],groEarlyNoEff[2], ## (effect, no effect) - Early
                  groJuvEff[2], groJuvNoEff[2], ##  (effect, no effect) - Juvenile
                  repAdEff[2], repAdNoEff[2], ##  (effect, no effect) - Adult
                 
                  ## >358
                  groEarlyEff[3],groEarlyNoEff[3], ## (effect, no effect) - Early
                  groJuvEff[3], groJuvNoEff[3], ##  (effect, no effect) - Juvenile
                  repAdEff[3],repAdNoEff[3], ##  (effect, no effect) - Adult
                  
                  ## no effect of growth, effect sst, no effect AT
                  ## 0-69
                  0, 1, ## (effect, no effect) - Early
                  0, 1, #  (effect, no effect) - Juvenile
                  repAdEff[1], repAdNoEff[1], ##  (effect, no effect) - Adult
                  
                  ## 69-358
                  0, 1, ## (effect, no effect) - Early
                  0, 1, #  (effect, no effect) - Juvenile
                  repAdEff[2], repAdNoEff[2], ##  (effect, no effect) - Adult
                  
                  ## >358
                  0, 1, ## (effect, no effect) - Early
                  0, 1, #  (effect, no effect) - Juvenile
                  repAdEff[3], repAdNoEff[3]), ##  (effect, no effect) - Adult
                                    
                      
                   dim = c(2,3, 3, 2, 3, 2), dimnames = list(ReproductiveRate = repr.lv, LifeStage = ls.lv, MPConc = MP.lv, Growth = gro.lv, SeaSurfaceTemp = sst.lv, 
                                                 AirTemperature = AT.lv))
repr.prob


# Mortality ---------------------------------------------------------------

### JUST MORTALITY
## adult 
mortprobsA <- mpprobs %>%
  filter(Endpoint == "Mortality", Life.Stage == "Adult")

MorAdEff <- mortprobsA$Probability.of.Effect
MorAdNoEff <- mortprobsA$Probability.of.No.Effect

## juvemnile
mortprobsJ <- mpprobs %>%
  filter(Endpoint == "Mortality", Life.Stage == "Juvenile")

MorJuvEff <- mortprobsJ$Probability.of.Effect
MorJuvNoEff <- mortprobsJ$Probability.of.No.Effect

## EARLY
mortprobsE <- mpprobs %>%
  filter(Endpoint == "Mortality", Life.Stage == "Early")

MorearEff <- mortprobsE$Probability.of.Effect
MorearNoEff <- mortprobsE$Probability.of.No.Effect

## effect of growth on mortality
groJuvEffMor <- groJuvEff*MorJuvEff
groearEffMor <- groEarlyEff*MorearEff
MorAdEff

##  growth and SST
groJuvEffssteff <- groJuvEff*0.7*MorJuvEff
groearEffssteff <- groEarlyEff*0.7*MorearEff

## effect of AT 
groJuvEffATeff <- groJuvEff*0.7*MorJuvEff
groearEffATeff <- groEarlyEff*0.7*MorearEff

## both temps effect
groJuvEffTemp <- groJuvEffATeff*groJuvEffssteff*MorJuvEff
groearEffTemp <- groearEffssteff*groearEffATeff*MorearEff

## effect of temp on MORTALITY

## effect adult
MorssteffA <- MorAdEff*0.7 ## sst
MorATeffA <- MorAdEff*0.7 ##at
MorTempeffA <- MorssteffA*MorATeffA
## juvenile
MorssteffJ <- MorJuvEff*0.7 ## sst
MorATeffJ <- MorJuvEff*0.7 ##at
MorTempeffJ <- MorssteffJ*MorATeffJ
## early
MorssteffE <- MorearEff*0.7 ## sst
MorATeffE <- MorearEff*0.7 ##at
MorTempeffE <- MorssteffE*MorATeffE


mor.prob <- array(c( ## effect of growth, effect sst, effect AT
  ## 0-69
  groearEffTemp[1], 1-(groearEffTemp[1]), ## (effect, no effect) - Early
  groJuvEffTemp[1], 1-(groJuvEffTemp[1]), #  (effect, no effect) - Juvenile
  MorTempeffA[1], 1-(MorTempeffA[1]), ##  (effect, no effect) - Adult

  ## 69-358
  groearEffTemp[2], 1-(groearEffTemp[2]), ## (effect, no effect) - Early
  groJuvEffTemp[2], 1-(groJuvEffTemp[2]), #  (effect, no effect) - Juvenile
  MorTempeffA[2], 1-(MorTempeffA[2]), ##  (effect, no effect) - Adult

  ## >358
  groearEffTemp[3], 1-(groearEffTemp[3]), ## (effect, no effect) - Early
  groJuvEffTemp[3], 1-(groJuvEffTemp[3]), #  (effect, no effect) - Juvenile
  MorTempeffA[3], 1-(MorTempeffA[3]), ##  (effect, no effect) - Adult
  
  ## no effect of growth, effect sst, effect AT
  ## 0-69
  MorTempeffE[1], 1-(MorTempeffE[1]), ## (effect, no effect) - Early
  MorTempeffJ[1], 1-(MorTempeffJ[1]), #  (effect, no effect) - Juvenile
  MorTempeffA[1], 1-(MorTempeffA[1]), ##  (effect, no effect) - Adult
 
  ## 69-358
  MorTempeffE[2], 1-(MorTempeffE[2]), ## (effect, no effect) - Early
  MorTempeffJ[2], 1-(MorTempeffJ[2]), #  (effect, no effect) - Juvenile
  MorTempeffA[2], 1-(MorTempeffA[2]), ##  (effect, no effect) - Adult

  ## >358
  MorTempeffE[3], 1-(MorTempeffE[3]), ## (effect, no effect) - Early
  MorTempeffJ[3], 1-(MorTempeffJ[3]), #  (effect, no effect) - Juvenile
  MorTempeffA[3], 1-(MorTempeffA[3]), ##  (effect, no effect) - Adult
  
  ## effect of growth, no effect sst, effect AT
  ## 0-69
  groearEffATeff[1], 1-(groearEffATeff[1]), ## (effect, no effect) - Early
  groJuvEffATeff[1], 1-(groJuvEffATeff[1]), #  (effect, no effect) - Juvenile
  MorATeffA[1], 1-(MorATeffA[1]), ##  (effect, no effect) - Adult

  ## 69-358
  groearEffATeff[2], 1-(groearEffATeff[2]), ## (effect, no effect) - Early
  groJuvEffATeff[2], 1-(groJuvEffATeff[2]), #  (effect, no effect) - Juvenile
  MorATeffA[2], 1-(MorATeffA[2]), ##  (effect, no effect) - Adult
  
  ## >358
  groearEffATeff[3], 1-(groearEffATeff[3]), ## (effect, no effect) - Early
  groJuvEffATeff[3], 1-(groJuvEffATeff[3]), #  (effect, no effect) - Juvenile
  MorATeffA[3], 1-(MorATeffA[3]), ##  (effect, no effect) - Adult
  
  ## no effect of growth, no effect sst, effect AT
  ## 0-69
  MorATeffE[1], 1-(MorATeffE[1]), ## (effect, no effect) - Early
  MorATeffJ[1], 1-(MorATeffJ[1]),  #  (effect, no effect) - Juvenile
  MorATeffA[1], 1-(MorATeffA[1]),  ##  (effect, no effect) - Adult

  ## 69-358
  MorATeffE[2], 1-(MorATeffE[2]), ## (effect, no effect) - Early
  MorATeffJ[2], 1-(MorATeffJ[2]),  #  (effect, no effect) - Juvenile
  MorATeffA[2], 1-(MorATeffA[2]),  ##  (effect, no effect) - Adult
  ## >358
  MorATeffE[3], 1-(MorATeffE[3]), ## (effect, no effect) - Early
  MorATeffJ[3], 1-(MorATeffJ[3]),  #  (effect, no effect) - Juvenile
  MorATeffA[3], 1-(MorATeffA[3]),  ##  (effect, no effect) - Adult
  
  ## effect of growth, effect sst, no effect AT
  ## 0-69
  groearEffssteff[1], 1-(groearEffssteff[1]), ## (effect, no effect) - Early
  groJuvEffssteff[1], 1-(groJuvEffssteff[1]), #  (effect, no effect) - Juvenile
  MorssteffA[1], 1-(MorssteffA[1]), ##  (effect, no effect) - Adult
  ## effect of growth, low sst, low AT
  ## 69-358
  groearEffssteff[2], 1-(groearEffssteff[2]), ## (effect, no effect) - Early
  groJuvEffssteff[2], 1-(groJuvEffssteff[2]), #  (effect, no effect) - Juvenile
  MorssteffA[2], 1-(MorssteffA[2]), ##  (effect, no effect) - Adult
  ## effect of growth, low sst, low AT
  ## >358
  groearEffssteff[3], 1-(groearEffssteff[3]), ## (effect, no effect) - Early
  groJuvEffssteff[3], 1-(groJuvEffssteff[3]), #  (effect, no effect) - Juvenile
  MorssteffA[3], 1-(MorssteffA[3]), ##  (effect, no effect) - Adult
  
  ## no effect of growth, effect sst, no effect AT
  ## 0-69
  MorssteffE[1], 1-(MorssteffE[1]), ## (effect, no effect) - Early
  MorssteffJ[1], 1-(MorssteffJ[1]), #  (effect, no effect) - Juvenile
  MorssteffA[1], 1-(MorssteffA[1]), ##  (effect, no effect) - Adult
  
  ## 69-358
  MorssteffE[2], 1-(MorssteffE[2]), ## (effect, no effect) - Early
  MorssteffJ[2], 1-(MorssteffJ[2]), #  (effect, no effect) - Juvenile
  MorssteffA[2], 1-(MorssteffA[2]), ##  (effect, no effect) - Adult

  ## >358
  MorssteffE[3], 1-(MorssteffE[3]), ## (effect, no effect) - Early
  MorssteffJ[3], 1-(MorssteffJ[3]), #  (effect, no effect) - Juvenile
  MorssteffA[3], 1-(MorssteffA[3]), ##  (effect, no effect) - Adult
  
  ## effect of growth, no effect sst, no effect AT
  ## 0-69
  groearEffMor[1],1-(groearEffMor[1]), ## (effect, no effect) - Early
  groJuvEffMor[1], 1-(groJuvEffMor[1]), ##  (effect, no effect) - Juvenile
  MorAdEff[1], MorAdNoEff[1], ##  (effect, no effect) - Adult
 
  ## 69-358
  groearEffMor[2],1-(groearEffMor[2]), ## (effect, no effect) - Early
  groJuvEffMor[2], 1-(groJuvEffMor[2]), ##  (effect, no effect) - Juvenile
  MorAdEff[2], MorAdNoEff[2], ##  (effect, no effect) - Adult

  ## >358
  groearEffMor[3],1-(groearEffMor[3]), ## (effect, no effect) - Early
  groJuvEffMor[3], 1-(groJuvEffMor[3]), ##  (effect, no effect) - Juvenile
  MorAdEff[3], MorAdNoEff[3], ##  (effect, no effect) - Adult
  
  ## no effect of growth, no effect sst, no effect AT
  ## 0-69
  MorearEff[1], MorearNoEff[1], ## (effect, no effect) - Early
  MorJuvEff[1], MorJuvNoEff[1], #  (effect, no effect) - Juvenile
  MorAdEff[1], MorAdNoEff[1], ##  (effect, no effect) - Adult

  ## 69-358
  MorearEff[2], MorearNoEff[2], ## (effect, no effect) - Early
  MorJuvEff[2], MorJuvNoEff[2], #  (effect, no effect) - Juvenile
  MorAdEff[2], MorAdNoEff[2], ##  (effect, no effect) - Adult
 
  ## >358
  MorearEff[3], MorearNoEff[3], ## (effect, no effect) - Early
  MorJuvEff[3], MorJuvNoEff[3], #  (effect, no effect) - Juvenile
  MorAdEff[3], MorAdNoEff[3]), ##  (effect, no effect) - Adult
  
  
  dim = c(2,3, 3, 2, 3, 2), dimnames = list(Mortality = mor.lv, LifeStage = ls.lv, MPConc = MP.lv, Growth = gro.lv, SeaSurfaceTemp = sst.lv, 
                                            AirTemperature = AT.lv))
mor.prob


# Population decline ------------------------------------------------------

## growth rates, negative values = increased population
popData <- read.csv("output_data/03_lambda_diffs_mort_reproduction_growth_development.csv") %>%
  mutate(differenceMortRepr = round(differenceRepr*differenceMort, digits = 3), ## effect of both reproduction and mortality
         GrowthRate2 = ifelse(GrowthRate > 1, 1, GrowthRate)) ## if growth rate > 1 = growing population so increase = 1, 
# other wise GrowthRate in None, and 1-GrowthRate in decrease

## change negatives to 0 - shouldn't be an increase in pop - fix in population model!!!!!
popData[popData < 0] <- 0


head(popData)

## site data
## treasure Island
popDataTI <- popData %>%
  filter(Site == "Treasure Island" , MPImpact == "Mean")

## effects

bothEffTI <- popDataTI$differenceMortRepr ## both
MortEffTI <- popDataTI$differenceMort ## mortality
ReprEffTI <- popDataTI$differenceRepr ## reproduction
NoEffTI <- popDataTI$GrowthRate2

## Shaws cove
popDataSC <- popData %>%
  filter(Site == "Shaws Cove" , MPImpact == "Mean")

## effects
bothEffSC <- popDataSC$differenceMortRepr ## both
MortEffSC <- popDataSC$differenceMort ## mortality
ReprEffSC <- popDataSC$differenceRepr ## reproduction
NoEffSC <- popDataSC$GrowthRate2

## crystal cove
popDataCC <- popData %>%
  filter(Site == "Crystal Cove" , MPImpact == "Mean")

## effects
bothEffCC <- popDataCC$differenceMortRepr ## both
MortEffCC <- popDataCC$differenceMort ## mortality - increase
ReprEffCC <- popDataCC$differenceRepr ## reproduction - increase
NoEffCC <- popDataCC$GrowthRate2
NoEffCC


popdec.prob <- array(c( # Treasure Island
                    # Mortality effect, reproduction effect
                    0, 1-(bothEffTI[1]), bothEffTI[1], ## Up to 69 (Increased, none, Reduced)
                    0, 1-(bothEffTI[2]), bothEffTI[2], ## 70-358 (Increased, none, Reduced)
                    0, 1-(bothEffTI[3]), bothEffTI[3], ## Over 358 (Increased, none, Reduced)
                    # Shaws cove
                    # Mortality effect, reproduction effect
                    0, 1-(bothEffSC[1]), bothEffSC[1], ## Up to 69 (Increased, none, Reduced)
                    0, 1-(bothEffSC[2]), bothEffSC[2], ## 70-358 (Increased, none, Reduced)
                    0, 1-(bothEffSC[3]), bothEffSC[3], ## Over 358 (Increased, none, Reduced)
                    ## Crystal cove
                    # Mortality effect, reproduction effect
                    0, 1-(bothEffCC[1]), bothEffCC[1], ## Up to 69 (Increased, none, Reduced)
                    0, 1-(bothEffCC[2]), bothEffCC[2], ## 70-358 (Increased, none, Reduced)
                    0, 1-(bothEffCC[3]), bothEffCC[3], ## Over 358 (Increased, none, Reduced)
                    # Treasure Island
                    # Mortality No effect, reproduction effect
                    0, 1-(ReprEffTI[1]), ReprEffTI[1], ## Up to 69 (Increased, none, Reduced)
                    0, 1-(ReprEffTI[2]), ReprEffTI[2], ## 70-358 (Increased, none, Reduced)
                    0, 1-(ReprEffTI[3]), ReprEffTI[3], ## Over 358 (Increased, none, Reduced)
                    # Shaws cove
                    # Mortality  no effect, reproduction effect
                    0, 1-(ReprEffSC[1]), ReprEffSC[1], ## Up to 69 (Increased, none, Reduced)
                    0, 1-(ReprEffSC[2]), ReprEffSC[2], ## 70-358 (Increased, none, Reduced)
                    0, 1-(ReprEffSC[3]), ReprEffSC[3], ## Over 358 (Increased, none, Reduced)
                    ## Crystal cove
                    # Mortality no effect, reproduction effect
                    0, 1-(ReprEffCC[1]), ReprEffCC[1], ## Up to 69 (Increased, none, Reduced)
                    0, 1-(ReprEffCC[2]), ReprEffCC[2], ## 70-358 (Increased, none, Reduced)
                    0, 1-(ReprEffCC[3]), ReprEffCC[3], ## Over 358 (Increased, none, Reduced)
                    
                    # Treasure Island
                    # Mortality  effect, reproduction no effect
                    0, 1-(MortEffTI[1]), MortEffTI[1], ## Up to 69 (Increased, none, Reduced)
                    0, 1-(MortEffTI[2]), MortEffTI[2], ## 70-358 (Increased, none, Reduced)
                    0, 1-(MortEffTI[3]), MortEffTI[3], ## Over 358 (Increased, none, Reduced)
                    # Shaws cove
                    # Mortality effect, reproduction no effect
                    0, 1-(MortEffSC[1]), MortEffSC[1], ## Up to 69 (Increased, none, Reduced)
                    0, 1-(MortEffSC[2]), MortEffSC[2], ## 70-358 (Increased, none, Reduced)
                    0, 1-(MortEffSC[3]), MortEffSC[3], ## Over 358 (Increased, none, Reduced)
                    ## Crystal cove
                    # Mortality effect, reproduction no effect
                    0, 1-(MortEffCC[1]), MortEffCC[1], ## Up to 69 (Increased, none, Reduced)
                    0, 1-(MortEffCC[2]), MortEffCC[2], ## 70-358 (Increased, none, Reduced)
                    0, 1-(MortEffCC[3]), MortEffCC[3], ## Over 358 (Increased, none, Reduced)
                    
                    # Treasure Island
                    # Mortality  no effect, reproduction no effect
                    NoEffTI[1], 0, 0, ## Up to 69 (Increased, none, Reduced)
                    NoEffTI[2], 0, 0, ## 70-358 (Increased, none, Reduced)
                    NoEffTI[3], 0, 0, ## Over 358 (Increased, none, Reduced)
                    # Shaws cove
                    # mortality no effect, reproduction no effect
                    0, NoEffSC[1], 1-(NoEffSC[1]), ## Up to 69 (Increased, none, Reduced)
                    0, NoEffSC[2], 1-(NoEffSC[2]), ## 70-358 (Increased, none, Reduced)
                    0, NoEffSC[3], 1-(NoEffSC[3]), ## Over 358 (Increased, none, Reduced)
                    ## Crystal cove
                    # Mortality no effect, reproduction no effect
                    0, NoEffCC[1], 1-(NoEffCC[1]), ## Up to 69 (Increased, none, Reduced)
                    0, NoEffCC[2], 1-(NoEffCC[2]), ## 70-358 (Increased, none, Reduced)
                    0, NoEffCC[3], 1-(NoEffCC[3])), ## Over 358 (Increased, none, Reduced)
                  dim = c(3,3,3, 2, 2), dimnames = list(PopulationDecline = pop.lv, MPConc = MP.lv, Metapopulation = met.lv, Mortality = mor.lv,
                                                        ReproductiveRate = repr.lv))
popdec.prob




# Model -------------------------------------------------------------------

#### put all together in conditional probs list
cpt <- list(Season = seas.prob, MPConc = MP.prob, SeaSurfaceTemp = sst.prob, Growth = Gr.prob, Mortality = mor.prob, ReproductiveRate = repr.prob,
            AirTemperature =AT.prob, PopulationDecline = popdec.prob, Metapopulation = meta.prob, LifeStage = ls.prob)
cpt$PopulationDecline
## save out
save(cpt, file="models/03_conditional_probability_table_mean_impact.RData")

# fit cpt table to network
bn <- custom.fit(dummyBN, dist=cpt, debug = T)

pdf(paste0(outdir, "updatedBBN_MP.pdf"), width=10, height=8)

graphviz.chart(bn, type = "barprob", grid = TRUE, bar.col = "darkgreen",
               strip.bg = "lightskyblue")

dev.off()

## extract info
## population
increasedPop <- cpquery(bn, event = PopulationDecline == "Increased", evidence = TRUE)
nonePop <- cpquery(bn, event = PopulationDecline == "none", evidence = TRUE)
decreasedPop <- cpquery(bn, event = PopulationDecline == "Reduced", evidence = TRUE)

## growth
GrowthEffect <- cpquery(bn, event = Growth == "Effect", evidence = TRUE)
GrowthNoEffect <- cpquery(bn, event = Growth == "No Effect", evidence = TRUE)

## Mortality
MortEffect <- cpquery(bn, event = Mortality == "Effect", evidence = TRUE)
MortNoEffect <- cpquery(bn, event = Mortality == "No Effect", evidence = TRUE)

# Reproduction
ReproEffect <- cpquery(bn, event = ReproductiveRate == "Effect", evidence = TRUE)
ReproNoEffect <- cpquery(bn, event = ReproductiveRate == "No Effect", evidence = TRUE)

df <- as.data.frame(matrix(nrow=9, ncol = 3))
colnames(df) <- c("Node", "Level", "Probability")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
        "Growth", "Growth",
        "Mortality", "Mortality", 
        "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
        "Effect", "No Effect",
        "Effect", "No Effect",
        "Effect", "No Effect") 

df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)

df

# Confidence intervals: Population decline --------------------------------

## change cpt to Lower and upper CI
cptpop<- cpt

## site data
## treasure Island
popDataTI <- popData %>%
  filter(Site == "Treasure Island" , MPImpact == "CI.Low")

## effects
bothEffTI <- popDataTI$differenceMortRepr ## both
MortEffTI <- popDataTI$differenceMort ## mortality
ReprEffTI <- popDataTI$differenceRepr ## reproduction
NoEffTI <- popDataTI$GrowthRate2

## Shaws cove
popDataSC <- popData %>%
  filter(Site == "Shaws Cove" , MPImpact == "CI.Low")

## effects
bothEffSC <- popDataSC$differenceMortRepr ## both
MortEffSC <- popDataSC$differenceMort ## mortality
ReprEffSC <- popDataSC$differenceRepr ## reproduction
NoEffSC <- popDataSC$GrowthRate2

## crystal cove
popDataCC <- popData %>%
  filter(Site == "Crystal Cove" , MPImpact == "CI.Low")

## effects
bothEffCC <- popDataCC$differenceMortRepr ## both
MortEffCC <- popDataCC$differenceMort ## mortality - increase
ReprEffCC <- popDataCC$differenceRepr ## reproduction - increase
NoEffCC <- popDataCC$GrowthRate2



cptpop$PopulationDecline <- array(c( # Treasure Island
  # Mortality effect, reproduction effect
  0, 1-(bothEffTI[1]), bothEffTI[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(bothEffTI[2]), bothEffTI[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(bothEffTI[3]), bothEffTI[3], ## Over 358 (Increased, none, Reduced)
  # Shaws cove
  # Mortality effect, reproduction effect
  0, 1-(bothEffSC[1]), bothEffSC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(bothEffSC[2]), bothEffSC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(bothEffSC[3]), bothEffSC[3], ## Over 358 (Increased, none, Reduced)
  ## Crystal cove
  # Mortality effect, reproduction effect
  0, 1-(bothEffCC[1]), bothEffCC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(bothEffCC[2]), bothEffCC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(bothEffCC[3]), bothEffCC[3], ## Over 358 (Increased, none, Reduced)
  # Treasure Island
  # Mortality No effect, reproduction effect
  0, 1-(ReprEffTI[1]), ReprEffTI[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(ReprEffTI[2]), ReprEffTI[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(ReprEffTI[3]), ReprEffTI[3], ## Over 358 (Increased, none, Reduced)
  # Shaws cove
  # Mortality  no effect, reproduction effect
  0, 1-(ReprEffSC[1]), ReprEffSC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(ReprEffSC[2]), ReprEffSC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(ReprEffSC[3]), ReprEffSC[3], ## Over 358 (Increased, none, Reduced)
  ## Crystal cove
  # Mortality no effect, reproduction effect
  0, 1-(ReprEffCC[1]), ReprEffCC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(ReprEffCC[2]), ReprEffCC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(ReprEffCC[3]), ReprEffCC[3], ## Over 358 (Increased, none, Reduced)
  
  # Treasure Island
  # Mortality  effect, reproduction no effect
  0, 1-(MortEffTI[1]), MortEffTI[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(MortEffTI[2]), MortEffTI[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(MortEffTI[3]), MortEffTI[3], ## Over 358 (Increased, none, Reduced)
  # Shaws cove
  # Mortality effect, reproduction no effect
  0, 1-(MortEffSC[1]), MortEffSC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(MortEffSC[2]), MortEffSC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(MortEffSC[3]), MortEffSC[3], ## Over 358 (Increased, none, Reduced)
  ## Crystal cove
  # Mortality effect, reproduction no effect
  0, 1-(MortEffCC[1]), MortEffCC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(MortEffCC[2]), MortEffCC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(MortEffCC[3]), MortEffCC[3], ## Over 358 (Increased, none, Reduced)
  
  # Treasure Island
  # Mortality  no effect, reproduction no effect
  NoEffTI[1], 0, 0, ## Up to 69 (Increased, none, Reduced)
  NoEffTI[2], 0, 0, ## 70-358 (Increased, none, Reduced)
  NoEffTI[3], 0, 0, ## Over 358 (Increased, none, Reduced)
  # Shaws cove
  # mortality no effect, reproduction no effect
  0, NoEffSC[1], 1-(NoEffSC[1]), ## Up to 69 (Increased, none, Reduced)
  0, NoEffSC[2], 1-(NoEffSC[2]), ## 70-358 (Increased, none, Reduced)
  0, NoEffSC[3], 1-(NoEffSC[3]), ## Over 358 (Increased, none, Reduced)
  ## Crystal cove
  # Mortality no effect, reproduction no effect
  0, NoEffCC[1], 1-(NoEffCC[1]), ## Up to 69 (Increased, none, Reduced)
  0, NoEffCC[2], 1-(NoEffCC[2]), ## 70-358 (Increased, none, Reduced)
  0, NoEffCC[3], 1-(NoEffCC[3])), ## Over 358 (Increased, none, Reduced)
  dim = c(3,3,3, 2, 2), dimnames = list(PopulationDecline = pop.lv, MPConc = MP.lv, Metapopulation = met.lv, Mortality = mor.lv,
                                        ReproductiveRate = repr.lv))


# fit cpt table to network
bn1 <- custom.fit(dummyBN, dist=cptpop, debug = T)

pdf(paste0(outdir, "updatedBBN_MP_CILow.pdf"), width=10, height=8)

graphviz.chart(bn1, type = "barprob", grid = TRUE, bar.col = "darkgreen",
               strip.bg = "lightskyblue")

dev.off()

## extract info
## population
increasedPop <- cpquery(bn1, event = PopulationDecline == "Increased", evidence = TRUE)
nonePop <- cpquery(bn1, event = PopulationDecline == "none", evidence = TRUE)
decreasedPop <- cpquery(bn1, event = PopulationDecline == "Reduced", evidence = TRUE)

## growth
GrowthEffect <- cpquery(bn1, event = Growth == "Effect", evidence = TRUE)
GrowthNoEffect <- cpquery(bn1, event = Growth == "No Effect", evidence = TRUE)

## Mortality
MortEffect <- cpquery(bn1, event = Mortality == "Effect", evidence = TRUE)
MortNoEffect <- cpquery(bn1, event = Mortality == "No Effect", evidence = TRUE)

# Reproduction
ReproEffect <- cpquery(bn1, event = ReproductiveRate == "Effect", evidence = TRUE)
ReproNoEffect <- cpquery(bn1, event = ReproductiveRate == "No Effect", evidence = TRUE)


df$Prob.CILow <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)

## CI High

## site data
## treasure Island
popDataTI <- popData %>%
  filter(Site == "Treasure Island" , MPImpact == "CI.High")

## effects
bothEffTI <- popDataTI$differenceMortRepr ## both
MortEffTI <- popDataTI$differenceMort ## mortality
ReprEffTI <- popDataTI$differenceRepr ## reproduction
NoEffTI <- popDataTI$GrowthRate2

## Shaws cove
popDataSC <- popData %>%
  filter(Site == "Shaws Cove" , MPImpact == "CI.High")

## effects
bothEffSC <- popDataSC$differenceMortRepr ## both
MortEffSC <- popDataSC$differenceMort ## mortality
ReprEffSC <- popDataSC$differenceRepr ## reproduction
NoEffSC <- popDataSC$GrowthRate2

## crystal cove
popDataCC <- popData %>%
  filter(Site == "Crystal Cove" , MPImpact == "CI.High")

## effects
bothEffCC <- popDataCC$differenceMortRepr ## both
MortEffCC <- popDataCC$differenceMort ## mortality - increase
ReprEffCC <- popDataCC$differenceRepr ## reproduction - increase
NoEffCC <- popDataCC$GrowthRate2

cptpop$PopulationDecline <- array(c( # Treasure Island
  # Mortality effect, reproduction effect
  0, 1-(bothEffTI[1]), bothEffTI[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(bothEffTI[2]), bothEffTI[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(bothEffTI[3]), bothEffTI[3], ## Over 358 (Increased, none, Reduced)
  # Shaws cove
  # Mortality effect, reproduction effect
  0, 1-(bothEffSC[1]), bothEffSC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(bothEffSC[2]), bothEffSC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(bothEffSC[3]), bothEffSC[3], ## Over 358 (Increased, none, Reduced)
  ## Crystal cove
  # Mortality effect, reproduction effect
  0, 1-(bothEffCC[1]), bothEffCC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(bothEffCC[2]), bothEffCC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(bothEffCC[3]), bothEffCC[3], ## Over 358 (Increased, none, Reduced)
  # Treasure Island
  # Mortality No effect, reproduction effect
  0, 1-(ReprEffTI[1]), ReprEffTI[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(ReprEffTI[2]), ReprEffTI[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(ReprEffTI[3]), ReprEffTI[3], ## Over 358 (Increased, none, Reduced)
  # Shaws cove
  # Mortality  no effect, reproduction effect
  0, 1-(ReprEffSC[1]), ReprEffSC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(ReprEffSC[2]), ReprEffSC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(ReprEffSC[3]), ReprEffSC[3], ## Over 358 (Increased, none, Reduced)
  ## Crystal cove
  # Mortality no effect, reproduction effect
  0, 1-(ReprEffCC[1]), ReprEffCC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(ReprEffCC[2]), ReprEffCC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(ReprEffCC[3]), ReprEffCC[3], ## Over 358 (Increased, none, Reduced)
  
  # Treasure Island
  # Mortality  effect, reproduction no effect
  0, 1-(MortEffTI[1]), MortEffTI[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(MortEffTI[2]), MortEffTI[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(MortEffTI[3]), MortEffTI[3], ## Over 358 (Increased, none, Reduced)
  # Shaws cove
  # Mortality effect, reproduction no effect
  0, 1-(MortEffSC[1]), MortEffSC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(MortEffSC[2]), MortEffSC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(MortEffSC[3]), MortEffSC[3], ## Over 358 (Increased, none, Reduced)
  ## Crystal cove
  # Mortality effect, reproduction no effect
  0, 1-(MortEffCC[1]), MortEffCC[1], ## Up to 69 (Increased, none, Reduced)
  0, 1-(MortEffCC[2]), MortEffCC[2], ## 70-358 (Increased, none, Reduced)
  0, 1-(MortEffCC[3]), MortEffCC[3], ## Over 358 (Increased, none, Reduced)
  
  # Treasure Island
  # Mortality  no effect, reproduction no effect
  NoEffTI[1], 0, 0, ## Up to 69 (Increased, none, Reduced)
  NoEffTI[2], 0, 0, ## 70-358 (Increased, none, Reduced)
  NoEffTI[3], 0, 0, ## Over 358 (Increased, none, Reduced)
  # Shaws cove
  # mortality no effect, reproduction no effect
  0, NoEffSC[1], 1-(NoEffSC[1]), ## Up to 69 (Increased, none, Reduced)
  0, NoEffSC[2], 1-(NoEffSC[2]), ## 70-358 (Increased, none, Reduced)
  0, NoEffSC[3], 1-(NoEffSC[3]), ## Over 358 (Increased, none, Reduced)
  ## Crystal cove
  # Mortality no effect, reproduction no effect
  0, NoEffCC[1], 1-(NoEffCC[1]), ## Up to 69 (Increased, none, Reduced)
  0, NoEffCC[2], 1-(NoEffCC[2]), ## 70-358 (Increased, none, Reduced)
  0, NoEffCC[3], 1-(NoEffCC[3])), ## Over 358 (Increased, none, Reduced)
  dim = c(3,3,3, 2, 2), dimnames = list(PopulationDecline = pop.lv, MPConc = MP.lv, Metapopulation = met.lv, Mortality = mor.lv,
                                        ReproductiveRate = repr.lv))


# fit cpt table to network
bn2 <- custom.fit(dummyBN, dist=cptpop, debug = T)

pdf(paste0(outdir, "updatedBBN_MP_CIHigh.pdf"), width=10, height=8)

graphviz.chart(bn2, type = "barprob", grid = TRUE, bar.col = "darkgreen",
               strip.bg = "lightskyblue")

dev.off()

## extract info
## population
increasedPop <- cpquery(bn2, event = PopulationDecline == "Increased", evidence = TRUE)
nonePop <- cpquery(bn2, event = PopulationDecline == "none", evidence = TRUE)
decreasedPop <- cpquery(bn2, event = PopulationDecline == "Reduced", evidence = TRUE)

## growth
GrowthEffect <- cpquery(bn2, event = Growth == "Effect", evidence = TRUE)
GrowthNoEffect <- cpquery(bn2, event = Growth == "No Effect", evidence = TRUE)

## Mortality
MortEffect <- cpquery(bn2, event = Mortality == "Effect", evidence = TRUE)
MortNoEffect <- cpquery(bn2, event = Mortality == "No Effect", evidence = TRUE)

# Reproduction
ReproEffect <- cpquery(bn2, event = ReproductiveRate == "Effect", evidence = TRUE)
ReproNoEffect <- cpquery(bn2, event = ReproductiveRate == "No Effect", evidence = TRUE)

## add to DF


df$Prob.CIHigh <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)

df

write.csv(df, "output_data/03_CIs_MpImpact_probs.csv")


# Validation --------------------------------------------------------------

# library(bnlearn)
# data(alarm)
# head(alarm)
# bn.cv(alarm, bn = "hc", algorithm.args = list(score = "bde", iss = 1))
# data(learning.test)
# head(learning.test)
# class(dummyBN)
# Sensitivity Analysis ----------------------------------------------------

# Define variables
# Variable_A <- "MPConc"
# Target_Variable <- "PopulationDecline"
# 
# # Influence analysis for a specific variable
# influence_results <- influence(dummyBN, nodes = "PopulationDecline")
# ?influence
# 
# # Define the event including the target variable and evidence variables
# event <- set.arc(dummyBN, from = Variable_A, to = Target_Variable )
# event
# ?set.arc
# # Baseline conditional probabilities of Target_Variable given Variable_A
# baseline_probabilities <- cpquery(bn, event, evidence = list(Variable_A = ""))
# 
# # Sensitivity analysis
# sensitivity_results <- list()
# perturbation_values <- c(0.1, 0.2, 0.3)  # Specify perturbation values
# 
# for (perturbation in perturbation_values) {
#   perturbed_network <- cpquery(your_network, nodes = Target_Variable, evidence = list(Variable_A = "baseline_value + perturbation"))
#   sensitivity_results[paste("Perturbation =", perturbation)] <- perturbed_network - baseline_probabilities
# }


# Sensitivity Analysis: MPConc ---------------------------------------------

## Mp conc
cptx<- cpt

## probsbilities to loop over
probs <- (seq(0.01,1,0.01))

## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 6))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "ProbabilityInput", "TestLevel")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 


dfx <- NULL

for(p in 1:length(probs)) {
  
  cptx$MPConc[1] <- 0
  cptx$MPConc[2] <- 1-probs[p]
  cptx$MPConc[3] <- probs[p]

  
  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  # pdf(paste0(outdir, "updatedBBN_MP_Conc_", probs[p] ,".pdf"), width=10, height=8)
  # 
  # graphviz.chart(bnx, type = "barprob", grid = TRUE, bar.col = "darkgreen",
  #                strip.bg = "lightskyblue")
  # 
  # dev.off()
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "MPConc"
  df[,5] <- cptx$MPConc[3]
  df[,6] <- "Over 358"
  
  dfx <- bind_rows(dfx,df)
}

## save out
write.csv(dfx, "output_data/03_MPConc_sensitivity_analysis.csv")


# Sensitivity Analysis: LifeStage -----------------------------------------

cptx<- cpt

## probsbilities to loop over
probs <- (seq(0.01,1,0.01))

## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 6))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "ProbabilityInput", "TestLevel")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 


dfx <- NULL

for(p in 1:length(probs)) {
  
  cptx$LifeStage[1] <- 0
  cptx$LifeStage[2] <- 1-probs[p]
  cptx$LifeStage[3] <- probs[p]
  
  
  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  # pdf(paste0(outdir, "updatedBBN_MP_Conc_", probs[p] ,".pdf"), width=10, height=8)
  # 
  # graphviz.chart(bnx, type = "barprob", grid = TRUE, bar.col = "darkgreen",
  #                strip.bg = "lightskyblue")
  # 
  # dev.off()
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "LifeStage"
  df[,5] <- cptx$LifeStage[3]
  df[,6] <- "Adult"
  
  dfx <- bind_rows(dfx,df)
}

## save out
write.csv(dfx, "output_data/03_LifeStage_sensitivity_analysis.csv")


# Sensitivity Analysis: Season --------------------------------------------------

cptx<- cpt

## probsbilities to loop over
probs <- (seq(0.01,1,0.01))

## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 6))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "ProbabilityInput", "TestLevel")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 


dfx <- NULL

for(p in 1:length(probs)) {
  
  # cptx$Season[1] <- 0
  cptx$Season[1] <- 1-probs[p]
  cptx$Season[2] <- probs[p]
  
  
  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  # pdf(paste0(outdir, "updatedBBN_MP_Conc_", probs[p] ,".pdf"), width=10, height=8)
  # 
  # graphviz.chart(bnx, type = "barprob", grid = TRUE, bar.col = "darkgreen",
  #                strip.bg = "lightskyblue")
  # 
  # dev.off()
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "Season"
  df[,5] <- cptx$Season[2]
  df[,6] <- "Fall"
  
  dfx <- bind_rows(dfx,df)
}

## save out
write.csv(dfx, "output_data/03_Season_sensitivity_analysis.csv")


# Create sensitivity figure -----------------------------------------------

mp <- read.csv("output_data/03_MPConc_sensitivity_analysis.csv")
ls <- read.csv("output_data/03_LifeStage_sensitivity_analysis.csv")
seas <- read.csv("output_data/03_Season_sensitivity_analysis.csv")

## join together data

allsens <- bind_rows(mp,ls,seas) %>%
  filter(Level %in% c("Reduced", "Effect"))
allsens

nodes <- unique(allsens$TestNode)
nodes
i=1
i
for(i in 1:length(nodes)) {
  
  allsensx <- allsens %>%
    filter(TestNode == nodes[i])
  
  levelx <- unique(allsensx$TestLevel)

  if(levelx == "Over 358") {
    
    xlabel <- "Increasing chance of MP Conc > 358"
  } else if(levelx == "Adult") {
    
    xlabel <- "Increasing chance of Mussel being Adult"
    
  } else if(levelx == "Fall") {
    
    xlabel <- "Increasing chance of Fall season"
    
  }
  
  s1 <- ggplot(allsensx, aes(x = ProbabilityInput, y = ProbabilityOutput, group = Node, colour = Node)) +
    geom_smooth() +
    scale_y_continuous("Probability of Effect") +
    scale_x_continuous(paste(xlabel))
  s1
  
  file.name1 <- paste0("Figures/03_Mytilus_simulated_pop_", nodes[i] ,".jpg")
  ggsave(s1, filename=file.name1, dpi=300, height=5, width=8)
  
}


# Sensitivity analysis: Air Temperature ---------------------------------------

## Air temp

cptx<- cpt

## probabilities to loop over
probs <- (seq(0.01,1,0.01))

## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 6))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "ProbabilityInput", "TestLevel")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 


dfx <- NULL

for(p in 1:length(probs)) {
  
  # cptx$MPConc[1] <- 0
  # cptx$MPConc[2] <- 1-probs[p]
  # cptx$MPConc[3] <- probs[p]
  
  cptx$AirTemperature[1] <- probs[p] ## effect
  cptx$AirTemperature[2] <- 1-probs[p] ## no effect
  
  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  # pdf(paste0(outdir, "updatedBBN_MP_Conc_", probs[p] ,".pdf"), width=10, height=8)
  # 
  # graphviz.chart(bnx, type = "barprob", grid = TRUE, bar.col = "darkgreen",
  #                strip.bg = "lightskyblue")
  # 
  # dev.off()
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "AirTemperature"
  df[,5] <- cptx$AirTemperature[1]
  df[,6] <- "Effect"
  
  dfx <- bind_rows(dfx,df)
}

## save out
write.csv(dfx, "output_data/03_AirTemp_sensitivity_analysis.csv")

# Sensitivity analysis: Sea Surface Temperature ---------------------------------------

cptx<- cpt

## probabilities to loop over
probs <- (seq(0.01,1,0.01))

## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 6))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "ProbabilityInput", "TestLevel")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 


dfx <- NULL

for(p in 1:length(probs)) {
  
  # cptx$MPConc[1] <- 0
  # cptx$MPConc[2] <- 1-probs[p]
  # cptx$MPConc[3] <- probs[p]
  
  cptx$SeaSurfaceTemp[1] <- probs[p] ## effect
  cptx$SeaSurfaceTemp[2] <- 1-probs[p] ## no effect
  
  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  # pdf(paste0(outdir, "updatedBBN_MP_Conc_", probs[p] ,".pdf"), width=10, height=8)
  # 
  # graphviz.chart(bnx, type = "barprob", grid = TRUE, bar.col = "darkgreen",
  #                strip.bg = "lightskyblue")
  # 
  # dev.off()
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "SeaSurfaceTemp"
  df[,5] <- cptx$SeaSurfaceTemp[1]
  df[,6] <- "Effect"
  
  dfx <- bind_rows(dfx,df)
}

## save out
write.csv(dfx, "output_data/03_SST_sensitivity_analysis.csv")

# Sensitivity analysis: Both Temps ---------------------------------------

cptx<- cpt

## probabilities to loop over
probs <- (seq(0.01,1,0.01))

## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 6))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "ProbabilityInput", "TestLevel")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 


dfx <- NULL

for(p in 1:length(probs)) {
  
  # cptx$MPConc[1] <- 0
  # cptx$MPConc[2] <- 1-probs[p]
  # cptx$MPConc[3] <- probs[p]
  
  cptx$SeaSurfaceTemp[1] <- probs[p] ## effect
  cptx$SeaSurfaceTemp[2] <- 1-probs[p] ## no effect
  
  cptx$AirTemperature[1] <- probs[p] ## effect
  cptx$AirTemperature[2] <- 1-probs[p] ## no effect
  
  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  # pdf(paste0(outdir, "updatedBBN_MP_Conc_", probs[p] ,".pdf"), width=10, height=8)
  # 
  # graphviz.chart(bnx, type = "barprob", grid = TRUE, bar.col = "darkgreen",
  #                strip.bg = "lightskyblue")
  # 
  # dev.off()
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "BothTemps"
  df[,5] <- cptx$SeaSurfaceTemp[1]
  df[,6] <- "Effect"
  
  dfx <- bind_rows(dfx,df)
}

## save out
write.csv(dfx, "output_data/03_SST_AT_sensitivity_analysis.csv")

# Sensitivity analysis: AT and MP Conc ---------------------------------------

cptx<- cpt

## probabilities to loop over
probs <- (seq(0.01,1,0.01))

## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 6))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "ProbabilityInput", "TestLevel")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 


dfx <- NULL

for(p in 1:length(probs)) {
  
  cptx$MPConc[1] <- 0
  cptx$MPConc[2] <- 1-probs[p]
  cptx$MPConc[3] <- probs[p]
  
  cptx$AirTemperature[1] <- probs[p] ## effect
  cptx$AirTemperature[2] <- 1-probs[p] ## no effect
  
  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  # pdf(paste0(outdir, "updatedBBN_MP_Conc_", probs[p] ,".pdf"), width=10, height=8)
  # 
  # graphviz.chart(bnx, type = "barprob", grid = TRUE, bar.col = "darkgreen",
  #                strip.bg = "lightskyblue")
  # 
  # dev.off()
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "MPConc_AT"
  df[,5] <- cptx$MPConc[3]
  df[,6] <- "Over 358/Effect"
  
  dfx <- bind_rows(dfx,df)
}

## save out
write.csv(dfx, "output_data/03_MPConc_AT_sensitivity_analysis.csv")

# Sensitivity analysis: SST and MP Conc ---------------------------------------

cptx<- cpt

## probabilities to loop over
probs <- (seq(0.01,1,0.01))

## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 6))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "ProbabilityInput", "TestLevel")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 


dfx <- NULL

for(p in 1:length(probs)) {
  
  cptx$MPConc[1] <- 0
  cptx$MPConc[2] <- 1-probs[p]
  cptx$MPConc[3] <- probs[p]
  
  # cptx$AirTemperature[1] <- probs[p] ## effect
  # cptx$AirTemperature[2] <- 1-probs[p] ## no effect
  
  cptx$SeaSurfaceTemp[1] <- probs[p] ## effect
  cptx$SeaSurfaceTemp[2] <- 1-probs[p] ## no effect
  
  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  # pdf(paste0(outdir, "updatedBBN_MP_Conc_", probs[p] ,".pdf"), width=10, height=8)
  # 
  # graphviz.chart(bnx, type = "barprob", grid = TRUE, bar.col = "darkgreen",
  #                strip.bg = "lightskyblue")
  # 
  # dev.off()
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "MPConc_SST"
  df[,5] <- cptx$MPConc[3]
  df[,6] <- "Over 358/Effect"
  
  dfx <- bind_rows(dfx,df)
}

## save out
write.csv(dfx, "output_data/03_MPConc_SST_sensitivity_analysis.csv")

# Sensitivity analysis: SST, AT and MP Conc ---------------------------------------

cptx<- cpt

## probabilities to loop over
probs <- (seq(0.01,1,0.01))

## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 6))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "ProbabilityInput", "TestLevel")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 


dfx <- NULL

for(p in 1:length(probs)) {
  
  cptx$MPConc[1] <- 0
  cptx$MPConc[2] <- 1-probs[p]
  cptx$MPConc[3] <- probs[p]
  
  cptx$AirTemperature[1] <- probs[p] ## effect
  cptx$AirTemperature[2] <- 1-probs[p] ## no effect
  
  cptx$SeaSurfaceTemp[1] <- probs[p] ## effect
  cptx$SeaSurfaceTemp[2] <- 1-probs[p] ## no effect
  
  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  # pdf(paste0(outdir, "updatedBBN_MP_Conc_", probs[p] ,".pdf"), width=10, height=8)
  # 
  # graphviz.chart(bnx, type = "barprob", grid = TRUE, bar.col = "darkgreen",
  #                strip.bg = "lightskyblue")
  # 
  # dev.off()
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "MPConc_SST_AT"
  df[,5] <- cptx$MPConc[3]
  df[,6] <- "Over 358_Effect"
  
  dfx <- bind_rows(dfx,df)
}

## save out
write.csv(dfx, "output_data/03_MPConc_SST_AT_sensitivity_analysis.csv")

#  Plot sensitivity -------------------------------------------------------

## all dfs
mp <- read.csv("output_data/03_MPConc_sensitivity_analysis.csv")
ls <- read.csv("output_data/03_LifeStage_sensitivity_analysis.csv")
seas <- read.csv("output_data/03_Season_sensitivity_analysis.csv")

at <- read.csv("output_data/03_AirTemp_sensitivity_analysis.csv")
sst <- read.csv("output_data/03_SST_sensitivity_analysis.csv")
sstat <- read.csv("output_data/03_SST_AT_sensitivity_analysis.csv")
mpat <- read.csv("output_data/03_MPConc_AT_sensitivity_analysis.csv")
mpsst <- read.csv("output_data/03_MPConc_SST_sensitivity_analysis.csv")
mpsstat <- read.csv("output_data/03_MPConc_SST_AT_sensitivity_analysis.csv")

## join together data

allsens <- bind_rows(mp,ls,seas,at,sst,sstat,mpat,mpsst,mpsstat) %>%
  filter(Level %in% c("Reduced", "Effect"))
allsens

nodes <- unique(allsens$TestNode)


## make look up table for names
nodesDF <- NULL

nodesDF$Node <- nodes
nodesDF$NodeName <- c("Microplastic Concentration", "Life Stage", "Season", "Air Temperature", "SeaSurfaceTemp",
                      "Air & Sea Temperature", "Microplastic Concentration & Air Temperature", "Microplastic Concentration & Sea Temperature",
                      "Microplastic Concentration, Sea & Air Temperature")

nodesDF <- as.data.frame(nodesDF) %>%
  filter(!Node %in% c("LifeStage", "Season", "BothTemps"))
nodesDF

for(i in 1:length(nodes)) {
  
  allsensx <- allsens %>%
    filter(TestNode == nodes[i])

  main.title <- filter(nodesDF, Node == nodes[i]) 
  main.title$NodeName

  s1 <- ggplot(allsensx, aes(x = ProbabilityInput, y = ProbabilityOutput, group = Node, colour = Node)) +
    geom_smooth() +
    scale_y_continuous("Probability of Effect") +
    scale_x_continuous(paste0("Probability of Stressor Occurring")) +
    ggtitle(paste0(main.title$NodeName)) +
    labs(color = "Vital Rate") +
    theme(plot.title = element_text(color="red", size=14, face="bold.italic"),
      axis.title.x = element_text(color="blue", size=14, face="bold"),
      axis.title.y = element_text(color="#993333", size=14, face="bold"))
    
  s1
  
  file.name1 <- paste0("Figures/03_BBN_sensitivity_", nodes[i] ,".jpg")
  ggsave(s1, filename=file.name1, dpi=300, height=5, width=8)
  
}

## plot per output proabability

allsens <- bind_rows(mp,ls,seas,at,sst,sstat,mpat,mpsst,mpsstat) %>%
  filter(Level %in% c("Reduced", "Effect")) %>%
  filter(!TestNode %in% c("LifeStage", "Season", "BothTemps"))
nodesDF
head(allsens)
unique(allsens$TestNode)

outnode <- unique(allsens$Node)
outnode[1]
i=1

nodesDF$NodeName

for(i in 1:length(outnode)) {
  
  allsensx <- allsens %>%
    filter(Node == outnode[i])
  
  s2 <- ggplot(allsensx, aes(x = ProbabilityInput, y = ProbabilityOutput, group = TestNode, colour = TestNode)) +
    geom_smooth() +
    scale_y_continuous("Probability of Effect") +
    scale_x_continuous(paste0("Probability of Stressor Occurring")) +
    ggtitle(paste0(outnode[i])) +
    theme(plot.title = element_text(color="red", size=14, face="bold.italic"),
          axis.title.x = element_text(color="blue", size=14, face="bold"),
          axis.title.y = element_text(color="#993333", size=14, face="bold")) +
    labs(color = "Stressor") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    scale_color_discrete(labels= nodesDF$NodeName)
  
  s2
  
  file.name1 <- paste0("Figures/03_BBN_sensitivity_",outnode[i] ,".jpg")
  ggsave(s2, filename=file.name1, dpi=300, height=7, width=12)
  
}

  

# Example runs for presentation -------------------------------------------

## need different levels of MP conc

# e.g., MP concentration = 30 p/l, 150 p/l, 500p/l
# MP.lv <- c("Up to 69", "70-358", "Over 358")

cptx<- cpt

## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 7))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "LowConc", "MedConc", "HighConc")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 

df
dfx <- NULL
## define prob combinations
probs <- c("prob1", "prob2", "prob3")

p=2

## loop around combinations
for (p in 1:length(probs)) {
  
  probsx <- probs[p]
  
  if (probsx == "prob1") {
    
    cptx$MPConc[1] <- 1 ## "Up to 69"
    cptx$MPConc[2] <- 0 ## "70-358"
    cptx$MPConc[3] <- 0 ## "Over 358"
    
  } else if (probsx == "prob2") {
    
    cptx$MPConc[1] <- 0 ## "Up to 69"
    cptx$MPConc[2] <- 1 ## "70-358"
    cptx$MPConc[3] <- 0 ## "Over 358"
    
  } else {
    
    cptx$MPConc[1] <- 0 ## "Up to 69"
    cptx$MPConc[2] <- 0 ## "70-358"
    cptx$MPConc[3] <- 1 ## "Over 358"
    
  }

  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "MPConc"
  df[,5] <- cptx$MPConc[1]
  df[,6] <- cptx$MPConc[2]
  df[,7] <- cptx$MPConc[3]
  
  
  dfx <- bind_rows(dfx,df)
  
}


## take just effects
dfx1 <- dfx %>%
  filter(Level %in% c("Reduced", "Effect")) %>%
  pivot_longer(LowConc:HighConc, names_to = "MPConc", values_to = "Probability") %>%
  filter(!Probability ==0) %>%
  pivot_wider(names_from = MPConc, values_from = ProbabilityOutput)

dfx1

write.csv(dfx1, "output_data/05_MPconc_3_runs.csv")

dfP <- dfx1 %>%
  select(-Probability) %>%
  pivot_longer(LowConc:HighConc, names_to = "Concentration", values_to = "Probability") %>%
  mutate(Concetration = as.factor(Concentration)) %>%
  mutate(Concentration = recode_factor(Concentration, LowConc = "Low", MedConc = "Medium", HighConc = "High"))
dfP

## plot

ggplot(dfP, aes(x=Concentration, y=Probability, group = Node, col = Node)) +
  geom_line()


### Sea Surface Temperature

## < 50f, 60f, >70

cptx<- cpt
cptx$SeaSurfaceTemp


## create DF
df <- as.data.frame(matrix(nrow=9, ncol = 7))
colnames(df) <- c("Node", "Level", "ProbabilityOutput", "TestNode", "LowTemp", "MediumTemp", "HighTemp")

df[,1] <- c("PopulationDecline", "PopulationDecline", "PopulationDecline",
            "Growth", "Growth",
            "Mortality", "Mortality", 
            "ReproductiveRate", "ReproductiveRate")

df[,2] <-c("Increased", "None", "Reduced",
           "Effect", "No Effect",
           "Effect", "No Effect",
           "Effect", "No Effect") 

## define prob combinations
probs <- c("prob1", "prob2", "prob3")

dfx <- NULL
p=2

## loop around combinations
for (p in 1:length(probs)) {
  
  probsx <- probs[p]
  
  if (probsx == "prob1") {
    ## low temps
    cptx$SeaSurfaceTemp[1] <- 0## low
    cptx$SeaSurfaceTemp[2] <- 0 ## medium
    cptx$SeaSurfaceTemp[3] <- 1 ## high
    
  } else if (probsx == "prob2") {
  
    ## mid temps
    cptx$SeaSurfaceTemp[1] <- 0## low
    cptx$SeaSurfaceTemp[2] <- 1 ## medium
    cptx$SeaSurfaceTemp[3] <- 0 ## high
  
  } else {
  ## low 
    cptx$SeaSurfaceTemp[1] <- 1## low
    cptx$SeaSurfaceTemp[2] <- 0 ## medium
    cptx$SeaSurfaceTemp[3] <- 0 ## high

  }

  # fit cpt table to network
  bnx <- custom.fit(dummyBN, dist=cptx, debug = T)
  
  ## extract info
  ## population
  increasedPop <- cpquery(bnx, event = PopulationDecline == "Increased", evidence = TRUE)
  nonePop <- cpquery(bnx, event = PopulationDecline == "none", evidence = TRUE)
  decreasedPop <- cpquery(bnx, event = PopulationDecline == "Reduced", evidence = TRUE)
  
  ## growth
  GrowthEffect <- cpquery(bnx, event = Growth == "Effect", evidence = TRUE)
  GrowthNoEffect <- cpquery(bnx, event = Growth == "No Effect", evidence = TRUE)
  
  ## Mortality
  MortEffect <- cpquery(bnx, event = Mortality == "Effect", evidence = TRUE)
  MortNoEffect <- cpquery(bnx, event = Mortality == "No Effect", evidence = TRUE)
  
  # Reproduction
  ReproEffect <- cpquery(bnx, event = ReproductiveRate == "Effect", evidence = TRUE)
  ReproNoEffect <- cpquery(bnx, event = ReproductiveRate == "No Effect", evidence = TRUE)
  
  ## add to DF
  
  df[,3] <- c(increasedPop,nonePop,decreasedPop,GrowthEffect,GrowthNoEffect,MortEffect,MortNoEffect,ReproEffect,ReproNoEffect)
  df[,4] <- "SST"
  df[,5] <- cptx$SeaSurfaceTemp[1]
  df[,6] <- cptx$SeaSurfaceTemp[2]
  df[,7] <- cptx$SeaSurfaceTemp[3]
  
  
  dfx <- bind_rows(dfx,df)
  
}

dfx
## take just effects
# dfx1 <- dfx %>%
#   filter(Level %in% c("Reduced", "Effect")) %>%
#   mutate(Level = ifelse(ExtremeTemp == 1.0, "High", NA)) %>%
#   mutate(Level = ifelse(ExtremeTemp == 0.0, "Medium", Level)) %>%
#   mutate(Level = ifelse(ExtremeTemp == 0.5, "Low", Level)) %>%
#   # pivot_longer(ExtremeTemp:NormalTemp, names_to = "SST", values_to = "Probability") #%>%
#   select(-c(ExtremeTemp:NormalTemp)) %>%
#   pivot_wider(names_from = Level, values_from = ProbabilityOutput)
# 
# dfx1

## take just effects
dfx1 <- dfx %>%
  filter(Level %in% c("Reduced", "Effect")) %>%
  pivot_longer(LowTemp:HighTemp, names_to = "Temp", values_to = "Probability") %>%
  filter(!Probability ==0) %>%
  pivot_wider(names_from = Temp, values_from = ProbabilityOutput)

dfx1

write.csv(dfx1, "output_data/05_Temp_3_runs.csv")

dfP <- dfx1 %>%
  select(-Probability) %>%
  pivot_longer(LowTemp:HighTemp, names_to = "Temp", values_to = "Probability") %>%
  mutate(Temp = as.factor(Temp)) %>%
  mutate(Temp = recode_factor(Temp, LowTemp = "Low", MediumTemp = "Medium", HighTemp = "High"))
dfP




# uncertainty -------------------------------------------------------------
bn_object <- bn
set.seed(123)

dim(cpt$Mortality) ## 2 3 3 2 2 2
cpt$Mortality[, , ,1 , , ]
dimnames(cpt$Mortality)
cpt$Mortality
# Assuming bn_object is your Bayesian network object with learned CPDs
# Simulate data for 1000 cases
# Simulate data from the Bayesian network
table(cpdist(bn_object, nodes = c("MPConc"), (Mortality == "Effect"), n = 1000))

simulated_dataM <- table(cpdist(bn_object, nodes = c("Mortality", "LifeStage", "SeaSurfaceTemp", "AirTemperature"), # 
                          (MPConc == "Up to 69")))
simulated_dataM

counts_slice <- simulated_dataM[, , 1, 1] 
counts_slice
test <- apply(counts_slice, c(2), function(x) x / sum(x))

tallyM <- simulated_dataM %>%  
  pivot_longer(LifeStage:AirTemperature) %>%
  group_by(MPConc, name, value) %>%
  tally()  %>% 
  mutate(np = n/sum(n)) #%>% rename(Mortality = np) %>% select(-n)
  tallyM
simulated_dataR <- cpdist(bn_object, nodes = c("MPConc"), (ReproductiveRate == "Effect"), n = 1000)
tallyR <- simulated_dataR %>% group_by(MPConc) %>% tally() %>% 
  mutate(np = n/sum(n)) %>% rename(ReproductiveRate = np) %>% select(-n)

simulated_dataG <- cpdist(bn_object, nodes = c("MPConc"), (Growth == "Effect"), n = 1000)
tallyG <- simulated_dataG %>% group_by(MPConc) %>% tally() %>% 
  mutate(np = n/sum(n)) %>% rename(Growth = np) %>% select(-n)

simulated_dataP <- cpdist(bn_object, nodes = c("MPConc"), (PopulationDecline == "Reduced"), n = 1000)
tallyP <- simulated_dataP %>% group_by(MPConc) %>% tally() %>% 
  mutate(np = n/sum(n)) %>% rename(PopulationDecline = np) %>% select(-n)

simulated_data <- full_join(tallyM, tallyR, by = "MPConc")
simulated_data <- full_join(simulated_data, tallyG, by = "MPConc")
simulated_data <- full_join(simulated_data, tallyP, by = "MPConc")

simulated_data
# Identify input and output nodes
input_nodes <- c("MPConc")  # Replace with your input nodes
output_nodes <- c("Mortality")  # Replace with your output nodes

# Perform sensitivity analysis by varying input nodes and observing output nodes
sensitivity_results <- sensitivity(dummyBN, nodes = output_nodes, evidence = input_nodes, data = simulated_data)

# View the sensitivity results
print(sensitivity_results)

