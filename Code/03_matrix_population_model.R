### population matrix

# install.packages("popdemo")
library(popdemo)

# install.packages("mpmsim")
library(mpmsim)
library(popbio)
library(tidyverse)
library(tidylog)



### fertility rate calculations
a <- 6.2/36.9
b <- 24.2/10.3
c <- 2/1.2
d <- 5.1/0.5
e <- 12.9/17
f <- 3.6/1.3
g <- 7/15
h <- 5.1/15.3
i <- 18.4/2.4

mean(a,b,c,d,e,f, g,h,i) ## fertiliy rate based on Carson et al (2010, table 1) (mean of all recruits/adults)


# Carson et al 2011: SoCal Matrix -----------------------------------------

CarsonBivalve <- matrix(c("Juvenile Statis", "A1 Fertility", "A2 Fertility", ## fertility line
                    "Juvenile Survival", "A1 Statis", 0, ## survival
                    0, "A1 Survival", "A2 Statis"), ## survival 
                  nrow = 3, 
                  byrow = TRUE) 

CarsonBivalve
## adults: >10mm

## let's assume that % cover = abundance (so 10% cover = 10 individuals)


## fall matrix from carson
JuvenileStatis  <- 0.0115
A1Fertility <- 22.83 ## 
A2Fertility <- 55.91 ## 
JuvenileSurvival <- 0.0144
A1Statis <- 0.1449
A1Survival <- 0.1266
A2Statis <- 0.4070

# Population model: no effect from MP-------------------------------------------------------------

exDFx <- read.csv("output_data/02_growth_extinction_Rates.csv") %>% select(-X) %>%
  mutate(StartCover1 = StartCover/100)
exDFx


AdultSurvivalTest = 1 # Probability of Adult survival
JuvenileSurvivalTest = 1 # Probability of Juvenile survival 
FertTest = 1 # Probability of reproduction 

FertilityRate <- mean(a,b,c,d,e,f, g,h,i) ## fecundity estimate (Carson et al 2010) recriuted individuals per adult 

JuvenileStatisO  <- JuvenileStatis*exDFx$GrowthRate*JuvenileSurvivalTest
A1FertilityO <- A1Fertility*FertTest ## Fert*
A2FertilityO <- A2Fertility*FertTest
JuvenileSurvivalO <- JuvenileSurvival*exDFx$GrowthRate*JuvenileSurvivalTest #exDFx$GrowthRate*
A1StatisO <- A1Statis*exDFx$GrowthRate*AdultSurvivalTest
A1SurvivalO <- A1Survival*exDFx$GrowthRate*AdultSurvivalTest
A2StatisO <- A2Statis*exDFx$GrowthRate*AdultSurvivalTest

## sensitivity analysis defs

sensFertA1 <- lnorms(1000, A1FertilityO, 1)
sensFertA2 <- lnorms(1000, A2FertilityO, 1)
sensJuvSurv <- sapply(1:1000, function(x) betaval(mean(JuvenileSurvivalO), 0.01))
sensA1Surv <- sapply(1:1000, function(x) betaval(mean(A1SurvivalO), 0.01))

## survival and fertility rates
# AdultSurvival1 <- exDFx$GrowthRate*NOECSurvival*0.32 ## Lambda of population multiplied by NOEC survival * predation &spawning from literature
# AdultSurvival2 <- exDFx$GrowthRate*NOECSurvival*0.32 ## Lambda of population multiplied by NOEC survival
# 
# JuvenileSurvival <- 0.4 ## juvenile survival(note that settlement is M. edulis)
# ##other option - recruitment rate (0.87) * growth rate of population. Change recruitment rate if needed

StartPop <- exDFx$StartCover ## starting % cover from population
StartPop ## change this to get uncertainty

# StartPop <- seq(0,100,1)
# Create Matrix -----------------------------------------------------------

## define sites 

sites <- exDFx$Site
sites

## get df to add on new lambdas
df <- data.frame(matrix(ncol = 1, nrow=14)) %>%
  rename(ObsLambda = 1)

dfSens <- data.frame(matrix(ncol = 1000, nrow=14)) %>%
  rename(SensLambda = 1)

## df for population sizes
dfSizes <- NULL
dfSensSizes <- NULL

## define time steps: 50 years, year 1 is initial population
t <- 51
s=1
a=1
for(s in 1: length(sites)) {
  
  ## caswell matrix
  bivalve <-  matrix(c(JuvenileStatisO[s], A1FertilityO, A2FertilityO, ## fertility line
                       JuvenileSurvivalO[s], A1StatisO[s], 0, ## survival
                       0, A1SurvivalO[s], A2StatisO[s]), ## survival 
                     nrow = 3, 
                     byrow = TRUE) 
  
  bivalve

  JuvPop <- (StartPop[s]/100)*17 ## proportion of juveniles (based on fertility rate Carson 2010)
  A1Pop <- (StartPop[s]-JuvPop)/2 ## remaining adult start pop divided between A1 and A2
  A2Pop <- (StartPop[s]-JuvPop)/2
  
  n0 <- c(JuvPop, ## proportion of juveniles
          A1Pop, ## proportion of adult 1
          A2Pop) ## proportion of adult 2
  
  for(a in 1:1000) {
    
    
    bivalveSens <-  matrix(c(JuvenileStatisO[s], sensFertA1[a], sensFertA2[a], ## fertility line
                             sensJuvSurv[a], A1StatisO[s], 0, ## survival
                         0, sensA1Surv[a], A2StatisO[s]), ## survival 
                       nrow = 3, 
                       byrow = TRUE) 
    
 
    
    # Project to the next time step:
    n1 <-  bivalveSens %*% n0 # matrix product
    
    # project the population over 50 years:
    resultsSens <- pop.projection(bivalveSens,n0,iterations = t)
   
    dfSens$SensLambda[s] <- resultsSens$lambda
    ## make data frame
    dfSensSizesx <- as.data.frame(resultsSens$pop.sizes) %>%
      mutate(Site = sites[s],
             Lambda = resultsSens$lambda, 
             Year = 1:51, 
             Type = "Sensitivity",
             FertilityA1 = sensFertA1[a],
             FertilityA2 = sensFertA2[a],
             SurvivalJuv = sensJuvSurv[a],
             SurvivalA1 = sensA1Surv[a],
             Iteration = a)
    
    dfSensSizes <- rbind(dfSensSizes, dfSensSizesx)
   
  }
  
  ## Start from an initial population n at time t=0

  # Project to the next time step:
  n1 <-  bivalve %*% n0 # matrix product
  
  # project the population over 50 years:
  results <- pop.projection(bivalve,n0,iterations = t)
  
  df$ObsLambda[s] <- results$lambda
  
  dfSizesx <- as.data.frame(results$pop.sizes) %>%
    mutate(Site = sites[s],
           Lambda = results$lambda, 
           Year = 1:51, 
           Type = "Observed")
  
  dfSizes <- rbind(dfSizes, dfSizesx)
  
  
}

### join new lambdas back to df

exDFx <- cbind(exDFx, df)
exDFx

## correlation of lambdas
cor(exDFx$GrowthRate, exDFx$ObsLambda) ## 0.9997858

write.csv(exDFx, "output_data/03_Observed_Lambdas.csv")

## format and save pop sizes
head(dfSizes)

dfSizes <- dfSizes %>%
  rename(PopSizes = 1)

write.csv(dfSizes, "output_data/03_pop_projections_Observed_lambdas.csv")


# Calculate uncertainty ---------------------------------------------------
head(dfSensSizes)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))

dfSensSizes <- dfSensSizes %>%
  rename(PopSizes = 1)

write.csv(dfSensSizes, "ignore/03_pop_projections_Observed_lambdas_fert_surv.csv")

dfSensSizes <- read.csv("ignore/03_pop_projections_Observed_lambdas_fert_surv.csv")

NPsites <- c("Crystal Cove","Dana Point","Shaws Cove", "Treasure Island")

subPopsens <- dfSensSizes %>%
  filter(Site %in% NPsites) %>%
  mutate(Year = as.factor(Year)) %>%
  group_by(Year, Iteration) %>%
  mutate(RelativePopSize = PopSizes/sum(PopSizes))

## get means and std error
uncertSens <- subPopsens %>%
  ungroup() %>%
  group_by(Year, Site) %>%
  summarise(MeanLambda = mean(Lambda), SELambda = std(Lambda),
            MeanPopSize = mean(PopSizes), SEPopSizes = std(PopSizes),
            MeanRelativePopSize = mean(RelativePopSize), SERelativePopSize = std(RelativePopSize))

head(uncertSens)

## TI L = 1.02 22.94437 56.64495 0.02658848. 0.1103498 - 708
## CC L = 0.14 - 21.53638 57.41814 0.0002542576 0.09737354 - 777
## SC l 0.99 - 23.12252 56.92628 0.02483294 0.1026765 - 746

### filter to iteration needed

TIt <- subPopsens %>%
  filter(Site == "Treasure Island" & Iteration == 708) 

CIt <- subPopsens %>%
  filter(Site == "Crystal Cove" & Iteration == 777) 

SIt <- subPopsens %>%
  filter( Site == "Shaws Cove" & Iteration == 746) 


chosenIt <- bind_rows(TIt, CIt, SIt)
head(chosenIt)

P1 <- ggplot(chosenIt, aes(y=RelativePopSize, x= Year), group = Site, col = Site) +
  geom_smooth(aes(group = Site, col = Site)) +
  # geom_point(aes(Year, col = Site)) +
  scale_y_continuous(name = "Projected Percent Cover")
P1

file.name1 <- "Figures/02_Mytilus_Observed_pop_relativeCover_best_rates.jpg"
ggsave(P1, filename=file.name1, dpi=300, height=5, width=8)

P2 <- ggplot(chosenIt, aes(y=PopSizes, x= Year), group = Site, col = Site) +
  geom_smooth(aes(group = Site, col = Site)) +
  # geom_point(aes(Year, col = Site)) +
  scale_y_continuous(name = "Projected Percent Cover")
P2

file.name1 <- "Figures/02_Mytilus_Observed_pop_absoluteCover_best_rates.jpg"
ggsave(P2, filename=file.name1, dpi=300, height=5, width=8)



# Calculate elasticity ----------------------------------------------------

## fall matrix from carson
JuvenileStatis  <- 0.0115
A1Fertility <- 22.83 ## 
A2Fertility <- 55.91 ## 
JuvenileSurvival <- 0.0144
A1Statis <- 0.1449
A1Survival <- 0.1266
A2Statis <- 0.4070

## MP changes

AdultSurvivalTest = 1 # Probability of Adult survival
JuvenileSurvivalTest = 1 # Probability of Juvenile survival 
FertTest = 1 # Probability of reproduction 

## vital rates 

vrs <- chosenIt %>%
  ungroup() %>%
  select(Site, Lambda, FertilityA1:SurvivalA1) %>%
  distinct()

write.csv(vrs, "output_data/03_best_rates_lambdas_for_sites.csv")

vrs

Site <- vrs$Site
vrs
## empty df
dfx <- data.frame(matrix(ncol = 2)) %>%
  rename(Elasticity = 1, Site = 2)
dfx

for(s in 1:length(Site)) {

  ## filter to site
  data1 <- vrs %>% filter(Site == Site[s])
  
  ## input values
  JuvenileStatisO  <- JuvenileStatis*data1$Lambda*JuvenileSurvivalTest
  A1FertilityO <- data1$FertilityA1
  A2FertilityO <- data1$FertilityA2
  JuvenileSurvivalO <- data1$SurvivalJuv
  A1StatisO <- A1Statis*data1$Lambda*AdultSurvivalTest
  A1SurvivalO <- data1$SurvivalA1
  A2StatisO <- A2Statis*data1$Lambda*AdultSurvivalTest
  
  ## caswell matrix
  bivalve <-  matrix(c(JuvenileStatisO, A1FertilityO, A2FertilityO, ## fertility line
                       JuvenileSurvivalO, A1StatisO, 0, ## survival
                       0, A1SurvivalO, A2StatisO), ## survival 
                     nrow = 3, 
                     byrow = TRUE) 
  bivalve
  CarsonBivalve
  ## caculate elasticity
  elas <- elasticity(bivalve)
  jpeg(filename = paste0("Figures/03_elasticity ", Site[s], ".jpeg"))
  image2(elas, mar=c(1,3.5,5,1) )
  title(paste0(Site[s], " elasticity matrix"), line=2.5)
  dev.off()
  
  # Summed elasticities.
  # fertility in last column, stasis P on diagonal, and growth in bottom-left triangle
  df <- as.data.frame(c(Fertility=sum(elas[,3],elas[,2]), Statis=sum(diag(elas)), Survival=sum(elas[row(elas)>col(elas)])))
  names(df)[1] <- "Elasticity"
  df$Site <- Site[s]
  
  dfx <- bind_rows(dfx,df)
  

}

write.csv(dfx, "output_data/03_elasticity.csv")


# Model with different MP conc values-------------------------------------------------------------

exDFx <- read.csv("output_data/02_growth_extinction_Rates.csv") %>% select(-X) %>%
  mutate(StartCover1 = StartCover/100)
exDFx

## fall matrix from carson
JuvenileStatis  <- 0.0115
A1Fertility <- 22.83 ## 
A2Fertility <- 55.91 ## 
JuvenileSurvival <- 0.0144
A1Statis <- 0.1449
A1Survival <- 0.1266
A2Statis <- 0.4070

## MP changes

AdultSurvivalTest =  sample(seq(0.001,1,0.001)) # Probability of Adult survival
JuvenileSurvivalTest = sample(seq(0.001,1,0.001)) # Probability of Juvenile survival 
FertTest = sample(seq(0.001,1,0.001)) # Probability of reproduction 

AdultSurvivalTest =  seq(0.001,1,0.001) # Probability of Adult survival
JuvenileSurvivalTest = seq(0.001,1,0.001) # Probability of Juvenile survival 
FertTest = seq(0.001,1,0.001) # Probability of reproduction 

## define sites
Sites <- vrs$Site
Sites

## get df to add on new lambdas
df <- data.frame(matrix()) %>%
  rename(MPLambda = 1)

## df for population sizes
dfSizes <- NULL

for(s in 1:length(Sites)) {
  
  ## filter to site
  data1 <- vrs %>% filter(Site == Sites[s])
  data1
  StartPop <- exDFx %>% filter(Site == Sites[s]) ## starting % cover from population
  
  ## start population
  JuvPop <- (StartPop$StartCover/100)*17 ## proportion of juveniles (based on fertility rate Carson 2010)
  A1Pop <- (StartPop$StartCover-JuvPop)/2 ## remaining adult start pop divided between A1 and A2
  A2Pop <- (StartPop$StartCover-JuvPop)/2

  n0 <- c(JuvPop, ## proportion of juveniles
          A1Pop, ## proportion of adult 1
          A2Pop) ## proportion of adult 2

  for(m in 1:1000) {
    
    ## input values
    JuvenileStatisO  <- JuvenileStatis*data1$Lambda*JuvenileSurvivalTest[m]
    A1FertilityO <- data1$FertilityA1*FertTest[m]
    A2FertilityO <- data1$FertilityA2*FertTest[m]
    JuvenileSurvivalO <- data1$SurvivalJuv*JuvenileSurvivalTest[m]
    A1StatisO <- A1Statis*data1$Lambda*AdultSurvivalTest[m]
    A1SurvivalO <- data1$SurvivalA1*AdultSurvivalTest[m]
    A2StatisO <- A2Statis*data1$Lambda*AdultSurvivalTest[m]
    data1$SurvivalJuv*JuvenileSurvivalTest[m]
    ## caswell matrix
    bivalve <-  matrix(c(JuvenileStatisO, A1FertilityO, A2FertilityO, ## fertility line
                         JuvenileSurvivalO, A1StatisO, 0, ## survival
                         0, A1SurvivalO, A2StatisO), ## survival 
                       nrow = 3, 
                       byrow = TRUE) 
    bivalve
    ## Start from an initial population n at time t=0
    
    # Project to the next time step:
    n1 <-  bivalve %*% n0 # matrix product
    n1
    # project the population over 50 years:
    results <- pop.projection(bivalve,n0,iterations = t)
    results$lambda
    
    dfSizesx <- as.data.frame(results$pop.sizes) %>%
      mutate(Site = Sites[s],
             Lambda = results$lambda, 
             Year = 1:51, 
             Type = "Simulated",
             FertMP = FertTest[m],
             JuvSurvMP = JuvenileSurvivalTest[m],
             AdultSurvMP = AdultSurvivalTest[m],
             MPImpact = m/1000)
    
    dfSizes <- rbind(dfSizes, dfSizesx)

  }


}

head(dfSizes)

dfSizes <- dfSizes %>%
  rename(PopSize = 1) %>%
  group_by(Lambda, Site) %>%
  mutate(MeanPopSize = mean(PopSize),
         StdPopSize = std(PopSize)) %>%
  pivot_longer(c(Lambda, PopSize), values_to="Values", names_to = "PopUnit")

ymax <- dfSizes$MeanPopSize + dfSizes$StdPopSize
ymin <- dfSizes$MeanPopSize - dfSizes$StdPopSize

ggplot(dfSizes, aes(y=MeanPopSize, x=1-MPImpact)) +
  geom_smooth(aes(y=MeanPopSize,x=1-MPImpact),linewidth=1,color="red")+
  geom_ribbon(aes(ymax=ymax, ymin=ymin,x=1-MPImpact),alpha=0.2,color="red")


ggplot(dfSizes, aes(y=Values, x=1-MPImpact)) +
  stat_smooth(method = "gam", se = T)+
  facet_grid(vars(PopUnit), vars(Site), scales = "free") 


# Population model with new MP conc probs ---------------------------------

mpprobs <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/SCCWRP Risk Assessment - General/PopulationModel/riskProbs.csv")
mpprobs

## format negatives to zero and decimals, also 1-value to multiply effect
mpprobsx <- mpprobs %>%
  mutate(CI.low = ifelse(CI.low < 0, 0, CI.low)) %>%
  mutate(CI.low = 1-(CI.low/100),
         CI.High = 1-(CI.High/100),
         MagEffect = 1-(Average.Effect.Magnitude/100)) 
  
## endpoints
scens <- unique(mpprobs$Endpoint)
scens

## concentrations
concs <- unique(mpprobs$MP.Concentration.Range..particles.L.)

## effects 
popeffs <- c("Mean", "CI.High", "CI.Low")

## vital rates for observed data

vrs <- chosenIt %>%
  ungroup() %>%
  select(Site, Lambda, FertilityA1:SurvivalA1) %>%
  distinct()

## get df to add on new lambdas
df <- data.frame(matrix()) %>%
  rename(MPLambda = 1)

## df for population sizes
dfSizes <- NULL

## start population
exDFx <- read.csv("output_data/02_growth_extinction_Rates.csv") %>% select(-X) %>%
  mutate(StartCover1 = StartCover/100)
exDFx

## fall matrix from carson
JuvenileStatis  <- 0.0115
A1Fertility <- 22.83 ## 
A2Fertility <- 55.91 ## 
JuvenileSurvival <- 0.0144
A1Statis <- 0.1449
A1Survival <- 0.1266
A2Statis <- 0.4070

## define sites
Sites <- vrs$Site
Sites

## get df to add on new lambdas
df <- data.frame(matrix()) %>%
  rename(MPLambda = 1)

## df for population sizes
dfSizes <- NULL
# m=1
s=1
for(s in 1:length(Sites)) {
  
  ## filter to site
  data1 <- vrs %>% filter(Site == Sites[s])

  StartPop <- exDFx %>% filter(Site == Sites[s]) ## starting % cover from population
  
  ## start population
  JuvPop <- (StartPop$StartCover/100)*17 ## proportion of juveniles (based on fertility rate Carson 2010)
  A1Pop <- (StartPop$StartCover-JuvPop)/2 ## remaining adult start pop divided between A1 and A2
  A2Pop <- (StartPop$StartCover-JuvPop)/2
  
  n0 <- c(JuvPop, ## proportion of juveniles
          A1Pop, ## proportion of adult 1
          A2Pop) ## proportion of adult 2

  ## loop over endpoints
  for(m in 1:length(scens)) {

    ## filter data to endpoint
    
    mpdat <- mpprobsx %>% filter(Endpoint == scens[m])
    mpdat
    ## loop over concentrations
    for(c in 1:length(concs)) {
      
      con <- mpdat %>% filter(MP.Concentration.Range..particles.L. == concs[c])

      ### MP effects per life stage
      if(scens[m] == "Mortality") {
        
        ## to life stage
        addat <- con %>% filter(Life.Stage == "Adult")
        juvdat <- con %>% filter(Life.Stage == "Juvenile")
        eardat <- con %>% filter(Life.Stage == "Early")

        AdultSurvivalTest =  c(addat$MagEffect , addat$CI.High, addat$CI.low) # Probability of Adult survival
        JuvenileSurvivalTest = c(juvdat$MagEffect, juvdat$CI.High, juvdat$CI.low) # Probability of Juvenile survival 
        FertTest = c(1,1,1) # Probability of reproduction 
        
        AdultSurvivalTest
        
      } else if (scens[m] == "Growth") {
        
        ## to life stage
        # addat <- con %>% filter(Life.Stage == "Adult")
        juvdat <- con %>% filter(Life.Stage == "Juvenile")
        eardat <- con %>% filter(Life.Stage == "Early")
   
        AdultSurvivalTest =  c(1,1,1)# Probability of Adult survival
        JuvenileSurvivalTest = c(juvdat$MagEffect, juvdat$CI.High,juvdat$CI.low) # Probability of Juvenile survival 
        FertTest = c(eardat$MagEffect, eardat$CI.High, eardat$CI.low) # Probability of reproduction 

    
      } else if (scens[m] == "Reproduction") {
        
        ## to life stage
        addat <- con %>% filter(Life.Stage == "Adult")
        # juvdat <- con %>% filter(Life.Stage == "Juvenile")
        # eardat <- con %>% filter(Life.Stage == "Early")
        
        AdultSurvivalTest =  c(1,1,1) # Probability of Adult survival
        JuvenileSurvivalTest = c(1,1,1) # Probability of Juvenile survival 
        FertTest =  c(addat$MagEffect, addat$CI.High, addat$CI.low) # Probability of reproduction 
        
      } else if (scens[m]  == "Development") {
        
        ## to life stage
        # addat <- con %>% filter(Life.Stage == "Adult")
        # juvdat <- con %>% filter(Life.Stage == "Juvenile")
        eardat <- con %>% filter(Life.Stage == "Early")
        
        AdultSurvivalTest =  c(1,1,1) # Probability of Adult survival
        JuvenileSurvivalTest = c(1,1,1) # Probability of Juvenile survival 
        FertTest =  c(eardat$MagEffect, eardat$CI.High, eardat$CI.low) # Probability of reproduction 
        
      }
      

      ## loop through effects
      for (p in 1:length(popeffs)) {
        
        ## input values
        JuvenileStatisO  <- JuvenileStatis*data1$Lambda*JuvenileSurvivalTest[p]
        A1FertilityO <- data1$FertilityA1*FertTest[p]
        A2FertilityO <- data1$FertilityA2*FertTest[p]
        JuvenileSurvivalO <- data1$SurvivalJuv*JuvenileSurvivalTest[p]
        A1StatisO <- A1Statis*data1$Lambda*AdultSurvivalTest[p]
        A1SurvivalO <- data1$SurvivalA1*AdultSurvivalTest[p]
        A2StatisO <- A2Statis*data1$Lambda*AdultSurvivalTest[p]
        
        
        ## caswell matrix
        bivalve <-  matrix(c(JuvenileStatisO, A1FertilityO, A2FertilityO, ## fertility line
                             JuvenileSurvivalO, A1StatisO, 0, ## survival
                             0, A1SurvivalO, A2StatisO), ## survival 
                           nrow = 3, 
                           byrow = TRUE) 

      
        
        # Project to the next time step:
        n1 <-  bivalve %*% n0 # matrix product

        # project the population over 50 years:
        results <- pop.projection(bivalve,n0,iterations = t)
 
     
        dfSizesx <- as.data.frame(results$pop.sizes) %>%
          mutate(Site = Sites[s],
                 Lambda = results$lambda, 
                 Year = 1:51, 
                 Type = "Simulated",
                 FertMP = FertTest[p],
                 JuvSurvMP = JuvenileSurvivalTest[p],
                 AdultSurvMP = AdultSurvivalTest[p],
                 MPImpact = popeffs[p],
                 Endpoint = scens[m],
                 MPConc = concs[c])
       
        
        
        dfSizes <- rbind(dfSizes, dfSizesx)
        
      }
 
      
    }
    
    
  }
  
  
}

head(dfSizes)

write.csv(dfSizes, "output_data/03_mpconcs_pop_model_outcome.csv")

# dfsizes <- read.csv("output_data/03_mpconcs_pop_model_outcome.csv")

## format data and make relative pops
dfSizes <- dfSizes %>%
  rename(PopSizes = 1) %>%
  filter(Site %in% NPsites) %>%
  mutate(Year = as.factor(Year)) %>%
  group_by(Year, MPImpact, MPConc, Endpoint) %>%
  mutate(RelativePopSize = PopSizes/sum(PopSizes))

ends <- unique(dfSizes$Endpoint)

## plot
for(e in 1:length(ends)) {
  
  enddf <- dfSizes %>%
    filter(Endpoint == ends[e])

  P1 <- ggplot(enddf, aes(y=PopSizes, x= Year), group = Site, col = Site) +
    geom_smooth(aes(group = Site, col = Site)) +
    facet_grid(vars(MPImpact), vars(MPConc)) +
  scale_y_continuous(name = "Projected Percent Cover")
  
  
  file.name1 <- paste0("Figures/02_Mytilus_simulated_pop_", ends[e] ,".jpg")
  ggsave(P1, filename=file.name1, dpi=300, height=5, width=8)
  
}
  
dfSizes

growths <- dfSizes %>%
  ungroup() %>%
  select(Site, Lambda, MPImpact:MPConc) %>%
  distinct() %>%
  pivot_wider(names_from = Endpoint, values_from = Lambda)

write.csv(growths, "output_data/03_mpconcs_pop_model_outcome_lambdas.csv")

growths
# Differences in Lambdas --------------------------------------------------
## observed lambdas

# exDFx <- read.csv("output_data/02_growth_extinction_Rates.csv") %>% select(-X) %>%
#   mutate(StartCover1 = StartCover/100) %>% select(Site, GrowthRate)
# exDFx

vrs <- read.csv("output_data/03_best_rates_lambdas_for_sites.csv") %>%
  select(Site, Lambda) %>% rename(GrowthRate = Lambda)
vrs
#simulated lambdas
growths <- read.csv("output_data/03_mpconcs_pop_model_outcome_lambdas.csv")

allLambdas <- inner_join(growths, vrs, by = "Site") %>%
  mutate(differenceRepr = ((GrowthRate-Reproduction)/GrowthRate)) %>% ## change in reproduction for each site  
  mutate(differenceMort = ((GrowthRate-Mortality)/GrowthRate)) %>% ## change in mortality for each site
  mutate(differenceGrowth = ((GrowthRate-Growth)/GrowthRate)) %>% ## change in growth for each site
    mutate(differenceDevel = ((GrowthRate-Development)/GrowthRate)) #%>% ## change in growth for each site
  # mutate(bothEffects = differenceRepr+differenceMort) ## effects of both reproduction and mortality reduced
allLambdas

write.csv(allLambdas, "output_data/03_lambda_diffs_mort_reproduction_growth_development.csv")



# Create Matrix -----------------------------------------------------------
0.018*365
## define sites 

sites <- exDFx$Site
sites
?pop.projection
## get df to add on new lambdas
df <- data.frame(matrix(ncol = 1, nrow=14)) %>%
  rename(NewLambda = 1)

## df for population sizes
dfSizes <- NULL

## define time steps: 50 years, year 1 is initial population
t <- 51
s=1

for(s in 1: length(sites)) {
  
  ## caswell matrix
  bivalve <- matrix(c(0, 0, FertilityRate, ## fertility line
                      LarvaeSurvival, 0, 0, ## survival
                      0, JuvenileSurvival, AdultSurvival1[s]), ## survival 
                    nrow = 3, 
                    byrow = TRUE) 
  
  bivalve

  ## Start from an initial population n at time t=0
  n0 <- c(0,0, StartPop[s])
  
  # Project to the next time step:
  n1 <-  bivalve %*% n0 # matrix product
  
  # project the population over 50 years:
  results <- pop.projection(bivalve,n0,iterations = t)
  
  df$NewLambda[s] <- results$lambda
  
  dfSizesx <- as.data.frame(results$pop.sizes) %>%
    mutate(Site = sites[s],
           Lambda = results$lambda, 
           Year = 1:51, 
           Type = "NOEC")
  
  dfSizes <- rbind(dfSizes, dfSizesx)

  
}

### join new lambdas back to df

exDFx <- cbind(exDFx, df)
exDFx

## correlation of lambdas
cor(exDFx$GrowthRate, exDFx$NewLambda) ## 0.9997858

write.csv(exDFx, "output_data/03_NOEC_Lambdas.csv")

## format and save pop sizes
head(dfSizes)

dfSizes <- dfSizes %>%
  rename(PopSizes = 1)

write.csv(dfSizes, "output_data/03_pop_projections_NOEC_lambdas.csv")


# Plot populations --------------------------------------------------------

NPsites <- c("Crystal Cove","Dana Point","Shaws Cove", "Treasure Island")

subPop <- dfSizes %>%
  filter(Site %in% NPsites) %>%
  mutate(Year = as.factor(Year))

P1 <- ggplot(subPop, aes(y=PopSizes, x= Year), group = Site, col = Site) +
  geom_line(aes(group = Site, col = Site)) +
  # geom_point(aes(Year, col = Site)) +
  scale_y_continuous(name = "Projected Percent Cover")
P1

file.name1 <- "Figures/02_Mytilus_NOEC_pop.jpg"
ggsave(P1, filename=file.name1, dpi=300, height=5, width=8)

# Populations with LOEC Effect ------------------------------------------
exDFx <- read.csv("output_data/03_NOEC_Lambdas.csv") %>% select(-X)
exDFx

## NOEC = conc with no effect - calculations from above until xx MP conc
## LOEC = conc with effect - reduce fertility rate and adult survival at xx MP conc


# Reduction in reproduction -----------------------------------------------

LOECFert <- 0.6 ## reduction in reproduction given MP concentration
NOECSurvival <- 0.57

## survival and fertility rates
LarvaeSurvival <- 0.02 ## static
AdultSurvival <- exDFx$GrowthRate*NOECSurvival ## Lambda of population. change to MP/DA NOEC, LOEC
FertilityRate <- 2*LOECFert ## change to MP/DA study control reproductive output, and NOEC, LOEC
JuvenileSurvival <- 0.35*(NOECSurvival) ## recruitment rate (0.35) * growth rate of population. Change recruitment rate if needed

StartPop <- exDFx$StartCover1 ## starting % cover from population
FertilityRate
# Create Matrix -----------------------------------------------------------

## define sites 

sites <- exDFx$Site
sites

## get df to add on new lambdas
df <- data.frame(matrix(ncol = 1, nrow=14)) %>%
  rename(LOECLambdaRepro = 1)

## df for population sizes
dfSizes <- NULL

## define time steps: 50 years, year 1 is initial population
t <- 51

for(s in 1: length(sites)) {
  
  ## caswell matrix
  bivalve <- matrix(c(0, 0, FertilityRate, ## fertility line
                      LarvaeSurvival, 0, 0, ## survival
                      0, JuvenileSurvival, AdultSurvival[s]), ## survival 
                    nrow = 3, 
                    byrow = TRUE) 

  ## Start from an initial population n at time t=0
  n0 <- c(0,0, StartPop[s])
  
  # Project to the next time step:
  n1 <-  bivalve %*% n0 # matrix product
  n1
  # project the population over 50 years:
  results <- pop.projection(bivalve,n0,iterations = t)
  
  df$LOECLambdaRepro[s] <- results$lambda
  
  dfSizesx <- as.data.frame(results$pop.sizes) %>%
    mutate(Site = sites[s],
           Lambda = results$lambda, 
           Year = 1:51, 
           Type = "LOECRepro")
  
  dfSizes <- rbind(dfSizes, dfSizesx)
  
  
}

### join new lambdas back to df

exDFx <- cbind(exDFx, df)
exDFx

## format and save pop sizes
head(dfSizes)

dfSizes <- dfSizes %>%
  rename(PopSizes = 1)

write.csv(dfSizes, "output_data/03_pop_projections_LOEC_lambdas_red_reproduction.csv")


# Plot LOEC populations ---------------------------------------------------

  
NPsites <- c("Crystal Cove","Dana Point","Shaws Cove", "Treasure Island")

subPop <- dfSizes %>%
  filter(Site %in% NPsites) %>%
  mutate(Year = as.factor(Year))

P2 <- ggplot(subPop, aes(y=PopSizes, x= Year), group = Site, col = Site) +
  geom_line(aes(group = Site, col = Site)) +
  # geom_point(aes(Year, col = Site)) +
  scale_y_continuous(name = "Projected Percent Cover")
P2

file.name1 <- "Figures/02_Mytilus_LOEC_REpr_pop.jpg"
ggsave(P2, filename=file.name1, dpi=300, height=5, width=8)


# reduced mortality -------------------------------------------------------

## NOEC = conc with no effect - calculations from above until xx MP conc
## LOEC = conc with effect - reduce fertility rate and adult survival at xx MP conc

NOECFert <- 0.72
LOECSurvival <- 0.41

## survival and fertility rates
LarvaeSurvival <- 0.02 ## static
AdultSurvival <- exDFx$GrowthRate*LOECSurvival ## Lambda of population. change to MP/DA NOEC, LOEC
FertilityRate <- 2*NOECFert ## change to MP/DA study control reproductive output, and NOEC, LOEC
JuvenileSurvival <- 0.35*(LOECSurvival) ## recruitment rate (0.35) * growth rate of population. Change recruitment rate if needed

StartPop <- exDFx$StartCover1 ## starting % cover from population

# Create Matrix -----------------------------------------------------------

## define sites 

sites <- exDFx$Site
sites

## get df to add on new lambdas
df <- data.frame(matrix(ncol = 1, nrow=14)) %>%
  rename(LOECLambdaMort = 1)

## df for population sizes
dfSizes <- NULL

## define time steps: 50 years, year 1 is initial population
t <- 51

for(s in 1: length(sites)) {
  
  ## caswell matrix
  bivalve <- matrix(c(0, 0, FertilityRate, ## fertility line
                      LarvaeSurvival, 0, 0, ## survival
                      0, JuvenileSurvival, AdultSurvival[s]), ## survival 
                    nrow = 3, 
                    byrow = TRUE) 
  
  ## Start from an initial population n at time t=0
  n0 <- c(0,0, StartPop[s])
  
  # Project to the next time step:
  n1 <-  bivalve %*% n0 # matrix product
  n1
  # project the population over 50 years:
  results <- pop.projection(bivalve,n0,iterations = t)
  
  df$LOECLambdaMort[s] <- results$lambda
  
  dfSizesx <- as.data.frame(results$pop.sizes) %>%
    mutate(Site = sites[s],
           Lambda = results$lambda, 
           Year = 1:51, 
           Type = "LOECMort")
  
  dfSizes <- rbind(dfSizes, dfSizesx)
  
  
}

### join new lambdas back to df

exDFx <- cbind(exDFx, df)
exDFx

## correlation of lambdas
cor(exDFx$GrowthRate, exDFx$LOECLambda) ## 0.9997858

## format and save pop sizes
head(dfSizes)

dfSizes <- dfSizes %>%
  rename(PopSizes = 1)

write.csv(dfSizes, "output_data/03_pop_projections_LOEC_lambdas_red_survival.csv")

# Plot LOEC populations ---------------------------------------------------


NPsites <- c("Crystal Cove","Dana Point","Shaws Cove", "Treasure Island")

subPop <- dfSizes %>%
  filter(Site %in% NPsites) %>%
  mutate(Year = as.factor(Year))

P3 <- ggplot(subPop, aes(y=PopSizes, x= Year), group = Site, col = Site) +
  geom_line(aes(group = Site, col = Site)) +
  # geom_point(aes(Year, col = Site)) +
  scale_y_continuous(name = "Projected Percent Cover")
P3

file.name1 <- "Figures/02_Mytilus_LOEC_Mort_pop.jpg"
ggsave(P3, filename=file.name1, dpi=300, height=5, width=8)

# reduced mortality and reproduction -------------------------------------------------------

## NOEC = conc with no effect - calculations from above until xx MP conc
## LOEC = conc with effect - reduce fertility rate and adult survival at xx MP conc

LOECFert <- 0.6
LOECSurvival <- 0.41

## survival and fertility rates
LarvaeSurvival <- 0.02 ## static
AdultSurvival <- exDFx$GrowthRate*LOECSurvival ## Lambda of population. change to MP/DA NOEC, LOEC
FertilityRate <- 2*LOECFert ## change to MP/DA study control reproductive output, and NOEC, LOEC
JuvenileSurvival <- 0.35*(LOECSurvival) ## recruitment rate (0.35) * growth rate of population. Change recruitment rate if needed

StartPop <- exDFx$StartCover1 ## starting % cover from population

# Create Matrix -----------------------------------------------------------

## define sites 

sites <- exDFx$Site
sites

## get df to add on new lambdas
df <- data.frame(matrix(ncol = 1, nrow=14)) %>%
  rename(LOECLambdaMortRep = 1)

## df for population sizes
dfSizes <- NULL

## define time steps: 50 years, year 1 is initial population
t <- 51

for(s in 1: length(sites)) {
  
  ## caswell matrix
  bivalve <- matrix(c(0, 0, FertilityRate, ## fertility line
                      LarvaeSurvival, 0, 0, ## survival
                      0, JuvenileSurvival, AdultSurvival[s]), ## survival 
                    nrow = 3, 
                    byrow = TRUE) 
  
  ## Start from an initial population n at time t=0
  n0 <- c(0,0, StartPop[s])
  
  # Project to the next time step:
  n1 <-  bivalve %*% n0 # matrix product
  n1
  # project the population over 50 years:
  results <- pop.projection(bivalve,n0,iterations = t)
  
  df$LOECLambdaMortRep[s] <- results$lambda
  
  dfSizesx <- as.data.frame(results$pop.sizes) %>%
    mutate(Site = sites[s],
           Lambda = results$lambda, 
           Year = 1:51, 
           Type = "LOECMortRep")
  
  dfSizes <- rbind(dfSizes, dfSizesx)
  
  
}

### join new lambdas back to df

exDFx <- cbind(exDFx, df)
exDFx

## correlation of lambdas
cor(exDFx$GrowthRate, exDFx$LOECLambda) ## 0.9997858

## format and save pop sizes
head(dfSizes)

dfSizes <- dfSizes %>%
  rename(PopSizes = 1)

write.csv(dfSizes, "output_data/03_pop_projections_LOEC_lambdas_red_survival_and_reprod.csv")

# Plot LOEC populations ---------------------------------------------------


NPsites <- c("Crystal Cove","Dana Point","Shaws Cove", "Treasure Island")

subPop <- dfSizes %>%
  filter(Site %in% NPsites) %>%
  mutate(Year = as.factor(Year))

P3 <- ggplot(subPop, aes(y=PopSizes, x= Year), group = Site, col = Site) +
  geom_line(aes(group = Site, col = Site)) +
  # geom_point(aes(Year, col = Site)) +
  scale_y_continuous(name = "Projected Percent Cover")
P3

file.name1 <- "Figures/02_Mytilus_LOEC_Mort_pop.jpg"
ggsave(P3, filename=file.name1, dpi=300, height=5, width=8)

# Plot NOEC & LOEC --------------------------------------------------------

loecrepr <- read.csv("output_data/03_pop_projections_LOEC_lambdas_red_reproduction.csv")
loecmort <- read.csv("output_data/03_pop_projections_LOEC_lambdas_red_survival.csv")
loecmortrep <- read.csv("output_data/03_pop_projections_LOEC_lambdas_red_survival_and_reprod.csv")
noec <- read.csv("output_data/03_pop_projections_NOEC_lambdas.csv")

head(loecrepr)
head(noec)

alldf <- bind_rows(loecrepr, loecmort,  noec, loecmortrep)

NPsites <- c("Crystal Cove","Shaws Cove", "Treasure Island")

subPop <- alldf %>%
  filter(Site %in% NPsites) %>%
  mutate(Year = as.factor(Year))

P2 <- ggplot(subPop, aes(y=PopSizes, x= Year), group = Site, col = Site) +
  geom_line(aes(group = Site, col = Site)) +
  # geom_point(aes(Year, col = Site)) +
  scale_y_continuous(name = "Projected Percent Cover") +
  facet_wrap(~Type)
P2


#  Differences in Lambda --------------------------------------------------


lambsr <-loecrepr %>%
  select(Site, Lambda) %>%
  rename(LOECLambdaRepr = Lambda) %>%
  distinct()

lambsm <-loecmort %>%
  select(Site, Lambda) %>%
  rename(LOECLambdaMort = Lambda) %>%
  distinct()

lambsmr <-loecmortrep %>%
  select(Site, Lambda) %>%
  rename(LOECLambdaMortRep = Lambda) %>%
  distinct()

lambsmr
lambsr
lambsm

noecs <- noec %>%
  select(Site, Lambda) %>%
  rename(NOECLambda = Lambda) %>%
  distinct()

allLambdas <- full_join(lambsm, lambsr, by = "Site") %>%
  full_join(lambsmr, by = "Site") %>%
  full_join(noecs, by = "Site") %>%
  mutate(differenceRepr = ((NOECLambda-LOECLambdaRepr)/NOECLambda)) %>% ## change in reproduction for each site  
  mutate(differenceMort = ((NOECLambda-LOECLambdaMort)/NOECLambda)) %>% ## change in mortality for each site
  mutate(differenceMortRepr = ((NOECLambda-LOECLambdaMortRep)/NOECLambda)) %>% ## change in mortality and repro for each site
  mutate(bothEffects = differenceRepr+differenceMort) ## effects of both reproduction and mortality reduced
  allLambdas

write.csv(allLambdas, "output_data/03_lambda_diffs_mort_reproduction.csv")


# Air temp ----------------------------------------------------------------

at <- read.csv("output_data/02_airTemp_glms_quants_probs.csv") %>%
  select(-X) %>%
  mutate(ATDecrease = 1-ProbQuant) %>%
  rename(ATIncrease= ProbQuant)
at

alldata <- inner_join(at, allLambdas, by = "Site")
alldata

write.csv(alldata, "output_data/03_percent_diffs_air_temp_probs.csv")
