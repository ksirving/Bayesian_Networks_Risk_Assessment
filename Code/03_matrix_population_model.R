### population matrix

# install.packages("popdemo")
library(popdemo)

# install.packages("popbio")
library(popbio)
library(tidyverse)
library(tidylog)

teasel
elas <- elasticity(teasel)
elas
image2(elas, mar=c(1,3.5,5,1) )
title("Teasel elasticity matrix", line=2.5)
# Summed elasticities for teasel.
# fertility in last column, stasis P on diagonal, and growth in bottom-left triangle
c(F=sum(elas[,6]), P=sum(diag(elas)), G=sum(elas[row(elas)>col(elas)]))


# Upload Data for Mytilus-------------------------------------------------------------

exDFx <- read.csv("output_data/02_growth_extinction_Rates.csv") %>% select(-X) %>%
  mutate(StartCover1 = StartCover/100)
exDFx

NOECSurvival = 0.57 # Probability of Adult survival in control
NOECFert = 0.72 # Probability of reproduction change to MP/DA NOEC

## survival and fertility rates
LarvaeSurvival <- 0.02 ## static
AdultSurvival <- exDFx$GrowthRate*NOECSurvival ## Lambda of population. change to MP/DA NOEC, LOEC
FertilityRate <- 2 ## 100% change to MP/DA study control reproductive output, and NOEC, LOEC

JuvenileSurvival <- 0.35*NOECFert ## recruitment rate (0.87) * growth rate of population. Change recruitment rate if needed

StartPop <- exDFx$StartCover1 ## starting % cover from population

# Create Matrix -----------------------------------------------------------

## define sites 

sites <- exDFx$Site
sites

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
                      0, JuvenileSurvival, AdultSurvival[s]), ## survival 
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
