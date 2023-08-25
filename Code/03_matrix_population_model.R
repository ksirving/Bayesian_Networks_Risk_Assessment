### population matrix

# install.packages("popdemo")
library(popdemo)

# install.packages("popbio")
library(popbio)



# Upload Data for Mytilus-------------------------------------------------------------

exDFx <- read.csv("output_data/02_growth_extinction_Rates.csv") %>% select(-X)

## survival and fertility rates
LarvaeSurvival <- 0.01 ## static
AdultSurvival <- exDFx$GrowthRate ## Lambda of population. change to MP/DA NOEC, LOEC
FertilityRate <- 1 ## change to MP/DA study control reproductive output, and NOEC, LOEC

JuvenileSurvival <- 0.35 ## recruitment rate (0.87) * growth rate of population. Change recruitment rate if needed

StartPop <- exDFx$StartCover ## starting % cover from population

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


# Populations with NOEC ------------------------------------------
exDFx <- read.csv("output_data/03_NOEC_Lambdas.csv") %>% select(-X)
exDFx

## NOEC = conc with no effect - calculations from above until xx MP conc
## LOEC = conc with effect - reduce fertility rate and adult survival at xx MP conc

LOECFert <- 0.2
LOECMort <- 0.7

## survival and fertility rates
LarvaeSurvival <- 0.01 ## static
AdultSurvival <- exDFx$GrowthRate*LOECMort## Lambda of population. change to MP/DA NOEC, LOEC
FertilityRate <- 1*LOECFert ## change to MP/DA study control reproductive output, and NOEC, LOEC
JuvenileSurvival <- 0.35*(1-LOECMort) ## recruitment rate (0.87) * growth rate of population. Change recruitment rate if needed
exDFx$GrowthRate*LOECMort
StartPop <- exDFx$StartCover ## starting % cover from population

# Create Matrix -----------------------------------------------------------

## define sites 

sites <- exDFx$Site
sites

## get df to add on new lambdas
df <- data.frame(matrix(ncol = 1, nrow=14)) %>%
  rename(LOECLambda = 1)

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
  
  df$LOECLambda[s] <- results$lambda
  
  dfSizesx <- as.data.frame(results$pop.sizes) %>%
    mutate(Site = sites[s],
           Lambda = results$lambda, 
           Year = 1:51, 
           Type = "LOEC")
  
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

write.csv(dfSizes, "output_data/03_pop_projections_LOEC_lambdas.csv")


# Plot NOEC & LOEC --------------------------------------------------------

loec <- read.csv("output_data/03_pop_projections_LOEC_lambdas.csv")
noec <- read.csv("output_data/03_pop_projections_NOEC_lambdas.csv")

head(loec)
head(noec)

alldf <- bind_rows(loec, noec)

NPsites <- c("Crystal Cove","Dana Point","Shaws Cove", "Treasure Island")

subPop <- alldf %>%
  filter(Site %in% NPsites) %>%
  mutate(Year = as.factor(Year))

P2 <- ggplot(subPop, aes(y=PopSizes, x= Year), group = Site, col = Site) +
  geom_line(aes(group = Site, col = Site)) +
  # geom_point(aes(Year, col = Site)) +
  scale_y_continuous(name = "Projected Percent Cover") +
  facet_wrap(~Type)
P2


