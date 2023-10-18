## exploring data and pop model

## packages
library(tidyverse)
library(lubridate)

library(mapview)
# webshot::install_phantomjs()
library(sf)

library(ggmap)
library(maptools)
library(rgdal)



# upload pop data ---------------------------------------------------------

## population per targetted survey
alldata <- read.csv("ignore/phototransummarysd_download.csv") %>%
  filter(georegion == "CA South", target_assemblage == "mytilus", species_code == "MYTCAL") #%>% ## filter on targetted mytilus surveys and the species code (value for species)
  # select(group_code:stderr)

head(alldata)
unique(alldata$georegion)

# ## get last percent cover in series - this is the starting population value
# 
# startPop <- alldata %>%
#   select(marine_site_name, marine_common_year, average_percent_cover) %>%
#   filter(marine_common_year == 2022) %>%
#   mutate(marine_common_year = as.character(marine_common_year))


### format date, take sites near newport bay
mytilus <- alldata %>%
  mutate(Date = ymd(min_survey_date)) %>%
  separate(Date, into = c("Year", "Month", "Day"), remove = F) %>%
  mutate(Year = as.integer(Year))


## plot

m11 <- ggplot(mytilus, aes(x=Date, y=average_percent_cover), group = marine_site_name, col = marine_site_name) + 
  geom_line(aes(group = marine_site_name, col =marine_site_name)) 
  # # scale_x_date(date_breaks = "1 month", date_labels = "%M")+
  # facet_wrap(~Units, scales= "free_y")

m11

### sites 
sites <- mytilus %>%
  dplyr::select(marine_site_name, site_lat, site_long) %>%
  distinct() %>%
  st_as_sf(coords=c( "site_long", "site_lat"), crs=4326, remove=F)

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)

## plot points
m1 <- mapview(sites, cex=6, col.regions="orange",
              layer.name="Mytilus Sites")# +
# mapview(bio_sub, cex=6, col.regions="blue",
#         layer.name="Toad Observations Other")  


m1
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

mapshot(m1, url = paste0(getwd(), "/Figures/Maps/Mytilus_Californicus_socal_sites.html"),
        file = paste0(getwd(), "/Figures/Maps/Mytilus_Californicus_socal_sites.png"))


## define sites near newport
NPsites <-unique(mytilus$marine_site_name)[c(1,2,6,7)]
NPsites
NPMytilus <- mytilus %>%
  filter(marine_site_name %in% NPsites, season_name == "Fall") ## keep to one season for now as not 
# equal years in fall and spring
range(NPMytilus$Year)
NPMytilus

coords<- NPMytilus %>% 
  dplyr::select(marine_site_name, site_lat, site_long) %>%
  distinct()

head(coords)

write.csv(coords, "output_data/02_sites_coords.csv")

## plot

m2 <- ggplot(NPMytilus, aes(x=Date, y=average_percent_cover), group = marine_site_name, col = marine_site_name) + 
  geom_line(aes(group = marine_site_name, col =marine_site_name)) 
# # scale_x_date(date_breaks = "1 month", date_labels = "%M")+
# facet_wrap(~Units, scales= "free_y")

m2

file.name1 <- "Figures/02_Mytilus_observed_cover.jpg"
ggsave(m2, filename=file.name1, dpi=300, height=5, width=8)

## population - mean % cover included other targetted species

## population per targetted survey
alldata1 <- read.csv("ignore/phototransummarysd_download.csv") %>%
  filter(georegion == "CA South", species_code == "MYTCAL") #%>% ## filter on targetted mytilus surveys and the species code (value for species)
# select(group_code:stderr)

head(alldata1)
unique(alldata1$georegion)


### format date, take sites near newport bay
mytilus1 <- alldata1 %>%
  mutate(Date = ymd(min_survey_date)) %>%
  separate(Date, into = c("Year", "Month", "Day"), remove = F) %>%
  mutate(Year = as.integer(Year))

NPMytilus1 <- mytilus1 %>%
  filter(marine_site_name %in% NPsites) %>%
  group_by(marine_site_name, site_code, marine_common_year, season_name, marine_season_code, Date) %>%
  summarise(MeanPercentCover = mean(average_percent_cover))

NPMytilus1

## plot

m3 <- ggplot(NPMytilus1, aes(x=Date, y=MeanPercentCover), group = marine_site_name, col = marine_site_name) + 
  geom_line(aes(group = marine_site_name, col =marine_site_name)) 
# # scale_x_date(date_breaks = "1 month", date_labels = "%M")+
# facet_wrap(~Units, scales= "free_y")

m3


# Basic population model --------------------------------------------------
## https://rstudio-pubs-static.s3.amazonaws.com/252603_9c6a71110dc74cc7832f154449235f7f.html

# install.packages("primer")
# install.packages("RColorBrewer")
library(primer)
library(RColorBrewer)

N0 = 100 ## start population size
lambda = 1.5 ## growth rate
t = 0:10 ## 0 -10 years

N = N0 * lambda^t ## equation
round(N, 0)

plot(t, N, type = "o", pch = 19, las = 1)

### bvarying the lambda

N0 = 100
lambda = seq(0.6, 1.4, 0.2)
t = 0:10
N = sapply(lambda, function(lambda) N0 * lambda^t)

matplot(t, N, las = 1)

## including stochasticisty

set.seed(2)
rs = rnorm(1000, mean = 0, sd = 0.1)
hist(rs, xlab = "r")

# For consistency of modeling purposes, we want to convert this distribution of growth rates to the 
# discrete population growth rate, λ which can also be expressed as er. So, the distribution of λ
# in this case is:
  
hist(exp(rs), xlab = "lambda", main = "Histogram of lambdas")


N0 = 100  #initial population size
times = 20  #number of years into the future
N = vector(length = times)  #empty vector to store pop. sizes
N[1] = N0  #initial population size should be the first N
lambda = 1.2  #growth rate

# start loop: Take previous year's N and multiply by lambda
for (t in 2:times) {
  N[t] = N[t - 1] * lambda
}

plot(1:times, N, type = "o", las = 1)

set.seed(2)
N0 = 100  #initial population size
times = 20  #number of years into the future
N = vector(length = times)  #empty vector to store pop. sizes
N
N[1] = N0  #initial population size should be the first N

# lambdas--we only need 19 numbers because growth only
# happens between 2 years.
lambda = rlnorm(times - 1, meanlog = 0, sdlog = 0.1)
lambda
# start loop: Take previous year's N and multiply by lambda
for (t in 2:times) {
  N[t] = N[t - 1] * lambda[t - 1]
}
plot(1:times, N, type = "o", las = 1)

## PVA - simple example
### calculating lambda from counts
data(sparrows)
head(sparrows)

counts = sparrows$Count
l = counts[-1]/counts[-length(counts)] ## calculate lambda
round(l, 2)

## distirbuion of growth rates
hist(l, breaks = 20, main = "Histogram of lambdas")

## mean and sd
mean(log(l))
sd(log(l))

## projecting over 20 years
# Let’s just assume that we can estimate the distribution of annual population growth rates λ
# as a log-normal distribution with those mean-logs and sd-logs. With this, we can generate 50 projected λ
# values:
set.seed(2)
sim.l = rlnorm(50, meanlog = mean(log(l)), sdlog = sd(log(l)))
round(sim.l, 2)

# Let’ use this to simulate this mytilus population over the next 20 years (or rather, from 2023 to 2043).
set.seed(2)
time = 21
N0 = cover[length(counts)]
N0
N = vector(length = time)
N[1] = N0

sim.l = rlnorm(time, meanlog = mean(log(l)), sdlog = sd(log(l)))
for (t in 2:time) {
  N[t] = N[t - 1] * sim.l[t - 1]
}
## plot
par(mar = c(4, 4, 1, 4))
plot(1:(time), N, type = "o", las = 1, xaxt = "n")
axis(side = 1, at = c(1, 6, 11, 16, 20), labels = c(2003, 2008, 
                                                    2013, 2018, 2023))


##mulitple simulations
set.seed(450)
sims = 5
outmat = sapply(1:sims, function(x) {
  time = 21
  N0 = 43
  N = vector(length = time)
  N[1] = N0
  sim.l = rlnorm(time, meanlog = mean(log(l)), sdlog = sd(log(l)))
  for (t in 2:time) {
    N[t] = N[t - 1] * sim.l[t - 1]
  }
  N
})
par(mar = c(4, 4, 1, 4))
matplot(1:time, outmat, type = "l", las = 1, lty = 5, ylab = "N", xaxt = "n", xlab = "Year")
axis(side = 1, at = c(1, 6, 11, 16, 20), labels = c(2003, 2008, 
                                                    2013, 2018, 2023))

## extinction risk
# let’s start by doing the simulations in the same way as above, but changing sims=1000 and time=101.

sims = 1000
outmat = sapply(1:sims, function(x) {
  time = 101
  N0 = 43
  N = vector(length = time)
  N[1] = N0
  sim.l = rlnorm(time, meanlog = mean(log(l)), sdlog = sd(log(l)))
  for (t in 2:time) {
    N[t] = N[t - 1] * sim.l[t - 1]
  }
  N
})
outmat
dim(outmat)

# which columns have at least one value less than 2? - 2 needed to reproduce
minpop = apply(outmat, 2, function(x) min(x) < 2)
sum(minpop + 0)/sims  #proportion of columns with TRUE

## Thus, there is approximately a 28% chance that the population will go 
# extinct within 100 years due purely to stochasticity.


# Population calculations on bivalves -------------------------------------
## function to normalise betwen 0-100


min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

set.seed(450)

## create empty DF
exDFx <- data.frame(matrix(nrow=length(sites), ncol = 4))
names(exDFx) <- c("Site", "ExtinctionRate", "GrowthRate", "GrowthSD")
exDFx

## loop around to get extinction risk of all sites

## define sites 

sites <- unique(mytilus$marine_site_name)
sites
s=2

for(s in 1:length(sites)) {
 
  ## filter to one site and season - spring only has 15 years
  mytilusx <- mytilus %>%
    filter(marine_site_name == sites[s], season_name == "Fall")
  names(mytilusx)
  ## plot
  mx1 <- ggplot(mytilusx, aes(x=marine_common_year, y=average_percent_cover)) + 
    geom_line() 

  ## save
  file.name1 <- paste0("Figures/02_Mytilus_", sites[s], "_observed_population_growth.jpg")
  ggsave(mx1, filename=file.name1, dpi=300, height=5, width=8)
  
  ## define per cent cover
  cover = mytilusx$average_percent_cover
  
  l = cover[-1]/cover[-length(cover)]
 
  
  # round(l, 2)
  
  lx <- ifelse( is.na(l), 0.00001, l)
  lx <- ifelse(lx==0, 0.00001, lx)## NAs to 0, 0 observed percent cover
  lx <- ifelse(is.infinite(lx), 1, lx) ## INF to 1 as grows from 0 to 1

  ## distirbuion of growth rates
  hist(lx, breaks = 100, main = "Histogram of lambdas")
  
  ## mean and sd
  
  # mean(log(lx)) ## -2.256036
  # sd(log(lx)) ## 4.932772
  
  ## projecting over 20 years
  sim.l = rlnorm(50, meanlog = mean(log(lx)), sdlog = sd(log(lx)))
  # round(sim.l, 2)
  time = 51 ## time line, 
  N0 = cover[length(cover)] ## cover of first year
  N = vector(length = time)
  N[1] = N0
  sim.l = rlnorm(time, meanlog = mean(log(lx)), sdlog = sd(log(lx)))
  mean(sim.l)
  
  for (t in 2:time) {
    N[t] = N[t - 1] * sim.l[t - 1]
  }
  
  N <- min_max_norm(N)*100 ## not=rmalise to 100
  df <- as.data.frame(N) %>% mutate(Year = seq(2022, 2072, 1)) ## make df
  
  ## plot
  mx <- ggplot(df, aes(x=Year, y=N)) + 
    geom_line() 

  ## save
  file.name1 <- paste0("Figures/02_Mytilus_", sites[s], "_predcited_population_growth.jpg")
  ggsave(mx, filename=file.name1, dpi=300, height=5, width=8)
  
  
  ### simulations
  
  sims = 5
  outmat = sapply(1:sims, function(x) {
    time = 51
    N0 = cover[length(cover)]
    N = vector(length = time)
    N[1] = N0
    sim.l = rlnorm(time, meanlog = mean(log(lx)), sdlog = sd(log(lx)))
    for (t in 2:time) {
      N[t] = N[t - 1] * sim.l[t - 1]
    }
    min_max_norm(N)*100
    
  })
  
  dfsim5 <- as.data.frame(outmat) %>% mutate(Year = seq(2022, 2072, 1)) %>%## make df
    pivot_longer(V1:V5, names_to = "Simulation", values_to = "PercentCover")
  dfsim5
  ## plot
  mx2 <- ggplot(dfsim5, aes(x=Year, y=PercentCover), group = Simulation, colour = Simulation) + 
    geom_line(aes(group = Simulation, colour = Simulation)) 
  mx2
  ## save
  file.name1 <- paste0("Figures/02_Mytilus_", sites[s], "_predcited_population_growth_sim5.jpg")
  ggsave(mx2, filename=file.name1, dpi=300, height=5, width=8)
  
  
  ### extinction risk
  sims = 1000 ## lots of simulations
  outmat = sapply(1:sims, function(x) {
    time = 51
    N0 = cover[length(cover)]
    N = vector(length = time)
    N[1] = N0
    sim.l = rlnorm(time, meanlog = mean(log(lx)), sdlog = sd(log(lx)))
    for (t in 2:time) {
      N[t] = N[t - 1] * sim.l[t - 1]
    }
    min_max_norm(N)*100
    
  })
  
  ## need better measure of pop viability
  ## pop viability = < 5% for 3 years as an example
  
  ConsecYears5 <- as.data.frame(outmat) %>% mutate(Year = seq(2022, 2072,1)) %>%
    pivot_longer(V1:V1000, names_to = "Simulation", values_to = "PercentCover") %>%
    group_by(Simulation) %>%
    mutate(Consec5 = sequence(rle(as.character(PercentCover < 5))$lengths)) %>% ## sequence per < 0.5% occurences
    filter(PercentCover < 0)  %>%
    summarise(MaxConsec = max(Consec5)) ## count max 5% occurence in each sim

  minpop = ConsecYears5$MaxConsec >= 3
  Ex <- sum(minpop + 0)/sims  #proportion of columns with TRUE

  ## make df
  exDFx[s,1] <- sites[s] 
  exDFx[s,2] <- Ex*100 ## extinction rate
  exDFx[s,3] <- exp(round(mean(log(lx)), digits = 2))## growth rate mean
  exDFx[s,4] <- round(sd(log(lx)), digits = 2) ## growth rate SD
  
}

## get juv survival by multiplying by survival rate
exDFx <- exDFx %>%
  mutate(JuvenialSurvival = GrowthRate*0.35) 
  
exDFx


# Start population from biodiversity surveys ------------------------------

Biodiv <- read.csv("ignore/katie_irving_cbs_mussel_per_cover_20230811.csv")
head(Biodiv)

## get percent cover of most recent year
MostRecent <- Biodiv %>%
  group_by(marine_site_name) %>%
  mutate(MostRecentYear = max(year)) %>%
  select(marine_site_name, year, percent_cover, MostRecentYear) %>%
  mutate(MatchYear = ifelse(year == MostRecentYear, "Yes", "No")) %>%
  filter(MatchYear == "Yes")
MostRecent
### add to growth rates etc df

allDF <- inner_join(exDFx, MostRecent, by = c("Site" = "marine_site_name")) %>%
  rename(StartCover = percent_cover) %>%
  select(-MatchYear)
  
allDF

write.csv(allDF, "output_data/02_growth_extinction_Rates.csv")

# SST ---------------------------------------------------------------------

sst <- read.csv("ignore/MARINe_daily_temperature_means_PST.csv") %>%
  filter(georegion == "CA South")
head(sst)
unique(sst$georegion)
# Map sites

### sites 
tempsites <- sst %>%
  select(marine_site_name, latitude, longitude) %>%
  distinct() %>%
  st_as_sf(coords=c( "longitude", "latitude"), crs=4326, remove=F)

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)

## plot points
t1 <- mapview(tempsites, cex=6, col.regions="red",
              layer.name="SST Sites") +
  mapview(sites, cex=6, col.regions="orange",
          layer.name="Mytilus Sites")
# mapview(bio_sub, cex=6, col.regions="blue",
#         layer.name="Toad Observations Other")  


t1
t1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


## join with bio
head(sst)
sstBio <- sst %>%
  select(marine_site_name:longitude, year, mean) %>%
  group_by(year, marine_site_name, latitude, longitude) %>%
  summarise(MeanTemp = mean(mean)) %>%
  full_join(NPMytilus, by = c("year" = "Year"), relationship = "many-to-many")

sstBio
## plot
range(na.omit(sstBio$MeanTemp)) ## temp 14.33195 15.95332
range(na.omit(sstBio$average_percent_cover)) ##  0.0 98.8
## get coeff for 2nd axis
coeff <- 99/21
coeff

m2 <- ggplot(sstBio, aes(x=Date, y=average_percent_cover), group = marine_site_name.y, col = marine_site_name.y) + 
geom_line(aes(group = marine_site_name.y, col =marine_site_name.y)) +
geom_line(aes(y=MeanTemp*coeff), col = "lightblue") +
  scale_y_continuous(
    # Features of the first axis
    name = "Average Percent Cover",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Sea Surface Temperature (c)")) 
# # scale_x_date(date_breaks = "1 month", date_labels = "%M")+
# facet_wrap(~Units, scales= "free_y")

m2


# Air temp ----------------------------------------------------------------
exDFx
## data 

cc <- read.csv("ignore/PRISM_ppt_tmean_stable_4km_2002_2022_33.5708_-117.8377_CrystalCove.csv") %>%
  mutate(Site = "Crystal Cove")
sc <- read.csv("ignore/PRISM_ppt_tmean_stable_4km_2002_2022_33.5448_-117.7994_ShawsCove.csv") %>%
  mutate(Site = "Shaws Cove")
ti <- read.csv("ignore/PRISM_ppt_tmean_stable_4km_2002_2022_33.5134_-117.7579_TreasureIsland.csv") %>%
  mutate(Site = "Treasure Island")
dp <- read.csv("ignore/PRISM_ppt_tmean_stable_4km_2002_2022_33.4599_-117.7147_DanaPoint.csv") %>%
  mutate(Site = "Dana Point")

head(cc)
head(sc)
head(ti)
head(dp)

allTemp <- bind_rows(cc,sc,ti,dp) %>%
  select(-"ppt..inches.")

allTemp_wide <- allTemp %>%
  pivot_wider(names_from = Site, values_from = "tmean..degrees.F.")

allTemp_widecor <- cor(allTemp_wide)


## join with pop data

allTempPop <- full_join(NPMytilus, allTemp, by = c("Year" = "Date", "marine_site_name" = "Site")) %>%
  rename(TempF = 35)
names(allTempPop)

## plot
ggplot(allTempPop, aes(x = TempF, y = average_percent_cover), group = marine_site_name, col = marine_site_name) + 
  geom_smooth(method = "lm", aes(group = marine_site_name, col =marine_site_name)) +
  facet_wrap(~marine_site_name)

median(na.omit(allTempPop$average_percent_cover))
  
## plot time series
range(allTempPop$TempF) ## 59.9 67.0
range(na.omit(allTempPop$average_percent_cover)) ## 0.0 98.8
coeff <- 99/67
coeff

m3 <- ggplot(allTempPop, aes(x=Year, y=average_percent_cover), group = marine_site_name, col = marine_site_name) + 
  geom_line(aes(group = marine_site_name, col =marine_site_name)) +
  geom_line(aes(y=TempF*coeff), col = "lightblue") +
  scale_y_continuous(
    # Features of the first axis
    name = "Average Percent Cover",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Air Temperature (c)")) +
  facet_wrap(~marine_site_name, scales= "free_y")
# # scale_x_date(date_breaks = "1 month", date_labels = "%M")+



m3


file.name1 <- "Figures/02_Mytilus_pop_ait_temp_TSs.jpg"
ggsave(m3, filename=file.name1, dpi=300, height=5, width=8)
names(allTempPop)
## median % cover - then can have probability of increase or decrease from that point

meds <- allTempPop %>%
  group_by(marine_site_name) %>%
  summarise(MedPop = median(na.omit(average_percent_cover))) ## 59.2
meds
## logidtic regression for thresholds of temp


## quantiles of temp
tempquants <- as.data.frame(quantile(allTempPop$TempF))
quants <- tempquants$`quantile(allTempPop$TempF)`[2:4]
quants

## median temp = 63F

allTempPop %>% group_by(marine_site_name) %>% summarise(median(TempF))

## define sites
sitesx <- unique(allTempPop$marine_site_name)
sitesx

##. empty df for results
res <- NULL

## empty df for quantiles
quantx <- NULL
quantx
## loop around sites, model glm predict median cover

for(s in 1:length(sitesx)) {
  
  dat <- allTempPop %>% filter(marine_site_name == sitesx[s])
  
  ## end cover for 1,0 threshold
  endCover <- meds %>% filter(marine_site_name == sitesx[s])
  endCoverThresh <- endCover$MedPop
  
  ## condition convert to increase/decrease
  dat <- dat %>%
    mutate(Cond = ifelse(average_percent_cover < endCoverThresh, 0, 1))
  
  mod <- glm(Cond~TempF, family = "binomial", data = dat)
  modsum <- summary(mod)
  
  newdata <- seq(range(dat$TempF)[1], range(dat$TempF)[2], 0.1)
  
  PredCover <- predict(mod, list(TempF = newdata), type = "response")
  
  resx <- as.data.frame(PredCover)
  resx[,2] <- newdata
  resx[,3] <- sitesx[s]
  
  colnames(resx) <- c("ProbIncrease", "TempF", "Site")
  
  res <- bind_rows(res, resx)
  
  ## probbilities per quantile of temp
  
  Predquants <- as.data.frame(predict(mod, list(TempF = quants), type = "response"))
  Predquants[,2] <- quants
  Predquants[,3] <- sitesx[s]
  
  colnames(Predquants) <- c("ProbQuant", "TempQuant", "Site")
  
  quantx <- bind_rows(Predquants, quantx)
  
}
res
write.csv(res, "output_data/02_airTemp_glms.csv")

write.csv(quantx, "output_data/02_airTemp_glms_quants_probs.csv")
quantx
## plot

p1 <- ggplot(res, aes(x=TempF, y = ProbIncrease)) +
  stat_smooth(method = "loess") +
  facet_wrap(~Site)

p1

file.name1 <- "Figures/02_Air_Temp_GLM.jpg"
ggsave(p1, filename=file.name1, dpi=300, height=5, width=8)

length(allTempPop$TempF)
hist(allTempPop$TempF)
