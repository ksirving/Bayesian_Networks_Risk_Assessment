
## packages
library(tidyverse)
library(tidylog)

## exposure data

data <- read.csv("ignore/MP_SW_Exposure_V2.csv") #
manta <- unique(data$sample_method)[c(1,3,4)]


data <- data %>%
  filter(sample_method %in% manta)


data

MP.prob <- array(c(0.45, 0.45, 0.1, ## Spring: <100 = 0.45, 100-100,000 = 0.45 , over 100,000 = 0.1
                   0.45, 0.45, 0.1), ## Fall: <100 = 0.45, 100-100,000 = 0.45 , over 100,000 = 0.1
                 dim = c(3,2), dimnames = list(MPConc = MP.lv,  Season = seas.lv))

MP.prob
wdat <- c(188, 161, 84, 52, 84, 114, 701, 414, 809, 641, 45, 48, 97, 63)
# min = 1, max=35
min(wdat)
max(wdat)
median(wdat)

## 1-
## 45-100
800/3
quantile(wdat)

 (6+49)/63 # 0.8730159
5/63 # 0.07936508
4/63 # 0.06349206

# 0%    25%    50%    75%   100% 
# 45.00  68.25 105.50 357.50 809.00 

## 49 from other paper 