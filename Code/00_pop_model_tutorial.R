### tutorial of basic pop model using lambda


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

### varying the lambda

N0 = 100
lambda = seq(0.6, 1.4, 0.2)
t = 0:10
N = sapply(lambda, function(lambda) N0 * lambda^t)

matplot(t, N, las = 1)

## including stochasticity

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

