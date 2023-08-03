### bayesian class
203+122
203/325
# Module 1 ----------------------------------------------------------------



## marble bags
BagA <- 1/3
BagB <- 1/3
BagC <- 1/3

BagABlue <- 3/5
BagARed <- 2/5

BagBBlue <- 1/6
BagBRed <- 5/6

BagCBlue <- 0/3
BagCRed <- 3/3
total <- 14
blue <- 4/14
red <- 10/14


BagABlue*BagA
BagBBlue*BagB
BagCBlue*BagC

0.2+0.06+0
## 0.26



pABlue1 <- BagABlue*blue

pABlue2a <- pABlue1
pABlue2b <- (1-pABlue1)*(1-blue)

pABlue1
pABlue2a
pABlue2b

pABlue3 <- pABlue2a+pABlue2b
pABlue3

pABlue4 <- pABlue1/pABlue3
pABlue4

## find p(C | red)

pCRed1 <- BagCRed*red
pCRed1

pCRed2a <- pCRed1
pCRed2b <- (1-pCRed1)*(1-red)

pCRed1
pCRed2a
pCRed2b

pCRed3 <- pCRed2a+pCRed2b
pCRed3

pCRed4 <- pCRed1/pCRed3
pCRed4


# Lesson 3: distributions----------------------------------------------------------------

0 * 0.5
1*0.2
2*0.2
3*0.1

# To calculate the probability of a specific value for a random variable following a binomial distribution, 
# you can use the probability mass function (PMF) formula for the binomial distribution:
#   
#   P(X = k) = C(n, k) * p^k * (1 - p)^(n - k)
# 
# where:
#   
# P(X = k) is the probability of getting exactly k successes in n trials.
# C(n, k) is the binomial coefficient, which represents the number of ways to choose k successes out of n trials, and is calculated as C(n, k) = n! / (k! * (n - k)!).
# p is the probability of success in a single trial.
# n is the total number of trials.
# k is the number of successes.
# In this case, X ~ Binomial(3, 0.2), which means the random variable X follows a binomial distribution 
# with n = 3 (total number of trials) and p = 0.2 (probability of success in each trial).
# 
# To calculate P(X = 0):
#   
#   P(X = 0) = C(3, 0) * (0.2)^0 * (1 - 0.2)^(3 - 0)
# = 1 * 1 * 0.8^3
# = 0.8^3
# = 0.512
# 
# So, the probability of X being equal to 0 in this binomial distribution is 0.512 or 51.2%.

1*1*(0.8)^3

0.2^2
3*0.04*0.8
2*0.2*0.8
0.09+0.51+0.32

0.8^1
2*0.04*0.8
0.8*0.8
3*0.2*0.64
0.064+0.51+0.38
