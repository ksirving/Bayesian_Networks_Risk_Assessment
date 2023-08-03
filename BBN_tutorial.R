## BBN in R tutorial from https://jacintoarias.github.io/bayesnetRtutorial/#sampling_data_from_a_bayesian_network
## and https://bookdown.org/robertness/causalml/docs/tutorial-probabilistic-modeling-with-bayesian-networks-and-bnlearn.html#the-survey-data-dataset

## packages
library(tidyverse)
library(lubridate)
# install.packages("bnlearn")
# install.packages("networkD3")
install.packages("Rgraphviz") ## not available on R version
library(Rgraphviz) 

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")

library(bnlearn)
library(networkD3)


# Creating structure of Bayesian networks ---------------------------------

## create empty graphs from set of variables
vars <- LETTERS[1:6]
dag  <- empty.graph(vars)
dag

## specify arcs
## The most common are: 

## As a two column (from, to) matrix, setting them via arcs to an existing network:

e <- matrix(
  c("A", "C", "B", "F", "C", "F"),
  ncol = 2, byrow = TRUE,
  dimnames = list(NULL, c("from", "to"))
)
arcs(dag) <- e
dag

## We can also use an adjancecy matrix, and assign it to a dag with amat:

adj <- matrix(
  0L, 
  ncol = 6, 
  nrow = 6,
  dimnames = list(vars, vars)
)
adj["A", "C"] = 1L
adj["B", "F"] = 1L
adj["C", "F"] = 1L
adj["D", "E"] = 1L
adj["A", "E"] = 1L
print(adj)

amat(dag) <- adj
dag


## The last option is to create a formula for a given set of variables. 
## Each node is specified by a pair of brackets [<var_name>]. 
## If the node has a parentset we denote it by | and specify the list of parents separated by colons :.
## We can compute the formula into a bn object with model2network.

dag <- model2network("[A][C][B|A][D|C][F|A:B:C][E|F]")
dag


# Plotting the DAG --------------------------------------------------------

plot(dag)

# Minimal aspects of the plot can be customized as documented in the corresponding help page. 
# Other packages can be used indrectly to plot graphs, bnlearn provides connections with some of 
# them but be aware that some of them might be outdated.
#
# Fortunatelly, graphs are a common data structure and we can find lots of utilities to work with them. 
# One of the most common visualization tools for graphs, and for data in general, is the D3 library from the Javascript domain.
# Thanks to the integration of R shiny with web technologies we can find wonderful ports such as the networkD3 package.
# 
# The next snippet is just a custom function to transform a bn object to the required information to plot a D3 force graph.
# 
# Please install the networkD3, be carefull as this package is currently under beta development and future version could break this code.

plotD3bn <- function(bn) {
  varNames <- nodes(bn)
  # Nodes should be zero indexed!
  links <- data.frame(arcs(bn)) %>%
    mutate(from = match(from, varNames)-1, to = match(to, varNames)-1, value = 1)
  
  nodes <- data.frame(name = varNames) %>%
    mutate(group = 1, size = 30)
  
  networkD3::forceNetwork(
    Links = links,  
    Nodes = nodes,
    Source = "from",
    Target = "to",
    Value = "value",
    NodeID = "name",
    Group = "group",
    fontSize = 20,
    zoom = TRUE,
    arrows = TRUE,
    bounded = TRUE,
    opacityNoHover = 1
  )
}

# We can now plot our last generated BN.

plotD3bn(dag)


# Loading BNs from files --------------------------------------------------

##


#  The bnlearn repo -------------------------------------------------------

# This downloads the RData file from the repository and loads it.
# The bn is loaded into a bn.fit variable called bn
load(url("http://www.bnlearn.com/bnrepository/asia/asia.rda"))
asia <- bn

bn.net(asia)
plotD3bn(asia)
asia

# Now is the time to review the parameters, this prints each node and the asociated probability table. 
# In this case all variables are discrete so the tables would be conditional probability tables.

asia

asia$smoke

bn.fit.barchart(asia$smoke)

dataRaw <- read_csv("./data/heatAlarm-lvl1.csv")

# Fit the BN --------------------------------------------------------------

data(learning.test)
learning.test

# learn the network structure.
res = pc.stable(learning.test)
plot(res)
# set the direction of the only undirected arc, A - B.
res = set.arc(res, "A", "B")
# estimate the parameters of the Bayesian network.
fitted = bn.fit(res, learning.test)
fitted
# replace the parameters of the node B.
new.cpt = matrix(c(0.1, 0.2, 0.3, 0.2, 0.5, 0.6, 0.7, 0.3, 0.1),
                 byrow = TRUE, ncol = 3,
                 dimnames = list(B = c("a", "b", "c"), A = c("a", "b", "c")))
fitted$B = as.table(new.cpt)
# the network structure is still the same.
all.equal(res, bn.net(fitted))

# learn the network structure.
res = hc(gaussian.test)
# estimate the parameters of the Bayesian network.
fitted = bn.fit(res, gaussian.test)
# replace the parameters of the node F.
fitted$F = list(coef = c(1, 2, 3, 4, 5), sd = 3)
# set again the original parameters
fitted$F = lm(F ~ A + D + E + G, data = gaussian.test)

# discrete Bayesian network from expert knowledge.
net = model2network("[A][B][C|A:B]")
net
plot(net)
cptA = matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
cptB = matrix(c(0.8, 0.2), ncol = 2, dimnames = list(NULL, c("GOOD", "BAD")))
cptA
cptC = c(0.5, 0.5, 0.4, 0.6, 0.3, 0.7, 0.2, 0.8)
dim(cptC) = c(2, 2, 2)
cptC
dimnames(cptC) = list("C" = c("TRUE", "FALSE"), "A" =  c("LOW", "HIGH"),
                      "B" = c("GOOD", "BAD"))
cptC
cfit = custom.fit(net, dist = list(A = cptA, B = cptB, C = cptC))
cfit
# for ordinal nodes it is nearly the same.
cfit = custom.fit(net, dist = list(A = cptA, B = cptB, C = cptC),
                  ordinal = c("A", "B"))

# Gaussian Bayesian network from expert knowledge.
distA = list(coef = c("(Intercept)" = 2), sd = 1)
distB = list(coef = c("(Intercept)" = 1), sd = 1.5)
distC = list(coef = c("(Intercept)" = 0.5, "A" = 0.75, "B" = 1.32), sd = 0.4)
cfit = custom.fit(net, dist = list(A = distA, B = distB, C = distC))

# conditional Gaussian Bayesian network from expert knowledge.
cptA = matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
distB = list(coef = c("(Intercept)" = 1), sd = 1.5)
distC = list(coef = matrix(c(1.2, 2.3, 3.4, 4.5), ncol = 2,
                           dimnames = list(c("(Intercept)", "B"), NULL)),
             sd = c(0.3, 0.6))
cgfit = custom.fit(net, dist = list(A = cptA, B = distB, C = distC))




# 2nd tutorial: create the DAG --------------------------------------------

dag <- empty.graph(nodes = c("A","S","E","O","R","T"))
arc.set <- matrix(c("A", "E",
                    "S", "E",
                    "E", "O",
                    "E", "R",
                    "O", "T",
                    "R", "T"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(dag) <- arc.set
nodes(dag)

plot (dag)
graphviz.plot(dag, layout = "dot")
graphviz.plot(dag, layout = "fdp")
graphviz.plot(dag, layout = "circo")

# If you want to change the color of the nodes or the edges of your graph, you can do this easily by 
# adding a highlight input to the graphviz.plot function.
# Let’s assume that we want to change the color of all the nodes and edges of our dag to blue.
hlight <- list(nodes = nodes(dag), arcs = arcs(dag),
               col = "blue", textCol = "blue")
pp <- graphviz.plot(dag, highlight = hlight)

#The look of the arcs can be customised as follows using the edgeRenderInfo function from Rgraphviz.
edgeRenderInfo(pp) <- list(col = c("S~E" = "black", "E~R" = "black"),
                           lwd = c("S~E" = 3, "E~R" = 3))

# Attributes being modified (i.e., col for the colour and lwd for the line width) are specified 
# again as the elements of a list. For each attribute, we specify a list containing the arcs we want 
# to modify and the value to use for each of them. Arcs are identified by labels of the form parent∼child, 
# e.g., S → E is S~E.
# Similarly, we can highlight nodes with nodeRenderInfo. We set their colour and the colour of the node labels to black and their background to grey.

nodeRenderInfo(pp) <- list(col = c("S" = "black", "E" = "black", "R" = "black"),
                           fill = c("E" = "grey"))

# Once we have made all the desired modifications, we can plot the DAG again with the renderGraph 
# function from Rgraphviz.

renderGraph(pp)


# Input probabilities manually --------------------------------------------
# Given the DAG, the joint probability distribution of the survey data variables factorizes as follows:

A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train", "other")

A.prob <- array(c(0.3,0.5,0.2), dim = 3, dimnames = list(A = A.lv))
S.prob <- array(c(0.6,0.4), dim = 2, dimnames = list(S = S.lv))
E.prob <- array(c(0.75,0.25,0.72,0.28,0.88,0.12,0.64,0.36,0.70,0.30,0.90,0.10), 
                dim = c(2,3,2), dimnames = list(E = E.lv, A = A.lv, S = S.lv))
E.prob

# , , S = M
# 
# A
# E      young adult  old
# high  0.75  0.72 0.88
# uni   0.25  0.28 0.12
# 
# , , S = F
# 
# A
# E      young adult old
# high  0.64   0.7 0.9
# uni   0.36   0.3 0.1


O.prob <- array(c(0.96,0.04,0.92,0.08), dim = c(2,2), dimnames = list(O = O.lv, E = E.lv))
R.prob <- array(c(0.25,0.75,0.2,0.8), dim = c(2,2), dimnames = list(R = R.lv, E = E.lv))
T.prob <- array(c(0.48,0.42,0.10,0.56,0.36,0.08,0.58,0.24,0.18,0.70,0.21,0.09), 
                dim = c(3,2,2), dimnames = list(T = T.lv, O = O.lv, R = R.lv))

cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, R = R.prob, T = T.prob)

cpt
