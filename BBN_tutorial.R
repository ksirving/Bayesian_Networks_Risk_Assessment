## BBN in R tutorial from https://jacintoarias.github.io/bayesnetRtutorial/#sampling_data_from_a_bayesian_network

## packages
library(tidyverse)
library(lubridate)
# install.packages("bnlearn")
install.packages("networkD3")
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
