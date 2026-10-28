###############################################################################
# Usage example of TUvalues shown in the paper (Section 3.3.1): exact
# computation of the allocation rules and the core for a four-player network.
#
# Run from the repository root with:  Rscript examples/exact_example.R
###############################################################################

## ----load_pkg, echo = TRUE----------------------------------------------------
library(TUvalues)


## ----setupExactExample, echo=TRUE---------------------------------------------
n_players <- 4
v <- c(1, 1, 1, 10, 6, 1, 10, 2, 10, 11, 9, 10, 11, 12, 39)
edges <- list(c(1,2), c(2,3), c(3,4))
unions <- list(c(1,2), c(3), c(4))


## ----coalition, echo=TRUE-----------------------------------------------------
coalitions(n_players)


## ----excatSimple, echo=TRUE---------------------------------------------------
shapley(v, method = "exact")
banzhaf(v, method = "exact")
egalitarian(v)
equal_surplus_division(v)


## ----coreExact, echo=TRUE-----------------------------------------------------
coreVertex(v, method = "exact")
centroid(v, method = "exact")


## ----exactUnions, echo=TRUE---------------------------------------------------
owen(v, union = unions, method = "exact")
banzhaf_owen(v, union = unions, method = "exact")
egalitarian_unions(v, unions, n_players)
equal_surplus_division_unions(v, unions, type = 1)
equal_surplus_division_unions(v, unions, type = 2)
equal_surplus_division_unions(v, unions, type = 3)


## ----exactComm, echo=TRUE-----------------------------------------------------
myerson(v, graph_edges = edges, method = "exact")
myerson_unions(v, graph_edges = edges, unions = unions, method = "exact")
