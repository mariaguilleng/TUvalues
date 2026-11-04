###############################################################################
# Usage example of TUvalues shown in the paper (Section 3.3.2): approximate
# computation of the allocation rules for the 47-player Zerkani network.
#
# Run from the repository root with:  Rscript examples/approximate_example.R
###############################################################################

## ----load_pkg, echo = TRUE----------------------------------------------------
library(TUvalues)


## ----components, echo=TRUE--------------------------------------------------
# Connected components of the subgraph induced by coalition
components <- function(coalition) {
  pending <- coalition
  result <- list()
  while (length(pending) > 0) {
    component <- pending[1]
    frontier <- pending[1]
    while (length(frontier) > 0) {
      frontier <- setdiff(intersect(unlist(neighbours[frontier]), coalition),
                          component)
      component <- c(component, frontier)
    }
    result <- c(result, list(component))
    pending <- setdiff(pending, component)
  }
  result
}


## ----f, echo=TRUE-----------------------------------------------------------
# Worth f(T) of a connected coalition T
f <- function(T) {
  if (length(T) == 1) return(players_weights[T])
  inside <- edges[, 1] %in% T & edges[, 2] %in% T
  sum(players_weights[T]) * max(graph_weights[inside])
}


## ----vAppro, echo=TRUE--------------------------------------------------------
# Characteristic function: worth of the best connected component
v <- function(coalition) {
  if (length(coalition) == 0) return(0)
  max(vapply(components(coalition), f, numeric(1)))
}


## ----approConfg, echo = TRUE--------------------------------------------------
n_players <- 47
graph_edges <- list(
  c(1,5), c(1,8), c(1,9), c(1, 11), c(1, 12), c(1,15), c(1, 19),
  c(1, 27), c(1, 31), c(1, 32), c(1, 33), c(1, 34),
  c(2, 26),
  c(3, 27), c(3, 10),
  c(4, 9), c(4, 29), c(4, 35),
  c(5, 27), c(5, 36), c(5, 37),
  c(6, 17), c(6, 27), c(6, 38),
  c(7, 26),
  c(8, 12), c(8, 13),
  c(9, 29), c(9, 31), c(9, 35), c(9, 39),
  c(10, 15),
  c(11, 13), c(11, 15),
  c(12, 19),
  c(13, 15), c(13, 28),
  c(14, 19), c(14, 21), c(14, 27), c(14, 40),
  c(15, 21), c(15, 26), c(15, 27), c(15, 28), c(15, 41),
  c(16, 26), c(16, 42),
  c(17, 21), c(17, 27), c(17, 28), c(17, 38), c(17, 43),
  c(18, 19), c(18, 21), c(18, 27), c(18, 46), c(18, 44),
  c(19, 21), c(19, 27), c(19, 45), c(19, 46),
  c(20, 27), c(20, 33),
  c(22, 27), c(22, 44),
  c(23, 36), c(23, 37),
  c(24, 36), c(24, 37),
  c(25, 26),
  c(26, 30),
  c(27, 47),
  c(28, 30), c(28, 32),
  c(29, 35)
)

players_weights <- rep(1,n_players)
players_weights[16] <- 2
players_weights[19] <- 3
players_weights[c(1,4)] <- 4
players_weights[15] <- 5

graph_weights <- rep(3,length(graph_edges))
graph_weights[c(5,6,12,15,31,36,42,46,48,52)] <- 2
graph_weights[c(55,60)] <- 5

unions <- list(
  c(1,9,15), #Comanders and recruiters
  c(8,11,12,10,22,28,32,33,29,35,5,4), #Associates of
  c(31,34,39,13,41,21,44), #Recruited or under the command of a commander
  c(23,24,36,37), #Terrorists that traveled together
  c(20,47,17,3),  #They traveled with Salah Abdeslam
  c(19,27,18,43), #Traveled, lived together or were associated before the Paris attack
  c(26,25,30,2,16,7,42), #Members of the Reda Kriket's cell
  c(14,40), #Associated in Forging ring
  c(46,45), #Arrested in Forest
  c(38,6) #Associated
)


## ----auxAppro, echo=FALSE-----------------------------------------------------
# Edges as a two-column matrix and neighbours of each player
edges <- do.call(rbind, graph_edges)
neighbours <- lapply(seq_len(n_players), function(i) {
  c(edges[edges[, 1] == i, 2], edges[edges[, 2] == i, 1])
})


## ----approSimple, echo=TRUE---------------------------------------------------
# The seed is set before each sampling-based rule, so that every result can be
# reproduced independently of the order in which the rules are computed
set.seed(123)
shapley(v, n_players = n_players, method = "appro", n_rep = 1000, echo = T)
set.seed(123)
banzhaf(v, n_players = n_players, method = "appro", n_rep = 1000, echo = T)
egalitarian(v,n_players)
equal_surplus_division(v,n_players)


## ----approUnions, echo=TRUE---------------------------------------------------
set.seed(123)
owen(v,union = unions,n_players = n_players, method = "appro", n_rep = 1000, echo = T)
set.seed(123)
banzhaf_owen(v,union = unions,n_players = n_players, method = "appro", n_rep = 1000, echo = T)
egalitarian_unions(v,unions,n_players)
equal_surplus_division_unions(v,unions,n_players, type = 1)
equal_surplus_division_unions(v,unions,n_players, type = 2)
equal_surplus_division_unions(v,unions,n_players, type = 3)


## ----approComm, echo=TRUE-----------------------------------------------------
set.seed(123)
myerson(v, graph_edges = graph_edges, n_players = n_players,
        method = "appro", n_rep = 1000, echo = T)
set.seed(123)
myerson_unions(v, graph_edges = graph_edges, union = unions, n_players = n_players,
               method = "appro", n_rep = 1000, echo = T)
