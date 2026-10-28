#' @title Myerson value with a priori unions
#'
#' @description
#' Calculate the Myerson value in a communication game with a priori unions.
#'
#' @param characteristic_func The characteristic function of the game. It can be
#' a numeric vector with the value of every coalition, in the order given by
#' \code{\link{coalitions}}: of length \eqn{2^n - 1}, without the empty
#' coalition (whose value is taken as 0), or of length \eqn{2^n}, with the
#' empty coalition first. Vectors support up to 30 players. It can also be a
#' function that receives a coalition (a vector of players) and returns its
#' value; in that case there is no limit on the number of players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param unions List of vectors indicating the a priori unions between the
#' players.
#' @param graph_edges Edges of the communication graph of the game: a
#' \code{list} of pairs of players (for example \code{list(c(1, 2), c(2, 3))}),
#' or a two-column matrix or data frame. Each edge must join two different
#' players.
#' @param method Method used to calculate the Myerson value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated polynomial
#' calculation, applying to the communication game the sampling procedure of the
#' Owen value proposed by Saavedra-Nieves et al. (2018).
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#' @param echo Only used if \code{method} is \code{appro}. Show progress of the
#' approximated calculation.
#'
#' @return The Myerson value for each player.
#'
#' @references Myerson, R. B. (1977). Graphs and Cooperation in Games.
#' Mathematics of Operations Research, 2(3), 225--229.
#' \doi{10.1287/moor.2.3.225}
#'
#' Saavedra-Nieves, A., García-Jurado, I., & Fiestras-Janeiro, M. G. (2018).
#' Estimation of the Owen value based on sampling. In The mathematics of the uncertain:
#' A tribute to Pedro Gil (pp. 347-356). Cham: Springer International Publishing.
#'
#' @examples
#' v <- c(
#'   0, 0, 0, 0,
#'   1, 1, 1, 0, 0, 0,
#'   1, 1, 1, 1,
#'   1
#' )
#' graph_edges <- list(c(2,3),c(3,1),c(1,4))
#' unions <- list(c(2,3),c(1),c(4))
#' myerson_unions(v, unions = unions, graph_edges = graph_edges, method = "exact")
#' myerson_unions(v, unions = unions, graph_edges = graph_edges, method = "appro", n_rep = 2000)
#'
#' @export

myerson_unions <- function(characteristic_func, n_players = 0, unions,
                           graph_edges,  method = "exact", n_rep = 10000, echo = TRUE){

  validate_method(method)
  n_players <- validate_game(characteristic_func, n_players)
  unions <- validate_union(unions, n_players, "unions")
  graph_edges <- validate_graph_edges(graph_edges, n_players)
  if (method == "appro") {
    n_rep <- validate_n_rep(n_rep)
    validate_flag(echo, "echo")
  }

  # Graph-restricted game
  game <- myerson_graph_game(characteristic_func, graph_edges, n_players, method)

  # Owen value of the graph-restricted game
  if (method == "exact") {
    owen_value <- owen_exact(game$characteristic_func, unions, game$n_players)
  } else {
    owen_value <- owen_appro(game$characteristic_func, unions, game$n_players, n_rep, echo)
  }

  return(owen_value)

}
