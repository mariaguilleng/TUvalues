#' @title Myerson value with a priori unions
#'
#' @description
#' Calculate the Myerson value in a communication game with a priori unions.
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players. It can be provided as a vector or as a function.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param unions List of vectors indicating the a priori unions between the
#' players.
#' @param graph_edges Edges of the communication graph of the game. It must be
#' a \code{list} of pairs indicating the connected players.
#' @param method Method used to  calculate the Myerson value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated polynomial
#' calculation based on sampling proposed.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#' @param echo Only used if \code{method} is \code{appro}. Show progress of the
#' approximated calculation.
#'
#' @return The Myerson value for each player.
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

  check_method(method)
  n_players <- check_game(characteristic_func, n_players)
  unions <- check_union(unions, n_players, "unions")
  graph_edges <- check_graph_edges(graph_edges, n_players)
  if (method == "appro") {
    n_rep <- check_n_rep(n_rep)
    check_flag(echo, "echo")
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
