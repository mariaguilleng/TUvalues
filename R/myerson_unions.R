#' @title Myerson value with a priori unions
#'
#' @description
#' Calculate the Myerson value in a communication game with a priori unions.
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players. It can be provided as a vector or as a function.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param graph_edges Edges of the communication graph of the game. It must be
#' a \code{list} of pairs indicating the connected players.
#' @param union List of vectors indicating the a priori unions between the
#' players.
#' @param method Method used to  calculate the Myerson value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated polynomial
#' calculation based on sampling proposed.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#'
#' @return The Myerson value for each player.
#'
#' @examples
#' characteristic_func <- c(
#' 1, 2, 0, 3,
#' 3, 1, 4, 2, 5, 3,
#' 3, 6, 4, 5,
#' 15
#' )
#' graph_edges <- list(c(1, 2), c(2, 4))
#' myerson(characteristic_func, graph_edges, method = "exact")
#' myerson(characteristic_func, graph_edges, method = "appro", n_rep = 100)
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
#' myerson_unions(v, graph_edges, unions, method = "exact")
#' myerson_unions(v, graph_edges, unions, method = "appro", n_rep = 1000)
#'
#' @export

myerson_unions <- function(characteristic_func, n_players = 0, unions,
                           graph_edges,  method = "exact", n_rep = 10000){

  if (! method %in% c("exact", "appro")) {
    stop("Invalid methos specified\n Use \"exact\" for the exact value or \"appro\"
         for the approximation.")
  }

  if(!is.vector(characteristic_func) && !is.function(characteristic_func)) {
    stop("Invalid characteristic_func provided.")
  }

  if (is.function(characteristic_func) && n_players < 2) {
    stop("Invalid number of players specified. n_players must be greater than 1.")
  }

  if (method == "appro" & n_rep < 1) {
    stop("Invalid number of iterations specified. n_rep must be greater than 0.")
  }

  if (is.vector(characteristic_func)) {

    # get number of players
    n_players<-log(length(characteristic_func),2)
    if (n_players!=round(n_players)){
      characteristic_func <- c(0, characteristic_func)
      n_players<-log(length(characteristic_func),2)
    }
    characteristic_func <- characteristic_func[-1]

  } else if (is.function(characteristic_func)) {
    # Calculate v_coalitions if function is provided
    results_by_size <- lapply(X = 1:n_players, FUN = function(m) {
      return(combn(x = 1:n_players, m = m, FUN = characteristic_func))
    })
    characteristic_func <- unlist(results_by_size)
  }

  # Get the modified characteristic function for the graph game
  characteristic_func_graph <- get_characteristic_func_graph(characteristic_func, n_players)

  # Calculate Shapley value using the modified characteristic function
  if (method == "exact") {
    owen_value <- owen_exact(characteristic_func_graph, unions, n_players)
  } else {
    owen_value <- owen_appro(characteristic_func_graph, unions, n_players, n_rep)
  }

  return(owen_value)

}
