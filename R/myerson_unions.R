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
    # get coalitions
    coa_set <- coalitions(n_players)[[2]]
  }

  # Adjacency Matrix from edges
  G_adj <- get_adj_matrix(n_players, graph_edges)

  # Calculate Graph Characteristic Function
  characteristic_func_graph <- function(coalition) {
    if (length(coalition) == 0) {
      return(0)
    }
    components <- get_connected_components(coalition, G_adj, n_players)
    v_s <- 0
    for (comp in components) {
      #browser()
      if (is.function(characteristic_func)) {
        v_s <- v_s + characteristic_func(comp)
      } else {
        comp_string <-  toString(comp)
        v_s <- v_s + characteristic_func[which(coa_set == comp_string)]
      }
    }
    return(v_s)
  }


  # Calculate Shapley value using the modified characteristic function
  if (method == "exact") {
    owen_value <- owen_exact(characteristic_func_graph, unions, n_players)
  } else {
    owen_value <- owen_appro(characteristic_func_graph, unions, n_players, n_rep)
  }

  return(owen_value)

}
