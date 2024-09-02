#' @title Shapley value
#'
#' @description
#' Calculate the Shapley value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param method Method used to calculate the Shapley value. Valid methods are:
#' \code{exact} for the exact value or \code{appro} for approximated polynomial
#' calculation based on sampling.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#'
#' @return The Shapley value for each player
#'
#' @example
#' n <- 14
#' v <- function(coalition) {
#' if (length(coalition) > n/2) {
#'    return(1)
#'  } else {
#'    return(0)
#'  }
#' }
#' shapley(v, method = "exact", n_players = n)
#' shapley(v, method = "appro", n_rep = 20000, n_players = n)
#'
#' @export

shapley <- function(characteristic_func, method = "exact", n_rep = 10000, n_players = 0){

  if (! method %in% c("exact", "appro")) {
    stop("Invalid methos specified\n Use \"exact\" for the exact value or \"appro\" for the approximation.")
  }

  if(!is.vector(characteristic_func) && !is.function(characteristic_func)) {
    stop("Invalid characteristic_func provided.")
  }

  if (method == "exact") { # exact
    if (is.function(characteristic_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    return(shapley_exact(characteristic_func, n_players))
  } else {
    if (n_rep < 1) { #appro
      stop("Invalid number of iterations specified. m must be greater than 0.")
    } else if (is.function(characteristic_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    return(shapley_appro(characteristic_func, n_players, n_rep))
  }

}


