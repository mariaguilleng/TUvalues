#' @title Banzhaf value
#'
#' @description
#' Calculate the Banzhaf value
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#' @param method Method used to calculate the Banzhaf value. Valid methods are:
#' \code{exact} for the exact value or \code{appro} for approximated polynomial
#' calculation based on sampling.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation
#' @param n_players Only used if \code{value_func} is a \code{function}.
#' The number of players in the game.
#' @param replace should sampling be with replacement?
#'
#' @return The Banzhaf value for each player
#'
#' @export

banzhaf <- function(value_func, method = "exact", n_rep = 10000, n_players = 0,
                    replace = FALSE){

  if (! method %in% c("exact", "appro")) {
    stop("Invalid methos specified\n Use \"exact\" for the exact value
         or \"appro\" for the approximation.")
  }

  if (method == "exact") {
    if (class(value_func) == "numeric") {
      return(banzhaf_exact_vector(value_func))
    } else if (class(value_func) == "function") {
      if (n_players < 2) {
        stop("Invalid numer of player specified. n_players must be greater
             than 1.")
      }
      return(banzhaf_exact_func(value_func, n_players))
    }
  } else {
    if (n_rep < 1) {
      stop("Invalid number of iterations specified. m must be grater than 0.")
    }
    if (class(value_func) == "numeric") {
      return(banzhaf_appro_vector(value_func,n_rep))
    } else if (class(value_func) == "function") {
      if (n_players < 2) {
        stop("Invalid numer of player specified. n_players must be greater
             than 1.")
      }
      return(banzhaf_appro_func(value_func,n_rep,n_players,replace))
    }
  }

  stop("Invalid value_func provided.")

}
