#' @title Shapley value
#'
#' @description
#' Calculate the Shapley value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param method Method used to  alculate the Shapley value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated polynomial
#' calculation based on sampling proposed by Castro et al. (2009).
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#' @param echo Only used if \code{method} is \code{appro}. Show progress of the
#' approximated calculation.
#'
#' @return The Shapley value for each player.
#'
#' @references Castro, J., Gómez, D., & Tejada, J. (2009). Polynomial calculation
#' of the Shapley value based on sampling. Computers & operations research,
#' 36(5), 1726-1730.
#'
#' @examples
#' n <- 10
#' v <- function(coalition) {
#' if (length(coalition) > n/2) {
#'    return(1)
#'  } else {
#'    return(0)
#'  }
#' }
#' shapley(v, method = "appro", n_rep = 4000, n_players = n)
#'
#' @examples
#' n <- 3
#' v <- c(1,1,2,1,2,2,2)
#' shapley(v, method = "exact")
#' shapley(v, method = "appro", n_rep = 4000)
#'
#' @export

shapley <- function(characteristic_func, n_players = 0, method = "exact", n_rep = 10000,
                    echo = TRUE){

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
      stop("Invalid number of iterations specified. n_rep must be greater than 0.")
    } else if (is.function(characteristic_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    return(shapley_appro(characteristic_func, n_players, n_rep, echo))
  }

}


