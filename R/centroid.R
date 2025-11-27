#' @title Centroid of the core of the game
#'
#' @description
#' Calculate the centroid of core of the game if it exits.
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param method Method used to calculate the core. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated core
#' based on Camacho et al. (2025).
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#'
#' @return The centroid of the core if it exists.
#'
#' @references Camacho, J., Gonçalves-Dosantos, J. C., & Sánchez-Soriano, J. (2025).
#' A Linear Programming Approach to Estimate the Core in Cooperative Games.
#' arXiv preprint arXiv:2510.01766.
#'
#' @examples
#' v <- c(2,3,5,5,7,8,10)
#' centroid(v, method = "exact")
#' centroid(v, method = "appro", n_rep = 100)
#'
#' @examples
#' n <- 3
#' v <- function(coalition) {
#'  size <- length(coalition)
#'  if (size <= 1) {
#'    return(0)
#'  } else if (size == 2) {
#'    return(10)
#'  } else if (size == 3) {
#'    return(24)
#'  } else {
#'    return(0)
#'  }
#' }
#' centroid(v, n, method = "exact")
#' centroid(v, n, method = "appro", n_rep = 200)
#'
#' @export

centroid <- function(characteristic_func, n_players = 0, method = "exact", n_rep = 1000){

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
    core <- core_exact(characteristic_func, n_players)
  } else {
    if (n_rep < 1) { #appro
      stop("Invalid number of iterations specified. n_rep must be greater than 0.")
    } else if (is.function(characteristic_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    core <- core_appro(characteristic_func, n_players, n_rep)
  }

  if (is.null(core)) {
    warning("No valid solution found. The core may be empty.")
    return(NULL)
  }

  return(colMeans(core))

}
