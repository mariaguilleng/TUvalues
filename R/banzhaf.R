#' @title Banzhaf value
#'
#' @description
#' Calculate the Banzhaf value
#'
#' @param characteristic_func The valued function defined on the subsets of the
#' number of players.
#' @param method Method used to calculate the Banzhaf value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated
#' polynomial calculation based on sampling.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param replace should sampling be with replacement?
#'
#' @return The Banzhaf value for each player
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
#' banzhaf(v, method = "exact", n_players = n)
#' banzhaf(v, method = "appro", n_rep = 4000, n_players = n, replace = TRUE)
#'
#' @examples
#' v<-c(0,0,0,1,2,1,3)
#' banzhaf(v, method = "exact")
#' banzhaf(v, method = "appro", n_rep = 4000, replace = TRUE)
#'
#' @export

banzhaf <- function(characteristic_func, method = "exact", n_rep = 10000, n_players = 0,
                    replace = FALSE){

  if (! method %in% c("exact", "appro")) {
    stop("Invalid methos specified\n Use \"exact\" for the exact value or \"appro\" for the approximation.")
  }

  if(!is.vector(characteristic_func) && !is.function(characteristic_func)) {
    stop("Invalid characteristic_func provided.")
  }

  if (method == "exact") {
    if (is.function(characteristic_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    return(banzhaf_exact(characteristic_func, n_players))
  } else {
    if (n_rep < 1) {
      stop("Invalid number of iterations specified. m must be greater than 0.")
    } else if (is.function(characteristic_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    return(banzhaf_appro(characteristic_func, n_players, n_rep, replace))
  }

}
