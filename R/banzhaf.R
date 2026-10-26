#' @title Banzhaf value
#'
#' @description
#' Calculate the Banzhaf value
#'
#' @param characteristic_func The valued function defined on the subsets of the
#' number of players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param method Method used to calculate the Banzhaf value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated
#' polynomial calculation based on sampling.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation
#' @param replace Should sampling be with replacement?
#' @param echo Only used if \code{method} is \code{appro}. Show progress of the
#' approximated calculation.
#'
#' @return The Banzhaf value for each player
#'
#' @examples
#' n <- 8
#' v <- function(coalition) {
#' if (length(coalition) > n/2) {
#'    return(1)
#'  } else {
#'    return(0)
#'  }
#' }
#' banzhaf(v, method = "exact", n_players = n)
#' banzhaf(v, method = "appro", n_rep = 2000, n_players = n, replace = TRUE)
#'
#' @examples
#' v<-c(0,0,0,1,2,1,3)
#' banzhaf(v, method = "exact")
#' banzhaf(v, method = "appro", n_rep = 2000, replace = TRUE)
#'
#' @export


banzhaf <- function(characteristic_func, n_players = 0, method = "exact", n_rep = 10000,
                    replace = FALSE, echo = TRUE){

  validate_method(method)
  n_players <- validate_game(characteristic_func, n_players)

  if (method == "exact") {
    return(banzhaf_exact(characteristic_func, n_players))
  } else {
    n_rep <- validate_n_rep(n_rep)
    validate_flag(replace, "replace")
    validate_flag(echo, "echo")
    return(banzhaf_appro(characteristic_func, n_players, n_rep, replace, echo))
  }

}
