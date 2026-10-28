#' @title Banzhaf value
#'
#' @description
#' Calculate the Banzhaf value
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
#' @param method Method used to calculate the Banzhaf value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated
#' polynomial calculation based on sampling.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation
#' @param replace Only used if \code{method} is \code{appro}. Should sampling
#' be with replacement? Without replacement, \code{n_rep} is reduced, if
#' necessary, to the number of different coalitions that can be sampled for
#' each player.
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
