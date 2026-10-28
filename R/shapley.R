#' @title Shapley value
#'
#' @description
#' Calculate the Shapley value
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
#' @param method Method used to calculate the Shapley value. Valid methods are:
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

  validate_method(method)
  n_players <- validate_game(characteristic_func, n_players)

  if (method == "exact") {
    return(shapley_exact(characteristic_func, n_players))
  } else {
    n_rep <- validate_n_rep(n_rep)
    validate_flag(echo, "echo")
    return(shapley_appro(characteristic_func, n_players, n_rep, echo))
  }

}


