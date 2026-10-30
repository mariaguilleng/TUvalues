#' @title Centroid of the core of the game
#'
#' @description
#' Calculate the centroid of the core of the game if it exists.
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
#' @param method Method used to calculate the core. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated core
#' based on Camacho et al. (2025).
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#' @param echo Only used if \code{method} is \code{appro}. Show progress of the
#' approximated calculation.
#' @param arithmetic Only used if \code{method} is \code{exact}. Arithmetic used
#' in the vertex enumeration: \code{"double"} (floating point) or
#' \code{"rational"} (exact rational arithmetic).
#' @param tol Only used if \code{method} is \code{exact}. Positive tolerance
#' used in floating point comparisons.
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

centroid <- function(characteristic_func, n_players = 0, method = "exact",
                     n_rep = 500, echo = TRUE,
                     arithmetic = c("double", "rational"), tol = 1e-10){

  validate_method(method)
  n_players <- validate_game(characteristic_func, n_players, min_players = 2L)

  if (method == "exact") {
    arithmetic <- match.arg(arithmetic)
    validate_tol(tol)
    core <- core_exact(characteristic_func, n_players, arithmetic, tol)
  } else {
    n_rep <- validate_n_rep(n_rep)
    validate_flag(echo, "echo")
    core <- core_appro(characteristic_func, n_players, n_rep, echo)
  }

  if (is.null(core)) {
    warning("No valid solution found. The core may be empty.")
    return(NULL)
  }

  return(colMeans(core))

}
