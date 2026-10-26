#' @title Owen value
#'
#' @description
#' Calculate the Owen value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players.
#' @param union List of vectors indicating the a priori unions between the
#' players.
#' @param n_players The number of players in the game.
#' @param method Method used to calculate the Owen value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated polynomial
#' calculation based on sampling proposed by Saavedra-Nieves et al. (2018).
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#' @param echo Only used if \code{method} is \code{appro}. Show progress of the
#' approximated calculation.
#'
#' @return The Owen value for each player.
#'
#' @references Saavedra-Nieves, A., García-Jurado, I., & Fiestras-Janeiro, M. G. (2018).
#' Estimation of the Owen value based on sampling. In The mathematics of the uncertain:
#' A tribute to Pedro Gil (pp. 347-356). Cham: Springer International Publishing.
#'
#' @examples
#' n <- 10
#' v <- function(coalition) {
#'   if (length(coalition) > n/2) {
#'     return(1)
#'   } else {
#'     return(0)
#'   }
#' }
#' u <- lapply(1:(n/2), function(i) c(2*i - 1, 2*i))
#' owen(v, union = u, method = "appro", n_rep = 4000, n_players = n)
#'
#' @examples
#' characteristic_func <- c(1,1,2,1,2,2,2)
#' union <- list(c(1,2),c(3))
#' owen(characteristic_func, union)
#' owen(characteristic_func, union, method = "appro", n_rep = 4000)
#'
#' @export

owen  <- function(characteristic_func, union, n_players = 0, method = "exact", n_rep = 10000, echo = TRUE){

  validate_method(method)
  n_players <- validate_game(characteristic_func, n_players)
  union <- validate_union(union, n_players)

  if (method == "exact") {
    return(owen_exact(characteristic_func, union, n_players))
  } else {
    n_rep <- validate_n_rep(n_rep)
    validate_flag(echo, "echo")
    return(owen_appro(characteristic_func, union, n_players, n_rep, echo))
  }

}
