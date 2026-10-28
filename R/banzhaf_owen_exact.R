#' @title Banzhaf-Owen value (exact)
#' @noRd
#'
#' @description Calculate the exact Banzhaf-Owen value with
#' \code{exact_unions}, which only evaluates the coalitions formed by a subset
#' of the members of one union together with complete unions.
#' Internal function called by \code{banzhaf_owen}, which checks the arguments.
#'
#' @param characteristic_func The characteristic function of the game, already
#' checked: a numeric vector of length \eqn{2^n - 1} or \eqn{2^n} in the order
#' given by \code{coalitions}, or a function that receives a coalition.
#' @param union The a priori unions, already checked: a list of integer
#' vectors that form a partition of the players.
#' @param n_players The number of players, already checked. Only used if
#' \code{characteristic_func} is a function.
#'
#' @return The Banzhaf-Owen value for each player.

banzhaf_owen_exact <- function(characteristic_func, union, n_players) {
  return(exact_unions(characteristic_func, union, n_players, "banzhaf"))
}
