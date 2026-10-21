#' @title Banzhaf-Owen Value
#'
#' @description Calculate the exact Banzhaf-Owen value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param union List of vectors indicating the a priori unions between the
#' players
#' @param n_players The number of players in the game.
#'
#' @return The Banzhaf Index for each player

banzhaf_owen_exact <- function(characteristic_func, union, n_players) {
  return(exact_unions(characteristic_func, union, n_players, "banzhaf"))
}
