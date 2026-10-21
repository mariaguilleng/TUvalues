#' @title Owen value (exact)
#'
#' @description Calculate the exact Owen
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param union List of vectors indicating the a priori unions between the
#' players
#' @param n_players The number of players
#'
#' @return The Owen value for each player

owen_exact <- function(characteristic_func, union, n_players = NULL) {
  return(exact_unions(characteristic_func, union, n_players, "owen"))
}
