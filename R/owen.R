#' @title Owen value
#'
#' @description
#' Calculate the Owen value
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#' @param union List of vectors indicating the apriori unions between the
#' players
#' @param method Method used to calculate the Owen value. Valid methods are:
#' \code{exact} for the exact value or \code{appro} for approximated polynomial
#' calculation based on sampling.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation
#' @param n_players Only used if \code{value_func} is a \code{function}.
#' The number of players in the game.
#'
#' @return The Owen value for each player
#'
#' @export

owen  <- function(value_func, union, method = "exact", n_rep = 10000, n_players = 0){

  browser()
  if (! method %in% c("exact", "appro")) {
    stop("Invalid methos specified\n Use \"exact\" for the exact value or \"appro\" for the approximation.")
  }

  if(!is.vector(value_func) || is.function(value_func)) {
    stop("Invalid value_func provided.")
  }

  if (method == "exact") {
    if (is.function(value_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    return(owen_exact(value_func, union, n_players))
  } else {
    if (n_rep < 1) {
      stop("Invalid number of iterations specified. m must be greater than 0.")
    } else if (is.function(value_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    return(owen_appro(value_func, union, n_players, n_rep))
  }

}
