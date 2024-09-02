#' @title Owen value
#'
#' @description
#' Calculate the Owen value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players.
#' @param union List of vectors indicating the apriori unions between the
#' players.
#' @param method Method used to calculate the Owen value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated polynomial
#' calculation based on sampling.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#' @param n_players The number of players in the game.
#'
#' @return The Owen value for each player.
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
#' owen(v, union = u, method = "exact", n_players = n)
#' owen(v, union = u, method = "appro", n_rep = 10000, n_players = n)
#'
#' @examples
#' characteristic_func <- c(1,1,2,1,2,2,2)
#' union <- list(c(1,2),c(3))
#' owen(characteristic_func, union)
#' owen(characteristic_func, union, method = "appro", n_rep = 10000)
#'
#' @export

owen  <- function(characteristic_func, union, method = "exact", n_rep = 10000, n_players = 0){

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
    return(owen_exact(characteristic_func, union, n_players))
  } else {
    if (n_rep < 1) {
      stop("Invalid number of iterations specified. m must be greater than 0.")
    } else if (is.function(characteristic_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    return(owen_appro(characteristic_func, union, n_players, n_rep))
  }

}
