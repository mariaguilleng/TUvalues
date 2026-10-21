#' @title Egalitarian value with a priori unions
#'
#' @description
#' Calculate the egalitarian value in games with a priori unions
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param union List of vectors indicating the a priori unions between the
#' players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#'
#' @return The egalitarian value for each player
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
#' union <- list(1:4,5:n)
#' egalitarian_unions(v,union,n)
#'
#' @examples
#' v <- c(1,1,2,1,2,2,2)
#' union <- list(c(1,2),c(3))
#' egalitarian_unions(v, union)
#'
#' @export

egalitarian_unions <- function(characteristic_func, union, n_players = 0) {

  n_players <- check_game(characteristic_func, n_players)
  union <- check_union(union, n_players)

  if (is.function(characteristic_func)) {
    v_grand <- characteristic_func(seq(n_players))
  } else {
    v_grand <- characteristic_func[length(characteristic_func)]
  }

  egalitarian_value <- rep(0, n_players)
  num_unions <- length(union)
  for (block in union) {
    egalitarian_value[block] <- v_grand / (length(block) * num_unions)
  }

  names(egalitarian_value) <- seq(n_players)
  return(egalitarian_value)

}
