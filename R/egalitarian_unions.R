#' @title Egalitarian value with a priori unions
#'
#' @description
#' Calculate the egalitarian value in games with a priori unions
#'
#' @param characteristic_func The characteristic function of the game. It can be
#' a numeric vector with the value of every coalition, in the order given by
#' \code{\link{coalitions}}: of length \eqn{2^n - 1}, without the empty
#' coalition (whose value is taken as 0), or of length \eqn{2^n}, with the
#' empty coalition first. Vectors support up to 30 players. It can also be a
#' function that receives a coalition (a vector of players) and returns its
#' value; in that case there is no limit on the number of players.
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

  n_players <- validate_game(characteristic_func, n_players)
  union <- validate_union(union, n_players)

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
