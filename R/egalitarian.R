#' @title Egalitarian value
#'
#' @description
#' Calculate the egalitarian value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
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
#' egalitarian(v,n)
#'
#' @examples
#' v <- c(1,1,2,1,2,2,2)
#' egalitarian(v)
#'
#' @export

egalitarian <- function(characteristic_func, n_players = 0) {

  n_players <- validate_game(characteristic_func, n_players)

  if (is.function(characteristic_func)) {
    v_grand <- characteristic_func(seq(n_players))
  } else {
    v_grand <- characteristic_func[length(characteristic_func)]
  }
  egalitarian_value <- v_grand/n_players

  egalitarian_value <- rep(egalitarian_value, n_players)
  names(egalitarian_value) <- seq(n_players)
  return(egalitarian_value)

}
