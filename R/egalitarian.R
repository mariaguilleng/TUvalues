#' @title Egalitarian value
#'
#' @description
#' Calculate the egalitarian value
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
