#' @title Equal Surplus Division value
#'
#' @description
#' Calculate the equal surplus division value
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
#' @return The equal surplus division value for each player
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
#' equal_surplus_division(v,n)
#'
#' @examples
#' v <- c(1,1,2,1,2,2,2)
#' equal_surplus_division(v)
#'
#' @export

equal_surplus_division <- function(characteristic_func, n_players = 0) {

  n_players <- validate_game(characteristic_func, n_players)

  if (!is.function(characteristic_func)) {

    characteristic_func <- game_values(characteristic_func)[-1]
    individual_sum <- sum(characteristic_func[seq(n_players)])
    v_grand <- characteristic_func[length(characteristic_func)]
    esd_value <- characteristic_func[seq(n_players)] + (v_grand - individual_sum)/n_players

  } else {

    esd_value <- rep(0, n_players)
    individual_sum <- sum(sapply(seq(n_players), characteristic_func))
    v_grand <- characteristic_func(seq(n_players))
    for(i in seq(n_players)) {
      esd_value[i] <- characteristic_func(i) + (v_grand - individual_sum)/n_players
    }

  }

  names(esd_value) <- seq(n_players)
  return(esd_value)

}
