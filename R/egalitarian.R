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
#' @export

egalitarian <- function(characteristic_func, n_players = 0) {

  if (is.vector(characteristic_func)) {

    # get number of players
    n_players<-log(length(characteristic_func),2)
    if (n_players!=round(n_players)) {
      characteristic_func <- c(0, characteristic_func)
      n_players<-log(length(characteristic_func+1),2)
    }

    egalitarian_value <- characteristic_func[n_players]/n_players

  } else if (is.function(characteristic_func)) {

    if (n_players < 2) {
      stop("Invalid numer of player specified. n_players must be greater
             than 1.")
    }

    egalitarian_value <- characteristic_func(1:n_players)/n_players

  } else {
    stop("Invalid characteristic_func provided.")
  }

  egalitarian_value <- rep(egalitarian_value, n_players)
  names(egalitarian_value) <- 1:n_players
  return(egalitarian_value)

}
