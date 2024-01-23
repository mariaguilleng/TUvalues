#' @title Egalitarian value
#'
#' @description
#' Calculate the egalitarian value
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#' @param n_players Only used if \code{value_func} is a \code{function}.
#' The number of players in the game.
#'
#' @return The egalitarian value for each player
#'
#' @export

egalitarian <- function(value_func, n_players = 0) {

  if (class(value_func) == "numeric") {

    # get number of players
    n_players<-log(length(value_func),2)
    if (n_players!=round(n_players)){
      value_func <- c(0, value_func)
      n_players<-log(length(value_func+1),2)
    }

    egalitarian_value <- value_func[n_players]/n_players

  } else if (class(value_func) == "function") {
    if (n_players < 2) {
      stop("Invalid numer of player specified. n_players must be greater
             than 1.")
    }
    egalitarian_value <- value_func(1:n_players)/n_players
  }
  egalitarian_value <- rep(egalitarian_value, n_players)
  names(egalitarian_value) <- 1:n_players
  return(egalitarian_value)

}
