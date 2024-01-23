#' @title Equal Surplus Division value
#'
#' @description
#' Calculate the equal surplus division value
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#' @param n_players Only used if \code{value_func} is a \code{function}.
#' The number of players in the game.
#'
#' @return The equal surplus division value for each player
#'
#' @export

equal_surplus_division <- function(value_func, n_players = 0) {

  browser()
  esd_value <- rep(0, n_players)
  if (class(value_func) == "numeric") {

    # get number of players
    n_players<-log(length(value_func),2)
    if (n_players!=round(n_players)){
      value_func <- c(0, value_func)
      n_players<-log(length(value_func+1),2)
    }
    value_func <- value_func[-1]
    individual_sum <- sum(value_func[1:n_players])
    for(i in 1:n_players) {
      esd_value[i] <- value_func[i] + (value_func[length(value_func)] - individual_sum)/n_players
    }

  } else if (class(value_func) == "function") {
    if (n_players < 2) {
      stop("Invalid numer of player specified. n_players must be greater
             than 1.")
    }
    individual_sum <- sum(sapply(1:n_players, value_func))
    for(i in 1:n_players) {
      esd_value[i] <- value_func(i) + (value_func(1:n_players) - individual_sum)/n_players
    }
  }
  names(esd_value) <- 1:n_players
  return(esd_value)

}
