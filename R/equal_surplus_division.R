#' @title Equal Surplus Division value
#'
#' @description
#' Calculate the equal surplus division value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
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
#' @export

equal_surplus_division <- function(characteristic_func, n_players = 0) {

  esd_value <- rep(0, n_players)
  if (is.vector(characteristic_func)) {

    # get number of players
    n_players<-log(length(characteristic_func),2)
    if (n_players!=round(n_players)){
      characteristic_func <- c(0, characteristic_func)
      n_players<-log(length(characteristic_func+1),2)
    }
    characteristic_func <- characteristic_func[-1]

    individual_sum <- sum(characteristic_func[1:n_players])
    for(i in 1:n_players) {
      esd_value[i] <- characteristic_func[i] + (characteristic_func[length(characteristic_func)] - individual_sum)/n_players
    }

  } else if (is.function(characteristic_func)) {

    if (n_players < 2) {
      stop("Invalid numer of player specified. n_players must be greater
             than 1.")
    }

    individual_sum <- sum(sapply(1:n_players, characteristic_func))
    for(i in 1:n_players) {
      esd_value[i] <- characteristic_func(i) + (characteristic_func(1:n_players) - individual_sum)/n_players
    }

  } else {
    stop("Invalid characteristic_func provided.")
  }

  names(esd_value) <- 1:n_players
  return(esd_value)

}
