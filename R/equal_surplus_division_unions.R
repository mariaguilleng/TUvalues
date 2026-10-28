#' @title Equal Surplus Division value with a priori unions
#'
#' @description
#' Calculate the equal surplus division value in games with a priori unions
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
#' @param type Number indicating the type of equal surplus division value to compute
#' following Alonso-Meijide et al. (2020). Values 1, 2 and 3 are implemented.
#'
#' @return The equal surplus division value for each player
#'
#' @references Alonso-Meijide, J. M., Costa, J., García-Jurado, I., &
#' Gonçalves-Dosantos, J. C. (2020). On egalitarian values for cooperative games
#' with a priori unions. Top, 28(3), 672-688.
#'
#' @examples
#' n <- 3
#' v <- function(coalition) {
#'  size <- length(coalition)
#'  if (size <= 1) {
#'    return(0)
#'  } else if (size == 2) {
#'    return(10)
#'  } else if (size == 3) {
#'    return(24)
#'  } else {
#'    return(0)
#'  }
#' }
#' union <- list(1:2,3)
#' equal_surplus_division_unions(v,union,n,type = 1)
#'
#' @examples
#' v <- c(1,1,2,1,2,2,2)
#' union <- list(c(1,2),c(3))
#' equal_surplus_division_unions(v, union, type = 2)
#'
#' @export

equal_surplus_division_unions <- function(characteristic_func, union, n_players = 0, type = 1) {

  if (!is.numeric(type) || length(type) != 1L || !type %in% c(1, 2, 3)) {
    stop("Invalid type specified. Type must be either 1, 2 or 3.")
  }
  n_players <- validate_game(characteristic_func, n_players)
  union <- validate_union(union, n_players)

  if (!is.function(characteristic_func)) {

    characteristic_func <- game_values(characteristic_func)
    esd_value <- rep(0, n_players)
    coa_set <- coalitions(n_players)[[2]]
    names(characteristic_func) <- coa_set

    v_grand <- characteristic_func[length(characteristic_func)]
    num_unions <- length(union)

    if (type == 1) {

      block_strings <- vapply(union, function(block) paste(sort(block), collapse = ", "), "")
      block_values <- characteristic_func[block_strings]
      sum_unions <- sum(block_values)

      for (i in seq_along(union)) {
        esd_value[union[[i]]] <- (block_values[i]/length(union[[i]])) + ((v_grand-sum_unions)/(num_unions*length(union[[i]])))
      }

    } else if (type == 2) {

      block_strings <- vapply(union, function(block) paste(sort(block), collapse = ", "), "")
      block_values <- characteristic_func[block_strings]
      sum_unions <- sum(block_values)

      for (i in seq_along(union)) {
        sum_players_block <- sum(characteristic_func[union[[i]]+1])
        for (j in union[[i]]) {
          esd_value[j] <- characteristic_func[j+1] + ((block_values[i]-sum_players_block)/length(union[[i]])) + ((v_grand-sum_unions)/(num_unions*length(union[[i]])))
        }
      }

    } else if (type == 3) {

      sum_players <- sum(characteristic_func[(1:n_players)+1])
      for (i in seq(n_players)) {
        union_index <- which(sapply(union, function(block) i %in% block))
        esd_value[i] <- characteristic_func[i+1]+((v_grand - sum_players)/(num_unions*length(union[[union_index]])))
      }

    }


  } else {

    esd_value <- rep(0, n_players)
    v_grand <- characteristic_func(seq(n_players))
    num_unions <- length(union)

    if (type == 1) {

      block_values <- sapply(union, function(block) characteristic_func(block))
      sum_unions <- sum(block_values)

      for (i in seq_along(union)) {
        esd_value[union[[i]]] <- (block_values[i]/length(union[[i]])) + ((v_grand-sum_unions)/(num_unions*length(union[[i]])))
      }

    } else if (type == 2) {

      block_values <- sapply(union, function(block) characteristic_func(block))
      sum_unions <- sum(block_values)

      for (i in seq_along(union)) {
        sum_players_block <- sum(sapply(union[[i]], function(j) characteristic_func(j)))
        for (j in union[[i]]) {
          esd_value[j] <- characteristic_func(j) + ((block_values[i]-sum_players_block)/length(union[[i]])) + ((v_grand-sum_unions)/(num_unions*length(union[[i]])))
        }
      }

    } else if (type == 3) {

      sum_players <- sum(sapply(1:n_players, function(i) characteristic_func(i)))
      for (i in seq(n_players)) {
        union_index <- which(sapply(union, function(block) i %in% block))
        esd_value[i] <- characteristic_func(i)+((v_grand - sum_players)/(num_unions*length(union[[union_index]])))
      }
    }

  }

  names(esd_value) <- seq(n_players)
  return(esd_value)

}
