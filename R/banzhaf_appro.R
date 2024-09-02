#' @title Banzhaf Index (approximated)
#'
#' @description Calculate the approximated Banzhaf Index based on sampling
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#' @param n_rep The number of iterations to perform in the approximated
#' calculation
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @return The Shapley value for each player

banzhaf_appro_vector <- function(value_func,n_rep){

  # get number of players
  n_players<-log(length(value_func),2)

  # check if empty coalition is included in value_func
  if (n_players!=round(n_players)){
    value_func <- c(0, value_func)
    n_players<-log(length(value_func+1),2)
  }

  # get coalitions
  coa_set <- coalitions(n_players)[[2]]

  # set n_rep
  n_rep <- min(n_rep, 2**(n_players-1))

  # init progress bar
  pb <- txtProgressBar(min = 0, max = n_rep, style = 3)

  banzhaf_value <- rep(0, n_players)

  used_coalitions <- list()
  prob_coalition <- c()
  for (i in 1:n_players) {
    prob_coalition[i] <- choose(n_players, i-1)
  }


  for (rep in 1:n_rep) {

    for (i in 1:n_players) {

      # Update the progress bar
      setTxtProgressBar(pb, ((i-1)*n_rep)+rep)

      # S in all(S) with probability 1/2**n without repetition
      repeat{
        size_coalition <- sample(0:(n_players-1), size = 1, prob = prob_coalition)
        coalition <- sort(sample(setdiff(seq(1, n_players), i), size = size_coalition))
        if (!any(sapply(used_coalitions, identical, c(coalition,i)))) {
          break
        }
      }
      used_coalitions <- append(used_coalitions, list(c(coalition,i)))
      # v(S U {i})
      coalition_player <- toString(sort(c(i,coalition)))
      v_coalition_player <- value_func[which(coa_set == coalition_player)]
      # v(S)
      coalition_noplayer <- toString(sort(coalition))
      v_coalition_noplayer <- value_func[which(coa_set == coalition_noplayer)]
      # x_i = v(S U {i}) - v(S)
      x_coalition_player <- v_coalition_player - v_coalition_noplayer
      # sh_i <- sh_i + x_i
      banzhaf_value[i] <- banzhaf_value[i] + x_coalition_player

    }
  }
  close(pb)
  banzhaf_value <- banzhaf_value/n_rep
  names(banzhaf_value) <- 1:n_players
  return(banzhaf_value)

}


#' @title Banzhaf value (approximation)
#'
#' @description Calculate the approximated Shapley value based on sampling
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#' @param n_rep The number of iterations to perform in the approximated
#' calculation
#' @param n_players Only used if \code{value_func} is a \code{function}.
#' The number of players in the game.
#' @param replace should sampling be with replacement?
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @return The Shapley value for each player

banzhaf_appro_func <- function(value_func,n_rep,n_players,replace = TRUE){

  # init progress bar
  if (!replace) {
    n_rep <- min(n_rep, 2**n_players)
  }
  pb <- txtProgressBar(min = 0, max = n_rep*n_players, style = 3)

  # probability of the size of the coalitions
  prob_coalition <- c()
  for (i in 1:n_players) {
    prob_coalition[i] <- choose(n_players, i-1)
  }

  banzhaf_value <- rep(0, n_players)
  for (i in 1:n_players) {

    used_coalitions <- list()

    for (rep in 1:n_rep) {

        # Update the progress bar
        setTxtProgressBar(pb, ((i-1)*n_rep)+rep)

        # S in all(S) with probability 1/2**n WITHOUT repetition
        if (!replace) {
          repeat{
            size_coalition <- sample(0:(n_players-1), size = 1, prob = prob_coalition)
            coalition <- sort(sample(setdiff(seq(1, n_players), i), size = size_coalition))
            if (!any(sapply(used_coalitions, identical, coalition))) {
              break
            }
          }
          used_coalitions <- append(used_coalitions, list(coalition))
        # S in all(S) with probability 1/2**n WITH repetition
        } else {
          size_coalition <- sample(0:(n_players-1), size = 1, prob = prob_coalition)
          coalition <- sort(sample(setdiff(seq(1, n_players), i), size = size_coalition))
        }

        # v(S U {i})
        v_coalition_player <- value_func(c(coalition, i))
        # v(S)
        v_coalition_noplayer <- value_func(coalition)
        # x_i = v(S U {i}) - v(S)
        x_coalition_player <- v_coalition_player - v_coalition_noplayer
        # sh_i <- sh_i + x_i
        banzhaf_value[i] <- banzhaf_value[i] + x_coalition_player

      }
  }
  close(pb)
  banzhaf_value <- banzhaf_value/n_rep
  names(banzhaf_value) <- 1:n_players
  return(banzhaf_value)

}



#' @title Banzhaf Index (approximated)
#'
#' @description Calculate the approximated Banzhaf Index based on sampling
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param n_players Only used if \code{value_func} is a \code{function}.
#' The number of players in the game
#' @param n_rep The number of iterations to perform in the approximated
#' calculation
#' @param replace should sampling be with replacement?
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @return The Shapley value for each player

banzhaf_appro <- function(characteristic_func,n_players,n_rep,replace = TRUE){

  if (is.vector(characteristic_func)) {
    # get number of players
    n_players<-log(length(characteristic_func),2)
    if (n_players!=round(n_players)){
      characteristic_func <- c(0, characteristic_func)
      n_players<-log(length(characteristic_func+1),2)
    }
    # get coalitions
    coa_set <- coalitions(n_players)[[2]]
  }

  # init progress bar
  if (!replace) {
    n_rep <- min(n_rep, 2**(n_players-1))
    used_coalitions <- list()
  }
  pb <- txtProgressBar(min = 0, max = n_rep*n_players, style = 3)

  # probability of the size of the coalitions
  prob_coalition <- sapply(1:n_players, function(i) choose(n_players, i - 1))

  banzhaf_value <- rep(0, n_players)
  for (i in 1:n_players) {

    used_coalitions <- list()

    for (rep in 1:n_rep) {

      # Update the progress bar
      setTxtProgressBar(pb, ((i-1)*n_rep)+rep)

      # S in all(S) with probability 1/2**n WITHOUT repetition
      if (!replace) {
        repeat{
          size_coalition <- sample(0:(n_players-1), size = 1, prob = prob_coalition)
          coalition <- sort(sample(setdiff(seq(1, n_players), i), size = size_coalition))
          if (!any(sapply(used_coalitions, identical, coalition))) {
            break
          }
        }
        used_coalitions <- append(used_coalitions, list(coalition))
      # S in all(S) with probability 1/2**n WITH repetition
      } else {
        size_coalition <- sample(0:(n_players-1), size = 1, prob = prob_coalition)
        coalition <- sort(sample(setdiff(seq(1, n_players), i), size = size_coalition))
      }

      if (is.vector(characteristic_func)) {
        # v(S U {i})
        coalition_player <- toString(sort(c(i,coalition)))
        v_coalition_player <- characteristic_func[which(coa_set == coalition_player)]
        # v(S)
        coalition_noplayer <- toString(sort(coalition))
        v_coalition_noplayer <- characteristic_func[which(coa_set == coalition_noplayer)]
      } else { #is.function(characteristic_func)
        # v(S U {i})
        v_coalition_player <- characteristic_func(c(coalition, i))
        # v(S)
        v_coalition_noplayer <- characteristic_func(coalition)
      }
      # x_i = v(S U {i}) - v(S)
      x_coalition_player <- v_coalition_player - v_coalition_noplayer
      # sh_i <- sh_i + x_i
      banzhaf_value[i] <- banzhaf_value[i] + x_coalition_player

    }
  }
  close(pb)
  banzhaf_value <- banzhaf_value/n_rep
  names(banzhaf_value) <- 1:n_players
  return(banzhaf_value)

}
