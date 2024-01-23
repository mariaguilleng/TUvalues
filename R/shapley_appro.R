#' @title Shapley value (approximation)
#'
#' @description Calculate the approximated Shapley value based on sampling
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#' @param n_rep The number of iterations to perform in the approximated
#' calculation
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @return The Shapley value for each player

shapley_appro_vector <- function(value_func,n_rep){

  # get number of players
  n_players<-log(length(value_func),2)

  # check if empty coalition is included in value_func
  if (n_players!=round(n_players)){
    value_func <- c(0, value_func)
    n_players<-log(length(value_func+1),2)
  }
  factorial_n <- factorial(n_players)

  # get coalitions
  coa_set <- coalitions(n_players)[[2]]

  pb <- txtProgressBar(min = 0, max = n_rep, style = 3)

  shapley_value <- rep(0, n_players)
  for (rep in 1:n_rep) {

    # Update the progress bar
    setTxtProgressBar(pb, rep)

    # O in pi(N) with probability 1/n!
    perm <- sample(1:n_players)

    for (i in 1:n_players) {
      # v(Pre_i(O) U {i})
      pre_perm_player_set <- toString(predecessor(perm, i, TRUE))
      v_pre_perm_player <- value_func[which(coa_set == pre_perm_player_set)]
      # v(Pre_i(O))
      pre_perm_player_set <- toString(predecessor(perm, i, FALSE))
      v_pre_perm_noplayer <- value_func[which(coa_set == pre_perm_player_set)]
      # x_i = v(Pre_i(O) U {i}) - v(Pre_i(O))
      x_perm_player <- v_pre_perm_player - v_pre_perm_noplayer
      shapley_value[i] <- shapley_value[i] + x_perm_player
    }
  }
  close(pb)

  shapley_value <- shapley_value/n_rep
  names(shapley_value) <- 1:n_players
  return(shapley_value)

}

#' @title Shapley value (approximation)
#'
#' @description Calculate the approximated Shapley value based on sampling
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#' @param n_rep The number of iterations to perform in the approximated
#' calculation
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @return The Shapley value for each player

shapley_appro_func <- function(value_func,n_rep,n_players){

  # init progress bar
  pb <- txtProgressBar(min = 0, max = n_rep, style = 3)

  shapley_value <- rep(0, n_players)
  for (rep in 1:n_rep) {

    # Update the progress bar
    setTxtProgressBar(pb, rep)

    # O in pi(N) with probability 1/n!
    perm <- sample(1:n_players)

    for (i in 1:n_players) {
      # v(Pre_i(O) U {i})
      v_pre_perm_player <- value_func(predecessor(perm, i, TRUE))
      # v(Pre_i(O))
      v_pre_perm_noplayer <- value_func(predecessor(perm, i, FALSE))
      # x_i = v(Pre_i(O) U {i}) - v(Pre_i(O))
      x_perm_player <- v_pre_perm_player - v_pre_perm_noplayer
      # sh_i <- sh_i + x_i
      shapley_value[i] <- shapley_value[i] + x_perm_player
    }
  }
  close(pb)
  shapley_value <- shapley_value/n_rep
  return(shapley_value)

}
