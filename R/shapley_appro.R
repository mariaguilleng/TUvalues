#' @title Shapley value (approximation)
#'
#' @description Calculate the approximated Shapley value based on sampling
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param n_rep The number of iterations to perform in the approximated
#' calculation
#' @param n_players The number of players
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @return The Shapley value for each player

shapley_appro <- function(characteristic_func,n_players,n_rep){

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
  pb <- txtProgressBar(min = 0, max = n_rep, style = 3)

  shapley_value <- rep(0, n_players)
  for (rep in 1:n_rep) {

    # Update the progress bar
    setTxtProgressBar(pb, rep)

    # O in pi(N) with probability 1/n!
    perm <- sample(1:n_players)

    for (i in 1:n_players) {

      if (is.function(characteristic_func)) {
        # Get v(Pre_i(O) U {i})
        v_pre_perm_player <- characteristic_func(predecessor(perm, i, TRUE))
        # Get v(Pre_i(O))
        v_pre_perm_noplayer <- characteristic_func(predecessor(perm, i, FALSE))
      } else {
        # Get v(Pre_i(O) U {i})
        pre_perm_player_set <- toString(predecessor(perm, i, TRUE))
        v_pre_perm_player <- characteristic_func[which(coa_set == pre_perm_player_set)]
        # Get v(Pre_i(O))
        pre_perm_player_set <- toString(predecessor(perm, i, FALSE))
        v_pre_perm_noplayer <- characteristic_func[which(coa_set == pre_perm_player_set)]
      }

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
