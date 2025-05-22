#' @title Owen value (approximation)
#'
#' @description Calculate the approximated Owen value based on sampling
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param union List of vectors indicating the a priori unions between the
#' players
#' @param n_players The number of players
#' @param n_rep The number of iterations to perform in the approximated
#' calculation
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @return The Owen value for each player

owen_appro <- function(characteristic_func, union, n_players, n_rep) {

  if (is.vector(characteristic_func)) {
    # Get number of players
    n_players <- log2(length(characteristic_func))
    if (n_players != round(n_players)) {
      characteristic_func <- c(0, characteristic_func)
      n_players <- log2(length(characteristic_func))
    }
    coa_set <- coalitions(n_players)[[2]]
  }

  # Init
  owen_value <- rep(0, n_players)
  pb <- txtProgressBar(min = 0, max = n_rep, style = 3)

  for (rep in 1:n_rep) {
    setTxtProgressBar(pb, rep)

    # Choose a valid perm
    perm <- unlist(lapply(sample(union), function(x) if (length(x) == 1) x else sample(x)))

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
      # Get x_i = v(Pre_i(O) U {i}) - v(Pre_i(O))
      x_perm_player <- v_pre_perm_player - v_pre_perm_noplayer
      owen_value[i] <- owen_value[i] + x_perm_player
    }
  }
  close(pb)
  owen_value <- owen_value / n_rep
  names(owen_value) <- 1:n_players
  return(owen_value)
}
