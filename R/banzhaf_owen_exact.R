#' @title Banzhaf-Owen Value
#'
#' @description Calculate the approximated Banzhaf-Owen value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param union List of vectors indicating the apriori unions between the
#' players
#' @param n_players The number of players in the game.
#'
#' @return The Banzhaf Index for each player

banzhaf_owen_exact <- function(characteristic_func, union, n_players) {

  if (is.vector(characteristic_func)) {
    # get number of players
    n_players<-log(length(characteristic_func),2)
    if (n_players!=round(n_players)){
      characteristic_func <- c(0, characteristic_func)
      n_players<-log(length(characteristic_func+1),2)
    }
  }

  # get coalitions
  coalition  <- coalitions(n_players)
  coa_set <- coalition[[2]]

  banzhaf_owen_value<-rep(0,n_players)

  for(i in 1:n_players){

    union_without_i <- union[!sapply(union, function(coa) i %in% coa)]

    for (j in 1:length(union_without_i)) {

      if (is.vector(characteristic_func)) {
        # v(T U {i})
        coalition_player <- toString(sort(c(i,union_without_i[[j]])))
        v_coalition_player <- characteristic_func[which(coa_set == coalition_player)]
        # v(T)
        coalition_noplayer <- toString(sort(union_without_i[[j]]))
        v_coalition_noplayer <- characteristic_func[which(coa_set == coalition_noplayer)]
      } else { #is.function(characteristic_func)
        # v(S U {i})
        v_coalition_player <- characteristic_func(c(union_without_i[[j]], i))
        # v(S)
        v_coalition_noplayer <- characteristic_func(union_without_i[[j]])
      }
      # x_i = v(S U {i}) - v(S)
      x_coalition_player <- v_coalition_player - v_coalition_noplayer
      # sh_i <- sh_i + x_i
      banzhaf_owen_value[i] <- banzhaf_owen_value[i] + x_coalition_player
    }
  }
  banzhafFactor <- 1/(length(union)-1)
  banzhaf_owen_value <- banzhafFactor*banzhaf_owen_value
  names(banzhaf_owen_value) <- 1:n_players
  return(banzhaf_owen_value)

}
