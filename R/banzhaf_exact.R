#' @title Banzhaf Index (exact)
#'
#' @description Calculate the approximated Banzhaf Index
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param n_players The number of players in the game.
#'
#' @return The Banzhaf Index for each player

banzhaf_exact <- function(characteristic_func, n_players){

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
  coa_binary <- coalition[[1]]
  coa_set <- coalition[[2]]
  if (is.vector(characteristic_func)) {
    coa_binary <- cbind(coa_binary, characteristic_func)
  }

  banzhaf_value<-rep(0,n_players)
  for(i in 1:n_players){
    if (is.vector(characteristic_func)) {
      #Get all coalitions K where player i takes part
      coa_player <- coa_binary[coa_binary[,i]==1,]
      #Get all coalitions K \ {i}
      coa_without_player <- coa_binary[coa_binary[,i]==0,]
      # Get value
      banzhaf_value[i] <- sum(coa_player[,"characteristic_func"]-coa_without_player[,"characteristic_func"])
    } else { # is.function(characteristic_func)
      #Get all coalitions K where player i takes part
      coa_player <- coa_binary[coa_binary[,i]==1,]
      coa_player <- apply(coa_player, 1, coalition_binary_to_numeric)
      #Get all coalitions K \ {i}
      coa_without_player <- coa_binary[coa_binary[,i]==0,]
      coa_without_player <- apply(coa_without_player, 1, coalition_binary_to_numeric)
      # Get value
      banzhaf_value[i] <- sum(sapply(coa_player, characteristic_func)-sapply(coa_without_player, characteristic_func))
    }

  }
  banzhafFactor <- 1/(2^(n_players-1))
  banzhaf_value <- banzhafFactor*banzhaf_value
  names(banzhaf_value) <- 1:n_players
  return(banzhaf_value)

}



