#' @title Banzhaf Index (exact)
#'
#' @description Calculate the approximated Banzhaf Index
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#'
#' @return The Banzhaf Index for each player

banzhaf_exact_vector <- function(value_func){

  # get number of players
  n_players<-log(length(value_func),2)

  # check if empty coalition is included in value_func
  if (n_players!=round(n_players)){
    value_func <- c(0, value_func)
    n_players<-log(length(value_func+1),2)
  }

  # get coalitions
  coa_binary  <-coalitions(n_players)[[1]]
  coa_binary <- cbind(coa_binary, value_func)

  banzhaf_value<-rep(0,n_players)
  for(i in 1:n_players){
    #Get all coalitions K where player i takes part
    coa_player <- coa_binary[coa_binary[,i]==1,]

    #Get all coalitions K \ {i}
    coa_without_player <- coa_binary[coa_binary[,i]==0,]

    banzhaf_value[i] <- sum(coa_player[,"value_func"]-coa_without_player[,"value_func"])
  }
  banzhafFactor <- 1/(2^(n_players-1))
  banzhaf_value <- banzhafFactor*banzhaf_value
  names(banzhaf_value) <- 1:n_players
  return(banzhaf_value)

}

#' @title Banzhaf Index (exact)
#'
#' @description Calculate the approximated Banzhaf Index
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#'
#' @return The Banzhaf Index for each player

banzhaf_exact_func <- function(value_func, n_players){

  # get coalitions
  coalition  <- coalitions(n_players)
  coa_binary <- coalition[[1]]
  coa_set <- coalition[[2]]

  banzhaf_value<-rep(0,n_players)
  for(i in 1:n_players){
    #Get all coalitions K where player i takes part
    coa_player <- coa_binary[coa_binary[,i]==1,]
    coa_player <- apply(coa_player, 1, coalition_binary_to_numeric)

    #Get all coalitions K \ {i}
    coa_without_player <- coa_binary[coa_binary[,i]==0,]
    coa_without_player <- apply(coa_without_player, 1, coalition_binary_to_numeric)

    banzhaf_value[i] <- sum(sapply(coa_player, value_func)-sapply(coa_without_player, value_func))
  }
  banzhafFactor <- 1/(2^(n_players-1))
  banzhaf_value <- as.vector(banzhafFactor*banzhaf_value)
  names(banzhaf_value) <- 1:n_players
  return(banzhaf_value)
}



