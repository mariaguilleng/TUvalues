#' @title Shapley value (exact)
#'
#' @description Calculate the exact Shapley value
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#'
#' @return The Shapley value for each player

shapley_exact_vector <- function(value_func){

  # get number of players
  n_players<-log(length(value_func),2)

  # check if empty coalition is included in value_func
  if (n_players!=round(n_players)){
    value_func <- c(0, value_func)
    n_players<-log(length(value_func+1),2)
  }
  value_func <- value_func[-1]

  # get coalitions
  coa_binary<-coalitions(n_players)[[1]][-1,]

  # calculate Shapley value
  coa_aux<-coa_binary
  factorial_n <- factorial(n_players)
  for (j in 1:(2^n_players-1)){
    for (i in 1:n_players){
      s <- coalition_binary_to_numeric(coa_binary[j,]) # Current coalition
      s_players <- length(s) # Number of players of the coalition
      if (coa_aux[j,i]==0){
        coa_aux[j,i]=-(factorial(s_players)*factorial(n_players-s_players-1))/factorial_n*value_func[j]
      } else {
        coa_aux[j,i]=(factorial(s_players-1)*factorial(n_players-s_players))/factorial_n*value_func[j]
      }
    }
  }
  shapley_value<-apply(coa_aux,2,sum)

  return(shapley_value)
}


#' @title Shapley value (exact)
#'
#' @description Calculate the exact Shapley value
#'
#' @param value_func The valued function defined on the subsets of the number
#' of players
#'
#' @return The Shapley value for each player
shapley_exact_func <- function(value_func, n_players){

  # get coalitions
  coa_binary<-coalitions(n_players)[[1]][-1,]

  # calculate Shapley value
  coa_aux<-coa_binary
  factorial_n <- factorial(n_players)
  for (j in 1:(2^n_players-1)){
    for (i in 1:n_players){
      s <- coalition_binary_to_numeric(coa_binary[j,]) # Current coalition
      s_players <- length(s) # Number of players of the coalition
      if (coa_aux[j,i]==0){
        coa_aux[j,i]=-(factorial(s_players)*factorial(n_players-s_players-1))/factorial_n*value_func(s)
      } else {
        coa_aux[j,i]=(factorial(s_players-1)*factorial(n_players-s_players))/factorial_n*value_func(s)
      }
    }
  }
  shapley_value<-apply(coa_aux,2,sum)

  return(shapley_value)
}
