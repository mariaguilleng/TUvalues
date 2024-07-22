#' @title Shapley value (exact)
#'
#' @description Calculate the exact Shapley value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param n_players The number of players
#'
#' @return The Shapley value for each player

shapley_exact <- function(characteristic_func, n_players){

  if (is.vector(characteristic_func)) {
    # get number of players
    n_players<-log(length(characteristic_func),2)
    if (n_players!=round(n_players)){
      characteristic_func <- c(0, characteristic_func)
      n_players<-log(length(characteristic_func+1),2)
    }
    characteristic_func <- characteristic_func[-1]
  }

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
        if (is.function(characteristic_func)) {
          coa_aux[j,i]=-(factorial(s_players)*factorial(n_players-s_players-1))/factorial_n*characteristic_func(s)
        } else {
          coa_aux[j,i]=-(factorial(s_players)*factorial(n_players-s_players-1))/factorial_n*characteristic_func[j]
        }
      } else {
        if (is.function(characteristic_func)) {
          coa_aux[j,i]=(factorial(s_players-1)*factorial(n_players-s_players))/factorial_n*characteristic_func(s)
        } else {
          coa_aux[j,i]=(factorial(s_players-1)*factorial(n_players-s_players))/factorial_n*characteristic_func[j]
        }
      }
    }
  }
  shapley_value<-apply(coa_aux,2,sum)
  return(shapley_value)
}
