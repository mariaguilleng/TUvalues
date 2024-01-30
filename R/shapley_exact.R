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

owen_exact <- function(characteristic_func, union, n_players = NULL) {

  if (is.vector(characteristic_func)) {
    # Get number of players
    n_players <- log2(length(characteristic_func))
    if (n_players != round(n_players)) {
      characteristic_func <- c(0, characteristic_func)
      n_players <- log2(length(characteristic_func))
    }
  }
  coa_set <- coalitions(n_players)[[2]]

  # Get valid permutations according to apriori unions
  n_perm_union <- factorial(length(union)) *
    prod(sapply(union, function(x) factorial(length(x))))

  p <- permutations(n_players, n_players)

  index_permutation <- c()
  for (k in 1:nrow(p)) {
    for (i in 1:length(union)) {
      union_aux <- union[[i]]
      index <- c()
      for (j in 1:length(union_aux)) {
        index[j] <- which(p[k, ] == union_aux[j])
      }
      index_sort <- sort(index)
      for (j in 1:(length(index_sort) - 1)) {
        if (length(index_sort) > 1) {
          if ((index_sort[j + 1] - index_sort[j]) != 1) {
            index_permutation <- c(index_permutation, k)
          }
        }
      }
    }
  }
  if (length(index_permutation) > 0) {
    p <- p[-index_permutation, ]
  }

  # Get value
  owen_value <- rep(0, n_players)
  for (j in 1:nrow(p)) {
    perm <- p[j, ]
    for (i in 1:n_players) {
      if (is.function(characteristic_func)) {
        v_pre_perm_player <- characteristic_func(predecessor(perm, i, TRUE))
        v_pre_perm_noplayer <- characteristic_func(predecessor(perm, i, FALSE))
      } else {
        pre_perm_player_set <- toString(predecessor(perm, i, TRUE))
        v_pre_perm_player <- characteristic_func[which(coa_set == pre_perm_player_set)]
        pre_perm_player_set <- toString(predecessor(perm, i, FALSE))
        v_pre_perm_noplayer <- characteristic_func[which(coa_set == pre_perm_player_set)]
      }
      x_perm_player <- v_pre_perm_player - v_pre_perm_noplayer
      owen_value[i] <- owen_value[i] + x_perm_player
    }
  }
  owen_value <- owen_value / n_perm_union
  names(owen_value) <- 1:n_players
  return(owen_value)
}
