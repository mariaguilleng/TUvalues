#' @title Shapley value (exact)
#'
#' @description
#' Calculate the (exact) Shapley value
#'
#' @param value_func The valued function
#'
#' @return The Shapley value for each player

shapley <- function(value_func, method = "exact", m = 100){

  if (! method %in% c("exact", "appro")) {
    stop("Invalid methos specified\n Use \"exact\" for the exact value\n
         or \"appro\" for the approximationn")
  }

  if (method == "exact") {
    return(shapley_exact(value_func))
  } else {
    if (m <= 0) {
      stop("Invalid number of iterations specified. m must be grater than 0")
    }
    return(shapley_appro(value_func, m))
  }

}



#' @title Shapley value (exact)
#'
#' @description
#' Calculate the (exact) Shapley value
#'
#' @param value_func The characteristic function
#'
#' @return The Shapley value for each player
#'
#' @export

shapley_exact <- function(value_func){

  # get number of players
  n_players<-log(length(value_func),2)

  # check if empty coalition is included in value_func
  if (n_players!=round(n_players)){
    value_func <- c(0, value_func)
    n_players<-log(length(value_func+1),2)
  }
  value_func <- value_func[-1]

  # get coalitions
  coa_binary<-coalitions(n)[[1]][-1,]

  # calculate Shapley value
  coa_aux<-coa_binary
  factorial_n <- factorial(n_players)
  for (j in 1:(2^n-1)){
    for (i in 1:n){
      s_players<-length(which(coa_binary[j,]!=0)) # Number of players of the coalition
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



#' @title Shapley value (approximation)
#'
#' @description
#' Calculate the approximated Shapley value
#'
#' @param value_func The valued function
#'
#' @return The Shapley value for each player
#'
#' @export

shapley_appro <- function(value_func,n_rep){

  # get number of players
  n_players<-log(length(value_func),2)

  # check if empty coalition is included in value_func
  if (n_players!=round(n_players)){
    value_func <- c(0, value_func)
    n_players<-log(length(value_func+1),2)
  }
  factorial_n <- factorial(n_players)

  # get coalitions
  coa_set <- coalitions(n)[[2]]

  # get permutations of the number of players
  perm_players <- as.data.frame(permutations(n = n_players, r = n_players))

  # take m permutations with probability 1/n!
  perm_index <- sample(x = 1:factorial_n, size = n_rep, replace = TRUE)

  shapley_value <- rep(0, n_players)
  for (rep in 1:n_rep) {
    for (i in 1:n_players) {
      perm <- perm_players[perm_index[rep], ]
      x_perm_player <- pre_comb_player(perm, i, coa_set, TRUE) -
        pre_comb_player(perm, i, coa_set, FALSE)
      shapley_value[i] <- shapley_value[i] + x_perm_player
    }
  }
  shapley_value <- shapley_value/n_players
  return(shapley_value)

}


pre_comb_player <- function(permutation, player, coalition_classic, include_player = FALSE) {
  pos_player <- which(permutation == player)
  if (!include_player) {
    pre_perm_player <- permutation[1:(pos_player - 1)]
  } else {
    pre_perm_player <- permutation[1:pos_player]
  }
  pre_perm_player_set <- paste0("{",
                                paste(sort(pre_perm_player), collapse = ","),
                                "}")
  pos_set <- which(coa_set == pre_perm_player_set)
  return(value_func[pos_set])
}

