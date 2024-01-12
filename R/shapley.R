#' @title Shapley value (exact)
#'
#' @description
#' Calculate the (exact) Shapley value
#'
#' @param characteristic_function The characteristic function
#'
#' @return The Shapley value for each player
#'
#' @export

shapley <- function(characteristic_function){

    # get number of players
    num_players<-log(length(characteristic_function),2)

    # check if empty coalition is included in characteristic_function
    if (num_players!=round(num_players)){
      characteristic_function <- c(0, characteristic_function)
      num_players<-log(length(characteristic_function+1),2)
    }
    characteristic_function <- characteristic_function[-1]

    # get coalitions
    coalitions_binary<-coalitions(n)[[1]][-1,]

    # calculate Shapley value
    coalitions_aux<-coalitions_binary
    factorial_num_players <- factorial(num_players)
    for (j in 1:(2^n-1)){
      for (i in 1:n){
        num_players_coalition<-length(which(coalitions_binary[j,]!=0))
        if (coalitions_aux[j,i]==0){
          coalitions_aux[j,i]=-(factorial(num_players_coalition)*factorial(num_players-num_players_coalition-1))/factorial_num_players*characteristic_function[j]
        } else {
          coalitions_aux[j,i]=(factorial(num_players_coalition-1)*factorial(num_players-num_players_coalition))/factorial_num_players*characteristic_function[j]
        }
      }
    }
    shapley_value<-apply(coalitions_aux,2,sum)

    return(shapley_value)
  }
