#' @title Owen value (exact)
#'
#' @description Calculate the exact Owen
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param union List of vectors indicating the a priori unions between the
#' players
#' @param n_players The number of players
#'
#' @importFrom gtools permutations
#'
#' @return The Owen value for each player


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

  # get valid permutations according to a apriori unions
  n_perm_union <- factorial(length(union)) * prod(sapply(union, function(x) factorial(length(x))))

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

  # get value
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



