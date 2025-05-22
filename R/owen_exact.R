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
  internal_perms <- lapply(union, function(block) permutations(n = length(block), r = length(block), v = block))
  block_orders <- permutations(n = length(union), r = length(union))

  p <- list()

  for (i in 1:nrow(block_orders)) {
    block_order <- block_orders[i, ]
    block_perm_lists <- lapply(block_order, function(idx) internal_perms[[idx]])

    # internal permutationaes for the blocks orders
    index_grid <- expand.grid(lapply(block_perm_lists, function(mat) 1:nrow(mat)))

    for (j in 1:nrow(index_grid)) {
      perm <- do.call(c, Map(function(mat, row_idx) mat[row_idx, ],
                             block_perm_lists, as.list(index_grid[j, ])))
      p[[length(p) + 1]] <- perm
    }
  }

  p <- do.call(rbind, p)
  n_perm_union <- nrow(p)

  # get value
  owen_value <- rep(0, n_players)
  for (j in 1:n_perm_union) {
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



