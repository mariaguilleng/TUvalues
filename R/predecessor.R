#' @title Predecessor
#'
#' @description  Given a permutation 0 of players and a player i, calculate the
#' set of predecessors of the player i in the order 0
#'
#' @param permutation A permutation of the players
#' @param player Number of the player i
#' @param include_player Whether the player i is included as predecessor of
#' itself or not
#'
#' @return The set of predecessors of the player i in the order 0

predecessor <- function(permutation, player, include_player = FALSE) {

  pos_player <- which(permutation == player)
  if (!include_player) {
    pre_perm_player <- permutation[0:(pos_player - 1)]
  } else {
    pre_perm_player <- permutation[0:pos_player]
  }
  pre_perm_player <- sort(unlist(pre_perm_player))
  return(pre_perm_player)
}
