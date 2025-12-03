#' @title coalitions
#'
#' @description Create all the possible coalitions given the number of players
#'
#' @param n_players Number of players
#'
#' @return A list containing a \code{data.frame} of the binary representation
#' of the coalitions and a \code{vector} of the classical representation (as
#' sets) of the coalitions
#'
#' @importFrom utils combn
#'
#' @export

coalitions <- function(n_players){

  # all posible combination of n_players
  results_by_size <- lapply(X = 1:n_players, FUN = function(m) {
    return(combn(x = 1:n_players, m = m))
  })

  # Empty coalition
  empty_coalition <- rep(0, n_players)

  # Binary matrix
  all_binary_rows <- list(empty_coalition)
  for (i in 1:n_players) {
    combinations <- results_by_size[[i]]
    for (j in 1:ncol(combinations)) {
      binary_row <- rep(0, n_players)
      players_in_coalition <- combinations[, j]
      binary_row[players_in_coalition] <- 1
      all_binary_rows[[length(all_binary_rows) + 1]] <- binary_row
    }
  }
  binary_coalitions <- do.call(rbind, all_binary_rows)

  # Output format
  colnames(binary_coalitions) <- 1:n_players
  rownames(binary_coalitions) <- 1:nrow(binary_coalitions)

  # Classical representation (as sets)
  classic_coalitions <- c()
  for (i in 1:nrow(binary_coalitions)) {
    cols <- which(binary_coalitions[i, ] == 1)
    classic_coalitions[i] <- toString(cols)
  }

  # Return both representations in a list
  sol<-list(binary_coalitions,classic_coalitions)
  names(sol)<-c("Binary","Classic")
  return(sol)

}

coalition_binary_to_numeric <- function(coalition_binary) {
  return(which(coalition_binary == 1))
}
