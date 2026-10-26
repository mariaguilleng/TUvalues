#' @title Banzhaf Index (exact)
#' @noRd
#'
#' @description Calculate the approximated Banzhaf Index
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param n_players The number of players in the game.
#'
#' @return The Banzhaf Index for each player

banzhaf_exact <- function(characteristic_func, n_players){

  # Function input: evaluate and accumulate by blocks of coalitions
  if (is.function(characteristic_func)) {
    return(exact_by_blocks(characteristic_func, n_players, "banzhaf"))
  }

  values <- game_values(characteristic_func)
  n <- as.integer(round(log2(length(values))))
  m <- 2^n

  k <- 0L
  for (i in seq_len(n)) k <- c(k, k + 1L)

  # coalitions() orders coalitions first by cardinality and then
  # lexicographically within each cardinality. The fast binary reduction
  # used below requires coalition values in pure binary-mask order.
  # ord gives, for every position in coalitions() order, the
  # corresponding position in binary order.
  key <- 0L
  for (i in seq_len(n)) {
    key <- c(2L * key, 2L * key + 1L)
  }
  ord <- order(k, -key, method = "radix")
  rm(key)

  v <- numeric(m)
  v[ord] <- values - values[1L]
  rm(values, ord, k)

  total <- sum(v)
  scale <- 2^(n - 1L)

  z <- v
  rm(v)
  banzhaf_value <- numeric(n)
  for (i in n:1L) {
    half <- length(z) %/% 2L
    idx <- seq_len(half)
    right <- z[idx + half]
    banzhaf_value[i] <- (2 * sum(right) - total) / scale
    if (i > 1L) z <- z[idx] + right
  }
  names(banzhaf_value) <- seq_len(n)
  return(banzhaf_value)

}
