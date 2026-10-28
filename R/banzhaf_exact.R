#' @title Banzhaf value (exact)
#' @noRd
#'
#' @description Calculate the exact Banzhaf value. For a vector, the value of
#' every player is obtained with a binary reduction over all the coalitions; for
#' a function, the coalitions are evaluated and accumulated by blocks
#' (\code{exact_by_blocks}), so the memory used does not depend on the number
#' of players.
#' Internal function called by \code{banzhaf}, which checks the arguments.
#'
#' @param characteristic_func The characteristic function of the game, already
#' checked: a numeric vector of length \eqn{2^n - 1} or \eqn{2^n} in the order
#' given by \code{coalitions}, or a function that receives a coalition.
#' @param n_players The number of players, already checked. Only used if
#' \code{characteristic_func} is a function.
#'
#' @return The Banzhaf value for each player.

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
