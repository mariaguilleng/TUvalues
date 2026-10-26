#' @title Shapley value (exact)
#' @noRd
#'
#' @description Calculate the exact Shapley value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param n_players The number of players
#'
#' @return The Shapley value for each player

shapley_exact <- function(characteristic_func, n_players){

  # Function input: evaluate and accumulate by blocks of coalitions
  if (is.function(characteristic_func)) {
    return(exact_by_blocks(characteristic_func, n_players, "shapley"))
  }

  values <- game_values(characteristic_func)
  # The empty coalition is assumed to have value 0
  values[1L] <- 0
  n <- as.integer(round(log2(length(values))))
  m <- 2^n

  # Coalition sizes in binary order; player 1 is the least significant bit.
  k <- 0L
  for (i in seq_len(n)) k <- c(k, k + 1L)

  # coalitions() orders coalitions by size and, within each size,
  # according to the lexicographic order produced by combn().
  # Reverse-bit keys reproduce that order from binary masks.
  key <- 0L
  for (i in seq_len(n)) {
    key <- c(2L * key, 2L * key + 1L)
  }

  ord <- order(k, -key, method = "radix")
  rm(key)

  # Put values in binary order.
  v <- numeric(m)
  v[ord] <- values
  rm(values, ord)

  w <- 1 / (n * choose(n - 1L, 0:(n - 1L)))
  a <- c(0, w)[k + 1L]
  b <- c(w, 0)[k + 1L]
  offset <- sum(v * b)
  z <- v * (a + b)
  rm(v, a, b, k)

  # Sum z over all coalitions containing each player, eliminating the highest
  # remaining bit after each step. Total processed lengths form a geometric sum.
  shapley_value <- numeric(n)
  for (i in n:1L) {
    half <- length(z) %/% 2L
    idx <- seq_len(half)
    right <- z[idx + half]
    shapley_value[i] <- sum(right) - offset
    if (i > 1L) z <- z[idx] + right
  }
  names(shapley_value) <- seq_len(n)
  return(shapley_value)
}
