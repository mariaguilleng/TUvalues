#' @title Banzhaf Index (approximated)
#'
#' @description Calculate the approximated Banzhaf Index based on sampling
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param n_rep The number of iterations to perform in the approximated
#' calculation
#' @param n_players The number of players
#' @param replace Should sampling be with replacement?
#' @param echo Show progress of the calculation.
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom stats runif
#'
#' @return The Shapley value for each player

banzhaf_appro <- function(characteristic_func,n_players,n_rep,replace, echo){

  is_fun <- is.function(characteristic_func)
  if (is_fun) {
    n_players <- as.integer(n_players)
  } else {
    values <- game_values(characteristic_func)
    n_players <- as.integer(round(log2(length(values))))
  }

  # S is sampled uniformly among the subsets of N \ {i}
  population <- 2^(n_players - 1)
  n_rep <- as.integer(if (replace) n_rep else min(n_rep, population))

  if (!is_fun) {
    # Convert the coalitions() order to binary-mask order
    k <- 0L
    key <- 0L
    for (j in seq_len(n_players)) {
      k <- c(k, k + 1L)
      key <- c(2L * key, 2L * key + 1L)
    }
    ord <- order(k, -key, method = "radix")
    v_binary <- numeric(2^n_players)
    v_binary[ord] <- values
    rm(values, ord, key, k)
    player_bit <- 2^(seq_len(n_players) - 1L)
  }

  players <- seq_len(n_players)
  banzhaf_value <- numeric(n_players)
  direct_sampling <- n_players <= 52L
  if (is_fun && direct_sampling) {
    compressed_bits <- 2^(seq_len(n_players - 1L) - 1L)
    if (n_players <= 31L) compressed_bits <- as.integer(compressed_bits)
  }
  if (echo) {
    pb <- txtProgressBar(min = 0, max = n_players, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (i in players) {
    if (direct_sampling) {
      # Each rank encodes one subset of N \ {i}
      ranks <- sample.int(population, size = n_rep, replace = replace) - 1
      if (!is_fun) {
        bit <- player_bit[i]
        low <- ranks %% bit
        masks <- low + 2 * (ranks - low)
        without <- v_binary[masks + 1]
        with <- v_binary[masks + bit + 1]
        banzhaf_value[i] <- mean(with - without)
      } else {
        available <- players[-i]
        total <- 0
        for (r in ranks) {
          if (n_players <= 31L) {
            coalition <- available[bitwAnd(as.integer(r), compressed_bits) != 0L]
          } else {
            coalition <- available[(r %/% compressed_bits) %% 2 != 0]
          }
          v_without <- characteristic_func(coalition)
          v_with <- characteristic_func(c(coalition, i))
          total <- total + v_with - v_without
        }
        banzhaf_value[i] <- total / n_rep
      }
    } else {
      # Too many players to index the subsets: sample each player with
      # probability 1/2, rejecting repeated coalitions if needed
      available <- players[-i]
      if (!replace) used <- new.env(hash = TRUE, parent = emptyenv())
      total <- 0
      for (rep in seq_len(n_rep)) {
        repeat {
          coalition <- available[runif(n_players - 1L) < 0.5]
          if (replace) break
          key <- if (length(coalition)) paste(coalition, collapse = ",") else "empty"
          if (!exists(key, envir = used, inherits = FALSE)) {
            assign(key, TRUE, envir = used)
            break
          }
        }
        v_without <- characteristic_func(coalition)
        v_with <- characteristic_func(c(coalition, i))
        total <- total + v_with - v_without
      }
      banzhaf_value[i] <- total / n_rep
    }
    if (echo) setTxtProgressBar(pb, i)
  }
  names(banzhaf_value) <- players
  return(banzhaf_value)

}
