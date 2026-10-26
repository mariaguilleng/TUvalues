#' @title Shapley value (approximation)
#' @noRd
#'
#' @description Calculate the approximated Shapley value based on sampling using
#' the algorithm proposed by Castro et al. (2009).
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param n_rep The number of iterations to perform in the approximated
#' calculation
#' @param n_players The number of players
#' @param echo Show progress of the calculation.
#'
#' @references Castro, J., Gómez, D., & Tejada, J. (2009). Polynomial calculation
#' of the Shapley value based on sampling. Computers & operations research,
#' 36(5), 1726-1730.
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @return The Shapley value for each player

shapley_appro <- function(characteristic_func,n_players,n_rep, echo){

  is_fun <- is.function(characteristic_func)
  n_rep <- as.integer(n_rep)
  if (is_fun) {
    n_players <- as.integer(n_players)
  } else {
    values <- game_values(characteristic_func)
    n_players <- as.integer(round(log2(length(values))))
  }

  if (!is_fun) {
    # Convert the coalitions() order to binary-mask order
    k <- 0L
    key <- 0L
    for (i in seq_len(n_players)) {
      k <- c(k, k + 1L)
      key <- c(2L * key, 2L * key + 1L)
    }
    ord <- order(k, -key, method = "radix")
    v_binary <- numeric(2^n_players)
    v_binary[ord] <- values
    rm(values, ord, key, k)
    player_bit <- 2^(seq_len(n_players) - 1L)
  }

  shapley_value <- numeric(n_players)
  positions <- seq_len(n_players)
  if (echo) {
    pb <- txtProgressBar(min = 0, max = n_rep, style = 3)
    on.exit(close(pb), add = TRUE)
    progress_step <- max(1, ceiling(n_rep / 100))
    next_progress <- progress_step
  }

  if (is_fun) {
    empty_value <- characteristic_func(integer(0))
    for (rep in seq_len(n_rep)) {
      # O in pi(N) with probability 1/n!
      perm <- sample.int(n_players)
      member <- logical(n_players)
      previous_value <- empty_value
      for (pos in positions) {
        player <- perm[pos]
        member[player] <- TRUE
        # v(Pre_i(O) U {i}), coalition given in increasing order
        current_value <- characteristic_func(which(member))
        shapley_value[player] <-
          shapley_value[player] + current_value - previous_value
        previous_value <- current_value
      }
      if (echo && (rep >= next_progress || rep == n_rep)) {
        setTxtProgressBar(pb, rep)
        next_progress <- next_progress + progress_step
      }
    }

  } else {
    empty_value <- v_binary[1L]
    for (rep in seq_len(n_rep)) {
      # O in pi(N) with probability 1/n!
      perm <- sample.int(n_players)
      current_values <- v_binary[cumsum(player_bit[perm]) + 1L]
      previous_values <- c(empty_value, current_values[-n_players])
      shapley_value[perm] <- shapley_value[perm] + current_values - previous_values
      if (echo && (rep >= next_progress || rep == n_rep)) {
        setTxtProgressBar(pb, rep)
        next_progress <- next_progress + progress_step
      }
    }
  }

  shapley_value <- shapley_value / n_rep
  names(shapley_value) <- seq_len(n_players)
  return(shapley_value)

}
