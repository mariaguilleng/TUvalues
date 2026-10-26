#' @title Banzhaf-Owen Value
#' @noRd
#'
#' @description Calculate the approximated Banzhaf-Owen value using the
#' algorithm proposed by Saavedra-Nieves & Fiestras-Janeiro (2021).
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param union List of vectors indicating the a priori unions between the
#' players
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#' @param n_players The number of players
#' @param replace Should sampling be with replacement?
#' @param echo Show progress of the calculation.
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom stats runif
#'
#' @return The Banzhaf-Owen Index for each player
#'
#' @references Saavedra-Nieves, A., & Fiestras-Janeiro, M. G. (2021). Sampling
#' methods to estimate the Banzhaf–Owen value. Annals of Operations Research,
#' 301(1), 199-223.

banzhaf_owen_appro <- function(characteristic_func, union, n_players, n_rep, replace, echo) {

  is_fun <- is.function(characteristic_func)
  n_rep <- as.integer(n_rep)
  if (is_fun) {
    n_players <- as.integer(n_players)
  } else {
    values <- game_values(characteristic_func)
    n_players <- as.integer(round(log2(length(values))))
  }

  n_unions <- length(union)
  player_union <- integer(n_players)
  for (u in seq_len(n_unions)) player_union[union[[u]]] <- u
  union_sizes <- lengths(union)

  # For player i, a configuration is a choice of other unions plus a choice
  # of partners inside the own union: d = (m - 1) + (|U_i| - 1) binary choices.
  dimensions <- (n_unions - 1L) + (union_sizes[player_union] - 1L)
  if (replace) {
    reps_per_player <- rep(n_rep, n_players)
  } else {
    max_configurations <- ifelse(dimensions <= 30L, 2^dimensions, Inf)
    reps_per_player <- pmin(n_rep, max_configurations)
  }

  if (!is_fun) {
    # Convert the coalitions() order to binary-mask order
    n <- n_players
    k <- 0L
    for (j in seq_len(n)) k <- c(k, k + 1L)
    key <- 0
    for (j in seq_len(n)) key <- c(2 * key, 2 * key + 1)
    ord <- order(k, -key, method = "radix")
    v_binary <- numeric(2^n)
    v_binary[ord] <- values
    rm(values, ord, key, k)
    player_bit <- 2^(seq_len(n) - 1L)
    union_mask <- vapply(union, function(block) sum(player_bit[block]), numeric(1))
  }

  banzhaf_owen_value <- numeric(n_players)
  total_iterations <- sum(reps_per_player)
  if (echo) {
    pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)
    on.exit(close(pb), add = TRUE)
    progress_step <- max(1, ceiling(total_iterations / 100))
    next_progress <- progress_step
  }
  progress <- 0

  for (i in seq_len(n_players)) {
    own_union_id <- player_union[i]
    own_union <- union[[own_union_id]]
    own_others <- own_union[own_union != i]
    other_union_ids <- setdiff(seq_len(n_unions), own_union_id)
    d <- length(other_union_ids) + length(own_others)
    reps_i <- as.integer(reps_per_player[i])

    if (is_fun) {
      components <- c(union[other_union_ids], as.list(own_others))
    } else {
      component_masks <- c(union_mask[other_union_ids], player_bit[own_others])
    }

    # Without replacement: sample distinct configuration codes
    if (!replace && d <= 30L) {
      codes <- sample.int(2^d, size = reps_i, replace = FALSE) - 1L
      config_bits <- if (d > 0L) as.integer(2^(0:(d - 1L))) else integer(0)
    }

    # Vectorised evaluation in batches for vector games
    if (!is_fun && d <= 30L) {
      batch_size <- 4096L
      completed <- 0L
      total <- 0
      while (completed < reps_i) {
        batch_n <- min(batch_size, reps_i - completed)
        masks <- numeric(batch_n)
        if (d > 0L) {
          if (replace) {
            selected_batch <- matrix(runif(d * batch_n) < 0.5,
                                     nrow = d, ncol = batch_n)
            for (j in seq_len(d)) {
              masks <- masks + component_masks[j] * selected_batch[j, ]
            }
          } else {
            batch_codes <- codes[completed + seq_len(batch_n)]
            for (j in seq_len(d)) {
              masks <- masks + component_masks[j] *
                (bitwAnd(batch_codes, config_bits[j]) != 0L)
            }
          }
        }
        total <- total + sum(
          v_binary[masks + player_bit[i] + 1] - v_binary[masks + 1]
        )
        completed <- completed + batch_n
        if (echo) {
          progress <- progress + batch_n
          if (progress >= next_progress || progress == total_iterations) {
            setTxtProgressBar(pb, progress)
            next_progress <- (floor(progress / progress_step) + 1) * progress_step
          }
        }
      }
      banzhaf_owen_value[i] <- total
      next
    }

    if (!replace && d > 30L) {
      used <- new.env(hash = TRUE, parent = emptyenv())
    }

    for (rep in seq_len(reps_i)) {
      if (replace) {
        selected <- if (d == 0L) integer(0) else which(runif(d) < 0.5)
      } else if (d <= 30L) {
        selected <- if (d == 0L) {
          integer(0)
        } else {
          which(bitwAnd(as.integer(codes[rep]), config_bits) != 0L)
        }
      } else {
        repeat {
          selected <- which(runif(d) < 0.5)
          config_key <- if (length(selected) == 0L) "empty" else paste(selected, collapse = ",")
          if (!exists(config_key, envir = used, inherits = FALSE)) {
            assign(config_key, TRUE, envir = used)
            break
          }
        }
      }

      if (is_fun) {
        coalition <- if (length(selected) == 0L) {
          integer(0)
        } else {
          unlist(components[selected], use.names = FALSE)
        }
        # v(S)
        v_without <- characteristic_func(coalition)
        # v(S U {i})
        v_with <- characteristic_func(c(coalition, i))
      } else {
        mask <- if (length(selected) == 0L) 0 else sum(component_masks[selected])
        v_without <- v_binary[mask + 1L]
        v_with <- v_binary[mask + player_bit[i] + 1L]
      }
      # x_i = v(S U {i}) - v(S)
      banzhaf_owen_value[i] <- banzhaf_owen_value[i] + v_with - v_without

      if (echo) {
        progress <- progress + 1
        if (progress >= next_progress || progress == total_iterations) {
          setTxtProgressBar(pb, progress)
          next_progress <- (floor(progress / progress_step) + 1) * progress_step
        }
      }
    }
  }

  banzhaf_owen_value <- banzhaf_owen_value / reps_per_player
  names(banzhaf_owen_value) <- seq_len(n_players)
  return(banzhaf_owen_value)

}
