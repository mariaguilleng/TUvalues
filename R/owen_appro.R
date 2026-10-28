#' @title Owen value (approximation)
#' @noRd
#'
#' @description Calculate the approximated Owen value based on sampling, using
#' the algorithm proposed by Saavedra-Nieves et al. (2018): \code{n_rep} random
#' permutations of the players in which the members of each union appear
#' consecutively, each giving one marginal contribution per player.
#' Internal function called by \code{owen}, which checks the arguments.
#'
#' @param characteristic_func The characteristic function of the game, already
#' checked: a numeric vector of length \eqn{2^n - 1} or \eqn{2^n} in the order
#' given by \code{coalitions}, or a function that receives a coalition.
#' @param union The a priori unions, already checked: a list of integer
#' vectors that form a partition of the players.
#' @param n_players The number of players, already checked. Only used if
#' \code{characteristic_func} is a function.
#' @param n_rep The number of iterations of the sampling, already checked.
#' @param echo Show progress of the calculation.
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @return The Owen value for each player.
#'
#' @references Saavedra-Nieves, A., García-Jurado, I., & Fiestras-Janeiro, M. G. (2018).
#' Estimation of the Owen value based on sampling. In The mathematics of the uncertain:
#' A tribute to Pedro Gil (pp. 347-356). Cham: Springer International Publishing.

owen_appro <- function(characteristic_func, union, n_players, n_rep, echo) {

  is_fun <- is.function(characteristic_func)
  n_rep <- as.integer(n_rep)
  if (is_fun) {
    n_players <- as.integer(n_players)
  } else {
    values <- game_values(characteristic_func)
    n_players <- as.integer(round(log2(length(values))))
  }

  n_unions <- length(union)
  block_sizes <- lengths(union)
  block_positions <- seq_len(n_unions)
  singleton_blocks <- all(block_sizes == 1L)
  if (singleton_blocks) singletons <- unlist(union, use.names = FALSE)
  pieces <- vector("list", n_unions)

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

  owen_value <- numeric(n_players)
  positions <- seq_len(n_players)
  if (echo) {
    pb <- txtProgressBar(min = 0, max = n_rep, style = 3)
    on.exit(close(pb), add = TRUE)
    progress_step <- max(1, ceiling(n_rep / 100))
    next_progress <- progress_step
  }
  previous_empty <- if (is_fun) characteristic_func(integer(0)) else v_binary[1L]

  for (rep in seq_len(n_rep)) {
    # Choose a valid perm: order the unions, then the players inside each union
    block_order <- sample.int(n_unions)
    if (singleton_blocks) {
      perm <- singletons[block_order]
    } else if (n_unions == 1L) {
      perm <- union[[1L]][sample.int(n_players)]
    } else {
      for (j in block_positions) {
        g <- block_order[j]
        block <- union[[g]]
        pieces[[j]] <- if (block_sizes[g] == 1L) {
          block
        } else {
          block[sample.int(block_sizes[g])]
        }
      }
      perm <- unlist(pieces, use.names = FALSE)
    }

    if (is_fun) {
      previous_value <- previous_empty
      member <- logical(n_players)
      for (pos in positions) {
        player <- perm[pos]
        member[player] <- TRUE
        # v(Pre_i(O) U {i}), coalition given in increasing order
        current_value <- characteristic_func(which(member))
        owen_value[player] <- owen_value[player] + current_value - previous_value
        previous_value <- current_value
      }
    } else {
      current_values <- v_binary[cumsum(player_bit[perm]) + 1L]
      previous_values <- c(previous_empty, current_values[-n_players])
      owen_value[perm] <- owen_value[perm] + current_values - previous_values
    }
    if (echo && (rep >= next_progress || rep == n_rep)) {
      setTxtProgressBar(pb, rep)
      next_progress <- next_progress + progress_step
    }
  }

  owen_value <- owen_value / n_rep
  names(owen_value) <- positions
  return(owen_value)
}
