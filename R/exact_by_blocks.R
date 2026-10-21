# Exact Shapley or Banzhaf value of a game given as a function, without storing
# the 2^n coalition values. Coalitions are processed in blocks of 2^L that
# share the same "high" players (L + 1, ..., n): the low players are handled
# with the binary reduction inside the block, and the high players belong to
# every coalition of the block. Memory is O(2^L) and time O(2^n), the same
# number of evaluations of characteristic_func as the full computation.
# The high players of each block are enumerated with a logical binary counter,
# so there is no limit on the number of players.
# The empty coalition is assumed to have value 0 and is never evaluated.

exact_by_blocks <- function(characteristic_func, n, type = c("shapley", "banzhaf"),
                            block_bits = 16L) {

  type <- match.arg(type)
  L <- min(n, block_bits)
  H <- n - L

  low_bits <- as.integer(2^(seq_len(L) - 1L))
  low_sets <- lapply(0:(2^L - 1L), function(mask) which(bitwAnd(mask, low_bits) != 0L))
  low_size <- lengths(low_sets)

  if (type == "shapley") {
    w <- exp(-log(n) - lchoose(n - 1, 0:(n - 1)))
    # Index |S| + 1: weight of v(S) for players inside / outside S
    a_k <- c(0, w)
    b_k <- c(w, 0)
  }

  # contain[i] accumulates z(S) over the coalitions S containing i
  contain <- numeric(n)
  offset <- 0

  # high[j] is TRUE if player L + j belongs to the coalitions of the block
  high <- logical(H)
  first_block <- TRUE

  repeat {
    high_set <- L + which(high)
    vals <- numeric(2^L)
    idx <- if (first_block) seq.int(2L, 2^L) else seq_len(2^L)
    if (length(idx)) {
      vals[idx] <- vapply(idx, function(j) {
        characteristic_func(c(low_sets[[j]], high_set))
      }, numeric(1))
    }
    if (any(!is.finite(vals))) stop("Coalition values must be finite.")

    if (type == "shapley") {
      k <- low_size + length(high_set) + 1L
      offset <- offset + sum(vals * b_k[k])
      z <- vals * (a_k[k] + b_k[k])
    } else {
      offset <- offset + sum(vals)
      z <- vals
    }

    # High players are in every coalition of the block
    if (length(high_set)) contain[high_set] <- contain[high_set] + sum(z)

    # Binary reduction for the low players
    for (i in L:1L) {
      half <- length(z) %/% 2L
      half_idx <- seq_len(half)
      right <- z[half_idx + half]
      contain[i] <- contain[i] + sum(right)
      if (i > 1L) z <- z[half_idx] + right
    }

    # Next block: add one to the binary counter of the high players
    first_block <- FALSE
    pos <- match(FALSE, high)
    if (is.na(pos)) break
    high[seq_len(pos - 1L)] <- FALSE
    high[pos] <- TRUE
  }

  if (type == "shapley") {
    value <- contain - offset
  } else {
    value <- (2 * contain - offset) / 2^(n - 1L)
  }
  names(value) <- seq_len(n)
  value
}
