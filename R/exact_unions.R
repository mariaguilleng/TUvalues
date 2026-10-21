# Exact Owen value (type "owen") or Banzhaf-Owen value (type "banzhaf") for
# an a priori partition 'union' (a checked list of integer player vectors).
#
# For each union g, the reduced game played by its members is
#   h(T) = sum over R subset of the other unions of w(|R|) v(Q_R U T),
# where Q_R is the union of the unions in R, and the value of the members
# of g is the Shapley (Owen) or Banzhaf (Banzhaf-Owen) value of h.
# Unions with one player take the value of the quotient game directly.
#
# A vector game is accessed through bit masks (at most 30 players). A function
# game is evaluated on explicit sets of players, so there is no limit on the
# number of players: only the coalitions Q_R U T are evaluated.
# The empty coalition takes value 0 for functions; for vectors every value is
# shifted by v(empty), which leaves the marginal contributions unchanged.

exact_unions <- function(characteristic_func, union, n_players,
                         type = c("owen", "banzhaf")) {

  type <- match.arg(type)
  is_fun <- is.function(characteristic_func)
  m <- length(union)

  # Number of elements of each subset, in binary order
  subset_sizes <- function(k) {
    sizes <- 0L
    for (i in seq_len(k)) sizes <- c(sizes, sizes + 1L)
    sizes
  }
  # Bit masks of the subsets, in binary order
  subset_masks <- function(x) {
    masks <- 0
    for (b in x) masks <- c(masks, masks + b)
    masks
  }

  if (is_fun) {
    n <- as.integer(n_players)

    # Players of every union of groups, in binary order
    subset_players <- function(groups) {
      sets <- list(integer(0))
      for (group in groups) sets <- c(sets, lapply(sets, function(s) c(s, group)))
      sets
    }
    value_of <- function(players) {
      if (!length(players)) return(0)
      value <- characteristic_func(sort(players))
      if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
        stop("characteristic_func must return one finite numeric value.")
      }
      as.numeric(value)
    }
    whole_values <- vapply(subset_players(union), value_of, numeric(1))

  } else {
    values <- game_values(characteristic_func)
    n <- as.integer(round(log2(length(values))))

    # coalitions() orders coalitions by cardinality and,
    # within each cardinality, in combn() lexicographic order.
    # Reverse-bit keys reproduce that order from binary masks.
    k <- 0L
    key <- 0L
    for (i in seq_len(n)) {
      k <- c(k, k + 1L)
      key <- c(2L * key, 2L * key + 1L)
    }
    ord <- order(k, -key, method = "radix")
    v <- numeric(2^n)
    v[ord] <- values - values[1L]
    rm(values, ord, key, k)

    bits <- 2^(seq_len(n) - 1L)
    block_masks <- vapply(union, function(g) sum(bits[g]), numeric(1))
    whole_values <- v[subset_masks(block_masks) + 1]
  }

  # Weights of the coalitions of unions R, by |R|
  if (type == "owen") {
    outer_weights <- exp(-log(m) - lchoose(m - 1, 0:(m - 1)))
  } else {
    outer_weights <- rep(1 / 2^(m - 1), m)
  }

  # Shapley value of a game h given in binary order
  reduce_shapley <- function(h, sizes, p) {
    h <- h - h[1L]
    w <- exp(-log(p) - lchoose(p - 1, 0:(p - 1)))
    a <- c(0, w)[sizes + 1L]
    b <- c(w, 0)[sizes + 1L]
    offset <- sum(h * b)
    z <- h * (a + b)
    answer <- numeric(p)
    for (j in p:1L) {
      half <- length(z) %/% 2L
      idx <- seq_len(half)
      right <- z[idx + half]
      answer[j] <- sum(right) - offset
      if (j > 1L) z <- z[idx] + right
    }
    answer
  }
  # Banzhaf value of a game h given in binary order
  reduce_banzhaf <- function(h, sizes, p) {
    z <- h - h[1L]
    answer <- numeric(p)
    for (j in p:1L) {
      half <- length(z) %/% 2L
      idx <- seq_len(half)
      left <- z[idx]
      right <- z[idx + half]
      answer[j] <- mean(right - left)
      if (j > 1L) z <- left / 2 + right / 2
    }
    answer
  }
  reduce <- if (type == "owen") reduce_shapley else reduce_banzhaf

  value <- numeric(n)
  block_bits <- 2^(seq_len(m) - 1L)

  if (any(lengths(union) == 1L)) {
    block_value <- reduce(whole_values, subset_sizes(m), m)
  }

  for (g in seq_len(m)) {
    members <- union[[g]]
    s <- length(members)
    if (s == 1L) {
      value[members] <- block_value[g]
      next
    }

    # Coalitions of complete external unions, as indices of whole_values
    outside_ids <- subset_masks(block_bits[-g])
    weights <- outer_weights[subset_sizes(m - 1L) + 1L]
    n_outside <- length(outside_ids)
    inside_sizes <- subset_sizes(s)
    count_inside <- length(inside_sizes)

    # Reduced game played by the members of union g
    h <- numeric(count_inside)
    h[1L] <- sum(weights * whole_values[outside_ids + 1])
    h[count_inside] <- sum(weights * whole_values[outside_ids + block_bits[g] + 1])

    if (is_fun) {
      inside_sets <- subset_players(as.list(members))
      outside_sets <- subset_players(union[-g])
    } else {
      inside_masks <- subset_masks(bits[members])
      outside_masks <- subset_masks(block_masks[-g])
    }

    # Proper nonempty subsets of the union, evaluated by blocks of rows
    interior <- seq.int(2L, count_inside - 1L)
    rows_per_block <- max(1L, 65536L %/% n_outside)
    for (start in seq.int(1L, length(interior), by = rows_per_block)) {
      rows <- interior[start:min(start + rows_per_block - 1L, length(interior))]
      n_rows <- length(rows)
      if (is_fun) {
        # Column-major order: the row index varies fastest
        vals <- vapply(seq_len(n_rows * n_outside), function(cell) {
          r <- rows[(cell - 1L) %% n_rows + 1L]
          o <- (cell - 1L) %/% n_rows + 1L
          value_of(c(inside_sets[[r]], outside_sets[[o]]))
        }, numeric(1))
      } else {
        vals <- v[as.vector(outer(inside_masks[rows], outside_masks, "+")) + 1]
      }
      h[rows] <- drop(matrix(vals, nrow = n_rows) %*% weights)
    }

    value[members] <- reduce(h, inside_sizes, s)
  }

  names(value) <- seq_len(n)
  value
}
