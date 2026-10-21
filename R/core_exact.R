#' @title Exact core of the game
#'
#' @description Calculate the vertices of core of the game. The vertices are
#' enumerated with the double-description method of cddlib (package
#' \code{rcdd}) in dimension \code{n_players - 1}, after reducing the problem
#' with the individual lower bounds.
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param arithmetic Arithmetic used in the polyhedral computations:
#' \code{"double"} (floating point) or \code{"rational"} (exact rational
#' arithmetic; only the final vertices are converted to doubles).
#' @param tol Positive tolerance used to compare values in floating point
#' arithmetic and to clean values close to zero.
#'
#' @return The vertices of the core
#'
#' @importFrom utils combn

core_exact <- function(characteristic_func, n_players = 0,
                       arithmetic = "double", tol = 1e-10) {

  is_fun <- is.function(characteristic_func)
  if (is_fun) {
    n <- as.integer(n_players)
  } else {
    # The empty coalition is assumed to have value 0
    values <- game_values(characteristic_func)[-1L]
    n <- as.integer(round(log2(length(values) + 1)))
  }

  # coalitions() orders coalitions by cardinality and, within each
  # cardinality, in combn() lexicographic order. Reverse-bit keys reproduce
  # that order from binary masks (player 1 is the least significant bit).
  # Masks are doubles, so there is no limit on the number of players.
  sizes <- 0L
  key <- 0
  for (j in seq_len(n)) {
    sizes <- c(sizes, sizes + 1L)
    key <- c(2 * key, 2 * key + 1)
  }
  ord <- order(sizes, -key, method = "radix")[-1L] - 1
  rm(sizes, key)

  bits <- 2^(seq_len(n) - 1L)
  # TRUE for the masks that contain player j
  has_player <- function(masks, j) (masks %/% bits[j]) %% 2 == 1

  if (is_fun) {
    values <- vapply(ord, function(mask) {
      value <- characteristic_func(which((mask %/% bits) %% 2 == 1))
      if (!is.numeric(value) || is.complex(value) ||
          length(value) != 1L || !is.finite(value)) {
        stop("characteristic_func must return one finite real numeric value.")
      }
      as.numeric(value)
    }, numeric(1))
  }

  # v[mask] is the value of the coalition with binary mask 'mask'
  count <- 2^n - 1
  v <- numeric(count)
  v[ord] <- values
  rm(values, ord)

  # ==========================================================
  # DOUBLE ARITHMETIC
  # ==========================================================

  if (arithmetic == "double") {

    # Shift every allocation by the individual lower bounds v({i})
    lower <- v[bits]
    subset_lower <- 0
    for (q in lower) subset_lower <- c(subset_lower, subset_lower + q)
    residual <- v - subset_lower[-1L]
    budget <- residual[count]
    scale <- max(1, abs(v), abs(subset_lower[-1L]))
    eps <- tol * scale
    rm(v, subset_lower)

    if (budget < -eps) {
      warning("The core is empty: individual lower bounds exceed v(N).")
      return(NULL)
    }

    if (abs(budget) <= eps) {
      if (any(residual > eps)) {
        warning(paste0(
          "The core is empty: the only individually feasible ",
          "allocation violates a coalition constraint."
        ))
        return(NULL)
      }
      vertices <- matrix(lower, nrow = 1L)
      vertices[abs(vertices) < eps] <- 0
      colnames(vertices) <- seq_len(n)
      rownames(vertices) <- NULL
      return(vertices)
    }

    # Keep only the constraints that are not implied by y >= 0
    keep <- residual > eps
    keep[bits] <- TRUE
    keep[count] <- FALSE
    masks <- which(keep)
    has_last <- as.integer(has_player(masks, n))

    # H-representation in the first n - 1 coordinates (y_n = budget - sum y)
    H <- matrix(0, nrow = length(masks), ncol = n + 1L)
    constant <- -residual[masks]
    selected <- which(has_last == 1L)
    if (length(selected)) constant[selected] <- constant[selected] + budget
    H[, 2L] <- constant
    for (j in seq_len(n - 1L)) {
      H[, j + 2L] <- as.numeric(has_player(masks, j)) - has_last
    }
    rm(residual, keep, masks, has_last, constant, selected)

    generators <- rcdd::scdd(H, representation = "H")$output
    rm(H)

    if (!nrow(generators)) {
      warning("The core is empty.")
      return(NULL)
    }
    if (any(abs(generators[, 1L]) > tol) ||
        any(abs(generators[, 2L] - 1) > tol)) {
      stop("Unexpected unbounded representation for the core.")
    }

    y <- generators[, -(1:2), drop = FALSE]
    last_y <- budget - rowSums(y)
    vertices <- sweep(cbind(y, last_y), 2L, lower, "+")

    cleanup_tol <- tol * max(1, abs(vertices))
    vertices[abs(vertices) <= cleanup_tol] <- 0
    colnames(vertices) <- seq_len(n)
    rownames(vertices) <- NULL
    return(vertices)
  }

  # ==========================================================
  # RATIONAL ARITHMETIC
  # ==========================================================

  vq <- rcdd::d2q(v)
  rm(v)

  lower <- vq[bits]
  subset_lower <- "0"
  for (q in lower) {
    subset_lower <- c(subset_lower,
                      rcdd::qpq(subset_lower, rep(q, length(subset_lower))))
  }
  residual <- rcdd::qpq(vq, rcdd::qneg(subset_lower[-1L]))
  budget <- residual[count]
  signs <- rcdd::qsign(residual)
  rm(vq, subset_lower)

  if (signs[count] < 0) {
    warning("The core is empty: individual lower bounds exceed v(N).")
    return(NULL)
  }

  if (signs[count] == 0) {
    if (any(signs > 0)) {
      warning(paste0(
        "The core is empty: the only individually feasible ",
        "allocation violates a coalition constraint."
      ))
      return(NULL)
    }
    vertices <- rcdd::q2d(matrix(lower, nrow = 1L))
    colnames(vertices) <- seq_len(n)
    rownames(vertices) <- NULL
    return(vertices)
  }

  keep <- signs > 0
  keep[bits] <- TRUE
  keep[count] <- FALSE
  masks <- which(keep)
  has_last <- as.integer(has_player(masks, n))

  Hq <- matrix("0", nrow = length(masks), ncol = n + 1L)
  constant <- rcdd::qneg(residual[masks])
  selected <- which(has_last == 1L)
  if (length(selected)) {
    constant[selected] <- rcdd::qpq(constant[selected],
                                    rep(budget, length(selected)))
  }
  Hq[, 2L] <- constant
  for (j in seq_len(n - 1L)) {
    Hq[, j + 2L] <- as.character(
      as.integer(has_player(masks, j)) - has_last
    )
  }
  rm(residual, signs, keep, masks, has_last, constant, selected)

  generators <- rcdd::scdd(Hq, representation = "H")$output
  rm(Hq)

  if (!nrow(generators)) {
    warning("The core is empty.")
    return(NULL)
  }
  if (any(generators[, 1L] != "0") || any(generators[, 2L] != "1")) {
    stop("Unexpected unbounded representation for the core.")
  }

  y <- generators[, -(1:2), drop = FALSE]
  total_y <- as.vector(rcdd::qmatmult(y, matrix("1", nrow = n - 1L, ncol = 1L)))
  last_y <- rcdd::qpq(rep(budget, nrow(y)), rcdd::qneg(total_y))
  vertices <- cbind(y, last_y)
  for (j in seq_len(n)) {
    vertices[, j] <- rcdd::qpq(vertices[, j], rep(lower[j], nrow(vertices)))
  }

  # Exact rational computation up to this point.
  # Convert only the final output to double.
  vertices <- rcdd::q2d(vertices)
  colnames(vertices) <- seq_len(n)
  rownames(vertices) <- NULL

  return(vertices)
}
