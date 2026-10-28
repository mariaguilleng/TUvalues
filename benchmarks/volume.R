# =====================================================================
# Core Volume, Volume Ratio, and Core Membership
# Compatible with exact and approximated core vertices (Camacho et al., 2025)
# =====================================================================

#' @title Volume of the core of the game
#'
#' @description
#' Calculate the volume of the core of a cooperative game (exact or approximated),
#' or of the convex hull formed by a given set of vertices.
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players, a vector of game values, or a matrix of precomputed core vertices.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param method Method used to calculate the core if vertices are not supplied directly.
#' Valid methods are: \code{"exact"} for the exact calculation or \code{"appro"} for
#' approximated core based on Camacho et al. (2025).
#' @param n_rep Only used if \code{method} is \code{"appro"}. The number of
#' iterations to perform in the approximated calculation.
#' @param echo Only used if \code{method} is \code{"appro"}. Show progress of the
#' approximated calculation.
#' @param engine Engine used to compute the volume: \code{"convhull"} for exact geometric
#' volume via convex hull (using the \code{geometry} package or exact analytic methods),
#' or \code{"mc"} for Adaptive Ellipsoid Monte Carlo.
#' @param intrinsic Logical. If \code{TRUE}, returns the intrinsic \eqn{(n-1)}-dimensional
#' volume on the hyperplane \eqn{\sum x_i = v(N)} (scaled by \eqn{\sqrt{n}}).
#' If \code{FALSE} (default), returns the projected volume dropping the \eqn{n}-th coordinate.
#' Note that the ratio between approximate and exact volumes is identical in both cases.
#' @param ... Additional parameters passed to the engine (e.g., \code{min_hits}, \code{batch_size} for \code{"mc"}).
#'
#' @return The volume of the core (numeric value). Returns 0 if the core is empty,
#' a single point, or lower-dimensional.
#'
#' @references Camacho, J., Gonçalves-Dosantos, J. C., & Sánchez-Soriano, J. (2025).
#' A Linear Programming Approach to Estimate the Core in Cooperative Games.
#' arXiv preprint arXiv:2510.01766.
#'
#' @examples
#' v <- c(2, 3, 5, 5, 7, 8, 10)
#' core_volume(v, method = "exact")
#' core_volume(v, method = "appro", n_rep = 200)
#'
#' @export
core_volume <- function(characteristic_func, n_players = 0, method = "exact",
                        n_rep = 1000, echo = FALSE, engine = "convhull",
                        intrinsic = FALSE, ...) {

  if (!engine %in% c("convhull", "mc")) {
    stop("Invalid engine specified. Choose either \"convhull\" or \"mc\".")
  }

  # If a matrix or data.frame is provided, treat it directly as vertices
  if (is.matrix(characteristic_func) || is.data.frame(characteristic_func)) {
    vertices <- as.matrix(characteristic_func)
    n_players <- ncol(vertices)
    char_func_prepared <- NULL
  } else {
    if (!method %in% c("exact", "appro")) {
      stop("Invalid method specified. Use \"exact\" for exact calculation or \"appro\" for approximation.")
    }
    if (!is.vector(characteristic_func) && !is.function(characteristic_func)) {
      stop("Invalid characteristic_func provided. Must be a function, a vector, or a matrix of vertices.")
    }
    if (is.function(characteristic_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }

    # Adapt function if it takes 2 arguments (e.g. v(T_set, n))
    char_func_prepared <- prepare_game_function(characteristic_func, n_players)

    # Calculate core vertices
    vertices <- coreVertex(char_func_prepared, n_players = n_players,
                           method = method, n_rep = n_rep, echo = echo)
  }

  if (is.null(vertices) || nrow(vertices) == 0) {
    warning("The core is empty or no vertices were found.")
    return(0)
  }

  # Remove duplicate vertices
  vertices <- unique(vertices)
  m <- nrow(vertices)
  n <- ncol(vertices)

  if (n < 2) {
    return(0)
  }

  d <- n - 1 # Dimension of the affine subspace on sum(x_i) = v(N)

  # A polytope in d dimensions needs at least d + 1 vertices for positive volume
  if (m < d + 1) {
    return(0)
  }

  if (engine == "convhull") {
    vol_proj <- compute_convhull_volume(vertices)
  } else {
    # Monte Carlo estimation
    vol_proj <- estimate_core_volume_emc(extreme_points = vertices,
                                         v_func = char_func_prepared,
                                         n = n, ...)
  }

  if (intrinsic) {
    return(sqrt(n) * vol_proj)
  } else {
    return(vol_proj)
  }
}

#' @rdname core_volume
#' @export
volume <- core_volume


#' @title Ratio between Approximated and Exact Core Volume
#'
#' @description
#' Calculates the ratio between the volume of the approximated core and the exact core:
#' \deqn{Ratio = \frac{Volume(Core_{appro})}{Volume(Core_{exact})}}
#' Since the approximated core is an inner approximation formed by a subset of core extreme
#' points, \eqn{Ratio \in [0, 1]}.
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players, or a vector of game values. Ignored if both \code{vertices_exact} and
#' \code{vertices_appro} are provided.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param n_rep Number of iterations to perform in the approximated core calculation.
#' @param echo Show progress of the approximated calculation.
#' @param vertices_exact Optional matrix of precomputed exact core vertices.
#' @param vertices_appro Optional matrix of precomputed approximated core vertices.
#' @param engine Engine used to compute the volume (\code{"convhull"} or \code{"mc"}).
#' @param intrinsic Logical. Whether to compute intrinsic or projected volume (the ratio is identical).
#' @param ... Additional arguments passed to \code{core_volume}.
#'
#' @return An object of class \code{"core_volume_ratio"} containing:
#' \item{ratio}{The volume ratio (approximated / exact).}
#' \item{vol_appro}{The volume of the approximated core.}
#' \item{vol_exact}{The volume of the exact core.}
#' \item{vertices_appro}{Number of vertices in the approximated core.}
#' \item{vertices_exact}{Number of vertices in the exact core.}
#'
#' @examples
#' v <- c(2, 3, 5, 5, 7, 8, 10)
#' core_volume_ratio(v, n_rep = 200)
#'
#' @export
core_volume_ratio <- function(characteristic_func = NULL, n_players = 0,
                              n_rep = 1000, echo = FALSE,
                              vertices_exact = NULL, vertices_appro = NULL,
                              engine = "convhull", intrinsic = FALSE, ...) {

  # Obtain exact vertices if not passed directly
  if (is.null(vertices_exact)) {
    if (is.null(characteristic_func)) {
      stop("Either 'vertices_exact' or 'characteristic_func' must be provided.")
    }
    char_func_prepared <- prepare_game_function(characteristic_func, n_players)
    vertices_exact <- coreVertex(char_func_prepared, n_players = n_players,
                                 method = "exact")
  }

  # Obtain approximated vertices if not passed directly
  if (is.null(vertices_appro)) {
    if (is.null(characteristic_func)) {
      stop("Either 'vertices_appro' or 'characteristic_func' must be provided.")
    }
    char_func_prepared <- prepare_game_function(characteristic_func, n_players)
    vertices_appro <- coreVertex(char_func_prepared, n_players = n_players,
                                 method = "appro", n_rep = n_rep, echo = echo)
  }

  # Compute volumes
  vol_exact <- core_volume(vertices_exact, engine = engine, intrinsic = intrinsic, ...)
  vol_appro <- core_volume(vertices_appro, engine = engine, intrinsic = intrinsic, ...)

  n_v_exact <- if (is.null(vertices_exact)) 0 else nrow(vertices_exact)
  n_v_appro <- if (is.null(vertices_appro)) 0 else nrow(vertices_appro)

  if (is.null(vol_exact) || vol_exact == 0) {
    if (is.null(vol_appro) || vol_appro == 0) {
      ratio <- 1.0 # Both have zero volume
    } else {
      ratio <- Inf
    }
  } else {
    ratio <- vol_appro / vol_exact
  }

  res <- list(
    ratio = ratio,
    vol_appro = vol_appro,
    vol_exact = vol_exact,
    vertices_appro = n_v_appro,
    vertices_exact = n_v_exact
  )
  class(res) <- "core_volume_ratio"
  return(res)
}

#' @rdname core_volume_ratio
#' @export
volume_ratio <- core_volume_ratio

#' @export
print.core_volume_ratio <- function(x, ...) {
  cat("Core Volume Ratio (Approximated / Exact):\n")
  cat(sprintf("  Ratio:               %.6f (%.2f%%)\n", x$ratio, x$ratio * 100))
  cat(sprintf("  Approximated Volume: %.6e (%d vertices)\n", x$vol_appro, x$vertices_appro))
  cat(sprintf("  Exact Volume:        %.6e (%d vertices)\n", x$vol_exact, x$vertices_exact))
  invisible(x)
}


#' @title Check if a point belongs to the core
#'
#' @description
#' Check whether an imputation point belongs to the core by verifying if it can be
#' represented as a convex combination of the core vertices (exact or approximated):
#' \deqn{x = \sum_{j=1}^m \lambda_j v_j, \quad \sum_{j=1}^m \lambda_j = 1, \quad \lambda_j \ge 0}
#'
#' @param point A numeric vector of length \code{n} representing the imputation to test,
#' or a matrix / data frame where each row is an imputation point.
#' @param vertices Matrix of core vertices (exact or approximated). If \code{NULL},
#' vertices will be calculated from \code{characteristic_func}.
#' @param characteristic_func The characteristic function or vector of the game.
#' Used to compute vertices if \code{vertices} is not provided.
#' @param n_players Number of players. Only used if \code{characteristic_func} is a function.
#' @param method Method used to calculate the core vertices if not provided:
#' \code{"exact"} or \code{"appro"}.
#' @param n_rep Number of iterations if \code{method = "appro"}.
#' @param echo Show progress if computing vertices.
#' @param tol Numerical tolerance for equality and feasibility.
#'
#' @return Logical value (\code{TRUE} or \code{FALSE}), or a logical vector if
#' \code{point} is a matrix of multiple points.
#'
#' @export
is_in_core <- function(point, vertices = NULL, characteristic_func = NULL,
                       n_players = 0, method = "exact", n_rep = 1000,
                       echo = FALSE, tol = 1e-7) {

  # Obtain vertices if needed
  if (is.null(vertices)) {
    if (is.null(characteristic_func)) {
      stop("Either 'vertices' or 'characteristic_func' must be provided.")
    }
    char_func_prepared <- prepare_game_function(characteristic_func, n_players)
    vertices <- coreVertex(char_func_prepared, n_players = n_players,
                           method = method, n_rep = n_rep, echo = echo)
  }

  if (is.null(vertices) || nrow(vertices) == 0) {
    # If the core is empty, no point can be in the core
    if (is.matrix(point) || is.data.frame(point)) {
      return(rep(FALSE, nrow(point)))
    }
    return(FALSE)
  }

  vertices <- as.matrix(vertices)

  # Support checking multiple points at once
  if (is.matrix(point) || is.data.frame(point)) {
    point_mat <- as.matrix(point)
    res <- apply(point_mat, 1, function(pt) {
      check_convex_combination(pt, vertices, tol = tol)
    })
    return(as.logical(res))
  }

  return(check_convex_combination(point, vertices, tol = tol))
}

#' @rdname is_in_core
#' @export
in_core <- is_in_core


# =====================================================================
# Internal Helpers
# =====================================================================

#' Wrap Game Function for Multi-Parameter Signatures
#'
#' Adapts game value functions that accept two arguments (e.g. v(T_set, n))
#' so they can be seamlessly called by combn() or 1-argument interfaces v(coalition).
#'
#' @param characteristic_func Function or vector.
#' @param n_players Number of players.
#' @return Prepared characteristic function or vector.
#' @noRd
prepare_game_function <- function(characteristic_func, n_players) {
  if (is.function(characteristic_func)) {
    arg_names <- names(formals(characteristic_func))
    # If it expects at least 2 arguments without defaults, wrap it
    if (length(arg_names) >= 2) {
      orig_func <- characteristic_func
      return(function(coalition) {
        orig_func(coalition, n_players)
      })
    }
  }
  return(characteristic_func)
}

#' Geometric Convex Hull Volume
#'
#' Computes the (n-1)-dimensional volume of vertices lying on the hyperplane
#' sum(x_i) = v(N) by projecting onto the first n-1 coordinates.
#'
#' @param vertices Matrix of vertices (rows = vertices, cols = player payoffs).
#' @return Projected volume in R^(n-1).
#' @noRd
compute_convhull_volume <- function(vertices) {
  n <- ncol(vertices)
  d <- n - 1
  P <- vertices[, -n, drop = FALSE]

  # 1D case (2 players): length of interval
  if (d == 1) {
    return(diff(range(P[, 1])))
  }

  # 2D case (3 players): try geometry, fallback to base R chull + Shoelace formula
  if (d == 2) {
    if (requireNamespace("geometry", quietly = TRUE)) {
      vol <- tryCatch({
        geometry::convhulln(P, options = "FA")$vol
      }, error = function(e) {
        geometry::convhulln(P, output.options = "FA")$vol
      })
      return(vol)
    } else {
      hull_idx <- grDevices::chull(P[, 1], P[, 2])
      hull_pts <- P[hull_idx, , drop = FALSE]
      k <- nrow(hull_pts)
      if (k >= 3) {
        x <- hull_pts[, 1]
        y <- hull_pts[, 2]
        return(0.5 * abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1]))))
      }
      return(0)
    }
  }

  # Higher dimensional case (d >= 3)
  if (requireNamespace("geometry", quietly = TRUE)) {
    vol <- tryCatch({
      geometry::convhulln(P, options = "FA")$vol
    }, error = function(e) {
      tryCatch({
        geometry::convhulln(P, output.options = "FA")$vol
      }, error = function(e2) {
        warning("convhulln calculation returned degenerate volume: ", e2$message)
        0
      })
    })
    return(vol)
  } else {
    warning("Package 'geometry' is recommended for exact convex hull volume in dimension >= 3. Falling back to Monte Carlo.")
    return(estimate_core_volume_emc(extreme_points = vertices, n = n))
  }
}

#' Check if a Point is a Convex Combination of Vertices
#'
#' Tests if there exists lambda >= 0 such that sum(lambda) = 1 and V^T * lambda = x.
#' Uses Linear Programming via ROI and GLPK, with fast geometric pre-filtering
#' and Frank-Wolfe projection fallback.
#'
#' @param point Imputation vector of length n.
#' @param vertices Matrix of vertices (m x n).
#' @param tol Numerical tolerance.
#' @return TRUE if point is in conv(vertices), FALSE otherwise.
#' @noRd
check_convex_combination <- function(point, vertices, tol = 1e-7) {
  m <- nrow(vertices)
  n <- ncol(vertices)

  if (length(point) != n) {
    stop(sprintf("Point dimension (%d) does not match vertices dimension (%d).",
                 length(point), n))
  }

  # 1. Fast pre-check: Efficiency constraint
  v_N <- sum(vertices[1, ])
  if (abs(sum(point) - v_N) > tol * max(1, abs(v_N))) {
    return(FALSE)
  }

  # 2. Fast pre-check: Coordinate bounding box
  col_min <- apply(vertices, 2, min)
  col_max <- apply(vertices, 2, max)
  if (any(point < col_min - tol) || any(point > col_max + tol)) {
    return(FALSE)
  }

  # 3. Fast pre-check: Point is already an exact vertex
  diff_sq <- rowSums(sweep(vertices, 2, point, "-")^2)
  if (any(diff_sq <= tol^2)) {
    return(TRUE)
  }

  # 4. Linear Programming feasibility via ROI + GLPK
  roi_res <- tryCatch({
    if (requireNamespace("ROI", quietly = TRUE)) {
      A_eq <- rbind(t(vertices), rep(1, m))
      b_eq <- c(point, 1)

      op <- ROI::OP(
        objective = ROI::L_objective(rep(0, m)),
        constraints = ROI::L_constraint(
          L = A_eq,
          dir = rep("==", length(b_eq)),
          rhs = b_eq
        ),
        bounds = ROI::V_bound(li = 1:m, lb = rep(0, m)),
        maximum = FALSE
      )

      # Solve LP
      res <- ROI::ROI_solve(op, solver = "glpk")
      if (res$status$code == 0) {
        sol <- ROI::solution(res)
        recon <- as.vector(t(vertices) %*% sol)
        if (max(abs(recon - point)) <= max(tol, 1e-5) &&
            abs(sum(sol) - 1) <= max(tol, 1e-5) &&
            all(sol >= -tol)) {
          return(TRUE)
        }
      }
      return(FALSE)
    }
    NULL
  }, error = function(e) {
    NULL
  })

  if (!is.null(roi_res)) {
    return(roi_res)
  }

  # 5. Fallback: Frank-Wolfe convex projection
  p <- colMeans(vertices)
  max_iter <- 300
  for (iter in seq_len(max_iter)) {
    diff_vec <- p - point
    if (sum(diff_vec^2) <= tol^2) return(TRUE)

    dots <- vertices %*% diff_vec
    s <- vertices[which.min(dots), ]

    d_vec <- s - p
    d_norm_sq <- sum(d_vec^2)
    if (d_norm_sq <= 1e-15) break

    gamma <- -sum(diff_vec * d_vec) / d_norm_sq
    gamma <- max(0, min(1, gamma))
    p <- p + gamma * d_vec

    gap <- sum(diff_vec * (p - s))
    if (gap <= tol^2) break
  }

  return(sum((p - point)^2) <= tol^2)
}


# =====================================================================
# Adaptive Ellipsoid Monte Carlo (EMC) Estimator
# =====================================================================

#' Estimate Core Volume (EMC)
#'
#' Estimates the projected volume of a core using Adaptive Ellipsoid Monte Carlo
#' with a 2-stage cascade filter.
#'
#' @param extreme_points Matrix of extreme points / vertices.
#' @param v_func The game value function or vector. If NULL, membership in
#' \code{conv(extreme_points)} is used.
#' @param n Total number of players (dimensions).
#' @param min_hits Target number of hits for statistical stability.
#' @param max_samples Safety timeout limit.
#' @param batch_size Number of darts to process simultaneously in memory.
#' @return The estimated projected volume of the core.
#' @export
estimate_core_volume_emc <- function(extreme_points, v_func = NULL,
                                     n = ncol(extreme_points),
                                     min_hits = 2500, max_samples = 10000000,
                                     batch_size = 20000) {

  extreme_points <- as.matrix(extreme_points)
  d <- n - 1

  # 1. Project to N-1 dimensions (Drop the dependent n-th coordinate)
  P <- extreme_points[, -n, drop = FALSE]

  # 2. Build Ellipsoid Geometry
  mu <- colMeans(P)
  cov_mat <- cov(P) + diag(1e-8, d)
  inv_cov <- solve(cov_mat)

  # 3. Find Radius needed to enclose ALL extreme points
  diff_mat <- sweep(P, 2, mu, "-")
  dists <- rowSums((diff_mat %*% inv_cov) * diff_mat)
  R <- sqrt(max(dists))

  # 4. Analytical Volume of the Ellipsoid
  unit_ball_vol <- (pi ^ (d / 2)) / gamma(d / 2 + 1)
  L <- t(chol(cov_mat))
  ellipsoid_vol <- unit_ball_vol * det(L) * (R ^ d)

  # If game function or vector is available, precompute constraints
  has_constraints <- !is.null(v_func)

  if (has_constraints) {
    if (is.function(v_func)) {
      v_N <- v_func(1:n)
      num_subsets <- (2^n) - 2
      S_mat <- matrix(0, nrow = num_subsets, ncol = n)
      v_vec <- numeric(num_subsets)

      for (i in seq_len(num_subsets)) {
        subset <- which(intToBits(i) == as.raw(1))
        S_mat[i, subset] <- 1
        v_vec[i] <- v_func(subset)
      }
    } else if (is.numeric(v_func)) {
      # v_func is a vector of coalition values
      v_N <- v_func[length(v_func)]
      num_subsets <- (2^n) - 2
      S_mat <- matrix(0, nrow = num_subsets, ncol = n)
      v_vec <- numeric(num_subsets)

      for (i in seq_len(num_subsets)) {
        subset <- which(intToBits(i) == as.raw(1))
        S_mat[i, subset] <- 1
        v_vec[i] <- v_func[i]
      }
    } else {
      has_constraints <- FALSE
    }
  }

  if (has_constraints) {
    # CASCADE SETUP: Fast Filter & Heavy Matrix
    split_idx <- min(50, nrow(S_mat))
    S_mat_stage1 <- S_mat[1:split_idx, , drop = FALSE]
    v_vec_stage1 <- v_vec[1:split_idx] - 1e-9

    S_mat_stage2 <- S_mat[(split_idx + 1):nrow(S_mat), , drop = FALSE]
    v_vec_stage2 <- v_vec[(split_idx + 1):nrow(S_mat)] - 1e-9
  } else {
    v_N <- sum(extreme_points[1, ])
  }

  hits <- 0
  samples <- 0

  # Adaptive Loop
  while (hits < min_hits && samples < max_samples) {
    u <- matrix(rnorm(batch_size * d), nrow = batch_size, ncol = d)
    u <- u / sqrt(rowSums(u^2))

    r_vals <- runif(batch_size) ^ (1 / d)
    ball_pts <- u * r_vals

    ellipsoid_pts <- (ball_pts %*% t(L)) * R
    ellipsoid_pts <- sweep(ellipsoid_pts, 2, mu, "+")

    x_n <- v_N - rowSums(ellipsoid_pts)
    x_full <- cbind(ellipsoid_pts, x_n)

    if (has_constraints) {
      # Stage 1: Fast Filter
      eval_stage1 <- x_full %*% t(S_mat_stage1)
      passed_stage1 <- sweep(eval_stage1, 2, v_vec_stage1, ">=")
      pass_stage1 <- rowSums(passed_stage1) == split_idx

      survivors <- x_full[pass_stage1, , drop = FALSE]

      # Stage 2: Heavy Verification
      if (nrow(survivors) > 0) {
        if (nrow(S_mat_stage2) > 0) {
          eval_stage2 <- survivors %*% t(S_mat_stage2)
          passed_stage2 <- sweep(eval_stage2, 2, v_vec_stage2, ">=")
          pass_stage2 <- rowSums(passed_stage2) == nrow(S_mat_stage2)
          hits <- hits + sum(pass_stage2)
        } else {
          hits <- hits + nrow(survivors)
        }
      }
    } else {
      # Check membership via convex combination
      in_core_darts <- is_in_core(x_full, vertices = extreme_points)
      hits <- hits + sum(in_core_darts)
    }

    samples <- samples + batch_size
  }

  ratio <- hits / samples
  core_vol <- ellipsoid_vol * ratio

  return(core_vol)
}