#' @title Approximated core of the game
#' @noRd
#'
#' @description Calculate vertices of the core of the game following Camacho et
#' al. (2025): in each of the \code{n_rep} iterations, a linear program over
#' the core constraints is solved with a random objective direction (ROI and
#' GLPK), and the distinct optimal vertices are kept. The value of the empty
#' coalition is taken as 0.
#' Internal function called by \code{coreVertex} and \code{centroid}, which
#' check the arguments.
#'
#' @param characteristic_func The characteristic function of the game, already
#' checked: a numeric vector of length \eqn{2^n - 1} or \eqn{2^n} in the order
#' given by \code{coalitions}, or a function that receives a coalition.
#' @param n_players The number of players, already checked. Only used if
#' \code{characteristic_func} is a function.
#' @param n_rep The number of iterations of the sampling, already checked.
#' @param echo Show progress of the calculation.
#'
#' @return A matrix with one vertex found per row and one column per player, or
#' \code{NULL} (with a warning) if no vertex is found.
#'
#' @importFrom ROI OP L_objective L_constraint ROI_solve solution
#' @import ROI.plugin.glpk
#' @importFrom utils combn txtProgressBar setTxtProgressBar
#' @importFrom stats rnorm
#'
#' @references Camacho, J., Gonçalves-Dosantos, J. C., & Sánchez-Soriano, J. (2025).
#' A Linear Programming Approach to Estimate the Core in Cooperative Games.
#' arXiv preprint arXiv:2510.01766.

core_appro <- function(characteristic_func, n_players = 0, n_rep = 1000, echo = TRUE){

  # Tolerance used to remove repeated vertices
  tol <- 1e-10

  if (is.function(characteristic_func)) {
    n <- as.integer(n_players)
    # get characteristic function values for every coalition
    values <- unlist(lapply(seq_len(n), function(size) {
      combn(seq_len(n), size, FUN = characteristic_func)
    }), use.names = FALSE)
    if (!is.numeric(values) || is.complex(values) || length(values) != 2^n - 1 ||
        any(!is.finite(values))) {
      stop("characteristic_func must return one finite real numeric value for each coalition.")
    }
  } else {
    # The empty coalition is assumed to have value 0
    values <- game_values(characteristic_func)[-1L]
    n <- as.integer(round(log2(length(values) + 1)))
  }

  # Create the ROI model once; only the objective changes between replications.
  # Core constraints: Ax >= v and the efficiency constraint: Ax_N = v(N)
  A <- as.matrix(coalitions(n)$Binary)[-1L, , drop = FALSE]
  directions <- c(rep(">=", length(values) - 1L), "==")
  op <- ROI::OP(
    objective = ROI::L_objective(numeric(n)),
    constraints = ROI::L_constraint(
      L = A,
      dir = directions,
      rhs = values
    ),
    bounds = ROI::V_bound(li = seq_len(n), lb = rep(0, n)),
    maximum = FALSE # Minimizing to find a boundary point (vertex)
  )

  solutions <- vector("list", n_rep)
  count <- 0L
  if (echo) {
    pb <- txtProgressBar(min = 0, max = n_rep, style = 3)
    on.exit(close(pb), add = TRUE)
    progress_step <- max(1, ceiling(n_rep / 100))
    next_progress <- progress_step
  }

  for (rep in seq_len(n_rep)) {
    # Get a random vector uniformly distributed on the unit sphere
    direction <- random_vector_sphere(n)
    ROI::objective(op) <- ROI::L_objective(direction)
    res <- ROI::ROI_solve(op, solver = "glpk")
    # Save the solution if it is optimal
    if (res$status$code == 0) {
      count <- count + 1L
      solutions[[count]] <- ROI::solution(res)
    }
    if (echo && (rep >= next_progress || rep == n_rep)) {
      setTxtProgressBar(pb, rep)
      next_progress <- next_progress + progress_step
    }
  }

  if (count == 0L) {
    warning("No valid solutions found. The core may be empty.")
    return(NULL)
  }

  # Format of the output, removing repeated vertices up to tol
  result <- do.call(rbind, solutions[seq_len(count)])
  keep <- !duplicated(as.data.frame(round(result / tol)))
  result <- result[keep, , drop = FALSE]
  colnames(result) <- seq_len(n)
  return(result)
}

# Generate Random vector on the unit sphere
random_vector_sphere <- function(n_players) {
  raw_vector <- rnorm(n_players)               # draw n standard normals
  euclidean_norm <- sqrt(sum(raw_vector^2))    # compute norm
  unit_vector <- abs(raw_vector) / euclidean_norm  # normalize
  return(unit_vector)
}
