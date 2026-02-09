#' @title Exact core of the game
#'
#' @description Calculate the vertices of core of the game.
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#'
#' @return The vertices of the core
#'
#' @importFrom utils combn

core_exact <- function(characteristic_func, n_players = 0) {


  if(!is.vector(characteristic_func) && !is.function(characteristic_func)) {
    stop("Invalid characteristic_func provided.")
  }

  if (is.function(characteristic_func) && n_players < 2) {
    stop("Invalid number of players specified. n_players must be greater than 1.")
  }

  if (is.vector(characteristic_func)) {
    # get number of players
    n_players<-log(length(characteristic_func),2)
    if (n_players!=round(n_players)){
      characteristic_func <- c(0, characteristic_func)
      n_players<-log(length(characteristic_func),2)
    }
    characteristic_func <- characteristic_func[-1]
  }

  if (is.function(characteristic_func)) {
    # get characteristic function values for every coalition
    results_by_size <- lapply(X = 1:n_players, FUN = function(m) {
      return(combn(x = 1:n_players, m = m, FUN = characteristic_func))
    })
    characteristic_func <- unlist(results_by_size)
  }

  coa_matrix <- as.matrix(coalitions(n_players)$Binary)[-1, ]
  idx_N <- nrow(coa_matrix)

  # Rationality Constraints (all coalitions S != N)
  A_S <- coa_matrix[-idx_N, ]
  b_S <- characteristic_func[-idx_N]

  # Efficiency Constraint (Grand Coalition N)
  A_eff <- coa_matrix[idx_N, ]
  b_eff <- characteristic_func[idx_N]

  n_rationality_constraints <- nrow(A_S)
  solutions <- list()

  # We select n-1 constraints from the n_rationality_constraints
  combination_indices <- combn(n_rationality_constraints, n_players - 1)
  for (k in seq_len(ncol(combination_indices))) {

    rows_to_use <- combination_indices[, k]

    A_eq <- rbind(A_eff, A_S[rows_to_use, ])
    b_eq <- c(b_eff, b_S[rows_to_use])

    # Check for linear independence and solve
    if (qr(A_eq)$rank == n_players) {
      x <- solve(A_eq, b_eq)

      # Check Feasibility (Verify ALL 2^n - 2 rationality constraints)
      coalition_sums <- A_S %*% x

      # Check if all inequalities are satisfied: Sum(x_i) >= v(S)
      is_feasible <- all(coalition_sums >= b_S)

      if (is_feasible) {
        solutions[[length(solutions) + 1]] <- x
      }
    }
  }

  if (length(solutions) == 0) {
    warning("The core is empty or no vertices were found.")
    return(NULL)
  }

  # Format of the output
  solutions <- unique(solutions)
  solutions_matrix <- do.call(rbind, solutions)
  colnames(solutions_matrix) <- seq_len(n_players)

  return(solutions_matrix)
}

# characteristic_func <- c(2,3,5,5,7,8,10)
# characteristic_func <- c(5,2,4,7,15,15,15,15,15,15,20,20,20,20,35)
# characteristic_func <- c(0,0,0,0,0,5,5,5,5,5,5,5,5,5,5,15,15,15,15,15,15,15,15,15,15,30,30,30,30,30,60)
# core_exact(characteristic_func)
