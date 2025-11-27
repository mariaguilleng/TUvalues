#' @title Approximated core of the game
#'
#' @description Calculate the vertices of the core of the game following
#' Camacho et al. (2025)
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param n_rep The number of iterations to perform in the algorithm.
#'
#' @return The vertices of the estimated core
#'
#' @importFrom highs highs_model highs_solver
#' @importFrom utils combn
#' @importFrom stats rnorm
#'
#' @references Camacho, J., Gonçalves-Dosantos, J. C., & Sánchez-Soriano, J. (2025).
#' A Linear Programming Approach to Estimate the Core in Cooperative Games.
#' arXiv preprint arXiv:2510.01766.
#'
#' @export

core_appro <- function(characteristic_func, n_players = 0, n_rep = 1000){

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

  # get coalition matrix
  coa_matrix <- as.matrix(coalitions(n_players)$Binary)

  solutions <- list()
  for (rep in seq(n_rep)) {

    # Get a random vector uniformly distributed on the unit sphere
    direction <- random_vector_sphere(n_players)

    # Create the model
    n_restrictions <- length(characteristic_func)
    rhs_vector <- rep(Inf, n_restrictions)
    rhs_vector[n_restrictions] <- characteristic_func[n_restrictions]
    model <- highs_model(
      L = direction,
      A = coa_matrix[-1,],
      lhs = characteristic_func,
      rhs = rhs_vector,
      lower  = rep(0, n_players)
    )

    # Solve the model
    solver <- highs_solver(model)
    solver$solve()

    # Save the solution if it exits
    if (solver$solution()$value_valid) {
      solutions[[length(solutions) + 1]] <- solver$solution()$col_value
    }

  }

  if (length(solutions) == 0) {
    warning("No valid solutions found. The core may be empty.")
    return(NULL)
  }

  # Format of the output
  solutions <- unique(solutions)
  solutions_matrix <- do.call(rbind, solutions)
  colnames(solutions_matrix) <- seq(n_players)
  return(solutions_matrix)

}

# Generate Random vecrtor on the unit sphere
random_vector_sphere <- function(n_players) {

  raw_vector <- rnorm(n_players)               # draw n standard normals
  euclidean_norm <- sqrt(sum(raw_vector^2))    # compute norm
  unit_vector <- abs(raw_vector) / euclidean_norm  # normalize
  return(unit_vector)

}

