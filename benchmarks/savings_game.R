###############################################################################
# Benchmark of exact vs approximate allocation rules in TUvalues on the
# savings (sequencing) game, Model 1 in Camacho, Gonçalves-Dosantos &
# Sánchez-Soriano (2025), arXiv:2510.01766.
#
# Run from the repository root with:
#   Rscript benchmarks/savings_game.R          rules and core, each in a new R process
#   Rscript benchmarks/savings_game.R rules    only the allocation rules
#   Rscript benchmarks/savings_game.R core     only the core
# Results are written to results/savings_game/; file names start with
# "savings_game_" ("savings_game_core_" for the core).
###############################################################################

source("benchmarks/benchmark_functions.R")


## ---- Game --------------------------------------------------------------------

# Savings game with initial order (1, 2, ..., n). For a
# coalition T connected with respect to that order,
#   v(T) = sum_{i, j in T} max(alpha_j * p_i - alpha_i * p_j, 0),
# and if T is disconnected, v(T) is the sum over its connected components.
savings_game <- function(p, alpha) {
  n <- length(p)
  # gain[i, j] = max(alpha_j * p_i - alpha_i * p_j, 0)
  gain <- pmax(outer(p, alpha) - outer(alpha, p), 0)

  function(coalition) {
    if (length(coalition) < 2) {
      return(0)
    }
    coalition <- sort(coalition)
    # Consecutive players belong to the same connected component
    component <- cumsum(c(1, diff(coalition) != 1))
    gain_coalition <- gain[coalition, coalition, drop = FALSE]
    return(sum(gain_coalition[outer(component, component, "==")]))
  }
}


## ---- Instances ---------------------------------------------------------------

seed <- 2026
n_players <- c(8, 12, 16, 20, 30)

# Parameters p and alpha: the first 8 players are those of Camacho et al.
# (2025), players 9 to 12 those of their code, and the rest are drawn at random
# from the same ranges. The game with n players uses the first n.
set.seed(seed)
p     <- c(3, 4, 6, 1, 3, 4, 5, 4, 2, 1, 3, 2)
alpha <- c(1, 2, 4, 2, 5, 2, 1, 4, 3, 1, 2, 3)
p     <- c(p,     sample(1:6, max(n_players) - 12, replace = TRUE))
alpha <- c(alpha, sample(1:5, max(n_players) - 12, replace = TRUE))

# A priori unions: one with half of the players, another with a quarter and
# the rest alone (see half_quarter_unions)
unions <- half_quarter_unions

# Communication graph: random graph with expected average degree 3, drawn for
# each n. Graphs containing the whole path 1-2-...-n are discarded, since then
# the Myerson value coincides with the Shapley value in this game.
graphs <- lapply(n_players, function(n) {
  random_graph(n, seed = seed + n, degree = 3,
               reject = function(edges) sum(edges[, 2] - edges[, 1] == 1) == n - 1)
})
names(graphs) <- n_players
graph <- function(n) graphs[[as.character(n)]]


## ---- Benchmark ---------------------------------------------------------------

parts     <- benchmark_parts("benchmarks/savings_game.R")
game_name <- "savings_game"
out_dir   <- file.path("results", game_name)

game <- function(n) savings_game(p[1:n], alpha[1:n])

# Allocation rules
if ("rules" %in% parts) {
  results <- run_benchmark(
    game             = game,
    n_players        = n_players,
    n_rep_grid       = c(100, 500, 1000, 5000),
    n_runs           = 50,
    rules            = allocation_rules(),
    unions           = unions,
    graph            = graph,
    exact_time_limit = 600  # seconds
  )
  save_benchmark(results, out_dir, prefix = paste0(game_name, "_"))
  save_structures(n_players, unions, graph, out_dir,
                  prefix = paste0(game_name, "_"))
}

# Core: it needs the 2^n - 1 coalitional constraints, so fewer players
if ("core" %in% parts) {
  core_results <- run_core_benchmark(
    game             = game,
    n_players        = c(6, 8, 10, 12),
    n_rep_grid       = c(100, 250, 500),
    n_runs           = 50,
    exact_time_limit = 600  # seconds
  )
  save_benchmark(core_results, out_dir, prefix = paste0(game_name, "_core_"))
}

write.csv2(data.frame(player = seq_along(p), p = p, alpha = alpha),
           file.path(out_dir, paste0(game_name, "_parameters.csv")),
           row.names = FALSE)
