###############################################################################
# Benchmark of exact vs approximate allocation rules in TUvalues on the
# non-convex game, Model 2 in Camacho, Gonçalves-Dosantos & Sánchez-Soriano
# (2025), arXiv:2510.01766.
#
# Run from the repository root with:
#   Rscript benchmarks/non_convex_game.R          rules and core, each in a new R process
#   Rscript benchmarks/non_convex_game.R rules    only the allocation rules
#   Rscript benchmarks/non_convex_game.R core     only the core
# Results are written to results/non_convex_game/; file names start with
# "non_convex_game_" ("non_convex_game_core_" for the core).
###############################################################################

source("benchmarks/benchmark_functions.R")


## ---- Game --------------------------------------------------------------------

# Non-convex game with n players, as in Camacho et al. (2025):
#   v(T) = 0                 if |T| = 1,
#   v(T) = (3/4) * |T| / n   if player n belongs to T and T is not N,
#   v(T) = |T| / n           otherwise (so v(N) = 1).
non_convex_game <- function(n) {
  function(coalition) {
    size <- length(coalition)
    if (size <= 1) {
      return(0)
    }
    if (n %in% coalition && size < n) {
      return(0.75 * size / n)
    }
    return(size / n)
  }
}


## ---- Instances ---------------------------------------------------------------

seed <- 2026
n_players <- c(8, 12, 16, 20, 30)

# A priori unions: one with half of the players, another with a quarter and
# the rest alone (see half_quarter_unions)
unions <- half_quarter_unions

# Communication graph: random graph with expected average degree 3, drawn for
# each n
graphs <- lapply(n_players, function(n) random_graph(n, seed = seed + n))
names(graphs) <- n_players
graph <- function(n) graphs[[as.character(n)]]


## ---- Benchmark ---------------------------------------------------------------

parts     <- benchmark_parts("benchmarks/non_convex_game.R")
game_name <- "non_convex_game"
out_dir   <- file.path("results", game_name)

# Allocation rules
if ("rules" %in% parts) {
  results <- run_benchmark(
    game             = non_convex_game,
    n_players        = n_players,
    n_rep_grid       = c(100, 500, 1000, 5000),
    n_runs           = 20,
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
    game             = non_convex_game,
    n_players        = c(6, 8, 10, 12),
    n_rep_grid       = c(100, 250, 500),
    n_runs           = 20,
    exact_time_limit = 600  # seconds
  )
  save_benchmark(core_results, out_dir, prefix = paste0(game_name, "_core_"))
}

