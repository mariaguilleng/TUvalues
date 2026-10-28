###############################################################################
# Benchmark of exact vs approximate allocation rules in TUvalues on a
# weighted majority game (Model 4).
#
# Run from the repository root with:
#   Rscript benchmarks/weighted_majority_game.R
# Results are written to results/weighted_majority_game/; file names start
# with "weighted_majority_game_".
###############################################################################

source("benchmarks/benchmark_functions.R")


## ---- Game --------------------------------------------------------------------

# Weighted majority game: a coalition wins (v = 1) if its total weight reaches
# the quota, floor(sum of the weights / 2) + 1, and loses (v = 0) otherwise.
weighted_majority_game <- function(weights) {
  quota <- floor(sum(weights) / 2) + 1

  function(coalition) {
    return(as.numeric(sum(weights[coalition]) >= quota))
  }
}


## ---- Instances ---------------------------------------------------------------

seed <- 2026
n_players <- c(8, 12, 16, 20, 30)

# Weights: the first 10 are fixed (12, 10, ..., 1); the rest are
# drawn at random from the same range. The game with n players uses the first
# n weights (and its own quota).
set.seed(seed)
weights <- c(12, 10, 9, 8, 7, 6, 5, 4, 3, 1)
weights <- c(weights, sample(1:12, max(n_players) - 10, replace = TRUE))

# A priori unions: one with half of the players, another with a quarter and
# the rest alone (see half_quarter_unions)
unions <- half_quarter_unions

# Communication graph: random graph with expected average degree 3, drawn for
# each n
graphs <- lapply(n_players, function(n) random_graph(n, seed = seed + n))
names(graphs) <- n_players
graph <- function(n) graphs[[as.character(n)]]


## ---- Benchmark ---------------------------------------------------------------

# Allocation rules. The core is not computed: it is empty in these games,
# since no player belongs to every winning coalition (no veto players).
results <- run_benchmark(
  game             = function(n) weighted_majority_game(weights[1:n]),
  n_players        = n_players,
  n_rep_grid       = c(100, 500, 1000, 5000),
  n_runs           = 50,
  rules            = allocation_rules(),
  unions           = unions,
  graph            = graph,
  exact_time_limit = 600  # seconds
)


## ---- Output ------------------------------------------------------------------

game_name <- "weighted_majority_game"
out_dir   <- file.path("results", game_name)
save_benchmark(results, out_dir, prefix = paste0(game_name, "_"))
save_structures(n_players, unions, graph, out_dir,
                  prefix = paste0(game_name, "_"))
write.csv2(data.frame(player = seq_along(weights), weight = weights),
           file.path(out_dir, paste0(game_name, "_parameters.csv")),
           row.names = FALSE)
