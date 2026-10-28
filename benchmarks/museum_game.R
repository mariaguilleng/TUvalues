###############################################################################
# Benchmark of exact vs approximate allocation rules in TUvalues on the
# museum pass game, Model 3 in Camacho, Gonçalves-Dosantos & Sánchez-Soriano
# (2025), arXiv:2510.01766.
#
# Run from the repository root with:
#   Rscript benchmarks/museum_game.R          rules and core, each in a new R process
#   Rscript benchmarks/museum_game.R rules    only the allocation rules
#   Rscript benchmarks/museum_game.R core     only the core
# Results are written to results/museum_game/; file names start with
# "museum_game_" ("museum_game_core_" for the core).
###############################################################################

source("benchmarks/benchmark_functions.R")


## ---- Game --------------------------------------------------------------------

# Museum pass game. visits is a binary matrix (visitors x museums) where
# visits[i, j] = 1 if visitor i visits museum j. The worth of a coalition T of
# museums is the number of visitors who only visit museums in T:
#   v(T) = |{i : N_i is non-empty and N_i is a subset of T}|,
# where N_i is the set of museums visited by visitor i.
museum_game <- function(visits) {
  visits <- visits[rowSums(visits) > 0, , drop = FALSE]
  n <- ncol(visits)

  function(coalition) {
    outside <- setdiff(1:n, coalition)
    return(sum(rowSums(visits[, outside, drop = FALSE]) == 0))
  }
}


## ---- Instances ---------------------------------------------------------------

seed <- 2026
n_players <- c(8, 12, 16, 20, 30)

# Visits: the first 11 museums are those of the paper; the rest are drawn at
# random with the same proportion of visits. The game with n museums uses the
# first n columns.
visits <- matrix(c(1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0,
                   0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0,
                   1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0,
                   0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1,
                   1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0),
                 nrow = 5, byrow = TRUE)
set.seed(seed)
extra <- max(n_players) - ncol(visits)
visits <- cbind(visits, matrix(rbinom(nrow(visits) * extra, 1, mean(visits)),
                               nrow = nrow(visits)))
colnames(visits) <- paste0("museum_", seq_len(ncol(visits)))

# A priori unions: one with half of the players, another with a quarter and
# the rest alone (see half_quarter_unions)
unions <- half_quarter_unions

# Communication graph: random graph with expected average degree 3, drawn for
# each n
graphs <- lapply(n_players, function(n) random_graph(n, seed = seed + n))
names(graphs) <- n_players
graph <- function(n) graphs[[as.character(n)]]


## ---- Benchmark ---------------------------------------------------------------

parts     <- benchmark_parts("benchmarks/museum_game.R")
game_name <- "museum_game"
out_dir   <- file.path("results", game_name)

game <- function(n) museum_game(visits[, 1:n, drop = FALSE])

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

write.csv2(data.frame(visitor = seq_len(nrow(visits)), visits),
           file.path(out_dir, paste0(game_name, "_parameters.csv")),
           row.names = FALSE)
