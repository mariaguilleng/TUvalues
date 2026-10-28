###############################################################################
# Comparison of TUvalues with other R packages for cooperative games (TUGLab,
# CoopGame and GameTheory) on the 10-player museum pass game (Model 3 of the
# benchmarks): Shapley, Banzhaf, Owen, Myerson and core vertices.
#
# Every function is first checked against a reference solution and run once as
# warm-up; then its execution time is measured over 50 runs. References:
#   - Shapley, Banzhaf and Owen: closed formulas (the game is a sum of
#     unanimity games, one per visitor).
#   - Myerson: its definition, evaluated over all coalitions in base R.
#   - Core vertices: the vertices computed by TUvalues with rational (exact)
#     arithmetic; the other packages must return the same set.
#
# Run from the repository root with:
#   Rscript package_comparison/package_comparison.R
# Results are written to results/package_comparison/package_comparison.csv.
###############################################################################

library(TUvalues)
library(TUGLab)
library(CoopGame)
library(GameTheory)
library(microbenchmark)
source("benchmarks/benchmark_functions.R")  # half_quarter_unions(), random_graph()

n_runs <- 50
n      <- 10
seed   <- 2026

# Coalitions in the order used by TUvalues and TUGLab, and their binary codes
coalition_matrix <- as.matrix(coalitions(n)$Binary)
codes <- drop(coalition_matrix %*% 2^(0:(n - 1)))


## ---- Reference values from the definitions ------------------------------------

# Value of S \ {i} for the coalitions S containing i (v_all includes the
# empty coalition)
without_player <- function(v_all, i, rows) {
  v_by_code <- numeric(2^n)
  v_by_code[codes + 1] <- v_all
  v_by_code[codes[rows] - 2^(i - 1) + 1]
}

shapley_definition <- function(v_all) {
  size <- rowSums(coalition_matrix)
  weight <- factorial(pmax(size - 1, 0)) * factorial(n - size) / factorial(n)
  sapply(seq_len(n), function(i) {
    rows <- which(coalition_matrix[, i] == 1)
    sum(weight[rows] * (v_all[rows] - without_player(v_all, i, rows)))
  })
}

banzhaf_definition <- function(v_all) {
  sapply(seq_len(n), function(i) {
    rows <- which(coalition_matrix[, i] == 1)
    sum(v_all[rows] - without_player(v_all, i, rows)) / 2^(n - 1)
  })
}

# Communication game: v(S) is the sum of v over the connected components of S
graph_game <- function(v_all, edges) {
  apply(coalition_matrix, 1, function(row) {
    players <- which(row == 1)
    if (length(players) == 0) return(0)
    component <- seq_along(players)  # union-find over the edges inside S
    for (edge in edges) {
      if (all(edge %in% players)) {
        a <- component[match(edge[1], players)]
        b <- component[match(edge[2], players)]
        component[component == b] <- a
      }
    }
    sum(sapply(unique(component), function(k) {
      v_all[which(codes == sum(2^(players[component == k] - 1)))]
    }))
  })
}


## ---- Museum pass game (Model 3) ------------------------------------------------

# Visits of the paper (5 visitors x 11 museums); the game uses the first 10
visits <- matrix(c(1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0,
                   0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0,
                   1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0,
                   0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1,
                   1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0),
                 nrow = 5, byrow = TRUE)[, 1:n]
visits <- visits[rowSums(visits) > 0, , drop = FALSE]
visited <- apply(visits, 1, function(row) which(row == 1), simplify = FALSE)

# v(S): number of visitors who only visit museums in S
museum_v_all <- rowSums(
  (coalition_matrix %*% t(visits)) == matrix(rowSums(visits),
                                             nrow(coalition_matrix),
                                             nrow(visits), byrow = TRUE))

museum_unions <- half_quarter_unions(n)
museum_graph  <- random_graph(n, seed = seed + n)

# Closed formulas: each visitor (unanimity game on its museums N_i) gives
#   Shapley: 1 / |N_i| to each museum in N_i
#   Banzhaf: 1 / 2^(|N_i| - 1) to each museum in N_i
#   Owen:    1 / (m_i * |N_i in U|) to each museum of N_i in union U, where
#            m_i is the number of unions that contain museums of N_i
museum_shapley <- museum_banzhaf <- museum_owen <- numeric(n)
for (N_i in visited) {
  museum_shapley[N_i] <- museum_shapley[N_i] + 1 / length(N_i)
  museum_banzhaf[N_i] <- museum_banzhaf[N_i] + 1 / 2^(length(N_i) - 1)
  unions_i <- Filter(function(u) any(N_i %in% u), museum_unions)
  for (u in unions_i) {
    inside <- intersect(N_i, u)
    museum_owen[inside] <- museum_owen[inside] +
      1 / (length(unions_i) * length(inside))
  }
}
stopifnot(all.equal(museum_shapley, shapley_definition(museum_v_all)),
          all.equal(museum_banzhaf, banzhaf_definition(museum_v_all)))

museum <- list(
  v      = museum_v_all[-1],
  unions = museum_unions,
  graph  = museum_graph,
  reference = list(
    Shapley = museum_shapley,
    Banzhaf = museum_banzhaf,
    Owen    = museum_owen,
    Myerson = shapley_definition(graph_game(museum_v_all, museum_graph)),
    `Core vertices` = unname(coreVertex(museum_v_all[-1], method = "exact",
                                        arithmetic = "rational"))))


games <- list(Museum = museum)


## ---- Functions compared -------------------------------------------------------

# For a game: concept, package and function computing the solution, for the
# concepts with reference. The GameTheory game object is built outside the
# timed function.
functions_to_compare <- function(game) {
  v <- game$v
  unions <- game$unions
  graph_edges <- game$graph
  game_gt <- GameTheory::DefineGame(n, v)
  all_functions <- list(
    list("Shapley", "TUvalues",   function() shapley(v, method = "exact")),
    list("Shapley", "TUGLab",     function() TUGLab::shapleyvalue(v, binary = FALSE)),
    list("Shapley", "CoopGame",   function() CoopGame::shapleyValue(v)),
    list("Shapley", "GameTheory", function() {
      as.numeric(suppressWarnings(GameTheory::ShapleyValue(game_gt))$SV)
    }),
    list("Banzhaf", "TUvalues",   function() banzhaf(v, method = "exact")),
    list("Banzhaf", "CoopGame",   function() CoopGame::banzhafValue(v)),
    list("Owen",    "TUvalues",   function() {
      owen(v, union = unions, method = "exact")
    }),
    list("Owen",    "TUGLab",     function() {
      TUGLab::owenvalue(v, binary = FALSE, partition = unions)
    }),
    list("Myerson", "TUvalues",   function() {
      myerson(v, graph_edges = graph_edges, method = "exact")
    }),
    list("Myerson", "TUGLab",     function() {
      TUGLab::myersonvalue(v, binary = FALSE, communications = graph_edges)
    }),
    list("Core vertices", "TUvalues (double)", function() {
      coreVertex(v, method = "exact", arithmetic = "double")
    }),
    list("Core vertices", "TUvalues (rational)", function() {
      coreVertex(v, method = "exact", arithmetic = "rational")
    }),
    list("Core vertices", "TUGLab",   function() {
      TUGLab::corevertices(v, binary = FALSE)
    }),
    list("Core vertices", "CoopGame", function() CoopGame::coreVertices(v))
  )
  Filter(function(f) f[[1]] %in% names(game$reference), all_functions)
}


## ---- Correctness checks -------------------------------------------------------

tol <- 1e-8

same_vector <- function(x, expected) {
  x <- as.numeric(x)
  length(x) == length(expected) && all(is.finite(x)) &&
    max(abs(x - expected)) < tol
}

# Core vertices form a set: packages may return them in different orders
same_vertices <- function(x, expected) {
  if (is.null(x)) return(FALSE)
  x <- unname(as.matrix(x))
  if (!identical(dim(x), dim(expected)) || any(!is.finite(x))) return(FALSE)
  contains <- function(vertices, vertex) {
    any(apply(abs(sweep(vertices, 2L, vertex)), 1L, max) < tol)
  }
  all(apply(expected, 1L, contains, vertices = x)) &&
    all(apply(x, 1L, contains, vertices = expected))
}


## ---- Benchmark ----------------------------------------------------------------

# Checks the result of fun against the reference and, if it is correct,
# measures its execution time (milliseconds) over n_runs runs
benchmark_function <- function(game, concept, package, fun, expected) {
  row <- data.frame(Game = game, Concept = concept, Package = package,
                    Status = "OK", Correct = NA, Mean_ms = NA_real_,
                    SD_ms = NA_real_, Median_ms = NA_real_, Min_ms = NA_real_,
                    Max_ms = NA_real_)

  result <- tryCatch(fun(), error = function(e) e)
  if (inherits(result, "error")) {
    row$Status <- paste("ERROR:", conditionMessage(result))
    return(row)
  }
  check <- if (concept == "Core vertices") same_vertices else same_vector
  row$Correct <- isTRUE(tryCatch(check(result, expected),
                                 error = function(e) FALSE))
  if (!row$Correct) {
    row$Status <- "INVALID RESULT"
    return(row)
  }

  invisible(fun())  # warm-up
  time_ms <- microbenchmark(fun(), times = n_runs)$time / 1e6
  row[c("Mean_ms", "SD_ms", "Median_ms", "Min_ms", "Max_ms")] <-
    c(mean(time_ms), sd(time_ms), median(time_ms), min(time_ms), max(time_ms))
  return(row)
}

results <- do.call(rbind, lapply(names(games), function(game) {
  message("Game: ", game)
  do.call(rbind, lapply(functions_to_compare(games[[game]]), function(f) {
    message("  ", f[[1]], ", ", f[[2]])
    benchmark_function(game, concept = f[[1]], package = f[[2]], fun = f[[3]],
                       expected = games[[game]]$reference[[f[[1]]]])
  }))
}))


## ---- Output -------------------------------------------------------------------

results <- results[order(results$Game, results$Concept, results$Mean_ms), ]
rownames(results) <- NULL
print(results, digits = 4)
out_dir <- file.path("results", "package_comparison")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write.csv2(results, file.path(out_dir, "package_comparison.csv"), row.names = FALSE)
