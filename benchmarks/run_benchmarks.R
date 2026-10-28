###############################################################################
# Runs all the TUvalues benchmarks: the four games, one after another, and
# then the figures and tables that summarise them.
#
# Run from the repository root with:  Rscript benchmarks/run_benchmarks.R
# Each game is run in a new R process, so that the timings of one game are
# not affected by the previous ones. Results are written to results/<game>/
# and the figures and tables to results/summary/.
###############################################################################

games <- c("savings_game", "non_convex_game", "museum_game",
           "weighted_majority_game")

for (game in games) {
  message(format(Sys.time(), "%d/%m %H:%M:%S  "), "Game: ", game)
  callr::rscript(file.path("benchmarks", paste0(game, ".R")), show = TRUE)
}

callr::rscript(file.path("benchmarks", "summary_figures.R"), show = TRUE)
