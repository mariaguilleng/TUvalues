###############################################################################
# Figures and tables of the TUvalues benchmarks for the manuscript, comparing
# the four games: scalability (Figure 1), accuracy and convergence (Figure 2
# and main table), real against estimated error (Figure 3) and core
# approximation (Figure 4). Supplementary tables
# collect the rest. Built from the results folders of the games
# (results/<game>/); games without results yet are skipped.
#
# Run from the repository root with:  Rscript benchmarks/summary_figures.R
# Output is written to results/summary/.
###############################################################################

library(ggplot2)
library(readxl)
library(writexl)

out_dir <- file.path("results", "summary")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

games <- c(savings_game           = "Savings",
           non_convex_game        = "Non-convex",
           museum_game            = "Museum",
           weighted_majority_game = "Weighted majority")
rules <- c(shapley        = "Shapley",
           banzhaf        = "Banzhaf",
           owen           = "Owen",
           banzhaf_owen   = "Banzhaf-Owen",
           myerson        = "Myerson",
           myerson_unions = "Myerson with unions")
time_limit <- 600   # seconds, limit of the exact computations
k_main     <- 5000  # number of samples of the main comparisons
k_time     <- 1000  # number of samples of the approximations in Figure 1
n_main     <- 20    # number of players of the main comparisons

# Categorical colours for the games and ordinal blue ramp for n
game_colours <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100"), games)
blue_ramp    <- c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281")

theme_summary <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.background = element_rect(colour = NA))

save_plot <- function(plot, name, width, height) {
  ggsave(file.path(out_dir, paste0(name, ".pdf")), plot,
         width = width, height = height, bg = "white")
  ggsave(file.path(out_dir, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300, bg = "white")
}

percent_labels <- function(x) paste0(format(100 * x, drop0trailing = TRUE), "%")


## ---- Reading the results ------------------------------------------------------

# Sheet of a results table of every game whose file exists
read_sheet <- function(file, sheet) {
  tables <- lapply(names(games), function(game) {
    path <- file.path("results", game, paste0(game, "_", file))
    if (!file.exists(path)) return(NULL)
    data.frame(game = games[[game]], read_excel(path, sheet = sheet))
  })
  table <- do.call(rbind, tables)
  table$game <- factor(table$game, levels = games)
  return(table)
}

results     <- read_sheet("results_table.xlsx", "results")
core        <- read_sheet("core_results_table.xlsx", "core")
core_timing <- read_sheet("core_results_table.xlsx", "results")

results$rule <- factor(rules[results$rule], levels = rules)
exact  <- results[results$n_rep == "exact", ]
approx <- results[results$n_rep != "exact", ]
approx$n_rep <- as.numeric(approx$n_rep)
message("Games included: ", paste(levels(droplevels(results$game)),
                                  collapse = ", "))

# Exact computations stopped at the time limit: for each game and rule, the
# first n after the largest one with exact value (larger n are not attempted)
stopped <- do.call(rbind, lapply(
  split(results, list(results$game, results$rule), drop = TRUE),
  function(d) {
    computed <- d$n[d$n_rep == "exact"]
    later <- sort(unique(d$n[d$n > max(computed)]))
    if (length(later) == 0) return(NULL)
    data.frame(game = d$game[1], rule = d$rule[1], n = later[1])
  }))
# One marker per rule and n (the games may share it)
stopped_marks <- unique(stopped[c("rule", "n")])


## ---- Figure 1: execution time against the number of players -----------------

approx_label <- paste0("Approximate (K = ", k_time, ")")
time_data <- rbind(
  data.frame(exact[c("game", "rule", "n", "wall_time_mean")],
             method = "Exact"),
  data.frame(approx[approx$n_rep == k_time,
                    c("game", "rule", "n", "wall_time_mean")],
             method = approx_label))
time_data$method <- factor(time_data$method, levels = c("Exact", approx_label))
time_data$wall_time_mean <- pmax(time_data$wall_time_mean, 0.01)  # log scale

figure_1 <- ggplot(time_data, aes(n, wall_time_mean, colour = game)) +
  geom_hline(yintercept = time_limit, colour = "grey55", linewidth = 0.4,
             linetype = "dotted") +
  annotate("text", x = 8, y = time_limit / 2.2, label = "600-s limit",
           hjust = 0, size = 2.6, colour = "grey35") +
  geom_line(aes(linetype = method), linewidth = 0.6) +
  geom_point(aes(shape = method), size = 1.6) +
  # Exact computations stopped at the limit, drawn above it
  geom_point(data = stopped_marks, aes(n, time_limit * 2.5),
             colour = "grey30", shape = 4, size = 2.4, stroke = 1,
             inherit.aes = FALSE) +
  facet_wrap(~ rule, ncol = 3) +
  scale_x_continuous(breaks = sort(unique(time_data$n))) +
  scale_y_log10(breaks = 10^(-2:3),
                labels = c("0.01", "0.1", "1", "10", "100", "1000")) +
  scale_colour_manual(values = game_colours) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_shape_manual(values = c(16, 17)) +
  labs(x = "Number of players (n)", y = "Execution time (s, log scale)",
       colour = "Game", linetype = "Method", shape = "Method") +
  theme_summary +
  guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))
save_plot(figure_1, "figure_1_scalability", width = 8, height = 6.2)


## ---- Figure 2: accuracy and convergence -------------------------------------

# Relative RMSE with respect to the exact value against K, with n = n_main
# (mean of the 50 runs)
convergence <- approx[approx$n == n_main & !is.na(approx$rel_rmse_exact_mean), ]
figure_2 <- ggplot(convergence,
                   aes(n_rep, rel_rmse_exact_mean, colour = game)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  facet_wrap(~ rule, ncol = 3) +
  scale_x_continuous(breaks = c(100, 1000, 2000, 3000, 4000, 5000)) +
  scale_y_continuous(labels = percent_labels, limits = c(0, NA)) +
  scale_colour_manual(values = game_colours) +
  labs(x = "Number of samples",
       y = "Relative RMSE vs exact value", colour = "Game") +
  theme_summary +
  theme(panel.spacing.x = unit(1.5, "lines"))
save_plot(figure_2, "figure_2_accuracy", width = 8, height = 5)


## ---- Figure 3: real against estimated error ----------------------------------

# For every game, rule, n and K with exact allocation phi, from the R runs:
#   real error:      sqrt(mean_r mean_i (phi_hat_i^(r) - phi_i)^2) / mean_i |phi_i|
#   estimated error: sqrt(mean_i Var_r(phi_hat_i^(r))) / mean_i |phi_i|
# The estimated error does not need phi (only the scale).
error_validation <- do.call(rbind, lapply(names(games), function(game) {
  path <- file.path("results", game, paste0(game, "_allocations.csv"))
  if (!file.exists(path)) return(NULL)
  allocations <- read.csv2(path)
  cases <- split(allocations, list(allocations$rule, allocations$n),
                 drop = TRUE)
  do.call(rbind, lapply(cases, function(d) {
    exact_value <- d$value[d$method == "exact"][order(d$player[
      d$method == "exact"])]
    if (length(exact_value) == 0) return(NULL)
    scale <- mean(abs(exact_value))
    do.call(rbind, lapply(split(d[d$method == "approx", ],
                                d$n_rep[d$method == "approx"]), function(a) {
      x <- matrix(a$value[order(a$run, a$player)], ncol = d$n[1],
                  byrow = TRUE)
      data.frame(game = games[[game]], rule = rules[[d$rule[1]]], n = d$n[1],
                 n_rep = a$n_rep[1], runs = nrow(x),
                 real = sqrt(mean(sweep(x, 2, exact_value)^2)) / scale,
                 estimated = sqrt(mean(apply(x, 2, var))) / scale)
    }))
  }))
}))
error_validation$game <- factor(error_validation$game, levels = games)
error_validation$rule <- factor(error_validation$rule, levels = rules)
plotted <- error_validation[error_validation$real > 0 &
                              error_validation$estimated > 0, ]

figure_3 <- ggplot(plotted, aes(real, estimated, colour = game)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey55", linewidth = 0.4) +
  geom_point(size = 1.4, alpha = 0.7) +
  scale_x_log10(labels = percent_labels) +
  scale_y_log10(labels = percent_labels) +
  scale_colour_manual(values = game_colours) +
  labs(x = "Real relative error (log scale)",
       y = "Estimated relative error (log scale)", colour = "Game") +
  theme_summary +
  theme(aspect.ratio = 1)
save_plot(figure_3, "figure_3_error_validation", width = 6, height = 6.4)

# Agreement between both errors, in percentage points: overall and by game
agreement <- function(d, group) {
  data.frame(group = group, points = nrow(d),
             mean_abs_difference_pp = 100 * mean(abs(d$estimated - d$real)),
             max_abs_difference_pp  = 100 * max(abs(d$estimated - d$real)),
             share_underestimated   = mean(d$estimated < d$real))
}
error_agreement <- rbind(
  agreement(error_validation, "All games"),
  do.call(rbind, lapply(split(error_validation, error_validation$game,
                              drop = TRUE), function(d) {
    agreement(d, as.character(d$game[1]))
  })))
print(error_agreement, digits = 3, row.names = FALSE)


## ---- Figure 4: core approximation ---------------------------------------------

# Extreme-point ratio (EPR) and volume ratio (VR) against K: one row per n,
# one column per measure and one line per game (mean of the 50 runs). A line
# is only drawn when the exact core (or its volume) is available.
measure_names <- c("A. Extreme-point ratio (EPR)", "B. Volume ratio (VR)")
core_approx <- core[core$n_rep != "exact", ]
core_data <- rbind(
  data.frame(core_approx[c("game", "n", "n_rep")],
             value = core_approx$vertex_ratio_mean,
             measure = measure_names[1]),
  data.frame(core_approx[c("game", "n", "n_rep")],
             value = core_approx$volume_ratio_mean,
             measure = measure_names[2]))
# n = 12 is left out: only the museum game has its exact core (and not its
# volume) for that size
core_data <- core_data[!is.na(core_data$value) & core_data$n != 12, ]
core_data$n_rep <- as.numeric(core_data$n_rep)
core_data$n <- factor(paste("n =", core_data$n),
                      levels = paste("n =", sort(unique(core_data$n))))
core_data$measure <- factor(core_data$measure, levels = measure_names)

figure_4 <- ggplot(core_data, aes(n_rep, value, colour = game)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  facet_grid(n ~ measure) +
  scale_x_continuous(breaks = sort(unique(core_data$n_rep))) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = percent_labels) +
  coord_cartesian(ylim = c(0, 1.02)) +
  scale_colour_manual(values = game_colours) +
  labs(x = "Number of samples (K)", y = NULL, colour = "Game") +
  theme_summary
save_plot(figure_4, "figure_4_core", width = 7, height = 6.2)


## ---- Main table: performance and accuracy for n = n_main, K = k_main -------

main <- unique(results[results$n == n_main, c("game", "rule")])
main <- main[order(main$game, main$rule), ]
# Column of data in the rows of main
pick <- function(data, column) {
  data[match(paste(main$game, main$rule), paste(data$game, data$rule)),
       column]
}
main_approx <- approx[approx$n == n_main & approx$n_rep == k_main, ]
main_exact  <- exact[exact$n == n_main, ]
main_table <- data.frame(
  Game                     = main$game,
  Rule                     = main$rule,
  `Exact time (s)`         = pick(main_exact, "wall_time_mean"),
  `Approx. time (s)`       = pick(main_approx, "wall_time_mean"),
  `Relative RMSE (%)`      = 100 * pick(main_approx, "rel_rmse_exact_mean"),
  `Estimated MC error (%)` = 100 * pick(main_approx, "rel_rmse_mc"),
  check.names = FALSE)
main_table[-(1:2)] <- lapply(main_table[-(1:2)], round, 2)

# LaTeX version, with one block of rows per game
latex_row <- function(row) {
  values <- ifelse(is.na(row[-(1:2)]), "--",
                   formatC(as.numeric(row[-(1:2)]), format = "f", digits = 2))
  paste(c(as.character(row[[2]]), values), collapse = " & ")
}
latex <- c("\\begin{tabular}{lrrrr}", "\\hline",
           paste("Rule & Exact time (s) & Approx. time (s) &",
                 "Relative RMSE (\\%) & Estimated MC error (\\%) \\\\"),
           "\\hline")
for (game in levels(droplevels(main_table$Game))) {
  block <- main_table[main_table$Game == game, ]
  latex <- c(latex,
             paste0("\\multicolumn{5}{l}{\\textit{", game, "}} \\\\"),
             paste0(vapply(seq_len(nrow(block)), function(i) {
               latex_row(block[i, ])
             }, character(1)), " \\\\"))
}
latex <- c(latex, "\\hline", "\\end{tabular}")
writeLines(latex, file.path(out_dir, "table_main.tex"))
write_xlsx(main_table, file.path(out_dir, "table_main.xlsx"))


## ---- Supplementary tables -------------------------------------------------------

# S1: relative RMSE (%) with K = k_main for every n; values marked with * are
# estimated from the runs because the exact value is not available
k_rows <- approx[approx$n_rep == k_main, ]
k_rows$error <- ifelse(
  is.na(k_rows$rel_rmse_exact_mean),
  paste0(formatC(100 * k_rows$rel_rmse_mc, format = "f", digits = 2), "*"),
  formatC(100 * k_rows$rel_rmse_exact_mean, format = "f", digits = 2))
errors_by_n <- reshape(k_rows[c("game", "rule", "n", "error")],
                       idvar = c("game", "rule"), timevar = "n",
                       direction = "wide")
names(errors_by_n) <- sub("^error\\.", "n = ", names(errors_by_n))
errors_by_n <- errors_by_n[order(errors_by_n$game, errors_by_n$rule), ]

# S2: execution time and evaluations of v of the exact and approximate methods
evaluations <- merge(
  exact[c("game", "rule", "n", "wall_time_mean", "v_evals_mean")],
  approx[approx$n_rep == k_main,
         c("game", "rule", "n", "wall_time_mean", "v_evals_mean")],
  by = c("game", "rule", "n"), all = TRUE, suffixes = c("_exact", "_approx"))
names(evaluations)[-(1:3)] <- c("exact_time_s", "exact_v_evaluations",
                                "approx_time_s", "approx_v_evaluations")
evaluations <- evaluations[order(evaluations$game, evaluations$rule,
                                 evaluations$n), ]

# S3: core, exact and approximate
core_exact <- merge(core[core$n_rep == "exact", c("game", "n",
                                                  "n_vertices_mean")],
                    core_timing[core_timing$n_rep == "exact",
                                c("game", "n", "wall_time_mean")],
                    by = c("game", "n"))
names(core_exact)[3:4] <- c("exact_vertices", "exact_time_s")
core_summary <- merge(
  core_approx[c("game", "n", "n_rep", "n_vertices_mean", "vertex_ratio_mean",
                "volume_ratio_mean")],
  core_timing[core_timing$n_rep != "exact",
              c("game", "n", "n_rep", "wall_time_mean")],
  by = c("game", "n", "n_rep"))
names(core_summary)[4:7] <- c("approx_vertices", "vertex_ratio",
                              "volume_ratio", "approx_time_s")
core_summary <- merge(core_exact, core_summary, by = c("game", "n"),
                      all.y = TRUE)
core_summary$n_rep <- as.numeric(core_summary$n_rep)
core_summary <- core_summary[order(core_summary$game, core_summary$n,
                                   core_summary$n_rep), ]

error_validation[c("real", "estimated")] <-
  100 * error_validation[c("real", "estimated")]
names(error_validation)[6:7] <- c("real_error_pct", "estimated_error_pct")
write_xlsx(list(S1_errors_by_n      = errors_by_n,
                S2_time_and_v       = evaluations,
                S3_core             = core_summary,
                S4_error_validation = error_validation,
                S5_error_agreement  = error_agreement),
           file.path(out_dir, "supplementary_tables.xlsx"))
