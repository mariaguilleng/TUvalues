###############################################################################
# Functions shared by the TUvalues benchmarks: exact vs approximate
# allocation rules and core. Experiments run with TUvalues 1.2.
###############################################################################

library(TUvalues)
library(ggplot2)
library(writexl)

# core_volume() computes the volume of the core from its vertices
volume_file <- normalizePath("benchmarks/volume.R")


## ---- Allocation rules --------------------------------------------------------

# Exact and approximate version of each rule. They receive the characteristic
# function v (an R function), the number of players n and the structures of
# the game: s$unions (a priori unions) and s$graph (edges of the
# communication graph).
allocation_rules <- function() {
  list(
    shapley = list(
      exact = function(v, n, s) shapley(v, n_players = n, method = "exact"),
      approx = function(v, n, n_rep, s) {
        shapley(v, n_players = n, method = "appro", n_rep = n_rep,
                echo = FALSE)
      }),
    banzhaf = list(
      exact = function(v, n, s) banzhaf(v, n_players = n, method = "exact"),
      approx = function(v, n, n_rep, s) {
        banzhaf(v, n_players = n, method = "appro", n_rep = n_rep,
                replace = TRUE, echo = FALSE)
      }),
    owen = list(
      exact = function(v, n, s) {
        owen(v, union = s$unions, n_players = n, method = "exact")
      },
      approx = function(v, n, n_rep, s) {
        owen(v, union = s$unions, n_players = n, method = "appro",
             n_rep = n_rep, echo = FALSE)
      }),
    banzhaf_owen = list(
      exact = function(v, n, s) {
        banzhaf_owen(v, union = s$unions, n_players = n, method = "exact")
      },
      approx = function(v, n, n_rep, s) {
        banzhaf_owen(v, union = s$unions, n_players = n, method = "appro",
                     n_rep = n_rep, replace = TRUE, echo = FALSE)
      }),
    myerson = list(
      exact = function(v, n, s) {
        myerson(v, graph_edges = s$graph, n_players = n, method = "exact")
      },
      approx = function(v, n, n_rep, s) {
        myerson(v, graph_edges = s$graph, n_players = n, method = "appro",
                n_rep = n_rep, echo = FALSE)
      }),
    myerson_unions = list(
      exact = function(v, n, s) {
        myerson_unions(v, n_players = n, unions = s$unions,
                       graph_edges = s$graph, method = "exact")
      },
      approx = function(v, n, n_rep, s) {
        myerson_unions(v, n_players = n, unions = s$unions,
                       graph_edges = s$graph, method = "appro",
                       n_rep = n_rep, echo = FALSE)
      })
  )
}

# Exact core vertices
exact_core <- function(v, n) coreVertex(v, n_players = n, method = "exact")


## ---- Structures --------------------------------------------------------------

# A priori unions: one union with the first ceiling(n / 2) players, another
# with the next ceiling(n / 4) players and the remaining players alone. The
# exact Owen and Banzhaf-Owen values evaluate v in
#   2^m + 2^(m - 1) * sum_k (2^|U_k| - 2)
# coalitions (m unions), so their cost grows with n as with the other rules.
half_quarter_unions <- function(n) {
  half    <- ceiling(n / 2)
  quarter <- ceiling(n / 4)
  c(list(1:half, (half + 1):(half + quarter)),
    as.list(seq_len(n)[-(1:(half + quarter))]))
}

# Random graph G(n, degree / (n - 1)), with expected average degree `degree`,
# as a list of edges c(i, j). Graphs for which reject(edges) is TRUE are
# discarded and drawn again.
random_graph <- function(n, seed, degree = 3, reject = function(edges) FALSE) {
  set.seed(seed)
  pairs <- t(combn(n, 2))
  repeat {
    edges <- pairs[runif(nrow(pairs)) < degree / (n - 1), , drop = FALSE]
    if (!reject(edges)) break
  }
  return(unname(split(edges, row(edges))))
}

# Saves the a priori unions and graph edges used for each number of players;
# file names start with prefix
save_structures <- function(n_players, unions, graph, out_dir, prefix = "") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  union_table <- do.call(rbind, lapply(n_players, function(n) {
    data.frame(n = n, player = 1:n,
               union = rep(seq_along(unions(n)), lengths(unions(n))))
  }))
  edge_table <- do.call(rbind, lapply(n_players, function(n) {
    edges <- do.call(rbind, graph(n))
    data.frame(n = n, player_1 = edges[, 1], player_2 = edges[, 2])
  }))
  write.csv2(union_table, file.path(out_dir, paste0(prefix, "unions.csv")),
             row.names = FALSE)
  write.csv2(edge_table, file.path(out_dir, paste0(prefix, "graph_edges.csv")),
             row.names = FALSE)
}


## ---- Running the experiments -------------------------------------------------

# Computes compute(v) and measures the resources used: wall and CPU time
# (seconds), peak memory used by R (MB) and number of evaluations of v
measure <- function(compute, v) {
  v_evals <- 0
  counted_v <- function(coalition) {
    v_evals <<- v_evals + 1
    v(coalition)
  }

  invisible(gc(reset = TRUE))
  time <- system.time(value <- compute(counted_v), gcFirst = FALSE)

  resources <- data.frame(wall_time = time[["elapsed"]],
                          cpu_time  = time[["user.self"]] + time[["sys.self"]],
                          memory_mb = sum(gc()[, 6]),  # max used (Mb)
                          v_evals   = v_evals)
  return(list(value = unname(value), resources = resources))
}

# Function of v computing f(v, ...): it only keeps f and its arguments, so it
# can be sent to another R session
with_args <- function(f, ...) {
  force(f)
  args <- list(...)
  function(v) do.call(f, c(list(v), args))
}

# Runs f(...) in a separate R session that is killed after time_limit seconds,
# so the limit also stops computations in compiled code. Returns its value, or
# NULL if the limit is exceeded or the computation fails.
run_with_limit <- function(f, args, time_limit) {
  tryCatch(
    callr::r(f, args = args, timeout = time_limit),
    error = function(e) {
      message("  stopped: ", conditionMessage(e))
      NULL
    })
}

# Exact computation measured with measure(), or NULL if it takes longer than
# time_limit seconds or fails
measure_exact <- function(compute, v, time_limit) {
  run_with_limit(function(measure, compute, v) {
    suppressPackageStartupMessages(library(TUvalues))
    measure(compute, v)
  }, list(measure = measure, compute = compute, v = v), time_limit)
}

# Volume of the convex hull of the core vertices (core_volume() in volume.R),
# or NA if it takes longer than time_limit seconds or fails
hull_volume <- function(vertices, time_limit) {
  volume <- run_with_limit(function(vertices, volume_file) {
    source(volume_file)
    suppressWarnings(core_volume(vertices))
  }, list(vertices = vertices, volume_file = volume_file), time_limit)
  return(if (is.null(volume)) NA else volume)
}

# Progress message preceded by the current time
progress <- function(...) message(format(Sys.time(), "%d/%m %H:%M:%S  "), ...)

# Runs the benchmark of the allocation rules for one family of games.
#   game:             function(n) returning the characteristic function
#   n_players:        numbers of players
#   n_rep_grid:       values of n_rep for the approximations
#   n_runs:           independent runs for each (rule, n, n_rep)
#   rules:            allocation rules (see allocation_rules)
#   unions, graph:    functions(n) returning the a priori unions and the
#                     edges of the communication graph
#   exact_time_limit: maximum time (seconds) of an exact computation; when it
#                     is exceeded, the exact value of that rule is not
#                     attempted for larger n
# Returns the resources used by each computation and the allocations obtained.
run_benchmark <- function(game, n_players, n_rep_grid, n_runs, rules,
                          unions, graph, exact_time_limit) {
  resources   <- list()
  allocations <- list()
  add <- function(rule, n, method, n_rep, run, result) {
    key <- data.frame(rule = rule, n = n, method = method, n_rep = n_rep,
                      run = run)
    resources[[length(resources) + 1]] <<- cbind(key, result$resources)
    allocations[[length(allocations) + 1]] <<-
      cbind(key, player = 1:n, value = result$value)
  }
  exact_stopped <- character(0)

  for (n in n_players) {
    v <- game(n)
    s <- list(unions = unions(n), graph = graph(n))
    for (rule in names(rules)) {
      progress("n = ", n, ", ", rule)
      if (!rule %in% exact_stopped) {
        compute <- with_args(rules[[rule]]$exact, n = n, s = s)
        result  <- measure_exact(compute, v, exact_time_limit)
        if (is.null(result)) {
          message("  exact ", rule, " not computed for n >= ", n)
          exact_stopped <- c(exact_stopped, rule)
        } else {
          add(rule, n, "exact", NA, NA, result)
        }
      }
      for (n_rep in n_rep_grid) {
        for (run in 1:n_runs) {
          set.seed(1e6 * n + 1e4 * run + n_rep)
          result <- measure(with_args(rules[[rule]]$approx, n = n,
                                      n_rep = n_rep, s = s), v)
          add(rule, n, "approx", n_rep, run, result)
        }
      }
    }
  }

  return(list(resources   = do.call(rbind, resources),
              allocations = do.call(rbind, allocations)))
}

# Runs the benchmark of the core for one family of games: exact and
# approximate core vertices (coreVertex). Each computation records its vertex
# centroid as an allocation (rule "core_centroid") and, in the core table, the
# number of vertices, the volume of the core and, when the exact core is
# available, the share of its vertices and of its volume captured by the
# approximation. The exact core and every volume are limited to
# exact_time_limit seconds. Other arguments as in run_benchmark().
run_core_benchmark <- function(game, n_players, n_rep_grid, n_runs,
                               exact_time_limit) {
  resources   <- list()
  allocations <- list()
  core        <- list()
  add <- function(n, method, n_rep, run, result, exact) {
    key <- data.frame(rule = "core_centroid", n = n, method = method,
                      n_rep = n_rep, run = run)
    vertices <- result$value
    # The volume of an approximate core is only useful (and computed) when
    # the volume of the exact core is available
    volume <- volume_time <- NA
    if (method == "exact" || (!is.null(exact) && !is.na(exact$volume))) {
      volume_time <- system.time(
        volume <- hull_volume(vertices, exact_time_limit))[["elapsed"]]
    }
    resources[[length(resources) + 1]] <<- cbind(key, result$resources)
    allocations[[length(allocations) + 1]] <<-
      cbind(key, player = 1:n, value = colMeans(vertices))
    core[[length(core) + 1]] <<- cbind(
      key[-1], n_vertices = nrow(vertices), volume = volume,
      volume_time  = volume_time,
      vertex_ratio = if (is.null(exact)) NA else nrow(vertices) / exact$n_vertices,
      volume_ratio = if (is.null(exact)) NA else volume / exact$volume)
  }
  exact_stopped <- FALSE

  for (n in n_players) {
    progress("core, n = ", n)
    v <- game(n)
    exact <- NULL
    if (!exact_stopped) {
      result <- measure_exact(with_args(exact_core, n = n), v, exact_time_limit)
      if (is.null(result)) {
        message("  exact core not computed for n >= ", n)
        exact_stopped <- TRUE
      } else {
        add(n, "exact", NA, NA, result, NULL)
        exact <- list(n_vertices = nrow(result$value),
                      volume = core[[length(core)]]$volume)
      }
    }
    for (n_rep in n_rep_grid) {
      for (run in 1:n_runs) {
        set.seed(1e6 * n + 1e4 * run + n_rep)
        result <- measure(function(v) {
          coreVertex(v, n_players = n, method = "appro", n_rep = n_rep,
                     echo = FALSE)
        }, v)
        add(n, "approx", n_rep, run, result, exact)
      }
    }
  }

  return(list(resources   = do.call(rbind, resources),
              allocations = do.call(rbind, allocations),
              core        = do.call(rbind, core)))
}


## ---- Error measures ----------------------------------------------------------

rmse <- function(x, y) sqrt(mean((x - y)^2))

# Errors of the approximations:
#   errors:   per run, RMSE and maximum absolute error with respect to the
#             exact value (if available), and step error: RMSE with respect
#             to the estimate of the previous n_rep (n_rep_prev) in the same
#             run
#   pairwise: per run, RMSE between the estimates of every pair of n_rep
#   mc:       per n_rep, RMSE estimated from the variability between runs,
#             sqrt(mean_i var(x_i)); it does not need the exact value
# Relative errors with respect to the exact value and from the runs divide by
# the mean absolute value of the exact allocation or, if not available, of the
# mean estimate with the largest n_rep. Relative step and pairwise errors
# divide by the mean absolute value of the estimate with the larger n_rep, so
# they only use the approximations and can serve as a stopping criterion.
compute_errors <- function(allocations) {
  errors   <- list()
  pairwise <- list()
  mc       <- list()

  cases <- unique(allocations[allocations$method == "approx", c("rule", "n")])
  for (k in seq_len(nrow(cases))) {
    rule <- cases$rule[k]
    n    <- cases$n[k]
    d <- allocations[allocations$rule == rule & allocations$n == n, ]
    d <- d[order(d$run, d$player), ]
    exact  <- d$value[d$method == "exact"]
    n_reps <- sort(unique(d$n_rep[d$method == "approx"]))
    # Estimates for each n_rep as a matrix (runs x players)
    estimates <- lapply(n_reps, function(n_rep) {
      matrix(d$value[which(d$method == "approx" & d$n_rep == n_rep)],
             ncol = n, byrow = TRUE)
    })
    has_exact <- length(exact) > 0
    reference <- if (has_exact) exact else colMeans(estimates[[length(n_reps)]])
    scale <- mean(abs(reference))

    for (i in seq_along(n_reps)) {
      x <- estimates[[i]]
      for (run in seq_len(nrow(x))) {
        estimate_scale <- mean(abs(x[run, ]))
        rmse_exact <- if (has_exact) rmse(x[run, ], exact) else NA
        rmse_step  <- NA
        if (i > 1) rmse_step <- rmse(x[run, ], estimates[[i - 1]][run, ])
        errors[[length(errors) + 1]] <- data.frame(
          rule = rule, n = n, n_rep = n_reps[i],
          n_rep_prev = if (i > 1) n_reps[i - 1] else NA, run = run,
          rmse_exact     = rmse_exact,
          rel_rmse_exact = rmse_exact / scale,
          max_abs_exact  = if (has_exact) max(abs(x[run, ] - exact)) else NA,
          rmse_step      = rmse_step,
          rel_rmse_step  = rmse_step / estimate_scale)
        for (j in seq_len(i - 1)) {
          rmse_pair <- rmse(x[run, ], estimates[[j]][run, ])
          pairwise[[length(pairwise) + 1]] <- data.frame(
            rule = rule, n = n, n_rep_1 = n_reps[j], n_rep_2 = n_reps[i],
            run = run, rmse = rmse_pair, rel_rmse = rmse_pair / estimate_scale)
        }
      }
      rmse_mc <- sqrt(mean(apply(x, 2, var)))
      mc[[length(mc) + 1]] <- data.frame(
        rule = rule, n = n, n_rep = n_reps[i],
        rmse_mc = rmse_mc, rel_rmse_mc = rmse_mc / scale)
    }
  }

  return(list(errors   = do.call(rbind, errors),
              pairwise = do.call(rbind, pairwise),
              mc       = do.call(rbind, mc)))
}


## ---- Summaries ---------------------------------------------------------------

# Mean and standard deviation of the measures for each group, side by side
mean_sd <- function(data, measures, groups) {
  means <- aggregate(data[measures], data[groups], mean)
  sds   <- aggregate(data[measures], data[groups], sd)
  names(means)[-seq_along(groups)] <- paste0(measures, "_mean")
  names(sds)[-seq_along(groups)]   <- paste0(measures, "_sd")
  summary <- merge(means, sds)
  return(summary[c(groups, rbind(paste0(measures, "_mean"),
                                 paste0(measures, "_sd")))])
}

# One row per (rule, n, n_rep) with the execution time, the number of
# evaluations of v and the errors of the approximations; n_rep_prev is the
# previous n_rep, used for the step error. For each n, the exact computation
# comes first, with n_rep "exact" and no errors. CPU time and memory are kept
# in resources.csv.
results_table <- function(results, errors) {
  resource_measures <- c("wall_time", "v_evals")
  error_measures <- c("rmse_exact", "rel_rmse_exact", "rmse_step",
                      "rel_rmse_step")
  groups <- c("rule", "n", "n_rep")
  is_exact <- results$resources$method == "exact"
  steps <- unique(errors$errors[c(groups, "n_rep_prev")])

  approx_rows <- Reduce(merge, list(
    steps,
    mean_sd(results$resources[!is_exact, ], resource_measures, groups),
    mean_sd(errors$errors, error_measures, groups),
    errors$mc))
  exact_rows <- results$resources[is_exact, c(groups, resource_measures)]
  names(exact_rows) <- c(groups, paste0(resource_measures, "_mean"))
  exact_rows[setdiff(names(approx_rows), names(exact_rows))] <- NA

  table <- rbind(exact_rows[names(approx_rows)], approx_rows)
  table <- table[order(table$rule, table$n, table$n_rep, na.last = FALSE), ]
  table$n_rep <- ifelse(is.na(table$n_rep), "exact", table$n_rep)
  table$v_evals_sd <- NULL  # the number of evaluations barely varies
  rownames(table) <- NULL
  return(table)
}

# Mean and standard deviation of the RMSE between every pair of n_rep
pairwise_table <- function(errors) {
  groups <- c("rule", "n", "n_rep_1", "n_rep_2")
  table <- mean_sd(errors$pairwise, c("rmse", "rel_rmse"), groups)
  table <- table[do.call(order, table[groups]), ]
  rownames(table) <- NULL
  return(table)
}

# One row per (n, n_rep) with the number of vertices, the volume and the share
# of the vertices and of the volume of the exact core captured by the
# approximation. For each n, the exact core comes first, with n_rep "exact".
core_table <- function(core) {
  measures <- c("n_vertices", "volume", "volume_time", "vertex_ratio",
                "volume_ratio")
  is_exact <- core$method == "exact"
  approx_rows <- mean_sd(core[!is_exact, ], measures, c("n", "n_rep"))
  exact_rows  <- core[is_exact, c("n", "n_rep", measures)]
  names(exact_rows) <- c("n", "n_rep", paste0(measures, "_mean"))
  exact_rows[setdiff(names(approx_rows), names(exact_rows))] <- NA

  table <- rbind(exact_rows[names(approx_rows)], approx_rows)
  table <- table[order(table$n, table$n_rep, na.last = FALSE), ]
  table$n_rep <- ifelse(is.na(table$n_rep), "exact", table$n_rep)
  rownames(table) <- NULL
  return(table)
}

# Relative RMSE against n_rep (log scales), with respect to the exact value
# and estimated from the variability between runs
plot_errors <- function(table) {
  approx <- table[table$n_rep != "exact", ]
  data <- rbind(
    data.frame(approx[c("rule", "n", "n_rep")],
               error = approx$rel_rmse_exact_mean, measure = "vs exact value"),
    data.frame(approx[c("rule", "n", "n_rep")],
               error = approx$rel_rmse_mc, measure = "estimated from runs"))
  data <- data[!is.na(data$error) & data$error > 0, ]  # log scale
  data$measure <- factor(data$measure,
                         levels = c("vs exact value", "estimated from runs"))
  data$rule    <- tools::toTitleCase(gsub("_", " ", data$rule))
  data$n_rep   <- as.numeric(data$n_rep)
  data$n       <- factor(data$n)

  # Ordinal blue ramp: darker for more players
  ramp <- c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281", "#0d366b")

  ggplot(data, aes(n_rep, error, colour = n)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.8) +
    facet_grid(measure ~ rule) +
    scale_x_log10(breaks = sort(unique(data$n_rep))) +
    scale_y_log10() +
    scale_colour_manual(values = colorRampPalette(ramp)(nlevels(data$n))) +
    labs(x = "Number of samples (n_rep)", y = "Relative RMSE",
         colour = "Players") +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          panel.spacing.x = unit(1.2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}


## ---- Saving ------------------------------------------------------------------

# Saves the raw results, the summary tables, the error plot and the
# computational environment in out_dir. Every file name starts with prefix,
# so the rules and the core of a game can be saved in the same folder. For the
# core (results of run_core_benchmark), the core measures are also saved.
save_benchmark <- function(results, out_dir, prefix = "") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  path   <- function(name) file.path(out_dir, paste0(prefix, name))
  errors <- compute_errors(results$allocations)
  table  <- results_table(results, errors)

  write.csv2(results$allocations, path("allocations.csv"), row.names = FALSE)
  write.csv2(results$resources, path("resources.csv"), row.names = FALSE)
  write.csv2(errors$errors, path("errors.csv"), row.names = FALSE)
  write.csv2(errors$pairwise, path("pairwise_errors.csv"), row.names = FALSE)
  sheets <- list(results = table, pairwise = pairwise_table(errors))
  if (!is.null(results$core)) {
    write.csv2(results$core, path("measures.csv"), row.names = FALSE)
    sheets$core <- core_table(results$core)
  }
  write_xlsx(sheets, path("results_table.xlsx"))

  plot  <- plot_errors(table)
  width <- 2 + 2.2 * length(unique(table$rule))
  ggsave(path("error_plot.pdf"), plot, width = width, height = 5)
  ggsave(path("error_plot.png"), plot, width = width, height = 5, dpi = 300)

  writeLines(c(paste("CPU:", Sys.getenv("PROCESSOR_IDENTIFIER")),
               paste("Cores:", parallel::detectCores()),
               capture.output(sessionInfo())),
             path("session_info.txt"))
}

# Parts of the benchmark requested on the command line: "rules" and/or
# "core". Without arguments, both parts are run, each one in a new R process
# started from `script` (so the core starts from a fresh session), and the
# current process ends.
benchmark_parts <- function(script) {
  parts <- commandArgs(trailingOnly = TRUE)
  if (length(parts) == 0) {
    for (part in c("rules", "core")) {
      callr::rscript(script, cmdargs = part, show = TRUE)
    }
    quit(save = "no")
  }
  return(parts)
}
