# Common argument checks.
# They are called by the interface functions (shapley, banzhaf, owen,
# banzhaf_owen, myerson, myerson_unions, coreVertex, centroid) and by the
# egalitarian functions. The internal *_exact and *_appro functions assume
# that their arguments have already been checked.


# Check the characteristic function and return the number of players.
# A vector must contain the value of every coalition in the coalitions()
# order, without (length 2^n - 1) or with (length 2^n) the empty coalition.
# A function requires n_players, which must be an integer greater than 1,
# with no upper limit. Vectors are limited to 30 players.
check_game <- function(characteristic_func, n_players, min_players = 1L) {

  if (is.function(characteristic_func)) {
    if (!is.numeric(n_players) || length(n_players) != 1L || is.na(n_players)) {
      stop("Invalid number of players specified. n_players must be an integer.")
    }
    if (n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    if (!is.finite(n_players) || n_players != floor(n_players) ||
        n_players > .Machine$integer.max) {
      stop("Invalid number of players specified. n_players must be an integer.")
    }
    n <- n_players

  } else if (is.numeric(characteristic_func) && !is.complex(characteristic_func) &&
             is.null(dim(characteristic_func))) {
    len <- length(characteristic_func)
    if (len < 1L) {
      stop("Invalid characteristic_func provided. The vector is empty.")
    }
    n <- round(log2(len + 1))
    if (len != 2^n - 1) {
      n <- round(log2(len))
      if (len != 2^n) {
        stop("Invalid characteristic_func provided. ",
             "Its length must be 2^n - 1 or 2^n for a game with n players.")
      }
    }
    if (any(!is.finite(characteristic_func))) {
      stop("Invalid characteristic_func provided. Coalition values must be finite.")
    }
    if (n < min_players) {
      stop("Invalid characteristic_func provided. ",
           "The game must have at least ", min_players, " players.")
    }
    if (n > 30L) {
      stop("Invalid characteristic_func provided. ",
           "A vector supports at most 30 players; use a function for larger games.")
    }

  } else {
    stop("Invalid characteristic_func provided.")
  }

  as.integer(n)
}


# Check the method of calculation
check_method <- function(method) {
  if (!is.character(method) || length(method) != 1L ||
      !method %in% c("exact", "appro")) {
    stop("Invalid method specified.\n",
         "Use \"exact\" for the exact value or \"appro\" for the approximation.")
  }
  invisible(method)
}


# Check the number of iterations and return it as an integer
check_n_rep <- function(n_rep) {
  if (!is.numeric(n_rep) || length(n_rep) != 1L || is.na(n_rep) || n_rep < 1) {
    stop("Invalid number of iterations specified. n_rep must be greater than 0.")
  }
  if (!is.finite(n_rep) || n_rep != floor(n_rep) || n_rep > .Machine$integer.max) {
    stop("Invalid number of iterations specified. n_rep must be an integer.")
  }
  as.integer(n_rep)
}


# Check a logical flag (echo, replace)
check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(name, " must be TRUE or FALSE.")
  }
  invisible(x)
}


# Check that the a priori unions form a partition of the players 1, ..., n.
# Return the unions as a list of integer vectors.
check_union <- function(union, n_players, name = "union") {
  if (!is.list(union) || length(union) < 1L) {
    stop("Invalid ", name, " provided. It must be a non-empty list of vectors of players.")
  }
  valid <- vapply(union, function(block) {
    is.numeric(block) && length(block) > 0L && all(is.finite(block)) &&
      all(block == floor(block))
  }, logical(1))
  if (!all(valid)) {
    stop("Invalid ", name, " provided. ",
         "Each element must be a non-empty vector of player indices.")
  }
  members <- unlist(union, use.names = FALSE)
  if (length(members) != n_players || anyDuplicated(members) ||
      !setequal(members, seq_len(n_players))) {
    stop("Invalid ", name, " provided. ",
         "It must be a partition of the set of players 1, ..., ", n_players, ".")
  }
  lapply(union, as.integer)
}


# Check the edges of the communication graph.
# Accept a list of pairs, a two-column matrix or data.frame, or NULL.
# Return a list of integer pairs.
check_graph_edges <- function(graph_edges, n_players) {
  if (is.null(graph_edges)) graph_edges <- list()
  if (is.data.frame(graph_edges)) graph_edges <- as.matrix(graph_edges)
  if (is.matrix(graph_edges)) {
    if (ncol(graph_edges) != 2L) {
      stop("Invalid graph_edges provided. A matrix must have two columns.")
    }
    graph_edges <- lapply(seq_len(nrow(graph_edges)), function(i) graph_edges[i, ])
  }
  if (!is.list(graph_edges)) {
    stop("Invalid graph_edges provided. It must be a list of pairs of players.")
  }
  for (edge in graph_edges) {
    if (!is.numeric(edge) || length(edge) != 2L || any(!is.finite(edge)) ||
        any(edge != floor(edge)) || any(edge < 1 | edge > n_players)) {
      stop("Invalid edges provided. Each edge must connect two players between 1 and ",
           n_players, ". Error in edge: ", paste(edge, collapse = ", "))
    }
  }
  lapply(graph_edges, as.integer)
}


# Check the tolerance of the exact core
check_tol <- function(tol) {
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0) {
    stop("tol must be one positive finite numeric value.")
  }
  invisible(tol)
}


# Values of a game given as a (checked) vector, in the coalitions() order and
# including the empty coalition, which is added with value 0 when missing.
game_values <- function(characteristic_func) {
  values <- as.numeric(characteristic_func)
  len <- length(values)
  if (len == 2^round(log2(len + 1)) - 1) values <- c(0, values)
  values
}
