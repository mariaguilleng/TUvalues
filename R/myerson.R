#' @title Myerson value
#'
#' @description
#' Calculate the Myerson value in a communication game.
#'
#' @param characteristic_func The characteristic function of the game. It can be
#' a numeric vector with the value of every coalition, in the order given by
#' \code{\link{coalitions}}: of length \eqn{2^n - 1}, without the empty
#' coalition (whose value is taken as 0), or of length \eqn{2^n}, with the
#' empty coalition first. Vectors support up to 30 players. It can also be a
#' function that receives a coalition (a vector of players) and returns its
#' value; in that case there is no limit on the number of players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param graph_edges Edges of the communication graph of the game: a
#' \code{list} of pairs of players (for example \code{list(c(1, 2), c(2, 3))}),
#' or a two-column matrix or data frame. Each edge must join two different
#' players.
#' @param method Method used to calculate the Myerson value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated polynomial
#' calculation, applying to the communication game the sampling procedure of the
#' Shapley value proposed by Castro et al. (2009).
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#' @param echo Only used if \code{method} is \code{appro}. Show progress of the
#' approximated calculation.
#'
#' @return The Myerson value for each player.
#'
#' @references Myerson, R. B. (1977). Graphs and Cooperation in Games.
#' Mathematics of Operations Research, 2(3), 225--229.
#' \doi{10.1287/moor.2.3.225}
#'
#' Castro, J., Gómez, D., & Tejada, J. (2009). Polynomial calculation
#' of the Shapley value based on sampling. Computers & operations research,
#' 36(5), 1726-1730.
#'
#' @examples
#' characteristic_func <- c(
#' 1, 2, 0, 3,
#' 3, 1, 4, 2, 5, 3,
#' 3, 6, 4, 5,
#' 15
#' )
#' graph_edges <- list(c(1, 2), c(2, 4))
#' myerson(characteristic_func, graph_edges, method = "exact")
#' myerson(characteristic_func, graph_edges, method = "appro", n_rep = 1000)
#'
#' @examples
#' v <- function(S) {
#'   if (length(S) == 2) {
#'     return(1)
#'    }
#'    return(0)
#' }
#' n <- 3
#' graph_edges <- list(c(1, 2))
#' myerson(v, graph_edges, n_players = n, method = "exact")
#' myerson(v, graph_edges, n_players = n, method = "appro", n_rep = 2000)
#'
#' @export

myerson <- function(characteristic_func, graph_edges, n_players = 0, method = "exact",
                    n_rep = 10000, echo = TRUE){

  validate_method(method)
  n_players <- validate_game(characteristic_func, n_players)
  graph_edges <- validate_graph_edges(graph_edges, n_players)
  if (method == "appro") {
    n_rep <- validate_n_rep(n_rep)
    validate_flag(echo, "echo")
  }

  # Graph-restricted game
  game <- myerson_graph_game(characteristic_func, graph_edges, n_players, method)

  # Myerson value = Shapley value of the graph-restricted game
  if (method == "exact") {
    shapley_value <- shapley_exact(game$characteristic_func, game$n_players)
  } else {
    shapley_value <- shapley_appro(game$characteristic_func, game$n_players, n_rep, echo)
  }

  return(shapley_value)

}


# Graph-restricted game v_G(S) = sum of v(C) over the connected components C
# of S in the communication graph.
# With method "exact" it is returned as a vector in the coalitions() order.
# With method "appro" it is returned as a function that is evaluated (and
# cached) only on the coalitions visited by the sampling.
# With more than 30 players (only possible for a function) it is returned,
# for both methods, as a function evaluated through the connected components.
# The empty coalition is assumed to have value 0.
# The arguments are assumed to be checked (graph_edges as a list of pairs).
myerson_graph_game <- function(characteristic_func, graph_edges, n_players, method) {

  is_fun <- is.function(characteristic_func)
  n <- n_players
  if (!is_fun) {
    values <- game_values(characteristic_func)
    values[1L] <- 0
  }

  # Bit masks are limited to 30 players. Larger games (only possible with a
  # function) use explicit connected components and are not tabulated.
  if (n > 30L) {
    G_adj <- get_adj_matrix(n, graph_edges)
    characteristic_func_graph <- function(coalition) {
      if (length(coalition) == 0) {
        return(0)
      }
      v_s <- 0
      for (comp in get_connected_components(coalition, G_adj, n)) {
        v_s <- v_s + characteristic_func(comp)
      }
      return(v_s)
    }
    return(list(characteristic_func = characteristic_func_graph, n_players = n))
  }

  bits <- as.integer(2^(seq_len(n) - 1L))

  # coalitions() orders coalitions by cardinality and, within each
  # cardinality, in combn() lexicographic order. Reverse-bit keys reproduce
  # that order from binary masks (player 1 is the least significant bit).
  sizes <- 0L
  key <- 0L
  for (i in seq_len(n)) {
    sizes <- c(sizes, sizes + 1L)
    key <- c(2L * key, 2L * key + 1L)
  }
  ord <- order(sizes, -key, method = "radix")
  rm(sizes, key)

  if (!is_fun) {
    v_binary <- numeric(2^n)
    v_binary[ord] <- values
    rm(values)
    get_value <- function(mask) v_binary[mask + 1L]
  } else {
    get_value <- function(mask) {
      value <- characteristic_func(which(bitwAnd(mask, bits) != 0L))
      if (!is.numeric(value) || is.complex(value) ||
          length(value) != 1L || !is.finite(value)) {
        stop("characteristic_func must return one finite real numeric value.")
      }
      as.numeric(value)
    }
  }

  # Adjacency represented as bit masks
  adjacency <- integer(n)
  for (edge in graph_edges) {
    a <- as.integer(edge[1L])
    b <- as.integer(edge[2L])
    adjacency[a] <- bitwOr(adjacency[a], bits[b])
    adjacency[b] <- bitwOr(adjacency[b], bits[a])
  }

  # Connected component of 'mask' containing its smallest player
  first_component <- function(mask) {
    pending <- bitwAnd(mask, -mask)
    component <- pending
    while (pending != 0L) {
      bit <- bitwAnd(pending, -pending)
      pending <- bitwXor(pending, bit)
      player <- match(bit, bits)
      fresh <- bitwAnd(bitwAnd(adjacency[player], mask), bitwNot(component))
      component <- bitwOr(component, fresh)
      pending <- bitwOr(pending, fresh)
    }
    component
  }

  if (method == "exact") {
    # Restricted values for every coalition, in binary-mask order.
    # component and rest are smaller masks, already computed.
    count <- 2^n - 1L
    restricted <- numeric(count + 1L)
    for (mask in seq_len(count)) {
      component <- first_component(mask)
      rest <- bitwXor(mask, component)
      restricted[mask + 1L] <- if (rest == 0L) {
        get_value(mask)
      } else {
        restricted[component + 1L] + restricted[rest + 1L]
      }
    }
    if (any(!is.finite(restricted))) stop("Overflow in restricted-game values.")
    return(list(characteristic_func = restricted[ord], n_players = n))
  }

  # Sparse cache: only the coalitions reached by the sampling are evaluated
  cache <- new.env(hash = TRUE, parent = emptyenv())
  assign("0", 0, envir = cache)
  restricted_value <- function(mask) {
    key <- as.character(mask)
    known <- cache[[key]]
    if (!is.null(known)) return(known)
    component <- first_component(mask)
    rest <- bitwXor(mask, component)
    value <- if (rest == 0L) {
      get_value(mask)
    } else {
      restricted_value(component) + restricted_value(rest)
    }
    if (!is.finite(value)) stop("Overflow in restricted-game values.")
    cache[[key]] <- value
    value
  }
  characteristic_func_graph <- function(coalition) {
    restricted_value(as.integer(sum(bits[coalition])))
  }

  return(list(characteristic_func = characteristic_func_graph, n_players = n))
}


# Build Adjacency Matrix (G) from edges
get_adj_matrix <- function(n_players, graph_edges) {

  G_adj <-  matrix(0, nrow = n_players, ncol = n_players)
  for (edge in graph_edges) {
    p1 <- edge[1]
    p2 <- edge[2]
    G_adj[p1, p2] <- 1
    G_adj[p2, p1] <- 1
  }
  return(G_adj)
}


# Helper Function to Find Connected Components (Depth-first search)
get_connected_components <- function(players_in_S, adj_matrix, n_players) {
  if (length(players_in_S) == 0) return(list())

  # Start with all players in S unvisited (TRUE for players OUTSIDE S)
  visited <- rep(FALSE, n_players)
  visited[setdiff(1:n_players, players_in_S)] <- TRUE

  components <- list()

  for (start_node in players_in_S) {
    if (!visited[start_node]) {
      component <- integer(0)
      stack <- start_node
      visited[start_node] <- TRUE

      while (length(stack) > 0) {
        current <- stack[1]
        stack <- stack[-1] # Pop
        component <- c(component, current)

        # Find neighbors of current node (within G_adj)
        neighbors <- which(adj_matrix[current, ] == 1)

        for (neighbor in neighbors) {
          # Check if neighbor is in S AND not yet visited
          if (neighbor %in% players_in_S && !visited[neighbor]) {
            visited[neighbor] <- TRUE
            stack <- c(neighbor, stack) # Push to stack (DFS)
          }
        }
      }
      components <- c(components, list(sort(component)))
    }
  }
  return(components)
}
