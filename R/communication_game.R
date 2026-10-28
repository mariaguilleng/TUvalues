#' @title Communication game
#'
#' @description
#' Build the communication (graph-restricted) game of a communication
#' situation, in which only connected players can cooperate (Myerson, 1977).
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players. It can be provided as a vector or as a function.
#' @param graph_edges Edges of the communication graph of the game. It must be
#' a \code{list} of pairs indicating the connected players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#'
#' @details
#' The worth of a coalition \eqn{S} in the communication game is the sum of
#' the worths of the connected components of the subgraph induced by \eqn{S}:
#' \deqn{v^G(S) = \sum_{C \in C^G(S)} v(C).}
#' The Myerson value is the Shapley value of this game, so
#' \code{shapley(communication_game(v, graph_edges))} equals
#' \code{myerson(v, graph_edges)}.
#'
#' The value of the empty coalition is taken as 0.
#'
#' @return If \code{characteristic_func} is a vector, a vector with the value
#' of every nonempty coalition of the communication game, in the order given
#' by \code{\link{coalitions}}. If \code{characteristic_func} is a function, a
#' function that receives a coalition (a vector of players) and returns its
#' value in the communication game. It evaluates \code{characteristic_func}
#' only on the connected components of the coalitions it receives, so there is
#' no limit on the number of players, and it can be passed directly to the
#' other functions of the package.
#'
#' @references Myerson, R. B. (1977). Graphs and Cooperation in Games.
#' Mathematics of Operations Research, 2(3), 225--229.
#' \doi{10.1287/moor.2.3.225}
#'
#' @seealso \code{\link{myerson}}, \code{\link{myerson_unions}}
#'
#' @examples
#' v <- c(1, 2, 3, 10, 20, 30, 100)
#' communication_game(v, graph_edges = list(c(1, 3)))
#'
#' @examples
#' v <- function(coalition) length(coalition)^2
#' graph_edges <- list(c(1, 2), c(2, 3))
#' vG <- communication_game(v, graph_edges, n_players = 4)
#' vG(c(1, 2, 4))
#' shapley(vG, n_players = 4)
#' myerson(v, graph_edges, n_players = 4)
#'
#' @export

communication_game <- function(characteristic_func, graph_edges, n_players = 0) {

  n_players <- validate_game(characteristic_func, n_players)
  graph_edges <- validate_graph_edges(graph_edges, n_players)

  # Vector game: values of every coalition, in the coalitions() order
  if (!is.function(characteristic_func)) {
    game <- myerson_graph_game(characteristic_func, graph_edges, n_players, "exact")
    return(game$characteristic_func[-1L])
  }

  # Function game: values computed when requested, from the connected
  # components of the coalition (no bit masks and no cache)
  n <- n_players
  neighbours <- replicate(n, integer(0), simplify = FALSE)
  for (edge in graph_edges) {
    neighbours[[edge[1L]]] <- c(neighbours[[edge[1L]]], edge[2L])
    neighbours[[edge[2L]]] <- c(neighbours[[edge[2L]]], edge[1L])
  }

  value_of <- function(players) {
    value <- characteristic_func(players)
    if (!is.numeric(value) || is.complex(value) ||
        length(value) != 1L || !is.finite(value)) {
      stop("characteristic_func must return one finite real numeric value.")
    }
    as.numeric(value)
  }

  function(coalition) {
    if (!is.numeric(coalition) || any(!is.finite(coalition)) ||
        any(coalition != floor(coalition)) || any(coalition < 1 | coalition > n) ||
        anyDuplicated(coalition)) {
      stop("Invalid coalition provided. It must be a vector of different players between 1 and ",
           n, ".")
    }
    in_coalition <- logical(n)
    in_coalition[coalition] <- TRUE
    seen <- logical(n)
    total <- 0
    for (start in sort(coalition)) {
      if (seen[start]) next
      # Connected component of start within the coalition
      seen[start] <- TRUE
      component <- start
      pending <- start
      while (length(pending)) {
        fresh <- neighbours[[pending[1L]]]
        fresh <- fresh[in_coalition[fresh] & !seen[fresh]]
        seen[fresh] <- TRUE
        component <- c(component, fresh)
        pending <- c(pending[-1L], fresh)
      }
      total <- total + value_of(sort(component))
    }
    total
  }
}
