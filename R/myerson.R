#' @title Myerson value
#'
#' @description
#' Calculate the Myerson value in a communication game.
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players. It can be provided as a vector or as a function.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param graph_edges Edges of the communication graph of the game. It must be
#' a \code{list} of pairs indicating the connected players.
#' @param method Method used to  calculate the Myerson value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated polynomial
#' calculation based on sampling proposed.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation.
#'
#' @return The Myerson value for each player.
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
#' myerson(characteristic_func, graph_edges, method = "appro", n_rep = 100)
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
#' myerson(v, graph_edges, n_players = n, method = "appro", n_rep = 10000)
#'
#' @export

myerson <- function(characteristic_func, graph_edges, n_players = 0, method = "exact",
                    n_rep = 10000){

  if (! method %in% c("exact", "appro")) {
    stop("Invalid methos specified\n Use \"exact\" for the exact value or \"appro\"
         for the approximation.")
  }

  if(!is.vector(characteristic_func) && !is.function(characteristic_func)) {
    stop("Invalid characteristic_func provided.")
  }

  if (is.function(characteristic_func) && n_players < 2) {
    stop("Invalid number of players specified. n_players must be greater than 1.")
  }

  if (method == "appro" & n_rep < 1) {
    stop("Invalid number of iterations specified. n_rep must be greater than 0.")
  }

  if (is.vector(characteristic_func)) {

    # get number of players
    n_players<-log(length(characteristic_func),2)
    if (n_players!=round(n_players)){
      characteristic_func <- c(0, characteristic_func)
      n_players<-log(length(characteristic_func),2)
    }
    characteristic_func <- characteristic_func[-1]

  } else if (is.function(characteristic_func)) {
    # Calculate v_coalitions if function is provided
    results_by_size <- lapply(X = 1:n_players, FUN = function(m) {
      return(combn(x = 1:n_players, m = m, FUN = characteristic_func))
    })
    characteristic_func <- unlist(results_by_size)
  }

  # Get the modified characteristic function for the graph game
  characteristic_func_graph <- get_characteristic_func_graph(characteristic_func, n_players)

  # Calculate Shapley value using the modified characteristic function
  if (method == "exact") {
    shapley_value <- shapley_exact(characteristic_func_graph, n_players)
  } else {
    shapley_value <- shapley_appro(characteristic_func_graph, n_players, n_rep)
  }

  return(shapley_value)

}


# Calculate Graph Characteristic Function
get_characteristic_func_graph <- function(characteristic_func, n_players) {

  # Build Adjacency Matrix (G) from edges
  G_adj <- matrix(0, nrow = n_players, ncol = n_players)
  if (length(graph_edges) > 0) {
    for (edge in graph_edges) {
      if (length(edge) != 2) {
        stop("Invalid edges provided. Each edge must connect two players.
             Error in edge: ", paste(edge, collapse = ", "))
      }
      p1 <- edge[1]
      p2 <- edge[2]
      G_adj[p1, p2] <- 1
      G_adj[p2, p1] <- 1
    }
  }

  # Calculate the characteristic function for the graph game
  characteristic_func_graph <- rep(0, length(characteristic_func))

  coa_matrix <- coalitions(n_players)$Binary[-1, ] # Exclude empty set
  for (i in seq_len(nrow(coa_matrix))) {
    coalition_vector <- coa_matrix[i, ]
    players_in_S <- which(coalition_vector == 1)

    # Find connected components within S
    components <- get_connected_components(players_in_S, G_adj, n_players)

    v_s <- 0
    for (comp in components) {
      # Create the binary vector for component comp
      binary_component <- rep(0, n_players)
      binary_component[comp] <- 1

      # Find the corresponding index in the coa_matrix
      coalition_index <- which(apply(coa_matrix, 1, function(row) all(row == binary_component)))
      v_s <- v_s + characteristic_func[coalition_index]

    }
    characteristic_func_graph[i] <- v_s
  }
  return(characteristic_func_graph)
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

