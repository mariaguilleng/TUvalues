#' @title Pairwise stability of a network under the Myerson value
#'
#' @description
#' Check whether an undirected communication network is pairwise stable
#' in the sense of Jackson and Wolinsky (1996), with payoffs determined
#' by the Myerson value (Myerson, 1977).
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players. It can be provided as a vector or as a function.
#' @param graph_edges Edges of the communication graph of the game. It must be
#' a \code{list} of pairs indicating the connected players.
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param details If \code{FALSE}, only \code{TRUE} or \code{FALSE} is returned
#' and the check stops at the first profitable deviation. If \code{TRUE}, every
#' pair of players is checked and reported.
#'
#' @details
#' A network is pairwise stable if no player strictly benefits from
#' unilaterally deleting an existing link, and no absent link can be
#' added such that both endpoints weakly benefit and at least one
#' strictly benefits.
#'
#' Payoffs are determined by the standard Myerson value.
#' Link costs, additional transfers, and simultaneous changes to
#' multiple links are not considered.
#'
#' The change of payoff caused by each link is computed with exact rational
#' arithmetic (package \code{rcdd}), so ties are detected exactly. Values are
#' interpreted as the binary numbers stored by R. The value of the empty
#' coalition is taken as 0. The reported payoffs are converted to numbers.
#'
#' The change caused by the link between players \eqn{i} and \eqn{j} only
#' depends on the connected components of \eqn{i} and \eqn{j}, so the cost
#' grows exponentially with the size of the components, not with the number
#' of players. When \code{characteristic_func} is a function there is no limit
#' on the number of players.
#'
#' @return If \code{details} is \code{FALSE}, \code{TRUE} if the network is
#' pairwise stable and \code{FALSE} otherwise. If \code{details} is \code{TRUE},
#' a list with:
#' \item{stable}{Whether the network is pairwise stable.}
#' \item{myerson_value}{The Myerson value of each player in the network.}
#' \item{deviations}{The rows of \code{comparisons} that are profitable.}
#' \item{comparisons}{A \code{data.frame} with one row per pair of players:
#' the action considered (\code{"remove"} an existing link or \code{"add"} an
#' absent one), the payoffs of both players before and after the change, the
#' gain of each player and whether the change is profitable.}
#'
#' @references
#' Jackson, M. O. and Wolinsky, A. (1996).
#' A Strategic Model of Social and Economic Networks.
#' Journal of Economic Theory, 71(1), 44--74.
#' \doi{10.1006/jeth.1996.0108}
#'
#' Myerson, R. B. (1977).
#' Graphs and Cooperation in Games.
#' Mathematics of Operations Research, 2(3), 225--229.
#' \doi{10.1287/moor.2.3.225}
#'
#' @examples
#' v <- function(coalition) {
#'   k <- length(coalition)
#'   k * (k - 1) / 2
#' }
#' graph_edges <- list(c(1, 2), c(2, 3))
#' check_myerson_stability(v, graph_edges, n_players = 3)
#' check_myerson_stability(v, graph_edges, n_players = 3, details = TRUE)
#'
#' @examples
#' v <- c(0, 0, 0, 1, 1, 1, 3)
#' check_myerson_stability(v, list(c(1, 2), c(1, 3), c(2, 3)))
#'
#' @export

check_myerson_stability <- function(characteristic_func, graph_edges,
                                    n_players = 0, details = FALSE) {

  n <- validate_game(characteristic_func, n_players)
  graph_edges <- validate_graph_edges(graph_edges, n)
  validate_flag(details, "details")
  is_fun <- is.function(characteristic_func)

  # ----------------------------------------------------------
  # Exact (rational) value of a nonempty coalition of sorted players
  # ----------------------------------------------------------

  if (!is_fun) {
    values <- game_values(characteristic_func)
    values[1L] <- 0
    v_binary <- character(2^n)
    v_binary[tu_order(n)] <- rcdd::d2q(values)
    rm(values)
    player_bits <- 2^(seq_len(n) - 1L)
    value_q <- function(players) v_binary[sum(player_bits[players]) + 1]
  } else {
    cache <- new.env(hash = TRUE, parent = emptyenv())
    value_q <- function(players) {
      key <- paste(players, collapse = ",")
      known <- cache[[key]]
      if (!is.null(known)) return(known)
      value <- characteristic_func(players)
      if (!is.numeric(value) || is.complex(value) ||
          length(value) != 1L || !is.finite(value)) {
        stop("characteristic_func must return one finite real numeric value.")
      }
      known <- rcdd::d2q(as.numeric(value))
      cache[[key]] <- known
      known
    }
  }

  # ----------------------------------------------------------
  # Graph: neighbours and connected components
  # ----------------------------------------------------------

  neighbours <- replicate(n, integer(0), simplify = FALSE)
  for (edge in graph_edges) {
    neighbours[[edge[1L]]] <- union(neighbours[[edge[1L]]], edge[2L])
    neighbours[[edge[2L]]] <- union(neighbours[[edge[2L]]], edge[1L])
  }

  component_id <- integer(n)
  components <- list()
  for (start in seq_len(n)) {
    if (component_id[start]) next
    id <- length(components) + 1L
    component_id[start] <- id
    pending <- start
    while (length(pending)) {
      fresh <- neighbours[[pending[1L]]]
      fresh <- fresh[!component_id[fresh]]
      component_id[fresh] <- id
      pending <- c(pending[-1L], fresh)
    }
    components[[id]] <- which(component_id == id)
  }

  # Shapley weights (|S| - 1)! (m - |S|)! / m! as rationals, cached by m
  weight_cache <- list()
  shapley_weights <- function(m) {
    key <- as.character(m)
    if (is.null(weight_cache[[key]])) {
      w <- character(m)
      w[1L] <- rcdd::qdq("1", as.character(m))
      for (k in seq_len(m - 1L)) {
        w[k + 1L] <- rcdd::qdq(rcdd::qxq(w[k], as.character(k)), as.character(m - k))
      }
      weight_cache[[key]] <<- w
    }
    weight_cache[[key]]
  }

  # ----------------------------------------------------------
  # Exact change of the Myerson value of i (and of j) when the link i-j is
  # added to the network without that link. Only the coalitions S of the
  # components of i and j that contain i and j, and in which i and j are not
  # connected, contribute v(A U B) - v(A) - v(B), where A and B are the
  # components of i and j in S. Common neighbours of i and j are excluded,
  # since they would connect i and j.
  # ----------------------------------------------------------

  link_gain <- function(i, j) {
    R <- sort(union(components[[component_id[i]]], components[[component_id[j]]]))
    m <- length(R)
    li <- match(i, R)
    lj <- match(j, R)
    nb <- lapply(neighbours[R], function(q) match(q, R))
    nb[[li]] <- setdiff(nb[[li]], lj)
    nb[[lj]] <- setdiff(nb[[lj]], li)
    free <- setdiff(seq_len(m), c(li, lj, intersect(nb[[li]], nb[[lj]])))
    w <- shapley_weights(m)

    if (m <= 30L) {
      # Local bit masks over the players of R
      lbits <- as.integer(2^(seq_len(m) - 1L))
      adj <- vapply(nb, function(q) as.integer(sum(lbits[q])), integer(1))
      component <- function(mask, start_bit) {
        seen <- start_bit
        pending <- start_bit
        while (pending != 0L) {
          bit <- bitwAnd(pending, -pending)
          pending <- bitwXor(pending, bit)
          fresh <- bitwAnd(bitwAnd(adj[match(bit, lbits)], mask), bitwNot(seen))
          seen <- bitwOr(seen, fresh)
          pending <- bitwOr(pending, fresh)
        }
        seen
      }
      players_of <- function(mask) R[bitwAnd(mask, lbits) != 0L]

      subs <- 0L
      sizes <- 0L
      for (b in lbits[free]) {
        subs <- c(subs, subs + b)
        sizes <- c(sizes, sizes + 1L)
      }
      pair_mask <- lbits[li] + lbits[lj]
      left <- integer(length(subs))
      right <- integer(length(subs))
      cardinality <- integer(length(subs))
      used <- 0L
      for (s in seq_along(subs)) {
        mask <- subs[s] + pair_mask
        a <- component(mask, lbits[li])
        if (bitwAnd(a, lbits[lj]) == 0L) {
          used <- used + 1L
          left[used] <- a
          right[used] <- component(bitwXor(mask, a), lbits[lj])
          cardinality[used] <- sizes[s] + 2L
        }
      }
      if (!used) return("0")
      take <- seq_len(used)
      left <- left[take]
      right <- right[take]
      values_of <- function(masks) vapply(masks, function(mask) value_q(players_of(mask)),
                                          character(1))
      difference <- rcdd::qmq(
        rcdd::qmq(values_of(bitwOr(left, right)), values_of(left)),
        values_of(right)
      )
      return(rcdd::qsum(rcdd::qxq(w[cardinality[take]], difference)))
    }

    # Large components (only possible with a function): explicit sets of
    # players and a logical binary counter over the free players
    component_set <- function(in_set, start) {
      seen <- logical(m)
      seen[start] <- TRUE
      pending <- start
      while (length(pending)) {
        fresh <- nb[[pending[1L]]]
        fresh <- fresh[in_set[fresh] & !seen[fresh]]
        seen[fresh] <- TRUE
        pending <- c(pending[-1L], fresh)
      }
      seen
    }
    gain <- "0"
    selected <- logical(length(free))
    repeat {
      in_set <- logical(m)
      in_set[c(li, lj, free[selected])] <- TRUE
      a <- component_set(in_set, li)
      if (!a[lj]) {
        b <- component_set(in_set & !a, lj)
        difference <- rcdd::qmq(rcdd::qmq(value_q(R[a | b]), value_q(R[a])), value_q(R[b]))
        gain <- rcdd::qpq(gain, rcdd::qxq(w[sum(in_set)], difference))
      }
      pos <- match(FALSE, selected)
      if (is.na(pos)) break
      selected[seq_len(pos - 1L)] <- FALSE
      selected[pos] <- TRUE
    }
    gain
  }

  # ----------------------------------------------------------
  # Every pair of players: remove an existing link or add an absent one
  # ----------------------------------------------------------

  pairs <- if (n > 1L) t(utils::combn(seq_len(n), 2L)) else matrix(integer(0), ncol = 2L)
  n_pairs <- nrow(pairs)
  if (details) {
    changes <- character(n_pairs)
    actions <- character(n_pairs)
    profitable <- logical(n_pairs)
  }

  for (p in seq_len(n_pairs)) {
    i <- pairs[p, 1L]
    j <- pairs[p, 2L]
    present <- j %in% neighbours[[i]]
    gain <- link_gain(i, j)
    if (present) gain <- rcdd::qneg(gain)
    is_profitable <- rcdd::qsign(gain) > 0L

    if (!details) {
      if (is_profitable) return(FALSE)
    } else {
      changes[p] <- gain
      actions[p] <- if (present) "remove" else "add"
      profitable[p] <- is_profitable
    }
  }

  if (!details) return(TRUE)

  # ----------------------------------------------------------
  # Current Myerson value, computed separately in each component
  # ----------------------------------------------------------

  current <- numeric(n)
  for (C in components) {
    if (length(C) == 1L) {
      current[C] <- rcdd::q2d(value_q(C))
      next
    }
    sub_game <- function(S) rcdd::q2d(value_q(C[S]))
    local_edges <- lapply(graph_edges[vapply(graph_edges, function(edge) edge[1L] %in% C,
                                              logical(1))],
                          function(edge) match(edge, C))
    game <- myerson_graph_game(sub_game, local_edges, length(C), "exact")
    current[C] <- shapley_exact(game$characteristic_func, game$n_players)
  }
  names(current) <- seq_len(n)

  gains <- if (n_pairs) rcdd::q2d(changes) else numeric(0)
  before_i <- current[pairs[, 1L]]
  before_j <- current[pairs[, 2L]]
  comparisons <- data.frame(
    player_i = pairs[, 1L],
    player_j = pairs[, 2L],
    action = actions,
    before_i = before_i,
    before_j = before_j,
    after_i = before_i + gains,
    after_j = before_j + gains,
    gain_i = gains,
    gain_j = gains,
    destabilizing = profitable,
    stringsAsFactors = FALSE
  )

  list(
    stable = !any(profitable),
    myerson_value = current,
    deviations = comparisons[profitable, , drop = FALSE],
    comparisons = comparisons
  )
}
