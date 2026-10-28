#' @title Core membership of an allocation
#'
#' @description
#' Check whether an allocation belongs to the core of the game, that is,
#' whether it is efficient (the players share exactly \eqn{v(N)}) and
#' coalitionally rational (every coalition \eqn{S} receives at least
#' \eqn{v(S)}).
#'
#' @param allocation Numeric vector with the payoff of each player. Its length
#' is the number of players of the game.
#' @param characteristic_func The characteristic function of the game. It can be
#' a numeric vector with the value of every coalition, in the order given by
#' \code{\link{coalitions}}: of length \eqn{2^n - 1}, without the empty
#' coalition (whose value is taken as 0), or of length \eqn{2^n}, with the
#' empty coalition first. Vectors support up to 30 players. It can also be a
#' function that receives a coalition (a vector of players) and returns its
#' value; in that case there is no limit on the number of players.
#' @param details If \code{FALSE}, only \code{TRUE} or \code{FALSE} is returned
#' and the check stops at the first violated constraint. If \code{TRUE}, every
#' constraint is checked and reported.
#'
#' @details
#' All comparisons are exact: integer allocations are summed exactly in double
#' precision and any other allocation is handled with exact rational
#' arithmetic (package \code{rcdd}). Values are interpreted as the binary
#' numbers stored by R. The value of the empty coalition is taken as 0.
#'
#' When \code{characteristic_func} is a function, there is no limit on the
#' number of players, but up to \eqn{2^n} coalitions must be evaluated.
#'
#' @return If \code{details} is \code{FALSE}, \code{TRUE} if the allocation
#' belongs to the core and \code{FALSE} otherwise. If \code{details} is
#' \code{TRUE}, a list with:
#' \item{in_core}{Whether the allocation belongs to the core.}
#' \item{efficient}{Whether the allocation is efficient.}
#' \item{coalitionally_rational}{Whether every coalition receives at least its value.}
#' \item{violations}{The rows of \code{constraints} that are not satisfied.}
#' \item{constraints}{A \code{data.frame} with one row per coalition (in the
#' order of \code{\link{coalitions}}): the payoff assigned to the coalition,
#' its value, the slack, the relation required and whether it is satisfied.}
#'
#' @examples
#' v <- c(0, 0, 0, 2, 2, 2, 3)
#' check_core(c(1, 1, 1), v)
#' check_core(c(0, 1, 2), v)
#' check_core(c(0, 1, 2), v, details = TRUE)
#'
#' @examples
#' v <- function(coalition) length(coalition)^2
#' check_core(rep(4, 4), v)
#'
#' @export

check_core <- function(allocation, characteristic_func, details = FALSE) {

  if (!is.numeric(allocation) || is.complex(allocation) ||
      !is.null(dim(allocation)) || !length(allocation) ||
      any(!is.finite(allocation))) {
    stop("Invalid allocation provided. It must be a non-empty vector of finite numbers.")
  }
  validate_flag(details, "details")
  x <- as.numeric(allocation)
  n <- length(x)
  n_game <- validate_game(characteristic_func, n)
  if (n_game != n) {
    stop("Invalid allocation provided. Its length (", n, ") must be equal to ",
         "the number of players of the game (", n_game, ").")
  }
  is_fun <- is.function(characteristic_func)

  evaluate <- function(players) {
    value <- characteristic_func(players)
    if (!is.numeric(value) || is.complex(value) ||
        length(value) != 1L || !is.finite(value)) {
      stop("characteristic_func must return one finite real numeric value.")
    }
    as.numeric(value)
  }

  # Integer allocations are summed exactly in double precision; otherwise
  # payoffs are converted to exact rationals.
  integer_safe <- all(x == trunc(x)) && all(abs(x) <= floor(2^52 / n))
  xq <- if (integer_safe) x else rcdd::d2q(x)
  zero <- if (integer_safe) 0 else "0"
  add <- function(a, b) if (integer_safe) a + b else rcdd::qpq(a, b)
  # Sign of assigned - required (required given as doubles)
  compare <- function(assigned, required) {
    if (integer_safe) {
      sign(assigned - required)
    } else {
      rcdd::qsign(rcdd::qmq(assigned, rcdd::d2q(required)))
    }
  }
  # Payoff of every subset of 'players', in binary order
  subset_sums <- function(players) {
    sums <- zero
    for (p in players) sums <- c(sums, add(sums, rep(xq[p], length(sums))))
    sums
  }
  total <- if (integer_safe) sum(x) else rcdd::qsum(xq)

  # ==========================================================
  # DETAILED REPORT: every constraint, in the coalitions() order
  # ==========================================================

  if (details) {
    count <- 2^n
    ord <- tu_order(n)
    bits <- 2^(seq_len(n) - 1L)
    members <- function(index) which(((index - 1) %/% bits) %% 2 == 1)

    if (is_fun) {
      required <- c(0, vapply(ord[-1L], function(index) evaluate(members(index)),
                              numeric(1)))
    } else {
      required <- game_values(characteristic_func)
      required[1L] <- 0
    }
    assigned <- subset_sums(seq_len(n))[ord]

    assigned_q <- if (integer_safe) rcdd::d2q(assigned) else assigned
    slack_q <- rcdd::qmq(assigned_q, rcdd::d2q(required))
    satisfied <- rcdd::qsign(slack_q) >= 0L
    efficient <- rcdd::qsign(slack_q[count]) == 0L
    coalitionally_rational <- all(satisfied[-count])
    satisfied[count] <- efficient

    labels <- c("{}", vapply(ord[-1L], function(index) toString(members(index)),
                             character(1)))
    constraints <- data.frame(
      coalition = labels,
      assigned = if (integer_safe) assigned else rcdd::q2d(assigned),
      required = required,
      slack = rcdd::q2d(slack_q),
      relation = c(rep(">=", count - 1), "=="),
      satisfied = satisfied,
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    return(list(
      in_core = efficient && coalitionally_rational,
      efficient = efficient,
      coalitionally_rational = coalitionally_rational,
      violations = constraints[!satisfied, , drop = FALSE],
      constraints = constraints
    ))
  }

  # ==========================================================
  # MEMBERSHIP ONLY: stop at the first violated constraint
  # ==========================================================

  if (!is_fun) {
    values <- game_values(characteristic_func)
    values[1L] <- 0
    v_binary <- numeric(2^n)
    v_binary[tu_order(n)] <- values
    if (compare(total, v_binary[2^n]) != 0) return(FALSE)
    return(all(compare(subset_sums(seq_len(n))[-1L], v_binary[-1L]) >= 0))
  }

  # Efficiency and individual rationality first
  if (compare(total, evaluate(seq_len(n))) != 0) return(FALSE)
  for (i in seq_len(n)) {
    if (compare(xq[i], evaluate(i)) < 0) return(FALSE)
  }

  # Remaining coalitions, by blocks of 2^L that share the same "high"
  # players (L + 1, ..., n), enumerated with a logical binary counter
  L <- min(n, 16L)
  H <- n - L
  low_bits <- as.integer(2^(seq_len(L) - 1L))
  low_sets <- lapply(0:(2^L - 1L), function(mask) which(bitwAnd(mask, low_bits) != 0L))
  low_sums <- subset_sums(seq_len(L))
  high <- logical(H)

  repeat {
    high_set <- L + which(high)
    # Skip the empty coalition and the singletons, already checked
    idx <- seq_len(2^L)
    if (!length(high_set)) idx <- idx[lengths(low_sets) >= 2L]
    if (length(idx)) {
      vals <- vapply(idx, function(j) evaluate(c(low_sets[[j]], high_set)), numeric(1))
      high_sum <- if (!length(high_set)) zero else if (integer_safe) {
        sum(x[high_set])
      } else {
        rcdd::qsum(xq[high_set])
      }
      assigned <- add(low_sums[idx], rep(high_sum, length(idx)))
      if (any(compare(assigned, vals) < 0)) return(FALSE)
    }

    # Next block: add one to the binary counter of the high players
    pos <- match(FALSE, high)
    if (is.na(pos)) break
    high[seq_len(pos - 1L)] <- FALSE
    high[pos] <- TRUE
  }

  return(TRUE)
}
