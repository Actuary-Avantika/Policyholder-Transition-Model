# ============================================================
# GLOBAL.R
# Policyholder Transition Model | Shared data & helper functions
# Loaded once, before ui.R and server.R
# ============================================================

library(shiny)
library(ggplot2)


# ============================================================
# 1. MODEL INPUTS
# ============================================================

states <- c(
  "Active",
  "Claim",
  "Lapsed",
  "Closed"
)

state_colors <- c(
  "Active" = "#145951",
  "Claim"  = "#8C4A44",
  "Lapsed" = "#B98B2E",
  "Closed" = "#5F6668"
)

# Base (assumed) transition matrix
#
# Rows    = current state
# Columns = next state
#
#                 Active Claim Lapsed Closed
# Active           0.47  0.10  0.27   0.16
# Claim            0.42  0.09  0.26   0.23
# Lapsed           0.14  0.02  0.62   0.22
# Closed           0.00  0.00  0.00   1.00

base_matrix <- matrix(
  c(
    0.47, 0.10, 0.27, 0.16,
    0.42, 0.09, 0.26, 0.23,
    0.14, 0.02, 0.62, 0.22,
    0.00, 0.00, 0.00, 1.00
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(states, states)
)

# Initial distribution
initial_distribution <- c(
  Active = 1,
  Claim = 0,
  Lapsed = 0,
  Closed = 0
)


# ============================================================
# 2. CORE MARKOV CHAIN FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# matrix_power(M, n)
#
# Returns M raised to the power n via repeated multiplication.
# Fine for the small n (<= 100) used in this app; for much
# larger n an eigendecomposition (or expm::`%^%`) would scale
# better since this is O(n) matrix multiplications.
# ------------------------------------------------------------

matrix_power <- function(M, n) {

  if (n == 0) {
    return(diag(nrow(M)))
  }

  result <- diag(nrow(M))

  for (i in seq_len(n)) {
    result <- result %*% M
  }

  result
}


# ------------------------------------------------------------
# scenario_matrix(lapse_rate)
#
# Builds a transition matrix that overrides Active -> Lapsed
# with `lapse_rate`. To keep the row summing to 1, the
# difference is transferred to Active -> Active.
#
# lapse_rate is capped to [0, original_lapse + P(Active,Active)]
# so Active -> Active can never go negative.
# ------------------------------------------------------------

scenario_matrix <- function(lapse_rate) {

  M <- base_matrix

  original_lapse <- M["Active", "Lapsed"]
  max_lapse <- original_lapse + M["Active", "Active"]

  lapse_rate <- max(0, min(lapse_rate, max_lapse))

  difference <- original_lapse - lapse_rate

  M["Active", "Lapsed"] <- lapse_rate

  M["Active", "Active"] <-
    M["Active", "Active"] + difference

  M
}


# ------------------------------------------------------------
# calculate_stationary(M)
#
# Solves pi P = pi, subject to sum(pi) = 1, i.e.
# (P' - I) pi' = 0 with one equation replaced by sum(pi) = 1.
# ------------------------------------------------------------

calculate_stationary <- function(M) {

  A <- t(M) - diag(nrow(M))

  A[nrow(A), ] <- 1

  b <- c(rep(0, nrow(M) - 1), 1)

  solution <- tryCatch(
    solve(A, b),
    error = function(e) rep(NA, nrow(M))
  )

  names(solution) <- rownames(M)

  # Remove tiny numerical errors
  solution[abs(solution) < 1e-10] <- 0

  solution
}


# ------------------------------------------------------------
# find_absorbing_states(M)
#
# A state i is absorbing when P(i,i) = 1 (equivalently, every
# other entry in row i is 0).
# ------------------------------------------------------------

find_absorbing_states <- function(M) {

  absorbing <- character(0)

  for (i in seq_len(nrow(M))) {

    if (
      abs(M[i, i] - 1) < 1e-10 &&
      sum(M[i, -i]) < 1e-10
    ) {

      absorbing <- c(
        absorbing,
        rownames(M)[i]
      )
    }
  }

  absorbing
}


# ------------------------------------------------------------
# expected_time_to_absorption(M)
#
# Uses the fundamental matrix of an absorbing Markov chain.
# For transient states, N = (I - Q)^-1, where Q is the
# sub-matrix of transition probabilities between transient
# states only. N %*% 1 gives the expected number of periods
# spent before absorption, starting from each transient state.
# ------------------------------------------------------------

expected_time_to_absorption <- function(M) {

  absorbing <- find_absorbing_states(M)
  transient <- setdiff(rownames(M), absorbing)

  if (length(transient) == 0) {
    return(numeric(0))
  }

  Q <- M[transient, transient, drop = FALSE]

  I <- diag(length(transient))

  N <- tryCatch(
    solve(I - Q),
    error = function(e) matrix(NA, nrow(Q), ncol(Q))
  )

  t_vec <- as.numeric(N %*% rep(1, length(transient)))

  names(t_vec) <- transient

  t_vec
}


# ------------------------------------------------------------
# format_matrix(M)
# ------------------------------------------------------------

format_matrix <- function(M) {

  data.frame(
    State = rownames(M),
    round(M, 4),
    check.names = FALSE,
    row.names = NULL
  )
}


# ------------------------------------------------------------
# project_portfolio(M, n, initial_dist)
#
# pi_n = pi_0 P^n
# ------------------------------------------------------------

project_portfolio <- function(
    M,
    n,
    initial_dist = initial_distribution
) {

  Pn <- matrix_power(M, n)

  distribution <- as.numeric(
    initial_dist %*% Pn
  )

  names(distribution) <- states

  distribution
}


# ============================================================
# 3. DATA SIMULATION & ESTIMATION
#
# Demonstrates how, in practice, a transition matrix would be
# *estimated* from observed policyholder experience rather than
# assumed. We simulate a synthetic panel of policyholders under
# the assumed base_matrix, then re-estimate the matrix from the
# simulated data using the standard maximum-likelihood estimator
# for a time-homogeneous discrete-time Markov chain:
#
#     p_hat(i,j) = N(i,j) / N(i,.)
#
# i.e. the observed count of i -> j transitions divided by the
# total number of transitions out of state i.
# ============================================================

# ------------------------------------------------------------
# simulate_policyholder_paths(M, initial_dist, n_policyholders,
#                              n_periods, seed)
#
# Simulates a synthetic policyholder panel under transition
# matrix M and returns it in long format:
# policyholder | period | state
# ------------------------------------------------------------

simulate_policyholder_paths <- function(
    M,
    initial_dist = initial_distribution,
    n_policyholders = 1000,
    n_periods = 10,
    seed = 123
) {

  set.seed(seed)

  state_names <- rownames(M)
  n_states <- length(state_names)

  path_matrix <- matrix(
    NA_character_,
    nrow = n_policyholders,
    ncol = n_periods + 1
  )

  start_idx <- sample(
    seq_len(n_states),
    n_policyholders,
    replace = TRUE,
    prob = initial_dist
  )

  path_matrix[, 1] <- state_names[start_idx]

  current_idx <- start_idx

  for (t in seq_len(n_periods)) {

    for (p in seq_len(n_policyholders)) {

      current_idx[p] <- sample(
        seq_len(n_states),
        1,
        prob = M[current_idx[p], ]
      )
    }

    path_matrix[, t + 1] <- state_names[current_idx]
  }

  data.frame(
    policyholder = rep(seq_len(n_policyholders), times = n_periods + 1),
    period = rep(0:n_periods, each = n_policyholders),
    state = as.vector(path_matrix),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# estimate_transition_matrix(sim_data, state_names)
#
# Counts observed i -> j transitions between consecutive
# periods for every policyholder, then row-normalises to get
# the MLE transition matrix.
# ------------------------------------------------------------

estimate_transition_matrix <- function(sim_data, state_names = states) {

  ordered_data <- sim_data[
    order(sim_data$policyholder, sim_data$period),
  ]

  n_states <- length(state_names)

  counts <- matrix(
    0,
    nrow = n_states,
    ncol = n_states,
    dimnames = list(state_names, state_names)
  )

  by_policyholder <- split(ordered_data$state, ordered_data$policyholder)

  for (path in by_policyholder) {

    if (length(path) < 2) next

    from_states <- path[-length(path)]
    to_states <- path[-1]

    for (i in seq_along(from_states)) {
      counts[from_states[i], to_states[i]] <-
        counts[from_states[i], to_states[i]] + 1
    }
  }

  row_totals <- rowSums(counts)

  estimated <- counts / ifelse(row_totals == 0, 1, row_totals)

  estimated
}
