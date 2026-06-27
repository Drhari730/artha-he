# =============================================================================
# fct_economics.R : core health-economics calculations (engine, no UI)
# These functions are pure: data in -> numbers out. Easy to unit-test.
# =============================================================================

#' Discount a future value (or vector of yearly values) to present value.
#'
#' @param value Numeric vector of cash flows, one per period.
#' @param rate  Annual discount rate (e.g. 0.03 for 3%).
#' @param start Period offset for the first element (0 = incurred now).
#' @return Numeric vector of discounted values (same length as `value`).
he_discount <- function(value, rate = 0.03, start = 0) {
  periods <- seq_along(value) - 1 + start
  value / (1 + rate)^periods
}

#' Inflate a cost from its base year to a target year using an index.
#'
#' @param cost        Numeric cost in `from_year` prices.
#' @param from_year   Year the cost was measured.
#' @param to_year     Year to express the cost in.
#' @param index       Named numeric vector of price indices keyed by year
#'                    (e.g. CPI). If NULL, a flat annual rate is used instead.
#' @param annual_rate Fallback annual inflation rate when `index` is NULL.
he_inflate <- function(cost, from_year, to_year, index = NULL, annual_rate = 0.05) {
  if (!is.null(index)) {
    stopifnot(as.character(from_year) %in% names(index),
              as.character(to_year)   %in% names(index))
    cost * index[[as.character(to_year)]] / index[[as.character(from_year)]]
  } else {
    cost * (1 + annual_rate)^(to_year - from_year)
  }
}

#' Convert a cost between currencies using purchasing power parity.
#'
#' @param cost      Numeric cost in the source currency.
#' @param ppp_factor PPP conversion factor (source units per international $).
he_ppp <- function(cost, ppp_factor) cost / ppp_factor

#' Incremental cost-effectiveness ratio between two strategies.
#'
#' @param cost_a,effect_a Cost and effect (e.g. QALYs) of strategy A.
#' @param cost_b,effect_b Cost and effect of the comparator B.
#' @return ICER = (cost_a - cost_b) / (effect_a - effect_b).
he_icer <- function(cost_a, effect_a, cost_b, effect_b) {
  (cost_a - cost_b) / (effect_a - effect_b)
}

#' Full incremental analysis with dominance ranking.
#'
#' @param df Data frame with columns: strategy, cost, effect.
#' @return Data frame ordered by cost, with incremental costs/effects, ICERs,
#'         and a `status` flag (dominated / extended dominance / on frontier).
he_incremental <- function(df) {
  stopifnot(all(c("strategy", "cost", "effect") %in% names(df)))
  d <- df[order(df$cost), , drop = FALSE]
  d$status <- "on frontier"

  # Strong (simple) dominance: more cost, less or equal effect than a cheaper option
  for (i in seq_len(nrow(d))) {
    cheaper <- d[d$cost < d$cost[i], , drop = FALSE]
    if (nrow(cheaper) && any(cheaper$effect >= d$effect[i])) d$status[i] <- "dominated"
  }

  frontier <- d[d$status != "dominated", , drop = FALSE]
  frontier <- frontier[order(frontier$cost), , drop = FALSE]
  frontier$inc_cost   <- c(NA, diff(frontier$cost))
  frontier$inc_effect <- c(NA, diff(frontier$effect))
  frontier$icer       <- frontier$inc_cost / frontier$inc_effect

  # Extended dominance: an ICER that is higher than the option above it
  if (nrow(frontier) > 2) {
    for (i in 3:nrow(frontier)) {
      if (!is.na(frontier$icer[i]) && !is.na(frontier$icer[i - 1]) &&
          frontier$icer[i] < frontier$icer[i - 1]) {
        frontier$status[i - 1] <- "extended dominance"
      }
    }
  }

  merge(d[, c("strategy", "cost", "effect", "status")],
        frontier[, c("strategy", "inc_cost", "inc_effect", "icer")],
        by = "strategy", all.x = TRUE) |>
    (\(x) x[order(x$cost), ])()
}

#' Net monetary benefit. NMB = effect * wtp - cost.
he_nmb <- function(cost, effect, wtp) effect * wtp - cost
