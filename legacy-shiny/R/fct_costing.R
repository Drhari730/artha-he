# =============================================================================
# fct_costing.R : micro-costing and gross-costing calculations (engine)
# =============================================================================

#' Micro-costing (bottom-up): cost each resource as quantity x unit cost,
#' then optionally adjust for inflation and discounting, and roll up by group.
#'
#' Expected columns in `df`:
#'   item       - resource name (character)
#'   category   - one of COST_CATEGORIES (character, optional)
#'   quantity   - units consumed (numeric)
#'   unit_cost  - cost per unit (numeric)
#'   year       - price year of unit_cost (numeric, optional)
#'
#' @param df          Resource-use data frame.
#' @param to_year     Target price year (inflate all items to this).
#' @param inflation   Annual inflation rate used when `year` differs.
#' @param discount    Annual discount rate (applied via `period` if present).
#' @return List with `lines` (per-item costs) and `by_category` summary.
micro_cost <- function(df, to_year = NULL, inflation = 0.05, discount = 0) {
  stopifnot(all(c("quantity", "unit_cost") %in% names(df)))
  d <- df
  if (!"category" %in% names(d)) d$category <- "Direct medical"
  if (!"item" %in% names(d))     d$item     <- paste0("item_", seq_len(nrow(d)))

  d$unit_cost_adj <- d$unit_cost
  if (!is.null(to_year) && "year" %in% names(d)) {
    d$unit_cost_adj <- mapply(
      function(c, y) if (is.na(y)) c else he_inflate(c, y, to_year, annual_rate = inflation),
      d$unit_cost, d$year
    )
  }

  d$line_cost <- d$quantity * d$unit_cost_adj

  if (discount > 0 && "period" %in% names(d)) {
    d$line_cost <- mapply(function(v, p) he_discount(v, discount, start = p)[1],
                          d$line_cost, d$period)
  }

  by_cat <- d |>
    group_by(category) |>
    summarise(cost = sum(line_cost, na.rm = TRUE), .groups = "drop") |>
    mutate(share = cost / sum(cost))

  list(lines = d, by_category = by_cat, total = sum(d$line_cost, na.rm = TRUE))
}

#' Gross-costing (top-down): allocate a total budget across output units.
#'
#' @param total_cost Total expenditure of the cost centre.
#' @param output     Number of output units (e.g. patients treated, visits).
#' @return Average cost per output unit.
gross_cost <- function(total_cost, output) {
  stopifnot(output > 0)
  total_cost / output
}

#' A small example micro-costing dataset (used when no file is uploaded).
example_costing_data <- function() {
  data.frame(
    item      = c("Outpatient consultation", "HbA1c test", "Metformin (1 month)",
                  "Insulin (1 month)", "Nursing time (hour)", "Patient travel",
                  "Lost work day"),
    category  = c("Direct medical", "Direct medical", "Direct medical",
                  "Direct medical", "Direct medical", "Direct non-medical",
                  "Indirect (productivity)"),
    quantity  = c(4, 2, 12, 6, 3, 8, 5),
    unit_cost = c(300, 450, 120, 900, 250, 150, 700),
    year      = c(2022, 2022, 2023, 2023, 2022, 2024, 2024),
    stringsAsFactors = FALSE
  )
}
