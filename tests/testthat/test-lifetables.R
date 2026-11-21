srs_path <- function() { for (p in c("../../public/data/srs_life_tables_2020_24.json", "public/data/srs_life_tables_2020_24.json")) if (file.exists(p)) return(p); NULL }

# A cohort that faces only background (all-cause) mortality should live, on
# average, the SRS life expectancy at its starting age.
le_from_model <- function(tab, startAge) {
  m <- list(states = list(list(name = "Alive", cost = 0, util = 1), list(name = "Dead", cost = 0, util = 0, absorbing = TRUE)),
            strategies = list(list(name = "x", addCost = 0, matrix = list(list(1, 0), list(0, 1)))),
            cycle = 0.25, horizon = 120 - startAge, dCost = 0, dEff = 0, lifeExp = 0, outcome = "QALY",
            bgMortality = TRUE, startAge = startAge, lifeTable = tab)
  modelRunAll(m)[[1]]$undisc$ly
}

test_that("all 207 SRS 2020-24 tables are complete and internally consistent", {
  p <- srs_path(); skip_if(is.null(p))
  srs <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  expect_equal(length(srs$regions), 23)
  n <- 0
  for (reg in srs$regions) for (res in reg) for (tab in res) {
    n <- n + 1
    expect_length(tab, 19)
    for (i in 1:18) expect_lt(abs(tab[[i]]$lx * (1 - tab[[i]]$nqx) - tab[[i + 1]]$lx), max(3, 5e-4 * tab[[i]]$lx))
  }
  expect_equal(n, 207)
  ind <- srs$regions$India$Total
  expect_equal(c(ind$Total[[1]]$ex, ind$Male[[1]]$ex, ind$Female[[1]]$ex), c(70.6, 68.7, 72.8))
  expect_equal(c(ind$Total[[14]]$ex, ind$Male[[14]]$ex, ind$Female[[14]]$ex), c(18.6, 17.5, 19.8))
})

test_that("abridged-to-annual conversion", {
  r <- abridged_to_annual(list(list(age = 5, n = 5, nqx = 0.05), list(age = 85, n = NULL, nqx = NULL, lx = 20000, nLx = 140000)))
  expect_equal(r[[1]]$q, 1 - 0.95^(1 / 5))
  expect_equal(r[[2]]$q, 1 - exp(-20000 / 140000))
})

test_that("the model reproduces SRS life expectancy (India, by sex, at birth and at 60)", {
  p <- srs_path(); skip_if(is.null(p))
  srs <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  for (sex in c("Total", "Male", "Female")) {
    tab <- srs$regions$India$Total[[sex]]
    expect_equal(le_from_model(tab, 0), tab[[1]]$ex, tolerance = 0.3, scale = 1)
    expect_equal(le_from_model(tab, 60), tab[[14]]$ex, tolerance = 0.3, scale = 1)
  }
  for (reg in c("Kerala", "Chhattisgarh")) {
    tab <- srs$regions[[reg]]$Total$Total
    expect_equal(le_from_model(tab, 0), tab[[1]]$ex, tolerance = 0.3, scale = 1)
  }
})
