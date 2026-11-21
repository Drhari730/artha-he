# plumber.R
library(plumber)
library(jsonlite)
source("engine.R")

# Indian digit grouping: 12,34,567
fmtINR <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  d <- formatC(abs(round(x)), format = "f", digits = 0)
  if (nchar(d) > 3) {
    head <- substr(d, 1, nchar(d) - 3); tail <- substr(d, nchar(d) - 2, nchar(d))
    d <- paste0(gsub("(\\d)(?=(\\d{2})+$)", "\\1,", head, perl = TRUE), ",", tail)
  }
  paste0(if (x < 0) "-" else "", "\u20B9", d)
}
fmtNum <- function(x, dp=2) formatC(x, format="f", big.mark=",", digits=dp)
pct <- function(x, dp=1) paste0(formatC(x * 100, format="f", digits=dp), "%")
# HTML-escape user-supplied text before it goes into the report.
esc <- function(x) {
  x <- as.character(if (is.null(x)) "" else x)
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE); x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}
DEFAULT_SEED <- 20251121L

# Strategy indices for PSA/DSA (0-based in the API), validated.
pick_pair <- function(ref, cmp, nStrat) {
  if (nStrat < 2) input_error("Sensitivity analysis needs at least two strategies in the model.")
  iRef <- as.integer(num(ref, 0)); iCmp <- as.integer(num(cmp, 1))
  if (iRef < 0 || iRef >= nStrat || iCmp < 0 || iCmp >= nStrat) input_error("Choose a valid reference and comparator strategy.")
  if (iRef == iCmp) input_error("Choose two different strategies to compare.")
  c(iRef, iCmp)
}

#* @assets ./public /
list()

parse_req <- function(req) {
  if (!is.null(req$bodyRaw)) {
    return(jsonlite::fromJSON(rawToChar(req$bodyRaw), simplifyVector = FALSE))
  }
  if (is.character(req$postBody)) {
    return(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE))
  }
  return(req$body)
}

#* @serializer unboxedJSON
#* @filter cors
function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  res$setHeader("Cache-Control", "no-cache, no-store, must-revalidate")
  plumber::forward()
}

#* @options /api/costing
#* @options /api/oop
#* @options /api/evaluation
#* @options /api/model
#* @options /api/sensitivity
#* @options /api/bia
function() {}

#* @serializer unboxedJSON
#* @post /api/costing
function(req) {
  c <- parse_req(req)
  if (!is.null(c$method) && c$method == "gross") {
    per <- if(!is.null(c$output) && as.numeric(c$output)>0) as.numeric(c$totalCost) / as.numeric(c$output) else 0
    return(list(method = "gross", total = as.numeric(c$totalCost), output = as.numeric(c$output), per = per))
  }
  if (!is.null(c$method) && c$method == "advanced") {
    return(advancedCost(c$adv))
  }
  r <- microCost(c$rows, as.numeric(c$toYear), as.numeric(c$inflation))
  list(method = "micro", total = r$total, byCat = r$byCat, lines = r$lines, toYear = as.numeric(c$toYear), inflation = as.numeric(c$inflation))
}

#* @serializer unboxedJSON
#* @post /api/oop
function(req) {
  o <- parse_req(req)
  oopRun(o)
}

#* @serializer unboxedJSON
#* @post /api/evaluation
function(req) {
  e <- parse_req(req)
  if (is.null(e$type) || !(e$type %in% c("CMA","CEA","CUA","CBA","CCA"))) input_error("Unknown evaluation type.")
  if (length(e$strats) < 1) input_error("Add at least one option to evaluate.")
  reqs <- evalRequirements(e$type, e$strats)
  
  if (e$type == "CBA") {
    d <- lapply(e$strats, function(s) { list(strategy=s$strategy, cost=num(s$cost), benefit=num(s$effect), net=num(s$effect)-num(s$cost), bcr=if(num(s$cost)>0) num(s$effect)/num(s$cost) else NA) })
    d <- d[order(sapply(d, function(x) -x$net))]
    interp <- list(
      paste0(d[[1]]$strategy, " gives the highest net monetary benefit (", fmtINR(d[[1]]$net), "), with a benefit-cost ratio of ", fmtNum(d[[1]]$bcr, 2), "."),
      "A benefit-cost ratio above 1 means monetised benefits exceed costs; in the chart, taller bars indicate better value for money.",
      "Cost-benefit needs outcomes credibly valued in money (e.g. willingness-to-pay, human-capital, or productivity methods) - state your valuation method and source when reporting."
    )
    return(list(type=e$type, requirements=reqs, rows=d, best=d[[1]], interpretation=interp))
  }
  if (e$type == "CMA") {
    d <- lapply(e$strats, function(s) { list(strategy=s$strategy, cost=num(s$cost), effect=num(s$effect)) })
    d <- d[order(sapply(d, function(x) x$cost))]
    eff <- vapply(e$strats, function(s) num(s$effect), numeric(1))
    equal <- mean(eff) == 0 || (max(eff) - min(eff)) / abs(mean(eff)) < 0.03
    interp <- list(
      paste0(d[[1]]$strategy, " is the least-costly option (", fmtINR(d[[1]]$cost), ") and is recommended - cost-minimisation assumes the options achieve equivalent outcomes."),
      if(equal) "The outcomes entered are effectively equal, so choosing on cost alone is appropriate here." else "The outcomes entered are NOT equal - cost-minimisation may be inappropriate; consider a CEA or CUA instead.",
      "When reporting, document the evidence that outcomes are equivalent (e.g. a non-inferiority trial or equivalence study)."
    )
    return(list(type=e$type, requirements=reqs, rows=d, best=d[[1]], equal=equal, interpretation=interp))
  }
  if (e$type == "CCA") {
    d <- lapply(e$strats, function(s) { list(strategy=s$strategy, cost=num(s$cost), effect=num(s$effect)) })
    d <- d[order(sapply(d, function(x) x$cost))]
    interp <- list(
      "Cost-consequence lays out the cost and the outcome(s) of each option side by side, without combining them into a single ratio.",
      "Compare each option's cost against its outcomes and judge which trade-off is acceptable for your population and budget.",
      "This is well suited to public-health programmes and policies where outcomes are multiple or not easily converted to QALYs."
    )
    return(list(type=e$type, requirements=reqs, rows=d, interpretation=interp))
  }
  
  if (length(e$strats) < 1) input_error("Add at least one option to evaluate.")
  wtp <- num(e$wtp)
  d <- icerIncremental(e$strats)
  ref <- d[[1]]
  d <- lapply(d, function(s) { s$nmb <- nmb(s$cost, s$effect, wtp); s })
  onFr <- Filter(function(s) s$status == "frontier", d)
  best <- d[[best_index(vapply(d, function(s) s$cost, numeric(1)), vapply(d, function(s) s$effect, numeric(1)), wtp)]]

  plane <- lapply(d, function(s) { list(label=s$strategy, dEff=s$effect - ref$effect, dCost=s$cost - ref$cost, ref=identical(s$strategy, ref$strategy)) })
  unit <- if(e$type == "CUA") "QALY" else "unit"
  nDom <- sum(vapply(d, function(s) s$status != "frontier", logical(1)))

  interp <- list(
    paste0("At a willingness-to-pay of ", fmtINR(wtp), " per ", unit, ", ", best$strategy, " is the cost-effective choice (highest net monetary benefit)", if(is.na(best$icer)) " - it is the least-costly option on the efficiency frontier" else paste0(", with an ICER of ", fmtINR(best$icer), " per ", unit, " versus the next option on the frontier"), "."),
    "On the cost-effectiveness plane, options toward the lower-right are better value; any point below the dashed line is cost-effective at this threshold.",
    paste0(if(nDom > 0) paste0(nDom, " option(s) are dominated (strongly or extendedly) and excluded; ICERs are recomputed against the next non-dominated option. ") else "", "The conclusion depends on the threshold and the cost/effect inputs - test their uncertainty in the Sensitivity tab.")
  )
  list(type=e$type, requirements=reqs, rows=d, plane=plane, best=best, wtp=wtp, unit=unit, interpretation=interp)
}

#* @serializer unboxedJSON
#* @post /api/model
function(req) {
  m <- parse_req(req)
  fixed <- fixModel(m)
  wtp <- num(m$wtp)
  isQ <- !identical(m$outcome, "DALY")
  res <- modelRunAll(fixed)
  res <- lapply(res, function(s) { s$eff <- if(isQ) s$qaly else s$daly; s })
  inc <- modelIncremental(res, !isQ)
  ref <- inc[[1]]
  unit <- if(isQ) "QALY" else "DALY averted"

  onFr <- Filter(function(s) s$status == "frontier", inc)
  best <- inc[[best_index(vapply(inc, function(s) s$cost, numeric(1)), vapply(inc, function(s) if (isQ) s$eff else -s$eff, numeric(1)), wtp)]]

  plane <- lapply(inc, function(s) { list(label=s$name, dEff=if(!isQ) ref$daly - s$daly else s$qaly - ref$qaly, dCost=s$cost - ref$cost, ref=identical(s$name, ref$name)) })

  as_idx <- if(!is.null(m$activeStrat)) as.numeric(m$activeStrat) + 1 else 1
  if (as_idx < 1 || as_idx > length(fixed$strategies)) as_idx <- 1
  as_strat <- fixed$strategies[[as_idx]]
  trace <- markovGeneric(fixed, as_strat)$trace

  series <- lapply(seq_along(m$states), function(si) {
    list(label=m$states[[si]]$name, idx=si-1, data=lapply(1:nrow(trace), function(t) { list(x=(t-1)*as.numeric(m$cycle), y=trace[t, si]) }))
  })
  
  ce <- is.na(best$icer) || best$icer <= wtp
  interp <- list(
    paste0("Over a ", m$horizon, "-year horizon, ", best$name, " is optimal (highest net monetary benefit) at a willingness-to-pay of ", fmtINR(wtp), " per ", unit, if(is.na(best$icer)) " (the least-costly strategy on the frontier)" else paste0(", with an ICER of ", fmtINR(best$icer), " per ", unit), " - ", if(ce) "cost-effective" else "not cost-effective", " at this threshold."),
    paste0("The cohort trace shows how the modelled population moves between the ", length(m$states), " health states over time; the cost-effectiveness plane summarises each strategy against the cheapest."),
    "These are deterministic (point-estimate) results - run the Sensitivity tab (PSA) to see how parameter uncertainty changes the probability of cost-effectiveness."
  )
  
  list(rows=inc, plane=plane, best=best, unit=unit, onFr=length(onFr), strategies=length(m$strategies), states=length(m$states), activeName=as_strat$name, series=series, stateNames=sapply(m$states, function(s) s$name), wtp=wtp, horizon=as.numeric(m$horizon), dCost=as.numeric(m$dCost), outcome=m$outcome, interpretation=interp)
}

#* @serializer unboxedJSON
#* @post /api/sensitivity
function(req) {
  p <- parse_req(req)
  if (is.null(p$model)) input_error("No model supplied.")
  N <- as.integer(num(p$N, 1000))
  pr <- pick_pair(p$ref, p$cmp, length(p$model$strategies)); iRef <- pr[1]; iCmp <- pr[2]
  wtp <- num(p$wtp)
  cv <- if(num(p$cv) > 0) num(p$cv) else 0.2
  seed <- as.integer(num(p$seed, DEFAULT_SEED))

  draws <- psaModel(p$model, N, iRef, iCmp, cv, p[["mode"]], p[["params"]], seed = seed)
  hiW <- max(1000000, 3 * wtp)
  curve <- ceac(draws, seq(0, hiW, length.out = 41))
  pCE <- mean(vapply(draws, function(x) nmb(x$incCost, x$incEff, wtp) > 0, logical(1)))
  ev <- evpi(draws, wtp)
  tor <- dsaModel(p$model, wtp, iRef, iCmp)

  list(pCE = pCE, evpi = ev, ceac = curve, draws = draws, tornado = tor, N = N, wtp = wtp, cv = cv, seed = seed, unit = if(identical(p$model$outcome, "DALY")) "DALY averted" else "QALY", refName = p$model$strategies[[iRef+1]]$name, cmpName = p$model$strategies[[iCmp+1]]$name)
}

#* @serializer unboxedJSON
#* @post /api/bia
function(req) {
  b <- parse_req(req)
  rows <- biaRun(b)
  list(rows = rows, cumulative = rows[[length(rows)]]$cum, peak = max(sapply(rows, function(r) r$impact)), eligible = as.numeric(b$population)*as.numeric(b$eligible), population = as.numeric(b$population), horizon = as.numeric(b$horizon), startYear = as.numeric(b$startYear))
}

#* @serializer unboxedJSON
#* @options /api/report
#* @post /api/report
function(req) {
  st <- parse_req(req)
  dt <- format(Sys.Date(), "%d %B %Y")
  tbl <- function(head, rows) {
    h <- paste0("<tr>", paste0("<th>", head, "</th>", collapse=""), "</tr>")
    b <- paste0(vapply(rows, function(r) paste0("<tr>", paste0("<td>", esc(r), "</td>", collapse=""), "</tr>"), character(1)), collapse="")
    paste0("<table>", h, b, "</table>")
  }
  # 1 Costing
  cst <- st$costing
  if (!is.null(cst$method) && cst$method == "gross") {
    per <- if (as.numeric(cst$output) > 0) as.numeric(cst$totalCost)/as.numeric(cst$output) else 0
    costSec <- paste0("<p>Gross (top-down) costing.</p>", tbl(c("Metric","Value"), list(c("Total cost", fmtINR(as.numeric(cst$totalCost))), c("Output units", fmtNum(as.numeric(cst$output),0)), c("Cost per unit", fmtINR(per)))))
  } else if (!is.null(cst$method) && cst$method == "advanced") {
    ac <- advancedCost(cst$adv)
    costSec <- paste0("<p>Advanced (activity-based) costing. <b>Total: ", fmtINR(ac$total), "</b> (", fmtINR(ac$perUnit), " per unit).</p>", tbl(c("Component","Amount","Share"), lapply(ac$breakdown, function(x) c(x$component, fmtINR(x$amount), pct(x$share)))))
  } else {
    r <- microCost(cst$rows, as.numeric(cst$toYear), as.numeric(cst$inflation))
    costSec <- paste0("<p>Micro (bottom-up) costing, ", esc(cst$toYear), " prices. <b>Total cost: ", fmtINR(r$total), "</b>.</p>", tbl(c("Category","Cost","Share"), lapply(r$byCat, function(x) c(x$category, fmtINR(x$cost), pct(x$share)))))
  }
  # 2 OOP
  o <- oopRun(st$oop)
  oopSec <- paste0("<p>Total out-of-pocket: <b>", fmtINR(o$total), "</b>, of which direct medical (health) payments ", fmtINR(o$health), " = ", pct(o$pctInc), " of household consumption/income. Catastrophic at 10%: <b>", if(o$che10) "Yes" else "No", "</b>; at 40% capacity-to-pay: <b>", if(o$che40) "Yes" else "No", "</b> (SDG 3.8.2 / WHO: health payments only).</p>")
  # 3 Evaluation
  e <- st$evaluation
  if (e$type == "CEA" || e$type == "CUA") {
    inc <- icerIncremental(e$strats)
    evalSec <- paste0("<p>", esc(e$type), ", WTP ", fmtINR(num(e$wtp)), ".</p>", tbl(c("Strategy","Cost","Effect","ICER","Status"), lapply(inc, function(s) c(s$strategy, fmtINR(s$cost), fmtNum(s$effect,2), if(is.na(s$icer)) "-" else fmtINR(s$icer), s$status))))
  } else {
    evalSec <- paste0("<p>", esc(e$type), " analysis.</p>", tbl(c("Strategy","Cost","Outcome"), lapply(e$strats, function(s) c(s$strategy, fmtINR(as.numeric(s$cost)), fmtNum(as.numeric(s$effect),2)))))
  }
  # 4 Model
  m <- st$model
  isQ <- !identical(m$outcome, "DALY")
  res <- modelRunAll(m)
  res <- lapply(res, function(s){ s$eff <- if(isQ) s$qaly else s$daly; s })
  minc <- modelIncremental(res, !isQ)
  unit <- if(isQ) "QALY" else "DALY averted"
  modelSec <- paste0("<p>", length(m$states), "-state Markov, ", length(m$strategies), " strategies, ", esc(m$horizon), "-year horizon, ", pct(num(m$dCost),0), " discounting. Outcome: ", if (isQ) "QALY" else "DALY", ".</p>", tbl(c("Strategy","Cost","Life-years","QALYs","DALYs","ICER","Status"), lapply(minc, function(s) c(s$name, fmtINR(s$cost), fmtNum(s$ly,3), fmtNum(s$qaly,3), fmtNum(s$daly,3), if(is.na(s$icer)) "-" else fmtINR(s$icer), s$status))))
  # 5 PSA
  if (length(m$strategies) >= 2) {
    pr <- tryCatch(pick_pair(st$sens$ref, st$sens$cmp, length(m$strategies)), error = function(e) c(0L, 1L))
    wtp <- num(st$sens$wtp); cvv <- if(num(st$sens$cv) > 0) num(st$sens$cv) else 0.2
    nIt <- as.integer(min(max(num(st$sens$N, 1000), 1), 2000)); seed <- as.integer(num(st$sens$seed, DEFAULT_SEED))
    draws <- psaModel(m, nIt, pr[1], pr[2], cvv, st$sens[["mode"]], st$sens[["params"]], seed = seed)
    pCE <- mean(vapply(draws, function(x) nmb(x$incCost, x$incEff, wtp) > 0, logical(1)))
    ev <- evpi(draws, wtp)
    psaSec <- paste0("<p>PSA (", nIt, " iterations, seed ", seed, "). Probability cost-effective at ", fmtINR(wtp), ": <b>", pct(pCE), "</b>. EVPI per patient: <b>", fmtINR(ev), "</b>.</p>")
  } else psaSec <- "<p>PSA not run (needs at least two strategies).</p>"
  # 6 BIA
  br <- biaRun(st$bia)
  biaSec <- paste0("<p>Budget impact over ", esc(st$bia$horizon), " years. Cumulative net impact: <b>", fmtINR(br[[length(br)]]$cum), "</b>.</p>", tbl(c("Year","Uptake","Net impact","Cumulative"), lapply(br, function(r) c(r$year, pct(r$uptake), fmtINR(r$impact), fmtINR(r$cum)))))

  html <- paste0(
    "<h1>Artha HE - Health Economic Analysis Report</h1>",
    "<p class='meta'>Generated ", dt, " . India reference case. Reporting structured to the CHEERS 2022 checklist.</p>",
    "<h2>1 . Costing</h2>", costSec,
    "<h2>2 . Out-of-pocket &amp; catastrophic expenditure</h2>", oopSec,
    "<h2>3 . Economic evaluation</h2>", evalSec,
    "<h2>4 . Decision-analytic model</h2>", modelSec,
    "<h2>5 . Sensitivity analysis</h2>", psaSec,
    "<h2>6 . Budget impact</h2>", biaSec,
    "<p class='foot'>Generated by Artha HE . Developed by Dr G Hari Prakash . for research &amp; teaching. Verify inputs before formal use.</p>"
  )
  list(html = html)
}

#* @serializer unboxedJSON
#* @options /api/validate
#* @post /api/validate
function(req) {
  payload <- parse_req(req)
  v <- list()
  ap <- function(a, b, t = 0.01) abs(a - b) <= t * max(1, abs(b))
  
  d <- 1000 / (1.03 ^ 5)
  v[[1]] <- list(n = "Discount ₹1,000 at 3% for 5 years", got = fmtNum(d, 2), exp = "862.61", ok = ap(d, 862.6088))
  
  ic <- (85000 - 40000) / (4.4 - 3.5)
  v[[2]] <- list(n = "ICER of (₹85k, 4.4) vs (₹40k, 3.5)", got = fmtINR(ic), exp = "₹50,000", ok = ap(ic, 50000))
  
  nb <- nmb(85000, 4.4, 200000)
  v[[3]] <- list(n = "NMB at WTP ₹2,00,000 (cost 85k, 4.4 QALY)", got = fmtINR(nb), exp = "₹7,95,000", ok = ap(nb, 795000))
  
  strats <- list(list(strategy="A", cost=40000, effect=3.5), list(strategy="B", cost=50000, effect=3.2))
  inc <- icerIncremental(strats)
  dom <- Filter(function(x) x$strategy == "B", inc)[[1]]$status
  v[[4]] <- list(n = "Costs more & less effective -> flagged dominated", got = dom, exp = "dominated", ok = dom == "dominated")
  
  o <- oopRun(list(income = 200000, nonFood = 120000, items = list(list(item="x", category="Direct medical", amount=47000))))
  v[[5]] <- list(n = "OOP ₹47,000 / income ₹2,00,000", got = pct(o$pctInc), exp = "23.5%", ok = ap(o$pctInc * 100, 23.5))
  
  if (!is.null(payload$model)) {
    fixed <- fixModel(payload$model)
    arm <- markovGeneric(fixed, fixed$strategies[[1]])
    last_row <- arm$trace[nrow(arm$trace), ]
    mass <- sum(last_row)
    v[[6]] <- list(n = "Markov cohort mass conserved (Σ states = 1)", got = fmtNum(mass, 6), exp = "1.000000", ok = abs(mass - 1) < 1e-9)
  }

  mc <- microCost(list(list(item="a", category="Direct medical", quantity=2, unit_cost=100, year=2024), list(item="b", category="Direct medical", quantity=3, unit_cost=50, year=2024)), 2024, 0)
  v[[length(v)+1]] <- list(n = "Micro-cost: 2×₹100 + 3×₹50", got = fmtINR(mc$total), exp = "₹350", ok = ap(mc$total, 350))
  
  # Extended dominance: B is beaten by a mix of A and C; C's ICER must be vs A.
  ext <- icerIncremental(list(list(strategy="A", cost=0, effect=0), list(strategy="B", cost=100, effect=0.5), list(strategy="C", cost=150, effect=1.5)))
  stB <- Filter(function(x) x$strategy == "B", ext)[[1]]$status
  icC <- Filter(function(x) x$strategy == "C", ext)[[1]]$icer
  v[[length(v)+1]] <- list(n = "Extended dominance: B removed, ICER of C vs A", got = paste0(stB, ", ", fmtINR(icC)), exp = "extended, \u20B9100", ok = stB == "extended" && ap(icC, 100))

  # Closed-form Markov check: 2 states, p(death)=0.1/yr, 10 yrs, utility 1, 3% discount.
  two <- list(states = list(list(name="Alive", cost=1000, util=1), list(name="Dead", cost=0, util=0, absorbing=TRUE)),
              strategies = list(list(name="x", addCost=0, matrix=list(list(0.9, 0.1), list(0, 1)))),
              cycle=1, horizon=10, dCost=0.03, dEff=0.03, lifeExp=0, outcome="QALY")
  rr <- 0.9/1.03; qExp <- 0.95 * (1 - rr^10) / (1 - rr)
  q2 <- modelRunAll(two)[[1]]$qaly
  v[[length(v)+1]] <- list(n = "Markov QALYs = closed-form geometric series (half-cycle, 3%)", got = fmtNum(q2, 6), exp = fmtNum(qExp, 6), ok = abs(q2 - qExp) < 1e-9)

  # PSA keeps absorbing states absorbing (no resurrection from Dead).
  set.seed(1); dr <- psaDraw(fixModel(two), 0.3, "global", NULL)$strategies[[1]]$matrix
  v[[length(v)+1]] <- list(n = "PSA sampling keeps Dead -> Dead = 1", got = fmtNum(dr[2,2], 6), exp = "1.000000", ok = dr[2,2] == 1 && dr[2,1] == 0)

  # CHE uses health payments only.
  o2 <- oopRun(list(income = 100000, items = list(list(category="Direct medical", amount=8000), list(category="Direct non-medical", amount=5000))))
  v[[length(v)+1]] <- list(n = "CHE ratio counts only direct medical payments", got = pct(o2$pctInc), exp = "8.0%", ok = ap(o2$pctInc * 100, 8))

  list(rows = v, allok = all(vapply(v, function(x) isTRUE(x$ok), logical(1))))
}
