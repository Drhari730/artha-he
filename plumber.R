# plumber.R
library(plumber)
library(jsonlite)
source("engine.R")

fmtINR <- function(x) paste0("\u20B9", formatC(x, format="f", big.mark=",", digits=0))
fmtNum <- function(x, dp=2) formatC(x, format="f", big.mark=",", digits=dp)
pct <- function(x, dp=1) paste0(formatC(x * 100, format="f", digits=dp), "%")

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
  reqs <- evalRequirements(e$type, e$strats)
  
  if (e$type == "CBA") {
    d <- lapply(e$strats, function(s) { list(strategy=s$strategy, cost=as.numeric(s$cost), benefit=as.numeric(s$effect), net=as.numeric(s$effect)-as.numeric(s$cost), bcr=if(as.numeric(s$cost)>0) as.numeric(s$effect)/as.numeric(s$cost) else 0) })
    d <- d[order(sapply(d, function(x) -x$net))]
    interp <- list(
      paste0(d[[1]]$strategy, " gives the highest net monetary benefit (", fmtINR(d[[1]]$net), "), with a benefit-cost ratio of ", fmtNum(d[[1]]$bcr, 2), "."),
      "A benefit-cost ratio above 1 means monetised benefits exceed costs; in the chart, taller bars indicate better value for money.",
      "Cost-benefit needs outcomes credibly valued in money (e.g. willingness-to-pay, human-capital, or productivity methods) - state your valuation method and source when reporting."
    )
    return(list(type=e$type, requirements=reqs, rows=d, best=d[[1]], interpretation=interp))
  }
  if (e$type == "CMA") {
    d <- lapply(e$strats, function(s) { list(strategy=s$strategy, cost=as.numeric(s$cost), effect=as.numeric(s$effect)) })
    d <- d[order(sapply(d, function(x) x$cost))]
    eff <- sapply(e$strats, function(s) as.numeric(s$effect))
    equal <- mean(eff) == 0 || (max(eff) - min(eff)) / abs(mean(eff)) < 0.03
    interp <- list(
      paste0(d[[1]]$strategy, " is the least-costly option (", fmtINR(d[[1]]$cost), ") and is recommended - cost-minimisation assumes the options achieve equivalent outcomes."),
      if(equal) "The outcomes entered are effectively equal, so choosing on cost alone is appropriate here." else "The outcomes entered are NOT equal - cost-minimisation may be inappropriate; consider a CEA or CUA instead.",
      "When reporting, document the evidence that outcomes are equivalent (e.g. a non-inferiority trial or equivalence study)."
    )
    return(list(type=e$type, requirements=reqs, rows=d, best=d[[1]], equal=equal, interpretation=interp))
  }
  if (e$type == "CCA") {
    d <- lapply(e$strats, function(s) { list(strategy=s$strategy, cost=as.numeric(s$cost), effect=as.numeric(s$effect)) })
    d <- d[order(sapply(d, function(x) x$cost))]
    interp <- list(
      "Cost-consequence lays out the cost and the outcome(s) of each option side by side, without combining them into a single ratio.",
      "Compare each option's cost against its outcomes and judge which trade-off is acceptable for your population and budget.",
      "This is well suited to public-health programmes and policies where outcomes are multiple or not easily converted to QALYs."
    )
    return(list(type=e$type, requirements=reqs, rows=d, interpretation=interp))
  }
  
  d <- icerIncremental(e$strats)
  ref <- d[[1]]
  d <- lapply(d, function(s) { s$nmb <- nmb(s$cost, s$effect, as.numeric(e$wtp)); s })
  onFr <- Filter(function(s) s$status == "frontier", d)
  
  best <- ref
  for (s in onFr) { if (is.na(s$icer) || s$icer <= as.numeric(e$wtp)) best <- s }
  
  plane <- lapply(d, function(s) { list(label=s$strategy, dEff=s$effect - ref$effect, dCost=s$cost - ref$cost, ref=s$strategy == ref$strategy) })
  unit <- if(e$type == "CUA") "QALY" else "unit"
  nDom <- sum(sapply(d, function(s) s$status == "dominated"))
  
  interp <- list(
    paste0("At a willingness-to-pay of ", fmtINR(as.numeric(e$wtp)), " per ", unit, ", ", best$strategy, " is the cost-effective choice", if(is.na(best$icer)) " (the least-costly option on the efficiency frontier)" else paste0(", with an ICER of ", fmtINR(best$icer), " per ", unit), "."),
    "On the cost-effectiveness plane, options toward the lower-right are better value; any point below the dashed line is cost-effective at this threshold.",
    paste0(if(nDom > 0) paste0(nDom, " option(s) are dominated and excluded. ") else "", "The conclusion depends on the threshold and the cost/effect inputs - test their uncertainty in the Sensitivity tab.")
  )
  list(type=e$type, requirements=reqs, rows=d, plane=plane, best=best, wtp=as.numeric(e$wtp), unit=unit, interpretation=interp)
}

#* @serializer unboxedJSON
#* @post /api/model
function(req) {
  m <- parse_req(req)
  fixed <- fixModel(m)
  res <- modelRunAll(fixed)
  res <- lapply(res, function(s) { s$eff <- if(m$outcome == "QALY") s$qaly else s$daly; s })
  inc <- modelIncremental(res, m$outcome == "DALY")
  ref <- inc[[1]]
  unit <- if(m$outcome == "QALY") "QALY" else "DALY averted"
  
  onFr <- Filter(function(s) s$status == "frontier", inc)
  best <- ref
  for (s in onFr) { if (is.na(s$icer) || s$icer <= as.numeric(m$wtp)) best <- s }
  
  plane <- lapply(inc, function(s) { list(label=s$name, dEff=if(m$outcome=="DALY") ref$daly - s$daly else s$qaly - ref$qaly, dCost=s$cost - ref$cost, ref=s$name == ref$name) })
  
  as_idx <- if(!is.null(m$activeStrat)) as.numeric(m$activeStrat) + 1 else 1
  if (as_idx < 1 || as_idx > length(fixed$strategies)) as_idx <- 1
  as_strat <- fixed$strategies[[as_idx]]
  trace <- markovGeneric(fixed, as_strat)$trace

  series <- lapply(seq_along(m$states), function(si) {
    list(label=m$states[[si]]$name, idx=si-1, data=lapply(1:nrow(trace), function(t) { list(x=(t-1)*as.numeric(m$cycle), y=trace[t, si]) }))
  })
  
  ce <- is.na(best$icer) || best$icer <= as.numeric(m$wtp)
  interp <- list(
    paste0("Over a ", m$horizon, "-year horizon, ", best$name, " is optimal at a willingness-to-pay of ", fmtINR(as.numeric(m$wtp)), " per ", unit, if(is.na(best$icer)) " (the least-costly strategy on the frontier)" else paste0(", with an ICER of ", fmtINR(best$icer), " per ", unit), " - ", if(ce) "cost-effective" else "not cost-effective", " at this threshold."),
    paste0("The cohort trace shows how the modelled population moves between the ", length(m$states), " health states over time; the cost-effectiveness plane summarises each strategy against the cheapest."),
    "These are deterministic (point-estimate) results - run the Sensitivity tab (PSA) to see how parameter uncertainty changes the probability of cost-effectiveness."
  )
  
  list(rows=inc, plane=plane, best=best, unit=unit, onFr=length(onFr), strategies=length(m$strategies), states=length(m$states), activeName=as_strat$name, series=series, stateNames=sapply(m$states, function(s) s$name), wtp=as.numeric(m$wtp), horizon=as.numeric(m$horizon), dCost=as.numeric(m$dCost), outcome=m$outcome, interpretation=interp)
}

#* @serializer unboxedJSON
#* @post /api/sensitivity
function(req) {
  p <- parse_req(req)
  N <- as.numeric(p$N)
  iRef <- min(if(!is.null(p$ref)) as.numeric(p$ref) else 0, length(p$model$strategies) - 1)
  iCmp <- min(if(!is.null(p$cmp)) as.numeric(p$cmp) else 1, length(p$model$strategies) - 1)
  wtp <- as.numeric(p$wtp)
  cv <- if(!is.null(p$cv) && as.numeric(p$cv)>0) as.numeric(p$cv) else 0.2
  
  draws <- psaModel(p$model, N, iRef, iCmp, cv, p[["mode"]], p[["params"]])
  curve <- ceac(draws, seq(0, 1000000, by=50000))
  pCE <- sum(sapply(draws, function(x) nmb(x$incCost, x$incEff, wtp) > 0)) / length(draws)
  ev <- evpi(draws, wtp)
  tor <- dsaModel(p$model, wtp, iRef, iCmp)

  list(pCE = pCE, evpi = ev, ceac = curve, draws = draws, tornado = tor, N = N, wtp = wtp, cv = cv, unit = if(p$model$outcome=="QALY") "QALY" else "DALY averted", refName = p$model$strategies[[iRef+1]]$name, cmpName = p$model$strategies[[iCmp+1]]$name)
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
    b <- paste0(sapply(rows, function(r) paste0("<tr>", paste0("<td>", r, "</td>", collapse=""), "</tr>")), collapse="")
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
    costSec <- paste0("<p>Micro (bottom-up) costing, ", cst$toYear, " prices. <b>Total cost: ", fmtINR(r$total), "</b>.</p>", tbl(c("Category","Cost","Share"), lapply(r$byCat, function(x) c(x$category, fmtINR(x$cost), pct(x$share)))))
  }
  # 2 OOP
  o <- oopRun(st$oop)
  oopSec <- paste0("<p>Total out-of-pocket: <b>", fmtINR(o$total), "</b> - ", pct(o$pctInc), " of income. Catastrophic at 10% income: <b>", if(o$che10) "Yes" else "No", "</b>; at 40% capacity-to-pay: <b>", if(o$che40) "Yes" else "No", "</b>.</p>")
  # 3 Evaluation
  e <- st$evaluation
  if (e$type == "CEA" || e$type == "CUA") {
    inc <- icerIncremental(e$strats)
    evalSec <- paste0("<p>", e$type, ", WTP ", fmtINR(as.numeric(e$wtp)), ".</p>", tbl(c("Strategy","Cost","Effect","ICER","Status"), lapply(inc, function(s) c(s$strategy, fmtINR(s$cost), fmtNum(s$effect,2), if(is.na(s$icer)) "-" else fmtINR(s$icer), s$status))))
  } else {
    evalSec <- paste0("<p>", e$type, " analysis.</p>", tbl(c("Strategy","Cost","Outcome"), lapply(e$strats, function(s) c(s$strategy, fmtINR(as.numeric(s$cost)), fmtNum(as.numeric(s$effect),2)))))
  }
  # 4 Model
  m <- st$model
  res <- modelRunAll(m)
  res <- lapply(res, function(s){ s$eff <- if(m$outcome=="QALY") s$qaly else s$daly; s })
  minc <- modelIncremental(res, m$outcome=="DALY")
  unit <- if(m$outcome=="QALY") "QALY" else "DALY averted"
  modelSec <- paste0("<p>", length(m$states), "-state Markov, ", length(m$strategies), " strategies, ", m$horizon, "-year horizon, ", pct(as.numeric(m$dCost),0), " discounting. Outcome: ", m$outcome, ".</p>", tbl(c("Strategy","Cost","QALYs","DALYs","ICER"), lapply(minc, function(s) c(s$name, fmtINR(s$cost), fmtNum(s$qaly,3), fmtNum(s$daly,3), if(is.na(s$icer)) "-" else fmtINR(s$icer)))))
  # 5 PSA
  iRef <- min(if(!is.null(st$sens$ref)) as.numeric(st$sens$ref) else 0, length(m$strategies)-1)
  iCmp <- min(if(!is.null(st$sens$cmp)) as.numeric(st$sens$cmp) else 1, length(m$strategies)-1)
  wtp <- as.numeric(st$sens$wtp); cvv <- if(!is.null(st$sens$cv)) as.numeric(st$sens$cv) else 0.2
  draws <- psaModel(m, 500, iRef, iCmp, cvv, NULL, NULL)
  pCE <- sum(sapply(draws, function(x) nmb(x$incCost, x$incEff, wtp) > 0)) / length(draws)
  ev <- evpi(draws, wtp)
  psaSec <- paste0("<p>PSA (500 iterations). Probability cost-effective at ", fmtINR(wtp), ": <b>", pct(pCE), "</b>. EVPI per patient: <b>", fmtINR(ev), "</b>.</p>")
  # 6 BIA
  br <- biaRun(st$bia)
  biaSec <- paste0("<p>Budget impact over ", st$bia$horizon, " years. Cumulative net impact: <b>", fmtINR(br[[length(br)]]$cum), "</b>.</p>", tbl(c("Year","Uptake","Net impact","Cumulative"), lapply(br, function(r) c(r$year, pct(r$uptake), fmtINR(r$impact), fmtINR(r$cum)))))

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
  
  list(rows = v, allok = all(sapply(v, function(x) x$ok)))
}
