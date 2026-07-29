# plumber.R
library(plumber)
library(jsonlite)
source("engine.R")

fmtINR <- function(x) paste0("\u20B9", formatC(x, format="f", big.mark=",", digits=0))
fmtNum <- function(x, dp=2) formatC(x, format="f", big.mark=",", digits=dp)
pct <- function(x, dp=1) paste0(formatC(x * 100, format="f", digits=dp), "%")

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

#* @post /api/costing
function(req) {
  c <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
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

#* @post /api/oop
function(req) {
  o <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  oopRun(o)
}

#* @post /api/evaluation
function(req) {
  e <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
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

#* @post /api/model
function(req) {
  m <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  res <- modelRunAll(m)
  res <- lapply(res, function(s) { s$eff <- if(m$outcome == "QALY") s$qaly else s$daly; s })
  inc <- modelIncremental(res, m$outcome == "DALY")
  ref <- inc[[1]]
  unit <- if(m$outcome == "QALY") "QALY" else "DALY averted"
  
  onFr <- Filter(function(s) s$status == "frontier", inc)
  best <- ref
  for (s in onFr) { if (is.na(s$icer) || s$icer <= as.numeric(m$wtp)) best <- s }
  
  plane <- lapply(inc, function(s) { list(label=s$name, dEff=if(m$outcome=="DALY") ref$daly - s$daly else s$qaly - ref$qaly, dCost=s$cost - ref$cost, ref=s$name == ref$name) })
  
  as_idx <- if(!is.null(m$activeStrat)) as.numeric(m$activeStrat) + 1 else 1
  as_strat <- m$strategies[[as_idx]]
  trace <- markovGeneric(fixModel(m), as_strat)$trace
  
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

#* @post /api/sensitivity
function(req) {
  p <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  N <- as.numeric(p$N)
  iRef <- min(if(!is.null(p$ref)) as.numeric(p$ref) else 0, length(p$model$strategies) - 1)
  iCmp <- min(if(!is.null(p$cmp)) as.numeric(p$cmp) else 1, length(p$model$strategies) - 1)
  wtp <- as.numeric(p$wtp)
  cv <- if(!is.null(p$cv) && as.numeric(p$cv)>0) as.numeric(p$cv) else 0.2
  
  draws <- psaModel(p$model, N, iRef, iCmp, cv, p$mode, p$params)
  curve <- ceac(draws, seq(0, 1000000, by=50000))
  pCE <- sum(sapply(draws, function(x) nmb(x$incCost, x$incEff, wtp) > 0)) / length(draws)
  ev <- evpi(draws, wtp)
  
  list(pCE = pCE, evpi = ev, ceac = curve, draws = draws, tornado = list(), N = N, wtp = wtp, cv = cv, unit = if(p$model$outcome=="QALY") "QALY" else "DALY averted", refName = p$model$strategies[[iRef+1]]$name, cmpName = p$model$strategies[[iCmp+1]]$name)
}

#* @post /api/bia
function(req) {
  b <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  rows <- biaRun(b)
  list(rows = rows, cumulative = rows[[length(rows)]]$cum, peak = max(sapply(rows, function(r) r$impact)), eligible = as.numeric(b$population)*as.numeric(b$eligible), population = as.numeric(b$population), horizon = as.numeric(b$horizon), startYear = as.numeric(b$startYear))
}
