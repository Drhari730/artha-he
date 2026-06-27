/* =============================================================================
   Artha HE — CONFIDENTIAL calculation engine (server-side only).
   This file is required by server.js and is NEVER served to the browser.
   The client sends inputs to /api/* and receives only finished results.
   ============================================================================= */
"use strict";

const RUPEE = "₹", GDP_PC = 200000;
const COST_CATEGORIES = ["Direct medical", "Direct non-medical", "Indirect (productivity)"];

/* ---------- formatting (used for server-built report) ---------- */
const fmtINR = (x, dp = 0) => RUPEE + Number(x).toLocaleString("en-IN", { minimumFractionDigits: dp, maximumFractionDigits: dp });
const fmtNum = (x, dp = 2) => Number(x).toLocaleString("en-IN", { minimumFractionDigits: dp, maximumFractionDigits: dp });
const pct = (x, dp = 1) => (x * 100).toFixed(dp) + "%";

/* ---------- math ---------- */
const sum = a => a.reduce((s, x) => s + x, 0), dot = (a, b) => a.reduce((s, x, i) => s + x * b[i], 0), seq = n => Array.from({ length: n }, (_, i) => i);
function rnorm(m = 0, sd = 1) { let u = 0, v = 0; while (!u) u = Math.random(); while (!v) v = Math.random(); return m + sd * Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v); }
function rgamma(sh, sc) { if (sh < 1) { const u = Math.random(); return rgamma(1 + sh, sc) * Math.pow(u, 1 / sh); } const d = sh - 1 / 3, c = 1 / Math.sqrt(9 * d); while (true) { let x, v; do { x = rnorm(); v = 1 + c * x; } while (v <= 0); v = v * v * v; const u = Math.random(); if (u < 1 - 0.0331 * x * x * x * x) return d * v * sc; if (Math.log(u) < 0.5 * x * x + d * (1 - v + Math.log(v))) return d * v * sc; } }
function rbeta(a, b) { const x = rgamma(a, 1), y = rgamma(b, 1); return x / (x + y); }
function betaMS(m, se) { if (se <= 0) return { a: m * 1e6, b: (1 - m) * 1e6 }; const v = se * se, t = m * (1 - m) / v - 1; return { a: Math.max(.01, m * t), b: Math.max(.01, (1 - m) * t) }; }
function gammaMS(m, se) { if (se <= 0) return { shape: 1e6, scale: m / 1e6 }; const v = se * se; return { shape: m * m / v, scale: v / m }; }
function rdirichlet(alpha) { const g = alpha.map(a => a > 0 ? rgamma(a, 1) : 0), s = sum(g) || 1; return g.map(x => x / s); }

/* ---------- evaluation type metadata ---------- */
const EVAL_TYPES = {
  CMA: { name: "Cost-minimisation", abbr: "CMA" },
  CEA: { name: "Cost-effectiveness", abbr: "CEA" },
  CUA: { name: "Cost-utility", abbr: "CUA" },
  CBA: { name: "Cost-benefit", abbr: "CBA" },
  CCA: { name: "Cost-consequence", abbr: "CCA" }
};

/* ---------- costing ---------- */
function inflate(c, fy, ty, r) { if (!fy || isNaN(fy)) return c; return c * Math.pow(1 + r, ty - fy); }
function microCost(rows, ty, infl) {
  const lines = rows.map(r => { const u = inflate(+r.unit_cost, +r.year, ty, infl); return { ...r, unit_cost_adj: u, line_cost: (+r.quantity) * u }; });
  const total = sum(lines.map(l => l.line_cost || 0));
  const byCat = COST_CATEGORIES.map(cat => { const cost = sum(lines.filter(l => l.category === cat).map(l => l.line_cost)); return { category: cat, cost, share: total ? cost / total : 0 }; }).filter(c => c.cost > 0);
  return { lines, total, byCat };
}

/* ---------- OOP / catastrophic expenditure ---------- */
function oopRun(o) {
  const items = o.items.map(i => ({ ...i, amount: +i.amount }));
  const total = sum(items.map(i => i.amount));
  const byCat = COST_CATEGORIES.map(cat => { const cost = sum(items.filter(i => i.category === cat).map(i => i.amount)); return { category: cat, cost, share: total ? cost / total : 0 }; }).filter(c => c.cost > 0);
  const pctInc = o.income ? total / o.income : 0, pctCTP = o.nonFood ? total / o.nonFood : 0;
  return { items, total, byCat, pctInc, pctCTP, che10: pctInc > 0.10, che25: pctInc > 0.25, che40: pctCTP > 0.40 };
}

/* ---------- ICER / dominance ---------- */
function icerIncremental(strats) {
  const d = strats.map(s => ({ ...s, cost: +s.cost, effect: +s.effect })).sort((a, b) => a.cost - b.cost);
  d.forEach(s => s.status = "frontier");
  d.forEach(s => { if (d.some(o => o.cost < s.cost && o.effect >= s.effect)) s.status = "dominated"; });
  let fr = d.filter(s => s.status !== "dominated");
  for (let i = 0; i < fr.length; i++) { if (i === 0) { fr[i].incCost = null; fr[i].incEff = null; fr[i].icer = null; } else { fr[i].incCost = fr[i].cost - fr[i - 1].cost; fr[i].incEff = fr[i].effect - fr[i - 1].effect; fr[i].icer = fr[i].incEff ? fr[i].incCost / fr[i].incEff : null; } }
  for (let i = 2; i < fr.length; i++) { if (fr[i].icer != null && fr[i - 1].icer != null && fr[i].icer < fr[i - 1].icer) fr[i - 1].status = "extended"; }
  d.forEach(s => { const f = fr.find(x => x.strategy === s.strategy); if (f) { s.incCost = f.incCost; s.incEff = f.incEff; s.icer = f.icer; s.status = f.status; } });
  return d;
}
const nmb = (c, e, w) => e * w - c;
function evalRequirements(type, strats) {
  const allCost = strats.every(s => +s.cost > 0);
  const allEff = strats.every(s => +s.effect !== 0 && s.effect !== "");
  const eff = strats.map(s => +s.effect), mean = sum(eff) / eff.length;
  const equal = mean === 0 ? true : (Math.max(...eff) - Math.min(...eff)) / Math.abs(mean) < 0.03;
  const items = [{ label: "Total cost for each option", ok: allCost }];
  if (type === "CMA") items.push({ label: "Outcomes equal across options", ok: equal, warn: !equal && "Outcomes differ — CEA/CUA may be more appropriate" });
  else if (type === "CBA") items.push({ label: "Monetised benefit (₹) for each option", ok: allEff });
  else if (type === "CCA") items.push({ label: "At least one outcome measure", ok: allEff });
  else if (type === "CUA") items.push({ label: "QALYs (utility × time) for each option", ok: allEff });
  else items.push({ label: "A common natural-unit effect for each option", ok: allEff });
  return { items, missing: items.filter(i => !i.ok).length };
}

/* ---------- configurable Markov ---------- */
function fixModel(m) { const n = m.states.length; m.strategies.forEach(s => { const M = []; for (let i = 0; i < n; i++) { const row = []; for (let j = 0; j < n; j++) { const v = s.matrix && s.matrix[i] && s.matrix[i][j] != null ? +s.matrix[i][j] : (i === j ? 1 : 0); row.push(v); } M.push(row); } s.matrix = M; }); }
function markovGeneric(m, strat) {
  const n = m.states.length, P = strat.matrix, nC = Math.round(m.horizon / m.cycle);
  let trace = [m.states.map((s, i) => i === 0 ? 1 : 0)];
  for (let t = 0; t < nC; t++) { const cur = trace[t], nx = new Array(n).fill(0); for (let i = 0; i < n; i++) { const row = P[i] || []; for (let j = 0; j < n; j++) nx[j] += cur[i] * (+row[j] || 0); } trace.push(nx); }
  let Cc = 0, Q = 0, YLD = 0;
  for (let t = 0; t < nC; t++) { const occ = trace[t].map((v, i) => (v + trace[t + 1][i]) / 2), dc = Math.pow(1 + m.dCost, t * m.cycle), de = Math.pow(1 + m.dEff, t * m.cycle); for (let i = 0; i < n; i++) { const st = m.states[i], cc = (+st.cost) + (st.absorbing ? 0 : (+strat.addCost || 0)); Cc += occ[i] * cc * m.cycle / dc; Q += occ[i] * (+st.util) * m.cycle / de; YLD += occ[i] * (+st.dw || 0) * m.cycle / de; } }
  const deadIdx = m.states.map((s, i) => s.absorbing ? i : -1).filter(i => i >= 0); let YLL = 0;
  for (let t = 0; t < nC; t++) { let nd = 0; deadIdx.forEach(i => nd += trace[t + 1][i] - trace[t][i]); YLL += nd * (+m.lifeExp) / Math.pow(1 + m.dEff, t * m.cycle); }
  return { cost: Cc, qaly: Q, daly: YLD + YLL, yll: YLL, trace, nC };
}
function modelRunAll(m) { fixModel(m); return m.strategies.map(s => { const r = markovGeneric(m, s); return { name: s.name, cost: r.cost, qaly: r.qaly, daly: r.daly, trace: r.trace }; }); }
function modelIncremental(arr, lowerBetter) {
  let d = arr.map(s => ({ ...s })).sort((a, b) => a.cost - b.cost);
  d.forEach(s => s.status = "frontier");
  d.forEach(s => { if (d.some(o => o.cost < s.cost && (lowerBetter ? o.eff <= s.eff : o.eff >= s.eff))) s.status = "dominated"; });
  let fr = d.filter(s => s.status !== "dominated");
  for (let i = 0; i < fr.length; i++) { if (i === 0) { fr[i].incCost = null; fr[i].incEff = null; fr[i].icer = null; } else { fr[i].incCost = fr[i].cost - fr[i - 1].cost; const de = lowerBetter ? (fr[i - 1].eff - fr[i].eff) : (fr[i].eff - fr[i - 1].eff); fr[i].incEff = de; fr[i].icer = de ? fr[i].incCost / de : null; } }
  for (let i = 2; i < fr.length; i++) { if (fr[i].icer != null && fr[i - 1].icer != null && fr[i].icer < fr[i - 1].icer) fr[i - 1].status = "extended"; }
  d.forEach(s => { const f = fr.find(x => x.name === s.name); if (f) { s.incCost = f.incCost; s.incEff = f.incEff; s.icer = f.icer; s.status = f.status; } });
  return d;
}
function psaModel(m, N, iRef, iCmp) {
  fixModel(m); const draws = [], K = 80;
  for (let it = 0; it < N; it++) {
    const mm = JSON.parse(JSON.stringify(m));
    mm.states.forEach(st => { if (+st.cost > 0) { const g = gammaMS(+st.cost, +st.cost * 0.2); st.cost = rgamma(g.shape, g.scale); } if (+st.util > 0 && +st.util < 1) { const b = betaMS(+st.util, 0.03); st.util = rbeta(b.a, b.b); } });
    mm.strategies.forEach(s => { s.matrix = s.matrix.map(row => { if (sum(row.map(Number)) === 0) return row; return rdirichlet(row.map(p => Math.max(0.0001, +p) * K)); }); });
    const ref = markovGeneric(mm, mm.strategies[iRef]), cmp = markovGeneric(mm, mm.strategies[iCmp]);
    const incEff = m.outcome === "QALY" ? (cmp.qaly - ref.qaly) : (ref.daly - cmp.daly);
    draws.push({ incCost: cmp.cost - ref.cost, incEff });
  }
  return draws;
}
const ceac = (d, ws) => ws.map(w => ({ wtp: w, prob: sum(d.map(x => nmb(x.incCost, x.incEff, w) > 0 ? 1 : 0)) / d.length }));
function evpi(d, w) { const nb = d.map(x => [0, nmb(x.incCost, x.incEff, w)]); const mm = sum(nb.map(x => Math.max(...x))) / nb.length; const mx = Math.max(sum(nb.map(x => x[0])) / nb.length, sum(nb.map(x => x[1])) / nb.length); return Math.max(0, mm - mx); }
function dsaModel(m, w, iRef, iCmp) {
  fixModel(m);
  const nmbPair = mm => { const r = markovGeneric(mm, mm.strategies[iRef]), c = markovGeneric(mm, mm.strategies[iCmp]); const incC = c.cost - r.cost, incE = m.outcome === "QALY" ? (c.qaly - r.qaly) : (r.daly - c.daly); return incE * w - incC; };
  const base = nmbPair(m), params = [];
  m.strategies.forEach((s, si) => { if (+s.addCost > 0) params.push({ label: s.name + " added cost", mut: (mm, v) => mm.strategies[si].addCost = v, lo: +s.addCost * 0.7, hi: +s.addCost * 1.3 }); });
  m.states.forEach((st, si) => { if (+st.cost > 0) params.push({ label: st.name + " cost", mut: (mm, v) => mm.states[si].cost = v, lo: +st.cost * 0.7, hi: +st.cost * 1.3 }); if (+st.util > 0) params.push({ label: st.name + " utility", mut: (mm, v) => mm.states[si].util = v, lo: Math.max(0, +st.util - 0.1), hi: Math.min(1, +st.util + 0.1) }); });
  return params.map(p => { const mlo = JSON.parse(JSON.stringify(m)); p.mut(mlo, p.lo); const mhi = JSON.parse(JSON.stringify(m)); p.mut(mhi, p.hi); const nL = nmbPair(mlo), nH = nmbPair(mhi); return { label: p.label, low: Math.min(nL, nH), high: Math.max(nL, nH), swing: Math.abs(nH - nL), base }; }).sort((a, b) => b.swing - a.swing).slice(0, 8);
}

/* ---------- budget impact ---------- */
function biaRun(b) {
  const years = seq(b.horizon).map(i => b.startYear + i);
  const rows = years.map((yr, i) => { const uptake = Math.min(b.maxUptake, b.maxUptake * (i + 1) / b.horizon); const treated = b.population * b.eligible * uptake; const onOld = b.population * b.eligible - treated; const worldNew = treated * b.costNew + onOld * b.costOld; const worldOld = b.population * b.eligible * b.costOld; return { year: yr, uptake, treated, worldOld, worldNew, impact: worldNew - worldOld }; });
  let cum = 0; rows.forEach(r => { cum += r.impact; r.cum = cum; });
  return rows;
}

/* ===========================================================================
   HIGH-LEVEL COMPUTE WRAPPERS — return render-ready objects to the client.
   =========================================================================== */
function computeCosting(c) {
  if (c.method === "gross") { const per = c.output ? c.totalCost / c.output : 0; return { method: "gross", total: c.totalCost, output: c.output, per }; }
  const r = microCost(c.rows, c.toYear, c.inflation);
  return { method: "micro", total: r.total, byCat: r.byCat, lines: r.lines, toYear: c.toYear, inflation: c.inflation };
}
function computeOOP(o) { return oopRun(o); }
function computeEvaluation(e) {
  const req = evalRequirements(e.type, e.strats);
  if (e.type === "CBA") {
    const d = e.strats.map(s => ({ strategy: s.strategy, cost: +s.cost, benefit: +s.effect, net: +s.effect - +s.cost, bcr: (+s.cost) ? (+s.effect) / (+s.cost) : 0 })).sort((a, b) => b.net - a.net);
    return { type: e.type, requirements: req, rows: d, best: d[0] };
  }
  if (e.type === "CMA") {
    const d = e.strats.map(s => ({ strategy: s.strategy, cost: +s.cost, effect: +s.effect })).sort((a, b) => a.cost - b.cost);
    const eff = e.strats.map(s => +s.effect), mean = sum(eff) / eff.length, equal = mean === 0 || (Math.max(...eff) - Math.min(...eff)) / Math.abs(mean) < 0.03;
    return { type: e.type, requirements: req, rows: d, best: d[0], equal };
  }
  if (e.type === "CCA") {
    const d = e.strats.map(s => ({ strategy: s.strategy, cost: +s.cost, effect: +s.effect })).sort((a, b) => a.cost - b.cost);
    return { type: e.type, requirements: req, rows: d };
  }
  const d = icerIncremental(e.strats), ref = [...d].sort((a, b) => a.cost - b.cost)[0];
  d.forEach(s => s.nmb = nmb(s.cost, s.effect, e.wtp));
  const onFr = d.filter(s => s.status === "frontier");
  const best = onFr.filter(s => s.icer == null || s.icer <= e.wtp).slice(-1)[0] || ref;
  const plane = d.map(s => ({ label: s.strategy, dEff: s.effect - ref.effect, dCost: s.cost - ref.cost, ref: s.strategy === ref.strategy }));
  return { type: e.type, requirements: req, rows: d.sort((a, b) => a.cost - b.cost), plane, best, wtp: e.wtp, unit: e.type === "CUA" ? "QALY" : "unit" };
}
function computeModel(m) {
  const res = modelRunAll(m), lowerBetter = m.outcome === "DALY";
  res.forEach(s => s.eff = lowerBetter ? s.daly : s.qaly);
  const inc = modelIncremental(res, lowerBetter), ref = [...inc].sort((a, b) => a.cost - b.cost)[0];
  const unit = m.outcome === "QALY" ? "QALY" : "DALY averted";
  const onFr = inc.filter(s => s.status === "frontier");
  const best = onFr.filter(s => s.icer == null || s.icer <= m.wtp).slice(-1)[0] || ref;
  const plane = inc.map(s => ({ label: s.name, dEff: lowerBetter ? (ref.daly - s.daly) : (s.qaly - ref.qaly), dCost: s.cost - ref.cost, ref: s.name === ref.name }));
  const as = m.strategies[m.activeStrat] || m.strategies[0];
  const trace = markovGeneric(m, as).trace;
  const series = m.states.map((st, si) => ({ label: st.name, idx: si, data: trace.map((row, t) => ({ x: t * m.cycle, y: row[si] })) }));
  return { rows: inc.sort((a, b) => a.cost - b.cost), plane, best, unit, onFr: onFr.length, strategies: m.strategies.length, states: m.states.length, activeName: as.name, series, stateNames: m.states.map(s => s.name), wtp: m.wtp, horizon: m.horizon, dCost: m.dCost, outcome: m.outcome };
}
function computeSensitivity(p) {
  const m = p.model, N = p.N, iRef = Math.min(p.ref ?? 0, m.strategies.length - 1), iCmp = Math.min(p.cmp ?? 1, m.strategies.length - 1), wtp = p.wtp;
  const draws = psaModel(m, N, iRef, iCmp), curve = ceac(draws, seq(21).map(i => i * 50000)), pCE = sum(draws.map(x => nmb(x.incCost, x.incEff, wtp) > 0 ? 1 : 0)) / draws.length, ev = evpi(draws, wtp), tor = dsaModel(m, wtp, iRef, iCmp);
  return { pCE, evpi: ev, ceac: curve, draws, tornado: tor, N, wtp, unit: m.outcome === "QALY" ? "QALY" : "DALY averted", refName: m.strategies[iRef].name, cmpName: m.strategies[iCmp].name };
}
function computeBIA(b) { const rows = biaRun(b); return { rows, cumulative: rows[rows.length - 1].cum, peak: Math.max(...rows.map(r => r.impact)), eligible: b.population * b.eligible, population: b.population, horizon: b.horizon, startYear: b.startYear }; }

/* ---------- validation ---------- */
function runValidations(model) {
  const ap = (a, b, t = 0.01) => Math.abs(a - b) <= t * Math.max(1, Math.abs(b)); const v = [];
  const d = 1000 / Math.pow(1.03, 5); v.push({ n: "Discount ₹1,000 at 3% for 5 years", got: fmtNum(d, 2), exp: "862.61", ok: ap(d, 862.6088) });
  const ic = (85000 - 40000) / (4.4 - 3.5); v.push({ n: "ICER of (₹85k, 4.4) vs (₹40k, 3.5)", got: fmtINR(ic), exp: "₹50,000", ok: ap(ic, 50000) });
  const nb = nmb(85000, 4.4, 200000); v.push({ n: "NMB at WTP ₹2,00,000 (cost 85k, 4.4 QALY)", got: fmtINR(nb), exp: "₹7,95,000", ok: ap(nb, 795000) });
  const dom = icerIncremental([{ strategy: "A", cost: 40000, effect: 3.5 }, { strategy: "B", cost: 50000, effect: 3.2 }]).find(x => x.strategy === "B").status; v.push({ n: "Costs more & less effective → flagged dominated", got: dom, exp: "dominated", ok: dom === "dominated" });
  const o = oopRun({ income: 200000, nonFood: 120000, items: [{ item: "x", category: "Direct medical", amount: 47000 }] }); v.push({ n: "OOP ₹47,000 ÷ income ₹2,00,000", got: pct(o.pctInc), exp: "23.5%", ok: ap(o.pctInc * 100, 23.5) });
  if (model) { const arm = markovGeneric(model, model.strategies[0]), last = arm.trace[arm.trace.length - 1], mass = sum(last); v.push({ n: "Markov cohort mass conserved (Σ states = 1)", got: fmtNum(mass, 6), exp: "1.000000", ok: Math.abs(mass - 1) < 1e-9 }); }
  const mc = microCost([{ item: "a", category: "Direct medical", quantity: 2, unit_cost: 100, year: 2024 }, { item: "b", category: "Direct medical", quantity: 3, unit_cost: 50, year: 2024 }], 2024, 0); v.push({ n: "Micro-cost: 2×₹100 + 3×₹50", got: fmtINR(mc.total), exp: "₹350", ok: ap(mc.total, 350) });
  return { rows: v, allok: v.every(x => x.ok) };
}

/* ---------- combined CHEERS report (HTML built server-side) ---------- */
function buildReport(state) {
  const dt = new Date().toLocaleDateString("en-IN", { year: "numeric", month: "long", day: "numeric" });
  const tbl = (head, rows) => `<table><tr>${head.map(h => `<th>${h}</th>`).join("")}</tr>${rows.map(r => `<tr>${r.map(c => `<td>${c}</td>`).join("")}</tr>`).join("")}</table>`;
  const c = state.costing; let costSec;
  if (c.method === "gross") { const per = c.output ? c.totalCost / c.output : 0; costSec = `<p>Gross (top-down) costing.</p>` + tbl(["Metric", "Value"], [["Total cost", fmtINR(c.totalCost)], ["Output units", fmtNum(c.output, 0)], ["Cost per unit", fmtINR(per, 2)]]); }
  else { const r = microCost(c.rows, c.toYear, c.inflation); costSec = `<p>Micro (bottom-up) costing, ${c.toYear} prices. <b>Total cost: ${fmtINR(r.total)}</b>.</p>` + tbl(["Category", "Cost", "Share"], r.byCat.map(b => [b.category, fmtINR(b.cost), pct(b.share)])); }
  const o = oopRun(state.oop);
  const oopSec = `<p>Total out-of-pocket: <b>${fmtINR(o.total)}</b> — ${pct(o.pctInc)} of household income. Catastrophic at 10% income: <b>${o.che10 ? "Yes" : "No"}</b>; at 40% capacity-to-pay: <b>${o.che40 ? "Yes" : "No"}</b>.</p>` + tbl(["Category", "Amount"], o.byCat.map(b => [b.category, fmtINR(b.cost)]));
  const e = state.evaluation; let evalSec;
  if (e.type === "CEA" || e.type === "CUA") { const inc = icerIncremental(e.strats).sort((a, b) => a.cost - b.cost); evalSec = `<p>${EVAL_TYPES[e.type].name} (${e.type}). WTP ${fmtINR(e.wtp)} per ${e.type === "CUA" ? "QALY" : "unit"}.</p>` + tbl(["Strategy", "Cost", "Effect", "ΔCost", "ΔEffect", "ICER", "Status"], inc.map(s => [s.strategy, fmtINR(s.cost), fmtNum(s.effect, 2), s.incCost == null ? "—" : fmtINR(s.incCost), s.incEff == null ? "—" : fmtNum(s.incEff, 2), s.icer == null ? "—" : fmtINR(s.icer), s.status])); }
  else if (e.type === "CBA") { const d2 = e.strats.map(s => ({ ...s, net: +s.effect - +s.cost, bcr: (+s.cost) ? (+s.effect) / (+s.cost) : 0 })).sort((a, b) => b.net - a.net); evalSec = `<p>Cost-benefit analysis. Best option: <b>${d2[0].strategy}</b> (net benefit ${fmtINR(d2[0].net)}).</p>` + tbl(["Strategy", "Cost", "Benefit", "Net benefit", "BCR"], d2.map(s => [s.strategy, fmtINR(s.cost), fmtINR(+s.effect), fmtINR(s.net), fmtNum(s.bcr, 2)])); }
  else { evalSec = `<p>${EVAL_TYPES[e.type].name} (${e.type}).</p>` + tbl(["Strategy", "Cost", "Outcome"], e.strats.slice().sort((a, b) => a.cost - b.cost).map(s => [s.strategy, fmtINR(+s.cost), fmtNum(+s.effect, 2)])); }
  const m = state.model, res = modelRunAll(m); res.forEach(s => s.eff = m.outcome === "QALY" ? s.qaly : s.daly);
  const minc = modelIncremental(res, m.outcome === "DALY").sort((a, b) => a.cost - b.cost), unit = m.outcome === "QALY" ? "QALY" : "DALY averted";
  const modelSec = `<p>${m.states.length}-state Markov, ${m.strategies.length} strategies, ${m.horizon}-year horizon, ${pct(m.dCost, 0)} discounting, half-cycle correction. Outcome: ${m.outcome}.</p>` + tbl(["Strategy", "Cost", "QALYs", "DALYs", "ICER (₹/" + unit + ")", "Status"], minc.map(s => [s.name, fmtINR(s.cost), fmtNum(s.qaly, 3), fmtNum(s.daly, 3), s.icer == null ? "—" : fmtINR(s.icer), s.status]));
  const iRef = Math.min(state.sens.ref ?? 0, m.strategies.length - 1), iCmp = Math.min(state.sens.cmp ?? 1, m.strategies.length - 1);
  const draws = psaModel(m, 500, iRef, iCmp), wtp = state.sens.wtp, pCE = sum(draws.map(x => nmb(x.incCost, x.incEff, wtp) > 0 ? 1 : 0)) / draws.length, ev = evpi(draws, wtp);
  const psaSec = `<p>Probabilistic sensitivity analysis (500 iterations), ${m.strategies[iCmp].name} vs ${m.strategies[iRef].name}. Probability cost-effective at ${fmtINR(wtp)}: <b>${pct(pCE)}</b>. EVPI per patient: <b>${fmtINR(ev)}</b>.</p>`;
  const b = biaRun(state.bia);
  const biaSec = `<p>Budget impact over ${state.bia.horizon} years from ${state.bia.startYear}. Cumulative net impact: <b>${fmtINR(b[b.length - 1].cum)}</b>.</p>` + tbl(["Year", "Uptake", "Net impact", "Cumulative"], b.map(r => [r.year, pct(r.uptake), fmtINR(r.impact), fmtINR(r.cum)]));
  const html = `<h1>Artha HE — Health Economic Analysis Report</h1>
    <p class="meta">Generated ${dt} · India reference case (₹, 3% discounting). Reporting structured to the CHEERS 2022 checklist.</p>
    <h2>1 · Costing</h2>${costSec}
    <h2>2 · Out-of-pocket &amp; catastrophic expenditure</h2>${oopSec}
    <h2>3 · Economic evaluation</h2>${evalSec}
    <h2>4 · Decision-analytic model</h2>${modelSec}
    <h2>5 · Sensitivity analysis</h2>${psaSec}
    <h2>6 · Budget impact</h2>${biaSec}
    <p class="foot">Generated by Artha HE · for research &amp; teaching. Verify inputs against your study protocol before use.</p>`;
  return { html };
}

/* ---------- dispatch table (endpoint name → function) ---------- */
const COMPUTE = {
  costing: computeCosting,
  oop: computeOOP,
  evaluation: computeEvaluation,
  model: computeModel,
  sensitivity: computeSensitivity,
  bia: computeBIA,
  report: buildReport,
  validate: payload => runValidations(payload && payload.model)
};

module.exports = { COMPUTE };
