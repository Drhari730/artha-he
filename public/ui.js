/* =============================================================================
   Artha HE — presentation layer (ui.js), design v5
   - numbers charts as "Figure n." and results tables as "Table n." (journal style)
   - counts up headline numbers, draws chart lines, staggers card entrances
   - calculation screens: verdict banner, scenario tabs, Checks panel
   Loaded last; shares the globals of app.js, models.js, library.js, flow.js.
   ============================================================================= */

const REDUCED_MOTION = (() => { try { return window.matchMedia("(prefers-reduced-motion: reduce)").matches; } catch (e) { return false; } })();

/* ------------------------- figure and table numbering ------------------------- */
function numberFigures(root) {
  let f = 0, t = 0;
  root.querySelectorAll(".card").forEach(card => {
    if (card.classList.contains("interp") || card.classList.contains("use-card") || card.classList.contains("checks-card")) return;
    const h = card.querySelector("h3"); if (!h) return;
    const old = h.querySelector(".fig-no"); if (old) old.remove();
    const isFig = !!card.querySelector("svg.chart"), isTab = !isFig && !!card.querySelector("table.results-table");
    if (!isFig && !isTab) return;
    const span = document.createElement("span"); span.className = "fig-no";
    span.textContent = isFig ? `Figure ${++f}.` : `Table ${++t}.`;
    h.insertBefore(span, h.firstChild);
  });
}

/* ------------------------------- count-up numbers ------------------------------- */
function countUp(el) {
  const txt = el.textContent.trim();
  const m = txt.match(/^([^\d-]*)(-?[\d,]*\.?\d+)(.*)$/);
  if (!m || txt.length > 22) return;
  const target = parseFloat(m[2].replace(/,/g, "")); if (!isFinite(target) || target === 0) return;
  const dec = (m[2].split(".")[1] || "").length, comma = m[2].includes(",");
  const fmt = v => comma ? v.toLocaleString("en-IN", { minimumFractionDigits: dec, maximumFractionDigits: dec }) : v.toFixed(dec);
  const start = performance.now(), dur = 900;
  const step = now => {
    if (!el.isConnected) return;
    const p = Math.min(1, (now - start) / dur), e = 1 - Math.pow(1 - p, 3);
    el.textContent = m[1] + fmt(target * e) + m[3];
    if (p < 1) requestAnimationFrame(step); else el.textContent = txt;
  };
  requestAnimationFrame(step);
}

/* ------------------------------- chart line drawing ------------------------------ */
function drawLines(root) {
  root.querySelectorAll(".pane.active svg.chart, .card > svg.chart, .pad > svg.chart").forEach(svg => {
    svg.querySelectorAll("polyline, path").forEach(el => {
      if (el.hasAttribute("stroke-dasharray") || el.getAttribute("fill") && el.getAttribute("fill") !== "none") return;
      let len = 0; try { len = el.getTotalLength(); } catch (e) { return; }
      if (!(len > 20 && len < 6000)) return;
      el.style.strokeDasharray = len; el.style.strokeDashoffset = len;
      el.getBoundingClientRect();
      el.style.transition = "stroke-dashoffset 1.1s cubic-bezier(.45,0,.2,1)";
      el.style.strokeDashoffset = 0;
      setTimeout(() => { el.style.strokeDasharray = ""; el.style.strokeDashoffset = ""; el.style.transition = ""; }, 1200);
    });
  });
}

/* ------------------------- run on every new results screen ------------------------ */
let uiBusy = false;
function presentWorkspace() {
  const ws = document.getElementById("workspace"); if (!ws || uiBusy) return;
  uiBusy = true;
  try {
    numberFigures(ws);
    if (!REDUCED_MOTION) {
      const items = ws.querySelectorAll(":scope > .kpis .kpi, :scope > .verdict, :scope > .card, :scope > .kpis, :scope > .pane.active > .card, :scope > .home > .card");
      let i = 0;
      items.forEach(el => { if (el.dataset.v5) return; el.dataset.v5 = "1"; el.classList.add("v5-rise"); el.style.animationDelay = Math.min(i++, 10) * 45 + "ms"; });
      ws.querySelectorAll(".kpi .k-val:not(.sm)").forEach(el => { if (el.dataset.counted) return; el.dataset.counted = "1"; countUp(el); });
      drawLines(ws);
    }
  } finally { uiBusy = false; }
}
(function watchWorkspace() {
  const ws = document.getElementById("workspace"); if (!ws) return;
  let queued = false;
  new MutationObserver(() => { if (uiBusy || queued) return; queued = true; requestAnimationFrame(() => { queued = false; presentWorkspace(); }); })
    .observe(ws, { childList: true });
  // tab switches reveal hidden charts: draw them when shown
  ws.addEventListener("click", e => { if (e.target.closest(".result-tab") && !REDUCED_MOTION) requestAnimationFrame(() => drawLines(ws)); });
})();

/* ------------------------------ scenario tabs ------------------------------ */
let SCEN_VIEW = null;  // {key, label, model} while a scenario is shown on the Modeling screen
function modelPayload() { return SCEN_VIEW ? SCEN_VIEW.model : activeModel(); }
const STD_SCEN = [
  { key: "d0", label: "Discount 0%", patch: m => Object.assign(m, { dCost: 0, dEff: 0 }) },
  { key: "d5", label: "Discount 5%", patch: m => Object.assign(m, { dCost: 0.05, dEff: 0.05 }) },
  { key: "ud", label: "Outcomes undiscounted", patch: m => Object.assign(m, { dCost: 0.03, dEff: 0 }) },
  { key: "hh", label: "Horizon halved", patch: m => Object.assign(m, { horizon: Math.max(+m.cycle || 1, (+m.horizon || 1) / 2) }) }
];
function scenarioPillsHTML() {
  const S = typeof scenState === "function" ? scenState() : { list: [] }, tree = modelKind() === "tree", cur = SCEN_VIEW ? SCEN_VIEW.key : "base";
  const b = (key, label, cls) => `<button type="button" data-scen="${key}" class="${cls || ""}${cur === key ? " on" : ""}">${escHTML(label)}</button>`;
  return `<div class="scen-pills" role="tablist" aria-label="Scenarios"><span class="lab">Scenario</span>${b("base", "Base case")}${tree ? "" : STD_SCEN.map(x => b(x.key, x.label)).join("")}${S.list.map((x, i) => b("s" + i, x.name)).join("")}<button type="button" class="add" data-scen="save">+ Save as scenario</button></div>`;
}
function wireScenarioPills(root) {
  root.querySelectorAll("[data-scen]").forEach(btn => btn.onclick = () => {
    const k = btn.dataset.scen;
    if (k === "save") {
      const name = prompt("Name this scenario (it saves the current model inputs):", "Scenario " + (scenState().list.length + 1));
      if (!name) return;
      scenState().list.push({ name: name.trim(), note: "Saved from the Modeling screen " + new Date().toISOString().slice(0, 10), model: JSON.parse(JSON.stringify(state.model)) });
      saveLocal(); const pills = root.querySelector(".scen-pills"); if (pills) { pills.outerHTML = scenarioPillsHTML(); wireScenarioPills(root); }
      return;
    }
    if (k === "base") SCEN_VIEW = null;
    else if (k[0] === "s") { const x = scenState().list[+k.slice(1)]; if (!x) return; SCEN_VIEW = { key: k, label: x.name, model: toApiModel(x.model) }; }
    else { const x = STD_SCEN.find(z => z.key === k); SCEN_VIEW = { key: k, label: x.label, model: x.patch(JSON.parse(JSON.stringify(activeModel()))) }; }
    renderModel();
  });
}
// Pressing Run model, or leaving the screen, returns to the base case.
document.getElementById("sidebar").addEventListener("click", e => { if (e.target.closest("#runModel")) SCEN_VIEW = null; }, true);
document.querySelectorAll("#topnav button").forEach(b => b.addEventListener("click", () => { SCEN_VIEW = null; }, true));

/* ----------------------------- verdict and checks ----------------------------- */
function verdictHTML(R) {
  const rows = R.rows || [], ref = rows[0], best = R.best, unit = R.unit || "QALY", wtp = +R.wtp || 0;
  if (!ref || !best) return "";
  const atGdp = Math.abs(wtp - GDP_PC) < 1 ? "1× GDP per capita" : `${fmtINR(wtp)} per ${unit}`;
  const pt = (R.plane || []).find(p => p.label === best.name) || { dEff: 0, dCost: 0 };
  const effWord = unit === "QALY" ? "QALYs" : unit;
  const ok = '<svg width="20" height="20" viewBox="0 0 20 20" aria-hidden="true"><path d="M4.5 10.5l3.5 3.5 7.5-8" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"/></svg>';
  const warn = '<svg width="20" height="20" viewBox="0 0 20 20" aria-hidden="true"><path d="M10 5v6M10 14.5v.5" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round"/></svg>';
  const scen = SCEN_VIEW ? ` <span class="tag ext">Scenario: ${escHTML(SCEN_VIEW.label)}</span>` : "";
  if (best.name === ref.name) {
    const next = rows.find(r => r.name !== ref.name && r.status === "frontier" && r.icer != null);
    return `<div class="verdict no"><span class="vi">${warn}</span><div class="vb"><div class="vt">${escHTML(ref.name)} remains the best use of resources at ${atGdp}.${scen}</div><div class="vs">${next ? `${escHTML(next.name)} would cost ${fmtINR(next.icer)} per ${unit} gained, above the threshold of ${fmtINR(wtp)}.` : "No alternative gives enough extra benefit for its extra cost at this threshold."}</div></div></div>`;
  }
  return `<div class="verdict"><span class="vi">${ok}</span><div class="vb"><div class="vt">${escHTML(best.name)} is cost-effective at ${atGdp}.${scen}</div><div class="vs">Compared with ${escHTML(ref.name)} it adds ${fmtNum(pt.dEff, 3)} ${effWord} for ${fmtINR(pt.dCost)} more per patient${best.icer != null ? `: ${fmtINR(best.icer)} per ${unit} against a threshold of ${fmtINR(wtp)}` : ""}.</div></div></div>`;
}
function checksCardHTML() {
  if (typeof flowChecks !== "function") return "";
  const m = activeModel(), items = [];
  if (m.type === "markov" && !m.strategies.some(s => !s.effect && s.matrix && s.matrix.some(r => Math.abs(rowSum(r) - 1) > 0.001))) items.push({ lvl: "ok", t: "Every transition row sums to 1" });
  if (m.type !== "tree" && +m.dCost === 0.03 && +m.dEff === 0.03) items.push({ lvl: "ok", t: "Discounting matches the India reference case (3%)" });
  if (m.type === "psm" && m.strategies.every(s => s.os && s.pfs)) items.push({ lvl: "ok", t: "Every strategy has OS and PFS curves" });
  flowChecks().filter(c => ["modeling", "sensitivity", "reference", "survival"].includes(c.go)).forEach(c => items.push({ lvl: c.lvl, t: c.t, go: c.go }));
  if (!items.length) return "";
  return `<div class="card checks-card"><h3>Checks</h3><ul>${items.map(c => `<li class="${c.lvl}"><span class="dot"></span><span>${c.t}${c.go ? ` <a href="#" data-go="${c.go}">Fix →</a>` : ""}</span></li>`).join("")}</ul></div>`;
}
function decorateModelResults(R) {
  const ws = document.getElementById("workspace"), head = ws.querySelector(".ws-head"); if (!head) return;
  head.insertAdjacentHTML("afterend", scenarioPillsHTML() + verdictHTML(R));
  wireScenarioPills(ws);
  const checks = checksCardHTML();
  if (checks) {
    const interp = ws.querySelector(".card.interp");
    if (interp) interp.insertAdjacentHTML("beforebegin", checks); else ws.insertAdjacentHTML("beforeend", checks);
    ws.querySelectorAll(".checks-card [data-go]").forEach(a => a.onclick = ev => { ev.preventDefault(); route(a.dataset.go); });
  }
  presentWorkspace();
}

/* ------------------------- keyboard: Enter runs the analysis ------------------------- */
// In any single-line input of the input panel, Enter commits the value and presses the
// panel's main button (Run model, Calculate, Analyse, Fit curves...).
document.getElementById("sidebar").addEventListener("keydown", e => {
  if (e.key !== "Enter" || e.shiftKey || e.isComposing) return;
  const t = e.target; if (!(t instanceof HTMLInputElement) || ["checkbox", "radio", "file", "range"].includes(t.type)) return;
  const run = document.querySelector("#sidebar .divider + .btn-primary.btn-block"); if (!run) return;
  e.preventDefault(); t.blur(); setTimeout(() => run.click(), 0);
});

/* --------------------- verdict banner for every results screen --------------------- */
// kind: "ok" (teal tick), "warn" (amber), "info" (neutral). Placed under the heading.
function setVerdict(kind, title, sub) {
  const ws = document.getElementById("workspace"); if (!ws) return;
  const old = ws.querySelector(":scope > .verdict"); if (old) old.remove();
  const icon = kind === "ok" ? '<path d="M4.5 10.5l3.5 3.5 7.5-8" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"/>'
    : kind === "warn" ? '<path d="M10 5v6M10 14.5v.5" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round"/>'
    : '<path d="M10 9v5M10 6v.5" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round"/>';
  const html = `<div class="verdict ${kind === "warn" ? "no" : kind === "info" ? "info" : ""}"><span class="vi"><svg width="20" height="20" viewBox="0 0 20 20" aria-hidden="true">${icon}</svg></span><div class="vb"><div class="vt">${title}</div>${sub ? `<div class="vs">${sub}</div>` : ""}</div></div>`;
  const anchor = ws.querySelector(":scope > .scen-pills") || ws.querySelector(":scope > .ws-head") || ws.querySelector(".ws-head");
  if (anchor) anchor.insertAdjacentHTML("afterend", html); else ws.insertAdjacentHTML("afterbegin", html);
}
const atThreshold = w => Math.abs(+w - GDP_PC) < 1 ? "1× GDP per capita" : fmtINR(+w);
