/* =============================================================================
   Artha HE — guided HTA flow (flow.js)
   One path from the decision question to a submission-ready report:
   Project (question + method + reference case) → Costs → Outcomes →
   Evaluation or Model → Uncertainty → Budget impact → Report.
   Also: "Use this result" links between modules (with provenance), plain-
   language checks, and an auto-filled CHEERS 2022 checklist.
   Loaded after app.js; shares its globals (state, route, api, fmt…).
   ============================================================================= */

const OUTCOMES={
  qaly:{label:"Quality-adjusted life-years (QALYs)",method:"CUA",model:"QALY"},
  daly:{label:"Disability-adjusted life-years (DALYs) averted",method:"CUA",model:"DALY"},
  life:{label:"Life-years gained or deaths averted",method:"CEA",model:"QALY"},
  clinical:{label:"A clinical measure (cases detected, mmHg, % controlled…)",method:"CEA",model:"QALY"},
  money:{label:"Benefits valued in money",method:"CBA"},
  multiple:{label:"Several outcomes, reported side by side",method:"CCA"},
  equal:{label:"Outcomes are the same for every option",method:"CMA"}
};
const PERSPECTIVES={health:"Health system / payer",societal:"Societal",patient:"Patient / household"};
const REF_CASES={
  htain:{label:"India: HTAIn-style defaults",disc:0.03,note:"Sets 3% discounting for costs and outcomes and a threshold of 1× GDP per capita; test discounting at 0% and 5%, and use a horizon long enough to capture all differences (often lifetime). Check the details against the current HTAIn Methods Manual."},
  custom:{label:"Custom: I will set everything myself",note:"Nothing is changed automatically; set the discount rate, horizon and threshold yourself."}
};
const METHOD_TEXT={
  CUA:"Cost-utility analysis: cost per QALY gained (or DALY averted). The HTA standard.",
  CEA:"Cost-effectiveness analysis: cost per natural unit (life-year, case detected…).",
  CBA:"Cost-benefit analysis: costs and benefits both in money (net benefit, benefit–cost ratio).",
  CCA:"Cost-consequence analysis: costs and several outcomes side by side.",
  CMA:"Cost-minimisation analysis: outcomes are equal, so the cheaper option wins."
};

function proj(){if(!state.project)state.project={};return state.project;}
function markRun(k){state.runs=state.runs||{};state.runs[k]=new Date().toISOString();if(typeof syncNav==="function")syncNav();}
function ran(k){return !!(state.runs&&state.runs[k]);}
function projMethod(){const o=OUTCOMES[proj().outcome];return o?o.method:null;}
function isFull(){return proj().scale==="full";}

/* ------------------------------- sidebar -------------------------------- */
function renderProjectSidebar(){
  const p=proj(),t=(id,l,ph)=>`<div class="field"><label>${l}</label><input type="text" id="pj_${id}" value="${escHTML(p[id]||"")}" placeholder="${ph||""}"></div>`;
  const ta=(id,l,ph)=>`<div class="field"><label>${l}</label><textarea id="pj_${id}" rows="2" style="width:100%" placeholder="${ph||""}">${escHTML(p[id]||"")}</textarea></div>`;
  const sel=(id,l,opts)=>`<div class="field"><label>${l}</label><select id="pj_${id}"><option value="">Choose…</option>${Object.entries(opts).map(([k,v])=>`<option value="${k}" ${p[id]===k?"selected":""}>${typeof v==="string"?v:v.label}</option>`).join("")}</select></div>`;
  document.getElementById("sidebar").innerHTML=`<h2><span class="section-num">00</span> Your HTA project</h2>
    <p class="hint">Start here. Describe the decision in plain words: Artha recommends the method, applies the reference case, and guides you step by step to a report.</p>
    ${t("title","Project title","e.g. HPV screening vs VIA in Indian women aged 30–49")}
    ${ta("objective","Decision question / objective","What decision will this analysis inform?")}
    ${t("population","Population","Who is treated or screened?")}
    ${t("intervention","Intervention","The new option being assessed")}
    ${t("comparators","Comparator(s)","Current practice / alternatives")}
    ${sel("outcome","Main outcome",OUTCOMES)}
    ${sel("perspective","Perspective",PERSPECTIVES)}
    ${t("setting","Setting / country","e.g. India, public sector")}
    ${sel("refCase","Reference case",REF_CASES)}
    <div class="sublabel">How detailed?</div>
    <div class="seg" id="pjScale"><button data-s="quick" class="${p.scale==="quick"?"active":""}">Quick: summary totals</button><button data-s="full" class="${p.scale==="full"?"active":""}">Full: model + uncertainty</button></div>
    <p class="hint" style="margin-top:6px">Quick: enter each option's total cost and outcome; results in minutes. Full: a Markov model with probabilistic sensitivity analysis, as HTA bodies expect. You can start quick and switch later.</p>
    <details class="ex-row" style="border-style:solid;margin-top:10px"><summary style="cursor:pointer;font-weight:600;font-size:12px">Reporting details (CHEERS 2022)</summary>
      ${ta("heterogeneity","Subgroups / heterogeneity","e.g. results by age group or risk level")}
      ${ta("distributional","Equity / distributional effects","e.g. out-of-pocket burden by income group")}
      ${ta("engagement","Patient & public engagement","How patients or others affected were involved, or 'none'")}
      ${t("funding","Source of funding","")}
      ${t("conflicts","Conflicts of interest","e.g. none declared")}
    </details>
    <div class="divider"></div><button class="btn btn-primary btn-block" id="pjApply">Save &amp; apply to the analysis</button>`;
  document.querySelectorAll("[id^=pj_]").forEach(el=>el.onchange=()=>{p[el.id.slice(3)]=el.value;saveLocal();renderProject();});
  document.querySelectorAll("#pjScale button").forEach(b=>b.onclick=()=>{p.scale=b.dataset.s;saveLocal();renderProjectSidebar();renderProject();});
  document.getElementById("pjApply").onclick=()=>{applyProject();renderProject();};
}

// Push the project's choices into the analyses.
function applyProject(){
  const p=proj(),o=OUTCOMES[p.outcome],rc=REF_CASES[p.refCase];
  const done=[];
  if(o){state.evaluation.type=o.method;done.push(`method set to ${o.method}`);
    if(o.model){state.model.outcome=o.model;done.push(`model outcome set to ${o.model==="QALY"?"QALYs":"DALYs averted"}`);}}
  if(rc&&rc.disc!=null){state.model.dCost=rc.disc;state.model.dEff=rc.disc;if(state.vb&&state.vb.qaly)state.vb.qaly.discount=rc.disc;
    state.evaluation.wtp=GDP_PC;state.model.wtp=GDP_PC;state.sens.wtp=GDP_PC;
    done.push(`discounting ${pct(rc.disc,0)} and threshold 1× GDP per capita (${compactINR(GDP_PC)})`);}
  p.appliedAt=new Date().toISOString();p.appliedSummary=done.join("; ");
  saveLocal();
}

/* ------------------------------- steps ---------------------------------- */
function flowSteps(){
  const p=proj(),e=state.evaluation,m=activeModel(),full=isFull(),method=projMethod(),mk=m.type;
  const qDone=!!(p.title&&p.population&&p.intervention&&p.comparators&&p.outcome&&p.perspective&&p.scale);
  const evalCosts=(e.strats||[]).length>=2&&(e.strats||[]).some(s=>+s.cost>0);
  const modelCosts=mk==="tree"?m.strategies.some(s=>s.nodes.some(n=>+n.cost>0)):m.states.some(s=>+s.cost>0)||m.strategies.some(s=>+s.addCost>0);
  const evalEff=(e.strats||[]).length>=2&&(e.strats||[]).some(s=>+s.effect!==0);
  const modelEff=mk==="tree"?m.strategies.some(s=>s.nodes.some(n=>+n.effect>0)):m.states.some(s=>+s.util>0)||m.states.some(s=>+s.dw>0);
  const S=[];
  S.push({key:"question",n:1,title:"Define the question",mod:"project",done:qDone,need:"required",
    desc:qDone?`${escHTML(p.intervention)} vs ${escHTML(p.comparators)} in ${escHTML(p.population)}.`:"Fill in the project panel on the left: population, intervention, comparators, outcome, perspective and level of detail."});
  S.push({key:"costs",n:2,title:"Work out the costs",mod:"costing",done:ran("costing")||(full?modelCosts:evalCosts),need:"required",
    desc:"Cost each option bottom-up (micro-costing) or top-down, then use the result directly in the evaluation or model with “Use this result”."});
  S.push({key:"outcomes",n:3,title:"Build the outcomes",mod:"vb",done:ran("vb")||(full?modelEff:evalEff),need:method==="CMA"?"optional":"required",
    desc:"Derive QALYs, utilities, life-years or transition probabilities from your evidence in the Value Builder, and send them where they are needed."});
  if(full){
    if(mk==="psm")S.push({key:"survival",n:4,title:"Fit the survival curves",mod:"survival",done:ran("survival")||m.strategies.some(s=>s.os&&s.os.dist),need:"required",
      desc:"Fit and extrapolate overall and progression-free survival from patient-level data or a published Kaplan-Meier curve, then send the chosen curves to the model."});
    S.push({key:"model",n:S.length+1,title:"Build and run the model",mod:"modeling",done:ran("modeling"),need:"required",
      desc:mk==="psm"?"Partitioned survival: OS and PFS curves (or hazard ratios) per strategy, state costs and utilities. Run it to get costs, QALYs and ICERs.":mk==="tree"?"Decision tree: branches with probabilities, costs and outcomes for each strategy. Run it to get expected costs, outcomes and ICERs.":"Choose the model type (Markov, partitioned survival or decision tree), set the inputs and run it to get costs, QALYs and ICERs."});
    S.push({key:"uncertainty",n:S.length+1,title:"Test the uncertainty",mod:"sensitivity",done:ran("sensitivity"),need:"required",
      desc:"Probabilistic (PSA, CEAC, EVPI) and one-way (tornado) sensitivity analysis: how sure is the conclusion?"});
  }else{
    S.push({key:"evaluation",n:4,title:"Compare the options",mod:"evaluation",done:ran("evaluation")&&(e.strats||[]).length>=2,need:"required",
      desc:`${method?method+": e":"E"}nter each option's total cost and outcome (or send them from Costing / Value Builder) and analyse.`});
    S.push({key:"uncertainty",n:5,title:"Test the uncertainty",mod:"sensitivity",done:ran("sensitivity"),need:"optional",
      desc:"Needs a model. Switch the project to Full when you need PSA for a formal submission."});
  }
  S.push({key:"equity",n:S.length+1,title:"Patient burden & equity",mod:"oop",done:ran("oop"),need:p.perspective==="societal"||p.perspective==="patient"?"recommended":"optional",
    desc:"Out-of-pocket spending and catastrophic health expenditure: who bears the cost?"});
  S.push({key:"bia",n:S.length+1,title:"Budget impact",mod:"bia",done:ran("bia"),need:"recommended",
    desc:"Is it affordable? Annual and cumulative spend for the payer as the intervention is adopted."});
  S.push({key:"report",n:S.length+1,title:"Report & submission pack",mod:"report",done:false,need:"required",
    desc:"One report with the CHEERS 2022 checklist, ready to print, save as PDF or download as Word."});
  return S;
}

/* ------------------------------- checks --------------------------------- */
function flowChecks(){
  const p=proj(),e=state.evaluation,m=activeModel(),full=isFull(),out=[];
  const warn=(t,go)=>out.push({lvl:"warn",t,go}),info=(t,go)=>out.push({lvl:"info",t,go});
  const method=projMethod();
  if(!p.outcome)warn("Choose the main outcome so Artha can recommend the right method.","project");
  if(method&&!full&&e.type!==method)warn(`The Evaluation tab is set to ${e.type}, but your outcome suggests ${method}. Press “Save & apply”.`,"project");
  if(p.refCase==="htain"){
    if(full&&(+m.dCost!==0.03||+m.dEff!==0.03))warn(`The model discounts at ${pct(m.dCost,0)} / ${pct(m.dEff,0)}; the reference case uses 3%.`,"modeling");
    const w=full?+m.wtp:+e.wtp;if(w&&Math.abs(w-GDP_PC)>1)info(`The threshold in use (${compactINR(w)}) differs from 1× GDP per capita (${compactINR(GDP_PC)}). Fine as a scenario; report it.`,full?"modeling":"evaluation");
  }
  const lg=state.refLib&&state.refLib.gdp;
  if(!(lg&&lg.verified&&+lg.value===GDP_PC))info(`The GDP per capita behind the threshold (${compactINR(GDP_PC)}) is not the verified figure in your Reference library${lg&&lg.verified?` (${compactINR(+lg.value)}, ${escHTML(lg.year)})`:""}.`,"reference");
  if(!full&&(e.strats||[]).length===1)warn("Only one option in the Evaluation tab: add the comparator.","evaluation");
  if(full&&m.type==="psm"&&m.strategies.some(s=>s.os&&s.os.src==="manual"))info("A survival curve was entered by hand: cite its source, and note that its parameter uncertainty is not sampled in PSA (curves fitted in the Survival tab are).","modeling");
  if(full&&m.type==="tree"&&m.strategies.some(s=>s.nodes.length<2))warn("A decision-tree strategy has no branches yet.","modeling");
  if(full&&m.type!=="tree"){
    if(+m.horizon<10)info(`The model horizon is ${m.horizon} years. HTA usually needs a horizon long enough to capture all differences (often lifetime).`,"modeling");
    if(m.type==="markov"&&+m.horizon>=20&&!m.bgMortality)info("Long horizon without background (all-cause) mortality: consider switching it on with your country's life table.","modeling");
    if(m.type==="markov"&&m.bgMortality&&!(m.lifeTable&&m.lifeTable.length))info("Background mortality uses the crude illustrative table. Paste an official life table (e.g. SRS) for formal work.","modeling");
    m.states.forEach(s=>{if(!s.absorbing&&(+s.util>1||+s.util<0))warn(`Utility of “${escHTML(s.name)}” is ${s.util}; utilities should be between 0 and 1.`,"modeling");});
    if(m.type==="markov")m.strategies.forEach(s=>{if(!s.effect&&s.matrix&&s.matrix.some(r=>Math.abs(rowSum(r)-1)>0.001))warn(`A row of the “${escHTML(s.name)}” matrix does not sum to 1.`,"modeling");});
    if(ran("modeling")&&!ran("sensitivity"))warn("The model has been run but the uncertainty has not been tested (PSA). HTA bodies expect it.","sensitivity");
  }
  const c=state.costing;if(c&&c.method==="micro"&&(c.rows||[]).length&&+c.toYear<new Date().getFullYear()-1)info(`Costs are in ${c.toYear} prices. Update the price year if you need current prices.`,"costing");
  if(ran("bia")&&full&&ran("modeling")&&!(state.provenance&&Object.keys(state.provenance).some(k=>k.startsWith("bia")))) info("Check that the budget-impact costs per patient match the costs used in your model.","bia");
  const unv=Object.values(state.refUsedMap||{}).filter(x=>!/^Verified/.test(x.status));
  if(unv.length)warn(`${unv.length} reference value(s) used in the analysis are not yet verified (e.g. ${escHTML(unv[0].item)}). Check them against the source in the Reference library.`,"reference");
  if(full&&ran("sensitivity")&&!ran("scenarios")&&modelKind()!=="tree")info("Scenario analysis has not been run. Add it in Sensitivity → Scenarios to test discounting, horizon and extrapolation choices.","sensitivity");
  if(!p.funding||!p.conflicts)info("Add the funding source and conflicts of interest (CHEERS items 27–28) under “Reporting details”.","project");
  return out;
}

/* ------------------------------- CHEERS --------------------------------- */
// Status: auto (Artha supplies it), partial (Artha supplies some; add sources),
// you (the author must write it), missing (a project field is empty).
function cheersItems(){
  const p=proj(),m=activeModel(),full=isFull(),c=state.costing,mm=m.correction==="simpson"?"Simpson's 1/3":"half-cycle";
  const val=(v,txt)=>v?{s:"auto",x:txt||v}:{s:"missing",x:"Fill in on the Project panel"};
  const runOr=(k,txt,miss)=>ran(k)?{s:"auto",x:txt}:{s:"missing",x:miss};
  const methodTxt=projMethod()?METHOD_TEXT[projMethod()]:"";
  const items=[
    ["Title",val(p.title,p.title&&`${p.title} (identifies a ${projMethod()||"economic"} evaluation)`)],
    ["Abstract",{s:"you",x:"Write a structured abstract; the report's summary tables give the numbers."}],
    ["Background and objectives",val(p.objective)],
    ["Health economic analysis plan",{s:"you",x:"State whether an analysis plan was written in advance and where it is available."}],
    ["Study population",val(p.population)],
    ["Setting and location",val(p.setting)],
    ["Comparators",val(p.comparators,p.comparators&&`${p.intervention||"Intervention"} vs ${p.comparators}`)],
    ["Perspective",val(PERSPECTIVES[p.perspective])],
    ["Time horizon",full&&m.type==="tree"?{s:"you",x:"State the period over which the tree's costs and outcomes apply."}:full?runOr("modeling",`${m.horizon} years (${m.cycle}-year cycles)`,"Run the model"):{s:"you",x:"State the period over which costs and outcomes were measured."}],
    ["Discount rate",full&&m.type==="tree"?{s:"you",x:"Decision tree: state whether outcomes beyond one year were discounted."}:full?runOr("modeling",`${pct(m.dCost,1)} costs, ${pct(m.dEff,1)} outcomes${p.refCase==="htain"?" (India reference case)":""}`,"Run the model"):{s:"you",x:"State the discount rate used for costs and outcomes beyond one year."}],
    ["Selection of outcomes",p.outcome?{s:"auto",x:`${OUTCOMES[p.outcome].label}. ${methodTxt}`}:{s:"missing",x:"Choose the main outcome"}],
    ["Measurement of outcomes",(ran("vb")||ran("modeling"))?{s:"partial",x:"Derived in Artha (Value Builder / model); add the source of each clinical input."}:{s:"you",x:"Describe how outcomes were measured."}],
    ["Valuation of outcomes",(ran("vb")||ran("modeling"))?{s:"partial",x:"Utilities entered per health state; name the instrument and value set (e.g. EQ-5D-5L India)."}:{s:"you",x:"Describe how outcomes were valued."}],
    ["Measurement and valuation of resources and costs",ran("costing")?{s:"partial",x:`${c.method==="advanced"?"Activity-based":c.method==="gross"?"Top-down (gross)":"Bottom-up (micro)"} costing in Artha; cite each unit-cost source.`}:{s:"missing",x:"Run the costing"}],
    ["Currency, price date, and conversion",ran("costing")?{s:"auto",x:`Indian rupees (₹)${c.method==="micro"?`, ${c.toYear} prices, inflated at ${pct(c.inflation)} per year`:""}.`}:{s:"missing",x:"Run the costing"}],
    ["Rationale and description of model",full?runOr("modeling",m.type==="psm"?`Partitioned survival model (progression-free, progressed, dead) driven by overall and progression-free survival curves; ${m.strategies.length} strategies. Explain why this structure suits the decision.`:m.type==="tree"?`Decision tree with ${m.strategies.length} strategies. Explain why a short-term tree suits the decision.`:`${m.states.length}-state Markov cohort model (${m.states.map(s=>s.name).join(", ")}), ${m.strategies.length} strategies.`,"Run the model"):{s:"you",x:"No decision model (summary-level comparison); say why that is adequate."}],
    ["Analytics and assumptions",full&&m.type==="psm"?runOr("modeling",`${mm} within-cycle correction; survival curves ${m.strategies.some(s=>s.os&&s.os.dist)?"fitted by maximum likelihood (exponential, Weibull, Gompertz, log-logistic, log-normal; chosen on AIC/BIC and plausibility)":"entered from published sources"}; treatment effects as hazard ratios where used; PFS capped at OS.`,"Run the model"):full&&m.type==="tree"?runOr("modeling","Expected values by rolling back the tree; costs and outcomes accumulate along each path.","Run the model"):full?runOr("modeling",`${mm} within-cycle correction; ${m.bgMortality?"age-specific background mortality":"no background mortality"}; ${m.strategies.some(s=>s.effect)?"treatment effects applied as ratios to the baseline matrix":"strategy-specific transition matrices"}.`,"Run the model"):{s:"you",x:"Describe the analytical methods and assumptions."}],
    ["Characterizing heterogeneity",p.heterogeneity?{s:"auto",x:p.heterogeneity}:{s:"you",x:"Describe subgroup analyses, or state none were done."}],
    ["Characterizing distributional effects",p.distributional?{s:"auto",x:p.distributional}:ran("oop")?{s:"partial",x:"Out-of-pocket and catastrophic expenditure analysed; describe equity implications."}:{s:"you",x:"Describe effects across population groups, or state none."}],
    ["Characterizing uncertainty",ran("sensitivity")?{s:"auto",x:`Probabilistic SA (${state.sens.N} iterations, seed ${state.sens.seed}), one-way SA (tornado), value of information (EVPI, EVPPI)${ran("scenarios")?" and scenario analysis":""}.`}:{s:"missing",x:"Run the sensitivity analysis"}],
    ["Approach to engagement with patients and others affected",p.engagement?{s:"auto",x:p.engagement}:{s:"you",x:"Describe engagement, or state there was none."}],
    ["Study parameters",full&&ran("modeling")?{s:"partial",x:"States, costs, utilities and transitions are listed in the report; add the source of each."}:{s:"you",x:"Tabulate all inputs with their sources and distributions."}],
    ["Summary of main results",(ran("modeling")||ran("evaluation"))?{s:"auto",x:"Costs, outcomes, incremental results and ICERs are in the report."}:{s:"missing",x:"Run the evaluation or the model"}],
    ["Effect of uncertainty",ran("sensitivity")?{s:"auto",x:"Probability cost-effective, EVPI, CEAC and tornado in the report."}:{s:"missing",x:"Run the sensitivity analysis"}],
    ["Effect of engagement with patients and others affected",p.engagement?{s:"partial",x:"Describe how engagement changed the approach or findings."}:{s:"you",x:"Report any effect of engagement, or state none."}],
    ["Study findings, limitations, generalizability, and current knowledge",{s:"you",x:"Discuss the findings, limitations and how they compare with other studies."}],
    ["Source of funding",val(p.funding)],
    ["Conflicts of interest",val(p.conflicts)]
  ];
  return items.map((it,i)=>({n:i+1,topic:it[0],status:it[1].s,text:it[1].x}));
}
const CH_TAG={auto:['frontier','Filled by Artha'],partial:['ext','Partly filled: add sources'],you:['ext','You write this'],missing:['dominated','Missing']};

/* ------------------------------- workspace ------------------------------ */
function renderProject(){
  const p=proj(),ws=document.getElementById("workspace"),method=projMethod();
  const steps=flowSteps(),checks=flowChecks(),ch=cheersItems();
  const req=steps.filter(s=>s.need==="required"&&s.key!=="report"),nDone=req.filter(s=>s.done).length;
  const cnt=k=>ch.filter(x=>x.status===k).length;
  const stepCard=s=>{const st=s.done?["frontier","Done"]:s.need==="required"?["dominated","To do"]:s.need==="recommended"?["ext","Recommended"]:["","Optional"];
    return `<div class="flow-step ${s.done?"done":""}"><div class="fs-n">${s.done?"✓":s.n}</div><div class="fs-body"><div class="fs-top"><b>${s.title}</b> <span class="tag ${st[0]}">${st[1]}</span></div><div class="fs-desc">${s.desc}</div></div><button class="btn ${s.done?"btn-ghost":"btn-secondary"} sm" data-go="${s.mod}">${s.key==="report"?"Generate":s.done?"Review":"Go"} →</button></div>`;};
  ws.innerHTML=`<div class="ws-head"><div><h2>${p.title?escHTML(p.title):"Your HTA, step by step"}</h2><div class="sub">${p.title?`${escHTML(p.intervention||"Intervention")} vs ${escHTML(p.comparators||"comparator")} · ${escHTML(p.population||"population")} · ${escHTML(PERSPECTIVES[p.perspective]||"perspective not set")}`:"Describe your decision on the left. Artha recommends the method, sets the reference case, and walks you through each step to a submission-ready report."}</div></div></div>
    <div class="kpis">
      <div class="kpi accent"><div class="k-label">Recommended method</div><div class="k-val sm">${method?method:"—"}</div><div class="k-sub">${method?METHOD_TEXT[method].split(":")[0]:"choose the main outcome"}</div></div>
      <div class="kpi ${nDone===req.length?"emerald":"gold"}"><div class="k-label">Progress</div><div class="k-val mono">${nDone} / ${req.length}</div><div class="k-sub">required steps done</div></div>
      <div class="kpi"><div class="k-label">CHEERS 2022</div><div class="k-val mono">${cnt("auto")+cnt("partial")} / 28</div><div class="k-sub">items filled or started</div></div></div>
    ${method?`<div class="callout"><b>${method}.</b> ${METHOD_TEXT[method]} ${isFull()?"You chose the <b>full</b> route: a Markov model with probabilistic sensitivity analysis.":p.scale==="quick"?"You chose the <b>quick</b> route: compare each option's total cost and outcome.":""} ${p.refCase&&REF_CASES[p.refCase]?`<br><b>Reference case:</b> ${REF_CASES[p.refCase].note}`:""}${p.appliedAt?`<br><span style="color:var(--emerald)">✓ Applied: ${escHTML(p.appliedSummary||"")}.</span>`:""}</div>`:""}
    <div class="card"><h3>Your steps</h3><div class="card-sub">Work down the list. Each step's results can be sent to the next with “Use this result”, so nothing is typed twice.</div><div class="flow-steps">${steps.map(stepCard).join("")}</div></div>
    <div class="card" style="border-left:4px solid ${checks.some(c=>c.lvl==="warn")?"var(--amber)":"var(--emerald)"}"><h3>Checks</h3><div class="card-sub">Plain-language checks on your inputs and choices.</div>
      ${checks.length?`<ul class="checklist">${checks.map(c=>`<li><span class="${c.lvl==="warn"?"no":"ok"}" style="${c.lvl==="warn"?"color:var(--amber)":"color:var(--muted)"}">${c.lvl==="warn"?"!":"i"}</span><span>${c.t} ${c.go?`<a href="#" data-go="${c.go}" style="color:var(--primary);font-weight:600">Fix →</a>`:""}</span></li>`).join("")}</ul>`:`<p style="color:var(--emerald);font-weight:600">No issues found.</p>`}</div>
    <div class="card"><h3>CHEERS 2022 reporting checklist</h3><div class="card-sub">Filled as you work, and included in the report. ${cnt("auto")} filled by Artha · ${cnt("partial")} partly filled · ${cnt("you")} for you to write · ${cnt("missing")} missing.</div>
      <div class="table-scroll"><table class="results-table"><thead><tr><th style="text-align:left">#</th><th style="text-align:left">Item</th><th style="text-align:left">Status</th><th style="text-align:left">Content / what to add</th></tr></thead><tbody>${ch.map(x=>`<tr><td style="text-align:left">${x.n}</td><td style="text-align:left">${x.topic}</td><td style="text-align:left"><span class="tag ${CH_TAG[x.status][0]}">${CH_TAG[x.status][1]}</span></td><td style="text-align:left;white-space:normal">${escHTML(x.text)}</td></tr>`).join("")}</tbody></table></div>
      <p class="note">CHEERS 2022: Husereau D, et al. Consolidated Health Economic Evaluation Reporting Standards 2022 (CHEERS 2022) statement. <i>Value Health</i> 2022;25(1):3–9.</p></div>`;
  ws.querySelectorAll("[data-go]").forEach(b=>b.onclick=ev=>{ev.preventDefault();const g=b.dataset.go;if(g==="report")openReport();else if(g==="project")renderProjectSidebar();else route(g);});
}

/* ---------------------- "Use this result" links ------------------------- */
// kind: cost | effect | utility | prob. Records provenance for the report.
function useDestinations(kind){
  const e=state.evaluation,mk=modelKind(),m=modelStore(),d=[];
  const sn=x=>escHTML(x||"");
  if(kind==="cost"){
    (e.strats||[]).forEach((s,i)=>d.push([`eval-cost:${i}`,`Evaluation · cost of “${sn(s.strategy)}”`]));
    d.push(["eval-new-cost","Evaluation · add as a new option"]);
    if(mk!=="tree"){m.states.forEach((s,i)=>{if(!s.absorbing)d.push([`model-state-cost:${i}`,`Model · annual cost in “${sn(s.name)}”`]);});
    m.strategies.forEach((s,i)=>d.push([`model-add-cost:${i}`,`Model · annual treatment cost of “${sn(s.name)}”`]));}
    d.push(["bia-costNew","Budget impact · new treatment cost per patient-year"],["bia-costOld","Budget impact · current treatment cost per patient-year"]);
  }else if(kind==="effect"){
    (e.strats||[]).forEach((s,i)=>d.push([`eval-effect:${i}`,`Evaluation · outcome of “${sn(s.strategy)}”`]));
    d.push(["eval-new-effect","Evaluation · add as a new option"]);
  }else if(kind==="utility"){
    if(mk!=="tree")m.states.forEach((s,i)=>{if(!s.absorbing)d.push([`model-state-util:${i}`,`Model · utility of “${sn(s.name)}”`]);});
  }else if(kind==="prob"){
    if(mk==="markov")m.strategies.forEach((s,k)=>{if(s.effect)return;m.states.forEach((a,i)=>{if(a.absorbing)return;m.states.forEach((b,j)=>{if(i!==j)d.push([`model-p:${k}:${i}:${j}`,`Model · “${sn(s.name)}”: ${sn(a.name)} → ${sn(b.name)}`]);});});});
  }
  return d;
}
function applyUse(dest,value,from){
  const e=state.evaluation,m=modelStore(),b=state.bia,[k,...a]=dest.split(":"),i=+a[0];
  let label="",go=null;
  switch(k){
    case "eval-cost":e.strats[i].cost=+value.toFixed(2);label=`cost of “${e.strats[i].strategy}”`;go="evaluation";break;
    case "eval-effect":e.strats[i].effect=+value.toFixed(4);label=`outcome of “${e.strats[i].strategy}”`;go="evaluation";break;
    case "eval-new-cost":e.strats.push({strategy:"Option "+(e.strats.length+1),cost:+value.toFixed(2),effect:0});label=`cost of a new option`;dest=`eval-cost:${e.strats.length-1}`;go="evaluation";break;
    case "eval-new-effect":e.strats.push({strategy:"Option "+(e.strats.length+1),cost:0,effect:+value.toFixed(4)});label=`outcome of a new option`;dest=`eval-effect:${e.strats.length-1}`;go="evaluation";break;
    case "model-state-cost":m.states[i].cost=+value.toFixed(2);label=`cost in state “${m.states[i].name}”`;go="modeling";break;
    case "model-add-cost":m.strategies[i].addCost=+value.toFixed(2);label=`added cost of “${m.strategies[i].name}”`;go="modeling";break;
    case "model-state-util":m.states[i].util=+value.toFixed(4);label=`utility of “${m.states[i].name}”`;go="modeling";break;
    case "model-p":{const s=m.strategies[i],r=+a[1],c=+a[2],P=s.matrix,old=+P[r][c],nd=+P[r][r]+old-value;
      if(nd<-1e-9)return {ok:false,msg:`That would make the probability of staying in “${m.states[r].name}” negative (${nd.toFixed(4)}). Lower other transitions in that row first.`};
      P[r][c]=+value.toFixed(6);P[r][r]=+Math.max(0,nd).toFixed(6);label=`${m.states[r].name} → ${m.states[c].name} in “${s.name}” (stay-probability adjusted to ${P[r][r].toFixed(4)})`;go="modeling";break;}
    case "bia-costNew":b.costNew=Math.round(value);label="new-treatment cost in budget impact";go="bia";break;
    case "bia-costOld":b.costOld=Math.round(value);label="current-treatment cost in budget impact";go="bia";break;
    default:return {ok:false,msg:"Unknown destination."};
  }
  state.provenance=state.provenance||{};
  state.provenance[dest]={target:label,from,value,at:new Date().toISOString()};
  saveLocal();
  return {ok:true,msg:`Used as the ${escHTML(label)}.`,go};
}
// Card appended to a result page. fmt formats the value for display.
function useCard(kind,value,what,from,fmt){
  if(!(isFinite(value)))return "";
  const d=useDestinations(kind),id="use"+Math.random().toString(36).slice(2,8);
  if(!d.length)return "";
  setTimeout(()=>{const go=document.getElementById(id+"_go");if(!go)return;go.onclick=()=>{
    const r=applyUse(document.getElementById(id+"_dest").value,value,from),msg=document.getElementById(id+"_msg");
    msg.innerHTML=r.ok?`<span style="color:var(--emerald);font-weight:600">✓ ${r.msg}</span> ${r.go?`<a href="#" id="${id}_open" style="color:var(--primary);font-weight:600">Open ${r.go==="bia"?"Budget impact":r.go[0].toUpperCase()+r.go.slice(1)} →</a>`:""}`:`<span style="color:var(--red)">${r.msg}</span>`;
    const o=document.getElementById(id+"_open");if(o)o.onclick=ev=>{ev.preventDefault();route(r.go);};
    if(r.ok){const sel=document.getElementById(id+"_dest"),cur=sel.value;sel.innerHTML=useDestinations(kind).map(([v,l])=>`<option value="${v}">${l}</option>`).join("");if([...sel.options].some(o=>o.value===cur))sel.value=cur;}
  };},0);
  return `<div class="card use-card" style="border-left:4px solid var(--primary)"><h3>Use this result</h3><div class="card-sub">Send ${what} (<b>${fmt?fmt(value):value}</b>) straight to the step that needs it. Artha records where it came from for the report.</div>
    <div style="display:flex;gap:8px;flex-wrap:wrap;align-items:center"><select id="${id}_dest" style="flex:1;min-width:240px">${d.map(([v,l])=>`<option value="${v}">${l}</option>`).join("")}</select><button class="btn btn-primary sm" id="${id}_go">Use it</button></div><div id="${id}_msg" style="margin-top:8px;font-size:12.5px"></div></div>`;
}
// Model results → Evaluation (one option per strategy).
function modelToEvalCard(rows,outcome){
  setTimeout(()=>{const b=document.getElementById("m2e");if(!b)return;b.onclick=()=>{
    state.evaluation.type="CUA";
    state.evaluation.strats=rows.map(r=>({strategy:r.name,cost:+r.cost.toFixed(2),effect:+(outcome==="DALY"?-r.daly:r.qaly).toFixed(4)}));
    state.provenance=state.provenance||{};
    rows.forEach((r,i)=>{state.provenance[`eval-cost:${i}`]={target:`cost and outcome of “${r.name}”`,from:"Markov model",value:r.cost,at:new Date().toISOString()};});
    saveLocal();route("evaluation");};},0);
  return `<div class="card use-card" style="border-left:4px solid var(--primary)"><h3>Use these results</h3><div class="card-sub">Send each strategy's discounted cost and ${outcome==="DALY"?"DALYs (as negative values, so higher is better)":"QALYs"} to the Evaluation tab as the options to compare.</div><button class="btn btn-primary sm" id="m2e">Send all strategies to Evaluation →</button></div>`;
}

/* ----------------------- report payload extras -------------------------- */
function reportPayload(){
  const p=proj();
  return Object.assign({},state,{
    project:Object.assign({},p,{outcomeLabel:OUTCOMES[p.outcome]?OUTCOMES[p.outcome].label:"",perspectiveLabel:PERSPECTIVES[p.perspective]||"",method:projMethod()||"",refCaseLabel:REF_CASES[p.refCase]?REF_CASES[p.refCase].label:""}),
    model:activeModel(),
    scenarioSet:typeof scenarioPayload==="function"?scenarioPayload():null,
    refUsed:Object.values(state.refUsedMap||{}),
    cheers:cheersItems().map(x=>({n:x.n,topic:x.topic,status:CH_TAG[x.status][1],text:x.text})),
    provenanceList:Object.values(state.provenance||{}).map(v=>({target:v.target,from:v.from,at:(v.at||"").slice(0,10)}))
  });
}
