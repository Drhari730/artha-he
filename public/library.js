/* =============================================================================
   Artha HE — reference-data library and scenario analysis (library.js)
   - An editable, shareable library of reference values (GDP per capita, CPI
     series, life tables, unit costs, utilities), each with its source and a
     "verified" flag, and one-click "Use" into the analysis.
   - Scenario analysis: save versions of the model, generate the standard HTA
     scenarios, run them side by side.
   Loaded after models.js; shares app.js globals.
   ============================================================================= */

/* ------------------------------ library ---------------------------------- */
function refLib(){
  if(!state.refLib){
    const t=REFDATA.thresholds;
    state.refLib={
      gdp:{value:GDP_DEFAULT,year:"2023–24",source:"MoSPI / World Bank",url:"https://www.mospi.gov.in",verified:false,verifiedOn:""},
      cpi:{rows:[],source:"Labour Bureau CPI-IW / MoSPI CPI (Health)",url:"https://labourbureau.gov.in",verified:false,verifiedOn:""},
      lifeTables:[],
      unitCosts:REFDATA.unitCosts.map(u=>Object.assign({},u,{source:REFDATA.unitCostSource.name,year:2024,verified:false})),
      utilities:REFDATA.weights.map(w=>({state:w.state,util:w.util,dw:w.dw,source:w.basis,verified:false})),
      notes:t.map(r=>({item:r.item,value:r.value,basis:r.basis,source:r.source,url:r.url}))
    };
  }
  return state.refLib;
}
const vBadge=(v,on)=>v?`<span class="tag frontier">Verified${on?" "+escHTML(on):""}</span>`:`<span class="tag ext">Indicative: verify</span>`;
function noteRefUse(item,value,source,verified){
  state.refUsedMap=state.refUsedMap||{};
  state.refUsedMap[item]={item,value:String(value),source:source||"",status:verified?"Verified":"Indicative (not verified)"};
  saveLocal();
}
function today(){return new Date().toISOString().slice(0,10);}

function renderReference(){
  const L=refLib(),ws=document.getElementById("workspace");
  const verRow=(key)=>`<label style="display:inline-flex;gap:6px;align-items:center;text-transform:none;letter-spacing:0;font-size:12px;margin:0"><input type="checkbox" data-ver="${key}" ${(key==="gdp"?L.gdp:L.cpi).verified?"checked":""} style="width:auto">I have checked this against the source</label>`;
  const lt=L.lifeTables.map((t,i)=>`<div class="ex-row" style="border-style:solid"><div style="display:flex;gap:8px;flex-wrap:wrap;align-items:center;justify-content:space-between"><b>${escHTML(t.name)}</b> ${vBadge(t.verified,t.verifiedOn)}</div>
      <div class="card-sub" style="margin:4px 0">${t.rows.length} age groups · ${escHTML(t.source||"source not given")}</div>
      <div class="btn-row"><button class="btn btn-primary sm" data-ltuse="${i}">Use as background mortality in the Markov model</button><label style="display:inline-flex;gap:6px;align-items:center;text-transform:none;letter-spacing:0;font-size:12px;margin:0"><input type="checkbox" data-ltver="${i}" ${t.verified?"checked":""} style="width:auto">Verified</label><button class="btn btn-ghost sm" data-ltdel="${i}">Remove</button></div></div>`).join("");
  const uc=L.unitCosts.map((u,i)=>`<tr><td><input data-uc="item" data-i="${i}" value="${escHTML(u.item)}"></td><td><input data-uc="value" data-i="${i}" type="number" value="${u.value}" style="text-align:right"></td><td><input data-uc="unit" data-i="${i}" value="${escHTML(u.unit||"")}"></td><td><input data-uc="year" data-i="${i}" type="number" value="${u.year||""}" style="text-align:right;width:70px"></td><td><input data-uc="source" data-i="${i}" value="${escHTML(u.source||"")}"></td><td style="text-align:center"><input type="checkbox" data-ucver="${i}" ${u.verified?"checked":""}></td><td><button class="btn btn-ghost sm" data-ucuse="${i}" style="padding:4px 8px">Add to costing</button></td><td class="row-del" data-ucdel="${i}">&times;</td></tr>`).join("");
  const ut=L.utilities.map((u,i)=>`<tr><td><input data-ut="state" data-i="${i}" value="${escHTML(u.state)}"></td><td><input data-ut="util" data-i="${i}" type="number" step="0.01" value="${u.util}" style="text-align:right"></td><td><input data-ut="dw" data-i="${i}" type="number" step="0.01" value="${u.dw}" style="text-align:right"></td><td><input data-ut="source" data-i="${i}" value="${escHTML(u.source||"")}"></td><td style="text-align:center"><input type="checkbox" data-utver="${i}" ${u.verified?"checked":""}></td><td class="row-del" data-utdel="${i}">&times;</td></tr>`).join("");
  ws.innerHTML=`<div class="home">
    <div class="ws-head"><div><h2>Reference data library</h2><div class="sub">Your own library of reference values: edit them, record the source, mark each as verified, and send it straight into the analysis. It is saved with the project; export it to share one verified library across your team.</div></div>
      <div style="display:flex;gap:8px;flex-wrap:wrap"><button class="btn btn-secondary sm" id="libExport">⤓ Export library</button><label class="btn btn-ghost sm" style="margin:0">⤒ Import library<input type="file" id="libImport" accept=".json" hidden></label></div></div>
    <div class="callout" style="background:var(--gold-soft);color:#8A6712"><b>Values marked “Indicative” are starting points, not official figures.</b> Confirm each against its source (MoSPI, SRS, Labour Bureau, NHSRC/CGHS, GBD…) and tick “verified”. The report lists every reference value used and whether it was verified.</div>

    <div class="two-col">
      <div class="card"><h3>GDP per capita (threshold) ${vBadge(L.gdp.verified,L.gdp.verifiedOn)}</h3><div class="card-sub">Sets the 1× / 3× GDP-per-capita willingness-to-pay markers.</div>
        <div class="field two"><div><label>Value (₹)</label><input type="number" id="libGdp" value="${L.gdp.value}"></div><div><label>Year</label><input type="text" id="libGdpYr" value="${escHTML(L.gdp.year)}"></div></div>
        <div class="field"><label>Source</label><input type="text" id="libGdpSrc" value="${escHTML(L.gdp.source)}"></div>
        <div class="btn-row">${verRow("gdp")}<button class="btn btn-primary sm" id="libGdpUse">Use as the threshold</button></div></div>
      <div class="card"><h3>Consumer price index ${vBadge(L.cpi.verified,L.cpi.verifiedOn)}</h3><div class="card-sub">Converts costs between price years by the index ratio (better than a flat inflation rate). One “year, index” per line.</div>
        <textarea id="libCpi" rows="5" style="font-family:var(--mono);font-size:11px" placeholder="2019, 100\n2020, 105.2\n…">${L.cpi.rows.map(r=>`${r.year}, ${r.index}`).join("\n")}</textarea>
        <div class="field" style="margin-top:8px"><label>Source</label><input type="text" id="libCpiSrc" value="${escHTML(L.cpi.source)}"></div>
        <div class="btn-row">${verRow("cpi")}<button class="btn btn-primary sm" id="libCpiUse" ${L.cpi.rows.length<2?"disabled":""}>Use in costing</button></div></div>
    </div>

    <div class="card"><h3>Life tables</h3><div class="card-sub">For age-specific background mortality. Paste an official table (e.g. SRS abridged life table for India, by sex): one “age, annual probability of death (q)” per line.</div>
      ${lt||'<p class="hint">No life tables yet.</p>'}
      <details class="ex-row" style="border-style:solid" ${L.lifeTables.length?"":"open"}><summary style="cursor:pointer;font-weight:600;font-size:12px">Add a life table</summary>
        <div class="field two" style="margin-top:8px"><div><label>Name</label><input type="text" id="ltName" placeholder="e.g. India SRS 2016–20, female"></div><div><label>Source</label><input type="text" id="ltSrc" placeholder="Office of the Registrar General, SRS"></div></div>
        <div class="field"><label>Age, q (one per line)</label><textarea id="ltRows" rows="6" style="font-family:var(--mono);font-size:11px" placeholder="0, 0.03\n1, 0.002\n5, 0.001\n…"></textarea></div>
        <button class="btn btn-secondary sm" id="ltAdd">Add life table</button></details></div>

    <div class="card"><h3>Unit costs (₹)</h3><div class="card-sub">Edit, add your own, and send any line to micro-costing. Tick “verified” once checked against the rate list.</div>
      <div class="grid-wrap" style="overflow-x:auto"><table class="data-grid" style="min-width:900px"><thead><tr><th style="width:28%">Item</th><th>₹</th><th>Per</th><th>Price yr</th><th>Source</th><th>Verified</th><th></th><th></th></tr></thead><tbody>${uc}</tbody></table></div>
      <button class="btn btn-secondary sm" id="ucAdd">+ Unit cost</button></div>

    <div class="card"><h3>Utilities &amp; disability weights</h3><div class="card-sub">Use the specific value for your condition and instrument (e.g. EQ-5D-5L India value set; GBD disability weights).</div>
      <div class="grid-wrap" style="overflow-x:auto"><table class="data-grid" style="min-width:620px"><thead><tr><th>Health state</th><th>Utility</th><th>Disability wt</th><th>Source</th><th>Verified</th><th></th></tr></thead><tbody>${ut}</tbody></table></div>
      <button class="btn btn-secondary sm" id="utAdd">+ Health state</button></div>

    <div class="card"><h3>Reference-case notes</h3><div class="table-scroll"><table class="results-table"><thead><tr><th style="text-align:left">Parameter</th><th style="text-align:left">Value</th><th style="text-align:left">Basis</th><th style="text-align:left">Source</th></tr></thead><tbody>${L.notes.map(r=>`<tr><td style="text-align:left">${escHTML(r.item)}</td><td style="text-align:left">${escHTML(r.value)}</td><td style="text-align:left">${escHTML(r.basis)}</td><td style="text-align:left"><a href="${escHTML(r.url)}" target="_blank" rel="noopener">${escHTML(r.source)} ↗</a></td></tr>`).join("")}</tbody></table></div></div>
    <footer class="foot">Artha HE · reference data library · values marked “Indicative” must be verified before formal use</footer></div>`;
  const re=()=>{saveLocal();setTimeout(renderReference,0);};  // defer: a change event can fire while focus is moving
  const val=id=>document.getElementById(id).value;
  document.getElementById("libGdp").onchange=e=>{L.gdp.value=+e.target.value;L.gdp.verified=false;re();};
  document.getElementById("libGdpYr").onchange=e=>{L.gdp.year=e.target.value;saveLocal();};
  document.getElementById("libGdpSrc").onchange=e=>{L.gdp.source=e.target.value;saveLocal();};
  document.getElementById("libGdpUse").onclick=()=>{if(!(+L.gdp.value>0))return;state.settings.gdp=+L.gdp.value;syncGdp();noteRefUse("GDP per capita ("+L.gdp.year+")",fmtINR(L.gdp.value),L.gdp.source,L.gdp.verified);alert("Threshold set to 1× GDP per capita = "+fmtINR(L.gdp.value)+". Update the WTP sliders in Evaluation / Modeling / Sensitivity with the 1× GDP chip.");re();};
  document.getElementById("libCpi").onchange=e=>{L.cpi.rows=parseRows(e.target.value,["year","index"]);L.cpi.verified=false;re();};
  document.getElementById("libCpiSrc").onchange=e=>{L.cpi.source=e.target.value;saveLocal();};
  document.getElementById("libCpiUse").onclick=()=>{state.costing.useCpi=true;noteRefUse("Consumer price index ("+L.cpi.rows.length+" years)",L.cpi.rows.map(r=>r.year).join(", "),L.cpi.source,L.cpi.verified);route("costing");};
  document.querySelectorAll("[data-ver]").forEach(el=>el.onchange=()=>{const o=el.dataset.ver==="gdp"?L.gdp:L.cpi;o.verified=el.checked;o.verifiedOn=el.checked?today():"";re();});
  document.getElementById("ltAdd").onclick=()=>{const rows=parseLifeTable(val("ltRows"));if(rows.length<2){alert("Paste at least two rows of “age, q”.");return;}if(rows.some(r=>r.q<0||r.q>1)){alert("q must be between 0 and 1 (an annual probability of death).");return;}
    L.lifeTables.push({name:val("ltName")||"Life table "+(L.lifeTables.length+1),source:val("ltSrc"),rows,verified:false,verifiedOn:""});re();};
  document.querySelectorAll("[data-ltuse]").forEach(b=>b.onclick=()=>{const t=L.lifeTables[+b.dataset.ltuse],m=state.model;m.lifeTable=t.rows;m.lifeTableText=t.rows.map(r=>`${r.age}, ${r.q}`).join("\n");m.bgMortality=true;
    noteRefUse("Life table: "+t.name,t.rows.length+" age groups",t.source,t.verified);state.model.type="markov";route("modeling");});
  document.querySelectorAll("[data-ltver]").forEach(el=>el.onchange=()=>{const t=L.lifeTables[+el.dataset.ltver];t.verified=el.checked;t.verifiedOn=el.checked?today():"";re();});
  document.querySelectorAll("[data-ltdel]").forEach(el=>el.onclick=()=>{L.lifeTables.splice(+el.dataset.ltdel,1);re();});
  document.querySelectorAll("[data-uc]").forEach(el=>el.onchange=()=>{const u=L.unitCosts[+el.dataset.i],f=el.dataset.uc;u[f]=f==="value"||f==="year"?+el.value:el.value;u.verified=false;re();});
  document.querySelectorAll("[data-ucver]").forEach(el=>el.onchange=()=>{L.unitCosts[+el.dataset.ucver].verified=el.checked;saveLocal();});
  document.querySelectorAll("[data-ucdel]").forEach(el=>el.onclick=()=>{L.unitCosts.splice(+el.dataset.ucdel,1);re();});
  document.querySelectorAll("[data-ucuse]").forEach(el=>el.onclick=()=>{const u=L.unitCosts[+el.dataset.ucuse],c=state.costing;c.method="micro";c.rows.push({item:u.item,category:u.category||"Direct medical",quantity:1,unit_cost:u.value,year:u.year||c.toYear});
    noteRefUse("Unit cost: "+u.item,fmtINR(u.value)+(u.unit?" "+u.unit:""),u.source,u.verified);el.textContent="✓ Added";});
  document.getElementById("ucAdd").onclick=()=>{L.unitCosts.push({item:"New item",value:0,unit:"per unit",category:"Direct medical",year:new Date().getFullYear(),source:"",verified:false});re();};
  document.querySelectorAll("[data-ut]").forEach(el=>el.onchange=()=>{const u=L.utilities[+el.dataset.i],f=el.dataset.ut;u[f]=f==="util"||f==="dw"?+el.value:el.value;u.verified=false;re();});
  document.querySelectorAll("[data-utver]").forEach(el=>el.onchange=()=>{L.utilities[+el.dataset.utver].verified=el.checked;saveLocal();});
  document.querySelectorAll("[data-utdel]").forEach(el=>el.onclick=()=>{L.utilities.splice(+el.dataset.utdel,1);re();});
  document.getElementById("utAdd").onclick=()=>{L.utilities.push({state:"New state",util:0.8,dw:0.1,source:"",verified:false});re();};
  document.getElementById("libExport").onclick=()=>dl(new Blob([JSON.stringify(L,null,2)],{type:"application/json"}),"ArthaHE_reference_library.json");
  document.getElementById("libImport").onchange=e=>{const f=e.target.files[0];if(!f)return;const rd=new FileReader();rd.onload=()=>{try{const o=scrub(JSON.parse(rd.result));if(!o||typeof o!=="object"||!o.gdp)throw 0;state.refLib=Object.assign(refLib(),o);re();}catch(err){alert("That is not an Artha HE reference library file.");}};rd.readAsText(f);};
}

/* ------------------------------ scenarios -------------------------------- */
function scenState(){if(!state.scenarios)state.scenarios={list:[],includeStandard:true};return state.scenarios;}
// Convert a saved copy of state.model into the model sent to the API.
function toApiModel(snap){const keep=state.model;state.model=snap;try{return activeModel();}finally{state.model=keep;}}
function scenarioPayload(){
  const S=scenState(),s=state.sens;
  return {base:activeModel(),scenarios:S.list.map(x=>({name:x.name,model:toApiModel(x.model)})),includeStandard:S.includeStandard,wtp:s.wtp,ref:s.ref,cmp:s.cmp};
}
function renderScenarioSidebar(){
  const S=scenState(),am=activeModel(),k=am.type;
  const items=S.list.map((x,i)=>`<tr><td style="font-size:12px;padding:6px 8px">${escHTML(x.name)}<div style="font-size:10.5px;color:var(--muted)">${escHTML(x.note||"")}</div></td><td style="white-space:nowrap"><button class="btn btn-ghost sm" data-scload="${i}" style="padding:4px 8px">Load</button></td><td class="row-del" data-scdel="${i}">&times;</td></tr>`).join("");
  const fits=k==="psm"?fittedCurves():[];
  return `<div class="sublabel">Scenario analysis</div>
    <p class="hint">Alternative assumptions run side by side with the base case (your current model). Change the model, save it as a scenario, then switch back, or let Artha generate the standard ones.</p>
    <label style="display:flex;gap:6px;align-items:center;text-transform:none;letter-spacing:0;font-size:12.5px;margin:6px 0"><input type="checkbox" id="scStd" ${S.includeStandard?"checked":""} style="width:auto">Include standard scenarios${k==="tree"?" (none for decision trees)":" (discounting 0% / 5%, outcomes undiscounted, horizon halved, other within-cycle correction"+(k==="markov"?", background mortality":"")+")"}</label>
    <div class="field two"><div><label>Save the current model as</label><input type="text" id="scName" placeholder="e.g. Price cut 20%"></div><div style="display:flex;align-items:flex-end"><button class="btn btn-secondary sm" id="scSave" style="width:100%">Save scenario</button></div></div>
    ${k==="psm"&&fits.length?`<button class="btn btn-ghost sm" id="scCurves">+ A scenario for each alternative fitted curve</button>`:""}
    ${items?`<div class="grid-wrap" style="margin-top:8px"><table class="data-grid"><tbody>${items}</tbody></table></div>`:'<p class="hint">No saved scenarios yet.</p>'}
    <div class="divider"></div><button class="btn btn-primary btn-block" id="scRun">Run scenarios</button>`;
}
function wireScenarioSidebar(){
  const S=scenState();
  document.getElementById("scStd").onchange=e=>{S.includeStandard=e.target.checked;saveLocal();};
  document.getElementById("scSave").onclick=()=>{const n=document.getElementById("scName").value.trim()||"Scenario "+(S.list.length+1);
    S.list.push({name:n,note:`${modelKind()==="psm"?"Partitioned survival":modelKind()==="tree"?"Decision tree":"Markov"} model saved ${today()}`,model:JSON.parse(JSON.stringify(state.model))});saveLocal();renderSensSidebar();};
  document.querySelectorAll("[data-scdel]").forEach(el=>el.onclick=()=>{S.list.splice(+el.dataset.scdel,1);saveLocal();renderSensSidebar();});
  document.querySelectorAll("[data-scload]").forEach(el=>el.onclick=()=>{if(!confirm("Replace the current model with this scenario? (Save the current one first if you need it.)"))return;state.model=JSON.parse(JSON.stringify(S.list[+el.dataset.scload].model));saveLocal();route("modeling");});
  const cv=document.getElementById("scCurves");if(cv)cv.onclick=()=>{
    const P=state.model.psm,fits=fittedCurves();let added=0;
    P.strategies.forEach((s,si)=>["os","pfs"].forEach(ep=>{const c=s[ep];if(!c||c.src!=="fit")return;const arm=c.label.split(",")[0];
      fits.filter(f=>f.spec.label.startsWith(arm+",")&&f.spec.dist!==c.dist).forEach(f=>{const snap=JSON.parse(JSON.stringify(state.model));snap.psm.strategies[si][ep]=Object.assign({src:"fit"},f.spec);
        S.list.push({name:`${s.name} ${ep.toUpperCase()}: ${SURV_LABEL[f.spec.dist]}`,note:`Alternative extrapolation (base: ${SURV_LABEL[c.dist]})`,model:snap});added++;});}));
    if(!added)alert("No alternative fitted curves found: the model's curves must come from the Survival tab (“Fitted curve”).");saveLocal();renderSensSidebar();};
  document.getElementById("scRun").onclick=renderScenarios;
}
async function renderScenarios(){
  const ws=document.getElementById("workspace"),am=activeModel();
  if(!modelReady(am))return emptyState("Scenario analysis","Set up and run your model in the Modeling tab first; it is the base case for every scenario.");
  wsBusy("Running scenarios");
  let R;try{R=await api("scenarios",scenarioPayload());}catch(e){return wsError(e);}
  markRun("scenarios");
  const rows=R.results,base=rows[0],unit=R.unit;
  const ok=rows.filter(r=>r.ok);
  const bars=ok.filter(r=>r.icer!=null).map((r,i)=>({label:r.name.length>34?r.name.slice(0,33)+"…":r.name,value:Math.max(0,r.icer),tag:r.icer<0?"dominant / dominated":compactINR(r.icer),color:i===0?C.ink:r.icer<=R.wtp?C.emerald:C.amber}));
  const tr=rows.map(r=>r.ok?`<tr><td style="text-align:left">${escHTML(r.name)}</td><td>${fmtINR(r.incCost)}</td><td>${fmtNum(r.incEff,3)}</td><td>${r.icer==null?"—":fmtINR(r.icer)}</td><td>${compactINR(r.inmb)}</td><td style="text-align:left">${escHTML(r.best)}${base.ok&&r.best!==base.best?' <span class="tag ext">changes</span>':""}</td></tr>`:`<tr><td style="text-align:left">${escHTML(r.name)}</td><td colspan="5" style="text-align:left;color:var(--red)">Could not run: ${escHTML(r.error)}</td></tr>`).join("");
  const changed=ok.filter(r=>base.ok&&r.best!==base.best).length;
  ws.innerHTML=`<div class="ws-head"><div><h2>Scenario analysis</h2><div class="sub">${base.ok?`<b>${escHTML(base.cmpName)}</b> vs <b>${escHTML(base.refName)}</b> (pick the pair in “Probabilistic & one-way”)`:""}, willingness-to-pay ${compactINR(R.wtp)} per ${unit}.</div></div>${EXPORT_BAR}</div>
    <div class="kpis"><div class="kpi accent"><div class="k-label">Scenarios run</div><div class="k-val mono">${rows.length}</div><div class="k-sub">including the base case</div></div><div class="kpi ${changed?"gold":"emerald"}"><div class="k-label">Decision changes in</div><div class="k-val mono">${changed}</div><div class="k-sub">scenario(s)</div></div><div class="kpi"><div class="k-label">Base-case ICER</div><div class="k-val mono">${base.ok&&base.icer!=null?compactINR(base.icer):"—"}</div><div class="k-sub">per ${unit}</div></div></div>
    <div class="result-tabs" id="scTabs"><button class="result-tab active" data-p="tbl">Results</button><button class="result-tab" data-p="chart">ICER by scenario</button></div>
    <div class="pane active" data-pane="tbl"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th style="text-align:left">Scenario</th><th>Δ Cost</th><th>Δ ${unit}</th><th>ICER</th><th>Incremental NMB</th><th style="text-align:left">Optimal strategy</th></tr></thead><tbody>${tr}</tbody></table></div></div></div>
    <div class="pane" data-pane="chart"><div class="card flush"><div class="pad"><h3>ICER in each scenario</h3><div class="card-sub">Black: base case. Green: below the threshold; amber: above it.</div>${bars.length?barChartH(bars):'<p class="hint">No ICERs to show.</p>'}</div></div></div>`;
  wireTabs("scTabs");wireExports("Artha Scenarios");
  addInterp({interpretation:[changed?`The optimal strategy changes in ${changed} scenario(s): the conclusion depends on those assumptions, so report them prominently.`:"The optimal strategy is the same in every scenario: the conclusion is robust to these structural and methodological choices.","Scenario analysis tests structural and methodological uncertainty (discounting, horizon, extrapolation, model choices) that probabilistic sensitivity analysis does not capture."]});
}
