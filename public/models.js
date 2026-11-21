/* =============================================================================
   Artha HE — model types for large HTAs (models.js)
   Survival curve fitting (IPD or digitised KM via Guyot), partitioned survival
   models, decision trees, and multi-strategy CEAC / CEAF charts.
   Loaded after app.js; shares its globals.
   ============================================================================= */

const SURV_LABEL={exponential:"Exponential",weibull:"Weibull",gompertz:"Gompertz",llogis:"Log-logistic",lnorm:"Log-normal"};
const SURV_PARAMS={exponential:["rate"],weibull:["shape","scale"],gompertz:["shape","rate"],llogis:["shape","scale"],lnorm:["meanlog","sdlog"]};
const UNITS={days:"days",weeks:"weeks",months:"months",years:"years"};

/* ------------------------------ model store ------------------------------- */
function modelKind(){return state.model.type||"markov";}
function ensureAltModels(){
  const m=state.model;
  if(!m.psm)m.psm={active:0,states:[{name:"Progression-free",cost:0,util:0,dw:0},{name:"Progressed",cost:0,util:0,dw:0},{name:"Dead",cost:0,util:0,dw:0,absorbing:true}],
    strategies:[{name:"Comparator",addCost:0,costOn:"pf",os:{src:"manual",dist:"weibull",par:{},unit:"months"},pfs:{src:"manual",dist:"weibull",par:{},unit:"months"}},
                {name:"Intervention",addCost:0,costOn:"pf",os:{src:"hr",ref:0,hr:1,lo:"",hi:""},pfs:{src:"hr",ref:0,hr:1,lo:"",hi:""}}]};
  if(!m.tree)m.tree={active:0,strategies:[{name:"Strategy A",nodes:[{id:"n1",parent:"",label:"Start",p:"",cost:0,effect:0}]},{name:"Strategy B",nodes:[{id:"n1",parent:"",label:"Start",p:"",cost:0,effect:0}]}]};
}
// The object that holds the states / strategies being edited.
function modelStore(){ensureAltModels();const k=modelKind();return k==="psm"?state.model.psm:k==="tree"?state.model.tree:state.model;}
function cleanCurve(c){
  if(!c)return null;
  if(c.src==="hr")return {ref:+c.ref,hr:+c.hr,lo:c.lo===""||c.lo==null?null:+c.lo,hi:c.hi===""||c.hi==null?null:+c.hi};
  const o={dist:c.dist,par:{},unit:c.unit||"months"};(SURV_PARAMS[c.dist]||[]).forEach(k=>o.par[k]=c.par&&c.par[k]!==""&&c.par[k]!=null?+c.par[k]:null);
  if(c.src==="fit"&&c.theta&&c.vcov){o.theta=c.theta;o.vcov=c.vcov;}
  return o;
}
// The model as sent to the API.
function activeModel(){
  ensureAltModels();const m=state.model,k=modelKind();
  if(k==="psm"){const p=m.psm;return Object.assign({},m,{type:"psm",activeStrat:p.active,states:p.states,
    strategies:p.strategies.map(s=>({name:s.name,addCost:+s.addCost||0,addCostStates:s.costOn==="alive"?[0,1]:[0],os:cleanCurve(s.os),pfs:cleanCurve(s.pfs)})),psm:undefined,tree:undefined});}
  if(k==="tree"){const t=m.tree;return Object.assign({},m,{type:"tree",activeStrat:t.active,states:[],strategies:t.strategies.map(s=>({name:s.name,nodes:s.nodes.map(n=>({id:n.id,parent:n.parent,label:n.label,p:String(n.p).trim()==="#"?"#":n.p===""?null:+n.p,cost:+n.cost||0,effect:+n.effect||0}))})),psm:undefined,tree:undefined});}
  const o=Object.assign({},m,{type:"markov"});delete o.psm;delete o.tree;return o;
}
function modelReady(m){
  if(m.type==="tree")return m.strategies.some(s=>s.nodes.length>1);
  if(m.type==="psm"){const ok=c=>c&&(c.ref!=null||(c.par&&Object.values(c.par).length&&Object.values(c.par).every(v=>v!=null&&v!=="")));return m.strategies.every(s=>ok(s.os)&&ok(s.pfs));}
  return m.states.some(x=>+x.util>0||+x.cost>0)||m.strategies.some(x=>+x.addCost>0);
}
function modelTypeSeg(){const k=modelKind();return `<div class="sublabel" style="margin-top:0">Model type</div><div class="seg" id="mType" style="margin-bottom:12px"><button data-mt="markov" class="${k==="markov"?"active":""}">Markov</button><button data-mt="psm" class="${k==="psm"?"active":""}">Partitioned survival</button><button data-mt="tree" class="${k==="tree"?"active":""}">Decision tree</button></div>`;}
function wireModelTypeSeg(){document.querySelectorAll("#mType button").forEach(b=>b.onclick=()=>{state.model.type=b.dataset.mt;ensureAltModels();state.sens.params=null;state.sens.ref=0;state.sens.cmp=1;saveLocal();renderModelSidebar();renderModel();});}
function commonSettings(m,opts){
  const o=opts||{};
  return `<div class="sublabel">Outcome &amp; settings</div>
    <div class="seg" id="outcomeSel"><button data-o="QALY" class="${m.outcome==="QALY"?"active":""}">QALYs</button><button data-o="DALY" class="${m.outcome==="DALY"?"active":""}">DALYs</button></div>
    ${m.outcome==="DALY"&&!o.tree?`<div class="field"><label>Life expectancy at death (yrs, for YLL)</label><input type="number" id="lifeExp" value="${m.lifeExp}"></div>`:""}
    ${o.tree?"":`<div class="field two"><div><label>Cycle (yrs)</label><input type="number" id="cycle" step="0.0833" value="${m.cycle}"></div><div><label>Horizon (yrs)</label><input type="number" id="horizon" value="${m.horizon}"></div></div>
    <div class="field two"><div><label>Disc. cost</label><input type="number" id="dCost" step="0.01" value="${m.dCost}"></div><div><label>Disc. effect</label><input type="number" id="dEff" step="0.01" value="${m.dEff}"></div></div>
    <div class="field"><label>Within-cycle correction</label><select id="corr"><option value="half-cycle" ${m.correction!=="simpson"?"selected":""}>Half-cycle (trapezoidal)</option><option value="simpson" ${m.correction==="simpson"?"selected":""}>Simpson's 1/3</option></select></div>`}
    <div class="field"><label>WTP per ${m.outcome==="QALY"?"QALY":"DALY averted"} <span class="lab-val" id="mwtpLab">${compactINR(m.wtp)}</span></label><input type="range" id="mwtp" min="0" max="${wtpMax()}" step="25000" value="${m.wtp}"><div class="wtp-chips"><button type="button" class="wchip" data-mwtp="${GDP_PC}">1× GDP · ${compactINR(GDP_PC)}</button><button type="button" class="wchip" data-mwtp="${GDP_PC*3}">3× GDP · ${compactINR(GDP_PC*3)}</button></div></div>
    ${gdpField()}`;
}
function wireCommonSettings(){
  const m=state.model;
  document.querySelectorAll("#outcomeSel button").forEach(b=>b.onclick=()=>{m.outcome=b.dataset.o;renderModelSidebar();});
  const le=document.getElementById("lifeExp");if(le)le.onchange=e=>m.lifeExp=+e.target.value;
  ["cycle","horizon","dCost","dEff"].forEach(k=>{const el=document.getElementById(k);if(el)el.onchange=()=>m[k]=+el.value;});
  const corr=document.getElementById("corr");if(corr)corr.onchange=()=>m.correction=corr.value;
  const w=document.getElementById("mwtp");if(w)w.oninput=()=>{m.wtp=+w.value;document.getElementById("mwtpLab").textContent=compactINR(+w.value);};
  document.querySelectorAll("#sidebar .wchip").forEach(b=>b.onclick=()=>{m.wtp=+b.dataset.mwtp;renderModelSidebar();});
  wireGdp();
}

/* ------------------------- partitioned survival -------------------------- */
const PSM_EXAMPLE={note:"Illustrative oncology example: comparator curves are Weibull (months); the new drug acts through hazard ratios on OS and PFS. Costs and utilities are illustrative only.",
  cycle:1/12,horizon:15,dCost:.03,dEff:.03,correction:"half-cycle",outcome:"QALY",
  psm:{active:1,states:[{name:"Progression-free",cost:60000,util:.75,dw:.2},{name:"Progressed",cost:90000,util:.55,dw:.45},{name:"Dead",cost:0,util:0,dw:0,absorbing:true}],
    strategies:[{name:"Standard chemotherapy",addCost:150000,costOn:"pf",os:{src:"manual",dist:"weibull",par:{shape:1.25,scale:22},unit:"months"},pfs:{src:"manual",dist:"weibull",par:{shape:1.1,scale:9},unit:"months"}},
      {name:"New targeted drug",addCost:650000,costOn:"pf",os:{src:"hr",ref:0,hr:.75,lo:.6,hi:.94},pfs:{src:"hr",ref:0,hr:.6,lo:.48,hi:.75}}]}};
function curveEditor(m,si,ep){
  const s=m.psm.strategies[si],c=s[ep],id=`c_${ep}`,fits=fittedCurves();
  const refOpts=m.psm.strategies.map((o,i)=>i===si?"":`<option value="${i}" ${+c.ref===i?"selected":""}>${escHTML(o.name)}</option>`).join("");
  let body="";
  if(c.src==="hr")body=`<div class="field two"><div><label>Reference strategy</label><select id="${id}_ref">${refOpts}</select></div><div><label>Hazard ratio</label><input type="number" step="0.01" id="${id}_hr" value="${c.hr}"></div></div>
    <div class="field"><label>95% CI low – high (for PSA and the tornado)</label><div style="display:flex;gap:6px"><input type="number" step="0.01" id="${id}_lo" value="${c.lo==null?"":c.lo}" placeholder="low"><input type="number" step="0.01" id="${id}_hi" value="${c.hi==null?"":c.hi}" placeholder="high"></div></div>`;
  else if(c.src==="fit")body=`<p class="hint" style="margin:0 0 6px">${escHTML(c.label||SURV_LABEL[c.dist])}: ${(SURV_PARAMS[c.dist]||[]).map(k=>`${k} ${(+c.par[k]).toPrecision(4)}`).join(", ")} (${c.unit}). Parameter uncertainty is sampled in PSA.</p>${fits.length?`<div class="field"><label>Change fitted curve</label><select id="${id}_fit"><option value="">Keep</option>${fits.map((f,i)=>`<option value="${i}">${escHTML(f.label)}</option>`).join("")}</select></div>`:""}`;
  else body=`<div class="field two"><div><label>Distribution</label><select id="${id}_dist">${Object.keys(SURV_LABEL).map(d=>`<option value="${d}" ${c.dist===d?"selected":""}>${SURV_LABEL[d]}</option>`).join("")}</select></div><div><label>Time unit of the parameters</label><select id="${id}_unit">${Object.keys(UNITS).map(u=>`<option ${c.unit===u?"selected":""}>${u}</option>`).join("")}</select></div></div>
    <div class="field two">${(SURV_PARAMS[c.dist]||[]).map(k=>`<div><label>${k}</label><input type="number" step="any" data-cp="${id}" data-k="${k}" value="${c.par&&c.par[k]!=null?c.par[k]:""}"></div>`).join("")}</div>`;
  return `<div class="ex-row" style="border-style:solid;margin-bottom:10px"><div class="sublabel" style="margin-top:0">${ep==="os"?"Overall survival (OS)":"Progression-free survival (PFS)"}</div>
    <div class="seg" id="${id}_src" style="margin-bottom:8px"><button data-src="fit" class="${c.src==="fit"?"active":""}">Fitted curve</button><button data-src="manual" class="${c.src==="manual"?"active":""}">Enter parameters</button>${m.psm.strategies.length>1?`<button data-src="hr" class="${c.src==="hr"?"active":""}">Hazard ratio</button>`:""}</div>${body}</div>`;
}
function wireCurveEditor(m,si,ep){
  const s=m.psm.strategies[si],id=`c_${ep}`,re=()=>renderModelSidebar();
  document.querySelectorAll(`#${id}_src button`).forEach(b=>b.onclick=()=>{const src=b.dataset.src;
    if(src==="fit"){const f=fittedCurves();if(!f.length){alert("Fit a curve in the Survival tab first, then choose it here (or use “Use this curve” there).");return;}s[ep]=Object.assign({src:"fit"},f[0].spec);}
    else if(src==="hr")s[ep]={src:"hr",ref:si===0?1:0,hr:1,lo:"",hi:""};
    else s[ep]={src:"manual",dist:(s[ep]&&s[ep].dist)||"weibull",par:Object.assign({},(s[ep]&&s[ep].par)||{}),unit:(s[ep]&&s[ep].unit)||"months"};re();});
  const g=k=>document.getElementById(`${id}_${k}`);
  if(g("ref"))g("ref").onchange=e=>s[ep].ref=+e.target.value;
  ["hr","lo","hi"].forEach(k=>{if(g(k))g(k).onchange=e=>s[ep][k]=e.target.value===""?"":+e.target.value;});
  if(g("dist"))g("dist").onchange=e=>{s[ep].dist=e.target.value;s[ep].par={};re();};
  if(g("unit"))g("unit").onchange=e=>s[ep].unit=e.target.value;
  if(g("fit"))g("fit").onchange=e=>{if(e.target.value==="")return;s[ep]=Object.assign({src:"fit"},fittedCurves()[+e.target.value].spec);re();};
  document.querySelectorAll(`[data-cp="${id}"]`).forEach(el=>el.onchange=()=>{s[ep].par[el.dataset.k]=el.value===""?"":+el.value;});
}
function renderPsmSidebar(){
  const m=state.model,P=m.psm,as=P.strategies[P.active]||P.strategies[0];if(!P.strategies[P.active])P.active=0;
  const stRows=P.states.map((s,i)=>`<tr><td><input data-ps="name" data-i="${i}" value="${escHTML(s.name)}"></td><td><input data-ps="cost" data-i="${i}" type="number" value="${s.cost}" style="text-align:right"></td><td><input data-ps="util" data-i="${i}" type="number" step="0.01" value="${s.util}" style="text-align:right"></td><td><input data-ps="dw" data-i="${i}" type="number" step="0.01" value="${s.dw}" style="text-align:right"></td></tr>`).join("");
  document.getElementById("sidebar").innerHTML=`<h2><span class="section-num">04</span> Partitioned survival model</h2>
    ${modelTypeSeg()}
    <p class="hint">The standard model for cancer and other survival-driven decisions: overall-survival and progression-free-survival curves set the share of patients progression-free, progressed and dead over time.</p>
    <div class="ex-row"><span class="ex-lab">Just exploring?</span><div class="ex-btns"><button id="psmEx">Load oncology example</button></div>${P.note?`<div class="ex-note">${P.note}</div>`:""}</div>
    <div class="sublabel">Health states (annual cost · utility · disability weight)</div>
    <div class="grid-wrap"><table class="data-grid"><thead><tr><th>State</th><th>Cost / year</th><th>Utility</th><th>Disability wt</th></tr></thead><tbody id="psRows">${stRows}</tbody></table></div>
    <div class="sublabel">Strategies</div>
    <div class="seg types" id="psStrat">${P.strategies.map((s,i)=>`<button data-i="${i}" class="${P.active===i?"active":""}">${escHTML(s.name)}</button>`).join("")}</div>
    <div class="field two"><div><label>Strategy name</label><input type="text" id="psName" value="${escHTML(as.name)}"></div><div><label>Treatment cost / year</label><input type="number" id="psAdd" value="${as.addCost}"></div></div>
    <div class="field"><label>Treatment cost applies</label><select id="psCostOn"><option value="pf" ${as.costOn!=="alive"?"selected":""}>While progression-free (treat to progression)</option><option value="alive" ${as.costOn==="alive"?"selected":""}>While alive</option></select></div>
    ${curveEditor(m,P.active,"os")}${curveEditor(m,P.active,"pfs")}
    <div class="btn-row"><button class="btn btn-secondary sm" id="psAddS">+ Strategy</button><button class="btn btn-ghost sm" id="psDelS">Remove</button></div>
    ${commonSettings(m)}
    <div class="divider"></div><button class="btn btn-primary btn-block" id="runModel">Run model</button>`;
  wireModelTypeSeg();
  document.querySelectorAll("#psRows [data-ps]").forEach(el=>el.onchange=()=>{const f=el.dataset.ps,i=+el.dataset.i;P.states[i][f]=f==="name"?el.value:+el.value;});
  document.querySelectorAll("#psStrat button").forEach(b=>b.onclick=()=>{P.active=+b.dataset.i;renderModelSidebar();});
  document.getElementById("psName").onchange=e=>{as.name=e.target.value;renderModelSidebar();};
  document.getElementById("psAdd").onchange=e=>as.addCost=+e.target.value;
  document.getElementById("psCostOn").onchange=e=>as.costOn=e.target.value;
  wireCurveEditor(m,P.active,"os");wireCurveEditor(m,P.active,"pfs");
  document.getElementById("psAddS").onclick=()=>{P.strategies.push({name:"Strategy "+(P.strategies.length+1),addCost:0,costOn:"pf",os:{src:"hr",ref:0,hr:1,lo:"",hi:""},pfs:{src:"hr",ref:0,hr:1,lo:"",hi:""}});P.active=P.strategies.length-1;renderModelSidebar();};
  document.getElementById("psDelS").onclick=()=>{if(P.strategies.length<=1)return;const k=P.active;
    P.strategies.forEach((o,i)=>{if(i===k)return;["os","pfs"].forEach(ep=>{const c=o[ep];if(c&&c.src==="hr"){if(+c.ref===k)o[ep]={src:"manual",dist:"weibull",par:{},unit:"months"};else if(+c.ref>k)c.ref=+c.ref-1;}});});
    P.strategies.splice(k,1);P.active=0;renderModelSidebar();};
  document.getElementById("psmEx").onclick=()=>{const ex=JSON.parse(JSON.stringify(PSM_EXAMPLE));Object.assign(m,{cycle:ex.cycle,horizon:ex.horizon,dCost:ex.dCost,dEff:ex.dEff,correction:ex.correction,outcome:ex.outcome,wtp:GDP_PC});m.psm=ex.psm;m.psm.note=ex.note;state.sens.params=null;renderModelSidebar();renderModel();};
  wireCommonSettings();
  document.getElementById("runModel").onclick=renderModel;
}

/* ------------------------------ decision tree ----------------------------- */
const TREE_EXAMPLE={note:"Illustrative: surgery vs watchful waiting for a condition with a risk of complications. Probabilities, costs (₹) and QALYs (over one year) are illustrative only. '#' means 'the rest' (1 − the other branches).",
  outcome:"QALY",tree:{active:1,strategies:[
    {name:"Watchful waiting",nodes:[{id:"n1",parent:"",label:"Watchful waiting",p:"",cost:2000,effect:0},{id:"n2",parent:"n1",label:"Recovers",p:0.6,cost:0,effect:0.95},{id:"n3",parent:"n1",label:"Complication",p:"#",cost:45000,effect:0},{id:"n4",parent:"n3",label:"Treated, recovers",p:0.8,cost:0,effect:0.8},{id:"n5",parent:"n3",label:"Long-term disability",p:"#",cost:60000,effect:0.5}]},
    {name:"Early surgery",nodes:[{id:"n1",parent:"",label:"Early surgery",p:"",cost:80000,effect:0},{id:"n2",parent:"n1",label:"Success",p:0.9,cost:0,effect:0.97},{id:"n3",parent:"n1",label:"Surgical complication",p:"#",cost:30000,effect:0},{id:"n4",parent:"n3",label:"Revision succeeds",p:0.7,cost:40000,effect:0.85},{id:"n5",parent:"n3",label:"Long-term disability",p:"#",cost:60000,effect:0.5}]}]}};
function renderTreeSidebar(){
  const m=state.model,T=m.tree;if(!T.strategies[T.active])T.active=0;const as=T.strategies[T.active];
  const lab=id=>{const n=as.nodes.find(x=>x.id===id);return n?n.label:"";};
  const rows=as.nodes.map((n,i)=>{const root=n.parent==="";
    const opts=as.nodes.filter(x=>x.id!==n.id).map(x=>`<option value="${x.id}" ${n.parent===x.id?"selected":""}>${escHTML(x.label)}</option>`).join("");
    return `<tr><td><input data-tn="label" data-i="${i}" value="${escHTML(n.label)}"></td><td>${root?'<span style="font-size:10px;color:var(--muted);padding:0 8px">root</span>':`<select data-tn="parent" data-i="${i}">${opts}</select>`}</td><td>${root?"":`<input data-tn="p" data-i="${i}" value="${n.p}" placeholder="0–1 or #" style="text-align:right">`}</td><td><input data-tn="cost" data-i="${i}" type="number" value="${n.cost}" style="text-align:right"></td><td><input data-tn="effect" data-i="${i}" type="number" step="0.01" value="${n.effect}" style="text-align:right"></td><td class="row-del" data-tdel="${i}">${root?"":"&times;"}</td></tr>`;}).join("");
  document.getElementById("sidebar").innerHTML=`<h2><span class="section-num">04</span> Decision tree</h2>
    ${modelTypeSeg()}
    <p class="hint">For short-term decisions such as screening, diagnostics and acute care. Each branch has a probability, a cost incurred on it and an outcome. Costs and outcomes add up along each path; the tree is rolled back to expected values.</p>
    <div class="ex-row"><span class="ex-lab">Just exploring?</span><div class="ex-btns"><button id="treeEx">Load surgery example</button></div>${T.note?`<div class="ex-note">${T.note}</div>`:""}</div>
    <div class="sublabel">Strategies</div>
    <div class="seg types" id="trStrat">${T.strategies.map((s,i)=>`<button data-i="${i}" class="${T.active===i?"active":""}">${escHTML(s.name)}</button>`).join("")}</div>
    <div class="field"><label>Strategy name</label><input type="text" id="trName" value="${escHTML(as.name)}"></div>
    <div class="grid-wrap" style="overflow-x:auto"><table class="data-grid" style="min-width:600px"><thead><tr><th style="width:30%">Branch</th><th>Comes after</th><th>Probability</th><th>Cost ₹</th><th>${m.outcome==="DALY"?"DALYs":"QALYs"}</th><th></th></tr></thead><tbody id="trRows">${rows}</tbody></table></div>
    <p class="hint" style="margin-top:4px">Probability: a number from 0 to 1, or <b>#</b> for “the rest” (1 − the other branches from the same point).</p>
    <div class="btn-row"><button class="btn btn-secondary sm" id="trAddN">+ Branch</button><button class="btn btn-secondary sm" id="trAddS">+ Strategy</button><button class="btn btn-ghost sm" id="trDelS">Remove strategy</button></div>
    ${commonSettings(m,{tree:true})}
    <div class="divider"></div><button class="btn btn-primary btn-block" id="runModel">Run model</button>`;
  wireModelTypeSeg();
  document.querySelectorAll("#trStrat button").forEach(b=>b.onclick=()=>{T.active=+b.dataset.i;renderModelSidebar();});
  document.getElementById("trName").onchange=e=>{as.name=e.target.value;if(as.nodes[0]&&as.nodes[0].label.startsWith("Strategy"))as.nodes[0].label=e.target.value;renderModelSidebar();};
  document.querySelectorAll("#trRows [data-tn]").forEach(el=>el.onchange=()=>{const f=el.dataset.tn,n=as.nodes[+el.dataset.i];
    n[f]=f==="cost"||f==="effect"?+el.value:f==="p"?(el.value.trim()==="#"?"#":el.value===""?"":+el.value):el.value;if(f==="label"||f==="parent")renderModelSidebar();});
  document.querySelectorAll("#trRows .row-del").forEach(el=>el.onclick=()=>{const i=+el.dataset.tdel,n=as.nodes[i];if(!n||n.parent==="")return;
    const kill=new Set([n.id]);let grew=true;while(grew){grew=false;as.nodes.forEach(x=>{if(!kill.has(x.id)&&kill.has(x.parent)){kill.add(x.id);grew=true;}});}
    as.nodes=as.nodes.filter(x=>!kill.has(x.id));renderModelSidebar();});
  document.getElementById("trAddN").onclick=()=>{const root=as.nodes.find(x=>x.parent==="");let k=as.nodes.length+1;while(as.nodes.some(x=>x.id==="n"+k))k++;as.nodes.push({id:"n"+k,parent:root?root.id:"",label:"Branch "+k,p:"#",cost:0,effect:0});renderModelSidebar();};
  document.getElementById("trAddS").onclick=()=>{T.strategies.push({name:"Strategy "+(T.strategies.length+1),nodes:[{id:"n1",parent:"",label:"Start",p:"",cost:0,effect:0}]});T.active=T.strategies.length-1;renderModelSidebar();};
  document.getElementById("trDelS").onclick=()=>{if(T.strategies.length<=1)return;T.strategies.splice(T.active,1);T.active=0;renderModelSidebar();};
  document.getElementById("treeEx").onclick=()=>{const ex=JSON.parse(JSON.stringify(TREE_EXAMPLE));m.outcome=ex.outcome;m.wtp=GDP_PC;m.tree=ex.tree;m.tree.note=ex.note;state.sens.params=null;renderModelSidebar();renderModel();};
  wireCommonSettings();
  document.getElementById("runModel").onclick=renderModel;
}
function treeDiagram(s,R){
  const kids=id=>s.nodes.filter(n=>n.parent===id);
  const node=(n,pp)=>{const ch=kids(n.id);const p=n.parent===""?1:(n.p==="#"?null:+n.p);
    return `<li><div class="tnode ${ch.length?"":"leaf"}"><b>${escHTML(n.label)}</b>${n.parent===""?"":` <span class="tp">p = ${n.p==="#"?"#":fmtNum(+n.p,3)}</span>`}${+n.cost?` <span class="tc">${compactINR(+n.cost)}</span>`:""}${+n.effect?` <span class="te">${fmtNum(+n.effect,3)}</span>`:""}</div>${ch.length?`<ul>${ch.map(c=>node(c)).join("")}</ul>`:""}</li>`;};
  const root=s.nodes.find(n=>n.parent==="");
  return root?`<ul class="tree">${node(root)}</ul>`:"";
}

/* ------------------------------ alt results ------------------------------- */
function renderAltModelSidebar(){ensureAltModels();modelKind()==="psm"?renderPsmSidebar():renderTreeSidebar();}
async function renderAltModel(){
  const k=modelKind(),am=activeModel(),ws=document.getElementById("workspace");
  if(!modelReady(am))return emptyState(k==="psm"?"Partitioned survival model":"Decision tree",k==="psm"?"Set the overall-survival and progression-free-survival curves for each strategy (fit them in the Survival tab, enter published parameters, or use a hazard ratio), the costs and utilities of each state, then Run model. Or load the oncology example.":"Build each strategy's tree in the panel on the left: branches with probabilities, costs and outcomes. Or load the surgery example.");
  wsBusy(k==="psm"?"Partitioned survival model":"Decision tree");
  let R;try{R=await api("model",am);}catch(e){return wsError(e);}
  markRun("modeling");
  const inc=R.rows,best=R.best,unit=R.unit;
  const tagFor=s=>s.status==="dominated"?`<span class="tag dominated">dominated</span>`:s.status==="extended"?`<span class="tag ext">ext. dom.</span>`:`<span class="tag frontier">frontier</span>`;
  const effCol=k==="tree"?(am.outcome==="DALY"?"DALYs":"QALYs"):null;
  const incRows=inc.map(s=>`<tr><td>${escHTML(s.name)}</td><td>${fmtINR(s.cost)}</td>${k==="psm"?`<td>${fmtNum(s.ly,3)}</td><td>${fmtNum(s.qaly,3)}</td>`:`<td>${fmtNum(am.outcome==="DALY"?s.daly:s.qaly,3)}</td>`}<td>${s.incCost==null?"—":fmtINR(s.incCost)}</td><td>${s.incEff==null?"—":fmtNum(s.incEff,3)}</td><td>${s.icer==null?"—":fmtINR(s.icer)}</td><td style="text-align:left">${tagFor(s)}</td></tr>`).join("");
  const head=k==="psm"?`<th>Life-years</th><th>QALYs</th>`:`<th>${effCol}</th>`;
  let tabs="",panes="";
  if(k==="psm"){
    const cs=[];(R.curves||[]).forEach((c,i)=>{cs.push({label:c.name+" · OS",data:c.os,color:SERIES[i%6]});cs.push({label:c.name+" · PFS",data:c.pfs,color:SERIES[i%6],dash:true});});
    tabs=`<button class="result-tab active" data-p="curves">Survival curves</button><button class="result-tab" data-p="trace">State occupancy</button>`;
    panes=`<div class="pane active" data-pane="curves"><div class="card flush"><div class="pad"><h3>Overall and progression-free survival</h3><div class="card-sub">The curves that drive the model, extrapolated over ${am.horizon} years.${R.crossing?' <b style="color:var(--red)">PFS rises above OS for at least one strategy; it is capped at OS. Check the curves.</b>':""}</div>${lineChart(cs,"Years",am.horizon,{tip:(s,d)=>`${s.label} · year ${fmtNum(d.x,2)}: ${(d.y*100).toFixed(1)}%`})}</div><div class="legend">${cs.map(s=>`<div class="item"><span class="sw" style="background:${s.dash?`repeating-linear-gradient(90deg,${s.color} 0 5px,transparent 5px 8px)`:s.color};height:3px"></span>${escHTML(s.label)}</div>`).join("")}</div></div></div>
      <div class="pane" data-pane="trace"><div class="card flush"><div class="pad"><h3>State occupancy — ${escHTML(R.activeName)}</h3><div class="card-sub">Progression-free = PFS; progressed = OS − PFS; dead = 1 − OS.</div>${lineChart(R.series,"Years",am.horizon)}</div><div class="legend">${am.states.map((s,i)=>`<div class="item"><span class="sw" style="background:${SERIES[i%6]}"></span>${escHTML(s.name)}</div>`).join("")}</div></div></div>`;
  }else{
    tabs=`<button class="result-tab active" data-p="tree">Trees</button><button class="result-tab" data-p="paths">Paths</button>`;
    panes=`<div class="pane active" data-pane="tree">${am.strategies.map((s,i)=>`<div class="card"><h3>${escHTML(s.name)}</h3><div class="card-sub"><span style="color:var(--primary)">p</span> = branch probability · <span style="color:var(--gold)">₹</span> = cost on the branch · <span style="color:var(--emerald)">green</span> = ${effCol}. Expected cost <b>${fmtINR(R.rows.find(r=>r.name===s.name).cost)}</b> · expected ${effCol} <b>${fmtNum(am.outcome==="DALY"?R.rows.find(r=>r.name===s.name).daly:R.rows.find(r=>r.name===s.name).qaly,3)}</b></div>${treeDiagram(state.model.tree.strategies[i])}</div>`).join("")}</div>
      <div class="pane" data-pane="paths"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th>Strategy</th><th style="text-align:left">Path</th><th>Probability</th><th>Cost</th><th>${effCol}</th></tr></thead><tbody>${inc.flatMap(r=>(r.paths||[]).map(p=>`<tr><td>${escHTML(r.name)}</td><td style="text-align:left;white-space:normal">${escHTML(p.path)}</td><td>${fmtNum(p.prob,4)}</td><td>${fmtINR(p.cost)}</td><td>${fmtNum(p.effect,3)}</td></tr>`)).join("")}</tbody></table></div></div></div>`;
  }
  ws.innerHTML=`<div class="ws-head"><div><h2>${k==="psm"?"Partitioned survival model":"Decision tree"}</h2><div class="sub">${am.strategies.length} strategies${k==="psm"?`, ${am.horizon}-yr horizon, ${pct(am.dCost,0)} discounting, ${R.correction==="simpson"?"Simpson's 1/3":"half-cycle"} correction`:""}. Outcome: ${am.outcome==="DALY"?"DALYs":"QALYs"}.</div></div>${EXPORT_BAR}</div>
    <div class="kpis"><div class="kpi accent"><div class="k-label">Optimal at WTP</div><div class="k-val sm">${escHTML(best.name)}</div><div class="k-sub">at ${compactINR(am.wtp)}/${unit}</div></div><div class="kpi gold"><div class="k-label">Its ICER</div><div class="k-val mono">${best.icer==null?"ref":fmtINR(best.icer)}</div><div class="k-sub">per ${unit}</div></div><div class="kpi emerald"><div class="k-label">Strategies</div><div class="k-val mono">${am.strategies.length}</div><div class="k-sub">${R.onFr} on frontier</div></div></div>
    <div class="result-tabs" id="mTabs">${tabs}<button class="result-tab" data-p="ce">CE plane</button><button class="result-tab" data-p="inc">Incremental</button></div>
    ${panes}
    <div class="pane" data-pane="ce"><div class="card flush"><div class="pad"><h3>Cost-effectiveness plane</h3><div class="card-sub">Each strategy vs the cheapest.</div>${cePlane(R.plane,am.wtp)}</div></div></div>
    <div class="pane" data-pane="inc"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th>Strategy</th><th>Cost</th>${head}<th>Δ Cost</th><th>Δ ${unit}</th><th>ICER</th><th style="text-align:left">Status</th></tr></thead><tbody>${incRows}</tbody></table></div></div></div>`;
  wireTabs("mTabs");wireExports(k==="psm"?"Artha Partitioned Survival":"Artha Decision Tree");addInterp(R);
  ws.insertAdjacentHTML("beforeend",modelToEvalCard(R.rows,am.outcome));
}

/* ------------------------------ survival tab ------------------------------ */
function survState(){
  if(!state.survival)state.survival={active:0,arms:[]};
  if(!state.survival.arms.length)state.survival.arms.push({name:"Arm 1",endpoint:"os",mode:"ipd",ipdText:"",digText:"",riskText:"",totalEvents:"",unit:"months",horizon:"",result:null,chosen:null});
  if(!state.survival.arms[state.survival.active])state.survival.active=0;
  return state.survival;
}
// All fitted curves (arm × distribution), as curve specs for the PSM.
function fittedCurves(){
  const S=state.survival;if(!S)return [];const out=[];
  S.arms.forEach(a=>{if(!a.result)return;a.result.fits.forEach(f=>out.push({label:`${a.name} · ${a.endpoint.toUpperCase()} · ${SURV_LABEL[f.dist]}${f.dist===a.result.best?" (best AIC)":""}`,
    spec:{dist:f.dist,par:Object.assign({},f.par),unit:a.result.unit,theta:f.theta,vcov:f.vcov,label:`${a.name} ${a.endpoint.toUpperCase()}, ${SURV_LABEL[f.dist]}`}}));});
  return out;
}
function parseRows(t,keys){const rows=[];String(t||"").split(/\r?\n/).forEach(l=>{const c=l.split(/[,;\t ]+/).filter(Boolean);if(c.length>=keys.length&&c.slice(0,keys.length).every(x=>isFinite(+x))){const o={};keys.forEach((k,i)=>o[k]=+c[i]);rows.push(o);}});return rows;}
function exampleIPD(){ // deterministic illustrative data: Weibull(1.3, 18 months), uniform censoring to 36 months
  let s=12345;const r=()=>{s=(s*1103515245+12345)%2147483648;return (s+0.5)/2147483648;};
  const out=[];for(let i=0;i<150;i++){const t=18*Math.pow(-Math.log(r()),1/1.3),c=36*r();out.push(`${Math.min(t,c).toFixed(2)}, ${t<=c?1:0}`);}return out.join("\n");
}
function renderSurvSidebar(){
  const S=survState(),a=S.arms[S.active];
  document.getElementById("sidebar").innerHTML=`<h2><span class="section-num">S</span> Survival curves</h2>
    <p class="hint">Fit parametric survival curves to trial data and extrapolate beyond follow-up, as HTA bodies expect (NICE DSU TSD 14). Use them in a partitioned survival model.</p>
    <div class="ex-row"><span class="ex-lab">Just exploring?</span><div class="ex-btns"><button id="svEx">Load example data</button></div></div>
    <div class="sublabel">Arms / endpoints</div>
    <div class="seg types" id="svArms">${S.arms.map((x,i)=>`<button data-i="${i}" class="${S.active===i?"active":""}">${escHTML(x.name)} · ${x.endpoint.toUpperCase()}</button>`).join("")}</div>
    <div class="field two"><div><label>Arm name</label><input type="text" id="svName" value="${escHTML(a.name)}"></div><div><label>Endpoint</label><select id="svEp"><option value="os" ${a.endpoint==="os"?"selected":""}>Overall survival</option><option value="pfs" ${a.endpoint==="pfs"?"selected":""}>Progression-free survival</option><option value="other" ${a.endpoint==="other"?"selected":""}>Other time-to-event</option></select></div></div>
    <div class="btn-row"><button class="btn btn-secondary sm" id="svAdd">+ Arm</button><button class="btn btn-ghost sm" id="svDel">Remove</button></div>
    <div class="sublabel">Your data</div>
    <div class="seg" id="svMode"><button data-m="ipd" class="${a.mode==="ipd"?"active":""}">Patient-level data</button><button data-m="km" class="${a.mode==="km"?"active":""}">Published KM curve</button></div>
    ${a.mode==="ipd"?`<div class="field" style="margin-top:8px"><label>One patient per line: time, event (1 = event, 0 = censored)</label><textarea id="svIpd" rows="8" style="font-family:var(--mono);font-size:11px" placeholder="3.2, 1\n5.0, 0\n…">${escHTML(a.ipdText)}</textarea></div>
      <label class="btn btn-ghost sm" style="margin:0">Import CSV<input type="file" id="svFile" accept=".csv,.txt" hidden></label>`
    :`<p class="hint" style="margin-top:8px">Read points off the published Kaplan-Meier plot (e.g. with WebPlotDigitizer) and copy the numbers-at-risk table under it. Artha rebuilds the patient-level data with the Guyot et al. (2012) algorithm.</p>
      <div class="field"><label>Digitised curve: time, survival (0–1 or %) per line</label><textarea id="svDig" rows="6" style="font-family:var(--mono);font-size:11px" placeholder="0, 1\n2.1, 0.95\n…">${escHTML(a.digText)}</textarea></div>
      <div class="field"><label>Numbers at risk: time, number per line (start at 0)</label><textarea id="svRisk" rows="4" style="font-family:var(--mono);font-size:11px" placeholder="0, 150\n6, 120\n…">${escHTML(a.riskText)}</textarea></div>
      <div class="field"><label>Total number of events (optional)</label><input type="number" id="svTot" value="${a.totalEvents}"></div>`}
    <div class="field two"><div><label>Time unit of the data</label><select id="svUnit">${Object.keys(UNITS).map(u=>`<option ${a.unit===u?"selected":""}>${u}</option>`).join("")}</select></div><div><label>Extrapolate to (same unit)</label><input type="number" id="svHor" value="${a.horizon}" placeholder="3 × follow-up"></div></div>
    <div class="divider"></div><button class="btn btn-primary btn-block" id="svFit">Fit curves</button>`;
  const re=()=>renderSurvSidebar();
  document.querySelectorAll("#svArms button").forEach(b=>b.onclick=()=>{S.active=+b.dataset.i;re();renderSurv();});
  document.getElementById("svName").onchange=e=>{a.name=e.target.value;re();};
  document.getElementById("svEp").onchange=e=>{a.endpoint=e.target.value;re();};
  document.getElementById("svAdd").onclick=()=>{S.arms.push({name:"Arm "+(S.arms.length+1),endpoint:"os",mode:"ipd",ipdText:"",digText:"",riskText:"",totalEvents:"",unit:a.unit,horizon:"",result:null,chosen:null});S.active=S.arms.length-1;re();renderSurv();};
  document.getElementById("svDel").onclick=()=>{if(S.arms.length<=1)return;S.arms.splice(S.active,1);S.active=0;re();renderSurv();};
  document.querySelectorAll("#svMode button").forEach(b=>b.onclick=()=>{a.mode=b.dataset.m;re();});
  const bind=(id,k)=>{const el=document.getElementById(id);if(el)el.onchange=()=>a[k]=el.value;};
  bind("svIpd","ipdText");bind("svDig","digText");bind("svRisk","riskText");bind("svTot","totalEvents");bind("svUnit","unit");bind("svHor","horizon");
  const f=document.getElementById("svFile");if(f)f.onchange=e=>{const file=e.target.files[0];if(!file)return;const rd=new FileReader();rd.onload=()=>{a.ipdText=String(rd.result).split(/\r?\n/).filter(l=>/^\s*[\d.]/.test(l)).join("\n");re();};rd.readAsText(file);};
  document.getElementById("svEx").onclick=()=>{Object.assign(a,{name:"Control",endpoint:"os",mode:"ipd",ipdText:exampleIPD(),unit:"months",horizon:120});re();renderSurv();};
  document.getElementById("svFit").onclick=renderSurv;
}
function survChart(km,curves,chosen,unit,T){
  const W=680,H=380,ox=56,oy=H-50,pw=W-ox-26,ph=oy-22,X=v=>ox+(v/T)*pw,Y=v=>oy-v*ph;let g="";
  for(let i=0;i<=4;i++){const yy=oy-(i/4)*ph;g+=`<line x1="${ox}" y1="${yy}" x2="${ox+pw}" y2="${yy}" stroke="${C.line}"/>`+txt(ox-8,yy+4,(i*25)+"%",{anchor:"end",fill:C.muted,size:10,mono:true});}
  for(let i=0;i<=5;i++){const v=T*i/5;g+=txt(X(v),oy+16,fmtNum(v,0),{anchor:"middle",fill:C.muted,size:10,mono:true});}
  g+=`<line x1="${ox}" y1="${oy}" x2="${ox+pw}" y2="${oy}" stroke="${C.ink}" stroke-width="1.2"/>`;
  curves.forEach((c,i)=>{const sel=c.dist===chosen;g+=`<polyline points="${c.points.map(p=>`${X(p.x)},${Y(p.y)}`).join(" ")}" fill="none" stroke="${SERIES[i%6]}" stroke-width="${sel?3.2:1.6}" ${sel?"":'stroke-dasharray="5 4" opacity="0.8"'}><title>${SURV_LABEL[c.dist]}</title></polyline>`;});
  let pts=[];km.forEach((k,i)=>{if(i>0)pts.push(`${X(k.time)},${Y(km[i-1].surv)}`);pts.push(`${X(k.time)},${Y(k.surv)}`);});
  g+=`<polyline points="${pts.join(" ")}" fill="none" stroke="${C.ink}" stroke-width="2"><title>Kaplan-Meier</title></polyline>`;
  g+=txt(ox+pw/2,H-8,"Time ("+unit+")",{anchor:"middle",fill:C.muted,size:11});return svgWrap(W,H,g);
}
async function renderSurv(){
  const S=survState(),a=S.arms[S.active],ws=document.getElementById("workspace");
  const payload={mode:a.mode,unit:a.unit,horizon:a.horizon===""?null:+a.horizon};
  if(a.mode==="ipd"){payload.ipd=parseRows(a.ipdText,["time","event"]);if(payload.ipd.length<2)return emptyState("Survival curves","Paste patient-level data (time, event) for this arm, or a digitised Kaplan-Meier curve with its numbers-at-risk table, then press Fit curves. Or load the example data.");}
  else{payload.digitised=parseRows(a.digText,["time","surv"]);payload.atRisk=parseRows(a.riskText,["time","n"]);payload.totalEvents=a.totalEvents===""?null:+a.totalEvents;
    if(payload.digitised.length<3||!payload.atRisk.length)return emptyState("Survival curves","Paste the digitised points of the Kaplan-Meier curve and the numbers-at-risk table, then press Fit curves.");}
  wsBusy("Fitting survival curves");
  let R;try{R=await api("survival",payload);}catch(e){return wsError(e);}
  a.result={fits:R.fits,best:R.best,unit:R.unit,n:R.n,events:R.events};if(!a.chosen||!R.fits.some(f=>f.dist===a.chosen))a.chosen=R.best;
  markRun("survival");saveLocal();
  const minA=Math.min(...R.fits.map(f=>f.aic));
  const rows=R.fits.sort((x,y)=>x.aic-y.aic).map(f=>`<tr><td style="text-align:left"><label style="display:flex;gap:6px;align-items:center;text-transform:none;letter-spacing:0;font-size:12.5px;margin:0"><input type="radio" name="svChoose" value="${f.dist}" ${a.chosen===f.dist?"checked":""} style="width:auto">${SURV_LABEL[f.dist]}</label></td><td style="text-align:left;font-size:11px">${Object.entries(f.par).map(([k,v])=>`${k} ${(+v).toPrecision(4)}`).join(", ")}</td><td>${fmtNum(f.aic,1)}</td><td>${fmtNum(f.aic-minA,1)}</td><td>${fmtNum(f.bic,1)}</td><td>${f.median==null?"—":fmtNum(f.median,1)}</td><td>${fmtNum(f.rmst,1)}</td></tr>`).join("");
  const T=R.horizon;
  ws.innerHTML=`<div class="ws-head"><div><h2>Survival curves — ${escHTML(a.name)} (${a.endpoint.toUpperCase()})</h2><div class="sub">${R.n} patients, ${R.events} events${R.reconstructed?" · reconstructed from the published curve (Guyot et al. 2012)":""}. Maximum-likelihood fits with right censoring; extrapolated to ${fmtNum(T,0)} ${R.unit}.</div></div>${EXPORT_BAR}</div>
    <div class="kpis"><div class="kpi accent"><div class="k-label">Best fit (lowest AIC)</div><div class="k-val sm">${SURV_LABEL[R.best]}</div><div class="k-sub">ΔAIC &lt; 2 = similar support</div></div><div class="kpi"><div class="k-label">Patients / events</div><div class="k-val mono">${R.n} / ${R.events}</div></div><div class="kpi gold"><div class="k-label">Chosen for the model</div><div class="k-val sm">${SURV_LABEL[a.chosen]}</div><div class="k-sub">pick on fit and clinical plausibility</div></div></div>
    <div class="result-tabs" id="svTabs"><button class="result-tab active" data-p="plot">Fit &amp; extrapolation</button><button class="result-tab" data-p="tbl">Goodness of fit</button></div>
    <div class="pane active" data-pane="plot"><div class="card flush"><div class="pad"><h3>Kaplan-Meier and fitted curves</h3><div class="card-sub">Black: Kaplan-Meier. Solid: the chosen curve; dashed: the alternatives. Judge the tail: extrapolation must be clinically plausible, not just fit well.</div>${survChart(R.km,R.curves,a.chosen,R.unit,T)}</div><div class="legend"><div class="item"><span class="sw" style="background:${C.ink}"></span>Kaplan-Meier</div>${R.curves.map((c,i)=>`<div class="item"><span class="sw" style="background:${SERIES[i%6]}"></span>${SURV_LABEL[c.dist]}</div>`).join("")}</div></div></div>
    <div class="pane" data-pane="tbl"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th style="text-align:left">Distribution</th><th style="text-align:left">Parameters (${R.unit})</th><th>AIC</th><th>ΔAIC</th><th>BIC</th><th>Median</th><th>Mean to ${fmtNum(T,0)}</th></tr></thead><tbody>${rows}</tbody></table></div><p class="note">Parameterisation as in R flexsurv. Fits are checked against R's survival::survreg in the test suite.</p></div></div>`;
  wireTabs("svTabs");wireExports("Artha Survival");
  document.querySelectorAll('input[name="svChoose"]').forEach(el=>el.onchange=()=>{a.chosen=el.value;saveLocal();renderSurv();});
  ws.insertAdjacentHTML("beforeend",curveUseCard(a));
  addInterp({interpretation:[`${SURV_LABEL[R.best]} fits best by AIC; models within about 2 AIC points have similar statistical support.`,"Choose the curve on statistical fit, visual fit and, above all, the clinical plausibility of the long-term extrapolation (NICE DSU TSD 14). Test the alternatives as scenarios.","When the curve is used in the partitioned survival model, its parameter uncertainty is carried into the probabilistic sensitivity analysis."]});
}
function curveUseCard(a){
  ensureAltModels();const P=state.model.psm,f=a.result.fits.find(x=>x.dist===a.chosen);if(!f)return "";
  const opts=P.strategies.flatMap((s,i)=>["os","pfs"].map(ep=>`<option value="${i}:${ep}" ${ep===a.endpoint?"":""}>Partitioned survival · “${escHTML(s.name)}” · ${ep.toUpperCase()}</option>`)).join("")+`<option value="new:${a.endpoint==="pfs"?"pfs":"os"}">Partitioned survival · new strategy · ${a.endpoint==="pfs"?"PFS":"OS"}</option>`;
  setTimeout(()=>{const go=document.getElementById("svUseGo");if(!go)return;go.onclick=()=>{
    let [i,ep]=document.getElementById("svUseDest").value.split(":");
    if(i==="new"){P.strategies.push({name:a.name,addCost:0,costOn:"pf",os:{src:"manual",dist:"weibull",par:{},unit:"months"},pfs:{src:"manual",dist:"weibull",par:{},unit:"months"}});i=P.strategies.length-1;}
    i=+i;const spec=fittedCurves().find(c=>c.spec.label===`${a.name} ${a.endpoint.toUpperCase()}, ${SURV_LABEL[a.chosen]}`);
    P.strategies[i][ep]=Object.assign({src:"fit"},spec.spec);
    state.provenance=state.provenance||{};state.provenance[`psm-${ep}:${i}`]={target:`${ep.toUpperCase()} curve of “${P.strategies[i].name}”`,from:`Survival fit: ${spec.spec.label}`,value:0,at:new Date().toISOString()};
    saveLocal();document.getElementById("svUseMsg").innerHTML=`<span style="color:var(--emerald);font-weight:600">✓ Used as the ${ep.toUpperCase()} curve of “${escHTML(P.strategies[i].name)}”.</span> <a href="#" id="svOpen" style="color:var(--primary);font-weight:600">Open the model →</a>`;
    document.getElementById("svOpen").onclick=ev=>{ev.preventDefault();state.model.type="psm";P.active=i;route("modeling");};};},0);
  return `<div class="card use-card" style="border-left:4px solid var(--primary)"><h3>Use this curve</h3><div class="card-sub">Send the ${SURV_LABEL[a.chosen]} curve (with its parameter uncertainty) to the partitioned survival model.</div>
    <div style="display:flex;gap:8px;flex-wrap:wrap;align-items:center"><select id="svUseDest" style="flex:1;min-width:240px">${opts}</select><button class="btn btn-primary sm" id="svUseGo">Use it</button></div><div id="svUseMsg" style="margin-top:8px;font-size:12.5px"></div></div>`;
}

/* -------------------- multi-strategy CEAC / CEAF chart -------------------- */
function ceacMultiChart(curves,ceaf){
  const W=680,H=380,ox=60,oy=H-50,pw=W-ox-26,ph=oy-22,pts0=curves[0].points,xmax=pts0[pts0.length-1].wtp,X=v=>ox+(v/xmax)*pw,Y=v=>oy-v*ph;let g="";
  for(let i=0;i<=4;i++){const yy=oy-(i/4)*ph;g+=`<line x1="${ox}" y1="${yy}" x2="${ox+pw}" y2="${yy}" stroke="${C.line}"/>`+txt(ox-8,yy+4,(i*25)+"%",{anchor:"end",fill:C.muted,size:10,mono:true});}
  g+=`<line x1="${ox}" y1="${oy}" x2="${ox+pw}" y2="${oy}" stroke="${C.ink}" stroke-width="1.2"/>`;
  [GDP_PC,GDP_PC*3].forEach((w,i)=>{if(w<=xmax)g+=`<line x1="${X(w)}" y1="18" x2="${X(w)}" y2="${oy}" stroke="${C.gold}" stroke-dasharray="4 4"/>`+txt(X(w),14,(i+1)+"×GDP",{anchor:"middle",fill:C.gold,size:10,weight:600});});
  curves.forEach((c,i)=>{g+=`<polyline points="${c.points.map(p=>`${X(p.wtp)},${Y(p.prob)}`).join(" ")}" fill="none" stroke="${SERIES[i%6]}" stroke-width="2.4"><title>${escHTML(c.name)}</title></polyline>`;});
  ceaf.forEach(f=>{g+=`<circle cx="${X(f.wtp)}" cy="${Y(f.prob)}" r="3.4" fill="none" stroke="${C.ink}" stroke-width="1.4"><title>Frontier: ${escHTML(f.strategy)} (${(f.prob*100).toFixed(0)}%)</title></circle>`;});
  g+=txt(ox+pw/2,H-12,"Willingness-to-pay (₹)",{anchor:"middle",fill:C.muted,size:11});return svgWrap(W,H,g);
}
function evpiChart(pts){return lineChart([{label:"EVPI per patient",data:pts.map(p=>({x:p.wtp/100000,y:p.evpi})),color:C.gold}],"Willingness-to-pay (₹ lakh)",pts[pts.length-1].wtp/100000,{tip:(s,d)=>`At ₹${fmtNum(d.x,1)} L: EVPI ${fmtINR(d.y)} per patient`});}
