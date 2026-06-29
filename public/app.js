/* =============================================================================
   Artha HE — Health Economics Workbench  (v1.1)
   Landing page · costing · OOP · evaluation types (CMA/CEA/CUA/CBA/CCA) ·
   Markov modeling · PSA/DSA · budget impact · templates · multi-format export.
   All client-side, validated HE formulae (mirrors R: hesim/heemod/dampack/BCEA).
   ============================================================================= */

const RUPEE="₹", GDP_PC=200000;
const COST_CATEGORIES=["Direct medical","Direct non-medical","Indirect (productivity)"];
const C={primary:"#5B4BD6",primaryL:"#7261F0",gold:"#C8971F",emerald:"#0E9E78",amber:"#DD9430",red:"#DC4B4B",ink:"#1A1733",muted:"#6E6A86",line:"#E5E3F0"};
const SERIES=[C.primary,C.gold,C.emerald,C.amber,C.red,"#6E6A86"];

/* ---------- formatting ---------- */
const fmtINR=(x,dp=0)=>RUPEE+Number(x).toLocaleString("en-IN",{minimumFractionDigits:dp,maximumFractionDigits:dp});
const fmtNum=(x,dp=2)=>Number(x).toLocaleString("en-IN",{minimumFractionDigits:dp,maximumFractionDigits:dp});
const pct=(x,dp=1)=>(x*100).toFixed(dp)+"%";
const compactINR=x=>{const a=Math.abs(x);if(a>=1e7)return RUPEE+(x/1e7).toFixed(2)+" Cr";if(a>=1e5)return RUPEE+(x/1e5).toFixed(2)+" L";return fmtINR(x);};

/* ---------- math ---------- */
const sum=a=>a.reduce((s,x)=>s+x,0), seq=n=>Array.from({length:n},(_,i)=>i);
const nmb=(c,e,w)=>e*w-c; // net benefit — only used to colour the PSA scatter client-side

/* ---------- API: every calculation runs on the private server engine ---------- */
async function api(name,payload){
  const r=await fetch("/api/"+name,{method:"POST",headers:{"Content-Type":"application/json"},body:JSON.stringify(payload||{})});
  if(!r.ok){let m="";try{m=(await r.json()).detail||"";}catch(e){}throw new Error(m||"Calculation failed.");}
  return r.json();
}
function wsBusy(msg){document.getElementById("workspace").innerHTML=`<div class="ws-head"><div><h2>${msg||"Computing…"}</h2><div class="sub">Running on the server…</div></div></div>`;}
function wsError(e){document.getElementById("workspace").innerHTML=`<div class="ws-head"><div><h2>Couldn't compute</h2><div class="sub">${(e&&e.message)||e}. Check your inputs and try again.</div></div></div>`;}
function interpCard(arr){if(!arr||!arr.length)return"";return `<div class="card interp"><h3>How to read this result</h3><ol class="interp-list">${arr.map(t=>`<li>${t}</li>`).join("")}</ol></div>`;}
function addInterp(R){const ws=document.getElementById("workspace");if(R&&R.interpretation)ws.insertAdjacentHTML("beforeend",interpCard(R.interpretation));}
const EVAL_TYPES={
  CMA:{name:"Cost-minimisation",abbr:"CMA",eff:"Outcome (assumed equal)",wtp:false,def:"Use when the health outcomes of the options are equivalent — then only costs matter; the cheapest wins.",req:["Total cost for each option","Evidence that outcomes are equal/equivalent"]},
  CEA:{name:"Cost-effectiveness",abbr:"CEA",eff:"Effect — natural unit (e.g. life-years)",wtp:true,def:"Cost per unit of a single natural outcome (life-years gained, cases averted, mmHg). ICER vs the next option.",req:["Total cost for each option","A common natural-unit effect for each option"]},
  CUA:{name:"Cost-utility",abbr:"CUA",eff:"QALYs",wtp:true,def:"Cost per QALY (or DALY averted) — captures both length and quality of life. The HTA standard.",req:["Total cost for each option","QALYs (utility × time) for each option"]},
  CBA:{name:"Cost-benefit",abbr:"CBA",eff:"Benefit (₹, monetised)",wtp:false,def:"Both costs and outcomes valued in money. Reports net monetary benefit and the benefit–cost ratio.",req:["Total cost for each option","Monetised benefit (₹) for each option"]},
  CCA:{name:"Cost-consequence",abbr:"CCA",eff:"An outcome measure",wtp:false,def:"A descriptive balance sheet: costs and a range of outcomes listed side by side, left for the reader to weigh.",req:["Total cost for each option","One or more outcome measures"]}
};
const ADVISOR={
  "qaly":{t:"CUA",why:"Quality-adjusted life-years capture quality + length of life → cost-utility analysis."},
  "life":{t:"CEA",why:"A single natural clinical outcome (life-years, cases averted) → cost-effectiveness analysis."},
  "clinical":{t:"CEA",why:"A clinical natural unit (e.g. mmHg, % controlled) → cost-effectiveness analysis."},
  "money":{t:"CBA",why:"Outcomes already valued in money → cost-benefit analysis (net benefit, BCR)."},
  "multiple":{t:"CCA",why:"Several different outcomes you don't want to combine → cost-consequence analysis."},
  "equal":{t:"CMA",why:"Outcomes are equivalent across options → cost-minimisation (compare cost only)."}
};
/* ---------- engine: configurable Markov (any states / strategies, QALY+DALY) ---------- */
function fixModel(m){ // ensure each strategy matrix is n×n
  const n=m.states.length;
  m.strategies.forEach(s=>{const M=[];for(let i=0;i<n;i++){const row=[];for(let j=0;j<n;j++){const v=s.matrix&&s.matrix[i]&&s.matrix[i][j]!=null?+s.matrix[i][j]:(i===j?1:0);row.push(v);}M.push(row);}s.matrix=M;});
}
function rowSum(row){return sum(row.map(Number));}

/* ---------- CSV + templates + export ---------- */
function parseCSV(t){const L=t.trim().split(/\r?\n/),h=L[0].split(",").map(s=>s.trim());return L.slice(1).map(ln=>{const c=ln.split(",");const o={};h.forEach((k,i)=>o[k]=(c[i]||"").trim());return o;});}
function toCSV(rows,cols){return cols.join(",")+"\n"+rows.map(r=>cols.map(c=>r[c]).join(",")).join("\n");}
function dl(blob,name){const a=document.createElement("a");a.href=URL.createObjectURL(blob);a.download=name;a.click();}
function download(name,text){dl(new Blob([text],{type:"text/csv"}),name);}
const TEMPLATES={
  costing:["artha_costing_template.csv","item,category,quantity,unit_cost,year\nConsultation,Direct medical,1,300,2024\nDrug (per month),Direct medical,12,120,2024\nDiagnostics,Direct medical,2,450,2024\nTransport,Direct non-medical,4,150,2024\nLost work day,Indirect (productivity),2,700,2024\n"],
  oop:["artha_oop_template.csv","item,category,amount\nConsultation,Direct medical,1500\nDiagnostics,Direct medical,3000\nMedicines,Direct medical,6000\nHospitalization,Direct medical,25000\nTransport,Direct non-medical,2000\nLost wages,Indirect (productivity),8000\n"],
  evaluation:["artha_evaluation_template.csv","strategy,cost,effect\nStandard care,40000,3.5\nNew option,85000,4.4\n"]
};
function dlTemplate(k){const[n,c]=TEMPLATES[k];download(n,c);}
function activeSVG(){return document.querySelector("#workspace .pane.active svg.chart")||document.querySelector("#workspace svg.chart");}
function wsTables(){const a=[...document.querySelectorAll("#workspace .pane.active table")];const t=a.length?a:[...document.querySelectorAll("#workspace table")];return t.map(x=>x.outerHTML).join("<br>");}
function chartMeta(title){const svg=activeSVG();if(!svg)return null;const card=svg.closest(".card")||svg.parentElement;
  const h3=card&&card.querySelector("h3")?card.querySelector("h3").textContent.trim():title;
  const sub=card&&card.querySelector(".card-sub")?card.querySelector(".card-sub").textContent.trim():"";
  return {svg,h3,sub};}
function wrapText(ctx,text,x,y,maxW,lh){const words=text.split(" ");let line="";for(const wd of words){const t=line+wd+" ";if(ctx.measureText(t).width>maxW&&line){ctx.fillText(line.trim(),x,y);line=wd+" ";y+=lh;}else line=t;}ctx.fillText(line.trim(),x,y);return y;}
function exportPNG(title){const m=chartMeta(title);if(!m)return alert("No chart on this view to export.");const svg=m.svg;
  const cl=svg.cloneNode(true);const vb=svg.viewBox.baseVal;const w=vb&&vb.width?vb.width:700,h=vb&&vb.height?vb.height:440;cl.setAttribute("width",w);cl.setAttribute("height",h);
  const xml=new XMLSerializer().serializeToString(cl),url="data:image/svg+xml;base64,"+btoa(unescape(encodeURIComponent(xml))),img=new Image();
  img.onload=()=>{const sc=2,padX=26,headH=m.sub?96:64,footH=30,cw=w+padX*2,chh=h+headH+footH;
    const cv=document.createElement("canvas");cv.width=cw*sc;cv.height=chh*sc;const x=cv.getContext("2d");x.scale(sc,sc);
    x.fillStyle="#fff";x.fillRect(0,0,cw,chh);
    x.fillStyle="#C8971F";x.fillRect(0,0,cw,5);
    x.fillStyle="#1A1733";x.font="700 21px Georgia, 'Times New Roman', serif";x.fillText(m.h3,padX,42);
    if(m.sub){x.fillStyle="#6E6A86";x.font="13px Arial";wrapText(x,m.sub,padX,64,cw-padX*2,16);}
    x.drawImage(img,padX,headH,w,h);
    x.fillStyle="#9B97B4";x.font="11px Arial";x.fillText("Artha HE · Developed by Dr G Hari Prakash · "+new Date().toLocaleDateString("en-IN"),padX,headH+h+20);
    cv.toBlob(b=>dl(b,title.replace(/\s+/g,"_")+".png"));};
  img.onerror=()=>alert("PNG export failed for this chart.");img.src=url;}
function exportXLS(title){
  const a=[...document.querySelectorAll("#workspace .pane.active table")];
  const tbls=a.length?a:[...document.querySelectorAll("#workspace table")];
  if(window.XLSX&&tbls.length){ // real .xlsx
    const wb=XLSX.utils.book_new();
    tbls.forEach((t,i)=>{const ws=XLSX.utils.table_to_sheet(t);XLSX.utils.book_append_sheet(wb,ws,("Sheet"+(i+1)).slice(0,31));});
    XLSX.writeFile(wb,title.replace(/\s+/g,"_")+".xlsx");return;
  }
  const html='<html xmlns:x="urn:schemas-microsoft-com:office:excel"><head><meta charset="utf-8"></head><body><h3>'+title+"</h3>"+wsTables()+"</body></html>";
  dl(new Blob(["﻿"+html],{type:"application/vnd.ms-excel"}),title.replace(/\s+/g,"_")+".xls");
}
function exportDOC(title){const m=chartMeta(title);const cap=m?('<h3 style="color:#5B4BD6;margin-bottom:2px">'+m.h3+'</h3>'+(m.sub?'<p style="color:#666;font-size:12px;margin-top:0">'+m.sub+'</p>':'')+m.svg.outerHTML):'';
  const html='<html xmlns:w="urn:schemas-microsoft-com:office:word"><head><meta charset="utf-8"><style>table{border-collapse:collapse}td,th{border:1px solid #999;padding:5px;font-size:12px}h1{color:#241F46}</style></head><body><h1>'+title+"</h1>"+cap+wsTables()+'<p style="color:#777;font-size:10px">Generated by Artha HE · Developed by Dr G Hari Prakash · '+new Date().toLocaleDateString("en-IN")+'</p></body></html>';dl(new Blob(["﻿"+html],{type:"application/msword"}),title.replace(/\s+/g,"_")+".doc");}
function exportPDF(title){const m=chartMeta(title),w=window.open("","_blank");if(!w)return alert("Allow pop-ups to export PDF.");const cap=m?('<h2 style="font-size:16px;color:#5B4BD6;margin-bottom:2px">'+m.h3+'</h2>'+(m.sub?'<p style="color:#666;font-size:12px;margin-top:0">'+m.sub+'</p>':'')+m.svg.outerHTML):'';
  w.document.write('<html><head><title>'+title+'</title><style>body{font-family:Arial,sans-serif;padding:26px;color:#1A1733}h1{font-size:22px;border-bottom:3px solid #C8971F;padding-bottom:6px}table{border-collapse:collapse;width:100%;margin:12px 0}th,td{border:1px solid #ccc;padding:6px;text-align:right;font-size:12px}th:first-child,td:first-child{text-align:left}svg{max-width:100%}</style></head><body><h1>'+title+"</h1>"+cap+wsTables()+'<p style="color:#888;font-size:11px">Generated by Artha HE · Developed by Dr G Hari Prakash · '+new Date().toLocaleDateString("en-IN")+'</p></body></html>');w.document.close();setTimeout(()=>{w.focus();w.print();},350);}
const EXPORT_BAR='<div class="exp-bar"><button class="btn btn-ghost sm" data-exp="png">PNG</button><button class="btn btn-ghost sm" data-exp="xls">Excel</button><button class="btn btn-ghost sm" data-exp="doc">Word</button><button class="btn btn-ghost sm" data-exp="pdf">PDF</button></div>';
function wireExports(title){document.querySelectorAll("#workspace [data-exp]").forEach(b=>b.onclick=()=>({png:exportPNG,xls:exportXLS,doc:exportDOC,pdf:exportPDF})[b.dataset.exp](title));}

/* ---------- unified CHEERS report (built on the server, opened client-side) ---------- */
async function openReport(){
  let body;
  try{ body=(await api("report",state)).html; }catch(e){ alert("Could not generate the report: "+((e&&e.message)||e)); return; }
  const wordDoc='<html xmlns:w="urn:schemas-microsoft-com:office:word"><head><meta charset="utf-8"><style>body{font-family:Arial}table{border-collapse:collapse;width:100%}th,td{border:1px solid #999;padding:5px;font-size:12px}h1{color:#241F46}h2{color:#5B4BD6}</style></head><body>'+body+'</body></html>';
  const wordHref="data:application/msword;base64,"+btoa(unescape(encodeURIComponent(wordDoc)));
  const css=`body{font-family:Arial,Helvetica,sans-serif;color:#1A1733;max-width:900px;margin:0 auto;padding:30px;line-height:1.55;}h1{font-size:24px;border-bottom:3px solid #C8971F;padding-bottom:8px;}h2{font-size:17px;color:#5B4BD6;margin-top:26px;border-left:4px solid #5B4BD6;padding-left:10px;}table{border-collapse:collapse;width:100%;margin:10px 0 18px;font-size:13px;}th,td{border:1px solid #ccc;padding:6px 9px;text-align:right;}th:first-child,td:first-child{text-align:left;}th{background:#F3F2F8;}.meta{font-size:12px;color:#666;}.foot{margin-top:30px;font-size:11px;color:#888;border-top:1px solid #ddd;padding-top:10px;}.rtoolbar{position:sticky;top:0;background:#1A1733;padding:12px;margin:-30px -30px 22px;display:flex;gap:12px;align-items:center;}.rtoolbar button,.rtoolbar a{background:#5B4BD6;color:#fff;border:none;padding:9px 16px;border-radius:7px;font-size:13px;font-weight:600;cursor:pointer;text-decoration:none;font-family:Arial;}.rtoolbar .t{color:#B9B5D4;font-size:12px;margin-left:auto;}@media print{.no-print{display:none!important;}body{padding:0;max-width:none;}}`;
  const w=window.open("","_blank");if(!w)return alert("Allow pop-ups to generate the report.");
  w.document.write(`<html><head><title>Artha HE — Report</title><style>${css}</style></head><body><div class="rtoolbar no-print"><button onclick="window.print()">🖶 Print / Save PDF</button><a download="ArthaHE_Report.doc" href="${wordHref}">⤓ Download Word</a><span class="t">CHEERS-structured · all analyses</span></div>${body}</body></html>`);
  w.document.close();
}

/* ---------- SVG charts ---------- */
function svgWrap(w,h,i){return`<svg class="chart" viewBox="0 0 ${w} ${h}" preserveAspectRatio="xMidYMid meet">${i}</svg>`;}
function txt(x,y,s,o={}){return`<text x="${x}" y="${y}" font-size="${o.size||12}" fill="${o.fill||C.muted}" text-anchor="${o.anchor||"start"}" font-family="${o.mono?"IBM Plex Mono":"Plus Jakarta Sans"}" font-weight="${o.weight||400}">${s}</text>`;}
function barChartH(data){const W=660,rh=50,H=data.length*rh+28,padL=185,padR=120,max=Math.max(...data.map(d=>d.value),1),bw=W-padL-padR;let g="";
  data.forEach((d,i)=>{const y=16+i*rh,w=Math.max(2,(d.value/max)*bw);g+=`<rect x="${padL}" y="${y}" width="${w}" height="26" rx="6" fill="${d.color}"><title>${d.label}: ${d.tag}</title></rect>`;g+=txt(padL-12,y+18,d.label,{anchor:"end",fill:C.ink,size:12.5});g+=txt(padL+w+10,y+18,d.tag,{fill:C.muted,size:11.5,mono:true});});
  return svgWrap(W,H,g);}
function barChartV(data,fmt){const W=660,H=380,ox=70,oy=H-50,pw=W-ox-30,ph=oy-24,max=Math.max(...data.map(d=>Math.abs(d.value)),1)*1.1;let g="";
  for(let i=0;i<=4;i++){const yy=oy-(i/4)*ph;g+=`<line x1="${ox}" y1="${yy}" x2="${ox+pw}" y2="${yy}" stroke="${C.line}"/>`;g+=txt(ox-8,yy+4,fmt(max*i/4),{anchor:"end",fill:C.muted,size:10,mono:true});}
  g+=`<line x1="${ox}" y1="${oy}" x2="${ox+pw}" y2="${oy}" stroke="${C.ink}" stroke-width="1.2"/>`;
  data.forEach((d,i)=>{const bw=pw/data.length*.5,x=ox+(i+.5)*pw/data.length-bw/2,bh=(d.value/max)*ph;g+=`<rect x="${x}" y="${oy-bh}" width="${bw}" height="${bh}" rx="6" fill="${d.color||SERIES[i%6]}"><title>${d.label}: ${fmt(d.value)}</title></rect>`;g+=txt(ox+(i+.5)*pw/data.length,oy+18,d.label,{anchor:"middle",fill:C.muted,size:11});});
  return svgWrap(W,H,g);}
function cePlane(points,wtp){const W=680,H=430,ox=110,oy=H-66,pw=W-ox-50,ph=oy-26,xMax=Math.max(.01,...points.map(p=>Math.abs(p.dEff)))*1.3,yMax=Math.max(1,...points.map(p=>Math.abs(p.dCost)))*1.3,X=v=>ox+(v/xMax)*pw,Y=v=>oy-(v/yMax)*ph;let g="";
  g+=`<line x1="${ox}" y1="18" x2="${ox}" y2="${oy+24}" stroke="${C.ink}" stroke-width="1.3"/><line x1="${ox-pw}" y1="${oy}" x2="${ox+pw}" y2="${oy}" stroke="${C.ink}" stroke-width="1.3"/>`;
  const cap=Math.min(yMax,wtp*xMax);g+=`<line x1="${ox}" y1="${oy}" x2="${X(cap/wtp)}" y2="${Y(cap)}" stroke="${C.emerald}" stroke-dasharray="7 5" stroke-width="1.6"/>`;
  g+=txt(X(cap/wtp)-6,Y(cap)-8,"WTP "+compactINR(wtp)+"/QALY",{anchor:"end",fill:C.emerald,size:11,weight:600});
  g+=txt(ox+pw,oy+22,"+ effect →",{anchor:"end",fill:C.muted,size:11})+txt(ox+8,26,"↑ + cost",{fill:C.muted,size:11});
  points.forEach(p=>{g+=`<circle cx="${X(p.dEff)}" cy="${Y(p.dCost)}" r="7" fill="${p.ref?C.muted:C.primary}" stroke="#fff" stroke-width="1.5"><title>${p.label} — Δcost ${compactINR(p.dCost)}, Δeffect ${(+p.dEff).toFixed(3)}</title></circle>`+txt(X(p.dEff)+12,Y(p.dCost)+4,p.label,{fill:C.ink,size:12,weight:500});});
  return svgWrap(W,H,g);}
function scatterPSA(d,wtp){const W=680,H=430,ox=110,oy=H-66,pw=W-ox-50,ph=oy-26,xMax=Math.max(.01,...d.map(x=>Math.abs(x.incEff)))*1.2,yMax=Math.max(1,...d.map(x=>Math.abs(x.incCost)))*1.2,X=v=>ox+(v/xMax)*pw,Y=v=>oy-(v/yMax)*ph;let g="";
  g+=`<line x1="${ox}" y1="18" x2="${ox}" y2="${oy+24}" stroke="${C.ink}" stroke-width="1.3"/><line x1="${ox-pw}" y1="${oy}" x2="${ox+pw}" y2="${oy}" stroke="${C.ink}" stroke-width="1.3"/>`;
  const cap=Math.min(yMax,wtp*xMax);g+=`<line x1="${ox}" y1="${oy}" x2="${X(cap/wtp)}" y2="${Y(cap)}" stroke="${C.emerald}" stroke-dasharray="7 5" stroke-width="1.6"/>`;
  d.forEach(x=>{g+=`<circle cx="${X(x.incEff)}" cy="${Y(x.incCost)}" r="3" fill="${nmb(x.incCost,x.incEff,wtp)>0?C.primary:C.red}" fill-opacity="0.45"/>`;});
  g+=txt(ox+pw,oy+22,"Δ QALYs →",{anchor:"end",fill:C.muted,size:11})+txt(ox+8,26,"↑ Δ cost",{fill:C.muted,size:11});
  return svgWrap(W,H,g);}
function lineChart(series,xlab,xmax){const W=680,H=380,ox=66,oy=H-50,pw=W-ox-26,ph=oy-22,yMax=Math.max(...series.flatMap(s=>s.data.map(d=>d.y)),.001)*1.05,X=v=>ox+(v/xmax)*pw,Y=v=>oy-(v/yMax)*ph;let g="";
  for(let i=0;i<=4;i++){const yy=oy-(i/4)*ph;g+=`<line x1="${ox}" y1="${yy}" x2="${ox+pw}" y2="${yy}" stroke="${C.line}"/>`+txt(ox-8,yy+4,(yMax*i/4).toFixed(yMax<3?2:0),{anchor:"end",fill:C.muted,size:10,mono:true});}
  g+=`<line x1="${ox}" y1="${oy}" x2="${ox+pw}" y2="${oy}" stroke="${C.ink}" stroke-width="1.2"/>`;
  series.forEach((s,si)=>{g+=`<polyline points="${s.data.map(d=>`${X(d.x)},${Y(d.y)}`).join(" ")}" fill="none" stroke="${s.color||SERIES[si%6]}" stroke-width="2.4"><title>${s.label}</title></polyline>`;
    s.data.forEach(d=>{g+=`<circle cx="${X(d.x)}" cy="${Y(d.y)}" r="3" fill="${s.color||SERIES[si%6]}" opacity="0"><title>${s.label} · year ${d.x}: ${(d.y*100).toFixed(1)}%</title></circle>`;});});
  g+=txt(ox+pw/2,H-12,xlab,{anchor:"middle",fill:C.muted,size:11});return svgWrap(W,H,g);}
function ceacChart(curve){const W=680,H=380,ox=60,oy=H-50,pw=W-ox-26,ph=oy-22,xmax=curve[curve.length-1].wtp,X=v=>ox+(v/xmax)*pw,Y=v=>oy-v*ph;let g="";
  for(let i=0;i<=4;i++){const yy=oy-(i/4)*ph;g+=`<line x1="${ox}" y1="${yy}" x2="${ox+pw}" y2="${yy}" stroke="${C.line}"/>`+txt(ox-8,yy+4,(i*25)+"%",{anchor:"end",fill:C.muted,size:10,mono:true});}
  g+=`<line x1="${ox}" y1="${oy}" x2="${ox+pw}" y2="${oy}" stroke="${C.ink}" stroke-width="1.2"/>`;
  [GDP_PC,GDP_PC*3].forEach((w,i)=>{if(w<=xmax){g+=`<line x1="${X(w)}" y1="18" x2="${X(w)}" y2="${oy}" stroke="${C.gold}" stroke-dasharray="4 4"/>`+txt(X(w),14,(i+1)+"×GDP",{anchor:"middle",fill:C.gold,size:10,weight:600});}});
  g+=`<polyline points="${curve.map(c=>`${X(c.wtp)},${Y(c.prob)}`).join(" ")}" fill="none" stroke="${C.primary}" stroke-width="2.6"/>`;
  g+=txt(ox+pw/2,H-12,"Willingness-to-pay per QALY (₹)",{anchor:"middle",fill:C.muted,size:11});return svgWrap(W,H,g);}
function tornado(rows){const W=680,rh=44,H=rows.length*rh+38,padL=200,mid=padL+(W-padL-40)/2,max=Math.max(...rows.map(r=>Math.max(Math.abs(r.high-r.base),Math.abs(r.low-r.base))),1),half=(W-padL-40)/2,scale=v=>(v/max)*half;let g="";
  g+=`<line x1="${mid}" y1="12" x2="${mid}" y2="${H-20}" stroke="${C.ink}" stroke-width="1.2"/>`+txt(mid,H-6,"Net monetary benefit swing",{anchor:"middle",fill:C.muted,size:10.5});
  rows.forEach((r,i)=>{const y=18+i*rh,loW=scale(r.base-r.low),hiW=scale(r.high-r.base);g+=`<rect x="${mid-loW}" y="${y}" width="${loW}" height="24" rx="4" fill="${C.amber}"><title>${r.label} — low: NMB ${compactINR(r.low)}</title></rect><rect x="${mid}" y="${y}" width="${hiW}" height="24" rx="4" fill="${C.primary}"><title>${r.label} — high: NMB ${compactINR(r.high)}</title></rect>`+txt(padL-14,y+17,r.label,{anchor:"end",fill:C.ink,size:12});});
  return svgWrap(W,H,g);}

/* ============================ STATE ============================ */
const state={
  module:"home",
  costing:{method:"micro",toYear:2024,inflation:.05,rows:[
    {item:"Outpatient consultation",category:"Direct medical",quantity:4,unit_cost:300,year:2022},
    {item:"HbA1c test",category:"Direct medical",quantity:2,unit_cost:450,year:2022},
    {item:"Metformin (1 month)",category:"Direct medical",quantity:12,unit_cost:120,year:2023},
    {item:"Insulin (1 month)",category:"Direct medical",quantity:6,unit_cost:900,year:2023},
    {item:"Nursing time (hour)",category:"Direct medical",quantity:3,unit_cost:250,year:2022},
    {item:"Patient travel",category:"Direct non-medical",quantity:8,unit_cost:150,year:2024},
    {item:"Lost work day",category:"Indirect (productivity)",quantity:5,unit_cost:700,year:2024}],totalCost:5000000,output:1200},
  oop:{income:200000,nonFood:120000,items:[
    {item:"Doctor consultation",category:"Direct medical",amount:1500},
    {item:"Diagnostics / lab",category:"Direct medical",amount:3000},
    {item:"Medicines",category:"Direct medical",amount:6000},
    {item:"Hospitalization",category:"Direct medical",amount:25000},
    {item:"Transport",category:"Direct non-medical",amount:2000},
    {item:"Food & lodging (carer)",category:"Direct non-medical",amount:1500},
    {item:"Lost wages",category:"Indirect (productivity)",amount:8000}]},
  evaluation:{type:"CUA",wtp:GDP_PC,strats:[
    {strategy:"Standard care",cost:40000,effect:3.5},
    {strategy:"New drug A",cost:85000,effect:4.4},
    {strategy:"New drug B",cost:120000,effect:4.7}]},
  model:{
    states:[
      {name:"Healthy",cost:2000,util:0.92,dw:0.05,absorbing:false},
      {name:"Sick",cost:14000,util:0.62,dw:0.40,absorbing:false},
      {name:"Dead",cost:0,util:0,dw:0,absorbing:true}],
    strategies:[
      {name:"Standard care",addCost:0,matrix:[[0.84,0.15,0.01],[0,0.90,0.10],[0,0,1]]},
      {name:"New treatment",addCost:9000,matrix:[[0.8930,0.0975,0.0095],[0,0.90,0.10],[0,0,1]]}],
    cycle:1,horizon:30,dCost:.03,dEff:.03,wtp:GDP_PC,outcome:"QALY",lifeExp:25,activeStrat:1},
  sens:{N:1000,wtp:GDP_PC,ref:0,cmp:1},
  bia:{population:1000000,eligible:.05,maxUptake:.6,horizon:5,startYear:2025,costNew:90000,costOld:40000}
};

/* ============================ EXAMPLES (general · public health · digital health) ====== */
const EXAMPLES={
  costing:{
    general:{note:"General: cost of diabetes outpatient care (micro-costing).",method:"micro",toYear:2024,inflation:.05,rows:[
      {item:"Outpatient consultation",category:"Direct medical",quantity:4,unit_cost:300,year:2022},
      {item:"HbA1c test",category:"Direct medical",quantity:2,unit_cost:450,year:2022},
      {item:"Metformin (1 month)",category:"Direct medical",quantity:12,unit_cost:120,year:2023},
      {item:"Insulin (1 month)",category:"Direct medical",quantity:6,unit_cost:900,year:2023},
      {item:"Nursing time (hour)",category:"Direct medical",quantity:3,unit_cost:250,year:2022},
      {item:"Patient travel",category:"Direct non-medical",quantity:8,unit_cost:150,year:2024},
      {item:"Lost work day",category:"Indirect (productivity)",quantity:5,unit_cost:700,year:2024}]},
    public:{note:"Public health: cost of an immunization outreach session.",method:"micro",toYear:2024,inflation:.05,rows:[
      {item:"ANM/ASHA staff time (hours)",category:"Direct medical",quantity:120,unit_cost:80,year:2024},
      {item:"Vaccine vials",category:"Direct medical",quantity:200,unit_cost:150,year:2024},
      {item:"AD syringes",category:"Direct medical",quantity:220,unit_cost:8,year:2024},
      {item:"Cold-chain ice packs & upkeep",category:"Direct medical",quantity:1,unit_cost:3000,year:2023},
      {item:"IEC materials & printing",category:"Direct non-medical",quantity:1,unit_cost:2500,year:2024},
      {item:"Outreach transport",category:"Direct non-medical",quantity:10,unit_cost:300,year:2024},
      {item:"Beneficiary travel & wait time",category:"Indirect (productivity)",quantity:200,unit_cost:50,year:2024}]},
    digital:{note:"Digital health: running cost of a teleconsultation programme (per 1000 consults).",method:"micro",toYear:2024,inflation:.05,rows:[
      {item:"App development (amortised/yr)",category:"Direct medical",quantity:1,unit_cost:40000,year:2023},
      {item:"Cloud server & hosting (yr)",category:"Direct medical",quantity:1,unit_cost:24000,year:2024},
      {item:"SMS / data charges",category:"Direct medical",quantity:1000,unit_cost:2,year:2024},
      {item:"Teleconsult physician time (hr)",category:"Direct medical",quantity:170,unit_cost:600,year:2024},
      {item:"Helpdesk / tech support (hr)",category:"Direct non-medical",quantity:100,unit_cost:200,year:2024},
      {item:"Tablet/smartphone (amortised)",category:"Direct medical",quantity:5,unit_cost:3000,year:2024}]}
  },
  oop:{
    general:{note:"General: out-of-pocket cost of a diabetes illness episode.",income:200000,nonFood:120000,items:[
      {item:"Doctor consultation",category:"Direct medical",amount:1500},
      {item:"Diagnostics / lab",category:"Direct medical",amount:3000},
      {item:"Medicines",category:"Direct medical",amount:6000},
      {item:"Hospitalization",category:"Direct medical",amount:25000},
      {item:"Transport",category:"Direct non-medical",amount:2000},
      {item:"Food & lodging (carer)",category:"Direct non-medical",amount:1500},
      {item:"Lost wages",category:"Indirect (productivity)",amount:8000}]},
    public:{note:"Public health: out-of-pocket burden on a TB patient despite free programme drugs.",income:120000,nonFood:80000,items:[
      {item:"Pre-diagnosis private care",category:"Direct medical",amount:2500},
      {item:"Out-of-programme diagnostics",category:"Direct medical",amount:1200},
      {item:"Travel to DOTS centre",category:"Direct non-medical",amount:3000},
      {item:"Nutrition supplements",category:"Direct non-medical",amount:4000},
      {item:"Lost wages (treatment period)",category:"Indirect (productivity)",amount:15000}]},
    digital:{note:"Digital health: out-of-pocket cost of a teleconsultation episode.",income:200000,nonFood:120000,items:[
      {item:"Teleconsultation fee",category:"Direct medical",amount:300},
      {item:"e-Prescription medicines",category:"Direct medical",amount:1200},
      {item:"Mobile data pack",category:"Direct non-medical",amount:200},
      {item:"Smartphone share (amortised)",category:"Direct non-medical",amount:500}]}
  },
  evaluation:{
    general:{note:"General: three treatment options compared on cost per QALY.",type:"CUA",wtp:GDP_PC,strats:[
      {strategy:"Standard care",cost:40000,effect:3.5},{strategy:"New drug A",cost:85000,effect:4.4},{strategy:"New drug B",cost:120000,effect:4.7}]},
    public:{note:"Public health: cervical cancer screening strategies (cost per QALY).",type:"CUA",wtp:GDP_PC,strats:[
      {strategy:"No screening",cost:0,effect:18.0},{strategy:"VIA screening",cost:8000,effect:18.6},{strategy:"HPV-DNA screening",cost:22000,effect:19.1}]},
    digital:{note:"Digital health: a diabetes self-management app vs usual care (cost per QALY).",type:"CUA",wtp:GDP_PC,strats:[
      {strategy:"Usual care",cost:45000,effect:7.8},{strategy:"mHealth app + usual care",cost:62000,effect:8.5}]}
  },
  model:{
    general:{note:"General: a new treatment that slows disease progression (Healthy→Sick→Dead).",outcome:"QALY",activeStrat:1,states:[
      {name:"Healthy",cost:2000,util:.92,dw:.05,absorbing:false},{name:"Sick",cost:14000,util:.62,dw:.40,absorbing:false},{name:"Dead",cost:0,util:0,dw:0,absorbing:true}],
      strategies:[{name:"Standard care",addCost:0,matrix:[[.84,.15,.01],[0,.90,.10],[0,0,1]]},{name:"New treatment",addCost:9000,matrix:[[.893,.0975,.0095],[0,.90,.10],[0,0,1]]}]},
    public:{note:"Public health: a screen-and-treat programme vs no programme.",outcome:"QALY",activeStrat:1,states:[
      {name:"At risk",cost:500,util:.95,dw:.02,absorbing:false},{name:"Disease",cost:18000,util:.65,dw:.35,absorbing:false},{name:"Dead",cost:0,util:0,dw:0,absorbing:true}],
      strategies:[{name:"No programme",addCost:0,matrix:[[.88,.11,.01],[0,.90,.10],[0,0,1]]},{name:"Screen & treat",addCost:800,matrix:[[.925,.07,.005],[0,.92,.08],[0,0,1]]}]},
    digital:{note:"Digital health: a digital adherence tool vs standard care.",outcome:"QALY",activeStrat:1,states:[
      {name:"On treatment",cost:3000,util:.85,dw:.10,absorbing:false},{name:"Poor control",cost:16000,util:.60,dw:.40,absorbing:false},{name:"Dead",cost:0,util:0,dw:0,absorbing:true}],
      strategies:[{name:"Standard care",addCost:0,matrix:[[.80,.18,.02],[.20,.70,.10],[0,0,1]]},{name:"Digital adherence tool",addCost:1500,matrix:[[.90,.085,.015],[.35,.58,.07],[0,0,1]]}]}
  },
  bia:{
    general:{note:"General: budget impact of adopting a new treatment.",population:1000000,eligible:.05,maxUptake:.6,horizon:5,startYear:2025,costNew:90000,costOld:40000},
    public:{note:"Public health: budget impact of a national screening rollout.",population:50000000,eligible:.02,maxUptake:.7,horizon:5,startYear:2025,costNew:1500,costOld:500},
    digital:{note:"Digital health: budget impact (often savings) of a telemedicine programme.",population:2000000,eligible:.10,maxUptake:.5,horizon:5,startYear:2025,costNew:1200,costOld:2500}
  }
};
const EX_NOTE={}, EX_CUR={};
const RENDERERS={costing:["renderCostingSidebar","renderCosting"],oop:["renderOopSidebar","renderOop"],evaluation:["renderEvalSidebar","renderEval"],model:["renderModelSidebar","renderModel"],bia:["renderBiaSidebar","renderBia"]};
function loadExample(mod,key){const ex=EXAMPLES[mod][key];if(!ex)return;const {note,...data}=JSON.parse(JSON.stringify(ex));Object.assign(state[mod],data);EX_NOTE[mod]=note;EX_CUR[mod]=key;const[sb,rn]=RENDERERS[mod];window[sb]();window[rn]();saveLocal();}
function exRow(mod){const cur=EX_CUR[mod];const b=(k,l)=>`<button data-ex="${k}" class="${cur===k?'active':''}">${l}</button>`;
  return `<div class="start-note"><span class="sn-title">▶ To run your analysis</span>Enter your own data in the fields below, or download a <b>template</b>, fill it in and upload — then click the button to run.</div>
  <div class="ex-row"><span class="ex-lab">Just exploring? Load a worked example to see how it works</span><div class="ex-btns">${b("general","General")}${b("public","Public health")}${b("digital","Digital health")}</div>${EX_NOTE[mod]?`<div class="ex-note">${EX_NOTE[mod]}</div>`:""}</div>`;}
function wireExamples(mod){document.querySelectorAll("#sidebar .ex-row [data-ex]").forEach(b=>b.onclick=()=>loadExample(mod,b.dataset.ex));}

/* ============================ HOME ============================ */
function renderLanding(){
  const defs=[
    {a:"Costing",cls:"ink",go:"costing",h:"Micro & gross costing",p:"Bottom-up (resource × unit cost) or top-down (budget ÷ output). The foundation of every evaluation."},
    {a:"OOP / CHE",cls:"gold",go:"oop",h:"Out-of-pocket & catastrophic expenditure",p:"What patients pay directly, and whether it crosses catastrophic (10% income / 40% capacity-to-pay) thresholds."},
    {a:"CMA",cls:"",go:"evaluation",h:"Cost-minimisation",p:"When outcomes are equal, compare cost only — cheapest option wins."},
    {a:"CEA",cls:"",go:"evaluation",h:"Cost-effectiveness",p:"Cost per natural outcome (life-year, case averted). Reports the ICER vs the next best option."},
    {a:"CUA",cls:"",go:"evaluation",h:"Cost-utility",p:"Cost per QALY — the HTA standard, capturing both quality and length of life."},
    {a:"CBA",cls:"",go:"evaluation",h:"Cost-benefit",p:"Costs and outcomes both in money — net monetary benefit and benefit–cost ratio."},
    {a:"Markov",cls:"em",go:"modeling",h:"Decision-analytic modeling",p:"State-transition (Markov) cohort models with half-cycle correction and discounting, in one click."},
    {a:"PSA / DSA",cls:"em",go:"sensitivity",h:"Sensitivity analysis",p:"Tornado (one-way), probabilistic Monte-Carlo, CEAC and EVPI — how robust is the conclusion?"},
    {a:"BIA",cls:"gold",go:"bia",h:"Budget impact analysis",p:"Affordability for the payer: annual and cumulative spend as a new treatment is adopted."},
    {a:"ICER",cls:"ink",h:"Incremental cost-effectiveness ratio",p:"Δcost ÷ Δeffect between options, with strong and extended dominance detected automatically."}
  ];
  const guide=[
    {t:"Micro-costing (bottom-up)",what:"Builds the cost of a service, programme or intervention from the ground up — every resource used, costed individually.",data:"For each resource: staff time + salary, consumables, equipment, space, quantity used, unit cost and price year. Staff are costed by salary × time-share; equipment is annualised over its useful life (+ maintenance).",how:["List every resource used","Enter quantity × unit cost (salary × time-share for staff; annualised cost for equipment)","Group by cost category","Express all costs in one price year"]},
    {t:"Gross-costing (top-down)",what:"Divides a total budget by the number of output units — quick when only aggregate spend is known.",data:"Total expenditure of the cost centre, and the number of output units (patients treated, visits, tests, doses delivered).",how:["Take the total cost of the service","Count the output units produced","Cost per unit = total ÷ output"]},
    {t:"Out-of-pocket & catastrophic expenditure",what:"What patients/households pay directly, and whether that payment is financially catastrophic.",data:"Direct medical (consultation, drugs, tests, hospitalisation), direct non-medical (transport, food, lodging), indirect (lost income); plus household income and non-food expenditure.",how:["Enter each payment by category","Add household income & capacity-to-pay","Compare OOP to thresholds (10%/25% income, 40% capacity-to-pay)"]},
    {t:"Cost-effectiveness (CEA)",what:"Cost per unit of a single natural outcome — life-years gained, cases averted, mmHg reduced, children fully immunised.",data:"Total cost and the common natural-unit effect for each option being compared.",how:["Enter cost & effect per option","ICER = Δcost ÷ Δeffect vs the next best option","Compare the ICER to your threshold"]},
    {t:"Cost-utility (CUA)",what:"Cost per QALY (or DALY averted) — captures both quality and length of life. The HTA standard.",data:"Total cost and QALYs (utility × time) for each option. DALYs may be used in LMIC/GBD work.",how:["Enter cost & QALYs per option","Compute the ICER per QALY","Compare to 1× / 3× GDP per capita"]},
    {t:"Cost-benefit (CBA)",what:"Costs and outcomes both expressed in money — lets you compare across very different programmes.",data:"Total cost and a monetised benefit (₹) for each option (e.g. willingness-to-pay or productivity value).",how:["Enter cost & monetised benefit","Net benefit = benefit − cost","Benefit-cost ratio > 1 = worthwhile"]},
    {t:"Decision modelling (Markov)",what:"Projects costs and outcomes over time across health states — for chronic disease, screening or long-run programmes.",data:"Health states, transition probabilities, state costs & utilities, time horizon and discount rate.",how:["Define the health states","Fill each strategy's transition matrix (rows sum to 1)","Run the cohort → read the ICER and trace"]},
    {t:"Sensitivity analysis (DSA / PSA)",what:"Tests how robust the conclusion is to uncertainty in the inputs.",data:"Plausible ranges (deterministic) or distributions (probabilistic) for the key parameters.",how:["One-way: vary each parameter → tornado","Probabilistic: Monte-Carlo → CEAC & EVPI","Read the probability of being cost-effective"]},
    {t:"Budget impact analysis (BIA)",what:"The payer's affordability question — total additional spend over time as a new option is adopted.",data:"Eligible population, uptake over time, current vs new cost per patient, and the time horizon.",how:["Size the eligible population","Set uptake over the horizon","Read annual & cumulative budget impact"]}
  ];
  const links=[
    {n:"DHR — Dept. of Health Research",d:"Apex body for health research policy under MoHFW.",u:"https://dhr.gov.in"},
    {n:"HTAIn — Health Technology Assessment in India",d:"National HTA programme: methods, India reference case & reports.",u:"https://htain.icmr.org.in"},
    {n:"ICMR",d:"Indian Council of Medical Research — guidelines & ethics.",u:"https://www.icmr.gov.in"},
    {n:"MoHFW",d:"Ministry of Health & Family Welfare — policies & programmes.",u:"https://mohfw.gov.in"},
    {n:"NHSRC",d:"National Health Systems Resource Centre — costing studies & National Health Accounts.",u:"https://nhsrcindia.org"},
    {n:"National Health Authority (PM-JAY)",d:"Ayushman Bharat package rates & health-benefit packages.",u:"https://nha.gov.in"},
    {n:"WHO-CHOICE",d:"WHO cost-effectiveness analysis & unit-cost database.",u:"https://www.who.int/teams/health-systems-governance-and-financing/economic-analysis/costing-and-technical-efficiency"},
    {n:"CGHS rates",d:"Central Government Health Scheme reference tariffs.",u:"https://cghs.gov.in"},
    {n:"ISPOR",d:"Global HEOR society — CHEERS 2022 & good-practice guidance.",u:"https://www.ispor.org"}
  ];
  document.getElementById("landing").innerHTML=`
    <div class="lhead">
      <div class="brand"><div class="logo">अ</div><div><div class="name">Artha<b> HE</b></div><div class="tagline">Health Economics Workbench</div></div></div>
      <button class="btn btn-primary" id="enterTop">Enter the Workbench →</button>
    </div>
    <div class="home">
    <section class="hero">
      <div class="fluid"><span class="b1"></span><span class="b2"></span><span class="b3"></span></div>
      <div class="hero-inner">
        <div class="logo-hero"><div class="ring"></div><div class="tile">अ</div></div>
        <div style="flex:1;min-width:280px">
          <h1>Artha <b>HE</b></h1>
          <div class="hero-tag">Making health-economic analysis easy — for everyone.</div>
          <p class="lead">Health economics is not just about medicines — it covers <b>public-health programmes, screening, vaccination, policies and service-delivery interventions</b> in community medicine too. Artha HE turns your data into full economic analyses — costing, out-of-pocket burden, cost-effectiveness, modelling, sensitivity and budget impact — with the complex maths handled for you.</p>
          <div class="pills"><span class="pill">Researchers</span><span class="pill">Teaching</span><span class="pill">India / LMIC</span><span class="pill">Payers / HTA</span></div>
          <div class="cta"><button class="btn btn-primary btn-lg" id="ctaStart">Enter the Workbench →</button>
            <button class="btn btn-secondary btn-lg" id="ctaMethods">See the methods</button></div>
        </div>
      </div>
    </section>

    <h2 class="sec">How it works</h2>
    <p class="secsub">Four steps — and if you're not sure which analysis you need, the Evaluation tab has a built-in advisor that recommends the right method and tells you exactly what data is required.</p>
    <div class="steps">
      <div class="step"><div class="n">1</div><h4>Pick an analysis</h4><p>Costing, OOP, an economic evaluation (CMA/CEA/CUA/CBA), a Markov model, sensitivity or budget impact.</p></div>
      <div class="step"><div class="n">2</div><h4>Use a template</h4><p>Download a ready-made CSV template, fill in your data, and upload it — or just type into the on-screen grid.</p></div>
      <div class="step"><div class="n">3</div><h4>Run — one click</h4><p>Validated formulae compute ICERs, QALYs, CEAC, EVPI and more in the background, with clear charts.</p></div>
      <div class="step"><div class="n">4</div><h4>Export</h4><p>Send any analysis to Excel, Word, PDF or PNG for your report, paper or presentation.</p></div>
    </div>

    <h2 class="sec">The components of economic evaluation</h2>
    <p class="secsub">A quick reference to the building blocks — click any card to open that tool.</p>
    <div class="defs" id="defGrid">
      ${defs.map(d=>`<div class="def" ${d.go?`data-go="${d.go}"`:""}>
        <span class="abbr ${d.cls}">${d.a}</span><h4>${d.h}</h4><p>${d.p}</p>${d.go?'<span class="go">Open tool →</span>':""}</div>`).join("")}
    </div>

    <h2 class="sec">How to do each analysis — and what data you need</h2>
    <p class="secsub">New to health economics? Each card explains what the method is, the data it requires, and the steps to run it — so you know exactly what to prepare before you start.</p>
    <div class="guide">
      ${guide.map(gd=>`<div class="gcard">
        <h4>${gd.t}</h4>
        <div class="grow"><span class="glab">What it is</span><p>${gd.what}</p></div>
        <div class="grow"><span class="glab">Data you need</span><p>${gd.data}</p></div>
        <div class="grow"><span class="glab">How to do it</span><ol class="gsteps">${gd.how.map(s=>`<li>${s}</li>`).join("")}</ol></div>
      </div>`).join("")}
    </div>

    <div class="method" id="methods">
      <h3>How Artha HE is built</h3>
      <p>Every number is produced with <b style="color:#fff">validated, published health-economics formulae</b> — the same methods implemented in the standard R toolchain (<b style="color:#fff">hesim, heemod, dampack, BCEA</b>). That includes discounting and half-cycle correction, ICER and dominance logic, probabilistic sensitivity analysis with Beta / Gamma / Lognormal distributions, cost-effectiveness acceptability curves and expected value of perfect information. Defaults follow Indian / LMIC reference cases (GDP-based willingness-to-pay, 3% discounting) and reporting aligns with the CHEERS 2022 checklist.</p>
      <div class="badges"><span class="badge">Private compute engine</span><span class="badge">CHEERS 2022</span><span class="badge">Half-cycle correction</span><span class="badge">PSA · CEAC · EVPI</span><span class="badge">India reference case</span></div>
      <p style="margin-top:18px;color:#fff;font-size:14px"><b>Developed by Dr G Hari Prakash</b></p>
    </div>
    <h2 class="sec">Useful resources &amp; official links</h2>
    <p class="secsub">Authoritative sources for HTA methods, reference cases, costing data and package rates — useful when preparing an analysis.</p>
    <div class="links">
      ${links.map(l=>`<a class="lcard" href="${l.u}" target="_blank" rel="noopener noreferrer"><div class="ln">${l.n} <span class="ext">↗</span></div><div class="ld">${l.d}</div></a>`).join("")}
    </div>

    <div style="text-align:center;margin-top:36px"><button class="btn btn-primary btn-lg" id="enterBottom">Enter the Workbench →</button></div>
    <footer class="foot">Artha HE · Health Economics Workbench · for research &amp; teaching<br><b style="color:var(--ink-soft)">Developed by Dr G Hari Prakash</b></footer>
  </div>`;
  const enter=()=>enterApp("costing");
  document.getElementById("enterTop").onclick=enter;
  document.getElementById("enterBottom").onclick=enter;
  document.getElementById("ctaStart").onclick=enter;
  document.getElementById("ctaMethods").onclick=()=>document.getElementById("methods").scrollIntoView({behavior:"smooth"});
  document.querySelectorAll("#defGrid .def[data-go]").forEach(d=>d.onclick=()=>enterApp(d.dataset.go));
}
function enterApp(mod){document.getElementById("landing").style.display="none";document.getElementById("appShell").style.display="";window.scrollTo(0,0);route(mod||"costing");}
function backToLanding(){document.getElementById("appShell").style.display="none";const l=document.getElementById("landing");l.style.display="";window.scrollTo(0,0);}

/* ============================ COSTING ============================ */
function renderCostingSidebar(){
  const c=state.costing,catOpts=cat=>COST_CATEGORIES.map(o=>`<option ${o===cat?"selected":""}>${o}</option>`).join("");
  const rows=c.rows.map((r,i)=>`<tr><td><input data-f="item" data-i="${i}" value="${r.item}"></td><td><select data-f="category" data-i="${i}">${catOpts(r.category)}</select></td><td><input data-f="quantity" data-i="${i}" type="number" value="${r.quantity}" style="text-align:right"></td><td><input data-f="unit_cost" data-i="${i}" type="number" value="${r.unit_cost}" style="text-align:right"></td><td><input data-f="year" data-i="${i}" type="number" value="${r.year}" style="text-align:right"></td><td class="row-del" data-del="${i}">&times;</td></tr>`).join("");
  document.getElementById("sidebar").innerHTML=`<h2><span class="section-num">01</span> Costing</h2>
    <p class="hint">Cost a programme from resource use (micro) or a total budget (gross).</p>
    ${exRow("costing")}
    <div class="seg" id="costMethod"><button data-v="micro" class="${c.method==="micro"?"active":""}">Micro-costing</button><button data-v="gross" class="${c.method==="gross"?"active":""}">Gross-costing</button></div>
    <div id="microC" style="display:${c.method==="micro"?"block":"none"}">
      <div class="grid-wrap"><table class="data-grid"><thead><tr><th>Item</th><th>Category</th><th>Qty</th><th>Unit ₹</th><th>Yr</th><th></th></tr></thead><tbody id="costRows">${rows}</tbody></table></div>
      <div class="btn-row"><button class="btn btn-secondary sm" id="addRow">+ Add item</button><button class="btn btn-ghost sm" id="clearRows">Clear all</button><label class="btn btn-ghost sm" style="margin:0">Import CSV<input type="file" id="csvFile" accept=".csv"></label><button class="btn btn-ghost sm" id="tplCost">Template</button></div>
      <div class="field"><label>Add from reference library <span style="float:right;font-weight:400;text-transform:none;color:var(--muted-light)">indicative ₹</span></label>
        <select id="refPick"><option value="">— insert an indicative unit cost —</option>${REFDATA.unitCosts.map((u,i)=>`<option value="${i}">${u.item} · ₹${u.value}/${u.unit}</option>`).join("")}</select></div>
      <div class="field"><label>Express all costs in price year</label><input type="number" id="toYear" value="${c.toYear}"></div>
      <div class="field"><label>Annual inflation <span class="lab-val" id="inflLab">${pct(c.inflation)}</span></label><input type="range" id="infl" min="0" max="15" step="0.5" value="${c.inflation*100}"></div></div>
    <div id="grossC" style="display:${c.method==="gross"?"block":"none"}">
      <div class="field"><label>Total cost of the service (₹)</label><input type="number" id="totalCost" value="${c.totalCost}"></div>
      <div class="field"><label>Output units (patients, visits…)</label><input type="number" id="output" value="${c.output}"></div></div>
    <div class="divider"></div><button class="btn btn-primary btn-block" id="calcBtn">Calculate</button>`;
  document.querySelectorAll("#costMethod button").forEach(b=>b.onclick=()=>{c.method=b.dataset.v;renderCostingSidebar();renderCosting();});
  document.querySelectorAll("#costRows [data-f]").forEach(el=>el.onchange=()=>c.rows[+el.dataset.i][el.dataset.f]=el.value);
  document.querySelectorAll("#costRows .row-del").forEach(el=>el.onclick=()=>{c.rows.splice(+el.dataset.del,1);renderCostingSidebar();});
  const a=document.getElementById("addRow");if(a)a.onclick=()=>{c.rows.push({item:"New item",category:"Direct medical",quantity:1,unit_cost:0,year:c.toYear});renderCostingSidebar();};
  const clr=document.getElementById("clearRows");if(clr)clr.onclick=()=>{c.rows=[];renderCostingSidebar();renderCosting();};
  const inf=document.getElementById("infl");if(inf)inf.oninput=()=>{c.inflation=+inf.value/100;document.getElementById("inflLab").textContent=pct(c.inflation);};
  const ty=document.getElementById("toYear");if(ty)ty.onchange=()=>c.toYear=+ty.value;
  const tc=document.getElementById("totalCost");if(tc)tc.onchange=()=>c.totalCost=+tc.value;
  const op=document.getElementById("output");if(op)op.onchange=()=>c.output=+op.value;
  const tpl=document.getElementById("tplCost");if(tpl)tpl.onclick=()=>dlTemplate("costing");
  const rp=document.getElementById("refPick");if(rp)rp.onchange=()=>{const u=REFDATA.unitCosts[+rp.value];if(!u)return;c.rows.push({item:u.item,category:u.category,quantity:1,unit_cost:u.value,year:c.toYear});renderCostingSidebar();renderCosting();};
  const cf=document.getElementById("csvFile");if(cf)cf.onchange=e=>{const f=e.target.files[0];if(!f)return;const rd=new FileReader();rd.onload=()=>{c.rows=parseCSV(rd.result);renderCostingSidebar();renderCosting();};rd.readAsText(f);};
  document.getElementById("calcBtn").onclick=renderCosting;
  wireExamples("costing");
}
async function renderCosting(){
  const c=state.costing,ws=document.getElementById("workspace");wsBusy("Costing");
  let res;try{res=await api("costing",c);}catch(e){return wsError(e);}
  if(res.method==="gross"){const per=res.per;
    ws.innerHTML=`<div class="ws-head"><div><h2>Gross costing</h2><div class="sub">Top-down: total expenditure ÷ output units.</div></div>${EXPORT_BAR}</div>
      <div class="kpis"><div class="kpi accent"><div class="k-label">Total cost</div><div class="k-val">${compactINR(res.total)}</div><div class="k-sub">${fmtINR(res.total)}</div></div><div class="kpi gold"><div class="k-label">Cost per unit</div><div class="k-val">${compactINR(per)}</div><div class="k-sub">over ${fmtNum(res.output,0)} units</div></div></div>
      <div class="card"><h3>Calculation</h3><table class="results-table"><tbody><tr><td>Total cost</td><td>${fmtINR(res.total)}</td></tr><tr><td>Output units</td><td>${fmtNum(res.output,0)}</td></tr><tr><td>Average cost per unit</td><td>${fmtINR(per,2)}</td></tr></tbody></table></div>`;
    wireExports("Artha Gross Costing");return;}
  const bars=res.byCat.map((b,i)=>({label:b.category,value:b.cost,tag:compactINR(b.cost)+"  ·  "+pct(b.share),color:SERIES[i%6]}));
  const lr=res.lines.map(l=>`<tr><td>${l.item}</td><td style="text-align:left;font-family:var(--sans);font-size:11px;color:var(--muted)">${l.category}</td><td>${fmtNum(l.quantity,0)}</td><td>${fmtINR(l.unit_cost_adj,0)}</td><td>${fmtINR(l.line_cost,0)}</td></tr>`).join("");
  ws.innerHTML=`<div class="ws-head"><div><h2>Micro costing</h2><div class="sub">Bottom-up: quantity × unit cost, inflated to ${c.toYear} prices (${pct(c.inflation)}/yr).</div></div>${EXPORT_BAR}</div>
    <div class="kpis"><div class="kpi accent"><div class="k-label">Total cost</div><div class="k-val">${compactINR(res.total)}</div><div class="k-sub">${fmtINR(res.total)}</div></div><div class="kpi"><div class="k-label">Cost lines</div><div class="k-val mono">${res.lines.length}</div></div><div class="kpi"><div class="k-label">Categories</div><div class="k-val mono">${res.byCat.length}</div></div></div>
    <div class="result-tabs" id="costTabs"><button class="result-tab active" data-p="cat">By category</button><button class="result-tab" data-p="lines">Cost lines</button></div>
    <div class="pane active" data-pane="cat"><div class="card flush"><div class="pad"><h3>Cost by category</h3><div class="card-sub">Share of total across perspective categories.</div>${barChartH(bars)}</div></div></div>
    <div class="pane" data-pane="lines"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th>Item</th><th style="text-align:left">Category</th><th>Qty</th><th>Unit (adj)</th><th>Line cost</th></tr></thead><tbody>${lr}</tbody><tfoot><tr><td>Total</td><td></td><td></td><td></td><td>${fmtINR(res.total)}</td></tr></tfoot></table></div></div></div>`;
  wireTabs("costTabs");wireExports("Artha Micro Costing");
}

/* ============================ OOP ============================ */
function renderOopSidebar(){
  const o=state.oop,catOpts=cat=>COST_CATEGORIES.map(x=>`<option ${x===cat?"selected":""}>${x}</option>`).join("");
  const rows=o.items.map((r,i)=>`<tr><td><input data-f="item" data-i="${i}" value="${r.item}"></td><td><select data-f="category" data-i="${i}">${catOpts(r.category)}</select></td><td><input data-f="amount" data-i="${i}" type="number" value="${r.amount}" style="text-align:right"></td><td class="row-del" data-del="${i}">&times;</td></tr>`).join("");
  document.getElementById("sidebar").innerHTML=`<h2><span class="section-num">02</span> Out-of-Pocket</h2>
    <p class="hint">What the patient/household pays directly for one illness episode — and whether it is <b>catastrophic</b>.</p>
    ${exRow("oop")}
    <div class="grid-wrap"><table class="data-grid"><thead><tr><th>Item</th><th>Category</th><th>₹ Amount</th><th></th></tr></thead><tbody id="oopRows">${rows}</tbody></table></div>
    <div class="btn-row"><button class="btn btn-secondary sm" id="addOop">+ Add item</button><button class="btn btn-ghost sm" id="clearOop">Clear all</button><label class="btn btn-ghost sm" style="margin:0">Import CSV<input type="file" id="oopFile" accept=".csv"></label><button class="btn btn-ghost sm" id="tplOop">Template</button></div>
    <div class="field"><label>Annual household income (₹)</label><input type="number" id="income" value="${o.income}"></div>
    <div class="field"><label>Annual non-food / capacity-to-pay (₹)</label><input type="number" id="nonFood" value="${o.nonFood}"></div>
    <div class="divider"></div><button class="btn btn-primary btn-block" id="calcOop">Calculate</button>`;
  document.querySelectorAll("#oopRows [data-f]").forEach(el=>el.onchange=()=>o.items[+el.dataset.i][el.dataset.f]=el.dataset.f==="amount"?+el.value:el.value);
  document.querySelectorAll("#oopRows .row-del").forEach(el=>el.onclick=()=>{o.items.splice(+el.dataset.del,1);renderOopSidebar();});
  document.getElementById("addOop").onclick=()=>{o.items.push({item:"New item",category:"Direct medical",amount:0});renderOopSidebar();};
  document.getElementById("clearOop").onclick=()=>{o.items=[];renderOopSidebar();renderOop();};
  document.getElementById("income").onchange=e=>o.income=+e.target.value;
  document.getElementById("nonFood").onchange=e=>o.nonFood=+e.target.value;
  document.getElementById("tplOop").onclick=()=>dlTemplate("oop");
  document.getElementById("oopFile").onchange=e=>{const f=e.target.files[0];if(!f)return;const rd=new FileReader();rd.onload=()=>{o.items=parseCSV(rd.result);renderOopSidebar();renderOop();};rd.readAsText(f);};
  document.getElementById("calcOop").onclick=renderOop;
  wireExamples("oop");
}
async function renderOop(){
  const o=state.oop,ws=document.getElementById("workspace");wsBusy("Out-of-pocket");
  let r;try{r=await api("oop",o);}catch(e){return wsError(e);}
  const bars=r.byCat.map((b,i)=>({label:b.category,value:b.cost,tag:compactINR(b.cost)+"  ·  "+pct(b.share),color:SERIES[i%6]}));
  const flag=(v,t)=>`<span class="tag ${v?"dominated":"frontier"}">${v?"Yes":"No"}</span> ${t}`;
  const it=r.items.map(i=>`<tr><td>${i.item}</td><td style="text-align:left;font-family:var(--sans);font-size:11px;color:var(--muted)">${i.category}</td><td>${fmtINR(i.amount)}</td></tr>`).join("");
  ws.innerHTML=`<div class="ws-head"><div><h2>Out-of-pocket expenditure</h2><div class="sub">Direct patient payments and catastrophic-health-expenditure (CHE) check at 10% of income and 40% of capacity-to-pay.</div></div>${EXPORT_BAR}</div>
    <div class="kpis">
      <div class="kpi accent"><div class="k-label">Total OOP</div><div class="k-val">${compactINR(r.total)}</div><div class="k-sub">${fmtINR(r.total)}</div></div>
      <div class="kpi gold"><div class="k-label">% of income</div><div class="k-val mono">${pct(r.pctInc)}</div><div class="k-sub">of ${compactINR(o.income)}/yr</div></div>
      <div class="kpi ${r.che10?'':'emerald'}"><div class="k-label">Catastrophic (10% income)</div><div class="k-val sm" style="color:${r.che10?C.red:C.emerald}">${r.che10?"Yes":"No"}</div><div class="k-sub">${pct(r.pctInc)} vs 10%</div></div>
      <div class="kpi ${r.che40?'':'emerald'}"><div class="k-label">Catastrophic (40% CTP)</div><div class="k-val sm" style="color:${r.che40?C.red:C.emerald}">${r.che40?"Yes":"No"}</div><div class="k-sub">${pct(r.pctCTP)} vs 40%</div></div></div>
    <div class="result-tabs" id="oopTabs"><button class="result-tab active" data-p="cat">By category</button><button class="result-tab" data-p="items">Items</button></div>
    <div class="pane active" data-pane="cat"><div class="card flush"><div class="pad"><h3>OOP by category</h3><div class="card-sub">Direct medical, direct non-medical and indirect (productivity) burden.</div>${barChartH(bars)}</div></div></div>
    <div class="pane" data-pane="items"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th>Item</th><th style="text-align:left">Category</th><th>Amount</th></tr></thead><tbody>${it}</tbody><tfoot><tr><td>Total OOP</td><td></td><td>${fmtINR(r.total)}</td></tr></tfoot></table></div>
      <p class="note">CHE definitions: OOP &gt; 10% (or 25%) of household income, or &gt; 40% of capacity-to-pay (non-food expenditure) — WHO/standard thresholds.</p></div></div>`;
  wireTabs("oopTabs");wireExports("Artha Out-of-Pocket");
}

/* ============================ EVALUATION ============================ */
function renderEvalSidebar(){
  const e=state.evaluation,t=EVAL_TYPES[e.type];
  const typeBtns=Object.keys(EVAL_TYPES).map(k=>`<button data-t="${k}" class="${e.type===k?"active":""}">${k}</button>`).join("");
  const rows=e.strats.map((s,i)=>`<tr><td><input data-f="strategy" data-i="${i}" value="${s.strategy}"></td><td><input data-f="cost" data-i="${i}" type="number" value="${s.cost}" style="text-align:right"></td><td><input data-f="effect" data-i="${i}" type="number" step="0.01" value="${s.effect}" style="text-align:right"></td><td class="row-del" data-del="${i}">&times;</td></tr>`).join("");
  document.getElementById("sidebar").innerHTML=`<h2><span class="section-num">03</span> Evaluation</h2>
    <p class="hint">Choose the type of economic evaluation — or let the advisor pick based on your data.</p>
    ${exRow("evaluation")}
    <div class="sublabel">Analysis type</div>
    <div class="seg types" id="evalType">${typeBtns}</div>
    <div class="advisor"><label style="font-size:10.5px;font-weight:600;color:var(--muted);text-transform:uppercase;letter-spacing:.04em">Not sure? What outcome do you have?</label>
      <select id="advisor" style="margin-top:7px"><option value="">— choose —</option>
        <option value="qaly">QALYs / quality of life</option><option value="life">Life-years / deaths averted</option>
        <option value="clinical">A clinical unit (mmHg, % controlled)</option><option value="money">A value in money (₹)</option>
        <option value="multiple">Several different outcomes</option><option value="equal">Outcomes are equal across options</option></select>
      <div id="advOut"></div></div>
    <div class="callout"><b>${t.abbr} — ${t.name}.</b> ${t.def}</div>
    <div class="grid-wrap"><table class="data-grid"><thead><tr><th>Strategy</th><th>Cost ₹</th><th>${e.type==="CBA"?"Benefit ₹":e.type==="CUA"?"QALYs":"Effect"}</th><th></th></tr></thead><tbody id="evalRows">${rows}</tbody></table></div>
    <div class="btn-row"><button class="btn btn-secondary sm" id="addStrat">+ Add</button><button class="btn btn-ghost sm" id="clearStrat">Clear all</button><label class="btn btn-ghost sm" style="margin:0">Import CSV<input type="file" id="evalFile" accept=".csv"></label><button class="btn btn-ghost sm" id="tplEval">Template</button></div>
    ${t.wtp?`<div class="field"><label>WTP per QALY <span class="lab-val" id="wtpLab">${compactINR(e.wtp)}</span></label><input type="range" id="wtp" min="0" max="1000000" step="25000" value="${e.wtp}"><div class="wtp-chips"><button type="button" class="wchip" data-wtp="${GDP_PC}">1× GDP · ${compactINR(GDP_PC)}</button><button type="button" class="wchip" data-wtp="${GDP_PC*3}">3× GDP · ${compactINR(GDP_PC*3)}</button></div></div>`:""}
    <div class="divider"></div><button class="btn btn-primary btn-block" id="analyseBtn">Analyse</button>`;
  document.querySelectorAll("#evalType button").forEach(b=>b.onclick=()=>{e.type=b.dataset.t;renderEvalSidebar();renderEval();});
  const adv=document.getElementById("advisor");adv.onchange=()=>{const a=ADVISOR[adv.value];const out=document.getElementById("advOut");if(!a){out.innerHTML="";return;}out.innerHTML=`<div class="rec">→ Recommended: ${a.t}</div><p style="font-size:11.5px;color:var(--ink-soft);line-height:1.45">${a.why}</p><button class="btn btn-primary sm" id="useType" style="margin-top:6px">Use ${a.t}</button>`;document.getElementById("useType").onclick=()=>{e.type=a.t;renderEvalSidebar();renderEval();};};
  document.querySelectorAll("#evalRows [data-f]").forEach(el=>el.onchange=()=>e.strats[+el.dataset.i][el.dataset.f]=el.dataset.f==="strategy"?el.value:+el.value);
  document.querySelectorAll("#evalRows .row-del").forEach(el=>el.onclick=()=>{e.strats.splice(+el.dataset.del,1);renderEvalSidebar();});
  document.getElementById("addStrat").onclick=()=>{e.strats.push({strategy:"New strategy",cost:0,effect:0});renderEvalSidebar();};
  document.getElementById("clearStrat").onclick=()=>{e.strats=[];renderEvalSidebar();renderEval();};
  document.getElementById("tplEval").onclick=()=>dlTemplate("evaluation");
  document.getElementById("evalFile").onchange=ev=>{const f=ev.target.files[0];if(!f)return;const rd=new FileReader();rd.onload=()=>{e.strats=parseCSV(rd.result).map(r=>({strategy:r.strategy,cost:+r.cost,effect:+r.effect}));renderEvalSidebar();renderEval();};rd.readAsText(f);};
  const w=document.getElementById("wtp");if(w)w.oninput=()=>{e.wtp=+w.value;document.getElementById("wtpLab").textContent=compactINR(+w.value);};
  document.querySelectorAll("#sidebar .wchip").forEach(b=>b.onclick=()=>{e.wtp=+b.dataset.wtp;renderEvalSidebar();renderEval();});
  document.getElementById("analyseBtn").onclick=renderEval;
  wireExamples("evaluation");
}
function reqCard(req,typeName){
  const items=req.items.map(i=>`<li><span class="${i.ok?"ok":"no"}">${i.ok?"✓":"✗"}</span><span>${i.label}${i.warn?` — <span style="color:var(--red)">${i.warn}</span>`:""}</span></li>`).join("");
  return`<div class="card"><h3>Data requirements${req.missing?` — ${req.missing} missing`:" — all met"}</h3><div class="card-sub">For a ${typeName} analysis you need:</div><ul class="checklist">${items}</ul></div>`;
}
async function renderEval(){
  const e=state.evaluation,ws=document.getElementById("workspace"),t=EVAL_TYPES[e.type];
  wsBusy(t.name);
  let R;try{R=await api("evaluation",e);}catch(err){return wsError(err);}
  let head=`<div class="ws-head"><div><h2>${t.name} (${t.abbr})</h2><div class="sub">${t.def}</div></div>${EXPORT_BAR}</div>`;
  if(e.type==="CBA"){
    const d=R.rows,best=R.best;const bars=d.map(s=>({label:s.strategy,value:s.net,color:s.strategy===best.strategy?C.emerald:C.primary}));
    const rows=d.map(s=>`<tr><td>${s.strategy}</td><td>${fmtINR(s.cost)}</td><td>${fmtINR(s.benefit)}</td><td>${fmtINR(s.net)}</td><td>${fmtNum(s.bcr,2)}</td></tr>`).join("");
    ws.innerHTML=head+`<div class="kpis"><div class="kpi emerald"><div class="k-label">Best option</div><div class="k-val sm">${best.strategy}</div><div class="k-sub">highest net benefit</div></div><div class="kpi accent"><div class="k-label">Net benefit</div><div class="k-val mono">${compactINR(best.net)}</div><div class="k-sub">benefit − cost</div></div><div class="kpi gold"><div class="k-label">Benefit–cost ratio</div><div class="k-val mono">${fmtNum(best.bcr,2)}</div><div class="k-sub">&gt;1 = worthwhile</div></div></div>
      <div class="result-tabs" id="evTabs"><button class="result-tab active" data-p="chart">Net benefit</button><button class="result-tab" data-p="table">Table</button><button class="result-tab" data-p="req">Requirements</button></div>
      <div class="pane active" data-pane="chart"><div class="card flush"><div class="pad"><h3>Net monetary benefit</h3><div class="card-sub">Monetised benefit minus cost for each option.</div>${barChartV(bars,compactINR)}</div></div></div>
      <div class="pane" data-pane="table"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th>Strategy</th><th>Cost</th><th>Benefit</th><th>Net benefit</th><th>BCR</th></tr></thead><tbody>${rows}</tbody></table></div></div></div>
      <div class="pane" data-pane="req">${reqCard(R.requirements,t.name)}</div>`;
    wireTabs("evTabs");wireExports("Artha CBA");addInterp(R);return;
  }
  if(e.type==="CMA"){
    const d=R.rows,best=R.best,equal=R.equal;
    const rows=d.map(s=>`<tr><td>${s.strategy}</td><td>${fmtINR(s.cost)}</td><td>${fmtNum(+s.effect,2)}</td></tr>`).join("");
    ws.innerHTML=head+`<div class="kpis"><div class="kpi emerald"><div class="k-label">Lowest cost</div><div class="k-val sm">${best.strategy}</div><div class="k-sub">recommended</div></div><div class="kpi accent"><div class="k-label">Its cost</div><div class="k-val mono">${compactINR(best.cost)}</div></div><div class="kpi ${equal?'':'gold'}"><div class="k-label">Outcomes equal?</div><div class="k-val sm" style="color:${equal?C.emerald:C.amber}">${equal?"Yes":"Check"}</div><div class="k-sub">${equal?"CMA valid":"consider CEA/CUA"}</div></div></div>
      <div class="result-tabs" id="evTabs"><button class="result-tab active" data-p="table">Cost comparison</button><button class="result-tab" data-p="req">Requirements</button></div>
      <div class="pane active" data-pane="table"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th>Strategy</th><th>Cost</th><th>Outcome</th></tr></thead><tbody>${rows}</tbody></table></div>${equal?"":'<p class="note">Outcomes differ across options — cost-minimisation assumes equivalence. A CEA or CUA may be more appropriate.</p>'}</div></div>
      <div class="pane" data-pane="req">${reqCard(R.requirements,t.name)}</div>`;
    wireTabs("evTabs");wireExports("Artha CMA");addInterp(R);return;
  }
  if(e.type==="CCA"){
    const rows=R.rows.map(s=>`<tr><td>${s.strategy}</td><td>${fmtINR(+s.cost)}</td><td>${fmtNum(+s.effect,2)}</td></tr>`).join("");
    ws.innerHTML=head+`<div class="callout">Cost-consequence is <b>descriptive</b>: costs and outcomes are presented side by side, with no single summary ratio — the reader weighs them.</div>
      <div class="result-tabs" id="evTabs"><button class="result-tab active" data-p="table">Balance sheet</button><button class="result-tab" data-p="req">Requirements</button></div>
      <div class="pane active" data-pane="table"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th>Strategy</th><th>Cost</th><th>Outcome</th></tr></thead><tbody>${rows}</tbody></table></div></div></div>
      <div class="pane" data-pane="req">${reqCard(R.requirements,t.name)}</div>`;
    wireTabs("evTabs");wireExports("Artha CCA");addInterp(R);return;
  }
  /* CEA / CUA */
  const d=R.rows,best=R.best,pts=R.plane,unit=R.unit;
  const tagFor=s=>s.status==="dominated"?`<span class="tag dominated">dominated</span>`:s.status==="extended"?`<span class="tag ext">ext. dominance</span>`:`<span class="tag frontier">on frontier</span>`;
  const rows=d.map(s=>`<tr><td>${s.strategy}</td><td>${fmtINR(s.cost)}</td><td>${fmtNum(s.effect,2)}</td><td>${s.incCost==null?"—":fmtINR(s.incCost)}</td><td>${s.incEff==null?"—":fmtNum(s.incEff,2)}</td><td>${s.icer==null?"—":fmtINR(s.icer)}</td><td style="text-align:left">${tagFor(s)}</td><td>${fmtINR(s.nmb)}</td></tr>`).join("");
  ws.innerHTML=head+`<div class="kpis"><div class="kpi accent"><div class="k-label">Optimal at WTP</div><div class="k-val sm">${best.strategy}</div><div class="k-sub">at ${compactINR(e.wtp)}/${unit}</div></div><div class="kpi gold"><div class="k-label">Its ICER</div><div class="k-val mono">${best.icer==null?"ref":fmtINR(best.icer)}</div><div class="k-sub">per ${unit}</div></div><div class="kpi emerald"><div class="k-label">Net benefit</div><div class="k-val mono">${compactINR(best.nmb)}</div><div class="k-sub">NMB at WTP</div></div></div>
    <div class="result-tabs" id="evTabs"><button class="result-tab active" data-p="plane">CE plane</button><button class="result-tab" data-p="table">Incremental table</button><button class="result-tab" data-p="req">Requirements</button></div>
    <div class="pane active" data-pane="plane"><div class="card flush"><div class="pad"><h3>Cost-effectiveness plane</h3><div class="card-sub">Each option vs the cheapest. Dashed line = willingness-to-pay threshold.</div>${cePlane(pts,e.wtp)}</div><div class="legend"><div class="item"><span class="sw" style="background:${C.muted}"></span>Reference</div><div class="item"><span class="sw" style="background:${C.primary}"></span>Comparator</div><div class="item"><span class="sw" style="background:${C.emerald};height:3px;width:18px"></span>WTP threshold</div></div></div></div>
    <div class="pane" data-pane="table"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th>Strategy</th><th>Cost</th><th>${unit==="QALY"?"QALYs":"Effect"}</th><th>Δ Cost</th><th>Δ Eff</th><th>ICER</th><th style="text-align:left">Status</th><th>NMB</th></tr></thead><tbody>${rows}</tbody></table></div></div></div>
    <div class="pane" data-pane="req">${reqCard(R.requirements,t.name)}</div>`;
  wireTabs("evTabs");wireExports("Artha "+e.type);addInterp(R);
}

/* ============================ MODELING ============================ */
function renderModelSidebar(){
  const m=state.model;fixModel(m);
  const stRows=m.states.map((s,i)=>`<tr><td><input data-sf="name" data-i="${i}" value="${s.name}"></td><td><input data-sf="cost" data-i="${i}" type="number" value="${s.cost}" style="text-align:right"></td><td><input data-sf="util" data-i="${i}" type="number" step="0.01" value="${s.util}" style="text-align:right"></td><td><input data-sf="dw" data-i="${i}" type="number" step="0.01" value="${s.dw}" style="text-align:right"></td><td style="text-align:center"><input data-sf="absorbing" data-i="${i}" type="checkbox" ${s.absorbing?"checked":""}></td><td class="row-del" data-sdel="${i}">&times;</td></tr>`).join("");
  const strBtns=m.strategies.map((s,i)=>`<button data-strat="${i}" class="${m.activeStrat===i?"active":""}">${s.name}</button>`).join("");
  const as=m.strategies[m.activeStrat];
  const head=m.states.map(s=>`<th>${s.name.slice(0,4)}</th>`).join("");
  const matRows=as.matrix.map((row,i)=>{const rs=rowSum(row);const cells=row.map((v,j)=>`<td><input data-mr="${i}" data-mc="${j}" type="number" step="0.01" value="${(+v).toFixed(4).replace(/0+$/,"").replace(/\.$/,"")}" style="text-align:right"></td>`).join("");return `<tr><td style="font-family:var(--sans);font-weight:600;font-size:10px;padding:6px 8px;white-space:nowrap">${m.states[i].name.slice(0,8)}</td>${cells}<td style="text-align:center;font-family:var(--mono);font-size:10px;color:${Math.abs(rs-1)<0.005?'var(--emerald)':'var(--red)'}">${rs.toFixed(2)}</td></tr>`;}).join("");
  document.getElementById("sidebar").innerHTML=`<h2><span class="section-num">04</span> Markov model</h2>
    <p class="hint">A fully configurable state-transition model. Edit the states, each strategy's transition matrix, and the settings.</p>
    ${exRow("model")}
    <div class="sublabel">Health states (cost · utility · disability wt · dead?)</div>
    <div class="grid-wrap"><table class="data-grid"><thead><tr><th>State</th><th>Cost</th><th>Util</th><th>DW</th><th>Dead</th><th></th></tr></thead><tbody id="stRows">${stRows}</tbody></table></div>
    <div class="btn-row"><button class="btn btn-secondary sm" id="addState">+ State</button></div>
    <div class="sublabel">Strategies &amp; transition matrix</div>
    <div class="seg types" id="stratSel">${strBtns}</div>
    <div class="field two"><div><label>Strategy name</label><input type="text" id="stratName" value="${as.name}"></div><div><label>Added cost / cycle</label><input type="number" id="stratAdd" value="${as.addCost}"></div></div>
    <p class="hint" style="margin:-4px 0 8px">Row = from-state, column = to-state. Each row should sum to 1.0 (shown right).</p>
    <div class="grid-wrap"><table class="data-grid"><thead><tr><th>from\\to</th>${head}<th>Σ</th></tr></thead><tbody id="matRows">${matRows}</tbody></table></div>
    <div class="btn-row"><button class="btn btn-secondary sm" id="addStrat">+ Strategy</button><button class="btn btn-ghost sm" id="delStrat">Remove</button></div>
    <div class="sublabel">Outcome &amp; settings</div>
    <div class="seg" id="outcomeSel"><button data-o="QALY" class="${m.outcome==="QALY"?"active":""}">QALYs</button><button data-o="DALY" class="${m.outcome==="DALY"?"active":""}">DALYs</button></div>
    ${m.outcome==="DALY"?`<div class="field"><label>Life expectancy at death (yrs, for YLL)</label><input type="number" id="lifeExp" value="${m.lifeExp}"></div>`:""}
    <div class="field two"><div><label>Cycle (yrs)</label><input type="number" id="cycle" step="0.5" value="${m.cycle}"></div><div><label>Horizon (yrs)</label><input type="number" id="horizon" value="${m.horizon}"></div></div>
    <div class="field two"><div><label>Disc. cost</label><input type="number" id="dCost" step="0.01" value="${m.dCost}"></div><div><label>Disc. effect</label><input type="number" id="dEff" step="0.01" value="${m.dEff}"></div></div>
    <div class="field"><label>WTP per ${m.outcome==="QALY"?"QALY":"DALY averted"} <span class="lab-val" id="mwtpLab">${compactINR(m.wtp)}</span></label><input type="range" id="mwtp" min="0" max="1000000" step="25000" value="${m.wtp}"><div class="wtp-chips"><button type="button" class="wchip" data-mwtp="${GDP_PC}">1× GDP · ${compactINR(GDP_PC)}</button><button type="button" class="wchip" data-mwtp="${GDP_PC*3}">3× GDP · ${compactINR(GDP_PC*3)}</button></div></div>
    <div class="divider"></div><button class="btn btn-primary btn-block" id="runModel">Run model</button>`;
  // state grid
  document.querySelectorAll("#stRows [data-sf]").forEach(el=>el.onchange=()=>{const f=el.dataset.sf,i=+el.dataset.i;m.states[i][f]=f==="name"?el.value:f==="absorbing"?el.checked:+el.value;if(f==="name"||f==="absorbing")renderModelSidebar();});
  document.querySelectorAll("#stRows .row-del").forEach(el=>el.onclick=()=>{if(m.states.length<=2)return;m.states.splice(+el.dataset.sdel,1);m.strategies.forEach(s=>{s.matrix.splice(+el.dataset.sdel,1);s.matrix.forEach(r=>r.splice(+el.dataset.sdel,1));});renderModelSidebar();});
  document.getElementById("addState").onclick=()=>{m.states.push({name:"State "+(m.states.length+1),cost:0,util:0.5,dw:0.2,absorbing:false});fixModel(m);renderModelSidebar();};
  // strategy selector
  document.querySelectorAll("#stratSel button").forEach(b=>b.onclick=()=>{m.activeStrat=+b.dataset.strat;renderModelSidebar();});
  document.getElementById("stratName").onchange=e=>{as.name=e.target.value;renderModelSidebar();};
  document.getElementById("stratAdd").onchange=e=>as.addCost=+e.target.value;
  document.querySelectorAll("#matRows [data-mr]").forEach(el=>el.onchange=()=>{as.matrix[+el.dataset.mr][+el.dataset.mc]=+el.value;renderModelSidebar();});
  document.getElementById("addStrat").onclick=()=>{const n=m.states.length,id=[];for(let i=0;i<n;i++){const r=new Array(n).fill(0);r[i]=1;id.push(r);}m.strategies.push({name:"Strategy "+(m.strategies.length+1),addCost:0,matrix:id});m.activeStrat=m.strategies.length-1;renderModelSidebar();};
  document.getElementById("delStrat").onclick=()=>{if(m.strategies.length<=2)return;m.strategies.splice(m.activeStrat,1);m.activeStrat=0;renderModelSidebar();};
  document.querySelectorAll("#outcomeSel button").forEach(b=>b.onclick=()=>{m.outcome=b.dataset.o;renderModelSidebar();renderModel();});
  const le=document.getElementById("lifeExp");if(le)le.onchange=e=>m.lifeExp=+e.target.value;
  ["cycle","horizon","dCost","dEff"].forEach(k=>{const el=document.getElementById(k);el.onchange=()=>m[k]=+el.value;});
  const w=document.getElementById("mwtp");w.oninput=()=>{m.wtp=+w.value;document.getElementById("mwtpLab").textContent=compactINR(+w.value);};
  document.querySelectorAll("#sidebar .wchip").forEach(b=>b.onclick=()=>{m.wtp=+b.dataset.mwtp;renderModelSidebar();renderModel();});
  document.getElementById("runModel").onclick=renderModel;
  wireExamples("model");
}
async function renderModel(){
  const m=state.model,ws=document.getElementById("workspace");wsBusy("Markov model");
  let R;try{R=await api("model",m);}catch(e){return wsError(e);}
  const inc=R.rows,best=R.best,unit=R.unit,lowerBetter=m.outcome==="DALY";
  const tagFor=s=>s.status==="dominated"?`<span class="tag dominated">dominated</span>`:s.status==="extended"?`<span class="tag ext">ext. dom.</span>`:`<span class="tag frontier">frontier</span>`;
  const incRows=inc.map(s=>`<tr><td>${s.name}</td><td>${fmtINR(s.cost)}</td><td>${fmtNum(s.qaly,3)}</td><td>${fmtNum(s.daly,3)}</td><td>${s.incCost==null?"—":fmtINR(s.incCost)}</td><td>${s.incEff==null?"—":fmtNum(s.incEff,3)}</td><td>${s.icer==null?"—":fmtINR(s.icer)}</td><td style="text-align:left">${tagFor(s)}</td></tr>`).join("");
  const pts=R.plane,series=R.series;
  ws.innerHTML=`<div class="ws-head"><div><h2>Decision-analytic modeling</h2><div class="sub">Configurable ${m.states.length}-state Markov, ${m.strategies.length} strategies, half-cycle correction, ${m.horizon}-yr horizon, ${pct(m.dCost,0)} discounting. Outcome: ${m.outcome==="QALY"?"QALYs":"DALYs"}.</div></div>${EXPORT_BAR}</div>
    <div class="kpis"><div class="kpi accent"><div class="k-label">Optimal at WTP</div><div class="k-val sm">${best.name}</div><div class="k-sub">at ${compactINR(m.wtp)}/${unit}</div></div><div class="kpi gold"><div class="k-label">Its ICER</div><div class="k-val mono">${best.icer==null?"ref":fmtINR(best.icer)}</div><div class="k-sub">per ${unit}</div></div><div class="kpi emerald"><div class="k-label">Strategies</div><div class="k-val mono">${m.strategies.length}</div><div class="k-sub">${R.onFr} on frontier</div></div></div>
    <div class="result-tabs" id="mTabs"><button class="result-tab active" data-p="trace">Cohort trace</button><button class="result-tab" data-p="ce">CE plane</button><button class="result-tab" data-p="inc">Incremental</button></div>
    <div class="pane active" data-pane="trace"><div class="card flush"><div class="pad"><h3>Cohort trace — ${R.activeName}</h3><div class="card-sub">Share of the cohort in each state over time (switch strategy in the sidebar).</div>${lineChart(series,"Years",m.horizon)}</div><div class="legend">${m.states.map((s,i)=>`<div class="item"><span class="sw" style="background:${SERIES[i%6]}"></span>${s.name}</div>`).join("")}</div></div></div>
    <div class="pane" data-pane="ce"><div class="card flush"><div class="pad"><h3>Cost-effectiveness plane</h3><div class="card-sub">Each strategy vs the cheapest. ${lowerBetter?"X = DALYs averted (right is better).":"X = incremental QALYs."}</div>${cePlane(pts,m.wtp)}</div></div></div>
    <div class="pane" data-pane="inc"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th>Strategy</th><th>Cost</th><th>QALYs</th><th>DALYs</th><th>Δ Cost</th><th>Δ ${unit}</th><th>ICER</th><th style="text-align:left">Status</th></tr></thead><tbody>${incRows}</tbody></table></div><p class="note">ICER in ₹ per ${unit}. India thresholds: 1×GDP ≈ ${fmtINR(GDP_PC)}, 3×GDP ≈ ${fmtINR(GDP_PC*3)}.</p></div></div>`;
  wireTabs("mTabs");wireExports("Artha Markov Model");addInterp(R);
}

/* ============================ SENSITIVITY ============================ */
function renderSensSidebar(){
  const s=state.sens,m=state.model;
  if(s.ref==null)s.ref=0; if(s.cmp==null)s.cmp=1;
  const opts=m.strategies.map((st,i)=>`<option value="${i}">${st.name}</option>`).join("");
  document.getElementById("sidebar").innerHTML=`<h2><span class="section-num">05</span> Sensitivity</h2>
    <p class="hint">How robust is the model's conclusion to uncertainty — deterministic (tornado) and probabilistic (PSA → CEAC, EVPI).</p>
    <div class="callout">Runs on the <b>Markov model</b> from the Modeling tab (outcome: <b>${m.outcome}</b>). Adjust states/matrix there, then re-run.</div>
    <div class="field two"><div><label>Reference</label><select id="sRef">${opts}</select></div><div><label>Compared</label><select id="sCmp">${opts}</select></div></div>
    <div class="field"><label>PSA iterations <span class="lab-val" id="nLab">${s.N}</span></label><input type="range" id="psaN" min="200" max="3000" step="100" value="${s.N}"></div>
    <div class="field"><label>WTP per ${m.outcome==="QALY"?"QALY":"DALY averted"} <span class="lab-val" id="swtpLab">${compactINR(s.wtp)}</span></label><input type="range" id="swtp" min="0" max="1000000" step="25000" value="${s.wtp}"></div>
    <div class="divider"></div><button class="btn btn-primary btn-block" id="runSens">Run analysis</button>`;
  document.getElementById("sRef").value=Math.min(s.ref,m.strategies.length-1);
  document.getElementById("sCmp").value=Math.min(s.cmp,m.strategies.length-1);
  document.getElementById("sRef").onchange=e=>s.ref=+e.target.value;
  document.getElementById("sCmp").onchange=e=>s.cmp=+e.target.value;
  const n=document.getElementById("psaN");n.oninput=()=>{s.N=+n.value;document.getElementById("nLab").textContent=s.N;};
  const w=document.getElementById("swtp");w.oninput=()=>{s.wtp=+w.value;document.getElementById("swtpLab").textContent=compactINR(+w.value);};
  document.getElementById("runSens").onclick=renderSens;
}
async function renderSens(){
  const s=state.sens,m=state.model,ws=document.getElementById("workspace");
  ws.innerHTML=`<div class="ws-head"><div><h2>Sensitivity analysis</h2><div class="sub">Running ${s.N} Monte-Carlo iterations on the server…</div></div></div>`;
  let R;try{R=await api("sensitivity",{model:m,N:s.N,wtp:s.wtp,ref:s.ref,cmp:s.cmp});}catch(e){return wsError(e);}
  const unit=R.unit;
  ws.innerHTML=`<div class="ws-head"><div><h2>Sensitivity analysis</h2><div class="sub">${R.N} PSA iterations: <b>${R.cmpName}</b> vs <b>${R.refName}</b> (outcome ${m.outcome}). Transition rows sampled Dirichlet; costs Gamma; utilities Beta.</div></div>${EXPORT_BAR}</div>
      <div class="kpis"><div class="kpi accent"><div class="k-label">P(cost-effective)</div><div class="k-val mono">${pct(R.pCE)}</div><div class="k-sub">at ${compactINR(R.wtp)}/${unit}</div></div><div class="kpi gold"><div class="k-label">EVPI / patient</div><div class="k-val mono">${compactINR(R.evpi)}</div><div class="k-sub">value of removing uncertainty</div></div><div class="kpi"><div class="k-label">Iterations</div><div class="k-val mono">${R.N}</div></div></div>
      <div class="result-tabs" id="sTabs"><button class="result-tab active" data-p="ceac">CEAC</button><button class="result-tab" data-p="scatter">PSA scatter</button><button class="result-tab" data-p="tor">Tornado (DSA)</button></div>
      <div class="pane active" data-pane="ceac"><div class="card flush"><div class="pad"><h3>Cost-effectiveness acceptability curve</h3><div class="card-sub">Probability the new treatment is cost-effective across WTP thresholds.</div>${ceacChart(R.ceac)}</div><div class="legend"><div class="item"><span class="sw" style="background:${C.primary}"></span>P(new cost-effective)</div><div class="item"><span class="sw" style="background:${C.gold};height:3px;width:18px"></span>1× / 3× GDP</div></div></div></div>
      <div class="pane" data-pane="scatter"><div class="card flush"><div class="pad"><h3>PSA on the CE plane</h3><div class="card-sub">Each point = one Monte-Carlo draw. Purple = cost-effective at current WTP.</div>${scatterPSA(R.draws,R.wtp)}</div></div></div>
      <div class="pane" data-pane="tor"><div class="card flush"><div class="pad"><h3>One-way sensitivity — tornado</h3><div class="card-sub">Swing in net monetary benefit as each parameter moves to its low/high value.</div>${tornado(R.tornado)}</div><div class="legend"><div class="item"><span class="sw" style="background:${C.amber}"></span>Low value</div><div class="item"><span class="sw" style="background:${C.primary}"></span>High value</div></div></div></div>`;
  wireTabs("sTabs");wireExports("Artha Sensitivity");
}

/* ============================ BUDGET IMPACT ============================ */
function renderBiaSidebar(){
  const b=state.bia,f=(id,l)=>`<div class="field"><label>${l}</label><input type="number" id="${id}" value="${b[id]}"></div>`;
  document.getElementById("sidebar").innerHTML=`<h2><span class="section-num">06</span> Budget impact</h2>
    <p class="hint">Projects the payer's annual and cumulative spend as a new treatment is adopted over time.</p>
    ${exRow("bia")}
    ${f("population","Catchment population")}
    <div class="field"><label>Eligible / prevalent share <span class="lab-val" id="eligLab">${pct(b.eligible)}</span></label><input type="range" id="eligible" min="0" max="0.5" step="0.005" value="${b.eligible}"></div>
    <div class="field"><label>Peak uptake of new tx <span class="lab-val" id="upLab">${pct(b.maxUptake)}</span></label><input type="range" id="maxUptake" min="0" max="1" step="0.05" value="${b.maxUptake}"></div>
    <div class="field two"><div><label>Horizon (yrs)</label><input type="number" id="horizon" value="${b.horizon}"></div><div><label>Start year</label><input type="number" id="startYear" value="${b.startYear}"></div></div>
    <div class="field two"><div><label>New tx cost/yr ₹</label><input type="number" id="costNew" value="${b.costNew}"></div><div><label>Current cost/yr ₹</label><input type="number" id="costOld" value="${b.costOld}"></div></div>
    <div class="divider"></div><button class="btn btn-primary btn-block" id="runBia">Project budget</button>`;
  ["population","horizon","startYear","costNew","costOld"].forEach(k=>{const el=document.getElementById(k);el.onchange=()=>b[k]=+el.value;});
  const el=document.getElementById("eligible");el.oninput=()=>{b.eligible=+el.value;document.getElementById("eligLab").textContent=pct(b.eligible);};
  const up=document.getElementById("maxUptake");up.oninput=()=>{b.maxUptake=+up.value;document.getElementById("upLab").textContent=pct(b.maxUptake);};
  document.getElementById("runBia").onclick=renderBia;
  wireExamples("bia");
}
async function renderBia(){
  const b=state.bia,ws=document.getElementById("workspace");wsBusy("Budget impact");
  let R;try{R=await api("bia",b);}catch(e){return wsError(e);}
  const rows=R.rows,totalImpact=R.cumulative;
  const bars=rows.map(r=>({label:r.year,value:r.impact,color:C.primary}));
  const tr=rows.map(r=>`<tr><td>${r.year}</td><td>${pct(r.uptake)}</td><td>${fmtNum(r.treated,0)}</td><td>${compactINR(r.worldNew)}</td><td>${compactINR(r.impact)}</td><td>${compactINR(r.cum)}</td></tr>`).join("");
  ws.innerHTML=`<div class="ws-head"><div><h2>Budget impact analysis</h2><div class="sub">Payer view: net spend as the new treatment is adopted over ${b.horizon} years from ${b.startYear}.</div></div>${EXPORT_BAR}</div>
    <div class="kpis"><div class="kpi accent"><div class="k-label">Eligible patients</div><div class="k-val mono">${fmtNum(b.population*b.eligible,0)}</div><div class="k-sub">of ${fmtNum(b.population,0)} pop.</div></div><div class="kpi gold"><div class="k-label">Cumulative impact</div><div class="k-val">${compactINR(totalImpact)}</div><div class="k-sub">over ${b.horizon} years</div></div><div class="kpi"><div class="k-label">Peak-year impact</div><div class="k-val mono">${compactINR(Math.max(...rows.map(r=>r.impact)))}</div><div class="k-sub">${rows[rows.length-1].year}</div></div></div>
    <div class="result-tabs" id="bTabs"><button class="result-tab active" data-p="chart">Annual impact</button><button class="result-tab" data-p="tbl">Year-by-year</button></div>
    <div class="pane active" data-pane="chart"><div class="card flush"><div class="pad"><h3>Net annual budget impact</h3><div class="card-sub">Additional spend vs staying on current treatment.</div>${barChartV(bars,compactINR)}</div></div></div>
    <div class="pane" data-pane="tbl"><div class="card"><div class="table-scroll"><table class="results-table"><thead><tr><th>Year</th><th>Uptake</th><th>On new tx</th><th>Total spend</th><th>Net impact</th><th>Cumulative</th></tr></thead><tbody>${tr}</tbody></table></div></div></div>`;
  wireTabs("bTabs");wireExports("Artha Budget Impact");
}

/* ============================ PERSISTENCE ============================ */
function saveLocal(){try{localStorage.setItem("arthaHE_v1",JSON.stringify(state));}catch(e){}}
function loadLocal(){try{const s=localStorage.getItem("arthaHE_v1");if(s){const o=JSON.parse(s);Object.keys(o).forEach(k=>{if(k!=="module"&&state[k]!==undefined)state[k]=o[k];});}}catch(e){}}
function exportProject(){dl(new Blob([JSON.stringify(state,null,2)],{type:"application/json"}),"artha_project_"+new Date().toISOString().slice(0,10)+".json");}
function importProject(file){const rd=new FileReader();rd.onload=()=>{try{const o=JSON.parse(rd.result);Object.keys(o).forEach(k=>{if(k!=="module"&&state[k]!==undefined)state[k]=o[k];});saveLocal();enterApp(o.module&&o.module!=="home"&&o.module!=="methods"?o.module:"costing");}catch(e){alert("That doesn't look like a valid Artha HE project file.");}};rd.readAsText(file);}

/* ============================ GLOSSARY ============================ */
const GLOSSARY={
  ICER:"Incremental cost-effectiveness ratio — the extra cost per extra unit of effect (e.g. per QALY) of one option versus the next best.",
  QALY:"Quality-adjusted life-year — one year in perfect health. Combines length and quality of life (utility × time).",
  DALY:"Disability-adjusted life-year — one lost year of healthy life. Used widely in LMIC/GBD analyses.",
  WTP:"Willingness-to-pay — the most a decision-maker will pay for one unit of health gain; often a multiple of GDP per capita.",
  NMB:"Net monetary benefit — effect × WTP − cost. Positive means worthwhile at that threshold.",
  PSA:"Probabilistic sensitivity analysis — re-runs the model thousands of times sampling uncertain inputs from distributions.",
  CEAC:"Cost-effectiveness acceptability curve — the probability an option is cost-effective across willingness-to-pay values.",
  EVPI:"Expected value of perfect information — the value of removing all decision uncertainty; an upper bound on further research.",
  Dominance:"A strategy is dominated if another costs less and is at least as effective (strong), or is beaten by a mix of others (extended).",
  "Half-cycle correction":"Adjustment assuming transitions happen mid-cycle, averaging start/end state occupancy to reduce bias.",
  Discounting:"Converting future costs and health to present value, because outcomes now are valued more than the same later.",
  CHE:"Catastrophic health expenditure — out-of-pocket spending above a threshold of income (10%/25%) or capacity-to-pay (40%).",
  CMA:"Cost-minimisation analysis — when outcomes are equivalent, compare costs only.",
  CEA:"Cost-effectiveness analysis — cost per natural-unit outcome (e.g. life-year, case averted).",
  CUA:"Cost-utility analysis — cost per QALY/DALY; the HTA standard.",
  CBA:"Cost-benefit analysis — costs and outcomes both valued in money (net benefit, benefit-cost ratio).",
  BIA:"Budget impact analysis — the affordability/total spend consequence of adopting a new option over time."
};
function term(t,label){const d=GLOSSARY[t];return d?`<abbr class="gl" title="${d.replace(/"/g,"&quot;")}">${label||t}</abbr>`:(label||t);}

/* ============================ METHODS & VALIDATION ============================ */
async function renderMethods(){
  const ws=document.getElementById("workspace");
  ws.innerHTML=`<div class="home"><div class="ws-head"><div><h2>Methods &amp; validation</h2><div class="sub">Loading…</div></div></div></div>`;
  let V;try{V=await api("validate",{model:state.model});}catch(e){return wsError(e);}
  const vals=V.rows,allok=V.allok;
  const glos=Object.keys(GLOSSARY).map(k=>`<dt>${k}</dt><dd>${GLOSSARY[k]}</dd>`).join("");
  ws.innerHTML=`<div class="home">
    <div class="ws-head"><div><h2>Methods &amp; validation</h2><div class="sub">All of Artha HE's calculations run on a private server engine — the formulae are not exposed in the browser. Below are live checks that the engine reproduces known, hand-calculated results.</div></div></div>
    <div class="card" style="border-left:3px solid ${allok?'var(--emerald)':'var(--red)'}">
      <h3>Engine validation ${allok?'<span class="tag frontier">all checks passed</span>':'<span class="tag dominated">check failed</span>'}</h3>
      <div class="card-sub">Each row recomputes a quantity in the engine and compares it to a known answer.</div>
      ${vals.map(v=>`<div class="valid-row"><span class="${v.ok?'vok':'vno'}">${v.ok?'✓':'✗'}</span><span>${v.n}</span><span class="vp">${v.got} = ${v.exp}</span></div>`).join("")}
    </div>
    <div class="card"><h3>Standards followed</h3><div class="card-sub">Discounting, half-cycle correction, ICER &amp; dominance, probabilistic sensitivity analysis, CEAC and EVPI — standard published health-economics methods, with reporting structured to the CHEERS 2022 checklist and defaults following Indian / LMIC reference cases.</div>
      <div style="display:flex;gap:8px;flex-wrap:wrap;margin-top:4px">${["Private server engine","CHEERS 2022","Half-cycle correction","PSA · CEAC · EVPI","India reference case"].map(b=>`<span class="pill">${b}</span>`).join("")}</div></div>
    <h2 class="sec">Glossary</h2>
    <p class="secsub">Plain-language definitions — hover the dotted terms anywhere in the app to see these.</p>
    <div class="card glossary"><dl>${glos}</dl></div>
    <footer class="foot">Artha HE · for research &amp; teaching · <b style="color:var(--ink-soft)">Developed by Dr G Hari Prakash</b></footer>
  </div>`;
}

/* ============================ REFERENCE DATA (India / LMIC) ============================ */
/* Indicative reference values with sources. Costs & disability weights are
   STARTING POINTS — users must confirm the current figure from the cited source. */
const REFDATA={
  thresholds:[
    {item:"GDP per capita, India",value:"≈ ₹2,00,000 (US$ ~2,500)",basis:"2023–24",source:"World Bank / MoSPI",url:"https://data.worldbank.org/indicator/NY.GDP.PCAP.CN?locations=IN"},
    {item:"Cost-effectiveness threshold (1× GDP/capita)",value:"≈ ₹2,00,000 per QALY",basis:"WHO-CHOICE convention (illustrative)",source:"WHO-CHOICE",url:"https://www.who.int/teams/health-systems-governance-and-financing/economic-analysis"},
    {item:"Threshold (3× GDP/capita)",value:"≈ ₹6,00,000 per QALY",basis:"upper illustrative bound",source:"WHO-CHOICE",url:"https://www.who.int/teams/health-systems-governance-and-financing/economic-analysis"},
    {item:"Discount rate (costs & health)",value:"3% per year",basis:"India HTA Reference Case",source:"DHR / HTAIn (2018)",url:"https://htain.icmr.org.in"},
    {item:"Analytic perspective",value:"Health-system / societal",basis:"state the chosen perspective",source:"India HTA Reference Case",url:"https://htain.icmr.org.in"},
    {item:"Medical inflation (CPI)",value:"≈ 5% per year",basis:"indicative",source:"MoSPI CPI",url:"https://www.mospi.gov.in"}
  ],
  unitCosts:[
    {item:"Outpatient consultation (public, secondary)",value:300,unit:"per visit",category:"Direct medical"},
    {item:"Inpatient bed-day (public, general ward)",value:2500,unit:"per day",category:"Direct medical"},
    {item:"Inpatient bed-day (public, ICU)",value:7000,unit:"per day",category:"Direct medical"},
    {item:"Complete blood count (CBC)",value:150,unit:"per test",category:"Direct medical"},
    {item:"Biochemistry panel",value:400,unit:"per test",category:"Direct medical"},
    {item:"X-ray (single view)",value:200,unit:"per film",category:"Direct medical"},
    {item:"ECG",value:100,unit:"per test",category:"Direct medical"},
    {item:"Ultrasound",value:600,unit:"per scan",category:"Direct medical"},
    {item:"ANM / health-worker time",value:120,unit:"per hour",category:"Direct medical"},
    {item:"Staff nurse time",value:250,unit:"per hour",category:"Direct medical"},
    {item:"Medical officer time",value:600,unit:"per hour",category:"Direct medical"},
    {item:"Ambulance trip",value:500,unit:"per trip",category:"Direct non-medical"}
  ],
  unitCostSource:{name:"NHSRC — Cost of Health Services in India (CHSI) & CGHS rate list",url:"https://nhsrcindia.org"},
  weights:[
    {state:"No disability / full health",dw:0.00,util:1.00,basis:"by definition"},
    {state:"Mild",dw:0.05,util:0.95,basis:"indicative band"},
    {state:"Moderate",dw:0.20,util:0.80,basis:"indicative band"},
    {state:"Severe",dw:0.40,util:0.60,basis:"indicative band"},
    {state:"Very severe",dw:0.55,util:0.45,basis:"indicative band"},
    {state:"Dead",dw:1.00,util:0.00,basis:"by definition"}
  ],
  weightSource:{name:"GBD 2019 disability-weight bands — use the specific weight for your condition",url:"https://www.healthdata.org/research-analysis/gbd"},
  lifeTable:[
    {age:0,le:70},{age:15,le:57},{age:30,le:43},{age:45,le:30},{age:60,le:18},{age:70,le:11}
  ],
  lifeSource:{name:"India abridged life table (SRS) — indicative",url:"https://censusindia.gov.in/census.website/data/SRSLT"}
};
function renderReference(){
  const ws=document.getElementById("workspace");
  const tT=REFDATA.thresholds.map(r=>`<tr><td>${r.item}</td><td>${r.value}</td><td style="text-align:left">${r.basis}</td><td style="text-align:left"><a href="${r.url}" target="_blank" rel="noopener">${r.source} ↗</a></td></tr>`).join("");
  const tC=REFDATA.unitCosts.map(r=>`<tr><td>${r.item}</td><td>${fmtINR(r.value)}</td><td style="text-align:left">${r.unit}</td><td style="text-align:left">${r.category}</td></tr>`).join("");
  const tW=REFDATA.weights.map(r=>`<tr><td style="text-align:left">${r.state}</td><td>${fmtNum(r.util,2)}</td><td>${fmtNum(r.dw,2)}</td><td style="text-align:left">${r.basis}</td></tr>`).join("");
  const tL=REFDATA.lifeTable.map(r=>`<tr><td>${r.age}</td><td>${r.le}</td></tr>`).join("");
  ws.innerHTML=`<div class="home">
    <div class="ws-head"><div><h2>Reference data — India / LMIC</h2><div class="sub">A starting library of thresholds, unit costs, disability weights and life expectancy, each with its source — so you can set up a credible analysis without hunting through PDFs.</div></div></div>
    <div class="callout" style="background:var(--gold-soft);color:#8A6712"><b>Important:</b> unit costs and disability weights below are <b>indicative starting values</b>. Always confirm the current figure from the cited official source before using it in a formal analysis or publication.</div>
    <div class="card"><h3>Thresholds, discounting &amp; reference case</h3><div class="table-scroll"><table class="results-table"><thead><tr><th>Parameter</th><th>Value</th><th style="text-align:left">Basis</th><th style="text-align:left">Source</th></tr></thead><tbody>${tT}</tbody></table></div></div>
    <div class="card"><h3>Indicative unit costs (₹)</h3><div class="card-sub">Source: <a href="${REFDATA.unitCostSource.url}" target="_blank" rel="noopener">${REFDATA.unitCostSource.name} ↗</a>. Use the <b>Costing</b> tab's "Add from reference library" to insert any of these.</div><div class="table-scroll"><table class="results-table"><thead><tr><th>Item</th><th>Unit cost</th><th style="text-align:left">Unit</th><th style="text-align:left">Category</th></tr></thead><tbody>${tC}</tbody></table></div></div>
    <div class="two-col">
      <div class="card"><h3>Utilities &amp; disability weights</h3><div class="card-sub">${REFDATA.weightSource.name}. <a href="${REFDATA.weightSource.url}" target="_blank" rel="noopener">GBD ↗</a></div><div class="table-scroll"><table class="results-table"><thead><tr><th style="text-align:left">Severity</th><th>Utility</th><th>Disability wt</th><th style="text-align:left">Basis</th></tr></thead><tbody>${tW}</tbody></table></div></div>
      <div class="card"><h3>Life expectancy (for YLL/DALYs)</h3><div class="card-sub"><a href="${REFDATA.lifeSource.url}" target="_blank" rel="noopener">${REFDATA.lifeSource.name} ↗</a></div><div class="table-scroll"><table class="results-table"><thead><tr><th>Age (yrs)</th><th>Remaining life expectancy</th></tr></thead><tbody>${tL}</tbody></table></div></div>
    </div>
    <footer class="foot">Artha HE · Reference data is indicative — verify against the cited source · Developed by Dr G Hari Prakash</footer>
  </div>`;
}

/* ============================ ROUTER ============================ */
function wireTabs(id){const host=document.getElementById(id);host.querySelectorAll(".result-tab").forEach(tab=>tab.onclick=()=>{host.querySelectorAll(".result-tab").forEach(t=>t.classList.remove("active"));tab.classList.add("active");document.querySelectorAll(".pane").forEach(p=>p.classList.toggle("active",p.dataset.pane===tab.dataset.p));});}
function route(mod){
  state.module=mod;
  document.querySelectorAll("#topnav button").forEach(b=>b.classList.toggle("active",b.dataset.mod===mod));
  const layout=document.getElementById("layout");
  if(mod==="methods"||mod==="reference"){layout.classList.add("full");(mod==="methods"?renderMethods:renderReference)();window.scrollTo(0,0);saveLocal();return;}
  layout.classList.remove("full");
  ({costing:()=>{renderCostingSidebar();renderCosting();},
    oop:()=>{renderOopSidebar();renderOop();},
    evaluation:()=>{renderEvalSidebar();renderEval();},
    modeling:()=>{renderModelSidebar();renderModel();},
    sensitivity:()=>{renderSensSidebar();renderSens();},
    bia:()=>{renderBiaSidebar();renderBia();}})[mod]();
  saveLocal();
}
document.querySelectorAll("#topnav button").forEach(b=>b.onclick=()=>route(b.dataset.mod));
document.getElementById("brandHome").onclick=backToLanding;
document.getElementById("saveProj").onclick=exportProject;
document.getElementById("loadProj").onchange=e=>{if(e.target.files[0])importProject(e.target.files[0]);};
document.getElementById("reportBtn").onclick=openReport;
window.addEventListener("beforeunload",saveLocal);
setInterval(saveLocal,5000);
/* draggable splitter between the entry (sidebar) and results (workspace) panels */
(function(){
  const bar=document.getElementById("dragbar");if(!bar)return;let drag=false;
  bar.addEventListener("mousedown",e=>{drag=true;bar.classList.add("dragging");document.body.style.userSelect="none";document.body.style.cursor="col-resize";e.preventDefault();});
  window.addEventListener("mousemove",e=>{if(!drag)return;const x=Math.max(300,Math.min(820,e.clientX));document.documentElement.style.setProperty("--sbw",x+"px");});
  window.addEventListener("mouseup",()=>{if(!drag)return;drag=false;bar.classList.remove("dragging");document.body.style.userSelect="";document.body.style.cursor="";try{localStorage.setItem("arthaSBW",getComputedStyle(document.documentElement).getPropertyValue("--sbw").trim());}catch(e){}});
  bar.addEventListener("dblclick",()=>{document.documentElement.style.setProperty("--sbw","408px");try{localStorage.removeItem("arthaSBW");}catch(e){}});
  try{const w=localStorage.getItem("arthaSBW");if(w)document.documentElement.style.setProperty("--sbw",w);}catch(e){}
})();
loadLocal();
renderLanding();
