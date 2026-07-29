// Compute reference outputs from the Artha HE engine for validation.
const { COMPUTE } = require("../engine.js");

const MODEL = {
  states:[{name:"Healthy",cost:2000,util:0.92,dw:0.05,absorbing:false},
          {name:"Sick",cost:14000,util:0.62,dw:0.40,absorbing:false},
          {name:"Dead",cost:0,util:0,dw:0,absorbing:true}],
  strategies:[{name:"Standard care",addCost:0,matrix:[[0.84,0.15,0.01],[0,0.90,0.10],[0,0,1]]},
              {name:"New treatment",addCost:9000,matrix:[[0.893,0.0975,0.0095],[0,0.90,0.10],[0,0,1]]}],
  cycle:1,horizon:30,dCost:0.03,dEff:0.03,wtp:200000,outcome:"QALY",lifeExp:25,activeStrat:1
};
const EVAL = {type:"CUA",wtp:200000,strats:[
  {strategy:"Standard care",cost:40000,effect:3.5},
  {strategy:"New drug A",cost:85000,effect:4.4},
  {strategy:"New drug B",cost:120000,effect:4.7}]};
const COSTING = {method:"micro",toYear:2024,inflation:0.05,rows:[
  {item:"Consultation",category:"Direct medical",quantity:4,unit_cost:300,year:2024},
  {item:"Test",category:"Direct medical",quantity:2,unit_cost:450,year:2024},
  {item:"Drug",category:"Direct medical",quantity:12,unit_cost:120,year:2024}]};

const m = COMPUTE.model(MODEL);
const std = m.rows.find(r=>r.name==="Standard care");
const nt = m.rows.find(r=>r.name==="New treatment");
const e = COMPUTE.evaluation(EVAL);
const c = COMPUTE.costing(COSTING);
console.log(JSON.stringify({
  model:{ std_cost:std.cost, std_qaly:std.qaly, nt_cost:nt.cost, nt_qaly:nt.qaly, icer:nt.icer },
  eval_icer_B_vs_A: e.rows.find(r=>r.strategy==="New drug A").icer,
  costing_total: c.total
}, null, 2));
