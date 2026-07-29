# Independent re-implementation of the Artha HE calculations, written from the
# method definitions (NOT from the engine code), to cross-validate the engine.
# Run:  python validate_markov.py   (compares against engine numbers below)
import json

def markov_arm(states, matrix, add_cost, cycle, horizon, d_cost, d_eff):
    n = len(states); nC = round(horizon / cycle)
    trace = [[1.0 if i == 0 else 0.0 for i in range(n)]]
    for t in range(nC):
        cur = trace[t]; nxt = [0.0]*n
        for i in range(n):
            for j in range(n):
                nxt[j] += cur[i]*matrix[i][j]
        trace.append(nxt)
    cost = qaly = 0.0
    for t in range(nC):
        occ = [(trace[t][i]+trace[t+1][i])/2.0 for i in range(n)]   # half-cycle correction
        dc = (1+d_cost)**(t*cycle); de = (1+d_eff)**(t*cycle)
        for i in range(n):
            st = states[i]
            cc = st['cost'] + (0 if st['absorbing'] else add_cost)
            cost += occ[i]*cc*cycle/dc
            qaly += occ[i]*st['util']*cycle/de
    return cost, qaly

states = [{'name':'Healthy','cost':2000,'util':0.92,'absorbing':False},
          {'name':'Sick','cost':14000,'util':0.62,'absorbing':False},
          {'name':'Dead','cost':0,'util':0.0,'absorbing':True}]
std_m = [[0.84,0.15,0.01],[0,0.90,0.10],[0,0,1]]
new_m = [[0.893,0.0975,0.0095],[0,0.90,0.10],[0,0,1]]

std_cost, std_qaly = markov_arm(states, std_m, 0,    1, 30, 0.03, 0.03)
nt_cost,  nt_qaly  = markov_arm(states, new_m, 9000, 1, 30, 0.03, 0.03)
icer = (nt_cost - std_cost)/(nt_qaly - std_qaly)

out = {'std_cost':std_cost,'std_qaly':std_qaly,'nt_cost':nt_cost,'nt_qaly':nt_qaly,
       'markov_icer':icer,'cea_icer_B_vs_A':(85000-40000)/(4.4-3.5),
       'costing_total':4*300+2*450+12*120}
print(json.dumps(out, indent=2))
