restart
loadPackage("NAG", FileName => "../NAG.m2"); debug NAG;
debug Core
R = CC[x,y,z]

g = x^2
h = y*z+x
--g = 3*y^2+2*x
--g = 1+ii + 2*x^2 + 3*x*y^2 + 4*z^2
g = random(3,R)

preG = poly2preSLP g -- gets g to the Horner form
preH = poly2preSLP h 
g3 = concatPreSLPs {preG,preG,preG}
gh = concatPreSLPs {preG,preH}
Dgh = jacobianPreSLP(gh,{0,1,2})
evaluatePreSLP(gh,gens R)
evaluatePreSLP(Dgh,gens R)-jacobian matrix{{g,h}}
-- test interpreted SLP -----------------------------------------------------
(constMAT, prog) = preSLPinterpretedSLP(3,g3) -- converts the "high-level" slp 
                           -- to a program encoded by an array of integers
rSLP = rawSLP(raw constMAT, prog)
K = CC_53
params = matrix{{ii_K,1_K,-1_K}}; 
result = rawEvaluateSLP(rSLP, raw params)
sub(g, params) - (map(K,result))_(0,0)

-- test compiled SLP -------------------------------------------------------
(constMAT, prog) = preSLPcompiledSLP(3,g3,1) -- "1" = part of library's name
rSLP = rawSLP(raw constMAT, prog)
K = CC_53
params = matrix{{ii_K,1_K,-1_K}}; 
result = rawEvaluateSLP(rSLP, raw params)
sub(g, params) - (map(K,result))_(0,0)

