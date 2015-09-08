restart
needsPackage "NAGtools"

X = inputGate x
F = matrix{{X^2}} 
PH = gateHomotopy4preimage(F,{X})
K = CC_53
setDefault(Software=>M2)
SPH = specialize(PH,matrix{{1_K},{2}})
p =matrix{{1_K}}
evaluateH(SPH,p,0)
evaluateHt(SPH,p,0)
evaluateHx(SPH,p,0)

peek PH.GateHomotopySystem    
degreeViaMonodromy(PH,p,p)
