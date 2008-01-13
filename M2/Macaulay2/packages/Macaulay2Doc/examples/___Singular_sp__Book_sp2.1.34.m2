R = QQ[x,y,z,MonomialOrder=>{Weights=>{-1,-1,-1},RevLex},Global=>false]
M = cokernel matrix{{0,y},{x*y-1,x*z},{x*y+1,x*z}}
prune M -- INCORRECT
