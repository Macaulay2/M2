TEST ///
R=QQ[]
try solveSystem {1_R} then error "no variables: should've FAILed" else PASS 

R=QQ[x,y] 
try solveSystem {x^2-y} then error "too few variables: should've FAILed" else PASS 

R=(ZZ/2)[x,y]
try solveSystem {x,y} then error "positive characteristic: should've FAILed" else PASS
///

