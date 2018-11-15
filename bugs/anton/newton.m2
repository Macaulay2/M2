-- works now
restart
errorDepth = 0 
needsPackage "NumericalAlgebraicGeometry"
CC[x]
F = polySystem {x^2-1}
X = matrix{{1.1}}
M = F.PolyMap

evaluate(M,X)
code methods evaluate
X = newton(F,X)
code methods newton

-- SIGSEGV
restart
C = matrix {{1_CC}}
lift(C,RR) -- works
lift(C,QQ) -- error
sub(C,QQ) -- SIGSEGV
