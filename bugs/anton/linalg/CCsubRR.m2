needsPackage "NAGtypes"
CC[x,y]
S = polySystem {x^2+y^2-6, 2*x^2-y}
p = point({{1.0,2.3}, ConditionNumber=>1000, ErrorBoundEstimate =>0.01});
evaluate(S,p)
