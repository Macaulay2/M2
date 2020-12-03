needsPackage "NAGtypes"
CC[x,y]
S = polySystem {x^2+y^2-6, 2*x^2-y}
p = point({{1.0,2.3}, ConditionNumber=>1000, ErrorBoundEstimate =>0.01});
evaluate(S,p)

restart
R = QQ[x]
m = matrix{{1/2 * x}}
sub(m, {x=>1_ZZ}) -- is correct behavior
sub(m, matrix{{1}}) -- just wrong.  Should give behavior on previous line
f = map(ZZ, R, {1}) -- REALLY: should give an error: cannot construct ring map from R --> ZZ.
f m -- wrong behavior currently.  But if f cannot be constructed, this will not be a problem
g = map(QQ[y], R, {1}) -- this one works now, and is the behavior we want
g m

R = CC[x]
m = matrix{{ii * x}}
sub(m, {x=>1_ZZ}) -- is correct behavior
sub(m, matrix{{1}}) -- just wrong.  Should give behavior on previous line
f = map(RR_53, R, {1}) -- REALLY: should give an error.
f m -- crashes in linalg branch, gives 0 in 1.6. Both are wrong...!
g = map(CC_53[y], R, {1}) -- this one works now, and is the behavior we want
g m


---------------------------------
restart
R = QQ[x,y,z]
X = hold x
Y = hold y
Z = hold z
f1 = (X+1)*(Z+1)
f2 = f1^3
sub(f2, {x=>1_QQ, y=>2_QQ, z=>3_QQ})
eval = method()
eval(Power, List) := (P, L) -> (
    (eval(P#0, L))^P#1
    )
eval(Product, List) := (P, L) -> product(toList P, x -> eval(x,L))
eval(Hold, List) := (var, L) -> 
eval(f2, {x=>1_QQ, y=>2_QQ, z=>3_QQ})
X = (hold x)*y - y*(hold x)
X^3
X * E_(i,j)
