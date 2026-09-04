-- test 'terms'
QQ[a,b][x,y]
assert( (terms (1+a+b)^3) === {a^3, 3*a^2*b, 3*a*b^2, b^3, 3*a^2, 6*a*b, 3*b^2, 3*a, 3*b, 1+a-a} )
assert( (terms (1+a+x)^3) === {x^3, (3*a+3)*x^2, (3*a^2+6*a+3)*x, a^3+3*a^2+3*a+1+x-x} )
try terms(ZZ,x) -- used to crash


-- another terms test, fixed 1/5/09
kk = QQ[a]/(a^2+1)
K = toField kk
R = K[x, y]
G = a*x^28+x^28 + a*x^2*y^5
assert(terms G === {(a+1)*x^28, a*x^2*y^5})

-- previously, rawTerm returned 0 and then we'd segfault creating the ideal
debug Core
R = QQ[x,y]
assert try rawTerm(raw R, raw 1_(ZZ/2), rawMakeMonomial {(0,1)}) then false else true
ideal vars R
