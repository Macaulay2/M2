restart
kk = QQ
kk = ZZ/32003

R=kk[s,z]
I=ideal(s^7+3*s^6*z+s^6+3*s^5*z^3+6*s^5*z^2+9*s^4*z^4+4*s^3*z^6-s^3*z^5-3*s^2*z^7-3*s*z^9-z^11);
A = R/I
time G=gens gb presentation integralClosure(A, Verbosity=>2)
  -- MES: this crashes, over QQ, after a while...

-- this one seems very fast
loadPackage "FractionalIdeals"
R = kk[z, s, MonomialOrder=>{1,1}]
I=ideal(s^7+3*s^6*z+s^6+3*s^5*z^3+6*s^5*z^2+9*s^4*z^4+4*s^3*z^6-s^3*z^5-3*s^2*z^7-3*s*z^9-z^11);
A = R/I
time integralClosureHypersurface A

-- This one does poorly
R = kk[s, z, MonomialOrder=>{1,1}]
I=ideal(s^7+3*s^6*z+s^6+3*s^5*z^3+6*s^5*z^2+9*s^4*z^4+4*s^3*z^6-s^3*z^5-3*s^2*z^7-3*s*z^9-z^11);
A = R/I
time integralClosureHypersurface A

-- puiseux is fast in both QQ and ZZ/13
loadPackage "IntegralBases"
R = QQ[s,z]
I=ideal(s^7+3*s^6*z+s^6+3*s^5*z^3+6*s^5*z^2+9*s^4*z^4+4*s^3*z^6-s^3*z^5-3*s^2*z^7-3*s*z^9-z^11);
findTruncations I_0
displayTruncations oo

loadPackage "IntegralBases"
R = QQ[z,s]
I=ideal(s^7+3*s^6*z+s^6+3*s^5*z^3+6*s^5*z^2+9*s^4*z^4+4*s^3*z^6-s^3*z^5-3*s^2*z^7-3*s*z^9-z^11);
findTruncations I_0
displayTruncations oo
eliminate(I + ideal jacobian I, {z})
eliminate(I + ideal jacobian I, {s})
loadPackage "IntegralBases"
R = ZZ/13[s,z]
I=ideal(s^7+3*s^6*z+s^6+3*s^5*z^3+6*s^5*z^2+9*s^4*z^4+4*s^3*z^6-s^3*z^5-3*s^2*z^7-3*s*z^9-z^11);
findTruncations I_0
displayTruncations oo

loadPackage "IntegralBases"
R = ZZ/2[s,z]
I=ideal(s^7+3*s^6*z+s^6+3*s^5*z^3+6*s^5*z^2+9*s^4*z^4+4*s^3*z^6-s^3*z^5-3*s^2*z^7-3*s*z^9-z^11);
findTruncations I_0
displayTruncations oo

-------------------------------------------------
-- Doing this via Puiseux expansions, with z as the indep variable
restart
debug loadPackage "IntegralBases"
R = QQ[z,s]
I=ideal(s^7+3*s^6*z+s^6+3*s^5*z^3+6*s^5*z^2+9*s^4*z^4+4*s^3*z^6-s^3*z^5-3*s^2*z^7-3*s*z^9-z^11);
time PX = findTruncations I_0
displayTruncations oo
factor discriminant(I_0, s) -- z^61 * (sqfree)
factor discriminant(I_0, z) -- s^66 * (sqfree)
eliminate(I + ideal jacobian I, {z}) -- (s^10)
eliminate(I + ideal jacobian I, {s}) -- (z^19)
factor (eliminate(I + ideal diff_z I_0, {s}))_0 -- (z^25) * (sqfree)
factor (eliminate(I + ideal diff_s I_0, {s}))_0 -- (z^24) * (sqfree)


-- Let's add to the basis one element at a time
-- a basis is of the form V = {{z^power, poly in deg i in s}, ...} and only goes up to a certain degree in s.
V = {{0, 1_R}, {0, s}}

debug loadPackage "IntegralBases"
Ps = apply(puiseuxTruncations I_0, last)

V = {1_R, s}
syz matrix makeEquations(Ps, V, 1) -- no solutions, so add new elem to V
V = append(V, s*(last V))
syz matrix makeEquations(Ps, V, 1) -- one solution, so modify last elem of V, mult others by z
V = append(apply(drop(V,-1), p -> z*p), V_2+V_1)
syz matrix makeEquations(Ps, V, 2) -- no solution so add new elem to V
V = append(V, s*(last V))
syz matrix makeEquations(Ps, V, 2) -- one solution, so modify last elem of V
V = append(apply(drop(V,-1), p -> z*p), V_3-3*V_1)
syz matrix makeEquations(Ps, V, 3) -- one solution
V = append(apply(drop(V,-1), p -> z*p), V_3+3*V_2+6*V_1)
syz matrix makeEquations(Ps, V, 4) -- none
V = append(V, s*(last V))
syz matrix makeEquations(Ps, V, 4) -- one solution
V = append(apply(drop(V,-1), p -> z*p), V_4-3*V_1)
syz matrix makeEquations(Ps, V, 5) -- one solution
V = append(apply(drop(V,-1), p -> z*p), V_4+3*V_2+9*V_1)
syz matrix makeEquations(Ps, V, 6) -- none
V = append(V, s*(last V))
syz matrix makeEquations(Ps, V, 6) -- one
V = append(apply(drop(V,-1), p -> z*p), V_5-V_1)
syz matrix makeEquations(Ps, V, 7) -- one
V = append(apply(drop(V,-1), p -> z*p), V_5+4*V_1)
syz matrix makeEquations(Ps, V, 8) -- one
V = append(apply(drop(V,-1), p -> z*p), (matrix{V} * oo)_(0,0))
syz matrix makeEquations(Ps, V, 9) -- none
V = append(V, s*(last V))
syz matrix makeEquations(Ps, V, 9) -- one
V = append(apply(drop(V,-1), p -> z*p), (matrix{V} * oo)_(0,0))
syz matrix makeEquations(Ps, V, 10) -- one
V = append(apply(drop(V,-1), p -> z*p), (matrix{V} * oo)_(0,0))
syz matrix makeEquations(Ps, V, 11) -- one
V = append(apply(drop(V,-1), p -> z*p), (matrix{V} * oo)_(0,0))
syz matrix makeEquations(Ps, V, 12) -- one
V = append(apply(drop(V,-1), p -> z*p), (matrix{V} * oo)_(0,0))
syz matrix makeEquations(Ps, V, 13) -- done!

V
-- V is the answer

Pf = for p in Ps list tcoeffs(p,R);
transpose matrix{flatten computeTruncation(Pf,0,1_R),
       flatten computeTruncation(Pf,0,s),
       flatten computeTruncation(Pf,1,s^2+s)
      }

restart
debug loadPackage "IntegralBases"
R = QQ[z,s]
I=ideal(s^7+3*s^6*z+s^6+3*s^5*z^3+6*s^5*z^2+9*s^4*z^4+4*s^3*z^6-s^3*z^5-3*s^2*z^7-3*s*z^9-z^11);
time Ps = apply(puiseuxTruncations I_0, last)
time localBasis(Ps, 7, z, s)
netList oo

Ps_0_0
cfs = flatten entries last coefficients Ps_0_0
(coefficientRing R, coefficientRing ring Ps_0_0)
vecSpaceMap(coefficientRing R, coefficientRing ring Ps_0_0)
cfs/oo
  -- current best:
  degree_s F -- 7
  factor discriminant(F,s)  -- z^61 * sqfree
  -- 61 = 2*n + (1-1) + (2-1) + (3-1) + (1-1) ==> n=3
  --   61 = 2*(1+3+5+8+12) + 3
  -- max floor(Int_i) = 12
  eliminate(ideal F + ideal jacobian ideal F, {s}) -- (z^19)
  Ps = puiseuxTruncations F
  netList oo
  time Ps = apply(Ps, last)
  netList time localBasis(Ps, degree_y F, x, y)
  

-- end of this example
-------------------------------------------------
restart
debug loadPackage "IntegralBases"
  R = QQ[x,y]
  F = poly"y16-4y12x6-4y11x8+y10x10+6y8x12+8y7x14+14y6x16+4y5x18+y4(x20-4x18)-4y3x20+y2x22+x24"
time Ps = apply(puiseuxTruncations F, last)
time localBasis(Ps, degree_y F, x, y)
netList oo
Ps_0_0
cfs = flatten entries last coefficients Ps_0_0
(coefficientRing R, coefficientRing ring Ps_0_0)
vecSpaceMap(coefficientRing R, coefficientRing ring Ps_0_0)
cfs/oo
