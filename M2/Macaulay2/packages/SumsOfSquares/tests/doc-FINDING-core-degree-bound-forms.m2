-- doc-FINDING-core-degree-bound-forms.m2 -- open finding (independent tester,
-- core).  Expected to FAIL on the original package.
-- Doc of solveSOS(RingElement,ZZ): "If a degree bound D is given, the method
-- will use the vector of monomials of degree at most D/2."
-- On the original, for a homogeneous f in a polynomial ring and D > deg f, the
-- monomial vector consists only of the monomials of degree exactly D/2, so no
-- Gram matrix exists and the result has Status null and GramMatrix null:
--   R = QQ[x,y]; solveSOS(x^2+y^2, 4)   -- Monomials = x^2, xy, y^2
--   solveSOS(2x^4+5y^4-2x^2y^2+2x^3y, 6) -- Monomials = the cubics
-- With the monomials of degree <= D/2 these are SOS (e.g. Q = diag(0,1,1,0,0,0)
-- on 1,x,y,x^2,xy,y^2), and inhomogeneous inputs (x^2+1, 4) work.
-- Asserted here: a valid certificate for these inputs.
setRandomSeed 0;  -- fixed seed for anything random in the solver path
R = QQ[x,y];
for p in {(x^2 + y^2, 4), (2*x^4+5*y^4-2*x^2*y^2+2*x^3*y, 6), (x^2*y^2, 6)} do (
    f := p#0;
    sol := solveSOS(f, p#1);
    assert(sol#GramMatrix =!= null);
    assert(sol#Status == "SDP solved, primal-dual feasible");
    assert all(flatten entries sol#Monomials, m -> first degree m <= (p#1)//2);
    s := sosPoly sol;
    assert(if ring sol#GramMatrix === QQ then value s == f
           else clean(1e-6, value s - sub(f, ring s)) == 0);
    );
