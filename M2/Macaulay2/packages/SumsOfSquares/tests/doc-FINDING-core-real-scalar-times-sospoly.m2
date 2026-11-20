-- doc-FINDING-core-real-scalar-times-sospoly.m2 -- open finding (independent
-- tester, core).  Expected to FAIL on the original package.
-- The documentation lists (symbol *, Number, SOSPoly).  For a rational SOSPoly
-- s and a real number c, c*s on the original returns an SOSPoly that keeps
-- the rational ring but has real coefficients, and value(c*s) then fails with
-- "error: can't promote number to ring":
--   R = QQ[x,y]; s = sosPoly(R, {x+1,y}, {2,3}); value(2.5*s)  -- error
-- Either the product should be refused at once (as 2.5*(x+1) is refused for
-- x in QQ[x,y]), or it should be a consistent SOSPoly.
-- Asserted here: if c*s succeeds, its value can be computed and equals c*value s.
setRandomSeed 0;  -- fixed seed for anything random in the solver path
R = QQ[x,y];
s = sosPoly(R, {x+1,y}, {2,3});
for c in {2.5, 0.5} do (
    w := try c*s else null;
    if w =!= null then (
        v := try value w else null;
        assert(v =!= null);
        -- compare the values numerically at some points
        evalAt := (g, a, b) -> toRR lift(sub(g, {(ring g)_0 => a, (ring g)_1 => b}), coefficientRing ring g);
        assert all({(0,0), (1,2), (-3,1/2)}, ab ->
            abs(evalAt(v, ab#0, ab#1) - c*evalAt(value s, ab#0, ab#1)) < 1e-9);
        );
    );
