TEST ///

F = (cokernel matrix {{4}})
U = (cokernel matrix {{2}})
r = map(U,F, matrix {{1}})
t = map(F,U, matrix {{2}})
c = map(U,U, matrix {{1}})
MF := makeCpMackeyFunctor(2,r,t,c)


-- verify free resolutions are in fact complexes
d = resolutionCohomological(MF,3)
for i to (length d) - 2 do (
    comp = d#i * d#(i+1);
    assert(comp.UnderlyingMap == 0 and comp.FixedMap == 0)
)

///

-- TorCoh and ExtCoh were previously exercised only by doc examples;
-- pin a concrete output so a regression in cohomological resolutions
-- or the M-tensor/Hom step would be caught.
TEST ///
-- M is the Mackey functor with Underlying = 0 and Fixed = ZZ/3.
N := cokernel(matrix {{3}});
M := makeZeroOnUnderlyingMackeyFunctor(3, N);
assert(isCohomological M);
-- TorCoh and ExtCoh return CpMackeyFunctors. For this self-dual
-- "zero-on-underlying ZZ/3" example, the Underlying part is 0 for
-- all i (no nontrivial torsion can land there).
for i to 3 do (
    T := TorCoh(i, M, M);
    E := ExtCoh(i, M, M);
    assert(class T === CpMackeyFunctor);
    assert(class E === CpMackeyFunctor);
    assert(prune T.Underlying == 0);
    assert(prune E.Underlying == 0);
    );
-- Pin the Fixed part in two specific degrees: i=0 is just M (Fixed=ZZ/3)
-- and i=3 is nonzero (cohomological Ext^3 of ZZ/3 with itself).
assert(prune (TorCoh(0, M, M)).Fixed == N);
assert(prune (ExtCoh(0, M, M)).Fixed == N);
assert(prune (TorCoh(3, M, M)).Fixed == N);
assert(prune (ExtCoh(3, M, M)).Fixed == N);
///

-- Ordinary Ext on the complex representation Burnside-like functor
-- RU = makeComplexRepresentationMackeyFunctor(3) gives a nontrivial
-- Ext^4 with Fixed = ZZ/3 and Underlying = 0.
TEST ///
RU := makeComplexRepresentationMackeyFunctor(3);
E := Ext^4(RU, RU);
assert(class E === CpMackeyFunctor);
assert(prune E.Underlying == 0);
assert(prune E.Fixed == cokernel matrix {{3}});
///
