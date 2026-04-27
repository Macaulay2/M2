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
