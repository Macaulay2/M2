-- Preliminary debugging interface for sagbi bases.
-- Warning!! This is not yet functional!
sagbi = method()
sagbi Matrix := (M) -> (
     -- check that M has a single row
     if numgens target M != 1
     then error "'sagbi' expects a one row matrix";
     notImplemented())

subduction = method()
subduction (Matrix, RingMap, Matrix) := (m,F,J) -> (
     R := ring m;
     n := numgens R;
     RS = ring J;
     nS := numgens RS - n;
     RtoRS = map(RS,R,(vars RS)_{0..n-1});
     RStoR = map(R,RS,(vars R) | matrix {toList(nS:0_R)});
     m = RtoRS m;
     g := J#{false,0};
     FRStoRS = map(RS,RS, (vars RS)_{0..n-1} | RtoRS (F.matrix));
     result := notImplemented();
     RStoR result)

///
R = ZZ/101[a,b,c]
m = matrix{{a^2+b^2}}
S = ZZ/101[x,y]
F = map(R,S,{a-b, b^2-c^2})
inF = map(R,S,leadTerm (F.matrix))
J = graphIdeal(inF, MonomialOrder=>ProductOrder{3,2})
generators gb J

subduction(m, F, J)
///


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
