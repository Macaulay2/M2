-- Copyright 1996 Michael E. Stillman
-- Based on the Macaulay script written by David Eisenbud

needs "newring.m2"

monomialCurveIdeal = (S, a) -> (
    -- check that S is a polynomial ring over a field
    n := # a;
    topa := max a;
    if not all(a, i -> instance(i,ZZ) and i >= 1)
      then error "expected positive integers";
    a = prepend(0,a);
    s := symbol s;
    t := symbol t;
    k := coefficientRing S;
    M1 := monoid [s,t];
    M2 := monoid [Variables=>n+1];
    R1 := k M1;
    R2 := k M2;
    s = R1_0;
    t = R1_1;
    mm := matrix table(1, n+1, (j,i) -> s^(a#i) * t^(topa - a#i));
    j := generators kernel map(R1, R2, mm);
    ideal substitute(j, submatrix(vars S, {0..n}))
    )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
