-- Copyright 1996 Michael E. Stillman
-- Based on the Macaulay script written by David Eisenbud

monomialCurve = (S, a) -> (
    -- check that S is a polynomial ring over a field
    n := # a;
    topa := max a;
    if not all(a, i -> instance(i,ZZ) and i >= 1)
      then error "expected positive integers";
    a = prepend(0,a);
    s := symbol s;
    t := symbol t;
    monsize := (options S).MonomialSize;
    R1 := (coefficientRing S)[s,t,MonomialSize=>monsize];
    R2 := (coefficientRing S)[Variables=>n+1,MonomialSize=>monsize];
    mm := matrix table(1, n+1, (j,i) -> s^(a#i) * t^(topa - a#i));
    j := generators kernel map(R1, R2, mm);
    ideal substitute(j, submatrix(vars S, {0..n}))
    )


