-- Copyright 1996 Michael E. Stillman
-- Based on the Macaulay script written by David Eisenbud

monomialCurve = (S, a) -> (
    -- check that S is a polynomial ring over a field
    n := # a;
    topa := max a;
    if not all(a, i -> instance(i,ZZ) and i >= 1)
      then error "expected positive integers";
    a = prepend(0,a);
    s := quote s;
    t := quote t;
    R1 := (coefficientRing S)[s,t];
    R2 := (coefficientRing S)[Variables=>n+1];
    mm := matrix table(1, n+1, (j,i) -> s^(a#i) * t^(topa - a#i));
    j := generators kernel map(R1, R2, mm);
    ideal substitute(j, submatrix(vars S, {0..n}))
    )

document{quote monomialCurve, 
    TT "monomialCurve(R,a)", " -- yields the defining ideal of the projective
    curve given parametrically on an affine piece by 
    t |---> (t^a1, ..., t^an).  The ideal is defined in the polynomial ring R,
    which must have at least n+1 variables, preferably all of equal 
    degree.  The first n+1 variables in the ring are used",
    "For example, the following defines a plane quintic curve of genus 6:",
    EXAMPLE "R = ZZ/101[a..f]",
    EXAMPLE "monomialCurve(R,{3,5})",
    "And a genus 2 curve with one singular point:",
    EXAMPLE "monomialCurve(R,{3,4,5})",
    "Two singular points, genus = 7:",
    EXAMPLE "monomialCurve(R,{6,7,8,9,11})",
    "Finally, the smooth rational quartic in P^3",
    EXAMPLE "monomialCurve(R,{1,3,4})"
    }

TEST "
    R := ZZ/101[a..f];
    -- plane quintic, genus=6
    I1 := monomialCurve(R,{3,5});
    assert(I1 == image matrix{{b^5-a^2*c^3}});

    -- one singular point, g=2
    I2 := monomialCurve(R,{3,4,5});
    assert(I2 == image matrix {{c^2-b*d, b^2*c-a*d^2, b^3-a*c*d}});

    -- two singular points, g=7
    I3 := monomialCurve(R,{6,7,8,9,11});
    assert(I3 == image matrix {{
               d*e-b*f, e^2-c*f, c*d-b*e, d^2-c*e, 
               c^2-b*d, b*c*e-a*f^2, b^2*d-a*e*f, b^2*c-a*d*f, b^3-a*c*f}});

    -- smooth rational quartic in P^3
    I4 := monomialCurve(R,{1,3,4});
    assert(I4 == image matrix {{b*c-a*d, c^3-b*d^2, a*c^2-b^2*d, b^3-a^2*c}});
"

