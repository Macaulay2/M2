-- Based on the Macaulay (classic) scripts written by 
-- D. Eisenbud.  Translated to Macaulay 2 by M. Stillman

-- Copyright 1996 by Michael E. Stillman

-- translated: remove_low_dim --> top
--             remove_low_dim_id --> top
--             remove_lowest_dim --> removeLowestDimension
--             radical --> radical
--             unmixed_radical --> radical(I,Unmixed=>true)

---------------------------
-- removeLowestDimension --
---------------------------

removeLowestDimension = method()
removeLowestDimension Module := (M) -> (
     -- only works for polynomial rings...
    local E;
    R := ring M;
    c := codim M;
    p := pdim M;
    -- now loop (starting at p) trying to find the largest
    -- d such that codim Ext^d(M,R) == d
    while p > c and codim (E = Ext^p(M,R)) > p do p = p-1;
    if p == c then (
        -- M is C.M. and unmixed, so return (1):
        ambient M
        )
    else (
        -- use the annihilator of Ext to improve M
        I := ann E;
        coker gens saturate(image presentation M,I))
    )
removeLowestDimension Ideal := (I) -> (
     -- only works for polynomial rings...
    local E;
    M := coker gens I;
    R := ring M;
    c := codim M;
    p := pdim M;
    -- now loop (starting at p) trying to find the largest
    -- d such that codim Ext^d(M,R) == d
    while p > c and codim (E = Ext^p(M,R)) > p do p = p-1;
    if p == c then (
        -- M is C.M. and unmixed, so return (1):
        ideal(1_R)
        )
    else (
        -- use the annihilator of Ext to improve M
        J := ann E;
        saturate(I,J))
    )

---------------------------
-- top dimensional part ---
---------------------------

top Ideal := (I) -> (
     R := ring I;
     c := codim I;
     ann Ext^c(coker gens I, R))
     
top Module := (M) -> (
    R := ring M;
    if not isPolynomialRing R or not isAffineRing R
    then error "expected a polynomial ring";
    c := codim M;
    p := pdim M;  -- will compute a resolution if needed...
    while p > c do (
	E := prune Ext^p(M,R);
	if E != 0 and codim E === p then (
	    -- improve M
	    J := ann E;
	    I := saturate(M, J);
	    -- alternate strategy: modify M as well:
	    -- this next line could be commented out
	    M = (ambient I)/I;
	);
	if pdim M < p 
	  then p = pdim M
	  else p = p-1;
	);
    M
    )

-------------
-- radical --
-------------

unmixedradical := (I) -> (
     -- First lift I to a polynomial ring...
     A := ring I;
     f := presentation A;
     B := ring f;
     I = lift(I,B);
     if I != ideal(1_B) and 
        I.generators =!= 0 
     then (
    	  c := codim I;
    	  size := 1;
	  R := A;
    	  while size <= c do (
	       R = B/I;
	       dR := jacobian R;
      	       J := minors(size,dR);

	       g1 := leadTerm presentation R;
	       g1 = g1 | lift(leadTerm J, B);

      	       if codim ideal g1 > c
	       then size = size+1
      	       else (
		    -- we would like the next line to read:
		    -- I = ann J;
		    I = ideal syz(transpose mingens J, 
		                  SyzygyRows=>1, Syzygies=>true);
		    I = lift(I,B); 
		    );
      	       );
	  );
     trim (I*A)
     )
-- unmixed radical, another Eisenbud-Huneke-Vasconcelos method
-- to compute radical(m), given a max regular sequence n contained in m.

unmixedradical2 := (J, CI) -> (
  if ring J =!= ring CI then 
      error "unmixedradical: expected ideals to be in the same ring";
  D := jacobian CI;
  c := numgens CI;  -- we assume that this is a complete intersection...
  K := CI : minors(c, D);  -- maybe work mod CI?
  K : (K : J)) -- do these mod K?

radical1 := (I) -> (
    -- possibly massage input, by removing obvious extraneous powers?
    -- at least of the monomials in the ideal?
    R := ring I;
    I1 := removeLowestDimension I;
    J := saturate(I, I1);
    J = unmixedradical J;
    if I1 == ideal(1_R)
        then J 
        else intersect(J, radical1 I1))

radical Ideal := (I,options) -> (
     if class options.CompleteIntersection === Ideal then
          unmixedradical2(I,options.CompleteIntersection)
     else if options.Unmixed then 
          unmixedradical I
     else radical1 I
     )

document { quote radical, 
  TT "radical I", " -- the radical of the ideal I",
  BR,NOINDENT,
  TT "radical(I,options)", " -- some options are allowed as well",
  PARA,
  "If I is an ideal in an affine ring (i.e. a quotient of a polynomial 
  ring over a field), and if the characteristic of this field is
  large enough (see below), then this routine yields the radical of
  the ideal I.",
  PARA,
  "The method used is the Eisenbud-Huneke-Vasconcelos algorithm.
  See their paper in invent. math. 1993 for more details on the
  algorithm.",
  PARA,
  "Allowable options include",
  MENU {
       TO "Unmixed",
       TO "CompleteIntersection"
       },
  PARA,
  "For an example, see ", TO "component example", ".",
  PARA,
  "The algorithms used generally require that the characteristic of the
  ground field is larger than the degree of each primary component.  In 
  practice, this means that if the characteristic is something like 32003,
  rather than e.g. 5, the methods used will produce the radical of I.  Of
  course, you may do the computation over QQ, but it will often run much
  slower.  In general, this routine still needs to be tuned for speed.",
  SEEALSO {"top", "removeLowestDimension", "saturate", "quotient"}
  }

document { quote CompleteIntersection,
     TT "CompleteIntersection => J", " -- an option to ", TO "radical", " 
     which indicates that the ideal I provided by the user is unmixed,
     and that J is an ideal in I which is a complete intersection of
     the same codimension.",
     PARA,
     "Providing this option allows a separate often faster
     algorithm to be used to compute the radical.  This option
     should only be used if J is nice in some way.  For example,
     if J is randomly generated, but I is relatively sparse, 
     then this will most likely run slower than just giving the
     ", TO "Unmixed", " option."
     }

document { quote Unmixed,
     TT "Unmixed => true", " -- an option to ", TO "radical", " which asserts
     that the ideal provided by the user is known to be unmixed.",
     PARA,
     "An ideal is said to be unmixed if all associated primes of R/I
     have the same dimension.  In this case the algorithm tends to be much faster."
     }

document { quote top,
     TT "top I", " -- yields the intersection of top dimensional primary
     components of the module or ideal I.",
     PARA,
     "For an example, ", SEEALSO "component example",
     PARA,
     "If I is a submodule of (a quotient module) M, then a possibly larger
     submodule of M is returned.  The method used is that of
     Eisenbud-Huneke-Vasconcelos, in their 1993 Inventiones Mathematicae
     paper.  For a very brief description of the method used, see ", TO
     "top-method", ".",
     SEEALSO {"removeLowestDimension", "saturate", "quotient", "radical"}
     }

document { quote removeLowestDimension,
     TT "removeLowestDimension", " I -- I an ideal or submodule of a free module.
     Yields the intersection of the primary
     components of I, excepting those of lowest dimension (and thus returns the
     ambient free module of I (or unit ideal), if I is pure dimensional).",
     PARA,
     "For an example, ",SEEALSO "component example",
     PARA,
     "Computes one free resolution, and some homology groups, but no
     projections or determinants are used.",
     "For a very brief description of the method used, see ", TO "top-method", ".",
     SEEALSO {"top", "saturate", "quotient", "radical", "decompose"}
     }

document { "component example",
     "The following simple example illustrates the use of ", 
     TO "removeLowestDimension", ",", TO "top", ",", TO "radical",
     ", and ", TO "decompose", ".",
     EXAMPLE {
	  "R = ZZ/32003[a..d];",
      	  "I = monomialCurve(R,{1,3,4})",
      	  "J = ideal(a^3,b^3,c^3-d^3)",
      	  "I = intersect(I,J)",
      	  "removeLowestDimension I",
      	  "top I",
      	  "radical I",
      	  "decompose I"
	  },
     }

document { "top-method",
     "If M is a module in a polynomial ring R, then the implementations of ",
     TO "top", " and ", TO "removeLowestDimension", " are based on 
     the following observations:",
     MENU {
	  "codim Ext^d(M,R) >= d, for all d (if the module is non-zero)",
	  "If P is an associated prime of M of codimension d := codim P > codim M,
	  then codim Ext^d(M,R) = d and the annihilator of Ext^d(M,R) is contained
	  in P",
	  "If codim Ext^d(M,R) = d, then there really is an associated prime 
	  of codimension d.",
	  "If M is R/I, then top(I) = ann Ext^c(R/I,R), where c = codim I"
	  }
     }

TEST "
    R = ZZ/32003[a..d]
    I = monomialCurve(R,{1,3,4})
    J = ideal(a^3,b^3,c^3-d^3)
    I = intersect(I,J)
    removeLowestDimension I
    top I
    radical I
    decompose I
"
TEST "
    -- test of removeLowestDimension
    R = ZZ/32003[a,b,c]
    I = ideal(a^2,b^2)
    J = ideal(a^3,b^3,c^3)
    I = intersect(I,J)
    time (I1 = removeLowestDimension I)
    time top I
    time radical I
"

     
TEST "
    -- examples of use of: radical, UnmixedRadical, 
    -- top, removeLowestDimension

    -- example 1: a simple monomial ideal
    R = ZZ/101[a..d]
    I = intersect(ideal(a^2,b^2,c), ideal(a,b^3,c^2))
    time (Irad = radical(I,Unmixed=>true))

    -- example 2: 
    R = ZZ/101[a..d]
    I = intersect(ideal(a^2,b^2,c), ideal(a,d^4), ideal(b^2,c^2,d^2))
    time (Itop = top I)
    time (I1 = removeLowestDimension I)
    time (Irad = radical I)
"

TEST "
R = ZZ/101[quote a..quote d]
I = monomialCurve(R,{1,2,3})
I^2
removeLowestDimension(I^2)
assert(I == 
     radical(I^2)
     )
assert(I == 
     radical(I^2, Unmixed=>true)
     )
assert(
     top (I^2) == I^2
     )
S = R/(a^3, b^3)
I = ideal(0_S)
J = I^2
J1 = top J
J1 == J   
time (radical I)

-- 3 by 3 nilpotent matrices
R = ZZ/101[vars(0..8)]
M = genericMatrix(R,a,3,3)
I = ideal (M^3)
I1 = ideal(I_0,I_1,I_2)
codim I1
radical(I, CompleteIntersection=>I1)
-- radical(I,Unmixed=>true)
-- I1 = removeLowestDimension I
-- I2 = removeLowestDimension I1
"
