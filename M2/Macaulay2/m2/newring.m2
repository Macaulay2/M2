-- Copyright 1996 Michael E. Stillman

----------------------------------
-- new polynomial ring from old --
----------------------------------

nothing := symbol nothing
mergeOptions := (x,y) -> merge(x, y, (a,b) -> if b === nothing then a else b)
modifyRing = method( Options => applyValues(monoidDefaults, x -> nothing) )

modifyRing Ring := Ring => options -> (R) -> (
     -- First check the type of ring of R
     -- The following is for the case when R is a polynomial ring,
     -- or a quotient of a polynomial ring

     if    (instance(options.Variables,List) 
              and #( options.Variables ) =!= numgens R)
        or (instance(options.Variables,ZZ) 
              and options.Variables =!= numgens R)
     then
         error "cannot change the number of variables using 'modifyRing'";
         
     options = mergeOptions((monoid R).Options,options);
     f := presentation R;
     A := ring f;
     k := coefficientRing A;
     S := k[options];
     f = substitute(f,vars S);
     S/image f
     )

-----------------------------
-- tensor product of rings --
-----------------------------

-- made a method and documented elsewhere.

tensor(Ring,Ring) := Ring ** Ring := Ring => (R,S) -> error "tensor product not implemented for these rings"

PolynomialRing ** PolynomialRing :=
QuotientRing ** PolynomialRing :=
PolynomialRing ** QuotientRing :=
QuotientRing ** QuotientRing := (R,S) -> tensor(R,S)

tensor(PolynomialRing, PolynomialRing) :=
tensor(QuotientRing, PolynomialRing) :=
tensor(PolynomialRing, QuotientRing) :=
tensor(QuotientRing, QuotientRing) := optns -> (R,S) -> (
     k := coefficientRing R;
     if k =!= coefficientRing S 
     then error "expected rings to have the same coefficient ring";
     f := presentation R; A := ring f; M := monoid A; m := numgens M;
     g := presentation S; B := ring g; N := monoid B; n := numgens N;
     AB := k tensor(M, N, 
	  MonomialSize => max((options M).MonomialSize, (options N).MonomialSize),
	  optns);
     fg := substitute(f,(vars AB)_{0 .. m-1}) | substitute(g,(vars AB)_{m .. m+n-1});
     -- forceGB fg;  -- if the monomial order chosen doesn't restrict, then this
                     -- is an error!! MES
     AB/image fg)

-------------------------
-- Graph of a ring map --
-------------------------

graphIdeal = method( Options => apply( {MonomialOrder, MonomialSize, VariableBaseName}, o -> o => monoidDefaults#o ))
graphRing = method( Options => options graphIdeal )

graphIdeal RingMap := Ideal => options -> (f) -> (
     -- return the ideal in the tensor product of the graph of f.
     -- if f is graded, then set the degrees correctly in the tensor ring.
     -- return the ideal (y_i - f_i : all i) in this ring.
     S := source f;
     R := target f;
     k := coefficientRing R;
     if not (
	  isAffineRing R
	  and isAffineRing S
	  and k === coefficientRing S
	  ) then error "expected polynomial rings over the same ring";
     if vars k ** R != f (vars k ** S)	  -- vars k isn't really enough...
     then error "expected ring map to be identity on coefficient ring";
     t := numgens source f.matrix;
     I := submatrix(f.matrix,,toList(t - numgens S .. t - 1));
     options = new MutableHashTable from options;
     options.Degrees = join((monoid R).degrees, 
	  apply(degrees source I, d -> if d === {0} then {1} else d)
	  );
     options = new OptionTable from options;
     RS := tensor(R,S,options);
     --RS := tensor(R,S,
     --             Degrees=>join((monoid R).degrees, degrees source I),
     --             MonomialOrder=>Eliminate numgens R,
     --             options);
     yvars := (vars RS)_{numgens R .. numgens RS - 1};
     xvars := (vars RS)_{0..numgens R - 1};
     ideal(yvars - substitute(I, xvars)))

graphRing RingMap := QuotientRing => options -> (f) -> (
     if f.cache.?graphRing then f.cache.graphRing else f.cache.graphRing = (
     	  I := graphIdeal(f,options);
     	  R := ring I;
     	  R/I))

-----------------------
-- Symmetric Algebra --
-----------------------

symmetricAlgebraIdeal := method( Options => monoidDefaults )

symmetricAlgebra = method( Options => monoidDefaults )


symmetricAlgebraIdeal Module := Ideal => opts -> (M) -> (
     R := ring M;
     K := coefficientRing ultimate(ambient, R);
     m := presentation M;
     N := if opts.Variables === monoidDefaults.Variables
          then monoid[Variables => numgens M]
          else monoid[Variables => opts.Variables];
     SM := tensor(K N, R, opts, Variables => monoidDefaults.Variables);
     xvars := submatrix(vars SM, {numgens target m .. numgens SM - 1});
     yvars := submatrix(vars SM, {0 .. numgens target m - 1});
     m = substitute(m,xvars);
     I := yvars*m)

symmetricAlgebra Module := Ring => options -> (M) -> (
     I := symmetricAlgebraIdeal(M,options);
     if I == 0 then ring I else (ring I)/(image I))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
