-- Copyright 1996 Michael E. Stillman

----------------------------------
-- new polynomial ring from old --
----------------------------------

local nothing

mergeOptions := args -> merge append(args, (a,b)-> if b === nothing then a else b)

modifyRing = method(
     Options => apply(options monoid, x -> nothing)
     )

modifyRing Ring := (R,options) -> (
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

document { quote modifyRing,
     TT "modifyRing(R,options)", " -- yields a ring similar to R, with 
     certain features changed.",
     PARA,
     "Allowed options include:",
     MENU {
	  },
     PARA,
     "Bug: doesn't work yet."
     }

TEST "
    R = ZZ/101[a..d,Degrees=>{1,2,3,4}]
    S = modifyRing(R,Degrees=>{1,1,1,1})
    S1 = modifyRing(R,MonomialOrder=>Eliminate 2,Degrees=>{1,1,1,1})
"
-----------------------------
-- tensor product of rings --
-----------------------------

-- made a method and documented elsewhere.

PolynomialRing ** PolynomialRing :=
QuotientRing ** PolynomialRing :=
PolynomialRing ** QuotientRing :=
QuotientRing ** QuotientRing := (R,S) -> tensor(R,S)

Ring ** Ring := { Ring, 
     ,					  -- no function, just documentation!
     TT "R ** S", " -- tensor product of rings.",
     PARA,
     "For complete documentation, see ", TO "tensor", "."
     }

tensor(PolynomialRing, PolynomialRing) :=
tensor(QuotientRing, PolynomialRing) :=
tensor(PolynomialRing, QuotientRing) :=
tensor(QuotientRing, QuotientRing) := (R,S,optns) -> (
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

graphIdeal = method( Options => options monoid )

graphRing = method( Options => options monoid )

graphIdeal RingMap := (f,options) -> (
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
     I := submatrix(f.matrix,,elements(t - numgens S .. t - 1));
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
     yvars - substitute(I, xvars)
     )

graphRing RingMap := (f,options) -> (
     I := graphIdeal(f,options);
     (ring I)/(image I))

document { quote graphIdeal,
     TT "graphIdeal f", " -- provides the ideal of the graph of the map
     associated to the ring map f.",
     SEEALSO "graphRing"
     }

document { quote graphRing,
     TT "graphRing f", " -- provides the ring of the graph of the map
     associated to the ring map f.",
     SEEALSO "graphIdeal"
     }

-----------------------
-- Symmetric Algebra --
-----------------------

symmetricAlgebraIdeal := method( Options => options monoid )

symmetricAlgebra = method( Options => options monoid )

document { quote symmetricAlgebra,
     TT "symmetricAlgebra M", " -- produces the symmetric algebra of a
     module M.",
     PARA,
     "Bugs: uses symbols from the beginning of the alphabet as variables in
     the new ring; makes a quotient ring when it doesn't have to."
     }

symmetricAlgebraIdeal Module := (M,opts) -> (
     R := ring M;
     K := coefficientRing ultimate(ambient, R);
     m := presentation M;
     N := if opts.Variables === (options monoid).Variables
          then monoid[Variables => numgens M]
          else monoid[Variables => opts.Variables];
     SM := tensor(K N, R, opts, Variables => (options monoid).Variables);
     xvars := submatrix(vars SM, {numgens target m .. numgens SM - 1});
     yvars := submatrix(vars SM, {0 .. numgens target m - 1});
     m = substitute(m,xvars);
     I := yvars*m)

symmetricAlgebra Module := (M,options) -> (
     I := symmetricAlgebraIdeal(M,options);
     (ring I)/(image I))

TEST "
    R = ZZ/101[s,t]
    J = image matrix {{s^4, s^3*t, s*t^3, t^4}}
    S = symmetricAlgebra J  -- MES: make an assertion here...
"
