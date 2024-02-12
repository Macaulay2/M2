-- -*- coding: utf-8 -*-
newPackage("Elimination",
     Version => "1.0", 
     Date => "January 5, 2005",
     Authors => {{Name => "Michael E. Stillman", Email => "mike@math.cornell.edu"}},
     Keywords => {"Commutative Algebra"},
     Headline => "elimination of variables"
     )

export {"eliminate", "sylvesterMatrix", "discriminant", "resultant"}

importFrom_Core {"monoidIndices"}

------------------------------
-- Elimination of variables --
------------------------------

eliminate = method()

-- The following was code MES wrote to give to Sottile's group (Spring, 2009).
-- I still want to work this into the eliminate command...
eliminateH = (v,I) -> (
     -- v is a list of variables
     -- I is an ideal
     R := ring I;
     h := local h;
     S := (coefficientRing R)[gens R, h, MonomialSize => 8];
     use R;
     IS := homogenize(sub(trim I,S), h);
     phi := map(R,S,vars R | matrix{{1_R}});
     eS := eliminate(v,IS);
     return trim phi eS;
     )

isFlatPolynomialRing = R -> (
     -- R should be a ring
     -- determines if R is a poly ring over ZZ or a field
     kk := coefficientRing R;
     isPolynomialRing R and (kk === ZZ or isField kk)
     )

eliminationRing = (elimvars, R) -> (
     -- input: R: flat polynomial ring
     --        elimvars: list of integer indices of vars to eliminate
     --        homog:Boolean: whether to add another variable for homogenization
     -- output: (F:R-->S, G:S-->R), where S is the new ring
     -- S is the same as R, except that the variables have been permuted, the 
     -- names of the variables are private, and the monomial ordering is an elim order.
     -- If R is a WeylAlgebra, homogenized Weyl algebra, skew commutative ring, or poly
     -- ring, then S will be the same, with the correct multiplication and grading
     keepvars := sort toList(set(0..numgens R-1) - set elimvars);
     perm := join(elimvars,keepvars);
     invperm := inversePermutation perm;
     vars := (options R).Variables;
     degs := (options R).Degrees;
     weyl := (options R).WeylAlgebra;
     skew := (options R).SkewCommutative;
     degs = degs_perm;
     vars = vars_perm;
     -- FIXME: remove this line when Weyl variables are stored as indices in the monoid
     weyl = monoidIndices_R weyl;
     weyl = apply(weyl, pair -> invperm_pair);
     M := monoid [vars,MonomialOrder=>Eliminate(#elimvars), Degrees=>degs, 
	  WeylAlgebra => weyl, SkewCommutative => skew, MonomialSize=>16];
     k := coefficientRing R;
     R1 := k M;
     toR1 := map(R1,R,apply(invperm,i->R1_i));
     toR := map(R,R1,apply(perm,i->R_i));
     (toR1,toR)
     )

eliminate1 = (elimindices,I) -> (
     -- at this point, I is an ideal in a flat ring, 
     -- and elimindices represents the variables
     -- to eliminate.
     (toR1,toR) := eliminationRing(elimindices,ring I);
     J := toR1 I;
     -- cache poincare
     if isHomogeneous I then poincare J = poincare I;
     -- compare with quickEliminate1 from MinimalPrimes
     ideal mingens ideal toR selectInSubring(1,generators gb J)
     )

eliminate (List, Ideal) := (v,I) -> (     
     R := ring I;
     -- if R is a quotient ring, then give error
     if not isFlatPolynomialRing R then
       error "expected a polynomial ring over ZZ or a field";
     if #v === 0 then return I;
     if not all(v, x -> class x === R) then error "expected a list of elements in the ring of the ideal";
     varlist := unique monoidIndices_R v;
     eliminate1(varlist, I)
     )

eliminate (Ideal, RingElement) := (I,v) -> eliminate({v},I)
eliminate (Ideal, List) := (I,v) -> eliminate(v,I)
eliminate(RingElement, Ideal) := (v,I) -> eliminate({v},I)

-----------------------------------------------
-- Sylvester matrix, resultant, discriminant --
-----------------------------------------------

sylvesterMatrix = method(TypicalValue => Matrix)
sylvesterMatrix(RingElement,RingElement,RingElement) := (f,g,x) -> (
     R := ring f;
     if R =!= ring g then error "expected same ring";
     v := index x;  -- just to check if x is a variable in the polyring R.
     if f == 0 or g == 0 then error "expected nonzero polynomials";
     degf := degree(x,f);
     degg := degree(x,g);
     if degf === 0 and degg === 0 then map(R^0,R^0,1)
     else if degf === 0 then map(R^degg,R^degg,f)
     else if degg === 0 then map(R^degf,R^degf,g)
     else (
       x1 := matrix{{x,1}};
       xfg := transpose symmetricPower(degf + degg - 1, x1);
       xf := symmetricPower(degf-1, x1);
       xg := symmetricPower(degg-1, x1);
       m := contract(xfg, (f ** xg) | (g ** xf));
       m = transpose m;
       substitute(m, x=>0)))

resultant = method(TypicalValue => RingElement, Options => { Algorithm => null })
resultant(RingElement, RingElement, RingElement) := o -> (f,g,x) ->
     det sylvesterMatrix(f,g,x)

discriminant = method(Options => { Algorithm => null })
discriminant(RingElement, RingElement) := RingElement => o -> (f,x) -> resultant(f, diff(x,f), x, o)

-----------------------------------------------
-- documentation and tests
-----------------------------------------------

beginDocumentation()

document {
     Key => Elimination,
     Headline => "eliminating specified variables, and Sylvester resultant",
     "This package contains functions to eliminate variables from an ideal (that is, 
     intersect an ideal with a subring generated by certain of the variables), and to compute
     Sylvester resultants.",
     PARA{
	  "It would be nice to implement multivariate resultants, Bezoutians, and
	  sparse resultants.  Laurent Buse has written code for this in the past.
	  We hope to provide a package, Resultants, which will do this.  If you
	  are willing to help write this, please contact the author if this package!"
	  },
     }

document {
     Key => {(resultant, RingElement, RingElement, RingElement),resultant},
     Usage => "resultant(f,g,x)",
     Inputs => { 
	  "f",
	  "g" => {"in the same polynomial ring ", TT "R", " as ", TT "f"},
	  "x" => {"a variable in ", TT "R"}
	  },
     Outputs => { RingElement => { "the Sylvester resultant of ", TT "f", " and ", TT "g", " with respect to the variable ", TT "x" }},
     PARA {
	  "The elements ", TT "f", " and ", TT "g", " should be polynomials in the same ring, and ", TT "x", " should be
	  a variable in that ring.  The result is the determinant of the Sylvester matrix, 
	  ", TT "sylvesterMatrix(f,g,x)", ".  The resultant of ", TT "f", " and its derivative with respect to ", TT "x", " is the
	  discriminant, ", TT "discriminant(f,x)", "."},
     EXAMPLE lines ///
	  R = ZZ[x,a,b,c,d]	  
	  f = x^7+3*x^4+a*x+b
	  g = x^8+x^5+c*x+d
	  time eliminate(ideal(f,g),x)
	  time ideal resultant(f,g,x)
	  sylvesterMatrix(f,g,x)
	  discriminant(f,x)
	  ///,
     SeeAlso => {"sylvesterMatrix", "discriminant", "eliminate"}
     }


undocumented {
	  (eliminate, Ideal, RingElement), 
	  (eliminate, Ideal, List)
     }
document {
     Key => {eliminate,
	  (eliminate, RingElement, Ideal), 
	  (eliminate, List, Ideal)
	  },
     Usage => "eliminate(v,J)",
     Inputs => {
	  "v" => Nothing => {ofClass RingElement, " or ", ofClass List, ", a variable or list of variables of a polynomial ring ", TT "R"},
	  "J" => Ideal => {"in the ring ", TT "R"},
	  },
     Outputs => {
	  Ideal => {"generated by the elements of J not involving the variables v"},
	  },
     "If the ideal ", TT "J", " is homogeneous, then an effort is made to use the Hilbert function to speed up the
     computation.",
     EXAMPLE lines ///
	  R = ZZ/101[x,a,b,c,d]	  
	  f = x^2+a*x+b
	  g = x^2+c*x+d
	  time eliminate(x,ideal(f,g))
	  time ideal resultant(f,g,x)
	  sylvesterMatrix(f,g,x)
	  discriminant(f,x)
	  ///,
     PARA{	  
       "One may also switch the order of arguments: the ideal being the first argument.  
       This usage has been deprecated, and should no longer be used."},
     Caveat => {"The ring ", TT "R", " should not be a quotient ring, or a non-commutative ring.
	  Additionally, it would be nice to be able to use a DegreeLimit, or to be able to interrupt
	  the computation."},
     SeeAlso => {resultant,discriminant,Elimination}
     }

document {
     Key => {(discriminant,RingElement,RingElement),discriminant},
     Usage => "discriminant(f,x)",
     Inputs => {
	  "f" => "a polynomial",
	  "x" => "a variable in the same ring"
	  },
     Outputs => {
     	  RingElement => {"the discriminant of ", TT "f", " with respect to ", TT "x"}
	  },
     EXAMPLE lines ///
	  R = ZZ/101[x,a,b,c,d]	  
	  f = x^2+a*x+b
	  g = x^2+c*x+d
	  time eliminate(x,ideal(f,g))
	  time ideal resultant(f,g,x)
	  sylvesterMatrix(f,g,x)
	  discriminant(f,x)
	  ///,
     SeeAlso => {resultant, eliminate, Elimination}
     }

document {
     Key => {(sylvesterMatrix, RingElement, RingElement, RingElement),sylvesterMatrix},
     Usage => "sylvesterMatrix(f,g,x)",
     Inputs => {
	  "f" => {"a polynomial in a ring ", TT "R"},
	  "g" => {"a polynomial in the same ring"},
	  "x" => {"a variable in ", TT "R"}
	  },
     Outputs => {
     	  Matrix => {"the Sylvester matrix of ", TT "f", " and ", TT "g", " with respect to ", TT "x"}
	  },
     "Its determinant is the resultant of ", TT "f", " and ", TT "g", ".",
     EXAMPLE lines ///
	  R = ZZ/101[x,a,b,c,d,Degrees=>{1,1,2,1,2}];
	  R = ZZ[x,a,b,c,d]	  
	  f = x^7+3*x^4+a*x+b
	  g = x^8+x^5+c*x+d
	  time eliminate(ideal(f,g),x)
	  time ideal resultant(f,g,x)
	  sylvesterMatrix(f,g,x)
	  discriminant(f,x)
	  ///,
     SeeAlso => {resultant, discriminant, Elimination}
     }

TEST ///
R = ZZ/101[a..d]
time I = monomialCurveIdeal(R,{1,3,4})
time eliminate(I,{b})

R = ZZ[a,b,c,d,e]
f1 = a^4 + b*a + c
degree(a,f1)
f2 = a^2 + d*a + e
time sylvesterMatrix(f1,f2,a)
time resultant(f1,f2,a)
time discriminant(f1,a)
f3 = 1_R
time resultant(f1,f3,a)

R = ZZ/32003[a,b,c,d,e]
f1 = a^4 + b*a + c
f2 = a^2 + d*a + e
time resultant(f1,f2,a)
time eliminate(ideal(f1,f2),a)
///

TEST ///
  debug Elimination
  W1 = QQ[x, t_0, dt_0, s, WeylAlgebra => {{1, 2}}]
  W2 = QQ[x, t_0, dt_0, s, WeylAlgebra => { 1=>2 }]
  W3 = QQ[x, t_0, dt_0, s, WeylAlgebra => { t_0=>dt_0 }]
  W4 = QQ[x, t_0, dt_0, s, WeylAlgebra => {{t_0, dt_0}}]

  checkWeylAlgebra = W -> (
      I := ideal(W_0*W_1*W_2, W_0^2, 2*W_1^2*W_2^2+3*W_1*W_2, W_1*W_2+W_3+1);
      J := eliminate(I, {W_1, W_2});
      assert isSubset(J, I);
      --
      (F, G) := eliminationRing({1, 2}, W);
      perm := {1, 2, 0, 3}; invperm := inversePermutation perm;
      weyl := apply(W.WeylAlgebra, pair -> invperm_pair);
      assert((target F).WeylAlgebra == weyl);
      --
      W = QQ[W_*_perm, WeylAlgebra => weyl, MonomialOrder => Eliminate 2];
      I = sub(I, W);
      J = ideal selectInSubring(1, gens gb I);
      assert isSubset(J, I);
      )
  scan({W1, W2, W3, W4}, checkWeylAlgebra)
///

end
loadPackage "Elimination"
installPackage Elimination

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Elimination pre-install"
-- End:
