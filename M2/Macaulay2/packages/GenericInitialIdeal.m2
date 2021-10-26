--======================================================================--

newPackage(
     "GenericInitialIdeal",
     Version => "0.2", 
     Date => "July 1, 2008",
     Authors => {
	  {Name => "Alexandra Seceleanu", Email => "asecele2@uiuc.edu"},
	  {Name => "Nathaniel Stapleton", Email => "nstaple2@math.uiuc.edu"}
	  },
     Headline => "find the generic initial ideal of a given ideal",
     Keywords => {"Commutative Algebra"},
     DebuggingMode => false
     )
--=========================================================================--
     
export{"gin","lexgin","AttemptCount","Modular","Multigraded"} -- if the new routines which you are adding have new
-- names, then they need to be exported; otherwise they should not be
-- exported

--========================================================================--
-- PURPOSE: find the most frequent entry in a list
-- if more than one most frequent entries it returns one of them
-- INPUT: List
-- OUTPUT: Thing

mode = L -> (
     w := hashTable apply(pairs tally L, (k,v) -> (v,k));
     return w#(max keys w)
     )

--========================================================================--
-- PURPOSE: helper function to build the generic element in the product
-- of GLs (for the multigraded case)
-- INPUT: Polynomial ring
-- OUTPUT: Matrix

rowMatrix = (S) -> (
    degZero := apply(degree((flatten entries vars S)_0), d -> 0);
    matrix apply(flatten entries vars S, v -> apply(flatten entries vars S, w -> if degree v == degree w then random(degZero, S)*random(degZero, S) else 0))
    );


--========================================================================--
-- PURPOSE: compute the generic initial ideal of a given ideal
-- 20/07/2018 Lorenzo: modified to compute the multigraded generic initial ideal
-- INPUT: Ideal
-- OUTPUT: MonomialIdeal

gin = method(Options => {AttemptCount => 7, Verbose => false, Modular => false, MonomialOrder => null, Multigraded => false})
gin(Ideal) := opts -> (I) -> (
     -- LOCAL VARIABLES:
     -- S is the variable in which we save the original ring;
     -- c is the coefficient field; it is the coefficient field of the ring of I if Modular is not used
     -- and it is ZZ mod a random large prime if Modular option is used
     -- l is a list of large primes
     -- R is the ring obtained from the ring of I by replacing the coefficient field with c and the
     -- monomial order with the one specitied by the optional parameter 
     -- attempts is a list of initial ideals out of which we choose the most frequent as the gin 
     S:= ring I;
     c := 0; 
     l := {32003, 32009, 32027, 32029, 32051, 32057, 32059, 32063, 32069, 32077, 32083, 32089, 32099, 32117, 32119, 32141, 32143, 32159, 32173, 32183, 32189, 32191}; -- l is a list of large primes; we use a random element of this list if the modular option is set to true
     if opts.Modular then (
	  p:=random(0,#l-1);
	  p=l_p;
     	  c=ZZ/p;
	  )
     else c = coefficientRing ring I;
     R := c(monoid [gens ring I, Degrees=> if opts.Multigraded then (options ring I).Degrees, MonomialOrder => {if opts.MonomialOrder =!= null then opts.MonomialOrder else (options ring I).MonomialOrder}]);--R has the specified MomomialOrder and Modular coeff field
         n := # gens R;
         f := map(R,ring I,gens R);
         I = f I; -- view I as an ideal of the new ring I;
     if isHomogeneous I then hf := poincare I; -- giving the Hilbert function helps with Grobner basis computations
     count := 0;
     attempts := {};
     -- addition for the multigraded case
     if opts.Multigraded then (
         attempts = for count to opts.AttemptCount-1 list (
             g := rowMatrix(ring I) * (transpose vars ring I); --uses rowMatrix
             genericI := sub(I,apply(entries g, (entries vars ring I)_0, (r, v) -> v => r_0));
     if isHomogeneous I then monomialIdeal leadTerm gb(genericI)
     else monomialIdeal leadTerm genericI
     );
     )-- make a list of initial ideals
     else (
     attempts = for count to opts.AttemptCount-1 list (
         g:= random(R^1,R^{n:-1}); --may want to make this transformation lower triangular or upper
	     F := map(R,R,g);-- or:  genericI := substitute(I,g);
	     genericI := F I;--time genericI := F I;
	     if isHomogeneous I then monomialIdeal leadTerm gb(genericI, Hilbert => hf)
	     else monomialIdeal leadTerm genericI
     	     );
     	   );-- make a list of initial ideals
     genericI := mode attempts; -- take the most frequent element of the list
     good := isBorel genericI; -- taken into account only for the standard graded case
     f = map(S,R,gens S);
     generic := f genericI; -- map the generic initial ideal back into the original ring
     if not good and not opts.Multigraded then stderr << "--warning: potential generic initial ideal is not strongly stable" << endl;
     if opts.Verbose then (
	  << "--potential generic ideal showed up "<< (tally attempts)#(genericI)<< " out of " << opts.AttemptCount << " times." << endl;
	  print netList pairs tally attempts;
	  );
     --use S;
     generic
     );

--======================================================================================================================
-- PURPOSE: get the generic initial ideal of I when  R/I is given
-- INPUT: QuotientRing
-- OUTPUT: MonomialIdeal

gin(QuotientRing) :=Ideal => (R) -> (
     gin(ideal R)
     );

-- ====================================================================================================================

lexgin = method(Options => {AttemptCount => 7, Verbose => false, Modular => false, Multigraded => false})
lexgin(Ideal) := opts -> (I) -> ( gin(I,AttemptCount => opts.AttemptCount, Verbose => opts.Verbose, Modular => opts.Modular, MonomialOrder => Lex, Multigraded => opts.Multigraded));
lexgin(QuotientRing):=Ideal => (R) -> (lexgin(ideal R));

--==================================================================================================================
beginDocumentation()

document {
     Key => {GenericInitialIdeal},
     Headline => "find the generic initial ideal of a given ideal",
     TT "GenericInitialIdeal", " is a package for computing generic initial ideals of ideals in a polynomial ring, that is, the monomial ideal of lead terms after a random change of coordinates.  All of these routines are probabilistic: 
with high probability, they give the correct answer, but it could be the case that the choice of coordinates is too special."
     }

-- 20/07/2018 Lorenzo: documentation updated with the multigraded gin
document {
     Key => {gin},
     Headline => "the generic initial ideal",
     Usage => " gin I",
     Inputs => {"I" => {"an ", TO Ideal, " in a polynomial", TO Ring},
	  AttemptCount => {"sets the number of  random coordinate changes the routine attempts before choosing the potential",TT "gin"," ."},
	  Modular => {"if set to be true, computations are performed modulo a large random prime ."},
	  MonomialOrder => {"sets the", TT " Monomial Order "," used in the computation of " ,TT "gin ","."},
	  Multigraded => {"if true computes the multigraded gin w.r.t. the multigrading of ",TT "ring I","."},
	  Verbose => {"provides a summary of the random initial ideals generated and warns if the selected one is not strongly stable."},
	  },
     Outputs => {{"an ", TO Ideal, ", the generic initial ideal of ", TT "I", "."}},
     Caveat => { "The method ", TT " gin"," uses a probabilistic algorithm. The returned answer is correct with high probability in characteristic zero and large positive characteristic, but might be wrong in small positive characteristic. For details in this situation it is recommended to use the Verbose option.",},
     SeeAlso =>"lexgin",
     PARA {"Example: a complete intersection of type (3,3) in P^3"},
     EXAMPLE lines ///
	  R = QQ[a..d];
	  I = ideal(a^3+c^2*d, b^3-a*d^2);
	  gin(I)
	  ///,
     PARA{"The Stanley-Reisner ideal of RP^2"},
     EXAMPLE lines ///
     R = QQ[x0,x1,x2,x3,x4,x5]
     M = matrix {{x1*x3*x4, x0*x3*x4, x1*x2*x4, x0*x2*x3, x0*x1*x2, x2*x4*x5, x0*x4*x5, x2*x3*x5, x1*x3*x5, x0*x1*x5}} --Stanley-Reisner ideal of RP^2
     I=ideal flatten entries M
     J=(ideal{x0,x1,x2})^3
     assert(gin(I)==J)
     ///,
     PARA {"Example 1.10 from Conca, De Negri, Gorla  'Cartwright-Sturmfels ideals
      associated to graphs and linear spaces'."},
     EXAMPLE lines ///
     R = QQ[x_1..x_3,y_1..y_3, Degrees=>{{1,0},{1,0},{1,0},{0,1},{0,1},{0,1}}];
     I = ideal(x_1*y_1,x_2*y_2,x_3*y_2,x_2*y_3,x_3*y_3);
     gin(I)
     gin(I, Multigraded => true)
     ///,
     PARA {"This symbol is provided by the package ", TO GenericInitialIdeal, "." }
     }

--These are documented in the above node.
undocumented { "AttemptCount", "Modular", "Multigraded" }

document { Key => {(gin,Ideal)}, }
document { Key => {(gin,QuotientRing)}, }

document {
     Key => {lexgin},
     Headline => "the generic initial ideal with respect to lexicographical order",
     Usage => "lexgin I",
     Inputs => {"I" => {"an ", TO Ideal, " in a polynomial", TO Ring},
	  AttemptCount => {"sets the number of  random coordinate changes the routine attempts before choosing the potential",TT "gin"," ."},
	  Modular => {"if set to be true, computations are performed modulo a large random prime ."},
	  Multigraded => {"if true computes the multigraded gin w.r.t. the multigrading of ",TT "ring I","."},
	  Verbose => {"provides a summary of the random initial ideals generated and warns if the selected one is not strongly stable."},
     },
     Outputs => {{"an ", TO Ideal, ", the generic initial ideal of ", TT "I", "."}},
     PARA {"Same as gin with MonomiaOrder => Lex"},
     SeeAlso => "gin",
      PARA {"This symbol is provided by the package ", TO GenericInitialIdeal, "." }
     }


document { Key => {(lexgin,Ideal)} }
document { Key => {(lexgin,QuotientRing)} }


TEST ///
R = QQ[x0,x1,x2,x3,x4,x5]
M = matrix {{x1*x3*x4, x0*x3*x4, x1*x2*x4, x0*x2*x3, x0*x1*x2, x2*x4*x5, x0*x4*x5, x2*x3*x5, x1*x3*x5, x0*x1*x5}} --Stanley-Reisner ideal of RP^2
I=ideal flatten entries M
J=(ideal{x0,x1,x2})^3
assert(gin(I)==J)
///

TEST ///
R=QQ[x,y,z]
I=ideal{x+y-6*z,x*y+5*x*z+y*z,x*y*z+4}
assert(lexgin I==ideal(x,y,z^6))
///

------------------------------------------------------------------------
-- 20/07/2018 Lorenzo: new tests
------------------------------------------------------------------------
-- Cartwright-Sturmfels ideals associated to graphs and linear spaces --
-- Aldo Conca, Emanuela De Negri, Elisa Gorla --
-- Ex. 1.10
------------------------------------------------------------------------
TEST ///
R = QQ[x_1..x_3,y_1..y_3, Degrees=>{{1,0},{1,0},{1,0},{0,1},{0,1},{0,1}}]
I = ideal(x_1*y_1,x_2*y_2,x_3*y_2,x_2*y_3,x_3*y_3)
standard = gin(I)
assert(standard == ideal(x_1^2,x_1*x_2,x_2^2,x_1*x_3,x_2*x_3,x_3^3,x_3^2*y_1))
J = gin(I, Multigraded => true)
assert(J == ideal(x_1*y_1,x_2*y_1,x_3*y_1,x_1*y_2,x_2*y_2,x_1^2*y_3,x_1*x_2*y_3))
///


TEST ///
R = QQ[x_1..x_3,y_1,y_2,z_1,z_2, Degrees=>{{1,0,0},{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}}]
I = ideal(x_1*y_1*z_1,x_2*y_2,x_1*x_3*y_2,y_1*z_2)
J = gin(I, Multigraded => true)
assert(J == ideal(x_1*y_1,x_2^2*y_1,y_1*z_1,x_1*y_2*z_1,x_2^2*y_2*z_1,x_2*y_1*z_2,x_1^2*y_2*z_2,x_1*x_2*y_2*z_2))
K = lexgin(I, Multigraded => true)
assert(K == ideal(x_1*y_1,x_2^2*y_1,y_1*z_1,x_1*y_2*z_1,x_2^2*y_2*z_1,x_2*y_1^2*z_2,x_1*y_2*z_2))
///
