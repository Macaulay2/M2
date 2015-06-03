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
     DebuggingMode => false
     )
--=========================================================================--
     
export{"gin","lexgin","AttemptCount","Modular" } -- if the new routines which you are adding have new
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
-- PURPOSE: compute the generic initial ideal of a given ideal
-- INPUT: Ideal
-- OUTPUT: MonomialIdeal

gin = method(Options => {AttemptCount => 7, Verbose => false, Modular => false, MonomialOrder => null})
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
     R := c(monoid [gens ring I, MonomialOrder => {if opts.MonomialOrder =!= null then opts.MonomialOrder else (options ring I).MonomialOrder}]);--R has the specified MomomialOrder and Modular coeff field
     n := # gens R;
     f := map(R,ring I,gens R);
     I = f I; -- view I as an ideal of the new ring I;
     if isHomogeneous I then hf := poincare I; -- giving the Hilbert function helps with Grobner basis computations
     count := 0;
     attempts := for count to opts.AttemptCount-1 list (
       	  g:= random(R^1,R^{n:-1}); --may want to make this transformation lower triangular or upper 
	  F := map(R,R,g);-- or:  genericI := substitute(I,g);
	  genericI := F I;--time genericI := F I;
	  if isHomogeneous I then monomialIdeal leadTerm gb(genericI, Hilbert => hf)
	  else monomialIdeal leadTerm genericI
     	  );-- make a list of initial ideals
     genericI := mode attempts; -- take the most frequent element of the list
     good := isBorel genericI; 
     f = map(S,R,gens S);
     generic := f genericI; -- map the generic initial ideal back into the original ring
     if not good then stderr << "--warning: potential generic initial ideal is not strongly stable" << endl;
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

lexgin = method(Options => {AttemptCount => 7, Verbose => false, Modular => false})
lexgin(Ideal) := opts -> (I) -> ( gin(I,AttemptCount => opts.AttemptCount, Verbose => opts.Verbose, Modular => opts.Modular, MonomialOrder => Lex));
lexgin(QuotientRing):=Ideal => (R) -> (lexgin(ideal R));

--==================================================================================================================
beginDocumentation()

document {
     Key => {GenericInitialIdeal},
     Headline => "find the generic initial ideal of a given ideal",
     TT "GenericInitialIdeal", " is a package for computing generic initial ideals of ideals in a polynomial ring, that is, the monomial ideal of lead terms after a random change of coordinates.  All of these routines are probabilistic: 
with high probability, they give the correct answer, but it could be the case that the choice of coordinates is too special."
     }

document {
     Key => {gin},
     Headline => "the generic initial ideal",
     Usage => " gin I",
     Inputs => {"I" => {"an ", TO Ideal, " in a polynomial", TO Ring},
	  AttemptCount => {"sets the number of  random coordinate changes the routine attempts before choosing the potential",TT "gin"," ."},
	  Modular => {"if set to be true, computations are performed modulo a large random prime ."},
	  MonomialOrder => {"sets the", TT "Monomial Order "," used in the computation of " ,TT "gin ","."},
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
     PARA {"This symbol is provided by the package ", TO GenericInitialIdeal, "." }
     }

document { Key => {(gin,Ideal)}, }
document { Key => {(gin,QuotientRing)}, }

document {
     Key => {lexgin},
     Headline => "the generic initial ideal with respect to lexicographical order",
     Usage => "lexgin I",
     Inputs => {"I" => {"an ", TO Ideal, " in a polynomial", TO Ring},
	   AttemptCount => {"sets the number of  random coordinate changes the routine attempts before choosing the potential",TT "gin"," ."},
	  Modular => {"if set to be true, computations are performed modulo a large random prime ."},
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
