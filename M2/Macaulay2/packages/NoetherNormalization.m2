--=========================================================================--

newPackage(
     "NoetherNormalization",
     Version => "0.1", 
     Date => "Jan 18, 2007",
     Authors => {
	  {Name => "Bart Snapp", Email => "snapp@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~snapp/"},
	  {Name => "Nathaniel Stapleton", Email => "nstaple2@math.uiuc.edu"}
	  },
     Headline => "place an ideal in Noether normal position",
     DebuggingMode => true
     )

-- The algorithm given here is based on A. Logar's algorithm as given
-- in:

-- A. Logar. A Computational Proof of the Noether Normalization
-- Lemma. In: Proceedings 6th AAEEC, Lecture Notes in Computer Science
-- 357, Springer, 1989, 259--273.

-- an additional algorithm was implemented from:

-- H. Kredel and V. Weispfenning. Computing Dimension and Independent
-- sets for Polynomial Ideals. J. Symbolic Computation (1988) 6,
-- 231--247.


--=========================================================================--
     
export{noetherNormalization,noetherPosition} 
        
--=========================================================================--

-- initial comments: noetherNormalization takes in an ideal I of a
-- ring R over a field k such that the dimension of I in R is d (fix
-- these symbols for all comments) and returns a linear transformation
-- that puts the ideal in Noether position as well as the independent
-- variables of R.

-- comments: The procedure integralSet takes a Grobner basis G (ie. a
-- list of polynomials) and returns the variables that already have an
-- integral relation. It does this by taking the lead monomial of each
-- polynomial and checking whether or not it consists of a power of a
-- single variable. We are assuming that the ring is over a field and
-- thus we don't check the lead coefficient.


integralSet = G -> (
     J := {};
     M := gens G;
     for i from 0 to numgens source M - 1 do ( -- check the gens of G to see if their leadMonomial is in a single variable
           if # support leadMonomial (M)_(0,i) === 1 then J = J | {support leadMonomial (M)_(0,i)} --checks how many vars are in the lead
           );
     J = unique flatten J; --note that according to the algorithm J is a set of integers (in fact indices), we choose to return the variables
     J);

--=========================================
--comments: 

-- varPrep is the initial function we run on the Grobner basis of the
-- inputted ideal I. It returns sets U and V, with U being
-- algebraically independent and V being algebracially dependent. For
-- all prime ideals it returns a maximal algebraically independent set
-- of variables whose cardinality is equal to d.

varPrep = (X,I) -> (
     U := support (independentSets(I,Limit => 1))_0;
     (U,X - set U)
     );

--================================================== 

-- comments: We use lastCheck to check that our final Grobner basis
-- (the gb of the ideal after the change of variables) witnesses the
-- integrality of each variable that should be integral. It first
-- checks that there are no elements of the Grobner basis with support
-- in the independent variables and then that each variable that
-- should be integral after the linear transformation is integral.

lastCheck = (X,G,d) -> (
     M := gens G;
     i := 0;
     while i < numgens source M and not isSubset(support M_(0,i),toList(X_0..X_(d-1))) do ( 
	  i = i+1;
	  );
     if i != numgens source M then return false
     else(
     	  if X_{d..#X-1} != integralSet(G) then return false
	  );
     true
     );
--==============================================

-- comments: inverseSequence is used to give the inverse of a ring
-- map. A ring map is given by a list explaining where each of the
-- ring's variables should go. If the ring map is just a permutation
-- of the variables then it is obviously an isomorphism and
-- inverseSequence returns the sequence that gives the inverse
-- morphism.

inverseSequence = (U,X) -> (
     N := {};
     for i to #X - 1 do (
	  for j to #U - 1 do (
	       if X_i == U_j then (
		    N = {X_j}|N;
		    break;
		    );
	       );
	  );
     return N;
     );
--========================================================

-- comments: randomSum is used to make the random linear
-- transformations which are candidates for putting I in
-- noetherPosition. It takes in two lists and adds the second to the
-- first with random coefficients.


randomSum = (U,V,k) -> (
     for j to #V - 1 do (
	  U = apply(U, i -> i + random(k)*V_j);
	  );
     return U;
     );
--========================================================


-- comments: noetherNormalization is the main method. I is passed to
-- it by the user and it's Grobner basis is immediately computed.  It
-- takes the ideal and does a random linear transformation adding to
-- the independent variables the dependent ones that are not initially
-- integral. It then checks that the transformation put the ideal in
-- Noether position. It does this bye partially computing a Grobner
-- basis for the ideal until the partially computed Grobner basis
-- witnesses the integrality of all of the dependent variables. If the
-- entire Grobner basis is computed and the integrality is never
-- witnessed then we apply another random linear transformation and
-- start the process again. While doing all this we keep track of the
-- maps and the inverses of the maps that we use.

noetherNormalization = method(Options => {Verbose => false})
noetherNormalization(Ideal) := opts -> I -> (
     A := ring I;
     if not isPolynomialRing A then error "expected an ideal over a polynomial ring";
     if not isField coefficientRing A then error "expected the ring to contain a field";
     k := coefficientRing A;
     R := k [gens A,MonomialOrder => Lex];
     ff := map(R,A,gens R); --these maps merely change the order of the ring
     ffinverse := map(A, R, gens A); --these maps merely change the order of the ring
     I = ff(I);
     homogeneous := (isHomogeneous(R) and isHomogeneous(I)); 
     if homogeneous then G = gb(I, Algorithm => Homogeneous2); --MAYBE SHOULD USE ANOTHER HOMOGENOUS ALG/MAYBE TAKE THIS OUT
     if not homogeneous then G = gb I;
     d := dim I;
     X := sort gens R;
     (U,V) := varPrep(X,I);
     counter := 0; --counts the number of times lastCheck is called
     limitsequence := {5,20,40,60,80,infinity}; -- this is for the basiselementlimit setting for computing gb and is based on experience (and nothing else)
     done := false;
     indep := U;
     f := map(R,R,inverseSequence(U|V,X));
     finverse := map(R,R, reverse(U|V)); -- USED TO BE JUST (U|V)
     J := apply(integralSet G,i -> f i); -- may need to do a gb comp.
     V = apply(V, i -> f(i)); --there might be a faster way to do this, perhaps V={x_(#U)..x_(#U+#V-1)}
     U = apply(U, i -> f(i)); -- might be faster to do U = {x_0..x_(#U-1)}
     while not done do ( 
	  seqindex := 0;
     	  stuff := U;
     	  if counter == 0 then U = randomSum(U,V-set J,k);
	  if counter >= 1 then U = randomSum(U,V-set integralSet(G),k);
	  stuff = stuff + (stuff - U);
    	  g := map(R,R,reverse(U|V));
	  ginverse := map(R,R,reverse(stuff|V));
	  if g*ginverse != map(R,R,gens R) then << "--NOOOOO! g*ginverse is not the id! " << g*ginverse << " NOOOOO!" <<endl;
	  f = g*f;
	  finverse = finverse*ginverse;
     	  while (not done and seqindex < #limitsequence) do (
	       if homogeneous then ( -- may want to define f I above, but this causes noetherNormalization to fail at the moment
	       	    G = gb(f I, Algorithm => Homogeneous2, BasisElementLimit => limitsequence_seqindex);
	       	    );     
	       if not homogeneous then G = gb(f I, BasisElementLimit => limitsequence_seqindex); 
	       done = lastCheck(X,G,d);
     	       seqindex = seqindex + 1;
	       );
	  counter = counter + 1;
	  if counter == 5 then << "--warning: no good linear transformation found by noetherNormalization" <<endl;
	  if done or counter == 5 then return (
	       if opts.Verbose then << "--number of transformations attempted = " << counter << ";" << " BasisElementLimit = " << limitsequence_{seqindex - 1} <<endl;
	       use A;
      	      -- return (apply(x_1..x_d,i -> ffinverse finverse ff i),apply(indep, i -> ffinverse i),ffinverse*f*ff);
	       return (apply(X_{0..d-1},i -> ffinverse finverse i),ffinverse*f*ff,ffinverse*finverse*ff);
	       );
     	  ); -- f puts the ideal into noether normal position. f inverse goes back to the original ring 
     );      -- WE STILL NEED TO CACHE finverse to f. Also, note that noetherPosition relies on this routine.

--======================================================================================================================

noetherNormalization(QuotientRing) := opts -> R -> (
     if not isPolynomialRing ring ideal R then error "expected a quotient of a polynomial ring";
     noetherNormalization(ideal R, Verbose => opts.Verbose)
     );

--======================================================================================================================

noetherPosition = method(Options => {Verbose => false})
noetherPosition(Ideal) := opts -> I -> (
     (noetherNormalization(I,Verbose => opts.Verbose))_1     
     );

--======================================================================================================================

noetherPosition(QuotientRing) := opts -> R -> (
     (noetherNormalization(R,Verbose => opts.Verbose))_1     
     );

--======================================================================================================================



-- alg dependent vars, ideal, map

--           p       s
-- I < k[x] <= k[y] <- k[p^-1(U)] 
--             J<
	    
-- we take I we currently return p^-1, we want p,s,J --MIKE AGREES


--homogeneous stuff
--do we have an input option for the homogeneous case or do we always use the homogeneous program on homogeneous ideals?
--how do we do the linear change of variables if they have some weird weighting on the variables?

--isHomogeneous works on rings and ideals and elements
--use gb(...,Algorithm => Homogeneous2)
--homogenize(m,v) m ring element, v variable
--degree(v,m) v variable, m ring element, degree of m with respect to v

--========================================================
--Dan example
-- 1. use information about variables integral, algebraic, and other and put them in this ordering and then try it.
-- 2. is lex only used for varprep
-- 3. ps, man ps or top tells how long it takes, shell command called time also
-- 4. or just stop singular from printing
-- gbTrace

--=========================================================================--

beginDocumentation() -- the start of the documentation

-----------------------------------------------------------------------------

document { 
     Key => NoetherNormalization,
     Headline => "routines related to Noether normalization",
     EM "NoetherNormalization", " is a package for computing ring homomorphisms 
     which will place ideals in Noether normal position. The algorithms
     used are based on algorithms found in A. Logar's paper: A
     Computational Proof of the Noether Normalization Lemma. In:
     Proceedings 6th AAEEC, Lecture Notes in Computer Science 357,
     Springer, 1989, 259-273."
     }

-----------------------------------------------------------------------------
document {
     Key => noetherNormalization,
     Headline => "data for Noether normalization",
     "The function ", TT "noetherNormalization", " takes an ideal or a
     quotient of a polynomial ring, and outputs a sequence
     consisting of the following items: the list of algebraically independent 
     variables in the ring ", TT "R/I", ", and a map from ", TT "R", " to ", --(R a polynomial ring)
     TT "R", " which will place ideal ", TT "I", " into Noether normal
     position. The ideal in question is placed into Noether normal
     position using a random linear change of coordinates, hence one should
     expect the output to change each time the routine is executed.",
     EXAMPLE {
	  "R = QQ[x_1..x_4];",
	  "I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);",
	  "noetherNormalization I",
	  "noetherNormalization(R/I)",
	  },
     "If ", TT "noetherNormalization", " is unable to place the ideal
     in Noether normal position after a few tries, the following warning is given:",
     EXAMPLE {
	  "R = ZZ/2[a,b];",
	  "I = ideal(a^2*b+a*b^2+1);",
	  "noetherNormalization I"
	  },
     "Here is an example with the option ", TT "Verbose => true", ":",
     EXAMPLE {
	  "R = QQ[x_1..x_4];",
	  "I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);",
	  "noetherNormalization(I,Verbose => true)"
	  },
     PARA {
     "This symbol is provided by the package ", TO NoetherNormalization, "."
     }
     }

--=========================================================================--

document {
     Key => noetherPosition,
     Headline => "a map to Noether normal position",
     "The function ", TT "noetherPosition", " takes an ideal or a
     quotient of a polynomial ring, and outputs a map from ", TT "R", " to ", --(R a polynomial ring)
     TT "R", " which will place ideal ", TT "I", " into Noether normal
     position. The ideal in question is placed into Noether normal
     position using a random linear change of coordinates, hence one should
     expect the output to change each time the routine is executed.",
     EXAMPLE {
	  "R = QQ[x_1..x_4];",
	  "I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);",
	  "noetherPosition I",
	  "noetherPosition(R/I)",
	  },
     "If ", TT "noetherPosition", " is unable to place the ideal
     in Noether normal position after a few tries, the following warning is given:",
     EXAMPLE {
	  "R = ZZ/2[a,b];",
	  "I = ideal(a^2*b+a*b^2+1);",
	  "noetherPosition I"
	  },
     "Here is an example with the option ", TT "Verbose => true", ":",
     EXAMPLE {
	  "R = QQ[x_1..x_4];",
	  "I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);",
	  "noetherPosition(I,Verbose => true)"
	  },
     PARA {
     "This symbol is provided by the package ", TO NoetherNormalization, "."
     }
     }
--=========================================================================--


