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

-- The algorithm given here is based on A. Loger's algorithm as given
-- in:

-- A. Logar. A Computational Proof of the Noether Normalization
-- Lemma. In: Proceedings 6th AAEEC, Lecture Notes in Computer Science
-- 357, Springer, 1989, 259--273.

-- an additional algorithm was implemented from:

-- H. Kredel and V. Weispfenning. Computing dimension and independent
-- set for polynomial ideals. 1986. -- CITE BOOK THIS OCCURES IN


--=========================================================================--
     
export{noetherNormalization} -- if the new routines which you are adding have new
-- names, then they need to be exported; otherwise they should not be
-- exported
        
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
     J = {};
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
-- of variables whose cardinality is equal to d. If the set returned
-- by varPrep is smaller than d (it should never be larger) then the
-- ideal is not prime. varPrep is proven to work in this way in
-- Logar's paper. In case varPrep returns a set smaller than d the
-- ideal is sent to maxAlgPerm. What it does:


varPrep = (X,G) -> (
     M := gens G;
     V := {};
     for j from 0 to #X - 1 do (
      	  for i from 0 to numgens source M - 1 do ( -- going from zero to the number of gens of the gb - 1      
	       if isSubset({X_j}, support (M)_(0,i)) -- checks to see if the there is a term of the desired index
	       and isSubset(support (M)_(0,i),take(X,j+1)) -- checks to see if there is a higher indexed term
	       then (
            	    V = V | {X_j};                -- repeatedly appending could be slow, try for ... list or while ... list
            	    break;   
		    );
               );     
      	  );
     (X-set(V),V)                        -- (x,y) = (U,V) ; (x,y) := (U,V) can be used by the caller if you return a sequence
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


-- comments: The purpose of maxAlgPerm is to take an ideal that
-- varPrep failed on (ie. varPrep(I)<d) and make a change of variables
-- in order for varPrep to succeed. It essentially takes the offending
-- Grobner basis and returns a maximal alg. independent set of
-- variables which are then moved to the highest valued variables in
-- the lex ordering via a permutation. The algorithm is referenced by
-- Logar and can be found in Kredel and Weispfenning, "Computing
-- dimension and independent sets for polynomial ideals"

maxAlgPerm = (R,X,G,d,S) -> (
     M := gens G;
     if #S == d then return S;
     for j to #X - 1 do (
	  for i to numgens source M -1 do (
	       if isSubset(support leadTerm M_(0,i),S|{X_j}) then break;
     	       if i == (numgens source M - 1) then ( 
		    S = maxAlgPerm(R,X - set({X_j}),G,d,S|{X_j});
		    if not instance(S,List) then S = {};
		    if #S == d then return S; 
		    ); 
	       );
	  );
     );
--======================================================

-- comments: inverseSequence is used to give the inverse of a ring
-- map. A ring map is given by a list explaining where each of the
-- ring's variables should go. If the ring map is just a permutation
-- of the variables then it is obviously an isomorphism and
-- inverseSequence returns the sequence that give the inverse
-- morphism.

inverseSequence = (U,X) -> (
     N = {};
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

-- comments: noetherPrime is where the action happens. It takes the
-- ideal (which may have been fixed by maxAlgPerm) and does a random
-- linear transformation adding to the independent variables the
-- dependent ones that are not initially integral. It then checks that
-- the transformation put the ideal in noether position. It does this
-- bye partially computing a Grobner basis for the ideal until the
-- partially computed Grobner basis witnesses the integrality of all
-- of the dependent variables. If the entire Grobner basis is computed
-- and the integrality is never witnessed then we apply another random
-- linear transformation and start the process again. While doing all
-- this we keep track of the maps and the inverses of the maps that we
-- use.


-- MAY WANT TO USE productOrder

noetherPrime = (R,X,I,G,U,V,d,np,npinverse,homogeneous) -> (
     counter := 0; --counts the number of times lastCheck is called
     limitsequence := {5,20,40,60,80,infinity}; -- this is for the basiselementlimit setting for computing gb and is based on experience (and nothing else)
     k := coefficientRing R;
     done := false;
     indep := U;
     f := map(R,R,inverseSequence(U|V,X));
     finverse := map(R,R, reverse(U|V));
     J := apply(integralSet(G),i -> f i); -- may need to do a gb comp.
     V = apply(V, i -> f(i)); --there might be a faster way to do this, perhaps V={x_(#U)..x_(#U+#V-1)}
     U = apply(U, i -> f(i)); -- might be faster to do U = {x_0..x_(#U-1)}
     while done == false do ( 
	  seqindex := 0;
     	  stuff := U;
     	  if counter == 0 then U = randomSum(U,V-set J,k);
	  if counter >= 1 then U = randomSum(U,V-set integralSet(G),k);
	  stuff = stuff + (stuff - U);
    	  g := map(R,R,reverse(U|V));
	  ginverse := map(R,R,reverse(stuff|V));
	  f = g*f;
	  finverse = finverse*ginverse;
     	  while (not done and seqindex < #limitsequence) do (
	       if homogeneous then ( -- SAVE f I here rather than below....
	       	    G = gb(f I, Algorithm => Homogeneous2, BasisElementLimit => limitsequence_seqindex);
	       	    );     
	       if not homogeneous then G = gb(f I, BasisElementLimit => limitsequence_seqindex); -- SAVE f I above, as opposed to running it new
	       done = lastCheck(X,G, d);
     	       seqindex = seqindex + 1;
	       );
	  counter = counter + 1;
	  if done or (counter == 100) then return((counter,limitsequence_{seqindex - 1},indep,f*np)); --U is the algebraically independent vars, if returning the inverse map then npinverse*finverse
      	  );
     );
--========================================================================================================================

-- comments: noetherNotPrime is where the ideals that fail varPrep
-- go. They are fixed by maxAlgPerm and then send to noetherPrime
-- along with the appropriate maps.

noetherNotPrime = (R,X,I,G,d,homogeneous) -> (
     S := maxAlgPerm(R,X,G,d,{});
     np := map(R,R, inverseSequence(reverse(X-set(S)|S),X));
     npinverse := map(R,R,(X-set(S)|S));
     I = np I;
     G = gb I;
     (U,V) := varPrep(X,G);
     noetherPrime(R,X,I,G,U,V,d,np,npinverse,homogeneous)
     );
--=======================================================================================================================

-- comments: noetherNormalization is the main method. I is passed to
-- it by the user and it's Grobner basis is immediately
-- computed. Using this it checks if varPrep returns a maximal
-- alg. independent set and decides to send it to noetherPrime if it
-- does and to noetherNotPrime if it doesn't.

noetherNormalization = method()
noetherNormalization(Ideal) := Sequence => (I) -> (-- GIVE ERROR IF NOT POLYNOMIAL RING
     R := coefficientRing ring I [gens ring I,MonomialOrder => Lex];
     f := map(R,ring I,gens R);
     I = f(I);
     homogeneous := (isHomogeneous(R) and isHomogeneous(I)); 
     if homogeneous then G = gb(I, Algorithm => Homogeneous2); --MAYBE SHOULD USE ANOTHER HOMOGENOUS ALG/MAYBE TAKE THIS OUT
     if not homogeneous then G = gb I;
     d := dim I;
     X := sort gens R;
     (U,V) := varPrep(X,G); -- MAYBE USE independentSets SEE DOCS limit of 1, note you'll have to take the support
     if d == #U then noetherPrime(R,X,I,G,U,V,d,id_R,id_R,homogeneous) else noetherNotPrime(R,X,I,G,d,homogeneous)
     );
--======================================================================================================================
noetherNormalization(QuotientRing) := Sequence => (R) -> (
     noetherNormalization(ideal R)
     );
--======================================================================================================================
-- alg dependent vars, ideal, map

--          p       s
--I >-> k[x] <=< k[y] <- k[t]
--            J<
	    
--we take I we currently return p^-1, we want p,s,J
--don't compute the inverse asking for it.      

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
     Headline => "place an ideal in Noether normal position",
     EM "NoetherNormalization", " is a package for computing ring homomorphisms 
     which will place ideals in Noether normal position. The algorithms
     used are based on algorithms found in A. Logar's paper: A
     Computational Proof of the Noether Normalization Lemma. In:
     Proceedings 6th AAEEC, Lecture Notes in Computer Science 357,
     Springer, 1989, 259-273. However, there are some important differences."
     }

-----------------------------------------------------------------------------
document {
     Key => (noetherNormalization, Ideal),
     Headline => "places an ideal in Noether normal position",
     Usage => "noetherNormalization(I)",
     Inputs => {
	  "I" => {"an ideal over a polynomial ring"}
	  },
     Outputs => {
	  Sequence => {}
	  },
     "The function ", TT "noetherNormalization", " outputs a sequence 
     consisting of the following items: the list of algebraically independent 
     variables in the ring ", TT "R/I", ", and a map from ", TT "R", " to ", --(R a polynomial ring)
     TT"R", " which will place ideal ", TT "I", " into Noether normal
     position. The routine requires the ring to ordered via the
     lexicographic ordering.",
     EXAMPLE {
	  "R = QQ[x_1..x_4, MonomialOrder => Lex];",
	  "I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);",
	  "noetherNormalization(I)"
	  },
     EXAMPLE {
	  "1+1"
	  },
     "Give some details of the algorithm.",
     PARA {
     "This symbol is provided by the package ", TO NoetherNormalization, "."
     }
     }

--=========================================================================--


