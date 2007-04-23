--=========================================================================--

newPackage(
     "NoetherNormalization",
     Version => "0.1", 
     Date => "Jan 18, 2007",
     Authors => {
	  {Name => "Nathaniel Stapleton", Email => "nstaple2@math.uiuc.edu"},
	  {Name => "Bart Snapp", Email => "snapp@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~snapp/"}
	  },
     Headline => "place an ideal in Noether normal position",
     DebuggingMode => true
     )

-- The algorithm given here is based on A. Loger's algorithm as given
-- in "A Computational Proof of the Noether Normalization Lemma" LNCS
-- 357.

--=========================================================================--
     
export{noetherNormalization} -- if the new routines which you are adding have new
-- names, then they need to be exported; otherwise they should not be
-- exported
        
--=========================================================================--
--We are not using the power of lemma 3.2 when we do this, only lemma 3.1 so we can currently apply this to all ideals
--However, lemma 3.2 only works if the ideal is prime and we never actually test for this (and probably shouldn't).
integralSet = G -> (
     J = {};
     M := gens G;
     for i from 0 to numgens source M - 1 do ( -- check the gens of G to see if their leadMonomial is in a single variable
           if # support leadMonomial (M)_(0,i) === 1 then J = J | {support leadMonomial (M)_(0,i)} --checks how many vars are in the lead
           );
     J = unique flatten J; --note that according to the algorithm J is a set of integers (in fact indices), we choose to return the variables
     J);

--=========================================
--finds algebraically independent variables
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
--makes sure that the that the transformation has put the ideal in Noether position
lastCheck = (X,G,d) -> (
     M := gens G;
     i := 0;
     while i < numgens source M and not isSubset(support M_(0,i),toList(X_0..X_(d-1))) do ( 
	  i = i+1;
	  );
     if i != numgens source M then return false
     else(
	  for j from d to #X-1 do (      
	       for p from 0 to numgens source M - 1 do (
		    if {X_j} == support leadTerm M_(0,p) then break;
                    if p == numgens source M - 1 then return false;
            	    );
               );
	  );
     true
     );

--==============================================
--maxAlgPerm is a recursive version of maxAlgPerm that for large #X and medium d should be far faster, it appears to be working.
--when an ideal is not good (not prime and varPrep does not give the dimension) then maxAlgPerm permutes
--the variables into a guaranteed good position (note: this algorithm is not from logar but was referenced by him)
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
--quick fix, this does need to be done at some point but I suspect that it can be returned with varPrep even and then we don't have to do this extra looping.
--merely finds the inverse permutation
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

randomSum = (U,V,k) -> (
     for j to #V - 1 do (
	  U = apply(U, i -> i + random(k)*V_j);
	  );
     return U;
     );
--========================================================

noetherPrime = (R,X,I,G,U,V,d,np,npinverse,homogeneous) -> (
     counter := 0; --counts the number of times lastCheck is called
     limitsequence = {5,20,40,60,80,infinity};
     seqindex = 0;
     k := coefficientRing R;
     done := false;
     f := map(R,R,inverseSequence(U|V,X));
     finverse := map(R,R, reverse(U|V));
     J := apply(integralSet(G),i -> f i); -- may need to do a gb comp.
     V = apply(V, i -> f(i)); --there might be a faster way to do this, perhaps V={x_(#U)..x_(#U+#V-1)}
     U = apply(U, i -> f(i)); -- might be faster to do U = {x_0..x_(#U-1)}
     while done == false do ( 
     	  stuff = U;
     	  if counter == 0 then U = randomSum(U,V-set J,k);
	  if counter >= 1 then U = randomSum(U,V-set integralSet(G),k);
	  stuff = stuff + (stuff - U);
    	  g := map(R,R,reverse(U|V));
	  ginverse := map(R,R,reverse(stuff|V));
	  f = g*f;
	  finverse = finverse*ginverse;
     	  while (done == false and seqindex < #limitsequence) do (
	       if homogeneous then (
	       	    G = gb(f I, Algorithm => Homogeneous2, BasisElementLimit => limitsequence_seqindex);
	       	    );     
	       if not homogeneous then G = gb(f I, BasisElementLimit => limitsequence_seqindex);
	       done = lastCheck(X,G, d);
     	       seqindex = seqindex + 1;
	       );
	  counter = counter + 1;
	  if done or (counter == 100) then return((counter,V,transpose gens G,f*np)); --if returning the inverse map then npinverse*finverse
      	  );
     );
 
noetherNotPrime = (R,X,I,G,d,homogeneous) -> (
     S := maxAlgPerm(R,X,G,d,{});
     np := map(R,R, inverseSequence(reverse(X-set(S)|S),X));
     npinverse := map(R,R,(X-set(S)|S));
     I = np I;
     G = gb I;
     (U,V) := varPrep(X,G);
     noetherPrime(R,X,I,G,U,V,d,np,npinverse,homogeneous)
     );

-- alg dependent vars, ideal, map

--          p       s
--I >-> k[x] <=< k[y] <- k[t]
--            J<
	    
--we take I we currently return p^-1, we want p,s,J
--don't compute the inverse asking for it. 

noetherNormalization = method()
noetherNormalization(Ideal) := Sequence => (I) -> (
     R := ring I;
     homogeneous := (isHomogeneous(R) and isHomogeneous(I));
     if homogeneous then G = gb(I, Algorithm => Homogeneous2);
     if not homogeneous then G = gb I;
     d := dim I;
     X := sort gens R;
     (U,V) := varPrep(X,G);
     if d == #U then noetherPrime(R,X,I,G,U,V,d,id_R,id_R,homogeneous) else noetherNotPrime(R,X,I,G,d,homogeneous)
     );     

--homogeneous stuff
--do we have an input option for the homogeneous case or do we always use the homogeneous program on homogeneous ideals?
--how do we do the linear change of variables if they have some weird weighting on the variables?

--isHomogeneous works on rings and ideals and elements
--use gb(...,Algorithm => Homogeneous2)
--homogenize(m,v) m ring element, v variable
--degree(v,m) v variable, m ring element, degree of m with respect to v

--========================================================
--dan example
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
     which will place ideals in Noether normal position.
     ."
     }
()
-----------------------------------------------------------------------------
document {
     Key => (noetherNormalization, Ideal),
     Headline => "places an ideal in Noether normal position",
     Usage => "noetherNormalization(I)",
     Inputs => {
	  "I" => {}
	  },
     Outputs => {
	  Sequence => {}
	  },
     "The function ", TT "noetherNormalization(I)", "outputs a sequence 
     consisting of the following items: the list of algebraically independent 
     variables in the ring of ", TT "I", ", and a map from ", TT "R", " to ", 
     TT"R", " which will place the original ideal into Noether normal position.",  
     EXAMPLE {
	  "A = QQ[x_1..x_3]/ideal(x_1^2, x_1*x_2);",
	  "x_1+x_2"
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


