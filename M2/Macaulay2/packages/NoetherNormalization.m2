--=========================================================================--

newPackage(
     "NoetherNormalization",
     Version => "0.1", 
     Date => "Jan 18, 2007",
     Authors => {
	  {Name => "Nathaniel Stapleton", Email => "nstaple2@math.uiuc.edu"},
	  {Name => "Bart Snapp", Email => "snapp@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~snapp/"}
	  },
     Headline => "computes Noether Normalization",
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

integralSet = G -> (
     J = {};
     M := gens G;
     for i from 0 to numgens source M - 1 do ( -- check the gens of G to see if their leadMonomial is in a single variable
           if # support leadMonomial (M)_(0,i) === 1 then J = J | {support leadMonomial (M)_(0,i)} --checks how many vars are in the lead
           );
     J = unique flatten J; --note that according to the algorithm J is a set of integers (in fact indices), we choose to return the variables
     J);

--=========================================

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

lastCheck = (X,G,d) -> (
     M := gens G;
     i := 0;
     while i < min(d,numgens source M) and not isSubset(support M_(0,i),toList(X_0..X_(d-1))) do (
	  i = i+1;
	  );
     if i != d then return false
     else(
	  for j from d to #X-1 do (      
	       for p from 0 to numgens source M - 1 do (
		    if {X_j} == support leadTerm M_(0,p) then break;
                    if p == numgens source M - 1 then false
            	    );
               );
	  );
     true
     );
--==============================================


noetherPrime = (X,I,G,U,V) -> (
     R := ring I;
     k := coefficientRing R;
     done := false;
     f := map(R,R,reverse(U|V));
     while done == false do ( 
--	  G = gb f(I); --we should not need to do this gb computation
	  J := apply(integralSet(G),i -> f i); -- may need to do a gb comp.
	  V = apply(V, i -> f(i)); --there might be a faster way to do this, perhaps V={x_(#U)..x_(#U+#V-1)}
	  U = apply(U, i -> f(i)); -- might be faster to do U = {x_0..x_(#U-1)}
	  U = apply(U, i -> i + random(k)*sum(V - set J)); --make sure V and J jive so that this makes sense, also in later version multiply the sum by a random in k
      	  --note that right now we can get stuck in an infinite loop as we aren't multiplying by a random
	  g := map(R,R,reverse(U|V));
--	  g := map(R,R,U|V);
	  h = g*f;
	  G = gb h I;
	  done = lastCheck(X,G, #U);
	  if done then return((gens G,h));
	  (U,V) = varPrep G;
      	  );
     );

maxAlgPerm = (R,X,G,d) -> ( -- may need a sort or reverse...
     S := subsets(X,d);
     M := gens G;
     for j to # S - 1 do (
     	  for i to numgens source M - 1 do (
     	       if isSubset(support leadTerm M_(0,i),S_j) then break;
     	       if i == (numgens source M - 1) then return map(R,R,S_j|(X-set(S_j)))
	       );
     	  );
     );





noetherNotPrime = (X,I,G,d) -> (
     R := ring G;
     k := coefficientRing R;
     np := maxAlgPerm(R,X,G,d);
     I = np I;
     G = gb I;
     (U,V) := varPrep(X,G);
     f := map(R,R,reverse(U|V));
     done := false;
     while done == false do ( 
--   	  G = gb f(I); --we should not need to do this gb computation
	  J := apply(integralSet(G),i -> f i); -- may need to do a gb comp.
	  V = apply(V, i -> f(i)); --there might be a faster way to do this, perhaps V={x_(#U)..x_(#U+#V-1)}
	  U = apply(U, i -> f(i)); -- might be faster to do U = {x_0..x_(#U-1)}
	  U = apply(U, i -> i + random(k)*sum(V - set J)); --make sure V and J jive so that this makes sense, 
	                                                   --also in later version multiply the sum by a random in k
      	  --note that right now we can get stuck in an infinite loop as we aren't multiplying by a random
	  g := map(R,R,reverse(U|V));
--	  g := map(R,R,U|V);
	  h = g*f;
	  G = gb h I;
	  done = lastCheck(X,G, #U);
	  if done then return((gens G,h));
	  (U,V) = varPrep G;
      	  );
);


noetherNormalization = method();
noetherNormalization(Ideal) := Sequence => I -> (
     G := gb I;
     d := dim I;
     X := sort gens ring G;
     (U,V) := varPrep(X,G);
     if d == #U then noetherPrime(X,I,G,U,V) else noetherNotPrime(X,I,G,d)
     );     



--========================================================
--Examples:


R = QQ[x_4,x_3,x_2,x_1, MonomialOrder => Lex]; --the same ordering as in the paper
p = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);
benchmark "noetherNormalization(p)"
q:= x_4^2+x_3^5+x_2*x_1
leadMonomial(q)

--Examples of not so good I
--We need to worry about this guy some. The basis we get out does not quite exhibit the integrality of the variables that we want
--I get x_4x_1^3+x_4x_1^2, x_4^2x_1+x_4x_1^2, x_4^3+x_4x_1, x_5-x_3+x_1^3, We should see that x_4 and x_5 are integral
--x_4 is integral, x_5 is integral.
R = QQ[x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1^3 + x_1*x_2, x_2^3-x_4+x_3, x_1^2*x_2+x_1*x_2^2)
G = gb I
d = dim I
X = sort gens R -- note that this "sort" is very important
varPrep(X,G)
np = maxAlgPerm(R,X,G,d)
G = gb np I
(U,V) = varPrep(X,G)
noetherNormalization I



-- TO DO:
-- clear up output
-- get randomness
-- implement for nonprime ideals

-- Ok as far as the not prime case is concerned, let's write it as follows:
-- 
-- We'll do it with 3 routines.
-- 
-- noetherDecider
--         Will output T, and decided with algoritm to use.
--
-- noetherPrime
--        Will utilize the T being outputted
--
-- noetherNotPrime
--     	  Will also use T being outputted.





--=========================================================================--

beginDocumentation() -- the start of the documentation

-----------------------------------------------------------------------------

--docs

--=========================================================================--


