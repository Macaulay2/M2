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

--tricky = (U,next)

--maxAlgPerm is a recursive version of maxAlgPerm that for large #X and medium d should be far faster, it appears to be working.
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

--quick fix, this does need to be done at some point but I suspect that it can be returned with varPrep even and then we don't have to do this extra looping.		    
inverseSequence = (U,X) -> (
     N = {};
     for i to #X - 1 do (
	  for j to #U - 1 do (
	       if X_i == U_j then (
		    N = N|{X_j};
		    break;
		    );
	       );
	  );
     return N;
     );

noetherPrime = (R,X,I,G,U,V,homogeneous) -> (
     counter := 0; --counts the number of times lastCheck is called
     k := coefficientRing R;
     done := false;
     J = integralSet(G);
     Y = V - set J;
     f := map(R,R,reverse inverseSequence(U|Y|J,X));
     J = apply(J, i -> f(i));
     Y = apply(Y, i -> f(i));
     V = apply(V, i -> f(i)); --there might be a faster way to do this, perhaps V={x_(#U)..x_(#U+#V-1)}
     U = apply(U, i -> f(i)); -- might be faster to do U = {x_0..x_(#U-1)}
     while done == false do ( 
--	  G = gb f(I); --we should not need to do this gb computation
      	  --J := apply(integralSet(G),i -> f i); -- may need to do a gb comp.
	  U = apply(U, i -> i + random(k)*sum(Y)); --used to be V - set J
	  g := map(R,R,reverse(U|Y|J));
--	  g := map(R,R,U|V);
	  h = g*f;
	  if homogeneous then (
	       G = gb( h I, Algorithm => Homogeneous2);
	       );
	  if not homogeneous then G = gb h I;
	  done = lastCheck(X,G, #U);
	  counter = counter + 1;
	  if done then return((counter,gens G,h));
	  (U,V) = varPrep(X,G);
      	  );
     );

noetherNotPrime = (R,X,I,G,d,homogeneous) -> (
     counter := 0; --counts the number of times lastCheck is called
     k := coefficientRing R;
     S := maxAlgPerm(R,X,G,d,{});
     np := map(R,R,(X-set(S)|S));
     I = np I;
     G = gb I;
     (U,V) := varPrep(X,G);
     f := map(R,R,reverse inverseSequence(U|V,X)); --can build the reverse into inverseSequence by doing {X_j}|N
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
	  if homogeneous then (
	       G = gb( h I, Algorithm => Homogeneous2);
	       );
	  if not homogeneous then G = gb h I;
	  done = lastCheck(X,G, #U);
	  counter = counter + 1;
	  if done then return((counter,gens G,h));
	  (U,V) = varPrep(X,G);
      	  );
     );


noetherNormalization = method()
noetherNormalization(Ideal) := Sequence => (I) -> (
     R := ring I;
     homogeneous := (isHomogeneous(R) and isHomogeneous(I));
     if homogeneous then G = gb(I, Algorithm => Homogeneous2);
     if not homogeneous then G = gb I;
     d := dim I;
     X := sort gens R;
     (U,V) := varPrep(X,G);
     if d == #U then noetherPrime(R,X,I,G,U,V,homogeneous) else noetherNotPrime(R,X,I,G,d,homogeneous)
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
xy(x+y)
(xy-1)(x+y)
x^2*y+x*y^2+1

R = ZZ/2[x,y, MonomialOrder => Lex];
I = ideal((x^2*y+x*y^2+1))
noetherNormalization I

--Examples:
clearAll
installPackage "NoetherNormalization"
methods noetherNormalization

--Homogeneous example
R = QQ[x_5,x_4,x_3,x_2,x_1, MonomialOrder => Lex];
I = ideal(x_2^2+x_1*x_2+x_5^2, x_1*x_2*x_3*x_4+x_5^4);
noetherNormalization I
isHomogeneous I


--Ex#1
R = QQ[x_4,x_3,x_2,x_1, MonomialOrder => Lex]; --the same ordering as in the paper
I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);
noetherNormalization(I)
benchmark "noetherNormalization(I)"
q:= x_4^2+x_3^5+x_2*x_1
leadMonomial(q)
--alg dependent vars, ideal, map

          p       s
I < k[x] <= k[y] <- k[t]
            J<
	    
we take I we currently return p^-1, we want p,s,J
don't compute the inverse asking for it. 


--Examples of not so good I
--Ex#2
R = QQ[x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1^3 + x_1*x_2, x_2^3-x_4+x_3, x_1^2*x_2+x_1*x_2^2)
noetherNormalization I
G = gb I
d = dim I
X = sort gens R -- note that this "sort" is very important
varPrep(X,G)
np = maxAlgPerm(R,X,G,d)
maxAlgPermC(R,X,G,d)
G = gb np I
(U,V) = varPrep(X,G)
noetherNormalization I


--Ex#3
R = QQ[x_1,x_2,x_3,MonomialOrder => Lex]
I = ideal(x_1*x_2,x_1*x_3)
noetherNormalization I
G = gb I
gens G
d = dim I
X = sort gens R -- note that this "sort" is very important
np = maxAlgPerm(R,X,G,d)
maxAlgPermC(R,X,G,d)


varPrep(X,G)
noetherNormalization(I)

--Ex#4
R = QQ[x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1*x_2, x_1*x_3)
noetherNormalization I
G = gb I
gens G
d = dim I
X = sort gens R -- note that this "sort" is very important
varPrep(X,G)
np = maxAlgPerm(R,X,G,d)
maxAlgPermC(R,X,G,d)
maxAlgPermB(R,X,G,d,{})


--Ex#5
R = QQ[x_1..x_6,MonomialOrder => Lex]
R = QQ[x_6,x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1*x_2,x_1*x_3, x_2*x_3,x_2*x_4,x_2*x_5,x_3*x_4,x_3*x_5,x_4*x_5, x_4*x_6, x_5*x_6)
G = gb I
d = dim I
X = sort gens R -- note that this "sort" is very important
varPrep(X,G)
np = maxAlgPerm(R,X,G,d)
G = gb np I
(U,V) = varPrep(X,G)
noetherNormalization I
x_5<x_4

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


