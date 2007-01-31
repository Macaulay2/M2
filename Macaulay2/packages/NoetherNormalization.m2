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

--=========================================================================--
     
export{} -- if the new routines which you are adding have new
-- names, then they need to be exported; otherwise they should not be
-- exported
        
--=========================================================================--
-- experiments:

-- need good and extreme examples of NNL. 

clearAll
installPackage "NoetherNormalization"

B = ZZ/7[x,y]/(y^2)
k = coefficientRing B
A = k[flatten entries vars B]
dim(B) -- may be useful in constructing a for loop
V = ideal vars A
V = ideal vars B
G = ideal B -- identify monic or make monic elements
V = eliminate(y,V) -- remove y terms Does it only work for poly rings???


B = ZZ/7[x,y]/(x*y+x+2)
k = coefficientRing B
A = k[flatten entries vars B]
dim(B) -- may be useful in constructing a for loop
V = ideal vars A
G = ideal B -- identify monic or make monic elements
gens G
V = eliminate(y,V) -- r
help eliminate


--====================================
-- Logar's Algorithm
--====================================

-- This is a set of code investigating alg 3.5 (p266) of Logar's paper
clearAll
R = QQ[x_1..x_4, MonomialOrder => {Weights => {1,2,3,4}, Lex}] -- trying to make this look like the example
k = coefficientRing R
p = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1)
G = gb p -- so far so good
gens G
-- now we'll define our varPrep function, a function like Tp in the paper -- how do we hide this from the users?
-- Goal: Simplify this code - can we get rid of "isempty"? 
-- Also check this with more examples!
varPrep = method();
varPrep(GroebnerBasis) := List => (G) -> (
     X := flatten entries vars ring G;
     U := {};
     V := {};
     for j from 0 to numgens ring G - 1 do (
	  isempty := true;
     	  for i from 0 to numgens source gens G - 1 do ( -- going from zero to the number of gens of the gb - 1	   
	       if isempty == false then break;
	       if regex(toString X_j,toString((gens G)_{i})) =!= null then ( -- check to see if X_j is in the ith term, goto next X_j term 
	       	    if j == numgens ring G - 1 then (
	   	    	 isempty = false;
		    	 break);
	       	    for k from 1 to numgens ring G - j - 1 do (-- checking to see if no higher degree vars in the poly 
		    	 if regex(toString X_(j+k),toString((gens G)_{i})) =!= null then break; --if higher degree vars appear in the poly then break
	       	    	 if j+k === numgens ring G - 1 then isempty = false -- if we make it through all the higher degree vars and none of them are in the poly then the intersection is not empty
	       	    	 );
		    );
	       );
     	  if isempty == true then U = U | {X_j} --if the intersection is empty then add the var to the list
	  else V = V | {X_j};
     	  );
     return U|V);
X = varPrep(G) -- note varPrep is basically Tp together with the first part of step 2 of the algorithm
-- what we might really want is varPrep to produce a list that will give the correct new monomial ordering
-- you can construct this list in a similar way to how X is constructed



A = k[flatten entries vars R, MonomialOrder => {Weights => splice {1,3,2,4}, Lex}]
f = map(A,R,matrix{{x_1,x_3,x_2,x_4}})
G = gb f(p) -- now we put the gb in the correct ring
gens G -- check it out.

-- now an attempt to get A, which we should call M, as we already have A as a ring.
-- so for this to work, our varPrep algorithm should output 1 less than the var number.
-- *or* maybe we should start indexing our variables starting with 0.
M = mutableMatrix id_(A^(numgens A))
M = rowPermute(M,0,{0,2,1,3})




-- Now we need to figure out the lambda_i's

--lambda_i's need to be chosen so that none are the root of a specific polynomial that we should be able to work out.
--But first we need to figure out "J" which is the set of integral variables (see prop 3.1, 3.2), use support(leadMonomial(f))===x_i?


--This Computes J, have tested it against the example and it appears to do what we want

J = {}
for i from 0 to numgens source gens G - 1 do ( -- check the gens of G to see if their leadMonomial is in a single variable
     if # support leadMonomial (gens G)_(0,i) === 1 then J = J | {support leadMonomial (gens G)_(0,i)} --checks how many vars are in the lead
     );
J = unique flatten J
-- note according to the algorithm, J is not x_3 but infact 3.


M_(1,3) = x_1
M
ring x_1
ring M
-- now for step 4:
-- note that here we actually need the "U" part of Tp
-- I will continue on as if varPrep works right
-- in this case d = 2.
V = {x_1, x_3} -- note at this point the ring will be wrong.
Mtemp = mutableMatrix{{mutableMatrix id_(A^(#V)),0},{mutableMatrix id_(A^(3-#V))
     



--=========================================================================--
	
	
--=========================================================================--

--code

--=========================================================================--

beginDocumentation() -- the start of the documentation

-----------------------------------------------------------------------------

--docs

--=========================================================================--
