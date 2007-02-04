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

--=========================================================================
--Methods: integralSet, varPrep
--=========================================================================
--The method integralSet - computes and returns J as in the paper
integralSet = method();
integralSet(GroebnerBasis) := List => (G) -> (
     J = {};
     for i from 0 to numgens source gens G - 1 do ( -- check the gens of G to see if their leadMonomial is in a single variable
     	  if # support leadMonomial (gens G)_(0,i) === 1 then J = J | {support leadMonomial (gens G)_(0,i)} --checks how many vars are in the lead
     	  );
     J = unique flatten J; --note that according to the algorithm J is a set of integers (in fact indices), we choose to return the variables
     return J);
--=========================================================================
--The method varPrep - computes and returns T_p and it's complement as in the paper
--we will beautify this by using support on the polys and <= for subset on the resulting lists
--here's what we're going to do:
--replace "regex(toString X_j,toString((gens G)_{i})) =!= null" with "{X_j} <= support (gens g)_(0,i)"
varPrep = method();
varPrep(GroebnerBasis) := List => (G) -> (
     X := flatten entries vars ring G; -- doesn't work because variables are backwards
     X = reverse X;
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
     return {U,V});
--=======================================================================
clearAll

assert(#(varPrep gb p)_0 >= dim p) -- this is for us. Something is wrong if this is not the case.
if #(varPrep gb p)_0 > dim p then 
-- hmm  we need to decide the outputs.... I'll think about that.


--this is essentially the order the final program should go in

R = QQ[x_4,x_3,x_2,x_1, MonomialOrder => Lex] --the same ordering as in the paper
k = coefficientRing R
p = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1)-- experiments:
G = gb p -- so far so good
U = (varPrep G)_0 
V = (varPrep G)_1
X = U | V
f = map(R,R,reverse X)
G = gb f(p) --we should not need to do this gb computation
J = integralSet(G)
V = apply(V, i -> f(i)) --there might be a faster way to do this, perhaps V={x_(#U)..x_(#U+#V-1)}
U = apply(U, i -> f(i)) -- might be faster to do U = {x_0..x_(#U-1)}
U = apply(U, i -> i + sum(V - set J)) --m2 magic. make sure V and J jive so that this makes sense, also in later version multiply the sum by a random in k
g = map(R,R,reverse(U|V))
gens gb g f p --What should we return in the final program? An ideal in the proper position? a variable transformation? both? what does singular give?
-- still need to build in a check to see if this is the basis is correct, ie. step 6 of the paper.
apply(x_1..x_4, i -> g f i)


--this idea, though not written out all of the way yet will allow us to put in different random numbers if we want to, but it's probably slower
--for s from 0 to #V-1 do (
--     if not {V#s} <= J then U = U + {pro

V = (varPrep G)_1
U = (varPrep G)_0
--reverse((U + {promote(random k,R)*last V, promote(random k, R)*last V})|V)
--g = map(R,R,(U + {promote(random k,R)*last V, promote(random k, R)*last V})|V)
g = map(R,R,(((U + {last V, last V})|V)))
--g = map(R,R,(U + {last V, last V})|V)
g x_1

gens gb g f p

---------------------------------------------------
-- here is the singular code for the example above:
ring R=0,(x_4,x_3,x_2,x_1),lp;
ideal I = x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1;
noetherNormal(I);
---------------------------------------------------

--=========================================================================--
	
	
--=========================================================================--

--code

--=========================================================================--

beginDocumentation() -- the start of the documentation

-----------------------------------------------------------------------------

--docs

--=========================================================================--


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

--=========================================
-- Nat's testing area/demolition zone
--=========================================
clearAll
R = QQ[x_4,x_3,x_2,x_1, MonomialOrder => Lex] --the same ordering as in the paper
p = ideal(x_3^2+x_1*x_3-x_4*x_3+1, (x_1-x_4)*(x_3)*(x_2-x_4)*(x_4)+1) --the transformed prime ideal
p = ideal(x_3^2+(x_1-x_4)*x_3+1, (x_1-x_4)*(x_2-x_4)*x_4-x_3-(x_1-x_4)) -- the transformed gb, these should be equal as ideals

p = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1) -- untransformed prime

G = gb p
gens G -- finally! correct output

f = map(R,R,{x_4,x_2,x_3,x_1})
g = map(R,R,{x_4,x_3,x_2+x_4,x_1+x_4})
h = map(R,R,{x_4,x_2+x_4,x_3,x_1+x_4})
gens gb g f(p)
gens gb h p
















--=========================================

--lexicographic groebner basis
--do we need to write our monomials in backwards? look for a good fix. 

clearAll
R = QQ[x_4, x_3, x_2, x_1, MonomialOrder => Lex] -- trying to make this look like the example
k = coefficientRing R
p = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1)
G = gb p -- so far so good
gens G
X = flatten entries vars ring G
-- now we'll define our varPrep function, a function like Tp in the paper -- how do we hide this from the users?
-- Goal: Simplify this code - can we get rid of "isempty"? 
-- Also check this with more examples!
--
--
--
varPrep = method();
varPrep(GroebnerBasis) := List => (G) -> (
     X := flatten entries vars ring G; -- doesn't work because variables are backwards
     X = reverse X;
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
     return {U,V});
X = varPrep(G) -- note varPrep is basically Tp together with the first part of step 2 of the algorithm
-- what we might really want is varPrep to produce a list that will give the correct new monomial ordering
-- you can construct this list in a similar way to how X is constructed



f = map(R,R,{x_4,x_2,x_3,x_1})
G = gb f(p) -- now we put the gb in the correct ring
gens G -- check it out.

J = {}
for i from 0 to numgens source gens G - 1 do ( -- check the gens of G to see if their leadMonomial is in a single variable
     if # support leadMonomial (gens G)_(0,i) === 1 then J = J | {support leadMonomial (gens G)_(0,i)} --checks how many vars are in the lead
     );
J = unique flatten J
-- note according to the algorithm, J is not x_3 but infact 3.





-- now for step 4:
-- this is written much more complicated than it has to be. 
-- Note that we may start with an identity matrix.
--We do need to know how many elements are in "U" above though

M = mutableMatrix id_(R^(numgens R))
M = rowPermute(M,0,{0,2,1,3})

U = {x_1, x_3} -- note at this point the ring will be wrong.
Mtemp = mutableMatrix id_(R^(numgens R))
for i from 0 to #U - 1 do(
     Mtemp_(i,numgens R - 1) = promote(random k,R);
     )


V = {x_2,x_4}
U = {x_1, x_3} -- note at this point the ring will be wrong.
reverse((U + {promote(random k,R)*last V, promote(random k, R)*last V})|V)
g = map(R,R,(U + {promote(random k,R)*last V, promote(random k, R)*last V})|V)
reverse ((U + {last V, last V})|V)
g = map(R,R,reverse((U + {last V, last V})|V))

gens gb g(p)

g = map(R,R,matrix{{x_1 + Mtemp_(0,numgens R-1)*x_4,x_3 + Mtemp_(1,numgens R-1)*x_4,x_2,x_4}})
M = matrix Mtemp * matrix M
M^(-1)
matrix{X}
flatten entries transpose( M^(-1)*(transpose matrix{reverse flatten entries vars ring G}))

 Mtemp_(1,numgens A-1)

R = QQ[y_1..y_4]




--=========================================

--do we need to write our monomials in backwards? look for a good fix. 

clearAll
R = QQ[x_1..x_4, MonomialOrder => Lex] -- trying to make this look like the example
k = coefficientRing R
p = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1)
G = gb p -- so far so good
flatten entries vars ring G
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
     return {U,V});
X = varPrep(G) -- note varPrep is basically Tp together with the first part of step 2 of the algorithm
-- what we might really want is varPrep to produce a list that will give the correct new monomial ordering
-- you can construct this list in a similar way to how X is constructed

f = map(R,R,matrix{{x_1,x_3,x_2,x_4}})
G = gb f(p) -- now we put the gb in the correct ring
gens G

J = {}
for i from 0 to numgens source gens G - 1 do ( -- check the gens of G to see if their leadMonomial is in a single variable
     if # support leadMonomial (gens G)_(0,i) === 1 then J = J | {support leadMonomial (gens G)_(0,i)} --checks how many vars are in the lead
     );
J = unique flatten J
-- note according to the algorithm, J is not x_3 but infact 3.



-- now for step 4:
-- this is written much more complicated than it has to be. 
-- Note that we may start with an identity matrix.
--We do need to know how many elements are in "U" above though

U = {x_1, x_3} -- note at this point the ring will be wrong.
Mtemp = mutableMatrix id_(A^(numgens A))
for i from 0 to #U - 1 do(
     Mtemp_(i,numgens A - 1) = promote(random k,A);
     )
-- ok we've made our matrix (A2 in the paper). Now what we need to do is a change of variables again.
-- to do this, I don't want to follow the algoritm given. Instead, I want to pull the coeffs
-- directly from Mtemp.
-- on second thought, maybe making these matricies is a waste of time.

Mtemp
g = map(A,A,matrix{{x_1 + Mtemp_(0,numgens A-1)*x_4,x_3 + Mtemp_(1,numgens A-1)*x_4,x_2,x_4}})
g gens G
gens G

k[x_1+x_2]
f(p)

matrix Mtemp * matrix M

 Mtemp_(1,numgens A-1)

R = QQ[y_1..y_4]