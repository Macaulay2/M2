-*
   Copyright 2014, Thomas Hawes.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

-* Functions not ported:

o 'generateGroup' applied to a List of matrices is replaced by
  'group' applied to FiniteGroupAction. The new function uses
  schreierGraph which is more efficient than brute-force.
o a 'reynoldsOperator' was written separately for the new types.
o 'OrderBound' optional argument is not ported. It would need to
  be reimplemented into our group generation function.
o 'IsGroup' optional argument is not ported because our methods
  compute the group in advance.

*-

------------------------------------------------
-- molienSeries
-- Calculates the Molien (Hilbert) series of the invariant ring of a finite 
-- group G.
-- Considers two cases: when the field the matrices are defined over is QQ 
-- and when it is a general number field.
-- This is to get around the fact that frac for number fields is not yet
-- implemented in Macaulay2
-- The author thanks an anonymous referee for suggested improvements to this 
-- method to ensure a consistent class of polynomial is returned 
-- Reference:
-- [S08] Sturmfels, B. "Algorithms in Invariant Theory". 2nd Edition. Wien New
-- York: Springer-Verlag, 2008.        
------------------------------------------------

-* Porting notes on molienSeries:

o Applies to FiniteGroupAction rather than RingOfInvariants
o Original code distinguished cases when coefficient ring K is QQ
  from other fields because frac(K[U]) is not implemented when
  K is a finite field extension of QQ created as quotient ring.
  New code treats all fields as the most general one.
o Fred suggests caching the result of molienSeries in the
  FiniteGroupAction because 1) it is computed independently of the
  ring of invariants, and 2) it is called upon by the methods for
  primary and secondary invariants.
o FUTURE TODO: extend to pos char in the non modular case

*-

molienSeries = method()

molienSeries FiniteGroupAction := { } >> opts -> (cacheValue (symbol molienSeries)) (G -> runHooks(FiniteGroupAction, symbol molienSeries, G) )

addHook(FiniteGroupAction, symbol molienSeries, G -> break (
--molienSeries FiniteGroupAction:= G -> (
     K:=coefficientRing ring G;
     if(isField K == false or char K =!= 0) then(
	  error "Action matrices must be defined over a field of characteristic zero"
	  );
     R:= ring G;
     if unique degrees R =!= {{1}} then
     error "Only implemented for standard graded polynomial rings";
     U:=symbol U;
     -- frac of a general number field is not implemented yet in Macaulay2. So
     -- need to calculate fractions and sum of fractions 'manually'. 
     n:= numgens R;
     Ku:= K[U]; 
     In:= id_(Ku^n);
     L:=apply(group G, M->(det(In-U*sub(M,Ku)))); 
     -- L is a list {p_1(U),...,p_m(U)}, say, (with m=#G), of reciprocals of 
     -- summands in the Molien series expansion.
     numerat:=sum(#L,i->product(drop(L,{i,i})));  
     -- in above notation, (and using LaTeX syntax):
     -- numerat=numerator of \frac{1}{p_1(U)}+...+\frac{1}{p_m(U)}.
     denominat:=#(group G)*product(L);
     A:=degreesRing(1);
     -- A is the ring containing the numerator and denominator of the
     -- Hilbert series
     T:= first gens A; -- T is the variable of the Hilbert series	  
     f:=map(A,Ku,{T}); -- Ring map K[U] -> A that sends U to T
     -- Note that any non-integer elements of K would be sent to zero.  
     h:=new Divide from {f(numerat),factor(f(denominat))};
     -- FG 6/17/2020, this code attempts a factorization
     -- of the denominator of the Molien series that prioritizes
     -- factors of the form (1-T^i)
     -- if this is not desired, uncomment the next line
     -- return reduceHilbert h;
     d := first degree denominat;
     h = reduceHilbert h;
     den := value(h#1);
     factors := {}; -- list of factors in the denominator
     while d>0 do (
	 e := 0;
	 while (den % (1-T^d) == 0) do (
	     e = e+1;
	     den = den // (1-T^d);
	     );
	 if e != 0 then (
	     factors = {new Power from {1-T^d,e}} | factors;
	     );
	 d = d-1;
	 );
     new Divide from {h#0,new Product from factors}
     ))

------------------------------------------------
-- primaryInvariants
-- Given a group and a polynomial ring over a number field, computes primary 
-- invariants for the invariant ring
-- Uses the method optimalHSOP by default to do this, or dadeHSOP if optional 
-- argument Dade is set to true
------------------------------------------------

-* Porting notes on primaryInvariants

o applies to FiniteGroupAction

*-
   
primaryInvariants=method(Options=>{Dade=>false, DegreeVector=>0})
primaryInvariants(FiniteGroupAction):=o->G->( 
     -- R - polynomial ring over a number field; 
     R := ring G;
     -- V2.0 note: the optimal HSOP method only works with standard grading
     -- It might be possible to make the Dade algorithm work with other
     -- gradings but this will require further consideration.
     -- For now, we throw an error if not in the standard graded case.
     if unique degrees R =!= {{1}} then
     error "Only implemented for standard graded polynomial rings";
     n:=numgens R;
     KK:=coefficientRing R;
     if(isField KK == false or (o.Dade==false and char KK =!= 0)) then(
	  error "The polynomial ring must be defined over a field of 
	  characteristic zero"
	  );
     if o.Dade==true then(
	  if(
	       instance(KK,GaloisField) or (
		    instance(KK,QuotientRing) and (ambient KK===ZZ)
		    )==true
	       ) 
	  then(
	       if KK.order<=((#G)^(n-1)) then(
		    s:=read "Warning, your field may be too small for the Dade
algorithm to terminate successfully. Are you sure you want
to continue? (Enter `y' to continue, `n' to stop execution
of function.): ";
		    if s=!="y" then error "Use a bigger field."
	       	    )
	       );
     	  if(
	       instance(KK,FractionField) and 
	       (char KK=!=0) and 
	       (dim(last(KK.baseRings))==0)
	       )==true then error(
		    "The ground field should be a field of characteristic zero
	       	    or a large finite field of type GaloisField or 
		    QuotientRing."
	       );
     return dadeHSOP G
     );
     if o.DegreeVector=!=0 then(
	  return optimalHSOP(G,DegreeVector=>(o.DegreeVector)) 
	  )
     else return optimalHSOP G
     ); 


------------------------------------------------
-- secondaryInvariants
-- Given a list of primary invariants for the invariant ring of a group, finds 
-- secondary invariants
------------------------------------------------

-* Porting notes on secondaryInvariants

o applies to a list of primary invariants + FiniteGroupAction

*-
   
secondaryInvariants=method(Options=>{PrintDegreePolynomial=>false});
secondaryInvariants(List,FiniteGroupAction):=o->(P,G)->(
     -- P - List of primary invariants, G - FiniteGroupAction
     R := ring G;
     if unique degrees R =!= {{1}} then
     error "Only implemented for standard graded polynomial rings";
     n:=numgens R;
     KK:=coefficientRing R;
     if(isField KK == false or char KK =!= 0) then(
	  error "Expected action over a field of characteristic zero"
	  );
     for g in P do(if ring(g) =!= R then(
	       error "All polynomials must be in the ring on which the group acts"
	       )
	  );
     molnum:=value numerator molienSeries G; 
     -- numerator of Molien series H(G,T)
     moldenom:=value denominator molienSeries G;
     -- denominator of Molien series H(G,T)
     A:=ring moldenom; 
     -- the ring H(G,T) lives in. Note T is invertible in this ring.
     t:=symbol t; B:=ZZ[t]; -- note t does not have an inverse in B
     F:=map(B,A,{t}); 
     -- F sends the variable T in A to t in B
     -- Note that this will send negative powers of T to zero
     -- This does not matter, because molnum and moldenom are concentrated in
     -- non-negative degrees
     pol:=(F(molnum)*(product apply(P,f->(1-t^((degree f)_0)))))//(F(moldenom)); 
     -- pol = H(G,t)*(1-t^{d_1})*...*(1-t^{d_n}) where d_i are degrees of
     -- primary invariants in P
     -- pol is used to calculate the number of secondary invariants of 
     -- certain degrees. 
     degscoeffs:=apply(
	  apply(
	       flatten entries ((coefficients pol)#0),degree
	       ),
	  D->{D,coefficient((sub(t,ZZ[t]))^(D_0),pol)}
	  );
     -- outputs a list of lists of the form {{d},c} where d is a featuring 
     -- degree in pol and c is the corresponding coefficient. Thus there 
     -- are c secondary invariants of degree d.
     I:=ideal P;
     normforms:=new MutableHashTable;
     secondary:=new MutableHashTable;
     for dc in degscoeffs do(
	  d:=(dc#0)#0;
	  bas:=basis(d,R); 
	  -- For this to work polynomials in P must have coefficients in a field 
	  -- (and Macaulay 2 know it is a field)
	  mons:=flatten entries bas; 
	  -- List of monomials of R of degree d 
	  bas2:=sub(basis(d,R/I),R); 
	  -- lifts a basis of degree d graded piece of R/I to elements of R
	  i:=1;
	  B:=matrix{{}};
	  for m in mons do(v:=(reynoldsOperator(m,G));
	      if((coefficientRing R)===QQ and v=!=sub(0,R)) then(
		   v=lcm(
			apply(
			     flatten entries transpose((coefficients(v))#1),
			     l->(denominator(sub(l,QQ)))
			     )
			)*v;
		   v=(1/gcd(
			     apply(
				  flatten entries transpose((coefficients v)#1),
				  l->sub(l,ZZ)
				  )
			     )
			)*v
		   ); 
	      -- If coefficient ring of R is QQ and v nonzero, prune v to get 
	      -- rid of fractions with denominators and leave coprime integer 
	      -- coefficients. 
	      nfv:=(v % I); -- find normal form of reynoldsOperator(m,G) mod I
	      if nfv==0 then continue else( 
		   -- if zero, discard and go to next monomial
	      	     if i==1 then(
			  normforms#(d,i)=nfv; 
			  secondary#(d,i)=v; 
			  B=((coefficients(nfv,Monomials=>bas2))#1); 
			  i=i+1
			  -- if nfv is first non zero normal form, then store 
			  -- in normforms, store reynoldsOperator(m,G) in 
			  -- secondary, add coefficients of nfv to B and 
			  -- do i=i+1
			  ) else(   
			  A:=B|((coefficients(nfv,Monomials=>bas2))#1); 
			  -- add coefficients of nfv to matrix of coefficients 
			  -- of stored normal forms
			  if rank(A)==i then(
			       B=B|((coefficients(nfv,Monomials=>bas2))#1); 
			       normforms#(d,i)=nfv; 
			       secondary#(d,i)=v; 
			       i=i+1
			       ) else continue 
			  -- a linear independence test. If nfv is linearly 
			  -- dependent on previously stored normal forms, then 
			  -- discard v. Else store v with the other secondary 
			  -- invariants found. 
			  );
		     );
		if i==(1+(dc#1)) then break() 
		-- break loop once c secondary invariants of degree d 
		-- have been found
		);
	  );
     if o.PrintDegreePolynomial == true then print pol;
     return sort values secondary
     );

------------------------------------------------
-- invariantRing
-- Encorportates the primaryInvariants and secondaryInvariants methods into 
-- one method.
------------------------------------------------

-* Porting notes on hironakaDecomposition

o this function is renamed from invariantRing to
  hironakaDecomposition to provide consistent outputs for the
  invariantRing method
o applies to a FiniteGroupAction, although would it be better
  if it applied to the ring of invariants?

*-
   
hironakaDecomposition=method(
     Options=>{
	  DegreeVector=>0,
	  PrintDegreePolynomial=>false
	  }
     );
hironakaDecomposition(FiniteGroupAction):=o->G->(
     L:=G;
--     if o.IsGroup==false then L=generateGroup(G,coefficientRing R);
     P:=primaryInvariants(G,DegreeVector=>(o.DegreeVector));
     S:=secondaryInvariants(P,G,PrintDegreePolynomial=>(
	       o.PrintDegreePolynomial
	       )
	  ); 
     return((P,S))
     );
     
------------------------------------------------
-- FUNCTIONS NOT EXPORTED --
-- (Can be viewed via listLocalSymbols command)
------------------------------------------------

-* Porting notes on unexported functions

o The functions groupAct, cart, vectors, isNonDecreasing,
  degreeSequencesGivenProduct, nextBest, tryDegrees, optimalHSOP,
  dadeHSOP below keep the original code. They have been minimally
  modified to accept objects of type FiniteGroupAction as input.

*-

------------------------------------------------
-- groupAct
-- Outputs a list of polynomials, one element for each application of a member
-- of G to f.
-- Innermost apply computes a matrix: transpose of A*(variables in ring f).
-- Outermost apply evaluates f at these linear forms.
------------------------------------------------

groupAct=(f,G)->(
     apply(
	  apply(
	       G, A->((vars ring f)*(transpose sub(A,ring f)))
	       ),T->sub(f,T)
	  )
     ); 

------------------------------------------------
-- cart
-- Takes two lists and outputs the Cartesian product of them as a list of lists
-- (the inner lists are the tuples).
-- E.g. ({1,2},{a,b})->{{1,a},{1,b},{2,a},{2,b}}
------------------------------------------------

cart=(A,B)->(
     flatten(for u in A list(for v in B list(flatten({u,v}))))
     );

------------------------------------------------
-- vectors
-- Inputs: n,k,i non-negative integers, with k<=i
-- Produces all n-tuples of natural numbers with total degrees from k to
-- i. Outputs a list of lists (vectors).
------------------------------------------------

vectors=(n,k,i)->(
     x:=symbol x;
     L:=ZZ[x_0..x_(n-1),MonomialOrder=>GLex];
     out:=flatten flatten( 
	  for j from k to i list( 
	  apply(flatten entries basis(j,L),exponents)
	  )
     );
     return out
     );

------------------------------------------------
-- nextBest
-- A 'next-best' function (see Kemper, G. An Algorithm to Compute Optimal
-- Homogeneous Systems of Parameters. Journal of Symbolic
-- Computation. 27. (1999) (hereon denoted by [kem99])). 
-- Takes as input a list of positive integers {d_1,...,d_n}, with sum s and
-- product d, and outputs a list of positive integers {e_1,...,e_n}, with
-- e_1*...*e_n = d+1, or 
-- e_1*...*e_n = d and e_1+...+e_n>s, or
-- e_1*...*e_n = d and e_1+...+e_n=s and {e_1,...,e_n}>{d_1,...,d_n}
-- (lexicographically).
-- This is the 'next-best function' suggested at the end of section 2 in 
-- [kem99]. 
-- nextBest uses auxiliary functions isNonDecreasing and 
-- degreeSequencesGivenProduct.
-- The author thanks an anonymous referee for suggesting the following 
-- implementation.  
------------------------------------------------

-- isNonDecreasing takes a list L and returns true if the list is (weakly)
-- increasing, false otherwise.

isNonDecreasing=L->(
	  for i to #L-2 do(
	       if L_i>L_(i+1) then return false;
	       );
	  return true; 
	  );

-- degreeSequencesGivenProduct takes a pair of positive integers (n,d) and
-- outputs a list L of lists of positive integers {d_1,...,d_n} with
-- product({d_1,...,d_n})=d. The elements of L are ordered first according to
-- sum of entries (lowest to highest), then lexicographically.
     
degreeSequencesGivenProduct = (n,d)->(
	  if isPrime(d) then(
	       div:={1,d};
	       )
	  else(
	       div=for i from 1 to d list(
		    if d%i==0 then i else continue
		    );
	       ); -- creates div - a list of divisors of d (including 1 and d)
	  L:=pack(1,div); -- L will ultimately be list of lists l of n elements   
	  --  with product(l)=d
	  for i from 2 to n do(
	       L= flatten for l in L list(
		    q:=d//product(l);
		    div2:=select(div,x->q%x==0); -- div2 consists of divisors of
		    -- d that divide q=d//product(l) 
		    select(apply(div2,x->prepend(x,l)),u->isNonDecreasing u)
		    -- To each element of div2, prepend to l and keep only if
		    -- resulting list is (weakly) increasing. 
		    ); 
	       );
	  L=select(L,l->product(l)==d); -- discard lists l with
	  -- product(l) not equal to d
	  S:=partition(sum,L); -- S is a hashtable, with integer keys
	  -- s, and s=>all lists l with sum(l)=s
	  L=flatten for s in sort keys S list(
	       sort S#s
	       ); -- sort the l in L first by increasing sum, then  
	  -- lexicographically
	  return L 
	  );

nextBest=vv->(
	  -- input is a list vv of non-negative integers
	  if instance(vv,List)==false then(
	       error "Input for nextBest must be a List of non-negative
	  integers"
	  );
     	  for i in vv do(
	       if instance(i,ZZ)==false then(
			 error "Input for nextBest must be a List of
	  non-negative integers"
	       );
	       if i<=0 then(
			 error "Input for nextBest must be a List of
	  non-negative integers"
	       );
          );
          v:=sort vv;
	  n:=#v;
	  d:=product v;
	  degseqs:= degreeSequencesGivenProduct(n,d);
	  j:=position(degseqs,x->x==v); -- find position of v in the list of
	  -- degree sequences degseqs
	  if j==(#degseqs-1) then(
	       return first degreeSequencesGivenProduct(n,d+1);
	       -- If v is the last element of degseqs, then take the smallest degree
	       -- sequence with product d+1... 
	       )
	  else(
	       return degseqs_(j+1); -- ... otherwise take the next degree
	       -- sequence in the list degseqs
	       )
	  );
     
------------------------------------------------
-- tryDegrees (Non-exported function)
-- The 'TryDegrees' function from [kem99].
-- Variable names mostly follow those in the TryDegrees algorithm in [kem99]. 
-- Is used to try and find an homogeneous system of parameters for the invariant
-- ring with the degrees input.
-- tryDegrees is the main computational process in the primaryInvariants method. 
------------------------------------------------

tryDegrees=method();
 
tryDegrees(FiniteGroupAction,List):=(H,Li)->(
     A := ring H;
     nn := numgens A;
     -- A - polynomial ring; H - FiniteGroupAction; nn - natural number, typically
     -- dimension of A; Li - list of degrees  
     Bases:=new MutableHashTable; 
     -- stores spanning sets for the vector spaces (A^H)_l, where l is a degree
     -- from a degree vector at various iterations of the computation method
     computation:=(I,G,n,L)->( 
	  -- 'computation' is the actual implementation of the TryDegrees 
	  -- algorithm in [kem99] on the quotient ring A/I   
          k:=0; m:=#L; V:={sub(0,A)};    
          if m>0 then(
	       for l in L do(
		    Bases#(n,l)=toList set apply(
			 flatten entries basis(l,A),m->(reynoldsOperator(m,G))
			 )
		    ); -- assigns to degree l a spanning set for (A^G)_l
	       V=(Bases#(n,(L#0))) -- V is the homogeneous piece of A^G 
	       -- corresponding to the first degree in the degree vector L 
	       ); 
          if V=!={sub(0,A)} then( 
	       -- only do the 'for f_1 \in V\0' part of loop in [kem99] if 
	       -- V\0 is nonempty
	       V0:=delete(sub(0,A),V); breakfloop:=false; d:=1; 
	       -- d is the degree of the 'coefficient vector'   
	       while breakfloop==false do(
	            for v in apply(
			 vectors(#V0,d,d),u->(apply(u,l->(sub(l,A))))
			 ) do( -- cycles through all vectors with degree d 
			 -- 'coefficient vector'. This gives a way to 
			 -- systematically try elements from the vector space 
			 -- spanned by elements of V0. 
			 -- Referring to p178, bullet point (a) of [Kem99], we 
			 -- have chosen to include the natural numbers in the 
			 -- ground field of A (which contains QQ) in the natural 
			 -- way, since there seems to be no advantage to 
			 -- choosing a different embedding. 
		         f:=(((matrix{v})*transpose(matrix{V0}))_(0,0)); 
			 -- the f_1 in [kem99]
			 nextf:=false;
		         if k>0 then( -- beginning of k>0 condition in [kem99] 
			      for M in(subsets toList(1..(k-1))) do(
			           union:=flatten apply(M,j->(Bases#(n,(L#j))));  
			           if dim(A/(I+ideal({f}|union)))>(n-(#M)-1) 
				   then(
					break(nextf=true)
					)
			           );
			      ); -- end of k>0 condition in [kem99]
		         if nextf==true then continue;
		         R:=computation(I+ideal(f),G,n-1,drop(L,1)); 
			 -- 'R' is same notation as in [kem99]
		         if instance(R,List)==true then return({f}|R);
		         if R>=k then( -- beginning of the 'if R>=k then' loop 
			      -- in [kem99]
			      k=R+1;
			      for M in(subsets toList(0..(k-1))) do(
			           union:=flatten apply(M,j->(Bases#(n,(L#j))));
				   -- corresponds to the union of homogeneous 
				   -- pieces of A that appears in the algorithm
				   -- in [kem99]
				   if dim(A/(I+promote(ideal(union),A)))>(n-(#M)) then(
					break(breakfloop=true)
					)
			           );
			      -- end of the 'if R>=k then' loop in [kem99]
			      );
		         if breakfloop==true then break
		         ); -- here is where the content of the 'for f_1 \in
			 -- V\0' 
			 -- in [kem99] loop ends
	            d=d+1;
	            ); -- here is where the 'for f_1 \in V\0' loop ACTUALLY ends 
	       ); 
          if k<=1 then(if dim(A/I)>n then k=0 else k=1); 
	  -- the 'if k<=1' loop near end on [kem99] algorithm
          if(m==0 and k==1) then k={}; 
          return k
          );
     return computation(ideal(sub(0,A)),H,nn,Li)
     );

------------------------------------------------
-- optimalHSOP (Non-exported function)
-- The 'HomogeneousParameterSystem' function from [kem99]
-- Uses the nextBest method to cycle through degree vectors ordered according to
-- rising values of the product of the degrees
-- Performs certain tests on a candidate degree vector to see if an hsop with
-- the degrees can't possibly exist 
-- Then applies tryDegrees to each vector that passes these tests   
------------------------------------------------

optimalHSOP=method(Options=>{DegreeVector=>0}); 
-- DegreeVector can be set to be a list of degrees if it is known an hsop with 
-- these degrees exists. Bypasses the tests and applies tryDegrees to this list 
-- straight away  
optimalHSOP(FiniteGroupAction):= o->G->(
     R:=ring G;
     n:=numgens R;
     if(o.DegreeVector=!=0 and instance(o.DegreeVector,List)==false) then(
	  error "DegreeVector must be zero or a List consisting of positive
	  integers"
	  );
     if(o.DegreeVector=!=0 and (#(o.DegreeVector))>(numgens R)) then(
	  error "Length of DegreeVector must not exceed dimension of polynomial 
	  ring"
	  );
     L:=toList(n:1); -- L is list of degrees of primary invariants, initially
     -- set to {1,...,1}. This will be changed by using nextBest and then
     -- a set of primary invariants of degrees from L will be constructed
     -- (subject to L passing various tests). 
     hsopfound:=false;
     S:=0;
     if o.DegreeVector=!=0 then(
	  S=tryDegrees(G,o.DegreeVector);
          if instance(S,List)==true then hsopfound=true else(
	       error "There is no homogeneous system of parameters with degrees 
	       given in DegreeVector"
	       )
	  );
     molnum:=value numerator molienSeries G;  
     -- numerator of Molien series H(G,T)
     moldenom:=value denominator molienSeries G;  
     -- denominator of Molien series H(G,T)
     A:=ring moldenom;  
     -- the ring H(G,T) lives in. Note T is invertible in this ring.
     t:=symbol t; B:=ZZ[t]; 
     -- note t does not have an inverse in B
     F:=map(B,A,{t}); 
     -- F sends the variable T in A to t in B
     -- Note that this will send negative powers of T to zero
     -- This does not matter, because molnum and moldenom are concentrated in
     -- non-negative degrees 
     a:=0;
     while hsopfound==false do(  
	  -- What follows are tests on the candidate degrees in L
	  if ((product L) % (#(group G)))=!=0 then(
	       L=(nextBest L); 
	       continue;
	       ); 
	  -- order of group must divide product of the degrees
	  pol:=F(molnum)*(product apply(L,d->(1-t^d)));
	  if pol%(F(moldenom))=!=(sub(0,B)) then(
	       L=nextBest(L); 
	       continue;
	       ); 
	  -- pol=F(molnum)*(1-t^{d_1})*...*(1-t^{d_n}) with d_i in L
	  -- pol should be divisible by the denominator of H(G,t)=F(H(G,T)) 
	  S=tryDegrees(G,L);
          if instance(S,List)==true then hsopfound=true else(
	       L=(nextBest L);
	       )
	  -- tryDegrees returns either a list of invariants or a 
	  -- positive integer
	  );
     if (coefficientRing R)=!=QQ then return S; 
     SS:=for g in S list(
	  lcm(apply(
		    flatten entries transpose((coefficients(g))#1),l->(
			 denominator(sub(l,QQ))
			 )
		    )
	       )*g); 
     -- apply pruning to give integer coefficients if R is a polynomial 
     -- ring over QQ
     return SS
     );

------------------------------------------------
-- dadeHSOP (Non-exported function)
-- Finds an hsop for the invariant ring using the Dade algorithm. 
-- (See Sturmfels, B., Algorithms in Invariant Theory. 2nd ed. 
-- Wien New York: Springer-Verlag, 2008.)
-- Uses 'random' to produce candidate Dade basis vectors (DBVs) and tests 
-- whether this is so using linear dependence tests, so is quite simple and 
-- quick. 
------------------------------------------------

dadeHSOP=method();
dadeHSOP(FiniteGroupAction):= G-> ( 
     R := ring G;
     n:=numgens R;
     dadebasis:=new MutableHashTable; 
     -- integer keys, d=>d'th Dade basis vector  
     orbits:=new MutableHashTable; 
     -- integer keys, d=> orbit of d'th Dade basis vector
     tuples:=new MutableHashTable; 
     -- integer keys, d=>orb(v_0) x ... x orb(v_{d-1}), d=0...n-1
     d:=0;
     dadebasis#d=((random(R^1,R^{-1}))_(0,0)); -- make a random linear form
     -- V2.0 note: this way of making linear forms requires standard grading
     orbits#d=toList(set groupAct(dadebasis#d,group G));
     -- assigns to d the orbit of the d'th Dade basis vector  
     tuples#d=(cart((orbits#d),{{}})); -- the orbit of the 0th DBV in this case
     while d<(n-1) do(
	  basisvectorfound:=false;
	  while basisvectorfound==false do(
	       u:=((random(R^1,R^{-1}))_(0,0)); 
	       -- a candidate for the d+1'th DBV 
	       l:=0;
     	       while l<(#(tuples#d)) do( 
		    -- will cycle through the possible tuples of orbit elements
	       	    k:=1; B:=matrix{{}};
		    for ww in((tuples#d)#l) do(
	       		 if k==1 then(
			      B=(coefficients(ww,Monomials=>(vars R)))#1; k=k+1
			      ) else(
			      B=B|((coefficients(ww,Monomials=>(vars R)))#1); 
			      k=k+1
			      );
	       		 ); -- creates matrix of coefficients B
	  	    augB:=B|((coefficients(u,Monomials=>(vars R)))#1); 
		    -- creates the augmented matrix of B
	  	    if (rank augB)==(rank B) then break() else l=l+1 
		    -- rank test to see if u is linearly dependent on the linear 
		    -- forms from the tuple being considered
	  	    );
	       if l==(#(tuples#d)) then(
		    basisvectorfound=true; 
		    d=d+1; 
		    dadebasis#d=u
		    );  
	       ); 
	  -- if we successfully made it through all tuples, break loop,
	  -- increment d and take u to be our new DBV
	  orbits#d=groupAct(dadebasis#d,group G);
	  -- create the orbit of the new DBV
	  if #(keys dadebasis)<n then tuples#d=cart((orbits#d),tuples#(d-1)); 
	  -- create new set of tuples only if we still need DBVs
	  );    
     hsop:=sort apply(keys dadebasis,k->product orbits#k); 
     -- for each DBV, creates the product of all linear forms in the orbit 
     if (coefficientRing R)=!=QQ then return hsop;
     SS:=apply(
	  hsop,g->(
	       lcm(
		    apply(
			 flatten entries transpose((coefficients(g))#1),
			 l->(denominator(sub(l,QQ))))
		    )*g
	       )
	  );
     return apply(
	  SS,
	  g->(
	       (1/gcd(apply(flatten entries transpose((coefficients(g))#1),
			      l->sub(l,ZZ)))
		    )*g
	       )
	  ) 
      -- apply pruning to coefficients if ground field of R is QQ
     );
