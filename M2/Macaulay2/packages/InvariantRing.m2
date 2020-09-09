-- -*- coding: utf-8 -*-

-- Last edited 10 October 2014

-*
   Copyright 2014, Thomas Hawes.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
   *-

--**********************************************
-- HEADER --
--**********************************************

-- I would like to thank Dr Diane Maclagan for suggesting that I write this
-- package as an undergraduate summer project in 2010 and for her supervision
-- during its development. 

-- I also thank an anonymous referee for suggesting improvements to the original
-- code that was submitted, and which is implemented in this version.
    
newPackage(
	"InvariantRing",
    	Version => "1.1.0", 
    	Date => "August 10, 2012",
    	Authors => {
	     {Name => "Thomas Hawes", Email => "thomas.hawes@maths.ox.ac.uk"}
	     },
    	Headline => "construct the invariant ring of a finite group",
	Keywords => {"Representation Theory", "Group Theory"},
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "Computing the invariant ring of a finite group",
	     "acceptance date" => "2013-05-16",
	     "published article URI" => "http://j-sag.org/Volume5/jsag-3-2013.pdf",
	     "published code URI" => "http://j-sag.org/Volume5/InvariantRing.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/InvariantRing.m2",
	     "release at publication" => "68f41d641fadb0a1054023432eb60177f1d7cbd9",
	     "version at publication" => "1.1.0",
	     "volume number" => "5",
	     "volume URI" => "http://j-sag.org/Volume5/"
	     },
	AuxiliaryFiles => false, 
	-- set to true if package comes with auxiliary files
    	DebuggingMode => false		 
	-- set to true only during development
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export{
     "generateGroup",
     "OrderBound",
     "reynoldsOperator",
     "molienSeries",
     "primaryInvariants",
     "DegreeVector",
     "Dade",
     "secondaryInvariants",
     "PrintDegreePolynomial",
     "invariantRing",
     "IsGroup"
     }


--**********************************************
-- BODY --
--**********************************************

------------------------------------------------
-- FUNCTIONS NOT EXPORTED --
-- (Can be viewed via listLocalSymbols command)
------------------------------------------------

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
-- EXPORTED FUNCTIONS (unless otherwise stated) --
------------------------------------------------

------------------------------------------------
-- generateGroup
-- Brute-force algorithm that creates a group of matrices, given generating 
-- matrices.
-- Outputs a list, the group of matrices defined over the field K generated by 
-- the list S
-- NB: not a sophiticated algorithm; tries all possible strings of increasing 
-- length until no new elements of group generated.
------------------------------------------------
 
generateGroup=method(Options=>{OrderBound=>100});
generateGroup(List,Ring):=o->(S,K)->(
     for M in S do(
	  if(
	     instance(M,Matrix)==false or rank M=!=numgens target M
	       ) 
	  then break(
	       error "The first argument must be a list of invertible matrices"
	       )
	  );
     if(isField K == false) then(
	  error "The second argument must be a field"
	  ); -- One must ensure the sencond argument is a field. Using 'toField' 
     -- means quotient rings that are in fact fields are recognised as such. 
     if(instance(o.OrderBound,ZZ)==false or o.OrderBound<1) then(
	  error "DegreeBound must be a positive integer"
	  );  
     L:=apply(S,m->(sub(m,K)));
     group:=L;
     groupfound:=false;
     while groupfound==false do(
	  A:=toList set group;
	  for g in L do(A=toList(set(A|apply(group,h->(g*h)))));
	  -- construct all products of two elements from 'group'   
	  if #A>o.OrderBound then(
	       error "The order of the group exceeds OrderBound"
	       );  
	  if set(A)===set(group) then groupfound=true else group=A;
	  -- If no new matrices created, then we have found a group
	  -- Otherwise, add all new matrices to 'group' and repeat 
	  -- loop. 
	  );
     return group
     );

------------------------------------------------
-- reynoldsOperator
-- Averages a ring element f over a group G (which is a List).
------------------------------------------------  

reynoldsOperator = method();
reynoldsOperator(RingElement,List):= (f,G)-> (
     if instance(ring f,PolynomialRing)==false then(
	       error "The first argument must be an element from a polynomial
	       ring in the same number of variables as the rank of the matrices
	       of the list in the second entry"
	       );
     for M in G do(
	  if instance(M,Matrix)==false then(
	       error "The second argument must be a list of matrices"
	       );
	  if rank M =!= numgens ring f then(
	       error "The first argument must be an element from a polynomial
	       ring in the same number of variables as the rank of the matrices
	       of the list in the second entry"
	       )   
	  );
     (1/#G)*(sum groupAct(f,G))
     );

------------------------------------------------
-- molienSeries
-- Calculates the Molien (Hilbert) series of the invariant ring of a finite 
-- group G.
-- Considers two cases: when the field the matrices are defined over is QQ 
-- and when it is a general number field.
-- This is to get around the fact that frac for number fields is not yet
-- implimented in Macaulay2
-- The author thanks an anonymous referee for suggested improvements to this 
-- method to ensure a consistent class of polynomial is returned 
-- Reference:
-- [S08] Sturmfels, B. "Algorithms in Invariant Theory". 2nd Edition. Wien New
-- York: Springer-Verlag, 2008.        
------------------------------------------------

molienSeries = method();
molienSeries List:= G -> (
     for M in G do(
	  if instance(M,Matrix)==false then(
	       error "The input must be a List of matrices defined over a
	       field"
	       )
	  );
     K:=ring(G#0); 
     -- set the field to be that which matrices in G are defined over.
     U:=symbol U;
     if(isField K == false or char K =!= 0) then(
	  error "Matrices must be defined over a field of characteristic zero"
	  );
     for M in G do(
	  if ring(M) =!= K then( 
	      error "all matrices in the List must be defined over the same
	      field"
	      );
	 );
	 -- since frac(QQ[T]) is understood in Macaulay2, calculating the Molien
	 -- series when K=QQ is easy.
     if K===QQ then(   
	  n:= numgens target(G#0);
     	  Ku:= frac(K[U]);
     	  In:= matrix mutableIdentity(Ku,n);
     	  s:= (1/#G)*(sum apply(G, M -> (det(In-U*sub(M,Ku)))^(-1)));
	  -- s is the Hilbert (Molien) series of the ring of invariants,
	  -- computed using Molien's formula (see [S08]).
	  numerat:= numerator s; 
	  denominat:= denominator s; 
     	  )
     -- frac of a general number field is not implemented yet in Macaulay2. So
     -- need to calculate fractions and sum of fractions 'manually'. 
     else(  
     	  n= numgens target(G#0);
     	  Ku= K[U]; 
     	  In= matrix mutableIdentity(Ku,n);
     	  L:=apply(G, M->(det(In-U*sub(M,Ku)))); 
	  -- L is a list {p_1(U),...,p_m(U)}, say, (with m=#G), of reciprocals of 
	  -- summands in the Molien series expansion.
     	  numerat=sum(#L,i->product(drop(L,{i,i})));  
	  -- in above notation, (and using LaTeX syntax): 
	  -- numerat=numerator of \frac{1}{p_1(U)}+...+\frac{1}{p_m(U)}.   
	  denominat=(#G)*product(L); 
	  );
     A:=degreesRing(1);
     -- A is the ring containing the numerator and denominator of the
     -- Hilbert series
     T:= first gens A; -- T is the variable of the Hilbert series	  
     f:=map(A,Ku,{T}); -- Ring map K[U] -> A that sends U to T
     -- Note that any non-integer elements of K would be sent to zero.  
     h:=new Divide from {f(numerat),factor(f(denominat))};
     return reduceHilbert h;
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
 
tryDegrees(PolynomialRing,List,ZZ,List):=(A,H,nn,Li)->(
     -- A - polynomial ring; H - finite group; nn - natural number, typically
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
optimalHSOP(PolynomialRing,List):= o->(R,G)->(
     if(o.DegreeVector=!=0 and instance(o.DegreeVector,List)==false) then(
	  error "DegreeVector must be zero or a List consisting of positive
	  integers"
	  );
     if(o.DegreeVector=!=0 and (#(o.DegreeVector))>(numgens R)) then(
	  error "Length of DegreeVector must not exceed dimension of polynomial 
	  ring"
	  );
     if (ring(G#0))=!=(coefficientRing R) then(
	  error "Matrices must be defined over the same field as the polynomial 
	  ring"
	  );
     n:=numgens R;
     L:=toList(n:1); -- L is list of degrees of primary invariants, initially
     -- set to {1,...,1}. This will be changed by using nextBest and then
     -- a set of primary invariants of degrees from L will be constructed
     -- (subject to L passing various tests). 
     hsopfound:=false;
     S:=0;
     if o.DegreeVector=!=0 then(
	  S=tryDegrees(R,G,n,o.DegreeVector);
          if instance(S,List)==true then hsopfound=true else(
	       error "There is no homogeneous system of parameters with degrees 
	       given in DegreeVector"
	       )
	  );
     -- mol:=value(molienSeries G);
     -- t:=(vars ring mol)_(0,0);
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
	  if ((product L) % (#G))=!=0 then(
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
	  S=tryDegrees(R,G,n,L);
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
dadeHSOP(PolynomialRing,List):= (R,G)-> ( 
     -- R - polynomial ring; G - list of matrices
     n:=numgens R;
     dadebasis:=new MutableHashTable; 
     -- integer keys, d=>d'th Dade basis vector  
     orbits:=new MutableHashTable; 
     -- integer keys, d=> orbit of d'th Dade basis vector
     tuples:=new MutableHashTable; 
     -- integer keys, d=>orb(v_0) x ... x orb(v_{d-1}), d=0...n-1
     d:=0;
     dadebasis#d=((random(R^1,R^{-1}))_(0,0)); -- make a random linear form
     orbits#d=toList(set groupAct(dadebasis#d,G));
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
	  orbits#d=groupAct(dadebasis#d,G);
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

------------------------------------------------
-- primaryInvariants
-- Given a group and a polynomial ring over a number field, computes primary 
-- invariants for the invariant ring
-- Uses the method optimalHSOP by default to do this, or dadeHSOP if optional 
-- argument Dade is set to true
------------------------------------------------
   
primaryInvariants=method(Options=>{Dade=>false, DegreeVector=>0})
primaryInvariants(PolynomialRing,List):=o->(R,G)->( 
     -- R - polynomial ring over a number field; 
     -- G - list of matrices defined over same field as ground field of R
     n:=numgens R;
     KK:=coefficientRing R;
     if(isField KK == false or (o.Dade==false and char KK =!= 0)) then(
	  error "The polynomial ring must be defined over a field of 
	  characteristic zero"
	  );
     for M in G do(
	  if instance(M,Matrix)==false then( 
	       error "The second argument must be a list of matrices that 
	       forms a group"
	       )
	  );
     for M in G do(
	  if ring(M) =!= KK then(
	       error "All matrices in the list must be defined over the same 
	       field as the polynomial ring"
	       )
	  );
     for M in G do(
	  if((numgens target M)=!= n or (numgens source M)=!= n) then( 
	       error "The list in the second argument must consist of invertible 
	       matrices of the same size as the number of generators of the 
	       polynomial ring"
	       )
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
     return dadeHSOP(R,G)
     );
     if o.DegreeVector=!=0 then(
	  return optimalHSOP(R,G,DegreeVector=>(o.DegreeVector)) 
	  )
     else return optimalHSOP(R,G)
     ); 

------------------------------------------------
-- secondaryInvariants
-- Given a list of primary invariants for the invariant ring of a group, finds 
-- secondary invariants
------------------------------------------------

secondaryInvariants=method(Options=>{PrintDegreePolynomial=>false});
secondaryInvariants(List,List):=o->(P,G)->( 
     -- P - List of primary invariants, G - finite matrix group 
     n:=numgens ring(P#0);
     KK:=coefficientRing ring(P#0);
     if(isField KK == false or char KK =!= 0) then(
	  error "The polynomials must have coefficients lying in a field of 
	  characteristic zero"
	  );
     for g in P do(if ring(g) =!= ring(P#0) then(
	       error "All polynomials must be from the same polynomial ring"
	       )
	  );
     for M in G do(
	  if instance(M,Matrix)==false then(
	       error "The second argument must be a list of matrices that forms
	       a group"
	       )
	  );
     for M in G do(if ring(M) =!= KK then(
	       error "All matrices in the list must be defined over the same
	       field as the polynomial ring"
	       )
	  );
     for M in G do(if((numgens target M)=!= n or (numgens source M)=!= n) then(
	       error "The list in the second argument must consist of invertible
	       matrices of the same size as the number of generators of the
	       polynomial ring"
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
     R:=ring(P#0); 
     -- The polynomaial ring containing the invariants. 
     -- Note that M2 must recognise coefficientRing(R) as a field.
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
-- By default assumes the list input is a group, but this can be changed by 
-- changing the optional argument IsGroup to false.
------------------------------------------------

invariantRing=method(
     Options=>{
	  IsGroup=>true,
	  DegreeVector=>0,
	  PrintDegreePolynomial=>false
	  }
     );
invariantRing(PolynomialRing,List):=o->(R,G)->(
     L:=G;
     if o.IsGroup==false then L=generateGroup(G,coefficientRing R);
     P:=primaryInvariants(R,L,DegreeVector=>(o.DegreeVector));
     S:=secondaryInvariants(P,L,PrintDegreePolynomial=>(
	       o.PrintDegreePolynomial
	       )
	  ); 
     return((P,S))
     );
     
--**********************************************
-- DOCUMENTATION --
--**********************************************

beginDocumentation()

document { 
	Key => InvariantRing,
	Headline => "a package to construct the invariant ring of a finite group",
	PARA{
	     TO InvariantRing, " is a package that enables the user to construct
	the invariant ring of a finite matrix subgroup of ", TEX "GL(n,K)", " as
	a free module decomposition. It outputs primary invariants ", TT "f",
	SUB TT "1", TT ",...," , TT "f", SUB TT "n", " and secondary
	invariants ", TT "g", SUB TT "1", TT ",...," , TT "g", SUB TT "r", 
	" such that ", TT "K[", TT "x", SUB TT "1", TT ",...," , TT "x", 
	SUB TT "n", TT "]", SUP TT "G", TT "=A", TT "g", SUB TT "1", TEX "\\oplus", 
	TT "...", TEX "\\oplus", TT "A", TT "g", SUB TT "r", ", where ", TT "A=K[", 
	TT "f", SUB TT "1", TT ",...," , TT "f", SUB TT "n", TT "]", "."
	},
	SeeAlso=>{"hsop algorithms"}
	}
------------------------------------------------
-- generateGroup
-- Optional arguments: OrderBound
------------------------------------------------
   
document {
     Key =>{generateGroup,(generateGroup,List,Ring)},
     Headline => "a brute-force algorithm to generate a group from a list 
     of matrices",
     Usage => "generateGroup(L,K)",
     Inputs => {
          "L"=> List =>{
	       "a list of matrices that generate a finite group, when the matrix 
	       entries are ", TO substitute , "d for values in ", TT "K", "."
	       },
	  "K"=> Ring =>{"a field"} 
          },
     Outputs => {
          List =>{
	       "a list of matrices with entries in ", TT "K", " that is a group"
	       }
          },
     PARA{
	  "The example below computes the dihedral group of order 6 from the two 
	  matrices in ", TT "L", ". ", TT "K", " is the field obtained by by 
	  adjoining a primitive third root of unity to ", TO QQ, "."
	  },
     EXAMPLE lines ///
          K=toField(QQ[w]/(w^2+w+1));
	  L={matrix{{0,1},{1,0}},matrix{{w,0},{0,w^2}}} 
          generateGroup(L,K)
          ///,
     PARA{
	  TO generateGroup, " can also be used to check whether a given ", 
	  TO List, " of matrices forms a group or not. This is done in the next 
	  example."
	  },
     EXAMPLE lines ///
     	  A=sub(matrix{{0,1,0},{0,0,1},{1,0,0}},QQ)
	  B=sub(matrix{{0,1,0},{1,0,0},{0,0,1}},QQ)
	  G0={A,A^2,B,A*B,A^2*B}
	  set(generateGroup(G0,QQ))===set(G0)
	  G1={A^0,A,A^2,B,A*B,A^2*B}
	  set(generateGroup(G1,QQ))===set(G1)
	  ///, 
     PARA{
	  "WARNING: the user needs to be careful to ensure that the entries in
     the matrices are preserved when using ", TO substitute, ". Observe how the 
     matrix defined over ", TT "K", " changes when its entries are substituted 
     for values in ", TO QQ, " in the next example."
     },
     EXAMPLE lines ///
     sub(matrix{{w,0},{0,w^2}},QQ)
     ///,  
     Caveat=>{
	  PARA{
	       "Care needs to be taken when choosing a field to enter in the 
	       second argument. The first step ", TO generateGroup, " performs 
	       is to use ", TO substitute, " on the matrices in the first 
	       argument and the field in the second argument. The field needs to 
	       be chosen so that this process preserves the matrices; see the 
	       last example above."
	       },
	  PARA{
	       TO generateGroup, " is an implementation of an iterative 
	       'brute-force' group generation algorithm. At each stage of the 
	       iteration it takes a ", TO List, TT " L", " of matrices and forms 
	       all possible words of length two from the alphabet ", TT "L", ". 
	       The resulting ", TO List, TT " L'", " is compared to ", TT " L",
	       " (as ", TO Set, "s) and termination of the algorithm occurs 
	       if ", TT "L'=L", ", otherwise the iteration proceeds with ", 
	       TT "L=L'", ". This is an adequate algorithm for small groups, but 
	       is cumbersome otherwise. A better implementation for inputting 
	       groups may be desirable."
	       }
	  },
     PARA{"This function is provided by the package ", TO InvariantRing, "."}
     }

document {
     Key =>{[generateGroup,OrderBound],OrderBound},
     Headline => "an optional argument for generateGroup that bounds the 
     permissible order of the group",
     Usage => "generateGroup(L,K)",
     Inputs => {
          "L"=> List =>{
	       "a list of matrices that generate a finite group, when the matrix 
	       entries are ", TO substitute , "d for values in K."
	       },
	  "K"=> Ring =>{"a field"} 
          },
     Outputs => {
          List =>{"a list of matrices with entries in K that is a group"}
          },
     PARA{
	  TO OrderBound, " takes values in ", TO ZZ, " and its default value is 
	  100. If the order of the group to be generated exceeds the value of ", 
	  TO OrderBound, ", then ", TO generateGroup, " will terminate with an 
	  error message. This is to avoid trying to process a never-ending
     loop."
     },
     PARA{
	  "The example below computes the dihedral group of order 6 from the two 
	  matrices in ", TT "L", ". ", TT "K", " is the field obtained by by 
	  adjoining a primitive third root of unity to ", TO QQ, ". ", 
	  TO OrderBound, " is set to 10, so in this case evaluation of 
	  generateGroup is successful."
	  },
     EXAMPLE lines ///
          K=toField(QQ[w]/(w^2+w+1));
	  L={matrix{{0,1},{1,0}},matrix{{w,0},{0,w^2}}}
          generateGroup(L,K,OrderBound=>10)
          ///,  
     PARA{
	  "If ", TO OrderBound, " is set to an integer less than the order of 
	  the group instead, this will cause an error to occur in the evaluation 
	  of ", TO generateGroup, "."
	  },
     SeeAlso=>{generateGroup} 
     }

------------------------------------------------
-- reynoldsOperator
------------------------------------------------
   
document {
	Key => {reynoldsOperator,(reynoldsOperator,RingElement,List)},
	Headline => "the Reynolds operator for averaging a polynomial over 
	a finite group",
	Usage => "reynoldsOperator(f,G)",
	Inputs => {
		"f" => RingElement => {
		     "in a polynomial ring over a field in ", 
		     TT "n", " variables"
		     },
		"G" => List => {
		     "a finite group of invertible ", TT "n*n", " matrices 
		     over a subring of the base field of the polynomial ring"
		     }
		},
	Outputs => {
		RingElement => {
		     "the result of applying the Reynolds operator to ", TT "f"
		     }
		},
	EXAMPLE lines ///
	     R=QQ[x,y,z];
	     A=matrix{{0,1,0},{-1,0,0},{0,0,-1}}; G={A,A^2,A^3,A^4}
	     f=x^2+y+z
	     reynoldsOperator(f,G)
	    ///,
	PARA{
	     "WARNING: During the execution of ", TO reynoldsOperator, " the
	method ", TO substitute, " is used on the matrices in the ", TO List, 
	TT " G", " with the ", TO coefficientRing, " of ", TT "f", ". This
	means, for example, that one can let ", TT "G", " be a ", TO List, " of
	integer matrices and it will act on a polynomial ring defined over the
	rational numbers when ", TO reynoldsOperator, " is called, but in
	general care should be taken to ensure the 
	entries of the matrices are preserved under the use of ", TO substitute, 
	". In the following example, notice how what should be an invariant
	polynomial is changed through an application of ", TO reynoldsOperator,
	"." 
	     },
	EXAMPLE lines ///
	     R=QQ[x,y,z];
	     K=toField(QQ[i]/(i^2+1));
	     A=matrix{{i,0,0},{0,i,0},{0,0,i}}; G={A,A^2,A^3,A^4}
	     f=x^4+y^4+z^4
	     reynoldsOperator(f,G)
	    ///,
	PARA{
	     "This function is provided by the package ", TO InvariantRing, "."
	     }    
	}


------------------------------------------------
-- primaryInvariants
-- Optional invariants: Dade, DegreeVector
------------------------------------------------

document {
     Key=> {primaryInvariants,(primaryInvariants,PolynomialRing,List)},
     Headline=> "computes a list of primary invariants for the 
     invariant ring of a finite group",
     Usage=> "primaryInvariants(R,G)",
     Inputs=>{
	  "R" => PolynomialRing => {
	       " defined in ", TT "n", " variables over a field"
	       },
	  "G" => List => {
	       " a group of ", TT "n*n", " invertible matrices"
	       }
	  },
     Outputs=>{
	  List => {
	       " consisting of an homogeneous system of parameters (hsop) of the 
	       invariant ring ", TT "R", SUP TT "G"
	       }
	  },
     PARA{
	  "There are two algorithms implemented in ", TO primaryInvariants, 
	  ". The default algorithm (corresponding to the optional argument ", 
	  TO Dade, " taking the value ", TO false, ") currently only works with 
	  polynomial rings over fields of characteristic zero. The second is the 
	  Dade algorithm, corresponding to the optional argument ", TO Dade, 
	  " taking the value ", TO true, ". This algorithm can calculate an 
	  hsop over finite fields, so long as the field is sufficiently large. 
	  See ", TO "hsop algorithms", " for a discussion comparing the two 
	  algorithms."
	  }, 
     EXAMPLE {
	  "A=matrix{{0,1,0},{0,0,1},{1,0,0}};", 
	  "B=matrix{{0,1,0},{1,0,0},{0,0,1}};", 
	  "S3=generateGroup({A,B},QQ)",
	  "primaryInvariants(QQ[x,y,z],S3)"
	  },
     PARA{
	  "Below, the invariant ring ", TT "QQ[x,y,z]", SUP TT "S3", " is 
	  calculated with ", TT "K", " being the field with 101 elements."
	  },
     EXAMPLE {
	  "K=GF(101)",
	  "S3=generateGroup({A,B},K)",
	  "primaryInvariants(K[x,y,z],S3,Dade=>true)"
	  },
     Caveat=> {
	  "Currently users can only use ", TO primaryInvariants, " to calculate 
	  an hsop for the invariant ring over a finite field by using the Dade 
	  algorithm. Users should enter the finite field as a ", TO GaloisField,
     	  " or a quotient field of the form ", TO ZZ, "/p and are advised to
      	  ensure that the ground field has 
	  cardinality greater than ", TT "|G|", SUP TT "n-1", ", where ", 
	  TT "n", " is the number of variables in the polynomial ring ", TT "R", 
	  ". Using a ground field smaller than this runs the risk of the
     	  algorithm getting stuck in an infinite loop; ", TO primaryInvariants,
     	  " displays a warning message asking the user whether they wish to
     	  continue with the computation in this case. See ", 
	  TO "hsop algorithms", " for a discussion on the Dade algorithm."
	  },
     PARA{
	  "This function is provided by the package ", TO InvariantRing, "."
	  }
     }

document {
     Key => {[primaryInvariants, Dade],Dade},
     Headline=> "an optional argument for primaryInvariants determining whether 
     to use the Dade algorithm",
     Usage=> "primaryInvariants(R,G)",
     Inputs=>{
	  "R" => PolynomialRing => {
	       " defined in ", TT "n", " variables over a field"
	       },
	  "G" => List => {
	       " a group of ", TT "n*n", " invertible matrices"
	       }
	  },
     Outputs=>{
	  List => {
	       " consisting of an homogeneous system of parameters (hsop) of the 
	       invariant ring ", TT "R", SUP TT "G"
	       }
	  },
     PARA{
	  TO Dade, " takes ", TO Boolean, " values and is set to ", TO false, 
	  " by default. If ", TO Dade, " is set to ", TO true, ", then ", 
	  TO primaryInvariants, " will use the Dade algorithm to calculate an 
	  homogeneous system of parameters (hsop) for the invariant ring of a
     	  finite group."
     	  },
     PARA{
	  "The example below computes the invariant ring of ", TT "S3", " acting 
	  on ", TT "QQ[x,y,z]", " by permutations on the variables. ", TO Dade, 
	  " is set to ", TO true, "."
	  },   
     EXAMPLE {
          "A=matrix{{0,1,0},{0,0,1},{1,0,0}};", 
	  "B=matrix{{0,1,0},{1,0,0},{0,0,1}};", 
	  "S3=generateGroup({A,B},QQ)",
          "primaryInvariants(QQ[x,y,z],S3,Dade=>true)"
          },
     PARA{
	  "Compare this result to the hsop output when Dade is left to its 
	  default value ", TO false, "."
	  }, 
     EXAMPLE {
          "A=matrix{{0,1,0},{0,0,1},{1,0,0}};", 
	  "B=matrix{{0,1,0},{1,0,0},{0,0,1}};", 
	  "S3=generateGroup({A,B},QQ)",
          "primaryInvariants(QQ[x,y,z],S3)"
          },
     PARA{
	  "Below, the invariant ring ", TT "QQ[x,y,z]", SUP TT "S3", " is 
	  calculated with ", TT "K", " being the field with 101 elements."
	  },
     EXAMPLE {
	  "K=GF(101)",
	  "S3=generateGroup({A,B},K)",
	  "primaryInvariants(K[x,y,z],S3,Dade=>true)"
	  },
     PARA{
	  "For more information about the algorithms used to calculate an hsop 
	  in primaryInvariants, see ", TO "hsop algorithms", "."
	  },
     Caveat=> {
	  "Currently users can only use ", TO primaryInvariants, " to calculate 
	  an hsop for the invariant ring over a finite field by using the Dade 
	  algorithm. Users should enter the finite field as a ", TO GaloisField,
     	  " or a quotient field of the form ", TO ZZ, "/p and are advised to
      	  ensure that the ground field has 
	  cardinality greater than ", TT "|G|", SUP TT "n-1", ", where ", 
	  TT "n", " is the number of variables in the polynomial ring ", TT "R", 
	  ". Using a ground field smaller than this runs the risk of the
     	  algorithm getting stuck in an infinite loop; ", TO primaryInvariants,
     	  " displays a warning message asking the user whether they wish to
     	  continue with the computation in this case. See ", 
	  TO "hsop algorithms", " for a discussion on the Dade algorithm."
	  },
     SeeAlso=>{"hsop algorithms","primaryInvariants"}
     }

document {
     Key => {[primaryInvariants, DegreeVector],DegreeVector},
     Headline=> "an optional argument for primaryInvariants that finds invariants 
     of certain degrees",
     Usage=> "primaryInvariants(R,G)",
     Inputs=>{
	  "R" => PolynomialRing => {
	       " defined in ", TT "n", " variables over a field"
	       },
	  "G" => List => {
	       " a group of ", TT "n*n", " invertible matrices"
	       }
	  },
     Outputs=>{
	  List => {
	       " consisting of an homogeneous system of parameters (hsop) of 
	       the invariant ring ", TT "R", SUP TT "G"
	       }
	  },
     PARA{
	  "By default, ", TO primaryInvariants," uses an optimising algorithm 
	  which tests for the existence of an homogeneous system of parameters 
	  (hsop) ", TT "(f", SUB TT "1", TT ",...,", TT "f", SUB TT "n", TT ")", 
	  " with positive degrees corresponding to ", TT "(d", SUB TT "1", 
	  TT ",...,", TT "d", SUB TT "n", TT ")", " in ", TO ZZ, SUP TT "n", ". 
	  If it is known that an hsop exists for a certain collection of 
	  degrees, this can be assigned, as a ", TO List, ", to the optional 
	  argument ", TO DegreeVector, ". ", TO primaryInvariants, " will then 
	  output an hsop corresponding to this list of degrees. If however no 
	  such hsop exists, ", TO primaryInvariants, " outputs an error 
	  message."
	  },
     PARA{
	  "Note that the ", TO List, " assigned to ", TO DegreeVector, " is
          ignored if ", TO Dade, " is set to ", TO true, "."
	  },
     PARA{
	  TO DegreeVector, " is also an optional argument for the method ",
	  TO invariantRing, "; see ", TO [invariantRing,DegreeVector], " for
     	  more information."
	  }, 
     EXAMPLE {
          "A=matrix{{0,1,0},{0,0,1},{1,0,0}};", 
	  "B=matrix{{0,1,0},{1,0,0},{0,0,1}};", 
	  "S3=generateGroup({A,B},QQ)",
          "primaryInvariants(QQ[x,y,z],S3,DegreeVector=>{3,3,4})"
          },
     Caveat=> {
	  "Currently users can only use ", TO primaryInvariants, " to calculate 
	  an hsop for the invariant ring over a finite field by using the Dade 
	  algorithm. Users should enter the finite field as a ", TO GaloisField,
     	  " or a quotient field of the form ", TO ZZ, "/p and are advised to
      	  ensure that the ground field has 
	  cardinality greater than ", TT "|G|", SUP TT "n-1", ", where ", 
	  TT "n", " is the number of variables in the polynomial ring ", TT "R", 
	  ". Using a ground field smaller than this runs the risk of the
     	  algorithm getting stuck in an infinite loop; ", TO primaryInvariants,
     	  " displays a warning message asking the user whether they wish to
     	  continue with the computation in this case. See ", 
	  TO "hsop algorithms", " for a discussion on the Dade algorithm."
	  },
     SeeAlso=>{primaryInvariants,[invariantRing,DegreeVector]}
     }

------------------------------------------------
-- hsop algorithms
-- Extra documentation that compares the default and Dade algorithms used in 
-- primaryInvariants
------------------------------------------------

document {
     Key => "hsop algorithms",
     Headline => "an overview of the algorithms used in primaryInvariants",
     PARA{
	  "This page contains a discussion on the two algorithms that are used 
	  in the function ", TO primaryInvariants, ", which computes an 
	  homogenous system of parameters (hsop) for the invariant ring ", 
	  TT "R:=K[x", SUB TT "1", TT ",...,x", SUB TT "n", TT "]", SUP TT "G", 
	  " of a finite group ", TT "G", ". Which algorithm is used depends on 
	  the ", TO Boolean, " value the optional argument ", TO Dade, " takes. 
	  In the case where it is set to ", TO false, " it uses what shall be 
	  referred to as the 'default' algorithm. If it is set to ", TO true, 
	  " then it uses what shall be called the 'Dade' algorithm."
	  },
     PARA{
	  "The default algorithm is an implementation of the 'optimal' algorithm 
	  given in [K]. It is optimal in the sense that it finds an hsop ", 
	  TT "f", SUB TT "1", TT ",...,", TT "f", SUB TT "n", " such that the 
	  number of secondary invariants required to make ", TT "R", " into a 
	  free ", TT "K[f", SUB TT "1", TT ",...,f", SUB TT "n", TT "]", 
	  "-module is minimal. The first step in the default algorithm is to 
	  cycle through ", TO List, "s ", TT "{d", SUB TT "1", TT ",...,", 
	  TT "d", SUB TT "n", TT "}", " of possible degrees for the hsop. It 
	  tests the degrees against two restrictions that are known to hold for 
	  any hsop of ", TT "R", ": firstly, the order of ", TT "G", " must 
	  divide the product ", TT "d", SUB TT "1", TT "*...*", TT "d", 
	  SUB TT "n", " and secondly, the polynomial ", TT EM "H", 
	  TT "(R,T)*(1-T", SUP TT "d1", TT ")*...*(1-T", SUP TT "dn", TT ")", 
	  " must lie in ", TO ZZ, "[", TT "T", "], where ", TT EM "H", 
	  TT "(R,T)", " is the Molien (Hilbert) series of ", TT "R", 
	  " [DK, p83]. Once a ", TO List, " of suitable degrees is found, the 
	  algorithm uses a Krull-dimension based test that holds for algebras 
	  over infinite fields to determine the existence of an hsop with the 
	  candidate degrees; see [K, Theorem 2]. It then finds such an hsop if 
	  one exists, or tries a new ", TO List, " of degrees if such an hsop 
	  does not exist. Note: if one knows a priori that an hsop exists for 
	  some ", TO List, " of degrees, this can be assigned to the optional 
	  argument ", TO DegreeVector, " and the default algorithm will compute 
	  an hsop with degrees corresponding to this ", TO List, ". Finally, 
	  users should be aware that the default algorithm currently only works 
	  in the case where ", TT "R", " is defined over a field of 
	  characteristic zero."
	  },
     PARA{
	  "The Dade algorithm is simpler than the default algorithm. It first 
	  constructs a Dade basis ", TT "v", SUB TT "1", TT ",...,v", 
	  SUB TT "n", " for the dual space ", TT "V", SUP TT "*", " spanned 
	  by ", TT "x", SUB TT "1", TT ",...,x", SUB TT "n", ". Then for each ", 
	  TT "i", ", it computes the polynomial ", TT "f", SUB TT "i", " defined 
	  as the product over the ", TT "G", "-orbit of ", TT "v", SUB TT "i", 
	  ". The resulting collection ", TT "f", SUB TT "1", TT ",...,", TT "f", 
	  SUB TT "n", " is an hsop for ", TT "R", "; see [DK, pp80,81]. In the 
	  implemented Dade algorithm, a Dade basis is constructed iteratively by 
	  choosing ", TO random, " linear forms such that ", TT "v", SUB TT "i", 
	  " is not contained in any of the vector subspaces ", TT "span", 
	  SUB TT "K", TT "{", TT "w", SUB TT "1", TT ",...,w", SUB TT "i-1", 
	  TT "}", ", where ", TT "w", SUB TT "j", " is in the ", TT "G", "-orbit 
	  of ", TT "v", SUB TT "j", ". The Dade algorithm can work with the case 
	  of finite fields, provided that the field is large enough to ensure ", 
	  TT "K", SUP TT "n", " cannot be filled by the union of the subspaces 
	  mentioned in the construction of the Dade basis. A sufficient, though 
	  not necessarily optimal, requirement is that ", TT "|K|>|G|", 
	  SUP TT "n-1", ". Because of the random generation involved in the 
	  construction of a Dade basis, the Dade algorithm will generally 
	  output ", TT "n", " primary invariants of degrees equalling the order 
	  of ", TT "G", " that have ugly coefficients."
	  }, 
     PARA{
	  "The example below provides a good comparison of the two different 
	  algorithms and their relative merits."
	  }, 
     EXAMPLE {
	  "A=matrix{{0,-1,0},{1,0,0},{0,0,-1}}",
	  "B=matrix{{0,-1,0},{1,0,0},{0,0,1}}",
          "C4xC2=generateGroup({A,B},QQ)",
          "S=QQ[x,y,z];"
          },
     PARA{
	  "The two algorithms used in ", TO primaryInvariants, " are 
	  timed. One sees that the Dade algorithm is faster, 
	  however the primary invariants output are all of degree 8 and have 
	  ugly coefficients."
	  },
     EXAMPLE {
	  "time P1=primaryInvariants(S,C4xC2)",
	  "time P2=primaryInvariants(S,C4xC2,Dade=>true)"
	  },
     PARA{
	  "The extra work done by the default algorithm to ensure an optimal 
	  hsop is rewarded by needing to calculate a smaller collection of 
	  corresponding secondary invariants.  In fact, it has proved quicker 
	  overall to calculate the invariant ring based on the optimal 
	  algorithm rather than the Dade algorithm."
	  },
     EXAMPLE {
	  "time secondaryInvariants(P1,C4xC2)",
	  "time secondaryInvariants(P2,C4xC2)",
	  "#oo"
	  },
     PARA{
	  "Of course, currently one advantage of the Dade algorithm is that it 
	  can calculate an hsop for the invariant ring when considering a 
	  finite field. Since ", TT "|C4xC2|", SUP TT "2", "=64, it is safe to 
	  consider the finite field with 101 elements."
	  },
     EXAMPLE{
	  "K=GF(101);",
	  "C4xC2=generateGroup({A,B},K)",
	  "primaryInvariants(K[x,y,z],C4xC2,Dade=>true)"
	  },     
     PARA{EM "References"},
     PARA{
	  "[DK] Derksen, H., Kemper, G. ", EM "Computational Invariant Theory", 
	  ". Berlin Heidelberg New York: Springer-Verlag, 2002"
	  },
     PARA{
	  "[K] Kemper, G. ", EM "An Algorithm to Calculate Optimal Homogeneous 
	  Systems of Parameters", ". J. Symbolic Computation ", EM "27", 
	  " (1999), 171-184"
	  },
     SeeAlso=>{primaryInvariants,Dade,DegreeVector,invariantRing}    
     }

------------------------------------------------
-- secondaryInvariants
-- Optional arguments: PrintDegreePolynomial 
------------------------------------------------

document {
	Key => {secondaryInvariants,(secondaryInvariants,List,List)},
	Headline => "computes secondary invariants for the invariant ring of a 
	finite group",
	Usage => "secondaryInvariants(P,G)",
	Inputs => {
		"P" => List => {
		     "a list of primary invariants in ", TT "n", " variables ", 
		     TT "f", SUB TT "1", TT ",...,", TT "f", SUB TT "n", " for 
		     the invariant ring of ", TT "G", " defined over a field ",
	             TT "K", " of characteristic zero"
		     },
		"G" => List => {
		     "a finite group of ", TT "n*n", " matrices over the same 
		     field as the polynomial ring of the ", TT "f", SUB TT "i"
		     }
		},
	Outputs => {
		List => {
		     "a list ", TT "S", " of secondary invariants for the 
		     invariant ring ", TT "R=K[x", SUB TT "1", TT ",...,x", 
		     SUB TT "n", TT "]", SUP TT "G", " of ", TT "G", " that 
		     makes ", TT "R", " into a free ", TT "K[f", SUB TT "1", 
		     TT ",...,f", SUB TT "n", TT "]", "-module with basis ", 
		     TT "S"
		     }
		},
	PARA{
	     "The example below computes the secondary invariants for the 
	     dihedral group with 6 elements, given a set of primary 
	     invariants", TT "P", "."
	     },
	EXAMPLE lines ///
	     K=toField(QQ[a]/(a^2+a+1));
	     R=K[x,y];
	     A=matrix{{a,0},{0,a^2}}; 
	     B=sub(matrix{{0,1},{1,0}},K); 
	     D6={A^0,A,A^2,B,A*B,A^2*B}
	     P={x^3+y^3,-(x^3-y^3)^2};
	     secondaryInvariants(P,D6)
	    ///,
	Caveat=> {
	     PARA{
		  "Currently, a user needs to ensure that the all primary 
		  invariants are defined with coefficients in a ring that ", 
		  EM "Macaulay2", " recognises as a characteristic zero field 
		  (see ", TO toField, " for a way to do this)."
		  },
	     PARA{
	  	  "Note also that the function ", TO secondaryInvariants, 
		  " only works when ", TT "R", " is defined over a field of 
		  characteristic zero."
	  	  }
	     },
	PARA{
	     "This function is provided by the package ", TO InvariantRing, "."
	     }
	}

document {
     Key => {[secondaryInvariants, PrintDegreePolynomial],PrintDegreePolynomial},
     Headline => "an optional argument for secondaryInvariants that determines 
     the printing of an informative polynomial",
     Usage => "secondaryInvariants(P,G)",
	Inputs => {
		"P" => List => {
		     "a list of primary invariants in ", TT "n", " variables ", 
		     TT "f", SUB TT "1", TT ",...,", TT "f", SUB TT "n", " for 
		     the invariant ring of ", TT "G", " defined over a field ",
                     TT "K", " of characteristic zero"
		     },
		"G" => List => {
		     "a finite group of ", TT "n*n", " matrices over the same 
		     field as the polynomial ring of the ", TT "f", SUB TT "i"
		     }
		},
	Outputs => {
		List => {
		     "a list ", TT "S", " of secondary invariants for the 
		     invariant ring ", TT "R=K[x", SUB TT "1", TT ",...,x", 
		     SUB TT "n", TT "]", SUP TT "G", " of ", TT "G", " that 
		     makes ", TT "R", " into a free ", TT "K[f", SUB TT "1", 
		     TT ",...,f", SUB TT "n", TT "]", "-module with basis ", 
		     TT "S"
		     }
		},
     PARA{
	  TO PrintDegreePolynomial, " takes a ", TO Boolean, " value and is 
	  set to ", 
	  TO false, " by default. If it is set to ", TO true, ", then ", 
	  TO secondaryInvariants, " will print a polynomial in the variable ",
          TT "T", ". This polynomial encodes the degrees of the secondary
          invariants (given by the exponents of ", TT "T", " appearing in it) 
	  and the number of secondary invariants of a given degree (the
          coefficient of the corresponding term in the
          polynomial). Specifically, if ", TO secondaryInvariants, " takes as 
          input a set of primary invariants of degrees ", TT "d", SUB TT "1", 
	  ",", TT "...", ",", TT "d", SUB TT "n", " for an invariant ring ", 
	  TT "S", 
	  SUP TT "G", " of a finite group ", TT "G", ", and ", TT "H(S", 
	  SUP TT "G", TT ",T)", "denotes the Molien (Hilbert) series of ",
	  TT "S", SUP TT "G", ", then ", TO secondaryInvariants, " will compute
	  the polynomial ",  TT "H(S", SUP TT "G", TT ",T)*", TT "(1-T", 
	  SUP(TT "d", SUB TT "1"), TT ")*...*(1-T", SUP(TT "d", SUB TT "n"), 
	  TT ")", "."   
     	  },
     PARA{
	  TO PrintDegreePolynomial, " is also an optional argument for the
     	  method ", TO invariantRing, "; see ", 
	  TO [invariantRing,PrintDegreePolynomial], " for more information." 
	  },    
     PARA{
	  "The example below computes the secondary invariants for the dihedral 
	  group with 6 elements, given a set of primary invariants ", TT "P",
          ". The optional argument ", TO PrintDegreePolynomial, " is set to ", 
	  TO true, " in order to see which degrees the secondary invariants 
	  should have."
	  },
	EXAMPLE lines ///
             K=toField(QQ[a]/(a^2+a+1));
	     R=K[x,y];
	     A=matrix{{a,0},{0,a^2}}; 
	     B=sub(matrix{{0,1},{1,0}},K); 
	     D6={A^0,A,A^2,B,A*B,A^2*B}
	     P={x^3+y^3,-(x^3-y^3)^2};
	     secondaryInvariants(P,D6,PrintDegreePolynomial=>true)
            ///,
	Caveat=> {
	     "Currently, a user needs to ensure that the all primary invariants 
	     are defined with coefficients in a ring that ", EM "Macaulay2", 
	     " recognises as a characteristic zero field (see ", TO toField, 
	     " for a way to do this)."
	},
   SeeAlso=>{[invariantRing,PrintDegreePolynomial]}
     }

------------------------------------------------
-- molienSeries
------------------------------------------------

document {
	Key => {molienSeries,(molienSeries,List)},
	Headline => "computes the Molien (Hilbert) series of the invariant ring 
	of a finite group",
	Usage => "molienSeries G",
	Inputs =>{
	     "G"=> List =>{
		  " a finite group of invertible matrices, all defined over the 
		  same field of characteristic zero"
		  }
	     },
	Outputs =>{
	     Divide=>{
		  "the Molien series of the invariant ring of G as a rational 
		  function in ", TT "T"
		  }
	     },  
	PARA{
	     "The example below computes the Molien series for the dihedral 
	     group with 6 elements. ", TT "K", " is the field obtained by 
	     adjoining a primitive third root of unity to ", TO QQ, "."
	     }, 
	EXAMPLE lines ///
	K=toField(QQ[a]/(a^2+a+1));
	A=matrix{{a,0},{0,a^2}}; 
	B=sub(matrix{{0,1},{1,0}},K); 
	D6={A^0,A,A^2,B,A*B,A^2*B}
	molienSeries D6
///,
        PARA{
	     "This function is provided by the package ", TO InvariantRing, "."
	     }  
}

------------------------------------------------
-- invariantRing
-- Optional arguments: IsGroup, DegreeVector, PrintDegreePolynomial
------------------------------------------------
 
document {
     Key =>{invariantRing,(invariantRing,PolynomialRing,List)},
     Headline => "calculates a Hironaka decomposition for the invariant ring of
     a finite group",
     Usage => "invariantRing(R,G)",
     Inputs => {
          "R"=> PolynomialRing =>{
	       "in ", TT "n", " variables over a field of characteristic zero"
	       },
	  "G"=> List =>{
	       " a list of ", TT "n*n", " invertible matrices over the same
               field as the polynomial ring that either is a finite group or 
	       generates a finite group"
	       } 
          },
     Outputs => {
          Sequence =>{
	       TT "(P,S)", ", where ", TT "P", " is a ", TO List, " of primary 
	       invariants (i.e. an homogeneous system of parameters for the 
	       invariant ring) and ", TT "S", " is a ", TO List, " of
               corresponding secondary invariants"
	       }
          },
     PARA{
	  TO invariantRing, "  makes use of the functions ",
     	  TO primaryInvariants, ", ", TO secondaryInvariants, " and ", 
	  TO generateGroup,
     	  " in order to compute the invariant ring of a finite group. Apart 
	  from ", TO IsGroup, ", all of the optional arguments of ", 
	  TO invariantRing, " play the same role as for these three functions.
	  Note that if ", TO IsGroup, " is set to ", TO false, " then the second 
	  argument in ", TO generateGroup, " will be set to the ", 
	  TO coefficientRing, " of the polynomial ring ", TT "R", ". The 
	  function ",
          TO invariantRing, " calls upon ", TO primaryInvariants, ", with the
     	  optional argument ", TO Dade, " set to ", TO false, ", to compute a set
    	  of primary invariants, resulting in Kemper's 'optimal' algorithm being
     	  used (see ", TO "hsop algorithms", " for more information)."   
          }, 
     PARA{
	  "The example below computes a set of primary and secondary invariants 
	  for an action of the cyclic group of order 4 on ", TT "QQ[x,y]", "."
	  },
     EXAMPLE lines ///
          C4=generateGroup({matrix{{0,-1},{1,0}}},QQ)
	  invariantRing(QQ[x,y],C4)
          ///,  
     PARA{
	  "From the output one sees that ", TT "QQ[x,y]", SUP(TT "C4"), 
	  TT "=QQ[f", SUB(TT "1"), TT "f", SUB(TT "2"), TT "]", TEX "\\oplus", TT "QQ[f", 
	  SUB(TT "1"), TT "f", SUB(TT "2"), TT "](x", SUP(TT "4"), TT "+y", 
	  SUP(TT "4"), TT ")", ", where ", TT "f", SUB(TT "1"), TT "=x", SUP(TT "2"), 
	  TT "+y", SUP(TT "2"), " and ",TT "f", SUB(TT "2"), TT "=xy", 
	  SUP(TT "3"), TT "-x", SUP(TT "3"), TT "y", "."
	  }, 
     PARA{
	  "Alternatively, one can calculate the invariant ring just by
     	  specifying the generator of the cyclic group and setting the optional 
	  argument ", TO IsGroup, " to ", TO false, "."
	  },
     EXAMPLE lines ///
	  invariantRing(QQ[x,y],{sub(matrix{{0,-1},{1,0}},QQ)},IsGroup=>false)
	  ///,
     Caveat=>{
	  "Currently invariantRing can only calculate with polynomial rings and 
	  matrices over fields of characteristic 0."
	  },
     SeeAlso=>{"hsop algorithms"},
     PARA{
	  "This function is provided by the package ", TO InvariantRing, "."
	  }  
     }

document {
     Key =>{[invariantRing,IsGroup],IsGroup},
     Headline => "an optional argument for invariantRing that specifies whether 
	  the List is a group",
     Usage => "invariantRing(R,G)",
     Inputs => {
          "R"=> PolynomialRing =>{
	       "in ", TT "n", " variables over a field of characteristic zero"
	       },
	  "G"=> List =>{
	       " a list of ", TT "n*n", " invertible matrices over the same 
	       field as the polynomial ring that either is a finite group or 
	       generates a finite group"
	       } 
          },
     Outputs => {
          Sequence =>{
	       TT "(P,S)", ", where ", TT "P", " is a ", TO List, " of primary 
	       invariants (i.e. an homogeneous system of parameters for the 
	       invariant ring) and ", TT "S", " is a ", TO List, " of
     	       corresponding secondary invariants"
	       }
          },
     PARA{
	  TO IsGroup, " takes ", TO Boolean, " values and by default is set to ",
	  TO true, ". If it is set to ", TO false, ", then ", TO invariantRing,
	  " will apply ", TO generateGroup, " to the input ", TO List, " at the 
	  beginning of the computation and work with the resulting group. Note 
	  that the second argument of ", TO generateGroup, " will be set to 
	  the ",
	  TO coefficientRing, " of the polynomial ring entered into ", 
	  TO invariantRing, ". Otherwise, ", TO invariantRing, " will assume 
	  that the ", TO List, " input is a group and will not apply ", 
	  TO generateGroup, " to it. This saves time in the evaluation of ", 
	  TO invariantRing, ", but will produce unintended results if the ", 
	  TO List, " input is not a group. One can test whether or not a ", 
	  TO List, " of matrices forms a group using ", TO generateGroup, "."
	  },
     PARA{
	  "The example below computes a set of primary and secondary invariants 
	  for an action of the cyclic group of order 4 on ", TT "QQ[x,y]", ". 
	  The optional argument ", TO IsGroup, " is set to ", TO false, " so 
	  that ", TO invariantRing, " will first generate a group of matrices, 
	  defined over the ", TO coefficientRing, " ", TO QQ, ", from the ", 
	  TO List, TT " C4", "."
	  },
     EXAMPLE lines ///
          C4={sub(matrix{{0,-1},{1,0}},QQ)};
	  invariantRing(QQ[x,y],C4,IsGroup=>false)
          ///,
     Caveat=>{
	  "By default ", TO IsGroup, " is set to ", TO true, ", meaning ", 
	  TO invariantRing, " will not check that the ", TO List, " input 
	  in the second argument is a group. This could produce unintended 
	  results if it turns out that the second argument is not a group."
	  },
     SeeAlso=>{invariantRing}   
     }

document {
     Key =>[invariantRing,DegreeVector],
     Headline => "an optional argument for invariantRing that finds primary 
     invariants of certain degrees",
     Usage => "invariantRing(R,G)",
     Inputs => {
          "R"=> PolynomialRing =>{
	       "in ", TT "n", " variables over a field of characteristic zero"
	       },
	  "G"=> List =>{
	       " a list of ", TT "n*n", " invertible matrices over the same 
	       field as the polynomial ring that either is a finite group or 
	       generates a finite group"
	       } 
          },
     Outputs => {
          Sequence =>{
	       TT "(P,S)", ", where ", TT "P", " is a ", TO List, " of primary 
	       invariants (i.e. an homogeneous system of parameters for the 
	       invariant ring) and ", TT "S", " is a ", TO List, " of 
	       corresponding secondary invariants"
	       }
          },
     PARA{
	  "By default, ", TO invariantRing," uses an optimising algorithm 
	  which tests for the existence of an homogeneous system of parameters 
	  (hsop) ", TT "(f", SUB TT "1", TT ",...,", TT "f", SUB TT "n", TT ")", 
	  " with positive degrees corresponding to ", TT "(d", SUB TT "1", 
	  TT ",...,", TT "d", SUB TT "n", TT ")", " in ", TO ZZ, SUP TT "n", ". 
	  If it is known that an hsop exists for a certain collection of 
	  degrees, this can be assigned, as a ", TO List, ", to the optional 
	  argument ", TO DegreeVector, ". The hsop output by ", 
	  TO invariantRing, " will then have degrees corresponding to this ", 
	  TO List, ". If however no such hsop exists, ", TO invariantRing, 
	  " outputs an error message."
	  },
     PARA{
	  TO DegreeVector, " is also an optional argument for the method ", 
	  TO primaryInvariants, "; see ", TO [primaryInvariants,DegreeVector], "
     	  for more information."
	  }, 
     PARA{
	  "The example below computes a set of primary and secondary invariants 
	  for the invariant ring of the symmetric group ", TT "S3", "acting 
	  on ", TT "QQ[x,y,z]", " by permuting the variables. The optional 
	  argument ", TO DegreeVector, " is set to ", TT "{2,3,4}", 
	  ", so that ", TO invariantRing, " will find primary invariants of 
	  degrees 2,3 and 4."
	  },
     EXAMPLE lines ///
          A=matrix{{0,1,0},{0,0,1},{1,0,0}};
	  B=matrix{{0,1,0},{1,0,0},{0,0,1}};
          S3=generateGroup({A,B},QQ)
	  invariantRing(QQ[x,y,z],S3,DegreeVector=>{2,3,4})
          ///,
	  SeeAlso=>{invariantRing,[primaryInvariants,DegreeVector]}   
     }

document {
     Key =>[invariantRing,PrintDegreePolynomial],
     Headline => "an optional argument for invariantRing that determines the 
     printing of an informative polynomial",
     Usage => "invariantRing(R,G)",
     Inputs => {
          "R"=> PolynomialRing =>{
	       "in ", TT "n", " variables over a field of characteristic zero"
	       },
	  "G"=> List =>{
	       " a list of ", TT "n*n", " invertible matrices over the same 
	       field as the polynomial ring that either is a finite group or 
	       generates a finite group"
	       } 
          },
     Outputs => {
          Sequence =>{
	       TT "(P,S)", ", where ", TT "P", " is a ", TO List, " of primary 
	       invariants (i.e. an homogeneous system of parameters for the 
	       invariant ring) and ", TT "S", " is a ", TO List, " of 
	       corresponding secondary invariants"
	       }
          },
     PARA{
	  TO PrintDegreePolynomial, " takes a ", TO Boolean, " value and is 
	  set to ", 
	  TO false, " by default. Its behaviour is identical to that of its use
	  in the method ", TO secondaryInvariants, ". If it is set to ", 
	  TO true, ", then ", TO invariantRing, " will print a polynomial in the 
	  variable ", TT "T", " giving information about the degrees of the 
	  secondary invariants it outputs. See ", 
	  TO [secondaryInvariants,PrintDegreePolynomial], " for more 
	  information."  
	  },
     PARA{
	  "The example below computes a set of primary and secondary invariants 
	  for an action of the cyclic group of order 4 on ", TT "QQ[x,y]", ". 
	  The optional argument ", TO PrintDegreePolynomial, " is set to ", 
	  TO true, ", so that ", TO invariantRing, " will print a polynomial 
	  before outputting the result. In the example, this polynomial tells us
     	  that there are 4 secondary invariants, with degrees 0,1,2 and 3."
	  },
     EXAMPLE lines ///
          L={sub(matrix{{0,-1},{1,0}},QQ)};
	  invariantRing(QQ[x,y],L,PrintDegreePolynomial=>true)
          ///,
	  SeeAlso=>{secondaryInvariants,PrintDegreePolynomial}   
     }

--**********************************************
-- TESTS --
--**********************************************

-- Note the following references for representation theory of finite groups and
-- basic group theory:
-- [JL01] James, G., Liebeck, M. "Representations and Characters of Groups". 
-- 2nd Edition. Cambridge, UK: Cambridge University Press, 2001.
-- [A88] Armstrong, M.A. "Groups and Symmetry". Springer Undergraduate Texts in
-- Mathematics. New York: Springer Verlag, 1988.
  
------------------------------------------------
-- Test 0: Test for reynoldsOperator
------------------------------------------------

TEST ///
R=QQ[x,y,z];
A=matrix{{0,1,0},{-1,0,0},{0,0,-1}}; G={A,A^2,A^3,A^4};
f=x^2+y+z;
assert(reynoldsOperator(f,G)==(1/2)*(x^2+y^2));
///

------------------------------------------------
-- Test 1: Test for generateGroup
------------------------------------------------

TEST ///
A=transpose matrix{{0,1,0,0},{0,0,1,0},{0,0,0,1},{-1,-1,-1,-1}};
B=matrix{{-1,1,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}};
S5=generateGroup({A,B}, QQ, OrderBound=>200);
assert(#S5==120)
C=matrix{{0,1,0,0},{0,0,1,0},{1,0,0,0},{0,0,0,1}};
D=matrix{{0,1,0,0},{1,0,0,0},{0,0,0,1},{0,0,1,0}};
A4=generateGroup({C,D},QQ);
assert(#A4==12)	       
///

-- A,B give the defining representation for S5 (symmetric group on 5 elements)
-- (see [J+L, Example 7.3 (3)])
-- C,D give the permutation representation for S4 (symmetric group on 4
-- elements). Note C is a transposition, D is a cyclic permutation of order 4.
-- (see [JL01, Chapter 4] and [A88, Chapter 27]) 
 
------------------------------------------------
-- Test 2:  
-- Checks primaryInvariants and secondaryInvariant for correct
-- output for the case where ground field is a number field.
-- The test for invariantRing is run twice: once with DegreeVector set to false
-- (the default) and a second time with DegreeVector set to true.
------------------------------------------------

TEST ///
K=toField(QQ[i]/(i^2+1));
R=K[x,y]; 
G=generateGroup({matrix{{i,0},{0,-i}},matrix{{0,1},{1,0}}},K);
assert(#G==8)
P=primaryInvariants(R,G);
assert(
     P=={x*y,(1/2)*x^4+(1/2)*y^4}
     )
assert(
     secondaryInvariants(P,G)=={sub(1,R)}
     )
///

-- G is the dihedral group of order 8
-- Note i is the square root of -1
-- If A=matrix{{i,0},{0,-i}}, B=matrix{{0,1},{1,0}}, one can check that 
-- A^4=B^2=(A*B)^2=1, which are the defining relations for the dihedral group of
-- order 8 (see [A88, Chapter 27])

------------------------------------------------
-- Test 3:
-- Checks invariantRing for correct output for the case where ground field is a 
-- number field. Test is performed three times: once with DegreeVector set to 
-- false (the default), a second time with DegreeVector set to the expected 
-- value, and  a third time with the degree vector set to the value {4,4}.
------------------------------------------------

TEST ///
K=toField(QQ[i]/(i^2+1));
R=K[x,y]; 
G=generateGroup({matrix{{i,0},{0,-i}},matrix{{0,1},{1,0}}},K);
assert(
     invariantRing(R,G)==({x*y,(1/2)*x^4+(1/2)*y^4},{sub(1,R)})
     )
assert(
     invariantRing(R,G,DegreeVector=>{2,4})==({x*y,(1/2)*x^4+(1/2)*y^4},{sub(1,R)})
     )
assert(
     invariantRing(R,G,DegreeVector=>{4,4})=!=({x*y,(1/2)*x^4+(1/2)*y^4},{sub(1,R)})
     )
///

------------------------------------------------
-- Test 4:
-- Checks primaryInvariants for the symmetric group S3 - these should be
-- the elementary symmetric polynomials in 3 variables
-- Checks also that 1 is the only secondary invariant
------------------------------------------------

-- TEST ///
-- setRandomSeed 2
-- S=QQ[x,y,z];
-- r=matrix{{0,1,0},{0,0,1},{1,0,0}};
-- s=matrix{{0,1,0},{1,0,0},{0,0,1}};
-- S3=generateGroup({r,s},QQ)
-- P=primaryInvariants(S,S3)
-- assert(
--      P=={x+y+z,x^2+y^2+z^2,x*y*z}
--      -- previous tests:
--      -- P=={x+y+z,x*y+x*z+y*z,x^3+y^3+z^3}
--      -- P=={x+y+z,x*y+x*z+y*z,x*y*z}
--      -- we don't know why the answer has changed twice
--      )
-- assert(secondaryInvariants(P,S3)=={sub(1,S)})
-- ///

-- The group in this example is the permutation representation of the symmetric
-- group on 3 elements (see [JL01, Chapter 4]). Note r is a cyclic permutation of
-- order 3 and s is a transposition, so these generate the group (see eg
-- [A88, Theorem 6.3]) 

------------------------------------------------
-- Test 5: Checks that the Dade algorithm works for large enough finite fields 
-- *NB it is possible that primaryInvariants(S,S3,Dade=>true) can run correctly 
-- and output an invariant polynomial of degree strictly less than the 
-- cardinality of the group. If a check on the package invariant ring reports 
-- failure of the folloing test, then one should see if the test is passed upon 
-- a second attempt. Only if the test fails a second time is it worth inspecting 
-- the code for errors.    
------------------------------------------------

TEST ///
K=GF(101);
S=K[x,y,z];
r=matrix{{0,1,0},{0,0,1},{1,0,0}};
s=matrix{{0,1,0},{1,0,0},{0,0,1}};
S3=generateGroup({r,s},K);
setRandomSeed 0
P=primaryInvariants(S,S3,Dade=>true);
P/degree			 -- under Ubuntu 32, this gives {{3}, {6}, {6}}
setRandomSeed 0
P=primaryInvariants(S,S3,Dade=>true);
P/degree			 -- under Ubuntu 32, this gives {{6}, {6}, {6}}
assert(
     P==apply(P,f->reynoldsOperator(f,S3))
     )
assert(
     dim(S/ideal(P))==0
     )
assert(
     apply(P,degree)==toList(#P:{#S3})
     )
///

------------------------------------------------
-- Test 6:
-- Checks molienSeries and secondaryInvariants on a known example where the
-- ground field is a number field 
------------------------------------------------

TEST ///
K=toField(QQ[a]/(a^2+a+1)); -- note a is a primitive cube root of 1 
A=sub(matrix{{a,0},{0,a^2}},K); 
B=sub(matrix{{0,1},{1,0}},K); 
D6={A^0,A,A^2,B,A*B,A^2*B};
R=K[x,y];
mol:=molienSeries(D6);
use ring value denominator mol;
assert(
     (value denominator mol)==(T^5-T^3-T^2+1)
     )
assert(
     (value numerator mol)==1
     )
assert(
     secondaryInvariants({x^3+y^3,-(x^3-y^3)^2},D6)=={1,x*y,x^2*y^2}
     )	    	 
///

-- One can check that A and B satisfy A^3=B^2=(A*B)^2=1, which are the defining
-- relations for the dihedral group of order 6 (see [A88, Chapter 27]). 

------------------------------------------------
-- Test 7:
-- Checks the dadeHSOP routine by checking that the list of polynomials output
-- has the expected output. Namely:
-- they are invariant polynimials,
-- they form a homogeneous system of parameters for the polynomial ring
-- they have degrees equal to the cardinality of the group (which should occur
-- with probability 1)*
-- *NB it is possible that dadeHSOP can run correctly and output an invariant
-- polynomial of degree strictly less than the cardinality of the group. If a
-- check on the package invariant ring reports failure of the folloing test,
-- then one should see if the test is passed upon a second attempt. Only if the
-- test fails a second time is it worth inspecting the code for errors.    
------------------------------------------------

TEST ///
setRandomSeed 0
S=QQ[x,y];
r=matrix{{0,-1},{1,0}};
s=matrix{{0,1},{1,0}};
G=generateGroup({r,s},QQ);
P=primaryInvariants(S,G,Dade=>true);
assert(
     P==apply(P,f->reynoldsOperator(f,G))
     )
assert(
     dim(S/ideal(P))==0
     )
assert(
     apply(P,degree)==toList(#P:{#G})
     )
///

-- Note that r^4=s^2=(r*s)^2=1 and r^2 =!= 1, so G is the dihedral group of
-- order 8 (see [A88, Chapter 27]).
end


restart
installPackage "InvariantRing"
check "InvariantRing"


restart
printWidth = 70; truncateOutput 140; needsPackage "InvariantRing";
A = matrix{{-1,1,0},{-1,0,1},{-1,0,0}}; B = matrix{{0,-1,1},{-1,0,1},{0,0,1}};
S = QQ[x,y,z];
G = generateGroup({A,B},QQ)
time prim1=primaryInvariants(S,G)
time prim2=primaryInvariants(S,G,Dade=>true)
apply(prim2,degree)
time sec1=secondaryInvariants(prim1,G)
time sec2=secondaryInvariants(prim2,G);
mol = molienSeries G
T = first gens ring numerator mol;
((value numerator mol)*(1-T^8)^3)//(value denominator mol)
secondaryInvariants(prim2,G,PrintDegreePolynomial=>true);
sort(2*T+T^2)
