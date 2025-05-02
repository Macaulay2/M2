       newPackage(
               "ChainComplexOperations",
               Version => "0.2", 
               Date => "Jan 4, 2017",
               Authors => {{Name => "David Eisenbud", 
                         Email => "de@msri.org"}},
               Headline => "sym2, wedge2, chi2 of a ChainComplex",
	       Keywords => {"Homological Algebra"},
               PackageExports => {"OldChainComplexes"},
               DebuggingMode => false
               )

       export {
	   "eulerCharacteristic",
	   "evenHomologyLength",
	   "oddHomologyLength",	   	   
	   "reverseFactors",
	   "sym2",
	   "wedge2",
	   "chi2",
	   "excess",
	   "testWalker"
	   }
///
restart
uninstallPackage"ChainComplexOperations"
installPackage"ChainComplexOperations"
check "ChainComplexOperations"
viewHelp ChainComplexOperations
///

reverseFactors = method()
reverseFactors(Module, Module, ZZ,ZZ) := (P,Q,s,t) ->(
    --regarding P as degree s and Q as degree t, produce the natural map
    --P**Q --> Q**P.
    S := ring P;
    p := rank P;
    q := rank Q;
    sgn := (-1)^(s*t);
    m := mutableMatrix(S,q*p,p*q);

    apply(p, i-> apply(q, j->
	    m_(j*p+i, i*q+j) = 1));
    ta := sgn*matrix m;
    map(Q**P,P**Q,ta)
    )

reverseFactors(ChainComplex, ChainComplex) := (F,G) ->(
    --define the iso (F**G --> G**F)
    tar := G**F;
    sour := F**G;
    Ln := symbol Ln;
    phi := for n from min sour to max sour list (
	Ln = for i from max(min G,n-max F) to min(max G,n-min F) list (
    	(tar_n)_[(i,n-i)]*reverseFactors(F_(n-i),G_i,n-i,i)*(sour_n)^[(n-i,i)]); 
    	sum Ln);
map(tar,sour,n->phi_(n-min sour))
    )

sym2 = method()
sym2 ChainComplex := F ->(
    tau := reverseFactors(F,F);
    G := F**F;
    Gs := image(id_(F**F)+tau);
    GGs := chainComplex(for i from min Gs+1 to max Gs list prune Gs.dd_i);
    GGs[-min G]) 

wedge2 = method()
wedge2 ChainComplex := F ->(
    tau := reverseFactors(F,F);
    G := F**F;
    Gs := image(id_(F**F)-tau);
    GGs := chainComplex(for i from min Gs+1 to max Gs list prune Gs.dd_i);
    GGs[-min G])

evenHomologyLength = method()
evenHomologyLength ChainComplex := F ->  (
    len := 0;
    L := for i from min F to max F list(
	if even i then len = length(HH_i F)  else len = 0;
	if len == infinity then error"length of even homology not finite";
	len);
    sum L)

oddHomologyLength = method()
oddHomologyLength ChainComplex := F ->  (
    len := 0;
    L := for i from min F to max F list(
	if odd i then len = length(HH_i F)  else len = 0;
	if len == infinity then error"length of odd homology not finite";
	len);
    sum L)

eulerCharacteristic = method()
eulerCharacteristic ChainComplex := F -> (
    len := 0;
    L := for i from min F to max F list(
	len = length(HH_i F);
	if len == infinity then error"length of homology not finite";
	len);
    sum L)

chi2 = method()
chi2 ChainComplex := F -> eulerCharacteristic sym2 F - eulerCharacteristic wedge2 F

excess = method()
excess ChainComplex := F ->(
    excess1a := 2*oddHomologyLength sym2 F;
    excess1b := 2*evenHomologyLength wedge2 F;
    G := F**F;
    excess2 := -sum(for i from min G to max G list degree HH_i(G)) +
         (length HH_0 F)*sum(for i from min F to max F list rank F_i);
    (excess1a, excess1b,excess2))

excess Module := M ->(
    F := res M;
    excess F)

testWalker = M ->(F:=res M; 
    sumbetti := sum(for i from min F to max F list rank F_i);
    (2^(codim M)*degree M + sum toList (excess M)) == (degree M)*sumbetti)


beginDocumentation()
       doc ///
       Key
         ChainComplexOperations
       Headline
         Symmetric and exterior squares of a complex and the 2nd Adams operation
       Description
         Text
	  This package implements the constructions
	  used in Mark Walker's November 2016 proof of the (weak) Buchsbaum-Eisenbud-Horrocks
	  conjecture, which states: If M is a module of codimension c
	  over a regular local ring S, then the sum of the ranks of the free modules
	  in a free resolution of M is at least 2^c. Walker's proof
	  works for rings where 2 is invertible, and in this package we work over a field
	  of characteristic $\neq 2$.
	  
	  The main new (to Eisenbud) tool in Walker's proof was the function chi2. Explicitly,
	  if F is a ChainComplex of free S-modules with finite length homology, then
	  chi2 F is the Euler characteristic of sym2 F minus that of wedge2 F.
	  The function chi2 should be regarded as the Euler characteristic of the 2nd Adams operation,
	  applied to F. It has two properties relevant for the proof:
	  1) Like the Euler characteristic of F, chi2 F is additive on
	  short exact sequences of complexes. 2) If S is a regular local ring
	  of dimension d with residue field k, then chi2 res k = 2^d.
	  
	  Sketch of Walker's proof:
	  
	  The question reduces by localization to the case where M has finite length.
	  Let F = res M, and let B be the sum of the ranks of the free modules in F.
	  Since F**F = sym2 F ++ wedge2 F, we may drop the negative terms
	  in the expression for chi2 --- the odd terms in the Euler characteristic
	  of sym2 F and the even terms in the Euler characteristic of wedge2 F --- to get
	  chi2 F \leq\ length HH(F**F). This length is evidently \leq  B*length M. 
	  On the other hand, the additivity of chi2 implies chi2 F = 2^d*length M. Thus
	  
	  	  2^d*length M = chi2 F\leq length HH(F**F) \leq B*length M

    	  QED

          Chi2 should be regarded as the Euler characteristic of
	  the second Adams operation, applied to a free Chain complex. Its additivity
	  follows from the fact that the Adams operations are ring homomorphism.
	  This is also easy to prove directly.
	  
	  It would be good to have the whole decomposition
	  of tensor powers of a module or complex under the action of the symmetric group
          (and thus also the Adams operations) available in M2. 
	  Stillman and Eisenbud have discussed
	  implementing this in the future, and anyone wishing to help with this project is
	  welcome to join (or replace!) us.
       ///

doc ///
   Key
    reverseFactors
    (reverseFactors, ChainComplex, ChainComplex)
    (reverseFactors, Module, Module, ZZ,ZZ)    
   Headline
    The isomorphism from F**G to G**F when F,G are complexes
   Usage
    phi = reverseFactors(F,G)
    phi = reverseFactors(M,N,p,q)
   Inputs
    F:ChainComplex
    G:ChainComplex    
    M:Module
    N:Module
    p:ZZ
    q:ZZ
   Outputs
    phi:ChainComplexMap
     to G**F from F**G
   Description
    Text
     maps F_{n-i}**G_i \to G_i**F_{n-i} changing the basis order and putting in sign (-1)^{i*(n-i)}.
     In reverseFactors(M,N,p,q) the integers p and q specify the homological degrees of M and N respectively. 
    Example
     S = ZZ/101[a,b]
     F = chainComplex{map(S^1,S^{-1},a)}
     G = chainComplex{map(S^1,S^{-1},b)}[3]
     phi = reverseFactors(F,G)
     G**F
     F**G
     --is it a map of complexes?
     apply(1+length(F**G), i->(
		 (phi_i)*((F**G).dd_(i+1)) ==  ((G**F).dd_(i+1))*phi_(i+1)
		 ))
     --Does reverseFactors create an isomorphism?
     apply(length (F**G), i -> (rank phi_i) == rank ((F**G)_i))
///
     
doc ///
   Key
    oddHomologyLength
    (oddHomologyLength, ChainComplex)
   Headline
    sum of the lengths of the odd degree homology groups
   Usage
    m = oddHomologyLength F
   Inputs
    F:ChainComplex
   Outputs
    m:ZZ
   Caveat
    Returns an error if any homology has infinite length
///
doc ///
   Key
    evenHomologyLength
    (evenHomologyLength, ChainComplex)
   Headline
    sum of the lengths of the even degree homology groups
   Usage
    m = evenHomologyLength F
   Inputs
    F:ChainComplex
   Outputs
    m:ZZ
   Caveat
    Returns an error if any homology has infinite length
///
doc ///
   Key
    eulerCharacteristic
    (eulerCharacteristic, ChainComplex)
   Headline
    sum of the lengths of the even degree homology minus the odd degree homology groups
   Usage
    m = eulerCharacteristic F
   Inputs
    F:ChainComplex
   Outputs
    m:ZZ
   Caveat
    Returns an error if any homology has infinite length
///
doc ///
   Key
    excess
    (excess, ChainComplex)
    (excess, Module)
   Headline
    Difference between the sum of the lengths of Tor_i(M,M) and the Walker bound 2^d*length(M)
   Usage
    exs = excess F
    exs = excess M
   Inputs
    F:ChainComplex
     with finite length homology
    M:Module
     of finite length
   Outputs
    exs:Sequence
     (excess1a, excess1b, excess2)
   Description
    Text
     The three positive summands that make up the difference (sum Betti numbers M) and 2^{codim M}
     in Walker's proof of the weak Buchsbaum-Eisenbud-Horrocks conjecture:

     excess1a = 2*oddHomologyLength sym2 F;

     excess1b = 2*evenHomologyLength wedge2 F;

     The difference between the sum of the lengths of Tor(M,M) and chi2 F
     is excess1a+excess1b.

     excess2 = (sum of the betti numbers of M)*length M - sum(length Tor_i(M,M))
    Example
     S = ZZ/101[a,b,c]
     mm = ideal vars S
     M = S^1/(mm^2)
     F = res M
     
     sumBetti = sum(4,i->rank F_i)          
     sumTor = sum(4,i->length(Tor_i(M,M)))
     chi2 F == eulerCharacteristic sym2 F-eulerCharacteristic wedge2 F
     
     2^(codim M)*(length M) == chi2 F
     sumTor - chi2 F
     sumBetti*(length M) - sumTor
     excess M
   Caveat
    Returns an error if any homology has infinite length
///
doc ///
   Key
    sym2
    (sym2, ChainComplex)
   Headline
    symmetric square of a chain complex
   Usage
    G = sym2 F
   Inputs
    F:ChainComplex
   Outputs
    G:ChainComplex
   Description
    Text
     If tau: F**F \to F**F is the chain map reversing the factors, with appropriate signs, then
     sym2 F = image(1+tau) = ker(1-tau) = coker(1-tau)

///
doc ///
   Key
    wedge2
    (wedge2, ChainComplex)
   Headline
    exterior square of a chain complex
   Usage
    G = wedge2 F
   Inputs
    F:ChainComplex
   Outputs
    G:ChainComplex
   Description
    Text
     If tau: F**F \to F**F is the chain map reversing the factors, with appropriate signs, then
     wedge2 F = image(1-tau) = ker(1+tau) = coker(1+tau)
///
doc ///
   Key
    chi2
    (chi2, ChainComplex)
   Headline
    Euler characteristic of the 2nd Adams operation applied to a complex
   Usage
    m = chi2 F    
   Inputs
    F:ChainComplex
   Outputs
    m:ZZ
   Description
    Text
     The definition:
     
     chi2 F :=  eulerCharacteristic sym2 F - eulerCharacteristic wedge2 F.
     
     Walker's proof that the sum of the Betti numbers is at least 2^{codim M),
     illustrated:
    Example
     S = ZZ/101[a,b,c]
     mm = ideal vars S
     M = S^1/(mm^2)
     F = res M
     
     sumBetti = sum(4,i->rank F_i)          
     sumTor = sum(4,i->length(Tor_i(M,M)))
     chi2 F == eulerCharacteristic sym2 F-eulerCharacteristic wedge2 F
     
     2^(codim M)*(length M) == chi2 F
     chi2 F <= sumTor
     sumTor <= sumBetti*(length M)
   Caveat
    Returns an error if any homology has infinite length
///
doc ///
   Key
    testWalker
   Headline
    tests Walker's formula
   Usage
    t = testWalker M
   Inputs
    M:Module
     of finite length
   Outputs
    t:Boolean
   Description
    Text
     Verifies Walker's Theorem for a finite length graded module over a polynomial ring of char not 2:
         (2^(codim M)*length M + sum toList (excess M)) == (sum of the betti numbers of M)*(length M)
   Caveat
    Returns an error if any homology has infinite length
///

TEST///
S = ZZ/101[a]
P= S^{0,1}
Q = S^{3,5}
s = 1;t=1
ta = reverseFactors(P,Q,s,t)
assert isHomogeneous ta
assert (reverseFactors(P,Q,s,t)*reverseFactors(Q,P,s,t) == id_(Q**P))
///

TEST///
S = ZZ/101[a,b,c]
F = chainComplex{map(S^1,S^1,0)}
assert (try eulerCharacteristic F then "finite" else "undefined" == "undefined")
///

TEST///
S = ZZ/101[a,b,c]
M = S^1/ideal{a^2,b^2,c^2}
N = S^1/((ideal gens S)^3)
betti(F = complete res M)
betti (G = complete res N)
phi = reverseFactors(F,G);
--is it a map of complexes?
assert all(apply(1+length(F**G), i->(
(phi_i)*((F**G).dd_(i+1)) ==  ((G**F).dd_(i+1))*phi_(i+1)
)),i->i == true)
--Does reverseFactors create an isomorphism?
assert all(apply(length (F**G), i -> (rank phi_i) == rank ((F**G)_i)), i->i==true)
///

end--
restart
uninstallPackage"ChainComplexOperations"
installPackage"ChainComplexOperations"
check "ChainComplexOperations"
viewHelp ChainComplexOperations

--Walker's inequality:
--If F is  a resolution of a module M of finite length, then
--(2^(codim M)*degree M + 2*(oddHomologyLength sym2 F + evenHomologyLength wedge2 F) = 
--sum(for i from min F**F to max F**F i-> degree HH_i(F**F)) <= (degree M)*sum(for i from min F to max F list rank F_i)

