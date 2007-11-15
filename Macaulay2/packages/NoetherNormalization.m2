--=========================================================================--

newPackage(
     "NoetherNormalization",
     Version => "0.9", 
     Date => "Jan 18, 2007",
     Authors => {
	  {Name => "Bart Snapp", Email => "snapp@coastal.edu", HomePage => "http://ww2.coastal.edu/snapp/"},
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
     
export{noetherNormalization,LimitSequence,RandomRange} 
        
--=========================================================================--

-- initial comments: noetherNormalization takes in an ideal I of a
-- ring R over a field k such that the dimension of I in R is d (fix
-- these symbols for all comments) and returns a linear transformation
-- that puts the ideal in Noether position as well as the independent
-- variables of R.

-- comments: The procedure integralSet takes a Groebner basis G (ie. a
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

-- varPrep is the initial function we run on the Groebner basis of the
-- inputted ideal I. It returns sets U and V, with U being
-- algebraically independent and V being algebraically dependent. For
-- all prime ideals it returns a maximal algebraically independent set
-- of variables whose cardinality is equal to d.

varPrep = (X,I) -> (
     U := support (independentSets(I,Limit => 1))_0;
     (U,X - set U)
     );

--================================================== 

-- comments: We use lastCheck to check that our final Groebner basis
-- (the gb of the ideal after the change of variables) witnesses the
-- integrality of each variable that should be integral. It first
-- checks that there are no elements of the Groebner basis with support
-- in the independent variables and then that each variable that
-- should be integral after the linear transformation is integral.

lastCheck = (X,G,d) -> (
     M := gens G;
     i := 0;
     while i < numgens source M and not isSubset(support M_(0,i),toList(X_{0..d-1})) do ( 
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

inversePermutation = f -> (
     N := {};
     X := gens source f;
     U := flatten entries matrix f;
     for i to #X - 1 do (
	  for j to #U - 1 do (
	       if X_i == U_j then (
		    N = N|{X_j};
		    break;
		    );
	       );
	  );
     return map(source f, target f, N);
     );

--========================================================

-- comments: randomSum is used to make the random linear
-- transformations which are candidates for putting I in
-- noetherPosition. It takes in two lists and adds the second to the
-- first with random coefficients.


randomSum = (U,V,k,rr) -> (
     for j to #V - 1 do (
	  if rr == {0} then 
	  U = apply(U, i -> i + random(k)*V_j) else 
	  U = apply(U, i -> i + random(rr_0,rr_1)*V_j);
	  );
     return U;
     );
--========================================================


-- comments: noetherNormalization is the main method. I is passed to
-- it by the user and it's Groebner basis is immediately computed.  It
-- takes the ideal and does a random linear transformation adding to
-- the independent variables the dependent ones that are not initially
-- integral. It then checks that the transformation put the ideal in
-- Noether position. It does this bye partially computing a Groebner
-- basis for the ideal until the partially computed Groebner basis
-- witnesses the integrality of all of the dependent variables. If the
-- entire Groebner basis is computed and the integrality is never
-- witnessed then we apply another random linear transformation and
-- start the process again. While doing all this we keep track of the
-- maps and the inverses of the maps that we use.

noetherNormalization = method(Options => {Verbose => false, LimitSequence => {5,20,40,60,80,infinity}, RandomRange => {0}})
noetherNormalization(Ideal) := opts -> I -> (
     A := ring I;
     (flatA,fAtoFlatA) := flattenRing A;
     fFlatAtoA := fAtoFlatA^-1;
--   (flatA,fAtoFlatA,fFlatAtoA) := flattenRing A;
     if not isPolynomialRing A then error "expected an ideal over a polynomial ring";
     k := coefficientRing flatA;
     if not isField k then error "expected the ring to contain a field";
     R := k (monoid [gens flatA,MonomialOrder => Lex]);
     ff := map(R,flatA,gens R)*fAtoFlatA; --these maps merely change the order of the ring
     ffinverse := fFlatAtoA*map(flatA, R, gens flatA); --these maps merely change the order of the ring
     I = ff(I);
     G = gb I;
     d := dim I;
     X := sort gens R;
     (U,V) := varPrep(X,I);
     counter := 1; --counts the number of times lastCheck is called
     limitsequence := opts.LimitSequence; -- {5,20,40,60,80,infinity}; -- this is for the basiselementlimit setting for computing gb and is based on experience (and nothing else)
     done := false;
     indep := U;
     f := map(R,R,inverseSequence(U|V,X));
     finverse := map(R,R, reverse(U|V));
     J := apply(integralSet G,i -> f i); -- may need to do a gb comp.
     V = apply(V, i -> f(i)); --there might be a faster way to do this, perhaps V={x_(#U)..x_(#U+#V-1)}
     U = apply(U, i -> f(i)); -- might be faster to do U = {x_0..x_(#U-1)}
     Uold := U;
     while not done do ( 
     	  if opts.Verbose then << "--trying random transformation: " << counter << endl;
	  seqindex := 0;
     	  stuff := Uold;
	  rr := opts.RandomRange;
     	  if counter == 0 then U = randomSum(U,V-set J,k,rr);
	  if counter >= 1 then U = randomSum(U,V-set integralSet(G),k,rr);
	  stuff = stuff + (stuff - U);
    	  g := map(R,R,reverse(U|V));
	  ginverse := map(R,R,reverse(stuff|V));
	  f = g*f;
	  finverse = finverse*ginverse;
     	  while (not done and seqindex < #limitsequence) do (
	       if opts.Verbose then (<< "--trying with basis element limit: " << limitsequence_seqindex << endl;);
	       G = gb(f I, BasisElementLimit => limitsequence_seqindex); 
	       done = lastCheck(X,G,d);-- may want to define f I above, but this causes noetherNormalization to fail at the moment
     	       seqindex = seqindex + 1;
	       );
	  if counter == 5 then << "--warning: no good linear transformation found by noetherNormalization" <<endl;
	  if done or counter == 5 then(
	       ffinal = ffinverse*f*ff;
	       ffinalInverse = ffinverse*finverse*ff;	     	  
	       ffinal.cache.inverse = ffinalInverse;
               ffinalInverse.cache.inverse = ffinal;
	       X = apply(X, i -> ffinverse i);   	       
	     --  return (ffinal, ffinverse f I,map(A, k[X_{0..d-1}], X_{0..d-1}));
	       return (ffinal, ffinverse f I,X_{0..d-1});
	       );
	  counter = counter + 1;
     	  ); -- f puts the ideal into noether normal position. f inverse goes back to the original ring 
     );  

nPosition = (I,X) -> (
     dimI := dim I - 1;
     algind := support (independentSets(I,Limit => 1))_0;
     d := dim(I + ideal(X_{((#X-1)-dimI)..(#X-1)}));
     return d;
     );


homNoetherNormalization = (I,R,k,X) -> (
     d := nPosition(I,X);
     algind := support (independentSets(I,Limit => 1))_0;
     f := map(R,R,reverse inverseSequence(X-set algind|algind,X));
     U := apply(algind,i->f i);
     V := apply(X-set algind,i->f i);
     while d > 0 do (
	  g = map(R,R,V|randomSum(U,V,k,rr));
     	  J := g f I;
	  d = dim(J+ideal(apply(algind,i->f i)));
	  );
     return g*f;
     );

--======================================================================================================================

noetherNormalization(QuotientRing) := noetherNormalization(PolynomialRing) := opts -> R -> (
     if not isPolynomialRing ring ideal R then error "expected a quotient of a polynomial ring";
     noetherNormalization(ideal R, Verbose => opts.Verbose)
     );

--======================================================================================================================
--Assertions

TEST ///
uninstallPackage "NoetherNormalization"
installPackage "NoetherNormalization"
A = QQ[x_1..x_4]
I = ideal(x_1^2 + x_1*x_4+1,x_1*x_2*x_3*x_4+1)
assert( 
(f,J,X) = noetherNormalization(I) 
help noetherNormalization
X_1
x_3
--     == 
--     o4 == (map(A,A,{x_1,(3/5)*x_2+x_4,(-2)*x_2+x_3,x_2}),
--	  ideal(x_1^2+x_1*x_2+1,(-6/5)*x_1*x_2^3 + (3/5) * 
--	       ideal(x_1 * (x_2^2) * x_3)
--	       x_3
--	  +(-2)*x_1*x_2^2*x_4+x_1*x_2*x_3*x_4+1)
--	  ,map(A,QQ[x_4,x_3],{x_4,x_3}))
--o4     
 
--assert(noetherNormalization(
///


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
     Key => {noetherNormalization, (noetherNormalization,Ideal), (noetherNormalization,QuotientRing), (noetherNormalization,PolynomialRing)},
     Headline => "data for Noether normalization",
     Usage => "(f,J,X) = noetherNormalization C",
     Inputs => {
	  "C" => null => {"which is either ", ofClass Ideal, " ", TT "I", ", or ", ofClass QuotientRing, " ", TT "R/I", " where ", TT "R", " is ", ofClass PolynomialRing }
	  },
     Outputs => {
	  "f" => RingMap => {"an automorphism of ", TT "R"},
	  "J" => Ideal => { "the image of ", TT "I", " under ", TT "f"},
	  "X" => List => { "a list of variables which are algebraically independent in ", TT "R/J"},
	  },
     "The computations performed in the routine ", TT "noetherNormalization", " use a random linear change of coordinates,
     hence one should expect the output to change each time the routine is executed.",
     EXAMPLE {
	  "R = QQ[x_1..x_4];",
	  "I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);",
	  "(f,J,X) = noetherNormalization I",
	  },
     "The next example shows how when we use the lexicographical ordering, we can see the integrality of ", 
     TT "R/ f I", " over the polynomial ring in ", TT "dim(R/I)", " variables:",
     EXAMPLE {
	  "R = QQ[x_1..x_5, MonomialOrder => Lex];",
	  "I = ideal(x_2*x_1-x_5^3, x_5*x_1^3);",
          "(f,J,X) = noetherNormalization I",
          "transpose gens gb J",
	  },
     "If ", TT "noetherNormalization", " is unable to place the ideal into the desired position after a few tries, the following warning is given:",
     EXAMPLE {
	  "R = ZZ/2[a,b];",
	  "I = ideal(a^2*b+a*b^2+1);",
	  "(f,J,X) = noetherNormalization I"
	  },
     "Here is an example with the option ", TT "Verbose => true", ":",
     EXAMPLE {
	  "R = QQ[x_1..x_4];",
	  "I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);",
	  "(f,J,X) = noetherNormalization(I,Verbose => true)"
	  },
     "The first number in the output above gives the number of
     linear transformations performed by the routine while attempting to
     place ", TT "I", " into the desired position.
     The second number tells which ", TO "BasisElementLimit", " was used when computing the (partial) Groebner basis.
     By default, ", TT "noetherNormalization", " tries to use a partial
     Groebner basis. It does this by sequentially computing a Groebner
     basis with the option ", TO "BasisElementLimit", " set to
     predetermined values. The default values come from the following list:", TT "{5,20,40,60,80,infinity}", 
     ". To set the values manually, use the option ", TT "LimitSequence", ":",
     EXAMPLE {
	  "R = QQ[x_1..x_4];",
	  "I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);",
	  "(f,J,X) = noetherNormalization(I,Verbose => true,LimitSequence => {5,10})"
	  },
     "To limit the randomness of the coefficients, use the option ", TT "RandomRange", 
     ". Here is an example where the coefficients of the linear transformation are 
     random integers from ", TT "-1", " to ", TT "2", ":", 
     EXAMPLE {
	  "R = QQ[x_1..x_4];",
	  "I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);",
	  "(f,J,X) = noetherNormalization(I,Verbose => true,RandomRange => {-1,2})"
	  },
     PARA {
     "This symbol is provided by the package ", TO NoetherNormalization, "."
     }
     }

--=========================================================================--
