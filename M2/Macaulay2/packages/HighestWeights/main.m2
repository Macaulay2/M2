--------------------------------------------------------------------------------
-- Copyright 2014  Federico Galetto
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

export {"LieWeights","setWeights","getWeights","propagateWeights",
     "GroupActing","MinimalityTest","LeadingTermTest","Forward",
     "highestWeightsDecomposition"}

--INPUT
--D: a Dynkin type
--L: a list of weights
--OUTPUT
--nothing if L is a list of weights in the weight lattice of the root system of 
--type D, an error otherwise
weightCheck = (D,L) -> (
     if not isTable(L) then (
	  error "expected all weights to have the same length";
	  );
     if (length first L != rank rootSystem D) then (
	  error ("expected the weights to have length ",
	       toString(rank rootSystem D));
	  );
     for w in L do (
	  for i in w do (
	       if not instance(i,ZZ) then (
		    error "expected weights to be lists of integers";
		    );
	       );
	  );
     )

--set (lie theoretic) weights of the variables a ring
setWeights = method(TypicalValue => Tally)
--INPUT
--R: a polynomial ring
--D: the Dynkin type (see WeylGroups) of group acting on R
--L: a list (actually a table) of Lie weights for the variables of R
--OUTPUT
--a tally with the highest weights decomposition of L
--COMMENTS
--creates a key "LieWeights" within the ring with value the output matrix
--which is used to compute Lie weights of monomials in the ring
--also creates a key "GroupActing" with value D which is used to decompose
--a list of weights into highest weights with multiplicities
setWeights (PolynomialRing, DynkinType, List) := Tally => (R,D,L) -> (
     if numgens(R)!=#L then (
	  error ("expected as many weights as generators of ",toString(R));
	  );
     weightCheck(D,L);
     R#GroupActing = D;
     R#LieWeights = matrix L;
     decomposeWeightsList(D,L)
     )

--return the weight of a ring element
getWeights = method(TypicalValue => List)
--INPUT
--r: a ring element (in a polynomial ring)
--OUTPUT
--a list of integers which is the weight of the leading monomial of r
getWeights (RingElement) := List => r -> (
     R := ring r;
     if not R#?LieWeights then (
	  error "weights of the variables not assigned";
	  );
     if r==0 then error "the weight of the zero element is undefined";
     m := matrix exponents leadMonomial r;
     w := m*(R.LieWeights);
     return flatten entries w;
     )

--operates a change of basis in the domain of the matrix so that all columns
--have different leading terms
--INPUT
--M: a matrix
--OUTPUT
--N: a matrix
--X change of basis so N*X==M is true
prepMatrix = M -> (
     --separate the columns of M by degree
     --creates a hash table with keys the degrees and values the indices of
     --the columns of M that have those degrees
     degs := partition(i->degree M_i,toList(0..numColumns(M)-1));
     --for each degree compute the GB of the submatrix of that degree
     --since the matrix is assumed to be minimal we can stop with minimal gens
     GBs := applyValues(degs,v ->gb(M_v, StopWithMinimalGenerators => true, 
	       ChangeMatrix => true));
     --get the generators of the GB for each degree
     --and the change of basis for the GB in each degree
     --if the module term ordering is position up then order columns
     --by increasing leading terms (the default), otherwise decreasing
     pos := (first select((options ring M).MonomialOrder,x->x#0==Position))#1;
     if pos==Up then (
          Bs := applyValues(GBs,v -> gens v);
          Cs := applyValues(GBs,v -> getChangeMatrix v);
          )
     else (
          Ps := applyValues(GBs,v -> sortColumns(gens v,MonomialOrder=>Descending));
          Bs = applyPairs(GBs,(k,v) -> (k,(gens v)_(Ps#k)));
          Cs = applyPairs(GBs,(k,v) -> (k,(getChangeMatrix v)_(Ps#k)));
          );
     K := keys degs;
     --takes the indices of the columns of M and orders the in the same way
     --they were sorted at the beginning
     perm := flatten apply(K,k->degs#k);
     --finds the inverse permutation so we can go back to the original ordering
     invperm := inversePermutation perm;
     --stitches together the GBs in the original order of degrees
     N := map(target M, source M, (matrix{apply(K,k->Bs#k)})_invperm);
     --stitches together the change of bases in the correct order
     C := map(source M, source M,(directSum apply(K,k->inverse(Cs#k)))^invperm_invperm);
     return (N,C);
     )

--propagate weights along a map of free modules
propagateWeights = method(Options => {MinimalityTest => true, LeadingTermTest 
	  => true, Forward => false})
--INPUT
--phi: a matrix
--W: a list of weights for the domain of phi
--OUTPUT
--V: a list of weights for the codomain of phi
--C: a matrix (change of basis so that phi*C^{-1} has all different LTs)
propagateWeights (Matrix, List) := o -> (phi,W) -> (
     if not isHomogeneous(phi) then error "expected a homogeneous map";
     --set up the matrix or its transpose
     M := phi;
     sign := 1;
     if o.Forward then (
	  M = transpose M;
	  sign = -1;
	  );
     --minimality test part 1
     if o.MinimalityTest then (
	  R := ring M;
	  S := R/ideal vars R; --coefficient field
	  f := map(S,R); --quotient map
	  if f(M) != 0 then (
	       error "expected a minimal map";
	       );
	  );
     --test whether the columns have all different leading terms
     --if they are not, operates a change of basis so that they are
     lM := leadTerm(M);
     C := id_(source M);
     lC := leadComponent(lM);
     --make a list of pairs where the first entry is the leading component
     --of a column of lM and the second entry is the leading monomial
     s := apply(#lC,i->(lC_i,leadMonomial(lM_i_(lC_i))));
     if o.LeadingTermTest == true then (
	  if #(set s) != numColumns(lM) then (
	       --here some LTs are repeated so reduce using previous function
	       (M,C) = prepMatrix(M);
	       --minimality test part 2
	       if numColumns(M) != numColumns(lM) then (
		    error "expected a minimal map";
		    );
	       lM = leadTerm M;
	       lC = leadComponent(lM);
	       s = apply(#lC,i->(lC_i,leadMonomial(lM_i_(lC_i))));
	       );
	  );
     --calculate weights of the columns
     V := apply(#s,j->W_(s_j_0) + sign*getWeights(s_j_1));
     return (V,C);
     )

--decompose resolutions and graded components into highest weights
--this method has many different applications (see below)
highestWeightsDecomposition = method(Options=>{Range=>{-infinity,infinity}})
--INPUT
--C: a chain complex of free modules
--i: an integer for a homological dimension of C
--L: a list of weights of C_i
--it is assumed that the coordinate basis of C_i is a homogeneous basis of 
--weight vectors and L#j is the weight of the j-th basis element
--OUTPUT
--a hash table with keys the homological dimensions of non zero modules of C
--and values the hw decomposition of those modules separated by degrees
--OPTIONAL
--Range: a list of 2 integers denoting the indices of the leftmost and
--rightmost modules to decompose (only used for complexes)
highestWeightsDecomposition (Complex, ZZ, List) := HashTable => o -> (C,i,L) ->(
     --define the range of the complex to be examined
     maxLeft := max{min(C),(o.Range)_0};
     maxRight := min{max(C),(o.Range)_1};
     if i<maxLeft or i>maxRight then (
	  error "expected the second argument to be an integer 
	  within the range of the complex";
	  );
     --check for free modules
     for j from maxLeft to maxRight do (
	  if not isFreeModule C_j then (
	       error "expected a complex of free modules";
	       );
	  );
     --ensure #L=rank C_i
     if rank(C_i)!=#L then (
	  error ("expected a list with "|toString(rank(C_i))|" weight(s)");
	  );
     --ensure weights in L are well defined
     D := (ring C)#GroupActing;
     weightCheck(D,L);
     --create a hash table with keys the homological dimensions of C and
     --values the weights of the corresponding free module
     h := new MutableHashTable from {i => L};
     temp := (h#i,id_(C_i));
     --if the complex is a resolution skip MinimalityTest
     if C.?Resolution then mt:=false else mt=true;
     --propagate weights backwards from homological dimension i
     for j from i+1 to maxRight when C_j!=0 do (
	  temp = propagateWeights(temp_1*C.dd_j,temp_0,MinimalityTest=>mt);
	  h#j = temp_0;
	  );
     temp = (h#i,id_(C_i));
     --propagate weights forward from homological dimension i
     for j from 1 to (i-maxLeft) when C_(i-j)!=0 do (
	  temp = propagateWeights(C.dd_(i-j+1)*transpose(temp_1),temp_0,
	       Forward=>true,MinimalityTest=>mt);
	  h#(i-j) = temp_0;
	  );
     --split each list of weights by degree, i.e.
     --create a hash table with the keys the homological dimensions of C
     --the values are hash tables with keys the degrees of the generators and
     --values the indices of the generators with that degree
     X := hashTable apply(keys h, k ->
	  (k, partition(i -> degree (C_k)_i, toList(0..rank(C_k)-1))));
     --return a hash table with the keys the homological dimensions of C
     --the values are hash tables with keys the degrees of the generators and
     --values the tallies giving the decomposition into irreducibles
     applyPairs(X, (k,x) -> (k, applyValues(x, v ->
		    decomposeWeightsList(D,(h#k)_v))))
     )

--INPUT
--C: a complex of free module where the first has rank one
--OUTPUT
--a hash table with keys the homological dimensions of non zero modules of C
--and values the hw decomposition of those modules separated by degrees
--OPTIONAL
--Range: a list of 2 integers denoting the indices of the leftmost and
--rightmost modules to decompose (only used for complexes)
--COMMENT
--This is a shortcut that can be used when resolving a quotient of a
--polynomial ring by an ideal; it uses the fact that the first module
--has rank one and therefore it is a trivial representation
highestWeightsDecomposition (Complex) := HashTable => o -> C ->(
    m := min C;
    if rank C_m != 1 then (
	error "expected the first module to have rank 1";
	);
    if m<(o.Range)_0 or m>(o.Range)_1 then (
         error ("expected the given range to be an interval 
	 containing "|toString(m));
         );
    R := ring C;
    r := numColumns(R#LieWeights);
    highestWeightsDecomposition(C,m,{toList(r:0)},Range => o.Range)
    )

--INPUT
--M: a module
--l: a (multi) degree given as a list
--w: a list of weights for the generators of M (gens in the presentation)
--OUTPUT
--the hw decomposition of the degree l part of M
highestWeightsDecomposition (Module, List, List) := Tally => o -> (M, l, w) -> (
     --ensure the numer of weights is correct
     if #w != numgens M then (
	  error "expected as many weights as generators in the presentation 
	  of M";
	  );
     --ensure weights in w are well defined
     D := (ring M)#GroupActing;
     weightCheck(D,w);
     --get a basis for M in degree l
     B := cover basis(l,M);
     temp := propagateWeights(B, w, MinimalityTest => false,
	  LeadingTermTest => false);
     decomposeWeightsList(D, temp_0)
     )

--INPUT
--M: a module
--d: an integer
--w: a list of weights for the generators of M (gens in the presentation)
--OUTPUT
--the hw decomposition of the degree d part of M
highestWeightsDecomposition (Module, ZZ, List) := Tally => o -> (M, d, w) ->
     highestWeightsDecomposition(M,{d},w)

--INPUT
--M: a module
--lo: an integer
--hi: an integer >= lo
--w: a list of weights for the generators of M (gens in the presentation)
--OUTPUT
--the hw decomposition of the graded components of M between degrees lo and hi
--I wrote a separate function for this so the weight check is not repeated
--for every degree
highestWeightsDecomposition (Module, ZZ, ZZ, List) := HashTable => o -> (M,lo,hi,w) -> (
     if lo>hi then (
	  error "expected the second argument to be smaller than the third";
	  );
     --ensure the numer of weights is correct
     if #w != numgens M then (
	  error "expected as many weights as generators in the presentation 
	  of M";
	  );
     --ensure weights in w are well defined
     D := (ring M)#GroupActing;
     weightCheck(D,w);
     --decompose degree by degree
     B := null;
     l := for d in toList(lo..hi) list (
     	  B = cover basis(d,M);
     	  temp := propagateWeights(B, w, MinimalityTest => false,
	       LeadingTermTest => false);
     	  {d, decomposeWeightsList(D, temp_0)}
	  );
     hashTable l
     )

--INPUT
--Q: a ring, a quotient of a polynomial ring with a group action
--l: a (multi)degree
--OUTPUT
--the hw decomposition of the degree l part of Q
highestWeightsDecomposition (Ring, List) := Tally => o -> (Q, l) -> (
     R := ambient Q;
     r := numColumns(R#LieWeights);
     highestWeightsDecomposition(R^1/ideal(Q),l,{toList(r:0)})
     )

--INPUT
--Q: a ring, a quotient of a polynomial ring with a group action
--d: an integer
--OUTPUT
--the hw decomposition of the degree d part of Q
highestWeightsDecomposition (Ring, ZZ) := Tally => o -> (Q, d) ->
     highestWeightsDecomposition(Q,{d})

--INPUT
--Q: a ring, a quotient of a polynomial ring with a group action
--lo: an integer
--hi: an integer >= lo
--OUTPUT
--the hw decomposition of the graded components of Q between degrees lo and hi
highestWeightsDecomposition (Ring, ZZ, ZZ) := HashTable => o -> (Q, lo, hi) -> (
     R := ambient Q;
     r := numColumns(R#LieWeights);
     highestWeightsDecomposition(R^1/ideal(Q),lo,hi,{toList(r:0)})
     )

--INPUT
--I: an equivariant ideal in a polynomial ring with a group action
--l: a (multi)degree
--OUTPUT
--the hw decomposition of the degree l part of I
highestWeightsDecomposition (Ideal, List) := Tally => o -> (I, l) -> (
     R := ring I;
     r := numColumns(R#LieWeights);
     J := ideal gens gb I;
     temp := propagateWeights(super basis(l,J), {toList(r:0)}, 
	  MinimalityTest => false, LeadingTermTest => false);
     decomposeWeightsList(R#GroupActing, temp_0)
     )

--INPUT
--I: an equivariant ideal in a polynomial ring with a group action
--d: an integer
--OUTPUT
--the hw decomposition of the degree d part of I
highestWeightsDecomposition (Ideal, ZZ) := Tally => o -> (I, d) -> 
     highestWeightsDecomposition(I,{d})

--INPUT
--I: an equivariant ideal in a polynomial ring with a group action
--lo: an integer
--hi: an integer >= lo
--OUTPUT
--the hw decomposition of the graded components of I between degrees lo and hi
highestWeightsDecomposition (Ideal, ZZ, ZZ) := HashTable => o -> (I, lo, hi) -> (
     if lo>hi then (
	  error "expected the second argument to be smaller than the third";
	  );
     R := ring I;
     r := numColumns(R#LieWeights);
     J := ideal gens gb I;
     D := R#GroupActing;
     --decompose degree by degree
     B := null;
     l := for d in toList(lo..hi) list (
     	  B = super basis(d,J);
     	  temp := propagateWeights(B,  {toList(r:0)}, MinimalityTest => false,
	       LeadingTermTest => false);
     	  {d, decomposeWeightsList(D, temp_0)}
	  );
     hashTable l
     )
