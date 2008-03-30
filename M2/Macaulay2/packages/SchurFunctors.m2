newPackage(
     	  "SchurFunctors",
     	  Version => "0.2.5",
	  Date => "March 30, 2008",
	  Authors => {{
		    Name => "Steven V Sam",
		    Email => "ssam@berkeley.edu",
		    HomePage => "http://www.ocf.berkeley.edu/~ssam/"
		    }},
	  Headline => "computations of Schur modules and maps between them",
	  DebuggingMode => false
	  )

export({straighten, inducedOlver, standardTableaux, pieri})

--------------------------------
-- subroutines (not exported) --
--------------------------------

-- All matrices computed are in terms of the ordered bases given by 
-- symmetricBasis and standardTableaux

-- Input:
-- List L: A partition in the form {n_1, ..., n_p} for example
-- Module V: Some free module
-- Output: 
-- M^{\otimes n_1} \otimes ... \otimes M^{\otimes n_p}
-- which holds additional data of V and L in cache
symmetricPower(List, Module) := (L, V) -> (
     R := ring V;
     r := rank V;
     result = 1;
     for i in L do result = result * binomial(i+r-1,i);
     N := R^result;
     -- Let's remember that the symmetric power has structure:
     -- Cache the original module and the partition L
     N.cache.underlying = V;
     N.cache.diagram = L;
     return N;
     )

-- Input:
-- List lambda: a partition
-- Module V: a free module
-- Output:
-- Let v_1, ..., v_r be an ordered basis of V. Returns basis for 
-- symmetricpower(lambda, V) in lex ordering
symmetricBasis = (lambda, V) -> (
     if #lambda == 1 then 
     return apply(rsort compositions(rank V, lambda#0), i->{i});
     B := symmetricBasis(remove(lambda, 0), V);
     S0 := rsort compositions(rank V, lambda#0);
     Sbasis := {};
     for i in S0 do for j in B do
     Sbasis = append(Sbasis, prepend(i, j));
     return Sbasis
     )

-- Input: 
-- List T: a tableau, e.g., {{0,1,2},{2,3}}
-- Output:
-- If T not standard (weakly increasing rows, increasing columns), return 
-- first violating entry (starting from bottom to top, left to right);
-- otherwise return null
isStandard = T -> (
     i := #T-2;
     while i >= 0 do (
	  a := T#i;
	  b := T#(i+1);
	  n := #b;
	  for j from 0 to n-1 do if a#j >= b#j then return (i,j);
	  i = i-1;
	  );
     null
     )

-- Input:
-- List T: a tableau
-- Integer col: specify a column
-- Integer row{1,2}, specify two rows
-- Output:
-- Take all entries in row1 from column col to the end and all entries in
-- row2 from beginning to col, and return all possible permutations of these
-- entries (ignoring that some entries might be equal). The output is given in 
-- the form a hash table where the keys are the resulting tableau and the 
-- values are -1
shuffle = (T, col, row1, row2) -> (
     len1 := #(T#row1);
     len2 := #(T#row2);
     truncatedrow1 := (T#row1)_{0..col-2}; -- grab row1 entries
     truncatedrow2 := (T#row2)_{col..len2-1}; -- grab row2 entries
     L := join((T#row1)_{col-1..len1-1}, (T#row2)_{0..col-1});
     P := permutations L;
     output := {};
     for i in P do ( -- spit out all permutations
	  a := (for j from 0 to #T-1 list (
		    if j == row1 then sort join(truncatedrow1, i_{0..len1-col})
	       	    else if j == row2 then sort join(i_{len1-col+1..#i-1}, truncatedrow2)
	       	    else T#j
	       	    ), -1);
	  if a#0 != T then output = append(output,a);
     );
     return hashTable(output);
     )

-- Input:
-- List T: a tableau
-- Output:
-- Writes T as a linear combination of other tableaux T' s.t. T'<T if T is not 
-- standard, otherwise writes T = T. The equalities are expressed in a hash 
-- table which contains tableaux T_i as keys and their values c_i which 
-- represent coefficients: T = c_1T_1 + ... + c_nT_n
towardStandard = T -> (
     x := isStandard T;
     if x === null then return new HashTable from {T=>1};
     H := new MutableHashTable from shuffle(T, x#1+1, x#0, x#0+1);
     if H #? T then (
	  coeff := -(H#T) + 1;
	  remove(H,T);
	  prehash := {};
	  for i in keys H do 
	  prehash = append(prehash, (i, H#i / coeff));
	  return hashTable(prehash)
     	  ) 
     else return new HashTable from H
     )

-- The following functions are based on the Olver map defined in 
-- Andrzej Daszkiewicz, ``On the Invariant Ideals of the Symmetric
-- Algebra S. (V \oplus \wedge^2 V)''
-- The notation used is taken from there

-- Input:
-- List L: L = (a,b) represents a \otimes b where a,b are monomials
-- Output: 
-- The coefficients of the image of L under the tau map defined in section 2.11 
-- given in a hash table
pretauhelper = L -> (
     images := {};
     for j from 0 to #(L#1)-1 when true do (
	  if (L#1)#j > 0 then (
	       temp := {};
	       for k from 0 to #(L#1)-1 when true do 
		    if k == j then temp = append(temp, 1)
		    else temp = append(temp, 0);
	       images = append(images, ({L#0 + temp, L#1 - temp}, (L#1)#j));
	       );
	  );
     return hashTable(images)
     )

-- Input:
-- Module M: a symmetric power with underlying module V
-- List lambda: a partition
-- Integer i,j: indices with i<j
-- Output:
-- Computes the function tau_{ij}: M -> S^lambda(V) as in section 2.11 as a 
-- matrix
pretau = (M, lambda, i, j) -> (
     V := M.cache.underlying;
     Mbasis := symmetricBasis(M.cache.diagram, V);
     T := symmetricPower(lambda, V);
     Tbasis := symmetricBasis(lambda, V);
     output := {};
     for v in Mbasis do (
	  row := {};
	  H := pretauhelper({v#i, v#j});
	  for w in Tbasis do (
	       if H#?{w#i, w#j} and remove(remove(w, j), i) == remove(remove(v, j), i)
		    then row = append(row, H#{w#i, w#j})
	       else row = append(row, 0);
	       );
     	  output = append(output, row);
     	  );
     return map(T, M, transpose(output))
     )

-- Input:
-- List L: a partition
-- Integer i,j: indices with i<j
-- Output: 
-- Partition gotten from L by adding 1 to ith part, subtracting 1 from jth part
tauhelper = (L, i, j) -> (
     result := {};
     for k from 0 to #L-1 when true do
     if k == i then result = append(result, L_k+1)
     else if k == j then result = append(result, L_k-1)
     else result = append(result, L_k);
     return result
     )

-- Input:
-- List mu: a partition
-- ZZ k: an index
-- Output:
-- Subtract one from the kth (in human count, not computer count) row of mu
subtractOne = (mu, k) -> (
     result := {};
     for i from 0 to #mu-1 when true do
     if i == k-1 then result = append(result, mu_i - 1)
     else result = append(result, mu_i);
     return result
     )

-- Input:
-- Module M: M = symmetricPower(mu, V)
-- List J: A list of indices not containing 0
-- Output:
-- Computes the function tau_J: M -> S_1(V) \otimes S_lambda(V) as in section 
-- 2.12 as a matrix, where mu is lambda with a box added at row length(J)
tau = (M, J) -> (
     mu := M.cache.diagram;
     lambda := tauhelper(mu, 0, J_0);
     result := pretau(M, lambda, 0, J_0);
     N := target result;
     for i from 0 to (length J)-2 when true do (
     	  lambda = tauhelper(lambda, J_i, J_(i+1));
     	  result = pretau(N, lambda, J_i, J_(i+1)) * result;
     	  N = target result;
     	  );
     return result;
     )

-- Input: 
-- List T: a tableau
-- Integer n: number of possible labels for T
-- Output:
-- Convert T to a "monomial," which is given as an exponent vector
tableauToMonomial = (T, n) -> (
     result := {};
     for i in T do (
	  m := {};
     	  for j from 0 to n-1 when true do (
	       counter := 0;
	       for k in i do if k==j then counter = counter + 1;
	       m = append(m,counter);
	       );
	  result = append(result, m);
	  );
     return result;
     )

-- Input: 
-- List m: m is the exponent vector of a tensor product of monomials
-- Output:
-- Converts m to a tableau; inverse operation of tableauToMonomial
monomialToTableau = m -> (
     output := {};
     for i in m do (
	  row := {};
	  for j from 0 to #i-1 when true do
	  for k from 1 to i#j when true do
	  row = append(row, j);
	  output = append(output, row);
     );
     return output
)

-- Input:
-- List mu: a partition
-- Integer k: specifies the row of mu as in section 2.12
-- Module V: a free module
-- Output:
-- Computes chi_mu^lambda as in section 2.13 as a matrix
symmetricOlver = (mu, k, V) -> (
     -- delete one box from kth row
     lambda := subtractOne(mu, k);
     -- create mu_+ and lambda_+
     mu = prepend(0, mu);
     lambda = prepend(1, lambda);
     S0 := symmetricPower(mu, V);
     S1 := symmetricPower(lambda, V);
     chi := map(S1, S0, 0);
     for p from 0 to k when true do (
     	  A := apply(subsets(1..(k-1), p), s -> append(s,k));
	  for J in A do (
     	       cJ := 1;
	       for q from 0 to p-1 when true do
	       cJ = cJ * (mu_(J_q) - mu_k + k - J_q);
	       chi = chi + ((-1)^(p+1) / cJ) * tau(S0, J);
	       );
     	  );
     return chi
     )

-- This is the same as inducedOlver(List, ZZ, Module) below, except that 
-- instead of computing the whole matrix, just gives the image of the tableau t
inducedOlver = method()
inducedOlver(List, ZZ, Module, List) := (mu, K, V, t) -> (
     f := symmetricOlver(mu, K, V);
     R := ring V;
     Ssymbasis := symmetricBasis(mu, V); -- basis of S^mu
     lambda := subtractOne(mu, K);
     Tsymbasis := symmetricBasis(prepend(1, lambda), V); -- basis of S_1 \otimes S^lambda
     temp := tableauToMonomial(t, rank V);
     img := null;
     prehash := {};
     for j from 0 to #Ssymbasis-1 when true do (
	  if temp == Ssymbasis#j then img = f_{j};
	  );
     for j from 0 to #Tsymbasis-1 when true do (
	  x := ((entries img)#j)#0;
	  if x != 0 then prehash = append(prehash, (Tsymbasis#j, x));
	  );
     H := hashTable(prehash);
     memo := new MutableHashTable from {}; -- memoize straightening results
     output := hashTable({});
     for j in keys H do (
	  firstpart := monomialToTableau({j#0});
	  secondpart := monomialToTableau(remove(j,0));
	  H2 := null;
	  newhash := {};
	  coeff := H#j;
	  if memo #? secondpart then (
	       H2 = memo#secondpart;
	       for k in keys H2 do
	       newhash = append(newhash, (join(firstpart, k), H2#k));
	       H2 = hashTable(newhash);
	       )
	  else (
	       -- first straighten the tableau
	       straighten(secondpart, memo);
	       H2 = memo#secondpart;
	       for k in keys H2 do
	       newhash = append(newhash, (k, H2#k * coeff));
	       H2 = hashTable(newhash);
	       memo#secondpart = H2;
	       -- now we need to record the factor in S_1
	       newhash = {};
	       for k in keys H2 do
	       newhash = append(newhash, (join(firstpart, k), H2#k));
	       H2 = hashTable(newhash);
	       );
	  output = merge(output, H2, plus);
	  );
     return output;
     )

-------------------
-- Main routines --
-------------------

-- Input:
-- Integer dim: number of labels to be used
-- List mu: a partition
-- Output: 
-- All standard tableaux of shape mu and with labels from 0..dim-1
standardTableaux = method()
standardTableaux(ZZ, List) := (dim, mu) -> (
     if #mu == 0 then return {{}};
     output := {};
     otherrows := standardTableaux(dim, remove(mu, 0));
     firstrow := compositions(dim, mu#0);
     for i in firstrow do (
     	  temp := {};
     	  for j from 0 to #i-1 do
     	  for k from 1 to i#j when true do
     	  temp = append(temp,j);     
     	  for j in otherrows do (
	       temp2 := prepend(temp,j);
     	       if isStandard(temp2) === null then
     	       output = append(output, temp2);
	       );
     	  );
     return output
     )

-- Input:
-- List t: a tableau
-- MutableHashTable h: a hash table which memoizes recursive calls and 
-- stores the coefficients of the straightening of t into standard tableaux
straighten = method()
straighten(List, MutableHashTable) := (t, h) -> (
     t = apply(t, i -> sort i);
     if h #? t then return null;
     if isStandard(t) === null then 
     h#t = new HashTable from {t => 1};
     
     firstIter := towardStandard(t);
     H := hashTable({}); -- straightening of t
     for i in keys firstIter do (
	  coeff := firstIter#i;
     	  straighten(i, h);
	  temp := {};
	  for j in keys h#i do temp = append(temp, (j, coeff * (h#i)#j));
	  H = merge(H, hashTable(temp), plus);
	  );
     h#t = H;
     return null;
     )

-- Input:
-- List mu: a partition
-- Integer K: a row of mu as in section 2.12
-- Module V: a free module
-- Output:
-- Computes (as a matrix) the Pieri inclusion from the Schur module on mu into 
-- the tensor V \otimes \bigwedge_lambda where lambda is mu with one box from 
-- the Kth row removed, and \bigwedge_lambda denotes the Schur module of lambda
inducedOlver(List, ZZ, Module) := (mu, K, V) -> (
     f := symmetricOlver(mu, K, V);
     R := ring V;
     Ssymbasis := symmetricBasis(mu, V); -- basis of S^mu
     Sbasis := standardTableaux(rank V, mu); -- basis of wedge_mu
     S := R^(#Sbasis);     
     lambda := subtractOne(mu, K);
     Tsymbasis := symmetricBasis(prepend(1, lambda), V); -- basis of S_1 \otimes S^lambda
     Tprebasis := standardTableaux(rank V, lambda);
     Tbasis := {};
     for i from 0 to (rank V-1) when true do
     Tbasis = join(Tbasis, apply(Tprebasis, j -> join(tableauToMonomial({{i}}, rank V), j)));
     T := R^(#Tbasis);
     result := {};
     
     for t in Sbasis do (
     	  temp := tableauToMonomial(t, rank V);
     	  img := null;
     	  prehash := {};
     	  for j from 0 to #Ssymbasis-1 when true do (
	       if temp == Ssymbasis#j then img = f_{j};
	       );
     	  for j from 0 to #Tsymbasis-1 when true do (
	       x := ((entries img)#j)#0;
	       if x != 0 then prehash = append(prehash, (Tsymbasis#j, x));
	       );
     	  H := hashTable(prehash);
     	  memo := new MutableHashTable from {}; -- memoize straightening results
     	  output := hashTable({});
     	  for j in keys H do (
	       firstpart := j#0;
	       secondpart := monomialToTableau(remove(j,0));
	       H2 := null;
	       newhash := {};
	       coeff := H#j;
	       if memo #? secondpart then (
	       	    H2 = memo#secondpart;
	       	    for k in keys H2 do
	       	    newhash = append(newhash, (prepend(firstpart, k), H2#k));
	       	    H2 = hashTable(newhash);
	       	    )
	       else (
	       	    -- first straighten the tableau
	       	    straighten(secondpart, memo);
	       	    H2 = memo#secondpart;
	       	    for k in keys H2 do
	       	    newhash = append(newhash, (k, H2#k * coeff));
	       	    H2 = hashTable(newhash);
	       	    memo#secondpart = H2;
	       	    -- now we need to record the factor in S_1
	       	    newhash = {};
	       	    for k in keys H2 do
	       	    newhash = append(newhash, (prepend(firstpart, k), H2#k));
	       	    H2 = hashTable(newhash);
	       	    );
	       output = merge(output, H2, plus);
	       );
	  row := {};
	  for j in Tbasis do 
	  if output #? j then row = append(row, output#j)
	  else row = append(row, 0);
	  result = append(result, row);
	  );
     return map(T, S, transpose(result))
     )

-- Input:
-- List mu: a partition
-- List boxes: list of integers containing the order of boxes to delete from mu
-- Module V: a free module
-- Output:
-- Composes the maps given by inducedOlver by successively deleting boxes
pieri = method()
pieri(List, List, Module) := (mu, boxes, V) -> (
     result := inducedOlver(mu, boxes#0, V);
     lambda := subtractOne(mu, boxes#0);
     for i from 1 to #boxes-1 do (
	  result = (id_V ** inducedOlver(lambda, boxes#i, V)) * result;
	  lambda = subtractOne(lambda, boxes#i);
	  );
     return result
     )

-------------------
-- Documentation --
-------------------

beginDocumentation()
document {
     Key => SchurFunctors,
     Headline => "Methods for computing Schur functors and Pieri inclusions",
     PARA {
	  "Some references:"
	  },
     UL {
	  TEX "Andrzej Daszkiewicz, On the Invariant Ideals of the Symmetric Algebra $S.(V \\oplus \\bigwedge^2 V)$, J. Algebra 125, 1989, 444-473.",
	  "William Fulton, Young Tableaux: With Applications to Representation Theory and Geometry, London Math. Society Student Texts 35, 1997.",
	  "Jerzy Weyman, Cohomology of Vector Bundles and Syzygies, Cambridge University Press, 2002."
	  }
     }

document {
     Key => {standardTableaux, (standardTableaux, ZZ, List)},
     Headline => "list all standard tableaux of a certain shape with bounded labels",
     Usage => "standardTableaux(dim, mu)",
     Inputs => { 
	  "dim" => {ofClass ZZ, ", number of labels to be used"},
	  "mu" => {ofClass List, ", a partition which gives the shape"}
	  },
     Outputs => {
	  List => {"list of all standard tableaux"}
	  },
     EXAMPLE lines ///
     	  standardTableaux(3, {2,2}) -- lists all standard tableaux on the square with entries 0,1,2
	  ///
     }

document {
     Key => {straighten, (straighten, List, MutableHashTable)},
     Headline => "computers straightening of a tableau",
     Usage => "straighten(List, MutableHashTable)",
     Inputs => {
	  "t" => {ofClass List, ", a tableau to straighten; a tableau looks like {{3,4}, {1,2}} for example, where we list the entries from left to right, top to bottom"},
	  "h" => {ofClass MutableHashTable, ", where the answers should be stored"}
	  },
     "Outputs nothing, just modifies h. When looking up values, remember that the keys are stored with rows weakly increasing.",
     EXAMPLE lines ///
     	  h = new MutableHashTable from {}
	  straighten({{3,4}, {1,2}}, h)
	  h#{{3,4}, {1,2}} -- get the coefficients
	  ///
     }

document {
     Key => {inducedOlver, (inducedOlver, List, ZZ, Module)},
     Headline => "Computes a matrix representation of Pieri inclusions",
     Usage => "inducedOlver(List, ZZ, Module)",
     Inputs => {
	  "mu" => {ofClass List, ", a partition where {n_1, ..., n_r} represents the partition n_1 + ... + n_r"},
	  "K" => {ofClass ZZ, ", specifies the row from which to remove a box of mu"},
	  "V" => {ofClass Module, ", a free module to which we apply the Schur functor"}
	  },
     Outputs => {ofClass Matrix},
     EXAMPLE lines ///
     	  inducedOlver({2,1}, 1, QQ^3) -- delete a box from the first row from the shape 2+1
	  ///
     }

document {
     Key => {pieri, (pieri, List, List, Module)},
     Headline => "For Pieri inclusions involving more than one box",
     Usage => "pieri(List, List, Module)",
     Inputs => {
	  "mu" => {ofClass List, ", a partition where {n_1, ..., n_r} represents the partition n_1 + ... + n_r"},
	  "boxes" => {ofClass List, ", a list of boxes to delete from mu; done in order of the list"},
	  "V" => {ofClass Module, ", a free module to which we apply the Schur functor"}
	  },
     Outputs => {ofClass Matrix},
     EXAMPLE lines ///
     	  pieri({3,2}, {1,2}, QQ^3) -- delete box from first row, then from second row from the shape 3+2
	  ///
     }

end
loadPackage "SchurFunctors"
installPackage SchurFunctors

TEST ///
standardTableaux(3,{3,2})
///

TEST ///
h = new MutableHashTable from {}
straighten({{1,2,3},{1,1}},h)
h = new MutableHashTable from {}
straighten({{3,4},{1,2}},h)
h = new MutableHashTable from {}
straighten({{1,4},{2,3}},h)
///

TEST ///
inducedOlver({2,1},1,QQ^3)
inducedOlver({3,2},1,QQ^3,{{0,1,2},{1,2}})
///
