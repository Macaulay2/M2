-- -*- coding: utf-8 -*-

--------------------------------------------------------------------------------
-- Copyright 2008, 2009  Steven V Sam
--
-- You may redistribute this program under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 3 of
-- the License, or any later version.
--------------------------------------------------------------------------------

newPackage(
    	  "PieriMaps",
   	  Version => "1.0",
	  Date => "July 3, 2009",
	  Certification => {
	       "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	       "journal URI" => "https://msp.org/jsag/",
	       "article title" => "Computing inclusions of Schur modules",
	       "acceptance date" => "2009-06-27",
	       "published article URI" => "https://msp.org/jsag/2009/1-1/p02.xhtml",
	       "published article DOI" => "10.2140/jsag.2009.1.5",
	       "published code URI" => "https://msp.org/jsag/2009/1-1/jsag-v1-n1-x02-code.zip",
	       "release at publication" => "38e96fec660168d488ad0449f8632e6608cc9ede",
	       "version at publication" => "1.0",
	       "volume number" => "1",
	       "volume URI" => "https://msp.org/jsag/2009/1-1/"
	       },
	  Authors => {{
		    Name => "Steven V Sam",
		    Email => "ssam@math.mit.edu",
		    HomePage => "http://math.mit.edu/~ssam/"
		    }},
	  Headline => "maps between representations of the general linear group based on the Pieri formulas",
	  Keywords => {"Representation Theory"},
	  DebuggingMode => false
	  )

export {"straighten", "standardTableaux", "pieri", "pureFree", "schurRank"}

--------------------------------
-- subroutines (not exported) --
--------------------------------

-- All matrices computed are in terms of the ordered bases given by 
-- standardTableaux

-- Input:
-- List L
-- Output:
-- True if L is weakly decreasing, false otherwise
isDecreasing = L -> (
     for i from 0 to #L-2 do if L#i < L#(i+1) then return false;
     true
     )

-- Input:
-- List L
-- Output:
-- True if L is strictly increasing, false otherwise
isIncreasing = L -> (
     for i from 0 to #L-2 do if L#i >= L#(i+1) then return false;
     true
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
     P = apply(P, i-> (for j from 0 to #T-1 list (
		    if j == row1 then sort join(truncatedrow1, i_{0..len1-col})
	       	    else if j == row2 then sort join(i_{len1-col+1..#i-1}, truncatedrow2)
	       	    else T#j)));
     coeff := 0;
     for i in P do if i == T then coeff = coeff + 1;
     for i in P do if i != T then output = append(output, (i, -1 / coeff));
     return hashTable(plus, output);
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

-------------------
-- Main routines --
-------------------

-- Returns dual partition
dualPart = method()
dualPart(List) := mu -> (
     result := {};
     for i from 1 to mu#0 when true do (
	  counter := 0;
     	  for j from 0 to #mu-1 when true do if mu#j >= i then counter = counter + 1;
	  result = append(result, counter);
	  );
     return result;
     )

-- Computes the dimension of S_mu V where V has dimension n and 
-- S_mu is the Schur functor associated to mu
schurRank = method()
schurRank(ZZ, List) := (n, mu) -> (
     M := mu;
     if #M < n then M = mu|toList(n-#M:0);
     det map(ZZ^n, ZZ^n, (i,j) -> binomial(M_i + n - 1 - i + j, n-1))
     )

-- Input:
-- Integer dim: number of labels to be used
-- List mu: a partition
-- Output: 
-- All standard tableaux of shape mu and with labels from 0..dim-1
standardTableaux = method()
standardTableaux(ZZ, List) := (dim, mu) -> (
     if #mu == 0 then return {{}};
     output := {};
     otherrows := standardTableaux(dim, drop(mu, 1));
     firstrow := rsort compositions(dim, mu#0);
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
straighten(List) := t -> (
     t = apply(t, i->sort(i));
     h := new MutableHashTable from {};
     straighten(t, h);
     h#t
     )

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
     	  temp = {};
	  for j in keys H do if H#j != 0 then temp = append(temp,(j,H#j));
	  H = hashTable(temp);
	  );
     h#t = H;
     return null;
     )

-- Input:
-- List mu: A partition
-- ZZ k: a number specifying which row to remove a box from mu
-- PolynomialRing P: the coefficient ring for the Pieri map over a field K
-- Output:
-- An GL(n,K)-equivariant map P \otimes S_mu K^n -> P \otimes S_lambda K^n 
-- where lambda is mu with a box removed from the kth row, and S_mu denotes 
-- the Schur functor associated to mu
pieriHelper = method()
pieriHelper(List, ZZ, PolynomialRing) := (mu, k, P) -> (
     d := numgens P;
     X := gens P;
     output := {};
     Sbasis := standardTableaux(d, mu);
     Tbasis := standardTableaux(d, subtractOne(mu, k));
     mu = prepend(0,mu);
     A := {};
     for p from 0 to k when true do A = join(A, apply(subsets(1..(k-1), p), s -> prepend(0, append(s, k))));
     for s in apply(Sbasis, i->prepend({},i)) do (
	  row := {};
     	  H := new HashTable from {};
     	  for J in A do (
	       cJ := 1;
	       for q from 1 to #J-2 when true do cJ = cJ * (mu_(J_q) - mu_k + k - J_q);
     	       h := hashTable({(s, (-1)^#J / cJ)});
	       for i from 0 to #J-2 when true do (
	       	    temp := {};
	       	    for T in keys h do (
		    	 for b from 0 to #(T_(J_(i+1)))-1 when true do (
		    	      U := new MutableList from T;
     	       	    	      U#(J_(i+1)) = drop(U#(J_(i+1)), {b, b});
			      U#(J_i) = append(U#(J_i), (T_(J_(i+1)))_b);
			      temp = append(temp, (new List from U, h#T));
			      );
		    	 );
	       	    h = hashTable(plus, temp);
	       	    );
	       H = merge(H, h, plus);
	       );
     	  H = new MutableHashTable from H;
     	  memo := new MutableHashTable from {};
     	  for T in keys H do (
	       U := apply(drop(T,1), i->sort(i));
	       coeff := H#T;
	       remove(H, T);
	       straighten(U, memo);
	       for i in keys memo#U do 
	       if H #? i then H#i = H#i + coeff * (memo#U)#i * X_((T_0)_0)
	       else H#i = coeff * (memo#U)#i * X_((T_0)_0);
	       );
     	  for t in Tbasis do if H #? t then row = append(row, H#t) else row = append(row, 0);
	  output = append(output, row);
	  );
     return map(P^(#Tbasis), P^{#Sbasis:-1}, transpose output);
     )

-- Input:
-- List mu: A partition
-- List boxes: A list of numbers which specifies the rows that boxes are to be 
-- removed from mu
-- PolynomialRing P: a polynomial ring over a field K with n generators
-- Output:
-- A GL(V)-equivariant map P \otimes S_mu V -> P \otimes S_lambda V, where
-- S_mu denotes the Schur functor associated to mu, and lambda is the 
-- partition obtained from mu by deleting boxes_0, boxes_1, ..., in order.
pieri = method()
pieri(List, List, PolynomialRing) := (mu, boxes, P) -> (
     -- check if row indices are okay
     for i in boxes do if i < 1 or i > #mu then error "The second argument must specify row indices of the partition in the first argument.";
     -- check if removal of boxes gives a partition
     lambda := mu;
     for i in boxes do (
	  lambda = subtractOne(lambda, i);
	  if not isDecreasing(lambda) then error "The second argument needs to specify a horizontal strip in the first argument.";
	  );
     -- check if mu/lambda is a horizontal strip
     for i from 0 to #mu-2 do 
     if lambda#i < mu#(i+1) then error "The second argument needs to specify a horizontal strip in the first argument.";
     -- error checking done
     if char P == 0 then pierizero(mu, boxes, P)
     else pierip(mu, boxes, P)
     )

pierizero = method()
pierizero(List, List, PolynomialRing) := (mu, boxes, P) -> (
     R := coefficientRing(P);
     d := numgens(P);
     result := pieriHelper(mu, boxes#0, P);
     for b from 1 to #boxes-1 do (
	  mu = subtractOne(mu, boxes#(b-1));
     	  result = pieriHelper(mu, boxes#b, P) * result;
	  );
     return result;
     )

pierip = method()
pierip(List, List, PolynomialRing) := (mu, boxes, P) -> (
     X := gens(P);
     d := numgens(P);
     R := QQ[X];
     f := pierizero(mu, boxes, R);
     mon := apply(compositions(d, #boxes), i-> (output := 1; for j from 0 to #i-1 do output = output * X_j^(i#j); output));
     result := {};
     denom := 1;
     for i from 0 to (rank source f)-1 do (
	  row := flatten entries f_{i};
	  newrow := {};
	  for j from 0 to #row-1 do 
	  for k in mon do (
	       coeff := coefficient(k, row#j);
	       temp := ceiling(1 / gcd(coeff, 1));
	       denom = denom * temp / gcd(denom, temp);
	       newrow = append(newrow, coeff);
	       );
	  result = append(result, newrow);
	  );
     result2 := {};
     for i in result do 
     result2 = append(result2, apply(i, j -> round(j*denom)));
     intmat := map(ZZ^((rank target f) * #mon), ZZ^(rank source f), transpose(result2));
     (D, S, T) := smithNormalForm(intmat);
     S' := (S^(-1))_{0..(rank source D)-1};
     ((id_(P^((rank target S') // #mon))) ** matrix {mon}) * (P ** S')
     )

-- Input:
-- List d: a degree sequence
-- Module V: A vector space over a field k of characteristic 0
-- Output:
-- An inclusion of GL(V)-representations whose graded minimal resolution is pure 
-- with degree sequence d
pureFree = method()
pureFree(List, PolynomialRing) := (d, P) -> (
     if not isIncreasing(d) then error "The first argument needs to be a strictly increasing list of degrees.";
     e := {};
     for i from 1 to #d-1 do e = append(e, d#i - d#(i-1));
     counter := -#e + 1;
     for i in e do counter = counter + i;
     lambda := {counter - e#0};
     for i from 1 to #e-1 do
     lambda = append(lambda, lambda#(i-1) - e#i + 1);
     lambda = prepend(counter, drop(lambda, 1));
     pieri(lambda, toList(e#0 : 1), P) ** P^{-d#0}
     )

-------------------
-- Documentation --
-------------------

beginDocumentation()
document {
     Key => PieriMaps,
     Headline => "Pieri inclusions",
     "For mathematical background of this package and some examples of use, see:",
     BR{},
     "Steven V Sam, Computing inclusions of Schur modules, arXiv:0810.4666",
     BR{},
     "Some other references:",
     BR{},
     "Andrzej Daszkiewicz, On the Invariant Ideals of the Symmetric Algebra $S.(V \\oplus \\wedge^2 V)$, J. Algebra 125, 1989, 444-473.",
     BR{},
     "David Eisenbud, Gunnar Fl\\o ystad, and Jerzy Weyman, The existence of pure free resolutions, arXiv:0709.1529.",
     BR{},
     "William Fulton, Young Tableaux: With Applications to Representation Theory and Geometry, London Math. Society Student Texts 35, 1997.",
     BR{},
     "Jerzy Weyman, Cohomology of Vector Bundles and Syzygies, Cambridge University Press, 2002.",
     BR{},
     BR{},     
     "Let V be a vector space over K. If K has characteristic 0, then
     given the partition mu and the partition mu' obtained from mu by
     removing a single box, there is a unique (up to nonzero scalar)
     GL(V)-equivariant inclusion S_mu V -> V otimes S_mu' V, where
     S_mu refers to the irreducible representation of GL(V) with
     highest weight mu. This can be extended uniquely to a map of P =
     Sym(V)-modules P otimes S_mu V -> P otimes S_mu' V. The purpose
     of this package is to write down matrix representatives for these
     maps. The main function for doing so is ",
     TO pieri,
     ". Here is an example of the use of the package PieriMaps, which is also
     designed to check whether the maps are being constructed correctly
     (important, since it is notoriously difficult to get the signs and
     coefficients right.) ",
     "We will construct by hand the free resolution in 3 variables corresponding
     to the degree sequence (0,2,3,6). Let's start with the packaged code:",
     EXAMPLE lines ///
     R = QQ[a,b,c];
     f = pureFree({0,2,3,6}, R)
     betti res coker f
     ///,
     "By the general theory from Eisenbud-Fl\\o ystad-Weyman, each of these free
     modules should be essentially Schur functors corresponding to the
     following partitions.",
     EXAMPLE lines ///
     needsPackage "SchurRings"
     schurRing(s,3)
     dim s_{2,2}
     dim s_{4,2}
     dim s_{4,3}
     dim s_{4,3,3}
     ///,
     "This package also provides a routine ",
     TO schurRank,
     " for computing this dimension:",
     EXAMPLE lines ///
     schurRank(3, {2,2})
     schurRank(3, {4,2})
     schurRank(3, {4,3})
     schurRank(3, {4,3,3})
     ///,
     "We now use ",
     TO pieri, 
     " to construct each of the maps of the resolution separately.",
     EXAMPLE lines ///
     f1 = pieri({4,2,0},{1,1}, R)
     f2 = pieri({4,3,0},{2}, R)
     f3 = pieri({4,3,3},{3,3,3}, R)
     ///,
     "Fix the degrees (i.e. make sure that the target of f2 is the source of f1,
     etc). Otherwise the test of exactness below would fail.",
     EXAMPLE lines ///
     f1
     f2 = map(source f1,,f2)
     f3 = map(source f2,,f3)
     f1 * f2
     f2 * f3
     ///,
     "Check that the complex is exact.",
     EXAMPLE lines ///
     ker f1 == image f2
     ker f2 == image f3
     ///,
     "Looks great! Now let's try it modulo some prime numbers and see if we get exactness.",
     EXAMPLE lines ///
     p = 32003
     R = ZZ/p[a,b,c];
     f1 = pieri({4,2,0},{1,1},R)
     betti res coker f1
     f2 = pieri({4,3,0},{2},R)
     f3 = pieri({4,3,3},{3,3,3},R)
     f2 = map(source f1,,f2)
     f3 = map(source f2,,f3)
     f1 * f2
     f2 * f3
     ker f1 == image f2
     ker f2 == image f3
     ///,
     "These do not piece together well. The reason is that ",
     TO pieri,
     " changes the bases of the free modules in a way which is not invertible
     (over ZZ) when the ground field has positive characteristic.",
  SeeAlso => {pieri, pureFree, schurRank}
  }     

document {
     Key => {standardTableaux, (standardTableaux, ZZ, List)},
     Headline => "list all standard tableaux of a certain shape with bounded labels",
     SeeAlso => schurRank,
     Usage => "standardTableaux(dim, mu)",
     Inputs => { 
	  "dim" => {ofClass ZZ, ", number of labels to be used"},
	  "mu" => {ofClass List, ", a partition which gives the shape"}
	  },
     Outputs => {
	  List => {"list of all standard tableaux of shape mu with labels 0,...,dim-1"}
	  },
     EXAMPLE lines ///
     	  standardTableaux(3, {2,2}) -- lists all standard tableaux on the 2x2 square with entries 0,1,2
	  ///
     }

document {
     Key => {straighten, (straighten, List), (straighten, List, MutableHashTable)},
     Headline => "computes straightening of a tableau",
     Usage => concatenate("straighten(t)", "straighten(t, h)"),
     Inputs => {
	  "t" => {ofClass List, ", a tableau to straighten; a tableau looks like {{3,4}, {1,2}} for example, where we list the entries from left to right, top to bottom"},
	  "h" => {ofClass MutableHashTable, ", where the answers should be stored"}
	  },
     Consequences => { "If provided, the hashtable h is updated with any calculations which are performed as a result of calling this function. " },
     "If a hashtable h is provided, then this outputs nothing, it simply just modifies h. When looking up values, remember that the keys are stored with rows weakly increasing. ",
     "If no hashtable is provided, then the user is simply given the straightening of the tableau in terms of semistandard tableaux. ",
     "The answer is in the form a hashtable: each key is a semistandard tableaux, and the value of the key is the coefficient of that semistandard tableaux used to write the input t as a linear combination. ",
     EXAMPLE lines ///
     	  h = new MutableHashTable from {}
	  straighten({{3,4}, {1,2}}, h)
	  h#{{3,4}, {1,2}} -- get the coefficients
	  straighten({{3,4}, {1,2}}) -- just get the answer instead
	  ///
     }

document {
     Key => {pieri, (pieri, List, List, PolynomialRing)},
     Headline => "computes a matrix representation for a Pieri inclusion of representations of a general linear group",
     SeeAlso => {pureFree},
     Usage => "pieri(mu, boxes, P)",
     Inputs => {
	  "mu" => {ofClass List, ", a partition (mu_1, ..., mu_r) where mu_i is the number of boxes in the ith row"},
	  "boxes" => {ofClass List, ", a list of rows from which to remove boxes (boxes are always removed from the end of the row). This specifies which map of GL(V) representations we want. The row indices start from 1 and not 0, and this must specify a horizontal strip in mu (see description below). "},
	  "P" => {ofClass PolynomialRing, ", a polynomial ring over a field K in n variables" }
	  },
     Outputs => {
	  Matrix => {"If K has characteristic 0, then given the partition mu and the partition mu' obtained from mu by removing a single box, 
	       there is a unique (up to nonzero scalar) GL(V)-equivariant inclusion S_mu V -> V otimes S_mu' V, where S_mu refers to the 
	       irreducible representation of GL(V) with highest weight mu. This can be extended uniquely to a map of P = Sym(V)-modules 
	       P otimes S_mu V -> P otimes S_mu' V. This method computes the matrix representation for the composition of maps that one obtains by 
	       iterating this procedure of removing boxes, i.e., the final output is a GL(V)-equivariant map P otimes S_mu V -> P otimes S_lambda V 
	       where lambda is the partition obtained from mu by deleting a box from row boxes_0, a box from row boxes_1, etc.
	       If K has positive characteristic, then the corresponding map is calculated over QQ, lifted to a ZZ-form of the representation which has 
	       the property that the map has a torsion-free cokernel over ZZ, and then the coefficients are reduced to K."
	       }
	  },
     "Convention: the partition (d) represents the dth symmetric power, while the partition (1,...,1) represents the dth exterior power. ",
     "Using the notation from the output, mu/lambda must be a horizontal strip. Precisely, this means that lambda_i >= mu_(i+1) for all i. 
     If this condition is not satisfied, the program throws an error because a nonzero equivariant map of the desired form will not exist. ",
     EXAMPLE lines ///
     	  pieri({3,1}, {1}, QQ[a,b,c]) -- removes the last box from row 1 of the partition {3,1}
	  res coker oo -- resolve this map
	  betti oo -- check that the resolution is pure
	  ///
     }

document {
     Key => {pureFree, (pureFree, List, PolynomialRing)},
     Headline => "computes a GL(V)-equivariant map whose resolution is pure, or the reduction mod p of such a map",
     SeeAlso => {pieri},
     Usage => "pureFree(d, P)",
     Inputs => {
	  "d" => {ofClass List, ", a list of degrees (increasing numbers)"},
	  "P" => {ofClass PolynomialRing, ", a polynomial ring over a field K in n variables" }
	  },
     Outputs => {
	  Matrix => {"A map whose cokernel has Betti diagram with degree sequence d if K has characteristic 0. 
	       If K has positive characteristic p, then the corresponding map is calculated over QQ and is lifted to a ZZ-form which is then reduced mod p." }
	  },
     "The function translates the data of a degree sequence d for a desired pure free resolution into the data of a Pieri map 
     according to the formula of Eisenbud-Fl\\o ystad-Weyman and then applies the function ",
     TO pieri,
     ".",
     EXAMPLE lines ///
     	  betti res coker pureFree({0,1,2,4}, QQ[a,b,c]) -- degree sequence {0,1,2,4}
	  betti res coker pureFree({0,1,2,4}, ZZ/2[a,b,c]) -- same map, but reduced mod 2
	  betti res coker pureFree({0,1,2,4}, GF(4)[a,b,c]) -- can also use non prime fields
     	  ///
     }

document {
     Key => {schurRank, (schurRank, ZZ, List)},
     Headline => "computes the dimension of the irreducible GL(QQ^n) representation associated to a partition",
     SeeAlso => standardTableaux,
     Usage => "schurRank(n, mu)",
     Inputs => {
	  "n" => {ofClass ZZ, ", the size of the matrix group GL(QQ^n)"},
	  "mu" => {ofClass List, ", a partition (mu_1, ..., m_r) where mu_i is the number of boxes in the ith row"}
	  },
     Outputs => {
	  ZZ => {"The dimension of the irreducible GL(QQ^n) representation associated to mu"}
	  },
     "The dimension is computed using the determinantal formula given by the Weyl character formula.",
     EXAMPLE lines ///
     	  schurRank(5, {4,3}) -- should be 560
     	  ///
     }     	  
     
TEST ///
t = new BettiTally from {(0,{0},0)=>8, (1,{1},1) =>15, (2,{3},3)=>10, (3,{5},5)=>3};
assert (t == (betti res coker pieri({3,1},{1},QQ[a,b,c])))
t = new BettiTally from {(0,{0},0)=>15, (1,{1},1) =>24, (2,{4},4)=>15, (3,{6},6)=>6};
assert (t == (betti res coker pieri({4,1},{1},QQ[a,b,c])))
t = new BettiTally from {(0,{0},0)=>20, (1,{2},2) =>60, (2,{3},3)=>64, (3,{4},4)=>20};
assert (t == (betti res coker pieri({3,2},{2,2},QQ[a,b,c,d])))
assert(schurRank(5, {4,3}) == 560)
t = new BettiTally from {(0,{0},0) => 3, (1,{1},1)=>8, (2,{2},2) => 6,
(3,{4},4)=>1};
assert(t == (betti res coker pureFree({0,1,2,4}, GF(4)[a,b,c])))
///

end     
loadPackage "PieriMaps"
installPackage PieriMaps
