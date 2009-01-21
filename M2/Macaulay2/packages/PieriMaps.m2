-- -*- coding: utf-8 -*-
newPackage(
    	  "SchurFunctors",
   	  Version => "0.5.0",
	  Date => "May 17, 2008",
	  Authors => {{
		    Name => "Steven V Sam",
		    Email => "ssam@mit.edu",
		    HomePage => "http://www.mit.edu/~ssam/"
		    }},
	  Headline => "computations of Schur modules and maps between them",
	  DebuggingMode => false
	  )

export({straighten, standardTableaux, pieri, saturatePieri, pureFree, schurRank})

--------------------------------
-- subroutines (not exported) --
--------------------------------

-- All matrices computed are in terms of the ordered bases given by 
-- standardTableaux

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
     otherrows := standardTableaux(dim, remove(mu, 0));
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
-- Ring P: a polynomial ring k[x_0, ..., x_(n-1)], k a field
-- Output:
-- An GL(n,k)-equivariant map P \otimes S_mu k^n -> P \otimes S_lambda k^n 
-- where lambda is mu with a box removed from the kth row, and S_mu denotes 
-- the Schur functor associated to mu
pieriHelper = method()
pieriHelper(List, ZZ, Ring) := (mu, k, P) -> (
     d := numgens P;
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
     	       	    	      U#(J_(i+1)) = remove(U#(J_(i+1)), b);
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
	       U := apply(remove(T,0), i->sort(i));
	       coeff := H#T;
	       remove(H, T);
	       straighten(U, memo);
	       for i in keys memo#U do 
	       if H #? i then H#i = H#i + coeff * (memo#U)#i * x_((T_0)_0)
	       else H#i = coeff * (memo#U)#i * x_((T_0)_0);
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
-- Module V: An n-dimensional vector space over a field k of characteristic 0
-- Output:
-- A GL(V)-equivariant map P \otimes S_mu V -> P \otimes S_lambda V where
-- P = k[x_0, ..., x_(n-1)], S_mu denotes the Schur functor associated to mu, 
-- and lambda is the partition obtained from mu by deleting boxes_0, boxes_1,
-- ..., in order.
pieri = method()
pieri(List, List, Module) := (mu, boxes, V) -> (
     R := ring V;
     d := rank V;
     P := R[x_0 .. x_(d-1)];
     result := pieriHelper(mu, boxes#0, P);
     for b from 1 to #boxes-1 do (
	  mu = subtractOne(mu, boxes#(b-1));
     	  result = pieriHelper(mu, boxes#b, P) * result;
	  );
     return result;
     )

-- Input:
-- List d: a degree sequence
-- Module V: A vector space over a field k of characteristic 0
-- Output:
-- An inclusion of GL(V)-representations whose graded minimal resolution is pure 
-- with degree sequence d
pureFree = method()
pureFree(List, Module) := (d, V) -> (
     e := {};
     for i from 1 to #d-1 do e = append(e, d#i - d#(i-1));
     counter := -#e + 1;
     for i in e do counter = counter + i;
     lambda := {counter - e#0};
     for i from 1 to #e-1 do
     lambda = append(lambda, lambda#(i-1) - e#i + 1);
     lambda = prepend(counter, remove(lambda, 0));
     pieri(lambda, toList(e#0 : 1), V)
     )

-- Same as above, except the resulting map is between modules over 
-- ZZ/p[x_0, ..., x_(n-1)] where n = dim V instead of QQ[x_0, ..., x_(n-1)]
-- The map is saturated so that the cokernel does not contain torsion
pureFree(List, Module, ZZ) := (d, V, p) -> (
     e := {};
     for i from 1 to #d-1 do e = append(e, d#i - d#(i-1));
     counter := -#e + 1;
     for i in e do counter = counter + i;
     lambda := {counter - e#0};
     for i from 1 to #e-1 do
     lambda = append(lambda, lambda#(i-1) - e#i + 1);
     lambda = prepend(counter, remove(lambda, 0));
     saturatePieri(lambda, toList(e#0 : 1), V, p)
     )

-- Produces a Pieri inclusion like the method pieri, except that we lift 
-- the map to ZZ from QQ, remove torsion from the cokernel, and then 
-- reduce modulo p
saturatePieri = method()
saturatePieri(List, List, Module, ZZ) := (mu, boxes, V, p) -> (
     d := rank V;
     f := pieri(mu, boxes, V ** QQ);
     R := ring f;
     RZ := ZZ/p[x_0 .. x_(d-1)];
     mon := apply(compositions(d, #boxes), i-> (output := 1; for j from 0 to #i-1 do output = output * x_j^(i#j); output));
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
     (D, P, Q) := smithNormalForm(intmat);
     P' := (P^(-1))_{0..(rank source D)-1};
     ((id_(RZ^((rank target P') // #mon))) ** matrix {mon}) * (RZ ** P')
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
     SeeAlso => schurRank,
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
     Headline => "computes straightening of a tableau",
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
     Key => {pieri, (pieri, List, List, Module)},
     Headline => "computes a matrix representation for a Pieri inclusion of representations of a general linear group",
     SeeAlso => {saturatePieri, pureFree},
     Usage => "pieri(List, List, Module)",
     Inputs => {
	  "mu" => {ofClass List, ", a partition (mu_1, ..., m_r) where mu_i is the number of boxes in the ith row"},
	  "boxes" => {ofClass List, ", a list of rows from which to remove boxes (specifies which map of GL(V) representations we want"},
	  "V" => {ofClass Module, ", an n-dimensional vector space over a field k of characteristic 0" }
	  },
     Outputs => {
	  Matrix => {"A GL(V)-equivariant map P otimes S_mu V -> P otimes S_lambda V where P = k[x_0, ..., x_(n-1)], S_mu denotes the Schur functor associated to mu, and lambda is the partition obtained from mu by deleting boxes_0, boxes_1, ..., in order."}
	  },
     EXAMPLE lines ///
     	  pieri({3,1}, {1}, QQ^3) -- removes the first box from the partition {3,1}
	  res coker oo -- resolve this map
	  betti oo -- check that the resolution is pure
	  ///
     }

document {
     Key => {saturatePieri, (saturatePieri, List, List, Module, ZZ)},
     Headline => "computes a matrix representation for a Pieri inclusion modulo p of representations of a general linear group",
     SeeAlso => {pureFree, pieri},
     Usage => "pieri(List, List, Module, ZZ)",
     Inputs => {
	  "mu" => {ofClass List, ", a partition (mu_1, ..., m_r) where mu_i is the number of boxes in the ith row"},
	  "boxes" => {ofClass List, ", a list of rows from which to remove boxes (specifies which map of GL(V) representations we want"},
	  "V" => {ofClass Module, ", an n-dimensional vector space over a field k of characteristic 0" },
	  "p" => {ofClass ZZ, ", a characteristic, the output is over the field ZZ/p"}
	  },
     Outputs => {
	  Matrix => {"A GL(V)-equivariant map P otimes S_mu V -> P otimes S_lambda V modulo p where P = k[x_0, ..., x_(n-1)], S_mu denotes the Schur functor associated to mu, and lambda is the partition obtained from mu by deleting boxes_0, boxes_1, ..., in order. The modulo p means that the pieri map is lifted to a map over ZZ, the torsion is removed from the cokernel, and the resulting map is reduced mod p."}
	  },
     EXAMPLE lines ///
     	  saturatePieri({3,1}, {1}, QQ^3, 5) -- removes the first box from the partition {3,1}, then reduces mod 5
	  res coker oo -- resolve this map
	  betti oo -- check that the resolution is pure (will not be pure for all characteristics)
	  ///
     }

document {
     Key => {pureFree, (pureFree, List, Module), (pureFree, List, Module, ZZ)},
     Headline => "computes an GL(V)-equivariant map whose resolution is pure, or the reduction mod p of such a map",
     SeeAlso => {pieri, saturatePieri},
     Usage => concatenate("pieri(List, Module)\n",
	  "pieri(List, Module, ZZ)"),
     Inputs => {
	  "d" => {ofClass List, ", a list of degrees (increasing numbers)"},
	  "V" => {ofClass Module, ", an n-dimensional vector space over a field of char 0"},
	  "p" => {ofClass ZZ, ", a characteristic, the output is over the field ZZ/p"}
	  },
     Outputs => {
	  Matrix => {"Either calls pieri or saturatePieri depending on if the argument p is given. The result is a map whose cokernel has Betti diagram with degree sequence d if p is not given, and is the same map reduced mod p if it is given."}
	  },
     EXAMPLE lines ///
     	  betti res coker pureFree({0,1,2,4},QQ^3) -- degree sequence {0,1,2,4}
	  betti res coker pureFree({0,1,2,4},QQ^3,2) -- same map, but reduced mod 2
     	  ///
     }

document {
     Key => {schurRank, (schurRank, ZZ, List)},
     Headline => "computes the dimension of the irreducible GL(QQ^n) representation associated to a partition",
     SeeAlso => standardTableaux,
     Usage => "schurRank(ZZ, List)",
     Inputs => {
	  "n" => {ofClass ZZ, ", the dimension of a vector space V"},
	  "mu" => {ofClass List, ", a partition (mu_1, ..., m_r) where mu_i is the number of boxes in the ith row"}
	  },
     Outputs => {
	  ZZ => {"The dimension of the irreducible GL(V) representation associated to mu, uses determinantal formula."}
	  },
     EXAMPLE lines ///
     	  schurRank(5, {4,3}) -- should be 560
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
t = new BettiTally from {(0,{0},0)=>8, (1,{1},1) =>15, (2,{3},3)=>10, (3,{5},5)=>3};
assert t == (betti res coker pieri({3,1},{1},QQ^3))
t = new BettiTally from {(0,{0},0)=>15, (1,{1},1) =>24, (2,{4},4)=>15, (3,{6},6)=>6};
assert t == (betti res coker pieri({4,1},{1},QQ^3))
t = new BettiTally from {(0,{0},0)=>20, (1,{2},2) =>60, (2,{3},3)=>64, (3,{4},4)=>20};
assert t == (betti res coker pieri({3,2},{2,2},QQ^4))
///

