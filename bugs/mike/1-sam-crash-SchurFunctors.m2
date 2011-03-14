--newPackage(
--     	  "SchurFunctors",
--     	  Version => "0.1",
--	  Date => "March 5, 2008",
--	  Authors => {
--	       {Name => "Steven V Sam", Email => "ssam@berkeley.edu"},
--	       {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
--	       },
--	  Headline => "computations of Schur modules and maps between them",
--	  DebuggingMode => false
--	  )
     
-- symmetric power of a free module
-- I'm cheating, this is a bad implementation
symmetricPower(List, Module) := (L,M) -> (
     R := ring M;
     r := rank M;
     result = 1;
     for i in L do result = result * binomial(i+r-1,i);
     N := R^result;
     -- let's remember that this has some structure
     -- namely, cache the original module and the partition L
     N.cache.underlying = M;
     N.cache.diagram = L;
     return N;
     )

-- pass in a module and partition, get an ordered basis
-- of its symmetric power
symmetricbasis = method()
symmetricbasis(List, Module) := (lambda, V) -> (
     if #lambda == 1 then 
     return apply(rsort compositions(rank V, lambda#0), i->{i});
     B := symmetricbasis(remove(lambda, 0), V);
     S0 := rsort compositions(rank V, lambda#0);
     Sbasis := {};
     for i in S0 do for j in B do
     Sbasis = append(Sbasis, prepend(i, j));
     return Sbasis
     )

Filling = new Type of BasicList

conjugate(Filling) := (T) -> (
     a := #T#0;
     new Filling from apply(0..a-1, i -> (
	       -- the i th element of each list (until length is too big)
	       for j from 0 to #T-1 when #T#j > i list T#j#i
	       ))
     )

-- compares two tableaux
Filling ? Filling := (T,U) -> (
     -- T and U should have the same shape
     if #T == 0 then symbol==
     else (
     	  a := T#-1;
     	  b := U#-1;
     	  i := #a-1;
     	  while i >= 0 do (
	       if a#i > b#i then return symbol>;
	       if a#i < b#i then return symbol<;
	       i = i-1;
	       );
	  drop(T,-1) ? drop(U,-1))
     )

-- return subset of rows
Filling _ List := (T,L) -> (toList T)_L

-- returns T with rows sorted
normalize = method()
normalize Filling := (T) -> (
     apply(T, t -> sort t)
     )

-- if T not standard, return violating entry (starting from bottom to top)
-- otherwise return null
isStandard = (T) -> (
     i := #T-2;
     while i >= 0 do (
	  a := T#i;
	  b := T#(i+1);
	  n := #b;
	  for j from 0 to n-1 do
	    if a#j >= b#j then return (i,j);
	  i = i-1;
	  );
     null
     )

exchange = (T, col1, col2, s) -> (
     -- s should be a list of positions of T#col1, that will be placed into col2
     -- The returned value is {(coeff, T')}
     -- coeff is 1 or -1.
     b := T#col2;
     M := new MutableList from T#col1;
     b = join(apply(#s, i -> (j := s#i; a := M#j; M#j = b#i; a)), drop(b,#s));
     M1 := sort toList M;
     b1 := sort b;
     (for i from 0 to #T-1 list (
	  if i == col1 then M1 else if i == col2 then b1 else T#i
	  ), 1)
     )

shuffle = (T, nrows, col1, col2) -> (
     -- replace the first nrows elems of col2 with all the possibles in col1.
     a := T#col1;
     b := T#col2;
     I := subsets(0..#a-1, nrows);
     select(apply(I,x -> exchange(T,col1,col2,toList x)), y -> y =!= null)
     )

-- writes T as a linear combination of other tableaux T' s.t. T'<T
-- if T is not standard
towardStandard = (T) -> (
     x := isStandard T;
     if x === null then return new HashTable from {T=>1};
     H := new MutableHashTable from shuffle(T, x#1+1, x#0, x#0+1);
     if H #? T then (
	  coeff := H#T + 1;
	  remove(H,T);
	  prehash := {};
	  for i in keys H do 
	  prehash = append(prehash, (i, H#i / coeff));
	  return hashTable(prehash)
     	  ) 
     else return new HashTable from H
     )

alltab = (dim,mu) -> (
     a := subsets(dim, mu#0);
     if #mu == 1 then apply(a, x -> {x})
     else (
	  b := alltab(dim, drop(mu,1));
     	  flatten apply(a, x -> apply(b, y  -> prepend(x,y)))
	  )	  
     )

standardTableaux = (dim,mu) ->  
select(alltab(dim, mu), T -> isStandard T === null)

-- The following functions based on Olver map defined in 
-- Andrzej Daszkiewicz, ``On the Invariant Ideals of the Symmetric
-- Algebra S. (V \oplus \wedge^2 V)''
-- The notation used is taken from there

-- create the map from 2.11 on p.449
-- I don't need this
pretauhelper2 = method()
pretauhelper2(Module, ZZ, ZZ) := (V, p, q) -> (
     S := symmetricPower({p,q}, V);
     T := symmetricPower({p+1,q-1}, V);
     -- make basis for S and T with lex ordering
     Sbasis := symmetricbasis({p,q}, V);
     Tbasis := symmetricbasis({p+1,q-1}, V);
     output := {};
     for i in Sbasis do ( -- i = (a,b) representing a \otimes b
	  images := {};
	  for j from 0 to #(i#1)-1 when true do (
	  if (i#1)#j > 0 then (
	       temp := {};
	       for k from 0 to #(i#1)-1 when true do 
		    if k == j then temp = append(temp, 1)
		    else temp = append(temp, 0);
	       images = append(images, ({i#0 + temp, i#1 - temp}, (i#1)#j));
	       );
	  );
	  H := hashTable(images);
	  row := {};
	  for j in Tbasis do
	       if H#?j then row = append(row, H#j)
	       else row = append(row, 0);
	  output = append(output,row);
	  );
     map(T, S, transpose(output)) -- target, source, matrix
     )

-- L = (a,b) representing a \otimes b
pretauhelper = method()
pretauhelper(List) := (L) -> (
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

-- tau_{ij} as in 2.11 of p.450
-- Let V be the underlying module of M
-- creates a map M -> S^lambda(V)
-- I think this is finished
pretau = method()
pretau(Module, List, ZZ, ZZ) := (M, lambda, i, j) -> (
     V := M.cache.underlying;
     Mbasis := symmetricbasis(M.cache.diagram, V);
     T := symmetricPower(lambda, V);
     Tbasis := symmetricbasis(lambda, V);
     output := {};
     for v in Mbasis do (
	  row := {};
	  H := pretauhelper({v#i, v#j});
	  for w in Tbasis do (
	       if H#?{w#i, w#j} then row = append(row, H#{w#i, w#j})
	       else row = append(row, 0);
	       );
     	  output = append(output, row);
     	  );
     return map(T, M, transpose(output))
     )
--pretau(symmetricPower({2,2,0,1},QQ^2),{2,2,1,0},2,3)

-- helper: add 1 to ith part, subtract 1 from jth part
-- finished for now
tauhelper = method()
tauhelper(List, ZZ, ZZ) := (L, i, j) -> (
     result := {};
     for k from 0 to (length L)-1 when true do
     if k == i then result = append(result, L_k+1)
     else if k == j then result = append(result, L_k-1)
     else result = append(result, L_k);
     return result
     )

-- tau_J as in 2.12 of p.450
-- Let V be the underlying module of M, mu the diagram of M,
-- creates a map from M to M to S_1 V \otimes S_lambda
-- where mu is lambda with a box added at row length(J)
-- finished for now
tau = method()
tau(Module, List) := (M, J) -> (
     lambda := M.cache.diagram;
     lambda = tauhelper(lambda, 0, J_0);
     result := pretau(M, lambda, 0, J_0);
     N := target result;
     for i from 0 to (length J)-2 when true do (
     lambda = tauhelper(lambda, J_i, J_(i+1));
     result = pretau(N, lambda, J_i, J_(i+1)) * result;
     N = target result;
     );
     return result;
     )

-- chi_mu^lambda as in 2.13 of p. 450
-- k is from 1 to #rows of mu
symmetricOlver = method()
symmetricOlver(List, ZZ, Module) := (mu, k, V) -> (
     -- delete one box from kth row
     lambda := {};
     for i from 0 to (length mu)-1 when true do
    	  if i == k-1 then lambda = append(lambda, mu_i - 1)
	  else lambda = append(lambda, mu_i);
     -- create mu_+ and lambda_+
     mu = prepend(0, mu);
     lambda = prepend(1, lambda);
     S0 := symmetricPower(mu, V);
     S1 := symmetricPower(lambda, V);
     chi := map(S1,S0,0);
     for p from 1 to k when true do (
     	  A := {};
	  if k==1 then A = {{1}}
     	  else A = apply(subsets(1..(k-1), p), s->append(s,k));
	  for J in A do (
     	       cJ := 1;
	       for q from 1 to p-2 when true do (
	       	    cJ = cJ * (mu_(J_q) - mu_k + k - J_q);
	       	    );
	       chi = chi + ((-1)^(p+1) / cJ) * tau(S0, J);
	       );
     	  );
     return chi
     )

-- convert a tableaux to a "monomial,"
-- which is really just the exponent vector
-- n is the number of labels we can use
tableauToMonomial = method()
tableauToMonomial(List, ZZ) := (T, n) -> (
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

-- convert a tensor product of monomials to a tableau
monomialToTableau = method()
monomialToTableau(List) := (m) -> (
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

-- t is a tableau
-- h is where we store computations of straighten
straighten = method()
straighten(List, MutableHashTable) := (t, h) -> (
     if h #? t then return null;
     if isStandard(t) === null then 
     h#t = new HashTable from {t => 1};
     
     firstIter := towardStandard(t);
     H := hashTable({}); --straightening of t
     for i in keys firstIter do (
     	  straighten(i, h);
	  H = merge(H, h#i, plus);
	  );
     h#t = H;
     return null;
     )

-- return the image of t, a tableau
inducedOlver = method()
inducedOlver(List, ZZ, Module, List) := (mu, k, V, t) -> (
     f := symmetricOlver(mu, k, V);
     R := ring V;
     Ssymbasis := symmetricbasis(mu, V); -- basis of S^mu
     Sbasis := standardTableaux(rank V, mu); -- basis of wedge_mu
     S := R^(#Sbasis);
     
     lambda := {};
     for i from 0 to (length mu)-1 when true do
     if i == k-1 then lambda = append(lambda, mu_i - 1)
     else lambda = append(lambda, mu_i);
     
     Tsymbasis := symmetricbasis(prepend(1, lambda), V); -- basis of S_1 \otimes S^lambda
     Tprebasis := standardTableaux(rank V, lambda);
     Tbasis := {};
     for i from 0 to rank V when true do
     Tbasis = join(Tbasis, apply(Tprebasis, j->(i,j)));
     T := R^(#Tbasis);
     result := {};
     
     temp := tableauToMonomial(t, rank V);
     img := null;
     prehash := {};
     for j from 0 to #Ssymbasis-1 when true do (
	  if temp == Ssymbasis#j then img = f_{j};
	  );
     for j from 0 to #Tsymbasis-1 when true do (
	  x := ((entries img)#j)#0;
	  if x>0 then prehash = append(prehash, (Tsymbasis#j, x));
	  );
     H := hashTable(prehash);
     memo := new MutableHashTable from {}; --memoize straightening results
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
	       newhash = append(newhash, (prepend(firstpart, k), coeff));
	       H2 = hashTable(newhash);
	       )
	  else (
	       -- first straighten the tableau
	       straighten(secondpart, memo);
	       H2 = memo#secondpart;
	       newhash = {};
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
     return output;
--     for i in Sbasis do (
--	  temp := tableauToMonomial(i, rank V);
--	  img := null;
--	  prehash := {};
--	  for j from 0 to #Ssymbasis-1 when true do (
--	       if temp == Ssymbasis#j then
--	       img = f_{j};
--	       );
--	  for j from 0 to #Tsymbasis-1 when true do (
--	       x := ((entries img)#j)#0;
--	       if x > 0 then prehash = append(prehash, (Tsymbasis#j, x));
--	       );
--	  H := hashTable(prehash); --image of i in S_1 \otimes S^lambda
--     	  );
--     return map(T,S,transpose(result));
     )
--inducedOlver({2,2},2,QQ^2,{{0,0},{1,1}})

-- The inclusion of V_mu in V_nu \otimes V_lambda where
-- where mu is obtained from lambda by adding shape nu
-- Returns a matrix
schurOlver = method()
schurOlver(Module, List, List) := (V, mu, lambda) -> (
     )

-- L is a partition of n
-- outputs all row permutations acting on L 
myShuffle = method()
myShuffle(List) := (L) -> (
     if #L == 1 then return permutations L#0;
     result := {};
     if #L == 2 then (
	  for i in permutations L#0 do
	  for j in permutations L#1 do
	  result = append(result, {i,j});
	  return result;
	  );
     rest := myShuffle(remove(L,0));
     for i in permutations L#0 do
     for j in rest do
     result = append(result, prepend(i, j));
     return result;
     )

end

beginDocumentation()
document {
     	  Key => SchurFunctors,
	  Headline => "for computing Schur functors"
	 }

restart
path=prepend("/Users/mike/src/M2/Macaulay2/bugs/mike/",path)
load "1-sam-crash-SchurFunctors.m2"
recursionLimit = 2037
inducedOlver({3,2},1,QQ^3,{{0,1,2},{1,2}})
towardStandard({{1,2,3},{1,1}})
h = new MutableHashTable from {}
time straighten({{8,9,10},{4,5,7},{2,3,6},{1}},h)
straighten({{1,2,3},{1,1}},h)
