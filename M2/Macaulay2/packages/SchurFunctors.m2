newPackage(
     	  "SchurFunctors",
     	  Version => "0.1",
	  Date => "March 5, 2008",
	  Authors => {
	       {Name => "Steven V Sam", Email => "ssam@berkeley.edu"},
	       {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
	       },
	  Headline => "computations of Schur modules and maps between them",
	  DebuggingMode => false
	  )
     
export{ schur, schurModule }

exteriorPower(List, Module) := opts -> (L,M) -> (
     if #L == 0 then exteriorPower(0,M)
     else exteriorPower(L#0, M) ** exteriorPower(drop(L,1), M)
     )

exteriorPower(List, Matrix) := opts -> (L,f) -> (
     if #L == 0 then exteriorPower(0,f)
     else exteriorPower(L#0, f) ** exteriorPower(drop(L,1), f)
     )

Filling = new Type of BasicList

conjugate Filling := (T) -> (
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

normalize = method()
normalize Filling := (T) -> (
     -- returns (c,T'), where c is 0,1 or -1.
     -- T' is T with rows sorted
     -- c=0 for repeats in rows, else {1,-1} is sign of permutation needed to sort
     coeff := 0;
     coeffzero := false;
     T' := apply(T, t -> (
	       (c,t') := sortLen t;
	       if c < 0 then coeffzero = true;
	       coeff = coeff + c;
	       t'));
     if coeffzero 
	then (0,null) 
	else
	  (if coeff % 2 == 0 then 1 else -1, new Filling from T')
     )

sortLen = (L) -> (
     -- L is a list of integers
     -- returned: (s, L')
     -- s is the length of the permutation to place L into order
     -- s will be -1 if L contains duplicate entries
     len := 0;
     s := new MutableList from L;
     n := #s;
     for i from 0 to n-2 do
     	  for j from 0 to n-i-2 do (
	       if s#j === s#(j+1) then return (-1,L);
	       if s#j > s#(j+1) then (
		    tmp := s#(j+1);
		    s#(j+1) = s#j;
		    s#j = tmp;
		    len = len+1;
		    )
	       );
     (len, toList s))

sortSign = (L) -> (
     (len,L1) := sortLen L;
     (if len =!= -1 then (if len % 2 === 0 then 1 else -1), L1))

isStandard = (T) -> (
     i := #T-2;
     while i >= 0 do (
	  a := T#i;
	  b := T#(i+1);
	  n := #b;
	  for j from 0 to n-1 do
	    if a#j > b#j then return (i,j);
	  i = i-1;
	  );
     null
     )

exchange = (T, col1, col2, s) -> (
     -- s should be a list of positions of T#col1, that will be placed into col2
     -- The returned value is {(coeff, T')}
     -- coeff is 1 or -1.  The length of the list is 0 or 1.
     b := T#col2;
     M := new MutableList from T#col1;
     b = join(apply(#s, i -> (j := s#i; a := M#j; M#j = b#i; a)), drop(b,#s));
     (sgn, M1) := sortSign M;
     (sgnb, b1) := sortSign b;
     if sgn === null or sgnb === null then null else
     (for i from 0 to #T-1 list (
	  if i == col1 then M1 else if i == col2 then b1 else T#i
	  ), sgn*sgnb)
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
     if x === null 
       then new HashTable from {T=>1}
       else (
	    new HashTable from shuffle(T, x#1+1, x#0, x#0+1)
	    )
     )

alltab = (dim,mu) -> (
     a := subsets(dim, mu#0);
     if #mu == 1 then apply(a, x -> {x})
     else (
	  b := alltab(dim, drop(mu,1));
     	  flatten apply(a, x -> apply(b, y  -> prepend(x,y)))
	  )	  
     )

standardTableaux = (dim,mu) ->  select(alltab(dim, mu), T -> isStandard T === null)

schurModule = method()
schurModule(List,Module) := (lambda,E) -> (
     R := ring E;
     lambda = new Partition from lambda;
     mu := toList conjugate lambda;
     -- create a hash table of all tableaux: T => i (index in wedgeE)
     -- A is the list of all of these tableaux.
     A := alltab(rank E, mu);
     A = apply(A, T -> new Filling from T);
     AT := hashTable toList apply(#A, i -> A#i => i);
     -- now we create the hash table ST of all standard tableaux: T => i
     -- where the index now is that in the resulting module M
     B := positions(A, T -> isStandard T === null);
     ST := hashTable toList apply(#B, i -> A#(B#i) => i);
     -- Make the two modules of interest:
     exteriorE := exteriorPower(mu,E);
     M := source exteriorE_B;
     -- Now make the change of basis matrix exteriorE --> M and its
     -- canonical lifting
     finv := map(exteriorE, M, (id_exteriorE)_B);
     m := mutableMatrix(R, numgens M, numgens exteriorE, Dense=>false);
     sortedT := rsort A;
     scan(sortedT, T -> (
	 col := AT#T;
	 if ST#?T then (
	      -- place a unit vector in this column
	      m_(ST#T,col) = 1_R;
	      )
	 else (
	      -- this column is a combination of others
	      a := towardStandard T;
	      scan(pairs a, (U,s) -> (
			newcol := AT#(new Filling from U);
			columnAdd(m, col, s * 1_R, newcol);
			));
	 )));
     f := map(M, exteriorE, matrix m);
     M.cache.Schur = {f, finv, AT, ST};
     M)
     
schur = method()
schur(List,Matrix) := (lambda,f) -> (
     M := source f;
     N := target f;
     SM := schurModule(lambda,M);
     SN := schurModule(lambda,N);
     mu := toList conjugate new Partition from lambda;
     F := exteriorPower(mu,f);
     gM := SM.cache.Schur_1;
     gN := SN.cache.Schur_0;
     gN * F * gM
     )

beginDocumentation()
document {
     	  Key => SchurFunctors,
	  Headline => "for computing Schur functors"
	 }

document {
     	  Key => {schurModule, (schurModule, List, Module)},
	  Headline => "creates Schur module from a partition and free module",
	  SeeAlso => schur,
	  Usage => "schurModule(lambda,E)",
	  Inputs => {
	       "lambda" => List => "a list of numbers representing a partition",
	       "E" => Module => "a free module"
	       },
	  Outputs => { 
	       "M" => Module => {"the Schur functor associated to lambda applied to E. M comes with cached data
		    (f, finv, AT, ST) where f is a map from M to exterior_mu E, 
		    finv is a map from exterior_mu E to M,
		    AT is a hash table of all tableaux,
		    ST is a hash table of all standard tableaux"}
		    }
	       }
	  
document {
     	  Key => {schur, (schur, List, Matrix)},
	  Headline => "creates a map between Schur modules",
	  SeeAlso => schurModule,
	  Usage => "schur(lambda, f)",
	  Inputs => {
	       "lambda" => List => "a list of numbers representing a partition",
	       "f" => Matrix => "a map between two free modules",
	       },
	  Outputs => {
	       "F" => Matrix => {"the Schur functor associated to lambda applied to f"}
	       }
	  }
     
     
end
restart
loadPackage "SchurFunctors"
installPackage SchurFunctors
