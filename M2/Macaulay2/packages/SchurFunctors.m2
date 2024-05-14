newPackage(
     	  "SchurFunctors",
     	  Version => "0.1",
	  Date => "March 5, 2008",
	  Authors => {
	       {Name => "Michael E. Stillman", 
		    Email => "mike@math.cornell.edu", 
		    HomePage => "http://www.math.cornell.edu/~mike"},
	       {Name => "Anton Leykin"},
	       {Name => "Mauricio Velasco"}
	       },
	  Headline => "Schur modules and maps between them",
	  Keywords => {"Homological Algebra", "Representation Theory"},
	  DebuggingMode => false,
	  AuxiliaryFiles=>true
     	  )
     
export{ "schur", "schurModule", "Filling", 
     "straighten", "printSchurModuleElement", "schurModulesMap", "augmentFilling", 
     "character", "splitCharacter", "characterRep", "decomposeRep"}

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

exchange(Filling, ZZ, ZZ, List) := (T, col1, col2, s) -> (
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
     M.cache#"Schur" = {f, finv, AT, ST};
     M)
     
schur = method()
schur(List,Matrix) := (lambda,f) -> (
     M := source f;
     N := target f;
     SM := schurModule(lambda,M);
     SN := schurModule(lambda,N);
     mu := toList conjugate new Partition from lambda;
     F := exteriorPower(mu,f);
     gM := SM.cache#"Schur"_1;
     gN := SN.cache#"Schur"_0;
     schurNM := gN * F * gM;
     (source schurNM).cache#"Schur" = SM.cache#"Schur";
     (target schurNM).cache#"Schur" = SN.cache#"Schur";
     schurNM
     )

---- MAURICIO and ANTON's additions ---------------------------------
net Filling := T -> netList {apply(toList T, c->stack apply(c, e->net e))};

augmentFilling=method()
augmentFilling (Filling,ZZ,ZZ):=(T,c,e)->(
     if c>=#T then join(T,{{e}})
     else new Filling from apply(#T,j->if j!=c then T#j else T#j|{e})
    )

straighten = method(TypicalValue=>Vector)
straighten (Filling, Module) := (T, M) -> (
     (c, S) := normalize T;
     if c == 0 then 0_M
     else (
	  if not M.cache#"Schur"#2#?S then error "tableau and Schur module incompatible"
	  else (
	       i := M.cache#"Schur"#2#S;
	       f := M.cache#"Schur"#0;
	       c*f*(source f)_i
	       ) 
	  ) 
     )

printSchurModuleElement = method()
printSchurModuleElement (Vector, Module) := (v,M) -> (
     l := applyPairs(M.cache#"Schur"#3, (T,i)->(i,T));
     scanKeys(l, i->  
	  if v_i != 0 then 
	  << v_i << "*" << l#i << " " );
     << endl;     
     )

schurModulesMap = method() 
schurModulesMap (Module, Module, Function) := (N,M,F) -> (
     l := applyPairs(M.cache#"Schur"#3, (T,i)->(i,T));
     matrix apply(#l, j->sum(F(l#j), a->a#0*straighten(a#1,N)))      
     )

maxFilling = method(TypicalValue=>Filling)
maxFilling (List,ZZ) := (p,d)->(
-- makes a maximal semistandard tableau filled with 0..d-1 for a given partition p      
     nCols := max p;
     new Filling from apply(nCols, c->(
	       h := #select(p,j->j>c); -- the length of the column
	       toList ((d-h)..(d-1))
	       ))     
     )
///
restart
loadPackage "SchurFunctors"
debug SchurFunctors
maxFilling({5,3,3,2}, 6)
///
character = method()
character (List, ZZ) := (L,d)->(
     L = reverse L;
     m:=#L;
     x := local x;
     R:=QQ[x_0..x_(d-1)];
     M:=map(R^d,R^d,matrix(apply(d,j->(apply(d,s->(if j==s then x_j else 0))))));
     apply(m,j->M=schur(L_j,M));
     return trace M
     )

-----------Decomposition of representations into irreducibles

Specialization=(M,F)->(
     EV:=map(ring M, ring F, matrix{flatten entries M}),
     return EV(F))


Identity=(d)->(
matrix(apply(d,i->apply(d,j->if i==j then 1_QQ else 0_QQ))))

Transvection=(param1,param2,d)->(
     M1:=matrix(apply(d,i->apply(d,j->if i==j then 1_QQ else 0_QQ)));
     M2:=matrix(apply(d,i->apply(d,j->if i==param1 and j==param2 then 1_QQ else 0_QQ)));
     M1+M2
     )

Transvections=(d)->(
     L:=flatten apply(d,j->(apply(d,s->if j>s then Transvection(s,j,d))));
     select(L,a->if a=!=null then true)
     )
     
diagonal=(L)->(
     d:=#L;
     matrix(apply(d,j->(apply(d,i->if i==j then L_i else 0))))
     )

findSubRep = method()
findSubRep (List,Matrix) := (p,F)->(
     d := round sqrt numgens ring F;
     T := maxFilling(p,d);
     Trans:=Transvections(d);
     TransEval:=apply(Trans,M->Specialization(M,F));
     TE:=matrix apply(TransEval,k->{k-Specialization(Identity(d),F)});
     D:=apply(d,j->(j+1)_QQ);
     M:=Specialization(diagonal(D),F)-weight(T,D)*Specialization(Identity(d),F);
     return syz(TE||M)
     )

characterRep = method()
characterRep Matrix := F->(
     d := round sqrt numgens ring F;
     x := symbol x;
     R := QQ[x_0..x_(d-1)];
     D := diagonalMatrix gens R;
     trace Specialization(D, F)
     ) 
decomposeRep = method()
decomposeRep Matrix := F -> (
     -- extremely readable code to follow...
     ir := flatten@@exponents \flatten entries first coefficients splitCharacter characterRep F;
     new HashTable from apply(ir, p->p=>findSubRep(p,F))
     ); 
///
restart
loadPackage "SchurFunctors"
debug SchurFunctors

R=QQ[w_1..w_9]
F=genericMatrix(R,3,3)
G=schur({4},schur({2},F));
splitCharacter characterRep G
decomposeRep G
///

weight = method()
weight (Filling, List) := (T,D) -> product(flatten toList T, i->D#i);

///
restart
loadPackage "SchurFunctors"
debug SchurFunctors
T = maxFilling({4,3,3,2}, 6)
weight(T, {1,1,1,1,1,2})
///

simpleFind=(d,F)->(
     Trans:=Transvections(d);
     TransEval:=apply(Trans,M->Specialization(M,F));
     T:=matrix apply(TransEval,k->{k-Specialization(Identity(d),F)});
     return syz(T)
     )



needsPackage "SymmetricPolynomials"
needsPackage "SchurRings"
-- schurRings interface has changed at version 0.5
schurVersion = value SchurRings.Options.Version

splitCharacter = method()
splitCharacter RingElement := ce -> (
     pe:=elementarySymmetric(ce);  
     -- Assumption: ring of pe: vars 0..n-1 
     --   are orig vars, n..2n-1 are elem symm fcns
     n:=numgens source vars ring ce;
     R2 := if schurVersion < .5 then (
          error "need SchurRings, version > 0.5";
     	  --symmRing n -- vars 0..n-1 are elem symm fcns
          )
     else 
     	  symmetricRing(coefficientRing ring pe, n); -- vars 0..n-1 are elem symm fcns
     es := (vars R2)_{0..n-1};
     toS substitute(pe,es|es)
     )

beginDocumentation()
document {
     	  Key => SchurFunctors,
	  Headline => "for computing Schur functors",
	  "This package provides methods for computing Schur functors."
	 }

doc get (currentFileDirectory | "SchurFunctors/schurModule.txt")
doc get (currentFileDirectory | "SchurFunctors/schur.txt")
doc get (currentFileDirectory | "SchurFunctors/straightenSchur.txt")
doc get (currentFileDirectory | "SchurFunctors/schurModulesMap.txt")
doc get (currentFileDirectory | "SchurFunctors/character.txt")
doc get (currentFileDirectory | "SchurFunctors/splitCharacter.txt")

TEST ///
      M = schurModule({2,2,2}, QQ^4)
      assert(rank M == 10)
      (f, finv, AT, ST) = toSequence M.cache#"Schur";
      assert(f*finv == map(QQ^10))
      -- straighten 
      M = schurModule({1,1,1}, QQ^4);
      v = straighten(new Filling from {{3,2,1}}, M)
      assert(v == vector{0_QQ,0,0,-1}) 
///     

TEST ///
c=character({{1,1,1},{2}},4)
assert ( splitCharacter(c) == s_(4,1,1)+s_(3,3) )
///

end
restart
loadPackage "SchurFunctors"
debug SchurFunctors
help schurModule
help schur
installPackage "SchurFunctors"

-- Discussion with Mike, Anton, Mauricio 3/26/09 about what needs to be done for this package.
1. write mathematical documentation for schur and schurModule, explaining our conventions
  Mauricio will do this by Sunday, April 5.
2. document the type Filling, and implement some functions for it:
  -- check whether a filling is standard, semi-standard
  -- transpose
  -- generate all (as a list) standard, semi-standard
  -- to/from lists
  -- pretty printing (done already).
  -- example in doc: should show how this related to a basis for schurModules
3. other functions need doc or examples:
  character, splitCharacter (maybe the name should change)
4. decomposeRep, characterRep, augmentFilling -- need documentation
