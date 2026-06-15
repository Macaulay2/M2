newPackage(
     	  "SchurFunctors",
     	  Version => "1.0",
	  Date => "April 18, 2026",
	  Authors => {
	       {Name => "Michael E. Stillman",
		    Email => "mike@math.cornell.edu",
		    HomePage => "http://www.math.cornell.edu/~mike"},
	       {Name => "Anton Leykin"},
	       {Name => "Mauricio Velasco"},
	       {Name => "Keller VandeBogert",
		    Email => "keller.v@uky.edu",
		    HomePage => "https://sites.google.com/view/kellervandebogert/home"}
	       },
	  Headline => "Schur modules and maps between them",
	  Keywords => {"Homological Algebra", "Representation Theory"},
	  DebuggingMode => false,
	  AuxiliaryFiles=>true
     	  )
     
export{ "schur", "schurModule", "Filling",
     "straighten", "printSchurModuleElement", "schurModulesMap", "augmentFilling",
     "isStandard", "standardTableaux", "normalize",
     "character", "splitCharacter", "characterRep", "decomposeRep",
     -- Weyl module base functionality (divided-power analogue of Filling)
     "WeylFilling", "weyl",
     "weylStraighten", "weylNormalize",
     "isWeylStandard", "towardWeylStandard",
     "standardWeylTableaux",
     "divComult", "divMult",
     -- Weyl module / functor (divided-power analogue of Schur)
     "weylModule", "dividedPower",
     "printWeylModuleElement", "weylModulesMap", "maxWeylFilling",
     "augmentWeylFilling", "weylCharacter"}

exteriorPower(List, Module) := opts -> (L,M) -> (
     if #L == 0 then exteriorPower(0,M)
     else exteriorPower(L#0, M) ** exteriorPower(drop(L,1), M)
     )

exteriorPower(List, Matrix) := opts -> (L,f) -> (
     if #L == 0 then exteriorPower(0,f)
     else exteriorPower(L#0, f) ** exteriorPower(drop(L,1), f)
     )

-- A Filling is a tableau stored column-wise: T#j is the j-th column, a
-- list of entries read top-to-bottom.  The Schur side.
Filling = new Type of BasicList

-- Partition-transpose at the tableau level: columns of T become rows of
-- conjugate T (truncated when a column runs out).
conjugate Filling := T -> (
     a := #T#0;
     new Filling from apply(0..a-1, i ->
	  for j from 0 to #T-1 when #T#j > i list T#j#i)
     )

-- Lex ordering on Fillings of the same shape.  We compare columns right-to-
-- left (outermost first) and, within a column, entries top-to-bottom.
-- Iterative form: avoids the per-call `drop` allocation that would otherwise
-- dominate rsort in schurModule when |A| is large.
Filling ? Filling := (T, U) -> (
     k := #T - 1;
     while k >= 0 do (
     	  a := T#k; b := U#k;
	  j := #a - 1;
	  while j >= 0 do (
	       if a#j > b#j then return symbol>;
	       if a#j < b#j then return symbol<;
	       j = j - 1;
	       );
	  k = k - 1;
	  );
     symbol==
     )

-- Subset of columns, indexed by a list of positions.
Filling _ List := (T, L) -> (toList T)_L

-- Sort a column into strictly-increasing order, tracking the permutation
-- sign.  Returns (-1, L) as a sentinel if the column has a repeated entry
-- (in which case the Filling represents 0 in the ambient exterior power).
-- Bubble sort is used because columns are short (~|lambda'_j|) and this
-- form makes the sign bookkeeping trivial.
sortLen = L -> (
     len := 0;
     s := new MutableList from L;
     n := #s;
     for i from 0 to n-2 do
     	  for j from 0 to n-i-2 do (
	       if s#j === s#(j+1) then return (-1, L);
	       if s#j > s#(j+1) then (
		    tmp := s#(j+1);
		    s#(j+1) = s#j;
		    s#j = tmp;
		    len = len+1;
		    )
	       );
     (len, toList s)
     )

-- As sortLen, but returns (sign, L') with sign in {1, -1} (or null if the
-- column is degenerate).
sortSign = L -> (
     (len, L1) := sortLen L;
     (if len =!= -1 then (if len % 2 === 0 then 1 else -1), L1)
     )

-- Sort every column of T in place.  Returns (c, T'), where
--   c =  0  if any column has a repeated entry (T represents 0),
--   c =  1  if the total permutation has even sign,
--   c = -1  if it has odd sign.
normalize = method(Options => true)
normalize Filling := {} >> opts -> T -> (
     coeff := 0;
     degenerate := false;
     T' := apply(T, t -> (
	       (c, t') := sortLen t;
	       if c < 0 then degenerate = true;
	       coeff = coeff + c;
	       t'));
     if degenerate then (0, null)
     else (if coeff % 2 == 0 then 1 else -1, new Filling from T')
     )

isStandard = method()
isStandard Filling := T -> (
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

-- Exchange step: swap the entries at positions in `s` of column col1 with
-- the first #s entries of column col2, then re-sort both columns and track
-- the sign of the permutation.  Returns null if either resulting column has
-- a repeated entry; otherwise (newColumns, coeff) with coeff in {1,-1}.
schurExchange = (T, col1, col2, s) -> (
     b := T#col2;
     M := new MutableList from T#col1;
     b = join(apply(#s, i -> (j := s#i; a := M#j; M#j = b#i; a)), drop(b,#s));
     (sgn, M1) := sortSign M;
     (sgnb, b1) := sortSign b;
     if sgn === null or sgnb === null then null
     else (for i from 0 to #T-1 list (
	       if i == col1 then M1 else if i == col2 then b1 else T#i
	       ), sgn*sgnb)
     )

-- Garnir shuffle between columns col1, col2: enumerate all ways to move
-- nrows entries between the columns.  Returns the list of non-degenerate
-- (newFilling, sign) outcomes.
shuffle' = (T, nrows, col1, col2) -> (
     I := subsets(0..#(T#col1)-1, nrows);
     select(apply(I, x -> schurExchange(T, col1, col2, toList x)), y -> y =!= null)
     )

-- Raw single-step: assumes T is already a normalized Filling (columns
-- sorted, no repeats).  Used by trusted callers (the main schurModule fill
-- loop, the worklist-based straighten) that handle normalization up front.
-- Same collision-safe accumulator as towardStandard.
towardStandardRaw = T -> (
     x := isStandard T;
     acc := new MutableHashTable;
     if x === null then acc#T = 1
     else (
	  for p in shuffle'(T, x#1+1, x#0, x#0+1) do (
	       U := new Filling from p#0;
	       s := p#1;
	       if acc#?U then (
		    v := acc#U + s;
		    if v == 0 then remove(acc, U) else acc#U = v;
		    )
	       else acc#U = s;
	       );
	  );
     new HashTable from acc
     )

-- Single Garnir step for Schur: writes T as a Z-linear combination of
-- strictly-larger (in rsort order) Fillings if T is non-standard.
-- Normalizes on entry (so input with un-sorted columns is handled), wraps
-- output keys as Filling (so downstream lookups work directly), and is
-- collision-safe (if two shuffle branches produce the same Filling their
-- coefficients are summed rather than the second overwriting the first).
towardStandard = T -> (
     (c, Tn) := normalize T;
     if c == 0 then return new HashTable from {};
     if c == 1 then towardStandardRaw Tn
     else (
	  -- c == -1: scale every coefficient in the raw expansion by -1.
	  H := towardStandardRaw Tn;
	  applyValues(H, v -> -v)
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

standardTableaux = method()
standardTableaux (ZZ, List) := (dim, mu) ->
     select(apply(alltab(dim, mu), T -> new Filling from T),
	  T -> isStandard T === null)

schurModule = method()
schurModule(List,Module) := (lambda,E) -> (
     R := ring E;
     lambda = new Partition from lambda;
     mu := toList conjugate lambda;
     -- create a hash table of all tableaux: T => i (index in wedgeE)
     -- A is the list of all of these tableaux.
     A := alltab(rank E, mu);
     A = apply(A, T -> new Filling from T);
     AT := hashTable apply(#A, i -> A#i => i);
     -- now we create the hash table ST of all standard tableaux: T => i
     -- where the index now is that in the resulting module M
     B := positions(A, T -> isStandard T === null);
     ST := hashTable apply(#B, i -> A#(B#i) => i);
     -- Make the two modules of interest:
     exteriorE := exteriorPower(mu,E);
     M := source exteriorE_B;
     -- Now make the change of basis matrix exteriorE --> M and its
     -- canonical lifting
     finv := map(exteriorE, M, (id_exteriorE)_B);
     m := mutableMatrix(R, numgens M, numgens exteriorE, Dense=>false);
     -- rsort the ambient basis via a precomputed flat lex key.  Our Filling ?
     -- Filling compares columns right-to-left and entries within a column
     -- top-to-bottom, so the corresponding flat key is
     --     reverse T#(#T-1) | reverse T#(#T-2) | ... | reverse T#0
     -- i.e.  flatten apply(reverse toList T, reverse).
     -- Dispatching to M2's built-in list ? inside compiled code gives an
     -- ~10x speedup over the interpreted Filling ? method.
     sortedT := apply(
	  rsort apply(A, T -> (flatten apply(reverse toList T, reverse), T)),
	  p -> p#1);
     scan(sortedT, T -> (
	 col := AT#T;
	 if ST#?T then (
	      -- place a unit vector in this column
	      m_(ST#T,col) = 1_R;
	      )
	 else (
	      -- T is a column of alltab, so it's already column-sorted and
	      -- has no repeats (otherwise it wouldn't be in A).  Skip the
	      -- normalize+sign check inside towardStandard.
	      a := towardStandardRaw T;
	      scan(pairs a, (U,s) -> (
			newcol := AT#U;
			columnAdd(m, col, s * 1_R, newcol);
			));
	 )));
     f := map(M, exteriorE, matrix m);
     M.cache#"Schur" = {f, finv, AT, ST};
     M)
     
-- Functorial action: a map f : M -> N of free modules induces
-- S_lambda(f) : S_lambda(M) -> S_lambda(N).  We realize it by lifting to
-- the ambient tensor-of-exteriors, applying the exterior functor there,
-- and projecting back down using the cached splittings / projections.
schur = method()
schur(List, Matrix) := (lambda, f) -> (
     SM := schurModule(lambda, source f);
     SN := schurModule(lambda, target f);
     mu := toList conjugate new Partition from lambda;
     F  := exteriorPower(mu, f);
     gM := SM.cache#"Schur"#1;  -- splitting:   S_lambda(M) -> ambient(M)
     gN := SN.cache#"Schur"#0;  -- projection:  ambient(N)  -> S_lambda(N)
     schurNM := gN * F * gM;
     (source schurNM).cache#"Schur" = SM.cache#"Schur";
     (target schurNM).cache#"Schur" = SN.cache#"Schur";
     schurNM
     )

-- Pretty-print a Filling as a single row of stacked columns.
net Filling := T -> netList {apply(toList T, c -> stack apply(c, e -> net e))}

-- Append the entry `e` to column `c` of T; if c is past the last column,
-- create a new singleton column {e}.
augmentFilling = method()
augmentFilling (Filling, ZZ, ZZ) := (T, c, e) -> (
     if c >= #T then join(T, {{e}})
     else new Filling from apply(#T, j -> if j != c then T#j else T#j | {e})
     )

-----------------------------------------------------------------------
-- Shared full-straightening worklist.
-- Used by both Schur (straighten) and Weyl (weylStraighten).
--   H       : HashTable of BasicList-shaped keys (Filling / WeylFilling /
--             plain List) with coefficients.
--   cast    : wraps a raw key as the correct Filling/WeylFilling type.
--   isStd   : predicate; returns null on standard keys, violation tuple else.
--   advance : single Garnir step, returning a HashTable (must normalize on
--             entry and produce keys strictly larger than its input in the
--             rsort ordering, so termination is guaranteed).
-- Returns a HashTable whose keys are all standard (w.r.t. isStd) and whose
-- values are the coefficients of the canonical standard-tableau expansion.
-- Every tableau encountered is visited at most once; coefficient collisions
-- are summed (rather than overwritten).
-----------------------------------------------------------------------
straightenWorklist := (H, cast, isStd, advance) -> (
     std  := new MutableHashTable;
     work := new MutableHashTable;
     bump := (t, K, c) -> (
	  if t#?K then (
	       v := t#K + c;
	       if v == 0 then remove(t, K) else t#K = v;
	       )
	  else t#K = c;
	  );
     for K in keys H do (
	  c := H#K;
	  if c != 0 then (
	       Kc := cast K;
	       if isStd Kc === null then bump(std, Kc, c) else bump(work, Kc, c);
	       );
	  );
     while #work > 0 do (
	  K := (keys work)#0;
	  c := work#K;
	  remove(work, K);
	  exp := advance K;
	  for U in keys exp do (
	       cc := c * exp#U;
	       if cc != 0 then (
		    Uc := cast U;
		    if isStd Uc === null then bump(std, Uc, cc) else bump(work, Uc, cc);
		    );
	       );
	  );
     new HashTable from std
     )

-----------------------------------------------------------------------
-- Schur straightening (parallel to weylStraighten below).
-----------------------------------------------------------------------
-- Fast path used by the 2-arg versions: the module-building phase has
-- already baked the full Garnir expansion of every ambient basis element
-- into the cached matrix f, so straighten(T, M) just looks up the column
-- of f indexed by the normalized T.  Full symbolic straightening (no
-- module required) goes through the worklist helper.

schurCast := K -> if instance(K, Filling) then K else new Filling from toList K

straighten = method()

-- Symbolic: writes T as a Z-linear combination of standard Fillings.
straighten Filling := T -> (
     (c, Tn) := normalize T;
     if c == 0 then new HashTable from {}
     else straightenWorklist(
	  new HashTable from {Tn => c},
	  schurCast, isStandard, towardStandard)
     )

-- Symbolic on a HashTable of (Filling => ZZ) terms.
straighten HashTable := H -> straightenWorklist(
     H, schurCast, isStandard, towardStandard)

-- Module-valued: T => Vector in the Schur module M.  Uses the cached
-- change-of-basis matrix f (which already encodes the full Garnir
-- expansion of every ambient basis element), so this is a single
-- sparse-column extraction.
straighten (Filling, Module) := Vector => (T, M) -> (
     if not M.cache#?"Schur" then error "module has no Schur cache";
     (c, S) := normalize T;
     if c == 0 then 0_M
     else (
	  AT := M.cache#"Schur"#2;
	  if not AT#?S then error "tableau and Schur module incompatible";
	  f := M.cache#"Schur"#0;
	  c * f * (source f)_(AT#S)
	  )
     )

-- Module-valued on a HashTable of terms.
straighten (HashTable, Module) := Vector => (H, M) -> (
     if not M.cache#?"Schur" then error "module has no Schur cache";
     AT := M.cache#"Schur"#2;
     f := M.cache#"Schur"#0;
     amb := source f;
     result := 0_M;
     for K in keys H do (
	  (c, S) := normalize K;
	  if c != 0 then (
	       if not AT#?S then error "tableau and Schur module incompatible";
	       result = result + (c * H#K) * f * amb_(AT#S);
	       );
	  );
     result
     )

printSchurModuleElement = method()
printSchurModuleElement (Vector, Module) := (v, M) -> (
     l := applyPairs(M.cache#"Schur"#3, (T, i) -> (i, T));
     scanKeys(l, i ->
	  if v_i != 0 then << v_i << "*" << l#i << " ");
     << endl
     )

schurModulesMap = method()
schurModulesMap (Module, Module, Function) := (N, M, F) -> (
     l := applyPairs(M.cache#"Schur"#3, (T, i) -> (i, T));
     matrix apply(#l, j -> sum(F(l#j), a -> a#0 * straighten(a#1, N)))
     )

-- Maximal semistandard Filling for shape p with entries in {0,...,d-1}.
-- Column c has height h = #{i : p_i > c} and is filled with d-h, d-h+1, ..., d-1.
maxFilling = method(TypicalValue => Filling)
maxFilling (List, ZZ) := (p, d) -> (
     nCols := max p;
     new Filling from apply(nCols, c -> (
	       h := #select(p, j -> j > c);
	       toList ((d-h)..(d-1))))
     )

-----------------------------------------------------------------------
-- Characters of (composed) Schur functors.
--   character(L, d) returns the character of S_{L_0} S_{L_1} ... (R^d)
--   as a symmetric polynomial in QQ[x_0..x_{d-1}], where L is a list of
--   partitions applied outside-in.
-----------------------------------------------------------------------
character = method()
character (List, ZZ) := (L, d) -> (
     L = reverse L;
     x := local x;
     R := QQ[x_0..x_(d-1)];
     M := diagonalMatrix gens R;
     scan(L, p -> M = schur(p, M));
     trace M
     )

-----------------------------------------------------------------------
-- Decomposition of (polynomial) GL_d representations into irreducibles.
--
-- F : an m x m matrix whose entries are polynomials in the d^2 generators
-- w_{ij} of R = QQ[w_1..w_{d^2}]; F represents the image of the generic
-- d x d matrix under some polynomial GL_d-representation rho.
--
-- specialize(M, F) evaluates F at the entries of M (so F is pulled back
-- to rho(M)).  transvections(d) produces the generators of the unipotent
-- radical, used to carve out highest-weight vectors.
-----------------------------------------------------------------------

-- Evaluate a matrix-valued polynomial F at the numerical matrix M, i.e.
-- return rho(M) where F = rho(generic matrix).
specialize = (M, F) -> (
     (map(ring M, ring F, matrix {flatten entries M})) F
     )

-- d x d transvection I + E_{i,j} (i > j).
transvection = (i, j, d) -> id_(QQ^d) + matrix(
     apply(d, r -> apply(d, c -> if r == i and c == j then 1_QQ else 0_QQ)))

-- The d*(d-1)/2 lower-triangular transvections generating the unipotent
-- radical of upper-triangular GL_d.
transvections = d -> flatten apply(d, i ->
     apply(i, j -> transvection(i, j, d)))

-- Weight of a filling with respect to a torus character D = (d_0,...,d_{n-1}):
-- weight(T, D) = prod_{cell} D#(entry).  For D = (1,2,...,d) this is the
-- numeric image of the maximal Weyl weight.
weight = method()
weight (Filling, List) := (T, D) -> product(flatten toList T, i -> D#i)

-- Find a highest-weight vector for the S_p-isotypic component of rho.
findSubRep = method()
findSubRep (List, Matrix) := (p, F) -> (
     d := round sqrt numgens ring F;
     T := maxFilling(p, d);
     TransEval := apply(transvections d, M -> specialize(M, F));
     idF := specialize(id_(QQ^d), F);
     -- each transvection must act as the identity on highest-weight vectors
     TE := matrix apply(TransEval, k -> {k - idF});
     -- the diagonal torus must act by the maximal weight of S_p
     D := apply(d, j -> (j+1)_QQ);
     M := specialize(diagonalMatrix D, F) - weight(T, D) * idF;
     syz(TE || M)
     )

characterRep = method()
characterRep Matrix := F -> (
     d := round sqrt numgens ring F;
     x := symbol x;
     R := QQ[x_0..x_(d-1)];
     trace specialize(diagonalMatrix gens R, F)
     )

decomposeRep = method()
decomposeRep Matrix := F -> (
     -- splitCharacter produces an element of a SchurRing;
     -- listForm returns a list of (partition, multiplicity) pairs.
     ir := apply(listForm splitCharacter characterRep F, first);
     new HashTable from apply(ir, p -> p => findSubRep(p, F))
     )

needsPackage "SymmetricPolynomials"
needsPackage "SchurRings"

-- splitCharacter: given a symmetric polynomial in x_0..x_{n-1} (produced
-- e.g. by characterRep), rewrite it as a linear combination of Schur
-- polynomials s_lambda in the SchurRing.  Uses SymmetricPolynomials to
-- pass through elementary symmetric functions, then toS to land in a
-- SchurRing of rank n.
splitCharacter = method()
splitCharacter RingElement := ce -> (
     pe := elementarySymmetric ce;
     -- elementarySymmetric returns a polynomial in 2n variables: the first
     -- n are the original x_i, the last n are the elementary symmetric
     -- functions e_1..e_n.
     n := numgens source vars ring ce;
     R2 := symmetricRing(coefficientRing ring pe, n);
     es := (vars R2)_{0..n-1};
     toS substitute(pe, es | es)
     )

-----------------------------------------------------------------------
-- Weyl modules: divided-power analogue of Filling / Schur straightening
-- Ported from Keller VandeBogert's Ext-hooks.m2
-- The straightening algorithm here enforces strictly-increasing rows
-- (the defining relation for W_lambda in characteristic-free form) and
-- uses divided-power comultiplication/multiplication, so no signs are
-- tracked when rows are re-sorted.
-----------------------------------------------------------------------

WeylFilling = new Type of BasicList

weyl = method()
weyl List := L -> new WeylFilling from L

net WeylFilling := T -> (net conjugate new Filling from toList T)

-- Compare two WeylFillings. Mirrors Filling ? Filling, with rows playing the
-- role that columns do for Filling: the LAST row is compared first, and
-- within a row we compare from the last entry toward the first.
-- Iterative version — avoids the per-comparison `drop` allocation that
-- would otherwise dominate rsort in weylModule.
WeylFilling ? WeylFilling := (T,U) -> (
     k := #T - 1;
     while k >= 0 do (
     	  a := T#k;
     	  b := U#k;
	  j := #a - 1;
	  while j >= 0 do (
	       if a#j > b#j then return symbol>;
	       if a#j < b#j then return symbol<;
	       j = j - 1;
	       );
	  k = k - 1;
	  );
     symbol==
     )

conjugate WeylFilling := T -> (
    a := #T#0;
    new WeylFilling from apply(0..a-1, i ->
        for j from 0 to #T-1 when #T#j > i list T#j#i)
    )

WeylFilling == WeylFilling := (T,P) -> toList T == toList P

-- row selection / single-row access
WeylFilling _ List := (T,L) -> (toList T)_L
WeylFilling _ ZZ   := (T,n) -> (toList T)_n

-- convert a tuple (weakly-increasing list representing a multiset) to its
-- exponent vector of length n.  Single pass over L, O(#L + n) rather than
-- the O(n*#L) scan-n-times implementation that was dominant in profiling.
tupleToExp = method()
tupleToExp(List,ZZ) := (L,n) -> (
     e := new MutableList from toList(n:0);
     for x in L do e#x = e#x + 1;
     toList e
     )
tupleToExp(List) := L -> (
     if #L == 0 then {}
     else tupleToExp(L, max L + 1)
     )

-- inverse of tupleToExp: exponent vector -> weakly-increasing tuple.
-- Single-pass with a MutableList — faster than the `splice ... (L_i):i` version
-- because it avoids allocating #L intermediate Sequences.
expToTuple = L -> (
     out := new MutableList;
     for i from 0 to #L-1 do for k from 1 to L#i do out#(#out) = i;
     toList out
     )

-- componentwise majorization test, used by divComult
totalBd = (L1,L2) -> (
    n := max(#L1, #L2);
    L1 = L1 | toList((n-#L1):0);
    L2 = L2 | toList((n-#L2):0);
    all(L2-L1, i -> i >= 0)
    )

boundedPower = (n,d,L) -> select(compositions(n,d), i -> totalBd(i,L))

-------------------------------------------------------------------------
-- Fast exponent-native divided-power helpers.
-- These work directly on exponent vectors (lists of length n whose entries
-- sum to the total degree), avoiding the tuple<->exp round-trips that
-- dominated profiling of weylShuffle.
-------------------------------------------------------------------------

-- Enumerate length-n exponent vectors summing to p, bounded componentwise
-- by Lexp.  Direct backtracking: no compositions(n,p) allocation, no filter.
boundedExpAux := (Lexp, p, idx, n, acc, out) -> (
     if idx == n-1 then (
	  if p <= Lexp#idx then (
	       acc#idx = p;
	       out#(#out) = toList acc;
	       );
	  ) else (
	  upper := min(p, Lexp#idx);
	  for v from 0 to upper do (
	       acc#idx = v;
	       boundedExpAux(Lexp, p-v, idx+1, n, acc, out);
	       );
	  );
     )

boundedExp = (Lexp, p) -> (
     n := #Lexp;
     if n == 0 then (if p == 0 then {{}} else {})
     else (
	  acc := new MutableList from toList(n:0);
	  out := new MutableList;
	  boundedExpAux(Lexp, p, 0, n, acc, out);
	  toList out
	  )
     )

-- Divided-power comultiplication on an exponent vector, returning a list
-- of (left-exp, right-exp) pairs with left + right = Lexp and sum right = p.
divComultExp = (Lexp, p) -> (
     for i in boundedExp(Lexp, p) list (Lexp - i, i)
     )

-- Divided-power multiplication on exponent vectors of the same length n.
-- Returns (c, L1+L2) where c = prod binomial(L1_k + L2_k, L1_k).
divMultExp = (L1, L2) -> (
     c := 1;
     for k from 0 to #L1-1 do c = c * binomial(L1#k + L2#k, L1#k);
     (c, L1 + L2)
     )

-- divided-power comultiplication D_{p+q} F -> D_p F ** D_q F
-- applied to the basis element encoded by the given exponent vector.
divComult = method()
divComult(Sequence,ZZ) := (L,p) -> (
    t := boundedPower(#L, p, toList L);
    for i in t list {toList L - i, i}
    )
divComult(List,ZZ,ZZ) := (L,p,n) -> (
    Lexp := tupleToExp(L, n);
    for pair in divComultExp(Lexp, p) list {expToTuple pair#0, expToTuple pair#1}
    )
divComult(List,ZZ) := (L,p) -> (
     if #L == 0 then divComult(L, p, 0)
     else divComult(L, p, max L + 1)
     )

-- divided-power multiplication D_p F ** D_q F -> D_{p+q} F on a basis pair
divMult = method()
divMult(Sequence,Sequence) := (S1,S2) -> (
    Lk := for i from 0 to #S1-1 list binomial(S1_i + S2_i, S1_i);
    {product Lk, toList S1 + toList S2}
    )
divMult(List,List,ZZ) := (L1,L2,n) -> (
    (c, sumExp) := divMultExp(tupleToExp(L1,n), tupleToExp(L2,n));
    {c, expToTuple sumExp}
    )
divMult(List,List) := (L1,L2) -> (
     m1 := if #L1 == 0 then -1 else max L1;
     m2 := if #L2 == 0 then -1 else max L2;
     divMult(L1, L2, max(m1, m2) + 1)
     )

-- rows-only sort. Since divided powers are symmetric, no sign is tracked.
weylNormalize = method()
weylNormalize WeylFilling := T -> new WeylFilling from (toList T)/sort

-- Returns null if T is Weyl-standard (strictly increasing down each column),
-- otherwise the (row,column) of the first violation from the bottom up.
isWeylStandard = method()
isWeylStandard WeylFilling := T -> (
    i := #T - 2;
    while i >= 0 do (
        a := T#i; b := T#(i+1);
        n := #b;
        for j from 0 to n-1 do if a#j >= b#j then return (i,j);
        i = i - 1;
        );
    null
    )

-- Exchange step between two columns. Divided-power setting, so coefficient is -1.
weylExchange = method()
weylExchange(WeylFilling,ZZ,ZZ,List) := (T,col1,col2,s) -> (
    b := reverse T#col2;
    M := new MutableList from reverse T#col1;
    b = join(apply(#s, i -> (j := s#i; a := M#j; M#j = b#i; a)), drop(b,#s));
    M1 := sort toList M;
    b1 := sort b;
    (for i from 0 to #T-1 list (
        if i == col1 then M1
        else if i == col2 then b1
        else T#i
        ), -1)
    )

-- Single Garnir-style shuffle using divided-power comultiplication
-- across rows (row1, row2) of T, at column ncols.  Assumes T is already
-- weylNormalize'd (rows weakly increasing).
--
-- All divided-power arithmetic is inlined:
--   * the comultiplication enumerates split vectors i (bounded by Jexp)
--     via a direct recursive backtracker — no intermediate list of splits
--     is ever allocated;
--   * for each non-identity split, the two divided-power multiplications
--     (r1head * (Jexp-i)  and  i * r2tail) are computed in a single pass
--     over the alphabet: the resulting exponent vectors d1, d2, and the
--     binomial coefficients c1, c2 are accumulated together.
-- This eliminates the per-split function-call overhead of divComultExp /
-- divMultExp (which together contributed ~80% of this routine's cost).
weylShuffle = (T,ncols,row1,row2) -> (
    r1 := T#row1;
    r2 := T#row2;
    -- T#row2 is already sorted, so "last j with r2#j == r2#(ncols-1)"
    -- is a rightward scan from ncols-1.
    pivot := r2#(ncols-1);
    newLast := ncols-1;
    while newLast+1 < #r2 and r2#(newLast+1) == pivot do newLast = newLast+1;

    r1head := r1_{0..ncols-2};
    r1tail := r1_{ncols-1..#r1-1};
    r2head := r2_{0..newLast};
    r2tail := r2_{newLast+1..#r2-1};

    -- Uniform alphabet size covering every slice.
    maxOf := L -> if #L == 0 then -1 else max L;
    n := 1 + max {maxOf r1head, maxOf r1tail, maxOf r2head, maxOf r2tail};
    p := newLast + 1;

    -- Exponent-form slices.  r1tail's exponent vector is captured inside
    -- Jexp (= r1tExp + r2hExp); we never materialize r1tExp separately.
    r1hExp  := tupleToExp(r1head,  n);
    r2hExp  := tupleToExp(r2head,  n);
    r2tExp  := tupleToExp(r2tail,  n);
    Jexp    := tupleToExp(r1tail | r2head, n);

    Tlist := toList T;
    result := new MutableList;
    accI := new MutableList from toList(n:0);

    -- The newly-built rows have fixed lengths equal to #r1 and #r2.
    r1len := #r1;
    r2len := #r2;

    -- Backtracking enumerator of i in boundedExp(Jexp, p), inlined:
    -- at each leaf we write newR1 and newR2 directly as weakly-increasing
    -- tuples (by iterating k = 0..n-1 and emitting k-copies into pre-sized
    -- MutableLists), accumulating c1, c2 in the same pass.  This skips
    -- materialization of intermediate exponent vectors d1, d2 and any call
    -- to expToTuple.  The identity split (i == r2hExp) is skipped.
    emit := null;
    emit = (idx, remaining) -> (
	 if idx == n-1 then (
	      if remaining > Jexp#idx then return;
	      accI#idx = remaining;
	      -- identity check: accI == r2hExp ?
	      isIdentity := true;
	      for k from 0 to n-1 do
		   if accI#k =!= r2hExp#k then (isIdentity = false; break);
	      if isIdentity then return;

	      -- Single pass: write newR1, newR2 directly into pre-sized
	      -- MutableLists and accumulate c1, c2 at the same time.
	      c1 := 1; c2 := 1;
	      newR1Mut := new MutableList from toList(r1len:0);
	      newR2Mut := new MutableList from toList(r2len:0);
	      i1 := 0; i2 := 0;
	      for k from 0 to n-1 do (
		   ik := accI#k;
		   a  := Jexp#k - ik;     -- split#0#k
		   r1hk := r1hExp#k;
		   r2tk := r2tExp#k;
		   d1k := r1hk + a;
		   d2k := ik + r2tk;
		   for q from 1 to d1k do (newR1Mut#i1 = k; i1 = i1+1);
		   for q from 1 to d2k do (newR2Mut#i2 = k; i2 = i2+1);
		   if r1hk > 0 and a > 0 then c1 = c1 * binomial(d1k, r1hk);
		   if ik > 0 and r2tk > 0 then c2 = c2 * binomial(d2k, ik);
		   );
	      newR1 := toList newR1Mut;
	      newR2 := toList newR2Mut;
	      newFilling := new WeylFilling from for j from 0 to #T-1 list (
		   if j == row1 then newR1
		   else if j == row2 then newR2
		   else Tlist#j);
	      result#(#result) = (newFilling, -c1*c2);
	      return;
	      );
	 upper := min(remaining, Jexp#idx);
	 for v from 0 to upper do (
	      accI#idx = v;
	      emit(idx+1, remaining-v);
	      );
	 );
    emit(0, p);
    toList result
    )

-- Raw single-step: assumes T is already weylNormalize'd (rows weakly
-- increasing).  Used by trusted callers (the main weylModule fill loop,
-- and the worklist-based weylStraighten) that handle normalization up front.
--
-- Note: weylShuffle's output keys are pairwise distinct -- different splits
-- i in boundedExp(Jexp, p) give different d2 = i + r2tExp, hence different
-- output WeylFillings -- so no collision accumulator is needed here; we can
-- construct the HashTable in one shot.
towardWeylStandardRaw = T -> (
    x := isWeylStandard T;
    if x === null then new HashTable from {T => 1}
    else hashTable weylShuffle(T, x#1+1, x#0, x#0+1))

-- One straightening step: writes T as a Z-linear combination of
-- WeylFillings T' that are strictly larger than T in the rsort ordering,
-- if T is not already standard.
-- Collision-safe: two shuffle branches that happen to land on the same
-- normalized filling have their coefficients summed (rather than the second
-- silently overwriting the first, as would happen with `new HashTable from`).
towardWeylStandard = T -> towardWeylStandardRaw weylNormalize T

-----------------------------------------------------------------------
-- Weyl straightening (parallel to straighten above).
-----------------------------------------------------------------------
-- Fast path used by the 2-arg versions: the module-building phase has
-- already baked the full Garnir expansion of every ambient basis element
-- into the cached matrix f, so weylStraighten(T, M) just looks up the
-- column of f indexed by the normalized T.  Full symbolic straightening
-- (no module required) goes through the shared worklist helper.

weylCast := K -> if instance(K, WeylFilling) then K else new WeylFilling from toList K

weylStraighten = method()

-- Symbolic: writes T as a Z-linear combination of standard WeylFillings.
weylStraighten WeylFilling := T -> straightenWorklist(
     new HashTable from {weylNormalize T => 1},
     weylCast, isWeylStandard, towardWeylStandard)

-- Symbolic on a HashTable of (WeylFilling => ZZ) terms.
weylStraighten HashTable := H -> straightenWorklist(
     H, weylCast, isWeylStandard, towardWeylStandard)

-----------------------------------------------------------------------
-- Divided-power functor  D^d : Mod(R) -> Mod(R)
-- Parallel to exteriorPower(ZZ|List, Module|Matrix)
-- On a free module of rank n, D^d has rank binomial(n+d-1, d), with basis
-- indexed by compositions(n,d) (so that dividedPower(lambda,E) as an
-- iterated tensor agrees with M2's ** order: first factor varies slowest).
-----------------------------------------------------------------------

dividedPower = method()

dividedPower(ZZ, Module) := (d,E) -> (
     R := ring E;
     n := rank E;
     R^(binomial(n+d-1, d))
     )

dividedPower(List, Module) := (L,E) -> (
     if #L == 0 then dividedPower(0,E)
     else dividedPower(L#0, E) ** dividedPower(drop(L,1), E)
     )

-- D^d applied to f: R^n -> R^m, on the basis indexed by compositions(n,d).
-- Formula: if f = [f_1 | ... | f_n] then D^d(f)(d_alpha) = prod_i (f_i)^{(alpha_i)}
-- computed in the divided-power algebra of the target, where
--   (sum_j v_j e_j)^{(k)} = sum_{|beta|=k} (prod_j v_j^{beta_j}) e^{(beta)}
--   e^{(beta^1)} * e^{(beta^2)} = (prod_j binomial(beta^1_j+beta^2_j, beta^1_j)) e^{(beta^1+beta^2)}
dividedPower(ZZ, Matrix) := (d,f) -> (
     R := ring f;
     src := source f;
     tgt := target f;
     n := rank src;
     m := rank tgt;
     srcBasis := compositions(n, d);
     tgtBasis := compositions(m, d);
     tgtIdx := hashTable apply(#tgtBasis, i -> tgtBasis#i => i);
     cols := if n == 0 then {} else entries transpose f;
     -- Precompute cols#i#j^k for 0 <= k <= d, once per matrix entry.
     -- Without this, the inner loop re-exponentiates every ring element on
     -- every (alpha, beta) pair.  colPow#i#j#k = (cols#i#j)^k.
     one := 1_R;
     colPow := apply(n, i -> apply(m, j -> (
		    v := cols#i#j;
		    pw := new MutableList from (d+1):one;
		    for k from 1 to d do pw#k = (pw#(k-1)) * v;
		    toList pw
		    )));
     -- Precompute compositions(m, k) once per k <= d; reused for every alpha.
     betasOf := apply(d+1, k -> compositions(m, k));
     zeroG := toList(m:0);
     DF := mutableMatrix(R, #tgtBasis, #srcBasis);
     scan(#srcBasis, aIdx -> (
	       alpha := srcBasis#aIdx;
	       acc := new MutableHashTable;
	       acc#zeroG = one;
	       for i from 0 to n-1 do (
		    ai := alpha#i;
		    if ai > 0 then (
			 newAcc := new MutableHashTable;
			 colPowI := colPow#i;
			 betas := betasOf#ai;
			 for g in keys acc do (
			      coef := acc#g;
			      for beta in betas do (
				   cc := coef;
				   for j from 0 to m-1 do (
					bj := beta#j;
					if bj > 0 then cc = cc * colPowI#j#bj;
					cc = cc * binomial(g#j + bj, bj);
					);
				   newG := g + beta;
				   if newAcc#?newG then newAcc#newG = newAcc#newG + cc
				   else newAcc#newG = cc;
				   );
			      );
			 acc = newAcc;
			 );
		    );
	       for g in keys acc do (
		    DF_(tgtIdx#g, aIdx) = acc#g;
		    );
	       ));
     map(dividedPower(d, tgt), dividedPower(d, src), matrix DF)
     )

dividedPower(List, Matrix) := (L,f) -> (
     if #L == 0 then dividedPower(0,f)
     else dividedPower(L#0, f) ** dividedPower(drop(L,1), f)
     )

-----------------------------------------------------------------------
-- Enumeration of WeylFillings
--   allWeylTab(dim, lambda)      mirrors alltab
--   standardWeylTableaux(...)    mirrors standardTableaux
-- Row 0 is enumerated slowest, matching the ** convention so that the
-- i-th WeylFilling in allWeylTab corresponds to the i-th basis element
-- of dividedPower(lambda, E).
-----------------------------------------------------------------------

allWeylTab = (dim, lambda) -> (
     if #lambda == 0 then return {{}};
     rows := apply(compositions(dim, lambda#0), e -> toList expToTuple e);
     if #lambda == 1 then apply(rows, r -> {r})
     else (
	  rest := allWeylTab(dim, drop(lambda, 1));
	  flatten apply(rows, r -> apply(rest, y -> prepend(r, y)))
	  )
     )

standardWeylTableaux = method()
standardWeylTableaux (ZZ, List) := (dim, lambda) ->
     select(apply(allWeylTab(dim, lambda), T -> new WeylFilling from T),
	  T -> isWeylStandard T === null)

-----------------------------------------------------------------------
-- weylModule : exact parallel of schurModule.
-- M = image of the canonical projection  D^{lambda_0}(E) ** ... ** D^{lambda_{k-1}}(E)
--       ---> W_lambda(E)
-- The cache key "Weyl" stores {f, finv, AT, ST}, analogous to "Schur".
-----------------------------------------------------------------------

weylModule = method()
weylModule(List, Module) := (lambda, E) -> (
     R := ring E;
     A := allWeylTab(rank E, lambda);
     A = apply(A, T -> new WeylFilling from T);
     AT := hashTable apply(#A, i -> A#i => i);
     B := positions(A, T -> isWeylStandard T === null);
     ST := hashTable apply(#B, i -> A#(B#i) => i);
     ambD := dividedPower(lambda, E);
     M := source ambD_B;
     finv := map(ambD, M, (id_ambD)_B);
     m := mutableMatrix(R, numgens M, numgens ambD, Dense=>false);
     -- rsort via a precomputed flat lex key (last row slowest, entries within
     -- a row last-to-first).  Built-in list ? runs in compiled code; on the
     -- WeylFilling ambient this is ~10x faster than sorting on the interpreted
     -- WeylFilling ? method directly.
     sortedT := apply(
	  rsort apply(A, T -> (flatten apply(reverse toList T, reverse), T)),
	  p -> p#1);
     scan(sortedT, T -> (
	       col := AT#T;
	       if ST#?T then (
		    m_(ST#T, col) = 1_R;
		    )
	       else (
		    -- T comes from allWeylTab so its rows are already weakly
		    -- increasing — skip the weylNormalize inside towardWeylStandard.
		    a := towardWeylStandardRaw T;
		    scan(pairs a, (U, s) -> (
			      -- towardWeylStandardRaw already returns WeylFilling keys.
			      columnAdd(m, col, s * 1_R, AT#U);
			      ));
		    )));
     f := map(M, ambD, matrix m);
     M.cache#"Weyl" = {f, finv, AT, ST};
     M
     )

-- Functorial action on morphisms: parallel of schur(List, Matrix).  A map
-- f : M -> N induces W_lambda(f) : W_lambda(M) -> W_lambda(N); we lift to
-- the tensor-of-divided-powers, apply D^lambda(f), and project back down.
weyl(List, Matrix) := (lambda, f) -> (
     WM := weylModule(lambda, source f);
     WN := weylModule(lambda, target f);
     F  := dividedPower(lambda, f);
     gM := WM.cache#"Weyl"#1;  -- splitting:   W_lambda(M) -> ambient(M)
     gN := WN.cache#"Weyl"#0;  -- projection:  ambient(N)  -> W_lambda(N)
     weylNM := gN * F * gM;
     (source weylNM).cache#"Weyl" = WM.cache#"Weyl";
     (target weylNM).cache#"Weyl" = WN.cache#"Weyl";
     weylNM
     )

-- Module-valued: T => Vector in the Weyl module M.  Uses the cached
-- change-of-basis matrix f (which already encodes the full Garnir
-- expansion of every ambient basis element), so this is a single
-- sparse-column extraction.
weylStraighten(WeylFilling, Module) := Vector => (T, M) -> (
     if not M.cache#?"Weyl" then error "module has no Weyl cache";
     Tn := weylNormalize T;
     AT := M.cache#"Weyl"#2;
     if not AT#?Tn then error "tableau and Weyl module incompatible";
     f := M.cache#"Weyl"#0;
     f * (source f)_(AT#Tn)
     )

-- Module-valued on a HashTable of terms.
weylStraighten(HashTable, Module) := Vector => (H, M) -> (
     if not M.cache#?"Weyl" then error "module has no Weyl cache";
     AT := M.cache#"Weyl"#2;
     f := M.cache#"Weyl"#0;
     amb := source f;
     result := 0_M;
     for K in keys H do (
	  c := H#K;
	  if c != 0 then (
	       S := weylNormalize weylCast K;
	       if not AT#?S then error "tableau and Weyl module incompatible";
	       result = result + c * f * amb_(AT#S);
	       );
	  );
     result
     )

printWeylModuleElement = method()
printWeylModuleElement (Vector, Module) := (v, M) -> (
     l := applyPairs(M.cache#"Weyl"#3, (T,i) -> (i,T));
     scanKeys(l, i ->
	  if v_i != 0 then
	  << v_i << "*" << l#i << " ");
     << endl;
     )

weylModulesMap = method()
weylModulesMap (Module, Module, Function) := (N, M, F) -> (
     l := applyPairs(M.cache#"Weyl"#3, (T,i) -> (i,T));
     matrix apply(#l, j -> sum(F(l#j), a -> a#0 * weylStraighten(a#1, N)))
     )

-- Maximal Weyl-standard tableau for shape p with entries 0..d-1, filled
-- with d - (conj p)_j + i at position (i,j). Same formula as maxFilling,
-- but stored row-wise.
maxWeylFilling = method(TypicalValue => WeylFilling)
maxWeylFilling (List, ZZ) := (p, d) -> (
     cp := toList conjugate new Partition from p;
     new WeylFilling from apply(#p, i ->
	  apply(p#i, j -> d - cp#j + i))
     )

-- Row-wise analogue of augmentFilling.  WeylFillings are stored row-wise
-- (#T is the number of rows), so augmentWeylFilling(T, r, e) extends row r
-- by the entry e, or appends a new singleton row {e} if r >= #T.
augmentWeylFilling = method()
augmentWeylFilling (WeylFilling, ZZ, ZZ) := (T, r, e) -> (
     if r >= #T then new WeylFilling from join(toList T, {{e}})
     else new WeylFilling from apply(#T, j -> if j != r then T#j else T#j | {e})
     )

-- Character of a (nested composition of) Weyl functor(s), parallel to
-- character(List, ZZ) on the Schur side.  L is a list of partitions; this
-- computes the trace of the composed Weyl-functor image of the diagonal
-- GL_d operator.  In characteristic 0 W_λ and S_λ are canonically
-- isomorphic, so weylCharacter(L, d) == character(L, d); we provide this
-- parallel wrapper for uniformity and in case one wants to see that the
-- diagonal-character computation goes through the Weyl side end-to-end.
weylCharacter = method()
weylCharacter (List, ZZ) := (L, d) -> (
     L = reverse L;
     m := #L;
     x := local x;
     R := QQ[x_0..x_(d-1)];
     M := map(R^d, R^d,
	  matrix(apply(d, j -> apply(d, s -> if j == s then x_j else 0))));
     apply(m, j -> M = weyl(L_j, M));
     trace M
     )


beginDocumentation()

-----------------------------------------------------------------------
-- Overview
-----------------------------------------------------------------------
doc ///
  Key
    SchurFunctors
  Headline
    Schur and Weyl functors from partitions and Young tableaux
  Description
    Text
      A {\em Schur functor} $S_\lambda$ is a polynomial functor on the
      category of free modules, indexed by a partition
      $\lambda = (\lambda_1 \ge \lambda_2 \ge \cdots \ge \lambda_r)$.
      Classical special cases include
    Text
      @UL {
        {TEX "$S_{(d)}(E) = \\mathrm{Sym}^d(E)$, the $d$-th symmetric power"},
        {TEX "$S_{(1^d)}(E) = \\wedge^d(E)$, the $d$-th exterior power"},
        {TEX "$S_{(d,\\dots,d)}(E) = (\\det E)^{\\otimes k}$ on a rank-$d$ factor (repeated $k$ times)"},
        {TEX "$S_{(2,1)}(E) \\subseteq E \\otimes \\wedge^2 E$, the first genuinely mixed case"}
      }@
    Text
      The {\em Weyl functor} $W_\lambda$ is the divided-power analogue:
      in characteristic zero $W_\lambda(E) \cong S_\lambda(E)$, but
      over $\mathbb{Z}$ or in positive characteristic they are
      genuinely different functors.  They are related by duality and
      appear symmetrically in Schur-Weyl duality and in the definition
      of Weyl modules of the general linear group.
    Text
      @STRONG "Construction."@  Both functors are presented by
      Garnir / straightening relations.  For $S_\lambda$ one has the
      presentation
      $$\textstyle\bigotimes_{i \ge 1}\wedge^{\lambda'_i}(E)
        \;\twoheadrightarrow\; S_\lambda(E),$$
      where $\lambda'$ is the conjugate (transpose) partition: $S_\lambda(E)$
      is the quotient of this tensor product of exterior powers by the
      Garnir relations across adjacent columns.  Dually, $W_\lambda$ is
      a quotient of a tensor product of divided powers,
      $$\textstyle\bigotimes_{i \ge 1} D^{\lambda_i}(E)
        \;\twoheadrightarrow\; W_\lambda(E),$$
      by (divided-power) Garnir relations across adjacent rows.
    Text
      @STRONG "Tableaux and the semistandard basis."@  A filling of a
      Young diagram of shape $\lambda$ by integers $\{0,\dots,n-1\}$
      (representing a basis of $E = R^n$) is {\em semistandard} if its
      entries weakly increase along rows and strictly increase down
      columns.  Over $\mathbb{Z}$ the semistandard fillings index a
      free basis of $S_\lambda(E)$ and of $W_\lambda(E)$.  Arbitrary
      fillings are reduced to combinations of semistandard ones by the
      @EM "straightening algorithm"@; see @TO straighten@ and
      @TO weylStraighten@.
    Text
      In this package Schur tableaux are stored {\em column-wise} as
      objects of type @TO Filling@, while Weyl tableaux are stored
      {\em row-wise} as objects of type @TO WeylFilling@.  Conjugation
      of partitions swaps the two conventions; see
      @TO (conjugate, Filling)@.
    Text
      @STRONG "Representation theory."@  Over a field of characteristic
      zero the Schur functors $S_\lambda(V)$, as $\lambda$ ranges over
      partitions with at most $\dim V$ parts, give all the polynomial
      irreducible representations of $GL(V)$.  The character of
      $S_\lambda(V)$ is the Schur function $s_\lambda$ in the
      eigenvalues of the diagonal torus; computing and decomposing
      characters is handled by @TO character@, @TO weylCharacter@,
      @TO splitCharacter@, @TO characterRep@, and @TO decomposeRep@.
    Text
      @STRONG "Package layout."@  The two worlds (Schur / Weyl) expose
      parallel sets of functions:
    Text
      @UL {
        {"Modules: ", TO schurModule, " / ", TO weylModule, "."},
        {"Functoriality: ", TO schur, " / ", TO weyl, " take a map ",
         TEX "$f : E \\to F$", " to the induced ",
         TEX "$S_\\lambda(f)$", " / ", TEX "$W_\\lambda(f)$", "."},
        {"Straightening: ", TO straighten, " / ", TO weylStraighten,
         " rewrite an arbitrary tableau in the semistandard basis."},
        {"Module maps: ", TO schurModulesMap, " / ", TO weylModulesMap,
         " build a homomorphism from its action on a tableau basis."},
        {"Tableau combinatorics: ", TO Filling, " / ", TO WeylFilling,
         ", with ", TO normalize, " / ", TO weylNormalize, ", ",
         TO isStandard, " / ", TO isWeylStandard, ", ",
         TO towardWeylStandard, ", ",
         TO standardTableaux, " / ", TO standardWeylTableaux, ", ",
         TO augmentFilling, " / ", TO augmentWeylFilling, ", ",
         TO maxWeylFilling, "."},
        {"Divided powers: ", TO dividedPower, ", ", TO divMult, ", ",
         TO divComult, "."},
        {"Characters: ", TO character, " / ", TO weylCharacter, ", ",
         TO splitCharacter, ", ", TO characterRep, ", ",
         TO decomposeRep, "."},
        {"Pretty-printing: ", TO printSchurModuleElement, " / ",
         TO printWeylModuleElement, "."}
      }@
    Text
      @STRONG "A first example."@  Take the smallest non-hook, non-trivial
      shape $(2,1)$:
    Example
      E = QQ^3;
      M = schurModule({2,1}, E)
      rank M
      W = weylModule({2,1}, E)
      rank W
    Text
      Both have rank 8, which is $\dim S_{(2,1)}(\mathbb{Q}^3) = 8$ by
      the Weyl character formula.  They are canonically isomorphic in
      characteristic 0, but are presented as quotients of different
      ambients:
      $\wedge^2(E)\otimes E \twoheadrightarrow \mathrm{schurModule}((2,1), \mathbb{Q}^3)$
      while
      $D^2(E)\otimes D^1(E) \twoheadrightarrow \mathrm{weylModule}((2,1), \mathbb{Q}^3)$.
    Text
      @STRONG "References."@  For the Schur side see W. Fulton,
      {\em Young Tableaux}, LMS Student Texts 35 (1997), Chapter 8.
      For the Weyl-module / divided-power construction see
      K. Akin, D. Buchsbaum, and J. Weyman,
      {\em Schur functors and Schur complexes}, Adv. Math. {\bf 44}
      (1982), 207-278; and the book J. Weyman,
      {\em Cohomology of Vector Bundles and Syzygies}, Cambridge Tracts
      149 (2003).
  SeeAlso
    Filling
    WeylFilling
    schurModule
    weylModule
    schur
    weyl
    straighten
    weylStraighten
    character
    weylCharacter
///

-----------------------------------------------------------------------
-- Filling type and tableau combinatorics (Schur side)
-----------------------------------------------------------------------
doc ///
  Key
    Filling
    (symbol ?, Filling, Filling)
    (symbol _, Filling, List)
    (net, Filling)
  Headline
    a Young tableau stored column-wise
  Description
    Text
      A @TT "Filling"@ is a Young tableau of partition shape, stored as
      a list of its columns.  A filling of shape
      $\mu = (\mu_1 \ge \mu_2 \ge \cdots \ge \mu_r)$ with entries from
      an alphabet $\{0, 1, \ldots, d-1\}$ is represented by
      @TT "new Filling from {c_1, c_2, ..., c_{mu_1}}"@,
      where $c_j$ is the list of entries in the $j$-th column read from
      top to bottom.  The length of $c_j$ equals $\mu'_j$, the length of
      the $j$-th column (equivalently, the $j$-th part of the conjugate
      partition $\mu'$).
    Text
      Because Schur modules are built from
      $\wedge^{\mu'_1}(E) \otimes \cdots \otimes \wedge^{\mu'_k}(E)$,
      the column-wise storage makes each column directly correspond to
      one wedge-power tensor factor.  This is the dual convention to
      @TO WeylFilling@, which stores rows.
    Example
      T = new Filling from {{0,1}, {1,2}}
      toList T
      T#0
      #T
    Text
      The tableau pretty-prints via its @TT "net"@ method:
    Example
      T
    Text
      Conjugation of a @TT "Filling"@ produces the tableau of conjugate
      (transposed) shape:
    Example
      conjugate T
      toList conjugate T
    Text
      A filling is @EM "standard"@ (in this package; equivalently,
      @EM "semistandard"@ in the standard combinatorial sense) if its
      entries strictly increase down each column and weakly increase
      along each row.  Over $\mathbb{Z}$ the standard fillings of shape
      $\mu$ with entries in $\{0,\ldots,d-1\}$ index a free basis of
      $S_\mu(R^d)$.
    Example
      S = standardTableaux(3, {2,1});
      #S == rank schurModule({2,1}, QQ^3)
  SeeAlso
    WeylFilling
    isStandard
    standardTableaux
    normalize
    augmentFilling
    (conjugate, Filling)
    schurModule
    straighten
///

doc ///
  Key
    (conjugate, Filling)
  Headline
    transpose of a Young tableau
  Usage
    conjugate T
  Inputs
    T:Filling
  Outputs
    :Filling
      the tableau of conjugate (transposed) shape whose columns are
      the rows of @TT "T"@
  Description
    Text
      The conjugate partition $\mu'$ of $\mu$ has
      $\mu'_j = \#\{i : \mu_i \ge j\}$.  As a Young diagram, $\mu'$ is
      the reflection of $\mu$ across its main diagonal.  Since Schur
      tableaux are stored column-wise (each @TT "T#j"@ is a column),
      conjugation swaps rows and columns.
    Example
      T = new Filling from {{0,1}, {1,2}}
      T
      conjugate T
    Text
      Conjugation is an involution and intertwines the two
      tableau-storage conventions:
    Example
      new WeylFilling from toList conjugate T
  SeeAlso
    Filling
    WeylFilling
    (conjugate, WeylFilling)
///

doc ///
  Key
    normalize
    (normalize, Filling)
  Headline
    sort each column of a Filling, with Koszul sign
  Usage
    (c, T') = normalize T
  Inputs
    T:Filling
  Outputs
    :Sequence
      a pair $(c, T')$ with $c \in \{-1, 0, 1\}$ and $T'$ either a
      @TT "Filling"@ or @TT "null"@
  Description
    Text
      Each column of a @TT "Filling"@ represents an element of an
      exterior power, so repeated entries in a column collapse the
      corresponding wedge to zero, and reordering a column changes the
      sign by the parity of the permutation.  @TT "normalize T"@
      returns
    Text
      @UL {
        TEX "$(0, \\texttt{null})$ if any column has a repeated entry;",
        TEX "$(1, T')$ if every column sorts into strictly increasing
             order under an even permutation;",
        TEX "$(-1, T')$ if the total permutation is odd."
      }@
    Text
      This is the first step of the straightening algorithm; after it,
      $T'$ lives in the standard ambient
      $\wedge^{\mu'_1}(E) \otimes \cdots \otimes \wedge^{\mu'_k}(E)$
      with a known sign.
    Example
      normalize new Filling from {{1,0}, {2,1}}
      normalize new Filling from {{0,0}, {1,2}}
      normalize new Filling from {{2,1,0}, {1,0}}
  SeeAlso
    Filling
    isStandard
    straighten
///

doc ///
  Key
    isStandard
    (isStandard, Filling)
  Headline
    test whether a Filling is semistandard
  Usage
    isStandard T
  Inputs
    T:Filling
      assumed to have strictly increasing columns (for example, the
      output of @TO normalize@); the routine scans adjacent columns
      right to left for the first row-ascent violation
  Outputs
    :
      @TT "null"@ if @TT "T"@ is semistandard (entries weakly increase
      along every row), otherwise a pair @TT "(i, j)"@ such that
      @TT "T#i#j > T#(i+1)#j"@
  Description
    Text
      In the combinatorial literature a {\em semistandard Young tableau}
      is a filling with entries strictly increasing down columns and
      weakly increasing along rows.  This package calls such a filling
      @EM "standard"@; @TT "isStandard"@ is its predicate.

      The return value on failure is the Garnir-violation location used
      by the straightening algorithm @TO straighten@ and by the main
      Schur-module construction @TO schurModule@.
    Example
      isStandard new Filling from {{0,1}, {1,2}}
      isStandard new Filling from {{1,2}, {0,1}}
    Text
      To enumerate all semistandard fillings of a given shape with
      entries in $\{0,\ldots,d-1\}$, use @TO standardTableaux@.
  SeeAlso
    Filling
    normalize
    standardTableaux
    straighten
    schurModule
///

doc ///
  Key
    standardTableaux
    (standardTableaux, ZZ, List)
  Headline
    all semistandard Young tableaux of a given shape
  Usage
    standardTableaux(d, mu)
  Inputs
    d:ZZ
      the size of the alphabet $\{0,\ldots,d-1\}$
    mu:List
      a partition giving the column lengths of the @TO Filling@
      (equivalently, the conjugate of the row shape $\lambda$;
      see @TO schurModule@)
  Outputs
    :List
      all semistandard @TO Filling@s with column lengths $\mu$
      and entries in $\{0,\ldots,d-1\}$
  Description
    Text
      The output is a $\mathbb{Z}$-basis of @TO schurModule@@TT "(mu, R^d)"@;
      its cardinality equals the Weyl dimension $\dim S_\mu(\mathbb{Q}^d)$,
      given by the hook-content formula
      $$\dim S_\mu(\mathbb{Q}^d) = \prod_{(i,j)\in\mu} \frac{d - i + j}{h(i,j)},$$
      where $h(i,j)$ is the hook length at cell $(i,j)$.
    Example
      S = standardTableaux(3, {2,1});
      #S
      rank schurModule({2,1}, QQ^3)
    Text
      For $\mu = (2,1)$ and $d = 3$ the hook-content formula gives
      $(3 \cdot 4 \cdot 2)/(3 \cdot 1 \cdot 1) = 8$.
    Example
      standardTableaux(2, {1,1})
      standardTableaux(4, {3})
  SeeAlso
    Filling
    isStandard
    schurModule
    standardWeylTableaux
///

doc ///
  Key
    augmentFilling
    (augmentFilling, Filling, ZZ, ZZ)
  Headline
    append an entry to a column of a Filling
  Usage
    augmentFilling(T, c, e)
  Inputs
    T:Filling
    c:ZZ
      column index
    e:ZZ
      entry to append
  Outputs
    :Filling
      a new filling where @TT "e"@ has been appended to the bottom of
      column @TT "c"@, or (if @TT "c >= #T"@) a new rightmost column
      @TT "{e}"@ has been created
  Description
    Text
      This is the natural cell-adding operation for Schur-functor
      combinatorics: extending a filling by one cell at the bottom of
      column $c$ (or creating a new rightmost column if $c$ is past
      the current rightmost column).  It is the building block used to
      describe Schur-module maps by their action on tableaux; see for
      example the Koszul-style differential built in the example of
      @TO schurModulesMap@.
    Example
      T = new Filling from {{0,1}, {1}}
      T
      augmentFilling(T, 0, 2)
      augmentFilling(T, 1, 2)
      augmentFilling(T, 5, 3)
  SeeAlso
    Filling
    augmentWeylFilling
    schurModulesMap
///

-----------------------------------------------------------------------
-- WeylFilling type and tableau combinatorics (Weyl side)
-----------------------------------------------------------------------
doc ///
  Key
    WeylFilling
    (symbol ?, WeylFilling, WeylFilling)
    (symbol ==, WeylFilling, WeylFilling)
    (symbol _, WeylFilling, List)
    (symbol _, WeylFilling, ZZ)
    (net, WeylFilling)
  Headline
    a Young tableau stored row-wise for the Weyl-module basis
  Description
    Text
      A @TT "WeylFilling"@ is a Young tableau of partition shape
      $\lambda$, stored as the list of its rows.  A filling is
      represented by
      @TT "new WeylFilling from {r_1, r_2, ..., r_k}"@,
      where @TT "r_i"@ is the list of entries in the $i$-th row
      (top-to-bottom, left-to-right within each row), of length
      $\lambda_i$.
    Text
      The row-wise storage matches the ambient decomposition of the
      Weyl module
      $$W_\lambda(E) \;\subseteq\; \textstyle\bigotimes_{i=1}^{r}
          D^{\lambda_i}(E),$$
      so each row directly corresponds to one divided-power tensor
      factor.  Entries in a row are stored in sorted (weakly-increasing)
      order since divided powers of a free module have a basis indexed
      by multisets; see @TO dividedPower@.
    Example
      T = weyl {{0,0,1}, {1,2}}
      T
      toList T
      #T
      T#0
    Text
      The constructor @TO "weyl"@ with a single @TT "List"@ argument
      wraps a list of rows into a @TT "WeylFilling"@; the two-argument
      form @TT "weyl(lambda, f)"@ builds the induced module map, see
      @TO (weyl, List, Matrix)@.
    Text
      A @TT "WeylFilling"@ is @EM "Weyl-semistandard"@ (=
      Weyl-standard in the terminology of this package) if its rows are
      weakly increasing and its columns are strictly increasing.  Over
      $\mathbb{Z}$ the Weyl-standard fillings of shape $\lambda$ with
      entries in $\{0,\ldots,d-1\}$ index a free basis of
      $W_\lambda(R^d)$.
    Example
      SW = standardWeylTableaux(3, {2,1});
      #SW == rank weylModule({2,1}, QQ^3)
    Text
      Comparison @TT "?"@, equality @TT "=="@, and row access
      @TT "T_i"@ / @TT "T_{i_0, i_1, ...}"@ are all defined and mirror
      the corresponding operations on @TO Filling@ (with rows in the
      role of columns).
  SeeAlso
    Filling
    weyl
    weylNormalize
    isWeylStandard
    towardWeylStandard
    standardWeylTableaux
    maxWeylFilling
    augmentWeylFilling
    (conjugate, WeylFilling)
    weylModule
    weylStraighten
///

doc ///
  Key
    (conjugate, WeylFilling)
  Headline
    transpose of a Weyl-stored Young tableau
  Usage
    conjugate T
  Inputs
    T:WeylFilling
  Outputs
    :WeylFilling
      the tableau of conjugate shape, stored row-wise (so its rows are
      the columns of @TT "T"@)
  Description
    Text
      Conjugation of a @TT "WeylFilling"@ is the combinatorial transpose
      of the underlying Young tableau, preserving the row-wise storage
      convention.
    Example
      T = weyl {{0,2,4},{1,3,5}}
      conjugate T
    Text
      The conjugate stores the columns of $T$ as its rows; equivalently,
      @TT "toList conjugate T"@ equals the list of columns of
      @TT "T"@, each sorted in its natural top-to-bottom order.
  SeeAlso
    WeylFilling
    (conjugate, Filling)
///

doc ///
  Key
    weylNormalize
    (weylNormalize, WeylFilling)
  Headline
    sort each row of a WeylFilling into weakly-increasing order
  Usage
    weylNormalize T
  Inputs
    T:WeylFilling
  Outputs
    :WeylFilling
      the WeylFilling obtained by sorting each row of @TT "T"@
  Description
    Text
      Each row of a @TT "WeylFilling"@ represents a basis element
      $x^{(a_1)} x^{(a_2)} \cdots \in D^\ell(E)$ of a divided power.
      Divided powers of a free module have a basis indexed by
      {\em unordered} multisets of basis indices (or, equivalently,
      exponent vectors), so two @TT "WeylFilling"@s with the same
      multiset of entries in each row represent the same element of
      the ambient.  @TT "weylNormalize"@ puts every row into its
      canonical sorted representative.
    Text
      Unlike @TO normalize@ on the Schur side, no sign is produced:
      divided powers are cocommutative / symmetric, not alternating.
    Example
      U = new WeylFilling from {{2,0,1}, {3,1}}
      weylNormalize U
  SeeAlso
    WeylFilling
    isWeylStandard
    weylStraighten
///

doc ///
  Key
    isWeylStandard
    (isWeylStandard, WeylFilling)
  Headline
    test whether a WeylFilling is Weyl-semistandard
  Usage
    isWeylStandard T
  Inputs
    T:WeylFilling
      assumed to have rows in sorted (weakly-increasing) order, for
      example the output of @TO weylNormalize@
  Outputs
    :
      @TT "null"@ if @TT "T"@ is Weyl-standard (columns strictly
      increasing top-to-bottom), otherwise a pair @TT "(i, j)"@ giving
      the first column violation: @TT "T#i#j >= T#(i+1)#j"@, scanning
      rows from bottom to top
  Description
    Text
      A @TT "WeylFilling"@ is {\em Weyl-standard} when, in addition to
      having sorted rows, every column is strictly increasing
      top-to-bottom.  This is exactly the semistandard condition,
      phrased for row-wise storage.
    Example
      isWeylStandard weyl {{0,1}, {1,2}}
      isWeylStandard weyl {{1,2}, {1,3}}   -- column 0: 1 >= 1 violates
    Text
      To straighten a non-standard @TT "WeylFilling"@ into a linear
      combination of standard ones, use @TO towardWeylStandard@ (single
      step) or @TO weylStraighten@ (full reduction).
  SeeAlso
    WeylFilling
    weylNormalize
    towardWeylStandard
    weylStraighten
    standardWeylTableaux
///

doc ///
  Key
    towardWeylStandard
  Headline
    a single Garnir-style straightening step for Weyl fillings
  Usage
    towardWeylStandard T
  Inputs
    T:WeylFilling
  Outputs
    H:HashTable
      mapping @TT "WeylFilling"@s @TT "T'"@ to integer coefficients,
      with $\sum_{T'} H(T') \cdot T' = T$ in the Weyl module and every
      key @TT "T'"@ strictly larger than @TT "T"@ in the term order
      used by @TO weylStraighten@
  Description
    Text
      If @TT "T"@ is already Weyl-standard the output is the singleton
      @TT "{T => 1}"@.  Otherwise @TT "towardWeylStandard"@ performs
      one divided-power Garnir shuffle across the first violation
      (located by @TO isWeylStandard@) and returns the resulting linear
      combination.  Repeated application terminates at a
      $\mathbb{Z}$-linear combination of Weyl-standard fillings; this
      is exactly what @TO weylStraighten@ computes.
    Text
      The replacements are generated by divided-power comultiplication
      (see @TO divComult@) followed by divided-power multiplication
      (see @TO divMult@); the coefficients are products of binomial
      coefficients, so in general @TT "towardWeylStandard"@ produces
      many terms unlike the sign-flip shuffle on the Schur side.
    Example
      T = weyl {{1,2},{0,0}}
      isWeylStandard T
      a = towardWeylStandard T
      scan(pairs a, (U, c) -> << "  " << c << " * " << toList U << endl)
  SeeAlso
    WeylFilling
    isWeylStandard
    weylStraighten
    divComult
    divMult
///

doc ///
  Key
    standardWeylTableaux
    (standardWeylTableaux, ZZ, List)
  Headline
    all Weyl-semistandard tableaux of a given shape
  Usage
    standardWeylTableaux(d, lambda)
  Inputs
    d:ZZ
      the size of the alphabet $\{0,\ldots,d-1\}$
    lambda:List
      a partition (the shape of the tableau)
  Outputs
    :List
      all Weyl-semistandard @TO WeylFilling@s of shape $\lambda$ with
      entries in $\{0,\ldots,d-1\}$
  Description
    Text
      The output is a $\mathbb{Z}$-basis of
      @TO weylModule@@TT "(lambda, R^d)"@.  Its cardinality equals
      $\dim S_\lambda(\mathbb{Q}^d)$, the same count given by
      @TO standardTableaux@ for the conjugate shape: $S_\lambda$ and
      $W_\lambda$ have the same rank in every characteristic.
    Example
      SW = standardWeylTableaux(3, {2,1});
      #SW
      rank weylModule({2,1}, QQ^3)
    Example
      standardWeylTableaux(2, {2})
      standardWeylTableaux(3, {1,1,1})
  SeeAlso
    WeylFilling
    isWeylStandard
    weylModule
    standardTableaux
///

doc ///
  Key
    augmentWeylFilling
    (augmentWeylFilling, WeylFilling, ZZ, ZZ)
  Headline
    append an entry to a row of a WeylFilling
  Usage
    augmentWeylFilling(T, r, e)
  Inputs
    T:WeylFilling
    r:ZZ
      row index
    e:ZZ
      entry to append
  Outputs
    :WeylFilling
      a new filling where @TT "e"@ has been appended to the end of
      row @TT "r"@, or (if @TT "r >= #T"@) a new final row
      @TT "{e}"@ has been created
  Description
    Text
      The Weyl analogue of @TO augmentFilling@.  Where @TO Filling@ is
      column-indexed, @TT "WeylFilling"@ is row-indexed; this routine
      grows a tableau by one cell in the natural row-wise direction and
      is the basic building block for describing Weyl-module maps
      tableau-by-tableau (see the example of @TO weylModulesMap@).
    Example
      T = weyl {{0,1},{2}}
      augmentWeylFilling(T, 0, 2)
      augmentWeylFilling(T, 1, 3)
      augmentWeylFilling(T, 2, 4)
  SeeAlso
    WeylFilling
    augmentFilling
    weylModulesMap
///

doc ///
  Key
    maxWeylFilling
    (maxWeylFilling, List, ZZ)
  Headline
    the lex-largest Weyl-standard tableau of given shape
  Usage
    maxWeylFilling(p, d)
  Inputs
    p:List
      a partition
    d:ZZ
      the rank of the underlying module
  Outputs
    :WeylFilling
      the Weyl-standard filling of shape $p$ with entries in
      $\{0,\ldots,d-1\}$ which is lex-largest in the row-reversed order
      used to sort the ambient basis in @TO weylModule@
  Description
    Text
      With $p' = (p'_1, \ldots, p'_{p_1})$ the conjugate of $p$, the
      cell $(i, j)$ (0-indexed) is filled with $d - p'_j + i$.  This
      coincides numerically with the formula for the lex-largest
      semistandard tableau on the Schur side (the internal helper
      @TT "maxFilling"@), but stored row-wise.
    Example
      maxWeylFilling({3, 2}, 4)
      isWeylStandard oo
    Text
      Useful as a starting point for constructing extreme elements of
      highest-weight subrepresentations; see @TO characterRep@ and
      @TO decomposeRep@.
  SeeAlso
    WeylFilling
    isWeylStandard
    standardWeylTableaux
///

-----------------------------------------------------------------------
-- Schur functor and module
-----------------------------------------------------------------------
doc ///
  Key
    schurModule
    (schurModule, List, Module)
  Headline
    Schur module of a partition applied to a free module
  Usage
    schurModule(lambda, E)
  Inputs
    lambda:List
      a partition $\lambda = (\lambda_1 \ge \cdots \ge \lambda_r)$
    E:Module
      a free module over some ring $R$
  Outputs
    :Module
      the Schur module $S_\lambda(E)$, a free $R$-module whose rank
      equals the number of semistandard Young tableaux of shape
      $\lambda$ with entries in $\{0,\ldots,\mathrm{rank}(E) - 1\}$
  Description
    Text
      The Schur functor $S_\lambda$ is a polynomial functor on free
      modules, realized here as the quotient of
      $\wedge^{\lambda'_1}(E) \otimes \cdots \otimes \wedge^{\lambda'_k}(E)$
      by the Garnir relations across adjacent columns of the Young
      diagram.  See Fulton, {\em Young Tableaux}, Chapter 8.

      The partition $\lambda$ is specified as a (non-strictly) decreasing
      list of positive integers.  The rank of the output module matches
      the Weyl dimension formula
      $$\dim S_\lambda(R^d) = \#\{\text{SSYT of shape } \lambda
          \text{ with entries in } 1,\ldots,d\}.$$
    Text
      @STRONG "Cached data."@  The returned module @TT "M"@ stores in
      @TT "M.cache#\"Schur\""@ the 4-tuple @TT "(f, finv, AT, ST)"@:
    Text
      @UL {
        {TT "f", " : the quotient projection ",
         TEX "$\\wedge^{\\lambda'_1}(E) \\otimes \\cdots \\otimes \\wedge^{\\lambda'_k}(E)
              \\twoheadrightarrow M$",
         " (the straightening map, realized in the SSYT basis of ", TT "M", ");"},
        {TT "finv", " : a splitting ", TT "M", " ",
         TEX "$\\to \\wedge^{\\lambda'_1}(E) \\otimes \\cdots$", "
         sending each standard-basis vector to its canonical lift in the
         ambient tensor product (so ", TT "f*finv == id_M", ");"},
        {TT "AT", " : a HashTable keyed by all column-standard Fillings
         of shape ", TEX "$\\lambda$", " with values their positions in the
         ambient tensor product;"},
        {TT "ST", " : the sub-HashTable keyed only by the SSYT
         (the standard basis of ", TT "M", ")."}
      }@
    Text
      These are consumed by @TO straighten@ (for a fast-path module
      evaluation) and by @TO schurModulesMap@ (for building homomorphisms).
    Text
      @STRONG "Examples."@  Symmetric and exterior powers recover the
      familiar functors; with a partition of shape $(k)$ we get
      $\mathrm{Sym}^k(E)$ and with shape $(1^k)$ we get
      $\wedge^k(E)$:
    Example
      E = QQ^3;
      schurModule({3}, E)
      schurModule({1,1,1}, E)   -- top exterior power: rank 1
    Text
      The first genuinely mixed shape:
    Example
      M = schurModule({2,1}, E);
      rank M
      standardTableaux(3, {2,1})
      M.cache#"Schur"
    Text
      The cached quotient projection / splitting satisfy
      @TT "f * finv == id_M"@ by construction:
    Example
      (f, finv, AT, ST) = toSequence M.cache#"Schur";
      f * finv == id_M
  Caveat
    The partition @TT "lambda"@ should be nonempty and weakly
    decreasing.  Entries of @TT "lambda"@ should not exceed
    @TT "rank E"@, or else the output module has rank 0.
  SeeAlso
    schur
    straighten
    schurModulesMap
    standardTableaux
    weylModule
///

doc ///
  Key
    schur
    (schur, List, Matrix)
  Headline
    the Schur functor applied to a matrix
  Usage
    schur(lambda, f)
  Inputs
    lambda:List
      a partition
    f:Matrix
      a map $f : E \to F$ between free modules over a ring $R$
  Outputs
    :Matrix
      the induced map $S_\lambda(f) : S_\lambda(E) \to S_\lambda(F)$
  Description
    Text
      For each partition $\lambda$, $S_\lambda$ is a polynomial functor.
      Applied to a morphism $f : E \to F$ of free modules it yields a
      morphism $S_\lambda(f) : S_\lambda(E) \to S_\lambda(F)$, and the
      construction is functorial:
      $S_\lambda(f \circ g) = S_\lambda(f) \circ S_\lambda(g)$ and
      $S_\lambda(\mathrm{id}_E) = \mathrm{id}_{S_\lambda(E)}$.
    Text
      The source and target matrices have modules that carry the same
      cached data as returned by @TO schurModule@.
    Example
      R = QQ[x_1,x_2,x_3];
      F = map(R^1, R^3, vars R);
      schur({2}, F)                          -- 2nd Veronese embedding
    Text
      Classical specializations.  Top exterior power = determinant:
    Example
      F = matrix{{1_QQ,2,4},{3,9,27},{4,16,64}};
      det F
      schur({1,1,1}, F)
    Text
      Second exterior power = matrix of $2\times2$ minors:
    Example
      entries schur({1,1}, F)
      entries gens minors(2, F)
    Example
      S = schur({3,1}, id_(QQ^3));               -- identity maps to the identity
      S == id_(source S)
    Text
      Composition is compatible with matrix composition:
    Example
      A = random(QQ^3, QQ^3);
      B = random(QQ^3, QQ^3);
      schur({2,1}, A * B) == schur({2,1}, A) * schur({2,1}, B)
  Caveat
    The partition @TT "lambda"@ should be nonempty and weakly
    decreasing.
  SeeAlso
    schurModule
    weyl
///

doc ///
  Key
    straighten
    (straighten, Filling)
    (straighten, HashTable)
    (straighten, Filling, Module)
    (straighten, HashTable, Module)
  Headline
    straighten a filling into a linear combination of semistandard tableaux
  Usage
    straighten T
    straighten H
    straighten(T, M)
    straighten(H, M)
  Inputs
    T:Filling
      a Young tableau of shape $\mu$
    H:HashTable
      a formal $\mathbb{Z}$-linear combination of @TT "Filling"@s
      (keys) with integer coefficients (values); all keys must have the
      same shape
    M:Module
      (optional) a Schur module $S_\mu(R^d)$ produced by
      @TO schurModule@; when supplied, the output is assembled as a
      vector in @TT "M"@
  Outputs
    :
      either a @TT "HashTable"@ keyed by semistandard fillings, with
      $\sum_{T'} H(T')\,T' = T$ in $S_\mu$, or (when @TT "M"@ is
      supplied) the corresponding @TT "Vector"@ in @TT "M"@
  Description
    Text
      The straightening algorithm writes an arbitrary tableau as a
      unique $\mathbb{Z}$-linear combination of semistandard tableaux of
      the same shape, modulo the Garnir relations.  This is the standard
      way to reduce to the SSYT basis of $S_\lambda(E)$; see Fulton,
      {\em Young Tableaux}, §8.1.

      The implementation repeatedly locates a violation with
      @TO isStandard@ and resolves it by a Garnir-style column shuffle,
      keeping track of the intermediate tableaux that still need to be
      rewritten; the process terminates when every tableau is
      semistandard.  The final output does not depend on the order in
      which violations are chosen.
    Text
      @STRONG "Symbolic straightening."@  Without a module argument the
      output is a @TT "HashTable"@ of @TT "Filling"@ keys and integer
      coefficients:
    Example
      T = new Filling from {{2,1,0}}
      straighten T
    Example
      T = new Filling from {{1,0}, {0}}
      straighten T
    Text
      @STRONG "Linearity."@  @TT "straighten"@ is $\mathbb{Z}$-linear
      in its input, so it extends to formal linear combinations
      presented as a @TT "HashTable"@ of (Filling => ZZ) pairs:
    Example
      H = hashTable {
           (new Filling from {{2,1}, {0}}, 1),
           (new Filling from {{1,0}, {2}}, -1)}
      straighten H
    Text
      @STRONG "Module evaluation."@  When a Schur module @TT "M"@ is
      supplied, @TT "straighten"@ returns the element of @TT "M"@
      corresponding to the straightened combination.  This is the usual
      way to lift a combinatorially-specified element to its actual
      vector representation:
    Example
      M = schurModule({1,1,1}, QQ^4);
      v = straighten(new Filling from {{3,2,1}}, M)
      printSchurModuleElement(v, M)
  SeeAlso
    Filling
    isStandard
    normalize
    schurModule
    weylStraighten
///

doc ///
  Key
    schurModulesMap
    (schurModulesMap, Module, Module, Function)
  Headline
    build a map between Schur modules from its action on tableaux
  Usage
    schurModulesMap(N, M, F)
  Inputs
    N:Module
      a Schur module from @TO schurModule@
    M:Module
      a Schur module from @TO schurModule@
    F:Function
      a function describing the map on tableaux: for every SSYT
      @TT "T"@ in the basis of @TT "M"@, @TT "F(T)"@ should produce a
      list of pairs @TT "(c, T')"@ where @TT "c"@ is a coefficient in
      @TT "ring N"@ and @TT "T'"@ is a (not-necessarily-semistandard)
      filling of the shape of @TT "N"@
  Outputs
    :Matrix
      the $R$-linear map @TT "M -> N"@ given, on each basis tableau
      @TT "T"@ of @TT "M"@, by the $R$-linear combination
      $\sum (c, T') \in F(T)$, implicitly straightened into $N$
  Description
    Text
      This constructor is the main tool for producing Schur-module
      homomorphisms from combinatorial data.  It amounts to specifying
      where each tableau in a basis of the source goes, in a
      possibly-non-standard form; the straightening algorithm is invoked
      internally on each output to assemble the resulting vector in
      @TT "N"@.
    Text
      @STRONG "Example: a piece of a Koszul-type differential."@  For
      $\lambda = (1^j)$ and $\lambda' = (1^{j+1})$ the natural map
      $\wedge^j(E) \to \wedge^{j+1}(E) \otimes E^*$ sends a basis
      tableau to $\sum_k x_k \otimes (T \cup \{k\})$.  Lifting this via
      @TT "schurModulesMap"@ gives (a piece of) the Koszul differential:
    Example
      n = 4;
      j = 2;
      R = QQ[x_1..x_n];
      M = schurModule(toList(j : 1), R^n);
      N = schurModule(toList(j+1 : 1), R^n);
      F = T -> apply(numgens R, k -> (R_k, augmentFilling(T, 0, k)));
      schurModulesMap(N, M, F)
  SeeAlso
    schurModule
    augmentFilling
    straighten
    weylModulesMap
///

-----------------------------------------------------------------------
-- Weyl functor and module
-----------------------------------------------------------------------
doc ///
  Key
    weylModule
    (weylModule, List, Module)
  Headline
    Weyl module of a partition applied to a free module
  Usage
    weylModule(lambda, E)
  Inputs
    lambda:List
      a partition $\lambda = (\lambda_1 \ge \cdots \ge \lambda_r)$
    E:Module
      a free module over some ring $R$
  Outputs
    :Module
      the Weyl module $W_\lambda(E)$, a free $R$-module whose rank
      equals the number of Weyl-semistandard fillings of shape
      $\lambda$ with entries in $\{0,\ldots,\mathrm{rank}(E) - 1\}$
  Description
    Text
      The Weyl functor $W_\lambda$ is the divided-power analogue of
      $S_\lambda$.  It is realized here as the quotient
      $$D^{\lambda_1}(E) \otimes \cdots \otimes D^{\lambda_r}(E)
        \;\twoheadrightarrow\; W_\lambda(E)$$
      by the divided-power Garnir relations across adjacent rows;
      see Akin-Buchsbaum-Weyman, {\em Schur functors and Schur
      complexes}, Adv. Math. {\bf 44} (1982).

      In characteristic zero $W_\lambda \cong S_\lambda$ canonically,
      but over $\mathbb{Z}$ (or in positive characteristic) $W_\lambda$
      and $S_\lambda$ are distinct functors related by duality.
    Text
      The rank matches the usual Weyl-dimension count:
    Example
      E = QQ^3;
      weylModule({2}, E)          -- divided square: rank C(3+1, 2) = 6
      rank oo
      weylModule({1,1}, E)        -- W_{(1,1)} = ∧^2, rank 3
      rank oo
    Text
      For a genuinely mixed shape:
    Example
      W = weylModule({2,1}, E);
      rank W
      standardWeylTableaux(3, {2,1})
    Text
      @STRONG "Cached data."@  The returned module @TT "W"@ stores in
      @TT "W.cache#\"Weyl\""@ the 4-tuple @TT "(f, finv, AT, ST)"@,
      exactly parallel to @TO schurModule@:
    Example
      W.cache#"Weyl"
      (f, finv, AT, ST) = toSequence W.cache#"Weyl";
      f * finv == id_W
  Caveat
    The partition @TT "lambda"@ should be nonempty and weakly
    decreasing.  Entries of @TT "lambda"@ need not bound the rank of
    @TT "E"@, but the output module may of course have small rank.
  SeeAlso
    weyl
    weylStraighten
    weylModulesMap
    standardWeylTableaux
    schurModule
    dividedPower
///

doc ///
  Key
    weyl
    (weyl, List)
    (weyl, List, Matrix)
  Headline
    construct a WeylFilling or apply the Weyl functor to a map
  Usage
    weyl L
    weyl(lambda, f)
  Inputs
    L:List
      a list of rows, each row a list of integers (weakly increasing);
      interpreted as the rows of a @TT "WeylFilling"@
    lambda:List
      a partition
    f:Matrix
      a map $f : E \to F$ between free modules
  Outputs
    :
      in the one-argument form, a @TT "WeylFilling"@;
      in the two-argument form, a @TT "Matrix"@ giving the induced map
      $W_\lambda(f) : W_\lambda(E) \to W_\lambda(F)$
  Description
    Text
      The one-argument form is a constructor for @TO WeylFilling@:
    Example
      T = weyl {{0,0,1}, {1,2}}
      class T
      isWeylStandard T
    Text
      The two-argument form is the Weyl functor applied to a morphism
      of free modules.  Functoriality: $W_\lambda(f \circ g) =
      W_\lambda(f) \circ W_\lambda(g)$ and
      $W_\lambda(\mathrm{id}) = \mathrm{id}$:
    Example
      F = matrix{{1_QQ,2},{3,4}};
      weyl({2}, F)
    Example
      A = random(QQ^3, QQ^3);
      B = random(QQ^3, QQ^3);
      weyl({2,1}, A * B) == weyl({2,1}, A) * weyl({2,1}, B)
    Text
      For special shapes $W_\lambda$ coincides with familiar functors:
      $W_{(d)} = D^d$ (divided power), $W_{(1^d)} = \wedge^d$
      (exterior power, also equal to $S_{(1^d)}$).
    Example
      weyl({2}, F) == dividedPower(2, F)
  SeeAlso
    WeylFilling
    weylModule
    dividedPower
    schur
///

doc ///
  Key
    weylStraighten
    (weylStraighten, WeylFilling)
    (weylStraighten, HashTable)
    (weylStraighten, WeylFilling, Module)
    (weylStraighten, HashTable, Module)
  Headline
    straighten a Weyl filling into a linear combination of Weyl-semistandard tableaux
  Usage
    weylStraighten T
    weylStraighten H
    weylStraighten(T, W)
    weylStraighten(H, W)
  Inputs
    T:WeylFilling
    H:HashTable
      a formal $\mathbb{Z}$-linear combination of @TT "WeylFilling"@s
      (all of the same shape)
    W:Module
      (optional) a Weyl module produced by @TO weylModule@
  Outputs
    :
      a @TT "HashTable"@ keyed by Weyl-semistandard fillings, or (when
      a module is supplied) the corresponding @TT "Vector"@ in @TT "W"@
  Description
    Text
      The divided-power analogue of @TO straighten@.  Reduces an
      arbitrary @TT "WeylFilling"@ (or a formal combination of them)
      to a unique linear combination of Weyl-standard tableaux modulo
      the divided-power Garnir relations.  Unlike on the Schur side,
      a single straightening step generally produces many terms with
      binomial-coefficient multiplicities (via @TO divComult@ /
      @TO divMult@) rather than a single sign flip.
    Example
      U = weyl {{1,2},{0,0}}
      weylStraighten U
    Text
      @STRONG "Linearity."@
    Example
      H = hashTable {
           (weyl {{1,2},{0,0}}, 1),
           (weyl {{0,2},{1,0}}, -1)};
      weylStraighten H
    Text
      @STRONG "Module evaluation."@  With a Weyl module @TT "W"@
      supplied, the combinatorial result is assembled as a vector:
    Example
      W = weylModule({2,2}, QQ^3);
      v = weylStraighten(weyl {{0,2},{1,0}}, W)
      printWeylModuleElement(v, W)
  SeeAlso
    WeylFilling
    isWeylStandard
    towardWeylStandard
    weylModule
    straighten
///

doc ///
  Key
    weylModulesMap
    (weylModulesMap, Module, Module, Function)
  Headline
    build a map between Weyl modules from its action on tableaux
  Usage
    weylModulesMap(N, M, F)
  Inputs
    N:Module
      a Weyl module from @TO weylModule@
    M:Module
      a Weyl module from @TO weylModule@
    F:Function
      a function such that for every Weyl-semistandard filling @TT "T"@
      in the basis of @TT "M"@, @TT "F(T)"@ produces a list of pairs
      @TT "(c, T')"@ with @TT "c"@ in @TT "ring N"@ and @TT "T'"@ a
      (not-necessarily-standard) filling of the shape of @TT "N"@
  Outputs
    :Matrix
      the $R$-linear map @TT "M -> N"@ whose value on each basis
      tableau is straightened into @TT "N"@
  Description
    Text
      The Weyl analogue of @TO schurModulesMap@.  The shape of @TT "N"@
      is the target shape of the tableaux produced by @TT "F"@; the
      straightening into the Weyl-standard basis is performed
      internally using @TO weylStraighten@.
    Text
      @STRONG "Example: a multiplication-style map
      $W_{(2)}(E) \\to W_{(2,1)}(E) \\otimes E^*$."@
      The analogue of the Koszul differential, with divided-power
      multiplication:
    Example
      n = 3;
      R = QQ[x_1..x_n];
      M = weylModule({2}, R^n);
      N = weylModule({2,1}, R^n);
      F = T -> apply(numgens R, k -> (R_k, augmentWeylFilling(T, 1, k)));
      weylModulesMap(N, M, F)
  SeeAlso
    weylModule
    augmentWeylFilling
    weylStraighten
    schurModulesMap
///

-----------------------------------------------------------------------
-- Divided-power utilities
-----------------------------------------------------------------------
doc ///
  Key
    dividedPower
    (dividedPower, ZZ, Module)
    (dividedPower, List, Module)
    (dividedPower, ZZ, Matrix)
    (dividedPower, List, Matrix)
  Headline
    divided-power functor on free modules and matrices
  Usage
    dividedPower(d, E)
    dividedPower(lambda, E)
    dividedPower(d, f)
    dividedPower(lambda, f)
  Inputs
    d:ZZ
    lambda:List
      a list of nonnegative integers; the output is the tensor product
      of divided powers $D^{\lambda_1}(\cdot) \otimes \cdots \otimes D^{\lambda_k}(\cdot)$
    E:Module
      a free module
    f:Matrix
      a map between free modules
  Outputs
    :
      the divided power $D^d(E)$ or the induced map $D^d(f)$, and for
      list input the tensor product of divided powers
  Description
    Text
      The {\em $d$-th divided power} $D^d(E)$ of a free module $E$ of
      rank $n$ is the degree-$d$ part of the divided-power algebra on
      $E$.  It is free of rank $\binom{n+d-1}{d}$, with a basis indexed
      by weakly-increasing tuples of length $d$ in $\{0,\ldots,n-1\}$
      (equivalently, compositions of $d$ into $n$ parts, or
      multisets of size $d$ from $\{0,\ldots,n-1\}$).
    Text
      Over a field of characteristic zero $D^d(E) \cong \mathrm{Sym}^d(E)$
      canonically (the isomorphism rescales basis vectors by factorials);
      over $\mathbb{Z}$ the two functors differ.
    Example
      E = QQ^3;
      dividedPower(2, E)                   -- rank C(4,2) = 6
      dividedPower({2,1,0,1}, E)           -- D^2 ⊗ D^1 ⊗ D^0 ⊗ D^1
      f = matrix{{1_QQ,2},{3,4}};
      dividedPower(2, f)                   -- induced map on D^2
    Text
      The single-row (non-list) form with a @TT "List"@ partition
      builds the ambient tensor product used by @TO weylModule@:
    Example
      dividedPower({2,1}, E) == dividedPower(2, E) ** dividedPower(1, E)
    Text
      @TT "dividedPower"@ is used as the ambient object in the
      construction of @TO weylModule@, playing the role that
      @TT "exteriorPower"@ plays for @TO schurModule@.
  SeeAlso
    weyl
    weylModule
    divMult
    divComult
///

doc ///
  Key
    divMult
    (divMult, List, List)
    (divMult, List, List, ZZ)
    (divMult, Sequence, Sequence)
    divComult
    (divComult, List, ZZ)
    (divComult, List, ZZ, ZZ)
    (divComult, Sequence, ZZ)
  Headline
    multiplication and comultiplication of divided powers
  Description
    Text
      The divided-power algebra $D^*(E) = \bigoplus_{d \ge 0} D^d(E)$
      on a free module $E$ of rank $n$ is a graded bi-algebra.  Its
      basis elements are indexed by weakly-increasing tuples of
      integers, or equivalently by their exponent vectors.  This
      package provides both the multiplication
      $D^p \otimes D^q \to D^{p+q}$ and the comultiplication
      $D^{p+q} \to D^p \otimes D^q$ on those bases.
    Text
      @STRONG "Multiplication."@  @TT "divMult(L1, L2)"@ takes two
      weakly-increasing lists @TT "L1"@, @TT "L2"@ (basis elements of
      $D^{|L_1|}$ and $D^{|L_2|}$) and returns the pair @TT "(c, L)"@
      where @TT "L"@ is the sorted concatenation and @TT "c"@ is the
      binomial coefficient encoding the divided-power product rule
      $x^{(p)} \cdot x^{(q)} = \binom{p+q}{p}\, x^{(p+q)}$.
    Example
      divMult({0,0,1}, {0,1})
      -- coefficient C(3,2) * C(2,1) accounting for the two '0's meeting
      -- the single '0' and the two '1's meeting another '1'
    Text
      @STRONG "Comultiplication."@  @TT "divComult(L, p)"@ takes a
      basis element @TT "L"@ of $D^{|L|}$ and returns the set of ways
      to split it as a pair @TT "(A, B)"@ with @TT "|A| = p"@,
      @TT "|B| = |L| - p"@, and $A + B = L$ as multisets.  This is the
      comultiplication
      $D^{|L|}(E) \to D^{p}(E) \otimes D^{|L| - p}(E)$.
    Example
      divComult({0,0,1,2}, 2)
    Text
      Internally, @TT "divComult"@ drives the Weyl-side straightening:
      a single Garnir shuffle across adjacent rows proceeds by
      comultiplying the concatenated row tails and remultiplying into
      the partner row; see @TO towardWeylStandard@.  A basis element
      can be stored either as a weakly-increasing tuple or as an
      exponent vector recording the multiplicity of each letter; for
      efficiency, much of the straightening uses the exponent-vector
      form, and both forms are accepted as input below.
    Text
      @STRONG "Variants."@  Both operations come in three forms:
    Text
      @UL {
        {TT "divMult(L1, L2)", " and ", TT "divComult(L, p)", ": tuple
         (weakly-increasing list) input; the alphabet size is inferred
         from the maximum entry appearing in the input."},
        {TT "divMult(L1, L2, n)", " and ", TT "divComult(L, p, n)",
         ": tuple input with an explicit alphabet size ", TT "n",
         ", useful when the tuples are padded or when the caller
         needs all output tuples to have a uniform length."},
        {TT "divMult(S1, S2)", " and ", TT "divComult(S, p)", ":
         ", EM "sequence", " input, interpreted as an exponent vector
         (that is, ", TT "S_i", " is the multiplicity of the letter
         ", TT "i", ").  Working directly with exponent vectors avoids
         the conversion to and from tuples, and is the form used
         inside the straightening loop."}
      }@
  SeeAlso
    dividedPower
    weylModule
    towardWeylStandard
///

-----------------------------------------------------------------------
-- Pretty-printers
-----------------------------------------------------------------------
doc ///
  Key
    printSchurModuleElement
    (printSchurModuleElement, Vector, Module)
  Headline
    display a Schur-module element as a linear combination of tableaux
  Usage
    printSchurModuleElement(v, M)
  Inputs
    v:Vector
      a vector in a Schur module @TT "M"@
    M:Module
      a Schur module returned by @TO schurModule@
  Description
    Text
      Interprets @TT "v"@ as an element of @TT "M"@ and prints it as a
      sum $\sum_T c_T \cdot T$ over semistandard fillings $T$ in the
      cached basis of @TT "M"@, with each @TT "T"@ drawn via its
      @TT "net"@ method.
    Example
      M = schurModule({2,1}, QQ^3);
      v = sum(numgens M, i -> (i+1) * M_i);
      printSchurModuleElement(v, M)
  SeeAlso
    schurModule
    printWeylModuleElement
///

doc ///
  Key
    printWeylModuleElement
    (printWeylModuleElement, Vector, Module)
  Headline
    display a Weyl-module element as a linear combination of tableaux
  Usage
    printWeylModuleElement(v, W)
  Inputs
    v:Vector
      a vector in a Weyl module @TT "W"@
    W:Module
      a Weyl module returned by @TO weylModule@
  Description
    Text
      The Weyl analogue of @TO printSchurModuleElement@: prints @TT "v"@
      as $\sum_T c_T \cdot T$ over Weyl-semistandard @TT "WeylFilling"@s
      $T$ in the cached basis of @TT "W"@.
    Example
      W = weylModule({2,1}, QQ^3);
      v = sum(numgens W, i -> (i+1) * W_i);
      printWeylModuleElement(v, W)
  SeeAlso
    weylModule
    printSchurModuleElement
///

-----------------------------------------------------------------------
-- Characters and GL-representation decomposition
-----------------------------------------------------------------------
doc ///
  Key
    character
    (character, List, ZZ)
  Headline
    the character of a composition of Schur functors on GL(V)
  Usage
    character(L, d)
  Inputs
    L:List
      a list of partitions $[\lambda^{(1)}, \lambda^{(2)}, \ldots, \lambda^{(m)}]$
    d:ZZ
      the rank of the underlying module
  Outputs
    :RingElement
      the character, as a symmetric polynomial in $x_0, \ldots, x_{d-1}$,
      of the composed Schur functor
      $S_{\lambda^{(1)}}(S_{\lambda^{(2)}}(\cdots S_{\lambda^{(m)}}(V)))$
      on the defining representation $V = \mathbb{Q}^d$ of $GL_d$
  Description
    Text
      The character of a polynomial $GL_d$-representation $W$ is the
      trace of the diagonal matrix $\mathrm{diag}(x_0, \ldots, x_{d-1})$
      acting on $W$.  For an irreducible $S_\lambda(V)$ this is the
      Schur polynomial $s_\lambda(x_0, \ldots, x_{d-1})$; for an
      arbitrary polynomial representation it is a non-negative integer
      combination of Schur polynomials, making @TT "character"@ the
      primary input to @TO splitCharacter@ and @TO decomposeRep@.
    Example
      character({{2,1}}, 3)
    Text
      Nested compositions are supported; for example the GL_4 character
      of $\wedge^3(S^2 V) = S_{(1,1,1)}(S_{(2)}(V))$:
    Example
      character({{1,1,1}, {2}}, 4)
    Text
      To split such a character into irreducibles, apply
      @TO splitCharacter@:
    Example
      splitCharacter character({{1,1,1}, {2}}, 4)
  SeeAlso
    weylCharacter
    splitCharacter
    characterRep
    decomposeRep
    schur
///

doc ///
  Key
    weylCharacter
    (weylCharacter, List, ZZ)
  Headline
    the character of a composition of Weyl functors on GL(V)
  Usage
    weylCharacter(L, d)
  Inputs
    L:List
      a list of partitions
    d:ZZ
      the rank of the underlying module
  Outputs
    :RingElement
      the character, as a symmetric polynomial in $x_0, \ldots, x_{d-1}$,
      of the composed Weyl functor on the defining representation
  Description
    Text
      The Weyl analogue of @TO character@, using @TO weyl@ instead of
      @TO schur@.  Over a field of characteristic zero
      $W_\lambda(V) \cong S_\lambda(V)$ canonically, so @TT "weylCharacter"@
      returns the same polynomial as @TT "character"@; this routine is
      provided for uniformity and for verifying that the diagonal-character
      computation goes through the Weyl-side construction end-to-end.
    Example
      toString character({{2,1}}, 3) == toString weylCharacter({{2,1}}, 3)
      splitCharacter weylCharacter({{1,1,1}, {2}}, 4)
  SeeAlso
    character
    splitCharacter
    weyl
///

doc ///
  Key
    splitCharacter
    (splitCharacter, RingElement)
  Headline
    decompose a symmetric polynomial into Schur functions
  Usage
    splitCharacter(p)
  Inputs
    p:RingElement
      a symmetric polynomial in the eigenvalues of the diagonal torus
  Outputs
    :RingElement
      the expansion of @TT "p"@ as a $\mathbb{Z}_{\ge 0}$-linear
      combination of Schur functions $s_\lambda$
  Description
    Text
      Together with @TO character@ this gives the isotypic
      decomposition of a polynomial $GL_d$-representation $W$ whose
      character is @TT "p"@:
      $$\chi_W = \sum_\lambda m_\lambda \cdot s_\lambda
         \;\Longleftrightarrow\;
         W \cong \bigoplus_\lambda S_\lambda(V)^{\oplus m_\lambda}.$$
      The coefficients $m_\lambda$ are the multiplicities of the
      irreducible polynomial $GL_d$-representations in $W$.
    Example
      c = character({{1,1,1}, {2}}, 4);
      splitCharacter c
    Text
      Verifies the Hermite-reciprocity / plethysm identity
      $\wedge^3(S^2 V_4) = S_{(4,1,1)}(V_4) \oplus S_{(3,3)}(V_4)$.
  SeeAlso
    character
    weylCharacter
    decomposeRep
///

doc ///
  Key
    characterRep
    (characterRep, Matrix)
  Headline
    extract the character of a polynomial GL-representation from its matrix
  Usage
    characterRep F
  Inputs
    F:Matrix
      a square matrix over a polynomial ring $R[w_1, \ldots, w_{d^2}]$
      representing a polynomial $GL_d$-action on some module; typically
      the output of @TO schur@ applied to a generic matrix
  Outputs
    :RingElement
      the trace of the diagonal specialization of @TT "F"@ in a fresh
      polynomial ring $\mathbb{Q}[x_0, \ldots, x_{d-1}]$
  Description
    Text
      Given a matrix $F$ whose entries are polynomials in the
      $d^2$ variables of a generic matrix on $V = \mathbb{Q}^d$,
      @TT "characterRep"@ substitutes a diagonal matrix
      $\mathrm{diag}(x_0, \ldots, x_{d-1})$ for that generic matrix and
      reads off the trace.  The result is the character of the
      $GL_d$-representation $F$ encodes.
    Example
      R = QQ[w_1..w_9];
      G = genericMatrix(R, 3, 3);
      F = schur({2,1}, G);
      characterRep F
      splitCharacter characterRep F
  SeeAlso
    character
    splitCharacter
    decomposeRep
    schur
///

doc ///
  Key
    decomposeRep
    (decomposeRep, Matrix)
  Headline
    decompose a polynomial GL-representation into irreducible subspaces
  Usage
    decomposeRep F
  Inputs
    F:Matrix
      a square matrix representing a polynomial $GL_d$-action, as in
      @TO characterRep@
  Outputs
    :HashTable
      keyed by the partitions $\lambda$ appearing in the decomposition
      of @TT "F"@ (as returned by @TO splitCharacter@), with each value
      a matrix whose columns span the $\lambda$-isotypic subspace
  Description
    Text
      Given a polynomial $GL_d$-representation $W$ whose matrix is
      @TT "F"@, @TT "decomposeRep"@ computes the character via
      @TO characterRep@, decomposes it via @TO splitCharacter@, and for
      each irreducible $S_\lambda$ appearing produces a basis of the
      $S_\lambda$-isotypic subspace as the syzygy space of two
      operators:
    Text
      @UL {
        "the transvections (upper-triangular unipotent generators)
         must act trivially on a highest-weight vector;",
        "the diagonal torus must act by the maximal-weight character
         of the $\\lambda$-isotypic piece."
      }@
    Text
      The partitions appearing as keys, and their multiplicities (as
      the number of columns of the corresponding basis matrix), agree
      with the output of @TT "splitCharacter characterRep F"@.
    Example
      R = QQ[w_1..w_9];
      G = genericMatrix(R, 3, 3);
      H = decomposeRep schur({2}, schur({2}, G));
      keys H
  SeeAlso
    character
    splitCharacter
    characterRep
    schur
///

-----------------------------------------------------------------------
-- Regression tests
-----------------------------------------------------------------------

TEST ///
      M = schurModule({2,2,2}, QQ^4)
      assert(rank M == 10)
      (f, finv, AT, ST) = toSequence M.cache#"Schur";
      assert(f*finv == map(QQ^10))
      M = schurModule({1,1,1}, QQ^4);
      v = straighten(new Filling from {{3,2,1}}, M)
      assert(v == vector{0_QQ,0,0,-1})
///

TEST ///
      c = character({{1,1,1},{2}}, 4)
      assert(splitCharacter(c) == s_(4,1,1) + s_(3,3))
///

TEST ///
      -- Weyl module rank sanity
      assert(rank weylModule({2,1}, QQ^3) == 8)
      assert(rank weylModule({1,1,1}, QQ^4) == 4)
      assert(rank weylModule({3}, QQ^2) == 4)
///

TEST ///
      -- Schur / Weyl character agreement (in char 0)
      assert(toString character({{2,1}}, 3) == toString weylCharacter({{2,1}}, 3))
      assert(toString character({{3,1}}, 3) == toString weylCharacter({{3,1}}, 3))
///

TEST ///
      -- standardTableaux dimensions (mu = column lengths)
      assert(#standardTableaux(3, {2,1}) == 8)
      assert(#standardWeylTableaux(3, {2,1}) == 8)
      assert(#standardTableaux(4, {3}) == 4)
///

TEST ///
      -- isStandard / isWeylStandard basic sanity
      assert(isStandard(new Filling from {{0,1},{1,2}}) === null)
      assert(isStandard(new Filling from {{1,2},{0,1}}) =!= null)
      assert(isWeylStandard(weyl {{0,1},{1,2}}) === null)
      assert(isWeylStandard(weyl {{1,2},{1,3}}) =!= null)
///

TEST ///
      -- augmentWeylFilling basic sanity
      T = weyl {{0,1},{2}}
      assert(toList augmentWeylFilling(T, 0, 2) == {{0,1,2},{2}})
      assert(toList augmentWeylFilling(T, 2, 5) == {{0,1},{2},{5}})
///

-- schur: the Schur functor on a map -- identity, determinant, functoriality
TEST ///
  F = matrix{{1_QQ,2,4},{3,9,27},{4,16,64}}
  assert instance(schur({2}, F), Matrix)
  -- top exterior power S_(1,1,1) is the determinant
  assert(schur({1,1,1}, F) == matrix{{det F}})
  -- the functor preserves the identity
  s = schur({3,1}, id_(QQ^3))
  assert(s == id_(source s))
  -- functoriality: S_lambda(A*B) = S_lambda(A) * S_lambda(B)
  A = matrix{{1_QQ,2,0},{0,1,3},{1,0,1}}
  B = matrix{{2_QQ,0,1},{1,1,0},{0,1,2}}
  assert(schur({2,1}, A*B) == schur({2,1}, A) * schur({2,1}, B))
///

-- schurModulesMap / weylModulesMap: the identity-on-tableaux specification gives the identity map
TEST ///
  M = schurModule({2,1}, QQ^3)
  sm = schurModulesMap(M, M, T -> {(1_QQ, T)})
  assert instance(sm, Matrix)
  assert(sm == id_M)
  W = weylModule({2,1}, QQ^3)
  assert(weylModulesMap(W, W, T -> {(1_QQ, T)}) == id_W)
///

-- normalize / augmentFilling: Schur-side tableau helpers
TEST ///
  -- normalize sorts each column, recording the Koszul sign
  (c, Tn) = normalize(new Filling from {{1,0},{2,1}})
  assert(c == 1)
  assert(toList Tn == {{0,1},{1,2}})
  -- a repeated entry in a column collapses the wedge to zero
  assert(normalize(new Filling from {{0,0},{1,2}}) === (0, null))
  -- augmentFilling appends an entry to a column (or starts a new rightmost column)
  T = new Filling from {{0,1},{1}}
  assert(toList augmentFilling(T,0,2) == {{0,1,2},{1}})
  assert(toList augmentFilling(T,1,2) == {{0,1},{1,2}})
  assert(toList augmentFilling(T,5,3) == {{0,1},{1},{3}})
///

-- weylNormalize / maxWeylFilling / towardWeylStandard: Weyl-side tableau helpers
TEST ///
  assert instance(weyl {{0,0,1},{1,2}}, WeylFilling)
  -- weylNormalize sorts each row (no sign -- divided powers are symmetric)
  assert(toList weylNormalize(new WeylFilling from {{2,0,1},{3,1}}) == {{0,1,2},{1,3}})
  -- maxWeylFilling produces a Weyl-standard tableau of the given shape
  mw = maxWeylFilling({3,2}, 4)
  assert(toList mw == {{2,2,3},{3,3}})
  assert(isWeylStandard mw === null)
  -- towardWeylStandard of an already-standard filling is the singleton {T => 1}
  Tstd = weyl {{0,1},{1,2}}
  H = towardWeylStandard Tstd
  assert(#H == 1)
  assert(H#Tstd === 1)
///

-- weylStraighten: divided-power straightening into the Weyl-standard basis
TEST ///
  assert instance(weylStraighten weyl {{1,2},{0,0}}, HashTable)
  -- an already-Weyl-standard filling straightens to itself
  Tstd = weyl {{0,1},{1,2}}
  H = weylStraighten Tstd
  assert(#H == 1)
  assert(H#Tstd === 1)
  -- the module-evaluation form lands in the supplied Weyl module
  W = weylModule({2,2}, QQ^3)
  assert(class weylStraighten(weyl {{0,2},{1,0}}, W) === W)
///

-- dividedPower: rank formula, tensor-product identity, functoriality
TEST ///
  d2 = dividedPower(2, QQ^3)
  -- D^d of a rank-n free module is free of rank binomial(n+d-1, d)
  assert(rank d2 == binomial(4,2))
  assert(rank dividedPower(3, QQ^4) == binomial(6,3))
  -- the List form is the tensor product of divided powers
  assert(dividedPower({2,1}, QQ^3) == d2 ** dividedPower(1, QQ^3))
  -- the functor preserves the identity
  dp = dividedPower(2, id_(QQ^3))
  assert(dp == id_(source dp))
///

-- divMult / divComult: divided-power multiplication and comultiplication
TEST ///
  -- divMult returns {coefficient, sorted concatenation}
  assert(divMult({0,0,1}, {0,1}) == {6, {0,0,0,1,1}})
  -- divComult lists every way to split L into a pair (A,B) with #A = p
  assert(divComult({0,0,1,2}, 2) == {{{0,0},{1,2}}, {{0,1},{0,2}}, {{0,2},{0,1}}, {{1,2},{0,0}}})
///

-- characterRep / decomposeRep: read a polynomial GL-representation off a matrix
TEST ///
  Rw = QQ[w_1..w_9]
  G = genericMatrix(Rw, 3, 3)
  -- the character read from schur({2,1}, genericMatrix) matches the abstract character
  assert(toString characterRep schur({2,1}, G) == toString character({{2,1}}, 3))
  -- S_2 of the standard representation is irreducible
  assert(keys decomposeRep schur({2}, G) === {{2}})
///

end
restart
loadPackage "SchurFunctors"
debug SchurFunctors
help SchurFunctors
installPackage "SchurFunctors"

-- Historical note: the 2009 TODO list (Mike, Anton, Mauricio -- 3/26/09)
-- has been addressed by the current overhaul:
--   1. mathematical documentation for schur / schurModule                 : DONE
--   2. Filling type: standard/semistandard check, transpose, listing,
--      to/from lists, pretty printing, example of basis correspondence   : DONE
--   3. documentation + examples for character, splitCharacter             : DONE
--   4. documentation for decomposeRep, characterRep, augmentFilling       : DONE
