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
   	  Version => "2.0",
	  Date => "May 1, 2026",
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
		    },
		   {
		    Name => "Keller VandeBogert",
		    Email => "keller.v@uky.edu",
		    HomePage => "https://sites.nd.edu/kellerv/"
		    }},
	  Headline => "Pieri inclusions and projections between Schur modules with multiple basis conventions",
	  Keywords => {"Representation Theory"},
	  DebuggingMode => false,
	  AuxiliaryFiles => true
	  )

-- SchurFunctors is needed for the convention-aware features (Filling /
-- WeylFilling bases, column-form Pieri, etc.) but is loaded lazily because
-- PieriMaps and SchurFunctors both export several symbols (`straighten`,
-- `standardTableaux`, `isStandard`) and we want PieriMaps's versions to take
-- precedence on the row-form side.  Convention features call ensureSchurFn
-- to lazily load SchurFunctors and resolve its symbols via its private
-- dictionary.

schurFnLoaded = false
sfStraightenPM = null
sfStandardTabPM = null
sfFillingType = null
sfWeylFillingType = null
sfWeylFn = null

-- PieriMaps' Filling/Weyl convention features depend on SchurFunctors v >= 1.1
-- (which exports `straighten Filling`, `straighten HashTable`, and the
-- `WeylFilling` / `weyl` symbols).  The system-installed version in some M2
-- distributions is older and only registers `straighten (Filling, Module)`.
-- We load lazily and detect the incomplete state at first use so the user
-- gets a clear, actionable error rather than a cryptic "no method for
-- straighten" later in the call stack.
ensureSchurFn = () -> (
     if schurFnLoaded then return;
     needsPackage "SchurFunctors";
     sfPkg := value getGlobalSymbol "SchurFunctors";
     d := sfPkg#"private dictionary";
     sfStraightenPM = value (d#"straighten");
     if #methods sfStraightenPM < 2 then error(
	  "PieriMaps' Filling and Weyl conventions require an updated " |
	  "SchurFunctors that exports `straighten Filling` directly.  The " |
	  "currently loaded SchurFunctors only has the (Filling, Module) " |
	  "signature.  Install the newer SchurFunctors and load it BEFORE " |
	  "PieriMaps via `loadPackage(\"SchurFunctors\", FileName => \"<path>\")`."
	  );
     sfStandardTabPM = value (d#"standardTableaux");
     sfFillingType = value (d#"Filling");
     -- WeylFilling and `weyl` are present only in the updated SchurFunctors;
     -- they are optional (Weyl convention requires them).
     if d#?"WeylFilling" then sfWeylFillingType = value (d#"WeylFilling");
     if d#?"weyl" then sfWeylFn = value (d#"weyl");
     schurFnLoaded = true;
     )

export {
     -- Original PieriMaps (unchanged signatures unless noted):
     "straighten", "standardTableaux", "pieri", "pureFree", "schurRank",
     -- LR maps (added in this overhaul):
     "lrTableaux", "lrMap", "applyLR", "displayLRImage",
     -- Convention-aware overhaul (Convention => "Row" | "Filling" | "Weyl"
     -- option on pieri / lrMap / applyLR / pureFree).  These are also exposed
     -- as standalone functions for explicit basis access:
     "Convention",
     "pmToFilling", "fillingToPM", "pmToWeyl", "weylToPM",
     "weylToFilling", "fillingToWeyl",
     "pmToFillingMatrix", "fillingToPMMatrix",
     -- Native column-form (SkewCommutative-target) Pieri:
     "pieriColumn",
     -- Convention compatibility checker:
     "verifyWellDefined",
     -- Dual / projection-direction maps (GL-equivariant):
     "dualPieri", "dualLR", "applyDualPieri", "applyDualLR",
     -- Inclusion-direction point evaluators:
     "applyPieri", "applyPieriColumn",
     -- Column-form (wedge-target) projection direction:
     "dualPieriColumn", "applyDualPieriColumn",
     -- General GL-equivariance check for matrices between Schur reps:
     "verifyEquivariant", "Direction",
     -- Pretty-print a matrix as a basis-labeled symbolic map:
     "symbolicForm"
     }

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
isStandardPM = T -> (
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
shuffle' = (T, col, row1, row2) -> (
     len1 := #(T#row1);
     len2 := #(T#row2);
     truncatedrow1 := (T#row1)_{0..col-2}; -- grab row1 entries
     truncatedrow2 := (T#row2)_{col..len2-1}; -- grab row2 entries
     L := join((T#row1)_{col-1..len1-1}, (T#row2)_{0..col-1});
     P := permutations L;
     P = apply(P, i-> (for j from 0 to #T-1 list (
		    if j == row1 then sort join(truncatedrow1, i_{0..len1-col})
	       	    else if j == row2 then sort join(i_{len1-col+1..#i-1}, truncatedrow2)
	       	    else T#j)));
     coeff := 0;
     for i in P do if i == T then coeff = coeff + 1;
     output := for i in P list if i != T then (i, -1 / coeff) else continue;
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
     x := isStandardPM T;
     if x === null then return new HashTable from {T=>1};
     H := new MutableHashTable from shuffle'(T, x#1+1, x#0, x#0+1);
     if H #? T then (
	  coeff := -(H#T) + 1;
	  remove(H,T);
	  prehash := for i in keys H list (i, H#i / coeff);
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
subtractOne = (mu, k) ->
     for i from 0 to #mu - 1 list (if i == k - 1 then mu_i - 1 else mu_i)

-------------------------------------------------------------------------------
-- Convention infrastructure (added in this overhaul).
--
-- These helpers are used by the convention-aware versions of pieri / lrMap /
-- applyLR / pureFree (Convention => "Row" | "Filling" | "Weyl") and by the
-- standalone basis conversions pmToFilling / fillingToPM / pmToWeyl /
-- weylToPM.  They depend on the SchurFunctors package being loaded
-- (PackageImports above).
-------------------------------------------------------------------------------

-- Sign-tracked sort.  Returns (sign, sortedList) with sign in {1, -1}, or null
-- if the input has a repeat.
sortWithSignPM = L -> (
     s := new MutableList from L;
     n := #s;
     swaps := 0;
     for i from 0 to n-2 do
          for j from 0 to n-i-2 do (
               if s#j === s#(j+1) then return null;
               if s#j > s#(j+1) then (
                    t := s#(j+1); s#(j+1) = s#j; s#j = t;
                    swaps = swaps + 1;
                    );
               );
     ((if swaps % 2 == 0 then 1 else -1), toList s)
     )

factorialPM = n -> if n <= 1 then 1 else n * factorialPM(n - 1)

-- Cartesian product enumerator: yields each combination as a List `current`
-- to the callback `body`.
cartesianForEachPM = (lists, body) -> (
     n := #lists;
     buf := new MutableList from toList(n : null);
     descend := null;
     descend = i -> (
          if i == n then body(toList buf)
          else for v in lists#i do (buf#i = v; descend(i + 1));
          );
     descend(0);
     )

-- Drop trailing zeros from a partition.
trimPartPM = mu -> (
     L := #mu;
     while L > 0 and mu#(L-1) == 0 do L = L - 1;
     for i from 0 to L - 1 list mu#i
     )

-- Conjugate of a partition (drops trailing zeros first).
conjPartPM = mu -> toList conjugate (new Partition from trimPartPM mu)

-------------------
-- Main routines --
-------------------

-- Returns dual partition
dualPart = method()
dualPart(List) := mu -> (
     for i from 1 to mu#0 list (
	  counter := 0;
	  for j from 0 to #mu - 1 do if mu#j >= i then counter = counter + 1;
	  counter
	  )
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
-- Memoize standardTableaux: it's called many times per pieri / lrMap with
-- the same (dim, mu) (e.g., for source and target shapes, repeated across
-- pieri compositions in lrMap, and between successive iterations of
-- benchmarks/loops).  The cache is purely a function of (dim, mu).
standardTableauxCache = new MutableHashTable

standardTableaux = method()
standardTableaux(ZZ, List) := (dim, mu) -> standardTableauxCache#(dim, mu) ??= (
     if #mu == 0 then {{}}
     else (
	  otherrows := standardTableaux(dim, drop(mu, 1));
	  firstrow := rsort compositions(dim, mu#0);
	  flatten for i in firstrow list (
	       temp := flatten for j from 0 to #i - 1 list toList(i#j : j);
	       for j in otherrows list (
		    temp2 := prepend(temp, j);
		    if isStandardPM(temp2) === null then temp2 else continue
		    )
	       )
	  )
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
     -- Short-circuit: if t is already standard, the straightening is just t.
     -- Avoids an unnecessary towardStandard + recursive straighten call that
     -- would otherwise recompute the same answer.
     if isStandardPM(t) === null then (
	  h#t = new HashTable from {t => 1};
	  return null;
	  );
     firstIter := towardStandard(t);
     H := hashTable({}); -- straightening of t
     for i in keys firstIter do (
	  coeff := firstIter#i;
     	  straighten(i, h);
	  temp := for j in keys h#i list (j, coeff * (h#i)#j);
	  H = merge(H, hashTable temp, plus);
	  );
     -- Drop zeros once at the end rather than after every iteration.
     h#t = hashTable for j in keys H list if H#j != 0 then (j, H#j) else continue;
     return null;
     )

-------------------------------------------------------------------------------
-- Basis conversion: PM (row-form, in Sym tensor) <--> Filling (column-form,
-- in exterior tensor) <--> WeylFilling (divided-power row-form).
--
-- pmToFilling : PM-tableau -> HashTable {standard Filling => QQ}
--             via symmetrize-then-antisymmetrize.
-- fillingToPM : Filling -> HashTable {standard PM-tableau => QQ}
--             via antisymmetrize-then-symmetrize.
-- pmToWeyl    : PM-tableau -> WeylFilling     (data identity)
-- weylToPM    : WeylFilling -> PM-tableau    (data identity)
--
-- These are equivariant isos S_lambda V_PM ~= S_lambda V_Filling (resp. WeylF)
-- and are inverse up to a uniform shape-dependent scalar c_lambda (the Young-
-- symmetrizer normalization).
-------------------------------------------------------------------------------

-- PM tableau -> WeylFilling (just wrap; data is identical).  Requires the
-- updated SchurFunctors that exports `weyl` and `WeylFilling`.
pmToWeyl = T -> (
     ensureSchurFn();
     if sfWeylFn === null then error "pmToWeyl: this requires the updated SchurFunctors with WeylFilling support; the system version does not export `weyl`/`WeylFilling`";
     sfWeylFn(toList apply(T, r -> sort toList r))
     )

-- WeylFilling -> PM tableau (just unwrap).
weylToPM = W -> apply(toList W, r -> sort toList r)

-- PM tableau -> raw HashTable {Filling => QQ} (not yet straightened).
pmToFillingExpansion = T -> (
     ensureSchurFn();
     T = apply(T, r -> sort toList r);
     lambda := apply(T, r -> #r);
     r := #lambda;
     if r == 0 then return new HashTable from {};
     s := lambda#0;
     for i from 1 to r - 1 do if lambda#i > lambda#(i-1) then error "pmToFilling: shape not a partition";
     lambdaT := for j from 0 to s - 1 list (
          c := 0;
          for i from 0 to r - 1 do if lambda#i > j then c = c + 1;
          c
          );
     rowPerms := apply(T, r -> permutations r);
     denom := product apply(lambda, k -> factorialPM k);
     accum := new MutableHashTable;
     cartesianForEachPM(rowPerms, sigmas -> (
          cols := for j from 0 to s - 1 list
               for i from 0 to lambdaT#j - 1 list (sigmas#i)#j;
          totalSign := 1;
          degenerate := false;
          sortedCols := for c in cols list (
               sgn := sortWithSignPM c;
               if sgn === null then ( degenerate = true; break {} );
               totalSign = totalSign * (sgn#0);
               sgn#1
               );
          if degenerate then return;
          F := new sfFillingType from sortedCols;
          contribution := (totalSign * 1_QQ) / denom;
          accum#F = (accum#F ?? 0) + contribution;
          ));
     new HashTable from for k in keys accum list if accum#k != 0 then k => accum#k else continue
     )

-- PM -> standard Filling decomposition (run SchurFunctors' straighten).
-- We iterate over the raw expansion's Filling keys manually rather than
-- passing the whole HashTable to SchurFunctors' straighten -- in some load
-- orders the (HashTable) signature of straighten is not registered.
pmToFilling = T -> (
     ensureSchurFn();
     raw := pmToFillingExpansion T;
     out := new MutableHashTable;
     for F in keys raw do (
	  c := raw#F;
	  decomp := sfStraightenPM F;
	  for k in keys decomp do (
	       v := c * decomp#k;
	       out#k = (out#k ?? 0) + v;
	       );
	  );
     new HashTable from for k in keys out list if out#k != 0 then k => out#k else continue
     )

-- Filling -> raw HashTable {PM-tableau => QQ}.
fillingToPMExpansion = F -> (
     F = toList F;
     s := #F;
     if s == 0 then return new HashTable from {};
     lambdaT := apply(F, length);
     r := lambdaT#0;
     for j from 1 to s - 1 do if lambdaT#j > lambdaT#(j-1) then error "fillingToPM: shape not a partition";
     lambda := for i from 0 to r - 1 list (
          c := 0;
          for j from 0 to s - 1 do if lambdaT#j > i then c = c + 1;
          c
          );
     colChoices := for j from 0 to s - 1 list (
          perms := permutations(F#j);
          for p in perms list (
               sgn := sortWithSignPM p;
               (sgn#0, p)
               )
          );
     denom := product apply(lambdaT, k -> factorialPM k);
     accum := new MutableHashTable;
     cartesianForEachPM(colChoices, choice -> (
          totalSign := 1;
          orderedCols := for c in choice list (
               totalSign = totalSign * (c#0);
               c#1
               );
          rows := for i from 0 to r - 1 list
               for j from 0 to lambda#i - 1 list (orderedCols#j)#i;
          sortedRows := apply(rows, sort);
          contribution := (totalSign * 1_QQ) / denom;
          accum#sortedRows = (accum#sortedRows ?? 0) + contribution;
          ));
     new HashTable from for k in keys accum list if accum#k != 0 then k => accum#k else continue
     )

-- Filling -> standard PM decomposition (run PieriMaps' straighten).
fillingToPM = F -> (
     raw := fillingToPMExpansion F;
     out := new MutableHashTable;
     memo := new MutableHashTable;
     for K in keys raw do (
          c := raw#K;
          (lookup(straighten, List, MutableHashTable))(K, memo);
          decomp := memo#(apply(K, sort));
          for std in keys decomp do (
               cc := c * decomp#std;
               out#std = (out#std ?? 0) + cc;
               );
          );
     new HashTable from for k in keys out list if out#k != 0 then k => out#k else continue
     )

weylToFilling = W -> pmToFilling weylToPM W

fillingToWeyl = F -> (
     ensureSchurFn();
     raw := fillingToPM F;
     new HashTable from for k in keys raw list sfWeylFn k => raw#k
     )

-- QQ change-of-basis matrices on a Schur shape mu over QQ^n.
--    fillingToPMMatrix(mu, n) : rows = pmStandardTab(n, mu), cols =
--      sfStandardTab(n, conjugate mu).  Entry (i, j) is the coefficient of
--      pmTab_i in fillingToPM(filling_j).  This matrix is invertible (up to a
--      shape-dependent scalar) since pmToFilling/fillingToPM are equivariant
--      isos between PM and Filling realizations of S_mu V.
-- Cache for change-of-basis matrices keyed by (mu_trimmed, n).  These
-- matrices are pure functions of (mu, n) and are reused across many pieri /
-- lrMap calls under Convention => "Filling".
fillingToPMMatrixCache = new MutableHashTable
pmToFillingMatrixCache = new MutableHashTable

fillingToPMMatrix = (mu, n) -> fillingToPMMatrixCache#(mm := trimPartPM mu, n) ??= (
     pmS := standardTableaux(n, mm);
     muTcols := conjPartPM mm;
     ensureSchurFn();
     sfS := sfStandardTabPM(n, muTcols);
     -- Hoisted: fillingToPM(sfS#sj) depends only on sj, not ti -- compute once
     -- per column of the result.  This converts an O(#pmS * #sfS) call count
     -- (in the original loop) to O(#sfS).
     decomps := for sj from 0 to #sfS - 1 list fillingToPM sfS#sj;
     result := matrix for ti from 0 to #pmS - 1 list (
          for sj from 0 to #sfS - 1 list (
               decomp := decomps#sj;
               U := pmS#ti;
               decomp#U ?? 0_QQ
               )
          );
     -- Source labels = standard Fillings; target labels = standard PM tableaux.
     attachBasisLabels(result, sfS, pmS);
     result
     )

pmToFillingMatrix = (mu, n) -> pmToFillingMatrixCache#(mm := trimPartPM mu, n) ??= (
     pmS := standardTableaux(n, mm);
     muTcols := conjPartPM mm;
     ensureSchurFn();
     sfS := sfStandardTabPM(n, muTcols);
     decomps := for pj from 0 to #pmS - 1 list pmToFilling pmS#pj;
     result := matrix for fi from 0 to #sfS - 1 list (
          for pj from 0 to #pmS - 1 list (
               decomp := decomps#pj;
               F := sfS#fi;
               decomp#F ?? 0_QQ
               )
          );
     -- Source labels = standard PM tableaux; target labels = standard Fillings.
     attachBasisLabels(result, pmS, sfS);
     result
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
     Sbasis := standardTableaux(d, mu);
     Tbasis := standardTableaux(d, subtractOne(mu, k));
     mu = prepend(0,mu);
     A := {};
     for p from 0 to k when true do A = join(A, apply(subsets(1..(k-1), p), s -> prepend(0, append(s, k))));
     -- Hoisted memo: shared across all source tableaux.  The straightening of
     -- a given intermediate tableau U does not depend on which source we are
     -- processing, so caching results once and reusing them across the s-loop
     -- avoids re-running expensive Garnir straightening on the same U.
     -- (~1.3x-1.7x speedup on medium/large pieri calls.)
     memo := new MutableHashTable;
     -- Hoisted Olver coefficients: cJ depends only on the path J, not on the
     -- source tableau s.  Compute (-1)^#J / cJ once per path so the s-loop
     -- doesn't redo the same arithmetic.
     hCoefs := for J in A list (
	  cJ := 1;
	  for q from 1 to #J-2 when true do cJ = cJ * (mu_(J_q) - mu_k + k - J_q);
	  (-1)^#J / cJ
	  );
     output := for s in apply(Sbasis, i->prepend({},i)) list (
     	  H := new HashTable from {};
     	  for jIdx from 0 to #A - 1 do (
	       J := A#jIdx;
     	       h := hashTable({(s, hCoefs#jIdx)});
	       for i from 0 to #J - 2 do (
	       	    temp := flatten for T in keys h list
		    	 for b from 0 to #(T_(J_(i+1))) - 1 list (
		    	      U := new MutableList from T;
     	       	    	      U#(J_(i+1)) = drop(U#(J_(i+1)), {b, b});
			      U#(J_i) = append(U#(J_i), (T_(J_(i+1)))_b);
			      (new List from U, h#T)
			      );
	       	    h = hashTable(plus, temp);
	       	    );
	       H = merge(H, h, plus);
	       );
     	  H = new MutableHashTable from H;
     	  for T in keys H do (
	       U := apply(drop(T,1), i->sort(i));
	       coeff := H#T;
	       remove(H, T);
	       straighten(U, memo);
	       for i in keys memo#U do
	       H#i = (H#i ?? 0) + coeff * (memo#U)#i * X_((T_0)_0);
	       );
     	  for t in Tbasis list (H#t ?? 0)
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
-- Stash source / target basis labels on a matrix's cache so that
-- `symbolicForm` can pretty-print the map in those basis labels.  Cache
-- keys are strings (rather than symbols) so that the labels are visible
-- across dictionaries (e.g. from user code or TEST blocks).
attachBasisLabels = (M, srcLabels, tgtLabels) -> (
     M.cache#"sourceBasis" = srcLabels;
     M.cache#"targetBasis" = tgtLabels;
     M
     );

pieri = method(Options => {Convention => "Row"})
pieri(List, List, PolynomialRing) := opts -> (mu, boxes, P) -> (
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
     M := if char P == 0 then pierizero(mu, boxes, P) else pierip(mu, boxes, P);
     conv := opts.Convention;
     n := numgens P;
     -- Trim trailing zeros from lambda for label and basis-change shape.
     while #lambda > 0 and lambda#(#lambda - 1) == 0 do lambda = drop(lambda, -1);
     if conv =!= "Row" and conv =!= "Weyl" and conv =!= "Filling" then
	  error ("pieri: Convention must be \"Row\", \"Filling\", or \"Weyl\"");
     if conv === "Filling" then (
	  A := fillingToPMMatrix(mu, n);
	  B := fillingToPMMatrix(lambda, n);
	  M = promote(B^-1, P) * M * promote(A, P);
	  );
     -- Attach symbolicForm labels: source basis = std tabs of mu, target = std tabs of lambda.
     attachBasisLabels(M, pmStdTabsByConv(n, mu, conv),
	  pmStdTabsByConv(n, lambda, conv))
     )

-------------------------------------------------------------------------------
-- pieriColumn: native column-form Pieri (vertical strip removal).
--
-- pieriColumn(mu, cols, P) where P is a SkewCommutative QQ-algebra with
-- numgens P = dim V, returns the matrix of the inclusion
--      S_mu V  -->  ^^k V (x) S_lambda V
-- where lambda = mu - {bottom box of cols#0, then bottom of cols#1 in the
-- intermediate shape, ...}.  The columns specified must form a vertical strip
-- (no two boxes in the same row).  Source basis = standard Fillings of mu;
-- target basis = standard Fillings of lambda; strip factor in the wedge ring.
--
-- The formula is the dual of the Olver row formula with
--    c'_J = prod_{q intermediate} (mu'_k - mu'_(J_q) + J_q - k)
-- (the negative of the literal transpose of the row coefficient, accounting
-- for the wedge anticommutativity).
-------------------------------------------------------------------------------

pieriColumnHelper = (mu, k, P) -> (
     ensureSchurFn();
     n := numgens P;
     X := gens P;
     for i from 0 to #mu - 2 do if mu#i < mu#(i+1) then error("not a partition: ", mu);
     muTrim := trimPartPM mu;
     muT := conjPartPM muTrim;
     if k < 1 or k > #muT then error("column index ", k, " out of range");
     rowToShorten := muT#(k-1);
     lambda := trimPartPM (for i from 0 to #muTrim - 1 list (if i == rowToShorten - 1 then muTrim#i - 1 else muTrim#i));
     muTaug := prepend(0, muT);
     muTcols := muT;
     lambdaTcols := conjPartPM lambda;
     Sbasis := sfStandardTabPM(n, muTcols);
     Tbasis := sfStandardTabPM(n, lambdaTcols);
     Tidx := new MutableHashTable;
     for i from 0 to #Tbasis - 1 do Tidx#(toList Tbasis#i) = i;
     A := {};
     for p from 0 to k do A = join(A, apply(subsets(toList(1..k-1), p), s -> prepend(0, append(s, k))));
     colsList := for F in Sbasis list (
	  s := prepend({}, toList F);
	  rowVec := new MutableList from toList(#Tbasis : 0_P);
	  H := new MutableHashTable;
	  for J in A do (
	       cJ := 1;
	       for q from 1 to #J - 2 do cJ = cJ * (muTaug#k - muTaug#(J#q) + J#q - k);
	       h := new MutableHashTable;
	       h#s = ((-1)^(#J - 1)) / cJ;
	       for i from 0 to #J - 2 do (
		    temp := new MutableHashTable;
		    for T in keys h do (
			 coeff := h#T;
			 srcCol := T#(J#(i+1));
			 for b from 0 to #srcCol - 1 do (
			      dropped := srcCol#b;
			      U := new MutableList from T;
			      U#(J#(i+1)) = drop(srcCol, {b, b});
			      U#(J#i) = prepend(dropped, U#(J#i));
			      sgn := if b % 2 == 0 then 1 else -1;
			      key := new List from U;
			      cur := temp#key ?? 0;
			      temp#key = cur + sgn * coeff;
			      );
			 );
		    h = temp;
		    );
	       for K in keys h do (
		    cur := H#K ?? 0;
		    H#K = cur + h#K;
		    );
	       );
	  memo := new MutableHashTable;
	  for T in keys H do (
	       coeff := H#T;
	       if coeff == 0 then continue;
	       if #(T#0) != 1 then error ("internal: prepended col should have 1 entry");
	       poppedEntry := (T#0)#0;
	       U := drop(T, 1);
	       while #U > 0 and #(U#(#U - 1)) == 0 do U = drop(U, -1);
	       UF := new sfFillingType from U;
	       UFkey := toList U;
	       decomp := memo#UFkey ??= sfStraightenPM UF;
	       for std in keys decomp do (
		    idx := Tidx#(toList std) ?? null;
		    if idx === null then continue;
		    rowVec#idx = rowVec#idx + coeff * (decomp#std) * X#poppedEntry;
		    );
	       );
	  toList rowVec
	  );
     map(P^(#Tbasis), P^{#Sbasis : -1}, transpose colsList)
     )

pieriColumn = method(Options => {Convention => "Filling"})
pieriColumn(List, List, PolynomialRing) := opts -> (mu, cols, P) -> (
     ensureSchurFn();
     curMu := trimPartPM mu;
     for kk in cols do (
	  if kk < 1 then error "column index must be positive";
	  muT := conjPartPM curMu;
	  if kk > #muT then error("intermediate shape ", curMu, " has no column ", kk);
	  rowR := muT#(kk-1);
	  newMu := trimPartPM (for i from 0 to #curMu - 1 list (if i == rowR - 1 then curMu#i - 1 else curMu#i));
	  for i from 0 to #newMu - 2 do
	       if newMu#i < newMu#(i+1) then error "second argument does not specify a vertical strip";
	  curMu = newMu;
	  );
     lambda := curMu;
     curMu = trimPartPM mu;
     result := pieriColumnHelper(curMu, cols#0, P);
     muT := conjPartPM curMu;
     curMu = trimPartPM (for i from 0 to #curMu - 1 list (if i == muT#(cols#0 - 1) - 1 then curMu#i - 1 else curMu#i));
     for b from 1 to #cols - 1 do (
	  stp := pieriColumnHelper(curMu, cols#b, P);
	  result = stp * result;
	  muT2 := conjPartPM curMu;
	  curMu = trimPartPM (for i from 0 to #curMu - 1 list (if i == muT2#(cols#b - 1) - 1 then curMu#i - 1 else curMu#i));
	  );
     conv := opts.Convention;
     n := numgens P;
     muTrim := trimPartPM mu;
     if conv =!= "Filling" and conv =!= "Row" and conv =!= "Weyl" then
	  error("pieriColumn: invalid Convention ", conv);
     if conv =!= "Filling" then (
	  -- Convert from native Filling basis to PM basis.
	  A := pmToFillingMatrix(muTrim, n);
	  B := fillingToPMMatrix(lambda, n);
	  result = promote(B, P) * result * promote(A, P);
	  );
     attachBasisLabels(result, pmStdTabsByConv(n, muTrim, conv),
	  pmStdTabsByConv(n, lambda, conv))
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
     denom := 1;
     result := for i from 0 to (rank source f) - 1 list (
	  row := flatten entries f_{i};
	  flatten for j from 0 to #row - 1 list
	       for k in mon list (
		    coeff := coefficient(k, row#j);
		    temp := ceiling(1 / gcd(coeff, 1));
		    denom = denom * temp / gcd(denom, temp);
		    coeff
		    )
	  );
     result2 := apply(result, i -> apply(i, j -> round(j * denom)));
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
pureFree = method(Options => {Convention => "Row"})
pureFree(List, PolynomialRing) := opts -> (d, P) -> (
     if not isIncreasing(d) then error "The first argument needs to be a strictly increasing list of degrees.";
     e := for i from 1 to #d - 1 list (d#i - d#(i-1));
     counter := -#e + 1 + sum e;
     -- lambda#0 = counter - e#0; lambda#i = lambda#(i-1) - e#i + 1 for i >= 1.
     -- Build the running tail; the head is later replaced with counter.
     cur := counter - e#0;
     tail := for i from 1 to #e - 1 list (cur = cur - e#i + 1; cur);
     lambda := prepend(counter, tail);
     pieri(lambda, toList(e#0 : 1), P, Convention => opts.Convention) ** P^{-d#0}
     )

-------------------------------------------------------------------------------
-- Littlewood--Richardson inclusions  Psi_Q : S_lambda V  ->  S_nu V (x) S_mu V
--
-- An LR skew tableau Q of shape lambda/mu with content nu encodes, for each
-- label a in 1..r (r = #nu), a horizontal strip; iterating row-Pieri maps
-- in the order a = r, r-1, ..., 1 produces a map
--    S_lambda V  ->  Sym^{nu_r} V (x) ... (x) Sym^{nu_1} V (x) S_mu V.
-- A final straightening (Schur projection) on the Sym tensor side lands in
-- S_nu V (x) S_mu V.  Different LR tableaux give linearly independent
-- inclusions; their span has dimension c^lambda_{mu,nu}.
-------------------------------------------------------------------------------

-- Pad mu with trailing zeros to length L.
padPart = (mu, L) -> (
     if #mu >= L then toList mu else toList mu | toList(L - #mu : 0)
     )

-- Subtract one from row k (1-indexed).
subOneRowLR = (mu, k) -> (
     for i from 0 to #mu - 1 list (if i == k - 1 then mu#i - 1 else mu#i)
     )

-- Strip rows (1-indexed) for label a (0-indexed) inside an LR filling Q.
stripRowsForLabel = (Q, a) ->
     flatten for i from 0 to #Q - 1 list
	  for lab in Q#i list if lab == a then i + 1 else continue

-- LR tableau enumerator.  Returns all fillings of lambda/mu with content nu
-- that satisfy: rows weakly increasing, columns strictly increasing among
-- skew cells, reverse-reading-word lattice condition.  Each filling is a
-- list of rows, row i a list of length lambda_i - mu_i with entries in
-- 0..#nu - 1 (0-indexed labels).
lrTableaux = method()
lrTableaux(List, List, List) := (lambda, mu, nu) -> (
     if sum lambda != sum mu + sum nu then return {};
     L := #lambda;
     lam := toList lambda;
     mm  := padPart(mu, L);
     for i from 0 to L - 1 do if lam#i < mm#i then return {};
     r := #nu;
     rowLens := for i from 0 to L - 1 list lam#i - mm#i;
     -- Cells in reverse-reading-word order.
     cells := flatten for i from 0 to L - 1 list (
	  rs := mm#i;
	  len := rowLens#i;
	  for t from 0 to len - 1 list (i, rs + len - 1 - t)
	  );
     cellIndex := new MutableHashTable;
     for t from 0 to #cells - 1 do cellIndex#(cells#t) = t;
     isSkewRC := (row, col) -> (
	  if row < 0 or row >= L then false
	  else (rs := mm#row; (rs <= col) and (col < rs + rowLens#row))
	  );
     assign := new MutableList from toList(#cells : -1);
     labelCount := new MutableList from toList(r : 0);
     prefixCount := new MutableList from toList(r : 0);
     results := new MutableList;
     backtrack := null;
     backtrack = idx -> (
	  if idx == #cells then (
	       filling := for i from 0 to L - 1 list (
		    rs := mm#i;
		    for k from 0 to rowLens#i - 1 list assign#(cellIndex#(i, rs + k))
		    );
	       results#(#results) = filling;
	       return null;
	       );
	  (row, col) := cells#idx;
	  rightLab := if isSkewRC(row, col + 1) then assign#(cellIndex#(row, col + 1)) else r;
	  aboveLab := -1;
	  pr := row - 1;
	  while pr >= 0 do (
	       if isSkewRC(pr, col) then ( aboveLab = assign#(cellIndex#(pr, col)); pr = -1 )
	       else pr = pr - 1;
	       );
	  for lab from 0 to r - 1 do (
	       if lab > rightLab then break;
	       if aboveLab >= 0 and lab <= aboveLab then continue;
	       if labelCount#lab >= nu#lab then continue;
	       if lab >= 1 and prefixCount#lab + 1 > prefixCount#(lab - 1) then continue;
	       assign#(cellIndex#(row, col)) = lab;
	       labelCount#lab = labelCount#lab + 1;
	       prefixCount#lab = prefixCount#lab + 1;
	       backtrack(idx + 1);
	       assign#(cellIndex#(row, col)) = -1;
	       labelCount#lab = labelCount#lab - 1;
	       prefixCount#lab = prefixCount#lab - 1;
	       );
	  return null;
	  );
     backtrack(0);
     toList results
     )

-- The LR inclusion Psi_Q : S_lambda V  ->  S_nu V (x) S_mu V where dim V = n.
-- Returns a Matrix over QQ whose
--   columns are indexed by standardTableaux(n, lambda),
--   rows    are indexed by (SSYT of nu) (x) (SSYT of mu padded to #lambda),
-- listed in lex order of (T_nu_index, T_mu_index).
lrMap = method(Options => {Convention => "Row"})
lrMap(Sequence, List, ZZ) := opts -> (shapes, Q, n) -> (
     if #shapes != 3 then error "lrMap: first argument must be the sequence (lambda, mu, nu)";
     lambda := shapes#0;
     mu := shapes#1;
     nu := shapes#2;
     L := #lambda;
     lam := padPart(lambda, L);
     r := #nu;
     -- Auxiliary multi-block ring with r blocks of n variables.  Block a
     -- (0-indexed) carries the Sym^{nu_a} factor.
     z := getSymbol "lrz";
     Pz := QQ[z_(0,0)..z_(r-1, n-1)];
     -- Build composite Pieri matrix M_1 * M_2 * ... * M_r (r applied first).
     curShape := lam;
     composite := null;
     for tt from 0 to r - 1 do (
	  aIdx := r - 1 - tt;
	  rows := stripRowsForLabel(Q, aIdx);
	  if #rows != nu#aIdx then error "lrMap: strip size mismatch (Q does not match nu)";
	  zloc := getSymbol "lrzloc";
	  Ploc := QQ[zloc_0..zloc_(n-1)];
	  Mloc := pieri(curShape, rows, Ploc);
	  phi := map(Pz, Ploc, for i from 0 to n - 1 list Pz_(aIdx * n + i));
	  Mlift := phi Mloc;
	  if composite === null then composite = Mlift else composite = Mlift * composite;
	  for rr in rows do curShape = subOneRowLR(curShape, rr);
	  );
     Sbasis := standardTableaux(n, lam);
     Nmu := standardTableaux(n, curShape);
     Nnu := standardTableaux(n, nu);
     -- Column index for each source tableau; row index for each (T_nu, T_mu) pair.
     muIdx := new MutableHashTable;
     for i from 0 to #Nmu - 1 do muIdx#(Nmu#i) = i;
     nuIdx := new MutableHashTable;
     for i from 0 to #Nnu - 1 do nuIdx#(Nnu#i) = i;
     numRows := #Nnu * #Nmu;
     numCols := #Sbasis;
     -- Fill matrix as nested list of QQ rows.
     entries := new MutableList from for ri from 0 to numRows - 1 list new MutableList from toList(numCols : 0_QQ);
     straightenH := new MutableHashTable;
     for j from 0 to #Sbasis - 1 do (
	  for i from 0 to #Nmu - 1 do (
	       f := composite_(i, j);
	       if f == 0 then continue;
	       for pair in listForm f do (
		    expv := pair#0;
		    c := pair#1;
		    if c == 0 then continue;
		    tvec := for a from 0 to r - 1 list
			 flatten for k from 0 to n - 1 list
			      toList(expv#(a*n + k) : k);
		    straighten(tvec, straightenH);
		    decomp := straightenH#tvec ?? new HashTable from {};
		    for ssyt in keys decomp do (
			 if not nuIdx#?ssyt then continue;
			 dc := decomp#ssyt;
			 ri := nuIdx#ssyt * #Nmu + i;
			 (entries#ri)#j = (entries#ri)#j + c * dc;
			 );
		    );
	       );
	  );
     M := map(QQ^numRows, QQ^numCols, toList apply(entries, r0 -> toList r0));
     conv := opts.Convention;
     muPad := if #mu < L then mu | toList(L - #mu : 0) else mu;
     if conv =!= "Row" and conv =!= "Weyl" and conv =!= "Filling" then
	  error ("lrMap: Convention must be \"Row\", \"Filling\", or \"Weyl\"");
     if conv === "Filling" then (
	  -- Change basis on source (lambda) and on both target factors (nu, mu).
	  Blam := pmToFillingMatrix(lambda, n);
	  Bnu  := pmToFillingMatrix(nu, n);
	  Bmu  := pmToFillingMatrix(muPad, n);
	  M = (Bnu ** Bmu) * M * (Blam)^-1;
	  );
     -- Build target labels: pairs (T_nu, T_mu) in the same lex order the matrix uses.
     stdsLam := pmStdTabsByConv(n, lambda, conv);
     stdsNu := pmStdTabsByConv(n, nu, conv);
     stdsMu := pmStdTabsByConv(n, muPad, conv);
     tgtLabels := flatten for tNu in stdsNu list for tMu in stdsMu list (tNu, tMu);
     attachBasisLabels(M, stdsLam, tgtLabels)
     )

-- Convenience: accept a polynomial ring and use its number of generators.
lrMap(Sequence, List, PolynomialRing) := opts -> (shapes, Q, P) -> lrMap(shapes, Q, numgens P, Convention => opts.Convention)

-- Pretty-printer for one tableau as a Net (rows stacked, entries 1-indexed
-- for human readability so they match the displayed mu/nu/lambda shapes).
netTableau = T -> (
     if T === {} or all(T, r -> #r == 0) then return net "()";
     stack apply(T, r -> horizontalJoin apply(r, x -> net (x + 1) | " "))
     )

-- Apply Psi_Q to one input tableau T of shape lambda; return its image as a
-- list of triples {coeff, T_nu, T_mu}.  T may be any list of rows of shape
-- lambda (entries 0..n-1).  Non-standard tableaux are first straightened into
-- the SSYT basis of S_lambda V; the GL-equivariant inclusion is then applied.
--
-- The output triples are filtered to nonzero coefficients and sorted by
-- (T_nu, T_mu) for deterministic display.
applyLR = method(Options => {Convention => "Row"})
applyLR(Sequence, List, BasicList, ZZ) := opts -> (shapes, Q, T, n) -> (
     if #shapes != 3 then error "applyLR: first argument must be (lambda, mu, nu)";
     lambda := shapes#0;
     mu := shapes#1;
     nu := shapes#2;
     L := #lambda;
     conv := opts.Convention;
     -- Build the LR matrix in the chosen convention.
     M := lrMap(shapes, Q, n, Convention => conv);
     muPad := if #mu < L then mu | toList(L - #mu : 0) else mu;
     -- Build source basis and look up T via convention-specific straightening.
     local Sb;
     local Nmu;
     local Nnu;
     local Tcoords;
     if conv === "Row" or conv === "Weyl" then (
	  Sb = standardTableaux(n, lambda);
	  Nmu = standardTableaux(n, muPad);
	  Nnu = standardTableaux(n, nu);
	  Tlist := apply(toList T, r -> sort toList r);
	  h := new MutableHashTable;
	  straighten(Tlist, h);
	  decomp := h#Tlist ?? new HashTable from {};
	  idx := new MutableHashTable;
	  for i from 0 to #Sb - 1 do idx#(Sb#i) = i;
	  Tcoords = new MutableList from toList(#Sb : 0_QQ);
	  for K in keys decomp do
	       if idx#?K then Tcoords#(idx#K) = decomp#K;
	  )
     else if conv === "Filling" then (
	  ensureSchurFn();
	  Sb = sfStandardTabPM(n, conjPartPM lambda);
	  Nmu = sfStandardTabPM(n, conjPartPM muPad);
	  Nnu = sfStandardTabPM(n, conjPartPM nu);
	  Tcast := if instance(T, sfFillingType) then T else new sfFillingType from toList T;
	  decompF := sfStraightenPM Tcast;
	  idxF := new MutableHashTable;
	  for i from 0 to #Sb - 1 do idxF#(toList Sb#i) = i;
	  Tcoords = new MutableList from toList(#Sb : 0_QQ);
	  for K in keys decompF do (
	       Klist := toList K;
	       if idxF#?Klist then Tcoords#(idxF#Klist) = decompF#K;
	       );
	  )
     else error("applyLR: invalid Convention ", conv);
     Tcol := transpose matrix {toList Tcoords};
     wcol := M * Tcol;
     out := new MutableList;
     for r from 0 to numRows wcol - 1 do (
	  v := wcol_(r, 0);
	  if v == 0 then continue;
	  i := r // (#Nmu);
	  jj := r % (#Nmu);
	  out#(#out) = {v, Nnu#i, Nmu#jj};
	  );
     toList out
     )

-- Pretty-print the result of applyLR as a sum  c_i * T_nu^(i) ⊗ T_mu^(i).
-- Each term is rendered with the coefficient (as a single-line string) on the
-- top row and the two tableaux side-by-side, separated by "⊗".
displayLRImage = method()
displayLRImage List := terms -> (
     if #terms == 0 then return net "0";
     parts := for trip in terms list (
         c := trip#0;
         Tnu := trip#1;
         Tmu := trip#2;
         coeffStr := if c == 1 then "  +  "
                     else if c == -1 then "  -  "
                     else (
                          s := toString c;
                          if c > 0 then "  +  " | s | " . "
                          else "  -  " | substring(s, 1) | " . "
                          );
         horizontalJoin {coeffStr, "[", netTableau Tnu, "] ⊗ [", netTableau Tmu, "]"}
         );
     -- Drop the leading "+ " from the very first term.
     first0 := parts#0;
     parts0 := if #(toString first0) > 0 then (
          firstStr := toString first0;
          (replace("^  \\+  ", "    ", firstStr));
          first0  -- keep as-is for now; stacking handles it
          ) else first0;
     stack parts
     )

-------------------------------------------------------------------------------
-- verifyWellDefined: check that an LR map respects the straightening
-- relations of the chosen tableau convention.
--
-- Strategy.  For each "test" non-standard tableau T of shape lambda:
--   (a) compute  applyLR(shapes, Q, T, n, Convention => conv)  directly;
--   (b) independently straighten T via the convention's straightening into a
--       sum  T = sum_i c_i T_std_i;
--   (c) compute  sum_i c_i applyLR(shapes, Q, T_std_i, n, Convention => conv);
--   (d) verify (a) and (c) agree.
--
-- Returns true on success.  Prints offending case and returns false on
-- failure.
-------------------------------------------------------------------------------

genNonStdTableauxPM = (lambda, n, convention) -> (
     out := new MutableList;
     if convention === "Filling" then (
	  ensureSchurFn();
	  shape := conjPartPM lambda;
	  stdF := sfStandardTabPM(n, shape);
	  if #stdF == 0 then return {};
	  for F in stdF do (
	       Fl := apply(toList F, toList);
	       for c from 0 to #Fl - 1 do (
		    if #(Fl#c) >= 2 then (
			 swapped := for j from 0 to #(Fl#c) - 1 list (
			      if j == 0 then (Fl#c)#1
			      else if j == 1 then (Fl#c)#0
			      else (Fl#c)#j
			      );
			 newF := for cc from 0 to #Fl - 1 list (if cc == c then swapped else Fl#cc);
			 out#(#out) = new sfFillingType from newF;
			 if #out >= 3 then return toList out;
			 );
		    );
	       );
	  ) else (
	  std := standardTableaux(n, lambda);
	  if #std == 0 then return {};
	  for T in std do (
	       for ri from 0 to #T - 1 do (
		    if #(T#ri) >= 2 and (T#ri)#0 != (T#ri)#1 then (
			 swapped := for j from 0 to #(T#ri) - 1 list (
			      if j == 0 then (T#ri)#1
			      else if j == 1 then (T#ri)#0
			      else (T#ri)#j
			      );
			 newT := for rr from 0 to #T - 1 list (if rr == ri then swapped else T#rr);
			 out#(#out) = newT;
			 if #out >= 3 then return toList out;
			 );
		    );
	       );
	  );
     toList out
     )

straightenByConvPM = (T, convention) -> (
     if convention === "Row" or convention === "Weyl" then (
	  Tlist := apply(toList T, r -> sort toList r);
	  h := new MutableHashTable;
	  straighten(Tlist, h);
	  h#Tlist ?? new HashTable from {}
	  )
     else if convention === "Filling" then (
	  ensureSchurFn();
	  Tcast := if instance(T, sfFillingType) then T else new sfFillingType from toList T;
	  sfStraightenPM Tcast
	  )
     else error "straightenByConv: invalid convention"
     )

-- Generic well-definedness driver for an apply-style function whose output
-- pairs are {value, basisKey}.  Compares applyFn(T) (T non-standard) against
-- sum c_i * applyFn(T_std_i) where T = sum c_i T_std_i in the Schur module.
testWellDefinedTwoTuple0 = (applyFn, srcShape, n, conv, zeroVal, verbose) -> (
     tests := genNonStdTableauxPM(srcShape, n, conv);
     if #tests == 0 then (
	  if verbose then stdio << "  (no non-standard test tableaux for " << toString srcShape << " in convention " << conv << ")" << endl;
	  return true;
	  );
     ok := true;
     for T in tests do (
	  imgD := applyFn T;
	  decomp := straightenByConvPM(T, conv);
	  imgM := new MutableHashTable;
	  for K in keys decomp do (
	       c := decomp#K;
	       img := applyFn K;
	       for pair in img do (
		    key := if class pair#1 === List then pair#1 else toList pair#1;
		    cur := imgM#key ?? zeroVal;
		    imgM#key = cur + c * pair#0;
		    );
	       );
	  imgDmap := new MutableHashTable;
	  for pair in imgD do (
	       key := if class pair#1 === List then pair#1 else toList pair#1;
	       imgDmap#key = pair#0;
	       );
	  allKeys := unique (keys imgDmap | keys imgM);
	  caseOk := true;
	  for k in allKeys do (
	       a := imgDmap#k ?? zeroVal;
	       b := imgM#k ?? zeroVal;
	       if a != b then caseOk = false;
	       );
	  if not caseOk then (
	       ok = false;
	       if verbose then stdio << "  FAIL on T = " << toString T << endl;
	       );
	  );
     ok
     );

verifyWellDefined = method(Options => {Convention => "Row", Verbose => true, Direction => "Inclusion"})

-- pieri / pieriColumn / dualPieri / dualPieriColumn well-definedness.
-- The PolynomialRing P determines which family: SkewCommutative => "column" form
-- (pieriColumn / dualPieriColumn), else symmetric (pieri / dualPieri).  The
-- Direction option chooses inclusion vs projection (default "Inclusion").
verifyWellDefined(List, List, PolynomialRing) := opts -> (mu, boxes, P) -> (
     n := numgens P;
     conv := opts.Convention;
     skewP := isSkewCommutative P;
     dual := opts.Direction === "Dual";
     verbose := opts.Verbose;
     if dual then (
	  -- Dual: source = wedge/Sym^d V (x) S_lambda; vary the S_lambda factor.
	  lambda := mu;
	  for b in boxes do lambda = subtractOne(lambda, b);
	  while #lambda > 0 and lambda#-1 == 0 do lambda = drop(lambda, -1);
	  d := #boxes;
	  monBasis := flatten entries basis(d, P);
	  if #monBasis == 0 then return true;
	  poly := monBasis#0;
	  applyFn := if skewP then
	       (T -> applyDualPieriColumn((mu, boxes), poly, T, n, Convention => conv))
	  else (T -> applyDualPieri((mu, boxes), poly, T, n, Convention => conv));
	  testWellDefinedTwoTuple0(applyFn, lambda, n, conv, 0_QQ, verbose)
	  )
     else (
	  -- Inclusion: source = S_mu; vary T_mu.
	  applyFnInc := if skewP then
	       (Targ -> applyPieriColumn((mu, boxes), Targ, P, Convention => conv))
	  else (Targ -> applyPieri((mu, boxes), Targ, P, Convention => conv));
	  testWellDefinedTwoTuple0(applyFnInc, mu, n, conv, 0_P, verbose)
	  )
     );

verifyWellDefined(Sequence, List, ZZ) := opts -> (shapes, Q, n) -> (
     if opts.Direction === "Dual" then (
	  -- Dual LR: source = S_nu (x) S_mu; vary T_mu (we just pick one factor).
	  (lamD, muD, nuD) := shapes;
	  convD := opts.Convention;
	  -- Pick a fixed standard T_nu for the test.
	  stdsNuD := pmStdTabsByConv(n, nuD, convD);
	  if #stdsNuD == 0 then return true;
	  TnuD := stdsNuD#0;
	  muPadD := muD | toList(#lamD - #muD : 0);
	  applyFnD := (TmuArg -> applyDualLR(shapes, Q, {TnuD, TmuArg}, n, Convention => convD));
	  return testWellDefinedTwoTuple0(applyFnD, muPadD, n, convD, 0_QQ, opts.Verbose);
	  );
     conv := opts.Convention;
     verbose := opts.Verbose;
     lambda := shapes#0;
     tests := genNonStdTableauxPM(lambda, n, conv);
     if #tests == 0 then (
	  if verbose then stdio << "verifyWellDefined: no non-standard test tableaux for shape " << toString lambda << " in convention " << conv << "; nothing to verify." << endl;
	  return true;
	  );
     ok := true;
     numTests := 0;
     for T in tests do (
	  numTests = numTests + 1;
	  imgD := applyLR(shapes, Q, T, n, Convention => conv);
	  decomp := straightenByConvPM(T, conv);
	  imgM := new MutableHashTable;
	  for K in keys decomp do (
	       c := decomp#K;
	       img := applyLR(shapes, Q, K, n, Convention => conv);
	       for trip in img do (
		    key := (toList trip#1, toList trip#2);
		    cur := imgM#key ?? 0;
		    imgM#key = cur + c * trip#0;
		    );
	       );
	  imgDmap := new MutableHashTable;
	  for trip in imgD do imgDmap#(toList trip#1, toList trip#2) = trip#0;
	  allKeys := unique (keys imgDmap | keys imgM);
	  caseOk := true;
	  for k in allKeys do (
	       a := imgDmap#k ?? 0;
	       b := imgM#k ?? 0;
	       if a != b then (
		    caseOk = false;
		    if verbose then
			 stdio << "  MISMATCH at " << toString k
			       << ": direct=" << a << " manual=" << b << endl;
		    );
	       );
	  if not caseOk then (
	       ok = false;
	       if verbose then stdio << "  Test #" << numTests << " (T = " << toString T << "): FAIL" << endl;
	       )
	  else if verbose then
	       stdio << "  Test #" << numTests << " (T = " << toString T << "): OK" << endl;
	  );
     if verbose then
	  stdio << "verifyWellDefined: " << numTests << " test(s) " << (if ok then "all PASSED" else "FAILED") << " for convention " << conv << "." << endl;
     ok
     )

-- ====================================================================
--  Dual maps (projection direction).
--
--  By Schur's lemma, a GL_n-equivariant projection
--      Sym^d V (x) S_lambda V --> S_mu V         (when mu = lambda + horizontal d-strip)
--      S_nu V (x) S_mu V     --> S_lambda V       (per LR tableau Q)
--  is unique up to scalar.  We construct each by stacking the relevant
--  inclusions for ALL summands of the source (Pieri's rule for the first,
--  the full LR decomposition for the second), inverting the resulting
--  square matrix, and reading off the rows for our chosen target.
--
--  This guarantees `dualPieri ∘ pieri = Id` exactly.
-- ====================================================================

-- Enumerate (mu', canonicalBoxes) for every mu' = lambda + horizontal
-- d-strip with at most n rows.  canonicalBoxes is the list of row
-- indices (1-indexed) sorted ascending.
pmAddableHStrips = (lambda, d, n) -> (
     lam := lambda;
     while #lam > 0 and lam#-1 == 0 do lam = drop(lam, -1);
     if #lam > n then return {};
     lamPad := lam | toList(n - #lam : 0);
     for comp in compositions(n, d) list (
	  muNew := for i from 0 to n-1 list lamPad_i + comp_i;
	  if not all(0..n-2, i -> muNew_i >= muNew_(i+1)) then continue;
	  -- horizontal strip: each column has at most one new cell, i.e.
	  -- muNew_(i+1) <= lamPad_i for all i.
	  if not all(0..n-2, i -> muNew_(i+1) <= lamPad_i) then continue;
	  muTrim := muNew;
	  while #muTrim > 0 and muTrim#-1 == 0 do muTrim = drop(muTrim, -1);
	  boxes := flatten for i from 0 to n-1 list toList((comp_i):(i+1));
	  (muTrim, boxes)
	  )
     );

-- All partitions of n with at most maxParts parts.
pmPartitionsAtMost = (n, maxParts) ->
     select(toList partitions n, p -> #(toList p) <= maxParts);

-- Enumerate (mu', canonicalCols) for every mu' = lambda + vertical d-strip
-- with at most n rows.  canonicalCols is the list of column indices (1-indexed)
-- where boxes are added, sorted descending so iterated single-column removal
-- yields valid intermediate partitions.
pmAddableVStrips = (lambda, d, n) -> (
     lam := lambda;
     while #lam > 0 and lam#-1 == 0 do lam = drop(lam, -1);
     if #lam > n then return {};
     lamPad := lam | toList(n - #lam : 0);
     for sub in subsets(toList(0..n-1), d) list (
	  muNew := for i from 0 to n-1 list (lamPad_i + (if member(i, sub) then 1 else 0));
	  if not all(0..n-2, i -> muNew_i >= muNew_(i+1)) then continue;
	  muTrim := muNew;
	  while #muTrim > 0 and muTrim#-1 == 0 do muTrim = drop(muTrim, -1);
	  -- For each row r in sub, the new box is at column lamPad_r + 1.
	  -- Sort by column descending (with ties broken by lower row first) so
	  -- iterated pieriColumn produces valid intermediate partitions.
	  pairs := sort apply(sub, r -> (lamPad_r + 1, r));
	  pairs = reverse pairs;
	  cols := apply(pairs, p -> p#0);
	  (muTrim, cols)
	  )
     );

-- Encode a P-matrix M (dim_lambda x dim_mu, entries deg d) as a K-matrix
-- of size (dim_lambda * #monBasis) x dim_mu.  Row index = (T_lambda, alpha)
-- in lex order with T_lambda outer (block size #monBasis), alpha inner.
pmEncodePieriKMat = (M, monBasis, K) -> (
     dimLam := numRows M;
     dimMu  := numColumns M;
     matrix(K, flatten for tLam from 0 to dimLam-1 list (
	       for alpha in monBasis list (
		    for tMu from 0 to dimMu-1 list (
			 lift(coefficient(alpha, M_(tLam, tMu)), K)
			 )
		    )
	       ))
     );

-- ====================================================================
-- Dual-map block factorization cache.
--
-- dualPieri / dualPieriColumn / dualLR all build the same stacked
-- block matrix A and only differ in which rows of A^{-1} they extract.
-- Caching A by (kind, lambda, d, n, K, conv) (resp. (mu, nu, n, K, conv)
-- for LR) lets sibling calls share the heavy block construction, and
-- using `solve` instead of `A^{-1}` extracts only the rows we want
-- (cost O(N^2 * blockSize) instead of O(N^3) for a full inverse).
--
-- A typical workflow projects onto many sister summands of the same
-- ambient tensor product; the cache turns those into nearly-free calls.
-- ====================================================================

pmDualBlockCache = new MutableHashTable;

-- Sign of the permutation that takes `ref` to `a` (assuming both are
-- permutations of the same multiset).  Returns 0 if the multisets do
-- not match.  Equal entries are treated as indistinguishable.
pmPermSign = (a, ref) -> (
     n := #a;
     if n =!= #ref then return 0;
     b := new MutableList from ref;
     sgn := 1;
     for i from 0 to n - 1 do (
	  if b#i === a#i then continue;
	  j := i + 1;
	  while j < n and b#j =!= a#i do j = j + 1;
	  if j >= n then return 0;
	  tmp := b#i; b#i = b#j; b#j = tmp;
	  sgn = -sgn;
	  );
     sgn
     );

-- Build (or look up) the stacked K-matrix and addable-strips list for
-- the horizontal-strip dual map at parameters (lambda, d, n, conv).
-- Returns (addable, A, offsets) with offsets#i = first column of block i.
pmGetDualHBlocks = (lambda, d, n, conv, P, K) ->
     pmDualBlockCache#("row", toList lambda, d, n, K, conv) ??= (
     monBasis := flatten entries basis(d, P);
     addable := pmAddableHStrips(lambda, d, n);
     blocks := for pair in addable list (
	  (muI, canBoxes) := pair;
	  pmEncodePieriKMat(
	       pieri(muI, canBoxes, P, Convention => conv),
	       monBasis, K)
	  );
     A := fold((a, b) -> a | b, blocks);
     offsets := prepend(0, accumulate(plus, 0, apply(blocks, numColumns)));
     (addable, A, offsets)
     );

-- Vertical-strip analogue.  Requires P SkewCommutative.
pmGetDualVBlocks = (lambda, d, n, conv, P, K) ->
     pmDualBlockCache#("col", toList lambda, d, n, K, conv) ??= (
     monBasis := flatten entries basis(d, P);
     addable := pmAddableVStrips(lambda, d, n);
     blocks := for pair in addable list (
	  (muI, canCols) := pair;
	  pmEncodePieriKMat(
	       pieriColumn(muI, canCols, P, Convention => conv),
	       monBasis, K)
	  );
     A := fold((a, b) -> a | b, blocks);
     offsets := prepend(0, accumulate(plus, 0, apply(blocks, numColumns)));
     (addable, A, offsets)
     );

-- Stacked LR-block matrix for dualLR with parameters (mu, nu, n, conv).
-- The key intentionally omits lambda (the target), since the same
-- decomposition supports projection onto every summand at once.
pmGetDualLRBlocks = (mu, nu, n, conv) ->
     pmDualBlockCache#("lr", toList mu, toList nu, n, conv) ??= (
     totalCells := sum mu + sum nu;
     cands := apply(pmPartitionsAtMost(totalCells, n), p -> toList p);
     -- Per-candidate (lambda', Q') pairs along with their lrMap blocks.
     pairs := flatten for lamP in cands list
	  for Qp in lrTableaux(lamP, mu, nu) list (
	       (lamP, Qp, lrMap((lamP, mu, nu), Qp, n, Convention => conv))
	       );
     blockInfo := apply(pairs, p -> (p#0, p#1));
     blocks := apply(pairs, p -> p#2);
     if #blocks == 0 then error "dualLR: no GL_n summands in S_nu V (x) S_mu V";
     A := fold((a, b) -> a | b, blocks);
     if numRows A =!= numColumns A then
	  error "dualLR: stacked LR matrix not square (decomposition incomplete)";
     offsets := prepend(0, accumulate(plus, 0, apply(blocks, numColumns)));
     (blockInfo, A, offsets)
     );

-- Extract rows [startRow .. startRow + blockSize - 1] of A^{-1} via a
-- single `solve` against a block of standard-basis columns, avoiding
-- the cost (and memory) of forming the full inverse.
--
-- Math: if Y is the desired row block, then Y * A = E where E is the
-- (blockSize x N) "rectangular identity" picking out those rows, so
-- transpose(A) * transpose(Y) = transpose(E), giving
--    Y = transpose solve(transpose A, transpose E).
pmDualSolveRows = (A, startRow, blockSize) -> (
     N := numRows A;
     K := ring A;
     E := submatrix(id_(K^N), , toList(startRow .. startRow + blockSize - 1));
     transpose solve(transpose A, E)
     );


dualPieri = method(Options => {Convention => "Row"})
dualPieri(List, List, PolynomialRing) := opts -> (mu, boxes, P) -> (
     n := numgens P;
     d := #boxes;
     K := coefficientRing P;
     -- Normalize boxes to canonical (sorted ascending) order so the
     -- result matches `pieri(mu, sort boxes, P)` exactly on round trip.
     -- (Different orderings of the same box-multiset produce pieri
     -- matrices that differ by a scalar, so we standardize.)
     canBoxes := sort toList boxes;
     lambda := mu;
     for b in canBoxes do lambda = subtractOne(lambda, b);
     while #lambda > 0 and lambda#-1 == 0 do lambda = drop(lambda, -1);
     (addable, A, offsets) := pmGetDualHBlocks(lambda, d, n, opts.Convention, P, K);
     muIdx := position(addable, p -> p#0 == mu);
     if muIdx === null then error("dualPieri: mu = ", mu,
	  " is not a horizontal ", d,
	  "-strip addition to lambda = ", lambda,
	  " in ", n, " variables");
     startRow := offsets#muIdx;
     dimMu := offsets#(muIdx + 1) - startRow;
     N := pmDualSolveRows(A, startRow, dimMu);
     -- Source basis: pairs (T_lambda, alpha) in lex order with T_lambda outer
     -- (matches pmEncodePieriKMat's row layout).  Target basis: T_mu.
     monBasis := flatten entries basis(d, P);
     stdsLam := pmStdTabsByConv(n, lambda, opts.Convention);
     stdsMu := pmStdTabsByConv(n, mu, opts.Convention);
     srcLabels := flatten for tLam in stdsLam list for alpha in monBasis list (tLam, alpha);
     attachBasisLabels(N, srcLabels, stdsMu)
     );

dualLR = method(Options => {Convention => "Row"})
dualLR(Sequence, List, ZZ) := opts -> (shapes, Q, n) -> (
     (lambda, mu, nu) := shapes;
     (blockInfo, A, offsets) := pmGetDualLRBlocks(mu, nu, n, opts.Convention);
     idx := position(blockInfo,
	  pair -> pair#0 == lambda and pair#1 == Q);
     if idx === null then error("dualLR: (lambda, Q) not found in LR decomposition of S_nu V (x) S_mu V");
     startRow := offsets#idx;
     dimLam := offsets#(idx + 1) - startRow;
     N := pmDualSolveRows(A, startRow, dimLam);
     -- Source basis: pairs (T_nu, T_mu) in lex order matching the lrMap row layout.
     muPad := mu | toList(#lambda - #mu : 0);
     stdsLam := pmStdTabsByConv(n, lambda, opts.Convention);
     stdsNu := pmStdTabsByConv(n, nu, opts.Convention);
     stdsMu := pmStdTabsByConv(n, muPad, opts.Convention);
     srcLabels := flatten for tNu in stdsNu list for tMu in stdsMu list (tNu, tMu);
     attachBasisLabels(N, srcLabels, stdsLam)
     );

dualLR(Sequence, List, PolynomialRing) := opts -> (shapes, Q, P) ->
     dualLR(shapes, Q, numgens P, opts);

-- dualPieriColumn: GL-equivariant projection wedge^d V (x) S_lambda V --> S_mu V
-- where mu = lambda + vertical d-strip.  Cached stack-and-solve construction
-- analogous to dualPieri.  The cache uses the canonical column ordering
-- produced by pmAddableVStrips; if the user passes a permuted cols list,
-- a single sign flip suffices since pieriColumn is sgn-equivariant under
-- column permutation (the wedge factor is antisymmetric).
-- Requires P to be a SkewCommutative ring (the wedge factor is encoded there).
dualPieriColumn = method(Options => {Convention => "Filling"})
dualPieriColumn(List, List, PolynomialRing) := opts -> (mu, cols, P) -> (
     n := numgens P;
     d := #cols;
     K := coefficientRing P;
     -- Compute lambda = mu after the cols vertical strip is removed.
     curMu := trimPartPM mu;
     for kk in cols do (
	  muT := conjPartPM curMu;
	  rowR := muT#(kk-1);
	  curMu = trimPartPM (for i from 0 to #curMu - 1 list (if i == rowR - 1 then curMu#i - 1 else curMu#i));
	  );
     lambda := curMu;
     (addable, A, offsets) := pmGetDualVBlocks(lambda, d, n, opts.Convention, P, K);
     muTrim := trimPartPM mu;
     muIdx := position(addable, p -> p#0 == muTrim);
     if muIdx === null then error("dualPieriColumn: mu = ", mu,
	  " is not a vertical ", d, "-strip addition to lambda = ",
	  lambda, " in ", n, " variables");
     -- Sign correction: A was built with the canonical cols ordering.
     canCols := addable#muIdx#1;
     sgn := pmPermSign(toList cols, canCols);
     if sgn == 0 then error("dualPieriColumn: cols ", cols,
	  " are not a permutation of canonical cols ", canCols);
     startRow := offsets#muIdx;
     dimMu := offsets#(muIdx + 1) - startRow;
     N := pmDualSolveRows(A, startRow, dimMu);
     if sgn == -1 then N = -N;
     monBasis := flatten entries basis(d, P);
     stdsLam := pmStdTabsByConv(n, lambda, opts.Convention);
     stdsMu := pmStdTabsByConv(n, muTrim, opts.Convention);
     srcLabels := flatten for tLam in stdsLam list for alpha in monBasis list (tLam, alpha);
     attachBasisLabels(N, srcLabels, stdsMu)
     );

-- applyDualPieriColumn: point evaluation of dualPieriColumn on a basis pair
-- (wedge_polynomial, T_lambda).  Returns a list of {coefficient, T_mu} pairs.
applyDualPieriColumn = method(Options => {Convention => "Filling"})
applyDualPieriColumn(Sequence, RingElement, BasicList, ZZ) := opts ->
     (muCols, poly, Tlam, n) -> (
     if poly == 0 then return {};
     P := ring poly;
     d := #(muCols#1);
     if not isHomogeneous poly or first degree poly =!= d then
	  error("applyDualPieriColumn: poly must be homogeneous of degree d = ", d);
     K := coefficientRing P;
     monBasis := flatten entries basis(d, P);
     (mu, cols) := muCols;
     -- compute lambda
     curMu := trimPartPM mu;
     for kk in cols do (
	  muT := conjPartPM curMu;
	  rowR := muT#(kk-1);
	  curMu = trimPartPM (for i from 0 to #curMu - 1 list (if i == rowR - 1 then curMu#i - 1 else curMu#i));
	  );
     lambda := curMu;
     coordsLam := pmTableauToCoords(Tlam, lambda, n, opts.Convention);
     polyCoords := for alpha in monBasis list lift(coefficient(alpha, poly), K);
     sourceVec := flatten for c in coordsLam list for pc in polyCoords list c * pc;
     N := dualPieriColumn(mu, cols, P, Convention => opts.Convention);
     resultCol := N * transpose matrix(K, {sourceVec});
     stds := pmStdTabsByConv(n, mu, opts.Convention);
     for i from 0 to numRows resultCol - 1 list (
	  c := lift(resultCol_(i, 0), K);
	  if c != 0 then {c, stds#i} else continue
	  )
     );

-- Convert a tableau T to a coordinate vector in the standard basis of
-- S_shape under the active Convention.
--
-- A List is interpreted in the natural form of the convention:
--   * Convention => "Row" or "Weyl": List = PM row form (rows of the tableau).
--   * Convention => "Filling":       List = column form (columns of the tableau,
--                                    top-to-bottom), automatically wrapped as
--                                    a SchurFunctors Filling.
-- A Filling argument is always treated as column form (its native meaning),
-- regardless of the convention -- this lets the user supply a column-form
-- tableau even in Row/Weyl convention.
--
-- Non-standard inputs are straightened first using the convention's
-- straightening relations (PieriMaps' row-Garnir for Row/Weyl, SchurFunctors'
-- column-Garnir for Filling).
pmTableauToCoords = (T, shape, n, conv) -> (
     ensureSchurFn();
     if conv === "Filling" then (
	  stdsF := sfStandardTabPM(n, conjPartPM shape);
	  Tcast := if class T === sfFillingType then T
	       else if class T === List then new sfFillingType from T
	       else error "tableau must be a List (column form) or Filling";
	  h := sfStraightenPM Tcast;
	  for s in stdsF list (h#s ?? 0_QQ)
	  )
     else if conv === "Row" or conv === "Weyl" then (
	  stds := standardTableaux(n, shape);
	  hRow := if class T === List then straighten T
	       else if class T === sfFillingType then fillingToPM T
	       else error "tableau must be a List (PM row form) or Filling (column form)";
	  for s in stds list (hRow#s ?? 0_QQ)
	  )
     else error("unknown Convention: ", conv)
     );

pmStdTabsByConv = (n, shape, conv) -> (
     if conv === "Filling" then (ensureSchurFn(); sfStandardTabPM(n, conjPartPM shape))
     else if conv === "Row" or conv === "Weyl" then standardTableaux(n, shape)
     else error("unknown Convention: ", conv)
     );

-- applyPieri: point evaluation of the inclusion-direction Pieri map.
-- Takes a single source tableau T of shape mu (List PM-row form, or Filling
-- column form) and returns the image as a list of {polynomial, T_lambda} pairs:
-- each polynomial is a homogeneous degree-d form in P encoding the V^d factor.
-- Non-standard inputs are straightened first via the Convention's relations.
applyPieri = method(Options => {Convention => "Row"})
applyPieri(Sequence, BasicList, PolynomialRing) := opts ->
     (muBoxes, T, P) -> (
     (mu, boxes) := muBoxes;
     n := numgens P;
     K := coefficientRing P;
     lambda := mu;
     for b in boxes do lambda = subtractOne(lambda, b);
     while #lambda > 0 and lambda#-1 == 0 do lambda = drop(lambda, -1);
     M := pieri(mu, boxes, P, Convention => opts.Convention);
     coordsMu := pmTableauToCoords(T, mu, n, opts.Convention);
     sourceVecP := transpose matrix(P, {apply(coordsMu, c -> promote(c, P))});
     resultCol := M * sourceVecP;
     stdsLam := pmStdTabsByConv(n, lambda, opts.Convention);
     for i from 0 to numRows resultCol - 1 list (
	  f := resultCol_(i, 0);
	  if f == 0 then continue else {f, stdsLam#i}
	  )
     );

-- applyPieriColumn: same but for the column-form Pieri (target wedge^d V).
-- Requires P to be a SkewCommutative ring (the wedge factor is encoded there).
applyPieriColumn = method(Options => {Convention => "Filling"})
applyPieriColumn(Sequence, BasicList, PolynomialRing) := opts ->
     (muCols, T, P) -> (
     (mu, cols) := muCols;
     n := numgens P;
     K := coefficientRing P;
     -- Compute lambda by removing the vertical strip described by cols.
     curMu := trimPartPM mu;
     for kk in cols do (
	  muT := conjPartPM curMu;
	  rowR := muT#(kk-1);
	  curMu = trimPartPM (for i from 0 to #curMu - 1 list (if i == rowR - 1 then curMu#i - 1 else curMu#i));
	  );
     lambda := curMu;
     M := pieriColumn(mu, cols, P, Convention => opts.Convention);
     coordsMu := pmTableauToCoords(T, mu, n, opts.Convention);
     sourceVecP := transpose matrix(P, {apply(coordsMu, c -> promote(c, P))});
     resultCol := M * sourceVecP;
     stdsLam := pmStdTabsByConv(n, lambda, opts.Convention);
     for i from 0 to numRows resultCol - 1 list (
	  f := resultCol_(i, 0);
	  if f == 0 then continue else {f, stdsLam#i}
	  )
     );

applyDualPieri = method(Options => {Convention => "Row"})
applyDualPieri(Sequence, RingElement, BasicList, ZZ) := opts ->
     (muBoxes, poly, Tlam, n) -> (
     (mu, boxes) := muBoxes;
     if poly == 0 then return {};
     P := ring poly;
     d := #boxes;
     if not isHomogeneous poly or first degree poly =!= d then
	  error("applyDualPieri: poly must be homogeneous of degree d = ", d);
     K := coefficientRing P;
     monBasis := flatten entries basis(d, P);
     lambda := mu;
     for b in boxes do lambda = subtractOne(lambda, b);
     while #lambda > 0 and lambda#-1 == 0 do lambda = drop(lambda, -1);
     coordsLam := pmTableauToCoords(Tlam, lambda, n, opts.Convention);
     polyCoords := for alpha in monBasis list lift(coefficient(alpha, poly), K);
     -- Source basis ordering: (T_lambda outer, alpha inner) -- match dualPieri.
     sourceVec := flatten for c in coordsLam list for pc in polyCoords list c * pc;
     N := dualPieri(mu, boxes, P, Convention => opts.Convention);
     resultCol := N * transpose matrix(K, {sourceVec});
     stds := pmStdTabsByConv(n, mu, opts.Convention);
     for i from 0 to numRows resultCol - 1 list (
	  c := lift(resultCol_(i, 0), K);
	  if c != 0 then {c, stds#i} else continue
	  )
     );

applyDualLR = method(Options => {Convention => "Row"})
-- NOTE: takes the (Tnu, Tmu) pair as a List {Tnu, Tmu}, not a Sequence,
-- because M2 flattens nested Sequences in function-call arg lists, but
-- Lists do not flatten.
applyDualLR(Sequence, List, List, ZZ) := opts ->
     (shapes, Q, TnuTmuList, n) -> (
     if #TnuTmuList =!= 2 then
	  error "applyDualLR: third argument must be a List {Tnu, Tmu}";
     (Tnu, Tmu) := (TnuTmuList#0, TnuTmuList#1);
     (lambda, mu, nu) := shapes;
     coordsNu := pmTableauToCoords(Tnu, nu, n, opts.Convention);
     muPad := mu | toList(#lambda - #mu : 0);
     coordsMu := pmTableauToCoords(Tmu, muPad, n, opts.Convention);
     -- Source basis ordering: (T_nu outer, T_mu inner) -- matches lrMap row indexing.
     sourceVec := flatten for cn in coordsNu list for cm in coordsMu list cn * cm;
     N := dualLR(shapes, Q, n, Convention => opts.Convention);
     resultCol := N * transpose matrix(QQ, {sourceVec});
     stds := pmStdTabsByConv(n, lambda, opts.Convention);
     for i from 0 to numRows resultCol - 1 list (
	  c := lift(resultCol_(i, 0), QQ);
	  if c != 0 then {c, stds#i} else continue
	  )
     );

-- ====================================================================
--  verifyEquivariant: rigorous GL_n-equivariance check.
--
--  Tests whether a matrix M between Schur-functor representations
--  commutes with the action of every Chevalley generator E_{i,j} of
--  gl_n (i != j), which suffices for full GL_n-equivariance (since the
--  E_{i,j} together with the diagonal -- under which our weight-graded
--  matrices commute automatically -- generate gl_n).
--
--  Action conventions:
--   * E_{i,j} acts on V = K^n by  x_l |-> delta_{l,j} * x_i
--     (i.e. as the differential operator x_i * d/dx_j on polynomials).
--   * E_{i,j} acts on S_lambda V (PM row-SSYT basis) by replacing each
--     entry equal to j with i, summing over positions, and applying
--     row-Garnir straightening.
--   * E_{i,j} acts on S_lambda V (Filling column basis) similarly but
--     with SchurFunctors' column-Garnir straightening.
--   * Action on a tensor product is via Leibniz rule.
--
--  Five overloads cover our existing maps:
--    verifyEquivariant(M, mu, boxes, P)               -- pieri / pieriColumn
--    verifyEquivariant(M, shapes, n)                  -- lrMap
--    verifyEquivariant(M, mu, boxes, P, "Dual")       -- dualPieri
--    verifyEquivariant(M, shapes, n, "Dual")          -- dualLR
--  In each, the Convention option matches the matrix's basis.
-- ====================================================================

-- Lie-algebra E_{i,j} action on a PM row-SSYT tableau (List of rows).
-- Returns a HashTable {std PM tab => coeff}.  Uses PieriMaps' straighten.
pmActE0 = (i, j, T) -> (
     totalH := new MutableHashTable;
     for r from 0 to #T-1 do
	  for c from 0 to #(T#r)-1 do
	       if T#r#c == j then (
		    Tprime := for r2 from 0 to #T-1 list
			 (if r2 == r then
			      for c2 from 0 to #(T#r2)-1 list
				   (if c2 == c then i else (T#r2)#c2)
			  else T#r2);
		    h := straighten Tprime;
		    for k in keys h do
			 totalH#k = (totalH#k ?? 0) + h#k;
		    );
     new HashTable from totalH
     );

-- E_{i,j} on a Filling F: replace each j entry by i, sum, then run
-- SchurFunctors' straighten (column-Garnir).  Returns HashTable {std Filling => coeff}.
-- Cache c_lambda values: the round-trip scalar of pmToFilling/fillingToPM.
pmCLambdaCache = new MutableHashTable

-- Compute c_lambda for the round-trip pmToFilling -> fillingToPM = c * Id.
-- We extract it as a diagonal entry of the product matrix.
pmComputeCLambda = (lambda, n) -> pmCLambdaCache#(trimPartPM lambda, n) ??= (
     A := pmToFillingMatrix(lambda, n);
     B := fillingToPMMatrix(lambda, n);
     (B * A)_(0, 0)
     );

-- E_{i,j} action on a Filling F of shape `lambda` over K^n.  We route through
-- the PM round-trip and divide out the c_lambda factor:
--    E.F  =  (1/c_lambda) * pmToFilling( pmActE0( fillingToPM(F) ) ).
-- This avoids depending on SchurFunctors' single-argument `straighten Filling`
-- signature (which can fail to register when SchurFunctors is loaded via
-- needsPackage *after* PieriMaps).
pmActEFilling0 = (i, j, F, lambda, n) -> (
     ensureSchurFn();
     raw := fillingToPM F;
     pmResult := new MutableHashTable;
     for T in keys raw do (
	  c := raw#T;
	  actT := pmActE0(i, j, T);
	  for k in keys actT do
	       pmResult#k = (pmResult#k ?? 0_QQ) + c * actT#k;
	  );
     fillResult := new MutableHashTable;
     for T in keys pmResult do (
	  c := pmResult#T;
	  fillExp := pmToFilling T;
	  for F2 in keys fillExp do
	       fillResult#F2 = (fillResult#F2 ?? 0_QQ) + c * fillExp#F2;
	  );
     if #fillResult == 0 then return new HashTable from {};
     cLam := pmComputeCLambda(lambda, n);
     new HashTable from for k in keys fillResult list (
	  if fillResult#k != 0 then k => fillResult#k / cLam else continue
	  )
     );

-- Pick the appropriate gl_n action based on Convention.
-- shape, n: only used in the Filling case (for c_lambda correction).
pmActEConv0 = (i, j, T, conv, shape, n) -> (
     if conv === "Row" or conv === "Weyl" then pmActE0(i, j, T)
     else if conv === "Filling" then pmActEFilling0(i, j, T, shape, n)
     else error("unknown Convention: ", conv)
     );

verifyEquivariant = method(Options => {Convention => "Row", Verbose => false, Direction => "Inclusion"})

-- Pieri (or pieriColumn) inclusion.  M : S_mu V -> Sym^d V (x) S_lambda V
-- (or wedge instead of Sym for SkewCommutative P).  M is dim_lambda x dim_mu
-- over P, entries homogeneous of degree d.
verifyEquivariant(Matrix, List, List, PolynomialRing) := opts ->
     (M, mu, boxes, P) -> (
     if opts.Direction === "Dual" then
	  return verifyEquivariantDual0(M, mu, boxes, P, opts);
     n := numgens P;
     X := gens P;
     conv := opts.Convention;
     lambda := mu;
     for b in boxes do lambda = subtractOne(lambda, b);
     while #lambda > 0 and lambda#-1 == 0 do lambda = drop(lambda, -1);
     stdsMu  := pmStdTabsByConv(n, mu,     conv);
     stdsLam := pmStdTabsByConv(n, lambda, conv);
     -- Index lookup tables: O(1) replacement for `position(stds, s -> s == k)`.
     idxOfMu  := hashTable for i from 0 to #stdsMu  - 1 list stdsMu#i  => i;
     idxOfLam := hashTable for i from 0 to #stdsLam - 1 list stdsLam#i => i;
     -- Cache columns once per tIdx (used by both LHS and RHS branches).
     colsM := for tIdx from 0 to #stdsMu - 1 list flatten entries M_{tIdx};
     ok := true;
     for i from 0 to n-1 do for j from 0 to n-1 do (
	  if i == j then continue;
	  -- Hoist: pmActEConv0(i, j, stdsLam#idx, conv) and (i, j, stdsMu#tIdx, conv)
	  -- depend ONLY on (i, j, idx) -- not on the outer tIdx loop variable.
	  -- Without this hoist they'd be recomputed #stdsMu times each.
	  lamActs := for idx from 0 to #stdsLam - 1 list pmActEConv0(i, j, stdsLam#idx, conv, lambda, n);
	  muActs  := for tIdx from 0 to #stdsMu - 1 list pmActEConv0(i, j, stdsMu#tIdx, conv, mu, n);
	  for tIdx from 0 to #stdsMu-1 do (
	       colT := colsM#tIdx;
	       -- RHS = E . M(T)
	       vAction := for f in colT list X#i * diff(X#j, f);
	       sAction := new MutableList from for s in stdsLam list 0_P;
	       for idx from 0 to #stdsLam-1 do (
		    h := lamActs#idx;
		    for kk in keys h do
			 if idxOfLam#?kk then
			      sAction#(idxOfLam#kk) = sAction#(idxOfLam#kk) + h#kk * colT#idx;
		    );
	       RHS := for idx from 0 to #stdsLam-1 list (vAction#idx + sAction#idx);
	       -- LHS = M(E . T)
	       LHS := new MutableList from for s in stdsLam list 0_P;
	       hT := muActs#tIdx;
	       for kk in keys hT do
		    if idxOfMu#?kk then (
			 colK := colsM#(idxOfMu#kk);
			 ck := hT#kk;
			 for r from 0 to #stdsLam-1 do LHS#r = LHS#r + ck * colK#r;
			 );
	       if toList LHS =!= RHS then (
		    ok = false;
		    if opts.Verbose then
			 stdio << "  [verifyEquivariant] FAIL at E_{" << i << "," << j
			       << "}, T = " << toString stdsMu#tIdx << endl;
		    );
	       );
	  );
     ok
     );

-- LR inclusion.  M : S_lambda V -> S_nu V (x) S_mu V.
-- M is (dim_nu * dim_mu) x dim_lambda over QQ; rows in lex order with T_nu outer.
verifyEquivariant(Matrix, Sequence, ZZ) := opts -> (M, shapes, n) -> (
     if opts.Direction === "Dual" then
	  return verifyEquivariantDualLR0(M, shapes, n, opts);
     (lambda, mu, nu) := shapes;
     muPad := mu | toList(#lambda - #mu : 0);
     conv := opts.Convention;
     stdsLam := pmStdTabsByConv(n, lambda, conv);
     stdsMu  := pmStdTabsByConv(n, muPad,  conv);
     stdsNu  := pmStdTabsByConv(n, nu,     conv);
     dimNu := #stdsNu; dimMu := #stdsMu; dimLam := #stdsLam;
     -- Index tables for O(1) tableau-to-position lookups.
     idxOfLam := hashTable for i from 0 to dimLam-1 list stdsLam#i => i;
     idxOfMu  := hashTable for i from 0 to dimMu-1  list stdsMu#i  => i;
     idxOfNu  := hashTable for i from 0 to dimNu-1  list stdsNu#i  => i;
     -- Cache columns of M.
     colsM := for tIdx from 0 to dimLam-1 list flatten entries M_{tIdx};
     ok := true;
     for i from 0 to n-1 do for j from 0 to n-1 do (
	  if i == j then continue;
	  -- Hoist actions: each pmActEConv0(i, j, std, conv) depends only on (i, j, std).
	  nuActs  := for iN from 0 to dimNu-1  list pmActEConv0(i, j, stdsNu#iN, conv, nu, n);
	  muActs  := for iM from 0 to dimMu-1  list pmActEConv0(i, j, stdsMu#iM, conv, muPad, n);
	  lamActs := for iL from 0 to dimLam-1 list pmActEConv0(i, j, stdsLam#iL, conv, lambda, n);
	  for tIdx from 0 to dimLam-1 do (
	       colT := colsM#tIdx;
	       -- RHS = E . M(T): Leibniz on S_nu (x) S_mu
	       RHSrows := new MutableList from for r from 0 to dimNu*dimMu-1 list 0_QQ;
	       for iN from 0 to dimNu-1 do for iM from 0 to dimMu-1 do (
		    coef := colT#(iN*dimMu + iM);
		    if coef == 0 then continue;
		    -- E acts on T_nu^{iN}
		    hN := nuActs#iN;
		    for kN in keys hN do
			 if idxOfNu#?kN then
			      RHSrows#((idxOfNu#kN)*dimMu + iM) = RHSrows#((idxOfNu#kN)*dimMu + iM) + hN#kN * coef;
		    -- E acts on T_mu^{iM}
		    hM := muActs#iM;
		    for kM in keys hM do
			 if idxOfMu#?kM then
			      RHSrows#(iN*dimMu + (idxOfMu#kM)) = RHSrows#(iN*dimMu + (idxOfMu#kM)) + hM#kM * coef;
		    );
	       RHS := toList RHSrows;
	       -- LHS = M(E . T)
	       LHS := new MutableList from for r from 0 to dimNu*dimMu-1 list 0_QQ;
	       hT := lamActs#tIdx;
	       for kk in keys hT do
		    if idxOfLam#?kk then (
			 colK := colsM#(idxOfLam#kk);
			 ck := hT#kk;
			 for r from 0 to dimNu*dimMu-1 do
			      LHS#r = LHS#r + ck * colK#r;
			 );
	       if toList LHS =!= RHS then (
		    ok = false;
		    if opts.Verbose then
			 stdio << "  [verifyEquivariant LR] FAIL at E_{" << i << "," << j
			       << "}, T = " << toString stdsLam#tIdx << endl;
		    );
	       );
	  );
     ok
     );

-- Dual Pieri projection.  M : Sym^d V (x) S_lambda V -> S_mu V.  M is K-matrix
-- of size dim_mu x (dim_lambda * #monsDeg_d), columns in (T_lambda, alpha) order.
-- Activated by Direction => "Dual"; with Direction => "Inclusion" (default), the
-- 4-arg overload above handles the inclusion direction.
verifyEquivariantDual0 = (M, mu, boxes, P, opts) -> (
     n := numgens P;
     X := gens P;
     d := #boxes;
     conv := opts.Convention;
     K := coefficientRing P;
     monBasis := flatten entries basis(d, P);
     nMons := #monBasis;
     lambda := mu;
     for b in boxes do lambda = subtractOne(lambda, b);
     while #lambda > 0 and lambda#-1 == 0 do lambda = drop(lambda, -1);
     stdsMu  := pmStdTabsByConv(n, mu,     conv);
     stdsLam := pmStdTabsByConv(n, lambda, conv);
     dimLam := #stdsLam; dimMu := #stdsMu;
     srcIdx := (iLam, iAlpha) -> iLam * nMons + iAlpha;
     -- Index tables for O(1) tableau lookup (replaces linear `position`).
     idxOfLam := hashTable for i from 0 to dimLam-1 list stdsLam#i => i;
     idxOfMu  := hashTable for i from 0 to dimMu-1  list stdsMu#i  => i;
     -- Cache all columns of M once (each column read used by many iterations).
     totalCols := dimLam * nMons;
     colsM := for c from 0 to totalCols - 1 list flatten entries M_{c};
     -- Pre-decompose E_{i,j}.x^alpha in the monomial basis ONCE for all alpha.
     ok := true;
     for i from 0 to n-1 do for j from 0 to n-1 do (
	  if i == j then continue;
	  -- Pre-compute the monomial-decomposition of E.x^alpha for each alpha.
	  monActDecomp := for iAlpha from 0 to nMons-1 list (
	       newPoly := X#i * diff(X#j, monBasis#iAlpha);
	       if newPoly == 0 then {} else
	            for iA2 from 0 to nMons-1 list (
			 c := lift(coefficient(monBasis#iA2, newPoly), K);
			 if c == 0 then continue else (iA2, c)
			 )
	       );
	  -- Pre-compute pmActEConv0(i, j, std, conv) for each std (lam, mu).
	  lamActs := for iLam from 0 to dimLam-1 list pmActEConv0(i, j, stdsLam#iLam, conv, lambda, n);
	  muActs  := for iM from 0 to dimMu-1  list pmActEConv0(i, j, stdsMu#iM,  conv, mu, n);
	  for iLam from 0 to dimLam-1 do for iAlpha from 0 to nMons-1 do (
	       sIdx := srcIdx(iLam, iAlpha);
	       -- LHS: M(E . source)
	       LHS := new MutableList from for r from 0 to dimMu-1 list 0_K;
	       -- (a) E acts on x^alpha
	       for pair in monActDecomp#iAlpha do (
		    (iA2, c) := pair;
		    colNew := colsM#(srcIdx(iLam, iA2));
		    for r from 0 to dimMu-1 do LHS#r = LHS#r + c * colNew#r;
		    );
	       -- (b) E acts on T_lambda
	       hL := lamActs#iLam;
	       for kk in keys hL do
		    if idxOfLam#?kk then (
			 colNew := colsM#(srcIdx(idxOfLam#kk, iAlpha));
			 ck := hL#kk;
			 for r from 0 to dimMu-1 do LHS#r = LHS#r + ck * colNew#r;
			 );
	       -- RHS: E . M(source)
	       colSource := colsM#sIdx;
	       RHS := new MutableList from for r from 0 to dimMu-1 list 0_K;
	       for iM from 0 to dimMu-1 do (
		    coef := colSource#iM;
		    if coef == 0 then continue;
		    hM := muActs#iM;
		    for kk in keys hM do
			 if idxOfMu#?kk then
			      RHS#(idxOfMu#kk) = RHS#(idxOfMu#kk) + hM#kk * coef;
		    );
	       if toList LHS =!= toList RHS then (
		    ok = false;
		    if opts.Verbose then stdio << "  [verifyEquivariant dualPieri] FAIL at E_{"
		         << i << "," << j << "}, (T_lambda, alpha) = (" << toString stdsLam#iLam
			 << ", " << toString monBasis#iAlpha << ")" << endl;
		    );
	       );
	  );
     ok
     );

-- Dual LR projection.  M : S_nu V (x) S_mu V -> S_lambda V.  M is QQ-matrix
-- of size dim_lambda x (dim_nu * dim_mu).
verifyEquivariantDualLR0 = (M, shapes, n, opts) -> (
     (lambda, mu, nu) := shapes;
     muPad := mu | toList(#lambda - #mu : 0);
     conv := opts.Convention;
     stdsLam := pmStdTabsByConv(n, lambda, conv);
     stdsMu  := pmStdTabsByConv(n, muPad,  conv);
     stdsNu  := pmStdTabsByConv(n, nu,     conv);
     dimNu := #stdsNu; dimMu := #stdsMu; dimLam := #stdsLam;
     srcIdx := (iN, iM) -> iN * dimMu + iM;
     -- Index tables and column cache for O(1) lookups.
     idxOfLam := hashTable for i from 0 to dimLam-1 list stdsLam#i => i;
     idxOfMu  := hashTable for i from 0 to dimMu-1  list stdsMu#i  => i;
     idxOfNu  := hashTable for i from 0 to dimNu-1  list stdsNu#i  => i;
     totalCols := dimNu * dimMu;
     colsM := for c from 0 to totalCols - 1 list flatten entries M_{c};
     ok := true;
     for i from 0 to n-1 do for j from 0 to n-1 do (
	  if i == j then continue;
	  -- Hoist actions: each pmActEConv0(i, j, std, conv) computed once per std.
	  nuActs  := for iN from 0 to dimNu-1  list pmActEConv0(i, j, stdsNu#iN, conv, nu, n);
	  muActs  := for iM from 0 to dimMu-1  list pmActEConv0(i, j, stdsMu#iM, conv, muPad, n);
	  lamActs := for iL from 0 to dimLam-1 list pmActEConv0(i, j, stdsLam#iL, conv, lambda, n);
	  for iN from 0 to dimNu-1 do for iM from 0 to dimMu-1 do (
	       sIdx := srcIdx(iN, iM);
	       -- LHS = M(E . (T_nu (x) T_mu))
	       LHS := new MutableList from for r from 0 to dimLam-1 list 0_QQ;
	       hN := nuActs#iN;
	       for kN in keys hN do
		    if idxOfNu#?kN then (
			 colNew := colsM#(srcIdx(idxOfNu#kN, iM));
			 ck := hN#kN;
			 for r from 0 to dimLam-1 do LHS#r = LHS#r + ck * colNew#r;
			 );
	       hM := muActs#iM;
	       for kM in keys hM do
		    if idxOfMu#?kM then (
			 colNew := colsM#(srcIdx(iN, idxOfMu#kM));
			 ck := hM#kM;
			 for r from 0 to dimLam-1 do LHS#r = LHS#r + ck * colNew#r;
			 );
	       -- RHS = E . M(source)
	       colSource := colsM#sIdx;
	       RHS := new MutableList from for r from 0 to dimLam-1 list 0_QQ;
	       for iL from 0 to dimLam-1 do (
		    coef := colSource#iL;
		    if coef == 0 then continue;
		    hL := lamActs#iL;
		    for kk in keys hL do
			 if idxOfLam#?kk then
			      RHS#(idxOfLam#kk) = RHS#(idxOfLam#kk) + hL#kk * coef;
		    );
	       if toList LHS =!= toList RHS then (
		    ok = false;
		    if opts.Verbose then stdio << "  [verifyEquivariant dualLR] FAIL at E_{"
		         << i << "," << j << "}, (T_nu, T_mu) = (" << toString stdsNu#iN
			 << ", " << toString stdsMu#iM << ")" << endl;
		    );
	       );
	  );
     ok
     );

-- ====================================================================
--  symbolicForm: pretty-print a labeled matrix as a readable basis-element
--  map.  Each row of the netList shows a source basis element and its image
--  as a list of (target_label, coefficient) terms.
--
--  When the matrix M was produced by pieri / pieriColumn / lrMap / dualPieri /
--  dualPieriColumn / dualLR, the source/target labels are pulled from
--  M.cache.sourceBasis / M.cache.targetBasis.  For other matrices, integer
--  indices 1..rank are used.
-- ====================================================================
-- Pretty-print a PM tableau (List of rows) as a boxed grid, columns top-down.
prettyPMTabPM = T -> (
     if T === {} or all(T, r -> #r == 0) then return net "()";
     width := max apply(T, r -> #r);
     cols := for j from 0 to width-1 list (
	  for i from 0 to #T - 1 list (if j < #(T#i) then T#i#j else continue));
     netList {apply(cols, col -> stack apply(col, e -> net e))}
     );

-- Heuristic check: does a List look like a row-form tableau (list of lists of integers)?
isPMTabPM = T -> instance(T, List) and all(T, r -> instance(r, List)) and
     all(T, r -> all(r, e -> instance(e, ZZ)));

-- Render a single basis label, recursively handling tuples.
prettyLabelPM = lab -> (
     if isPMTabPM lab then prettyPMTabPM lab
     else if instance(lab, Sequence) then (
	  parts := apply(toList lab, prettyLabelPM);
	  if #parts == 0 then net "()"
	  else fold((a, b) -> horizontalJoin {a, net "  ⊗  ", b}, parts)
	  )
     else net lab
     );

-- Render the image of one source basis element (a list of (target_label, coef) pairs).
prettyImagePM = img -> (
     if #img == 0 then net "0"
     else stack apply(img, term -> (
	       (lab, coef) := (term#0, term#1);
	       horizontalJoin {net coef, net "  *  ", prettyLabelPM lab}))
     );

-- Robust label-comparison key.  Lists compare by value automatically; Filling
-- instances need to be flattened to a List first since they don't have ==.
labelKeyPM = lab -> (
     if instance(lab, List) then lab
     else if instance(lab, Sequence) then toList apply(toList lab, labelKeyPM)
     else if instance(lab, BasicList) then toList lab
     else lab
     );

symbolicForm = method()
symbolicForm Matrix := M -> (
     M.cache#"sourceBasis" ??= toList(0 .. rank source M - 1);
     M.cache#"targetBasis" ??= toList(0 .. rank target M - 1);
     M.cache#"rule" ??= (
	  -- Build label-to-index map once for O(1) lookup.
	  srcLabels := M.cache#"sourceBasis";
	  idxOfSrc := hashTable for k from 0 to #srcLabels - 1 list
	       labelKeyPM(srcLabels#k) => k;
	  i -> (
	       key := labelKeyPM i;
	       if not idxOfSrc#?key then error "symbolicForm: source label not found";
	       l := idxOfSrc#key;
	       col := flatten entries M_{l};
	       L := positions(col, j -> not(j == 0_(ring M)));
	       for j in L list ((M.cache#"targetBasis")_j, col_j)
	       )
	  );
     netList for i in M.cache#"sourceBasis" list (
	  {prettyLabelPM i, prettyImagePM ((M.cache#"rule")(i))})
     );

----------------------------------------------------------------------
-- Tests and Documentation are kept in auxiliary files; see PieriMaps/
----------------------------------------------------------------------

load "./PieriMaps/tests.m2"

beginDocumentation()
load "./PieriMaps/doc.m2"

end
loadPackage "PieriMaps"
installPackage PieriMaps
