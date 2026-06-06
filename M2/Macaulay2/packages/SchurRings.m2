---*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 2007, 2011 Michael Stillman
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

newPackage(
	"SchurRings",
    	Version => "2.0",
    	Date => "April 17, 2026",
    	Authors => {
	     {Name => "Michael Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"},
	     {Name => "Hal Schenck"},
	     {Name => "Claudiu Raicu", Email => "craicu@nd.edu", HomePage => "http://math.berkeley.edu/~claudiu/"},
	     {Name => "Keller VandeBogert", Email => "keller.v@uky.edu", HomePage => "https://sites.google.com/view/kellervandebogert/home"}
	     },
	Keywords => {"Representation Theory"},
    	Headline => "representation rings of general linear groups and of symmetric groups",
	DebuggingMode => false
    	)

export {"schurRing", "SchurRing", "symmetricRing",
     "toS", "toE", "toP", "toH", "toM", "toSp", "toO", "specialize",
     "kostkaNumber",
     "jacobiTrudi", "plethysm",
     "centralizerSize", "classFunction", "symmetricFunction",
     "scalarProduct", "internalProduct",
     "SchurRingIndexedVariableTable", "EHPVariables", "SVariable",
     "ClassFunction", "schurLevel",
     "schurResolution",
     "SchurRingElement",
     "Memoize", "Schur", "EorH", "GroupActing", "Basis", "OddOrEven",
     "eVariable", "pVariable", "hVariable",
     "modificationRule",
     "branch",
     "toRatGL",
     "toSymm", "toGL", "toSn", "convert"
     }

debug Core

protect symbol symRingForE;
protect symbol mapToE;
protect symbol symRingForP;
protect symbol mapToP;
protect symbol mapFromP;
protect symbol grbE
protect symbol PtoETable
protect symbol HtoETable
protect symbol grbH
protect symbol PtoHTable
protect symbol EtoHTable
protect symbol grbP
protect symbol EtoPTable
protect symbol HtoPTable
--protect symbol plethysmMaps
protect symbol mapFromE
protect symbol sFunction
-- dispatch attributes for variant support (GL/Sn/Sp/O, Monomial, etc.)
protect symbol multiplySchurLevel1
protect symbol highLevelCombine
protect symbol plethysmFcn
protect symbol recTransOp
protect symbol kostkaCache
protect symbol monomialBasisRing
protect symbol symplecticBasisRing
protect symbol orthogonalBasisRing
protect symbol schurBasisRing
protect symbol stableSchurHelper
protect symbol plethysmHelpers
protect symbol specializedSpRings
protect symbol specializedORings
protect symbol specializedGLRings
protect symbol specializedSLRings
protect symbol specializedRatGLRings
protect symbol specializedSnRings
protect symbol associatedRatGLRing
protect symbol ratNegRing
protect symbol ratNegSym
protect symbol ratPosSym
protect symbol ratOuterRing
protect symbol ratRank
protect symbol ratStableHelper


SchurRing = new Type of EngineRing
SchurRing.synonym = "Schur ring"
ClassFunction = new Type of HashTable
ClassFunction.synonym = "Class function"

describe SchurRing := S -> Describe (expression schurRing) (expression last S.baseRings, S.Symbol, S.numgens)
undocumented (describe, SchurRing)

expression SchurRing := S -> (
    if hasAttribute(S, ReverseDictionary)
    then toString getAttribute(S, ReverseDictionary)
    else new FunctionApplication from unhold describe S)
undocumented (expression, SchurRing)

-----------------------------------------------------------------------------
-- Engine boundary: SchurRing plumbing.
-- This section wraps the C++-side `rawSchurRing1` engine (which implements
-- Littlewood-Richardson multiplication on Schur-partition monomials) as an
-- M2 `SchurRing` type.  The engine stores elements as polynomials in
-- monomials-that-are-partitions; the helpers below convert to/from that
-- representation and install element-access sugar (s_lambda, etc.) on top
-- of the raw engine ring.
-----------------------------------------------------------------------------

rawmonom2partition = (m) -> (
     -- Decode an engine Schur monomial (= a raw monomial representing s_lambda)
     -- into its partition.  `rawSparseListFormMonomial m` returns pairs
     -- (x,e) = (row-length, multiplicity); `e:x` builds an e-tuple of x's,
     -- `splice` flattens the list of tuples into the multiset of row lengths,
     -- and `reverse` puts them in weakly-decreasing (partition) order.
     reverse splice apply(rawSparseListFormMonomial m, (x,e) -> e:x)
     )

-- Strip trailing zero parts from a (weakly-decreasing) list representing a
-- partition.  Used to normalize partition keys for memoization and for
-- returning partitions without padding.
stripTrailingZeros = lst -> (
     k := #lst;
     while k > 0 and lst#(k-1) == 0 do k = k - 1;
     take(lst, k)
     )

--various ways of addressing elements of a Schur ring
-- s_{lambda_1, lambda_2, ...} from an explicit partition list
SchurRing _ List := (SR, L) -> new SR from rawSchurFromPartition(raw SR, L)
-- s_(lambda_1, lambda_2, ...) from a partition given as a sequence
SchurRing _ Sequence := (SR, L) -> new SR from rawSchurFromPartition(raw SR, L)
-- s_L for a single-row partition L; `1:L` is M2 syntax for the length-1 tuple (L)
SchurRing _ ZZ := (SR, L) -> new SR from rawSchurFromPartition(raw SR, 1:L)
--
coefficientRing SchurRing := Ring => R -> last R.baseRings
numgens SchurRing := Ring => R -> R.numgens

-- `schurLevel R` = nesting depth: the number of times schurRing/symmetricRing
-- was applied to build R.  Plain rings (e.g. QQ) have level 0; a single
-- schurRing(QQ, s, n) has level 1; schurRing(schurRing(QQ, t, m), s, n) has
-- level 2; each wrap increments the stored value by 1 (see newSchur2 below).
schurLevel = method()
schurLevel (Ring) := R -> if R.?schurLevel then R.schurLevel else 0

--Construction of Schur rings
newSchur2 = method()
newSchur2(Ring,Symbol) := (A,p) -> newSchur2(A,p,-1)

-- Concrete element type returned by `newSchurEngineRing`, used so that M2's
-- method dispatch can distinguish SchurRing elements from generic RingElement.
SchurRingElement = new Type of RingElement
newSchurEngineRing = R -> (
     -- Wrap the raw engine ring R as an M2 SchurRing of SchurRingElement,
     -- bind the RawRing pointer, and cache the identity/zero shortcuts.
     S := new SchurRing of SchurRingElement;
     S.RawRing = R;
     S#1 = 1_S;
     S#0 = 0_S;
     S)

newSchur2(Ring,Symbol,ZZ) := (A,p,n) -> (
     if not (A.?Engine and A.Engine)
     then error "expected coefficient ring handled by the engine";
     -- Build a SchurRing over coefficient ring A with rank n
     -- (n = -1 is the engine's sentinel for "infinite rank / stable GL").
     -- Steps:
     --   (1) build the engine ring via rawSchurRing1 and wrap it;
     --   (2) attach M2-side metadata (Symbol, baseRings, numgens, ...);
     --   (3) install generic engine-backed overloads (+, *, ...);
     --   (4) override expression / listForm to present elements as
     --       partition-indexed s_lambda terms;
     --   (5) propagate .char and bump .schurLevel.
     -- (1) engine ring + M2 wrapper
     SR := newSchurEngineRing rawSchurRing1(raw A,n);
     -- (2) M2-side metadata
     SR.Symbol = p;
     SR.baseRings = append(A.baseRings,A);
     SR.generators = {};
     SR.numgens = if n < 0 then infinity else n;
     SR.degreeLength = 0;
     -- (3) the basic features of SR (arithmetic, etc.) are coded at the engine level
     commonEngineRingInitializations SR;
     ONE := SR#1;
     if A.?char then SR.char = A.char;
     -- (4) pretty-printing: walk (coeff, monomial) pairs from the engine and
     --     render each monomial as s_lambda via rawmonom2partition.
     toExternalString SR := r -> toString expression r;
     expression SR := f -> (
	  (coeffs,monoms) -> sum(
	       coeffs,monoms,
	       (a,m) -> expression (if a == 1 then 1 else new A from a) *
	          new Subscript from {p, (
		    t1 := toSequence rawmonom2partition m;
		    if #t1 === 1 then t1#0 else t1
		    )})
	  ) rawPairs(raw A, raw f);
     -- structured enumeration: return [(partition, coefficient)] pairs
     listForm SR := (f) -> (
     	  n := numgens SR;
     	  (cc,mm) := rawPairs(raw A, raw f);
     	  toList apply(cc, mm, (c,m) -> (rawmonom2partition m, new A from c)));
     -- (5) nesting-depth bookkeeping
     if (A.?schurLevel) then SR.schurLevel = A.schurLevel + 1
     else SR.schurLevel = 1;
     SR
     )

-- SL(n) canonicalization: for finite SL(n), irreducibles are indexed by
-- partitions lambda with lambda_n = 0.  Any partition with n rows of
-- length k at the bottom (i.e. lambda_n = k > 0) equals
--     det^k tensor s_{lambda_1-k, ..., lambda_{n-1}-k}
-- which in SL collapses to s_{lambda_1-k, ..., lambda_{n-1}-k}.
-- Apply this row-by-row: if #lambda == n, strip lambda_n from every part.
-- Rings with numgens = infinity are treated as "stable SL" == GL (no
-- determinant to collapse since there's no top row to pin down).
slCanonicalize = (f, S) -> (
     n := numgens S;
     if n === infinity then return f;
     rawRes := raw(0_S);
     for term in listForm f do (
	  lam := term#0;
	  c := term#1;
	  if #lam > n then continue;  -- engine usually prevents this; be safe
	  lamList := toList lam | toList((n - #lam) : 0);
	  k := lamList#(n-1);
	  lamNew := (
	       if k == 0 then toList lam
	       else stripTrailingZeros(for i from 0 to n-1 list lamList#i - k)
	       );
	  rawRes = rawRes + raw promote(c, S) * raw (S_lamNew);
	  );
     new S from rawRes
     );

schurRing = method(Options => {EHPVariables => (getSymbol"e",getSymbol"h",getSymbol"p"), SVariable => getSymbol"s", GroupActing => "GL", Basis => "Schur", OddOrEven => null})
schurRing(Ring,Thing,ZZ) := SchurRing => opts -> (A,p,n) -> (
     try p = baseName p else error "schurRing: can't use provided thing as variable";
     if class p === Symbol then schurRing(A,p,n,opts)
     else error "schurRing: can't use provided thing as variable"
     );
schurRing(Ring,Thing) := SchurRing => opts -> (A,p) -> (
     try p = baseName p else error "schurRing: can't use provided thing as variable";
     if class p === Symbol then schurRing(A,p,opts)
     else error "schurRing: can't use provided thing as variable"
     );

dim SchurRingElement := s -> dimSchur s;
dim(List,SchurRingElement) := (lis,s) -> dimSchur(lis, s);
dim(Thing,SchurRingElement) := (n,s) -> dimSchur(n, s);


---------------------------------------------------------------
---- Rational (type A rational) SchurRings (Koike-Terada) ------
---------------------------------------------------------------
-- A rational irreducible representation of GL(n) is indexed by a pair
-- (alpha, beta) of partitions with ell(alpha) + ell(beta) <= n, giving the
-- dominant integral weight
--     (alpha_1, ..., alpha_p, 0, ..., 0, -beta_q, ..., -beta_1).
-- The stable universal rational character ring (Koike-Terada) has basis
-- {r_{alpha, beta}} and is isomorphic to S (x) S (two disjoint copies of the
-- ring of symmetric functions).  Multiplication is componentwise:
--    r_{alpha, beta} * r_{gamma, delta}
--         = sum_{mu, nu} c^mu_{alpha, gamma} c^nu_{beta, delta} r_{mu, nu}.
-- We therefore implement RatGL as a thin re-tagging of a level-2 GL/GL
-- SchurRing: the outer layer tracks alpha, the inner (auto-generated) layer
-- tracks beta.  All existing level-2 GL/GL multiplication infrastructure
-- applies unchanged.
--
-- Specialization of the stable universal character to a finite rank n is the
-- Koike-Terada modification rule, which is computed here by applying the
-- Weyl reflection formula to the composite weight above.
---------------------------------------------------------------

-- Koike-Terada / Sam-Snowden-Weyman modification rule for rational GL(n).
--
-- Given a stable bipartition label (alpha, beta), specialize
--     chi^{stable}_{alpha, beta} |_{GL(n)}
-- to a finite rational GL(n) character following [SSW] Sec. 5.4 in the
-- border-strip form.  A pair (alpha, beta) is *admissible* when
-- ell(alpha) + ell(beta) <= n; for admissible pairs the specialization is
-- chi^{GL(n)}_{alpha, beta} with sign +1.
--
-- When (alpha, beta) is not admissible, remove a border strip of length
--     L = ell(alpha) + ell(beta) - n - 1
-- from BOTH alpha and beta, starting at the first box of the final row
-- (equivalently the hook at (k,1) with alpha_k + ell(alpha) - k = L and
-- likewise for beta).  The sign contribution of the step is
--     (-1)^(c(R_alpha) + c(R_beta) - 1)
-- where c(R) is the number of columns the strip occupies.  Recurse until
-- admissible, or return 0 if at some step no valid strip of the required
-- length exists.  (L = 0 also forces vanishing: the strip must be non-empty.)
--
-- Returns a list of triples (alpha', beta', coef): either empty (character
-- vanishes) or a singleton [(alpha', beta', sign)] with sign in {+1, -1}.
ratGLModify = (alpha, beta, n) -> (
     a := stripTrailingZeros toList alpha;
     b := stripTrailingZeros toList beta;
     if #a + #b <= n then return {(a, b, 1)};
     sign := 1;
     while #a + #b > n do (
	  L := #a + #b - n - 1;
	  if L == 0 then return {};
	  kA := findBorderStripRow(a, L);
	  if kA === null then return {};
	  kB := findBorderStripRow(b, L);
	  if kB === null then return {};
	  (newA, cA) := removeBorderStripAtFirstColumn(a, kA);
	  (newB, cB) := removeBorderStripAtFirstColumn(b, kB);
	  if odd (cA + cB - 1) then sign = -sign;
	  a = newA;
	  b = newB;
	  );
     {(a, b, sign)}
     )

-- Enumerate all partitions contained componentwise in the given bound.
-- Result includes the empty partition and the bound itself; trailing zeros
-- are stripped.
allSubpartitionsBoundedBy = (bound) -> (
     b := stripTrailingZeros toList bound;
     if #b == 0 then return {{}};
     aux := (idx, prevMax) -> (
	  if idx >= #b then return {{}};
	  upper := min(b#idx, prevMax);
	  flatten for v from 0 to upper list
	       for t in aux(idx+1, v) list prepend(v, t)
	  );
     apply(aux(0, infinity), stripTrailingZeros)
     )

-- Iterate all (alpha, beta, scalar) triples of a RatGL element.
-- Calls f(alpha, beta, scalar) for each triple; scalar lives in the ultimate
-- base (coefficient) ring of the inner layer.
iterateRatGLTerms = (elt, f) -> (
     for outerTerm in listForm elt do (
	  alpha := toList outerTerm#0;
	  bElt := outerTerm#1;
	  for innerTerm in listForm bElt do (
	       beta := toList innerTerm#0;
	       scalar := innerTerm#1;
	       f(alpha, beta, scalar);
	       );
	  );
     )

-- Stable (infinity-rank) RatGL helper ring attached to a finite RatGL ring S.
-- Lazily constructed and cached on the ring.  Used by finite RatGL
-- multiplication (stable componentwise LR, then Koike-Terada modification).
stableRatGLHelperOf = (S) -> (
     if S.?ratStableHelper then S.ratStableHelper
     else (
	  baseR := coefficientRing (S.ratNegRing);
	  helperSym := getSymbol("ratHelper" | toString hash S);
	  H := schurRing(baseR, helperSym, infinity, GroupActing => "RatGL");
	  S.ratStableHelper = H;
	  H
	  )
     )

buildRatGLRing = (R, p, n, opts) -> (
     ------------------------------------------------------------------
     -- (1) Inner / outer ring construction
     --     Build a two-layer GL SchurRing tower: the inner layer B
     --     (beta / "negative" variables) sits over R, and the outer
     --     layer S (alpha / "positive" variables) sits over B.  The
     --     bipartition basis element r_{alpha,beta} will later be
     --     realized as (S_alpha) * (lift of B_beta into S).
     ------------------------------------------------------------------
     negSym := getSymbol(toString p | "Neg");
     innerOpts := opts ++ {GroupActing => "GL", OddOrEven => null};
     B := schurRing(R, negSym, n, innerOpts);
     outerOpts := opts ++ {GroupActing => "GL", OddOrEven => null};
     S := schurRing(B, p, n, outerOpts);

     ------------------------------------------------------------------
     -- (2) Metadata tagging
     --     Re-tag S as the RatGL ring and remember both layers.
     ------------------------------------------------------------------
     S.GroupActing = "RatGL";
     S.ratNegRing = B;
     S.ratNegSym = negSym;
     S.ratPosSym = p;
     S.ratRank = n;
     B.ratOuterRing = S;

     ------------------------------------------------------------------
     -- (3) Multiplication override
     --     Multiplication on universal rational characters is the
     --     Koike product, NOT the componentwise Littlewood-Richardson
     --     product on the auxiliary Schur-polynomial basis.
     --       * Stable rank (n = infinity): install the raw Koike
     --         product directly.
     --       * Finite rank n: lift both factors into the stable
     --         helper ring, multiply there via Koike, then apply the
     --         Koike-Terada (Sam-Snowden-Weyman) modification rule
     --         to specialize back down to rank n.
     ------------------------------------------------------------------
     isFiniteN := not (class n === InfiniteNumber or n < 0);
     if isFiniteN then (
	  S * S := (f1, f2) -> (
	       H := stableRatGLHelperOf S;
	       h1 := toRatGL(f1, H);
	       h2 := toRatGL(f2, H);
	       hProd := h1 * h2;
	       specializeRatGLInto(hProd, n, S)
	       );
	  )
     else (
	  -- Stable RatGL: override componentwise LR with the Koike product.
	  S * S := (f1, f2) -> new S from ratGLKoikeProductRaw(S, f1, f2);
	  );

     ------------------------------------------------------------------
     -- (4) Bipartition-syntax override via IndexedVariableTable
     --     Override so that user-facing bipartition syntax
     --       r_{{a1,a2,...}, {b1,b2,...}}   ->   r_{(alpha, beta)}
     --     builds the basis element directly.  At finite rank, if
     --     #alpha + #beta > n the character reduces via Koike-Terada;
     --     we apply that modification rule here.
     ------------------------------------------------------------------
     t := value p;
     t#symbol _ = a -> (
	  isListish := x -> instance(x, VisibleList);
	  if isListish a and #a == 2 and isListish (a#0) and isListish (a#1) then (
	       alpha := toList a#0;
	       beta := toList a#1;
	       isFin := not (class n === InfiniteNumber or n < 0);
	       if isFin and (#alpha + #beta) > n then (
		    return specializeRatGLInto(
			 new S from ratGLBasisRaw(S, alpha, beta),
			 n, S);
		    );
	       return new S from ratGLBasisRaw(S, alpha, beta);
	       );
	  S _ a
	  );
     S.use = So -> (globalAssign(p, t); So);
     S.use S;

     ------------------------------------------------------------------
     -- (5) Pretty-printing override
     --     Display r_(alpha, beta) instead of the internal product
     --     a_alpha * b_beta that actually represents it.
     ------------------------------------------------------------------
     expression S := f -> (
	  acc := null;
	  (outerCoeffs, outerMonoms) := rawPairs(raw B, raw f);
	  for i from 0 to #outerMonoms - 1 do (
	       alphaLst := toList rawmonom2partition (outerMonoms#i);
	       bElt := new B from (outerCoeffs#i);
	       (innerCoeffs, innerMonoms) := rawPairs(raw R, raw bElt);
	       for j from 0 to #innerMonoms - 1 do (
		    betaLst := toList rawmonom2partition (innerMonoms#j);
		    sc := new R from (innerCoeffs#j);
		    sub := new Subscript from {p, {alphaLst, betaLst}};
		    term := if sc == 1 then expression sub
			    else (expression sc) * (expression sub);
		    acc = if acc === null then term else acc + term;
		    );
	       );
	  if acc === null then expression 0 else acc
	  );
     S
     )

schurRing(Ring,Symbol) := opts -> (R,p) -> schurRing(R,p,infinity,opts)
schurRing(Ring,Symbol,InfiniteNumber) :=
schurRing(Ring,Symbol,ZZ) := SchurRing => opts -> (R,p,n) -> (
     ------------------------------------------------------------------
     -- (1) RatGL short-circuit
     --     Rational GL (Koike-Terada bipartition) rings have their
     --     own builder; dispatch there and return immediately.
     ------------------------------------------------------------------
     if opts.GroupActing == "RatGL" then return buildRatGLRing(R, p, n, opts);

     ------------------------------------------------------------------
     -- (2) newSchur2 construction + field copies
     --     Create the raw engine-level Schur ring S and copy across
     --     option data (EHPVariables, GroupActing, Basis).  Also:
     --       * handle the OddOrEven tag for orthogonal rings,
     --       * initialize the kostkaCache,
     --       * install plethysm-related operator overloads (@, ^,
     --         symmetricPower, exteriorPower) that are common to all
     --         GroupActing variants.
     ------------------------------------------------------------------
     S := local S;
     if n == infinity then S = newSchur2(R,p,-1) else S = newSchur2(R,p,n);
     S.EHPVariables = opts.EHPVariables;
     --S.SVariable = opts.SVariable;
     S.GroupActing = opts.GroupActing;
     S.Basis = opts.Basis;

     -- For GroupActing "O", distinguish O(2n+1) (type B_n, "Odd") from O(2n)
     -- (type D_n, "Even") at finite rank.  Stable rings (n = infinity) ignore
     -- the tag (both limits coincide) but we still store a default for
     -- downstream code that wants to read it.
     if opts.GroupActing == "O" then (
	  if opts.OddOrEven === null then S.OddOrEven = "Odd"
	  else if opts.OddOrEven === "Odd" or opts.OddOrEven === "Even" then
	       S.OddOrEven = opts.OddOrEven
	  else error("schurRing: OddOrEven must be \"Odd\" or \"Even\"; got " | toString opts.OddOrEven);
	  )
     else if opts.OddOrEven =!= null then
	  error "schurRing: OddOrEven is only meaningful with GroupActing => \"O\"";

     S.kostkaCache = new MutableHashTable;
     S @ RingElement := RingElement @ S := (f1,f2) -> plethysm(f1,f2);
     S^ZZ := (f,n) -> product apply(n,i->f);
     symmetricPower(ZZ,S) := (n,s) -> plethysm({n},s);
     exteriorPower(ZZ,S) := opts -> (n,s) -> plethysm(splice{n:1},s);

     --define the multiplication on S
     -- Raw (engine-level) multiplication — available for all variants
     rawMult := (f1,f2) -> new S from raw f1 * raw f2;

     -- For Sn: install ** operator (needed for recTrans and higher-level combine)
     if opts.GroupActing == "Sn" then (
     	  S ** S := (f1,f2) -> rawMult(f1,f2);
	  RingElement ** S := (f,g) -> if member(ring f,S.baseRings | {S}) then promote(f,S) ** g
	       	    	      	   	else if member(S,(ring f).baseRings | {ring f}) then f ** promote(g,S);
	  Number ** S := (f,g) -> if member(ring f,S.baseRings | {S}) then promote(f,S) ** g;
	  S ** Number := (f,g) -> if member(ring g,S.baseRings | {S}) then f ** promote(g,S);
     );

     ------------------------------------------------------------------
     -- (3) Dispatch table
     --     Each GroupActing variant installs three callbacks on S
     --     that are consumed by the unified S*S routine below:
     --       * S.multiplySchurLevel1(f1,f2)  -- product at schurLevel 1
     --       * S.highLevelCombine(c, bProd)  -- how to stitch a
     --                                          coefficient-ring
     --                                          product with a
     --                                          level-1 basis product
     --                                          at higher schurLevel
     --       * S.plethysmFcn                 -- plethysm algorithm
     --       * S.recTransOp                  -- operator used inside
     --                                          recTrans for
     --                                          recursive basis
     --                                          conversion
     ------------------------------------------------------------------
     -- Register dispatch functions based on GroupActing
     if opts.GroupActing == "GL" then (
	  -- -- GL: ordinary general linear group
	  --    Level-1 product is the raw engine Littlewood-Richardson
	  --    product; higher-level combine is ordinary * ; plethysm
	  --    and recTrans both multiplicative.
	  S.multiplySchurLevel1 = (f1,f2) -> rawMult(f1,f2);
	  S.highLevelCombine = (coeff, basisProd) -> rawMult(promote(coeff,S), basisProd);
	  S.plethysmFcn = plethysmGL;
	  S.recTransOp = (a,b) -> a*b;
     ) else if opts.GroupActing == "SL" then (
	  -- -- SL: special linear (collapse determinant)
	  --    Same multiplication as GL, then strip a full column of n
	  --    rows (determinant character is trivial in SL).  Stable
	  --    SL (numgens = infinity) coincides with stable GL.
	  S.multiplySchurLevel1 = (f1,f2) -> slCanonicalize(rawMult(f1,f2), S);
	  S.highLevelCombine = (coeff, basisProd) ->
	       rawMult(promote(coeff,S), basisProd);
	  S.plethysmFcn = plethysmGL;
	  S.recTransOp = (a,b) -> a*b;
     ) else if opts.GroupActing == "Sn" then (
	  -- -- Sn: symmetric group (internal product / Kronecker)
	  --    Level-1 product is the internal (Kronecker) product of
	  --    Sn characters; both high-level combine and recTransOp
	  --    also use **, since the Sn ring is "internal" at every
	  --    schurLevel.  Liftable / zero shortcut avoids engine
	  --    edge cases on scalar factors.
	  S.multiplySchurLevel1 = (f1,f2) -> (
	       cS := coefficientRing S;
	       if liftable(f1,cS) or liftable(f2,cS) then f1 ** f2 else
	       if f1 == 0 or f2 == 0 then 0_S else
	       internalProduct(f1,f2)
	  );
	  S.highLevelCombine = (coeff, basisProd) -> coeff ** basisProd;
	  S.plethysmFcn = plethysmSn;
	  S.recTransOp = (a,b) -> a**b;
     ) else if opts.GroupActing == "Sp" then (
	  -- -- Sp: symplectic (Newell-Littlewood via stable helper)
	  --    Multiplication goes via convert-to-Schur, LR-multiply,
	  --    convert-back (== Newell-Littlewood rule).  We compute
	  --    the GL product in a STABLE Schur helper ring so that
	  --    partitions with more than numgens S rows survive to be
	  --    collapsed by the modification rule in schurToSpRE (the
	  --    finite-n Sp ring's own engine would otherwise truncate
	  --    them too early).
	  S.multiplySchurLevel1 = (f1,f2) -> (
	       H := stableSchurHelperOf S;
	       sf1 := spToSchurRE(f1, H);
	       sf2 := spToSchurRE(f2, H);
	       prod := new H from raw sf1 * raw sf2;
	       schurToSpRE(prod, S)
	  );
	  S.highLevelCombine = (coeff, basisProd) -> rawMult(promote(coeff,S), basisProd);
	  S.plethysmFcn = plethysmSp;
	  S.recTransOp = (a,b) -> a*b;
     ) else if opts.GroupActing == "O" then (
	  -- -- O: orthogonal (Newell-Littlewood via stable helper)
	  --    Same stable-helper trick as Sp, but with the orthogonal
	  --    conversion maps oToSchurRE / schurToORE.
	  S.multiplySchurLevel1 = (f1,f2) -> (
	       H := stableSchurHelperOf S;
	       sf1 := oToSchurRE(f1, H);
	       sf2 := oToSchurRE(f2, H);
	       prod := new H from raw sf1 * raw sf2;
	       schurToORE(prod, S)
	  );
	  S.highLevelCombine = (coeff, basisProd) -> rawMult(promote(coeff,S), basisProd);
	  S.plethysmFcn = plethysmO;
	  S.recTransOp = (a,b) -> a*b;
     ) else error("Unknown GroupActing: " | toString opts.GroupActing);

     ------------------------------------------------------------------
     -- (4) Monomial-basis override
     --     If the user selected the monomial-symmetric basis, we
     --     overwrite multiplySchurLevel1 post-hoc: convert both
     --     factors to the Schur basis, multiply via engine LR, and
     --     convert back.  All other dispatch callbacks are inherited
     --     from the GroupActing branch above.
     ------------------------------------------------------------------
     -- Override multiplication for Monomial basis
     if opts.Basis == "Monomial" then (
	  S.multiplySchurLevel1 = (f1,f2) -> (
	       -- Convert from monomial to Schur
	       sf1 := monomialToSchurRE(f1, S);
	       sf2 := monomialToSchurRE(f2, S);
	       -- Multiply via engine (Littlewood-Richardson)
	       prod := new S from raw sf1 * raw sf2;
	       -- Convert back to monomial
	       schurToMonomialRE(prod, S)
	  );
     );

     ------------------------------------------------------------------
     -- (5) Unified S * S dispatch
     --     At schurLevel 1 we call multiplySchurLevel1 directly.  At
     --     higher schurLevel we expand each factor via listForm into
     --     (partition, coefficient) pairs, multiply bases pairwise
     --     via multiplySchurLevel1, and stitch the coefficient
     --     product back in via highLevelCombine.
     ------------------------------------------------------------------
     -- Unified multiplication dispatch
     S * S := (f1,f2) ->
	  if schurLevel S == 1 then S.multiplySchurLevel1(f1,f2)
	  else (
	       lF1 := listForm f1;
	       lF2 := listForm f2;
	       sum flatten for p1 in lF1 list
		    for p2 in lF2 list
			 S.highLevelCombine(
			      (last p1) * (last p2),
			      S.multiplySchurLevel1(S_(first p1), S_(first p2))
			 )
	  );

     ------------------------------------------------------------------
     -- (6) IndexedVariableTable setup + S.use
     --     Wire up the subscript syntax S_lambda.  For SL rings we
     --     canonicalize the result (strip determinant columns).
     ------------------------------------------------------------------
     t := new SchurRingIndexedVariableTable from p;
     t.SchurRing = S;
     if opts.GroupActing == "SL" then
	  t#symbol _ = a -> slCanonicalize(S _ a, S)
     else
	  t#symbol _ = a -> ( S _ a);
     S.use = S -> (globalAssign(p,t); S);
     S.use S;
     S)

--constructs the Schur ring of a symmetric ring R
--this is a ring with basis consisting of s-polynomials (Schur functions) that
--is abstractly isomorphic to R
--schurRingOf = method()
--schurRingOf (Ring) := R -> (
schurRing (Ring) := opts -> R -> (
     	  if R.?Schur then R.Schur else
	  if schurLevel R > 0 then
	  (
	       if instance(R, SchurRing) then R else
	       (
		    s := R.SVariable;
     	       	    if schurLevel R == 1 then R.Schur = schurRing(coefficientRing R,s,R.dim,EHPVariables => R.EHPVariables, GroupActing => R.GroupActing, Basis => if R.?Basis then R.Basis else "Schur")
		       else R.Schur = schurRing(schurRing coefficientRing R,s,R.dim,EHPVariables => R.EHPVariables, GroupActing => R.GroupActing, Basis => if R.?Basis then R.Basis else "Schur"); --symmetricRing is wrong, right?
     	       	    R.Schur.symmetricRing = R;
	       	    R.Schur
		    )
	       )
	  else error"Expected ring to have a Schur Ring"
     )

schurRing(Thing,ZZ) := opts -> (s,n) -> schurRing(QQ,s,n,opts)
schurRing(Thing,InfiniteNumber) := opts -> (s,n) -> schurRing(QQ,s,n,opts)
schurRing(Thing) := opts -> (s) -> schurRing(QQ,s,-1,opts)

undocumented (schurRing,Ring,Symbol,InfiniteNumber)
undocumented (schurRing,Thing,InfiniteNumber)

--a new type that indexes the elements in the s-basis of a Schur ring
SchurRingIndexedVariableTable = new Type of IndexedVariableTable
SchurRingIndexedVariableTable _ Thing := (x,i) -> x#symbol _ i

--construction of symmetric rings
symmetricRing = method(Options => options schurRing)
symmetricRing (Ring,ZZ) := opts -> (A,n) -> (
	  -- ===================================================================
	  -- Build R = A[e_1..e_n, p_1..p_n, h_1..h_n]: a polynomial ring over A
	  -- with 3n generators in the fixed block order
	  --     R_0       .. R_{n-1}   = e_1..e_n    (elementary)
	  --     R_n       .. R_{2n-1}  = p_1..p_n    (power sum)
	  --     R_{2n}    .. R_{3n-1}  = h_1..h_n    (complete homogeneous)
	  -- deg(e_i) = deg(p_i) = deg(h_i) = i.
	  --
	  -- symRingForE is the SAME ring with the variable order [h | p | e];
	  -- under GRevLex this makes e smallest, so a GB killing
	  --     { h_i - H_i(e),  p_i - P_i(e) }
	  -- reduces any polynomial to e-only. symRingForP is analogous with
	  -- e and p swapped, making p smallest. For H-reduction we just reuse
	  -- R itself (default GRevLex places h last, so h is already smallest).
	  -- ===================================================================

	  -- ==== Construct R and register E/P/H variable accessors ====
     	  (e,h,p) := opts.EHPVariables;
     	  R := A[e_1..e_n,p_1..p_n,h_1..h_n,
	    Degrees => toList(1..n,1..n,1..n), MonomialSize => 8];
     	  R.EHPVariables = opts.EHPVariables;
	  R.SVariable = opts.SVariable;
	  -- Index arithmetic follows the block layout above:
	  --   e_i = R_(i-1),  p_i = R_(n+i-1),  h_i = R_(2n+i-1).
       	  R.eVariable = (i) -> if 1 <= i and i <= n then R_(i-1) else error"Invalid index";
       	  R.pVariable = (i) -> if 1 <= i and i <= n then R_(n+i-1) else error"Invalid index";
       	  R.hVariable = (i) -> if 1 <= i and i <= n then R_(2*n+i-1) else error"Invalid index";
     	  R.GroupActing = opts.GroupActing;
	  R.Basis = opts.Basis;
     	  R.dim = n;

	  -- ==== Propagate dispatch function for plethysm ====
	  if opts.GroupActing == "GL" or opts.GroupActing == "SL" then R.plethysmFcn = plethysmGL
	  else if opts.GroupActing == "Sn" then R.plethysmFcn = plethysmSn;

	  -- ==== Operator overloads on R ====
	  R ** R := (f1,f2) -> internalProduct(f1,f2); --internal product of symmetric functions
     	  R @ RingElement := RingElement @ R := (f1,f2) -> plethysm(f1,f2);
     	  symmetricPower(ZZ,R) := (n,r) -> plethysm({n},r);
     	  exteriorPower(ZZ,R) := opts -> (n,r) -> plethysm(splice{n:1},r);

	  -- ==== Degree sequence and block indices inside R ====
	  -- The degrees of e_i, p_i, h_i are all i, so (1,2,..,n) is repeated
	  -- three times across the concatenated variable list.
	  degSeq := toList(1..n);
	  -- eIdx / pIdx / hIdx are the R-indices of the e / p / h blocks.
     	  blocks := {toList(0..(n-1)),toList(n..(2*n-1)),toList(2*n..(3*n-1))};
	  eIdx := blocks#0;
	  pIdx := blocks#1;
	  hIdx := blocks#2;

	  -- ==== Fresh placeholder symbols for the reordered auxiliary rings ====
	  -- Note: `vrs := symbol vrs` is the idiomatic way to introduce a
	  -- fresh local symbol in M2; the LHS declaration also keeps the
	  -- package's symbol checker happy.
	  vrs := symbol vrs;
	  tempSym := vrs;
	  tempVarsE := apply(eIdx,i->tempSym_i);
	  tempVarsP := apply(pIdx,i->tempSym_i);
	  tempVarsH := apply(hIdx,i->tempSym_i);

	  -- ==== Auxiliary rings with reordered variables ====
	  -- They differ from R only in the order of the variables.
	  -- R itself is used by default for conversion to H-polynomials.

	  -- symRingForE: variable order [h | p | e].  Under GRevLex this makes
	  -- the e-block smallest, so grbE (defined below) reduces any
	  -- polynomial to its e-only representative.
          R.symRingForE = A[tempVarsH | tempVarsP | tempVarsE ,Degrees=>flatten toList(3:degSeq),MonomialOrder=>GRevLex, MonomialSize => 8];
     	  R.mapToE = map(R.symRingForE,R,apply(hIdx|pIdx|eIdx,i->(R.symRingForE)_i));
     	  R.mapFromE = map(R,R.symRingForE,apply(hIdx|pIdx|eIdx,i->R_i));

	  -- symRingForP: variable order [h | e | p].  Under GRevLex this makes
	  -- the p-block smallest, so grbP reduces any polynomial to its
	  -- p-only representative.
     	  R.symRingForP = A[tempVarsH | tempVarsE | tempVarsP,Degrees=>flatten toList(3:degSeq),MonomialOrder=>GRevLex, MonomialSize => 8];
     	  R.mapToP = map(R.symRingForP,R,apply(pIdx|hIdx|eIdx,i->(R.symRingForP)_i));
     	  R.mapFromP = map(R,R.symRingForP,apply(hIdx|eIdx|pIdx,i->R_i));

	  -- ==== Conversion tables between E-, H- and P- polynomials ====
     	  EtoP(n,R);
     	  PtoE(n,R);
     	  HtoE(n,R);
     	  EtoH(n,R);
     	  PtoH(n,R);
     	  HtoP(n,R);

	  -- ==== Groebner bases for conversion between E-, H- and P- polynomials ====
	  -- Each GB lists, for i = 1..n, relations of the form
	  --     p_i - (p_i expressed in the target basis)
	  --     h_i - (h_i expressed in the target basis)
	  -- so that reduction mod the GB kills the non-target blocks.
	  -- grbE / grbP live in symRingForE / symRingForP; grbH lives in R
	  -- itself (default GRevLex already makes h smallest).
     	  R.grbE = forceGB matrix(R.symRingForE, {flatten apply(splice{1..n},i->{R.mapToE(R_(n-1+i))-R.PtoETable#i,R.mapToE(R_(2*n-1+i))-R.HtoETable#i})});
     	  R.grbH = forceGB matrix(R, {flatten apply(splice{1..n},i->{R_(n-1+i)-R.PtoHTable#i,R_(-1+i)-R.EtoHTable#i})});
     	  R.grbP = forceGB matrix(R.symRingForP, {flatten apply(splice{1..n},i->{R.mapToP(R_(-1+i))-R.EtoPTable#i,R.mapToP(R_(2*n-1+i))-R.HtoPTable#i})});
     	  collectGarbage();

	  -- ==== Basis-projection maps: rewrite f using only E-, P- or H-vars ====
     	  R.mapSymToE = (f) -> R.mapFromE(R.mapToE(f)%R.grbE);
     	  R.mapSymToP = (f) -> R.mapFromP(R.mapToP(f)%R.grbP);
     	  R.mapSymToH = (f) -> f%R.grbH;

	  -- ==== Schur level: one more than that of the base ring ====
	  if (A.?schurLevel) then R.schurLevel = A.schurLevel + 1
     	  else R.schurLevel = 1;
     	  R)

--constructs the symmetric ring of a Schur ring 
--if the Schur ring has dimension n, its symmetric ring is the polynomial ring
--in the variables e_1,...,e_n,p_1,...,p_n,h_1,...,h_n, i.e. the other types of
--symmetric functions (besides the Schur functions) that the package implements
symmetricRing (Ring) := opts -> R -> (
     	  if R.?symmetricRing then R.symmetricRing else
	  if class R === SchurRing then
	  (
	       if numgens R === infinity then 
	          error"symmetric ring expects finite schurRings";
     	       if coefficientRing R === ZZ then
	       	  error"base ring has to be QQ";
     	       R.symmetricRing = symmetricRing(symmetricRing coefficientRing R,numgens R,EHPVariables => R.EHPVariables, SVariable => R.Symbol, GroupActing => R.GroupActing, Basis => R.Basis);
     	       R.symmetricRing.Schur = R;
	       R.symmetricRing
	       )
	  else R
     )

symmetricRing(ZZ) := opts -> n -> symmetricRing(QQ,n,opts)

---------------------------------------------------------------
--------------Jacobi-Trudi-------------------------------------
---------------------------------------------------------------
--
-- The Jacobi-Trudi identity expresses a Schur function as a
-- determinant in the complete-homogeneous (h) or elementary (e)
-- symmetric functions:
--
--     s_lambda      = det( h_{lambda_i - i + j} )        (H-variant)
--     s_lambda      = det( e_{lambda'_i - i + j} )       (E-variant,
--                                                        lambda' = conjugate)
--
-- `jacobiTrudi` builds the matrix and either calls det() directly
-- (Memoize => false) or expands cofactor-wise via the recursive
-- helper `jT` (Memoize => true), caching results on R.sFunction.
--
-- File-scope "channel" between jacobiTrudi and jT:
-- `jT` is a plain (non-method) recursive function and needs access to
-- the ambient ring, its dim, and the E/H flag on every recursive call.
-- Rather than thread these through every argument list, we publish them
-- as file-scope locals that `jacobiTrudi` sets just before calling `jT`.
-- These names are a deliberate protocol; do not rename them.
auxR = local auxR;      -- the ambient symmetricRing in use
auxn = local auxn;      -- R.dim (number of variables)
auxEH = local auxEH;    -- 0 for E-variant, 1 for H-variant
----

-- Shared cached symmetricRing workspace.  Multiple internal routines
-- (skewSchurExpansion, plethysm, kostkaNumber, ...) need a transient
-- symmetricRing(QQ, n) just to run jacobiTrudi / toS on a partition
-- before reading off the partition-coefficient list.  Constructing a
-- fresh symmetricRing each time costs ~40-90ms, which dominates small
-- calls; caching a single ring and growing it on demand makes repeat
-- calls essentially free.
--
-- Safety: callers only ever lift partition/coefficient data out of the
-- ring; they never hand a raw element back to the user or compare
-- elements from different calls for identity.  Growing the ring between
-- calls therefore can't invalidate earlier results.
workSymRing := null;
workSymRingSize := 0;

-- Ensure the cached symmetricRing has dim >= need.  Grow (not shrink)
-- in powers-of-two-ish increments to amortize allocation cost.
-- Uses *private* (local) symbols for the e/h/p/s variables so that
-- constructing (or growing) the cached ring does NOT rebind the
-- user's global e, h, p, s at the top level.  Rebinding those would
-- cause expressions like `e_4` or `h_3` entered after a call into
-- plethysm / skewSchurExpansion to evaluate into the wrong ring.
ensureWorkSymRing := (need) -> (
     if workSymRing === null or workSymRingSize < need then (
	  newSize := max(need, 8);
	  if workSymRingSize > 0 then
	       newSize = max(newSize, 2 * workSymRingSize);
	  kv := local kv;
	  ev := local ev;
	  hv := local hv;
	  pv := local pv;
	  workSymRing = symmetricRing(QQ, newSize,
	       SVariable => kv,
	       EHPVariables => (ev, hv, pv));
	  workSymRingSize = newSize;
	  );
     workSymRing
     )

-- Backward-compat alias for readers; same cache.
ensureSkewWorkRing := ensureWorkSymRing

jacobiTrudi = method(Options => {Memoize => true, EorH => "E"})
jacobiTrudi(BasicList,Ring) := opts -> (lambda,R) ->
(
     lam := new Partition from lambda;
     rez := local rez;
     local u;
     if opts.EorH == "H" then u = R.hVariable else (u = R.eVariable;lam = conjugate lam;);
     if opts.Memoize then
     (
	  if not R.?sFunction then R.sFunction = new MutableHashTable;
	  if opts.EorH == "E" then
	  (
     	       -----sFunction#0 records s-polynomials in terms of the e-variables
	       if not R.sFunction#?0 then R.sFunction#0 = new MutableHashTable;
	       auxEH = 0;
	       )
	  else
	  (
     	       -----sFunction#1 records s-polynomials in terms of the h-variables
	       if not R.sFunction#?1 then R.sFunction#1 = new MutableHashTable;
	       auxEH = 1;
	       );
     	  auxR = R;
     	  auxn = R.dim;
     	  rez = jT(lam);
	  )
     else
     (
     	  n := #lam;
     	  rez = det(map(R^n, n, (i,j) -> 
	       (
	       	    aux := lam#i-i+j;
	       	    if aux < 0 or aux>R.dim then 0_R
	       	    else if aux == 0 then 1_R else u aux)
	       ),
	  Strategy => Cofactor);
	  );
     rez
     )

-- Computes the Jacobi-Trudi determinant recursively, via cofactor
-- expansion along the last row of the Jacobi-Trudi matrix.
--
-- The underlying symmetricRing lays its 3n generators out as
--     indices  0 .. n-1      e_1 .. e_n     (elementary)
--     indices  n .. 2n-1     p_1 .. p_n     (power sums)
--     indices  2n .. 3n-1    h_1 .. h_n     (complete homogeneous)
-- so we reach e_k and h_k uniformly by offsetting into the generator
-- list.  With auxEH in {0,1}, the single expression
--
--     auxR_(2*auxEH*auxn - 1 + k)
--
-- selects e_k (auxEH = 0: offset -1, i.e. indices start at 0) or
-- h_k (auxEH = 1: offset 2n-1, i.e. indices start at 2n).  We bind
-- this as `basisOffset` for readability.
jT = (lambda) ->
(
     lambda = toList lambda;
     rez := local rez;
     if auxR.sFunction#auxEH#?lambda then rez = auxR.sFunction#auxEH#lambda
     else
     (
     basisOffset := 2*auxEH*auxn - 1;  -- see comment above
     k := #lambda;
     if k == 0 or lambda#0 == 0 then rez = 1_auxR else
     if k == 1 then rez = auxR_(basisOffset + lambda#0) else
     (
	  -- Cofactor expansion along the last row of the Jacobi-Trudi
	  -- matrix.  At step i we pull out the entry coming from the
	  -- (ll-1-i)-th part of lambda; `leftPart` is the prefix whose
	  -- indices have not yet been consumed, `rightPart` collects
	  -- the shifted tail contributed by the already-consumed
	  -- parts, and `sign` alternates the cofactor sign.
	  leftPart := drop(lambda,-1);
     	  rightPart := {};
	  rez = 0;
	  sign := 1;
	  for i from 0 to k-1 do
	  (
     	       if lambda#(k-1-i)+i <= auxn then --just added, won't work for h-polynomials
	       rez = rez + sign*auxR_(basisOffset + lambda#(k-1-i) + i)*jT(leftPart|rightPart);
	       sign = - sign;
	       leftPart = drop(leftPart,-1);
	       if lambda#(k-1-i) > 1 then
	       rightPart = {lambda#(k-1-i)-1} | rightPart;
	       );
	  );
     auxR.sFunction#auxEH#lambda = rez;
     );
     rez
     )
---------------------------------------------------------------
--------------End Jacobi-Trudi---------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
--------------Plethysm-----------------------------------------
---------------------------------------------------------------

--the cycle type of the k-th power of any permutation of cycle type cyc
powerCycleType := method()
powerCycleType(ZZ,List) := (k,cyc) ->
(
     rsort(flatten (for i in cyc list (g := gcd(i,k);splice{g:i//g})))
     )

-------------------------------------------------------------------------
-- plethysmMap(d, maxg, R)
-------------------------------------------------------------------------
-- The power-sum plethysm operator p_d acting on a symmetricRing R of
-- rank nS.  On the power-sum generators it is the substitution
--
--        p_i  |-->  p_{i*d}.
--
-- When i*d <= nS, p_{i*d} is already a generator of R.  When
-- i*d >  nS, the element p_{i*d} is not directly a variable; instead
-- its expansion in the elementary-symmetric variables lives in
-- R.PtoETable#(i*d), and R.mapFromE re-embeds that expansion into R.
--
-- The returned ring map has the layout expected by symmetricRing
-- generators, namely three blocks of length nS each:
--
--        [ e-slot (nS zeros) | p-slot (images) | h-slot (nS zeros) ].
--
-- Only the p-slot is populated (through index maxg); the e- and
-- h-generators are sent to 0 because callers only feed p-polynomials
-- through this map.
-------------------------------------------------------------------------
-- d is an integer
-- R is symmetricRing n
-- returns the plethysm map p_d : R --> R
--    which sends p_i to p_(i*d).
plethysmMap = (d,maxg,R) -> (
     nS   := R.dim;
     nSd  := nS // d;                            -- largest i with i*d <= nS
     fs   := splice{nS:0_R};                     -- e-block: nS zeros
     topf := min(maxg,nSd);
     -- p-block, part 1: i in 1..topf, image p_{i*d} is an actual variable
     fs = join(fs, apply(1..topf, j -> R.pVariable(d*j)));
     -- p-block, part 2: i in topf+1..maxg, image p_{i*d} is out of range,
     --                  pull it from the cached E-expansion and re-embed
     if maxg > nSd then
        fs = join(fs, apply(topf+1..maxg, j -> R.mapFromE R.PtoETable#(d*j)));
     -- pad the rest of the p-block (up to nS) and then the full h-block;
     -- total remaining length is 2*nS - maxg
     fs = join(fs, 2*nS-maxg:0_R);
     map(R,R,fs)
     )

-------------------------------------------------------------------------
-- plethysmGL(f, g)   --   exterior (GL) plethysm f o g
-------------------------------------------------------------------------
-- Computes the composition of Schur functors applied to
-- GL-representations.  Strategy:
--
--   1. Rewrite f in power-sum variables:  pf = f(p_1,...,p_{nf}).
--   2. Rewrite g in power-sum variables:  pg.
--   3. For each j, the power-sum plethysm p_j o g is obtained from pg
--      by the substitution p_i |-> p_{i*j}; this is exactly
--          (plethysmMap(j, maxg, SRg)) pg.
--   4. Then  f o g  =  pf( p_1 o g, p_2 o g, ..., p_{nf} o g ),
--      realized as a ring map  phi : SRf -> SRg  that sends
--          p_j  |-->  (plethysmMap(j, maxg, SRg)) pg,
--      and sends the e- and h-generators to 0 (pf is pure in p).
--   5. Convert the result back to the Schur basis when the ambient
--      ring of g is a SchurRing.
-------------------------------------------------------------------------
-- exterior plethysm (corresponding to composition
-- of Schur functors of GL-representations)
-- f is a polynomial in symmetricRing / SchurRing SA
-- g is a polynomial in symmetricRing / SchurRing SB
-- result is in symmetricRing / SchurRing SB
plethysmGL = method()
plethysmGL(RingElement,RingElement) := (f,g) -> (
     Rg := ring g;
     Rf := ring f;
     if schurLevel Rf > 1 then error"Undefined plethysm operation";

     issy := not instance(Rg,SchurRing);         -- true => stay in symmetricRing
     pg := toP g;
     pf := toP f;

     SRg := ring pg;                             -- symmetric ring of Rg
     SRf := ring pf;                             -- symmetric ring of Rf

     nf := SRf.dim;
     -- maxf: largest i such that p_i appears in pf
     maxf := max(support(pf)/index//max-nf+1,0);

     auxS := SRg;
     nS   := auxS.dim;
     lev  := schurLevel auxS;
     spg  := support(pg)/index;
     -- maxg: largest i such that p_i appears in pg
     maxg := max(select(spg,i->i<3*nS)//max-nS+1,0);
     -- ensure the E-expansion table reaches index maxf*maxg
     if maxf*maxg >= #auxS.PtoETable then PtoE(maxf*maxg,auxS);

     -- phi : SRf -> SRg sends p_i to the plethystic composition p_i o pg,
     -- so phi(pf) = pf o pg = f o g.
     -- The flattened argument has three blocks matching SRf's generators:
     --   e-block (nf zeros), p-block (plethysmMap images), h-block (nf zeros).
     phi := map(SRg, SRf, flatten splice {
               nf:0_SRg,                         -- e-block: nf zeros
               apply(1..nf, j ->                 -- p-block: p_j o pg for j<=maxf, else 0
                    (if j<=maxf then (plethysmMap(j,maxg,SRg))pg else 0_SRg)),
               nf:0_SRg                          -- h-block: nf zeros
               });
     pl := phi pf;
     if issy then pl else toS pl
)


-- interior plethysm (corresponding to the result of
-- the application of a Schur functor to an S_n-representation)
-- f is a polynomial in symmetricRing N / SchurRing SA
-- g is a polynomial in symmetricRing n / SchurRing SB
-- result is in symmetricRing n / SchurRing SB
plethysmSn = method()
plethysmSn(RingElement,RingElement) := (f,g) ->
(
     symmetricFunction(plethysm(f,classFunction g), ring g)
     )

-- Build / fetch a FINITE GL Schur ring large enough to hold the
-- Schur expansion of a plethysm f \circ g, using degrees of f,g.
-- plethysmGL passes through a symmetricRing, which requires finite numgens;
-- the stable (infinity) helper cannot be used here.
plethysmHelperOf = (S, nWanted) -> (
     key := (coefficientRing S, nWanted);
     if not S.?plethysmHelpers then S.plethysmHelpers = new MutableHashTable;
     if S.plethysmHelpers#?key then S.plethysmHelpers#key
     else (
	  sSym := getSymbol "splhlp";
	  T := schurRing(coefficientRing S, sSym, nWanted);
	  S.plethysmHelpers#key = T;
	  T
	  )
     )

-- Estimate a safe numgens for the finite Schur helper used during
-- plethysm: partitions in the result f \circ g have size deg(f)*deg(g)
-- and thus at most deg(f)*deg(g) rows.
plethysmHelperSize = (f,g) -> (
     lf := listForm f;
     lg := listForm g;
     degF := if #lf == 0 then 0
	     else max apply(lf, t -> sum toList first t);
     degG := if #lg == 0 then 0
	     else max apply(lg, t -> sum toList first t);
     max(degF * degG, 4)
     )

-- Plethysm for the symplectic character ring.
--   f \circ g  where g is a character of Sp(2n).
-- Strategy: convert g to the GL-Schur basis in a sufficiently large finite
-- helper ring H, apply GL plethysm in H, then fold the result back to the
-- Sp-basis via the Littlewood inverse (schurToSpRE), which handles
-- modification rules when the target Sp ring has finite rank.
plethysmSp = method()
plethysmSp(RingElement,RingElement) := (f,g) -> (
     S := ring g;
     Rf := ring f;
     nFin := plethysmHelperSize(f,g);
     H := plethysmHelperOf(S, nFin);
     gS := spToSchurRE(g, H);
     fS := if instance(Rf, SchurRing) and Rf.?GroupActing then (
	       if Rf.GroupActing == "Sp" then
		    spToSchurRE(f, plethysmHelperOf(Rf, nFin))
	       else if Rf.GroupActing == "O" then
		    oToSchurRE(f, plethysmHelperOf(Rf, nFin))
	       else f
	  ) else f;
     plS := plethysmGL(fS, gS);
     schurToSpRE(plS, S)
     )

-- Plethysm for the orthogonal character ring (analogous to Sp).
plethysmO = method()
plethysmO(RingElement,RingElement) := (f,g) -> (
     S := ring g;
     Rf := ring f;
     nFin := plethysmHelperSize(f,g);
     H := plethysmHelperOf(S, nFin);
     gS := oToSchurRE(g, H);
     fS := if instance(Rf, SchurRing) and Rf.?GroupActing then (
	       if Rf.GroupActing == "Sp" then
		    spToSchurRE(f, plethysmHelperOf(Rf, nFin))
	       else if Rf.GroupActing == "O" then
		    oToSchurRE(f, plethysmHelperOf(Rf, nFin))
	       else f
	  ) else f;
     plS := plethysmGL(fS, gS);
     schurToORE(plS, S)
     )

-- plethysm of symmetric functions
plethysm = method()

-- this function is not exported
-- it is used to compute the plethysm of f and g
-- when f is a power-sum symmetric polynomial
auxplet = method()
auxplet(RingElement,RingElement) := (f,g) ->
(
     Rg := ring g;
     pl := local pl;
     if Rg.?plethysmFcn then pl = Rg.plethysmFcn
     else if Rg.GroupActing == "GL" then pl = plethysmGL
     else if Rg.GroupActing == "Sn" then pl = plethysmSn;
     sLg := schurLevel Rg;
     
     if sLg == 1 then return pl(f,g) else
     (
     	  lF := listForm g;
	  return sum for t in lF list auxplet(f,last t) * pl(f,Rg_(first t))
	  );
     )

-- the most general form of plethysm
-- f is an arbitrary symmetric functions
-- g is an element of a representation ring of a product of general linear and/or symmetric groups
plethysm(RingElement,RingElement) := (f,g) ->
(
     pf := toP f;
     Rf := ring pf;
     if schurLevel Rf > 1 then error"Undefined plethysm operation";
     
     pls := new MutableHashTable from {};
     lpf := listForm pf;
     m := (ring pf).dim;
     isSchur := instance(ring g,SchurRing);

     auxg := local auxg;
     if isSchur then auxg = g else auxg = toS g;

     pl := sum for t in lpf list ((last t) * product select(apply(splice{0..m-1}, i -> (ex := (first t)#(m+i);
     	       if ex > 0 then (if pls#?i then (pls#i)^ex else 
	        (pls#i = auxplet(Rf.pVariable(i+1),auxg);(pls#i)^ex)))),j -> j =!= null)); -- this is bad when g is not in a SchurRing

     if isSchur then pl else toSymm pl
     )

-- plethysm of s_lambda and g
plethysm(BasicList,RingElement) := (lambda,g) -> (
     d := sum toList lambda;
     -- Reuse the cached workSymRing instead of constructing a fresh
     -- symmetricRing(QQ,d) per call.  jacobiTrudi only needs dim >= d
     -- (a larger dim is harmless; extra e/h/p variables simply never
     -- appear in the monomials it produces).  See ensureWorkSymRing.
     Rf := ensureWorkSymRing max(d, 1);
     f := jacobiTrudi(lambda,Rf);
     plethysm(f,g)
     )

-- (inner) plethysm of symmetric function f with the class function cF (the character of a certain S_n-representation)
plethysm(RingElement,ClassFunction) := (f,cF) ->
(
     R := ring(cF#(first keys cF));
     if R === ZZ then R = QQ;

     pf := toP f;
     n := degree cF;
     k := (ring pf).dim;
     pvars := (ring pf).pVariable;
     parsn := toList \ partitions(n);  
     newHT := new MutableHashTable;
     for sig in parsn do
     (
	  sublist := for i from 1 to k list
	  (
	       pct := powerCycleType(i,sig);     
	       if cF#?pct then cF#pct else 0
	       );
	  newHT#sig = (map(R,ring pf,splice{k:0} | sublist | splice{k:0})) pf;
	  );
     new ClassFunction from newHT
     )

-- (inner) plethysm of s_lambda with the class function cF (the character of a certain S_n-representation)
plethysm(BasicList,ClassFunction) := (lambda,cF) -> (
     d := sum toList lambda;
     -- Reuse the cached workSymRing; see plethysm(BasicList,RingElement).
     Rf := ensureWorkSymRing max(d, 1);
     f := jacobiTrudi(lambda,Rf);
     plethysm(f,cF))

-*
-- degree of a polynomial in a SchurRing
-- this is no longer used
degSchurPol = method()
degSchurPol(RingElement) := ps -> (
     tms := listForm ps;
     tms/first/sum//max
     )
*-
---------------------------------------------------------------
-----------End plethysm----------------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
----Transition between various types of symmetric functions----
---------------------------------------------------------------

-----------------------------------------------------------------
-- Classical basis conversions
-----------------------------------------------------------------
-- At schurLevel 1, symmetricRing(QQ, n) is a free polynomial ring
-- on 3n generators, partitioned as:
--     e_1, ..., e_n  (elementary)    indices   0 .. n-1
--     p_1, ..., p_n  (power-sum)     indices   n .. 2n-1
--     h_1, ..., h_n  (complete)      indices 2n .. 3n-1
-- The ring carries Groebner bases grbE, grbH, grbP (installed by
-- symmetricRing(Ring,ZZ)) which kill all but one of the three
-- families of generators.  The stored maps R.mapSymToE,
-- R.mapSymToH, R.mapSymToP reduce an arbitrary symmetric polynomial
-- against the relevant GB, rewriting it in one family of generators.
--
-- Conversion strategy:
--     toSymm : SchurRing -> symmetricRing    (via jacobiTrudi)
--     toE / toH / toP                        (via GB reduction,
--                                             recursing on schurLevel
--                                             so higher-level
--                                             coefficients are
--                                             rewritten first).
-----------------------------------------------------------------

-- toSymm
toSymm = method()

-- if ps is an element of a schurRing R
-- toSymm returns the symmetric function corresponding to ps, as an element of a symmetricRing, the symmetricRing R;
-- otherwise ps is returned;
toSymm(RingElement) := (ps) ->
(
     S := ring ps;
     if instance(S, SchurRing) then
     (
     -- Special case: RatGL rings have no associated symmetricRing (they
     -- model rational, not polynomial, representations).  An element is
     -- a polynomial character iff every bipartition has empty beta, in
     -- which case we can map it to the symmetricRing via jacobiTrudi on
     -- alpha.  Otherwise error cleanly.
     if S.?GroupActing and S.GroupActing == "RatGL" then (
	  -- Walk the RatGL terms: each is indexed by a bipartition
	  -- (alpha, beta).  #beta > 0  <=>  non-polynomial (dual) part
	  -- is present  =>  set hasNeg and bail out afterwards, since
	  -- there is no canonical map to a symmetricRing in that case.
	  -- For a stable RatGL ring numgens S === infinity, so we size
	  -- the target symmetricRing to the largest alpha that actually
	  -- appears.
	  polyTerms := new MutableList;
	  hasNeg := false;
	  iterateRatGLTerms(ps, (alpha, beta, scalar) -> (
		    -- #beta > 0  <=>  non-polynomial character  ->  hasNeg
		    if #beta > 0 then hasNeg = true
		    else polyTerms#(#polyTerms) = (alpha, scalar);
		    ));
	  if hasNeg then error("toSymm: RatGL element has nonzero negative "
	       | "(beta) components and is not a polynomial character; no "
	       | "canonical map to a symmetricRing exists.  Use "
	       | "specialize(f, n) to evaluate at GL(n) instead.");
	  if #polyTerms == 0 then return 0;
	  -- maximum #alpha determines the minimum symmetric-ring dim.
	  maxLen := max apply(toList polyTerms, t -> #(t#0));
	  nT := numgens S;
	  targetDim := if class nT === InfiniteNumber
	       then max(maxLen, 1) else nT;
	  Rsym := symmetricRing(coefficientRing (S.ratNegRing), targetDim);
	  return sum apply(toList polyTerms, (alpha, c) ->
	       c * jacobiTrudi(alpha, Rsym));
	  );
     R := symmetricRing S;
     tms := listForm ps;
     -- Each (p, a) in tms represents the Schur monomial a * s_p.
     -- jacobiTrudi(p, R) realises s_p as a polynomial in R.
     -- The 'try ... else error ...' catches the case where p has
     -- more rows than R.dim (jacobiTrudi fails): the user needs a
     -- symmetricRing of strictly larger dimension.
     -- The coefficient a is lifted to coefficientRing S and fed
     -- back through toSymm, handling higher-schurLevel coefficients.
     sum apply(tms,(p,a)->(
	       (try b:=jacobiTrudi(p,R) then b else error"Need symmetric ring of higher dimension")*
	       toSymm(lift(a,coefficientRing S))))
     )
     else return ps
)

-- this is the base case of the recursive operation in the general case
-- needed when ps is an element of ZZ or QQ, because ZZ, QQ don't have
-- RingElement as an ancestor
toSymm(Number) := (ps) -> ps

mapSymToE = method()
-- writes the symmetric functions of maximal schurLevel in f (i.e. those
-- not contained in the coefficient ring of R) in terms of the e-polynomials
mapSymToE (RingElement) := (f) -> (
     R:=ring f; 
     if R.?mapSymToE then R.mapSymToE f else f
)
mapSymToH = method()
-- writes the symmetric functions of maximal schurLevel in f (i.e. those
-- not contained in the coefficient ring of R) in terms of the h-polynomials
mapSymToH (RingElement) := (f) -> (
     R:=ring f; 
     if R.?mapSymToH then R.mapSymToH f else f
)
mapSymToP = method()
-- writes the symmetric functions of maximal schurLevel in f (i.e. those
-- not contained in the coefficient ring of R) in terms of the p-polynomials
mapSymToP (RingElement) := (f) -> (
     R:=ring f; 
     if R.?mapSymToP then R.mapSymToP f else f
)

-- Guard used by toE/toH/toP/toSymm: refuse early (with a helpful
-- message) when a SchurRing input is rank-infinite, because the
-- associated symmetricRing cannot be constructed for stable rings.
-- The underlying engine error ("symmetric ring expects finite
-- schurRings") is internal and not actionable; this guard tells the
-- user what to do instead.
stableRingConversionGuard := (R, opname) -> (
     if class R === SchurRing then (
	  n := numgens R;
	  if class n === InfiniteNumber then
	       error(opname | ": cannot convert an element of a stable "
		    | "(rank-infinite) Schur ring to an e/h/p-basis "
		    | "symmetric function.  Specialize the element to a "
		    | "finite rank first (e.g. via `specialize(f, n)`), "
		    | "or create a finite-rank SchurRing to begin with.");
	  );
     )

toE = method()
-- writes a symmetric function (possibly in a ring
-- with schurLevel larger than one) in terms of
-- elementary symmetric polynomials
toE (RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing then (
	  stableRingConversionGuard(R, "toE");
	  toE toSymm f
	  )
     else
     (
	  if not R.?schurLevel then f else
	  -- schurLevel > 1: split each term t into leadCoefficient (which
	  -- lives in the coefficient ring, one schurLevel down) and
	  -- leadMonomial (a pure level-1 symmetric monomial).  Recurse
	  -- into toE on the coefficient (driving the recursion on
	  -- schurLevel), and apply mapSymToE to the monomial (a GB
	  -- reduction at level 1).  Sum the pieces back up.
	  if R.schurLevel>1 then terms f/(i->(toE leadCoefficient i*(mapSymToE leadMonomial i)))//sum
	  else mapSymToE f
	  )
     )

toP = method()
-- writes a symmetric function (possibly in a ring
-- with schurLevel larger than one) in terms of
-- power sums
toP (RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing then (
	  stableRingConversionGuard(R, "toP");
	  toP toSymm f
	  )
     else
     (
	  if not R.?schurLevel then f else
	  -- Same pattern as toE: leadCoefficient descends one schurLevel
	  -- (recursive toP), leadMonomial is a level-1 symmetric monomial
	  -- reduced to the power-sum family via mapSymToP.
	  if R.schurLevel>1 then terms f/(i->(toP leadCoefficient i*(mapSymToP leadMonomial i)))//sum
	  else mapSymToP f
	  )
     )

toH = method()
-- writes a symmetric function (possibly in a ring
-- with schurLevel larger than one) in terms of
-- complete symmetric polynomials
toH (RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing then (
	  stableRingConversionGuard(R, "toH");
	  toH toSymm f
	  )
     else
     (
	  if not R.?schurLevel then f else
	  -- Same pattern as toE: leadCoefficient recurses one schurLevel
	  -- down via toH, leadMonomial is reduced at level 1 via
	  -- mapSymToH (GB reduction to the complete-homogeneous family).
	  if R.schurLevel>1 then terms f/(i->(toH leadCoefficient i*(mapSymToH leadMonomial i)))//sum
	  else mapSymToH f
	  )
     )

---------------------------------------------------------------
--------------Monomial basis (toM)-----------------------------
---------------------------------------------------------------

-- Kostka number K_{lambda,mu}: number of SSYT of shape lambda, content mu.
--
-- We compute this via the direct SSYT recursion, which is far faster
-- (and uses less memory) than expanding h_mu into the Schur basis and
-- reading off a coefficient.  The recursion, following Stanley (EC2,
-- Prop. 7.10.4 and the horizontal-strip interpretation of h_mu):
--
--   K_{lambda, mu} = sum_{nu} K_{nu, mu'}
--
-- where mu' drops the last (smallest) part mu_r of mu, and nu ranges
-- over partitions obtained from lambda by removing a horizontal strip
-- of size mu_r.  Equivalently, the last symbol "r" is placed in a
-- horizontal strip of size mu_r in the shape lambda, and the rest of
-- the tableau fills the smaller shape nu = lambda \ strip with content
-- mu'.  Base case: K_{{}, {}} = 1.
--
-- Complexity per call: O(|lambda|^{ell(mu)} * ell(mu)) in the worst
-- case, but memoization on (lambda, mu) makes repeat calls O(1) and
-- makes the full Kostka matrix of degree d an O(p(d)^2) computation.
--
-- The memo table kostkaMemo is process-local and keyed on normalized
-- (stripped) partitions, so repeated calls with equivalent inputs hit
-- the cache.

kostkaMemo := new MutableHashTable

-- Enumerate sub-partitions nu of lambda obtained by removing a
-- horizontal strip of size s.  A horizontal strip is a skew shape with
-- at most one box in each column, i.e. the consecutive row differences
-- satisfy lambda_i >= nu_i >= lambda_{i+1} (row i of nu fits between
-- row i and row i+1 of lambda).
--
-- Returns a list of partitions nu (with trailing zeros stripped).
horizontalStripComplements = (lambda, s) -> (
     lam := toList lambda;
     l := #lam;
     if s == 0 then return {lam};
     if s < 0 or s > sum lam then return {};
     -- aux(i, remaining, prevLamBelow) = choose nu_0, nu_1, ..., nu_{i}
     -- given that nu_i <= lam_i and nu_i >= lam_{i+1} (for horizontal
     -- strip: row i of nu must lie between lam_i and lam_{i+1}).
     aux := (idx, remaining) -> (
	  if idx == l then (
	       if remaining == 0 then return {{}} else return {};
	       );
	  lamI  := lam#idx;
	  lowerI := if idx + 1 < l then lam#(idx+1) else 0;
	  -- nu_idx in [lowerI, lamI]; must also not remove more than
	  -- `remaining` boxes across this and all later rows.
	  -- amount removed from this row is lamI - nu_idx
	  -- so nu_idx ranges: max(lowerI, lamI - remaining) ... lamI
	  lo := max(lowerI, lamI - remaining);
	  flatten for v from lo to lamI list
	       for tail in aux(idx+1, remaining - (lamI - v)) list prepend(v, tail)
	  );
     -- Strip trailing zeros from each candidate.
     apply(aux(0, s), stripTrailingZeros)
     )

kostkaNumber = method()
kostkaNumber(BasicList,BasicList) := (lambda,mu) -> (
     lam := stripTrailingZeros toList lambda;
     m   := stripTrailingZeros toList mu;
     if sum lam != sum m then return 0;
     if sum lam == 0 then return 1;
     -- Memoize on normalized (lam, m).
     key := (lam, m);
     if kostkaMemo#?key then return kostkaMemo#key;
     -- Peel off the last (smallest) part of m.
     mLast   := m#(#m - 1);
     mRest   := drop(m, -1);
     -- Sum over all sub-partitions nu of lam obtained by removing a
     -- horizontal strip of size mLast.
     total := sum for nu in horizontalStripComplements(lam, mLast) list
	  kostkaNumber(nu, mRest);
     kostkaMemo#key = total;
     total
     )

-- toM: expand a symmetric function in the monomial basis.
-- Returns a RingElement in a monomial-basis SchurRing (Basis => "Monomial").
-- With no target supplied, an associated monomial ring is cached on the
-- input ring (lazy construction) and used as the output ring.
toM = method()

toM(RingElement) := (f) -> (
     R := ring f;
     if class R =!= SchurRing then return toM(toS f);
     n := numgens R;
     if n === infinity then error "toM requires a SchurRing with finite number of generators";
     -- If R is itself a monomial-basis ring, f is already in monomial form.
     if R.?Basis and R.Basis == "Monomial" then return f;
     M := monomialBasisRingOf R;
     toM(f, M)
     )

toM(RingElement,SchurRing) := (f, M) -> (
     if not (M.?Basis and M.Basis == "Monomial") then
	  error "expected second argument to be a SchurRing with Basis => \"Monomial\"";
     R := ring f;
     if class R =!= SchurRing then return toM(toS f, M);
     dimM := numgens M;
     if dimM === infinity then error "toM requires a target SchurRing with finite number of generators";
     -- Case 1: input is already in monomial basis -- identity map on partition labels
     if R.?Basis and R.Basis == "Monomial" then (
	  rawRes := raw(0_M);
	  for term in listForm f do (
	       if #(term#0) <= dimM then (
		    sc := raw promote(term#1, M);
		    ba := raw M_(term#0);
		    rawRes = rawRes + sc * ba;
		    );
	       );
	  new M from rawRes
	  )
     -- Case 2: input is in Schur basis -- apply Kostka conversion
     else schurToMonomialRE(f, M)
     )

-- Compute Kostka matrix and its inverse for degree d in n variables
-- Returns (K, Kinv) where K#lambda#{mu} = K_{lambda,mu}, Kinv#mu#{lambda} = (K^{-1})_{mu,lambda}
computeKostkaMatrices = (d, n) -> (
     parts := select(partitions d, p -> #(toList p) <= n);
     partsL := apply(parts, p -> toList p);
     k := #partsL;
     if k == 0 then return (new HashTable, new HashTable);
     -- Build Kostka matrix using toS(h_mu); reuse the cached workSymRing.
     auxR := ensureWorkSymRing max(n, 1);
     KMat := mutableMatrix(QQ, k, k);
     for j from 0 to k-1 do (
	  mu := partsL#j;
	  hProd := product for i from 0 to #mu - 1 list auxR.hVariable(mu#i);
	  sExp := listForm toS hProd;
	  lookup := new HashTable from apply(sExp, t -> t#0 => t#1);
	  for i from 0 to k-1 do (
	       lam := partsL#i;
	       if lookup#?lam then KMat_(i,j) = promote(lookup#lam, QQ);
	       );
	  );
     -- Invert
     KMatFinal := matrix KMat;
     KInvMat := KMatFinal^(-1);
     -- Build hash tables
     K := new MutableHashTable;
     Kinv := new MutableHashTable;
     for i from 0 to k-1 do (
	  lam := partsL#i;
	  row := new MutableHashTable;
	  for j from 0 to k-1 do (
	       val := KMat_(i,j);
	       if val != 0 then row#(partsL#j) = lift(val, ZZ);
	       );
	  K#lam = new HashTable from row;
	  );
     for j from 0 to k-1 do (
	  mu := partsL#j;
	  row := new MutableHashTable;
	  for i from 0 to k-1 do (
	       val := KInvMat_(j,i);
	       if val != 0 then row#(partsL#i) = lift(val, ZZ);
	       );
	  Kinv#mu = new HashTable from row;
	  );
     (new HashTable from K, new HashTable from Kinv)
     )

-- Convert element from monomial-basis interpretation to Schur-basis interpretation
-- f in SchurRing S where S_(mu) represents m_mu; returns element where S_(lambda) represents s_lambda
-- Uses raw engine operations to avoid triggering overloaded S*S multiplication
monomialToSchurRE = (f, S) -> (
     if f == 0 then return 0_S;
     n := numgens S;
     lf := listForm f;
     rawRes := raw(0_S);
     for term in lf do (
	  mu := term#0;
	  c := term#1;
	  d := sum mu;
	  if d == 0 then rawRes = rawRes + raw promote(c, S)
	  else (
	       if not S.kostkaCache#?d then S.kostkaCache#d = computeKostkaMatrices(d, n);
	       Kinv := (S.kostkaCache#d)#1;
	       muL := toList mu;
	       if Kinv#?muL then
		    for pair in pairs Kinv#muL do (
			 sc := raw promote(c * (pair#1), S);
			 ba := raw S_(pair#0);
			 rawRes = rawRes + sc * ba;
			 );
	       );
	  );
     new S from rawRes
     )

-- Convert element from Schur-basis interpretation to monomial-basis interpretation
-- f in SchurRing S where S_(lambda) represents s_lambda; returns element where S_(mu) represents m_mu
-- Uses raw engine operations to avoid triggering overloaded S*S multiplication
schurToMonomialRE = (f, S) -> (
     if f == 0 then return 0_S;
     n := numgens S;
     lf := listForm f;
     rawRes := raw(0_S);
     for term in lf do (
	  lam := term#0;
	  c := term#1;
	  d := sum lam;
	  if d == 0 then rawRes = rawRes + raw promote(c, S)
	  else (
	       if not S.kostkaCache#?d then S.kostkaCache#d = computeKostkaMatrices(d, n);
	       K := (S.kostkaCache#d)#0;
	       lamL := toList lam;
	       if K#?lamL then
		    for pair in pairs K#lamL do (
			 sc := raw promote(c * (pair#1), S);
			 ba := raw S_(pair#0);
			 rawRes = rawRes + sc * ba;
			 );
	       );
	  );
     new S from rawRes
     )

-- Get or create the default monomial-basis SchurRing associated to S.
-- Used by toM to produce a RingElement output when no target ring is supplied.
monomialBasisRingOf = (S) -> (
     if S.?monomialBasisRing then S.monomialBasisRing
     else (
	  n := numgens S;
	  if n === infinity then error "monomialBasisRingOf requires a SchurRing with finite number of generators";
	  -- use a package-private symbol named "m" to avoid clobbering user globals
	  mSym := getSymbol "m";
	  M := schurRing(coefficientRing S, mSym, n, Basis => "Monomial");
	  S.monomialBasisRing = M;
	  M
	  )
     )

---------------------------------------------------------------
--------------End Monomial basis-------------------------------
---------------------------------------------------------------

---------------------------------------------------------------
--------------Sp / O stable character rings--------------------
---------------------------------------------------------------

-- Koike branching formulas for the stable (universal) character ring:
--    sp_lambda = sum_{delta}  (-1)^{|delta|/2} s_{lambda/delta}
--       where delta runs over partitions contained in lambda with all
--       columns of even length (equivalently, the conjugate delta' has
--       all even parts -- i.e. delta's parts come in equal pairs).
--    o_lambda  = sum_{delta}  (-1)^{|delta|/2} s_{lambda/delta}
--       where delta runs over partitions contained in lambda with all
--       parts even.
-- The inverse formulas are obtained by dropping the sign:
--    s_lambda  = sum_{delta even cols}  sp_{lambda/delta}  (skew Sp character)
--    s_lambda  = sum_{delta even rows}  o_{lambda/delta}   (skew O character)
-- In both directions, skew characters expand as sum_nu c^{lambda}_{delta,nu} X_nu
-- where c is the usual Littlewood-Richardson coefficient and X is the
-- relevant basis; this matches the s-expansion of s_{lambda/delta}.

-- Skew Schur expansion: returns s_{lambda/mu} as a list of (partition, coeff)
-- pairs in the Schur basis.  Uses the Jacobi-Trudi determinant.
--
-- Performance notes.  Two caches make this fast on repeated calls:
--   (1) A single symmetricRing is reused across calls (skewWorkRing),
--       grown on demand when a caller needs h_k with k exceeding the
--       current ring's dim.  This avoids the ~40-90ms symmetricRing
--       construction overhead that would otherwise dominate each call.
--   (2) Results are memoized on the normalized (lambda, mu) key in
--       skewMemo so repeated skew queries (common in Koike product and
--       Sp/O modification rules) cost O(1) after the first.
-- Both caches are process-global and safe because:
--   - skewWorkRing is used only inside this function; growing it never
--     breaks a previous computation (earlier results have already been
--     lifted out of the ring as partition/coefficient pairs).
--   - skewMemo stores only immutable (partition, integer) pairs.
-- Memo table for skewSchurExpansion results.  The cached workSymRing
-- used below is declared earlier in the file (near jacobiTrudi) so that
-- plethysm can share it.
skewMemo := new MutableHashTable;

skewSchurExpansion = method()
skewSchurExpansion(BasicList, BasicList) := (lambda, mu) -> (
     -- Strip trailing zeros (normalize so cache keys collide)
     lam := stripTrailingZeros toList lambda;
     m := stripTrailingZeros toList mu;
     if sum m > sum lam then return {};
     l := #lam;
     if l == 0 then return {({}, 1)};
     mPad := m | toList(l - #m : 0);
     for i from 0 to l-1 do if lam#i < mPad#i then return {};
     if sum lam == sum mPad then return {({}, 1)};
     -- Memo lookup on normalized key (stripped lam, stripped m)
     key := (lam, m);
     if skewMemo#?key then return skewMemo#key;
     -- Jacobi-Trudi: det(h_{lam_i - mPad_j - i + j}) for 1 <= i,j <= l
     nmax := lam#0 + l;
     R := ensureSkewWorkRing nmax;
     M := mutableMatrix(R, l, l);
     for i from 0 to l-1 do for j from 0 to l-1 do (
	  k := lam#i - mPad#j - i + j;
	  if k < 0 then M_(i,j) = 0_R
	  else if k == 0 then M_(i,j) = 1_R
	  else M_(i,j) = R.hVariable(k);
	  );
     dd := det(matrix M, Strategy => Cofactor);
     sExp := toS dd;
     -- listForm of a SchurRingElement is list of (partition, coeff) pairs
     skewMemo#key = apply(listForm sExp, t -> (t#0, lift(t#1, ZZ)))
     )

-- Enumerate partitions mu, contained in lambda componentwise, with all parts even.
-- Each mu is returned as a list of positive parts (trailing zeros stripped).
evenRowsSubpartitionsOf = method()
evenRowsSubpartitionsOf(BasicList) := (lambda) -> (
     lam := stripTrailingZeros toList lambda;
     aux := (idx, prevMax) -> (
	  if idx >= #lam then return {{}};
	  upper := min(lam#idx, prevMax);
	  flatten for v from 0 to upper list (
	       if odd v then continue;
	       for t in aux(idx+1, v) list prepend(v, t)
	       )
	  );
     -- Strip trailing zeros off each candidate
     apply(aux(0, infinity), stripTrailingZeros)
     )

-- Enumerate partitions mu, contained in lambda componentwise, with all columns
-- of even length (equivalently, conjugate mu' has all even parts -- parts of mu
-- come in equal pairs).
evenColsSubpartitionsOf = method()
evenColsSubpartitionsOf(BasicList) := (lambda) -> (
     lam := stripTrailingZeros toList lambda;
     if #lam == 0 then return {{}};
     lamC := toList conjugate new Partition from lam;
     evens := evenRowsSubpartitionsOf(lamC);
     apply(evens, mu -> (
	       if #mu == 0 then {}
	       else toList conjugate new Partition from mu
	       ))
     )

---------------------------------------------------------------
-- Sam-Snowden-Weyman modification rules (border-strip form)
---------------------------------------------------------------
--
-- References:
--   [SSW] Sam-Snowden-Weyman, "Homology of Littlewood complexes"
--         Selecta Math. 19 (2013), 655-698; arXiv:1209.3509
--         Sec. 3.4 (symplectic), Sec. 4.4 (orthogonal).
--   [Kin] King, "Modification rules ... for Bn, Cn, Dn", J. Algebra 107 (1987).
--
-- Given a (possibly invalid) partition lambda, these rules return a pair
--     (tau, sign)
-- such that, formally,
--     {Sp|O}_lambda  ==  sign * {Sp|O}_tau
-- in the corresponding character ring at rank n, OR null if lambda maps to 0.
-- tau is an admissible partition (length <= n for type C; additional first-
-- two-columns condition for types B, D).
--
-- The border-strip description (SSW 3.4 / 4.4) finds a border strip R of a
-- prescribed size starting at the first box of the final row of the current
-- partition, and removes it.  By Remark 3.4 in [SSW], such a border strip R
-- corresponds exactly to a cell b = (k, 1) in the first column whose hook
-- length equals the prescribed size L, i.e. lambda_k + r - k = L.  Removing
-- R then equals removing the hook of (k, 1), and the result is the
-- partition
--     (lambda_1, ..., lambda_{k-1}, lambda_{k+1}-1, ..., lambda_r-1).
-- The number of columns the strip occupies is c(R) = lambda_k.
--
-- findBorderStripRow(lam, L):
--   Given partition lam (with r = #lam rows) and target strip length L,
--   find the unique k in {1,...,r} with lam_k + r - k = L, or null if none.
findBorderStripRow = (lam, L) -> (
     r := #lam;
     k := -1;
     for kk from 1 to r do (
	  if lam#(kk-1) + r - kk == L then (k = kk; break);
	  );
     if k == -1 then null else k
     )

-- removeBorderStripAtFirstColumn(lam, k):
--   Remove the hook of cell (k, 1) from partition lam.
--   Returns (newLam, cR) where cR = # columns occupied by the strip.
removeBorderStripAtFirstColumn = (lam, k) -> (
     r := #lam;
     cR := lam#(k-1);  -- c(R) = lam_k (# columns occupied by the strip)
     newLam := stripTrailingZeros join(
	  take(lam, k-1),
	  for i from k+1 to r list lam#(i-1) - 1
	  );
     (newLam, cR)
     )

-- sigmaInvolution(lam, m):
--   The type B/D involution on partitions: replace lam_1^T (first column
--   length) by (m - lam_1^T) and keep lam_i^T for i >= 2, then transpose back.
--   Returns null if the result is not a valid partition (i.e. if
--   m - lam_1^T < lam_2^T).
sigmaInvolution = (lam, m) -> (
     if #lam == 0 then (
	  if m == 0 then return {}
	  else return toList(m:1)   -- empty^sigma = column of length m
	  );
     lamT := toList conjugate new Partition from lam;
     a := lamT#0;
     newA := m - a;
     if #lamT >= 2 and newA < lamT#1 then return null;
     if newA < 0 then return null;
     if newA == 0 then (
	  trunc := drop(lamT, 1);
	  if #trunc == 0 then return {};
	  return toList conjugate new Partition from trunc;
	  );
     newLamT := prepend(newA, drop(lamT, 1));
     toList conjugate new Partition from newLamT
     )

-- sswTypeC(lambda, n):
--   SSW modification rule for Sp(2n).
--   Returns (tau, sign) or null (meaning zero).
sswTypeC = (lambda, n) -> (
     lam := stripTrailingZeros toList lambda;
     if #lam <= n then return (lam, 1);
     sign := 1;
     current := lam;
     while #current > n do (
	  r := #current;
	  L := 2*(r - n - 1);
	  if L == 0 then return null;  -- strip must be non-empty
	  k := findBorderStripRow(current, L);
	  if k === null then return null;
	  (newLam, cR) := removeBorderStripAtFirstColumn(current, k);
	  sign = sign * (if even cR then 1 else -1);
	  current = newLam;
	  );
     (current, sign)
     )

-- sswTypeBD(lambda, m):
--   SSW modification rule for O(m) (type B if m = 2n+1, type D if m = 2n).
--   Returns (tau, sign) or null.  Here tau must satisfy the O-admissibility
--   condition lam^T_1 + lam^T_2 <= m; the iteration stops whenever this
--   holds.  Differences from type C (see [SSW 4.4]):
--     (D1) strip length L = 2 * ell(lambda) - m,
--     (D2) sign contribution uses c(R) - 1 instead of c(R),
--     (D3) if an odd total number of strips was removed, apply sigma to tau.
sswTypeBD = (lambda, m) -> (
     lam := stripTrailingZeros toList lambda;
     -- Admissibility test: lam^T_1 + lam^T_2 <= m.
     admissible := (p) -> (
	  if #p == 0 then return true;
	  pT := toList conjugate new Partition from p;
	  a := if #pT >= 1 then pT#0 else 0;
	  b := if #pT >= 2 then pT#1 else 0;
	  a + b <= m
	  );
     if admissible lam then return (lam, 1);
     sign := 1;
     numStripsRemoved := 0;
     current := lam;
     while not admissible current do (
	  r := #current;
	  L := 2 * r - m;
	  if L <= 0 then return null;
	  k := findBorderStripRow(current, L);
	  if k === null then return null;
	  (newLam, cR) := removeBorderStripAtFirstColumn(current, k);
	  -- Sign: (-1)^{c(R) - 1}.
	  sign = sign * (if odd cR then 1 else -1);
	  numStripsRemoved = numStripsRemoved + 1;
	  current = newLam;
	  );
     -- (D3) If odd number of strips removed, apply sigma.
     if odd numStripsRemoved then (
	  sigmaResult := sigmaInvolution(current, m);
	  if sigmaResult === null then return null;
	  current = sigmaResult;
	  );
     (current, sign)
     )

-- modificationRule(lambda, n, type):
--   Unified entry point.  type is one of:
--     "C"  (symplectic, Sp(2n))
--     "B"  (odd orthogonal, O(2n+1))
--     "D"  (even orthogonal, O(2n))
--   Returns (tau, sign) or null.
modificationRule = (lambda, n, type) -> (
     if type == "C" then sswTypeC(lambda, n)
     else if type == "B" then sswTypeBD(lambda, 2*n+1)
     else if type == "D" then sswTypeBD(lambda, 2*n)
     else error("modificationRule: unknown type " | toString type)
     )

---------------------------------------------------------------
-- Stable sp_lambda and o_lambda expressed in the Schur basis.
---------------------------------------------------------------
--
-- There is NO clean closed-form Koike-type formula
--     sp_lambda = sum_{delta in C} (-1)^{|delta|/2} s_{lambda/delta}
-- valid for all lambda: such a formula would force every Schur term s_mu
-- appearing in sp_lambda to have |lambda| - |mu| even, and that fails
-- (e.g. sp_{(2,2,2)} contains -s_{(2,1,1)} with |diff|=1 ... is actually
-- the opposite: in our conventions |lambda/mu| *is* always even; the
-- issue is that the naive even-col class over-counts).
--
-- Instead we invert the *forward* (correct) Littlewood branching rule
--     s_lambda = sum_{mu, nu : nu has all even cols} c^{lambda}_{mu,nu} sp_mu
-- recursively:
--     sp_lambda  =  s_lambda  -  sum_{nu even-col, nu != {}} sum_{mu}
--                                       c^{lambda}_{mu,nu} sp_mu.
-- Since nu != {} forces |mu| < |lambda|, recursion terminates.
-- We memoize the resulting s-basis coefficient lists.

-- Cache: partition lambda (list) -> list of (mu, integer) pairs representing
-- sp_lambda = sum_mu (coef) s_mu in the stable universal character ring.
stableSpInSchurCache := new MutableHashTable

-- Return list of (mu, coef) with coef nonzero integer, such that
--   sp_lambda  ==  sum_mu coef * s_mu.
stableSpInSchur = (lambda) -> (
     lam := stripTrailingZeros toList lambda;
     if stableSpInSchurCache#?lam then return stableSpInSchurCache#lam;
     acc := new MutableHashTable;
     acc#lam = 1;
     for nu in evenColsSubpartitionsOf(lam) do (
	  if #nu == 0 then continue;
	  for pair in skewSchurExpansion(lam, nu) do (
	       mu := pair#0;
	       lrCoef := pair#1;
	       for t in stableSpInSchur(mu) do (
		    mu2 := t#0;
		    c := t#1;
		    prev := if acc#?mu2 then acc#mu2 else 0;
		    newVal := prev - lrCoef * c;
		    if newVal == 0 then (
			 if acc#?mu2 then remove(acc, mu2);
			 )
		    else acc#mu2 = newVal;
		    );
	       );
	  );
     ans := for k in keys acc list (k, acc#k);
     stableSpInSchurCache#lam = ans;
     ans
     )

-- Same, but for orthogonal characters (Littlewood uses nu with all even rows).
stableOInSchurCache := new MutableHashTable

stableOInSchur = (lambda) -> (
     lam := stripTrailingZeros toList lambda;
     if stableOInSchurCache#?lam then return stableOInSchurCache#lam;
     acc := new MutableHashTable;
     acc#lam = 1;
     for nu in evenRowsSubpartitionsOf(lam) do (
	  if #nu == 0 then continue;
	  for pair in skewSchurExpansion(lam, nu) do (
	       mu := pair#0;
	       lrCoef := pair#1;
	       for t in stableOInSchur(mu) do (
		    mu2 := t#0;
		    c := t#1;
		    prev := if acc#?mu2 then acc#mu2 else 0;
		    newVal := prev - lrCoef * c;
		    if newVal == 0 then (
			 if acc#?mu2 then remove(acc, mu2);
			 )
		    else acc#mu2 = newVal;
		    );
	       );
	  );
     ans := for k in keys acc list (k, acc#k);
     stableOInSchurCache#lam = ans;
     ans
     )

---------------------------------------------------------------

-----------------------------------------------------------------------------
-- Sp/O <-> Schur basis conversions: "RE" (Raw Engine) family
-----------------------------------------------------------------------------
-- The four functions below (spToSchurRE, schurToSpRE, oToSchurRE,
-- schurToORE) are the low-level engines used to reinterpret a polynomial
-- between the Sp-/O-basis view of a SchurRing and the Schur-basis view of
-- another (possibly the same) SchurRing T.
--
-- "RE" suffix = Raw Engine.  These routines deliberately use raw(f)*raw(g)
-- on the engine side rather than the overloaded S*S product: for Sp/O
-- rings the high-level product is itself defined in terms of these
-- conversions, so calling it here would recurse back into this file.
-- Working at the raw level short-circuits that.
--
-- Forward conversions (*ToSchurRE):
--   spToSchurRE / oToSchurRE expand each basis element sp_lambda (resp.
--   o_lambda) in the stable Schur basis by looking up the precomputed
--   tables stableSpInSchur / stableOInSchur.  Those tables encode the
--   Koike universal-character branching formulas for Sp/O -> GL.  When
--   T has finite rank we simply drop Schur summands s_mu with
--   #mu > rank(T).
--
-- Reverse conversions (schurTo*RE):
--   These invert the forward map via unitriangular peeling (see the loop
--   comments inside each function).  In the finite-rank case we apply
--   the Sam-Snowden-Weyman modification rules (sswTypeC for type C,
--   sswTypeBD for types B/D), and for B/D we further apply
--   sigmaInvolution to keep the output inside the length <= n window
--   the engine can represent.
-----------------------------------------------------------------------------

-- Convert an Sp-basis element f in S to its Schur-basis interpretation in T.
-- T may equal S (in-place reinterpretation) or be a different SchurRing.
-- Uses raw engine arithmetic to avoid triggering overloaded S*S multiplication.
spToSchurRE = (f, T) -> (
     if f == 0 then return 0_T;
     dimT := numgens T;
     rawRes := raw(0_T);
     -- For each term c * sp_lambda of f, expand sp_lambda in the stable
     -- Schur basis via stableSpInSchur (Koike branching) and accumulate
     -- into rawRes; truncate to length <= dimT when T has finite rank.
     for term in listForm f do (
	  lam := term#0;
	  c := term#1;
	  for pair in stableSpInSchur(lam) do (
	       mu := pair#0;
	       coef := pair#1;
	       if dimT === infinity or #mu <= dimT then (
		    sc := raw promote(c * coef, T);
		    ba := raw T_(mu);
		    rawRes = rawRes + sc * ba;
		    );
	       );
	  );
     new T from rawRes
     )

-- Convert a Schur-basis element f to its Sp-basis interpretation in T.
--
-- Unified stable/finite algorithm (unitriangular peeling):
--
-- Stable sp_lambda = s_lambda + (lower terms), so we can compute the
-- sp-basis expansion iteratively: peel off an s_lambda term, record it
-- as the sp_lambda coefficient, and subtract (stable) sp_lambda's full
-- s-basis expansion from the running "working" polynomial.  Repeat until
-- empty.  Termination follows from unitriangularity of sp in s.
--
-- For stable T (numgens infinity), each peeled sp_lambda is already a
-- valid basis element.
--
-- For finite T (Sp(2n)), the peeled sp_lambda is a *stable* sp label that
-- maps to the finite ring via the Sam-Snowden-Weyman type-C modification
-- rule: sp_lambda (stable) = sign * sp_tau (finite) or 0.
schurToSpRE = (f, T) -> (
     if f == 0 then return 0_T;
     dimT := numgens T;
     -- working: mutable { mu -> coefficient } holding the residual Schur
     -- expansion still to be peeled.  Seeded with the terms of f.
     working := new MutableHashTable;
     for term in listForm f do working#(term#0) = term#1;
     rawRes := raw(0_T);
     -- Peeling loop:
     --   (1) pick any lambda with nonzero coefficient c in `working`;
     --   (2) by unitriangularity, lambda must be a leading sp_lambda --
     --       i.e. sp_lambda = s_lambda + (strictly lower Schur terms);
     --   (3) record c * sp_lambda in rawRes (finite rank: apply the
     --       type-C modification rule sswTypeC);
     --   (4) subtract c * (sp_lambda - s_lambda), i.e. the lower-order
     --       s-basis terms, from `working`, and continue.
     while #working > 0 do (
	  lam := first keys working;
	  c := working#lam;
	  remove(working, lam);
	  if c == 0 then continue;
	  -- Step (3): contribute c * sp_lambda to the output.
	  if dimT === infinity then (
	       rawRes = rawRes + raw promote(c, T) * raw T_lam;
	       )
	  else (
	       -- Finite rank: sswTypeC returns null (term vanishes under
	       -- the type-C modification rule) or a pair (tau, sign)
	       -- giving the admissible partition tau and its overall sign.
	       r := sswTypeC(lam, dimT);
	       if r =!= null then (
		    tau := r#0;
		    sgnMod := r#1;
		    rawRes = rawRes + raw promote(c * sgnMod, T) * raw T_tau;
		    );
	       );
	  -- Step (4): subtract c * sp_lam in the s-basis (minus the s_lam
	  -- term itself, which we already consumed by removing lam from
	  -- working).
	  for pair in stableSpInSchur(lam) do (
	       mu := pair#0;
	       coef := pair#1;
	       if mu === lam then continue;
	       prev := if working#?mu then working#mu else 0;
	       newVal := prev - c * coef;
	       if newVal == 0 then (
		    if working#?mu then remove(working, mu);
		    )
	       else working#mu = newVal;
	       );
	  );
     new T from rawRes
     )

-- Convert an O-basis element f to Schur-basis interpretation.
oToSchurRE = (f, T) -> (
     if f == 0 then return 0_T;
     dimT := numgens T;
     rawRes := raw(0_T);
     -- Mirrors spToSchurRE, but expands o_lambda using the orthogonal
     -- branching table stableOInSchur.
     for term in listForm f do (
	  lam := term#0;
	  c := term#1;
	  for pair in stableOInSchur(lam) do (
	       mu := pair#0;
	       coef := pair#1;
	       if dimT === infinity or #mu <= dimT then (
		    sc := raw promote(c * coef, T);
		    ba := raw T_(mu);
		    rawRes = rawRes + sc * ba;
		    );
	       );
	  );
     new T from rawRes
     )

-- Convert a Schur-basis element f to O-basis interpretation in T.
--
-- Same unified stable/finite algorithm as schurToSpRE (unitriangular
-- peeling), but with even-row Littlewood (GL->O uses nu with all even
-- rows) and with the type B/D SSW rule in the finite case.
-- T.OddOrEven selects:
--   "Odd"  (default) -> O(2n+1) = type B_n, m = 2n+1
--   "Even"           -> O(2n)   = type D_n, m = 2n
schurToORE = (f, T) -> (
     if f == 0 then return 0_T;
     dimT := numgens T;
     kind := if T.?OddOrEven then T.OddOrEven else "Odd";
     m := if dimT === infinity then null else
          (if kind == "Odd" then 2*dimT+1 else 2*dimT);
     -- working: mutable { mu -> coefficient } residual Schur expansion.
     working := new MutableHashTable;
     for term in listForm f do working#(term#0) = term#1;
     rawRes := raw(0_T);
     -- Peeling loop (mirrors schurToSpRE):
     --   (1) pick lambda with nonzero coefficient c in `working`;
     --   (2) unitriangularity: o_lambda = s_lambda + (lower terms);
     --   (3) record c * o_lambda (finite case: apply sswTypeBD, then
     --       sigmaInvolution if the admissible tau has length > n);
     --   (4) subtract c * (o_lambda - s_lambda) from `working` and loop.
     while #working > 0 do (
	  lam := first keys working;
	  c := working#lam;
	  remove(working, lam);
	  if c == 0 then continue;
	  -- Step (3): contribute c * o_lambda to the output.
	  if dimT === infinity then (
	       rawRes = rawRes + raw promote(c, T) * raw T_lam;
	       )
	  else (
	       -- Finite rank: sswTypeBD returns null (term vanishes) or a
	       -- pair (tau, sign) giving the admissible partition under
	       -- the type B/D modification rule.
	       r := sswTypeBD(lam, m);
	       if r =!= null then (
		    tau := r#0;
		    sgnMod := r#1;
		    -- SSW admissibility (col_1(tau)+col_2(tau) <= m) permits
		    -- partitions with length > n.  For the SchurRing engine,
		    -- which only holds length <= n, apply sigmaInvolution:
		    -- for SO(m), o_tau == o_{sigma(tau)} (det is trivial).
		    -- If sigma still doesn't reduce length <= n, the term
		    -- does not correspond to a ring-representable irrep
		    -- and is dropped.
		    if #tau > dimT then (
			 sigmaTau := sigmaInvolution(tau, m);
			 if sigmaTau =!= null and #sigmaTau <= dimT then
			      rawRes = rawRes + raw promote(c * sgnMod, T) * raw T_sigmaTau;
			 )
		    else rawRes = rawRes + raw promote(c * sgnMod, T) * raw T_tau;
		    );
	       );
	  -- Step (4): subtract c * o_lam in the s-basis (minus the s_lam
	  -- term itself, already consumed above).
	  for pair in stableOInSchur(lam) do (
	       mu := pair#0;
	       coef := pair#1;
	       if mu === lam then continue;
	       prev := if working#?mu then working#mu else 0;
	       newVal := prev - c * coef;
	       if newVal == 0 then (
		    if working#?mu then remove(working, mu);
		    )
	       else working#mu = newVal;
	       );
	  );
     new T from rawRes
     )

-- Get or create the associated symplectic-basis SchurRing (stable by default).
symplecticBasisRingOf = (S) -> (
     if S.?symplecticBasisRing then S.symplecticBasisRing
     else (
	  n := numgens S;
	  spSym := getSymbol "sp";
	  T := if n === infinity
	       then schurRing(coefficientRing S, spSym, GroupActing => "Sp")
	       else schurRing(coefficientRing S, spSym, n, GroupActing => "Sp");
	  S.symplecticBasisRing = T;
	  T
	  )
     )

-- Get or create the associated orthogonal-basis SchurRing (stable by default).
orthogonalBasisRingOf = (S) -> (
     if S.?orthogonalBasisRing then S.orthogonalBasisRing
     else (
	  n := numgens S;
	  oSym := getSymbol "o";
	  T := if n === infinity
	       then schurRing(coefficientRing S, oSym, GroupActing => "O")
	       else schurRing(coefficientRing S, oSym, n, GroupActing => "O");
	  S.orthogonalBasisRing = T;
	  T
	  )
     )

-- Get or create the associated plain Schur-basis ring (GroupActing "GL",
-- Basis "Schur") of the same size as S.  Used by toS on Sp/O/Monomial inputs.
schurBasisRingOf = (S) -> (
     if S.?schurBasisRing then S.schurBasisRing
     else (
	  n := numgens S;
	  sSym := getSymbol "s";
	  T := if n === infinity
	       then schurRing(coefficientRing S, sSym)
	       else schurRing(coefficientRing S, sSym, n);
	  S.schurBasisRing = T;
	  T
	  )
     )

-- Get or create a STABLE (infinity-many-generator) plain Schur-basis helper
-- ring for use by Sp/O multiplication.  Finite-n Sp/O multiplication must
-- compute its GL product in an unrestricted Schur ring so partitions with
-- more than numgens S rows (which would be truncated by the engine) are
-- preserved until the modification rule in schurToSpRE/schurToORE can
-- collapse them back into valid Sp/O-basis elements.
stableSchurHelperOf = (S) -> (
     if S.?stableSchurHelper then S.stableSchurHelper
     else (
	  sSym := getSymbol "s";
	  T := schurRing(coefficientRing S, sSym);
	  S.stableSchurHelper = T;
	  T
	  )
     )

---------------------------------------------------------------
--------------End Sp / O stable character rings----------------
---------------------------------------------------------------

-- Module-scope closures read by recTrans.  They are set inside
-- toS(RingElement) (symmetricRing branch) immediately before the call to
-- recTrans, and describe ring-specific logic that recTrans needs at every
-- level of the recursion:
--   leadTermFcn(pl) -- pick the leading h-variable h_i of pl, or null
--                      when pl has no h-variables left (it is a scalar in
--                      the coefficient ring).
--   retFcn(pl)      -- base case: lift pl into the coefficient ring and
--                      continue with toS there (descends one schurLevel).
--   mappingFcn(v)   -- given v = h_i, return the Schur generator s_i of
--                      the corresponding SchurRing.
-- These are kept at module scope (rather than passed as arguments) because
-- recTrans is mutually recursive through toS and needs to see the same
-- closures at every level of the h-polynomial.
leadTermFcn := local leadTermFcn;
retFcn := local retFcn;
mappingFcn := local mappingFcn;

--------------------------------------------------------------------------
-- toS(f): rewrite f in the Schur basis.
--
-- Dispatch tree:
--   1. schurLevel(ring f) == 0  ---->  return f unchanged (base ring).
--   2. ring f is a SchurRing:
--        a. variant bases route through dedicated converters:
--             Basis == "Monomial"    -> monomialToSchurRE
--             GroupActing == "Sp"    -> spToSchurRE
--             GroupActing == "O"     -> oToSchurRE
--             GroupActing == "SL"    -> relabel partitions (sl_lambda
--                                       lifts to s_lambda with
--                                       lambda_n = 0);
--        b. plain GL/Sn/RatGL rings are already Schur-indexed, so toS is
--           a no-op (use toGL/toSn/convert/specialize to move rings).
--   3. ring f is a symmetricRing with schurLevel > 0:
--        convert f to the complete-homogeneous (h) basis via toH, then
--        apply recTrans to rewrite the result in the Schur basis.
--
-- The three closures above are installed in case 3 and consumed by
-- recTrans.  The cryptic mappingFcn index formula
--     (schurRing ring v)_{index v - 2*(ring v).dim + 1}
-- decodes as follows: in a symmetricRing of dimension n the variable
-- layout is e_1..e_n, p_1..p_n, h_1..h_n, so the h-block starts at
-- generator-index 2n and h_i has index 2n + i - 1.  Subtracting 2n - 1
-- recovers i, which then indexes the Schur generator s_i on the
-- single-row partition {i}.
--------------------------------------------------------------------------
toS = method()

toS(RingElement) := (f) -> (
     R := ring f;
     if schurLevel R == 0 then return f;
     if class R === SchurRing then (
	  -- Variant-basis SchurRings: convert to an associated plain Schur ring.
	  local TSch;
	  if R.?Basis and R.Basis == "Monomial" then (
	       TSch = schurBasisRingOf R;
	       return monomialToSchurRE(f, TSch);
	       );
	  if R.?GroupActing and R.GroupActing == "Sp" then (
	       TSch = schurBasisRingOf R;
	       return spToSchurRE(f, TSch);
	       );
	  if R.?GroupActing and R.GroupActing == "O" then (
	       TSch = schurBasisRingOf R;
	       return oToSchurRE(f, TSch);
	       );
	  if R.?GroupActing and R.GroupActing == "SL" then (
	       -- sl_lambda lifts to the GL irrep s_lambda (canonical choice
	       -- with lambda_n = 0).  Just relabel partitions.
	       TSch = schurBasisRingOf R;
	       dimTSch := numgens TSch;
	       rawRes := raw(0_TSch);
	       for term in listForm f do (
		    if dimTSch === infinity or #(term#0) <= dimTSch then (
			 rawRes = rawRes
			      + raw promote(term#1, TSch) * raw (TSch_(term#0));
			 );
		    );
	       return new TSch from rawRes;
	       );
	  -- Fall-through: plain GL, Sn, and RatGL rings are all already
	  -- represented in a Schur-indexed basis, so toS is a no-op --
	  -- there is no finer "plain Schur" basis to convert to.  Users
	  -- wishing to move into a *different* ring (e.g. an Sn element
	  -- into a GL ring, or a RatGL element into a polynomial ring at
	  -- some specific rank) should use toGL(f, T), toSn(f, T),
	  -- convert(f, T), or specialize(f, n) explicitly.
	  return f;
	  );
     -- symmetricRing with schurLevel > 0: go via the h-basis and recTrans.
     (
	  S := schurRing R;
     	  local hf;
     	  n := R.dim;
     	  d := first degree f;
     	  ngS := numgens S;
	  -- mappingFcn: send h-variable v = h_i to the Schur generator s_i
	  -- in the correct SchurRing (see header for the index arithmetic).
     	  mappingFcn = (v) -> (schurRing ring v)_{index v-2*(ring v).dim+1};
	  -- leadTermFcn: return the h-variable of maximal index appearing in
	  -- pl, or null when pl has no h-variable left (it is a scalar).
     	  leadTermFcn = (pl) -> (
     	       R := ring pl;
     	       spl := select(support pl,i->index i<numgens R);
     	       if spl == {} then null else last spl
     	       );
	  -- retFcn: base case of recTrans.  pl is liftable into the
	  -- coefficient ring (it has no h-variables); lift it and recurse
	  -- on toS there, descending one schurLevel.
     	  retFcn = (pl) -> toS lift(pl,(coefficientRing ring pl));
     	  promote(recTrans(toH f),S)
	  )
     )

toS(Thing) := (f) -> f
undocumented(toS,Thing)

toS(Thing,Ring) := (f,T) -> try(lift(f,T)) else f
undocumented(toS,Thing,Ring)

toS(RingElement,SchurRing) := (f, T) ->
(
     R := ring f;
     if schurLevel R == 0 then
     (
	  U := T;
	  while schurLevel U > 0 do U = coefficientRing U;
	  toS(f,U)
	  )
     else
     (
     	  fS := toS f;
     	  dimT := numgens T;
     	  (listForm fS)/(i-> if dimT === infinity or #i#0<=dimT then T_(i#0)*toS(i#1,coefficientRing T) else 0_T)//sum
	  )
     )

-----------------------------------------------------------------------
-- User-facing "change of basis" API for SchurRing variants.
--
-- Five basis-specific conversion methods plus a universal dispatcher:
--
--   toSp  : expand in symplectic    (Sp)   characters
--   toO   : expand in orthogonal    (O)    characters
--   toGL  : expand in general linear(GL)   characters (= plain Schur)
--   toSn  : relabel as symmetric-group (Sn) characters via Frobenius
--   convert : universal entry point; routes to one of the above by
--             inspecting the target ring's GroupActing tag
--
-- Each toX comes in two forms:
--   toX(f)    -- no explicit target; build/fetch a default output ring
--   toX(f,T)  -- target an explicit SchurRing T (must have the
--                appropriate GroupActing tag)
--
-- The common dispatch pattern inside each toX(f,T):
--   1. If f already lives in an X-basis ring, just copy partition
--      labels into T (respecting T's rank) and return.
--   2. If f lives in a different variant basis, route through plain
--      Schur (toS) first, then re-enter toX.
--   3. Otherwise f is in a plain Schur ring: invoke the reverse
--      conversion (schurToSpRE / schurToORE / ...).
-- Thing / Number base cases simply promote or return unchanged.
-----------------------------------------------------------------------

-- ==== toSp ====
-- toSp: expand a symmetric function in the basis of symplectic characters.
-- Returns a RingElement in a SchurRing with GroupActing => "Sp".  With no
-- target supplied, an associated symplectic ring is cached on the input ring
-- (lazy construction) and used as the output ring.
toSp = method()

toSp(RingElement) := (f) -> (
     R := ring f;
     if class R =!= SchurRing then return toSp(toS f);
     if R.?GroupActing and R.GroupActing == "Sp" then return f;
     T := symplecticBasisRingOf R;
     toSp(f, T)
     )

toSp(RingElement, SchurRing) := (f, T) -> (
     if not (T.?GroupActing and T.GroupActing == "Sp") then
	  error "expected second argument to be a SchurRing with GroupActing => \"Sp\"";
     R := ring f;
     if class R =!= SchurRing then return toSp(toS f, T);
     dimT := numgens T;
     -- If f is already in the Sp basis, just re-promote partition labels.
     if R.?GroupActing and R.GroupActing == "Sp" then (
	  rawRes := raw(0_T);
	  for term in listForm f do (
	       if dimT === infinity or #(term#0) <= dimT then (
		    sc := raw promote(term#1, T);
		    ba := raw T_(term#0);
		    rawRes = rawRes + sc * ba;
		    );
	       );
	  return new T from rawRes;
	  );
     -- If f is in another variant basis (O, Monomial), route through plain S.
     if (R.?GroupActing and R.GroupActing == "O")
	  or (R.?Basis and R.Basis == "Monomial")
	  then return toSp(toS f, T);
     -- Otherwise f is in a plain Schur ring: apply Koike inverse formula.
     schurToSpRE(f, T)
     )

toSp(Thing) := (f) -> f
undocumented(toSp,Thing)

-- ==== toO ====
-- toO: expand a symmetric function in the basis of orthogonal characters.
-- Returns a RingElement in a SchurRing with GroupActing => "O".  The
-- OddOrEven tag attached to T (default "Odd", i.e. O(2n+1) / type B_n)
-- governs which Littlewood branching rule is used when going from a
-- plain Schur expansion into the O basis.
toO = method()

toO(RingElement) := (f) -> (
     R := ring f;
     if class R =!= SchurRing then return toO(toS f);
     if R.?GroupActing and R.GroupActing == "O" then return f;
     T := orthogonalBasisRingOf R;
     toO(f, T)
     )

toO(RingElement, SchurRing) := (f, T) -> (
     if not (T.?GroupActing and T.GroupActing == "O") then
	  error "expected second argument to be a SchurRing with GroupActing => \"O\"";
     R := ring f;
     if class R =!= SchurRing then return toO(toS f, T);
     dimT := numgens T;
     -- Source is already in the O basis: just re-promote partition labels.
     if R.?GroupActing and R.GroupActing == "O" then (
	  rawRes := raw(0_T);
	  for term in listForm f do (
	       if dimT === infinity or #(term#0) <= dimT then (
		    sc := raw promote(term#1, T);
		    ba := raw T_(term#0);
		    rawRes = rawRes + sc * ba;
		    );
	       );
	  return new T from rawRes;
	  );
     -- Source is in another variant basis (Sp, Monomial): route through plain S.
     if (R.?GroupActing and R.GroupActing == "Sp")
	  or (R.?Basis and R.Basis == "Monomial")
	  then return toO(toS f, T);
     -- Fallback: source is a plain Schur ring; apply Littlewood branching.
     schurToORE(f, T)
     )

toO(Thing) := (f) -> f
undocumented(toO,Thing)

-- ==== toGL ====
-- toGL: re-express an element in the plain GL Schur basis.  This is a
-- thin synonym for toS that reads naturally when the intent is to
-- obtain a GL character.  With a target ring T the element is promoted
-- into T (which must have GroupActing => "GL" and may have finite or
-- infinite rank; finite-rank targets truncate partitions longer than
-- numgens T via specializeGLInto inside toS).
toGL = method()
toGL(RingElement) := (f) -> toS f
toGL(RingElement, SchurRing) := (f, T) -> (
     -- Reject targets whose GroupActing tag is anything other than "GL".
     if T.?GroupActing and T.GroupActing != "GL" then
	  error ("toGL: target ring must have GroupActing => \"GL\", got \""
	       | toString T.GroupActing | "\"");
     toS(f, T)
     )
toGL(Thing) := (f) -> f
undocumented(toGL, Thing)

-- ==== toSn ====
-- toSn: re-express an element as an S_n class function written in the
-- Schur basis (the Frobenius-characteristic convention).  The
-- underlying partition data is carried over verbatim; toSn does NOT
-- apply any restriction formula, because the Sn and GL Schur rings use
-- the same partition index set -- only the multiplication differs
-- (internal product vs. Littlewood-Richardson).  Finite-rank targets
-- drop partitions with more than numgens T parts.
toSn = method()
toSn(RingElement, SchurRing) := (f, T) -> (
     if not (T.?GroupActing and T.GroupActing == "Sn") then
	  error "toSn: target ring must have GroupActing => \"Sn\"";
     R := ring f;
     -- Non-SchurRing inputs: first convert to Schur form.
     if class R =!= SchurRing then return toSn(toS f, T);
     -- From a variant basis other than plain GL/Sn, go through toS.
     if R.?GroupActing and R.GroupActing != "GL" and R.GroupActing != "Sn" then
	  return toSn(toS f, T);
     if R.?Basis and R.Basis == "Monomial" then
	  return toSn(toS f, T);
     -- Copy partition labels into T, respecting the target rank.
     dimT := numgens T;
     rawRes := raw(0_T);
     for term in listForm f do (
	  lam := term#0;
	  c   := term#1;
	  if dimT === infinity or (class dimT =!= InfiniteNumber and #lam <= dimT) then (
	       sc := raw promote(c, T);
	       rawRes = rawRes + sc * raw T_lam;
	       );
	  );
     new T from rawRes
     )
toSn(Thing, SchurRing) := (f, T) -> try promote(f, T) else error "toSn: cannot promote input to target ring"
undocumented(toSn, Thing, SchurRing)

-- ==== convert (universal dispatch) ====
-- convert: universal dispatcher.  Given any symmetric-function-like
-- element f and a target ring T, pick the right converter.  The target
-- ring's classification drives the dispatch:
--   * SchurRing with GroupActing "GL"    -> toS(f, T)
--   * SchurRing with GroupActing "Sn"    -> toSn(f, T)
--   * SchurRing with GroupActing "Sp"    -> toSp(f, T)
--   * SchurRing with GroupActing "O"     -> toO(f, T)
--   * SchurRing with GroupActing "SL"    -> toS(f, T)  (GL collapse)
--   * SchurRing with GroupActing "RatGL" -> toRatGL(f, T)
--   * symmetricRing                      -> re-apply Jacobi-Trudi in T
--   * any other ring                     -> try promote(f, T)
-- This is a pure routing layer; no new math is performed here.
convert = method()
convert(RingElement, Ring) := (f, T) -> (
     -- SchurRing target: dispatch on GroupActing
     if instance(T, SchurRing) then (
	  ga := if T.?GroupActing then T.GroupActing else "GL";
	  if ga == "GL" then return toS(f, T);
	  if ga == "Sn" then return toSn(f, T);
	  if ga == "Sp" then return toSp(f, T);
	  if ga == "O"  then return toO(f, T);
	  if ga == "SL" then return toS(f, T);
	  if ga == "RatGL" then return toRatGL(f, T);
	  error ("convert: unknown GroupActing on target: " | toString ga);
	  );
     -- symmetricRing target: re-apply Jacobi-Trudi into T.  toSymm alone
     -- uses the source ring's pre-attached symmetricRing, which is
     -- typically a different ring object from T; computing jacobiTrudi
     -- directly in T produces an element that actually lives in T.
     if T.?schurLevel and schurLevel T > 0 and class T =!= SchurRing then (
	  R := ring f;
	  if R === T then return f;
	  -- From a SchurRing: sum c_lambda * jacobiTrudi(lambda, T)
	  if instance(R, SchurRing) then (
	       tms := listForm f;
	       cR := coefficientRing R;
	       return sum apply(tms, (p, c) ->
		    (try b := jacobiTrudi(toList p, T)
			 else error "convert: target symmetricRing has smaller dim than source partitions; enlarge target")
		    * promote(lift(c, cR), T));
	       );
	  -- Otherwise already in some symmetric ring: try plain promotion.
	  return try promote(f, T) else
	       error "convert: symmetric-ring target is not compatible with source";
	  );
     -- Generic ring: promote if possible
     try promote(f, T) else
	  error ("convert: no route from " | toString ring f | " to " | toString T)
     )
convert(Number, Ring) := (f, T) -> try promote(f, T) else f
convert(Thing, Ring) := (f, T) -> try promote(f, T) else f
undocumented(convert, Thing, Ring)

-----------------------------------------------------------------------
-- Specialization of universal characters to finite-rank groups.
--
-- This section implements the restriction of stable (universal)
-- characters to finite-rank classical groups.  The main entry point is
-- `specialize(RingElement, ZZ)`, which dispatches on `R.GroupActing`.
-- Below it live the per-variant workers `specialize<G>` (which fetch or
-- build a cached finite target) and the `<G>Into` variants (which
-- accept an explicit target ring T).  The `specialized<G>RingOf`
-- helpers memoize one finite ring per (stable source, rank) pair so
-- repeated specializations share a target.
-----------------------------------------------------------------------

-- ==== Cached target rings ====

-- Get or create the cached finite-dimensional Sp(2n) character ring attached
-- to a stable Sp ring S.  Keyed by the half-dimension n.
--
-- The ring is constructed with a rank-tagged symbol (e.g. sp_fin_1, sp_fin_2)
-- so that the globalAssign inside schurRing does NOT clobber the user's
-- original stable symbol (e.g. sp).  Users should reach the finite ring via
-- `ring(specialize(f,n))` rather than via a global variable.
specializedSpRingOf = (S, n) -> (
     if not S.?specializedSpRings then S.specializedSpRings = new MutableHashTable;
     if (S.specializedSpRings)#?n then (S.specializedSpRings)#n
     else (
	  spSym := getSymbol("spfin" | toString n);
	  T := schurRing(coefficientRing S, spSym, n, GroupActing => "Sp");
	  (S.specializedSpRings)#n = T;
	  T
	  )
     )

-- Get or create the cached finite-dimensional O character ring attached to a
-- stable O ring S.  Keyed by the pair (n, kind) where kind is "Odd" for
-- SO(2n+1) / O(2n+1) (type B_n) or "Even" for O(2n) / SO(2n) (type D_n).
specializedORingOf = method(Options => {OddOrEven => "Odd"})
specializedORingOf(SchurRing, ZZ) := opts -> (S, n) -> (
     kind := opts.OddOrEven;
     if kind =!= "Odd" and kind =!= "Even" then
	  error "specializedORingOf: OddOrEven must be \"Odd\" or \"Even\"";
     if not S.?specializedORings then S.specializedORings = new MutableHashTable;
     key := (n, kind);
     if (S.specializedORings)#?key then (S.specializedORings)#key
     else (
	  tag := if kind == "Odd" then "finodd" else "fineven";
	  oSym := getSymbol("o" | tag | toString n);
	  T := schurRing(coefficientRing S, oSym, n, GroupActing => "O", OddOrEven => kind);
	  (S.specializedORings)#key = T;
	  T
	  )
     )

-- ==== Main specialize dispatch ====

-- Specialize a stable Sp (resp. O) universal character to the finite-dim
-- Sp(2n) (resp. O(2n+1)/O(2n)) character ring.
--
-- Algorithm: let f live in a stable Sp ring.  Writing f in the stable Schur
-- basis via the forward Koike formula gives  f = sum_nu c_nu s_nu.  The
-- GL(2n) specialization truncates ell(nu) > 2n to zero.  The surviving
-- s_nu's restrict to Sp(2n) via Littlewood's finite-dimensional branching
-- rule
--   s_nu |_{Sp(2n)} = sum_{delta even-col} sum_{mu : ell(mu) <= n}
--                       c^nu_{delta,mu} sp_mu ,
-- i.e., take the even-column skew expansion and drop partitions of length
-- exceeding n.  The whole specialization is a genuine ring homomorphism:
-- specialize(f*g, n) == specialize(f,n) * specialize(g,n) at the level of
-- GL(2n) characters.  In the sp-basis target ring, the user should be aware
-- that multiplication currently uses the stable Newell-Littlewood product
-- with a length cutoff; a faithful finite-dim product (Phase 5d) is not yet
-- wired in.  For an element of a stable Sp ring whose stable Newell-
-- Littlewood product projects inside ell <= n, the two agree.
specialize = method(Options => {OddOrEven => null})
specialize(RingElement, ZZ) := opts -> (f, n) -> (
     R := ring f;
     if class R =!= SchurRing then error "specialize expects a SchurRing element";
     if n < 0 then error "specialize: dimension n must be >= 0";
     if not R.?GroupActing then error "specialize: missing GroupActing attribute on ring";
     if R.GroupActing == "Sp" then (
	  if opts.OddOrEven =!= null then
	       error "specialize: OddOrEven is only meaningful for O rings";
	  specializeSp(f, n)
	  )
     else if R.GroupActing == "O" then (
	  kind := if opts.OddOrEven === null then
	       (if R.?OddOrEven then R.OddOrEven else "Odd")
	       else opts.OddOrEven;
	  specializeO(f, n, OddOrEven => kind)
	  )
     else if R.GroupActing == "GL" then (
	  if opts.OddOrEven =!= null then
	       error "specialize: OddOrEven is only meaningful for O rings";
	  specializeGL(f, n)
	  )
     else if R.GroupActing == "SL" then (
	  if opts.OddOrEven =!= null then
	       error "specialize: OddOrEven is only meaningful for O rings";
	  specializeSL(f, n)
	  )
     else if R.GroupActing == "RatGL" then (
	  if opts.OddOrEven =!= null then
	       error "specialize: OddOrEven is only meaningful for O rings";
	  specializeRatGL(f, n)
	  )
     else if R.GroupActing == "Sn" then (
	  if opts.OddOrEven =!= null then
	       error "specialize: OddOrEven is only meaningful for O rings";
	  specializeSn(f, n)
	  )
     else error("specialize is not implemented for GroupActing => \"" | R.GroupActing | "\"")
     )

-- Specialize to an entire tower of ranks at once.  The list describes the
-- target ranks from the topmost layer inward; `infinity` (or any
-- InfiniteNumber) leaves that layer untouched.  Example: for a ring S of
-- schurLevel 2 whose outer layer is a stable Sp ring and inner layer is a
-- stable GL ring, `specialize(f, {4, 3})` produces an element of Sp(8) over
-- GL(3).  A shorter list only specializes the outermost layers.
specialize(RingElement, List) := opts -> (f, ranks) -> (
     R := ring f;
     if #ranks == 0 then return f;
     nTop := ranks#0;
     g := if class nTop === InfiniteNumber then f
	  else specialize(f, nTop, opts);
     if #ranks == 1 then return g;
     -- recurse into coefficient ring
     S := ring g;
     A := coefficientRing S;
     if class A =!= SchurRing then (
	  if #ranks > 1 then error(
	       "specialize: rank list has " | toString (#ranks)
	       | " entries but only "
	       | toString (schurLevel S)
	       | " SchurRing layers are present");
	  return g;
	  );
     -- Rebuild g by specializing each coefficient
     innerRanks := drop(ranks, 1);
     -- Specialize one coefficient (which lives in A)
     specOneCoef := (c) -> specialize(c, innerRanks, opts);
     -- Take inner specialization of an example coefficient to find target A'
     sampleCoef := 1_A;
     Asp := ring specOneCoef(sampleCoef);
     -- Build the specialized outer ring with coefficient ring Asp
     S' := local S';
     spSym := S.Symbol;
     outerRank := numgens S;
     gActing := S.GroupActing;
     oddEven := if S.?OddOrEven then S.OddOrEven else null;
     basis := if S.?Basis then S.Basis else "Schur";
     S' = if oddEven =!= null then
	       schurRing(Asp, spSym, outerRank,
		    GroupActing => gActing, OddOrEven => oddEven, Basis => basis)
	  else
	       schurRing(Asp, spSym, outerRank,
		    GroupActing => gActing, Basis => basis);
     rawRes := raw(0_S');
     for term in listForm g do (
	  lam := term#0;
	  c := term#1;
	  cSpec := specOneCoef(c);
	  sc := raw promote(cSpec, S');
	  ba := raw (S'_(toList lam));
	  rawRes = rawRes + sc * ba;
	  );
     new S' from rawRes
     )

-- Also allow specializing from a stable Schur (GL) ring directly, with an
-- explicit target kind.  This is occasionally useful when the user already
-- has a Schur-basis representation and wants to restrict along GL -> Sp/O.
specialize(RingElement, ZZ, SchurRing) := opts -> (f, n, T) -> (
     if not (class T === SchurRing and T.?GroupActing) then
	  error "specialize: target ring must be an Sp or O SchurRing";
     if opts.OddOrEven =!= null then (
	  -- allow user to override/verify T's tag
	  if T.?OddOrEven and T.OddOrEven =!= opts.OddOrEven then
	       error("specialize: OddOrEven option " | toString opts.OddOrEven
		    | " does not match target ring's OddOrEven = "
		    | toString T.OddOrEven);
	  );
     if T.GroupActing == "Sp" then specializeSpInto(f, n, T)
     else if T.GroupActing == "O" then specializeOInto(f, n, T)
     else if T.GroupActing == "GL" then specializeGLInto(f, n, T)
     else if T.GroupActing == "SL" then specializeSLInto(f, n, T)
     else if T.GroupActing == "Sn" then specializeSnInto(f, n, T)
     else error("specialize into GroupActing \"" | T.GroupActing | "\" is not supported")
     )

-- ==== Per-variant specialize and *Into workers ====

-- Inject a GL element (possibly coming from an Sp/O/Monomial ring via toS)
-- into an explicit finite GL ring T, truncating partitions of length > n.
-- The `*Into` form accepts an explicit target; the plain `specializeGL`
-- below fetches a cached target via `specializedGLRingOf`.
specializeGLInto = (f, n, T) -> (
     R := ring f;
     local fs;
     if class R === SchurRing and R.?GroupActing and R.GroupActing =!= "GL"
	  and R.GroupActing =!= "SL" then (
	  sStable := schurBasisRingOf R;
	  fs = toS(f, sStable);
	  ) else fs = f;
     rawRes := raw(0_T);
     for term in listForm fs do (
	  lam := term#0;
	  c := term#1;
	  if #lam > n then continue;
	  sc := raw promote(c, T);
	  ba := raw (T_(toList lam));
	  rawRes = rawRes + sc * ba;
	  );
     new T from rawRes
     )

-- Inject a GL/SL element into an explicit finite SL(n) ring T: truncate
-- partitions of length > n, then apply slCanonicalize to normalize
-- representatives modulo the determinant relation.
specializeSLInto = (f, n, T) -> (
     R := ring f;
     local fs;
     if class R === SchurRing and R.?GroupActing and R.GroupActing =!= "GL"
	  and R.GroupActing =!= "SL" then (
	  sStable := schurBasisRingOf R;
	  fs = toS(f, sStable);
	  ) else fs = f;
     rawRes := raw(0_T);
     for term in listForm fs do (
	  lam := term#0;
	  c := term#1;
	  if #lam > n then continue;
	  sc := raw promote(c, T);
	  ba := raw (T_(toList lam));
	  rawRes = rawRes + sc * ba;
	  );
     slCanonicalize(new T from rawRes, T)
     )

-- Restrict a stable GL/Sp character into an explicit finite Sp(2n) ring T
-- via Littlewood's branching rule.  After pushing f to the Schur basis,
-- partitions with ell(nu) > 2n are discarded (GL(2n) truncation), and
-- each surviving s_nu expands as sum over even-column subpartitions
-- delta of nu, then the skew s_{nu/delta} = sum c^nu_{delta,mu} s_mu is
-- projected to the sp-basis by keeping only #mu <= n.
specializeSpInto = (f, n, T) -> (
     R := ring f;
     local fs;
     if class R === SchurRing and R.?GroupActing and R.GroupActing == "Sp" then (
	  sStable := schurBasisRingOf R;
	  fs = toS(f, sStable);
	  ) else fs = toS f;
     rawRes := raw(0_T);
     for term in listForm fs do (
	  nu := term#0;
	  c := term#1;
	  if #nu > 2*n then continue;
	  -- Littlewood Sp branching: s_nu|_Sp(2n) = sum_{delta even-col subpart of nu}
	  -- sum_mu c^nu_{delta,mu} sp_mu, truncated to #mu <= n.
	  for delta in evenColsSubpartitionsOf(nu) do (
	       for pair in skewSchurExpansion(nu, delta) do (
		    mu := pair#0;
		    lrCoef := pair#1;
		    if #mu <= n then (
			 sc := raw promote(c * lrCoef, T);
			 ba := raw T_mu;
			 rawRes = rawRes + sc * ba;
			 );
		    );
	       );
	  );
     new T from rawRes
     )

-- Restrict a stable GL/O character into an explicit finite O ring T via
-- Littlewood's branching rule.  nuCutoff is 2n+1 for odd O (type B_n)
-- and 2n for even O (type D_n); partitions longer than this are dropped.
-- Each surviving s_nu restricts via sum over even-row subpartitions
-- delta: sum_mu c^nu_{delta,mu} o_mu, keeping only #mu <= n.
specializeOInto = (f, n, T) -> (
     R := ring f;
     local fs;
     if class R === SchurRing and R.?GroupActing and R.GroupActing == "O" then (
	  sStable := schurBasisRingOf R;
	  fs = toS(f, sStable);
	  ) else fs = toS f;
     kind := if T.?OddOrEven then T.OddOrEven else "Odd";
     nuCutoff := if kind == "Odd" then 2*n+1 else 2*n;
     rawRes := raw(0_T);
     for term in listForm fs do (
	  nu := term#0;
	  c := term#1;
	  if #nu > nuCutoff then continue;
	  -- Littlewood O branching: s_nu|_O = sum_{delta even-row subpart of nu}
	  -- sum_mu c^nu_{delta,mu} o_mu, truncated to #mu <= n.
	  for delta in evenRowsSubpartitionsOf(nu) do (
	       for pair in skewSchurExpansion(nu, delta) do (
		    mu := pair#0;
		    lrCoef := pair#1;
		    if #mu <= n then (
			 sc := raw promote(c * lrCoef, T);
			 ba := raw T_mu;
			 rawRes = rawRes + sc * ba;
			 );
		    );
	       );
	  );
     new T from rawRes
     )

-- Restrict a stable Sp character to the cached finite Sp(2n) ring.  See
-- `specializeSpInto` for the Littlewood even-column branching rule.
specializeSp = (f, n) -> (
     R := ring f;
     T := specializedSpRingOf(R, n);
     sStable := schurBasisRingOf R;
     fs := toS(f, sStable);
     rawRes := raw(0_T);
     for term in listForm fs do (
	  nu := term#0;
	  c := term#1;
	  if #nu > 2*n then continue;
	  -- Littlewood Sp branching (see specializeSpInto): even-column delta,
	  -- skew expansion s_{nu/delta}, keep #mu <= n.
	  for delta in evenColsSubpartitionsOf(nu) do (
	       for pair in skewSchurExpansion(nu, delta) do (
		    mu := pair#0;
		    lrCoef := pair#1;
		    if #mu <= n then (
			 sc := raw promote(c * lrCoef, T);
			 ba := raw T_mu;
			 rawRes = rawRes + sc * ba;
			 );
		    );
	       );
	  );
     new T from rawRes
     )

-- Restrict a stable O character to the cached finite O(2n+1) or O(2n)
-- ring, selected by the OddOrEven option.  See `specializeOInto` for
-- the Littlewood even-row branching rule.
specializeO = method(Options => {OddOrEven => "Odd"})
specializeO(RingElement, ZZ) := opts -> (f, n) -> (
     R := ring f;
     T := specializedORingOf(R, n, OddOrEven => opts.OddOrEven);
     sStable := schurBasisRingOf R;
     fs := toS(f, sStable);
     kind := opts.OddOrEven;
     nuCutoff := if kind == "Odd" then 2*n+1 else 2*n;
     rawRes := raw(0_T);
     for term in listForm fs do (
	  nu := term#0;
	  c := term#1;
	  if #nu > nuCutoff then continue;
	  -- Littlewood O branching (see specializeOInto): even-row delta,
	  -- skew expansion s_{nu/delta}, keep #mu <= n.
	  for delta in evenRowsSubpartitionsOf(nu) do (
	       for pair in skewSchurExpansion(nu, delta) do (
		    mu := pair#0;
		    lrCoef := pair#1;
		    if #mu <= n then (
			 sc := raw promote(c * lrCoef, T);
			 ba := raw T_mu;
			 rawRes = rawRes + sc * ba;
			 );
		    );
	       );
	  );
     new T from rawRes
     )

-- Get or create the cached finite-dimensional GL(n) Schur ring attached to a
-- stable GL ring S.  Analogous to specializedSpRingOf.
specializedGLRingOf = (S, n) -> (
     if not S.?specializedGLRings then S.specializedGLRings = new MutableHashTable;
     if (S.specializedGLRings)#?n then (S.specializedGLRings)#n
     else (
	  glSym := getSymbol("glfin" | toString n);
	  T := schurRing(coefficientRing S, glSym, n, GroupActing => "GL");
	  (S.specializedGLRings)#n = T;
	  T
	  )
     )

-- Get or create the cached finite-dimensional SL(n) Schur ring attached to a
-- stable SL (or GL) ring S.
specializedSLRingOf = (S, n) -> (
     if not S.?specializedSLRings then S.specializedSLRings = new MutableHashTable;
     if (S.specializedSLRings)#?n then (S.specializedSLRings)#n
     else (
	  slSym := getSymbol("slfin" | toString n);
	  T := schurRing(coefficientRing S, slSym, n, GroupActing => "SL");
	  (S.specializedSLRings)#n = T;
	  T
	  )
     )

-- Specialize a GL element to GL(n): truncate partitions of length > n.  If
-- the source ring is itself finite with numgens <= n, the engine already
-- handles this and specialize just returns a relabeled element.
specializeGL = (f, n) -> (
     R := ring f;
     T := specializedGLRingOf(R, n);
     rawRes := raw(0_T);
     for term in listForm f do (
	  lam := term#0;
	  c := term#1;
	  if #lam > n then continue;
	  sc := raw promote(c, T);
	  ba := raw (T_(toList lam));
	  rawRes = rawRes + sc * ba;
	  );
     new T from rawRes
     )

-- Specialize a GL element to SL(n): truncate partitions of length > n, then
-- canonicalize via the determinant relation (strip lambda_n from every part).
-- Also accepts an SL input.
specializeSL = (f, n) -> (
     R := ring f;
     T := specializedSLRingOf(R, n);
     rawRes := raw(0_T);
     for term in listForm f do (
	  lam := term#0;
	  c := term#1;
	  if #lam > n then continue;
	  sc := raw promote(c, T);
	  ba := raw (T_(toList lam));
	  rawRes = rawRes + sc * ba;
	  );
     slCanonicalize(new T from rawRes, T)
     )

---------------------------------------------------------------
-- Specialization for Sn rings (truncation by number of parts)
---------------------------------------------------------------
-- Semantic: an Sn-SchurRing of "rank n" (finite n) allows partitions
-- of arbitrary size but bounded by having at most n parts (matching the
-- constructor schurRing(..., n, GroupActing => "Sn")).  `specialize(f, n)`
-- on an Sn element (stable or finite rank M >= n) simply drops terms
-- whose partition has more than n parts and rebuilds the result in the
-- cached finite target ring.  Unlike GL, there is no modification rule
-- because the finite Sn ring is literally the quotient of the stable
-- ring by the ideal of partitions with > n parts (not a character
-- specialization in the Lie-theory sense, but the user-level analog).

specializedSnRingOf = (S, n) -> (
     if not S.?specializedSnRings then S.specializedSnRings = new MutableHashTable;
     if (S.specializedSnRings)#?n then (S.specializedSnRings)#n
     else (
	  snSym := getSymbol("snfin" | toString n);
	  T := schurRing(coefficientRing S, snSym, n, GroupActing => "Sn");
	  (S.specializedSnRings)#n = T;
	  T
	  )
     )

specializeSn = (f, n) -> (
     R := ring f;
     T := specializedSnRingOf(R, n);
     specializeSnInto(f, n, T)
     )

specializeSnInto = (f, n, T) -> (
     if T.GroupActing =!= "Sn" then
	  error "specializeSn: target ring must have GroupActing => \"Sn\"";
     rawRes := raw(0_T);
     for term in listForm f do (
	  lam := toList term#0;
	  c := term#1;
	  if #lam > n then continue;
	  sc := raw promote(c, T);
	  ba := raw (T_lam);
	  rawRes = rawRes + sc * ba;
	  );
     new T from rawRes
     )

---------------------------------------------------------------
-- Specialization for rational GL (Koike-Terada modification rule)
---------------------------------------------------------------
-- specializedRatGLRingOf(S, n): get or create a finite-rank GL(n) rational
-- Schur ring attached to a stable (or higher-rank) RatGL ring S.  Caching by
-- rank n mirrors specializedGLRingOf / specializedSpRingOf.
specializedRatGLRingOf = (S, n) -> (
     if not S.?specializedRatGLRings then S.specializedRatGLRings = new MutableHashTable;
     if (S.specializedRatGLRings)#?n then (S.specializedRatGLRings)#n
     else (
	  baseR := coefficientRing (S.ratNegRing);
	  ratSym := getSymbol(toString (S.ratPosSym) | "fin" | toString n);
	  T := schurRing(baseR, ratSym, n, GroupActing => "RatGL");
	  (S.specializedRatGLRings)#n = T;
	  T
	  )
     )

-- specializeRatGL(f, n): push f through the Koike-Terada modification rule at
-- rank n and build the result in a cached finite target ring.  The rule is a
-- ring homomorphism: it is a specialization (character evaluation at GL(n)),
-- so it is multiplicative on the universal rational character ring.  The rule
-- lives in ratGLModify (above) and iterateRatGLTerms walks the (alpha, beta,
-- scalar) triples.
specializeRatGL = (f, n) -> (
     R := ring f;
     T := specializedRatGLRingOf(R, n);
     specializeRatGLInto(f, n, T)
     )

-- Construct the RatGL basis element T_{alpha, beta} directly via engine-level
-- raw multiplication.  This bypasses any overridden S*S (which may recurse
-- into Koike-Terada modification) and simply combines outer alpha with inner
-- beta to give the pure bipartition element.
ratGLBasisRaw = (T, alpha, beta) -> (
     B := T.ratNegRing;
     (raw T_alpha) * (raw promote(B_beta, T))
     )

-- Koike universal character product.  For universal rational characters
-- chi_{alpha,beta} the correct product is
--    chi_{alpha,beta} * chi_{gamma,delta}
--      = sum_{epsilon, eta} (s_{alpha/epsilon} s_{gamma/eta})(x) *
--                           (s_{beta/eta}   s_{delta/epsilon})(y)
-- re-expressed in the bipartition basis.  We evaluate the sum by:
--   * iterating over pairs (epsilon, eta), with epsilon contained in both
--     alpha and delta, and eta in both beta and gamma;
--   * lifting the outer skew-Schur expansions to pure-alpha elements of S
--     and the inner ones to pure-beta elements, and multiplying them at the
--     engine level (which performs componentwise LR on the bipartition
--     basis, precisely what is needed for a given (epsilon, eta)).
-- Returns a raw (engine-level) element of S.  Used by RatGL multiplication
-- at both stable and finite rank.
ratGLKoikeProductRaw = (S, f1, f2) -> (
     acc := raw(0_S);
     iterateRatGLTerms(f1, (alpha, beta, sa) -> (
	       iterateRatGLTerms(f2, (gamma, delta, sg) -> (
			 -- Common epsilon bound: componentwise min of alpha and delta.
			 mAD := min(#alpha, #delta);
			 epsBound := for i from 0 to mAD - 1 list min(alpha#i, delta#i);
			 mBG := min(#beta, #gamma);
			 etaBound := for i from 0 to mBG - 1 list min(beta#i, gamma#i);
			 for eps in allSubpartitionsBoundedBy epsBound do (
			      skewAE := skewSchurExpansion(alpha, eps);
			      if #skewAE == 0 then continue;
			      skewDE := skewSchurExpansion(delta, eps);
			      if #skewDE == 0 then continue;
			      for eta in allSubpartitionsBoundedBy etaBound do (
				   skewGH := skewSchurExpansion(gamma, eta);
				   if #skewGH == 0 then continue;
				   skewBH := skewSchurExpansion(beta, eta);
				   if #skewBH == 0 then continue;
				   -- Build A = sum_{mu1} c^alpha_{mu1,eps} S_{mu1,()}.
				   rA := raw(0_S);
				   for pA in skewAE do
					rA = rA + (pA#1) * ratGLBasisRaw(S, toList pA#0, {});
				   rG := raw(0_S);
				   for pG in skewGH do
					rG = rG + (pG#1) * ratGLBasisRaw(S, toList pG#0, {});
				   rB := raw(0_S);
				   for pB in skewBH do
					rB = rB + (pB#1) * ratGLBasisRaw(S, {}, toList pB#0);
				   rD := raw(0_S);
				   for pD in skewDE do
					rD = rD + (pD#1) * ratGLBasisRaw(S, {}, toList pD#0);
				   -- Raw engine multiplication is componentwise LR on
				   -- the bipartition basis, which is what we want.
				   contribution := rA * rG * rB * rD;
				   sc := raw promote(sa * sg, S);
				   acc = acc + sc * contribution;
				   );
			      );
			 ));
	       ));
     acc
     )

-- specializeRatGLInto(f, n, T): same as specializeRatGL but the target ring T
-- is provided explicitly (must have GroupActing => "RatGL" and numgens T == n).
specializeRatGLInto = (f, n, T) -> (
     if T.GroupActing =!= "RatGL" then
	  error "specializeRatGLInto: target ring must have GroupActing => \"RatGL\"";
     if numgens T =!= n then
	  error("specializeRatGLInto: target ring has numgens = " | toString numgens T
	       | " but expected " | toString n);
     rawRes := raw(0_T);
     iterateRatGLTerms(f, (alpha, beta, scalar) -> (
	       for trip in ratGLModify(alpha, beta, n) do (
		    alphaP := trip#0;
		    betaP  := trip#1;
		    coef   := trip#2;
		    sc := raw promote(coef * scalar, T);
		    rawRes = rawRes + sc * ratGLBasisRaw(T, alphaP, betaP);
		    );
	       ));
     new T from rawRes
     )

---------------------------------------------------------------
-- Lifting normal type-A characters to rational characters
---------------------------------------------------------------
-- toRatGL(f, T): given an element f of a plain GL (or SL) SchurRing, or an
-- element of another RatGL ring, lift it into the RatGL ring T by the obvious
-- embedding alpha |-> (alpha, ()) on basis elements.  This realizes the
-- inclusion of polynomial characters into rational characters.
toRatGL = method()
toRatGL(RingElement, SchurRing) := (f, T) -> (
     if T.GroupActing =!= "RatGL" then
	  error "toRatGL: target ring must have GroupActing => \"RatGL\"";
     R := ring f;
     rawRes := raw(0_T);
     if class R === SchurRing and R.?GroupActing and R.GroupActing == "RatGL" then (
	  -- Already rational: re-embed (alpha, beta).  If target rank is finite
	  -- and smaller than the bipartition's "length" ell(alpha)+ell(beta),
	  -- apply the Koike-Terada modification.
	  iterateRatGLTerms(f, (alpha, beta, scalar) -> (
		    nT := numgens T;
		    if class nT === InfiniteNumber then (
			 sc  := raw promote(scalar, T);
			 rawRes = rawRes + sc * ratGLBasisRaw(T, alpha, beta);
			 )
		    else (
			 for trip in ratGLModify(alpha, beta, nT) do (
			      alphaP := trip#0;
			      betaP  := trip#1;
			      coef   := trip#2;
			      sc     := raw promote(coef * scalar, T);
			      rawRes = rawRes + sc * ratGLBasisRaw(T, alphaP, betaP);
			      );
			 );
		    ));
	  )
     else if class R === SchurRing and R.?GroupActing
	  and (R.GroupActing == "GL" or R.GroupActing == "SL") then (
	  -- Lift a plain GL/SL element: alpha |-> (alpha, ()).
	  nT := numgens T;
	  for term in listForm f do (
	       alpha := toList term#0;
	       c     := term#1;
	       if (class nT =!= InfiniteNumber) and #alpha > nT then continue;
	       sc  := raw promote(c, T);
	       rawRes = rawRes + sc * ratGLBasisRaw(T, alpha, {});
	       );
	  )
     else if class R =!= SchurRing and R.?EHPVariables then (
	  -- symmetricRing input: convert to its associated Schur ring (GL) first,
	  -- then recurse into the GL branch above.
	  return toRatGL(toS f, T);
	  )
     else error("toRatGL: source ring must be a SchurRing of type GL/SL/RatGL or a symmetricRing");
     new T from rawRes
     )

-- Convenience: if no target ring is supplied, build a stable RatGL ring
-- attached to the source automatically.  Parallels toSp/toO overloads.
toRatGL(RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing and R.?GroupActing and R.GroupActing == "RatGL" then
	  return f;
     if not R.?associatedRatGLRing then (
	  rtSym := getSymbol "rt";
	  R.associatedRatGLRing = schurRing(QQ, rtSym, infinity, GroupActing => "RatGL");
	  );
     toRatGL(f, R.associatedRatGLRing)
     )

---------------------------------------------------------------
------------- King branching formulas --------------------------
---------------------------------------------------------------
--
-- For the three families of classical groups we implement the
-- restriction of an irreducible character from a "total" group to a
-- block-diagonal Levi subgroup, following King 1975 ("Branching rules
-- for classical Lie groups using tensor and spinor methods",
-- J. Phys. A 8, 429-449).  In terms of Littlewood-Richardson numbers
--
--   c^lambda_{alpha,beta,gamma}
--     = coefficient of s_lambda in s_alpha * s_beta * s_gamma,
--
-- the three rules are
--
--   GL(m+n) down to GL(m) x GL(n):
--     s_lambda |--> sum_{mu,nu} c^lambda_{(), mu, nu}  s_mu x s_nu
--
--   Sp(2m+2n) down to Sp(2m) x Sp(2n):
--     sp_lambda |--> sum_{delta, mu, nu}
--                      c^lambda_{delta, mu, nu}  sp_mu x sp_nu,
--     where delta ranges over partitions each of whose columns has
--     even length (equivalently, every part of delta has even
--     multiplicity).
--
--   O(m+n) down to O(m) x O(n):
--     o_lambda  |--> sum_{delta, mu, nu}
--                      c^lambda_{delta, mu, nu}  o_mu x o_nu,
--     where delta ranges over partitions all of whose parts are even.
--
-- Partitions mu, nu exceeding the ranks of the respective factor rings
-- are collapsed via the Sam-Snowden-Weyman modification rule
-- (@TO modificationRule@): a single (mu', nu') may appear with a
-- signed integer multiplicity, or (mu, nu) may be suppressed entirely.

-- Partitions of n all of whose parts appear with even multiplicity
-- (conjugate to partitions-of-n-with-even-parts).
partitionsPartsInPairs = (n) -> (
     if n == 0 then return {{}};
     if odd n then return {};
     apply(partitions(n // 2), p -> flatten apply(toList p, e -> {e, e}))
     )

-- Partitions of n all of whose parts are even.
partitionsAllPartsEven = (n) -> (
     if n == 0 then return {{}};
     if odd n then return {};
     apply(partitions(n // 2), p -> apply(toList p, e -> 2*e))
     )

-- Triple Littlewood-Richardson coefficient c^lambda_{a,b,c}
--   = coefficient of s_lambda in s_a * s_b * s_c (in any GL Schur ring).
-- Returns 0 if any partition does not fit the ring's rank.
tripleLRCoeff = (lambda, a, b, c, T) -> (
     nT := numgens T;
     if nT =!= infinity and (#a > nT or #b > nT or #c > nT) then return 0;
     prod := T_(toList a) * T_(toList b) * T_(toList c);
     cf := 0;
     lam := toList lambda;
     for t in listForm prod do
	  if toList first t == lam then cf = last t;
     cf
     )

-- Apply the modification rule to a partition `mu` for a finite-rank
-- target ring S; returns (mu', sign) with sign = 0 meaning "suppressed".
-- For stable targets (numgens infinity) this is the identity.
applyBranchMod = (mu, S) -> (
     n := numgens S;
     if n === infinity then return (toList mu, 1);
     kind := if S.?GroupActing then S.GroupActing else "GL";
     if kind == "GL" then (
	  if #mu > n then return (toList mu, 0) else return (toList mu, 1);
	  );
     if kind == "Sp" then (
	  rSp := modificationRule(toList mu, n, "C");
	  if rSp === null then return (toList mu, 0);
	  return rSp;
	  );
     if kind == "O" then (
	  odd0 := if S.?OddOrEven then S.OddOrEven else "Odd";
	  typ := if odd0 == "Odd" then "B" else "D";
	  rO := modificationRule(toList mu, n, typ);
	  if rO === null then return (toList mu, 0);
	  return rO;
	  );
     error("applyBranchMod: unknown kind " | toString kind)
     )

-- Branch a single basis partition lambda of type `kind` using King's rule,
-- into pairs (mu, nu) of partitions indexing the factor rings S1, S2.
-- Returns a MutableHashTable with entries (mu, nu) -> integer.
branchBasisPartition = (lambda, kind, S1, S2, T) -> (
     lam := toList lambda;
     d := sum lam;
     result := new MutableHashTable;
     addEntry := (mu, nu, c) -> (
	  key := (toList mu, toList nu);
	  if result#?key then result#key = result#key + c
	  else result#key = c;
	  );
     deltas := if kind == "GL" then {{}}
	       else if kind == "Sp" then
		    flatten for de from 0 to d list partitionsPartsInPairs de
	       else if kind == "O" then
		    flatten for de from 0 to d list partitionsAllPartsEven de
	       else error("branch: unsupported kind " | toString kind);
     m := numgens S1;
     n := numgens S2;
     for delta in deltas do (
	  rem := d - sum delta;
	  for k from 0 to rem do (
	       parsL := toList \ partitions k;
	       parsR := toList \ partitions (rem - k);
	       for mu in parsL do for nu in parsR do (
		    c := tripleLRCoeff(lam, delta, mu, nu, T);
		    if c == 0 then continue;
		    -- Apply modification rules for finite ranks
		    muMod := applyBranchMod(mu, S1);
		    if muMod#1 == 0 then continue;
		    nuMod := applyBranchMod(nu, S2);
		    if nuMod#1 == 0 then continue;
		    addEntry(muMod#0, nuMod#0, c * muMod#1 * nuMod#1);
		    );
	       );
	  );
     -- Drop zero entries
     for k in keys result do if result#k == 0 then remove(result, k);
     result
     )

branch = method()

-- Branch an element of a SchurRing with respect to a two-factor restriction.
-- S1 and S2 must have the same GroupActing as the input ring.  Returns a
-- HashTable mapping (mu, nu) -> coefficient.  The coefficients lie in the
-- coefficient ring of the input ring (in particular over QQ for stable
-- rings).
branch(RingElement, SchurRing, SchurRing) := HashTable => (f, S1, S2) -> (
     R := ring f;
     if class R =!= SchurRing then
	  error "branch: input must live in a SchurRing";
     gk := if R.?GroupActing then R.GroupActing else "GL";
     g1 := if S1.?GroupActing then S1.GroupActing else "GL";
     g2 := if S2.?GroupActing then S2.GroupActing else "GL";
     if g1 =!= gk or g2 =!= gk then error(
	  "branch: all three rings must share GroupActing (got "
	       | gk | ", " | g1 | ", " | g2 | ")");
     nR := numgens R;
     n1 := numgens S1;
     n2 := numgens S2;
     -- Sanity check on total rank (skip if any is stable)
     if nR =!= infinity and n1 =!= infinity and n2 =!= infinity then (
	  if n1 + n2 =!= nR then error(
	       "branch: numgens(S1) + numgens(S2) must equal numgens of ring f ("
		    | toString n1 | " + " | toString n2 | " != " | toString nR | ")");
	  );
     lfF := listForm f;
     d := if #lfF == 0 then 0
	  else max apply(lfF, t -> sum toList first t);
     helperSize := max(d, 1);
     T := plethysmHelperOf(R, helperSize);
     result := new MutableHashTable;
     addEntry := (key, v) -> (
	  if result#?key then result#key = result#key + v
	  else result#key = v;
	  );
     for term in lfF do (
	  lambda := toList first term;
	  coef := last term;
	  partResult := branchBasisPartition(lambda, gk, S1, S2, T);
	  for k in keys partResult do addEntry(k, coef * partResult#k);
	  );
     for k in keys result do if result#k == 0 then remove(result, k);
     new HashTable from result
     )

-- Convenience: branch(f, m, n) defaults to anonymous factor rings of ranks
-- m and n with the same GroupActing (and OddOrEven, for O) as ring f.
branch(RingElement, ZZ, ZZ) := HashTable => (f, m, n) -> (
     R := ring f;
     gk := if R.?GroupActing then R.GroupActing else "GL";
     sym1 := getSymbol "f1";
     sym2 := getSymbol "f2";
     oddTag := if R.?OddOrEven then R.OddOrEven else "Odd";
     S1 := if gk == "O"
	   then schurRing(coefficientRing R, sym1, m, GroupActing => "O", OddOrEven => oddTag)
	   else schurRing(coefficientRing R, sym1, m, GroupActing => gk);
     S2 := if gk == "O"
	   then schurRing(coefficientRing R, sym2, n, GroupActing => "O", OddOrEven => oddTag)
	   else schurRing(coefficientRing R, sym2, n, GroupActing => gk);
     branch(f, S1, S2)
     )

---------------------------------------------------------------
----------- End King branching formulas ------------------------
---------------------------------------------------------------

-- recTrans: recursive transform from the h-basis to the Schur basis.
--   Horner-style expansion by the leading h-variable lead = h_i (i maximal
--   appearing in pl).  Writing pl = sum_k coe#k * lead^fdeg#k (with fdeg
--   strictly decreasing), we accumulate
--       rez = ((...((0 * s_i + recTrans(coe#0)) * s_i) + recTrans(coe#1)) ...)
--   where the product "*" is dispatched by auxRecTransOp:
--       * for GL/SL/Sp/O rings (ordinary multiplication in the SchurRing),
--       ** for Sn rings (inner tensor product).
--   Each coe#k is itself recursively rewritten by recTrans (it lives in a
--   SchurRing of lower schurLevel, or ultimately at the base case below).
--   At the base case (lead === null, i.e. pl has no remaining h-variables),
--   retFcn lifts pl into the coefficient ring and calls toS there.
recTrans = method()
recTrans (RingElement) := (pl) ->
(
-- lead = leading h-variable = h_i with i maximal, or null if pl is a scalar
     lead := leadTermFcn pl;
     if lead === null then retFcn pl else
     (
	  -- read the multiplication op off the ring: * for GL/SL/Sp/O, ** for Sn
          auxRecTransOp := (schurRing ring pl).recTransOp;
-- monomials/coefficients of pl viewed as a polynomial in lead
	  (mon,coe) := coefficients(pl,Variables=>{lead});
	  mon = flatten entries mon;
	  coe = flatten entries coe;
     	  rez := 0;
	  cdeg := degree(lead,mon#0)+1;
	  for i from 0 to #mon-1 do
	  (
	       fdeg := degree(lead,mon#i);
	       -- fill in "gaps" in the Horner accumulator: bring cdeg down to
	       -- fdeg+1 by multiplying rez by s_i once per missing power
	       while (cdeg>fdeg+1) do
	       (
		    cdeg = cdeg - 1;
		    rez = auxRecTransOp(rez, mappingFcn(lead));
		    );
	       rez = auxRecTransOp(rez, mappingFcn(lead)) + recTrans(coe#i);
	       cdeg = cdeg - 1;
	       );
	  -- trailing Horner steps for any powers of lead below the smallest fdeg
	  while cdeg>0 do
	       (
		    cdeg = cdeg - 1;
		    rez = auxRecTransOp(rez, mappingFcn(lead));
		    );
	  rez
     	  )
     )

recTrans(Thing) := p -> p

--------
--------
--given a recursive relation for a sequence a_n, given by a convolution of (a_n) with (L_n)
--convolve computes formulas for a_n in terms of L_n
--the main routine is coded in the engine
--the value of conv is used to indicate one of several types of convolution
convolve = method()
convolve(List,ZZ) := (L,conv) -> (
     A := ring L_0;
     toList drop(apply(rawConvolve(L/raw//toSequence, conv), f -> new A from f),1)
     )

--a_n = p_n
--L_n = e_n
PtoE = (m,R) -> (
     n := R.dim;
     A := R.symRingForE;
     p2e := prepend(1_A, for i from 1 to n list ((-1)^(i+1) * A_(2*n+i-1)));
     if m>n then p2e = join(p2e,toList((m-n):0_A));
     R.PtoETable = if n == 0 then {1_A} else {1_A} | (- convolve(p2e,2));
     )

--a_n = h_n
--L_n = e_n
HtoE = (m,R) -> (
     n := R.dim;
     A := R.symRingForE;
     h2e := prepend(1_A, for i from 1 to n list (-1)^(i+1)*A_(2*n+i-1));
     R.HtoETable = if n == 0 then {1_A} else {1_A} | convolve(h2e,0);
     )

--a_n = h_n
--L_n = p_n
HtoP = (m,R) -> (
     n := R.dim;
     A := R.symRingForP;
     h2p := prepend(1_A, for i from 1 to n list A_(2*n+i-1));
     R.HtoPTable = if n == 0 then {1_A} else {1_A} | convolve(h2p,1);
     )

--a_n = e_n
--L_n = p_n
EtoP = (m,R) -> (
     n := R.dim;
     A := R.symRingForP;
     e2p := prepend(1_A, for i from 1 to n list (-1)^(i+1)*A_(2*n+i-1));
     R.EtoPTable = if n == 0 then {1_A} else {1_A} | convolve(e2p,1);
     )

--a_n = p_n
--L_n = h_n
PtoH = (m,R) -> (
     n := R.dim;
     A := R;
     p2h := prepend(1_A, for i from 1 to n list (- A_(2*n+i-1)));
     R.PtoHTable = if n == 0 then {1_A} else {1_A} | convolve(p2h,2);
     )

--a_n = e_n
--L_n = h_n
EtoH = (m,R) -> (
     n := R.dim;
     A := R;
     e2h := prepend(1_A, for i from 1 to n list (-1)^(i+1)*A_(2*n+i-1));
     R.EtoHTable = if n == 0 then {1_A} else {1_A} | convolve(e2h,0);
     )


---------------------------------------------------------------
--------------End transition-----------------------------------
---------------------------------------------------------------

---------------------------------------------------------------
-------------Schur Resolutions---------------------------------
---------------------------------------------------------------

--recsyz is a recursive method that takes as input an element el of a SchurRing of positive schurLevel
--and returns the sum of the terms having negative coefficients
--it is used in the routine schurRes to determine representations that are forced to be generators
--of syzygy modules in an equivariant resolution
recsyz = method()
recsyz (Thing) := (el) -> min(el,0)
recsyz (RingElement) := (el) ->
(
     T := ring el;
     listForm el/((u,v)->T_u*recsyz(v))//sum
     )

-----------------------------------------------------------------------------
-- schurResolution
-----------------------------------------------------------------------------
-- Computes the Schur-character data of the minimal free resolution of a
-- GL-equivariant graded module M over a symmetric algebra Sym(V).
--
-- Inputs:
--   rep  : a Schur-ring element giving the GL-character of the representation
--          V (typically s_{(1)}, the defining representation).
--   M    : {M_0, M_1, ..., M_{d-1}} -- the Schur characters of the graded
--          pieces of the module M in internal degrees 0, 1, ..., d-1.
--   plets: (in the (rep,M,plets) variant) the pre-computed list
--          [Sym^0 rep, Sym^1 rep, ..., Sym^degreeBound rep] of symmetric
--          powers of V, i.e. the graded characters of Sym(V). In the
--          (rep,M) variant these are computed on the fly.
--
-- Options:
--   DegreeLimit  : the highest internal degree to resolve to (defaults to
--                  #M - 1 when 0 is passed).
--   SyzygyLimit  : the highest syzygy index to compute (0 means: keep going
--                  until a syzygy step produces no new generators).
--
-- Output:
--   A list of lists {gens_0, gens_1, ...}, where gens_k is a list of pairs
--   (degree, character) giving the generators of the k-th syzygy module of
--   the equivariant minimal free resolution of M. Equivalently, this is the
--   Schur-character refinement of the equivariant Betti table.
-----------------------------------------------------------------------------

schurResolution = method(Options => {DegreeLimit => 0, SyzygyLimit => 0})
schurResolution(RingElement,List) := opts -> (rep,M) ->
(
     degreeBound := opts.DegreeLimit;
     if degreeBound == 0 then degreeBound = #M-1;
     syzygyBound := opts.SyzygyLimit;

     T := ring rep;
     n := schurLevel T;
--symPowersOfRep = [Sym^0 rep, Sym^1 rep, ..., Sym^degreeBound rep],
--i.e. the graded characters of Sym(V); pre-computed once and passed to schurRes.
     symPowersOfRep := new MutableList;
     symPowersOfRep#0 = 1_T;
     for i from 1 to degreeBound do symPowersOfRep#i = symmetricPower(i,rep);

     schurRes(rep,M,new List from symPowersOfRep,DegreeLimit => degreeBound,SyzygyLimit => syzygyBound)
     )

schurResolution(RingElement,List,List) := opts -> (rep,M,plets) ->
(
     degreeBound := opts.DegreeLimit;
     if degreeBound == 0 then degreeBound = #M-1;
     syzygyBound := opts.SyzygyLimit;

     schurRes(rep,M,plets,DegreeLimit => degreeBound,SyzygyLimit => syzygyBound)
     )

schurRes = method(Options => options schurResolution)
schurRes(RingElement,List,List) := opts -> (rep,M,plets) ->
(
-----------------------------------------------------------------------------
-- schurRes -- the actual worker for schurResolution.
--
-- Iterative covering algorithm. Maintains a "residue" moduleResidue#i: the
-- part of the character in internal degree i that has not yet been realized
-- by the resolution so far. At syzygy step k, for each degree i we:
--   (1) Lift the current k-th syzygy generators into degree i by multiplying
--       each generator's character by the appropriate sym power of V:
--         degreeICover = sum_{sy in syzy#k, sy#0 <= i} plets#(i - sy#0) * sy#1
--   (2) Subtract the outstanding residue:   degreeICover -= moduleResidue#i.
--   (3) Extract the negative-coefficient part via recsyz -- these are the
--       irreducibles the current syzygies fail to cover, which must become
--       fresh syzygy generators at this step.
--   (4) Record (i, -newSyzyPart) as a generator of syzy#k if nonzero.
--   (5) Update moduleResidue#i to reflect what has just been absorbed.
-- Stop either after reaching syzygyBound, or (when syzygyBound == 0) as
-- soon as a syzygy step introduces no new generators.
-----------------------------------------------------------------------------
     T := ring rep;
     degreeBound := opts.DegreeLimit;
     syzygyBound := opts.SyzygyLimit;

--moduleResidue#i = the degree-i character still needing to be covered by
--the differential in the equivariant complex; starts as M, padded with
--zeros out to degreeBound.
     moduleResidue := new MutableList from (M | toList((degreeBound+1-#M):0));
     moreToCompute := true;
     syzStep := 0;
--syzygyChars#k = list of (degree, character) pairs for the generators of
--the k-th syzygy module.
     syzygyChars := new MutableList;
     syzygyChars#syzStep = {};
     local degreeICover;
     local newSyzyPart;

--syzygy modules are constructed step by step; stop either on reaching the
--syzygyBound limit, or on finding no new syzygies at a given step.
     while moreToCompute do
     (
	  for i from 0 to degreeBound do
	  (
--(1) Lift the current k-th syzygy generators into degree i.
     	       degreeICover = 0_T;
	       for sy in syzygyChars#syzStep do
	       	    if sy#0 <= i then degreeICover = degreeICover + plets#(i-sy#0) * sy#1
		    else break;
--(2) Subtract the residue. degreeICover must cover moduleResidue#i, i.e.
--there must be a surjection of representations from degreeICover onto
--moduleResidue#i; negative coefficients in the difference signal failure.
	       degreeICover = degreeICover - moduleResidue#i;
--(3) recsyz (defined above) extracts the sum of negative-coefficient
--terms -- exactly the irreducibles that must be covered by fresh syzygies.
	       newSyzyPart = recsyz(degreeICover);
--(4) Record (i, -newSyzyPart) as a new generator of syzy#k if nonzero.
	       if newSyzyPart != 0 then syzygyChars#syzStep = syzygyChars#syzStep | {(i,-newSyzyPart)};
--(5) Update the residue to reflect what has just been absorbed.
	       moduleResidue#i = degreeICover - newSyzyPart;
	       );
--Stopping condition: syzygyBound == 0 means "no user limit" -- keep going
--until a syzygy step produces no new generators; otherwise stop once
--syzStep reaches the requested syzygyBound.
     	  if syzygyBound == 0 then moreToCompute = not (syzygyChars#syzStep == {})
	  else moreToCompute = (syzStep<syzygyBound);
     	  syzStep = syzStep + 1;
	  syzygyChars#syzStep = {};
 	  );
     select(toList syzygyChars,i-> i != {})
     )
					      
---------------------------------------------------------------
-------------end Schur Resolutions-----------------------------
---------------------------------------------------------------


---------------------------------------------------------------
--------------Characters of Symmetric Group--------------------
---------------------------------------------------------------

--given a partition lambda as a nonincreasing sequence of positive integers
--seqToMults returns the representation of this partition as a sequence
--of multiplicities: rez#i is the number of parts of lambda of size (i+1)
seqToMults = method()
seqToMults(List) := (lambda) ->
(
     lam := new Partition from lambda;
     aux := toList(conjugate lam)|{0};
     rez := {};
     for j from 0 to #aux-2 do
     (
     	  dif := aux#j-aux#(j+1);
       	  rez = rez | {dif};
	  );
     rez 
     )

-------------------------------------------------------------------------------
-- Class functions and the Frobenius characteristic map
-------------------------------------------------------------------------------
-- The Frobenius characteristic is the isomorphism
--
--      ch : R(S_n) ----> Lambda^n
--
-- between the ring of virtual characters of the symmetric group S_n and the
-- degree-n component of the ring of symmetric functions. Explicitly,
--
--      ch(chi) = (1/n!) sum_{sigma in S_n} chi(sigma) p_{cycletype(sigma)}
--              = sum_{lambda |- n} (chi(lambda) / z_lambda) p_lambda,
--
-- where z_lambda = centralizerSize(lambda) is the order of the centralizer
-- in S_n of any permutation of cycle type lambda; equivalently, writing
-- m_i for the number of parts of lambda equal to i,
--
--      z_lambda = prod_i i^{m_i} * m_i!.
--
-- A ClassFunction is stored as a HashTable keyed by partitions (encoded as
-- Sequences of parts in weakly-decreasing order); the value chi#lambda is
-- the character value on the conjugacy class of cycle type lambda. If a
-- symmetric function is written in the power-sum basis as
--
--      f = sum_lambda (c_lambda / z_lambda) p_lambda,
--
-- then the matching class function is chi#lambda = c_lambda.
-------------------------------------------------------------------------------

--given a partition lambda represented in as a sequence of multiplicities mults
--where mults#i is the number of parts of lambda of size (i+1)
--multsToSeq represents lambda as a nonincreasing sequence of positive integers
--(inverse of seqToMults, defined elsewhere)
multsToSeq = method()
multsToSeq(List) := (mults) ->
(
     n := #mults;
     par := {};
     for i from 0 to n-1 do
         par = par | splice{mults#i:(i+1)};
     reverse par
     )

--the size z_lambda of the centralizer in S_n of a permutation of cycle type
--lambda (passed here in multiplicity form: lambda#i = m_{i+1}):
--      z_lambda = prod_i i^{m_i} * m_i!
centralizerSize = method()
centralizerSize(List) := lambda ->
(
     product for i from 0 to #lambda-1 list((i+1)^(lambda#i)*(lambda#i)!)
     )

keysCF := method()
keysCF(ClassFunction) := (cF) -> keys cF

degree(ClassFunction) := ch ->
(
     ke := keysCF ch;
     if #ke == 0 then -1 else sum(first ke)
     )

-------------------------------------------------------------------------------
-- Frobenius maps: classFunction (Lambda^n -> R(S_n))
--                 symmetricFunction (R(S_n) -> Lambda^n)
-------------------------------------------------------------------------------

--go from symmetric functions to class functions:
--given f = sum_lambda (c_lambda / z_lambda) p_lambda, return chi with
--chi#lambda = c_lambda. We read f in the p-basis and, for each p-monomial,
--recover lambda from the exponent vector and store coeff * z_lambda.
classFunction = method()
classFunction(RingElement) := (f)->
(
     Rf := ring f;

     R := symmetricRing Rf;
     pf := toP f;
     n := R.dim;

     if (degree pf)#0 > n then error"Can't interpret ring element as a symmetric function";

     (mon,coe) := apply(coefficients pf,i->flatten entries i);
     ch := new MutableHashTable;
     for j from 0 to #mon-1 do
     (
	  -- The symmetricRing has 3n generators laid out as the e-, p-, and
	  -- h-blocks in that order. Indices n..2n-1 pick out the p-block, so
	  -- this slice reads the exponents of p_1, p_2, ..., p_n in mon#j,
	  -- i.e. the multiplicity form of the partition lambda.
     	  degs := (flatten exponents mon#j)_{(n)..(2*n-1)};
     	  par := multsToSeq(degs);
	  ch#par = lift(coe#j,coefficientRing R) * centralizerSize(degs);
	  );
     new ClassFunction from ch
     )

classFunction(BasicList) := (lambda)->
(
     lam := toList(lambda);
     s := symbol s;
     R := schurRing(QQ,s,sum lam);
     classFunction(R_lam)
     )

--go from class functions to symmetric functions:
--apply the Frobenius formula ch(chi) = sum_lambda (chi#lambda / z_lambda) p_lambda.
symmetricFunction = method()
symmetricFunction(ClassFunction,Ring) := (ch,S)->
(
     R := symmetricRing S;
     rez := 0_R;
     n := R.dim;
     for lam in keysCF ch do
     	  rez = rez + ch#lam * (product for i from 0 to #lam-1 list R.pVariable(lam#i)) / centralizerSize(seqToMults lam);
     if instance(S, SchurRing) then toS rez else rez
     )

-------------------------------------------------------------------------------
-- ClassFunction arithmetic (pointwise on conjugacy classes)
-------------------------------------------------------------------------------

ClassFunction + ClassFunction := (ch1,ch2)->
(
     clSum := new MutableHashTable;
     l1 := sum((keysCF ch1)#0);
     l2 := sum((keysCF ch2)#0);
     if l1 != l2 then error("The symmetric functions/characters must have the same degree");
     for i in unique(keysCF(ch1)|keysCF(ch2)) do
     	  (
	       a := b := 0;
	       if ch1#?i then a = ch1#i;
	       if ch2#?i then b = ch2#i;
	       if (a+b != 0) then clSum#i = a + b;
	       );
     new ClassFunction from clSum
     )

RingElement * ClassFunction := Number * ClassFunction := (n,ch) ->
(
     clProd := new MutableHashTable;
     for i in keysCF ch do clProd#i = n*ch#i;
     new ClassFunction from clProd
     )

ClassFunction * RingElement := ClassFunction * Number := (ch,n) -> n*ch;


ClassFunction - ClassFunction := (ch1,ch2)-> ch1 + (-1)*ch2;

ClassFunction == ClassFunction := (ch1,ch2) ->
(
     equ := true;
     for i in unique(keysCF ch1 | keysCF ch2) do
     	  if not ((not ch1#?i and not ch2#?i) or (ch1#?i and ch2#?i and ch1#i == ch2#i)) then
     	  (
	       equ = false;
	       break;
	       );
     equ
     )

-------------------------------------------------------------------------------
-- Scalar (Hall) product and internal (Kronecker) product
-------------------------------------------------------------------------------

--Hall inner product on Lambda: <f,g> = sum_lambda chi_f(lambda) chi_g(lambda) / z_lambda.
--On irreducibles (Schur functions) this counts common constituents: <s_lambda, s_mu> = delta_{lambda,mu}.
scalarProduct = method()
scalarProduct(ClassFunction,ClassFunction) := (ch1,ch2)->
(
     scProd := 0;
     chProd := internalProduct(ch1,ch2);
     for i in keysCF(chProd) do
     	  scProd = scProd + chProd#i / centralizerSize(seqToMults i);
     scProd
     )

scalarProduct(RingElement,RingElement) := (f1,f2)->
(
     ch1 := classFunction f1;
     ch2 := classFunction f2;
     scalarProduct(ch1,ch2)
     )

--Internal (Kronecker) product: pointwise product of class functions on each
--conjugacy class, corresponding under Frobenius to the tensor product of
--S_n-representations. On symmetric functions it is transported through the
--Frobenius isomorphism classFunction / symmetricFunction.
internalProduct = method()
ClassFunction * ClassFunction :=
internalProduct(ClassFunction,ClassFunction) := (ch1,ch2)->
(
     iProd := new MutableHashTable;
     l1 := sum((keysCF ch1)#0);
     l2 := sum((keysCF ch2)#0);
     if l1 == 0 then return(ch1#{} * ch2);
     if l2 == 0 then return(ch2#{} * ch1);
     if l1 != l2 then error("The symmetric functions/characters must have the same degree");
     for i in keysCF(ch1) do
     	  if ch2#?i then iProd#i = ch1#i * ch2#i;
     new ClassFunction from iProd
     )

internalProduct(RingElement,RingElement) := (f1,f2)->
(
     R2 := ring f2;
     R := local R;
     issy := false;
     if (class R2 =!= SchurRing) then issy = true;
     R = symmetricRing ring f2;
     ch1 := classFunction f1;
     ch2 := classFunction f2;
     rez := symmetricFunction(internalProduct(ch1,ch2),R);
     if issy then rez else
     toS rez
     )

-*
chi(BasicList,BasicList) := (lambda, rho) ->
(
     la := toList lambda;
     rh := toList rho;
     ll := sum la;
     if ll != sum(rh) then error"Partitions must have the same size.";
     R := symmetricRing(QQ,ll);
     sl := jacobiTrudi(la,R);
     pr := 1_R;
     for i from 0 to #rh-1 do pr = pr * R_(ll-1+rh#i);
     scalarProduct(sl,pr)
     )
*-
---------------------------------------------------------------
--------------End characters-----------------------------------
---------------------------------------------------------------

--------------------------------
-- Dimension -------------------
--------------------------------
-- Function to compute the dimension of a virtual representation

hooklengths = (lambda) -> (
     mu := conjugate lambda;
     product for i from 0 to #lambda-1 list (
	  product for j from 0 to lambda#i-1 list (
	       lambda#i + mu#j - i - j -1
	       ))
     )

-- Dispatch table for dimension formulas, extensible for Sp/O
dimFormulaTable = new MutableHashTable from {
     "GL" => (n, lambda, powers, base) -> (
     	  if not instance(n,ZZ) then n = hold n;
	  num := product for s from 0 to #powers-1 list (n + (base+s))^(powers#s);
     	  num/hooklengths(new Partition from lambda)
	  ),
     "Sn" => (n, lambda, powers, base) -> (
     	  (sum toList lambda)! / hooklengths(new Partition from lambda)
	  ),
     -- Weyl dimension formula for Sp(2n), type C_n, highest weight lambda
     -- with at most n parts.  Returns 0 if lambda has more than n parts.
     -- Formula:  dim V_lambda
     --   = prod_{1<=i<j<=n} (l_i - l_j)(l_i + l_j) / ((j-i)(2n+2-i-j))
     --   * prod_{1<=i<=n} l_i / (n+1-i)
     -- where l_i = lambda_i + n + 1 - i (pad lambda with zeros to length n).
     "Sp" => (n, lambda, powers, base) -> (
	  if not instance(n,ZZ) then error "Sp dim formula requires an integer n";
	  r := #lambda;
	  if r > n then return 0;
	  N := n;
	  L := for i from 1 to N list ((if i <= r then lambda#(i-1) else 0) + N + 1 - i);
	  num := 1; den := 1;
	  for i from 1 to N do (
	       num = num * L#(i-1);
	       den = den * (N + 1 - i);
	       for j from i+1 to N do (
		    num = num * (L#(i-1) - L#(j-1)) * (L#(i-1) + L#(j-1));
		    den = den * (j - i) * (2*N + 2 - i - j);
		    );
	       );
	  num / den
	  ),
     -- Weyl dimension formula for O(2n+1), type B_n.  Positive roots
     -- e_i - e_j, e_i + e_j (1<=i<j<=n) and e_i (1<=i<=n).  rho =
     -- (n-1/2, n-3/2, ..., 1/2) so L_i = 2*lambda_i + 2n+1-2i clears the
     -- half-integers.
     "O_Odd" => (n, lambda, powers, base) -> (
	  if not instance(n,ZZ) then error "O dim formula requires an integer n";
	  r := #lambda;
	  if r > n then return 0;
	  N := n;
	  L := for i from 1 to N list (2*(if i <= r then lambda#(i-1) else 0) + 2*N + 1 - 2*i);
	  num := 1; den := 1;
	  for i from 1 to N do (
	       num = num * L#(i-1);
	       den = den * (2*N + 1 - 2*i);
	       for j from i+1 to N do (
		    num = num * (L#(i-1) - L#(j-1)) * (L#(i-1) + L#(j-1));
		    den = den * (2*j - 2*i) * (4*N + 2 - 2*i - 2*j);
		    );
	       );
	  num / den
	  ),
     -- Weyl dimension formula for SO(2n), type D_n.  Positive roots are
     -- e_i +/- e_j (1<=i<j<=n).  rho = (n-1, n-2, ..., 0) is integral, so
     -- l_i = lambda_i + n - i stays integral too.
     --
     -- For partitions with lambda_n > 0 this gives the dimension of a single
     -- SO(2n) irrep; the corresponding O(2n) irrep induced from it has the
     -- same dimension (we parametrize O(2n) irreps by partitions of length
     -- <= n, matching Koike-Terada stable limits).
     "O_Even" => (n, lambda, powers, base) -> (
	  if not instance(n,ZZ) then error "O dim formula requires an integer n";
	  r := #lambda;
	  if r > n then return 0;
	  N := n;
	  if N == 0 then return 1;  -- trivial group
	  L := for i from 1 to N list ((if i <= r then lambda#(i-1) else 0) + N - i);
	  num := 1; den := 1;
	  for i from 1 to N do (
	       for j from i+1 to N do (
		    num = num * (L#(i-1) - L#(j-1)) * (L#(i-1) + L#(j-1));
		    den = den * (j - i) * (2*N - i - j);
		    );
	       );
	  num / den
	  ),
     -- Legacy alias: plain "O" defaults to B_n (matches pre-split behavior).
     "O" => (n, lambda, powers, base) -> (
	  if not instance(n,ZZ) then error "O dim formula requires an integer n";
	  r := #lambda;
	  if r > n then return 0;
	  N := n;
	  L := for i from 1 to N list (2*(if i <= r then lambda#(i-1) else 0) + 2*N + 1 - 2*i);
	  num := 1; den := 1;
	  for i from 1 to N do (
	       num = num * L#(i-1);
	       den = den * (2*N + 1 - 2*i);
	       for j from i+1 to N do (
		    num = num * (L#(i-1) - L#(j-1)) * (L#(i-1) + L#(j-1));
		    den = den * (2*j - 2*i) * (4*N + 2 - 2*i - 2*j);
		    );
	       );
	  num / den
	  )
}

dimSchur = method(Options => {GroupActing => "GL"})
dimSchur(Thing,List) := opts -> (n, lambda) -> (
     -- lambda is a list {a0,a1,...,a(r-1)}, a0 >= a1 >= ... >= a(r-1) > 0
     -- n can be a number or a symbol
     powers := new MutableList from toList((if lambda#?0 then lambda#0 else 0) + #lambda - 1 : 0);
     base := 1 - #lambda;
     for i from 0 to #lambda-1 do
       for j from 0 to lambda#i-1 do
       	    powers#(j-i-base) = powers#(j-i-base) + 1;
     ga := opts.GroupActing;
     if not dimFormulaTable#?ga then error("No dimension formula for GroupActing => " | toString ga);
     (dimFormulaTable#ga)(n, lambda, powers, base)
     )
dimSchur(Thing,SchurRingElement) := opts -> (n, F) -> (
     -- assumption: F is an element in a SchurRing of level 1
     if schurLevel(ring F) != 1 then error"Expected a list as input";
     L := listForm F;
     sum apply(L, p -> (
     	       lambda := p#0;
	       p#1 * dimSchur(n,lambda,opts)))
     )

dimSchur(List,SchurRingElement) := opts -> (lis, F) -> (
     -- assumption: F is an element in a SchurRing
     if #lis != schurLevel(ring F) then error"Input list has incorrect size";
     R := ring F;
     gr := R.GroupActing;
     -- Rational GL: the two layers (alpha outer, beta inner) together define
     -- a single GL(n) weight.  We bypass the standard recursion and instead
     -- apply the Weyl dim formula directly to the composite weight.
     if gr == "RatGL" then (
	  nOut := lis#0;
	  nIn  := if #lis >= 2 then lis#1 else nOut;
	  if nOut =!= nIn then
	       error("dimSchur: RatGL requires equal ranks for both layers; got "
		    | toString lis);
	  return dimRatGLList(nOut, F);
	  );
     -- Resolve O into O_Odd / O_Even via the ring's OddOrEven tag.  If the
     -- ring is stable (length-infinity), the choice of tag does not matter
     -- at the ring level but the specialization point n does: dimSchur is
     -- called with a specific n, so we honor whatever tag was set on R
     -- (default "Odd" for stable O).
     if gr == "O" then (
	  if R.?OddOrEven then gr = "O_" | R.OddOrEven
	  else gr = "O_Odd";
	  );
     L := listForm F;
     sum apply(L, p -> (
     	       lambda := p#0;
	       if instance(p#1,SchurRingElement) then dimSchur(drop(lis,1),p#1) * dimSchur(lis#0,lambda,GroupActing => gr)
                  else p#1 * dimSchur(lis#0,lambda,GroupActing => gr)))
     )

dimSchur(SchurRingElement) := opts -> (F) -> (
     schurdims := (S) -> (
	  if schurLevel S === 0 then {}
	  else prepend(numgens S, schurdims coefficientRing S));
     ns := schurdims ring F;
     if any(ns, i -> not instance(i,ZZ))
     then error "expected finitely generated Schur rings";
     dS := dimSchur(ns,F);
     if liftable(dS,ZZ) then lift(dS,ZZ) else dS
     )

-- Weyl dimension formula for GL(n) at a general integral highest weight w =
-- (w_1 >= w_2 >= ... >= w_n).  Works for weights whose parts can be negative
-- (i.e., rational characters of GL(n)), and reduces to the Schur hook-length
-- formula for dominant polynomial lambda when w_n >= 0.
dimGLweight = (n, w) -> (
     if n === 0 then return 1;
     -- Pad w with zeros to length n (dominant, non-increasing assumed).
     if #w > n then error("dimGLweight: weight has more parts than rank n = " | toString n);
     ww := for i from 0 to n-1 list (if i < #w then w#i else 0);
     -- Weyl dim formula: prod_{i<j} ((w_i - w_j) + (j - i)) / (j - i).
     num := 1; den := 1;
     for i from 0 to n-2 do
	  for j from i+1 to n-1 do (
	       num = num * ((ww#i - ww#j) + (j - i));
	       den = den * (j - i);
	       );
     num / den
     )

-- Dimension of a rational GL(n) irrep indexed by (alpha, beta).  Returns 0 if
-- ell(alpha) + ell(beta) > n.
dimRatGLBasis = (n, alpha, beta) -> (
     p := #alpha; q := #beta;
     if p + q > n then return 0;
     w := for i from 1 to n list (
	       (if i <= p then alpha#(i-1) else 0)
	       - (if i > n - q then beta#(n - i) else 0)
	       );
     dimGLweight(n, w)
     )

-- Dimension of a general element F of a RatGL ring at rank n, summed over
-- (alpha, beta, scalar) triples.  Used by dimSchur(List, SchurRingElement)
-- when gr == "RatGL".
dimRatGLList = (n, F) -> (
     acc := 0;
     iterateRatGLTerms(F, (alpha, beta, scalar) -> (
	       d := dimRatGLBasis(n, alpha, beta);
	       acc = acc + scalar * d;
	       ));
     acc
     )
---------------------------------------------------------------
--------End dimension----------------------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
-----------Partitions-related functions------------------------
---------------------------------------------------------------
--this part might have to be moved elsewhere
--since it's not directly connected to the package
parts := (d, n) -> (
     -- d is an integer >= 0
     -- n is an integer >= 1
     -- returns a list of all of the partitions of d
     --    having <= n parts.
     x := partitions(d);
     select(x, xi -> #xi <= n))     

-------Generate all the partitions of a set
-------with a given shape
locS = local locS;
locL = local locL;
locLengthL = local locLengthL;
locParts = local locParts;
locPartitions = local locPartitions;
locind = local locind;
genPartitions = local genPartitions;

genPartitions = method()
genPartitions(ZZ) := (k)->
(
     if k==length locS then (locind = locind + 1;locPartitions#locind = set toList locParts) else
     (
     for i from 0 to locLengthL-1 do
     	  if (i==0 and #locParts#0 < locL#0) or (((locL#(i-1)>locL#i) or (#locParts#(i-1)>0)) and (#locParts#i<locL#i)) then
	  (
	       locParts#i = locParts#i + set{locS#k};
	       genPartitions(k+1);
	       locParts#i = locParts#i - set{locS#k};
	       );
     )
);

partitions(Set,BasicList) := (S,L)->
(
     locS = toList S;
     locL = L;
     locLengthL = #L;
     locParts = new MutableList;
     for i from 0 to locLengthL-1 do locParts#i = set{};
     locPartitions = new MutableList;
     locind = -1;
     genPartitions(0);
     toList locPartitions
     )

--------end generate partitions

---------------------------------------------------------------
--------End partitions-related functions-----------------------
---------------------------------------------------------------

beginDocumentation()

undocumented {Schur}

doc ///
Key
  SchurRings
Headline
  Rings representing irreducible representations of general linear or symmetric groups
Description
  Text
    This package makes computations in the representation rings of general linear groups 
    and symmetric groups possible.
    
    Given a positive integer {\tt n} we may define a polynomial ring in {\tt n}
    variables over an arbitrary base ring , whose monomials correspond to the irreducible 
    representations of {\tt GL(n)}, and where multiplication is given by the decomposition of 
    the tensor product of representations. We create such a ring in Macaulay2 using the 
    @TO schurRing@ function.
  
  Example
    S = schurRing(QQ,s,4)
    R = schurRing(r,infinity)
    
  Text    
    Note that in the above, {\tt n} is allowed to be equal to {\tt \infty}. However, in this
    version of the package, many of the features from the case {\tt n} finite are missing
    from the infinite case, so the user is advised to use large values for {\tt n} as a
    substitute, whenever necessary.

    We determine the relative dimension of a SchurRing over its base using the @TO numgens@ function:

  Example 
    numgens S
    numgens R
   
  Text
    
    For {\tt k\leq n}, one may interpret the degree
    {\tt k} homogeneous component of a @TO SchurRing@ as the representation ring of the symmetric
    group {\tt S_k}. In this ring, the multiplication is different than the one in 
    the representation ring of {\tt GL(n)}. By default, the elements of a @TO SchurRing@ are 
    interpreted as (virtual) characters of 
    a general linear group. This interpretation is controlled by the option @TO GroupActing@,
    whose default value is "GL". To indicate that the elements of a Schur ring should
    be interpreted as characters of the symmetric group, one has to set the option @TO GroupActing@
    to "Sn".
  
  Example
    Q = schurRing(q,4,GroupActing => "Sn")
    
  Text
        
    A monomial in {\tt S} represents the irreducible representation with a given highest weight. 
    The standard {\tt GL(4)}-representation is
   
  Example
    V = s_1

  Text
    
    We may see the dimension of the corresponding irreducible representation using @TO dim@:

  Example
    dim V

  Text
    Multiplication of elements corresponds to tensor product of representations. The 
    value is computed using a variant of the Littlewood-Richardson rule.
  
  Example
    V * V
    V^3

  Text 
    
    The third symmetric power of {\tt V} is obtained by
     
  Example
    W = s_{3}
    dim W
   
  Text
    
    and the third exterior power of {\tt V} can be obtained using

  Example
    U = s_{1,1,1}
    dim U
    
  Text
  
    Alternatively, one can use the functions @TO symmetricPower@ and @TO exteriorPower@:
    
  Example
    W = symmetricPower(3,V)
    U = exteriorPower(3,V)
    
  Text
  
    We can in fact take symmetric powers and exterior powers of any representation:
    
  Example
    exteriorPower(2,W)
    symmetricPower(2,U)

  Text
  
    and compute even more general forms of @TO plethysm@:
    
  Example
     plethysm(W+U,W+U)
   
  Text
    
    Alternatively, we can use the binary operator @TO symbol \@ @ to compute plethysm:
  
  Example
    s_2 @ s_3
    (W+U) @ (W+U)

  Text
  
    All the above calculations assume that we're dealing with representations of {\tt GL(4)}.
    But as symmetric functions of degree three, {\tt W} and {\tt U}, can be thought of as characters of the
    symmetric group {\tt S_3}. Let us first ``move'' these symmetric functions into a Schur ring
    designed to deal with characters of symmetric groups (like the ring {\tt Q} defined
    above):
    
  Example
    W' = toS(W,Q)
    U' = toS(U,Q)
    
  Text
    
    Now {\tt W'} corresponds to the trivial representation of {\tt S_3},
    and {\tt U'} to the sign representation. As such, we can tensor them together using the
    function @TO internalProduct@, or the binary operator @TO symbol *@.
    
  Example
    W' * U'

  Text
  
    We can generate the class function corresponding to an {\tt S_n}-representation, using
    the function @TO classFunction@:
    
  Example
    cfW = classFunction(W')
    cfU = classFunction(U')
    
  Text
    
    We can multiply class functions together, and transform class functions into symmetric
    functions using the function @TO symmetricFunction@:
    
  Example
    cfWU = cfW * cfU
    symmetricFunction(cfWU,Q)
    
  Text
  
    The result of the previous computation is of course the same as that of taking the product
    of {\tt W'} and {\tt U'}.
    
    We can take exterior and symmetric powers of {\tt S_n}-representations, just as for
    {\tt GL}-modules (compare to {\tt o16} and {\tt o17}):
    
  Example
    exteriorPower(2,W')
    symmetricPower(2,U')

  Text
      
    We can write any symmetric function in terms of the standard {\tt e}- (elementary
    symmetric), {\tt h}- (complete) and {\tt p}- (power sum) bases, using the functions 
    @TO toE@, @TO toH@, @TO toP@ respectively:
    
  Example
    toE U
    toH U
    toP W
    
  Text
    
    These expressions live in the Symmetric ring associated to {\tt S}, which can be obtained
    using the function @TO symmetricRing@:
    
  Example
    A = symmetricRing S
    
  Text
  
    Similarly, any Symmetric ring has a Schur ring attached to it, which can be obtained using
    the function @TO schurRing@:
    
  Example
    schurRing A === S
  
  Text  
  
    We construct tensor products of Schur rings iteratively by allowing Schur rings over
    base rings that are also Schur rings:
    
  Example
    T = schurRing(S,t,3)
    
  Text
    
    The Schur ring {\tt T} can thus be thought of as the representation ring of 
    {\tt GL(V)\times GL(V')}, where {\tt V} is as before a vector space of dimension 
    {\tt 4}, and {\tt V'} is a vector space of dimension {\tt 3}. The representation 
    corresponding to {\tt V'} is
  
  Example
    V' = t_1
  
  Text
   
    The function @TO schurLevel@ indicates the number of Schur rings that have been
    tensored together to obtain any given ring:
    
  Example
    schurLevel T
    schurLevel S
    schurLevel QQ
    
  Text
    
    We can now check Cauchy's formula for decomposing symmetric/exterior powers of a
    tensor product:
    
  Example
    symmetricPower(3,V*V')
    exteriorPower(3,V*V')
  
  Text
  
    We end with the computation of the {\tt GL(n)}- and {\tt S_n}-equivariant resolutions
    of the residue field of a polynomial ring in {\tt n} variables. The function that does
    this calculation, @TO schurResolution@, is based on an empirical method, which gives
    the correct answer in surprisingly many situations.
    
    In the {\tt GL(n)} situation, we are resolving the residue field which as a representation
    has character {\tt 1_S}. The space of linear forms in the polynomial ring 
    considered as a {\tt GL}-representation has character {\tt V = s_1}.
    
  Example
    n = 4
    M = {1_S}
    schurResolution(V,M,DegreeLimit => n)
    
  Text
  
    Not surprisingly, the syzygy modules are generated by the exterior powers of {\tt V}.

    The residue field as a representation of the symmetric group {\tt S_n}
    has character {\tt s_n}. The space of linear forms in the polynomial ring 
    considered as an {\tt S_n}-representation coincides with the permutation representation
    of {\tt S_n}, thus has character {\tt s_n + s_{n-1,1}}.

  Example
    rep = q_n + q_(n-1,1)
    M = {q_n}
    sR = schurResolution(rep,M,DegreeLimit => n)
 
  Text
    
    We can check that the second syzygy module is generated by the second exterior power of the permutation
    representation.
    
  Example
    eP2rep = exteriorPower(2,rep)
    eP2rep == last sR#2#0

  Text

    {\bf Variant character rings.}  Beyond the general linear and
    symmetric groups, the package supports several other families of
    representation rings.  All of them are realized as @TO SchurRing@s,
    with behavior controlled by the option @TO GroupActing@ (and, for
    basis conversions, @TO Basis@).  The current values are:

    $\bullet$ {\tt "GL"} (default): polynomial representations of
    $GL_n$, with basis the Schur functions $s_\lambda$ and the
    Littlewood-Richardson product.

    $\bullet$ {\tt "Sn"}: the representation ring of the symmetric
    group $S_n$, with basis indexed by partitions and product the
    internal product (Kronecker product of $S_n$-characters).  See
    @TO internalProduct@.

    $\bullet$ {\tt "Sp"}: the (universal or finite-rank) character
    ring of the symplectic groups, with basis $sp_\lambda$ and product
    the Newell-Littlewood product.  See @TO toSp@.

    $\bullet$ {\tt "O"}: the (universal or finite-rank) character ring
    of the orthogonal groups, with basis $o_\lambda$.  The finite-rank
    ring distinguishes type $B$ and type $D$ via the option
    @TO OddOrEven@.  See @TO toO@.

    $\bullet$ {\tt "RatGL"}: the ring of rational (i.e. finite-dim'l)
    representations of $GL_n$, whose irreducibles are indexed by
    bipartitions $(\alpha,\beta)$.  See @TO toRatGL@.

    $\bullet$ {\tt "SL"}: the ring of polynomial representations of
    $SL_n$ (rows of length $n$ are killed).

    Orthogonally, the option @TO Basis@ $=>$ {\tt "Monomial"} replaces
    the Schur basis with the monomial symmetric functions $m_\lambda$;
    multiplication is implemented by round-tripping through the Schur
    basis using Kostka numbers (@TO kostkaNumber@, @TO toM@).

  Example
    Sp = schurRing(QQ, sp, 3, GroupActing => "Sp");
    sp_{1} * sp_{1}
    O = schurRing(QQ, o, 4, GroupActing => "O");
    o_{1} * o_{2}
    Rat = schurRing(QQ, r, 3, GroupActing => "RatGL");
    r_{{1},{1}} * r_{{1},{}}
    Mon = schurRing(QQ, m, 4, Basis => "Monomial");
    m_{1} * m_{1}

  Text

    {\bf Stable vs.\ finite-rank rings.}  Each of the above flavors
    comes in two sizes.  Passing a positive integer {\tt n} to
    @TO schurRing@ constructs the {\em finite-rank} ring, where
    partitions are restricted in length (to $n$ for {\tt GL}/{\tt Sn}
    and to the appropriate rank for {\tt Sp}/{\tt O}/{\tt RatGL}) and
    the Sam-Snowden-Weyman modification rules are applied
    automatically.  Passing {\tt infinity} instead constructs the
    {\em stable} (universal) ring: a polynomial ring with countably
    many generators, one for each partition, on which all operations
    are performed without modification.  This is the ring of universal
    characters of @TO2 {GroupActing,"Koike--Terada"}@ --- a single
    computation that specializes to every finite rank.

  Example
    StabGL = schurRing(QQ, sg, infinity);
    StabSp = schurRing(QQ, tp, infinity, GroupActing => "Sp");
    tp_{1,1} * tp_{1,1}

  Text

    Having computed in the stable ring, one can @TO specialize@ to a
    finite rank at the end.  The modification rule drops or re-signs
    any partition that exceeds the target rank.

  Example
    f = tp_{1,1} * tp_{1,1};
    specialize(f, 2)     -- inside Sp(4)
    specialize(f, 3)     -- inside Sp(6)

  Text

    {\bf Conversions between variants.}  Every basis conversion is
    reversible and functorial.  The table below summarizes the
    user-level commands for moving between them:

    $\bullet$ @TO toS@, @TO toGL@: to the plain Schur (GL) basis.

    $\bullet$ @TO toE@, @TO toH@, @TO toP@: to the $e/h/p$-basis (lives
    in the associated @TO symmetricRing@).

    $\bullet$ @TO toSymm@: inverse of @TO toS@; pushes a Schur-basis
    element into the symmetric ring.

    $\bullet$ @TO toM@: to the monomial basis.

    $\bullet$ @TO toSp@, @TO toO@: to the symplectic or orthogonal
    character basis, via the inverse Koike branching formulas.

    $\bullet$ @TO toRatGL@: embeds a (polynomial) GL character into
    the ring of rational GL characters.

    $\bullet$ @TO toSn@: carries coefficient data from a GL-style ring
    into an Sn-style ring (same partition labels, different product).

    $\bullet$ @TO convert@: a universal dispatcher --- given any source
    and any target, it picks the right converter.

    $\bullet$ @TO specialize@: stable ring $\to$ finite-rank ring, for
    any variant (applies the appropriate modification rule).

    $\bullet$ @TO branch@: restricts a Schur, Sp, or O character along
    the diagonal of a product of two classical groups.

  Example
    S = schurRing(QQ, s, 5);
    Sp = schurRing(QQ, sp, 2, GroupActing => "Sp");
    convert(s_{2,1}, Sp)
    convert(oo, S)

  Text

    The compatibility of these conversions can be verified against the
    Newell-Littlewood product directly.  For instance, in the stable
    symplectic ring, $sp_{(1)}^2 = sp_{(2)} + sp_{(1,1)} + 1$; equivalently,
    $(s_{(1)})^2 = s_{(2)} + s_{(1,1)}$, and expanding each Schur function
    in the Sp basis via @TO toSp@ and adding yields the same element.

  Text

    {\bf Mathematical background.}  We collect some pointers for
    readers who wish to consult the original sources.

    $\bullet$ {\it Schur-Weyl duality} states that the group algebras
    of $GL_n$ and $S_d$ act on $V^{\otimes d}$ with mutually centralizing
    images, so the decomposition of $V^{\otimes d}$ as a $GL_n \times
    S_d$-module provides a bijection between polynomial representations
    of $GL_n$ of degree $d$ and representations of $S_d$.  The
    @TO2 {"toSn","toSn"}@ / @TO toGL@ pair is the computational
    realization of this bijection.

    $\bullet$ The {\it Frobenius characteristic} identifies the
    representation ring of $S_n$ with the degree-$n$ component of the
    ring of symmetric functions.  Under this isomorphism, the
    character of a virtual representation corresponds to a symmetric
    function, and the internal product of characters to a product
    expressible in the power-sum basis.  See @TO classFunction@ and
    @TO symmetricFunction@.

    $\bullet$ The {\it Koike-Terada universal characters} provide a
    single family of symmetric functions whose specializations at
    $n$ variables recover the irreducible characters of $Sp(2n)$ (or
    $O(n)$) for every $n$.  Equivalently, the stable Sp/O character
    rings form a purely combinatorial object, and the Sam-Snowden-Weyman
    modification rule tells us how to specialize to finite rank.

    $\bullet$ The {\it Newell-Littlewood product} is the multiplication
    in the stable Sp/O ring, expressed as a Littlewood-Richardson-like
    sum:
    $$sp_\mu \cdot sp_\nu = \sum_{\alpha,\beta,\gamma}
      c^\mu_{\alpha,\beta}\, c^\nu_{\alpha,\gamma}\, sp_{\beta\gamma},$$
    where $c^\lambda_{\mu\nu}$ are the Littlewood-Richardson
    coefficients.  The analogous formula holds for $o$.  Our
    implementation replaces this with the equivalent route
    (Schur basis, multiply, back to Sp/O via @TO toSp@/@TO toO@),
    which is both faster and easier to certify.

    $\bullet$ {\it King branching formulas} describe the restriction
    of a character of a classical group to a two-factor subgroup.
    See @TO branch@.

    $\bullet$ {\it Plethysm} $f[g]$ encodes composition of Schur
    functors: $f[g]$ is the character of the $GL(V)$-representation
    obtained by applying the functor with character $f$ to the
    representation with character $g$.  Our implementation agrees with
    the classical definition on power-sum generators:
    $p_n[g(x_1,x_2,\dots)] = g(x_1^n, x_2^n, \dots)$.  See
    @TO plethysm@.

  Text

    {\bf References.}

    $\bullet$ I.\ G.\ Macdonald, {\it Symmetric Functions and Hall
    Polynomials}, 2nd ed., Oxford University Press, 1995.

    $\bullet$ W.\ Fulton, {\it Young Tableaux}, LMSST {\bf 35},
    Cambridge University Press, 1997.

    $\bullet$ W.\ Fulton and J.\ Harris, {\it Representation Theory.
    A First Course}, GTM {\bf 129}, Springer, 1991.

    $\bullet$ R.\ P.\ Stanley, {\it Enumerative Combinatorics, Vol.\ 2},
    Cambridge University Press, 1999.

    $\bullet$ B.\ E.\ Sagan, {\it The Symmetric Group.  Representations,
    Combinatorial Algorithms, and Symmetric Functions}, 2nd ed., GTM
    {\bf 203}, Springer, 2001.

    $\bullet$ J.\ Weyman, {\it Cohomology of Vector Bundles and
    Syzygies}, Cambridge Tracts in Math.\ {\bf 149}, 2003.

    $\bullet$ K.\ Koike, {\it On the decomposition of tensor products
    of the representations of classical groups: by means of universal
    characters}, Adv.\ Math.\ {\bf 74} (1989), 57--86.

    $\bullet$ K.\ Koike and I.\ Terada, {\it Young-diagrammatic methods
    for the representation theory of the classical groups of type
    $B_n$, $C_n$, $D_n$}, J.\ Algebra {\bf 107} (1987), 466--511.

    $\bullet$ R.\ C.\ King, {\it Branching rules for classical Lie
    groups using tensor and spinor methods}, J.\ Phys.\ A {\bf 8}
    (1975), 429--449.

    $\bullet$ S.\ V.\ Sam and A.\ Snowden, {\it Introduction to twisted
    commutative algebras}, arXiv:1209.5122 (2012).

    $\bullet$ S.\ V.\ Sam and A.\ Snowden, {\it Stability patterns in
    representation theory}, Forum Math.\ Sigma {\bf 3} (2015), e11.

    $\bullet$ S.\ V.\ Sam, A.\ Snowden, and J.\ Weyman, {\it Homology of
    Littlewood complexes}, Selecta Math.\ (N.S.) {\bf 19} (2013),
    655--698.
///

doc ///
Key
  SchurRing
  (symbol _,SchurRing,List)
  (symbol _,SchurRing,Sequence)
  (symbol _,SchurRing,ZZ)
Headline
  The class of all Schur rings
Description
  Text
    A Schur ring is the representation ring for the general linear group of {\tt n\times n}
    matrices, and one can be constructed with @TO schurRing@.

  Example
    S = schurRing(QQ,s,4)

  Text

    Alternatively, its elements can be interpreted as virtual characters of symmetric groups,
    by setting the value of the option @TO GroupActing@ to {\tt "Sn"}.

  Example
    Q = schurRing(QQ,q,4,GroupActing => "Sn")
  Text

    The element corresponding to the Young diagram {\tt \{3,2,1\}}, is obtained as follows.

  Example
    s_{3,2,1}

  Text

    Alternatively, we can use a @TO Sequence@ instead of a @TO List@ as the index of a Schur
    function.

  Example
    s_(3,2,1)

  Text

    For Young diagrams with only one row one can use positive integers as subscripts.

  Example
    q_4

  Text

    The name of the Schur ring can be used with a subscript to describe a symmetric
    function.

  Example
    Q_{2,2}
    S_5

  Text

    The dimension of the underlying virtual {\tt GL}-representation can be obtained
    with @TO dim@.

  Example
    dim s_{3,2,1}

  Text

    Multiplication in the ring comes from tensor product of representations.

  Example
    s_{3,2,1} * s_{1,1}
    q_{2,1} * q_{2,1}

  Text

    To extract data in an element in a SchurRing, use @TO "listForm"@:

  Example
    listForm (s_{3})^2
    q_{2,1} * q_{2,1}
    listForm oo

  Text

    By varying the option @TO GroupActing@ one obtains Schur rings for a wide range
    of classical groups.  The {\tt "Sp"} flavor is the representation ring of the
    symplectic group {\tt Sp(2n)}; multiplication in this ring is governed by the
    Newell--Littlewood rule rather than the Littlewood--Richardson rule.

  Example
    Sp = schurRing(QQ,sp,4,GroupActing => "Sp");
    sp_{2,1} * sp_{1,1}
    dim sp_{2,1}
    exteriorPower(2,sp_{1})

  Text

    For the orthogonal flavor {\tt "O"} one must further specify the parity
    of the ambient vector space via @TO OddOrEven@.  For instance,
    {\tt O(5)} (type {\tt B_2}) is obtained as follows.

  Example
    O5 = schurRing(QQ,oo5,5,GroupActing => "O",OddOrEven => "Odd");
    dim oo5_{2,2,1}

  Text

    The rational-{\tt GL} flavor {\tt "RatGL"} implements virtual {\tt GL}-modules
    whose weights may be negative, using the {\tt (\alpha,\beta)} pair convention.

  Example
    Rg = schurRing(QQ,r,3,GroupActing => "RatGL");
    r_({2,1},{1})
    r_({1},{}) * r_({},{1})

  Text

    One can also iterate the construction to form tensor products of Schur rings,
    which is useful for bivariate characters.

  Example
    S = schurRing(QQ,s,4);
    T = schurRing(S,t,3);
    (s_{2,1} + t_{1,1})^2

SeeAlso
  schurRing
  symmetricRing
  GroupActing
  Basis
  OddOrEven
///

doc ///
Key
  schurRing
  (schurRing,Ring,Symbol,ZZ)
  (schurRing,Ring,Thing,ZZ)
  (schurRing,Ring,Symbol)
  (schurRing,Ring,Thing)
  (schurRing,Thing,ZZ)
  (schurRing,Thing)
Headline
  Make a SchurRing
Description
  Text
    {\tt S = schurRing(A,s,n)} creates a Schur ring of degree {\tt n} over the base ring
    {\tt A}, with variables based on the symbol {\tt s}. This is the representation ring
    for the general linear group of {\tt n} by {\tt n} matrices, tensored with the ring
    {\tt A}. If {\tt s} is already assigned a value as a variable in a ring, its base
    symbol will be used, if it is possible to determine.

  Example
    S = schurRing(QQ[x],s,3);
    (x*s_{2,1}+s_3)^2

  Text
    Alternatively, the elements of a Schur ring may be interpreted as characters of
    symmetric groups. To indicate this interpretation, one has to set the value of the option
    @TO GroupActing@ to "Sn".

  Example
    S = schurRing(s,4,GroupActing => "Sn");
    exteriorPower(2,s_(3,1))

  Text
    If the dimension {\tt n} is not specified, then one should think of {\tt S} as the
    full ring of symmetric functions over the base {\tt A}, i.e. there is no restriction
    on the number of parts of the partitions indexing the generators of {\tt S}.

  Example
    S = schurRing(ZZ/5,t)
    (t_(2,1)-t_3)^2

  Text
    If the base ring {\tt A} is not specified, then @TO QQ@ is used instead.

  Example
    S = schurRing(r,2,EHPVariables => (re,rh,rp))
    toH r_(2,1)

  Text

    Beyond the default {\tt GL} flavor, the options @TO GroupActing@
    and @TO Basis@ choose between the symmetric-group ({\tt "Sn"}),
    symplectic ({\tt "Sp"}), orthogonal ({\tt "O"}), special-linear
    ({\tt "SL"}), rational-GL ({\tt "RatGL"}), and monomial-basis
    variants.  See the main @TO SchurRings@ page for the full landscape.

    For example, the symplectic representation ring lives in its own
    @TO SchurRing@, and the orthogonal flavor takes an additional
    @TO OddOrEven@ option to distinguish {\tt O(2n+1)} from {\tt O(2n)}.

  Example
    Sp = schurRing(QQ,sp,4,GroupActing => "Sp");
    sp_{2,1}*sp_{1,1}
    O4 = schurRing(QQ,o,4,GroupActing => "O",OddOrEven => "Even");
    dim o_{3,2,1}

  Text

    The monomial-symmetric-function basis is activated with
    {\tt Basis => "Monomial"}, and multiplication switches to the
    corresponding convolution on weak compositions.

  Example
    M = schurRing(QQ,m,4,Basis => "Monomial");
    m_{2,1} * m_{1}
    toS m_{2,1}

  Text

    One can iterate @TO schurRing@ to produce a tower of Schur rings,
    a convenient setting for bi-graded or bivariate character calculations.

  Example
    S = schurRing(QQ,s,4);
    T = schurRing(S,t,3);
    coefficientRing T
    s_{1,1} * t_{2,1}

  Text

    Passing {\tt n => infinity} (or omitting the dimension) builds the stable
    ring of symmetric functions, with no bound on the number of parts.

  Example
    Sinf = schurRing(ZZ/7,u);
    numgens Sinf
    (u_{2,1})^2

SeeAlso
  SchurRing
  symmetricRing
  GroupActing
  Basis
  OddOrEven
///

doc ///
Key
  (coefficientRing, SchurRing)
Headline
  Coefficient ring of a Schur ring
Usage
  coefficientRing S
Inputs
  S:SchurRing
Description
  Text
    Given a Schur ring {\tt S}, the function returns its coefficient ring.
    The coefficient ring may be any commutative ring that Macaulay2 supports,
    including other Schur rings obtained by iterating @TO schurRing@.

  Example
    S = schurRing(ZZ[x],s,4);
    coefficientRing S
    A = schurRing(QQ,a,3);
    B = schurRing(A,b,2);
    coefficientRing B

  Text

    For a tower of Schur rings, {\tt coefficientRing} peels off one layer at a
    time, allowing the user to navigate the entire construction.

  Example
    T = schurRing(B,t,2);
    coefficientRing T
    coefficientRing coefficientRing T

  Text

    One can build Schur rings over finite-field coefficients as well.

  Example
    P = schurRing(ZZ/5,p,4);
    coefficientRing P
    (p_{2,1} + p_{1})^2
SeeAlso
  schurRing
  SchurRing
///

document {
     Key => {SchurRingIndexedVariableTable,(symbol _,SchurRingIndexedVariableTable,Thing)},
     "This class is used as part of the implementation of a type of indexed variable used just for Schur rings.",
     PARA{"It is what makes the partition-indexed notation ", TT "s_{3,2,1}", " or ",
	  TT "s_(3,2,1)", " work: when you write ", TT "s", " in a Schur ring, ", TT "s",
	  " is bound to a ", TO "SchurRingIndexedVariableTable", " whose ", TT "_",
	  " method accepts a list, a sequence, or an integer and returns the corresponding",
	  " Schur-basis element.  The same mechanism drives the partition-indexed notation
	  in every flavor of ", TO "SchurRing", " -- monomial, symplectic, orthogonal,
	  symmetric-group, and rational-GL variants all share this indexing interface."},
     EXAMPLE {
	  "S = schurRing(QQ, s, 4);",
	  "class s",
	  "s_{2,1}",
	  "s_(2,1)",
	  "s_2"
	  },
     PARA{"Each Schur ring comes with its own indexed variable table, and partitions with
	  many parts print in the same compact way."},
     EXAMPLE {
	  "T = schurRing(QQ, t, 5);",
	  "t_{4,3,2,1}",
	  "t_{1,1,1,1}",
	  "dim t_{4,3,2,1}"
	  },
     PARA{"For rings with ", TT "Basis => \"Monomial\"", ", the same table selects the
	  monomial-symmetric-function basis elements instead."},
     EXAMPLE {
	  "M = schurRing(QQ, m, 4, Basis => \"Monomial\");",
	  "m_{2,1}",
	  "m_{2,1} * m_{1}"
	  },
     PARA{"In the rational-GL flavor, subscripts are pairs of lists encoding positive
	  and negative weights."},
     EXAMPLE {
	  "R = schurRing(QQ, r, 3, GroupActing => \"RatGL\");",
	  "r_({2,1},{1})"
	  },
     SeeAlso => { IndexedVariableTable, SchurRing, schurRing }
     }

doc ///
Key
  symmetricRing
  (symmetricRing,Ring,ZZ)
  (symmetricRing,ZZ)
Headline
  Make a Symmetric ring
Usage
  symmetricRing(A,n)
  symmetricRing n
Inputs
  A:Ring
  n:ZZ
Description
  Text

    The method {\tt symmetricRing} creates a Symmetric ring of dimension {\tt n} over a base ring
    {\tt A}. This is the subring of the ring of symmetric functions over the base {\tt A}
    consisting of polynomials in the first {\tt n} elementary (or complete, or power sum)
    symmetric functions. If {\tt A} is not specified, then it is assumed to be @TO QQ@.

  Example
    R = symmetricRing(QQ[x,y,z],4)
    e_2*x+y*p_3+h_2
    toS oo

  Text

    The elements of a Symmetric ring can be interpreted as characters of either symmetric or
    general linear groups. This is controlled by the value of the option @TO GroupActing@, whose
    default value is "GL" (general linear group). The other possibility for its value is
    "Sn" (symmetric group).

  Example
    R = symmetricRing(QQ,3,GroupActing => "Sn")
    toE symmetricPower(2,e_2)

  Text

    The three symmetric-function generators -- elementary, complete, and power
    sum -- are all accessible in the same ring, and conversions between them
    are handled by @TO toE@, @TO toH@, @TO toP@, and @TO toS@.

  Example
    R = symmetricRing(QQ,5);
    toS ((R.pVariable 2)^3)
    toH (R.pVariable 2)
    toS (R.eVariable 2 * R.hVariable 2)

  Text

    A Symmetric ring can be built over any commutative base; coefficients can
    be polynomial rings, modular rings, or even other Schur rings.

  Example
    R7 = symmetricRing(ZZ/7,4);
    (R7.eVariable 2)^3
    Rt = symmetricRing(QQ[t],3,GroupActing => "Sn");
    toS(Rt.eVariable 2 * t)

SeeAlso
  SchurRing
  schurRing
  eVariable
  hVariable
  pVariable
///

doc ///
Key
  eVariable
Headline
  Elementary symmetric functions in a Symmetric ring
Description
  Text
    For a Symmetric ring {\tt R} of dimension {\tt n}, {\tt R.eVariable} is a function
    which assigns to each index {\tt 1\leq i\leq n} the {\tt i}-th elementary symmetric
    function. If {\tt i} is outside the given bounds, an error is returned.

  Example
    R = symmetricRing(QQ,5,EHPVariables => (a,b,c));
    R.eVariable 3

  Text

    The elementary generators are related to the Schur basis via {\tt e_k =
    s_{1^k}}; @TO toS@ realizes this on any product of {\tt e}-variables.

  Example
    R = symmetricRing(QQ,4);
    toS (R.eVariable 2)
    toS ((R.eVariable 2)^2)

  Text

    One may also use {\tt eVariable} from a Symmetric ring built with a
    non-standard coefficient ring or group acting on it.

  Example
    Rsn = symmetricRing(QQ,4,GroupActing => "Sn");
    toS symmetricPower(2, Rsn.eVariable 2)
    Rmod = symmetricRing(ZZ/5,3);
    (Rmod.eVariable 1)^5

SeeAlso
  hVariable
  pVariable
  symmetricRing
  toS
///

doc ///
Key
  hVariable
Headline
  Complete symmetric functions in a Symmetric ring
Description
  Text
    For a Symmetric ring {\tt R} of dimension {\tt n}, {\tt R.hVariable} is a function
    which assigns to each index {\tt 1\leq i\leq n} the {\tt i}-th complete symmetric
    function. If {\tt i} is outside the given bounds, an error is returned.

  Example
    R = symmetricRing(QQ,2,EHPVariables => (x,y,z));
    R.hVariable 2

  Text

    Complete symmetric functions translate to one-row Schur functions via
    {\tt h_k = s_k}; this is the other half of the {\tt e/h}-duality.

  Example
    R = symmetricRing(QQ,4);
    toS (R.hVariable 3)
    toS (R.eVariable 2 * R.hVariable 2)

  Text

    They interact cleanly with power sums: Newton's identities are realized by
    @TO toE@ and @TO toP@, and here we convert a cube of {\tt h_2} into the
    power-sum basis.

  Example
    R = symmetricRing(QQ,3);
    toP ((R.hVariable 2)^2)

  Text

    Complete symmetric functions are available over any coefficient ring.

  Example
    Rt = symmetricRing(QQ[t],3);
    toS (Rt.hVariable 2 + t * Rt.eVariable 2)

SeeAlso
  eVariable
  pVariable
  symmetricRing
  toS
///

doc ///
Key
  pVariable
Headline
  Power-sum symmetric functions in a Symmetric ring
Description
  Text
    For a Symmetric ring {\tt R} of dimension {\tt n}, {\tt R.pVariable} is a function
    which assigns to each index {\tt 1\leq i\leq n} the {\tt i}-th power-sum symmetric
    function. If {\tt i} is outside the given bounds, an error is returned.

  Example
    R = symmetricRing(QQ,4);
    R.pVariable 2

  Text

    Power sums are an algebraically independent generating set for the ring of
    symmetric functions over @TO QQ@, and products of {\tt p_i}'s expand into
    the Schur basis using the character table of the symmetric group.

  Example
    R = symmetricRing(QQ,5);
    toS ((R.pVariable 2)^3)

  Text

    The same variable in a Symmetric ring with {\tt GroupActing => "Sn"}
    represents a virtual character of a symmetric group, and conversion to
    elementary and complete generators is handled by @TO toE@ and @TO toH@.

  Example
    Rsn = symmetricRing(QQ,4,GroupActing => "Sn");
    toE (Rsn.pVariable 2)
    toH (Rsn.pVariable 3)

  Text

    Power sums are also useful as the natural input to @TO plethysm@.

  Example
    R = symmetricRing(QQ,4);
    toS plethysm(R.pVariable 2, R.hVariable 2)

SeeAlso
  eVariable
  hVariable
  symmetricRing
  toS
  plethysm
///

doc ///
   Key
     (numgens,SchurRing)
   Headline
     Number of generators of Schur ring.
   Description
      Text

     	  Given a Schur ring {\tt S}, the function {\tt numgens} outputs the number
	  of generators of {\tt S}. This is equal to the relative dimension of {\tt S}
	  over its base ring, and also to the maximal number of parts of a partition
	  allowed as an index for the elements of {\tt S}.

      Example
      	  R = schurRing(QQ,r,6);
	  numgens R
	  S = schurRing(s);
	  numgens S

      Text

	  When a Schur ring is built on top of another Schur ring as its coefficient
	  ring, {\tt numgens} measures only the outermost (relative) layer.  Nested
	  Schur rings thus model tensor products of representation rings, each layer
	  tracked by its own @TO numgens@.

      Example
	  A = schurRing(QQ,a,3);
	  B = schurRing(A,b,2);
	  numgens B
	  numgens coefficientRing B

      Text

	  The {\tt numgens} value for a stable Schur ring (built with {\tt n => infinity}
	  or without an explicit rank) is @TO infinity@, reflecting the fact that no
	  partition length is excluded.

      Example
	  Sinf = schurRing(QQ,u);
	  numgens Sinf
	  u_{4,3,2,1}

      Text

	  The same rule applies to the symplectic and orthogonal flavors.

      Example
	  Sp = schurRing(QQ,sp,4,GroupActing => "Sp");
	  numgens Sp
	  O = schurRing(QQ,o,5,GroupActing => "O",OddOrEven => "Odd");
	  numgens O
   SeeAlso
     schurRing
     SchurRing
///

doc ///
   Key
     (schurRing,Ring)
   Headline
     The Schur ring corresponding to a given Symmetric ring.
   Usage
     S = schurRing R
   Inputs
     R:Ring
   Outputs
     S:SchurRing
   Description
      Text

     	  Given a ring {\tt R}, the function {\tt schurRing} attempts to return a
	  Schur ring {\tt S} that is associated to {\tt R} in a natural way. Namely, if
	  the attribute {\tt R.Schur} points to a Schur ring, then the function returns
	  that ring. If {\tt R} is already a Schur ring, then the ring {\tt R} is returned.
	  Otherwise, if the Schur level of {\tt R} is at least one, then the function
	  constructs a Schur ring over the base ring {\tt A} of {\tt R}, having the same
	  relative dimension over {\tt A} as {\tt R}. If the Schur level of {\tt R} is zero, then
	  an error is returned.

      Example
      	  R = schurRing(QQ,r,6);
	  schurRing R
	  Q = symmetricRing(QQ,3);
	  A = schurRing Q;
	  schurRing Q

      Text

	  Passing an existing Schur ring to {\tt schurRing} simply returns it, which is
	  convenient as a guard when a function wants to accept either a Symmetric ring
	  or a Schur ring as input.

      Example
	  S = schurRing(QQ,s,4);
	  schurRing S === S

      Text

	  For a Symmetric ring with the {\tt "Sn"} interpretation, the associated Schur
	  ring inherits this flavor and is cached on the ring.

      Example
	  Rsn = symmetricRing(QQ,4,GroupActing => "Sn");
	  Ssn = schurRing Rsn;
	  numgens Ssn

      Text

	  The construction also works over polynomial coefficient rings, producing a
	  Schur ring with the same parameters as coefficients.

      Example
	  Rx = symmetricRing(QQ[x],3);
	  Sx = schurRing Rx;
	  coefficientRing Sx
   SeeAlso
     symmetricRing
     SchurRing
     schurRing
///

doc ///
   Key
     (symmetricRing,Ring)
   Headline
     The Symmetric ring corresponding to a given (Schur) ring.
   Usage
     R = symmetricRing S
   Inputs
     S:Ring
   Outputs
     R:Ring
   Description
      Text

     	  Given a (Schur) ring {\tt S}, the function {\tt symmetricRing} returns a
	  (Symmetric) ring {\tt R} that is associated to {\tt S} in a natural way. Namely, if
	  the attribute {\tt S.symmetricRing} points to a ring, then the function returns
	  that ring. If {\tt S} is not a Schur ring, then the function returns {\tt S}.
	  Otherwise, if {\tt S} is a Schur ring, then the function
	  constructs a polynomial ring over the Symmetric ring {\tt R_A} of the base ring {\tt A} of
	  {\tt R}, having the same relative dimension over {\tt R_A} as {\tt S} over {\tt A}.

      Example
      	  A = schurRing(QQ,a,6);
	  B = schurRing(A,b,3);
	  symmetricRing B
	  symmetricRing ZZ

      Text

	  For a plain Schur ring, the associated Symmetric ring is a polynomial ring
	  in the elementary, complete, and power-sum generators.

      Example
	  S = schurRing(QQ,s,4);
	  R = symmetricRing S;
	  (R.eVariable 2)^2
	  toS ((R.eVariable 2)^2)

      Text

	  The construction plays well with coefficient rings of different flavors, and
	  with the symmetric-group interpretation via {\tt GroupActing => "Sn"}.

      Example
	  Ssn = schurRing(QQ,c,4,GroupActing => "Sn");
	  Rsn = symmetricRing Ssn;
	  numgens Rsn

      Text

	  On a tower of Schur rings, {\tt symmetricRing} produces a tower of Symmetric
	  rings mirroring the coefficient-ring structure.

      Example
	  A = schurRing(QQ,a,3);
	  B = schurRing(A,b,2);
	  RB = symmetricRing B;
	  coefficientRing RB
   SeeAlso
     schurRing
     SchurRing
     symmetricRing
///

doc ///
   Key
     toS
     (toS,RingElement)
     (toS,RingElement,SchurRing)
   Headline
     Schur (s-) basis representation
   Usage
     fs = toS f
     fs = toS(f,S)
   Description
      Text

    	Given a symmetric function {\tt f}, the function 
        {\tt toS} yields a representation of {\tt f} as a linear
	combination of Schur functions. 

     	If {\tt f} is an element of a Symmetric ring {\tt R} and the output Schur ring {\tt S}
	is not specified, then the output {\tt fs} is an element of the Schur ring 
	associated to {\tt R} (see @TO schurRing@).
        
      Example
        R = symmetricRing(QQ,4);
        fs = toS(e_1*h_2+p_3)
        S = schurRing(s,2);
	toS(fs,S)
	
      Text
      
        This also works over tensor products of Symmetric/Schur rings.
	
      Example
        R = symmetricRing(4, EHPVariables => (a,b,c), SVariable => r);
	S = symmetricRing(R, 2, EHPVariables => (x,y,z), SVariable => s);
	T = symmetricRing(S, 3, SVariable => t);
	A = schurRing T;
	f = a_3*x_2*e_1 - b_1*z_2*p_3
	toS f

      Text

        The Jacobi-Trudi determinant $s_\lambda = \det(h_{\lambda_i - i + j})$
	is inverted by {\tt toS}: feeding a Jacobi-Trudi expression back
	through {\tt toS} recovers a single Schur label.

      Example
        R = symmetricRing(QQ,5);
        toS jacobiTrudi({3,2,1},R)

      Text

        {\bf GL to Sn.}  The GL Schur basis and the Frobenius-characteristic
	(Sn) basis share the same partition index set.  Combining {\tt toS}
	with @TO toSn@ carries coefficient data between the two flavors.

      Example
        G  = schurRing(QQ, g, 4);
        Sn = schurRing(QQ, n, 4, GroupActing => "Sn");
        a  = toSn(g_{2,1}, Sn)
        toS(a, G)

      Text

        {\bf Sp {\rm to} S {\rm to} Sp.}  Elements of a variant-basis
	ring (e.g.\ {\tt "Sp"}, {\tt "O"}) can be expanded into the plain
	GL Schur basis with {\tt toS}, and the resulting combination
	re-expressed in a variant-basis ring via @TO convert@.

      Example
        Sp = schurRing(QQ, sp, 4, GroupActing => "Sp");
        f  = toS sp_{2,1}
        Sp' = schurRing(QQ, sq, 4, GroupActing => "Sp");
        convert(f, Sp')

   SeeAlso
     toH
     toE
     toP
     toSn
     toSymm
     jacobiTrudi
///

doc ///
  Key
    toE
    (toE,RingElement)
  Headline
     Elementary symmetric (e-) basis representation
  Usage
     fe = toE f
  Inputs
     f:RingElement
       element of a Symmetric or Schur ring
  Outputs
     fe:RingElement
        element of a Symmetric ring
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toE} yields a representation of {\tt f} as a polynomial
	  in the elementary symmetric functions.

  	  If {\tt f} is an element of a Schur ring {\tt S} then the output {\tt fe} is an 
	  element of the Symmetric ring associated to {\tt S} (see @TO symmetricRing@).

      Example
      	  R = symmetricRing 7;
	  toE(h_3*e_3)
	  S = schurRing(s,4)
	  toE S_{3,2,1}

      Text

        This also works over tensor products of Symmetric/Schur rings.

      Example
        R = schurRing(r, 4, EHPVariables => (a,b,c));
	S = schurRing(R, s, 2, EHPVariables => (x,y,z));
	T = schurRing(S, t, 3);
	A = symmetricRing T;
	f = (r_1+s_1+t_1)^2
	toE f

      Text

        {\bf Variant bases.}  When {\tt f} lives in a @TO SchurRing@
	with {\tt GroupActing} in $\{${\tt "Sp"}, {\tt "O"},
	{\tt "RatGL"}, {\tt "SL"}$\}$ or {\tt Basis => "Monomial"},
	the output is obtained by treating the partition labels of
	{\tt f} as plain Schur labels and applying the Jacobi-Trudi
	determinant.  This is {\it not} the same as the symmetric
	function representing the character of the underlying irrep.
	To obtain the character expansion, first call @TO toS@ to get
	the plain GL Schur expansion, and then compose with {\tt toE}:
	{\tt toE(toS f)}.

      Example
        Sp = schurRing(QQ, sp, 3, GroupActing => "Sp");
        toE sp_{2,1}
        toE toS sp_{2,1}

      Text

        Composing {\tt toE} with @TO toS@ lets you convert an arbitrary
	$e$/$h$/$p$-expression into the $e$-basis via the Schur basis,
	which is sometimes numerically cleaner than Newton's identities.

      Example
        R = symmetricRing(QQ,4);
        toE toS (e_1 * h_2 + p_3)
        toE toS (h_2^2)

      Text

        The names of the output variables are controlled by
	@TO EHPVariables@; {\tt toE} writes its result in the first
	slot of that triple.

      Example
        Rxyz = symmetricRing(QQ, 4, EHPVariables => (x,y,z));
        toE(y_3)
        toE(z_2)

      Text

        {\bf Stable rings.}  If {\tt f} lives in a rank-infinite
	SchurRing (created with {\tt numgens => infinity}), {\tt toE}
	raises an error: the associated symmetric ring can only be
	constructed at finite rank.  Use @TO specialize@ to fix a rank
	first.

      Example
        Sinf = schurRing(QQ, u, infinity);
        try toE u_{2,1} else "error: stable ring has no symmetricRing"
        toE specialize(u_{2,1}, 3)

  SeeAlso
    toH
    toS
    toP
    toSymm
    specialize
    EHPVariables
///

doc ///
  Key
    toH
    (toH,RingElement)
  Headline
     Complete symmetric (h-) basis representation
  Usage
     fh = toH f
  Inputs
     f:RingElement
       element of a Symmetric or Schur ring
  Outputs
     fh:RingElement
        element of a Symmetric ring
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toH} yields a representation of {\tt f} as a polynomial
	  in the complete symmetric functions.

  	  If {\tt f} is an element of a Schur ring {\tt S} then the output {\tt fh} is an 
	  element of the Symmetric ring associated to {\tt S} (see @TO symmetricRing@).

      Example
      	  R = symmetricRing 7;
	  toH(h_3*e_3)
	  S = schurRing(s,4)
	  toH S_{3,2,1}

      Text

        This also works over tensor products of Symmetric/Schur rings.
	
      Example
        R = schurRing(r, 4, EHPVariables => (a,b,c));
	S = schurRing(R, s, 2, EHPVariables => (x,y,z));
	T = schurRing(S, t, 3);
	A = symmetricRing T;
	f = (r_1+s_1+t_1)^2
	toH f

      Text

        {\bf Variant bases.}  When {\tt f} lives in a @TO SchurRing@
	with {\tt GroupActing} in $\{${\tt "Sp"}, {\tt "O"},
	{\tt "RatGL"}, {\tt "SL"}$\}$ or {\tt Basis => "Monomial"},
	partition labels are treated as plain Schur labels (Jacobi-Trudi
	is applied directly); the result is not the same as the
	character expansion.  Use {\tt toH(toS f)} for the latter.

      Example
        O = schurRing(QQ, o, 4, GroupActing => "O");
        toH o_{2,1}
        toH toS o_{2,1}

      Text

        Roundtripping through the Schur basis recovers the original
	$h$-expression (up to the usual commutative-polynomial rewriting).

      Example
        R = symmetricRing(QQ,5);
        toH toS (h_2 * h_3)
        toH toE toS (h_2^2)

      Text

        The names of the output variables follow @TO EHPVariables@;
	{\tt toH} writes its result using the second name in that triple.

      Example
        Rxyz = symmetricRing(QQ, 4, EHPVariables => (x,y,z));
        toH(x_1 * x_2)
        toH(z_3)

      Text

        {\bf Stable rings.}  Rank-infinite SchurRings raise an error;
	use @TO specialize@ to fix a rank first.

      Example
        Sinf = schurRing(QQ, v, infinity);
        try toH v_{3,1} else "error: stable ring has no symmetricRing"
        toH specialize(v_{3,1}, 4)

  SeeAlso
    toE
    toS
    toP
    toSymm
    specialize
    EHPVariables
///

doc ///
  Key
    toP
    (toP,RingElement)
  Headline
     Power-sum (p-) basis representation
  Usage
     fp = toP f
  Inputs
     f:RingElement
       element of a Symmetric or Schur ring
  Outputs
     fp:RingElement
        element of a Symmetric ring
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toP} yields a representation of {\tt f} as a polynomial
	  in the power-sum symmetric functions.

  	  If {\tt f} is an element of a Schur ring {\tt S} then the output {\tt fp} is an 
	  element of the Symmetric ring associated to {\tt S} (see @TO symmetricRing@).

      Example
      	  R = symmetricRing 7;
	  toP(h_3*e_3)
	  S = schurRing(s,4)
	  toP S_{3,2,1}

      Text

        This also works over tensor products of Symmetric/Schur rings.
	
      Example
        R = schurRing(r, 4, EHPVariables => (a,b,c));
	S = schurRing(R, s, 2, EHPVariables => (x,y,z));
	T = schurRing(S, t, 3);
	A = symmetricRing T;
	f = (r_1+s_1+t_1)^2
	toP f

      Text

        {\bf Variant bases.}  Same caveat as @TO toE@ and @TO toH@: on
	elements of a variant-basis Schur ring, labels are treated as
	plain Schur labels; use {\tt toP(toS f)} to get the character
	expansion in the power-sum basis.

      Example
        Sp = schurRing(QQ, sp, 3, GroupActing => "Sp");
        toP sp_{2,1}
        toP toS sp_{2,1}

      Text

        Composing {\tt toP} with @TO toE@ or @TO toH@ exercises
	Newton's identities: {\tt toP(toE f)} returns {\tt f} in the
	$p$-basis, and {\tt toH(toP f)} returns it back in the $h$-basis.

      Example
        R = symmetricRing(QQ,5);
        toP toE (p_2 * p_3)
        toH toP (h_2 * h_3)

      Text

        The output variables for {\tt toP} use the third slot of
	@TO EHPVariables@.

      Example
        Rxyz = symmetricRing(QQ, 4, EHPVariables => (x,y,z));
        toP(y_3)
        toP(x_1 * x_2)

      Text

        {\bf Stable rings.}  Rank-infinite SchurRings raise an error;
	use @TO specialize@ to fix a rank first.

      Example
        Sinf = schurRing(QQ, w, infinity);
        try toP w_{2,2} else "error: stable ring has no symmetricRing"
        toP specialize(w_{2,2}, 4)

  SeeAlso
    toH
    toS
    toE
    toSymm
    specialize
    EHPVariables
///

doc ///
Key
  jacobiTrudi
  (jacobiTrudi,BasicList,Ring)
Headline
  Jacobi-Trudi determinant
Usage
  f = jacobiTrudi(lambda,R)
Inputs
  lambda:BasicList
         a nonincreasing list of integers, or a partition
  R:Ring
    a Symmetric ring
Outputs
  f:RingElement
    an element of a Symmetric ring
Description
  Text

    Given a partition {\tt lambda} and Symmetric ring {\tt R},
    the method evaluates the Jacobi-Trudi determinant corresponding
    to the partition {\tt lambda}, yielding a representation of
    the Schur function {\tt s_{lambda}} as a symmetric function
    in {\tt R}. The default option is to represent this symmetric
    function in terms of {\tt e-}polynomials.

  Example
    R = symmetricRing(QQ,10);
    jacobiTrudi({3,2,2,1},R)
    jacobiTrudi(new Partition from {4,4,1},R,EorH => "H")
    toS oo

  Text

    Selecting {\tt EorH => "H"} uses the conjugate determinant
    formula $s_\lambda = \det(h_{\lambda_i - i + j})$.  The two
    branches produce different {\tt e}- vs {\tt h}-polynomials
    but always represent the same Schur function:

  Example
    R = symmetricRing(QQ,8);
    lam = {4,3,2,1};
    fe = jacobiTrudi(lam,R,EorH => "E");
    fh = jacobiTrudi(lam,R,EorH => "H");
    toS fe
    toS fh
    toS fe == toS fh

  Text

    The routine caches intermediate subdeterminants on the ring
    via @TO [jacobiTrudi,Memoize]@, so a second call on a large
    partition returns almost instantly:

  Example
    R = symmetricRing(QQ,6);
    elapsedTime jacobiTrudi({4,3,2,1},R);
    elapsedTime jacobiTrudi({4,3,2,1},R);

  Text

    Passing a partition through @TO toSymm@ applied to the
    corresponding Schur label reproduces the Jacobi-Trudi output:

  Example
    R = symmetricRing(QQ,5);
    S = schurRing R;
    jacobiTrudi({3,2,1},R) == toSymm(S_{3,2,1})

  Text

    {\tt jacobiTrudi} works over tensor products of Symmetric
    rings, producing a determinant in the outermost set of
    generators:

  Example
    R = symmetricRing(QQ, 4, EHPVariables => (a,b,c));
    T = symmetricRing(R, 3, EHPVariables => (x,y,z));
    jacobiTrudi({2,1},T)
    jacobiTrudi({3,2},T, EorH => "H")
///

doc///
   Key
     EorH
     [jacobiTrudi,EorH]
   Headline
     e- or h- representation of Jacobi-Trudi determinant
   Usage
     EorH => s
   Inputs
     s:String
       either "E" or "H"
   Description
     Text
       This option allows one to choose between evaluating the
       Jacobi-Trudi determinant in the {\tt e}- or {\tt h}- basis.
       If the length of the conjugate partition {\tt lambda'} is
       larger than the length of {\tt lambda}, then it is
       computationally less expensive to set the option {\tt EorH}
       to {\tt "H"}. Otherwise, the default value {\tt "E"} is more
       efficient.

     Example
       R = symmetricRing(QQ,8);
       fe = jacobiTrudi({2,2,2,2,2},R,EorH => "E");
       fh = jacobiTrudi({2,2,2,2,2},R,EorH => "H");
       fe
       fh

     Text

       Although the two polynomials are superficially different,
       they are equal as symmetric functions, as seen after
       applying @TO toS@:

     Example
       toS fe == toS fh

     Text

       When the conjugate partition is much longer than
       {\tt lambda} itself, the {\tt "H"}-branch requires a
       smaller determinant and runs measurably faster.  For
       example on {\tt lambda = (10)} the conjugate is
       $(1^{10})$, so {\tt "H"} only sets up a 1x1 determinant:

     Example
       R = symmetricRing(QQ,12);
       elapsedTime jacobiTrudi({10},R,EorH => "E",Memoize => false);
       elapsedTime jacobiTrudi({10},R,EorH => "H",Memoize => false);
///

doc///
   Key
     [jacobiTrudi,Memoize]
   Headline
     Store values of the jacobiTrudi function.
   Usage
     Memoize => b
   Inputs
     b:Boolean
   Description
     Text

       If the option is set to {\tt true} then all the values of the jacobiTrudi
       function that are computed are recorded into a special hash table attached
       to the symmetric ring inside which the computations are done.  This makes
       repeated evaluations on related partitions substantially faster, at the
       cost of some extra memory in the ring.

     Example
       R = symmetricRing(QQ,6);
       jacobiTrudi({4,3,2,1},R,Memoize => true) == jacobiTrudi({4,3,2,1},R,Memoize => false)
       elapsedTime jacobiTrudi({5,4,3,2,1},R,Memoize => true);
       elapsedTime jacobiTrudi({5,4,3,2,1},R,Memoize => true);
       elapsedTime jacobiTrudi({5,4,3,2,1},R,Memoize => false);
///

doc ///
Key
  plethysm
  (plethysm,RingElement,RingElement)
Headline
  Plethystic operations on representations
Usage
  pl = plethysm(f,g)
  pl = f @ g
Inputs
  f:RingElement
    element of Symmetric ring or Schur ring
  g:RingElement
    element of Symmetric ring or Schur ring
Outputs
  pl:RingElement
     element of the ring of {\tt g}
Description
  Text
    Given a symmetric function {\tt f} and the character {\tt g} of a virtual representation of a product
    of general linear and symmetric groups, the method computes the character of the
    plethystic composition of {\tt f} and {\tt g}. The result of this operation will be an element of
    the ring of {\tt g}. We use the binary operator @TO symbol \@ @ as a synonym for the plethysm function.

  Example
    R = symmetricRing(QQ,5);
    pl = plethysm(h_2,h_3)
    toS pl
    S = schurRing(QQ,q,3);
    h_2 @ q_{2,1}
    plethysm(q_{2,1},q_{2,1})
    T = schurRing(S,t,2,GroupActing => "Sn");
    plethysm(q_{1,1,1}-q_{2,1}+q_{3},q_{2,1}*t_2-t_{1,1})
    p_3 @ (q_{2,1}*t_2-t_{1,1})

  Text

    Since the power-sum basis behaves multiplicatively under
    plethysm, one has the identity
    {\tt plethysm(p_m, p_n) = p_{mn}}, and more generally any
    complete/power-sum pair commutes under plethysm, as shown
    below:

  Example
    R = symmetricRing(QQ,8);
    toS plethysm(p_2,p_3) == toS p_6
    toS plethysm(h_3,p_2) == toS plethysm(p_2,h_3)

  Text

    The symmetric and antisymmetric squares of an irreducible
    representation decompose via {\tt Sym^2 = plethysm(\{2\},-)}
    and {\tt \Lambda^2 = plethysm(\{1,1\},-)}, and their sum
    recovers the ordinary tensor square:

  Example
    S = schurRing(QQ,s,4);
    sym2 = plethysm({2},s_{2,1})
    wedge2 = plethysm({1,1},s_{2,1})
    s_{2,1}*s_{2,1} - sym2 - wedge2

  Text

    Plethysm makes sense for representations of symmetric groups
    as well.  In an {\tt Sn}-flavored ring, {\tt plethysm(lambda, chi)}
    applies the Schur functor {\tt S_lambda} to the
    {\tt Sn}-representation {\tt chi}:

  Example
    Sn = schurRing(QQ,c,4,GroupActing => "Sn");
    plethysm({2,1},c_{2,1,1})

  Text

    Plethysm also works over tensor products of Schur rings,
    mixing a GL factor and an {\tt Sn} factor:

  Example
    G = schurRing(QQ,g,3);
    N = schurRing(G,n,3,GroupActing => "Sn");
    plethysm({2},g_1*n_{2,1})
///

doc ///
Key
  (plethysm,BasicList,RingElement)
Headline
  Plethystic operations on representations
Usage
  pl = plethysm(lambda,g)
Inputs
  lambda:BasicList
         nonincreasing sequence of positive integers, or partition
  g:RingElement
    element of Symmetric ring or Schur ring
Outputs
  pl:RingElement
     element of the ring of {\tt g}
Description
  Text

    The method computes the character of the representation obtained by applying the Schur functor
    {\tt S_{\lambda}} to the representation with character {\tt g}, where
    {\tt \lambda} is a partition.

  Example
    R = symmetricRing(QQ,3);
    S = schurRing(QQ,q,3);
    toE plethysm({2,1},e_1*e_2-e_3)
    plethysm({2,1,1},q_{1,1})
    T = schurRing(S,t,4,GroupActing => "Sn");
    plethysm({1,1},q_1*t_{3,1})

  Text

    Even simple plethysms of Schur functions are not obvious a priori.
    For example, {\tt Sym^2} of the antisymmetric square
    $\Lambda^2 V = S_{1,1}V$ breaks up as:

  Example
    S = schurRing(QQ,s,4);
    plethysm({2},s_{1,1})

  Text

    Applying a partition directly lets one extract isotypic
    summands, e.g.\ the two pieces of {\tt V^{\otimes 2}} for
    an {\tt Sn}-representation:

  Example
    Sn = schurRing(QQ,c,4,GroupActing => "Sn");
    sym2chi = plethysm({2}, c_{3,1})
    wed2chi = plethysm({1,1}, c_{3,1})
    c_{3,1}*c_{3,1} - sym2chi - wed2chi

  Text

    For representations of products of groups, the plethysm is
    applied diagonally; here on a GL x GL tensor product:

  Example
    A = schurRing(QQ,a,3);
    B = schurRing(A,b,2);
    plethysm({2},a_1*b_1)
///

doc ///
Key
  (plethysm,RingElement,ClassFunction)
  (plethysm,BasicList,ClassFunction)
Headline
  Plethystic operations on class functions
Usage
  pl = plethysm(f,cF)
  pl = plethysm(lambda,cF)
Description
  Text

    These methods describe the result of applying plethystic operations to a virtual
    character of a symmetric group. These operations are described either via a symmetric
    function {\tt f}, or a partition {\tt lambda}. Since {\tt cF} corresponds to an {\tt S_n}-
    representation, the option @TO GroupActing@ is irrelevant in this case.

  Example
    cF = new ClassFunction from {{2} => 1, {1,1} => -1};
    pl1 = plethysm({1,1},cF)
    R = symmetricRing 5;
    pl2 = plethysm(e_1+e_2,cF)
    S = schurRing R;
    symmetricFunction(cF,S)
    symmetricFunction(pl1,S)
    symmetricFunction(pl2,S)

  Text

    Applying the partition {\tt \{2\}} to the sign character of
    {\tt S_2} gives {\tt Sym^2} of the sign, which is the trivial
    representation of {\tt S_4}:

  Example
    sgn = new ClassFunction from {{2} => -1, {1,1} => 1};
    pl = plethysm({2},sgn)
    symmetricFunction(pl, schurRing(QQ, s, 4, GroupActing => "Sn"))

  Text

    Plethysm by a power-sum class function is Adams-type:
    {\tt p_k} sends a representation to its {\tt k}-th Adams
    operation (on class functions).

  Example
    cF2 = new ClassFunction from {{3} => 2, {2,1} => 0, {1,1,1} => -1};
    R2 = symmetricRing 4;
    plethysm(p_2, cF2)
///
-*
doc ///
Key
  (exteriorPower,ZZ,RingElement)
Headline
  Exterior power of a representation
Usage
  ep = exteriorPower(n,rep)
Inputs
  n:ZZ
  rep:RingElement
      an element of a SchurRing
Outputs
  ep:RingElement
Description
  Text
  
     Given a representation {\tt rep} of a product of general linear
     groups, and a positive integer {\tt n}, the function returns the
     {\tt n}-th exterior power of this representation.
     
  Example
     S = schurRing(QQ,s,2)
     T = schurRing(S,t,3)
     exteriorPower(4,s_{1}+t_{1})
///

doc ///
Key
  (symmetricPower,ZZ,RingElement)
Headline
  Symmetric power of a representation
Usage
  ep = symmetricPower(n,rep)
Inputs
  n:ZZ
  rep:RingElement
      an element of a SchurRing
Outputs
  ep:RingElement
Description
  Text
  
     Given a representation {\tt rep} of a product of general linear
     groups, and a positive integer {\tt n}, the function returns the
     {\tt n}-th symmetric power of this representation.
     
  Example
     S = schurRing(QQ,s,2)
     T = schurRing(S,t,3)
     symmetricPower(4,s_{1}+t_{1})
///
*-

doc ///
Key
  schurResolution
  (schurResolution,RingElement,List,List)
  (schurResolution,RingElement,List)
Headline
  Compute an ``approximate'' equivariant resolution of a module.
Usage
  resol = schurResolution(rep,M,lS)
  resol = schurResolution(rep,M)
Inputs
  rep:RingElement
      element of a SchurRing
  M:List
    list of representations, corresponding to the homogeneous components of a module {\tt M}.
  lS:List
    list of representations, corresponding to the homogeneous components of a polynomial ring {\tt S}.
Outputs
  resol:List
Description
  Text
  
     Given a representation {\tt rep} of a (product of) general linear
     or symmetric group(s) {\tt G}, we consider the symmetric algebra {\tt S = Sym(rep)}
     and an {\tt S}-module {\tt M} which is also a {\tt G}-module in such
     a way that the {\tt S}-module structure on {\tt M} respects the 
     {\tt G}-action. More generally, {\tt S} can be any graded ring, of which one inputs only
     finitely many homogeneous components as a list {\tt lS} of characters of {\tt G}. The main reason
     why we allow this generality is because most of the time it is computationally expensive to calculate
     the symmetric powers of the representation {\tt rep}, so we give the user the option to compute these
     symmetric powers by different methods and use the results as input for the schurResolution routine.
     
     We are interested in computing an equivariant 
     resolution of {\tt M}. This depends on both the {\tt G}- and {\tt S}-module structure 
     of {\tt M} in general, but in many examples that occur in practice, it turns out that
     the differentials in the resolution have maximal rank among all 
     {\tt G}-module homomorphisms between the free modules in the resolution.
     We will therefore assume that this is the case for the module {\tt M} that we are
     trying to resolve, and thus disregard its {\tt S}-module structure.
     
     More precisely, the assumptions that we make about {\tt M} are as follows: {\tt M} is 
     a graded {\tt S}-module, with {\tt M_i = 0} for {\tt i<0}, where the grading on {\tt S} is standard,
     given by setting the degrees of the elements of {\tt rep} equal to 1. Since we assumed
     that the {\tt G}-structure of {\tt M} determines the syzygies, all the relevant
     information is concentrated in finitely many homogeneous components of {\tt M} (namely
     up to {\tt reg(M)+pd(M)}, the sum of the regularity and the projective dimension of
     {\tt M}). We will thus assume that {\tt M}
     is given as a list of {\tt G}-representations, corresponding to (a subset of) the
     relevant homogeneous components. The function {\tt schurResolution} takes as 
     inputs the representation {\tt rep}, the module {\tt M}, and as optional arguments a {\tt DegreeLimit}
     {\tt d}, and a {\tt SyzygyLimit} {\tt c}. The ring {\tt S} itself can occur as input data, being
     described as a list of {\tt G}-representations, just like {\tt M}.
     The routine outputs the generators of degree at most {\tt d} of the 
     first {\tt c+1} syzygy modules (from {\tt 0} to {\tt c}). They are listed as a 
     sequence of pairs, consisting of the degree of the generators of the syzygy modules 
     together with the characters of the {\tt G}-representations they correspond to. 
     If the syzygy bound {\tt c} is not given,
     then all syzygy modules are computed. If the degree bound {\tt d} is not given, then
     it is assumed to be equal to the largest degree among the homogeneous components of
     {\tt M} in the input, i.e. one less than the length of the @TO List@ {\tt M}.
     
     The example below computes the resolution of the quadratic Veronese 
     surface in {\tt P^5}.
      
  Example
    S = schurRing(QQ,s,3)
    rep = s_{2}
    M = {1_S,s_{2},s_{4},s_{6},s_{8},s_{10},s_{12}}
    schurResolution(rep,M)

  Text
  
    Next, we compute the syzygies of degree at most {\tt 7} in the resolution 
    of the cubic Veronese embedding of {\tt P^2}.
    
  Example
    rep = s_{3}
    M = {1_S,s_{3},s_{6},s_{9},s_{12},s_{15},s_{18},s_{21},s_{24},s_{27}}
    d = 7
    schurResolution(rep,M,DegreeLimit => d)

  Text
    
    We can compute the resolution of the ideal of {\tt 2\times 2} minors of a {\tt 3\times 4}
    matrix, which corresponds to the Segre embedding of {\tt P^2\times P^3}:
    
  Example
    T = schurRing(S,t,4)
    rep = s_1 * t_1
    M = {1_T} | apply(splice{1..8},i -> s_i * t_i)
    schurResolution(rep,M)

  Text
  
    The following example computes the equivariant resolution of the residue field of a 
    polynomial ring in {\tt n=5} variables, with respect to the action of the symmetric 
    group {\tt S_n}.
  
  Example
    n = 5;
    S = schurRing(QQ,s,n,GroupActing => "Sn");
    rep = s_n + s_{n-1,1};
    M = {s_n}
    schurResolution(rep,M,DegreeLimit => n)

  Text

    Generalizing this, we can compute the equivariant resolution of the quotient of the
    polynomial ring in {\tt n=5} variables by the ideal of square-free monomials of
    degree two, with respect to the action of the symmetric group {\tt S_n}.

  Example
    M = {s_n} | splice{n:rep};
    schurResolution(rep,M)

  Text

    Ordinary Segre embeddings P^a x P^b can be treated as above by
    taking a tensor product of GL-factors.  For P^1 x P^3, keeping
    only the syzygies in degree at most {\tt 4}:

  Example
    U = schurRing(QQ,u,2);
    V = schurRing(U,v,4);
    rep = u_1 * v_1;
    M = {1_V} | apply(splice{1..5}, i -> u_i * v_i);
    schurResolution(rep,M,DegreeLimit => 4)

  Text

    The cubic Veronese of P^2 has a rich equivariant resolution;
    asking for the first few syzygies of total degree at most {\tt 4}
    gives the classical Koszul-type pattern:

  Example
    S = schurRing(QQ,s,3);
    rep = s_{3};
    M = apply(splice{0..8}, i -> s_{3*i});
    schurResolution(rep,M,DegreeLimit => 4, SyzygyLimit => 3)

  Text

    A larger example: the equivariant resolution of the residue
    field of {\tt Sym(V)} at {\tt rank(V) = 6} under the
    symmetric-group action:

  Example
    n = 6;
    Sb = schurRing(QQ,s,n,GroupActing => "Sn");
    rep = s_n + s_{n-1,1};
    schurResolution(rep, {s_n}, DegreeLimit => n)

///

doc ///
Key
  [schurResolution,DegreeLimit]
Headline
  Specifies the maximal degree of syzygies to be computed
Description
  Text
    This is an optional argument for the @TO schurResolution@ routine. It specifies an upper bound for the
    degrees of the generators of the syzygy modules in the equivariant resolution of an equivariant module {\tt M}
    to be computed by the routine. If a {\tt DegreeLimit} is not specified, then it is assumed to be equal to the
    maximal degree in which the module {\tt M} is specified as a representation.

  Example
    A = schurRing(a,3,GroupActing => "Sn");
    B = schurRing(A,b,2);
    rep = (a_3 + a_{2,1}) * b_1
    d = dim rep
    M = {a_3 * 1_B};
    sR = schurResolution(rep,M,DegreeLimit => d)

  Text

    Decreasing {\tt DegreeLimit} truncates the output to syzygies
    of total degree at most the specified value, while the
    representations themselves are unchanged:

  Example
    S = schurRing(QQ,s,3);
    rep = s_{2};
    M = {1_S,s_{2},s_{4},s_{6},s_{8},s_{10},s_{12}};
    schurResolution(rep,M)
    schurResolution(rep,M,DegreeLimit => 3)

SeeAlso
  [schurResolution,SyzygyLimit]
///
    
doc ///
Key
  [schurResolution,SyzygyLimit]
Headline
  Specifies the number of syzygy modules to be computed
Description
  Text
    This is an optional argument for the @TO schurResolution@ routine. It specifies an upper bound for the
    number of syzygy modules in the equivariant resolution of an equivariant module {\tt M} to be computed
    by the routine. If a {\tt SyzygyLimit} is not specified, then all syzygy modules are computed.

    The example below computes the {\tt 0}-th to {\tt 3}-rd syzygy modules of the {\tt 5}-th Veronese embedding
    of {\tt P^2}.

  Example
    S = schurRing(s,3);
    rep = s_{5};
    M = {1_S,s_{5},s_{10},s_{15},s_{20},s_{25},s_{30}};
    schurResolution(rep,M,SyzygyLimit => 3)

  Text

    Lowering {\tt SyzygyLimit} simply chops the output at the
    requested homological position.  For the quadratic Veronese
    of P^2, asking for only the first syzygy module yields:

  Example
    T = schurRing(QQ,t,3);
    rep2 = t_{2};
    M2 = {1_T,t_{2},t_{4},t_{6},t_{8},t_{10},t_{12}};
    schurResolution(rep2,M2,SyzygyLimit => 1)
    schurResolution(rep2,M2,SyzygyLimit => 2)

SeeAlso
  [schurResolution,DegreeLimit]
///
    
doc ///
  Key
    schurLevel
    (schurLevel,Ring)
  Headline
    Number of SchurRings the ring is a tensor product of.
  Usage
    lev = schurLevel(R)
  Inputs
    R:Ring
  Outputs
    lev:ZZ
  Description
    Text

      For the representation ring {\tt R} of a product of {\tt lev}
      general linear and/or symmetric groups, the function returns
      {\tt lev}.  If {\tt R} is not a representation ring, the
      function returns 0.

    Example
      R = schurRing(QQ,r,3);
      S = schurRing(R,s,5);
      T = schurRing(S,t,2);
      schurLevel R
      schurLevel S
      schurLevel T
      schurLevel QQ

    Text

      A three-level tower mixing {\tt GL}-factors with an {\tt S_n}-factor
      counts each layer, regardless of which group is acting:

    Example
      A = schurRing(QQ,a,3);
      B = schurRing(A,b,4);
      C = schurRing(B,c,2,GroupActing => "Sn");
      schurLevel C

    Text

      A Symmetric ring (produced by @TO symmetricRing@) sits over a
      single tower slot and reports {\tt schurLevel} equal to 1, while
      an ordinary polynomial ring or the base field report 0:

    Example
      schurLevel(symmetricRing(QQ,5))
      schurLevel(QQ[x,y,z])
      schurLevel ZZ
///

doc ///
  Key
    (partitions,Set,BasicList)
  Headline
    Partitions of a set
  Usage
    par = partitions(S,L)
  Inputs
    S:Set
    L:BasicList
      a nonincreasing list of integers, or a partition
  Outputs
    par:List
  Description
    Text

      Given a set {\tt S} and a partition {\tt L=\{l_1\geq l_2\geq\cdots\}},
      the method returns the list of set-partitions of {\tt S} of type
      {\tt L}, i.e. ways of writing {\tt S=S_1\cup S_2\cup\cdots} with the
      {\tt S_i} pairwise disjoint and {\tt |S_i|=l_i}.  The blocks come out
      as an unordered collection of sets.

    Example
      partitions(set{1,2,3,4},{2,1,1})
      partitions(set{a,b,c,d,e},new Partition from {3,2})

    Text

      Changing the shape {\tt L} changes the cycle type.  Two blocks of
      size 2 and one fixed point on five points is the number of
      permutations of cycle type {\tt (2,2,1)} divided by the size of
      the corresponding centralizer:

    Example
      partitions(set{1,2,3,4,5},{2,2,1})
      #partitions(set{1,2,3,4,5},{2,2,1})

    Text

      Passing a @TO Partition@ is equivalent to passing the underlying
      list; counting block-partitions of shape {\tt (3,1)} recovers
      {\tt 4 \choose 1}:

    Example
      partitions(set{1,2,3,4},new Partition from {3,1})
      #partitions(set{1,2,3,4},new Partition from {3,1})

    Text

      Supplying a single-block shape returns a singleton list: the
      only set-partition of type {\tt (n)} is {\tt S} itself.

    Example
      partitions(set{1,2,3},{3})
///

-*
doc ///
 Key
  (chi,BasicList,BasicList)
 Headline
  Irreducible character of symmetric group
Usage
  v = chi(lambda,rho)
Inputs
  lambda:BasicList
   	 a nondecreasing list of positive integers, or a partition
  rho:BasicList
      a nondecreasing list of positive integers, or a partition
Outputs
  v:QQ
Description
  Text

    Given two partitions {\tt lambda} and {\tt rho} of the integer {\tt N}, this method
    computes the value of the irreducible character of the symmetric group
    corresponding to the partition {\tt lambda} evaluated on
    any permutation of cycle-type {\tt rho}.

    The character of the trivial representation takes the value
    1 on any permutation:
  
  Example
    chi({4},{2,1,1})
    
  Text
  
    The character of the sign representation takes the value -1 on
    a cycle of length 4:
  
  Example
    chi({1,1,1,1},{4})
SeeAlso
   symmetricFunction
   classFunction
///
*-

doc ///
Key
  SchurRingElement
Headline
  A type describing elements of a SchurRing
Description
  Text
    Elements of any @TO SchurRing@ -- whether a {\tt GL}-ring, a
    symmetric-group ring, or an orthogonal/symplectic ring -- have
    type {\tt SchurRingElement}.  Products, sums and (in characteristic
    zero) rational scalings of Schur ring elements are again of this
    type.

  Example
    S = schurRing(s,5)
    a = s_{3,2,1}
    instance(a,SchurRingElement)

    T = schurRing(S,t,3,GroupActing => "Sn")
    b = t_{2,1}+t_3
    instance(a*b,SchurRingElement)

  Text
    The same type is used for symplectic characters.  Multiplying two
    {\tt Sp}-characters stays inside the ring:

  Example
    Sp = schurRing(QQ,sp,3,GroupActing => "Sp");
    u = sp_{2,1};
    instance(u,SchurRingElement)
    instance(u*u, SchurRingElement)

  Text
    For an {\tt Sn}-flavored Schur ring the ordinary ring
    multiplication is the tensor product of characters, while
    @TO internalProduct@ gives the pointwise (Kronecker) product:

  Example
    Sn = schurRing(QQ,c,4,GroupActing => "Sn");
    internalProduct(c_{3,1}, c_{2,1,1})
    instance(c_{3,1} * c_{2,1,1}, SchurRingElement)

  Text
    In a two-level ring the SchurRingElement type is closed under
    multiplication across levels:

  Example
    G = schurRing(QQ,g,3);
    H = schurRing(G,h,2);
    z = g_{1,1} * h_{2}
    instance(z, SchurRingElement)
///

doc ///
   Key
     (dim,List,SchurRingElement)
     (dim,Thing,SchurRingElement)
     (dim,SchurRingElement)
   Headline
     dimension of representation
   Usage
     d = dim(lis,s)
     d = dim(n,s)
     d = dim s
   Inputs
     lis: List
     	  or @TO Thing@
     s: SchurRingElement
   Outputs
     d: ZZ
        or @TO Expression@
   Description
     Text

       The method returns the dimension of the virtual representation whose
       character is represented by {\tt s}.

     Example
       S = schurRing(s,3)
       dim s_2
       T = schurRing(t,4,GroupActing => "Sn")
       dim t_{2,2}
       U = schurRing(T,u,3)
       dim (t_{2,2}*u_2)

     Text

       Schur characters see the rank of the ambient vector space.
       The representation {\tt s_{2,1}} of {\tt GL(3)} is 8-dimensional,
       while the same shape in {\tt GL(4)} gives a 20-dimensional
       representation:

     Example
       dim ((schurRing(s3,3))_{2,1})
       dim ((schurRing(s4,4))_{2,1})

     Text

       Dimensions are also computed for symplectic and orthogonal
       characters.  For orthogonal groups the option @TO OddOrEven@
       selects {\tt O(2m+1)} versus {\tt O(2m)}:

     Example
       Sp = schurRing(QQ, sp, 3, GroupActing => "Sp");
       dim sp_{2,1}
       Oodd = schurRing(QQ, od, 3, GroupActing => "O", OddOrEven => "Odd");
       dim od_{2,1}
       Oeven = schurRing(QQ, oe, 3, GroupActing => "O", OddOrEven => "Even");
       dim oe_{2,1}

     Text

       If {\tt S} is a @TO SchurRing@ of level 1, the ring of polynomial representations of some {\tt GL(V)}, it
       may sometimes be convenient to compute dimensions of {\tt GL(V)}-representations symbolically, without
       specifying the dimension of {\tt V}.  Letting {\tt n} denote the parameter corresponding to {\tt dim(V)}
       we have for example:

     Example
       S = schurRing(s,3)
       dim(n,s_2)
       dim(n,s_{1,1})
       dim(n,s_{2,1})

     Text

       Similar calculations make sense over products of general linear groups. The dimensions of the representations
       can be computed symbolically as functions of a number of parameters
       equal to the @TO schurLevel@ of the ring. The parameters corresponding to levels where the group acting
       is a symmetric group don't have a good interpretation, so they are disregarded in the dimension calculation.
       The order of the input parameters is the descending order of the @TO schurLevel@s: in the example below
       {\tt a} corresponds to {\tt Q}, {\tt b} corresponds to {\tt T} and {\tt c} corresponds to {\tt S}.

     Example
       S = schurRing(s,3)
       T = schurRing(S,t,4)
       Q = schurRing(T,q,5,GroupActing => "Sn")
       dExpr = dim({a,b,c},s_{2}*t_{1,1}*q_{4,1})
       P = QQ[a,b,c]
       value dExpr
       dim({1,2,3},s_{2}*t_{1,1}*q_{4,1})

     Text

       Over a two-level {\tt GL\times GL} tower the formula factors
       as a product of dimensions, one per level, in descending
       {\tt schurLevel} order:

     Example
       A = schurRing(aR,3);
       B = schurRing(A,bR,2);
       dim(aR_{2,1} * bR_{1,1})
       dim({4,5}, aR_{2,1} * bR_{1,1})
///


doc ///
Key
  ClassFunction
  (symbol +,ClassFunction,ClassFunction)
  (symbol -,ClassFunction,ClassFunction)
  (symbol *,ClassFunction,ClassFunction)
  (symbol *,ClassFunction,RingElement)
  (symbol *,RingElement,ClassFunction)
  (symbol *,ClassFunction,Number)
  (symbol *,Number,ClassFunction)
  (symbol ==,ClassFunction,ClassFunction)
Headline
  The class of all Class functions
Description
  Text
    A class function (or virtual character of a symmetric group {\tt S_n})
    is a function that is constant on the conjugacy classes of {\tt S_n}.
    Class functions for {\tt S_n} are in one-to-one correspondence with
    symmetric functions of degree {\tt n}.  The class functions corresponding
    to actual representations of {\tt S_n} are called {\tt characters}.

    The character of the standard representation of {\tt S_3} is

  Example
    S = schurRing(QQ,s,3);
    classFunction(s_{2,1})

  Text
    The character of the sign representation of {\tt S_5} is

  Example
    S = schurRing(QQ,s,5);
    classFunction(s_{1,1,1,1,1})

  Text
    We can go back and forth between class functions and symmetric functions.

  Example
    R = symmetricRing(QQ,3);
    cF = new ClassFunction from {{1,1,1} => 2, {3} => -1};
    sF = symmetricFunction(cF,R)
    toS sF
    classFunction sF

  Text
    We can add, subtract, multiply, scale class functions:

  Example
    S = schurRing(QQ,s,4);
    c1 = classFunction(S_{2,1,1}-S_{4});
    c2 = classFunction(S_{3,1});
    c1 + c2
    c1 * c2
    3*c1 - c2*2

  Text
    The trivial and sign representations of {\tt S_4} are the characters
    of the shapes {\tt (4)} and {\tt (1,1,1,1)}.  Their pointwise product
    (which is {\tt c_1 * c_2} on {\tt ClassFunction}) gives the sign
    representation back:

  Example
    T = schurRing(QQ,t,4);
    triv = classFunction(t_{4})
    sgn = classFunction(t_{1,1,1,1})
    triv * sgn == sgn

  Text
    The regular representation of {\tt S_n} has character {\tt n!} on the
    identity class {\tt (1^n)} and 0 elsewhere.  By Frobenius
    reciprocity, it pairs trivially with the trivial character:

  Example
    reg = new ClassFunction from {{1,1,1,1} => 24}
    scalarProduct(reg, triv)

  Text
    Products of class functions of induced/restricted representations
    recover well-known decompositions: the tensor square of the
    standard representation of {\tt S_4} pairs nontrivially with both
    the trivial and sign characters:

  Example
    std = classFunction(t_{3,1});
    sq = std * std;
    scalarProduct(sq, triv)
    scalarProduct(sq, sgn)
///

doc ///
Key
  (degree,ClassFunction)
Headline
  Degree of virtual character
Description
  Text
    For a virtual character {\tt ch} of a symmetric group on {\tt n}
    letters, the degree of {\tt ch} is {\tt n}.

  Example
    S = schurRing(s,5);
    ch = classFunction s_(3,1,1)
    degree ch

  Text
    The degree of {\tt classFunction(s_\lambda)} always agrees with
    {\tt |\lambda|}:

  Example
    lam = {4,2,1};
    degree classFunction(new Partition from lam) == sum lam

  Text
    A sum of characters of the same {\tt S_n} keeps the same degree --
    mixing distinct partitions of {\tt 5} still yields a class function
    of degree {\tt 5}:

  Example
    R = symmetricRing(QQ,5);
    mix = classFunction(jacobiTrudi({4,1},R)) + 2*classFunction(jacobiTrudi({3,2},R));
    degree mix
///

doc ///
Key
  symmetricFunction
  (symmetricFunction,ClassFunction,Ring)
Headline
  Converts class function to symmetric function
Usage
  f = symmetricFunction(ch,S)
Inputs
  ch:ClassFunction
  S:Ring
    a Symmetric or Schur ring
Outputs
  f:RingElement
    element of a Symmetric or Schur ring
Description
  Text
    Given a virtual character {\tt cF} of a symmetric group, and given a
    Symmetric ring {\tt S}, the method computes the corresponding
    symmetric function as an element of {\tt S}.  The conversion uses
    the Frobenius characteristic map: the regular representation
    ({\tt n!} on the identity, zero elsewhere) maps to {\tt n! e_1^n}
    in the {\tt e}-basis, equivalently to {\tt sum_\lambda f^\lambda s_\lambda}
    in the {\tt s}-basis.

  Example
    S = symmetricRing(QQ,4);
    cF = new ClassFunction from {{1,1,1,1}=>24};
    symmetricFunction(cF,S)
    symmetricFunction(cF,schurRing S)

  Text
    The standard representation of {\tt S_4} has character given by
    {\tt classFunction(s_{3,1})}; passing it through {\tt symmetricFunction}
    recovers {\tt s_{3,1}} on the @TO SchurRing@ side:

  Example
    R = symmetricRing(QQ,4);
    Sch = schurRing R;
    stdCh = classFunction(jacobiTrudi({3,1},R));
    symmetricFunction(stdCh, Sch)

  Text
    Composing {\tt classFunction} with {\tt symmetricFunction} is the
    identity on the {\tt S_n}-side of the Frobenius correspondence --
    roundtripping a class function through the symmetric function
    ring returns it unchanged:

  Example
    sF = symmetricFunction(stdCh, R);
    classFunction sF == stdCh

  Text
    The trivial character of {\tt S_5} has Frobenius image {\tt h_5};
    the sign character maps to {\tt e_5}:

  Example
    R5 = symmetricRing(QQ,5);
    symmetricFunction(classFunction{5}, R5)
    symmetricFunction(classFunction{1,1,1,1,1}, R5)
SeeAlso
  classFunction
--  chi
///

doc ///
Key
  classFunction
  (classFunction,RingElement)
Headline
  Converts symmetric function to class function
Usage
  ch = classFunction(f)
Inputs
  f:RingElement
    element of a Symmetric ring
Outputs
  ch:ClassFunction
Description
  Text
    Given a symmetric function {\tt f}, homogeneous of degree {\tt N},
    the method computes the corresponding virtual character of the
    symmetric group {\tt S_N}.

    The character of the standard representation of {\tt S_5} is

  Example
    R = symmetricRing(QQ,5);
    classFunction(jacobiTrudi({4,1},R))

  Text

    The character of the second exterior power of the standard representation of {\tt S_5} is

  Example
    R = symmetricRing(QQ,5);
    classFunction(jacobiTrudi({3,1,1},R))

  Text
    The sign representation of {\tt S_n} corresponds to the Schur
    polynomial of shape {\tt (1^n)}.  Its class function takes the
    value {\tt sgn(\sigma)} on a permutation of cycle type {\tt \rho}
    -- that is, {\tt -1} raised to the number of even-length cycles:

  Example
    Ssign = schurRing(QQ,s,5);
    classFunction(s_{1,1,1,1,1})

  Text
    Small-{\tt n} character tables are assembled row-by-row from
    {\tt classFunction}.  For {\tt S_3} there are three conjugacy
    classes {\tt (1^3), (2,1), (3)} and three irreducibles, and the
    values are collected below:

  Example
    R3 = symmetricRing(QQ,3);
    for lam in {{3},{2,1},{1,1,1}} list classFunction(jacobiTrudi(lam,R3))

  Text
    Tensor products of {\tt S_n}-representations correspond to
    {\tt internalProduct} of class functions.  For {\tt S_4} the
    tensor square of the standard representation decomposes into
    irreducibles by pairing with each irreducible character via
    @TO scalarProduct@:

  Example
    R4 = symmetricRing(QQ,4);
    std = classFunction(jacobiTrudi({3,1},R4));
    sq = internalProduct(std, std);
    for lam in {{4},{3,1},{2,2},{2,1,1},{1,1,1,1}} list
      scalarProduct(sq, classFunction(jacobiTrudi(lam,R4)))

SeeAlso
  symmetricFunction
--  chi
///

doc ///
Key
  (classFunction,BasicList)
Headline
  Character of irreducible representation of symmetric group
Usage
  ch = classFunction(l)
Inputs
  l:BasicList
    partition
Outputs
  ch:ClassFunction
Description
  Text
    Given a partition {\tt l} of {\tt N}, the method computes the
    character of the irreducible {\tt S_N}-representation corresponding
    to the partition {\tt l}.

  Example
    R = symmetricRing(QQ,7);
    cF = classFunction({3,2,1})
    toS(symmetricFunction(cF,R))

  Text
    Enumerating the irreducible characters of {\tt S_4} and pairing
    them with @TO scalarProduct@ recovers the orthonormality relations
    from representation theory -- the diagonal entries are 1 and the
    off-diagonal entries are 0:

  Example
    chars = for lam in partitions 4 list classFunction toList lam;
    matrix for a in chars list for b in chars list scalarProduct(a,b)

  Text
    Irreducible characters of different shapes are orthogonal.  Here
    we verify orthogonality for three partitions of 6:

  Example
    c1 = classFunction({3,2,1});
    c2 = classFunction({4,1,1});
    c3 = classFunction({2,2,2});
    scalarProduct(c1,c2)
    scalarProduct(c2,c3)

  Text
    Self-pairings return 1 for each irreducible, independent of the
    shape, and the trivial (row shape) and sign (column shape)
    characters are the 1-dimensional irreducibles:

  Example
    scalarProduct(c1,c1)
    classFunction({5})
    classFunction({1,1,1,1,1})
SeeAlso
  symmetricFunction

///

doc ///
Key
  scalarProduct
Headline
  Standard pairing on symmetric functions/class functions
Description
  Text

    This method computes the standard Hall scalar product on the ring
    {\tt \Lambda} of symmetric functions. One way to define this product
    is by imposing that the collection of Schur functions {\tt s_{\lambda}}
    form an orthonormal basis.

    Alternatively, by the correspondence between symmetric functions
    and virtual characters of symmetric groups, this scalar product
    coincides with the standard scalar product on class functions.

    The number of standard tableaux of shape {\tt \{4,3,2,1\}} is:

  Example
    R = symmetricRing(QQ,10);
    S = schurRing(QQ,s,10);
    scalarProduct(h_1^10,s_{4,3,2,1})

  Text

    The Schur basis is orthonormal: {\tt <s_\lambda, s_\mu>} equals
    {\tt 1} if {\tt \lambda = \mu} and {\tt 0} otherwise.

  Example
    T = schurRing(QQ,t,5);
    scalarProduct(t_{3,1,1}, t_{3,1,1})
    scalarProduct(t_{3,1,1}, t_{2,2,1})
    scalarProduct(t_{4,1}, t_{3,2})

  Text

    The power-sum basis is orthogonal with norms given by the
    centralizer sizes: {\tt <p_\rho, p_\rho> = z_\rho}.  Here we
    verify this for the three partitions of {\tt 3}.

  Example
    U = symmetricRing(QQ,4);
    scalarProduct(p_3, p_3) == centralizerSize{0,0,1}
    scalarProduct(p_2*p_1, p_2*p_1) == centralizerSize{1,1}
    scalarProduct(p_1^3, p_1^3) == centralizerSize{3}

  Text

    By the Cauchy identity, {\tt <p_1^n, p_1^n>} counts the
    elements of {\tt S_n}, i.e.\ it equals {\tt n!}:

  Example
    scalarProduct(p_1^4, p_1^4)
SeeAlso
  internalProduct
  centralizerSize
///

doc ///
Key
  (scalarProduct,RingElement,RingElement)
Headline
  Standard scalar product of symmetric functions
Usage
  sp = scalarProduct(f1,f2)
Inputs
  f1:RingElement
     element of a Symmetric Ring
  f2:RingElement
     element of a Symmetric Ring
Outputs
  sp:QQ
Description
  Text

    Given symmetric functions {\tt f1} and {\tt f2}, the method
    computes the standard Hall pairing between {\tt f1} and {\tt f2}.

  Example
    R = symmetricRing(QQ,5);
    S = schurRing R
    scalarProduct(h_5,p_5)
    scalarProduct(S_{4,1},p_5)

  Text

    Indeed, the coefficients of {\tt s_5} and {\tt s_{4,1}} in the
    s-basis expansion of {\tt p_5} are as computed above:

  Example
    toS p_5

  Text

    The pairing {\tt <e_n, p_n>} equals {\tt (-1)^{n-1}}, reflecting
    the sign character of the symmetric group:

  Example
    scalarProduct(e_2, p_2)
    scalarProduct(e_3, p_3)
    scalarProduct(e_4, p_4)

  Text

    The pairing {\tt <s_\lambda, h_\mu>} recovers the Kostka number
    {\tt K_{\lambda,\mu}}, the number of semistandard Young tableaux of
    shape {\tt \lambda} and content {\tt \mu}.  We cross-check this
    against @TO kostkaNumber@:

  Example
    scalarProduct(S_{3,2}, h_2*h_2*h_1) == kostkaNumber({3,2}, {2,2,1})
    scalarProduct(S_{3,2}, h_1^5) == kostkaNumber({3,2}, {1,1,1,1,1})
    scalarProduct(S_{4,1}, h_3*h_2) == kostkaNumber({4,1}, {3,2})
SeeAlso
  kostkaNumber
///

doc ///
Key
  (scalarProduct,ClassFunction,ClassFunction)
Headline
  Standard scalar product of class functions
Usage
  sp = scalarProduct(ch1,ch2)
Inputs
  ch1:ClassFunction
  ch2:ClassFunction
Outputs
  sp:QQ
Description
  Text

    Given virtual characters {\tt ch1} and {\tt ch2}, the method
    computes the standard pairing between {\tt ch1} and {\tt ch2}.

  Example
    ch1 = new ClassFunction from {{3,2} => 2, {2,2,1} => -2, {3,1,1} => 2, {5} => 1};
    ch2 = new ClassFunction from {{2,2,1} => -2, {5} => 1, {1,1,1,1,1} => 5, {3,2} => 3, {4,1} => -2};
    scalarProduct(ch1,ch2)

  Text

    The irreducible characters of {\tt S_n} indexed by distinct
    partitions are orthonormal.  For partitions of {\tt 5}, we can
    verify the full orthogonality relations:

  Example
    S = schurRing(QQ,s,5);
    cF32 = classFunction s_{3,2};
    cF41 = classFunction s_{4,1};
    cF221 = classFunction s_{2,2,1};
    scalarProduct(cF32, cF32)
    scalarProduct(cF32, cF41)
    scalarProduct(cF41, cF221)

  Text

    Decomposing an arbitrary virtual character into irreducibles by
    pairing with each Schur class function recovers the multiplicities:

  Example
    psi = cF32 + 2*cF41 - cF221;
    {scalarProduct(psi, cF32), scalarProduct(psi, cF41), scalarProduct(psi, cF221)}
SeeAlso
  classFunction
///

doc ///
Key
  internalProduct
Headline
  Internal product of symmetric functions/class functions
Description
  Text

    This method computes the internal (Kronecker) product of two homogeneous
    symmetric functions of the same degree.  If we think of these functions
    as virtual characters of some symmetric group, then their internal
    product is just the character of the tensor product of the corresponding
    virtual representations.  We use the binary operator @TO symbol **@ as a
    shorthand for @TO internalProduct@.

    The complete symmetric function of degree {\tt n} corresponds
    to the trivial {\tt S_n}-representation and is therefore
    the unit of the representation ring of {\tt S_n}:

  Example
    R = symmetricRing(QQ,5);
    S = schurRing(QQ,s,3);
    internalProduct(h_3,s_{2,1})
    toE(h_3 ** e_3)

  Text

    The square of the sign representation is the trivial representation:

  Example
    toH internalProduct(e_3,e_3)

  Text

    Working in a Schur ring directly, Kronecker products of Schur
    functions give the decomposition of tensor products of irreducible
    {\tt S_n}-representations.  The Kronecker square of {\tt s_{2,1}}
    (the standard representation of {\tt S_3}) decomposes as:

  Example
    T = schurRing(QQ,t,3);
    internalProduct(t_{2,1}, t_{2,1})

  Text

    The exterior square of the sign representation of {\tt S_3} is
    the trivial representation, which on the symmetric-function side
    is the identity {\tt s_{1,1,1} \otimes s_{1,1,1} = s_3}:

  Example
    internalProduct(t_{1,1,1}, t_{1,1,1})
SeeAlso
  scalarProduct
///

doc ///
Key
  (internalProduct,RingElement,RingElement)
Headline
  Kronecker product of symmetric functions
Usage
  ip = internalProduct(f1,f2)
Inputs
  f1:RingElement
     element of a Symmetric ring or a Schur ring
  f2:RingElement
     element of a Symmetric ring or a Schur ring
Outputs
  ip:Ring
     a Symmetric ring or a Schur Ring
Description
  Text

    Given symmetric functions {\tt f1} and {\tt f2}, the method
    computes the Kronecker product {\tt ip} between {\tt f1} and {\tt f2}.
    The output {\tt ip} is an element in the ring of {\tt f2}.

  Example
     R = symmetricRing(QQ,6);
     S = schurRing(QQ,s,6);
     toE(h_3**e_3)
     Q = schurRing(QQ,q,6);
     internalProduct(s_{3,3},q_{4,2})

  Text

    An error is returned if {\tt f1} and {\tt f2} don't have the
    same degree.

    Products of complete homogeneous functions give characters of
    permutation representations of {\tt S_n}; their Kronecker product
    decomposes accordingly.  For instance, in degree {\tt 4}:

  Example
    internalProduct(h_3*h_1, h_2*h_2)

  Text

    The same computation can be carried out directly in a Schur ring
    with option {\tt GroupActing => "Sn"}, where multiplication
    {\tt *} is the internal product.  The Kronecker square of the
    standard representation {\tt s_{3,1}} of {\tt S_4} decomposes as
    trivial + sign + standard + {\tt s_{2,2}}:

  Example
    Sn = schurRing(QQ,c,4,GroupActing => "Sn");
    c_{3,1} * c_{3,1}
    internalProduct(c_{3,1}, c_{3,1})

  Text

    The method is compatible with @TO toSn@: we can first convert a
    product of {\tt h}'s into the Schur basis of an {\tt S_n} ring,
    then take Kronecker products there.

  Example
    toSn(h_2*h_1*h_1, Sn) * c_{3,1}
///

doc ///
Key
  (internalProduct,ClassFunction,ClassFunction)
Headline
  Tensor product of virtual representations
Usage
  ip = internalProduct(ch1,ch2)
Inputs
  ch1:ClassFunction
  ch2:ClassFunction
Outputs
  ip:ClassFunction
Description
  Text

    Given virtual characters {\tt ch1} and {\tt ch2}, the method
    computes the character of the tensor product of corresponding
    virtual representations of the symmetric group.

  Example
    ch1 = new ClassFunction from {{4,4} => 2, {8} => -1, {5,2,1} => 2, {3,2,2,1} => 1};
    ch2 = new ClassFunction from {{2,2,2,2} => -4, {5,2,1} => 1, {3,2,2,1} => 3};
    internalProduct(ch1,ch2)
    ch1 * ch2

  Text

    A classical example: the tensor square of the standard
    representation of {\tt S_4} has character values obtained by
    squaring the standard character.  Pairing with itself recovers
    the multiplicities of the irreducibles in the tensor square:

  Example
    S = schurRing(QQ,s,4);
    std = classFunction s_{3,1};
    sq = internalProduct(std, std)
    scalarProduct(sq, classFunction s_{4})
    scalarProduct(sq, classFunction s_{3,1})
    scalarProduct(sq, classFunction s_{2,2})
    scalarProduct(sq, classFunction s_{2,1,1})
SeeAlso
  classFunction
///

doc ///
Key
  centralizerSize
  (centralizerSize,List)
Headline
  Size of the centralizer of a permutation
Usage
  n = centralizerSize(rho)
Inputs
  rho:List
Outputs
  n:ZZ
Description
  Text

    {\tt rho} is a list representing the cycle type of some permutation:
    the {\tt i}-th entry in {\tt rho} is the number of cycles of length
    {\tt i} of the permutation.
    The output of the function {\tt centralizerSize} is the size of the
    centralizer in the symmetric group of any permutation of cycle type
    {\tt rho}.  If the cycle type {\tt rho} corresponds to a partition
    {\tt \lambda}, then {\tt centralizerSize(rho)} is also the value of
    the square norm {\tt <p_\lambda, p_\lambda>}.

  Example
    centralizerSize{1,1,1}
    R = symmetricRing(QQ,6);
    u = p_1 * p_2 * p_3;
    scalarProduct(u,u)

  Text

    A few values of {\tt z_\rho} for small cycle types: the identity
    of {\tt S_n} has centralizer size {\tt n!}, while an {\tt n}-cycle
    has centralizer of size {\tt n} (generated by itself).

  Example
    centralizerSize{4}           -- cycle type (1,1,1,1), all of S_4
    centralizerSize{0,0,0,1}     -- a single 4-cycle
    centralizerSize{2,1}         -- cycle type (2,1,1)

  Text

    Burnside's classical identity {\tt \sum_{\lambda \vdash n} 1/z_\lambda = 1}
    expresses that the uniform measure on {\tt S_n} sums to {\tt 1}.
    We verify this for {\tt n = 5}:

  Example
    parts5 = {{5}, {4,1}, {3,2}, {3,1,1}, {2,2,1}, {2,1,1,1}, {1,1,1,1,1}};
    cycMult = la -> apply(toList(1..max la), i -> #positions(la, j -> j == i));
    sum(parts5, la -> 1/centralizerSize cycMult la)
SeeAlso
  scalarProduct
///

doc ///
  Key
    Memoize
  Headline
    Option to record values of the jacobiTrudi function
  Description
    Text

      This is an optional argument for the @TO jacobiTrudi@
      function, allowing one to store its values
      in order to speed up computations.  When {\tt Memoize => true},
      every computed value is cached in a hash table on the symmetric
      ring, so repeated calls on the same partition return the cached
      value in constant time.

    Example
      R = symmetricRing(QQ, 10);
      elapsedTime jacobiTrudi({4,3,2,1}, R, Memoize => true);
      elapsedTime jacobiTrudi({4,3,2,1}, R, Memoize => true);

    Text

      The cache is attached to the ring {\tt R}.  After one partition
      is memoized, subsequent calls with a different partition perform
      the full Jacobi-Trudi determinant expansion, then cache it as
      well:

    Example
      elapsedTime jacobiTrudi({5,3,2}, R, Memoize => true);
      elapsedTime jacobiTrudi({5,3,2}, R, Memoize => true);

    Text

      Without {\tt Memoize => true}, each call recomputes the
      determinant from scratch; for large partitions this can be
      substantially more expensive than a single cached lookup.

    Example
      elapsedTime jacobiTrudi({4,3,2,1}, R);
      elapsedTime jacobiTrudi({4,3,2,1}, R);
  SeeAlso
    jacobiTrudi
///

doc ///
Key
  SVariable
  [schurRing,SVariable]
  [symmetricRing,SVariable]
Headline
  Specifies symbol representing s-functions
Description
  Text
    This is an optional argument for the constructor of a Symmetric ring. It indicates the
    symbol to be used to denote s-functions in the associated Schur ring. The default value
    is {\tt s}.

  Example
    R = symmetricRing(QQ,5,SVariable => getSymbol"s");
    S = schurRing R
    s_2^2

  Text

    The chosen symbol becomes the actual indexing name for the
    basis elements of the Schur ring.  Any symbol may be used; for
    instance, {\tt sigma} yields Schur elements {\tt sigma_\lambda}
    which multiply by Littlewood-Richardson:

  Example
    T = schurRing(QQ,sigma,4)
    sigma_{1,1}^2

  Text

    An {\tt Sn}-flavored ring using a custom {\tt SVariable} stores
    characters of the symmetric group on that symbol:

  Example
    Sn = schurRing(QQ,sig,4,GroupActing => "Sn");
    sig_{3,1} * sig_{2,2}

  Text

    Distinct {\tt SVariable} choices allow a tensor product of two
    Schur rings to carry unambiguous variable names for each
    factor, so a bi-representation has a clean display:

  Example
    S = schurRing(QQ,s,3);
    T = schurRing(S,tau,2);
    s_{2,1} * tau_{1,1}

SeeAlso
  EHPVariables
///

doc ///
Key
  EHPVariables
  [schurRing,EHPVariables]
  [symmetricRing,EHPVariables]
Headline
  Specifies sequence of symbols representing e-, h-, and p-functions
Description
  Text
    This is an optional argument for the constructor of a Symmetric or Schur ring. It indicates the
    symbols to be used to denote e-, h-, and p-functions in the associated Symmetric ring. The
    default values are {\tt (e,h,p)}.

  Example
    S = schurRing(QQ,s,4,EHPVariables => (getSymbol"a",getSymbol"b",getSymbol"c"));
    R = symmetricRing S
    epol = toE s_{2,2,2}
    toS epol

  Text

    The three symbols become the polynomial-ring generators of
    the associated @TO symmetricRing@.  With the default
    {\tt (e,h,p)}, inspecting {\tt gens} shows all three families
    indexed by {\tt 1..n}:

  Example
    R = symmetricRing(QQ,3);
    gens R

  Text

    A custom triple works the same way.  Below, {\tt a_i, c_i, b_i}
    correspond respectively to the elementary, power-sum and
    complete homogeneous symmetric functions in the associated
    Symmetric ring:

  Example
    S2 = schurRing(QQ,s,4,EHPVariables => (getSymbol"a",getSymbol"b",getSymbol"c"));
    R2 = symmetricRing S2;
    gens R2

  Text

    The option propagates across tensor products of Schur rings,
    so each factor may carry its own e/h/p alphabet without name
    clashes:

  Example
    A = schurRing(QQ,sa,3,EHPVariables => (getSymbol"ea",getSymbol"ha",getSymbol"pa"));
    B = schurRing(A,sb,2,EHPVariables => (getSymbol"eb",getSymbol"hb",getSymbol"pb"));
    schurLevel B

SeeAlso
  SVariable
///

doc ///
Key
  GroupActing
  [schurRing,GroupActing]
  [symmetricRing,GroupActing]
Headline
  Specifies the group that is acting
Description
  Text
    This is an optional argument for the @TO schurRing@ and @TO symmetricRing@ functions.
    When the exterior or symmetric powers of a symmetric function {\tt g} are computed, the result
    depends on whether {\tt g} is interpreted as a virtual representation of a general
    linear, symmetric, symplectic, or orthogonal group. The option {\tt GroupActing} specifies the
    interpretation to be considered. Its possible values are {\tt "GL"} (the default),
    {\tt "Sn"}, {\tt "SL"}, {\tt "Sp"}, {\tt "O"}, and {\tt "RatGL"}.

  Example
    S = schurRing(s,2);
    exteriorPower(3,s_2)
    T = schurRing(t,2,GroupActing => "Sn");
    symmetricPower(2,t_{1,1})

  Text

    The first example computes the decomposition of {\tt \Lambda^3(Sym^2(V))} into irreducible
    {\tt GL(V)}-representations, while the second one computes the
    second symmetric power of the sign representation of the symmetric group {\tt S_2}.

    Multiplication differs sharply between the {\tt "GL"} and
    {\tt "Sn"} interpretations.  Under {\tt "GL"}, the product is
    the Littlewood-Richardson tensor product of polynomial
    representations, which is degree-additive in the partitions.
    Under {\tt "Sn"}, multiplication is the ordinary tensor
    product of characters of a single symmetric group, so only
    partitions of the {\it same} size may be multiplied, and the
    product is expanded in the Kronecker coefficients:

  Example
    Sgl = schurRing(QQ,s,4);
    s_{2,1} * s_{2,1}
    Ssn = schurRing(QQ,t,4,GroupActing => "Sn");
    t_{2,1,1} * t_{2,1,1}

  Text

    The values {\tt "Sp"} and {\tt "O"} select the stable (universal) character ring of
    the symplectic and orthogonal groups, respectively. The basis elements are indexed by
    partitions, with {\tt sp_\lambda} (respectively {\tt o_\lambda}) standing for the
    irreducible symplectic (respectively orthogonal) character associated to {\tt \lambda}.
    Multiplication in these rings is the Newell-Littlewood product, implemented by
    conversion to the Schur basis via Koike's branching formulas and back:

  Example
    Sp = schurRing(QQ,sp,GroupActing => "Sp");
    sp_{1} * sp_{1}
    sp_{1,1}
    toS sp_{1,1}
    O = schurRing(QQ,o,GroupActing => "O");
    o_{1} * o_{1}
    toS o_{2}

  Text

    Exterior and symmetric powers are likewise reinterpreted.
    In the orthogonal ring, exterior powers of the second
    fundamental character mix several Newell-Littlewood terms:

  Example
    Ofin = schurRing(QQ,obar,5,GroupActing => "O");
    exteriorPower(3, obar_2)

  Text

    Setting {\tt GroupActing => "SL"} forces the top-row
    reduction {\tt s_\lambda = s_{(\lambda_1 - \lambda_n, \ldots, \lambda_{n-1} - \lambda_n)}},
    reflecting the fact that the determinant representation is
    trivial in {\tt SL}.  Long partitions collapse accordingly:

  Example
    SL3 = schurRing(QQ,sl,3,GroupActing => "SL");
    sl_{3,2,1}
    sl_{4,2,2}
    sl_{2,1,1}

  Text

    Finally, {\tt GroupActing => "RatGL"} produces the ring of
    rational (mixed) polynomial representations of {\tt GL(V)},
    whose characters are indexed by pairs of partitions
    (bipartitions), and whose tensor product is again a
    Littlewood-Richardson-style rule:

  Example
    Rg = schurRing(QQ,r,3,GroupActing => "RatGL");
    r_({1},{}) * r_({},{1})

  Text

    Stable rings (no dimension specified) allow arbitrarily many parts in the
    partition labels; a concrete dimension can be supplied to restrict the partitions
    and to model a finite-dimensional group.

SeeAlso
  toSp
  toO
  toS
///

doc ///
Key
  Basis
  [schurRing,Basis]
  [symmetricRing,Basis]
Headline
  Specifies the basis to use for a Schur ring
Description
  Text
    This is an optional argument for the @TO schurRing@ and @TO symmetricRing@
    functions. It selects how the partition-indexed generators of the ring are
    interpreted as symmetric functions. The possible values are {\tt "Schur"}
    (the default) and {\tt "Monomial"}.

    When {\tt Basis => "Schur"}, the ring generator {\tt s_\lambda} represents the
    Schur function indexed by {\tt \lambda}. Multiplication uses the
    Littlewood-Richardson rule supplied by the Macaulay2 engine.

  Example
    S = schurRing(QQ,s,4);
    s_{2,1} * s_{1}

  Text
    When {\tt Basis => "Monomial"}, the ring generator {\tt m_\lambda} instead
    represents the monomial symmetric function indexed by {\tt \lambda}.
    Multiplication is implemented by converting to the Schur basis, multiplying
    there via Littlewood-Richardson, and converting back to the monomial basis
    using Kostka numbers. The resulting ring is abstractly isomorphic to the
    Schur-basis ring but its elements are displayed and stored as linear
    combinations of monomial symmetric functions.

  Example
    M = schurRing(QQ,m,4,Basis => "Monomial");
    m_{1} * m_{1}
    m_{2,1} * m_{1}

  Text
    The consistency of the two bases can be verified against @TO toM@, which
    converts a symmetric function to the monomial basis:

  Example
    S = schurRing(QQ,s,4);
    M = schurRing(QQ,m,4,Basis => "Monomial");
    toM(s_{1} * s_{1},M) == m_{1} * m_{1}

  Text
    A monomial-basis product agrees with the Schur-basis product
    after round-tripping through {\tt toS}, verifying that the two
    rings are isomorphic with isomorphism {\tt toM}/{\tt toS}:

  Example
    x = s_{2,1} * s_{1};
    y = toM(x,M);
    y
    toS(y,S) == x

  Text
    Converting back from the monomial basis to the Schur basis
    recovers the original Schur element:

  Example
    toS(m_{2,1} + m_{1,1,1}, S)

  Text
    The monomial-to-Schur transition is essentially indexed by
    @TO kostkaNumber@: the coefficient of {\tt m_\mu} in
    {\tt s_\lambda} equals {\tt K_{\lambda,\mu}}.  For example,
    the Kostka numbers with {\tt \lambda = (2,1)} reproduce the
    monomial expansion of {\tt s_{(2,1)}}:

  Example
    toM s_{2,1}
    kostkaNumber({2,1},{2,1})
    kostkaNumber({2,1},{1,1,1})

SeeAlso
  schurRing
  symmetricRing
  toM
  kostkaNumber
  GroupActing
///

doc ///
Key
  kostkaNumber
  (kostkaNumber,BasicList,BasicList)
Headline
  Compute a Kostka number
Usage
  k = kostkaNumber(lambda,mu)
Inputs
  lambda:BasicList
    a partition
  mu:BasicList
    a composition (or partition) of the same size as {\tt lambda}
Outputs
  k:ZZ
    the Kostka number {\tt K_{\lambda,\mu}}
Description
  Text

    The Kostka number {\tt K_{\lambda,\mu}} is the number of
    semistandard Young tableaux of shape {\tt \lambda} and content {\tt \mu}.
    Equivalently, it is the coefficient of the Schur function {\tt s_\lambda}
    in the product of complete symmetric functions {\tt h_{\mu_1} h_{\mu_2} \cdots}.

  Example
    kostkaNumber({2,1},{1,1,1})
    kostkaNumber({3},{2,1})
    kostkaNumber({2,1},{2,1})
    kostkaNumber({2,1},{3})

  Text

    Returns {\tt 0} whenever {\tt \lambda} and {\tt \mu} do not have the same
    size, or whenever {\tt \mu} does not dominate {\tt \lambda} (so that no
    tableaux of the requested shape and content exist).

  Text

    The full Kostka matrix {\tt K_{\lambda,\mu}} for partitions of
    {\tt 4}, listed in reverse dominance order
    {\tt (4), (3,1), (2,2), (2,1,1), (1,1,1,1)}, is upper
    triangular with ones on the diagonal (a reflection of the fact
    that Schur functions form an {\tt h}-triangular basis with
    respect to the monomial basis):

  Example
    P = {{4},{3,1},{2,2},{2,1,1},{1,1,1,1}};
    matrix apply(P, la -> apply(P, mu -> kostkaNumber(la,mu)))

  Text

    A useful boundary identity: for any partition {\tt \mu} of
    {\tt n}, the Kostka number with {\tt \lambda = (n)} is {\tt 1},
    counting the unique row tableau:

  Example
    for mu in P list kostkaNumber({4}, mu)

  Text

    Similarly, the Kostka number {\tt K_{\lambda,(1^n)}} counts
    standard Young tableaux of shape {\tt \lambda}, which for
    {\tt \lambda = (2,2)} is {\tt 2} (the hook-length value),
    matching the dimension of the corresponding irreducible
    {\tt S_4}-representation:

  Example
    kostkaNumber({2,2}, {1,1,1,1})
    kostkaNumber({2,1,1}, {1,1,1,1})

SeeAlso
  toM
  toS
  Basis
///

doc ///
   Key
     toM
     (toM,RingElement)
     (toM,RingElement,SchurRing)
   Headline
     Monomial (m-) basis representation
   Usage
     fm = toM f
     fm = toM(f,M)
   Description
      Text

        Given a symmetric function {\tt f}, the function {\tt toM} returns a
        representation of {\tt f} as a linear combination of monomial symmetric
        functions. The output is a {\tt RingElement} in a
        @TO SchurRing@ with @TO Basis@ {\tt => "Monomial"}, so that the
        basis element {\tt m_\mu} represents the monomial symmetric function
        indexed by the partition {\tt \mu}.

        Internally, the conversion uses Kostka numbers: the Schur-to-monomial
        transition is {\tt s_\lambda = \sum_\mu K_{\lambda,\mu} m_\mu}.

      Example
        S = schurRing(QQ,s,4);
        toM s_{2,1}
        toM s_{3}
        toM (s_{2,1} + 2*s_{1,1,1})

      Text

        If the target monomial ring {\tt M} is not specified, the first call to
        {\tt toM} on an element of {\tt S} creates and caches an associated
        monomial-basis ring with the same number of generators and the same
        coefficient ring as {\tt S}; subsequent calls reuse this cached ring.

      Example
        ring(toM s_{2,1}) === ring(toM s_{3})

      Text

        Alternatively one can supply the target monomial ring explicitly.

      Example
        M = schurRing(QQ,m,4,Basis => "Monomial");
        toM(s_{2,1},M)

      Text

        The function also accepts elements of a Symmetric ring, in which case
        the input is first converted to the Schur basis via @TO toS@.

      Example
        R = symmetricRing(QQ,4);
        toM(h_2 * h_1)

      Text

        The identity {\tt h_n = \sum_\mu m_\mu}, summed over all
        partitions of {\tt n}, reflects the fact that the Kostka
        numbers {\tt K_{(n),\mu}} are all {\tt 1}:

      Example
        R5 = symmetricRing(QQ,5);
        toM h_3
        toM h_4

      Text

        Power-sum functions admit a particularly simple monomial
        expansion: {\tt p_n = m_{(n)}} identically, since
        {\tt p_n = \sum_i x_i^n}.  This can be read off for any
        {\tt n}:

      Example
        toM p_3
        toM p_5

      Text

        Longer partitions can still be handled; the coefficients
        are the corresponding Kostka numbers:

      Example
        S6 = schurRing(QQ,s,6);
        toM s_{4,1,1,1}

      Text

        The map {\tt toM} is invertible via @TO toS@, so a Schur
        element roundtrips through the monomial basis and back to
        the same Schur ring:

      Example
        f = s_{3,2,1};
        toS(toM f, S6) == f

   SeeAlso
     toS
     toH
     toE
     toP
     kostkaNumber
     Basis
///

doc ///
   Key
     toSp
     (toSp,RingElement)
     (toSp,RingElement,SchurRing)
   Headline
     Expansion in the basis of symplectic characters
   Usage
     fsp = toSp f
     fsp = toSp(f,Sp)
   Description
      Text

        Given a symmetric function {\tt f}, the function {\tt toSp} returns the
        expression of {\tt f} in the basis of irreducible symplectic characters.
        The output is a {\tt RingElement} in a @TO SchurRing@ with @TO GroupActing@
        {\tt => "Sp"}, so that the basis element {\tt sp_\lambda} stands for the
        irreducible {\tt Sp}-representation associated to the partition {\tt \lambda}.

        The conversion implements the inverse Koike branching formula
        {\tt s_\lambda = \sum_\delta sp_{\lambda/\delta}}, where {\tt \delta} ranges
        over partitions {\tt \delta \subseteq \lambda} all of whose columns have even
        length (equivalently, the parts of {\tt \delta} occur in equal pairs), and
        skew characters are expanded using Littlewood-Richardson coefficients.

      Example
        S = schurRing(QQ,s);
        toSp s_{2}
        toSp s_{1,1}
        toSp s_{3,1}
        toSp (s_{2,1} + 2*s_{1,1})

      Text

        If the target symplectic ring is not specified, the first call to {\tt toSp}
        on an element of {\tt S} creates and caches an associated stable symplectic
        ring with the same coefficient ring as {\tt S}; subsequent calls reuse the
        cached ring.

      Example
        ring(toSp s_{2,1}) === ring(toSp s_{3,1})

      Text

        Alternatively one can supply the target symplectic ring explicitly.

      Example
        Sp = schurRing(QQ,sp,4,GroupActing => "Sp");
        toSp(s_{2,1},Sp)

      Text

        The conversion is inverse to @TO toS@ on the symplectic ring, and
        multiplication of symplectic characters factors through this round-trip.
        At stable rank, {\tt toS(sp_{(1,1)}) = s_{(1,1)} - 1} and
        {\tt toS(sp_{(2,2)}) = s_{(2,2)} - s_{(1,1)}}; the product
        {\tt sp_{(1)}^2} equals the Newell-Littlewood product
        {\tt sp_{(2)} + sp_{(1,1)} + 1}.

      Example
        Sp = schurRing(QQ,sp,GroupActing => "Sp");
        toS sp_{1,1}
        toS sp_{2,2}
        toSp(toS(sp_{2,1}, S), Sp) == sp_{2,1}
        sp_{1}*sp_{1}
        sp_{1,1}*sp_{1,1}

      Text

        At finite rank {\tt n} (the ring parameter {\tt numVariables}), the
        modification rule of Sam-Snowden-Weyman is applied automatically, so
        characters whose partitions exceed {\tt n} rows are folded back to
        admissible Sp basis elements (or zero).  Weyl dimensions are
        available via @TO dim@.  Plethysm is supported and routes through the
        GL Schur ring.

      Example
        Sp4 = schurRing(QQ,sp4,2,GroupActing => "Sp");
        sp4_{1,1}^2
        dim sp4_{2,2}
        plethysm({2}, sp4_{1})

      Text

        An explicit symplectic character decomposition from a
        Schur polynomial can be obtained directly; linearity lets
        the user combine several Schur functions at once:

      Example
        S = schurRing(QQ,s);
        toSp(s_{2,2} + s_{1,1})

      Text

        Plethysm of a symplectic character is well-defined through
        a round-trip to the Schur basis.  Here, the plethysm of
        the trivial {\tt (2)}-power on the defining representation
        {\tt sp_{(1)}} recovers the usual {\tt Sym^2} character,
        and {\tt toS} confirms it is {\tt s_{(2)}}:

      Example
        Sp = schurRing(QQ,sp,GroupActing => "Sp");
        q = plethysm({2}, sp_{1})
        toS q

      Text

        At finite rank, the modification rule folds partitions
        that exceed the rank.  Passing {\tt s_{(2,1,1,1)}} to the
        rank-{\tt 2} symplectic ring {\tt Sp_4} returns the
        modified (and much shorter) character:

      Example
        toSp(s_{2,1,1,1}, Sp4)

   SeeAlso
     toO
     toS
     GroupActing
     dim
     plethysm
///

doc ///
   Key
     toO
     (toO,RingElement)
     (toO,RingElement,SchurRing)
   Headline
     Expansion in the basis of orthogonal characters
   Usage
     fo = toO f
     fo = toO(f,O)
   Description
      Text

        Given a symmetric function {\tt f}, the function {\tt toO} returns the
        expression of {\tt f} in the basis of irreducible orthogonal characters.
        The output is a {\tt RingElement} in a @TO SchurRing@ with @TO GroupActing@
        {\tt => "O"}, so that the basis element {\tt o_\lambda} stands for the
        irreducible {\tt O}-representation associated to the partition {\tt \lambda}.

        The conversion implements the inverse Koike branching formula
        {\tt s_\lambda = \sum_\delta o_{\lambda/\delta}}, where {\tt \delta} ranges
        over partitions {\tt \delta \subseteq \lambda} with all parts even, and skew
        characters are expanded using Littlewood-Richardson coefficients.

      Example
        S = schurRing(QQ,s);
        toO s_{2}
        toO s_{1,1}
        toO s_{3}
        toO (2*s_{2,1} - s_{1,1,1})

      Text

        If the target orthogonal ring is not specified, the first call to {\tt toO}
        on an element of {\tt S} creates and caches an associated stable orthogonal
        ring; alternatively one can supply the target ring explicitly.

      Example
        O = schurRing(QQ,o,4,GroupActing => "O");
        toO(s_{2,1},O)

      Text

        The conversion is inverse to @TO toS@ on the orthogonal ring.  At
        stable rank, {\tt toS(o_{(2)}) = s_{(2)} - 1} and
        {\tt toS(o_{(1,1)}) = s_{(1,1)}}; the product {\tt o_{(1)}^2}
        equals the Newell-Littlewood product
        {\tt o_{(2)} + o_{(1,1)} + 1}.

      Example
        O = schurRing(QQ,o,GroupActing => "O");
        toS o_{2}
        toS o_{1,1}
        toO(toS(o_{2,1}, S), O) == o_{2,1}
        o_{1}*o_{1}

      Text

        At finite rank, the modification rule for types B_n / D_n is applied
        automatically.  The tag @TO OddOrEven@ (default {\tt "Odd"})
        distinguishes {\tt O(2n+1)} (type B_n) from {\tt O(2n)} (type D_n)
        and is used by @TO dim@ to pick the right Weyl dimension formula.
        Plethysm routes through the GL Schur ring.

      Example
        OB2 = schurRing(QQ,oB,2,GroupActing => "O", OddOrEven => "Odd");
        oB_{1}^2
        dim oB_{2,2}
        plethysm({2}, oB_{1})

      Text

        The simplest nontrivial example of the inverse Koike
        branching is the second symmetric power: {\tt s_{(2)}}
        decomposes as {\tt o_{(2)} + 1} (the traceless part plus
        the invariant form):

      Example
        S = schurRing(QQ,s);
        toO s_{2}

      Text

        The conversion composes with the other symmetric-function
        transitions.  One can start from an element of a
        Symmetric ring expressed in {\tt e}- or {\tt h}-variables,
        and let {\tt toO} route it through the Schur basis:

      Example
        R = symmetricRing(QQ,5);
        toO (e_2)
        toO (h_2 * h_1)

      Text

        A rank comparison between odd and even orthogonal groups
        shows that the Weyl dimension truly depends on the value
        of {\tt OddOrEven}: the partition {\tt (2,1)} indexes a
        {\tt 105}-dimensional irreducible of {\tt O(2\cdot 3+1) = O(7)}
        and a {\tt 64}-dimensional irreducible of {\tt O(2\cdot 3) = O(6)}:

      Example
        Oodd  = schurRing(QQ, od, 3, GroupActing => "O", OddOrEven => "Odd");
        Oeven = schurRing(QQ, oe, 3, GroupActing => "O", OddOrEven => "Even");
        dim od_{2,1}
        dim oe_{2,1}

   SeeAlso
     toSp
     toS
     GroupActing
     OddOrEven
     dim
     plethysm
///

doc ///
Key
  branch
  (branch,RingElement,SchurRing,SchurRing)
  (branch,RingElement,ZZ,ZZ)
Headline
  Restrict a Schur, Sp, or O character along a two-factor subgroup
Usage
  h = branch(f, S1, S2)
  h = branch(f, m, n)
Inputs
  f:RingElement
    an element of a SchurRing of type GL, Sp, or O (stable or finite rank)
  S1:SchurRing
    the first factor ring (same GroupActing as the ring of {\tt f})
  S2:SchurRing
    the second factor ring (same GroupActing as the ring of {\tt f})
  m:ZZ
    the rank of the first factor (alternate interface)
  n:ZZ
    the rank of the second factor
Outputs
  h:HashTable
    mapping pairs of partitions {\tt (mu, nu)} to coefficients.  The entry {\tt h#(mu,nu)} is the multiplicity of the irreducible indexed by the pair in the branching of {\tt f}.
Description
  Text

    Implements the classical branching rules of R.\ C.\ King,
    {\it Branching rules for classical Lie groups using tensor and
    spinor methods}, J.\ Phys.\ A {\bf 8} (1975), 429--449.  For any
    partition $\lambda$ and a two-factor restriction of the classical
    group, the character decomposes by a universal formula expressed
    via the triple Littlewood--Richardson coefficient
    $c^\lambda_{\alpha,\beta,\gamma}$ (the coefficient of
    $s_\lambda$ in $s_\alpha s_\beta s_\gamma$).

    $\bullet$ {\tt GL}:  $s_\lambda \mid_{GL(m)\times GL(n)} = \sum c^\lambda_{\mu,\nu}\, s_\mu \otimes s_\nu$
    (the coproduct of Schur functions).

    $\bullet$ {\tt Sp}:  $sp_\lambda \mid_{Sp(2m)\times Sp(2n)} = \sum_{\delta\text{ cols-even}} c^\lambda_{\delta,\mu,\nu}\, sp_\mu \otimes sp_\nu$,
    the sum running over partitions $\delta$ whose conjugate has all parts even (equivalently, the parts of $\delta$ appear with even multiplicity).

    $\bullet$ {\tt O}:  $o_\lambda \mid_{O(a)\times O(b)} = \sum_{\delta\text{ rows-even}} c^\lambda_{\delta,\mu,\nu}\, o_\mu \otimes o_\nu$,
    where $\delta$ ranges over partitions with all parts even.

    For finite-rank factors the output partitions are collapsed via the Sam--Snowden--Weyman modification rules, so any $(\mu,\nu)$ that is killed or re-signed by the rule is handled automatically.

  Example
    S = schurRing(QQ, s, infinity);
    A = schurRing(QQ, a, infinity);
    B = schurRing(QQ, b, infinity);
    pairs branch(S_{2,1}, A, B)
  Text

    A slightly larger GL example: the restriction of $s_{3,2}$ along
    $GL(\cdot) \times GL(\cdot)$ produces the full coproduct of the
    Schur function, one term per ordered pair $(\mu,\nu)$ with
    $c^{(3,2)}_{\mu,\nu}$ nonzero:

  Example
    pairs branch(S_{3,2}, A, B)
  Text

    The Sp branching picks up an extra {\tt (mu, nu) = ({}, {})} term for $sp_{1,1}$ via $\delta = (1,1)$:

  Example
    Sp  = schurRing(QQ, sp,  infinity, GroupActing => "Sp");
    Asp = schurRing(QQ, asp, infinity, GroupActing => "Sp");
    Bsp = schurRing(QQ, bsp, infinity, GroupActing => "Sp");
    pairs branch(Sp_{1,1}, Asp, Bsp)
  Text

    For $sp_{2,1}$ the Sp branching sum runs over the two columns-even
    deltas $\delta = ()$ and $\delta = (1,1)$; the latter gives the
    correction terms supported on partitions of total weight one:

  Example
    pairs branch(Sp_{2,1}, Asp, Bsp)
  Text

    On the orthogonal side, the branching of $o_{2,1}$ runs
    over rows-even deltas $\delta = ()$ and $\delta = (2)$,
    contributing correction terms of total weight one:

  Example
    O  = schurRing(QQ, oGp, infinity, GroupActing => "O");
    AO = schurRing(QQ, aO,  infinity, GroupActing => "O");
    BO = schurRing(QQ, bO,  infinity, GroupActing => "O");
    pairs branch(oGp_{2,1}, AO, BO)
  Text

    The {\tt ZZ, ZZ} form is a convenience that builds anonymous factor rings of the requested ranks (with the same {\tt GroupActing} and, for O, the same {\tt OddOrEven}).

  Example
    Sp4 = schurRing(QQ, sp4, 2, GroupActing => "Sp");
    pairs branch(Sp4_{2,1}, 1, 1)
  Text

    The {\tt (m,n)} shortcut agrees with the result of spelling out
    anonymous factor rings.  For the GL branching of $s_{3,2}$ along
    $GL(2)\times GL(2)$:

  Example
    pairs branch(S_{3,2}, 2, 2)
SeeAlso
  schurRing
  specialize
  GroupActing
  modificationRule
///


doc ///
Key
  toRatGL
  (toRatGL,RingElement,SchurRing)
Headline
  Lift a Schur (GL, SL) character into a rational-GL Schur ring
Usage
  g = toRatGL(f, R)
Inputs
  f:RingElement
    an element of a SchurRing with GroupActing {\tt "GL"} or {\tt "SL"}, or already {\tt "RatGL"}
  R:SchurRing
    a target SchurRing with GroupActing {\tt "RatGL"}
Outputs
  g:RingElement
    the image of {\tt f} in {\tt R}, with each Schur basis element $s_\lambda$ sent to the rational Schur character $s_{\lambda, ()}$
Description
  Text

    A GroupActing {\tt "RatGL"} ring represents rational GL characters
    with bipartition weights $(\alpha, \beta)$, where $\alpha$ is the
    positive part and $\beta$ the negative part of the highest weight.
    See Koike, {\it On the decomposition of tensor products of the
    representations of classical groups}, Adv.\ Math.\ {\bf 74}
    (1989).  Ordinary GL/SL Schur characters $s_\lambda$ embed as
    rational characters with trivial second weight, $s_{\lambda, ()}$;
    {\tt toRatGL} performs this embedding coefficientwise.

    The IndexedVariableTable for a RatGL ring accepts either a bipartition
    ($s_{\{\alpha,\beta\}}$) or a plain partition ($s_{2,1}$, which is
    implicitly $s_{\{2,1\},\{\}}$).

  Example
    G = schurRing(QQ, getSymbol "g", infinity, GroupActing => "GL");
    R = schurRing(QQ, getSymbol "sRat", infinity, GroupActing => "RatGL");
    toRatGL(g_{2,1} + 3 * g_1, R)
  Text

    The simplest embedding sends the standard GL character $s_{1}$
    to the rational character $s_{(1),()}$.  A slightly richer
    input (here an LR product re-expressed by {\tt toRatGL}) goes
    through coefficientwise, so the GL-basis expansion on the right
    matches the one on the left:

  Example
    toRatGL(g_{2,1}, R)
    toRatGL(g_{1} * g_{1}, R)
  Text

    Rational GL characters admit negative powers of the determinant.
    The bipartition $s_{(), (1)}$ is the dual of the standard
    representation, and the tensor product of the standard with its
    dual decomposes as $s_{(1),(1)} \oplus s_{(),()}$ (the adjoint
    representation plus the trivial):

  Example
    Rdual = schurRing(QQ, ratD, infinity, GroupActing => "RatGL");
    ratD_{{1},{}} * ratD_{{},{1}}
  Text

    If {\tt f} already lives in a RatGL ring, it is re-embedded into
    the target {\tt R}.  When {\tt R} has finite rank $n$, a pair
    $(\alpha,\beta)$ with $\ell(\alpha)+\ell(\beta) \leq n$ is
    {\it admissible} and $s_{\alpha,\beta}$ is left unchanged;
    otherwise the Koike--Terada modification rule is applied.  The
    rule iteratively removes a border strip of length
    $L = \ell(\alpha)+\ell(\beta)-n-1$ starting at the first box of
    the last row, from both $\alpha$ and $\beta$, contributing a sign
    $(-1)^{c(R_\alpha)+c(R_\beta)-1}$ per step (where $c(R)$ counts
    the columns the strip occupies), until the result is admissible.
    The character vanishes if at some step $L = 0$ or either
    partition admits no border strip of the required length.

    The admissible boundary case $\ell(\alpha)+\ell(\beta) = n+1$
    therefore always vanishes ($L = 0$):

  Example
    S = schurRing(QQ, getSymbol "sStab", infinity, GroupActing => "RatGL");
    T = schurRing(QQ, getSymbol "sFin",  3,        GroupActing => "RatGL");
    toRatGL(sStab_{{1,1},{1,1}} + sStab_{{1},{1}}, T)
  Text

    Embedding the stable bipartitions $((2,1),(\,))$,
    $((1,1,1),(\,))$, and $((1),(1,1))$ into a finite-rank $GL(2)$
    rational ring keeps the first and kills the last two.  The first
    is admissible; the other two hit the boundary
    $\ell(\alpha)+\ell(\beta) = n+1 = 3$ so $L = 0$:

  Example
    Tfin2 = schurRing(QQ, getSymbol "sFin2", 2, GroupActing => "RatGL");
    toRatGL(sStab_{{2,1},{}}, Tfin2)
    toRatGL(sStab_{{1,1,1},{}}, Tfin2)
    toRatGL(sStab_{{1},{1,1}}, Tfin2)
  Text

    When $\ell(\alpha)+\ell(\beta) > n+1$ the rule is genuinely
    non-trivial and can produce a non-zero modified bipartition
    with a sign.  For instance, at $GL(3)$ with
    $\alpha = (4,3,2,2)$ and $\beta = (5,2,2,1,1)$ the rule removes
    a border strip of length $5$ from each partition, leaving
    $(4,1,1)$ and $(5,1)$; a further pass (now with $L=1$) strips
    one box from each, giving $(4,1)$ and $(5)$ with an overall
    sign of $-1$:

  Example
    Tfin3 = schurRing(QQ, getSymbol "sFin3", 3, GroupActing => "RatGL");
    toRatGL(sStab_{{4,3,2,2},{5,2,2,1,1}}, Tfin3)

  Text

    Another non-trivial example at $GL(4)$: the columns
    $(1^3,1^3)$ satisfy $\ell(\alpha)+\ell(\beta) = 6 > n+1 = 5$,
    so one border strip of length $L = 1$ is removed from each,
    producing $(1^2,1^2)$ with an overall sign of $-1$:

  Example
    Tfin4 = schurRing(QQ, getSymbol "sFin4", 4, GroupActing => "RatGL");
    toRatGL(sStab_{{1,1,1},{1,1,1}}, Tfin4)
SeeAlso
  schurRing
  specialize
  GroupActing
///


doc ///
Key
  toSymm
  (toSymm,RingElement)
  (toSymm,Number)
Headline
  Convert a Schur ring element to an element of the associated symmetric ring
Usage
  g = toSymm f
Inputs
  f:RingElement
    typically an element of a @TO SchurRing@ (scalars are returned unchanged)
Outputs
  g:RingElement
    the image of {\tt f} in the @TO symmetricRing@ attached to its parent ring
Description
  Text

    Every @TO SchurRing@ has an associated @TO symmetricRing@ with
    variables $e_i$, $h_i$, $p_i$.  {\tt toSymm} rewrites a Schur-basis
    element in that symmetric ring via the Jacobi-Trudi determinant
    $s_\lambda = \det(h_{\lambda_i - i + j})$.  Scalars are returned
    unchanged.

    This is the dual of @TO toS@, and the two together let you move
    freely between Schur-basis and $e$/$h$/$p$-basis representations.

  Example
    S = schurRing(QQ, s, 4);
    toSymm s_{2,1}
  Example
    toS oo
  Text

    A larger partition produces a bigger Jacobi-Trudi expansion in
    the $e$-basis; here is $s_{3,2,1}$ written in the
    symmetric ring:

  Example
    toSymm s_{3,2,1}
  Text

    {\tt toSymm} is additive and is a left-inverse of @TO toS@, so
    {\tt toS toSymm} is the identity on Schur-basis elements.  It
    distributes over sums and scalars just like any ring map:

  Example
    toSymm(s_{2,1} + 3 * s_{1,1})
    toS toSymm s_{2,1} == s_{2,1}
  Text

    In a tensor-product (two-layer) Schur ring, {\tt toSymm} is
    applied to the outermost layer only, so a product like
    $s_{2,1}\otimes t_{1,1}$ returns an element of the symmetric
    ring of {\tt S} times the original {\tt t} factor:

  Example
    T = schurRing(S, t, 3);
    toSymm(s_{2,1} * t_{1,1})
SeeAlso
  toS
  toE
  toH
  toP
  jacobiTrudi
///


doc ///
Key
  toGL
  (toGL,RingElement)
  (toGL,RingElement,SchurRing)
Headline
  Express an element in the plain GL Schur basis
Usage
  g = toGL f
  g = toGL(f, T)
Inputs
  f:RingElement
    any symmetric-function-like element (in a @TO SchurRing@ of any
    flavor or in a @TO symmetricRing@)
  T:SchurRing
    optional target ring; must have {\tt GroupActing => "GL"}
Outputs
  g:RingElement
    {\tt f} re-expressed in the Schur basis of an associated (or
    user-supplied) GL character ring
Description
  Text

    {\tt toGL} is a readability synonym for @TO toS@.  It is provided
    so that calling code documenting an intent to obtain a {\em GL
    character} reads naturally, in contrast to the more neutral
    {\tt toS}.  Conversions from @TO "Basis"@ {\tt "Monomial"}
    rings and from {\tt GroupActing}-variant rings ({\tt "Sp"},
    {\tt "O"}, {\tt "SL"}, {\tt "RatGL"}) are handled transparently.

  Example
    R = symmetricRing(QQ, 4);
    toGL(R_{0} * R_{5})  -- e_1 * h_2
  Example
    T = schurRing(QQ, t, 3);
    toGL(R_{0} * R_{5}, T)
  Text

    {\tt toGL} and @TO toS@ produce the same output on symmetric-ring
    elements; only the name of the function differs:

  Example
    toGL(e_1 * h_2) == toS(e_1 * h_2)
    toGL(h_3 - p_3) == toS(h_3 - p_3)
  Text

    A monomial-basis Schur ring expands to the plain Schur basis
    in the same way as a Kostka-inverse computation: the monomial
    symmetric function $m_\lambda$ is a signed sum of Schur
    functions.

  Example
    M = schurRing(QQ, m, 3, Basis => "Monomial");
    toGL m_{2,1}
  Text

    Conversion from an {\tt Sp}-ring applies the Koike-Terada
    branching rule that expands a symplectic character in the GL
    Schur basis; the {\tt GL(3)} Schur expansion of the $Sp(6)$
    character $sp_{2,1}$ is:

  Example
    Sp = schurRing(QQ, sp, 3, GroupActing => "Sp");
    toGL sp_{2,1}
  Text

    Finally, {\tt toGL} of a polynomial expression built from
    $e$-, $h$-, or $p$-generators in a {\tt symmetricRing} is an
    efficient way to ask for its Schur decomposition:

  Example
    U = symmetricRing(QQ, 4);
    toGL(e_1 * p_2 + h_3)
SeeAlso
  toS
  toSn
  toSp
  toO
  convert
///


doc ///
Key
  toSn
  (toSn,RingElement,SchurRing)
Headline
  Promote a Schur-basis element into an Sn character ring
Usage
  g = toSn(f, T)
Inputs
  f:RingElement
    element of a @TO SchurRing@ or @TO symmetricRing@
  T:SchurRing
    target ring; must have {\tt GroupActing => "Sn"}
Outputs
  g:RingElement
    the element of {\tt T} with the same partition-indexed
    coefficients as {\tt f} (after converting {\tt f} to the Schur
    basis)
Description
  Text

    The GL and Sn flavors of a @TO SchurRing@ share the same partition
    index set -- the distinction is in the multiplication (LR product
    vs.\ internal product) and in the semantics (polynomial GL
    character vs.\ Frobenius characteristic of an $S_n$-class
    function).  {\tt toSn} simply carries the coefficient data across.

    Finite-rank targets drop partitions with more than {\tt numgens T}
    parts.  Inputs in a variant basis ({\tt "Sp"}, {\tt "O"},
    {\tt "RatGL"}, {\tt "Monomial"}) are first expanded in the plain
    Schur basis via @TO toS@.

  Example
    S  = schurRing(QQ, s,  4);
    Sn = schurRing(QQ, n, 4, GroupActing => "Sn");
    toSn(s_{2,1} + 3 * s_{1,1,1}, Sn)
  Example
    -- internal product: trivial rep at n=3 has Frobenius characteristic s_3
    a = toSn(s_3, Sn);
    a * a
  Text

    Partition labels on the two sides match, but the multiplication
    does not: the GL (LR) product of $s_{2,1}$ with itself lives
    in degree 6, while the $S_3$-Kronecker product of the standard
    representation with itself stays in degree 3 and decomposes as
    $n_{3} + n_{2,1} + n_{1,1,1}$:

  Example
    S3  = schurRing(QQ, s3, 3);
    Sn3 = schurRing(QQ, n3, 3, GroupActing => "Sn");
    s3_{2,1} * s3_{2,1}
    b = toSn(s3_{2,1}, Sn3);
    b * b
  Text

    Inputs in variant bases ({\tt "Sp"}, {\tt "O"}, {\tt "RatGL"},
    {\tt "Monomial"}) are first expanded through @TO toS@, so
    {\tt toSn} also accepts symplectic or orthogonal characters and
    returns the corresponding $S_n$-class function:

  Example
    Sp  = schurRing(QQ, sp,  3, GroupActing => "Sp");
    toSn(sp_{2,1}, Sn3)
    Oo  = schurRing(QQ, ooX, 3, GroupActing => "O");
    toSn(ooX_{2,1}, Sn3)
SeeAlso
  toS
  toGL
  internalProduct
  convert
///


doc ///
Key
  convert
  (convert,RingElement,Ring)
Headline
  Universal dispatcher for converting between Schur ring flavors
Usage
  g = convert(f, T)
Inputs
  f:RingElement
    any symmetric-function-like element
  T:Ring
    target @TO SchurRing@ or @TO symmetricRing@
Outputs
  g:RingElement
    {\tt f} expressed in the natural basis of {\tt T}
Description
  Text

    {\tt convert} is a thin routing layer that inspects the target
    ring's classification and dispatches to the appropriate
    specialized converter:

    \begin{itemize}
      \item @TO SchurRing@ with {\tt GroupActing => "GL"}  $\to$ @TO toS@,
      \item @TO SchurRing@ with {\tt GroupActing => "Sn"}  $\to$ @TO toSn@,
      \item @TO SchurRing@ with {\tt GroupActing => "Sp"}  $\to$ @TO toSp@,
      \item @TO SchurRing@ with {\tt GroupActing => "O"}   $\to$ @TO toO@,
      \item @TO SchurRing@ with {\tt GroupActing => "SL"}  $\to$ @TO toS@,
      \item @TO SchurRing@ with {\tt GroupActing => "RatGL"} $\to$ @TO toRatGL@,
      \item @TO symmetricRing@ $\to$ @TO toSymm@ followed by promotion.
    \end{itemize}

    No new conversion mathematics is performed here; {\tt convert}
    exists purely to simplify user-level code that does not want to
    know which specialized converter to call.

  Example
    S  = schurRing(QQ, s, 4);
    Sp = schurRing(QQ, p, 2, GroupActing => "Sp");
    O  = schurRing(QQ, o, 4, GroupActing => "O");
    Sn = schurRing(QQ, n, 4, GroupActing => "Sn");
    convert(s_{2,1}, Sp)
    convert(s_{2,1}, O)
    convert(s_{2,1}, Sn)
  Text

    The dispatch works from any source basis to any target basis.
    Below we start from a monomial-basis Schur ring and send the
    same element into each of the flavors -- the dispatcher
    selects {\tt toS}, {\tt toSp}, {\tt toO}, and {\tt toSn}
    respectively:

  Example
    M    = schurRing(QQ, m, 4, Basis => "Monomial");
    convert(m_{2,1}, S)
    convert(m_{2,1}, Sp)
    convert(m_{2,1}, O)
    convert(m_{2,1}, Sn)
  Text

    To a rational-GL target, the dispatcher uses @TO toRatGL@,
    which embeds a plain GL Schur character $s_\lambda$ as the
    bipartition $s_{(\lambda, ())}$:

  Example
    RRat = schurRing(QQ, rRat, 4, GroupActing => "RatGL");
    convert(s_{2,1}, RRat)
    convert(s_{3} + 2*s_{1,1}, RRat)
  Text

    A {\tt symmetricRing} source is also handled: here the
    dispatcher routes through {\tt toSymm} and lands in the
    Schur basis of the target ring.

  Example
    U = symmetricRing(QQ, 4);
    convert(e_1 * h_2, S)
    convert(p_3, S)
SeeAlso
  toS
  toSn
  toGL
  toSp
  toO
  toRatGL
  toSymm
///


doc ///
Key
  specialize
  (specialize,RingElement,ZZ)
  (specialize,RingElement,List)
Headline
  Specialize a stable character to a finite rank
Usage
  g = specialize(f, n)
  g = specialize(f, ranks)
Inputs
  f:RingElement
    an element of a @TO SchurRing@ (of any {\tt GroupActing} flavor)
  n:ZZ
    the target rank (a nonnegative integer)
  ranks:List
    a list of target ranks, one per @TO schurLevel@; an entry equal to
    {\tt infinity} leaves that layer unchanged
Outputs
  g:RingElement
    the image of {\tt f} in the corresponding finite-rank ring
Description
  Text

    Every ring produced by @TO schurRing@ comes in two sizes: the
    {\em stable} ring (rank {\tt infinity}), which is a universal
    object admitting arbitrarily many row labels, and the finite-rank
    ring (rank {\tt n}), on which the relevant representation-theoretic
    modification rule is enforced (see @TO modificationRule@).
    The function {\tt specialize} bridges the two: it maps every
    partition-indexed basis element of the stable ring to its image in
    the finite-rank ring, collapsing or re-signing partitions that are
    ``too long'' via the modification rule of
    Sam-Snowden-Weyman (for {\tt Sp}/{\tt O}/{\tt RatGL}) or simply
    truncating (for {\tt GL}/{\tt SL}/{\tt Sn}).

    {\bf GL specialization} drops every Schur label with more than
    {\tt n} parts:

  Example
    S = schurRing(QQ, s, infinity);
    f = s_{3,2,1} + s_{2,1} + s_{1}
    specialize(f, 3)
    specialize(f, 2)

  Text

    {\bf Sp specialization} applies the type-C modification rule:
    characters $sp_\lambda$ with $\ell(\lambda) > n$ are re-expressed
    in the finite-rank ring (possibly with a sign, or as zero).

  Example
    Sp = schurRing(QQ, sp, infinity, GroupActing => "Sp");
    specialize(sp_{2,1} + sp_{1,1,1}, 2)
    specialize(sp_{1,1,1}, 1)

  Text

    Concrete low-rank $Sp$ specializations show the modification in
    action.  At rank 1 ($Sp(2)$), $sp_{1,1,1}$ is modified via
    {\tt "C"} to $-sp_{1}$, and at rank 2 ($Sp(4)$), $sp_{2,1,1}$
    has $\ell(\lambda) = 3 > 2$ and is also modified to a signed
    lower-rank character:

  Example
    specialize(sp_{1,1,1}, 1)
    specialize(sp_{2,1,1}, 1)

  Text

    {\bf O specialization} distinguishes type $B_n$ ({\tt O(2n+1)})
    from type $D_n$ ({\tt O(2n)}) via the @TO OddOrEven@ option.  If
    the stable ring has a stored {\tt OddOrEven} attribute, that value
    is used; otherwise the option must be supplied at the call site.

  Example
    O = schurRing(QQ, o, infinity, GroupActing => "O");
    specialize(o_{2,1}, 3)
    specialize(o_{2,1}, 2, OddOrEven => "Even")

  Text

    The two $O$ flavors give genuinely different images of the same
    partition on the same target rank.  For $\lambda = (2,1)$ at
    rank 3 we compare $O(7)$ with $O(6)$:

  Example
    specialize(o_{2,1}, 3, OddOrEven => "Odd")
    specialize(o_{2,1}, 3, OddOrEven => "Even")

  Text

    {\bf SL specialization} drops rows of length equal to the full
    rank, i.e. "columns" of height $n$, because the determinant
    representation is trivial in $SL(n)$.  Here the stable
    $s_{3,2}$ collapses to $s_1$ in $SL(2)$, and $s_{3,3,1}$
    collapses to $s_{2,2}$ in $SL(3)$:

  Example
    SL = schurRing(QQ, sl, infinity, GroupActing => "SL");
    specialize(sl_{3,2}, 2)
    specialize(sl_{3,3,1}, 3)

  Text

    {\bf Tower specialization}.  For a @TO SchurRing@ obtained by
    iterating the @TO schurRing@ constructor over a coefficient ring
    that is itself a @TO SchurRing@, one can specialize several layers
    at once.  The layers are listed from outermost to innermost, and
    an entry equal to {\tt infinity} leaves that layer stable.

  Example
    A = schurRing(QQ, a, infinity);
    B = schurRing(A, b, infinity, GroupActing => "Sp");
    specialize(b_{1,1} * a_{2}, {2, 3})

  Text

    In a two-layer tower of GL and Sp flavors, the outer and inner
    ranks can be adjusted independently; here is the same element
    specialized to a second choice of ranks:

  Example
    specialize(b_{2,1} + a_{3,1,1}, {3, 2})

SeeAlso
  schurRing
  modificationRule
  OddOrEven
  toSp
  toO
  toRatGL
///

doc ///
Key
  OddOrEven
  [schurRing,OddOrEven]
  [symmetricRing,OddOrEven]
  [specialize,OddOrEven]
Headline
  Select type $B_n$ or type $D_n$ for an orthogonal Schur ring
Description
  Text
    This is an optional argument for the @TO schurRing@ constructor
    (and, by extension, the @TO symmetricRing@ and @TO specialize@
    methods), relevant only when {\tt GroupActing => "O"}.  It
    distinguishes the two flavors of orthogonal group: odd
    ({\tt O(2n+1)}, type $B_n$) and even ({\tt O(2n)}, type $D_n$).
    Its possible values are {\tt "Odd"} (the default) and {\tt "Even"};
    any other value raises an error.

    For stable orthogonal rings (rank {\tt infinity}) the distinction
    is invisible at the level of multiplication, but it affects the
    dimension formula and the modification rule applied when
    @TO specialize@ is called.

  Example
    OB = schurRing(QQ, oB, 2, GroupActing => "O", OddOrEven => "Odd");
    OD = schurRing(QQ, oD, 2, GroupActing => "O", OddOrEven => "Even");
    dim oB_{1}
    dim oD_{1}

  Text

    At rank 3 and $\lambda = (2,1)$, the difference is much more
    pronounced: {\tt O(7)} (type $B_3$) gives a 105-dimensional
    irreducible, whereas {\tt O(6)} (type $D_3$) gives a
    64-dimensional one:

  Example
    Oodd  = schurRing(QQ, od, 3, GroupActing => "O", OddOrEven => "Odd");
    Oeven = schurRing(QQ, oe, 3, GroupActing => "O", OddOrEven => "Even");
    dim od_{2,1}
    dim oe_{2,1}

  Text

    The same partition of larger weight separates the two flavors
    by an even bigger factor.  For $\lambda = (3,2)$ the $O(7)$
    Weyl dimension is 693 while the $O(6)$ Weyl dimension is 300:

  Example
    dim od_{3,2}
    dim oe_{3,2}

  Text

    Supplying {\tt OddOrEven} for a non-orthogonal ring (for example
    {\tt GroupActing => "Sp"} or {\tt "GL"}) is an error.

  Example
    try (schurRing(QQ, bad, 2, GroupActing => "GL", OddOrEven => "Odd")) else print "schurRing rejected OddOrEven on a non-O ring"

SeeAlso
  GroupActing
  schurRing
  specialize
  toO
  dim
///

doc ///
Key
  modificationRule
Headline
  Apply the Sam-Snowden-Weyman modification rule
Usage
  result = modificationRule(lambda, n, type)
Inputs
  lambda:BasicList
    a partition
  n:ZZ
    the target rank
  type:String
    one of {\tt "C"}, {\tt "B"}, or {\tt "D"} -- see below
Outputs
  result:Sequence
    either a pair {\tt (tau, sign)} giving the modified partition
    $\tau$ and a sign $\pm 1$, or @TO null@ if $\lambda$ reduces to
    zero under the rule
Description
  Text

    This is the underlying combinatorial primitive used by
    @TO specialize@ and by the finite-rank multiplication in
    {\tt Sp} and {\tt O} character rings.  It implements the
    modification rules of Sam, Snowden, and Weyman, which describe
    how a universal classical group character becomes a character of
    the finite-rank group (or vanishes).

    The {\tt type} argument selects the classical family:

    $\bullet$ {\tt "C"}: symplectic groups {\tt Sp(2n)}.

    $\bullet$ {\tt "B"}: odd orthogonal groups {\tt O(2n+1)}.

    $\bullet$ {\tt "D"}: even orthogonal groups {\tt O(2n)}.

    Given a partition {\tt lambda}, the rule either returns a pair
    {\tt (tau, sign)}, meaning that the universal character indexed by
    {\tt lambda} equals {\tt sign} times the finite-rank character
    indexed by {\tt tau}, or returns {\tt null}, meaning the
    finite-rank character vanishes.

  Example
    modificationRule({2,1,1}, 1, "C")
    modificationRule({2,1,1}, 2, "C")
    modificationRule({3,1,1}, 1, "B")
    modificationRule({2,2}, 1, "D")

  Text

    A partition can become ``stuck in the bulk'' after modification:
    the rule reduces a long partition to a shorter (but still
    nonempty) partition, possibly with a sign.  For example
    $\lambda = (4,4,2,1)$ at rank 2 in type $B$ reduces to
    $\tau = (4,4,1)$ with sign $-1$:

  Example
    modificationRule({4,4,2,1}, 2, "B")

  Text

    Other partitions cancel to {\tt null}: the rule applies and the
    finite-rank character vanishes outright.  For instance, in type
    $C$ the partition $(3,2,2,2,1)$ at rank 2 is killed, and so is
    $(4,3,2,1)$ at rank 2:

  Example
    modificationRule({3,2,2,2,1}, 2, "C")
    modificationRule({4,3,2,1}, 2, "C")

  Text

    Type $B$ and type $D$ give different answers on the same
    partition even at the same rank.  For $\lambda = (3,2,1)$ at
    rank 2, type $B$ keeps the partition unchanged (with sign
    $+1$) while type $D$ kills it:

  Example
    modificationRule({3,2,1}, 2, "B")
    modificationRule({3,2,1}, 2, "D")

  Text

    Conversely, $\lambda = (4,3,2)$ at rank 2 is killed by type
    $B$ but survives (with a sign) in type $D$:

  Example
    modificationRule({4,3,2}, 2, "B")
    modificationRule({4,3,2}, 2, "D")

  Text

    Most users will not call {\tt modificationRule} directly; it is
    invoked automatically by the finite-rank multiplication and by
    @TO specialize@.  It is exported so that library code that wishes
    to implement custom variants (e.g. twisted character rings, or
    non-standard specialization schemes) can share the same
    combinatorics.

SeeAlso
  specialize
  toSp
  toO
  GroupActing
  OddOrEven
///

--------------------
-- test Jacobi-Trudi
--------------------
TEST ///
E = symmetricRing(QQ,5)
f = jacobiTrudi({4,1},E)
g = toS f
G = ring g
assert (g == G_{4,1})
///

TEST ///
E = symmetricRing(QQ,5)
f = jacobiTrudi({2,1},E)
g = toS f
G = ring g
assert (g == G_{2,1})
///

TEST ///
E = symmetricRing(QQ,13)
f = jacobiTrudi({7,4,2},E)
g = toS f
G = ring g
assert (toS f == G_{7,4,2})
///

TEST ///
P = symmetricRing(QQ,6)
f = toS plethysm(jacobiTrudi({2},P), jacobiTrudi({2},P))
G = ring f
assert(f == G_{4}+G_{2,2})
///

TEST ///
Q = symmetricRing(QQ,5)
--S = schurRing(QQ,q,4)
S = schurRing Q
f = toS(plethysm(jacobiTrudi({3},Q), jacobiTrudi({2},Q)))
--assert(dim f == 220)
assert(dim(4,f) == 220)
///
------------------------
-- end test Jacobi-Trudi
------------------------

-----------
-- test dim
-----------

TEST ///
R = schurRing(r,3,GroupActing => "Sn")
S = schurRing(R,s,2)
T = schurRing(S,t,4,GroupActing => "Sn")
assert( (dim(r_1)) == 1 )
assert( (dim(s_2)) == 3 )
assert( (dim(s_3)) == 4 )
assert( (dim(t_4)) == 1 )
assert( (dim(r_1 * s_2 * t_3)) == 3 )
assert( (dim(r_1 * s_3 + t_4)) == 5 )
///

---------------
-- end test dim
---------------

---------------------
-- test plethysm, toS
---------------------
TEST ///
R = symmetricRing(QQ,4)
pl = plethysm({1,1},jacobiTrudi({2},R))
G = schurRing ring pl
assert(toS pl == G_{3,1})
///

TEST ///
R = symmetricRing(QQ,12)
pl = plethysm({1,1,1},jacobiTrudi({4},R))
assert(#listForm(toS pl) == 7)
///

TEST ///
R = symmetricRing(QQ, 9)
S = schurRing(QQ,q,3)
pl = plethysm(h_3,q_{2,1})
assert (dim(pl) == 120)
///

TEST ///
R = symmetricRing(QQ,3)
S = schurRing R
assert(toS(h_3 @ e_3) == S_{3,3,3})
///

TEST ///
S = schurRing(QQ,q,4)
assert(plethysm(q_{2,1},q_{1,1,1}) == q_{3,3,2,1})
///

TEST ///
R = symmetricRing(QQ, 12)
f = e_4
lambda = new Partition from {3}
assert(plethysm(lambda,f) == h_3 @ e_4)
///

TEST ///
schurRing(QQ,s,2)
assert(dim(plethysm(s_{2,1}+s_{3},s_{3})) == 40)
///

TEST ///
R = symmetricRing(QQ,20)
assert(#listForm(toS plethysm(h_5,h_4)) == 95)
///


TEST ///
R = symmetricRing(QQ, 10)
S = schurRing R
sch = toS(plethysm({2,1},h_3))
assert(dim(5,sch) == 14280)
///

TEST ///
R = symmetricRing 5
S = schurRing(s,3)
assert( ((h_2 + p_2) @ s_{2,1}) == 2*s_(4,2)-s_(4,1,1)-s_(3,3)+s_(3,2,1)+2*s_(2,2,2) )
///

TEST ///
S = schurRing(s,3)
T = schurRing(S,t,4,GroupActing => "Sn")
Q = schurRing(T,q,2)
assert( (s_2 @ q_{2,1}) == q_(4,2) )
assert( (s_{2,1} - s_{1,1,1} @ (t_3 + t_(2,1))) == -s_()*t_(1,1,1)+s_(2,1)*t_() )
assert( (plethysm({3},s_1 * t_1 * q_1)) == s_3*t_1*q_3+s_(2,1)*t_1*q_(2,1) )
assert( (plethysm({2,1},s_1 + t_1 + q_1)) == q_(2,1)+(t_1+s_1*t_())*q_2+(t_1+s_1*t_())*q_(1,1)+((2*s_1+s_())*t_1+(s_2+s_(1,1))*t_())*q_1+((s_2+s_(1,1)+s_1)*t_1+s_(2,1)*t_())*q_() )
assert( toH plethysm({2,1},s_1 + t_1 + q_1) == toH plethysm({2,1},toE(s_1 + t_1 + q_1)) )
///

----------------------------
-- end test of plethysm, toS
----------------------------

------------------------------------
----- symmetricPower & exteriorPower
------------------------------------

TEST ///
S = schurRing(s,5)
T = schurRing(S,t,3,GroupActing => "Sn")
assert( (symmetricPower(2,s_3)) == s_6+s_(4,2) )
assert( (symmetricPower(2,t_2)) == t_2 )
assert( (symmetricPower(2,s_3+t_2)) == (s_3+s_())*t_2+(s_6+s_(4,2))*t_() )
assert( (exteriorPower(3,s_3 * t_{2,1} - t_3)) == (s_(7,1,1)+s_(6,3)+s_(5,3,1)-s_(5,1)+s_(3,3,3)-s_(3,3)-s_())*t_3+(s_(8,1)+s_(7,2)+s_(7,1,1)+2*s_(6,3)+s_(6,2,1)+s_(5,4)+2*s_(5,3,1)-s_(5,1)+s_(4,3,2)+s_(3,3,3)-s_(3,3)+s_3)*t_(2,1)+(s_(7,1,1)+s_(6,3)-s_6+s_(5,3,1)-s_(4,2)+s_(3,3,3))*t_(1,1,1) )
assert( (exteriorPower(5,s_1 * t_1)) == s_(1,1,1,1,1)*t_1 )
///

TEST ///
R = symmetricRing(3,GroupActing => "Sn")
S = symmetricRing(R,4)
T = symmetricRing(S,2,GroupActing => "Sn")

a = R.hVariable 3
b = S.eVariable 4
c = T.pVariable 2

assert (toS(symmetricPower(3,a*b*c)) == symmetricPower(3,toS(a*b*c)))
assert (toS(exteriorPower(3,a*b*c)) == exteriorPower(3,toS(a*b*c)))
assert (toS(symmetricPower(3,a+b-c)) == symmetricPower(3,toS(a+b-c)))
assert (toS(exteriorPower(3,a*b-c)) == exteriorPower(3,toS(a*b-c)))
///

----------------------------------------
----- end symmetricPower & exteriorPower
----------------------------------------

-------------------------------------------------------------------
----- test characters of symmetric groups, scalarProd, internalProd
-------------------------------------------------------------------
TEST ///
R = symmetricRing(QQ,20)
S = schurRing(QQ,o,20)
assert(scalarProduct(o_{6,4,3,2,1},jacobiTrudi({3,3,3},symmetricRing S)*toP(o_{4,2,1})) == 2)
assert(scalarProduct(jacobiTrudi({6,4,3,2,1},R),jacobiTrudi({4,3,3,3,2,1},R)) == 0)
assert(scalarProduct(jacobiTrudi({6,4,3,2,1},R),o_{4,3,3,3,2,1}) == 0)
///

TEST ///
R = symmetricRing(QQ,5)
A = schurRing(QQ,a,4)
assert(internalProduct(e_2+h_2,a_{2}) == a_{2}+a_{1,1})
assert(toE internalProduct(a_{2},e_2+h_2) == toE p_1^2)
assert(dim internalProduct(a_{2,1}*a_{1},a_{2,2}) == 176)
///


TEST ///
R = symmetricRing(QQ,10)
ch1 = new ClassFunction from {{4,4} => 2, {8} => -1, {5,2,1} => 2, {3,2,2,1} => 1};
ch2 = new ClassFunction from {{2,2,2,2} => -4, {5,2,1} => 1, {3,2,2,1} => 3};
assert(toP symmetricFunction(internalProduct(ch1,ch2),R) == 1/8*p_1*p_2^2*p_3+1/5*p_1*p_2*p_5)
assert(toP symmetricFunction(ch1*ch2,R) == 1/8*p_1*p_2^2*p_3+1/5*p_1*p_2*p_5)
///

TEST ///
R = symmetricRing(QQ,4)
f = p_2^2
g = (e_2+h_2)^2
ch1 = classFunction(f)
ch2 = classFunction(g)
assert(symmetricFunction(internalProduct(ch1,ch2),R) == 0)
assert(internalProduct(f,g) == 0)
///
---------------------------------------------------------------------
--- end test characters of symmetric groups, scalarProd, internalProd
---------------------------------------------------------------------

---------------------------
--- test toS, toP, toE, toH
---------------------------
TEST ///
R = symmetricRing(QQ,6)
assert(toE(toS(e_1*e_2*e_3)) == e_1*e_2*e_3)
///

TEST ///
R = symmetricRing(QQ,5)
S = schurRing(QQ,q,3)
assert(toE(q_{2}) + e_2 == e_1^2)
///

TEST///
R = symmetricRing(QQ, 4)
assert(toP toE toH toE toH toP toE toE toP toH (p_1+p_2+p_3) == p_1+p_2+p_3)
///

TEST ///
R = symmetricRing(QQ,6)
S = schurRing R
toSf = map(S, R, apply(gens R, x -> toS(x)))
assert(toSf(e_1*e_2*e_3) == S_(3,2,1)+S_(3,1,1,1)+S_(2,2,2)+2*S_(2,2,1,1)+2*S_(2,1,1,1,1)+S_(1,1,1,1,1,1))
assert(toSf(h_1*h_2*h_3) == S_{1}*S_{2}*S_{3})
///

TEST ///
R = symmetricRing(QQ,7)
assert(toH toP toE (toS (jacobiTrudi({2,1},R))^2) == (h_1*h_2-h_3)^2)
///

TEST ///
S = schurRing(s,5,GroupActing => "Sn")
R = symmetricRing S
T = schurRing(S,t,3,EHPVariables => (getSymbol "eT", getSymbol "hT", getSymbol "pT"))
Q = symmetricRing T
a = toS((R.pVariable 2) * (R.eVariable 3))
b = a * (t_3 - t_{2,1})
c = toH(a + b)
d = toE(a - b)
f = toP b
assert( (a) == s_(3,1,1)-s_(2,2,1)-s_(1,1,1,1,1) )
assert( (b) == (s_(3,1,1)-s_(2,2,1)-s_(1,1,1,1,1))*t_3+(-s_(3,1,1)+s_(2,2,1)+s_(1,1,1,1,1))*t_(2,1) )
assert( (c) == (h_1^5-4*h_1^3*h_2+4*h_1*h_2^2+h_1^2*h_3-2*h_2*h_3)*hT_1*hT_2+(-2*h_1^5+8*h_1^3*h_2-8*h_1*h_2^2-2*h_1^2*h_3+4*h_2*h_3)*hT_3-h_1^5+4*h_1^3*h_2-4*h_1*h_2^2-h_1^2*h_3+2*h_2*h_3 )
assert( (d) == (-e_1^2*e_3+2*e_2*e_3)*eT_1^3+(3*e_1^2*e_3-6*e_2*e_3)*eT_1*eT_2+(-2*e_1^2*e_3+4*e_2*e_3)*eT_3+e_1^2*e_3-2*e_2*e_3 )
assert( (f) == (-(1/36)*p_1^3*p_2+(1/12)*p_1*p_2^2-(1/18)*p_2*p_3)*pT_1^3+((1/12)*p_1^3*p_2-(1/4)*p_1*p_2^2+(1/6)*p_2*p_3)*pT_1*pT_2+((1/9)*p_1^3*p_2-(1/3)*p_1*p_2^2+(2/9)*p_2*p_3)*pT_3 )
assert( (toH(c - d - 2*f)) == 0 )
assert( (toE(c - d - 2*f)) == 0 )
assert( (toP(c - d - 2*f)) == 0 )
///
-------------------------------
--- end test toS, toP, toE, toH
-------------------------------

-------------------------------------------------
--- test schurLevel, symmetricRing, schurRing
-------------------------------------------------

TEST ///
R = symmetricRing 5
S = schurRing R
S1 = schurRing(R,s1,3,GroupActing => "Sn")
R1 = symmetricRing S1
R2 = symmetricRing(R1,3,GroupActing => "Sn")
S2 = schurRing R2
assert( (schurLevel QQ) == 0 )
assert( schurLevel (ZZ/5) == 0 )
assert( (schurLevel R) == 1 )
assert( (schurLevel R1) == 2 )
assert( (schurLevel R2) == 3 )
assert( (schurLevel S) == 1 )
assert( (schurLevel S1) == 2 )
assert( (schurLevel S2) == 3 )
///

-----------------------------------------------------
--- end test schurLevel, symmetricRing, schurRing
-----------------------------------------------------

-----------------------
-- test centralizerSize
-----------------------

TEST ///
assert( (centralizerSize{3,2,1}) === 144 )
assert( (centralizerSize{1,1,1}) === 6 )
assert( (centralizerSize{3}) === 6 )
assert( (centralizerSize{5,2,1,1,1}) === 57600 )
assert( (centralizerSize{5,5,5}) === 13436928000 )
assert( (centralizerSize{4,4,2,2}) === 5308416 )
assert( (centralizerSize{1}) === 1 )
///

---------------------------
-- end test centralizerSize
---------------------------

-----------------------
-- test schurResolution
-----------------------

TEST ///
S = schurRing(QQ,s,3)
rep = s_{2}
M = {1_S,s_{2},s_{4},s_{6},s_{8},s_{10},s_{12}}
sR = schurResolution(rep,M)
assert( (#sR) == 4 )
assert( (sR#2#0#1) == s_(3,2,1) )
assert( (last last sR) == (4,s_(3,3,2)) )

rep = s_{3}
M = {1_S,s_{3},s_{6},s_{9},s_{12},s_{15},s_{18},s_{21},s_{24},s_{27}}
d = 7
sR = schurResolution(rep,M,DegreeLimit => d)
assert( (#sR) == 7 )
assert( (sR#2#0#0) == 3 )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1}, {27}, {105}, {189}, {189}, {105}, {27}} )

T = schurRing(S,t,4)
rep = s_1 * t_1
M = {1_T} | apply(splice{1..8},i -> s_i * t_i)
sR = schurResolution(rep,M)
assert( (last last sR) == (8,s_(3,3,2)*t_(2,2,2,2)) )
assert( (first first sR) == (0,t_()) )
assert( (sR#1#0) == (2,s_(1,1)*t_(1,1)) )
assert( (sR#4#1) == (6,s_(2,2,2)*t_(2,2,2)) )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1},{18},{52},{60},{24,10},{12},{3}} )

n = 5;
S = schurRing(QQ,s,n,GroupActing => "Sn");
rep = s_n + s_{n-1,1};
M = {s_n}
sR = schurResolution(rep,M,DegreeLimit => n)
assert( (last sR#2) == (2,s_(4,1)+s_(3,1,1)) )
assert( (first sR#3) == (3,s_(3,1,1)+s_(2,1,1,1)) )
assert( (last last sR) == (5,s_(1,1,1,1,1)) )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1},{5},{10},{10},{5},{1}} )

M = {s_n} | splice{n:rep};
sR = schurResolution(rep,M)    
assert( (last sR#2) == (3,s_(4,1)+s_(3,2)+s_(3,1,1)+s_(2,2,1)) )
assert( (first sR#3) == (4,s_(3,1,1)+s_(2,2,1)+s_(2,1,1,1)) )
assert( (last last sR) == (5,s_(2,1,1,1)) )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1},{10},{20},{15},{4}} )
///

---------------------------
-- end test schurResolution
---------------------------

------------------------------------
-- tests for kostkaNumber
------------------------------------
TEST ///
-- Standard known Kostka numbers
assert(kostkaNumber({2,1},{2,1}) == 1)
assert(kostkaNumber({2,1},{1,1,1}) == 2)
assert(kostkaNumber({3},{2,1}) == 1)
assert(kostkaNumber({3},{3}) == 1)
assert(kostkaNumber({1,1,1},{1,1,1}) == 1)
assert(kostkaNumber({2,2},{2,1,1}) == 1)
-- Dominance order: K = 0 if mu does not dominate lambda
assert(kostkaNumber({2,1},{3}) == 0)
-- Size mismatch
assert(kostkaNumber({2,1},{2,2}) == 0)
///

------------------------------------
-- tests for toM
------------------------------------
TEST ///
S = schurRing(QQ,s,4);

-- Output is a RingElement in a monomial-basis SchurRing
r1 = toM s_{2,1};
assert(class ring r1 === SchurRing);
assert((ring r1).Basis == "Monomial");

-- s_{2,1} = m_{2,1} + 2 m_{1,1,1}
lf1 = new HashTable from listForm r1;
assert(lf1#{2,1} == 1);
assert(lf1#{1,1,1} == 2);
assert(#(keys lf1) == 2);

-- s_{3} = m_3 + m_{2,1} + m_{1,1,1}
r2 = toM s_{3};
lf2 = new HashTable from listForm r2;
assert(lf2#{3} == 1);
assert(lf2#{2,1} == 1);
assert(lf2#{1,1,1} == 1);

-- s_{1,1,1} = m_{1,1,1}
r3 = toM s_{1,1,1};
lf3 = new HashTable from listForm r3;
assert(lf3#{1,1,1} == 1);
assert(#(keys lf3) == 1);

-- Linearity
r4 = toM(s_{3} + 2*s_{2,1});
lf4 = new HashTable from listForm r4;
assert(lf4#{3} == 1);
assert(lf4#{2,1} == 3);
assert(lf4#{1,1,1} == 5);

-- Zero
r5 = toM(0_S);
assert(r5 == 0);

-- The associated monomial ring is cached across calls
assert(ring r1 === ring r2);

-- Two-argument form with user-supplied target ring
MM = schurRing(QQ,mm,4,Basis => "Monomial");
r6 = toM(s_{2,1},MM);
assert(ring r6 === MM);
lf6 = new HashTable from listForm r6;
assert(lf6#{2,1} == 1);
assert(lf6#{1,1,1} == 2);

-- Monomial-basis input: identity
r7 = toM MM_{2,1};
lf7 = new HashTable from listForm r7;
assert(lf7#{2,1} == 1);
assert(#(keys lf7) == 1);

-- Error if second argument is not a monomial-basis ring
caught := false;
try toM(s_{2,1},S) else caught = true;
assert(caught);

-- Works through a Symmetric ring by first going to Schur
R = symmetricRing(QQ,4);
r8 = toM(h_2);
lf8 = new HashTable from listForm r8;
assert(lf8#{2} == 1);
assert(lf8#{1,1} == 1);
///

------------------------------------
-- tests for Monomial-basis ring
------------------------------------
TEST ///
M = schurRing(QQ,m,4,Basis => "Monomial");
assert(M.Basis == "Monomial");

-- m_1 * m_1 = m_2 + 2 m_{1,1}
p1 = m_{1} * m_{1};
lf1 = new HashTable from listForm p1;
assert(lf1#{2} == 1);
assert(lf1#{1,1} == 2);

-- m_2 * m_1 = m_3 + m_{2,1}
p2 = m_{2} * m_{1};
lf2 = new HashTable from listForm p2;
assert(lf2#{3} == 1);
assert(lf2#{2,1} == 1);

-- m_{1,1} * m_1 = m_{2,1} + 3 m_{1,1,1}
p3 = m_{1,1} * m_{1};
lf3 = new HashTable from listForm p3;
assert(lf3#{2,1} == 1);
assert(lf3#{1,1,1} == 3);

-- Consistency with Schur-basis product: m_1 = s_1, so m_1*m_1 in M
-- should match the m-expansion of s_1*s_1 computed via toM into M
S = schurRing(QQ,s,4);
assert(toM(s_{1} * s_{1}, M) == m_{1} * m_{1});
///

--------------------
-- tests for Sp stable ring (GroupActing "Sp")
--------------------
TEST ///
Sp = schurRing(QQ,sp,GroupActing => "Sp");
S = schurRing(QQ,s);
assert(Sp.GroupActing == "Sp");
-- Koike branching: sp_1,1 = s_1,1 - 1; use two-arg toS to target S explicitly.
assert(toS(sp_{1,1}, S) == S_{1,1} - 1);
-- sp_2 = s_2 (Sym^2 is irreducible for Sp)
assert(toS(sp_{2}, S) == S_{2});
-- sp_{1,1,1} = s_{1,1,1} - s_1
assert(toS(sp_{1,1,1}, S) == S_{1,1,1} - S_{1});
-- sp_{2,1,1} = s_{2,1,1} - s_2 - s_{1,1} + 1.
-- Verify: at Sp(6), dim sp_{2,1,1} = 105 - 21 - 15 + 1 = 70, matches
-- Littlewood branching s_{2,1,1}|_Sp(6) = sp_{2,1,1} + sp_{2} + sp_{1,1}.
assert(toS(sp_{2,1,1}, S) == S_{2,1,1} - S_{2} - S_{1,1} + 1);
///

TEST ///
S = schurRing(QQ,s);
Sp = schurRing(QQ,sp,GroupActing => "Sp");
-- Inverse Koike: s_{1,1} = sp_{1,1} + 1
assert(toSp(s_{1,1}, Sp) == Sp_{1,1} + 1_Sp);
-- s_2 = sp_2
assert(toSp(s_2, Sp) == Sp_{2});
-- Round trip: toSp o toS is identity on sp-basis
for lam in {{3,2,1}, {4,2}, {2,1,1}, {5}, {1,1,1,1}} do
    assert(toSp(toS(Sp_lam, S), Sp) == Sp_lam);
-- Round trip: toS o toSp is identity on s-basis
for lam in {{3,2,1}, {4,2}, {2,1,1}, {5}, {1,1,1,1}} do
    assert(toS(toSp(S_lam, Sp), S) == S_lam);
///

TEST ///
-- Sp multiplication is the Newell-Littlewood product
Sp = schurRing(QQ,sp,GroupActing => "Sp");
-- sp_1 * sp_1 = sp_2 + sp_{1,1} + 1 (since V otimes V for Sp = Sym^2 + Lambda^2,
-- and Lambda^2 contains a 1-dim invariant form)
assert(sp_{1} * sp_{1} == sp_{2} + sp_{1,1} + 1_Sp);
-- sp_2 * sp_1 = sp_3 + sp_{2,1} + sp_1
assert(sp_{2} * sp_{1} == sp_{3} + sp_{2,1} + sp_{1});
-- sp_{1,1} * sp_1 = sp_{2,1} + sp_{1,1,1} + sp_1
assert(sp_{1,1} * sp_{1} == sp_{2,1} + sp_{1,1,1} + sp_{1});
-- Associativity: (sp_1 * sp_1) * sp_1 == sp_1 * (sp_1 * sp_1)
assert((sp_{1} * sp_{1}) * sp_{1} == sp_{1} * (sp_{1} * sp_{1}));
///

--------------------
-- tests for O stable ring (GroupActing "O")
--------------------
TEST ///
O = schurRing(QQ,o,GroupActing => "O");
S = schurRing(QQ,s);
assert(O.GroupActing == "O");
-- Koike branching: o_2 = s_2 - 1 (Sym^2 loses the invariant bilinear form)
assert(toS(o_{2}, S) == S_{2} - 1);
-- o_{1,1} = s_{1,1}  (Lambda^2 V is irreducible for O(n), n >= 3)
assert(toS(o_{1,1}, S) == S_{1,1});
-- o_3 = s_3 - s_1
assert(toS(o_{3}, S) == S_{3} - S_{1});
-- o_{2,1} = s_{2,1} - s_1
assert(toS(o_{2,1}, S) == S_{2,1} - S_{1});
///

TEST ///
S = schurRing(QQ,s);
O = schurRing(QQ,o,GroupActing => "O");
-- Inverse Koike: s_2 = o_2 + 1
assert(toO(s_{2}, O) == O_{2} + 1_O);
-- s_{1,1} = o_{1,1}
assert(toO(s_{1,1}, O) == O_{1,1});
-- Round trip: toO o toS is identity on o-basis
for lam in {{3,2,1}, {4,2}, {2,1,1}, {5}, {1,1,1,1}} do
    assert(toO(toS(O_lam, S), O) == O_lam);
-- Round trip: toS o toO is identity on s-basis
for lam in {{3,2,1}, {4,2}, {2,1,1}, {5}, {1,1,1,1}} do
    assert(toS(toO(S_lam, O), S) == S_lam);
///

TEST ///
-- O multiplication is the Newell-Littlewood product for O
O = schurRing(QQ,o,GroupActing => "O");
-- o_1 * o_1 = o_2 + o_{1,1} + 1
assert(o_{1} * o_{1} == o_{2} + o_{1,1} + 1_O);
-- o_2 * o_1 = o_3 + o_{2,1} + o_1
assert(o_{2} * o_{1} == o_{3} + o_{2,1} + o_{1});
-- Associativity
assert((o_{1} * o_{1}) * o_{1} == o_{1} * (o_{1} * o_{1}));
///

TEST ///
-- Cross-conversion between Sp and O: route via a shared Schur ring.
Sp = schurRing(QQ,sp,GroupActing => "Sp");
O = schurRing(QQ,o,GroupActing => "O");
S = schurRing(QQ,s);
assert(toSp(o_{2}, Sp) == toSp(toS(o_{2}, S), Sp));
assert(toO(sp_{2}, O) == toO(toS(sp_{2}, S), O));
-- Cached associated rings reused
assert(ring(toSp o_{2}) === ring(toSp o_{1,1}));
///

--------------------
-- tests for O odd/even (B_n vs D_n) distinction
--------------------
TEST ///
-- Direct construction of finite Odd (B_n) and Even (D_n) O rings.
Oodd  = schurRing(QQ, ob, 2, GroupActing => "O", OddOrEven => "Odd");
Oeven = schurRing(QQ, od, 2, GroupActing => "O", OddOrEven => "Even");
assert(Oodd.OddOrEven  == "Odd");
assert(Oeven.OddOrEven == "Even");

-- O(5) = B_2 Weyl dimensions
assert(dim ob_{1}     == 5);   -- defining rep
assert(dim ob_{1,1}   == 10);  -- Lambda^2 V, adjoint of SO(5)
assert(dim ob_{2}     == 14);  -- Sym^2 V minus trace
assert(dim ob_{2,1}   == 35);

-- O(4) = D_2 Weyl dimensions (SO(4) = SU(2) x SU(2) / Z_2)
assert(dim od_{1}     == 4);   -- defining rep
assert(dim od_{2}     == 9);   -- Sym^2 V minus trace (dim 10-1)
-- Lambda^2 V splits under SO(4) into 3+3; partition (1,1) picks one component.
assert(dim od_{1,1}   == 3);
-- SO(4) = SU(2) x SU(2) / Z_2.  Partition (a,b) with a>=b>=0 corresponds to
-- (j_1,j_2) = ((a+b)/2,(a-b)/2), dim (2j_1+1)(2j_2+1); (2,1) -> (3/2,1/2).
assert(dim od_{2,1}   == 8);

-- Default "O" ring (no OddOrEven) falls back to Odd (B_n): backward compat.
Odefault = schurRing(QQ, odef, 2, GroupActing => "O");
assert(Odefault.OddOrEven == "Odd");
assert(dim odef_{1} == 5);
///

TEST ///
-- specialize with OddOrEven option.
Ost = schurRing(QQ, o2, GroupActing => "O");
-- Default: Odd (B_n).
g = specialize(o2_{1}, 2);
assert((ring g).OddOrEven == "Odd");
assert(dim g == 5);
-- Explicit OddOrEven => "Even" selects D_n.
g2 = specialize(o2_{1}, 2, OddOrEven => "Even");
assert((ring g2).OddOrEven == "Even");
assert(dim g2 == 4);
-- Odd and Even at the same n get distinct cached rings.
assert(ring g =!= ring g2);
///

TEST ///
-- OddOrEven is rejected for non-O rings.
ok := true;
try (schurRing(QQ, xx, GroupActing => "Sp", OddOrEven => "Odd"); ok = false) else null;
assert ok;
-- Invalid value rejected.
ok = true;
try (schurRing(QQ, yy, 3, GroupActing => "O", OddOrEven => "Bogus"); ok = false) else null;
assert ok;
///

--------------------
-- tests for SL(n) Schur ring
--------------------
TEST ///
-- Basic SL(n) canonicalization: partitions with n nonzero rows collapse
-- by subtracting the last row from every part.
SL3 = schurRing(QQ, sl, 3, GroupActing => "SL");
-- sl_{1,1,1} = det = trivial rep in SL(3)
assert(sl_{1,1,1} == 1_SL3);
-- sl_{2,1,1} = sl_{1} after subtracting 1 from each row
assert(sl_{2,1,1} == sl_{1});
-- sl_{3,3,3} = trivial
assert(sl_{3,3,3} == 1_SL3);
-- sl_{4,2,1} = sl_{3,1}
assert(sl_{4,2,1} == sl_{3,1});
-- Partitions with fewer than n parts are unchanged
assert(sl_{2,1} == sl_{2,1});
assert(sl_{3} == sl_{3});
///

TEST ///
-- SL multiplication agrees with GL mult followed by det-collapse.
-- In SL(3), sl_{1} * sl_{1,1} = s_{2,1} + s_{1,1,1} = s_{2,1} + 1.
SL3 = schurRing(QQ, sl, 3, GroupActing => "SL");
assert(sl_{1} * sl_{1,1} == sl_{2,1} + 1_SL3);
-- sl_{1} * sl_{1} = sl_{2} + sl_{1,1}
assert(sl_{1} * sl_{1} == sl_{2} + sl_{1,1});
-- (sl_{1})^3 = sl_{3} + 2 sl_{2,1} + sl_{1,1,1}
--           = sl_{3} + 2 sl_{2,1} + 1     (in SL(3))
assert(sl_{1}^3 == sl_{3} + 2*sl_{2,1} + 1_SL3);
-- Associativity
assert((sl_{2} * sl_{1}) * sl_{1} == sl_{2} * (sl_{1} * sl_{1}));
///

TEST ///
-- SL(2): every SL(2) rep is indexed by a single integer (highest weight).
SL2 = schurRing(QQ, sl2, 2, GroupActing => "SL");
-- sl2_{1,1} = det = trivial
assert(sl2_{1,1} == 1_SL2);
-- sl2_{2,1} = sl2_{1}
assert(sl2_{2,1} == sl2_{1});
-- Clebsch-Gordan for SU(2): V_a * V_b = sum_{k} V_{a+b-2k}, k=0..min(a,b).
-- In partition language: sl2_{a} * sl2_{b} = sum sl2_{a+b-2k}.
-- sl2_{2} * sl2_{2} = sl2_{4} + sl2_{2} + 1  (spins 2,1,0)
assert(sl2_{2} * sl2_{2} == sl2_{4} + sl2_{2} + 1_SL2);
-- sl2_{3} * sl2_{1} = sl2_{4} + sl2_{2}
assert(sl2_{3} * sl2_{1} == sl2_{4} + sl2_{2});
///

TEST ///
-- Stable SL (numgens = infinity) coincides with stable GL.
SLstable = schurRing(QQ, slinf, GroupActing => "SL");
-- No det-collapse happens; partitions are preserved.
assert(slinf_{1,1,1,1} != 1_SLstable);
-- Multiplication matches plain GL.
assert(slinf_{1} * slinf_{1} == slinf_{2} + slinf_{1,1});
///

TEST ///
-- toS on an SL element lifts to the associated plain GL ring.
SL3 = schurRing(QQ, sl, 3, GroupActing => "SL");
f = sl_{2,1};
g = toS f;
T = ring g;
assert(class T === SchurRing);
assert(T.GroupActing == "GL");
assert(numgens T == 3);
assert(g == T_{2,1});
-- sl_{1,1,1} = 1 lifts to 1 in GL, not to s_{1,1,1}.
assert(toS (sl_{1,1,1}) == 1_T);
///

--------------------
-- tests for SSW modification rules
--------------------
TEST ///
-- Paper's worked example ([SSW] Example 3.20):
-- lambda = (6,5,4,4,3,3,2), n=2, gives tau=(6,5), i=8, sign=+1.
assert(modificationRule({6,5,4,4,3,3,2}, 2, "C") == ({6,5}, 1));

-- Admissible partitions pass through unchanged with sign +1.
assert(modificationRule({3,2,1}, 3, "C") == ({3,2,1}, 1));
assert(modificationRule({2,1}, 3, "C") == ({2,1}, 1));
assert(modificationRule({}, 2, "C") == ({}, 1));

-- sp_{1,1} in Sp(2): SSW-C returns null (strip size = 0), so the
-- formal character is zero in Sp(2).  Corresponds to Koike: s_{1,1} =
-- sp_{1,1} + 1, and in Sp(2), s_{1,1} = Lambda^2 V = det = trivial, so
-- sp_{1,1} must be 0.
assert(modificationRule({1,1}, 1, "C") === null);

-- sp_{1,1,1} in Sp(2): SSW-C returns ((1), -1).  Corresponds to
-- s_{1,1,1} = Lambda^3 V = 0 for dim V = 2 via Koike
-- s_{1,1,1} = sp_{1,1,1} + sp_{1} = -sp_{1} + sp_{1} = 0.
assert(modificationRule({1,1,1}, 1, "C") == ({1}, -1));

-- Longer test: lambda = (2,2,1), n=1.  r=3, L=2. k with lam_k+3-k=2:
-- k=1: 2, no.  k=2: 3, no.  k=3: 3, no.  No k works -> null.
assert(modificationRule({2,2,1}, 1, "C") === null);

-- Testing type D (m = 2n, even-dim orthogonal O(2n)).
-- lambda = (1,1) in O(2): admissible (lam^T_1 + lam^T_2 = 2+0 = 2 = m).
-- so we should get ((1,1), 1).
assert(modificationRule({1,1}, 1, "D") == ({1,1}, 1));

-- Testing type B (m = 2n+1, odd-dim orthogonal O(2n+1)).
-- lambda = (1) in O(3): admissible (lam^T = (1); 1+0 = 1 <= 3).
assert(modificationRule({1}, 1, "B") == ({1}, 1));
///

-- Regression: Weyl dimensions of Sp(2n) and O(2n+1), O(2n) via dim
TEST ///
-- Sp(4) = C_2 Weyl dims
Sp4 = schurRing(QQ, getSymbol "sp4", 2, GroupActing => "Sp");
assert(dim Sp4_{1} == 4);
assert(dim Sp4_{2} == 10);
assert(dim Sp4_{1,1} == 5);
assert(dim Sp4_{2,1} == 16);
assert(dim Sp4_{2,2} == 14);
-- Sp(4) multiplication
assert(Sp4_{1,1}*Sp4_{1,1} == Sp4_{2,2} + Sp4_{2} + 1_Sp4);
-- dim of a product equals product of dims
assert(dim(Sp4_{1}*Sp4_{1}) == 16);

-- Sp(6) = C_3 Weyl dims
Sp6 = schurRing(QQ, getSymbol "sp6", 3, GroupActing => "Sp");
assert(dim Sp6_{1} == 6);
assert(dim Sp6_{1,1} == 14);
assert(dim Sp6_{2} == 21);
assert(dim Sp6_{1,1,1} == 14);

-- O(5) = B_2 Weyl dims (numgens 2, Odd)
O5 = schurRing(QQ, getSymbol "o5", 2, GroupActing => "O", OddOrEven => "Odd");
assert(dim O5_{1} == 5);
assert(dim O5_{2} == 14);
assert(dim O5_{1,1} == 10);
assert(dim O5_{2,1} == 35);
assert(O5_{1}*O5_{1} == O5_{2} + O5_{1,1} + 1_O5);
assert(dim(O5_{1}*O5_{1}) == 25);

-- O(6) = D_3 Weyl dims (numgens 3, Even)
O6 = schurRing(QQ, getSymbol "o6", 3, GroupActing => "O", OddOrEven => "Even");
assert(dim O6_{1} == 6);
assert(dim O6_{1,1} == 15);
assert(dim O6_{2} == 20);
///

-- Regression: plethysm in Sp and O rings routes via GL Schur
TEST ///
-- Stable GL helper
Sp = schurRing(QQ, getSymbol "sp", GroupActing => "Sp");
O  = schurRing(QQ, getSymbol "oo", GroupActing => "O");

-- plethysm of identity (s_1) is identity
assert(plethysm({1}, Sp_{2})   == Sp_{2});
assert(plethysm({1}, O_{1,1}) == O_{1,1});

-- Sym^2 and Lambda^2 of the defining rep for Sp and O (stable)
assert(plethysm({2},   Sp_{1}) == Sp_{2});                 -- adjoint of Sp
assert(plethysm({1,1}, Sp_{1}) == Sp_{1,1} + 1_Sp);
assert(plethysm({2},   O_{1})  == O_{2} + 1_O);
assert(plethysm({1,1}, O_{1})  == O_{1,1});                 -- adjoint of O

-- Finite-rank plethysm works and matches detour via Schur
Sp4 = schurRing(QQ, getSymbol "sp4", 2, GroupActing => "Sp");
O5  = schurRing(QQ, getSymbol "o5",  2, GroupActing => "O", OddOrEven => "Odd");
assert(plethysm({2}, Sp4_{1}) == Sp4_{2});
assert(plethysm({2}, O5_{1})  == O5_{2} + 1_O5);
///

--------------------------------------------------------------------
-- Branching formulas (King, J. Phys. A 8 (1975), 429-449)
--------------------------------------------------------------------
TEST ///
-- GL branching (stable): Delta(s_lambda) = sum c^lambda_{mu,nu} s_mu (x) s_nu
SGL = schurRing(QQ, getSymbol "sGL", infinity, GroupActing => "GL");
A   = schurRing(QQ, getSymbol "aGL", infinity, GroupActing => "GL");
B   = schurRing(QQ, getSymbol "bGL", infinity, GroupActing => "GL");
h = branch(SGL_{2}, A, B);
assert(h#({2}, {})   == 1);
assert(h#({1}, {1}) == 1);
assert(h#({}, {2})  == 1);
h = branch(SGL_{1,1}, A, B);
assert(h#({1,1}, {})  == 1);
assert(h#({1}, {1})   == 1);
assert(h#({}, {1,1})  == 1);

-- Finite-rank GL(6) -> GL(3) x GL(3): dimensions must match
SGL6 = schurRing(QQ, getSymbol "sGL6", 6, GroupActing => "GL");
A3   = schurRing(QQ, getSymbol "aGL3", 3, GroupActing => "GL");
B3   = schurRing(QQ, getSymbol "bGL3", 3, GroupActing => "GL");
h = branch(SGL6_{2}, A3, B3);
d := sum for k in keys h list lift(h#k, ZZ) * dim(A3_(k#0)) * dim(B3_(k#1));
assert(d == 21);    -- dim Sym^2(C^6) = 21
h = branch(SGL6_{1,1}, A3, B3);
d = sum for k in keys h list lift(h#k, ZZ) * dim(A3_(k#0)) * dim(B3_(k#1));
assert(d == 15);    -- dim Lambda^2(C^6) = 15
///

TEST ///
-- Sp branching (stable): extra terms from delta with parts in pairs.
Sp  = schurRing(QQ, getSymbol "spS", infinity, GroupActing => "Sp");
Asp = schurRing(QQ, getSymbol "asp", infinity, GroupActing => "Sp");
Bsp = schurRing(QQ, getSymbol "bsp", infinity, GroupActing => "Sp");
h = branch(Sp_{1,1}, Asp, Bsp);
-- (mu, nu) from delta = {} (standard LR) plus (,) from delta = {1,1} (cols-even)
assert(h#({}, {})     == 1);
assert(h#({1,1}, {})  == 1);
assert(h#({}, {1,1})  == 1);
assert(h#({1}, {1})   == 1);

-- Finite Sp(4) -> Sp(2) x Sp(2): dimensions match after modification.
RSp4 = schurRing(QQ, getSymbol "sprSp4", 2, GroupActing => "Sp");
R1   = schurRing(QQ, getSymbol "sprA",   1, GroupActing => "Sp");
R2   = schurRing(QQ, getSymbol "sprB",   1, GroupActing => "Sp");
h = branch(RSp4_{1,1}, R1, R2);
d := sum for k in keys h list lift(h#k, ZZ) * dim(R1_(k#0)) * dim(R2_(k#1));
assert(d == 5);
h = branch(RSp4_{2}, R1, R2);
d  = sum for k in keys h list lift(h#k, ZZ) * dim(R1_(k#0)) * dim(R2_(k#1));
assert(d == 10);
h = branch(RSp4_{2,1}, R1, R2);
d  = sum for k in keys h list lift(h#k, ZZ) * dim(R1_(k#0)) * dim(R2_(k#1));
assert(d == 16);
///

TEST ///
-- O branching (stable): extra terms from delta with all parts even.
OS = schurRing(QQ, getSymbol "oS", infinity, GroupActing => "O");
OA = schurRing(QQ, getSymbol "oA", infinity, GroupActing => "O");
OB = schurRing(QQ, getSymbol "oB", infinity, GroupActing => "O");
h = branch(OS_{2}, OA, OB);
assert(h#({}, {})     == 1);  -- from delta = {2}
assert(h#({2}, {})    == 1);
assert(h#({}, {2})    == 1);
assert(h#({1}, {1})   == 1);

-- Finite O(7) -> O(3) x O(4):
RO7 = schurRing(QQ, getSymbol "or7", 3, GroupActing => "O", OddOrEven => "Odd");
RO3 = schurRing(QQ, getSymbol "or3", 1, GroupActing => "O", OddOrEven => "Odd");
RO4 = schurRing(QQ, getSymbol "or4", 2, GroupActing => "O", OddOrEven => "Even");
h = branch(RO7_{1}, RO3, RO4);
d := sum for k in keys h list lift(h#k, ZZ) * dim(RO3_(k#0)) * dim(RO4_(k#1));
assert(d == 7);
h = branch(RO7_{2}, RO3, RO4);
d  = sum for k in keys h list lift(h#k, ZZ) * dim(RO3_(k#0)) * dim(RO4_(k#1));
assert(d == 27);  -- dim(Sym^2 C^7 - trace)

-- ZZ shortcut form:  branch(f, m, n) builds factor rings automatically.
h2 = branch(RO7_{1}, 1, 2);
assert(#keys h2 >= 1);
///

--------------------------------------------------------------------
-- specialize on GL/SL rings and on multi-level towers
--------------------------------------------------------------------
TEST ///
-- Stable GL -> finite GL(n) truncates partitions of length > n.
GLst = schurRing(QQ, getSymbol "glstT", infinity, GroupActing => "GL");
f1 = specialize(GLst_{2,1}, 2);
assert(f1 != 0);
assert((ring f1).GroupActing == "GL");
assert(numgens ring f1 == 2);
-- partition with too many rows vanishes
assert(specialize(GLst_{1,1,1}, 2) == 0);

-- SL specialize collapses determinant row.
SLst = schurRing(QQ, getSymbol "slstT", infinity, GroupActing => "SL");
f2 = specialize(SLst_{2,1}, 2);
assert((ring f2).GroupActing == "SL");
-- In SL(2), s_{2,1} == det * s_{1} -> sl_{1}.
assert(f2 == (ring f2)_{1});
///

TEST ///
-- Multi-level towers: GL over GL.
A = schurRing(QQ, getSymbol "aTowr", 3, GroupActing => "GL");
B = schurRing(A, getSymbol "bTowr", 3, GroupActing => "GL");
-- Multiplication works (previously tested) and specialize works on topmost.
g = specialize(B_{2,1}, 2);
assert(g != 0);
assert((ring g).GroupActing == "GL");
assert(numgens ring g == 2);

-- Multi-level list specialize: outer rank then inner rank.
r = specialize(B_{2,1}, {2, 2});
assert(schurLevel ring r == 2);
assert(numgens ring r == 2);
assert(numgens coefficientRing ring r == 2);
-- infinity placeholder leaves a layer alone.
r2 = specialize(B_{2,1}, {2, infinity});
assert(numgens ring r2 == 2);
assert(numgens coefficientRing ring r2 == 3);
///

TEST ///
-- Stable Sp and O towers over a GL coefficient ring: multiplication.
AG = schurRing(QQ, getSymbol "agTT", 3, GroupActing => "GL");
SpG = schurRing(AG, getSymbol "spgTT", 2, GroupActing => "Sp");
-- Sp(4) over AG: V tensor V = sp_2 + sp_{1,1} + 1.
assert(SpG_{1} * SpG_{1} == SpG_{2} + SpG_{1,1} + 1_SpG);

OG = schurRing(AG, getSymbol "ogTT", 2, GroupActing => "O", OddOrEven => "Odd");
-- V tensor V in O(5) = B_2: o_2 + o_{1,1} + 1.
assert(OG_{1} * OG_{1} == OG_{2} + OG_{1,1} + 1_OG);

-- Specialize Sp-over-GL stays a level-2 ring with same GL coefficient ring.
SpSt = schurRing(AG, getSymbol "spstTT", infinity, GroupActing => "Sp");
f = (AG_{1}) * SpSt_{2,1};
sf = specialize(f, 2);
assert(schurLevel ring sf == 2);
assert(coefficientRing ring sf === AG);
-- Coefficient preserved through specialize.
assert(not zero sf);
///

TEST ///
-- RatGL stable ring: construction, bipartition indexing, Koike product.
S = schurRing(QQ, getSymbol "sRat", infinity, GroupActing => "RatGL");
-- Bipartition indexing uses the IVT symbol: sRat_{{alpha},{beta}}.
assert(sRat_{{1},{}} != 0);
assert(sRat_{1} == sRat_{{1},{}});       -- flat partition syntax lifts with empty second weight
assert(sRat_{2,1} == sRat_{{2,1},{}});
assert(sRat_{{},{}} == 1_S);             -- trivial character

-- Koike product: chi_{(1),()} * chi_{(),(1)} = chi_{(1),(1)} + chi_{(),()}
v    = sRat_{{1},{}};
vdul = sRat_{{},{1}};
assert(v * vdul == sRat_{{1},{1}} + sRat_{{},{}});

-- chi_{(1),(1)}^2 = sum over 5 bipartitions + trivial + 2*adjoint.
prod = (sRat_{{1},{1}})^2;
expectedProd = (sRat_{{2},{2}} + sRat_{{2},{1,1}} + sRat_{{1,1},{2}}
     + sRat_{{1,1},{1,1}} + 2*sRat_{{1},{1}} + sRat_{{},{}});
assert(prod == expectedProd);

-- Koike-Terada boundary case: when ell(alpha) + ell(beta) = n+1 the
-- border-strip length L = ell(alpha)+ell(beta)-n-1 is zero, so the
-- character specializes to 0.
assert(specialize(sRat_{{1,1},{1,1}}, 3) == 0);
assert(specialize(sRat_{{2},{1}},     1) == 0);
assert(specialize(sRat_{{1},{1}},     1) == 0);

-- Non-trivial modification: at GL(3), alpha=(4,3,2,2), beta=(5,2,2,1,1)
-- has ell+ell=9 > n+1=4, and the Koike-Terada rule reduces it by two
-- border-strip passes to the admissible pair ((4,1),(5)) with sign -1.
Tfin3 = schurRing(QQ, getSymbol "tFin3", 3, GroupActing => "RatGL");
assert(toRatGL(sRat_{{4,3,2,2},{5,2,2,1,1}}, Tfin3) == -tFin3_{{4,1},{5}});
-- Another non-trivial pass: at GL(4) the column bipartition (1^3,1^3)
-- drops to (1^2,1^2) with sign -1 (one border-strip step of length 1).
Tfin4 = schurRing(QQ, getSymbol "tFin4", 4, GroupActing => "RatGL");
assert(toRatGL(sRat_{{1,1,1},{1,1,1}}, Tfin4) == -tFin4_{{1,1},{1,1}});

-- Ring-homomorphism property of specialize.
a = sRat_{{2,1},{1}};
b = sRat_{{1},{2}};
assert(specialize(a*b, 3) == specialize(a,3) * specialize(b,3));

-- toRatGL: lift a plain GL character by adjoining trivial second weight.
G = schurRing(QQ, getSymbol "gRG", infinity, GroupActing => "GL");
R = schurRing(QQ, getSymbol "tRat", infinity, GroupActing => "RatGL");
assert(toRatGL(gRG_{2,1} + 3*gRG_{1}, R) == tRat_{{2,1},{}} + 3*tRat_{{1},{}});
///

TEST ///
-- RatGL finite ranks: dimension formula and modification on products.
T = schurRing(QQ, getSymbol "uRat", 3, GroupActing => "RatGL");
-- Fundamental dims at GL(3).
assert(dim(1_T)             == 1);
assert(dim(uRat_{{1},{}})   == 3);  -- standard V
assert(dim(uRat_{{},{1}})   == 3);  -- dual V*
assert(dim(uRat_{{1},{1}})  == 8);  -- adjoint (trace-free part of V tensor V*)
assert(dim(uRat_{{2},{}})   == 6);  -- Sym^2 V
assert(dim(uRat_{{1,1},{}}) == 3);  -- Alt^2 V

-- Adjoint squared at GL(3): dimensions total 64 = 8^2.  chi_{(1,1),(1,1)}
-- hits the Koike-Terada boundary (ell sum 4 = n+1 = 4, so L = 0) and
-- vanishes in the expansion.
adj = uRat_{{1},{1}};
adj2 = adj^2;
assert(dim adj2 == 64);
expectedAdj2 = (uRat_{{2},{2}} + uRat_{{2},{1,1}} + uRat_{{1,1},{2}}
     + 2*uRat_{{1},{1}} + uRat_{{},{}});
assert(adj2 == expectedAdj2);

-- Alt^2 V tensor Alt^2 V* at GL(3) isomorphic to V tensor V* (dim 9).
-- Stable decomposition has chi_{(1,1),(1,1)} + chi_{(1),(1)} + chi_{(),()};
-- the first term is dropped at GL(3).
prodAltAlt = uRat_{{1,1},{}} * uRat_{{},{1,1}};
assert(dim prodAltAlt == 9);
assert(prodAltAlt == uRat_{{1},{1}} + uRat_{{},{}});
///


-------------------------------------------------------------------------
-- Tier B API (toSymm exported, toGL, toSn, convert) + Tier A guards  --
-------------------------------------------------------------------------

TEST ///
-- toSymm round-trip: s_{2,1} -> Jacobi-Trudi -> back to s_{2,1}
S = schurRing(QQ, getSymbol "s", 4);
f = s_{2,1};
g = toSymm f;
assert(ring g =!= S);           -- lands in the associated symmetric ring
assert(toS g == f);              -- round-trips
-- scalar passes through
assert(toSymm(3_S) == toSymm(3));
-- toSymm on a plain number is the identity
assert(toSymm(5) == 5);
///

TEST ///
-- toGL synonym and target-ring promotion.
R = symmetricRing(QQ, 4);
-- h_3 -> Schur is s_3.  (R_{0} ... R_{11} lays out e/p/h; avoid depending
-- on the exact index order by using the named generators.)
debug SchurRings
use R;
assert(toGL(h_3) == toS(h_3));

T = schurRing(QQ, getSymbol "t", 3);
assert(toGL(h_3, T) == t_{3});
assert(ring toGL(h_3, T) === T);

-- toGL rejects a non-GL target.
Sn = schurRing(QQ, getSymbol "nT", 4, GroupActing => "Sn");
assert(try (toGL(h_3, Sn); false) else true);
///

TEST ///
-- toSn: re-label into an Sn ring; respect finite-rank drop.
S  = schurRing(QQ, getSymbol "s", 4);
Sn = schurRing(QQ, getSymbol "n", 4, GroupActing => "Sn");

x = s_{2,1} + 3 * s_{1,1,1};
y = toSn(x, Sn);
assert(ring y === Sn);
assert(y == n_{2,1} + 3 * n_{1,1,1});

-- internalProduct via the re-labeled element: s_{3} corresponds to the
-- trivial S_3-irrep (dim 1) whose internal product with itself is
-- itself.  At schurRing level this exercises the Sn multiplication path.
t = toSn(s_{3}, Sn);
assert(t * t == n_{3});

-- From a symmetric ring: toSn(h_2) should equal toS(h_2) relabeled.
R = symmetricRing(QQ, 4);
use R;
assert(toSn(h_2, Sn) == n_{2});

-- Rejects wrong target.
assert(try (toSn(s_{2,1}, S); false) else true);

-- Finite-rank drop: a partition with more parts than numgens Sn is dropped.
Sn2 = schurRing(QQ, getSymbol "nT2", 2, GroupActing => "Sn");
assert(toSn(s_{1,1,1}, Sn2) == 0_Sn2);
assert(toSn(s_{2} + s_{1,1,1}, Sn2) == nT2_{2});
///

TEST ///
-- convert dispatcher: route by target's GroupActing / ring kind.
SGL = schurRing(QQ, getSymbol "sC", 4);
SSn = schurRing(QQ, getSymbol "nC", 4, GroupActing => "Sn");
SSp = schurRing(QQ, getSymbol "pC", 2, GroupActing => "Sp");
SOrt = schurRing(QQ, getSymbol "oC", 4, GroupActing => "O");
SRat = schurRing(QQ, getSymbol "rC", infinity, GroupActing => "RatGL");

x = sC_{2,1};
assert(convert(x, SSp) == toSp(x, SSp));
assert(convert(x, SOrt) == toO (x, SOrt));
assert(convert(x, SSn) == toSn(x, SSn));
assert(convert(x, SRat) == toRatGL(x, SRat));
assert(convert(x, SGL)  == x);                  -- identity

-- symmetric-ring target
R = symmetricRing(QQ, 4);
cR = convert(x, R);
assert(ring cR === R);
-- Value check: jacobiTrudi({2,1}) in R is the symmetric function of s_{2,1}.
assert(cR == jacobiTrudi({2,1}, R));
///

TEST ///
-- Tier A#2: stable (rank-infinite) SchurRings reject toE/toH/toP with
-- a clear, actionable error.
SinfGL = schurRing(QQ, getSymbol "sInf", infinity);
assert(try (toE(sInf_{2,1}); false) else true);
assert(try (toH(sInf_{2,1}); false) else true);
assert(try (toP(sInf_{2,1}); false) else true);

-- Same for a stable Sn ring.
SinfSn = schurRing(QQ, getSymbol "nInf", infinity, GroupActing => "Sn");
assert(try (toE(nInf_{2,1}); false) else true);

-- Finite-rank Schur ring still works.
Sfin = schurRing(QQ, getSymbol "sFin", 4);
use Sfin;
assert(toE(sFin_{2,1}) != 0);
assert(toH(sFin_{2,1}) != 0);
assert(toP(sFin_{2,1}) != 0);
///

TEST ///
-- Tier A#3 documentation: verify Sp/O -> toE/toH/toP behavior (treated as
-- plain Schur labels).  sp_{2,1} labeled as a Schur partition goes to
-- Jacobi-Trudi of {2,1} = h_1 h_2 - h_3.  (CHARACTER semantics instead
-- would require going through toS first, which lands in a different
-- GL Schur ring whose symmetricRing is distinct -- we document that
-- elsewhere; here we only verify the "label" behavior.)
Sp3 = schurRing(QQ, getSymbol "spA", 3, GroupActing => "Sp");
use Sp3;
hLabel = toH(spA_{2,1});
RH = ring hLabel;
assert(hLabel == RH.hVariable(1) * RH.hVariable(2) - RH.hVariable(3));

-- And toS of sp_{2,1} at Sp(6) is the known Koike inverse s_{2,1} - s_1.
tsElt = toS(spA_{2,1});
glRing = ring tsElt;
assert(tsElt == glRing_{2,1} - glRing_{1});
///

TEST ///
-- Tier C#9: skewSchurExpansion returns the same result on the second
-- call (memoization), and the values are correct.
debug SchurRings;
a = skewSchurExpansion({3,2,1}, {2,1});
b = skewSchurExpansion({3,2,1}, {2,1});
assert(a == b);
-- Known LR content: s_{3,2,1 / 2,1} = s_3 + 2 s_{2,1} + s_{1,1,1}.
-- (The two LR tableaux for s_{2,1} come from the two valid column-strict
-- fillings of the disconnected skew shape with content (2,1).)
-- Sort the partition list so the assertion is order-independent.
expected = sort {({1,1,1}, 1), ({2,1}, 2), ({3}, 1)};
assert(sort a == expected);

-- Trivial corners.
assert(skewSchurExpansion({}, {})     == {({}, 1)});
assert(skewSchurExpansion({3}, {3})   == {({}, 1)});
assert(skewSchurExpansion({3}, {4})   == {});
assert(skewSchurExpansion({2,1}, {2,2}) == {});  -- mu not contained in lam

-- Another known case: s_{2,1 / 1} = s_2 + s_{1,1}.
assert(sort skewSchurExpansion({2,1}, {1}) == sort {({2}, 1), ({1,1}, 1)});
///

TEST ///
-- Tier C#10: the shared cached workSymRing used by plethysm,
-- skewSchurExpansion, and the Kostka routines must NOT rebind the
-- user's global e/h/p/s symbols when it is created or grown.  Before
-- this guard was added, calling plethysm would leak the cache ring into
-- the top-level environment and subsequent expressions like `e_4` or
-- `h_3` would evaluate into the wrong ring, breaking == comparisons.
R = symmetricRing(QQ, 12);
use R;
beforeRing = ring (e_4);
assert(beforeRing === R);
-- Force creation of the cached workSymRing (skewSchurExpansion uses it,
-- as do plethysm(BasicList,RingElement) and kostkaNumber).
lambda = new Partition from {3};
p1 = plethysm(lambda, e_4);
-- After plethysm, user's e/h/p must still resolve inside R.
assert(ring (e_4) === R);
assert(ring (h_3) === R);
assert(ring (p_2) === R);
-- And the plethysm result must live in the user's ring, not the cache.
assert(ring p1 === R);
-- Sanity: equality with the `@` form (h_3 @ e_4) still holds -- both
-- sides must live in R.
p2 = h_3 @ e_4;
assert(ring p2 === R);
assert(p1 == p2);
///

TEST ///
-- kostkaNumber: values against known references (hook-length formula,
-- trivial content, dominance-order zeros), and cross-check against
-- the independent h_mu -> Schur polynomial expansion.
assert(kostkaNumber({3,1},   {1,1,1,1})   == 3);
assert(kostkaNumber({2,2},   {1,1,1,1})   == 2);
assert(kostkaNumber({5},     {2,1,1,1})   == 1);
assert(kostkaNumber({3,2,1}, {3,2,1})     == 1);
assert(kostkaNumber({2,2,1}, {3,1,1})     == 0);  -- mu not dominated
assert(kostkaNumber({},      {})          == 1);
assert(kostkaNumber({3,2},   {2,2,1})     == 2);

-- Cross-check against h_mu -> Schur expansion on every pair in degree 6.
R = symmetricRing(QQ, 6);
use R;
for mu in partitions 6 do (
     muL := toList mu;
     hProd := product for i from 0 to #muL - 1 list (R.hVariable(muL#i));
     refMap := new MutableHashTable;
     for t in listForm toS hProd do refMap#(t#0) = lift(t#1, ZZ);
     for la in partitions 6 do (
	  lamL := toList la;
	  vRef := if refMap#?lamL then refMap#lamL else 0;
	  assert(kostkaNumber(lamL, muL) == vRef);
	  );
     );
///

TEST ///
-- Sn specialize: stable Sn -> finite Sn via truncation by #parts.
-- Also verify the finite -> finite form.
Sn = schurRing(QQ, getSymbol "snA", infinity, GroupActing => "Sn");
use Sn;
f = snA_{3,2} + snA_{4,1,1};  -- two partitions: 2 parts and 3 parts
g = specialize(f, 2);
-- Target ring snfin2 has at most 2 parts, so the 3-part term drops.
assert(ring g =!= Sn);
assert((ring g).GroupActing == "Sn");
assert(numgens ring g == 2);

-- Finite Sn -> smaller finite Sn (the (3,2) term survives; drops nothing yet).
Sn5 = schurRing(QQ, getSymbol "snB", 5, GroupActing => "Sn");
use Sn5;
h = snB_{3,2} + snB_{1,1,1,1,1};  -- both fit in 5 parts
g2 = specialize(h, 2);
assert(numgens ring g2 == 2);
-- Only the 2-part partition survives (the 5-parts one drops).
assert(size g2 == 1);
///

TEST ///
-- toRatGL from a symmetric ring: should route through toS.
-- And toSymm on a RatGL "polynomial" element (no negative components)
-- should recover the symmetric-ring representation.
R = symmetricRing(QQ, 4);
use R;
Rt = schurRing(QQ, getSymbol "rtR", infinity, GroupActing => "RatGL");
x = toRatGL(h_2, Rt);
assert(ring x === Rt);
-- h_2 = s_2, so x should equal rt_{{2},{}}.
assert(x == rtR_{{2},{}});

-- toSymm round-trip on a polynomial RatGL element: rt_{{2,1},{}}
-- should map back to the symmetric function for s_{2,1}.
use Rt;
y = rtR_{{2,1},{}};
ys = toSymm y;
-- Must live in some symmetricRing, and toS(ys) should recover s_{2,1}.
assert((ring ys).?EHPVariables);  -- it's a symmetric ring
-- Going through the associated SchurRing, confirm the partition.
zs = toS ys;
assert((listForm zs)#0#0 == {2,1});
assert((listForm zs)#0#1 == 1);

-- toSymm on a non-polynomial (beta nonempty) element must error cleanly.
z = rtR_{{2,1},{1}};
assert(try (toSymm z; false) else true);
///


end
