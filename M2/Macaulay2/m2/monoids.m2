--		Copyright 1993-2002 by Daniel R. Grayson

needs "engine.m2"
needs "expressions.m2"
needs "indeterminates.m2"
needs "methods.m2"
needs "remember.m2"
needs "shared.m2" -- for tensor
needs "variables.m2"

-- TODO:
-- 1. implement a free monoid M whose degrees have torsion [done]
-- 2. implement the degrees ring of M as a quotient ring (optional?)
-- 3. implement the degrees monoid of M as a monoid with relations (optional?)

-- see "degreesRing 0" in last.m2
madeTrivialMonoid := false

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

dotprod = (c,d) -> sum( min(#c, #d), i -> c#i * d#i )

makeSparse := v -> select(pairs v, (k, v) -> v != 0)

listSplice := L -> deepSplice flatten sequence L

baseName' = var -> baseName if instance(var, String) and match("[[:alnum:]$]+", var) then getSymbol var else var

-- TODO: where should this go?
-- MES: I grabbed this from polyrings.m2, to handle skew variables in the monoid/ring.
indices := (M, variables) -> apply(variables, x -> (
	x = baseName x;
	if M.index#?x then M.index#x
	else error "expected a variable of the ring"))

sameMonoid := (x, y) -> if (M := class x) === class y then M else error "expected elements in the same monoid"

-----------------------------------------------------------------------------
-- MonoidElement type declarations and basic methods
-----------------------------------------------------------------------------

MonoidElement = new Type of HashTable
MonoidElement.synonym = "monoid element"

-- TODO: why is this a hash table? compare with RingElement
new MonoidElement from RawMonomial := (M, f) -> hashTable{ symbol RawMonomial => f }
raw MonoidElement := x -> x.RawMonomial

MonoidElement == MonoidElement := (x, y) -> x === y
MonoidElement  ? MonoidElement := (x, y) -> rawCompareMonomial(raw sameMonoid(x, y), raw x, raw y);
MonoidElement  * MonoidElement := (x, y) -> new sameMonoid(x, y) from x.RawMonomial * y.RawMonomial;
MonoidElement  : MonoidElement := (x, y) -> new sameMonoid(x, y) from x.RawMonomial : y.RawMonomial;
MonoidElement  / MonoidElement := (x, y) -> (sameMonoid(x, y))_(listForm x - listForm y);

MonoidElement Array := (m, x) -> product(rawSparseListFormMonomial raw m, (k, e) -> x#k^e)
MonoidElement == ZZ := (m, i) -> if i === 1 then m == (class m)#1 else error "no method for '=='"
MonoidElement  * ZZ := (x, i) -> if i === 1 then x else error "no method for multiplying available"
MonoidElement  ^ ZZ := (x, n) -> new class x from x.RawMonomial ^ n
MonoidElement  ? ZZ := (x, n) -> x ? n_(class x)
ZZ  ? MonoidElement := (n, x) -> n_(class x) ? x
ZZ  * MonoidElement := (i, x) -> x  * i
ZZ == MonoidElement := (i, m) -> m == i

-- TODO: would skipping the the free module check make this slower?
-- TODO: make new G from vector deg do this in the engine?
reduceDegree = (G, deg) -> if isFreeModule G then deg else entries sum(deg, (super G)_*, times)

-- TODO: move to engine
degree MonoidElement := m -> (
    degrk := degreeLength(M := class m);
    if m == 1 then return toList(degrk : 0);
    degs := degrees M;
    reduceDegree(M.degreeGroup,
	sum(rawSparseListFormMonomial raw m,
	    (k, e) -> e * degs#k)))

baseName MonoidElement := m -> if #(s := rawSparseListFormMonomial raw m) == 1 and s#0#1 == 1
    then (class m).generatorSymbols#(s#0#0) else error "expected a generator"

promote(IndexedVariable, RingElement) := RingElement => (m, R) -> promote(value m, R)
promote(MonoidElement, RingElement) := RingElement => (m, R) -> (
    k := coefficientRing first flattenRing R;
    -- TODO: audit this code
    if instance(m, k)
    or instance(m, monoid R)
    or instance(m, R.FlatMonoid)
    -- TODO: what does rawTerm expect?
    then new R from rawTerm(R.RawRing, raw 1_k, m.RawMonomial)
    else "expected monomial from same ring")

lift(IndexedVariable, MonoidElement) := MonoidElement => (m, M) -> lift(value m, M)
lift(RingElement, MonoidElement) := MonoidElement => (m, M) -> (
    k := coefficientRing first flattenRing(R := ring m);
    if instance(m, monoid k)
    or instance(m, monoid R)
    or instance(m, R.FlatMonoid)
    then new M from m.RawMonomial
    else error "expected monomial from same monoid")

-- printing helpers
expressionTerms := (M, trms) -> ( exps := M.generatorExpressions;
    expressionTerm  := (k, v) -> if v =!= 1 then Power{exps#k, v} else exps#k;
    if #trms === 0 then ONE else if #trms === 1 then hold expressionTerm trms#0 else Product apply(trms, expressionTerm))
 -- hold needed for single variables

expression MonoidElement := x -> expressionTerms(class x, rawSparseListFormMonomial x.RawMonomial);

toExternalString MonoidElement :=
toString MonoidElement := toString @@ expression;
net      MonoidElement :=      net @@ expression;
texMath  MonoidElement :=  texMath @@ expression;

-----------------------------------------------------------------------------

rawLeadMonomialR = method()
rawLeadMonomialR RingElement := RawMonomial => f -> rawLeadMonomial(numgens monoid ring f, raw f)

-- TODO: confirm whether this way of catching is efficient
leadMonomial RingElement := RingElement => f -> (
     R := ring f;
     k := coefficientRing R;
     n := numgens monoid R;
     leadMonomial R := f -> new R from rawTerm(raw R, raw 1_k, rawLeadMonomial(n, raw f)); -- quicker the second time
     leadMonomial f)

listForm = method()
listForm MonoidElement := m -> (
    x := new MutableList from numgens class m : 0;
    scan(rawSparseListFormMonomial raw m, (i, e) -> x#i = e);
    toList x)
listForm RingElement := f -> (
    R := ring f;
    n := numgens R;
    k := coefficientRing R;
    (cc, mm) := rawPairs(raw k, raw f);
    toList apply(cc, mm, (c, m) -> (exponents(n, m), promote(c, k))))

-- declared in engine.m2, where exponents(ZZ, RawMonomial) is defined
exponents MonoidElement := lookup(listForm, MonoidElement)
exponents RingElement   := f -> first \ listForm f

-- declared in engine.m2
standardForm MonoidElement := m -> new HashTable from rawSparseListFormMonomial raw m
standardForm RingElement   := f -> (
    R := ring f;
    k := coefficientRing R;
    (cc, mm) := rawPairs(raw k, raw f);
    new HashTable from toList apply(cc, mm, (c, m) -> (standardForm m, new k from c)))

-- used to be in matrix2.m2
coefficient = method (TypicalValue => RingElement)
coefficient(MonoidElement, RingElement) := (m, f) -> (
    R := ring f;
    M := monoid R;
    k := coefficientRing R;
    if instance(m, M)
    then new k from rawCoefficient(raw k, raw f, raw m)
    else error "coefficient: expected a monomial from the corresponding monoid")
coefficient(RingElement, RingElement) := (m, f) -> (
    R := ring f;
    k := coefficientRing R;
    -- TODO: audit this code and how it is used in towers and inexact fields
    if size m === 1 and leadCoefficient m == 1
    then promote(rawCoefficient(raw k, raw f, rawLeadMonomialR m), k)
    else error "coefficient: expected a monomial from the same ring")

RingElement _ MonoidElement := RingElement => (f, m) -> coefficient(m, f)
RingElement _ RingElement   := RingElement => (f, m) -> coefficient(m, f)

-----------------------------------------------------------------------------
-- Monoid type declarations and basic methods
-----------------------------------------------------------------------------

-- internal Monoid keys
protect generatorExpressions
protect generatorSymbols
protect indexStrings
protect indexSymbols

Monoid = new Type of Type
Monoid.synonym = "monoid"

OrderedMonoid = new Type of Monoid
OrderedMonoid.synonym = "ordered monoid"

GeneralOrderedMonoid = new Type of OrderedMonoid
GeneralOrderedMonoid.synonym = "general ordered monoid"
GeneralOrderedMonoid.Engine = true
raw GeneralOrderedMonoid := M -> M.RawMonoid

use        Monoid := M ->(if M.?use     then M.use M; M)
vars       Monoid := M -> if M.?vars    then M.vars    else {}
numgens    Monoid := M -> if M.?numgens then M.numgens else 0
options    Monoid := M -> if M.?Options then M.Options
generators Monoid := o -> lookup(vars, Monoid)

-- TODO: neither Matrix nor Module are defined yet
degreeGroup = method()
degreeGroup  Ring   :=
degreeGroup  Monoid := M -> if M.?degreeGroup  then M.degreeGroup  else 0
degrees      Monoid := M -> if (o := options M).?Degrees then o.Degrees else {}
degreeLength Monoid := M -> if M.?degreeLength then M.degreeLength      else 0

monomialOrderMatrix Ring   := R -> monomialOrderMatrix monoid R
monomialOrderMatrix Monoid := M -> monomialOrderMatrix M.RawMonomialOrdering

-----------------------------------------------------------------------------

Monoid _*     := List => M -> vars M
-- this implementation is for sparse monomials, but it might
-- make sense to have a dense implementation
Monoid _ ZZ   := MonoidElement => (M, i) -> (vars M)#i
Monoid _ List := MonoidElement => (M, v) -> if #v === 0 then M#1 else product(
    take(vars M, #v), v, (x, i) -> x^i)

ZZ            _ Monoid := MonoidElement => (i, M) -> if i === 1 then M#1 else error "expected integer to be 1"
RingElement   _ Monoid :=
MonoidElement _ Monoid := MonoidElement => (x, M) -> (baseName x)_M

String _ Monoid := MonoidElement => (s, M) -> (
    if M.?indexStrings and M.indexStrings#?s then M.indexStrings#s
    else error "variable not found in monoid")
Symbol _ Monoid := MonoidElement => (x, M) -> (
    if M.?indexSymbols and M.indexSymbols#?x then M.indexSymbols#x
    else error "symbol not found in monoid")
IndexedVariable _ Monoid := MonoidElement => (x, M) -> (
    if M.?indexSymbols and M.indexSymbols#?x then M.indexSymbols#x
    else error "indexed variable not found in monoid")

RingElement   _ Ring :=
MonoidElement _ Ring := RingElement => (x, M) -> try (baseName x)_M else promote(x, M)

String _ Ring := RingElement => (s, M) -> (
    if M.?indexStrings and M.indexStrings#?s then M.indexStrings#s
    else error "variable not found in ring")
Symbol _ Ring := RingElement => (x, M) -> (
    if M.?indexSymbols and M.indexSymbols#?x then M.indexSymbols#x
    else error "symbol not found in ring")
IndexedVariable _ Ring := RingElement => (x, M) -> (
    if M.?indexSymbols and M.indexSymbols#?x then M.indexSymbols#x
    else error "indexed variable not found in ring")

-- kept for backwards compatibility
Ring _ String :=
Ring _ Symbol :=
Ring _ IndexedVariable := RingElement => (M, x) -> x_M -- deprecated

-- fixes the issue of declaring kk[x] followed by kk[x_1]
-- TODO: remove the warning
RingElement   _ Thing :=
MonoidElement _ Thing := IndexedVariable => (x, i) -> (baseName x)_i

-----------------------------------------------------------------------------
-- monoid
-----------------------------------------------------------------------------

monoidDefaults = new OptionTable from {
    Variables        => null,		-- either the number or a list of variables
    VariableBaseName => "p",		-- the symbol to use as a IndexedVariableTable
    -- VariableOrder => null,		-- not implemented yet
    --
    Global   => true,			-- means that all variables are > 1
    Local    => false,			-- means that all variables are < 1, default weight = -1, and implies Global => false
    Inverses => false,			-- means that the exponent vectors may be negative
    Weights  => {},			-- default weight is 1, unless Local=>true
    --
    Degrees     => null,		-- the degrees of the variables in the monoid
    DegreeLift  => null,		-- a degree lifting map from the monoid ring to the coeff ring. Gives an error if lifting is not possible
    DegreeMap   => null -* identity *-,	-- the degree map to use, if Join=>false is specified, for converting degrees in the coeff ring to degrees in the monoid ring
    DegreeRank  => null,		-- specifying DegreeRank=>3 and no Degrees means degrees {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {0, 0, 1}, ...}
    DegreeGroup => null,		-- a ZZ-module representing the degree group of the ring
    Heft        => null -* find one *-,	-- an integer vector whose dot product with the degrees of all variables is positive
    Join        => null -* true *-,	-- whether the degrees in the monoid ring are given by joining the degree of the coefficient with the degree of the monomial
    --
    MonomialOrder => {			-- the monomial ordering of the monoid
	GRevLex, Position => Up},
    MonomialSize  => 32,		-- we had this set to null, but some of the code needs a number here...
    --
    SkewCommutative => {},		-- list of skew commuting variables in the monoid ring
    WeylAlgebra     => {},		-- pairs of Weyl algebra variables in the monoid ring
    Constants       => false,		-- whether to use rawTowerRing when making a monoid ring
    }

monoid = method(
    Dispatch     => Thing,
    Options      => monoidDefaults,
    TypicalValue => Monoid)
monoid List  := opts -> args -> monoid(new Array from args, opts, Local => true)
monoid Array := opts -> args -> (
    (opts, args) = override(opts, nonnull toSequence args);
    if opts.Variables === null
    then opts = merge(opts, new OptionTable from {Variables => args}, last)
    else if args =!= () then error "variables provided conflict with Variables option";
    newMonoid setMonoidOptions opts)

-- TODO: Would these be useful?
--monoid MonoidElement := o -> x -> class x
--monoid RingElement   := o -> x -> monoid ring x

-----------------------------------------------------------------------------
-- helpers for printing Monoid objects
-----------------------------------------------------------------------------

isDefault = (opts, key) -> (opts#key === monoidDefaults#key
-- TODO: uncomment more lines to adjust which default options are shown
    or key == DegreeGroup   and isFreeModule opts#key
--    or key == Degrees       and unique opts#key == {{1}}
--    or key == Heft          and unique opts#key ==  {1}
    or key == MonomialOrder and opts#key === new VerticalList from {
	MonomialSize => 32, GRevLex => toList(#opts.Degrees:1), Position => Up }
    )

monoidParts = M -> (
    opts := M.Options;
    G := if M.?generatorExpressions then toSequence runLengthEncode M.generatorExpressions;
    D := runLengthEncode if opts.DegreeRank === 1 then flatten opts.Degrees else opts.Degrees / (deg -> VerticalList deg);
    L := nonnull splice ( G, if not isDefault(opts, Degrees) then Degrees => D,
	apply(( DegreeGroup, Heft, Join, MonomialOrder, WeylAlgebra, SkewCommutative, Inverses, Local, Global ),
	    key -> if opts#?key and not isDefault(opts, key) then key => rle opts#key)))

expressionMonoid = M -> (
    T := if (options M).Local === true then List else Array;
    new T from apply(monoidParts M, expression))

describe   Monoid := M -> Describe new Parenthesize from { (expression monoid) expressionMonoid M }
expression Monoid := M -> (
    if hasAttribute(M, ReverseDictionary)
    then expression getAttribute(M, ReverseDictionary)
    else new Parenthesize from { (expression monoid) expressionMonoid M } )

toExternalString Monoid := toString @@ describe
toString Monoid := toString @@ expression
net      Monoid :=      net @@ expression
texMath  Monoid :=  texMath @@ expression

Monoid#AfterPrint = M -> (
    class M,
    if not isFreeModule degreeGroup M
    then ", with torsion degree group"
    -- TODO: print whether M is ordered, a free algebra, etc.
    )

-----------------------------------------------------------------------------
-- degreesMonoid
-----------------------------------------------------------------------------

degreesMonoid = method(TypicalValue => Monoid)
degreesMonoid Ring := R -> error "no degrees monoid present"
degreesMonoid Monoid := M -> if M.?degreesMonoid then M.degreesMonoid else error "no degrees monoid present"

degreesMonoid ZZ := n -> if n == 0 then degreesMonoid {} else (
    T := getSymbol "T";
    monoid [ if n === 1 then T else T_0 .. T_(n-1),
	Degrees       => { n:{} },
	DegreeRank    => 0,
	MonomialOrder => { Weights => { n:-1 }, GroupLex => n }, -- TODO: why are there weights?
	Global        => false,
	Inverses      => true])
degreesMonoid ZZ := memoize lookup(degreesMonoid, ZZ)

degreesMonoid List := vec -> (
    T := getSymbol "T";
    n := if isListOfIntegers(vec = deepSplice vec) then #vec
    else error "degreesMonoid: expected a list of integers";
    monoid [ if n === 1 then T else T_0 .. T_(n-1),
	Degrees       => vec,
	DegreeRank    => min(#vec, 1),
	MonomialOrder => { Weights => -vec, GroupLex => n },
	Global        => false,
	Inverses      => true])
degreesMonoid List := memoize lookup(degreesMonoid, List)

-----------------------------------------------------------------------------
-- findHeft
-----------------------------------------------------------------------------

checkHeft = (degs, heftvec) -> all(degs, d -> sum(d, heftvec, times) > 0)
-- vector that zeros the torsion part of the degrees
-- TODO: what should happen when there are zero-divisors that are not torsion?
-- compare with freeComponents and torsionComponents in basis.m2
zeroTorsion = (degrk, G) -> if instance(G, Module) then apply(numgens G,
    i -> if QQ ** G_{i} == 0 then 0 else 1) else toList(degrk : 1)

findHeft = method(Options => { DegreeRank => null, DegreeGroup => null }, TypicalValue => List )
findHeft List := opts -> degs -> (
    -- this function is adapted from one written by Greg Smith;
    -- it appears in the FourierMotzkin package documentation
    -- we return null if no heft vector exists
    degrk := opts.DegreeRank;
    group := opts.DegreeGroup;
    if not isListOfListsOfIntegers degs   then error "findHeft: expected a list of lists of integers";
    if degrk === null then (
	if #degs > 0 then degrk = #degs#0 else error "findHeft: expected either a degree list or DegreeRank");
    if not instance(degrk, ZZ)            then error "findHeft: expected option DegreeRank to be an integer";
    if not all(degs, d -> #d === degrk)   then error("findHeft: expected all degrees to be of length ", degrk);
    --
    ones := zeroTorsion(degrk, group);
    if group =!= null then degs = apply(degs, deg -> apply(#deg, i -> deg_i * ones_i));
    if #degs === 0 then return ones;
     if degrk === 0 then return null;
     if degrk === 1 then return if all(degs,d->d#0 > 0) then {1} else if all(degs,d->d#0 < 0) then {-1} ;
    -- TODO: should this heuristic look at other degree components also?
     if all(degs,d->d#0 > 0) then return splice {  1, degrk-1:0 };
     if all(degs,d->d#0 < 0) then return splice { -1, degrk-1:0 };
    heftvec := - sum entries map(ZZ, rawFourierMotzkin raw matrix degs);
    if heftvec === 0 then return null;
    if (g := gcd heftvec) > 1   then heftvec = apply(heftvec, h -> h // g);
    if checkHeft(degs, heftvec) then heftvec)

-----------------------------------------------------------------------------
-- helpers for monoid
-----------------------------------------------------------------------------

processHeft = (degrk, degs, group, heftvec, inverses) -> (
    if inverses then return null;
    if heftvec =!= null then (
	heftvec = splice heftvec;
	if not isListOfIntegers heftvec then error "expected Heft option to be a list of integers";
	if #heftvec > degrk then error("expected Heft option to be of length at most the degree rank (", degrk, ")");
	if #heftvec < degrk then heftvec = join(heftvec, degrk - #heftvec : 0));
    if heftvec =!= null and checkHeft(degs, heftvec)
    then heftvec else findHeft(degs, DegreeRank => degrk, DegreeGroup => group))

-----------------------------------------------------------------------------

diagonalDegrees = (degrk, nvars) -> table(nvars, degrk,
    (i, j) -> if j === i or i >= degrk and j === degrk-1 then 1 else 0)

processDegrees = (degs, degrk, group, nvars) -> (
    if not (degrk === null or instance(degrk, ZZ) and degrk >= 0)
    then error("expected DegreeRank option to be a non-negative integer");
    if not (group === null or instance(group, Module) and ring group === ZZ)
    then error "expected DegreeGroup option to be a ZZ-module";
    -- read the degrees from the generators of the degree group
    if degs === null then degs = (
	if group =!= null then
	if nvars === numgens group then generators group
	else diagonalDegrees(rank ambient group, nvars)
	else diagonalDegrees(if degrk === null then 1 else degrk, nvars));
    -- read the degrees from (the columns of) a map of ZZ-modules
    if instance(degs, Matrix) then (
	if group === null then group = target degs;
	if degrk === null then degrk = numRows degs;
	degs = entries transpose matrix degs);
    -- sanity checks
    if instance(degs, List) then (
	degs = apply(spliceInside degs, d -> if class d === ZZ then {d} else spliceInside d);
	degrk = if degrk =!= null then degrk else if degs#?0 then #degs#0 else 1; -- so that degreeLength monoid[] = 1
	group = if group =!= null then group else ZZ^degrk;
	if not #degs === nvars                          then error "expected as many degrees as there are variables";
	if not isListOfListsOfIntegers degs             then error "expected each degree to be an integer or list of integers";
	if degs#?0 and unique(length \ degs) != {degrk} then error("expected all degrees to have length ", degrk);
	if degrk != rank ambient group                  then error "expected all degrees to be in the degree group";
	(degs, degrk, group))
    else error "expected Degrees option to be list of degrees")

-----------------------------------------------------------------------------

makeVars = (n, var) -> toList(
    (name, ind) := if instance(var = baseName' var, IndexedVariable) then toSequence var else (var, null);
    (a, b) := if ind === null then (0, n-1) else (prepend(0, listSplice ind), prepend(n-1, listSplice ind));
    name_a .. name_b)

-- check that the objects serving as variables have an assignment method
-- TODO: why is (symbol <-, T) and not (symbol <-, T, Thing) the right method sequence for assignment?
checkSymbol = sym -> if instance(sym, Symbol) or lookup(symbol <-, class sym) =!= null then sym else error()

-- turns {x, y, z, y} into {x, y_0, z, y_1}
-- adding 'toString' in a few places will eliminate more duplications
-- but makes creating temporary rings in functions more difficult.
dedupSymbols = varlist -> (
    while 0 < repeats varlist do (
	counter := applyPairs(tally varlist, (name, count) ->
	    name => new MutableList from if count == 1 then {name} else makeVars(count, name));
	varlist  = apply(varlist, var -> remove(counter#var, 0)));
    varlist)

-- also used in AssociativeAlgebras.m2
findSymbols = varlist -> dedupSymbols toList apply(pairs listSplice varlist,
    -- varlist is a list or sequence of items we wish to use for variable names.
    -- these may be: Symbol's, RingElement's (which are variables in a ring) or lists or sequences of such.
    -- Return value: a List of Symbol's and IndexVariable's (or an error message gets issued)
    (i, var) -> try ( if class var === ZZ then var else checkSymbol baseName' var ) else error concatenate(
	"encountered object not usable as variable at position ", toString i, " in list:",
	newline, 8, silentRobustNetWithClass(max(printWidth, 80) - 8, 5, 3, var)))

processVars := method()
processVars Thing := x -> findSymbols {x}
processVars VisibleList := findSymbols
processVars(VisibleList, Thing) := (v, xx) -> findSymbols v
processVars(Thing, Thing) := (x, xx) -> findSymbols {x}
processVars(ZZ,    Thing) := (n, xx) -> makeVars(n, xx)
processVars ZZ := x -> {x}

processSkew := (n, skewvars) -> toList(
    if skewvars === true  then 0 ..< n else
    if skewvars === false then {}      else
    if instance(skewvars, VisibleList) then flatten apply(listSplice skewvars, processVars)
    else error "SkewCommutative: expected option to be true, false, or a list or indices or variables")

processWeyl := weylvars -> (
    (xvars, dvars, hvar) := ({}, {}, {});
    scan(flatten {weylvars},
	x -> if instance(x, VisibleList) or instance(x, Option) then (
	    if #x == 2 then (xvars, dvars) = (join(xvars, processVars splice {x#0}), join(dvars, processVars splice {x#1}))
	    else error "WeylAlgebra: expected option formats {{x,dx},...}, {x=>dx,...}, or {(x,...)=>(dx,...)}")
	else hvar = join(hvar, processVars x));
    if #dvars =!= #xvars              then error "WeylAlgebra: unexpected number of differential variables";
    if #hvar > min(#dvars, 1)         then error "WeylAlgebra: encountered extra homogenizing variable";
    if 0 < repeats join(xvars, dvars) then error "WeylAlgebra: encountered a repeated variable";
    join(pack_2 mingle(xvars, dvars), hvar))

-----------------------------------------------------------------------------

-- one-time warning for torsion grading groups; to be removed soon
showTorsionWarning = true

-- check the options for consistency, and set everything to the correct defaults
setMonoidOptions = opts -> (
    opts = new MutableHashTable from opts;
    opts.Variables = processVars(opts.Variables, opts.VariableBaseName);
    (degs, degrk, group) := processDegrees(
	opts.Degrees, opts.DegreeRank, opts.DegreeGroup, n := #opts.Variables);
    opts.Heft = processHeft(degrk, degs, group, opts.Heft, opts.Inverses);
    opts.Degrees = degs;
    opts.DegreeRank = degrk;
    opts.DegreeGroup = group;
    if showTorsionWarning and instance(group, Module) and not isFreeModule group
    then ( showTorsionWarning = false; printerr "Warning: computations over rings with torsion grading groups are experimental" );
    if not isMember(opts.Join, {null, true, false}) then error "expected Join option to be true, false, or null";
    -- if opts.Join =!= false then (
    --	if opts.DegreeMap =!= null then error "DegreeMap option provided without Join=>false";
    --	if opts.DegreeLift =!= null then error "DegreeLift option provided without Join=>false";
    --	);
    -- TODO: bring the sanity checking for the Weyl and Skew variables here
    opts.WeylAlgebra = processWeyl opts.WeylAlgebra;
    opts.SkewCommutative = processSkew(n, opts.SkewCommutative);
    -- TODO: allow rings with only some invertible variables
    if class opts.Inverses =!= Boolean then error "expected Inverses option to be true or false";
    -- TODO: allow rings with some skew commuting variables and some inverses https://github.com/Macaulay2/M2/issues/1440
    if opts.SkewCommutative =!= {} and opts.Inverses then error "skew commutative monoid with inverses requested";
    if opts.Local === true then ( opts.Global = false;
	if opts.Weights === {} then opts.Weights = toList(n:-1));
    opts)

-----------------------------------------------------------------------------
-- Main monoid constructor
-----------------------------------------------------------------------------

newMonoid = opts -> (
    M := new GeneralOrderedMonoid of MonoidElement;
    M#1 = new M from rawMakeMonomial{}; -- identity
    M.Engine = true; -- used in quotring.m2 and res.m2
    --
    varlist := baseName       \ opts.Variables;
    group   := M.degreeGroup  = opts.DegreeGroup;
    degrk   := M.degreeLength = opts.DegreeRank;
    degs    := M.degrees      = opts.Degrees;
    nvars   := M.numgens      = #varlist;
    M.vars   = M.generators   = apply(nvars, i -> new M from rawVarMonomial(i, 1));
    heftvec := M.heft         = if opts.Heft === null then toList(degrk:0) else opts.Heft;
    -- see engine.m2
    (MOopts, MOoptsint, rawMO, logMO) := makeMonomialOrdering(
	opts.MonomialSize, opts.Inverses, nvars,
	if degreeLength M == 0 or opts.Heft === null then toList(nvars:1)
	else apply(degs, d -> if (w := dotprod(d, heftvec)) > 0 then w else 1),
	opts.Weights, opts.MonomialOrder);
    M.RawMonomialOrdering = rawMO;
    M#"raw creation log" = new Bag from {logMO};
    M#"options" = MOopts; -- these are exactly the arguments given to rawMonomialOrdering
    -- these are the options given to rawMonomialOrdering minus MonomialSize,
    -- except Tiny and Small aren't there yet and GRevLex=>nvars hasn't been expanded
    opts.MonomialOrder = new VerticalList from MOoptsint;
    remove(opts, MonomialSize);
    remove(opts, Weights);
    remove(opts, VariableBaseName);
    -- symbols and expressions
    M.generatorSymbols     = varlist;
    M.generatorExpressions = apply(varlist, x -> if instance(x, Symbol) then x else expression x);
    -- the empty monoid is the unique monoid without a degrees ring or degrees monoid
    M.RawMonoid = if nvars == 0 and not madeTrivialMonoid then (
	madeTrivialMonoid = true;
	rawMonoid())
    else (
	M.degreesRing = if opts.Heft =!= null then degreesRing heftvec else degreesRing degrk; -* shouldn't really be needed *-
	M.degreesMonoid = monoid M.degreesRing;
	rawMonoid(
	    M.RawMonomialOrdering,
	    raw M.degreesRing,
	    toSequence M.generators / toString,
	    flatten degs,
	    flatten heftvec));
    -- TODO: is this necessary?
    if opts.Global and not opts.Inverses and not all(M.generators, x -> x > M#1)
    then error "not all variables are > 1, and Global => true";
    --
    M.index        = hashTable apply(M.generatorSymbols, 0 ..< nvars,  identity);
    M.indexSymbols = hashTable apply(M.generatorSymbols, M.generators, identity);
    try M.indexStrings = applyKeys(M.indexSymbols, toString); -- no error, because this is often harmless
    M.use = M -> scan(M.generatorSymbols, M.vars, (sym, val) -> sym <- val);
    M.Options = (new OptionTable from opts) ++ {
	WeylAlgebra     => monoidSymbols_M opts.WeylAlgebra, -- FIXME: monoidIndices breaks Dmodules
	SkewCommutative => monoidIndices_M opts.SkewCommutative,
	};
    M)

monoidSymbols = (M, v) -> apply(v, monoidSymbol_M)
monoidSymbol  = (M, x) -> ( b := try baseName x;
    if instance(x, ZZ)    then M.generators#x else
    if instance(x, List)  then monoidSymbols(M, x)  else
    if M.indexSymbols#?b  then M.indexSymbols#b     else
    if M.?indexStrings
    and M.indexStrings#?x then M.indexSymbols#x     else
    error("expected an index, symbol, or name of variable of the ring or monoid: ", toString x))

-- also used in Elimination and Msolve
monoidIndices = (M, v) -> apply(v, monoidIndex_M)
monoidIndex   = (M, x) -> ( b := try baseName x;
    if instance(x, ZZ)    then x else
    if instance(x, List)  then monoidIndices(M, x) else
    if M.index#?b         then M.index#b else
    try index x else -- last ditch attempt for ring elements
    error("expected an integer or variable of the ring or monoid: ", toString x))

-----------------------------------------------------------------------------

monoidTensorDefaults = monoidDefaults ++ {
    MonomialOrder    => null,
    VariableBaseName => null,	-- monoids being tensored already have variable names
    Inverses         => null,	-- so we can detect a mixture and give an error
    }

-- TODO: not being used currently
tensoradj := (f,g,m,n) -> (
     if f === identity then (
	  if g === identity 
	  then identity
     	  else x -> join(take(x,m), g take(x,-n))
	  )
     else (
	  if g === identity 
	  then x -> join(f take(x,m), take(x,-n))
     	  else x -> join(f take(x,m), g take(x,-n))
	  ))

trimMO := o -> (
     numseen := 0;
     select(o, x -> not instance(x,Option) or x#0 =!= Position or 1 == (numseen = numseen + 1)))

degreePad = (n,x) -> (
     if instance(x,ZZ) then x = {x};
     if not instance(x,List) or not all(x,i -> instance(i,ZZ)) then error "expected degree map to return a list of integers";
     if #x > n then error("with Join => false, expected degree map to return a list of length at most ",toString n);
     join(toList(n-#x:0),x));

degreeNoLift = () -> error "degree not liftable"

-- for handling SkewCommutative and WeylAlgebra indices when adjoining monoids
shiftAndJoin = (n, L, R) -> if R === null then L else join(L,
    apply(R, r -> if instance(r, ZZ) then r + n else apply(r, plus_n)))

-- TODO: do we want to support a syntax this?
--   'tensor (a => ZZ^2, b => ZZ^3, c => ZZ^4)'
Monoid ** Monoid := Monoid => (M, N) -> tensor(M, N)
Monoid^** ZZ     := Monoid => (M, n) -> BinaryPowerMethod(M, n, tensor, M -> monoid [],
    M -> error "Monoid ^** ZZ: expected non-negative integer")
tensor(Monoid, Monoid) := Monoid => monoidTensorDefaults >> opts0 -> (M, N) -> (
     Mopts := M.Options;
     Nopts := N.Options;
     opts := new MutableHashTable from opts0;
     opts.Weights = {};
     if opts.Variables === null 
     then opts.Variables = join(Mopts.Variables, Nopts.Variables)
     else opts.Variables = spliceInside opts.Variables;
     if opts.VariableBaseName =!= null then (
	  x := baseName' opts.VariableBaseName;
	  opts.Variables = apply(#opts.Variables, i -> x_i);
	  );
     if opts.MonomialOrder === null 
     then opts.MonomialOrder = trimMO join(Mopts.MonomialOrder,Nopts.MonomialOrder); -- product order
     if instance(opts.Degrees,List) then opts.Degrees = spliceInside opts.Degrees;
     if opts.Join === null then opts.Join = Mopts.Join;
     if opts.Degrees === null and opts.DegreeRank === null and opts.DegreeGroup === null then opts.DegreeGroup = (
	 if opts.Join === false then Mopts.DegreeGroup else Mopts.DegreeGroup ++ Nopts.DegreeGroup);
     if opts.Degrees === null and opts.DegreeRank === null then (
	  M0 := apply(Mopts.DegreeRank, i -> 0);
	  N0 := apply(Nopts.DegreeRank, i -> 0);
	  if opts.Join === null or opts.Join === true then (
	       opts.DegreeRank = Mopts.DegreeRank + Nopts.DegreeRank;
	       opts.Degrees = join( apply(Mopts.Degrees, d -> join(d,N0)), apply(Nopts.Degrees, e -> join(M0,e)) );
	       if opts.Heft === null and Nopts.Heft =!= null and Mopts.Heft =!= null then opts.Heft = join(Mopts.Heft,Nopts.Heft);
	       opts.DegreeMap = d -> join(M0,d);
	       opts.DegreeLift = d -> (
		    for i to #M0-1 do if d#i =!= 0 then degreeNoLift();
		    drop(d,#M0));
	       )
	  else if opts.Join === false then (
	       opts.DegreeRank = Mopts.DegreeRank;
	       dm := if opts.DegreeMap =!= null then opts.DegreeMap else if Mopts.DegreeMap =!= null then Mopts.DegreeMap else identity;
	       opts.DegreeMap = d -> degreePad(opts.DegreeRank,dm d);
	       lm := if opts.DegreeLift =!= null then opts.DegreeLift else if Mopts.DegreeLift =!= null then Mopts.DegreeLift;
	       opts.DegreeLift = (
		    if lm === null then (
			 if dm === identity then (
		    	      d -> (
			 	   for i from #N0 to #M0-1 do if d#i =!= 0 then degreeNoLift();
			 	   drop(d,#M0-#N0)))
	       		 else x -> error "degree lift function not provided (DegreeLift option)")
		    else lm);
	       opts.Degrees = join(Mopts.Degrees, apply(Nopts.Degrees, opts.DegreeMap));
	       if opts.Heft === null and Mopts.Heft =!= null then opts.Heft = Mopts.Heft -* a hint *-;
	       )
	  else error "tensor: expected Join option to be true, false, or null")
     else (
	  (degs,degrk,group) := processDegrees(opts.Degrees, opts.DegreeRank, opts.DegreeGroup, length opts.Variables);
	  opts.Degrees = degs;
	  opts.DegreeRank = degrk;
	  opts.DegreeGroup = group;
	  if opts.DegreeMap === null then opts.DegreeMap = Mopts.DegreeMap;
	  if opts.DegreeLift === null then opts.DegreeLift = Mopts.DegreeLift;
	  );
     if opts.Inverses === null 
     then (
	  if Mopts.Inverses =!= Nopts.Inverses then error "in tensor product of two monoids, one has Inverses=>true and the other doesn't";
	  opts.Inverses = Mopts.Inverses;
	  )
     else opts.Inverses = opts.Inverses;
    opts.Heft = processHeft(opts.DegreeRank, opts.Degrees, opts.DegreeGroup, opts.Heft, opts.Inverses);
     if Mopts.Global === false or Nopts.Global === false then opts.Global = false;
     oddp := x -> x#?0 and odd x#0;
     m := numgens M;
    -- FIXME: remove these two lines once Weyl variables are stored as indices in the monoid
    opts.WeylAlgebra     = shiftAndJoin_m (monoidIndices_M Mopts.WeylAlgebra, monoidIndices_N Nopts.WeylAlgebra);
    opts.SkewCommutative = shiftAndJoin_m (Mopts.SkewCommutative, Nopts.SkewCommutative);
     newMonoid setMonoidOptions opts)

-----------------------------------------------------------------------------
