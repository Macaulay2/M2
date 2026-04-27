-- -*- coding: utf-8 -*-

-*
Copyright 2023 Michael Morrow

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>
*-

newPackage("OIGroebnerBases",
    Headline => "OI-modules over Noetherian polynomial OI-algebras",
    Version => "1.0.0",
    Date => "September 6, 2023",
    Keywords => { "Commutative Algebra" },
    Authors => {
        { Name => "Michael Morrow", HomePage => "https://michaelmorrow.me", Email => "michaelhmorrow98@gmail.com" }
    },
    DebuggingMode => false,
    HomePage => "https://github.com/morrowmh/OIGroebnerBases"
)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- EXPORT AND PROTECT ----------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

export {
        "PolynomialOIAlgebra",
        "FreeOIModule", "ModuleInWidth", "VectorInWidth", "FreeOIModuleMap",
        "OIResolution",
        "ColUpRowUp", "ColUpRowDown", "ColDownRowUp", "ColDownRowDown",
        "RowUpColUp", "RowUpColDown", "RowDownColUp", "RowDownColDown",
        "makePolynomialOIAlgebra",
        "makeFreeOIModule", "installGeneratorsInWidth", "isZero", "getBasisElements", "getWidth", "getFreeOIModule", "getSchreyerMap", "getRank", "oiOrbit",
        "oiGB", "minimizeOIGB", "reduceOIGB", "isOIGB",
        "oiSyz",
        "describeFull", "ranks", "restrictedRanks", "oiRes", "isComplex",
        "VariableOrder",
        "DegreeShifts", "OIMonomialOrder",
        "TopNonminimal"
}

scan({
        targWidth, img,
        varRows, varSym, baseField, varOrder, algebras, maps,
        basisSym, genWidths, degShifts, polyOIAlg, monOrder, modules, basisKeys, wid, rawMod, freeOIMod, key, vec, oiMap, srcMod, targMod, genImages,
        quo, rem, divTuples,
        map0, idx0, im0, map1, idx1, im1
}, protect)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- BODY ------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Should be of the form {targWidth => ZZ, img => List}
OIMap = new Type of HashTable

net OIMap := f -> "Source: [" | net(#f.img) | "] Target: [" | net f.targWidth | "]" || "Image: " | net f.img

-- Evaluate an OI-map f at an integer n
OIMap ZZ := (f, n) -> f.img#(n - 1)

-- Make a new OIMap
-- Args: n = ZZ, L = List
makeOIMap := (n, L) -> new OIMap from {targWidth => n, img => L}

-- Get the OI-maps between two widths
-- Args: m = ZZ, n = ZZ
getOIMaps := (m, n) -> (
    if n < m then return {};

    sets := subsets(1..n, m);
    for i to #sets - 1 list makeOIMap(n, sets#i)
)

-- Compose two OI-maps
OIMap OIMap := (f, g) -> makeOIMap(f.targWidth, for i from 1 to #g.img list f g i)

-- Should be of the form {varRows => ZZ, varSym => Symbol, baseField => Ring, varOrder => Symbol, algebras => MutableHashTable, maps => MutableHashTable}
PolynomialOIAlgebra = new Type of HashTable

toString PolynomialOIAlgebra := P -> "(" | toString P.varRows | ", " | toString P.varSym | ", " | toString P.baseField | ", " | toString P.varOrder | ")"

net PolynomialOIAlgebra := P -> "Number of variable rows: " | net P.varRows ||
    "Variable symbol: " | net P.varSym ||
    "Base field: " | net P.baseField ||
    "Variable order: " | net P.varOrder

makePolynomialOIAlgebra = method(TypicalValue => PolynomialOIAlgebra, Options => {VariableOrder => RowUpColUp})
makePolynomialOIAlgebra(ZZ, Symbol, Ring) := opts -> (c, x, K) -> (
    if c < 1 then error "expected at least one row of variables";

    v := opts.VariableOrder;
    if not member(v, {
        ColUpRowUp, ColUpRowDown, ColDownRowUp, ColDownRowDown,
        RowUpColUp, RowUpColDown, RowDownColUp, RowDownColDown
    }) then error "invalid variable order";

    new PolynomialOIAlgebra from {
            varRows => c,
            varSym => x,
            baseField => K,
            varOrder => v,
            algebras => new MutableHashTable,
            maps => new MutableHashTable}
)

-- Lookup table for linearFromRowCol
orderTable := new HashTable from {
    ColUpRowUp => (P, n, i, j) -> P.varRows * (n - j + 1) - i,      -- x_(i',j') < x_(i,j) if j'<j or j'=j and i'<i
    ColUpRowDown => (P, n, i, j) -> P.varRows * (n - j) + i - 1,    -- x_(i',j') < x_(i,j) if j'<j or j'=j and i'>i
    ColDownRowUp => (P, n, i, j) -> P.varRows * j - i,              -- x_(i',j') < x_(i,j) if j'>j or j'=j and i'<i
    ColDownRowDown => (P, n, i, j) -> P.varRows * (j - 1) + i - 1,  -- x_(i',j') < x_(i,j) if j'>j or j'=j and i'>i
    RowUpColUp => (P, n, i, j) -> n * (P.varRows - i + 1) - j,      -- x_(i',j') < x_(i,j) if i'<i or i'=i and j'<j
    RowUpColDown => (P, n, i, j) -> n * (P.varRows - i) + j - 1,    -- x_(i',j') < x_(i,j) if i'<i or i'=i and j'>j
    RowDownColUp => (P, n, i, j) -> n * i - j,                      -- x_(i',j') < x_(i,j) if i'>i or i'=i and j'<j
    RowDownColDown => (P, n, i, j) -> n * (i - 1) + j - 1           -- x_(i',j') < x_(i,j) if i'>i or i'=i and j'>j
}

-- Linearize the variables based on P.varOrder
-- Args: P = PolynomialOIAlgebra, n = ZZ, i = ZZ, j = ZZ
linearFromRowCol := (P, n, i, j) -> (orderTable#(P.varOrder))(P, n, i, j)

-- Get the algebra of P in width n
-- Args: P = PolynomialOIAlgebra, n = ZZ
getAlgebraInWidth := (P, n) -> (
    -- Return the algebra if it already exists
    if P.algebras#?n then return P.algebras#n;

    -- Generate the variables
    local ret;
    variables := new MutableList;
    for j from 1 to n do
        for i from 1 to P.varRows do variables#(linearFromRowCol(P, n, i, j)) = P.varSym_(i, j);

    -- Make the algebra
    ret = P.baseField[toList variables, Degrees => {#variables:1}, MonomialOrder => {Lex}];

    -- Store the algebra
    P.algebras#n = ret
)

PolynomialOIAlgebra _ ZZ := (P, n) -> getAlgebraInWidth(P, n)

-- Get the algebra map induced by an OI-map
-- Args: P = PolynomialOIAlgebra, f = OIMap
getInducedAlgebraMap := (P, f) -> (
    -- Return the map if it already exists
    if P.maps#?f then return P.maps#f;

    -- Generate the assignments
    m := #f.img;
    n := f.targWidth;
    src := P_m;
    targ := P_n;
    subs := flatten for j from 1 to m list
        for i from 1 to P.varRows list src_(linearFromRowCol(P, m, i, j)) => targ_(linearFromRowCol(P, n, i, f j)); -- Permute the second index

    -- Make the map
    ret := map(targ, src, subs);

    -- Store the map
    P.maps#f = ret
)

-- Should be of the form {basisSym => Symbol, genWidths => List, degShifts => List, polyOIAlg => PolynomialOIAlgebra, monOrder => Thing, modules => MutableHashTable, maps => MutableHashTable, basisKeys => MutableHashTable}
FreeOIModule = new Type of HashTable

toString FreeOIModule := F -> "(" | toString F.basisSym | ", " | toString F.genWidths | ", " | toString F.degShifts | ")"

net FreeOIModule := F -> (
    monOrderNet := if F.monOrder === Lex then net Lex
    else if instance(F.monOrder, List) then "Schreyer"
    else error "invalid monomial order";

    "Basis symbol: " | net F.basisSym ||
    "Basis element widths: " | net F.genWidths ||
    "Degree shifts: " | net F.degShifts ||
    "Polynomial OI-algebra: " | toString F.polyOIAlg ||
    "Monomial order: " | monOrderNet
)

makeFreeOIModule = method(TypicalValue => FreeOIModule, Options => {DegreeShifts => null, OIMonomialOrder => Lex})
makeFreeOIModule(Symbol, List, PolynomialOIAlgebra) := opts -> (e, W, P) -> (
    shifts := if opts.DegreeShifts === null then toList(#W : 0)
    else if instance(opts.DegreeShifts, List) and #opts.DegreeShifts === #W then opts.DegreeShifts
    else error "invalid DegreeShifts option";

    -- Validate the monomial order
    if not opts.OIMonomialOrder === Lex and not (
        instance(opts.OIMonomialOrder, List) and 
        W === apply(opts.OIMonomialOrder, getWidth) and 
        #set apply(opts.OIMonomialOrder, getFreeOIModule) == 1) then error "invalid monomial order";

    new FreeOIModule from {
        basisSym => e,
        genWidths => W,
        degShifts => shifts,
        polyOIAlg => P,
        monOrder => opts.OIMonomialOrder,
        modules => new MutableHashTable,
        maps => new MutableHashTable,
        basisKeys => new MutableHashTable}
)

-- Get the rank of a FreeOIModule
getRank = method(TypicalValue => ZZ)
getRank FreeOIModule := F -> #F.genWidths

-- Check if a FreeOIModule is zero
isZero = method(TypicalValue => Boolean)
isZero FreeOIModule := F -> F.genWidths === {}

-- Should be of the form {wid => ZZ, rawMod => Module, freeOIMod => FreeOIModule}
ModuleInWidth = new Type of HashTable

net ModuleInWidth := M -> net M.rawMod | " in width " | net M.wid |
    if not set M.freeOIMod.degShifts === set {0} and not zero M.rawMod then ", degrees " | net flatten degrees M.rawMod else ""

-- Get the module of F in width n
-- Args: F = FreeOIModule, n = ZZ
getModuleInWidth := (F, n) -> (
    -- Return the module if it already exists
    if F.modules#?n then return F.modules#n;

    -- Generate the degrees
    alg := getAlgebraInWidth(F.polyOIAlg, n);
    degList := for i to #F.genWidths - 1 list binomial(n, F.genWidths#i) : F.degShifts#i;

    -- Generate and store the module
    F.modules#n = new ModuleInWidth of VectorInWidth from hashTable {
        wid => n,
        rawMod => alg^degList,
        freeOIMod => F
    }
)

FreeOIModule _ ZZ := (F, n) -> getModuleInWidth(F, n)

use ModuleInWidth := M -> ( use getAlgebraInWidth(M.freeOIMod.polyOIAlg, M.wid); M )

VectorInWidth = new Type of HashTable

-- Should be of the form {key => Sequence, vec => VectorInWidth}
KeyedVectorInWidth = new Type of HashTable

-- Get the width of a VectorInWidth
getWidth = method(TypicalValue => ZZ)
getWidth VectorInWidth := v -> (class v).wid

-- Get the FreeOIModule of a VectorInWidth
getFreeOIModule = method(TypicalValue => FreeOIModule)
getFreeOIModule VectorInWidth := v -> (class v).freeOIMod

-- Get the basis keys in a given width
-- Args: F = FreeOIModule, n = ZZ
getBasisKeys := (F, n) -> (
    -- Return the basis keys if they already exist
    if F.basisKeys#?n then return F.basisKeys#n;

    -- Store the basis keys
    F.basisKeys#n = flatten for i to #F.genWidths - 1 list
        for oiMap in getOIMaps(F.genWidths#i, n) list (oiMap, i + 1)
)

-- Make a VectorInWidth
-- Args: M = ModuleInWidth, A => List
makeVectorInWidth := (M, A) -> new M from new VectorInWidth from A

-- Make a VectorInWidth with a single basis key
-- Args: M = ModuleInWidth, key = Sequence, elt = RingElement
makeSingle := (M, key, elt) -> (
    A := for keyj in getBasisKeys(M.freeOIMod, M.wid) list keyj => if key === keyj then elt else 0_(class elt);
    makeVectorInWidth(M, A)
)

-- Make a KeyedVectorInWidth
-- Args: M = ModuleInWidth, key0 = Sequence, elt = RingElement
makeKeyedVectorInWidth := (M, key0, elt) -> new KeyedVectorInWidth from {key => key0, vec => makeSingle(M, key0, elt)}

-- Make the zero VectorInWidth
-- Args: M = ModuleInWidth
makeZero := M -> (
    A := for key in getBasisKeys(M.freeOIMod, M.wid) list key => 0_(getAlgebraInWidth(M.freeOIMod.polyOIAlg, M.wid));
    makeVectorInWidth(M, A)
)

-- Install the generators in a given width
installGeneratorsInWidth = method();
installGeneratorsInWidth(FreeOIModule, ZZ) := (F, n) -> (
    M := getModuleInWidth(F, n);
    K := getBasisKeys(F, n);
    
    for key in K do F.basisSym_((key#0).targWidth, (key#0).img, key#1) <- makeSingle(M, key, 1_(getAlgebraInWidth(F.polyOIAlg, n)))
)

-- Check if a VectorInWidth is zero
isZero VectorInWidth := v -> (
    for val in values v do if not zero val then return false;
    true
)

-- Get the terms of a VectorInWidth
terms VectorInWidth := v -> flatten for key in keys v list
    for term in terms v#key list makeSingle(class v, key, term)

-- Get the keyed terms of a VectorInWidth
-- Args: v = VectorInWidth
keyedTerms := v -> flatten for key in keys v list
    for term in terms v#key list makeKeyedVectorInWidth(class v, key, term)

-- Get the keyed singles of a VectorInWidth
-- Args: v = VectorInWidth
keyedSingles := v -> flatten for key in keys v list
    if zero v#key then continue else makeKeyedVectorInWidth(class v, key, v#key)

-- Get the ith basis element of a FreeOIModule
-- Args: F = FreeOIModule, i = ZZ
-- Comment: expects 0 <= i <= #F.genWidths - 1
getBasisElement := (F, i) -> (
    n := F.genWidths#i;
    M := getModuleInWidth(F, n);
    key := (makeOIMap(n, toList(1..n)), i + 1);
    makeSingle(M, key, 1_(getAlgebraInWidth(F.polyOIAlg, n)))
)

-- Get the basis elements of a FreeOIModule
getBasisElements = method(TypicalValue => List)
getBasisElements FreeOIModule := F -> for i to #F.genWidths - 1 list getBasisElement(F, i)

-- Get the keyed lead term of a VectorInWidth
keyedLeadTerm := v -> (
    if isZero v then return new KeyedVectorInWidth from {key => null, vec => v};

    T := keyedTerms v;
    if #T === 1 then return T#0;

    largest := T#0;
    for term in T do if largest < term then largest = term;
    largest
)

-- Get the lead term of a VectorInWidth
leadTerm VectorInWidth := v -> (keyedLeadTerm v).vec

-- Get the keyed lead monomial of a VectorInWidth
keyedLeadMonomial := v -> (
    if isZero v then error "the zero element has no lead monomial";
    lt := keyedLeadTerm v;
    makeKeyedVectorInWidth(class v, lt.key, leadMonomial lt.vec#(lt.key))
)

-- Get the lead monomial of a VectorInWidth
leadMonomial VectorInWidth := v -> (keyedLeadMonomial v).vec

-- Get the lead coefficient of a VectorInWidth
leadCoefficient VectorInWidth := v -> (
    if isZero v then return 0_(getAlgebraInWidth((class v).freeOIMod.polyOIAlg, (class v).wid));
    lt := keyedLeadTerm v;
    leadCoefficient lt.vec#(lt.key)
)

-- Cache for storing KeyedVectorInWidth term comparisons
compCache = new MutableHashTable

-- Comparison method for KeyedVectorInWidth terms
KeyedVectorInWidth ? KeyedVectorInWidth := (v, w) -> (
    keyv := v.key;
    keyw := w.key;
    monv := leadMonomial(v.vec#keyv);
    monw := leadMonomial(w.vec#keyw);
    oiMapv := keyv#0;
    oiMapw := keyw#0;
    idxv := keyv#1;
    idxw := keyw#1;
    fmod := (class v.vec).freeOIMod;
    ord := fmod.monOrder;

    -- Return the comparison if it already exists
    if compCache#?(keyv, monv, keyw, monw, ord) then return compCache#(keyv, monv, keyw, monw, ord);

    -- Generate the comparison
    local ret;
    if v === w then ret = symbol ==
    else if ord === Lex then ( -- Lex order
        if not idxv === idxw then ( if idxv < idxw then ret = symbol > else ret = symbol < )
        else if not oiMapv.targWidth === oiMapw.targWidth then ret = oiMapv.targWidth ? oiMapw.targWidth
        else if not oiMapv.img === oiMapw.img then ret = oiMapv.img ? oiMapw.img
        else ret = monv ? monw
    )
    else if instance(ord, List) then ( -- Schreyer order
        fmodMap := new FreeOIModuleMap from {srcMod => fmod, targMod => getFreeOIModule ord#0, genImages => ord};
        lmimgv := keyedLeadMonomial fmodMap v.vec;
        lmimgw := keyedLeadMonomial fmodMap w.vec;

        if not lmimgv === lmimgw then ret = lmimgv ? lmimgw
        else if not idxv === idxw then ( if idxv < idxw then ret = symbol > else ret = symbol < )
        else if not oiMapv.targWidth === oiMapw.targWidth then ( if oiMapv.targWidth < oiMapw.targWidth then ret = symbol > else ret = symbol < )
        else if not oiMapv.img === oiMapw.img then ( if oiMapv.img < oiMapw.img then ret = symbol > else ret = symbol < )
        else ret = symbol ==
    )
    else error "invalid monomial order";

    -- Store the comparison
    compCache#(keyv, monv, keyw, monw, ord) = ret
)

-- Addition method for VectorInWidth
VectorInWidth + VectorInWidth := (v, w) -> (
    if not class v === class w then error("cannot add " | net v | " and " | net w);
    if isZero v then return w else if isZero w then return v;

    cls := class v;
    K := getBasisKeys(cls.freeOIMod, cls.wid);
    A := for key in K list key => v#key + w#key;

    makeVectorInWidth(cls, A)
)

-- Module multiplication method for VectorInWidth
RingElement * VectorInWidth := (r, v) -> (
    clsv := class v;
    fmod := clsv.freeOIMod;
    wid := clsv.wid;

    if not class r === getAlgebraInWidth(fmod.polyOIAlg, wid) then error("cannot multiply " | net r | " and " | net v);
    if isZero v then return v;

    K := getBasisKeys(fmod, wid);
    A := for key in K list key => r * v#key;

    makeVectorInWidth(clsv, A)
)

-- Number multiplication method for VectorInWidth
Number * VectorInWidth := (n, v) -> (
    cls := class v;
    n_(getAlgebraInWidth(cls.freeOIMod.polyOIAlg, cls.wid)) * v
)

-- Negative method for VectorInWidth
- VectorInWidth := v -> (-1) * v

-- Subtraction method for VectorInWidth
VectorInWidth - VectorInWidth := (v, w) -> v + -w

-- Get the degree of a VectorInWidth
degree VectorInWidth := v -> (
    if isZero v then return 0;

    lt := keyedLeadTerm v;
    elt := lt.vec#(lt.key);
    degElt := (degree elt)#0;

    basisIdx := lt.key#1;
    degBasisElt := -(class v).freeOIMod.degShifts#(basisIdx - 1);

    degElt + degBasisElt
)

-- Check if a VectorInWidth is homogeneous
isHomogeneous VectorInWidth := v -> (
    if isZero v then return true;

    #set apply(terms v, degree) === 1
)

-- Make a VectorInWidth monic
-- Args: v = VectorInWidth
-- Comment: assumes v is nonzero
makeMonic := v -> (1 / leadCoefficient v) * v

-- Display a VectorInWidth with terms in order
net VectorInWidth := v -> (
    if isZero v then return net 0;
    
    fmod := (class v).freeOIMod;
    sorted := reverse sort keyedTerms v;

    firstTerm := sorted#0;
    N := net firstTerm.vec#(firstTerm.key) | net fmod.basisSym_(toString (firstTerm.key#0).targWidth, toString (firstTerm.key#0).img, toString firstTerm.key#1);
    
    for i from 1 to #sorted - 1 do (
        term := sorted#i;
        elt := term.vec#(term.key);
        coeff := leadCoefficient elt;
        basisNet := net fmod.basisSym_(toString (term.key#0).targWidth, toString (term.key#0).img, toString term.key#1);

        N = N | if coeff > 0 then " + " | net elt | basisNet else " - " | net(-elt) | basisNet
    );

    N
)

-- Should be of the form {freeOIMod => FreeOIModule, oiMap => OIMap, img => HashTable}
InducedModuleMap = new Type of HashTable

-- Get the module map induced by an OI-map
-- Args: F = FreeOIModule, f = OIMap
getInducedModuleMap := (F, f) -> (
    -- Return the map if it already exists
    if F.maps#?(F, f) then return F.maps#(F, f);

    -- Generate the basis element assignments
    m := #f.img;
    K := getBasisKeys(F, m);
    H := hashTable for key in K list key => (f key#0, key#1);

    -- Store the map
    F.maps#(F, f) = new InducedModuleMap from {freeOIMod => F, oiMap => f, img => H}
)

-- Apply an InducedModuleMap to a VectorInWidth
-- Comment: expects v to belong to the domain of f
InducedModuleMap VectorInWidth := (f, v) -> (
    fmod := f.freeOIMod;
    targWidth := f.oiMap.targWidth;
    targMod := getModuleInWidth(fmod, targWidth);

    -- Handle the zero vector
    if isZero v then return makeZero targMod;

    algMap := getInducedAlgebraMap(fmod.polyOIAlg, f.oiMap);

    sum for single in keyedSingles v list makeSingle(targMod, f.img#(single.key), algMap single.vec#(single.key))
)

-- Should be of the form {srcMod => FreeOIModule, targMod => FreeOIModule, genImages => List}
FreeOIModuleMap = new Type of HashTable

describe FreeOIModuleMap := f -> "Source: " | toString f.srcMod | " Target: " | toString f.targMod || "Basis element images: " | net f.genImages

net FreeOIModuleMap := f -> "Source: " | toString f.srcMod | " Target: " | toString f.targMod

image FreeOIModuleMap := f -> f.genImages

-- Check if a FreeOIModuleMap is zero
isZero FreeOIModuleMap := f -> isZero f.srcMod or isZero f.targMod or set apply(f.genImages, isZero) === set {true}

-- Apply a FreeOIModuleMap to a VectorInWidth
-- Comment: expects v to belong to the domain of f
FreeOIModuleMap VectorInWidth := (f, v) -> (
    -- Handle the zero vector or zero map
    if isZero f or isZero v then return makeZero getModuleInWidth(f.targMod, getWidth v);

    sum for single in keyedSingles v list (
        elt := single.vec#(single.key);
        oiMap := single.key#0;
        basisIdx := single.key#1;
        modMap := getInducedModuleMap(f.targMod, oiMap);
        elt * modMap f.genImages#(basisIdx - 1)
    )
)

-- Check if a FreeOIModuleMap is a graded map
isHomogeneous FreeOIModuleMap := f -> (
    if isZero f then return true;

    for elt in f.genImages do if not isHomogeneous elt then return false;

    -- -f.srcMod.degShifts === apply(f.genImages, degree)
    for i to #f.genImages - 1 do if not (isZero(f.genImages#i) or -f.srcMod.degShifts#i === degree(f.genImages#i)) then return false;

    true
)

-- Compute the n-orbit of a List of VectorInWidth objects
oiOrbit = method(TypicalValue => List)
oiOrbit(List, ZZ) := (L, n) -> (
    if n < 0 then error "expected a nonnegative integer";

    unique flatten for elt in L list for oimap in getOIMaps(getWidth elt, n) list (getInducedModuleMap(getFreeOIModule elt, oimap)) elt
)

-- Get the Schreyer map of a FreeOIModule object, if it exists
getSchreyerMap = method(TypicalValue => FreeOIModuleMap)
getSchreyerMap FreeOIModule := F -> if not instance(F.monOrder, List) then error "invalid monomial order" else (
    new FreeOIModuleMap from {srcMod => F, targMod => getFreeOIModule(F.monOrder#0), genImages => F.monOrder}
)

-- Division function for KeyedVectorInWidth terms
-- Args: v = KeyedVectorInWidth, w = KeyedVectorInWidth
-- Comment: tries to divide v by w and returns a HashTable of the form {quo => RingElement, oiMap => OIMap}
termDiv := (v, w) -> (
    clsv := class v.vec;
    clsw := class w.vec;
    fmod := clsv.freeOIMod;

    if isZero v.vec then return hashTable {quo => 0_(getAlgebraInWidth(fmod.polyOIAlg, clsv.wid)), oiMap => null};

    widv := clsv.wid;
    widw := clsw.wid;
    keyv := v.key;
    keyw := w.key;

    if widv === widw then (
        if keyv === keyw and zero(v.vec#keyv % w.vec#keyw) then
            return hashTable {quo => v.vec#keyv // w.vec#keyw, oiMap => (getOIMaps(widw, widv))#0}
    )
    else for oiMap0 in getOIMaps(widw, widv) do (
        modMap := getInducedModuleMap(fmod, oiMap0);
        imgw := modMap w.vec;
        keyimgw := (oiMap0 w.key#0, w.key#1);

        if keyv === keyimgw and zero(v.vec#keyv % imgw#keyimgw) then
            return hashTable {quo => v.vec#keyv // imgw#keyimgw, oiMap => oiMap0}
    );

    return hashTable {quo => 0_(getAlgebraInWidth(fmod.polyOIAlg, clsv.wid)), oiMap => null}
)

-- Divide a VectorInWidth by a List of VectorInWidth objects
-- Args: v = VectorInWidth, L = List
-- Comment: returns a HashTable of the form {quo => VectorInWidth, rem => VectorInWidth, divTuples => List}
-- Comment: expects L to consist of nonzero elements
polyDiv := (v, L) -> (
    if isZero v then return new HashTable from {quo => v, rem => v, divTuples => {}};

    cls := class v;
    quo0 := makeZero cls;
    rem0 := v;

    done := false;
    divTuples0 := while not done list (
        divTuple := null;
        for i to #L - 1 do (
            elt := L#i;
            div := termDiv(keyedLeadTerm rem0, keyedLeadTerm elt);
            if zero div.quo then continue;

            modMap := getInducedModuleMap(cls.freeOIMod, div.oiMap);
            q := modMap elt;
            quo0 = quo0 + div.quo * q;
            rem0 = rem0 - div.quo * q;

            divTuple = (div, i);
            break
        );

        if divTuple === null then break;
        if isZero rem0 then done = true;

        divTuple
    );

    new HashTable from {quo => quo0, rem => rem0, divTuples => divTuples0}
)

-- Compute the normal form of a VectorInWidth modulo a List of VectorInWidth objects
-- Args: v = VectorInWidth, L = List
-- Comment: expects L to consist of nonzero elements
oiNormalForm := (v, L) -> (
    if isZero v then return v;

    cls := class v;
    rem := makeZero cls;

    while not isZero v do (
        divisionOccurred := false;

        for elt in L do (
            div := termDiv(keyedLeadTerm v, keyedLeadTerm elt);
            if zero div.quo then continue;

            modMap := getInducedModuleMap(cls.freeOIMod, div.oiMap);
            v = v - div.quo * modMap elt;

            divisionOccurred = true;
            break
        );

        if not divisionOccurred then (
            rem = rem + leadTerm v;
            v = v - leadTerm v
        )
    );

    rem
)

-- Compute the S-polynomial of two VectorInWidth objects
-- Args: v = VectorInWidth, w = VectorInWidth
-- Comment: expects class v === class w
SPolynomial := (v, w) -> (
    cls := class v;

    if isZero v or isZero w then return makeZero cls;

    ltv := keyedLeadTerm v;
    ltw := keyedLeadTerm w;
    ltvelt := ltv.vec#(ltv.key);
    ltwelt := ltw.vec#(ltw.key);
    lcmlmvw := lcm(leadMonomial ltvelt, leadMonomial ltwelt);

    (lcmlmvw // ltvelt) * v - (lcmlmvw // ltwelt) * w
)

-- Should be of the form {map0 => OIMap, vec0 => VectorInWidth, im0 => VectorInWidth, map1 => OIMap, vec1 => VectorInWidth, im1 => VectorInWidth}
OIPair = new Type of HashTable

-- Comparison method for OIPair objects
OIPair ? OIPair := (p, q) -> getWidth p.im0 ? getWidth q.im0

-- Compute the critical pairs for a List of VectorInWidth objects
-- Args: L = List, V = Boolean
-- Comment: map0 and map1 are the OI-maps applied to vec0 and vec1 to make im0 and im1
oiPairs := (L, V) -> sort unique flatten flatten flatten flatten for fIdx to #L - 1 list (
    f := L#fIdx;
    ltf := keyedLeadTerm f;
    for gIdx from fIdx to #L - 1 list (
        g := L#gIdx;
        ltg := keyedLeadTerm g;
        clsf := class f;
        clsg := class g;

        if not ltf.key#1 === ltg.key#1 then continue; -- These will have lcm zero

        widf := clsf.wid;
        widg := clsg.wid;
        searchMin := max(widf, widg);
        searchMax := widf + widg;
        for i to searchMax - searchMin list (
            k := searchMax - i;
            oiMapsFromf := getOIMaps(widf, k);

            -- Given an OI-map from f, we construct the corresponding OI-maps from g
            for oiMapFromf in oiMapsFromf list (
                base := set(1..k) - set oiMapFromf.img; -- Get the starting set

                -- Add back in the i-element subsets of oiMapFromf.img and make the pairs
                for subset in subsets(oiMapFromf.img, i) list (
                    oiMapFromg := makeOIMap(k, sort toList(base + set subset));

                    if not oiMapFromf ltf.key#0 === oiMapFromg ltg.key#0 then continue; -- These will have lcm zero
                    if fIdx === gIdx and oiMapFromf === oiMapFromg then continue; -- These will yield trivial S-polynomials and syzygies

                    if V then print("Found suitable OI-maps " | net oiMapFromf | " and " | net oiMapFromg);

                    modMapFromf := getInducedModuleMap(clsf.freeOIMod, oiMapFromf);
                    modMapFromg := getInducedModuleMap(clsg.freeOIMod, oiMapFromg);

                    new OIPair from {map0 => oiMapFromf, idx0 => fIdx, im0 => modMapFromf f, map1 => oiMapFromg, idx1 => gIdx, im1 => modMapFromg g}
                )
            )
        )
    )
)

-- Cache for storing OI-Groebner bases
oiGBCache = new MutableHashTable

-- Compute an OI-Groebner basis for a List of VectorInWidth objects
oiGB = method(TypicalValue => List, Options => {Verbose => false, Strategy => Minimize})
oiGB List := opts -> L -> (
    if not (opts.Strategy === FastNonminimal or opts.Strategy === Minimize or opts.Strategy === Reduce) then
        error "expected Strategy => FastNonminimal or Strategy => Minimize or Strategy => Reduce";
    
    if opts.Verbose then print "Computing OIGB...";

    -- Return the GB if it already exists
    if oiGBCache#?(L, opts.Strategy) then return oiGBCache#(L, opts.Strategy);

    -- Throw out any repeated or zero elements
    ret := unique for elt in L list if isZero elt then continue else elt;
    if #ret === 0 then error "expected a nonempty list of nonzero elements";

    encountered := new List;
    totalAdded := 0;

    -- Enter the main loop: terminates by an equivariant Noetherianity argument
    while true do (
        oipairs := oiPairs(ret, opts.Verbose);

        remToAdd := null;
        for i to #oipairs - 1 do (
            s := SPolynomial((oipairs#i).im0, (oipairs#i).im1);
            if isZero s then continue;

            if member(s, encountered) then continue -- Skip S-Polynomials that have already appeared
            else encountered = append(encountered, s);

            if opts.Verbose then print("On critical pair " | toString(i + 1) | " out of " | toString(#oipairs));

            rem := (polyDiv(s, ret)).rem;
            if not isZero rem and not member(rem, ret) then (
                if opts.Verbose then (
                    print("Found nonzero remainder: " | net rem);
                    totalAdded = totalAdded + 1;
                    print("Elements added total: " | net totalAdded);
                );

                remToAdd = rem;
                break
            )
        );

        if remToAdd === null then break;
        ret = append(ret, remToAdd)
    );

    -- Minimize the basis
    if opts.Strategy === Minimize then (
        if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n";
        ret = minimizeOIGB(ret, Verbose => opts.Verbose)
    );

    -- Reduce the basis
    if opts.Strategy === Reduce then (
        if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n";
        ret = reduceOIGB(ret, Verbose => opts.Verbose)
    );

    -- Store the GB
    oiGBCache#(L, opts.Strategy) = ret
)

-- Minimize an OI-Groebner basis in the sense of monic and lt(p) not in <lt(G - {p})> for all p in G
minimizeOIGB = method(TypicalValue => List, Options => {Verbose => false})
minimizeOIGB List := opts -> G -> (
    if opts.Verbose then print "Computing minimal OIGB...";

    -- Throw out any repeated or zero elements
    G = unique for elt in G list if isZero elt then continue else elt;
    if #G === 0 then error "expected a nonempty list of nonzero elements";

    nonRedundant := new List;
    currentBasis := unique apply(G, makeMonic); -- unique is used again because collisions may happen after makeMonic

    if #currentBasis === 1 then return currentBasis;

    while true do (
        redundantFound := false;

        for p in currentBasis do (
            if member(p, nonRedundant) then continue; -- Skip elements already verified to be nonredundant

            minusp := toList((set currentBasis) - set {p});
            ltp := keyedLeadTerm p;
            for elt in minusp do if not zero (termDiv(ltp, keyedLeadTerm elt)).quo then (
                if opts.Verbose then print("Found redundant element: " | net p);
                redundantFound = true;
                currentBasis = minusp;
                break
            );

            if redundantFound then break;
            nonRedundant = append(nonRedundant, p);
        );

        if not redundantFound then break
    );

    currentBasis
)

-- Remove a single element from a List
-- Args: L = List, i = ZZ
sdrop := (L, i) -> drop(L, {i, i})

-- Reduce an OI-Groebner basis in the sense of monic and no term of any element is OI-divisible by the lead term of any other
reduceOIGB = method(TypicalValue => List, Options => {Verbose => false})
reduceOIGB List := opts -> G -> (
    minG := minimizeOIGB(G, Verbose => opts.Verbose);

    if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n\nComputing reduced OIGB...";

    if #minG === 1 then return minG;

    -- Reduce the basis
    newG := new MutableList from minG;
    for i to #newG - 1 do (
        if opts.Verbose then print("Reducing element " | toString(i + 1) | " of " | toString(#newG));
        dropped := flatten sdrop(toList newG, i);
        red := oiNormalForm(newG#i, dropped);
        newG#i = if member(red, dropped) then {} else red
    );

    flatten toList newG
)

-- Check if a List is an OI-Groebner basis
isOIGB = method(TypicalValue => Boolean, Options => {Verbose => false})
isOIGB List := opts -> L -> (
    if opts.Verbose then print "Checking Buchberger's Criterion...";

    -- Throw out any repeated or zero elements
    L = unique for elt in L list if isZero elt then continue else elt;
    if #L === 0 then error "expected a nonempty list of nonzero elements";

    encountered := new List;
    oipairs := oiPairs(L, opts.Verbose);
    for i to #oipairs - 1 do (
        s := SPolynomial((oipairs#i).im0, (oipairs#i).im1);
        if isZero s then continue;

        if member(s, encountered) then continue -- Skip S-Polynomials that have already appeared
        else encountered = append(encountered, s);

        if opts.Verbose then print("On critical pair " | toString(i + 1) | " out of " | toString(#oipairs));
    
        rem := (polyDiv(s, L)).rem;
        if not isZero rem then (
            if opts.Verbose then print("Found nonzero remainder: " | net rem);
            return false
        )
    );

    true
)

-- Cache for storing Groebner bases computed with oiSyz
oiSyzCache = new MutableHashTable

-- Compute an OI-Groebner basis for the syzygy module of a List of VectorInWidth objects
oiSyz = method(TypicalValue => List, Options => {Verbose => false, Strategy => Minimize})
oiSyz(List, Symbol) := opts -> (L, d) -> (
    if not (opts.Strategy === FastNonminimal or opts.Strategy === Minimize or opts.Strategy === Reduce) then
        error "expected Strategy => FastNonminimal or Strategy => Minimize or Strategy => Reduce";
    
    if opts.Verbose then print "Computing syzygies...";
    
    -- Return the GB if it already exists
    if oiSyzCache#?(L, d, opts.Strategy) then return oiSyzCache#(L, d, opts.Strategy);

    -- Throw out any repeated or zero elements
    L = unique for elt in L list if isZero elt then continue else elt;
    if #L === 0 then error "expected a nonempty list of nonzero elements";

    fmod := getFreeOIModule L#0;
    shifts := for elt in L list -degree elt;
    widths := for elt in L list getWidth elt;
    G := makeFreeOIModule(d, widths, fmod.polyOIAlg, DegreeShifts => shifts, OIMonomialOrder => L);

    oipairs := oiPairs(L, opts.Verbose);
    if opts.Verbose then print "Iterating through critical pairs...";
    i := 0;
    ret := for pair in oipairs list (        
        if opts.Verbose then (
            print("On critical pair " | toString(i + 1) | " out of " | toString(#oipairs));
            print("Pair: (" | net pair.im0 | ", " | net pair.im1 | ")");
            i = i + 1
        );

        ltf := keyedLeadTerm pair.im0;
        ltg := keyedLeadTerm pair.im1;
        ltfelt := ltf.vec#(ltf.key);
        ltgelt := ltg.vec#(ltg.key);
        lcmlmfg := lcm(leadMonomial ltfelt, leadMonomial ltgelt);
        s := SPolynomial(pair.im0, pair.im1);
        M := getModuleInWidth(G, getWidth s);
        thingToSubtract := makeZero M;

        -- Calculate the stuff to subtract off
        if not isZero s then for tuple in (polyDiv(s, L)).divTuples do
            thingToSubtract = thingToSubtract + makeSingle(M, ((tuple#0).oiMap, 1 + tuple#1), (tuple#0).quo);
        
        -- Make the syzygy
        sing1 := makeSingle(M, (pair.map0, 1 + pair.idx0), lcmlmfg // ltfelt);
        sing2 := makeSingle(M, (pair.map1, 1 + pair.idx1), lcmlmfg // ltgelt);
        syzygy := sing1 - sing2 - thingToSubtract;

        if opts.Verbose then print("Generated syzygy: " | net syzygy);

        syzygy
    );

    -- Throw out any repeated or zero elements
    ret = unique for elt in ret list if isZero elt then continue else elt;

    -- Minimize the basis
    if #ret > 0 and opts.Strategy === Minimize then (
        if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n";
        ret = minimizeOIGB(ret, Verbose => opts.Verbose)
    );

    -- Reduce the basis
    if #ret > 0 and opts.Strategy === Reduce then (
        if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n";
        ret = reduceOIGB(ret, Verbose => opts.Verbose)
    );

    -- Store the GB
    oiSyzCache#(L, d, opts.Strategy) = ret
)

-- Cache for storing OI-resolutions
oiResCache = new MutableHashTable

-- Should be of the form {dd => List, modules => List}
OIResolution = new Type of HashTable

net OIResolution := C -> (
    N := "0: " | toString C.modules#0;
    for i from 1 to #C.modules - 1 do N = N || toString i | ": " | toString C.modules#i;
    N
)

describe OIResolution := C -> (
    N := "0: Module: " | net C.modules#0 || "Differential: " | net C.dd#0;
    for i from 1 to #C.modules - 1 do N = N || toString i | ": Module: " | net C.modules#i || "Differential: " | net C.dd#i;
    N
)

describeFull = method(TypicalValue => Net)
describeFull OIResolution := C -> (
    N := "0: Module: " | net C.modules#0 || "Differential: " | net C.dd#0;
    for i from 1 to #C.modules - 1 do N = N || toString i | ": Module: " | net C.modules#i || "Differential: " | describe C.dd#i;
    N
)

ranks = method(TypicalValue => Net)
ranks OIResolution := C -> (
    N := "0: rank " | toString getRank C.modules#0;
    for i from 1 to #C.modules - 1 do N = N || toString i | ": rank " | toString getRank C.modules#i;
    N
)

restrictedRanks = method(TypicalValue => Net)
restrictedRanks(OIResolution, ZZ) := (C, w) -> (
    N := "0: rank " | #degrees ((C_0)_w).rawMod;
    for i from 1 to #C.modules - 1 do N = N || toString i | ": rank " | #degrees ((C_i)_w).rawMod;
    N
)

OIResolution _ ZZ := (C, n) -> C.modules#n

-- Compute an OI-resolution of length n for the OI-module generated by L
oiRes = method(TypicalValue => OIResolution, Options => {Verbose => false, Strategy => Minimize, TopNonminimal => false})
oiRes(List, ZZ) := opts -> (L, n) -> (
    if not (opts.Verbose === true or opts.Verbose === false) then error "expected Verbose => true or Verbose => false";
    if not (opts.TopNonminimal === true or opts.TopNonminimal === false) then error "expected TopNonminimal => true or TopNonminimal => false";
    if not (opts.Strategy === FastNonminimal or opts.Strategy === Minimize or opts.Strategy === Reduce) then
        error "expected Strategy => FastNonminimal or Strategy => Minimize or Strategy => Reduce";
    
    if n < 0 then error "expected a nonnegative integer";

    if opts.Verbose then print "Computing OI-resolution";

    -- Return the resolution if it already exists
    if oiResCache#?(L, n, opts.Strategy, opts.TopNonminimal) then return oiResCache#(L, n, opts.Strategy, opts.TopNonminimal);

    strat := opts.Strategy;
    if n === 0 and opts.TopNonminimal then strat = FastNonminimal;
    oigb := oiGB(L, Verbose => opts.Verbose, Strategy => strat);
    currentGB := oigb;

    ddMut := new MutableList;
    modulesMut := new MutableList;
    groundFreeOIMod := getFreeOIModule currentGB#0;
    e := groundFreeOIMod.basisSym;
    currentSymbol := getSymbol concatenate(e, "0");
    count := 0;

    if n > 0 then for i to n - 1 do (
            if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n";

            if i === n - 1 and opts.TopNonminimal then strat = FastNonminimal;
            syzGens := oiSyz(currentGB, currentSymbol, Verbose => opts.Verbose, Strategy => strat);

            if #syzGens === 0 then break;
            count = count + 1;

            targFreeOIMod := getFreeOIModule currentGB#0;
            srcFreeOIMod := getFreeOIModule syzGens#0;

            modulesMut#i = srcFreeOIMod;
            ddMut#i = new FreeOIModuleMap from {srcMod => srcFreeOIMod, targMod => targFreeOIMod, genImages => currentGB};

            currentGB = syzGens;
            currentSymbol = getSymbol concatenate(e, toString count)
    );

    -- Append the last term in the sequence
    shifts := for elt in currentGB list -degree elt;
    widths := for elt in currentGB list getWidth elt;
    modulesMut#count = makeFreeOIModule(currentSymbol, widths, groundFreeOIMod.polyOIAlg, DegreeShifts => shifts, OIMonomialOrder => currentGB);
    ddMut#count = new FreeOIModuleMap from {srcMod => modulesMut#count, targMod => if count === 0 then groundFreeOIMod else modulesMut#(count - 1), genImages => currentGB};

    -- Cap the sequence with zeros
    for i from count + 1 to n do (
        currentSymbol = getSymbol concatenate(e, toString i);
        modulesMut#i = makeFreeOIModule(currentSymbol, {}, groundFreeOIMod.polyOIAlg);
        ddMut#i = new FreeOIModuleMap from {srcMod => modulesMut#i, targMod => modulesMut#(i - 1), genImages => {}}
    );

    -- Minimize the resolution
    if #ddMut > 1 and not isZero ddMut#1 then (
        if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n\nMinimizing resolution...";

        done := false;
        while not done do (
            done = true;

            -- Look for units on identity basis elements
            unitFound := false;
            local data;
            for i from 1 to #ddMut - 1 do (
                ddMap := ddMut#i;
                if isZero ddMap then continue;
                
                srcFreeOIMod := ddMap.srcMod;
                targFreeOIMod := ddMap.targMod;
                for j to #ddMap.genImages - 1 do (
                    if isZero ddMap.genImages#j then continue;

                    for single in keyedSingles ddMap.genImages#j do if (single.key#0).img === toList(1..(single.key#0).targWidth) and isUnit single.vec#(single.key) then (
                        unitFound = true;
                        done = false;
                        data = {i, j, single};
                        if opts.Verbose then print("Unit found on term: " | net single.vec);
                        break
                    );

                    if unitFound then break
                );

                if unitFound then break
            );

            -- Prune the sequence
            if unitFound then (
                if opts.Verbose then print "Pruning...";

                unitSingle := data#2;
                targBasisPos := unitSingle.key#1 - 1;
                srcBasisPos := data#1;
                ddMap := ddMut#(data#0);
                srcFreeOIMod := ddMap.srcMod;
                targFreeOIMod := ddMap.targMod;

                -- Make the new free OI-modules
                newSrcWidths := sdrop(srcFreeOIMod.genWidths, srcBasisPos);
                newSrcShifts := sdrop(srcFreeOIMod.degShifts, srcBasisPos);
                newTargWidths := sdrop(targFreeOIMod.genWidths, targBasisPos);
                newTargShifts := sdrop(targFreeOIMod.degShifts, targBasisPos);
                newSrcFreeOIMod := makeFreeOIModule(srcFreeOIMod.basisSym, newSrcWidths, srcFreeOIMod.polyOIAlg, DegreeShifts => newSrcShifts);
                newTargFreeOIMod := makeFreeOIModule(targFreeOIMod.basisSym, newTargWidths, targFreeOIMod.polyOIAlg, DegreeShifts => newTargShifts);

                -- Compute the new differential
                newGenImages := for i to #srcFreeOIMod.genWidths - 1 list (
                    if i === srcBasisPos then continue;

                    -- Calculate the stuff to subtract off
                    thingToSubtract := makeZero getModuleInWidth(srcFreeOIMod, srcFreeOIMod.genWidths#i);
                    for single in keyedSingles ddMap.genImages#i do (
                        if not single.key#1 === targBasisPos + 1 then continue;

                        modMap := getInducedModuleMap(srcFreeOIMod, single.key#0);
                        basisElt := getBasisElement(srcFreeOIMod, srcBasisPos);
                        thingToSubtract = thingToSubtract + single.vec#(single.key) * modMap basisElt
                    );

                    -- Calculate the new image
                    basisElt := getBasisElement(srcFreeOIMod, i);
                    newGenImage0 := ddMap(basisElt - lift(1 // unitSingle.vec#(unitSingle.key), srcFreeOIMod.polyOIAlg.baseField) * thingToSubtract);
                    M := getModuleInWidth(newTargFreeOIMod, getWidth newGenImage0);
                    newGenImage := makeZero M;
                    for newSingle in keyedSingles newGenImage0 do (
                        idx := newSingle.key#1;
                        if idx > targBasisPos + 1 then idx = idx - 1; -- Relabel
                        newGenImage = newGenImage + makeSingle(M, (newSingle.key#0, idx), newSingle.vec#(newSingle.key))
                    );

                    newGenImage
                );

                ddMut#(data#0) = new FreeOIModuleMap from {srcMod => newSrcFreeOIMod, targMod => newTargFreeOIMod, genImages => newGenImages};
                modulesMut#(data#0) = newSrcFreeOIMod;
                modulesMut#(data#0 - 1) = newTargFreeOIMod;

                -- Adjust the map to the right
                ddMap = ddMut#(data#0 - 1);
                ddMut#(data#0 - 1) = new FreeOIModuleMap from {srcMod => newTargFreeOIMod, targMod => ddMap.targMod, genImages => sdrop(ddMap.genImages, targBasisPos)}; -- Restriction

                -- Adjust the map to the left
                if data#0 < #ddMut - 1 then (
                    ddMap = ddMut#(data#0 + 1);
                    newGenImages = new MutableList;

                    for i to #ddMap.genImages - 1 do (
                        M := getModuleInWidth(newSrcFreeOIMod, getWidth ddMap.genImages#i);
                        newGenImage := makeZero M;
                        for single in keyedSingles ddMap.genImages#i do (
                            idx := single.key#1;
                            if idx === srcBasisPos + 1 then continue; -- Projection
                            if idx > srcBasisPos + 1 then idx = idx - 1; -- Relabel
                            newGenImage = newGenImage + makeSingle(M, (single.key#0, idx), single.vec#(single.key))
                        );

                        newGenImages#i = newGenImage
                    );

                    ddMut#(data#0 + 1) = new FreeOIModuleMap from {srcMod => ddMap.srcMod, targMod => newSrcFreeOIMod, genImages => new List from newGenImages}
                )
            )
        )
    );

    -- Store the resolution
    oiResCache#(L, n, opts.Strategy, opts.TopNonminimal) = new OIResolution from {dd => new List from ddMut, modules => new List from modulesMut}
)

-- Verify that an OIResolution is a complex
isComplex = method(TypicalValue => Boolean, Options => {Verbose => false})
isComplex OIResolution := opts -> C -> (
    if #C.dd < 2 then error "expected a sequence with at least two maps";

    -- Check if the maps compose to zero
    for i from 1 to #C.dd - 1 do (
        modMap0 := C.dd#(i - 1);
        modMap1 := C.dd#i;
        if isZero modMap0 or isZero modMap1 then continue;

        for basisElt in getBasisElements modMap1.srcMod do (
            result := modMap0 modMap1 basisElt;

            if opts.Verbose then print(net basisElt | " maps to " | net result);
            
            if not isZero result then (
                if opts.Verbose then print("Found nonzero image: " | net result);
                return false
            )
        )
    );

    true
)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- DOCUMENTATION ---------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

beginDocumentation()

doc ///
    Key
        OIGroebnerBases
    Headline
        OI-modules over Noetherian polynomial OI-algebras
    Description
        Text
            {\em OIGroebnerBases} is a package for Gröbner bases, syzygies and free resolutions for submodules of free OI-modules over Noetherian polynomial OI-algebras. For an introduction to the theory of OI-modules, see [3].

            Given a Noetherian polynomial OI-algebra $\mathbf{P} := (\mathbf{X}^{\text{OI},1})^{\otimes c}$ for some integer $c > 0$, one can consider free OI-modules $\mathbf{F} := \bigoplus_{i=1}^s\mathbf{F}^{\text{OI}, d_i}$ over $\mathbf{P}$ for integers $d_i\geq 0$.
            
            Gröbner bases for submodules of $\mathbf{F}$ were introduced in [3]. Free resolutions and homological aspects of submodules have been studied in [2,3]. Using the methods of [1], Gröbner bases, syzygy modules, and free resolutions for submodules can be computed with @TO oiGB@, @TO oiSyz@ and @TO oiRes@ respectively.

            {\em References:}

            [1] M. Morrow and U. Nagel, {\it Computing Gröbner Bases and Free Resolutions of OI-Modules}, Preprint, arXiv:2303.06725, 2023.

            [2] N. Fieldsteel and U. Nagel, {\it Minimal and cellular free resolutions over polynomial OI-algebras}, Preprint, arXiv:2105.08603, 2021.

            [3] U. Nagel and T. Römer, {\it FI- and OI-modules with varying coefficients}, J. Algebra 535 (2019), 286-322.
    Subnodes
        :Polynomial OI-algebras
        PolynomialOIAlgebra
        makePolynomialOIAlgebra
        VariableOrder
        ColUpRowUp
        ColUpRowDown
        ColDownRowUp
        ColDownRowDown
        RowUpColUp
        RowUpColDown
        RowDownColUp
        RowDownColDown
        (net,PolynomialOIAlgebra)
        (toString,PolynomialOIAlgebra)
        (symbol _,PolynomialOIAlgebra,ZZ)
        :Free OI-modules
        FreeOIModule
        FreeOIModuleMap
        ModuleInWidth
        VectorInWidth
        makeFreeOIModule
        DegreeShifts
        OIMonomialOrder
        installGeneratorsInWidth
        isZero
        getBasisElements
        getFreeOIModule
        getSchreyerMap
        getWidth
        getRank
        oiOrbit
        (isZero,FreeOIModuleMap)
        (isZero,VectorInWidth)
        (symbol SPACE,FreeOIModuleMap,VectorInWidth)
        (net,FreeOIModule)
        (net,FreeOIModuleMap)
        (net,ModuleInWidth)
        (net,VectorInWidth)
        (image,FreeOIModuleMap)
        (toString,FreeOIModule)
        (symbol _,FreeOIModule,ZZ)
        (degree,VectorInWidth)
        (use,ModuleInWidth)
        (terms,VectorInWidth)
        (leadTerm,VectorInWidth)
        (leadMonomial,VectorInWidth)
        (leadCoefficient,VectorInWidth)
        (symbol *,Number,VectorInWidth)
        (symbol +,VectorInWidth,VectorInWidth)
        (symbol *,RingElement,VectorInWidth)
        (symbol -,VectorInWidth)
        (symbol -,VectorInWidth,VectorInWidth)
        (describe,FreeOIModuleMap)
        (isHomogeneous,FreeOIModuleMap)
        (isHomogeneous,VectorInWidth)
        :OI-Gröbner bases
        oiGB
        minimizeOIGB
        reduceOIGB
        isOIGB
        :OI-syzygies
        oiSyz
        :OI-resolutions
        OIResolution
        oiRes
        ranks
        restrictedRanks
        TopNonminimal
        isComplex
        describeFull
        (describe,OIResolution)
        (net,OIResolution)
        (symbol _,OIResolution,ZZ)
///

doc ///
    Key
        PolynomialOIAlgebra
    Headline
        the class of all Noetherian polynomial OI-algebras over a field
    Description
        Text
            This type implements OI-algebras of the form $(\mathbf{X}^{\text{OI},1})^{\otimes c}$ for some integer $c>0$. To make a @TT "PolynomialOIAlgebra"@ object, use @TO makePolynomialOIAlgebra@. Each @TT "PolynomialOIAlgebra"@ object is equipped with a variable order; see @TO VariableOrder@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ, VariableOrder => RowDownColUp)
///

doc ///
    Key
        makePolynomialOIAlgebra
        (makePolynomialOIAlgebra,ZZ,Symbol,Ring)
        [makePolynomialOIAlgebra,VariableOrder]
    Headline
        make a PolynomialOIAlgebra object
    Usage
        makePolynomialOIAlgebra(c,x,K)
    Inputs
        c:ZZ
        x:Symbol
        K:Ring
    Outputs
        :PolynomialOIAlgebra
    Description
        Text
            Makes a polynomial OI-algebra over the field @TT "K"@ with @TT "c"@ rows of variables in the symbol @TT "x"@. The @TO VariableOrder@ option is used to specify the ordering of the variables.
        Example
            P = makePolynomialOIAlgebra(1, x, QQ)
            Q = makePolynomialOIAlgebra(2, y, QQ, VariableOrder => RowDownColUp)
///

doc ///
    Key
        VariableOrder
    Headline
        variable ordering for polynomial OI-algebras
    Description
        Text
            Used as an optional argument in @TO makePolynomialOIAlgebra@ to specify an ordering on the variables of a polynomial OI-algebra.
        
            Permissible values:
        Code
            UL {
                {TT "VariableOrder => ", TT TO ColUpRowUp},
                {TT "VariableOrder => ", TT TO ColUpRowDown},
                {TT "VariableOrder => ", TT TO ColDownRowUp},
                {TT "VariableOrder => ", TT TO ColDownRowDown},
                {TT "VariableOrder => ", TT TO RowUpColUp},
                {TT "VariableOrder => ", TT TO RowUpColDown},
                {TT "VariableOrder => ", TT TO RowDownColUp},
                {TT "VariableOrder => ", TT TO RowDownColDown}
            }
        Example
            P = makePolynomialOIAlgebra(2, x, QQ, VariableOrder => RowDownColUp)
///

doc ///
    Key
        ColUpRowUp
    Headline
        column up row up variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $j'<j$ or $j'=j$ and $i'<i$.
///

doc ///
    Key
        ColUpRowDown
    Headline
        column up row down variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $j'<j$ or $j'=j$ and $i'>i$.
///

doc ///
    Key
        ColDownRowUp
    Headline
        column down row up variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $j'>j$ or $j'=j$ and $i'<i$.
///

doc ///
    Key
        ColDownRowDown
    Headline
        column down row down variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $j'>j$ or $j'=j$ and $i'>i$.
///

doc ///
    Key
        RowUpColUp
    Headline
        row up column up variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $i'<i$ or $i'=i$ and $j'<j$.
///

doc ///
    Key
        RowUpColDown
    Headline
        row up column down variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $i'<i$ or $i'=i$ and $j'>j$.
///

doc ///
    Key
        RowDownColUp
    Headline
        row up column up variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $i'>i$ or $i'=i$ and $j'<j$.
///

doc ///
    Key
        RowDownColDown
    Headline
        row down column down variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $i'>i$ or $i'=i$ and $j'>j$.
///

doc ///
    Key
        (net,PolynomialOIAlgebra)
    Headline
        display a polynomial OI-algebra
    Usage
        net P
    Inputs
        P:PolynomialOIAlgebra
    Outputs
        :Net
    Description
        Text
            Displays the base field, number of variable rows, variable symbol, and variable order of a @TO PolynomialOIAlgebra@ object.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            net P
///

doc ///
    Key
        (toString,PolynomialOIAlgebra)
    Headline
        display a polynomial OI-algebra in condensed form
    Usage
        toString P
    Inputs
        P:PolynomialOIAlgebra
    Outputs
        :String
    Description
        Text
            Displays the base field, number of variable rows, variable symbol, and variable order of a @TO PolynomialOIAlgebra@ object as a string.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            toString P
///

doc ///
    Key
        (symbol _,PolynomialOIAlgebra,ZZ)
    Headline
        get the $K$-algebra in a specified width of a polynomial OI-algebra
    Usage
        P_n
    Inputs
        P:PolynomialOIAlgebra
        n:ZZ
    Outputs
        :PolynomialRing
    Description
        Text
            Returns the $K$-algebra in width @TT "n"@ of a polynomial OI-algebra.
        Example
            P = makePolynomialOIAlgebra(2, y, QQ);
            P_4
///

doc ///
    Key
        FreeOIModule
    Headline
        the class of all free OI-modules over a polynomial OI-algebra
    Description
        Text
            This type implements free OI-modules over polynomial OI-algebras. To make a @TT "FreeOIModule"@ object, use @TO makeFreeOIModule@. To get the basis elements and rank of a free OI-module, use @TO getBasisElements@ and @TO getRank@ respectively. To install the generators of a component of a free OI-module in a specified width, use @TO installGeneratorsInWidth@.

            Each @TT "FreeOIModule"@ object comes equipped with either the @TO Lex@ monomial order induced by the monomial order on its underlying polynomial OI-algebra, or the Schreyer monomial order induced by another free OI-module; see @TO makeFreeOIModule@ and @TO OIMonomialOrder@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P)
///

doc ///
    Key
        FreeOIModuleMap
    Headline
        the class of all maps between free OI-modules
    Description
        Text
            This type implements morphisms between free OI-modules. Given free OI-modules $\mathbf{F}$ and $\mathbf{G}$ over an OI-algebra $\mathbf{P}$, a $\mathbf{P}$-linear map $\varphi:\mathbf{F}\to\mathbf{G}$ is determined by the images of the basis elements of $\mathbf{F}$.
            
            To evaluate $\varphi$ on an element of $\mathbf{F}$, use @TO (symbol SPACE,FreeOIModuleMap,VectorInWidth)@.
            
            One obtains @TT "FreeOIModuleMap"@ objects through the use of @TO oiRes@, as in the below example.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
            C = oiRes({b}, 2)
            phi = C.dd_1
            G = getBasisElements C_1
            phi G#0
            phi G#1
///

doc ///
    Key
        ModuleInWidth
    Headline
        the class of all modules that appear as a component of a free OI-module
    Description
        Text
            The width $n$ component of a free OI-module is implemented as a @TT "ModuleInWidth"@ object. To obtain a @TT "ModuleInWidth"@ object, one restricts a given free OI-module to a specified width using @TO (symbol _,FreeOIModule,ZZ)@, as seen in the example below.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            F_2
///

doc ///
    Key
        VectorInWidth
    Headline
        the class of all elements of a free OI-module
    Description
        Text
            An element of a free OI-module $\mathbf{F}$ is defined to be an element of $\mathbf{F}_n$ for some integer $n\geq0$. Such an element is implemented as a @TT "VectorInWidth"@ object. One typically makes @TT "VectorInWidth"@ objects by defining a @TO FreeOIModule@ object, calling @TO installGeneratorsInWidth@, and then manipulating the generators; see below for an example.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)
            instance(b, VectorInWidth)
///

doc ///
    Key
        makeFreeOIModule
        (makeFreeOIModule,Symbol,List,PolynomialOIAlgebra)
        [makeFreeOIModule,DegreeShifts]
        [makeFreeOIModule,OIMonomialOrder]
    Headline
        make a FreeOIModule object
    Usage
        makeFreeOIModule(e,L,P)
    Inputs
        e:Symbol
        L:List
        P:PolynomialOIAlgebra
    Outputs
        :FreeOIModule
    Description
        Text
            Makes a free OI-module of the form $\bigoplus_{i=1}^s\mathbf{F}^{\text{OI}, d_i}$ over the @TO PolynomialOIAlgebra@ object @TT "P"@ with $L=\{d_1,\ldots,d_s\}$ and basis symbol @TT "e"@.

            The @TO DegreeShifts@ option is used to specify a shift of grading. This option must be set to either @TT "null"@ for no shifts, or a list of integers describing the desired shifts.

            The @TO OIMonomialOrder@ option must be set to either @TO Lex@, for the lexicographic order, or a list of elements of some free OI-module for the Schreyer order. See below for examples.
        
            @HEADER2 "Example 1: Lex"@
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P, DegreeShifts => {3,2,1})
        Text
            @HEADER2 "Example 2: Schreyer"@
        Example
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2);
            G = makeFreeOIModule(d, {2}, P, DegreeShifts => {-degree b}, OIMonomialOrder => {b})
///

doc ///
    Key
        DegreeShifts
    Headline
        grading shifts for free OI-modules
    Description
        Text
            Used as an optional argument in @TO makeFreeOIModule@ to specify shifts of grading.

            Permissible values:
        Code
            UL {
                {TT "DegreeShifts => ", TT "null", " for no shift of grading"},
                {TT "DegreeShifts => ", TT TO List, " for a shift of grading indicated by a list of integers"}
            }
        Text
            @HEADER2 "Example 1: no shifts"@

            The free OI-module $\mathbf{F}^{\text{OI},1}\oplus\mathbf{F}^{\text{OI},2}$ has its basis elements in degree zero.
        Example
            P = makePolynomialOIAlgebra(1, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P)
            apply(getBasisElements F, degree)
        Text
            @HEADER2 "Example 2: nontrivial shifts"@
            
            The free OI-module $\mathbf{F}^{\text{OI},1}(-3)\oplus\mathbf{F}^{\text{OI},2}(-4)$ has its basis elements in degrees $3$ and $4$.
        Example
            F = makeFreeOIModule(e, {1,2}, P, DegreeShifts => {-3,-4})
            apply(getBasisElements F, degree)
///

doc ///
    Key
        OIMonomialOrder
    Headline
        monomial order option for free OI-modules
    Description
        Text
            Used as an optional argument in @TO makeFreeOIModule@ to specify the desired monomial order.
        
            Permissible values:
        Code
            UL {
                {TT "OIMonomialOrder => ", TT TO Lex, " for the lexicographic order induced by the monomial order of the underlying polynomial OI-algebra; see [1, Example 3.2]"},
                {TT "OIMonomialOrder => ", TT TO List, " for the Schreyer order induced by a list of elements of a free OI-module; see [1, Definition 4.2]"}
            }
        Text
            {\em References:}

            [1] M. Morrow and U. Nagel, {\it Computing Gröbner Bases and Free Resolutions of OI-Modules}, Preprint, arXiv:2303.06725, 2023.            
///

doc ///
    Key
        installGeneratorsInWidth
        (installGeneratorsInWidth,FreeOIModule,ZZ)
    Headline
        install the generators for a component of a free OI-module in a specified width
    Usage
        installGeneratorsInWidth(F, n)
    Inputs
        F:FreeOIModule
        n:ZZ
    Description
        Text
            This method assigns the generators of the width @TT "n"@ component of @TT "F"@ to the appropriate @TO IndexedVariable@ corresponding to the basis symbol of @TT "F"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            e_(2,{1},1)
            e_(2,{1},2)
            e_(2,{2},1)
            e_(2,{2},2)
            x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
///

doc ///
    Key
        getBasisElements
        (getBasisElements,FreeOIModule)
    Headline
        get the basis elements of a free OI-module
    Usage
        getBasisElements F
    Inputs
        F:FreeOIModule
    Outputs
        :List
    Description
        Text
            Returns the basis elements of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            getBasisElements F
///

doc ///
    Key
        getFreeOIModule
        (getFreeOIModule,VectorInWidth)
    Headline
        get the free OI-module of an element
    Usage
        getFreeOIModule f
    Inputs
        f:VectorInWidth
    Outputs
        :FreeOIModule
    Description
        Text
            Returns the free OI-module in which the element @TT "f"@ lives.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installGeneratorsInWidth(F, 4);
            f = x_(2,4)*e_(4,{3},1)+x_(1,3)^2*e_(4,{2,4},3)
            getFreeOIModule f
///

doc ///
    Key
        getSchreyerMap
        (getSchreyerMap,FreeOIModule)
    Headline
        get the Schreyer map of a free OI-module if it exists
    Usage
        getSchreyerMap H
    Inputs
        H:FreeOIModule
    Outputs
        :FreeOIModuleMap
    Description
        Text
            Let $G'$ be a non-empty Gröbner basis for the syzygy module of a finitely generated submodule $\mathbf{M}$ of a free OI-module $\mathbf{F}$ computed using @TO oiSyz@. Let @TT "H"@ be the free OI-module obtained by applying @TO getFreeOIModule@ to any element of $G'$. This method returns the canonical surjective map from @TT "H"@ to $\mathbf{M}$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            G = oiGB {b}
            G' = oiSyz(G, d)
            H = getFreeOIModule G'#0
            getSchreyerMap H
    Caveat
        If $G'$ is empty or if @TT "H"@ does not have a Schreyer order, this method will throw an error.
///

doc ///
    Key
        getWidth
        (getWidth,VectorInWidth)
    Headline
        get the width of an element of a free OI-module
    Usage
        getWidth f
    Inputs
        f:VectorInWidth
    Outputs
        :ZZ
    Description
        Text
            Returns the width of an element of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installGeneratorsInWidth(F, 4);
            f = x_(2,4)*e_(4,{3},1)+x_(1,3)^2*e_(4,{2,4},3)
            getWidth f
///

doc ///
    Key
        getRank
        (getRank,FreeOIModule)
    Headline
        get the rank of a free OI-module
    Usage
        getRank F
    Inputs
        F:FreeOIModule
    Outputs
        :ZZ
    Description
        Text
            Returns the rank of a free OI-module. Recall that the rank of a free OI-module $\bigoplus_{i=1}^s\mathbf{F}^{\text{OI}, d_i}$ is defined to be $s$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            getRank F
///

doc ///
    Key
        oiOrbit
        (oiOrbit,List,ZZ)
    Headline
        compute the n-orbit of a list of elements of a free OI-module
    Usage
        oiOrbit(L, n)
    Inputs
        L:List
        n:ZZ
    Outputs
        :List
    Description
        Text
            Returns the $n$-orbit of the list @TT "L"@ of @TO VectorInWidth@ objects.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)
            oiOrbit({b}, 4)
///

doc ///
    Key
        (symbol SPACE,FreeOIModuleMap,VectorInWidth)
    Headline
        apply a free OI-module map to an element
    Usage
        phi f
    Inputs
        phi:FreeOIModuleMap
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Evaluates @TT "phi"@ at @TT "f"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
            C = oiRes({b}, 2)
            phi = C.dd_1
            G = getBasisElements C_1
            phi G#0
            phi G#1
///

doc ///
    Key
        (net,FreeOIModule)
    Headline
        display a free OI-module
    Usage
        net F
    Inputs
        F:FreeOIModule
    Outputs
        :Net
    Description
        Text
            Displays the basis symbol, basis element widths, degree shifts, underlying polynomial OI-algebra, and monomial order of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            net F
///

doc ///
    Key
        (net,FreeOIModuleMap)
    Headline
        display a free OI-module map source and target
    Usage
        net phi
    Inputs
        phi:FreeOIModuleMap
    Outputs
        :Net
    Description
        Text
            Displays the source module and target module a free OI-module map.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
            C = oiRes({b}, 2);
            phi = C.dd_1;
            net phi
///

doc ///
    Key
        (image,FreeOIModuleMap)
    Headline
        get the basis element images of a free OI-module map
    Usage
        image phi
    Inputs
        phi:FreeOIModuleMap
    Outputs
        :List
    Description
        Text
            Returns a list containing the basis element images of a free OI-module map.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
            C = oiRes({b}, 2);
            phi = C.dd_1;
            image phi
///

doc ///
    Key
        (describe,FreeOIModuleMap)
    Headline
        display a free OI-module map
    Usage
        describe phi
    Inputs
        phi:FreeOIModuleMap
    Outputs
        :Net
    Description
        Text
            Displays the source module, target module, and basis element images of a free OI-module map.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
            C = oiRes({b}, 2);
            phi = C.dd_1;
            describe phi
///

doc ///
    Key
        (net,ModuleInWidth)
    Headline
        display a component of a free OI-module in a specified width
    Usage
        net M
    Inputs
        M:ModuleInWidth
    Outputs
        :Net
    Description
        Text
            Displays information about a widthwise component of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            net F_5
///

doc ///
    Key
        (net,VectorInWidth)
    Headline
        display an element of a free OI-module
    Usage
        net f
    Inputs
        f:VectorInWidth
    Outputs
        :Net
    Description
        Text
            Displays an element of a free OI-module. Note: terms are displayed in descending order according to the monomial order of the free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2);
            net f
///

doc ///
    Key
        (toString,FreeOIModule)
    Headline
        display a free OI-module in condensed form
    Usage
        toString F
    Inputs
        F:FreeOIModule
    Outputs
        :String
    Description
        Text
            Displays the basis symbol, basis element widths, and degree shifts of a free OI-module as a string.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            toString F
///

doc ///
    Key
        (symbol _,FreeOIModule,ZZ)
    Headline
        get the width n component of a free OI-module
    Usage
        F _ n
    Inputs
        F:FreeOIModule
        n:ZZ
    Outputs
        :ModuleInWidth
    Description
        Text
            Returns the module in width @TT "n"@ of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            F_5
///

doc ///
    Key
        isZero
        (isZero,FreeOIModule)
    Headline
        check if a free OI-module is zero
    Usage
        isZero F
    Inputs
        F:FreeOIModule
    Outputs
        :Boolean
    Description
        Text
            Checks if a free OI-module is the zero module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            G = makeFreeOIModule(d, {}, P);
            isZero F
            isZero G
///

doc ///
    Key
        (isZero,FreeOIModuleMap)
    Headline
        check if a free OI-module map is zero
    Usage
        isZero phi
    Inputs
        phi:FreeOIModuleMap
    Outputs
        :Boolean
    Description
        Text
            Checks if a free OI-module map is the zero map.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {2}, P);
            installGeneratorsInWidth(F, 2);
            b = e_(2,{1,2},1);
            C = oiRes({b}, 2)
            phi0 = C.dd_0
            phi1 = C.dd_1
            isZero phi0
            isZero phi1
///

doc ///
    Key
        (isZero,VectorInWidth)
    Headline
        check if an element of a free OI-module is zero
    Usage
        isZero f
    Inputs
        f:VectorInWidth
    Outputs
        :Boolean
    Description
        Text
            Checks if an element of a free OI-module is zero.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            f = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2)
            isZero f
            isZero(f-f)
///

doc ///
    Key
        (degree,VectorInWidth)
    Headline
        get the degree of an element of a free OI-module
    Usage
        degree f
    Inputs
        f:VectorInWidth
    Outputs
        :ZZ
    Description
        Text
            Returns the degree of the lead term of an element of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            f = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2)
            degree f
///

doc ///
    Key
        (use,ModuleInWidth)
    Headline
        use a component of a free OI-module
    Usage
        use M
    Inputs
        M:ModuleInWidth
    Description
        Text
            This method invokes @TO use@ on the underlying polynomial ring of @TT "M"@. This is useful since distinct components of a free OI-module need not be modules over the same polynomial ring.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installGeneratorsInWidth(F, 1);
            installGeneratorsInWidth(F, 2);
            use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2)
            use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3)
///

doc ///
    Key
        (terms,VectorInWidth)
    Headline
        get the terms of an element of a free OI-module
    Usage
        terms f
    Inputs
        f:VectorInWidth
    Outputs
        :List
    Description
        Text
            Returns the list of terms of an element of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            terms f
    Caveat
        The list of terms need not be in order. To find the lead term of an element, see @TO (leadTerm,VectorInWidth)@.
///

doc ///
    Key
        (leadTerm,VectorInWidth)
    Headline
        get the lead term of an element of a free OI-module
    Usage
        leadTerm f
    Inputs
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Returns the lead term of an element of a free OI-module according to the specified @TO OIMonomialOrder@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            leadTerm f
///

doc ///
    Key
        (leadMonomial,VectorInWidth)
    Headline
        get the lead monomial of an element of a free OI-module
    Usage
        leadMonomial f
    Inputs
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Returns the lead monomial of an element of a free OI-module according to the specified @TO OIMonomialOrder@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            leadMonomial f
    Caveat
        Asking for the lead monomial of a zero element will cause an error.
///

doc ///
    Key
        (leadCoefficient,VectorInWidth)
    Headline
        get the lead coefficient of an element of a free OI-module
    Usage
        leadCoefficient f
    Inputs
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Returns the lead coefficient of an element of a free OI-module according to the specified @TO OIMonomialOrder@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-5*x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            leadCoefficient f
///

doc ///
    Key
        (symbol *,Number,VectorInWidth)
    Headline
        multiply an element of a free OI-module by a number
    Usage
        n * f
    Inputs
        n:Number
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Multiplies @TT "f"@ by @TT "n"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-5*x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            22*f
///

doc ///
    Key
        (symbol +,VectorInWidth,VectorInWidth)
    Headline
        add two elements of a free OI-module
    Usage
        f + g
    Inputs
        f:VectorInWidth
        g:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Adds @TT "f"@ and @TT "g"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-5*x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            g = 5*x_(1,2)*e_(2,{2},1)
            f + g
///

doc ///
    Key
        (symbol *,RingElement,VectorInWidth)
    Headline
        multiply an element of a free OI-module by a ring element
    Usage
        r * f
    Inputs
        r:RingElement
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Multiplies @TT "f"@ by @TT "r"@ provided that @TT "r"@ belongs to the underlying polynomial ring of the component in which @TT "f"@ lives.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            (x_(1,2)+x_(2,2)^2)*f
///

doc ///
    Key
        (symbol -,VectorInWidth)
    Headline
        multiply an element of a free OI-module by -1
    Usage
        - f
    Inputs
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Flips the sign of an element of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            f = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2)
            -f
///

doc ///
    Key
        (symbol -,VectorInWidth,VectorInWidth)
    Headline
        subtract an element of a free OI-module from another
    Usage
        f - g
    Inputs
        f:VectorInWidth
        g:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Subtracts @TT "g"@ from @TT "f"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            f = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2)
            g = x_(2,2)*x_(2,1)*e_(3,{1,3},2)+x_(2,1)*e_(3,{1,2},2)
            f - g
///

doc ///
    Key
        (isHomogeneous,VectorInWidth)
    Headline
        check if an element of a free OI-module is homogeneous
    Usage
        isHomogeneous f
    Inputs
        f:VectorInWidth
    Outputs
        :Boolean
    Description
        Text
            Checks if an element of a free OI-module is homogeneous, i.e., if every term has the same degree.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            f = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2)
            g = x_(2,2)*x_(2,1)*e_(3,{1,3},2)+x_(2,1)*e_(3,{1,2},2)
            isHomogeneous f
            isHomogeneous g
///

doc ///
    Key
        (isHomogeneous,FreeOIModuleMap)
    Headline
        checks if a free OI-module map is homogeneous
    Usage
        isHomogeneous phi
    Inputs
        phi:FreeOIModuleMap
    Outputs
        :Boolean
    Description
        Text
            Checks if a map of free OI-modules is homogeneous, i.e., if it preserves degrees of elements.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installGeneratorsInWidth(F, 3);
            b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
            C = oiRes({b}, 2);
            phi = C.dd_1
            isHomogeneous phi
///

doc ///
    Key
        oiGB
        (oiGB,List)
        [oiGB,Verbose]
        [oiGB,Strategy]
    Headline
        compute a Gröbner basis for a submodule of a free OI-module
    Usage
        oiGB L
    Inputs
        L:List
    Outputs
        :List
    Description
        Text
            Given a list of elements @TT "L"@ belonging to a free OI-module $\mathbf{F}$, this method computes a Gröbner basis for the submodule generated by @TT "L"@ with respect to the monomial order of $\mathbf{F}$. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.

            The @TO Strategy@ option has the following permissible values:
        Code
            UL {
                {TT "Strategy => ", TT TO FastNonminimal, " for no post-processing of the basis"},
                {TT "Strategy => ", TT TO Minimize, " to minimize the basis after it is computed; see ", TO minimizeOIGB},
                {TT "Strategy => ", TT TO Reduce, " to reduce the basis after it is computed; see ", TO reduceOIGB}
            }
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installGeneratorsInWidth(F, 1);
            installGeneratorsInWidth(F, 2);
            use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
            use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
            time oiGB {b1, b2}
///

doc ///
    Key
        minimizeOIGB
        (minimizeOIGB,List)
        [minimizeOIGB,Verbose]
    Headline
        minimize an OI-Gröbner basis
    Usage
        minimizeOIGB G
    Inputs
        G:List
    Outputs
        :List
    Description
        Text
            This method minimizes @TT "G"@ with respect to the Gröbner property, i.e., removes any elements whose leading monomial is OI-divisible by the leading monomial of another element of @TT "G"@. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installGeneratorsInWidth(F, 1);
            installGeneratorsInWidth(F, 2);
            installGeneratorsInWidth(F, 3);
            use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
            use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
            time B = oiGB {b1, b2}
            use F_3; b3 = x_(2,3)*x_(2,2)*x_(1,1)*e_(3,{2,3},3)-x_(2,1)*x_(1,2)^2*e_(3,{1,3},3);
            C = append(B, b3) -- dump in a redundant element
            minimizeOIGB C -- an element gets removed
///

doc ///
    Key
        reduceOIGB
        (reduceOIGB,List)
        [reduceOIGB,Verbose]
    Headline
        reduce an OI-Gröbner basis
    Usage
        reduceOIGB G
    Inputs
        G:List
    Outputs
        :List
    Description
        Text
            This method reduces @TT "G"@ as a Gröbner basis, i.e., ensures that no monomial of any element of @TT "G"@ is OI-divisible by the leading monomial of any other element of @TT "G"@. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installGeneratorsInWidth(F, 1);
            installGeneratorsInWidth(F, 2);
            use F_1; b1 = x_(2,1)*e_(1,{1},2)+x_(1,1)*e_(1,{1},2);
            use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(1,2)*e_(2,{2},2);
            time B = oiGB({b1, b2}, Strategy => FastNonminimal)
            minimizeOIGB B -- does not change the basis
            reduceOIGB B -- changes the basis
///

doc ///
    Key
        isOIGB
        (isOIGB,List)
        [isOIGB,Verbose]
    Headline
        check if a list of elements of a free OI-module forms a Gröbner basis
    Usage
        isOIGB L
    Inputs
        L:List
    Outputs
        :Boolean
    Description
        Text
            Checks if a @TT "L"@ forms a Gröbner basis for the submodule it generates. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installGeneratorsInWidth(F, 1);
            installGeneratorsInWidth(F, 2);
            installGeneratorsInWidth(F, 3);
            use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
            use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
            isOIGB {b1, b2}
            time B = oiGB {b1, b2}
            isOIGB B
///

doc ///
    Key
        oiSyz
        (oiSyz,List,Symbol)
        [oiSyz,Verbose]
        [oiSyz,Strategy]
    Headline
        compute a Gröbner basis for the syzygy module of a submodule of a free OI-module
    Usage
        oiSyz(G, d)
    Inputs
        G:List
        d:Symbol
    Outputs
        :List
    Description
        Text
            Given a non-empty Gröbner basis @TT "G"@ for a submodule $\mathbf{M}$ of a free OI-module $\mathbf{F}$, this method computes a Gröbner basis $G'$ for the syzygy module of $\mathbf{M}$ with respect to the Schreyer order induced by $G$; see @TO OIMonomialOrder@.

            The new Gröbner basis $G'$ lives in an appropriate free OI-module $\mathbf{G}$ with basis symbol @TT "d"@ whose basis elements are mapped onto the elements of $G$ by a canonical surjective map $\varphi:\mathbf{G}\to\mathbf{M}$ (see Definition 4.1 of [1]). Moreover, the degrees of the basis elements of $\mathbf{G}$ are automatically shifted to coincide with the degrees of the elements of $G$, so that $\varphi$ is homogeneous if $G$ consists of homogeneous elements. If $G'$ is not empty, then one obtains $\mathbf{G}$ by applying @TO getFreeOIModule@ to any element of $G'$. One obtains $\varphi$ by using @TO getSchreyerMap@.

            The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.

            The @TO Strategy@ option has the following permissible values:
        Code
            UL {
                {TT "Strategy => ", TT TO FastNonminimal, " for no post-processing of the basis"},
                {TT "Strategy => ", TT TO Minimize, " to minimize the basis after it is computed; see ", TO minimizeOIGB},
                {TT "Strategy => ", TT TO Reduce, " to reduce the basis after it is computed; see ", TO reduceOIGB}
            }
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            G = oiGB {b}
            oiSyz(G, d)
        Text
            {\em References:}

            [1] M. Morrow and U. Nagel, {\it Computing Gröbner Bases and Free Resolutions of OI-Modules}, Preprint, arXiv:2303.06725, 2023.
///

doc ///
    Key
        OIResolution
    Headline
        the class of all resolutions of submodules of free OI-modules
    Description
        Text
            This type implements free resolutions of submodules of free OI-modules. To make an @TT "OIResolution"@ object, use @TO oiRes@. To verify that an OI-resolution is a complex, use @TO isComplex@. To get the $n$th differential in an OI-resolution @TT "C"@, use @TT "C.dd_n"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 1)
            C.dd_0
///

doc ///
    Key
        describeFull
        (describeFull, OIResolution)
    Headline
        describe an OI-resolution and the maps
    Usage
        describe C
    Inputs
        C:OIResolution
    Outputs
        :Net
    Description
        Text
            Displays the free OI-modules and describes the differentials of an OI-resolution.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 1);
            describeFull C 
///

doc ///
    Key
        (describe,OIResolution)
    Headline
        describe an OI-resolution
    Usage
        describe C
    Inputs
        C:OIResolution
    Outputs
        :Net
    Description
        Text
            Displays the free OI-modules and differentials of an OI-resolution.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 1);
            describe C
///

doc ///
    Key
        (net,OIResolution)
    Headline
        display an OI-resolution
    Usage
        net C
    Inputs
        C:OIResolution
    Outputs
        :Net
    Description
        Text
            Displays the basis element widths and degree shifts of the free OI-modules in an OI-resolution.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 1);
            net C
///

doc ///
    Key
        (symbol _,OIResolution,ZZ)
    Headline
        get a module of an OI-resolution is specified homological degree
    Usage
        C _ n
    Inputs
        C:OIResolution
        n:ZZ
    Outputs
        :FreeOIModule
    Description
        Text
            Returns the free OI-module of $C$ in homological degree $n$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 1);
            C_0
            C_1
///

doc ///
    Key
        oiRes
        (oiRes,List,ZZ)
        [oiRes,Verbose]
        [oiRes,Strategy]
        [oiRes,TopNonminimal]
    Headline
        compute an OI-resolution
    Usage
        oiRes(L, n)
    Inputs
        L:List
        n:ZZ
    Outputs
        :OIResolution
    Description
        Text
            Computes an OI-resolution of the submodule generated by @TT "L"@ out to homological degree @TT "n"@. If @TT "L"@ consists of homogeneous elements, then the resulting resolution will be graded and minimal out to homological degree $n-1$. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.

            The @TO Strategy@ option has the following permissible values:
        Code
            UL {
                {TT "Strategy => ", TT TO FastNonminimal, " for no post-processing of the Gröbner basis computed at each step"},
                {TT "Strategy => ", TT TO Minimize, " to minimize the Gröbner basis after it is computed at each step; see ", TO minimizeOIGB},
                {TT "Strategy => ", TT TO Reduce, " to reduce the Gröbner basis after it is computed at each step; see ", TO reduceOIGB}
            }
        Text
            The @TO TopNonminimal@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants the Gröbner basis in homological degree $n-1$ to be minimized. Therefore, use @TT "TopNonminimal => true"@ for no minimization of the basis in degree $n-1$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time oiRes({b}, 2, TopNonminimal => true)
///

doc ///
    Key
        ranks
        (ranks,OIResolution)
    Headline
        display the ranks of an OI-resolution
    Usage
        ranks C
    Inputs
        C:OIResolution
    Outputs
        :Net
    Description
        Text
            Given an OI-resolution $C$, this method displays the ranks of the modules in $C$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            C = oiRes({b}, 2)
            ranks C
///

doc ///
    Key
        restrictedRanks
        (restrictedRanks,OIResolution,ZZ)
    Headline
        display the ranks of an OI-resolution restricted to a given width
    Usage
        restrictedRanks(C,w)
    Inputs
        C:OIResolution
        w:ZZ
    Outputs
        :Net
    Description
        Text
            Given an OI-resolution $C$ and a width $w$, this method displays the ranks of the modules in $C$ restricted to width $w$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            C = oiRes({b}, 2)
            ranks C
            restrictedRanks(C,5)
///

doc ///
    Key
        TopNonminimal
    Headline
        option for minimizing the top Gröbner basis of an OI-resolution
    Description
        Text
            Used as an optional argument in the @TO oiRes@ method. The @TT "TopNonminimal"@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants the Gröbner basis in homological degree $n-1$ to be minimized. Therefore, use @TT "TopNonminimal => true"@ for no minimization of the basis in degree $n-1$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time oiRes({b}, 2, TopNonminimal => true)
///

doc ///
    Key
        isComplex
        (isComplex,OIResolution)
        [isComplex,Verbose]
    Headline
        verify that an OI-resolution is a complex
    Usage
        isComplex C
    Inputs
        C:OIResolution
    Outputs
        :Boolean
    Description
        Text
            This method verifies that an OI-resolution is indeed a complex. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installGeneratorsInWidth(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 2, TopNonminimal => true)
            isComplex C
///

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- TESTS -----------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- TEST 0
TEST ///
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1,2}, P);
installGeneratorsInWidth(F, 1);
installGeneratorsInWidth(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
B = oiGB {b1, b2};
assert(#B === 3);
assert isOIGB B
///

-- TEST 1
TEST ///
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installGeneratorsInWidth(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
B = oiGB {b};
assert(#B === 2);
C = oiSyz(B, d);
assert(#C === 3);
assert isOIGB C
///

-- TEST 2
TEST ///
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,2}, P);
installGeneratorsInWidth(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
C = oiRes({b}, 2);
assert isComplex C;
assert(getRank C_0 === 1);
assert(getRank C_1 === 2);
assert(apply(getBasisElements C_0, getWidth) === {3});
assert(apply(getBasisElements C_1, getWidth) === {5, 5})
///

-- TEST 3
TEST ///
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installGeneratorsInWidth(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
C = oiRes({b}, 3);
assert isComplex C;
assert isHomogeneous(C.dd_0);
assert isHomogeneous(C.dd_1);
assert isHomogeneous(C.dd_2);
assert isHomogeneous(C.dd_3);
assert(getRank C_0 === 1);
assert(getRank C_1 === 1);
assert(getRank C_2 === 2);
assert(apply(getBasisElements C_0, getWidth) === {2});
assert(apply(getBasisElements C_1, getWidth) === {4});
assert(apply(getBasisElements C_2, getWidth) === {5,5})
///

end

-- GB example 1: one linear and one quadratic
-- Comment: see https://arxiv.org/pdf/2303.06725.pdf example 3.20
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1,2}, P);
installGeneratorsInWidth(F, 1);
installGeneratorsInWidth(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
time B = oiGB({b1, b2}, Verbose => true)

-- Res example 1: single quadratic in width 3
-- Comment: see https://arxiv.org/pdf/2303.06725.pdf example 5.5 (i)
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,2}, P);
installGeneratorsInWidth(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
time C = oiRes({b}, 2, Verbose => true)

-- Res example 2: single quadratic in width 2
-- Comment: see https://arxiv.org/pdf/2303.06725.pdf example 5.5 (ii)
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installGeneratorsInWidth(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
time C = oiRes({b}, 4, Verbose => true)

-- Res example 3: single quadratic in width 2
-- Comment: compare with res example 1
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1}, P);
installGeneratorsInWidth(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},1);
time C = oiRes({b}, 5, Verbose => true) -- Takes my laptop 30 minutes (minimal ranks 1, 2, 5, 9, 14)

-- Res example 4: single quadratic in width 3
-- Comment: compare with res example 1
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installGeneratorsInWidth(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1},2);
time C = oiRes({b}, 5, Verbose => true)

-- OI-ideal example
-- Comment: 2x2 minors with a gap of at least 1
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {0}, P);
installGeneratorsInWidth(F, 3);
b = (x_(1,1)*x_(2,3)-x_(2,1)*x_(1,3))*e_(3,{},1);
time C = oiRes({b}, 2, Verbose => true)

-- Res example 5: single quadratic in width 3
-- Comment: compare with res examples 1 and 4
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installGeneratorsInWidth(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1},2);
time C = oiRes({b}, 3, Verbose => true)
ranks C