newPackage(
    "Permutations",
    AuxiliaryFiles => true,
    Version => "1.0", 
    Date => "August 7, 2024",
    Keywords => {"Combinatorics"},
    Authors => {
        {Name => "Sean Grate", 
         Email => "sean.grate@auburn.edu", 
         HomePage => "https://seangrate.com/"}
    },
    Headline => "functions for working with permutations"
)

export {
    -- types
    "Permutation",
    -- methods
    "permutation",
    "cycleDecomposition",
    "cycleType",
    "ascents",
    "descents",
    "ascendingRuns",
    "descendingRuns",
    "exceedances",
    "saliances",
    "records",
    "avoidsPattern",
    "avoidsPatterns",
    "isVexillary",
    "isCartwrightSturmfels",
    "isCDG",
    "foataBijection",
    "ord",
    "sign",
    "isEven",
    "isOdd",
    "isDerangement",
    "fixedPoints",
    "inversions",
    -- symbols
    "Weak"
}

-----------------------------------------------------------------------------
-- **CODE** --
-----------------------------------------------------------------------------
------------------------------------
-- Local utilities
------------------------------------
to1Index := w -> (w / (i -> i+1))
to0Index := w -> (w / (i -> i-1))

------------------------------------
-- Permutation type declarations and basic constructors
------------------------------------
Permutation = new Type of VisibleList
Permutation.synonym = "permutation"

new Permutation from VisibleList := (typeofPermutation,w) -> w

permutation = method()
permutation VisibleList := Permutation => w -> new Permutation from w

isWellDefined Permutation := Boolean => w -> (
    wList := toList w;
    ((sort wList) == toList(1..#wList)) and not (#wList == 0)
)

------------------------------------
-- Permutation string representations
------------------------------------
expression Permutation := w -> expression toList w
toString Permutation := w -> toString expression toList w
tex Permutation := w -> tex expression toSequence w
html Permutation := w -> html expression toList w

------------------------------------
-- Indexing permutations as lists
------------------------------------
Permutation _ ZZ := ZZ => (w,n) -> ((toList w)_(n-1))
Permutation _ List := List => (w,l) -> ((toList w)_l)
Permutation _ Sequence := List => (w,s) -> ((toList w)_(toList s))

------------------------------------
-- Change which symmetric group S_n to view a permutation as an element of
------------------------------------
trim Permutation := Permutation => o -> w -> (
    w = w_(select(#w, i -> w_{i ..< #w} != toList(i+1 .. #w)));
    if #w == 0 then permutation {1} else permutation w
)

extend (Permutation, ZZ) := Permutation => o -> (w,n) -> (
    if n < #w then error(toString w | " is a permutation on more than " | toString n | " letters.");
    permutation(toList(w) | toList(#w+1..n))
)
extend (Permutation, Permutation) := Sequence => o -> (w,v) -> (
    n := max(#w, #v);
    (extend(w,n), extend(v,n))
)

------------------------------------
-- Basic permutation operations
------------------------------------
Permutation == Permutation := Boolean => (w, v) -> (
    (w, v) = extend(w,v);
    toList(w) == toList(v)
)
Permutation * Permutation := Permutation => (w, v) -> (
    (w,v) = extend(w,v);
    trim permutation w_(to0Index toList v)
)
-- power implementation modified from Mahrud's in https://github.com/Macaulay2/M2/issues/2581
Permutation ^ ZZ := Permutation => (w, n) -> fold(if n < 0 then (-n):(permutation to1Index inversePermutation to0Index toList w) else n:w,
                                                  permutation toList (1 .. #w),
                                                  (w, v) -> w*v)

------------------------------------
-- Matrix representation of a permutation
------------------------------------
-- some people prefer the transpose of this
matrix Permutation := Matrix => o -> w -> (
    id_(ZZ^(#w))_(to0Index toList w)
)

------------------------------------
-- Group actions
------------------------------------
Permutation * VisibleList := VisibleList => (w, l) -> (
    if #(trim w) > #l then error(toString w | " permutes more than " | toString #l | " elements.") 
    else l_(to0Index toList extend(w, #l))
)
VisibleList _ Permutation := VisibleList => (l, w) -> (w*l)

-- group action on a matrix permutes the rows/columns of the matrix
Permutation * Matrix := Matrix => (w, M) -> (
    m := numRows M;
    if #(trim w) > m then error(toString w | " permutes more than " | toString m | " elements.") 
    else (matrix extend(w, m)) * M
)
Matrix _ Permutation := Matrix => (M, w) -> (w*M)
Matrix * Permutation := Matrix => (M, w) -> (transpose(w*(transpose M)))
Matrix ^ Permutation := Matrix => (M, w) -> (M*w)

------------------------------------
-- Cycle decomposition of a permutation
------------------------------------
-- every permutation can be written as a product of disjoint cycles (in cycle notation)
cycleDecomposition = method()
cycleDecomposition Permutation := List => w -> (
    w = to0Index toList w;
    cycles := {};
    unvisited := toList(0 ..< #w);
    while #unvisited != 0 do (
        startIdx := unvisited#0;
        visited := {startIdx};
        nextIdx := w#startIdx;
        while nextIdx != startIdx do (
            visited = append(visited, nextIdx);
            nextIdx = w#nextIdx;
        );
        cycles = append(cycles, visited);
        for idx in visited do unvisited = delete(idx, unvisited);
    );
    cycles = cycles / to1Index / toSequence;
    -- put decomposition into its standard or canonical representation (see https://en.wikipedia.org/wiki/Permutation#canonical_cycle_notation)
    -- a permutation's cycle decomposition is "canonical" or "standard" if
    --     1. each cycle lists the largest element first and
    --     2. the cycles are sorted by their first element in increasing order
    sortedCycles := for cycle in cycles list toSequence rotate(maxPosition cycle, toList cycle);
    sort sortedCycles
)

cycleType = method()
cycleType Permutation := Sequence => w -> (
    toSequence rsort for cycle in cycleDecomposition w list #cycle
)

------------------------------------
-- Ascents, descents, runs, exceedances, and records
-- NOTE: All return the 1-indexed results for consistency with the permutation notation
------------------------------------
ascents = method()
ascents Permutation := List => w -> (
    for i in 1 ..< #w list if w_i < w_(i+1) then i else continue
)

descents = method()
descents Permutation := List => w -> (
    for i in 1 ..< #w list if w_i > w_(i+1) then i else continue
)

ascendingRuns = method()
ascendingRuns Permutation := List => w -> (
    -- inspired from the SageMath implementation
    -- https://github.com/sagemath/sage/blob/develop/src/sage/combinat/permutation.py
    if #w == 1 then allRuns := {toSequence w}
    else (
        allRuns = {};
        currentRun := {w#0};
        for wi in w_(toList(1 ..< #w)) do (
            if wi > last currentRun then currentRun = append(currentRun, wi)
            else (
                allRuns = append(allRuns, toSequence currentRun);
                currentRun = {wi};
            );
        );
        allRuns = append(allRuns, toSequence currentRun);
    );
    allRuns
)

descendingRuns = method()
descendingRuns Permutation := List => w -> (
    -- inspired from the SageMath implementation
    -- https://github.com/sagemath/sage/blob/develop/src/sage/combinat/permutation.py
    if #w == 1 then allRuns := {toSequence w}
    else (
        allRuns = {};
        currentRun := {w#0};
        for wi in w_(toList(1 ..< #w)) do (
            if wi < last currentRun then currentRun = append(currentRun, wi)
            else (
                allRuns = append(allRuns, toSequence currentRun);
                currentRun = {wi};
            );
        );
        allRuns = append(allRuns, toSequence currentRun);
    );
    allRuns
)

exceedances = method(Options => {Weak => false})
exceedances Permutation := List => opts -> w -> (
    compare := if opts.Weak then ((i,j) -> i <= j) else ((i,j) -> i < j);
    for i in 1 .. #w list if compare(i,w_i) then i else continue 
)

saliances = method()
saliances Permutation := List => w -> (
    to1Index positions(1 .. #w, i -> all(i+1 .. #w, j -> w_i > w_j))
)

records = method()
records Permutation := List => w -> (
    to1Index positions(1 .. #w, i -> all(1 ..< i, j -> w_j < w_i))
)

------------------------------------
-- Pattern avoidance
------------------------------------
avoidsPattern = method(TypicalValue => Boolean)
avoidsPattern (Permutation,List) := (w, pattern) -> (
    --assume permutation is pattern-avoiding, break if not true
    for idx in subsets(0 .. #w-1, #pattern) do {
        vals := w_(idx);
        sortedVals := sort(vals);
        relPositions := hashTable toList apply(0..#vals-1, i -> {sortedVals#i, i});
        p := toList apply(vals, i -> (relPositions#i) + 1); 
        if p == pattern then return false;
    };
    true
)

avoidsPatterns = method(TypicalValue => Boolean)
avoidsPatterns (Permutation, List) := (w, patterns) -> (
    all(patterns, pattern -> avoidsPattern(w, pattern))
)

isVexillary = method(TypicalValue => Boolean)
isVexillary Permutation := (w) -> (
    avoidsPattern(w, {2,1,4,3})
)

isCartwrightSturmfels = method(TypicalValue => Boolean)
isCartwrightSturmfels Permutation := w -> (
    patterns := {{1,2,5,4,3}, 
                 {1,3,2,5,4}, 
                 {1,3,5,2,4}, 
                 {1,3,5,4,2}, 
                 {2,1,5,4,3}, 
                 {1,2,5,3,6,4}, 
                 {1,2,5,6,3,4}, 
                 {2,1,5,3,6,4},
                 {2,1,5,6,3,4},
                 {3,1,5,2,6,4},
                 {3,1,5,6,2,4},
                 {3,1,5,6,4,2}};
    avoidsPatterns(w, patterns)
)

isCDG = method(TypicalValue => Boolean)
isCDG Permutation := w -> (
    patterns := {{1,3,2,5,4},
                 {2,1,5,4,3},
                 {2,1,4,6,3,5},
                 {2,1,5,3,6,4},
                 {2,1,5,6,3,4},
                 {2,4,1,6,3,5},
                 {3,1,5,2,6,4},
                 {4,2,6,1,7,3,5}};
    avoidsPatterns(w, patterns)
)

------------------------------------
-- Foata's fundamental bijection
------------------------------------
-- see https://en.wikipedia.org/wiki/Permutation#Foata's_transition_lemma
foataBijection = method()
foataBijection Permutation := Permutation => w -> (
    permutation splice cycleDecomposition w
)

------------------------------------
-- Miscellaneous
------------------------------------
-- inverse = method()
inverse Permutation := Permutation => w -> (permutation to1Index inversePermutation to0Index toList w)

-- order of a permutation, i.e. smallest integer n such that w^n = identity
-- the order of a permutation can be expressed as the lcm of its cycle lengths
ord = method()
ord Permutation := ZZ => w -> (
    lcm((cycleDecomposition w) / length)
)

-- see https://en.wikipedia.org/wiki/Parity_of_a_permutation for different ways
-- to compute the sign or parity of a permutation
sign = method()
sign Permutation := ZZ => w -> (
    if even(#w - #(cycleDecomposition w)) then 1 else -1
)

isEven = method(TypicalValue => Boolean)
isEven Permutation := w -> (
    sign w == 1
)

isOdd = method(TypicalValue => Boolean)
isOdd Permutation := w -> (
    sign w == -1
)

isDerangement = method(TypicalValue => Boolean)
isDerangement Permutation := w -> (not any(cycleDecomposition w, cycle -> #cycle == 1))

fixedPoints = method()
fixedPoints Permutation := List => w -> (for cycle in cycleDecomposition w list if #cycle == 1 then unsequence cycle else continue)

inversions = method()
inversions Permutation := List => w -> (
    for idxPair in sort(subsets(toList w, 2) / sort) list if w_(idxPair#0) > w_(idxPair#1) then idxPair else continue
)

length Permutation := ZZ => w -> (#(inversions w))

-----------------------------------------------------------------------------
-- **DOCUMENTATION** --
-----------------------------------------------------------------------------
beginDocumentation()
load "./Permutations/docs.m2"

-----------------------------------------------------------------------------
-- **TESTS** --
-----------------------------------------------------------------------------
load "./Permutations/tests.m2"
end

-----------------------------------------------------------------------------
--Development Section
-----------------------------------------------------------------------------
restart
uninstallPackage "Permutations"
restart
installPackage "Permutations"
restart
needsPackage "Permutations"
elapsedTime check "Permutations"
viewHelp "Permutations"