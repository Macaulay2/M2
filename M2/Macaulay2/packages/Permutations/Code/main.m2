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
-- Some permutation constructors
------------------------------------
-- some people prefer the transpose of this
matrix Permutation := Matrix => o -> w -> (
    id_(ZZ^(#w))_(to0Index toList w)
)


transposition = method()
transposition ZZ := Permutation => (i) -> (permutation switch(i-1, i, toList(1..i+1)))
transposition (ZZ, ZZ) := Permutation => (i, j) -> (permutation switch(i-1, j-1, toList(1..(max(i,j)))))


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
sign Permutation := ZZ => w -> (
    if even(#w - #(cycleDecomposition w)) then 1 else -1
)


isEven = method(TypicalValue => Boolean)
isEven Permutation := w -> (sign w == 1)


isOdd = method(TypicalValue => Boolean)
isOdd Permutation := w -> (sign w == -1)


isDerangement = method(TypicalValue => Boolean)
isDerangement Permutation := w -> (not any(cycleDecomposition w, cycle -> #cycle == 1))


fixedPoints = method()
fixedPoints Permutation := List => w -> (for cycle in cycleDecomposition w list if #cycle == 1 then unsequence cycle else continue)


inversions = method()
inversions Permutation := List => w -> (
    for idxPair in sort(subsets(toList w, 2) / sort) list if w_(idxPair#0) > w_(idxPair#1) then idxPair else continue
)


length Permutation := ZZ => w -> (#(inversions w))


------------------------------------
-- Random permutations
------------------------------------
randomPermutation = method()
randomPermutation ZZ := Permutation => (n) -> (permutation random toList(1..n))


------------------------------------
-- Words
------------------------------------
reducedWords = method()
reducedWords Permutation := List => w -> (
    -- This is an adaptation of the MAPLE procedure found on page 7
    -- "The Saga of Reduced Factorizations of Elements of the Symmetric Group" [Garsia, 2001]
    -- https://mathweb.ucsd.edu/~garsia/somepapers/saga.pdf
    predecessors := apply(descents w, i -> (i, w * (permutation switch(i-1, i, toList(1..#w)))));

    -- base case
    if predecessors === {} then return {{}};
    -- recursive case
    flatten for idxPermPair in predecessors list (
        (idx, tau) := idxPermPair;
        (reducedWords tau) / (word -> prepend(idx, word))
    )
)


------------------------------------
-- Bruhat orders
------------------------------------
leftWeakBruhatOrder = method()
leftWeakBruhatOrder (Permutation, Permutation) := Boolean => (w, v) -> (
    -- Proposition 3.1.2 (ii) of "Combinatorics of Coxeter Groups" [Bjorner-Brenti, 2005]
    length w + length(w * (inverse v)) == length v
)


rightWeakBruhatOrder = method()
rightWeakBruhatOrder (Permutation, Permutation) := Boolean => (w, v) -> (
    -- Proposition 3.1.2 (ii) of "Combinatorics of Coxeter Groups" [Bjorner-Brenti, 2005]
    length w + length((inverse w) * v) == length v
)


weakBruhatOrder = method(Options => {Side => "right"})
weakBruhatOrder (Permutation, Permutation) := Boolean => opts -> (w, v) -> (
    orders := hashTable {"left" => leftWeakBruhatOrder,
                         "right" => rightWeakBruhatOrder};
    if not orders#?(opts.Side) then error("Invalid Bruhat order side: " | opts.Side)
    else orders#(opts.Side)(w, v)
)


strongBruhatOrder = method()
strongBruhatOrder (Permutation, Permutation) := Boolean => (w, v) -> (
    -- METHOD 1
    -- w <= v if and only if every principal submatrix in w has at least as 
    -- many 1's as the corresponding principal submatrix in v.
    -- (See Theorem 2.1.5 of "Combinatorics of Coxeter Groups" [Bjorner-Brenti, 2005]) 

    -- METHOD 2 (Tableau criterion)
    -- w <= v if and only if the cumulative sorted tableau of w is component-wise
    -- less than or equal to the cumulative sorted tableau of v.
    -- (See Theorem 2.6.3 of "Combinatorics of Coxeter Groups" [Bjorner-Brenti, 2005])

    -- NOTE: Method 2 turns out to be much faster than Method 1.
    -- NOTE: Remarkably, computing descent sets does not seem to provide any speedup on average
    --       (see https://github.com/seangrate/M2/issues/22#issuecomment-2868631145).
    (w, v) = extend(w, v);
    n := #w;
    for k in 0 ..< n do (
        if any(sort w_{0..k}, sort v_{0..k}, (i,j) -> i > j) then return false;
    );
    true
)


symmetricGroupPoset = method()
symmetricGroupPoset (ZZ, Function) := Poset => (n, comparisonFunction) -> (
    Sn := apply(permutations n, p -> permutation to1Index p);
    poset(Sn, comparisonFunction)
)