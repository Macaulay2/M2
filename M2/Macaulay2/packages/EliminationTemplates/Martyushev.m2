-- Martyushev.m2: port of Martyushev's matrixHi + adjustParams (CVPR 2022).
--
-- Design:
-- - Work in a flat extended ring R = FF[vars..., alphas...] from the start.
-- - Free alpha parameters are ring variables; adjustParams substitutes
--   concrete field values into H directly, without intermediate
--   decompositions.
--
-- Loaded two ways:
-- (a) from inside EliminationTemplates.m2 newPackage (no needsPackage — that
--     would be a circular dep).
-- (b) by external tests that `debug needsPackage "EliminationTemplates"`
--     first, then `load "EliminationTemplates/Martyushev.m2"`.

-- ============================================================================
-- buildH: main function combining matrixHi + adjustParams + constructTemplate
--
-- Input: action variable, basis B, ideal J
-- Output: (Hoptimized, residualMons, Blist, alphaEnv, extendedRing)
-- where Hoptimized is an nG x nF matrix with symbolic alpha entries that
-- have been partially reduced by the greedy.
-- ============================================================================
buildGapPolys = method()
buildGapPolys(RingElement, Matrix, Ideal) := (aVar, B, J) -> (
    R := ring J;
    aVar2 := sub(aVar, R);
    Blist := flatten entries lift(B, R);
    Bset := set Blist;
    aBlist := apply(Blist, b -> aVar2 * b);
    resMons := select(aBlist, m -> not Bset#?m);
    gapP := apply(resMons, r -> r - (r % J));
    (gapP, toList resMons, Blist)
);
-- Non-standard basis variant (Direction 4.1 / Larsson 2018). `Blist` is any
-- list of |B| = deg(I) monomials forming a basis of R/J. We express the
-- normal form of each residual in Blist via a change-of-basis through the
-- Gröbner-determined standard basis: NF(r,J) = sum c_i b_i, solve for c_i,
-- then the gap polynomial is r - sum c_i b_i (an element of J). Errors if
-- Blist is linearly dependent mod J.
buildGapPolys(RingElement, List, Ideal) := (aVar, Blist, J) -> (
    R := ring J;
    FF := coefficientRing R;
    aVar2 := sub(aVar, R);
    -- Accept Blist entries either in R or in R/J; lift to R uniformly.
    Blist = apply(Blist, b -> sub(b, R));
    stdB := basis(R/J);
    stdBlist := flatten entries lift(stdB, R);
    nB := #Blist;
    if nB != #stdBlist then error ("buildGapPolys: basis size " | toString nB
        | " != deg R/J = " | toString #stdBlist);
    -- Columns = Blist reduced mod J expressed in stdB coords.
    BmatStd := sub(last coefficients(matrix{apply(Blist, b -> b % J)},
        Monomials => matrix{stdBlist}), FF);
    if rank BmatStd < nB then error "buildGapPolys: Blist is not a basis of R/J";
    BmatStdInv := inverse BmatStd;
    Bset := set Blist;
    aBlist := apply(Blist, b -> aVar2 * b);
    resMons := select(aBlist, m -> not Bset#?m);
    gapP := apply(resMons, r -> (
        rNF := r % J;
        rStd := sub(last coefficients(matrix{{rNF}},
            Monomials => matrix{stdBlist}), FF);
        cB := BmatStdInv * rStd;
        reExpressInB := sum prepend(0_R, apply(nB, i -> sub(cB_(i,0), R) * Blist#i));
        r - reExpressInB
    ));
    (gapP, toList resMons, Blist)
);

-- Validate a candidate basis: return true iff Blist is a basis of R/J.
isValidBasis = method()
isValidBasis(List, Ideal) := (Blist, J) -> (
    R := ring J;
    FF := coefficientRing R;
    Blist = apply(Blist, b -> sub(b, R));
    stdBlist := flatten entries lift(basis(R/J), R);
    if #Blist != #stdBlist then return false;
    BmatStd := sub(last coefficients(matrix{apply(Blist, b -> b % J)},
        Monomials => matrix{stdBlist}), FF);
    rank BmatStd == #stdBlist
);

-- Build a Greedy template (matrixHi + adjustParams + constructTemplate) on
-- a specified basis Blist of R/J. Returns the template matrix directly over
-- coefficientRing(ring J). Set `"RunGreedy" => false` to skip adjustParams
-- (just MatrixHi's particular solution alpha=0) — ~100x faster, useful for
-- coarse basis-search screening before re-running Greedy on the winner.
buildGreedyTemplateWithBasis = method(Options => {Verbose => false, "RunGreedy" => true})
buildGreedyTemplateWithBasis(RingElement, Ideal, List) := o -> (aVar, J, Blist) -> (
    R := ring J;
    FF := coefficientRing R;
    Flist := flatten entries gens J;
    (gp, resMons, B) := buildGapPolys(aVar, Blist, J);
    if #gp == 0 then return map(FF^0, FF^(#B), 0);
    RB := (toList resMons) | B;
    (Hsym, Rext, alphaVars, perRow) := buildHSymbolic(Flist, gp, Verbose => o.Verbose);
    Hfinal := if o#"RunGreedy" then (
        adjustParams(Flist, Hsym, Rext, (alphaVars, RB), Verbose => o.Verbose)
    ) else (
        -- MatrixHi particular solution: substitute all alphas to 0.
        if #alphaVars > 0 then (
            finalSubst := map(R, Rext,
                apply(numgens R, i -> R_i) | apply(#alphaVars, k -> 0_R));
            matrix apply(numRows Hsym, i ->
                apply(numColumns Hsym, j -> finalSubst(Hsym_(i,j))))
        ) else Hsym
    );
    (sh, M, V, Eexcess) := buildTemplateFromH(Hfinal, Flist, RB, Verbose => o.Verbose);
    M
);

-- Route (b), Martyushev CVPR 2022 §5: draw `n` random *standard* bases of
-- R/J by choosing a random positive weight vector, computing the Gröbner
-- basis of J under `Weights => w, GRevLex`, and taking the resulting
-- quotient basis. Deduplicates by the monomial set. Returns a list of
-- monomial lists in R (not in R/J); callers can pass any of these to
-- `buildGreedyTemplateWithBasis` or `searchBases`.
randomStandardBases = method(Options => {"Budget" => 5})
randomStandardBases(Ideal, ZZ) := o -> (J, n) -> (
    R := ring J;
    nv := numgens R;
    budget := o#"Budget";
    out := new MutableList;
    seen := new MutableHashTable;
    for i from 0 to n - 1 do (
        w := apply(nv, k -> 1 + random 99);
        ok := true;
        local B;
        -- alarm budget guards against pathological weight vectors that make
        -- basis(R/J) run indefinitely; try/else catches both the resulting
        -- AlarmInterrupt and any other runtime error.
        alarm budget;
        try (
            Rprime := newRing(R, MonomialOrder => {Weights => w, GRevLex});
            Jprime := sub(J, Rprime);
            Bprime := flatten entries lift(basis(Rprime / Jprime), Rprime);
            phi := map(R, Rprime, apply(nv, k -> R_k));
            B = apply(Bprime, b -> phi(b));
        ) else ( ok = false; );
        alarm 0;
        if not ok then continue;
        key := set B;
        if seen#?key then continue;
        seen#key = true;
        out#(#out) = B;
    );
    toList out
);

-- Route (c), Larsson 2018: sample `n` random *non-standard* bases by picking
-- |B| = deg(R/J) random monomials from an extended monomial pool and keeping
-- those that pass `isValidBasis`. The pool defaults to all monomials of
-- degree up to twice the max degree of the standard basis, plus 1. Times
-- out after `n * 20` attempts to avoid infinite loops on problems where
-- valid non-standard bases are rare.
randomNonstandardBases = method()
randomNonstandardBases(Ideal, ZZ) := (J, n) -> (
    R := ring J;
    stdBlist := flatten entries lift(basis(R/J), R);
    nB := #stdBlist;
    maxDeg := max apply(stdBlist, b -> first degree b);
    pool := flatten entries basis(0, 2 * maxDeg + 1, R);
    out := new MutableList;
    seen := new MutableHashTable;
    attempts := 0;
    while #out < n and attempts < 20 * n do (
        attempts = attempts + 1;
        cand := randomSubset(pool, nB);
        if not isValidBasis(cand, J) then continue;
        key := set cand;
        if seen#?key then continue;
        seen#key = true;
        out#(#out) = cand;
    );
    toList out
);

-- Parse a Martyushev `_greedyAG/bases/b_<prob>` file into a list of monomial
-- lists over the ring `R`. Each input line is of the form
--     [mon1, mon2, ..., mon_d]
-- and is evaluated in R. Lines that fail to parse (e.g. they use a variable
-- not in R) are silently skipped; the caller gets whichever bases were
-- readable. Intended use: run `buildGreedyTemplateWithBasis` on every parsed
-- basis and keep the smallest template.
parseMartyushevBases = method()
parseMartyushevBases(String, Ring) := (path, R) -> (
    content := get path;
    use R;
    out := for line in lines content list (
        if #line == 0 or first line != "[" then continue;
        listStr := replace("\\[", "{", replace("\\]", "}", line));
        ok := true;
        local mons;
        try ( mons = value listStr; ) else ( ok = false; );
        if ok and instance(mons, List) then continue;
	mons
    );
    out
);

-- Basis-search loop. Default: screens each candidate with MatrixHi (fast,
-- O(seconds per basis)), then re-runs Greedy on the winner. Pass
-- `"RunGreedyAll" => true` to run full Greedy on every candidate (slow but may
-- find bases where adjustParams strictly improves over MatrixHi).
searchBases = method(Options => {Verbose => false, "RunGreedyAll" => false})
searchBases(RingElement, Ideal, List) := o -> (aVar, J, candidates) -> (
    bestRows := infinity;
    bestCols := 0;
    bestM := null;
    bestB := null;
    idx := 0;
    for B in candidates do (
        idx = idx + 1;
        if not isValidBasis(B, J) then continue;
        local M;
        ok := true;
        try (
            M = buildGreedyTemplateWithBasis(aVar, J, B,
                "RunGreedy" => o#"RunGreedyAll", Verbose => false);
        ) else ( ok = false; );
        if not ok then continue;
        if numRows M < bestRows or (numRows M == bestRows and numColumns M < bestCols) then (
            bestRows = numRows M;
            bestCols = numColumns M;
            bestM = M;
            bestB = B;
            if o.Verbose then
                << "  candidate #" << idx << ": " << bestRows << "x" << bestCols
                   << " (new best)" << endl;
        );
    );
    -- If we screened with MatrixHi only, re-run Greedy on the winner.
    if bestB =!= null and not o#"RunGreedyAll" then (
        local Mg;
        okG := true;
        try (
            Mg = buildGreedyTemplateWithBasis(aVar, J, bestB,
                "RunGreedy" => true, Verbose => false);
        ) else ( okG = false; );
        if okG and (numRows Mg < bestRows or
                (numRows Mg == bestRows and numColumns Mg < bestCols)) then (
            bestRows = numRows Mg;
            bestCols = numColumns Mg;
            bestM = Mg;
            if o.Verbose then
                << "  winner refined by Greedy: " << bestRows << "x" << bestCols << endl;
        );
    );
    (bestB, bestM)
);

-- Build the symbolic H in a FLAT extended ring.
-- Returns (Hsym, Rext, alphaVars, perRowData) where:
--   Rext = FF[base vars..., alpha_0, ..., alpha_{n-1}]
--   Hsym is nG x nF over Rext
--   alphaVars = list of alpha ring elements
--   perRowData[i] = (entMons, alphaRange) for row i
--
-- IMPORTANT: This handles the degree increment correctly — if the minimum
-- degree H[i,j] doesn't admit a solution, it incrementally raises d until
-- the linear system becomes feasible. This matches Maple's matrixHi.
buildHSymbolic = method(Options => {Verbose => false})
buildHSymbolic(List, List) := o -> (F, gapPolys) -> (
    R := ring first F;
    FF := coefficientRing R;
    nF := #F;
    nG := #gapPolys;
    degF := apply(F, f -> first degree f);

    -- For each row, find the right degree and solve
    rowResults := apply(nG, i -> (
        dG := first degree gapPolys#i;
        -- Try increasing d until feasible (same as Maple's matrixHi)
        d := 0;
        dMinMet := 0;
        chosenEm := null;
        chosenRref := null;
        chosenPivots := null;
        chosenNParams := 0;
	local em;
	local nParams;
        while d - dMinMet <= 0 do (
            em = apply(nF, j -> flatten entries basis(0, max(0, dG - degF#j) + d, R));
            nParams = sum(em, m -> #m);
            targetMons := unique flatten (
                flatten apply(nF, j -> flatten apply(em#j, mk -> flatten entries monomials(mk * F#j)))
                | flatten entries monomials(gapPolys#i)
            );
            Amat := matrix(FF, apply(targetMons, m -> (
                flatten apply(nF, j -> apply(em#j, mk -> coefficient(m, mk * F#j)))
            )));
            bvec := matrix(FF, apply(targetMons, m -> {coefficient(m, gapPolys#i)}));
            -- Check feasibility: rank(A) == rank(A|b)
            rkA := rank Amat;
            rkAug := rank(Amat | bvec);
            if rkAug > rkA then (
                -- Infeasible at this degree
                dMinMet = d + 1;
                d = d + 1;
                if d > 30 then error ("matrixHiRow: degree too high");
            ) else (
                chosenEm = em;
                chosenNParams = nParams;
                chosenRref = reducedRowEchelonForm(Amat | bvec);
                break;
            );
        );
        em = chosenEm;
        nParams = chosenNParams;
        rref := chosenRref;
        -- Identify pivots
        (curR, col) := (0, 0);
        pivotCols := while (curR < numrows rref and col < nParams) list (
	    oldCol := col;
	    col = col + 1;
	    if rref_(curR, oldCol) != 1_FF then continue else (
		curR = curR + 1;
		oldCol
	    )
        );
        pivotSet := set pivotCols;
        freeCols := toList select(0..nParams-1, c -> not pivotSet#?c);
        -- For each pivot, store: (pivot col, pivot row, list of (freeColIdx, -coef))
        pivotInfo := apply(pivotCols, col -> (
            rowIdx := position(0..numRows rref - 1, r -> rref_(r, col) == 1_FF);
            constant := rref_(rowIdx, nParams);
            deps := apply(freeCols, fc -> (fc, -rref_(rowIdx, fc)));
            (col, constant, deps)
        ));
        (em, nParams, pivotInfo, freeCols)
    ));

    -- Total free params across all rows
    totalFree := sum(rowResults, r -> #(r#3));
    if o.Verbose then << "buildHSymbolic: totalFree=" << totalFree << endl;

    -- Build flat extended ring
    baseNames := apply(numgens R, i -> toString R_i);
    alphaNames := apply(totalFree, k -> "aa" | toString k);
    Rext := if totalFree > 0 then
        FF[(baseNames | alphaNames) / getSymbol, MonomialOrder => {numgens R, totalFree}]
    else R;
    toRext := if totalFree > 0 then
        map(Rext, R, apply(numgens R, i -> Rext_i))
    else map(R, R);
    alphaVars := if totalFree > 0 then apply(totalFree, k -> Rext_(numgens R + k)) else {};

    -- Build Hsym row by row
    alphaOffset := 0;
    perRowData := new MutableList from apply(nG, i -> null);
    Hrows := apply(nG, i -> (
        (em, nParams, pivotInfo, freeCols) := rowResults#i;
        nFreeRow := #freeCols;
        rowAlphas := apply(nFreeRow, k -> alphaVars#(alphaOffset + k));

        -- Mapping from free col idx in this row → global alpha var
        freeIdxToAlpha := hashTable apply(nFreeRow, k -> freeCols#k => rowAlphas#k);

        -- Build alpha value expression for each param idx 0..nParams-1
        alphaExpr := new MutableList from apply(nParams, p -> 0_Rext);
        -- Set free cols to their corresponding alpha var
        scan(nFreeRow, k -> (
            alphaExpr#(freeCols#k) = rowAlphas#k;
        ));
        -- Set pivot cols to their expression: constant - sum (coef * alpha)
        scan(pivotInfo, pi -> (
            (col, constant, deps) := pi;
            val := promote(constant, Rext);
            scan(deps, dep -> (
                (fc, coef) := dep;
                if freeIdxToAlpha#?fc then
                    val = val + promote(coef, Rext) * freeIdxToAlpha#fc;
            ));
            alphaExpr#col = val;
        ));

        -- Build the row: for each gen j, sum (alphaExpr#p * entMon) over monomials of entMons#j
        rowPolys := apply(nF, j -> (
            pStart := sum(j, jj -> #(em#jj));
            sum(#(em#j), k -> alphaExpr#(pStart + k) * toRext(em#j#k))
        ));

        perRowData#i = (em, toList alphaExpr, rowAlphas);
        alphaOffset = alphaOffset + nFreeRow;
        rowPolys
    ));

    Hsym := matrix(Rext, Hrows);
    (Hsym, Rext, alphaVars, toList perRowData)
);

-- ============================================================================
-- adjustParams: a simpler, direct implementation of Martyushev CVPR 2022 §4.
--
-- Strategy: For each excessive monomial, directly collect the per-entry
-- coefficient expressions that contribute to it, build a linear system,
-- solve, and substitute into Hsym.
-- ============================================================================
-- Note: 5-arg method bundled as (F, Hsym, Rext, (alphaVars, RB))
adjustParams = method(Options => {Verbose => false, "MaxIter" => 1000})
adjustParams(List, Matrix, Ring, Sequence) := o -> (F, Hsym, Rext, pair) -> (
    (alphaVars, RB) := pair;
    R := ring first F;
    FF := coefficientRing R;
    nF := #F;
    nG := numRows Hsym;
    baseVars := apply(numgens R, i -> Rext_i);
    Fext := apply(F, f -> sub(f, Rext));
    RBset := set apply(RB, m -> sub(m, Rext));

    Hcurrent := mutableMatrix Hsym;

    -- Get current shift monomials per generator (in vars only)
    getShiftMons := (HM) -> (
        apply(nF, j -> (
            unique flatten apply(nG, i -> (
                e := HM_(i,j);
                if e == 0 then {} else (
                    (mm, cc) := coefficients(e, Variables => baseVars);
                    flatten entries mm
                )
            ))
        ))
    );

    -- Get excessive monomials
    getExcessive := (HM) -> (
        sh := getShiftMons HM;
        allExp := unique flatten apply(nF, j -> (
            flatten apply(sh#j, m -> flatten entries monomials(m * Fext#j, Variables => baseVars))
        ));
        select(allExp, m -> not RBset#?m)
    );

    initialExcessive := getExcessive(matrix Hcurrent);
    if o.Verbose then
        << "adjustParams: " << #initialExcessive << " initial excessive" << endl;

    if #initialExcessive == 0 then return matrix Hcurrent;

    -- Martyushev CVPR 2022 / `adjustParams` (Maple): sort excessives DESC by
    -- (|rm|, np) where rm[e] = set of (j,k) positions whose m*F[j] contains e,
    -- and np[e] = sum over positions p in rm[e] of (number of excessives using
    -- p). Intuition: attack excessives controlled by the most positions first,
    -- breaking ties by "downstream impact" so we spend positions that are most
    -- contested. Our earlier port sorted ASC and did not compute np.
    iterations := 0;
    remainingExc := initialExcessive;

    -- Cache `supp(m * F[j])` as a set for reuse across outer iterations. The
    -- support only depends on m and F[j]; α-commits don't invalidate it.
    suppCache := new MutableHashTable;
    suppOf := (j, m) -> (
        key := (j, m);
        if not suppCache#?key then
            suppCache#key = set flatten entries monomials(m * Fext#j);
        suppCache#key
    );

    -- Per-iteration cache of shift monomials per generator column. Rebuilt
    -- once at the start of each outer iteration instead of re-extracted per
    -- excessive.
    currentShifts := null;
    refreshShifts := HM -> (
        currentShifts = apply(nF, j -> (
            unique flatten apply(nG, i -> (
                entry := HM_(i,j);
                if entry == 0 then {} else (
                    (mm, cc) := coefficients(entry, Variables => baseVars);
                    flatten entries mm
                )
            ))
        ));
    );

    -- rm[e]: list of (j, m) pairs with m in column j's shift set AND e in
    -- supp(m * F[j]).
    rmOf := e -> (
        unique flatten apply(nF, j -> (
            apply(select(currentShifts#j, m -> (suppOf(j, m))#?e), m -> (j, m))
        ))
    );

    while iterations < o#"MaxIter" and #remainingExc > 0 do (
        iterations = iterations + 1;
        improved := false;

        -- Compute rm and np for every remaining excessive, using cached
        -- per-column shift lists and supp(m*F[j]) memoization.
        HcurMat := matrix Hcurrent;
        refreshShifts(HcurMat);
        rmTable := hashTable apply(remainingExc, e -> e => rmOf(e));
        posCounts := new MutableHashTable;
        for e in remainingExc do (
            for pos in rmTable#e do (
                posCounts#pos = if posCounts#?pos then posCounts#pos + 1 else 1;
            );
        );
        -- Sort DESC by (|rm|, np). M2 sort is ASC by default, so negate keys.
        exWithKey := apply(remainingExc, e -> (
            rmList := rmTable#e;
            rmSize := #rmList;
            np := sum prepend(0, apply(rmList, pos -> posCounts#pos));
            (- rmSize, - np, e)
        ));
        sortedEx := apply(sort exWithKey, t -> t#2);

        -- Early break (Martyushev): if every H entry is already numeric (no
        -- free α survives), adjusting anything further is impossible.
        allEntries := flatten apply(nG, i -> apply(nF, j -> Hcurrent_(i,j)));
        aliveAlphas := any(allEntries, e -> any(alphaVars, a -> coefficient(a, e) != 0_Rext));
        if not aliveAlphas then break;

        -- Try each in order
        tookStep := false;
        scan(sortedEx, e -> (
            if tookStep then return;

            -- Collect all entries of H that contribute to e (when expanded via F)
            -- For each gen j, find shift monomials m such that coefficient(e, m * F[j]) != 0
            -- Then for each row i, take coefficient(m, H[i,j]) — this is an expression in alphas
            -- that must be zero across the sum over rows

            -- The specific equation: sum_i [(total coeff of e contributed by m * F[j] row i)] = 0
            -- More precisely, for each (j, m) pair, for each row i, the contribution to e is
            --   coefficient(m, H[i,j]) * coefficient(e, m * F[j])
            -- Setting ALL row contributions to zero means: for each i with coefficient(e, m*F[j]) != 0,
            --   coefficient(m, H[i,j]) = 0
            -- (or we'd need the SUM to be zero, but that requires summing rows)

            -- Actually each row of H contributes a separate shifted polynomial to the template,
            -- so each row's contribution to e must be independently zero for e to not appear.
            -- So we get one equation per (i, j, m) tuple where m * F[j] contains e.

            eqs := flatten apply(nG, i -> (
                flatten apply(nF, j -> (
                    entry := Hcurrent_(i,j);
                    if entry == 0 then return {};
                    (mm, cc) := coefficients(entry, Variables => baseVars);
                    monList := flatten entries mm;
                    coefList := flatten entries cc;
                    -- For each monomial m in the entry, check if m * F[j] contains e
                    eqsForEntry := toList apply(#monList, k -> (
                        m := monList#k;
                        if coefficient(e, m * Fext#j) != 0_Rext then
                            coefList#k  -- the alpha-expression coefficient
                        else
                            null
                    ));
                    select(eqsForEntry, x -> x =!= null)
                ))
            ));

            -- Filter to nonzero
            nonzeroEqs := select(eqs, x -> x != 0_Rext);
            if #nonzeroEqs == 0 then return;

            -- Build linear system in alphaVars
            nAlphas := #alphaVars;
            sysRows := apply(nonzeroEqs, eq -> (
                -- Extract linear coefficients wrt alphaVars
                row := apply(nAlphas, k -> sub(coefficient(alphaVars#k, eq), FF));
                -- Constant term = eq with alphas set to 0
                constSub := map(Rext, Rext, baseVars | apply(nAlphas, k -> 0_Rext));
                constVal := sub(constSub eq, FF);
                row | {-constVal}
            ));

            -- Drop trivial all-zero rows
            sysRows = select(sysRows, r -> not all(r, x -> x == 0_FF));
            if #sysRows == 0 then (
                -- e is already zero, remove from list
                remainingExc = select(remainingExc, m -> m != e);
                improved = true;
                tookStep = true;
                return;
            );

            sysMat := matrix(FF, sysRows);
            coeffMat := sysMat_{0..nAlphas-1};
            rhsCol := sysMat_{nAlphas};

            -- Check consistency
            if rank(coeffMat | rhsCol) > rank coeffMat then return;  -- skip, not solvable

            -- Solve and extract assignment
            solRref := reducedRowEchelonForm(coeffMat | rhsCol);
            newAlphaVals := new MutableList from apply(nAlphas, k -> null);  -- null = unassigned
            curR := 0;
            for col from 0 to nAlphas - 1 do (
                if curR >= numRows solRref then break;
                if solRref_(curR, col) == 1_FF then (
                    newAlphaVals#col = sub(solRref_(curR, nAlphas), Rext);
                    curR = curR + 1;
                );
            );
            -- Set unassigned alphas to 0
            substImages := apply(nAlphas, k ->
                if newAlphaVals#k =!= null then newAlphaVals#k else 0_Rext);
            substMap := map(Rext, Rext, baseVars | substImages);

            -- Apply substitution to Hcurrent
            for i from 0 to nG - 1 do
                for j from 0 to nF - 1 do
                    Hcurrent_(i,j) = substMap(Hcurrent_(i,j));

            improved = true;
            tookStep = true;
            remainingExc = select(remainingExc, m -> m != e);
            if o.Verbose and iterations % 5 == 0 then (
                currExc := getExcessive(matrix Hcurrent);
                << "  iter " << iterations << ": eliminated e, " << #currExc << " remain" << endl;
            );
        ));

        if not improved then break;

        -- Recompute remaining excessive (some may have been zeroed as side effects)
        remainingExc = select(remainingExc, e -> (
            all(nF, j -> (
                sh := unique flatten apply(nG, i -> (
                    entry := Hcurrent_(i,j);
                    if entry == 0 then {} else (
                        (mm, cc) := coefficients(entry, Variables => baseVars);
                        flatten entries mm
                    )
                ));
                any(sh, m -> coefficient(e, m * Fext#j) != 0_Rext)
            ))
        ));
    );

    if o.Verbose then (
        finalExc := getExcessive(matrix Hcurrent);
        << "adjustParams: eliminated " << (#initialExcessive - #finalExc)
           << " of " << #initialExcessive << " excessive (in " << iterations << " iters)" << endl;
    );

    -- Project back to R (substituting all remaining alphas to 0)
    if #alphaVars > 0 then (
        nBase := numgens R;
        finalSubst := map(R, Rext, apply(nBase, i -> R_i) | apply(#alphaVars, k -> 0_R));
        matrix apply(nG, i -> apply(nF, j -> finalSubst(Hcurrent_(i,j))))
    ) else matrix Hcurrent
);

-- ============================================================================
-- buildTemplateFromH: from H, F, and RB = residualMons | Blist, build the
-- elimination template matrix M over the base field. For each generator F[j],
-- the shift monomials come from column j of H. The template columns are
-- [ excessMons | RB ].
-- ============================================================================
buildTemplateFromH = method(Options => {Verbose => false})
buildTemplateFromH(Matrix, List, List) := o -> (Hmat, F, RB) -> (
    R := ring first F;
    FF := coefficientRing R;
    nF := #F;
    nG := numRows Hmat;

    sh := apply(nF, j ->
        toList set flatten apply(nG, i -> flatten entries monomials Hmat_(i,j))
    );
    xF := flatten apply(nF, j -> apply(sh#j, m -> m * F#j));
    V0 := toList set flatten apply(xF, p -> flatten entries monomials p);
    RBset := set RB;
    Eexcess := rsort select(V0, m -> not RBset#?m);
    V := Eexcess | RB;
    M := matrix(FF, apply(xF, p -> apply(V, m -> coefficient(m, p))));
    if o.Verbose then
        << "buildTemplateFromH: " << numRows M << "x" << numColumns M
           << " (excess=" << #Eexcess << ")" << endl << flush;
    (sh, M, V, Eexcess)
)

end
