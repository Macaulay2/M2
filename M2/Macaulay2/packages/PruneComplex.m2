---------------------------------------------------------------------------
-- PURPOSE : Pruning chain complexes over polynomial and local rings.
--
-- PROGRAMMERS : Mahrud Sayrafi and Mike Stillman.
--
-- UPDATE HISTORY : created 4 January 2017; last update 25 October 2017.
--
-- NOTE : Everything here works in the local, graded, and inhomogeneous case.
--        Algorithms are implemented in both the Macaulay2 language and C++ for speed.
--        pruneComplex may not treat degrees of free modules correctly when working
--        on a free chain complex rather than a free resolution.
---------------------------------------------------------------------------
newPackage(
    "PruneComplex",
    Version => "1.0",
    Date => "January 14th, 2017",
    Authors => {
        {Name => "Mahrud Sayrafi", Email => "mahrud@berkeley.edu",   HomePage => "http://ocf.berkeley.edu/~mahrud/"},
        {Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"}
        },
    Headline => "pruning chain complexes over polynomial and local rings",
    Keywords => {"Commutative Algebra", "Homological Algebra"},
    AuxiliaryFiles => true
    )

export {
    "toMutableComplex",
    "toChainComplex",
    "pruneComplex",
    "pruneUnit",
    "pruneDiff",
    "isScalar",
--    "findUnit",
--    "findAllUnits",
--    "findSparseUnit",
--    "isQuasiIsomorphism", -- see ChainComplexExtras.m2
--    "isCommutative",
--    "isAcyclic",
--    "isMinimal",
--    "freeRes",
--    "testM2",
--    "testEngine",
--  Options:
    "Direction", "PruningMap", "UnitTest"
    }

-- << "--------------------------------------------------------------------------------------" << endl;
-- << "-- The PruneComplex package is experimental.                                        --" << endl;
-- << "-- See the documentation and comments in the package to learn more.                 --" << endl;
-- << "--------------------------------------------------------------------------------------" << endl;

debug Core;

--=================================== Chain Complex Operations ===================================--
-- TODO see if the new Chain Complexes package can be incorporated

-- converts a ChainComplex into a list of mutable matrices
-- TODO: take an option to make sparse mutable matrices here
toMutableComplex = method()
toMutableComplex ChainComplex := C -> for i from min C to max C list mutableMatrix C.dd_(i+1)

-- Converts a list of mutable matrices into a ChainComplex.
-- TODO: make sure the information about source and target modules for general complexes are kept
toChainComplex = method()
toChainComplex List          :=  mComplex     -> (
    if #mComplex == 0 then error "toChainComplex: expected at least one differential map.";
    toChainComplex(mComplex, target matrix (mComplex#0))
    )
toChainComplex(List, Module) := (mComplex, F) -> (
    if #mComplex == 0 then error "toChainComplex: expected at least one differential map.";
    places := select(0..(length mComplex)-1, i -> mComplex_i != 0);
    len := if #places === 0 then 0 else max places - min places;
    chainComplex for i from 0 to len list (
        m := map(F, , matrix mComplex_i);
        F = source m; m)
    )

--=================================== Unit Finding Operations  ===================================--

isScalar = method()
isScalar RingElement := f -> f != 0 and liftable(f, coefficientRing ring f)

-- Returns the first unit in M after pos
-- A mutable matrix and a pair of coordinates -> a pair of coordinates for a unit or null
-- start scanning at this location, row by row, return the coordinates of the first unit or scalar
-- start from the beginning if no coordinate is given
-- caveat: how to find bigger than 1x1 units?
findUnit = method(Options => {UnitTest => isUnit})
findUnit Matrix                   :=
findUnit MutableMatrix            := opts ->  M                  -> findUnit(M, (0, 0), opts)
findUnit(Matrix, Sequence)        :=
findUnit(MutableMatrix, Sequence) := opts -> (M, pos) -> (
    -- start scanning at this location, row by row
    -- return the coordinates of the first unit or scalar
    (r, c) := pos;
    numrow := numRows M;
    numcol := numColumns M;
    if numcol == 0 or numrow == 0 then return null;
    func := opts.UnitTest;
    dl := debugLevel >= 3;
    for i from c to numcol - 1 do
      if func M_(r, i) then (if dl then <<"Unit "<<M_(r, i)<<" at "<<(r, i)<<endl; return (r, i));
    for j from r+1 to numrow - 1 do
      for i from 0 to numcol - 1 do
        if func M_(j, i) then (if dl then <<"Unit "<<M_(j, i)<<" at "<<(j, i)<<endl; return (j, i));
    )

-- Returns a list of all units
-- A mutable matrix -> List (of coordinates for all units or scalars in a matrix)
findAllUnits = method(Options => options findUnit)
findAllUnits Matrix        :=
findAllUnits MutableMatrix := opts -> M -> (
    -- return all units or scalars in a matrix
    lastpos := (0, -1);
    if numcols M == 0 or numrows M == 0 then return {};
    while (pos := findUnit(M, (lastpos_0, lastpos_1 + 1), opts)) =!= null list (lastpos = pos; pos)
    )

-- If matrix is over a local ring, return a matrix of number of terms in the numerator of each element;
-- if matrix is over a poly ring, return a matrix of number of terms of each element
sizeMatrix = method()
sizeMatrix Matrix        :=
sizeMatrix MutableMatrix := M -> (
    if instance(ring M, LocalRing)
      then matrix (entries M / (e->e/numerator/size))
      else matrix (entries M / (e->e/size))
    )

-- Returns the coordinates of the unit or scalar with the sparsest row and column in a matrix
-- A mutable matrix -> a coordinate pair and a complexity integer
-- lists all units, uses heuristics to calculate a complexity for each unit, then returns the simplest
-- uses different measures for poly ring and local ring; for poly ring returns the coordinates of the
-- unit with the sparsest row and column in a matrix; for local ring takes into account the sum of
-- number of terms in the numerators for all elements in row and column as well.
-- caveat: super time intensive
-- Note: if no unit found, returns ((-1,-1), infinity)
findSparseUnit = method(Options => {UnitTest => isUnit, Strategy => "NoSort"})
findSparseUnit Matrix :=
findSparseUnit MutableMatrix := opts -> M -> (
    if opts.Strategy =!= "NoSort" then (
      c := runHooks(MutableMatrix, symbol findSparseUnit, (opts, M));
      if c =!= null then return c;
      );
    unit := findUnit (M, UnitTest => opts.UnitTest);
    if unit === null then (
      if debugLevel >= 3 then << "No units found." << endl;
      return ((-1, -1), infinity);
      );
    (unit, 0)
    )

-- A heuristic measure to find the easiest unit to prune first
-- TODO this could be made more efficient
complexity := (elt, row, col) -> (
    if instance(ring elt, LocalRing) then
      (size numerator elt) * (row/numerator/size//sum) * (col/numerator/size//sum)
    else
      (#(for i in row list if i != 0 then 1 else continue)-1) *
      (#(for j in col list if j != 0 then 1 else continue)-1)
    )

-- TODO this could be made more efficient.
addHook(MutableMatrix, symbol findSparseUnit, (opts, M) -> (
        units := for unit in findAllUnits (M, UnitTest => opts.UnitTest) list (
            (r, c) := unit;
            elt := M_unit;
            row := flatten entries submatrix(M, {r},    );
            col := flatten entries submatrix(M,    , {c});
            cmp := complexity(elt, row, col);
            unit, cmp
            );
        if #units == 0 then (
            if debugLevel >= 3 then << "No unit found." << endl;
            return ((-1,-1),infinity);
            );
        argMin := minPosition (units/last);
        unit := first units#argMin;
        cmp := last units#argMin;
        if debugLevel >= 3 then
          << "Found sparse unit of complexity " << cmp << " at " << unit << ": " << M_unit << endl;
        break (unit, cmp)))

--============================ Pruning Operations ===================================--

deleteRows =    (M, rfirst, rlast) -> ( rawDeleteRows(raw M, rfirst, rlast); M )
deleteColumns = (M, cfirst, clast) -> ( rawDeleteColumns(raw M, cfirst, clast); M )

-- Moves a given row and column to the end while keeping the neighbouring differentials compatible
-- Then reduces mComplex#n using the unit in the last row and column
-- Input: a list of mutable matrices mComplex followed by an iterator n
-- Output: a list of mutable matrices mComplex of smaller size
pruneUnit = method(Options => {PruningMap => true, UnitTest => isUnit})
pruneUnit(List, ZZ, Sequence, List) := opts -> (mComplex, n, unit, pruningMorph) -> (
    if debugLevel >= 3 then
      <<"   Removing a unit from differential #"<<n<<endl;
    if n < 0 or n >= #mComplex then error "out of range";
    (r,c) := unit;
    R := ring mComplex#n;
    -- Initialize pruning maps if needed
    if opts.PruningMap =!= false then (
        if pruningMorph#n     === null then pruningMorph#n     = mutableIdentity(R, numRows mComplex#n);
        if pruningMorph#(n+1) === null then pruningMorph#(n+1) = mutableIdentity(R, numColumns mComplex#n);
        );
    -- Move to last row
    r2 := numRows mComplex#n - 1;
    rowSwap(mComplex#n, r, r2);
    if 0 < n then
      columnSwap(mComplex#(n-1), r, r2);
    if opts.PruningMap =!= false then
      columnSwap(pruningMorph#n, r, r2);
    -- Move to last column
    c2 := numColumns mComplex#n - 1;
    columnSwap(mComplex#n, c, c2);
    if n < #mComplex - 1 then
      rowSwap(mComplex#(n+1), c, c2);
    if opts.PruningMap =!= false then
      columnSwap(pruningMorph#(n+1), c, c2);
    -- Get the pivot
    inversePivot := if instance(R, LocalRing) then (mComplex#n_(r2, c2))^-1 else (leadCoefficient mComplex#n_(r2, c2))^-1;
    -- Clear the column
    for r from 0 to r2 do (
        f := (mComplex#n)_(r, c2);
        rowAdd(mComplex#n, r, -f * inversePivot, r2);
        if 0 < n then
            columnAdd(mComplex#(n-1), r2, f * inversePivot, r);
        if opts.PruningMap =!= false then
            columnAdd(pruningMorph#n, r2, f * inversePivot, r);
        );
    -- Clear the row
    for c from 0 to c2 do (
        f := (mComplex#n)_(r2, c);
        columnAdd(mComplex#n, c, -f * inversePivot, c2);
        if n < #mComplex - 1 then
            rowAdd(mComplex#(n+1), c2, f * inversePivot, c);
        if opts.PruningMap =!= false then
            columnAdd(pruningMorph#(n+1), c, -f * inversePivot, c2);
        );
    -- delete rows/columns
    deleteColumns(mComplex#n, c2, c2);
    if n < #mComplex-1 then
       deleteRows(mComplex#(n+1), c2, c2);
    if opts.PruningMap =!= false then
        deleteColumns(pruningMorph#(n+1), c2, c2);

    deleteRows(mComplex#n, r2, r2);
    if 0 < n then
        deleteColumns(mComplex#(n-1), r2, r2);
    if opts.PruningMap =!= false then
        deleteColumns(pruningMorph#n, r2, r2);

    mComplex
    )

-- Prunes a single differential by reducing the units in a while loop, starting with the ones in
-- sparcest row/column. Uses pruneUnit.
-- TODO: handle the case of twisted complexes and free modules with degrees (both are OK in the engine)
pruneDiff = method(Options => {PruningMap => true, UnitTest => isUnit})
pruneDiff(ChainComplex, ZZ) := opts -> (C, n) -> (
    if opts.PruningMap === true then (
        (D, pruningMorph) := pruneDiff(toMutableComplex C, n, opts);
        return (toChainComplex D, pruningMorph);
        );
    toChainComplex pruneDiff(toMutableComplex C, n, opts)
    )
pruneDiff(ChainComplex, ZZ, List) := opts -> (C, n, M) -> (
    if opts.PruningMap === true then (
        (D, pruningMorph) := pruneDiff(toMutableComplex C, n, M, opts);
        return (toChainComplex D, pruningMorph);
        );
    toChainComplex pruneDiff(toMutableComplex C, n, M, opts)
    )
pruneDiff(List, ZZ)         := opts -> (mComplex, n) -> (
    pruningMorph := new MutableList;
    if opts.PruningMap === true then (
        for i from 0 to #mComplex - 1 do pruningMorph#i = mutableIdentity(ring mComplex#0, numRows mComplex#i);
        pruningMorph#(#mComplex) = mutableIdentity(ring mComplex#0, numColumns mComplex#(#mComplex - 1));
        );
    pruningMorph = new List from pruningMorph;
    pruneDiff(mComplex, n, pruningMorph, opts)
    )
pruneDiff(List, ZZ, List)         := opts -> (mComplex, n, pruningMorph) -> (
    M := mComplex#n;
    if debugLevel >= 2 then
      << "Pruning differential #" << n << " of size " << (numRows M, numColumns M) << endl;
    while (
        unit := findSparseUnit(M, UnitTest => opts.UnitTest);
        last unit =!= infinity
        ) do pruneUnit(mComplex, n, first unit, pruningMorph, opts);
    if debugLevel >= 2 then
      << "\tDifferential reduced to => " << (numRows M, numColumns M) << endl;
    if opts.PruningMap === true then return (mComplex, pruningMorph);
    mComplex
    )

-- Prune a chain complex C into a free resolution by removing unit elements from the differentials
-- TODO: keep track of degrees of the free modules in M2 and in the Engine
-- TODO: give error if not given a free chain complex
pruneComplex = method(
    Options => {
        Strategy => Engine, -- set to null to use the methods above
        Direction => "left",
        PruningMap => true, -- TODO: grading may be incorrect if this is set to false
        UnitTest => isUnit -- TODO: detect when all units are scalars and choose that
        })
pruneComplex ChainComplex      := opts ->  C          -> pruneComplex(C, -1, opts)
pruneComplex(ChainComplex, ZZ) := opts -> (C, nsteps) -> (
    m := min C;
    mComplex := toMutableComplex C;
    (D, M) := pruneComplex(mComplex, nsteps, opts);
    F := if opts.PruningMap == true
    then source map(target C.dd_(m+1), , matrix M#0)
    else target matrix D#0;
    D = (toChainComplex(D, F))[-m];
    R := ring D;
    if opts.PruningMap == true then
      D.cache.pruningMap = map(C, D, i -> M#(i-m)//matrix);
    D
    )
pruneComplex List      := opts ->  mComplex          -> pruneComplex(mComplex, -1, opts)
pruneComplex(List, ZZ) := opts -> (mComplex, nsteps) -> (
    len := length mComplex;
    mComplex = mComplex/mutableMatrix;
    if nsteps == -1 then nsteps = len;
    pruningMorph := new MutableList;
    if opts.PruningMap === true then (
        for i from 0 to #mComplex - 1 do pruningMorph#i = mutableIdentity(ring mComplex#0, numRows mComplex#i);
        pruningMorph#(#mComplex) = mutableIdentity(ring mComplex#0, numColumns mComplex#(#mComplex - 1));
        );
    pruningMorph = new List from pruningMorph;
    if      opts.Strategy === Engine  then                              -- call rawPruneComplex
      return enginePruneComplex(mComplex, nsteps, opts)
    else if opts.Direction == "left"  then                              -- pruning the left one first
      for i from 0 to nsteps-1 do
        pruneDiff(mComplex,       i, pruningMorph, PruningMap => opts.PruningMap, UnitTest => opts.UnitTest)
    else if opts.Direction == "right" then                              -- pruning the right one first
      for i from 0 to nsteps-1 do
        pruneDiff(mComplex, len-i-1, pruningMorph, PruningMap => opts.PruningMap, UnitTest => opts.UnitTest)
    else if opts.Direction == "both"  then                              -- pruning outside-in
      (unique splice for i from 1 to lift((nsteps - nsteps % 2)/2, ZZ) list (i, len-i)) /
      (n -> pruneDiff(mComplex,   n, pruningMorph, PruningMap => opts.PruningMap, UnitTest => opts.UnitTest))
    else if opts.Direction == "best"  then                              -- pruning the sparsest unit
      while (
        units := for i from 0 to nsteps - 1 list (
          M := mComplex#i;
          if debugLevel >= 3 then << "Looking for sparsest unit in differential #" << i << endl;
          findSparseUnit(M, UnitTest => opts.UnitTest)               --TODO only recheck the neighbours
          );
        n := minPosition(units/last);
        unit := units#n;
        last unit != infinity
        ) do pruneUnit(mComplex, n, first unit, pruningMorph, PruningMap => opts.PruningMap, UnitTest => opts.UnitTest);
    (mComplex, pruningMorph)
    )

enginePruneComplex = method(Options => options pruneComplex) -- ++ {...}
enginePruneComplex List      := opts ->  C          -> enginePruneComplex(C, -1, opts)
enginePruneComplex(List, ZZ) := opts -> (C, nsteps) -> (
    R := ring C#0;
    flag := 0;
    -- See e/mutablecomplex.cpp for reference
    flag = flag | (if opts.PruningMap            then     1 else 0); -- See `help pruningMap` in M2
    flag = flag | (if false                      then     2 else 0); -- Delete pruned rows and columns
    flag = flag | (if false                      then     4 else 0); -- Only prune -1,+1
    flag = flag | (if opts.UnitTest === isScalar then     8 else 0); -- Only prune constants
    flag = flag | (if false                      then    16 else 0); -- Only prune functions with constants
    flag = flag | (if false                      then    32 else 0); -- Pruning for maximal ideal
    flag = flag | (if false                      then    64 else 0); -- Pruning for prime ideal
    flag = flag | (if true                       then  1024 else 0); -- Prune sparsest unit first
    flag = flag | (if false                      then  2048 else 0); -- Prune best matrix first
    flag = flag | (if opts.Direction === "right" then 65536 else 0); -- Prune the matrices in reverse order
    -- create the raw chain complex
    if debugLevel >= 2 then << "Using enginePruneComplex." << endl;
    A := rawMutableComplex(C/raw//toSequence);
    if debugLevel >= 2 then << A << endl;
    L := rawPruneBetti(A, nsteps, flag) // sum;
    -- prune the raw chain complex
    rawPruneComplex(A, nsteps, flag);
    if debugLevel >= 2 then << A << endl;
    B := rawPruneBetti(A, nsteps, flag);
    -- trim the complex
    D := apply(length C, i -> submatrix(C#i, 0..B#i-1, 0..B#(i+1)-1));
    if debugLevel >= 2 then << L - B//sum << endl;
    -- retrieve the pruning maps
    M := null;
    if flag & 1 == 1 then M = rawPruningMorphism(A, nsteps, flag)/map_R;
    (D, M)
    )

--================================= Testing and Checking Operations =================================--

-- Checks that source phi is quasi-isomorphic to target phi
-- Source: ChainComplexExtras.m2
isQuasiIsomorphism = method(Options => {LengthLimit => infinity})
isQuasiIsomorphism ChainComplexMap := Boolean => opts -> phi -> (
    C := cone phi;
    if all((min C,min(max C, opts.LengthLimit)), i -> (prune HH_i(C) == 0)) then true else false
    )
-- Checks that C is a resolution of N
isQuasiIsomorphism(ChainComplex, Ideal)  := Boolean => opts -> (C, I) -> isQuasiIsomorphism(C, coker gens I)
isQuasiIsomorphism(ChainComplex, Module) := Boolean => opts -> (C, N) -> (
    R := ring N;
    D := chainComplex map(N, R^0, 0);
    if C == 0 then return D == 0;
    M := {map(D_0, C_0, 1)} | for i from 1 to max(length C, length D) list map(D_i, C_i, 0);
    isQuasiIsomorphism map(D, C, i -> M#i)
    )

-- Checks that C is an acyclic chain complex
isAcyclic = C -> isQuasiIsomorphism(C, 0)

-- Checks whether there are any more units are left in the complex
-- Note: this only implies that the resolution is minimal in the local and graded case
isMinimal = method(Options => options findUnit)
isMinimal Matrix        :=
isMinimal MutableMatrix := Boolean => opts -> M -> 0 == #findAllUnits(M, opts)
isMinimal ChainComplex  := Boolean => opts -> C -> (
    if any(length C + 1, i -> not isMinimal(matrix (C.dd_i), opts)) then false else true
    )

-- Checks commutativity of chain complexes C and D with chain complex map M
isCommutative ChainComplexMap := Boolean => f -> (
    D := source f;
    C := target f;
    for i from 1 to max(length C, length D) do
      if C.dd_i * f_i - f_(i-1) * D.dd_i != 0 then
        return false;
    true
    )

load ("./PruneComplex/tests.m2")
beginDocumentation()
load ("./PruneComplex/doc.m2")

end--
