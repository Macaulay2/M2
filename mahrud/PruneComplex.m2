---------------------------------------------------------------------------
-- PURPOSE : Pruning chain complexes over polynomial and local rings
--
-- PROGRAMMERs : Mike Stillman and Mahrud Sayrafi
--
-- UPDATE HISTORY : created 4 January 2017, and later
--
-- NOTE : Pruning chain complexes over local rings is essential for many operations
--        implemented in LocalRings.m2.
---------------------------------------------------------------------------
newPackage(
    "PruneComplex",
    Version => "1", 
    Date => "January 14th, 2017",
    Authors => {
      {Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"},
      {Name => "Mahrud Sayrafi", Email => "mahrud@berkeley.edu",   HomePage => "http://ocf.berkeley.edu/~mahrud/"}
      },
    Headline => "Prunes a given complex over R, R_m, and R_p",
    PackageImports => {"Localization"},
    DebuggingMode => false,
    AuxiliaryFiles => true
    )

export {
    "freeRes",
    "toMutable",
    "toChainComplex",
--    "moveToEnd",
    "isScalar",
--    "findUnit",
--    "findAllUnits",
--    "findSparseUnit",
--    "pruneUnit",
    "pruneDiff",
    "pruneComplex",
    "isAcyclic",
--    "isHomotopic",
    "sizeMatrix",
    --Options:
    "UnitTest", "NoSort", "PruningMaps", "left", "right", "both", "whole"
    }


freeRes = method()
freeRes Module := M -> (
    -- TODO
    )
freeRes Ideal := I -> (
    -- Warning: only works for finite fields
    R := ring I;
    homogvar := local homogvar;
    S := (coefficientRing R)(monoid [gens R, homogvar]);
    Ih := ideal homogenize(sub(gens gb I, S), S_(numgens R));
    C1 := res(Ih, FastNonminimal=>true);
    phi := map(R,S,gens R | {1});
    phi C1
    )

-- TODO: let mutableComplex be a MutableChainComplex instead of a List
-- TODO: find better name for pruneMorph and mutableComplex

debug Core;
deleteRows = method()
deleteRows(MutableMatrix,ZZ,ZZ)    := (M, rfirst, rlast) -> ( rawDeleteRows(raw M, rfirst, rlast); M )

deleteColumns = method()
deleteColumns(MutableMatrix,ZZ,ZZ) := (M, cfirst, clast) -> ( rawDeleteColumns(raw M, cfirst, clast); M )

toMutable = method()
toMutable ChainComplex := (C) -> for i from 1 to length C list mutableMatrix C.dd_i

toChainComplex = method()
 -- FIXME keep the information about source and target modules for general complexes
toChainComplex List := (mutableComplex) -> (
    if #mutableComplex == 0 then error "toChainComplex: cannot build a chain complex without a ring.";
    R := ring mutableComplex#0; 
    chainComplex for M in mutableComplex list  map(R^(numrows M), R^(numcols M), matrix M)
    )

moveToEnd = method()
moveToEnd(List, ZZ, Sequence) := (mutableComplex,loc,rc) -> (
    -- Moves a given row and column to the end while keeping the neighbouring differentials compatible
    (r,c) := rc;
    -- swap rows and columns if in range
    if loc < 0 or loc >= #mutableComplex then error "out of range";
    r2 := numRows mutableComplex#loc - 1;
    rowSwap(mutableComplex#loc, r, r2);
    if loc > 0 then columnSwap(mutableComplex#(loc-1), r, r2);
    c2 := numColumns mutableComplex#loc - 1;
    columnSwap(mutableComplex#loc, c, c2);
    if loc < #mutableComplex-1 then rowSwap(mutableComplex#(loc+1), c, c2);
    mutableComplex
    )

isScalar = method()
isScalar RingElement := f -> f != 0 and liftable(f, coefficientRing ring f)

findUnit = method(Options => {UnitTest => isUnit})
findUnit Matrix                   :=
findUnit MutableMatrix            := opts ->  M                  -> findUnit(M, (0, 0), opts)
findUnit(Matrix, Sequence)        :=
findUnit(MutableMatrix, Sequence) := opts -> (M, pos) -> (
    -- start scanning at this location, row by row
    -- return the coordinates of the first unit or scalar
    (r, c) := pos;
    numcol := numColumns M;
    numrow := numRows M;
    if numcol == 0 or numrow == 0 then break;
    func := opts.UnitTest;
    dl := debugLevel >= 3;
    for i from c to numcol - 1 do
      if func M_(r, i) then (if dl then <<"Unit "<<M_(r, i)<<" at "<<(r, i)<<endl; return (r, i));
    for j from r+1 to numrow - 1 do
      for i from 0 to numcol - 1 do
        if func M_(j, i) then (if dl then <<"Unit "<<M_(j, i)<<" at "<<(j, i)<<endl; return (j, i));
    )

isMinimal = method(Options => options findUnit)
isMinimal Matrix        :=
isMinimal MutableMatrix := isMinimal Matrix := opts -> M -> findAllUnits(M, opts) === {}

findAllUnits = method(Options => options findUnit)
findAllUnits Matrix        :=
findAllUnits MutableMatrix := opts -> M -> (
    -- return all units or scalars in a matrix
    lastpos := (0, -1);
    while (pos := findUnit(M, (lastpos_0, lastpos_1 + 1), opts)) =!= null list (lastpos = pos; pos)
    )

localComplexity := (elt, row, col) -> (                                    -- TODO be smart here
    (size numerator elt) * (row/numerator/size//sum) * (col/numerator/size//sum)
    )

complexity := (elt, row, col) -> (
    (#(for i in row list if i != 0 then 1 else continue)-1) *
    (#(for j in col list if j != 0 then 1 else continue)-1)
    )

findSparseUnit = method(Options => {UnitTest => isUnit, NoSort => true})
findSparseUnit Matrix :=
findSparseUnit MutableMatrix := opts -> M -> (
    -- returns the coordinates of the unit or scalar with the sparsest row and column in a matrix
    if opts.NoSort == false then (
      c := runHooks(MutableMatrix, symbol findSparseUnit, (opts, M));
      if c =!= null then return c;
      );

    unit := findUnit (M, UnitTest => opts.UnitTest);
    if unit === null then (
      if debugLevel >= 1 then
        <<"No unit found"<<endl;
      return ((-1,-1),infinity);                                         --FIXME this is a hack
      );
    unit, 0
    )

-- TODO be smart here
addHook(MutableMatrix, symbol findSparseUnit, (opts, M) -> (
        func := if instance(ring M, LocalRing) then localComplexity else complexity;
        units := for unit in findAllUnits (M, UnitTest => opts.UnitTest) list (
            (r, c) := unit;
            elt := M_unit;
            row := flatten entries submatrix(M, {r},    );
            col := flatten entries submatrix(M,    , {c});
            cmp := func(elt, row, col);
            unit, cmp
            );
        if #units == 0 then (
            if debugLevel >= 1 then
            <<"No unit found"<<endl;
            return ((-1,-1),infinity);                                       --FIXME this is a hack
            );
        argMin := minPosition (units/last);
        unit := first units#argMin;
        cmp := last units#argMin;
        if debugLevel >= 1 then 
          << "Found sparse unit of complexity "<<cmp<< " at "<<unit<<": "<<M_unit<<endl;
        break (unit, cmp)))

pruneUnit = method(Options => {PruningMaps => false, UnitTest => isUnit})
pruneUnit(List, ZZ) := opts -> (mutableComplex, loc) -> (
    -- Input: a list of mutable matrices mutableComplex followed by an iterator loc
    -- Output: a list of mutable matrices mutableComplex of smaller size
    -- reduce mutableComplex#loc using the unit in the last row and column
    if debugLevel >= 1 then
      <<"   Removing a unit at the end of differential #"<<loc<<endl;
    M := mutableComplex#loc;
    clast := numColumns M - 1;
    rlast := numRows M - 1;
    inversePivot := if instance(ring M, LocalRing) then 
        1/(M_(rlast, clast))
      else 
        1/(leadCoefficient M_(rlast, clast));
    for c from 0 to clast - 1 do columnAdd(M, c, -M_(rlast, c) * inversePivot, clast);
    -- delete the last row and column of M
    deleteRows(M, rlast, rlast);
    deleteColumns(M, clast, clast);
    -- delete the corresponding row/column of adjacent differentials
    if loc > 0 then deleteColumns(mutableComplex#(loc-1), rlast, rlast);
    if loc < #mutableComplex-1 then deleteRows(mutableComplex#(loc+1), clast, clast);
    if debugLevel >= 2 and 
       not instance(ring M, LocalRing) and                 -- FIXME only b/c prune uses fraction fields
       prune HH_1 toChainComplex mutableComplex != 0        -- TODO is this the best spot check?
       then error "Somehow the complex is no longer a resolution";
    mutableComplex
    )

pruneMorphism = method()
pruneMorphism(List, MutableList, ZZ, Sequence) := (mutableComplex, pruneMorph, loc, unit) -> (
    M := mutableComplex#loc;
    R := ring M;
    (r, c) := unit;
    (rlast, clast) := (numRows M - 1, numColumns M - 1);
    f := mutableIdentity (R, numRows M);
    g := mutableIdentity (R, numColumns M);
    inversePivot := if instance(R, LocalRing) then 1/(M_(rlast, clast)) else 1/(leadCoefficient M_(rlast, clast));
    for i from 0 to numColumns g - 1 do g_(numRows g - 1, i) = -inversePivot * M_(rlast, i);
    rowSwap(f, r, numRows f - 1);
    rowSwap(g, c, numRows g - 1);
    deleteColumns(g, numColumns g - 1, numColumns g - 1);
    deleteColumns(f, numColumns f - 1, numColumns f - 1);
    pruneMorph#loc =       if pruneMorph#loc === null       then f else pruneMorph#loc * f;
    pruneMorph#(loc + 1) = if pruneMorph#(loc + 1) === null then g else pruneMorph#(loc + 1) * g;
    )

pruneDiff = method(Options => {PruningMaps => false, UnitTest => isUnit})
pruneDiff(ChainComplex, ZZ) := opts -> (C, loc) -> toChainComplex pruneDiff(toMutable C, loc, opts)
pruneDiff(List, ZZ)         := opts -> (mutableComplex, loc) -> (
    -- Prunes a single differential by reducing the unit, starting with the ones in sparcest row/column.
    M := mutableComplex#loc;
    if debugLevel >= 1 then <<"Pruning differential#"<<loc<<" of size "<<(numRows M,numColumns M)<<endl;
    pruneMorph := if opts.PruningMaps === true then pruneMorph = new MutableList
        else if instance(opts.PruningMaps, MutableList) then opts.PruningMaps;
    if opts.PruningMaps === true then pruneMorph#(#mutableComplex) = null;
    while (
        unit := findSparseUnit(M, UnitTest => opts.UnitTest);
        last unit =!= infinity
        ) do (
        moveToEnd(mutableComplex, loc, first unit);
        if opts.PruningMaps =!= false then pruneMorphism(mutableComplex, pruneMorph, loc, first unit);
        pruneUnit(mutableComplex, loc, opts);
        );
    if debugLevel >= 1 then
      <<"\tDifferential reduced to => "<<(numRows M,numColumns M)<<endl;
    mutableComplex, pruneMorph --FIXME figure out how to do this in pruneComplex
    )

pruneComplex = method(Options => {Strategy => left, PruningMaps => false, UnitTest => isUnit})
pruneComplex ChainComplex := opts -> C              -> toChainComplex pruneComplex(toMutable C, opts)
pruneComplex List         := opts -> mutableComplex -> (
    -- Prune a chain complex C into a free resolution by removing unit elements from the differentials
    l := length mutableComplex;
    if      opts.Strategy == left  then                              -- pruning the right one first
      for i from 1 to l do
        pruneDiff(mutableComplex,   l-i, PruningMaps => opts.PruningMaps, UnitTest => opts.UnitTest)
    else if opts.Strategy == right then                              -- pruning the left one first
      for i from 0 to l - 1 do
        pruneDiff(mutableComplex,     i, PruningMaps => opts.PruningMaps, UnitTest => opts.UnitTest)
    else if opts.Strategy == both  then                              -- pruning outside-in
      (unique splice for i from 1 to lift((l - l % 2)/2, ZZ) list (i, l-i)) / 
      (n -> pruneDiff(mutableComplex, n, UnitTest => opts.UnitTest))
    else if opts.Strategy == whole then                              -- pruning the sparsest unit
      while (
        units := for i from 0 to l - 1 list (
          M := mutableComplex_i;
          if debugLevel >= 1 then
            << "Looking for sparsest unit in differential #"<<i<<" ... ";
          findSparseUnit(M, UnitTest => opts.UnitTest)               --TODO only recheck the neighbours
          );
        loc := minPosition (units/last);
        unit := units#loc;
        last (units#loc) != infinity
        ) do (
        moveToEnd(mutableComplex, loc, first unit);                 --TODO how to do this in pruneDiff?
        pruneUnit(mutableComplex, loc, UnitTest => opts.UnitTest);
        );
    mutableComplex
    )

isAcyclic = method()
isAcyclic ChainComplex          :=  C     -> isAcyclic(C, 0)
isAcyclic(ChainComplex, Ideal)  := (C, I) -> isAcyclic(C, coker gens I)
isAcyclic(ChainComplex, Module) := (C, M) -> (
    D := prune HH C;
    if D#0 != M then return false;
    for i from 1 to length D do if D#i != 0 then return false;
    true
    )

isHomotopic = method()
isHomotopic(ChainComplex, ChainComplex) := (C, D) -> (
    -- TODO compare isomorphism of the first module and equality of homologies
    )

sizeMatrix = method()
sizeMatrix Matrix        :=
sizeMatrix MutableMatrix := M -> (
    if instance(ring M, LocalRing)
      then matrix (entries M / (e->e/numerator/size))
      else matrix (entries M / (e->e/size))
    )

beginDocumentation()
load (currentFileDirectory | "PruneComplex/doc.m2")

TEST ///
  R = ZZ/32003[a..f]
  I = ideal"abc-def,ab2-cd2-c,acd-b3-1"
  C = freeRes I
  D = pruneComplex(C, UnitTest => isScalar, Strategy => both)
  --prune HH D
  assert(D.dd^2 == 0)
  assert(isAcyclic(D, I))

  R = ZZ/32003[a..f]
  I = ideal"abc-def,ab2-cd2,acd-b3"
  C = freeRes I
  D = pruneComplex(C, UnitTest => isScalar)
  --prune HH D
  assert(D.dd^2 == 0)
  assert(isAcyclic(D, I))

  R = ZZ/32003[x,y,z]
  I = ideal"xyz+z5,2x2+y3+z7,3z5+y5"
  C = freeRes I
  D = pruneComplex(C, UnitTest => isScalar)
  --prune HH_1 D
  assert(D.dd^2 == 0)
  assert(isAcyclic(D,I))
///

TEST ///
  needsPackage "LocalRings"
  R = ZZ/32003[a..f]
  I = ideal"abc-def,ab2-cd2-c,-b3+acd"
  C = freeRes I 
  D = pruneComplex(C, UnitTest => isScalar)
  P = ideal"a,b,c,d,e,f"
  RP = localRing(R, P)
  E = D ** RP
  assert(E.dd^2 == 0)
  F = pruneComplex(E, UnitTest => isUnit)
  assert(F.dd^2 == 0)
  --assert(isAcyclic(D, I))
  --prune HH E -- FIXME this is all 0 because it's using fraction fields
  use R
  setMaxIdeal ideal gens R
  L = localResolution I
  L**RP == F
///

TEST ///
  needsPackage "LocalRings"
  R = ZZ/32003[a..d]
  I = monomialCurveIdeal(R,{1,3,4})
  C = freeRes I 
  D = pruneComplex(C, UnitTest => isScalar)
  P = ideal"a,b,c"
  M = ideal"a,b,c,d"
  RP = localRing(R, P)
  RM = localRing(R, M)
  E = D ** RP
  E' = D ** RM
  assert(E.dd^2 == 0)
  assert(E'.dd^2 == 0)

  F = pruneComplex(E, UnitTest => isUnit)
  assert(F.dd^2 == 0)
  --assert(isAcyclic(F, ideal(gens I ** RP)))
  G = liftUp F
  --prune HH G  -- FIXME this is all 0 because it's using fraction fields
  --assert(isAcyclic(G ** RP, ideal (RP ** gens I))
  
  F' = pruneComplex(E', UnitTest => isUnit)
  assert(F'.dd^2 == 0)
  --assert(isAcyclic(F', ideal(gens I ** RM)))
  G' = liftUp F'
  --prune HH G'  -- FIXME this is all 0 because it's using fraction fields
  --assert(isAcyclic(G' ** RP, ideal (RP ** gens I))

  use R
  setMaxIdeal ideal gens R
  L = localResolution I
  --assert(isHomotopic(L**RM, E'))
///

TEST ///
  R = ZZ/32003[a..f]
  I = ideal"abc-def,ab2-cd2-c,acd-b3-1"
  C = freeRes I
  D = pruneComplex(C, UnitTest => isScalar, Strategy => whole)
  D' = pruneComplex(C, UnitTest => isScalar)
  assert(D.dd^2 == 0)
  assert(D'.dd^2 == 0)
  --isHomotopic(D, D')
  --prune HH D
  assert(isAcyclic(D, I))
///



end--

///
--  Random stuff
uninstallPackage "PruneComplex"
restart
installPackage "PruneComplex"
viewHelp "pruneComplex"
restart
--path = prepend("~/src/m2/M2-local-rings/M2/Macaulay2/packages/", path)       -- Mike
needsPackage "PruneComplex"
needsPackage "LocalRings"
debugLevel=1
debug PruneComplex
debug LocalRings
elapsedTime check PruneComplex
elapsedTime check LocalRings

