newPackage( "FastMinors",
Version => "1.2.4", Date => "March 13th, 2022", Authors => {
    {Name => "Boyana Martinova",
    Email=> "u1056124@utah.edu"
    },
    {Name => "Marcus Robinson",
    Email => "robinson@math.utah.edu",
    HomePage => "http://www.math.utah.edu/~robinson"
    },
    {Name => "Karl Schwede",
    Email=> "schwede@math.utah.edu",
    HomePage=> "http://www.math.utah.edu/~schwede"
    },
    {Name => "Yuhui (Wei) Yao",
    Email=> "yuhuiyao4ever@gmail.com"
    }
}, --this file is in the public domain
Headline => "faster linear algebra operations", PackageExports => {"RandomPoints"}, PackageImports => {"RandomPoints"}, DebuggingMode => true, Reload=>false)
export{
--  "selectSmallestTerms",
  "chooseSubmatrixSmallestDegree", --there are checks
  "chooseSubmatrixLargestDegree", --there are checks
  "chooseRandomSubmatrix",
  "reorderPolynomialRing", --there are checks
  "regularInCodimension", --there are checks  
  "chooseGoodMinors",
  "projDim", --there are checks
  "isRankAtLeast", --there are checks
  "getSubmatrixOfRank",
  "recursiveMinors",
  "isCodimAtLeast",
  "isDimAtMost",
  --"internalChooseMinor",
  --options to export
  "DetStrategy", --to pass on to calls of determinant
  "MinDimension", --option for projDim
  "Random",
  "LexLargest",
  "LexSmallest",
  "LexSmallestTerm",
  "RandomNonzero",
  "GRevLexLargest",
  "GRevLexSmallest",
  "GRevLexSmallestTerm",  
  "MaxMinors",
  "Points",
  "Rank", --a value for Strategy in isRankAtLeast
 -- "MutableSmallest",
 -- "MutableLargest",
  "Threads",
  "MinorsCache",
  "Modulus",
--  "MaxMinorsFunction", 
  "MinMinorsFunction",
  "CodimCheckFunction",
  "PeriodicCheckFunction",
  --"RecursiveMinors",
  --premade stratgies
  "Recursive",
  "StrategyDefault",
  "StrategyDefaultNonRandom",
  "StrategyDefaultWithPoints",
  "StrategyGRevLexSmallest",
  "StrategyLexSmallest",
  "StrategyRandom",
  "StrategyPoints",
  "StrategyCurrent",  
  "SPairsFunction",
  "PointOptions", --options to be based to the RandomPoints package
  "UseOnlyFastCodim",
  "RegularInCodimensionTutorial", --help file
  "FastMinorsStrategyTutorial"
}

protect MutableSmallest;
protect MutableLargest;

--***********************************
--*****Control options universally***
--***********************************
StrategyDefault = new OptionTable from {
    LexLargest => 0,
    LexSmallestTerm => 16,
    LexSmallest => 16,
    GRevLexSmallestTerm => 16,
    GRevLexSmallest => 16,
    GRevLexLargest => 0,
    Random => 16,
    RandomNonzero => 16,
    Points => 0
};

StrategyDefaultNonRandom = new OptionTable from {
    LexLargest => 0,
    LexSmallestTerm => 25,
    LexSmallest => 25,
    GRevLexSmallestTerm => 25,
    GRevLexSmallest => 25,
    GRevLexLargest => 0,
    Random => 0,
    RandomNonzero => 0,
    Points => 0
};

StrategyDefaultWithPoints = new OptionTable from {
    LexLargest => 0,
    LexSmallestTerm => 16,
    LexSmallest => 16,
    GRevLexSmallestTerm => 16,
    GRevLexSmallest => 16,
    GRevLexLargest => 0,
    Random => 0,
    RandomNonzero => 0,
    Points => 32
};

StrategyCurrent = new MutableHashTable from StrategyDefault;

StrategyGRevLexSmallest = new OptionTable from {LexLargest=>0, LexSmallestTerm => 0, LexSmallest=>0, GRevLexSmallestTerm => 50, GRevLexSmallest => 50, GRevLexLargest=>0,Random=>0,RandomNonzero=>0, Points => 0};

StrategyLexSmallest = new OptionTable from {LexLargest=>0, LexSmallestTerm => 50, LexSmallest=>50, GRevLexSmallestTerm=>0, GRevLexSmallest=>0, GRevLexLargest=>0,Random=>0,RandomNonzero=>0, Points => 0};

StrategyRandom = new OptionTable from {LexLargest=>0, LexSmallestTerm => 0, LexSmallest=>0, GRevLexSmallestTerm=>0, GRevLexSmallest=>0, GRevLexLargest=>0,Random=>100,RandomNonzero=>0, Points => 0};

StrategyPoints = new OptionTable from {LexLargest=>0, LexSmallestTerm => 0, LexSmallest=>0, GRevLexSmallestTerm=>0, GRevLexSmallest=>0, GRevLexLargest=>0,Random=>0, RandomNonzero=>0, Points => 100};

optPoints := {
    Strategy=>Default, 
    Homogeneous => false,  
    Replacement => Binomial,    
    ExtendField => true,
    PointCheckAttempts => 0,
    DecompositionStrategy => Decompose,
    NumThreadsToUse => 1,
    DimensionFunction => dim,
    Verbose => false
};

optRn := {
    Verbose => false,
    MaxMinors => ((x,y) -> (10*x + 8*log_(1.3)(y))),
    Strategy => StrategyDefault,
    DetStrategy => Cofactor,
    Modulus => 0,     
--    MaxMinorsFunction => , 
    MinMinorsFunction => ((x) -> 2*x + 3), 
    CodimCheckFunction => ((k) -> log_1.3(k+3)),
    PairLimit => 100,
    UseOnlyFastCodim => false, 
--    DegreeFunction => ( (t,i) -> ceiling((i+1)*t))
    SPairsFunction => (i -> ceiling(i^1.5)),
    PointOptions => optPoints
};

optInternalChooseMinor := {
    MutableSmallest => null,
    MutableLargest => null,
    Strategy => StrategyDefault,
    Verbose => false,
    PointOptions => optPoints
};

optProjDim := {
    MinDimension => 0,
    Verbose => false,
    Strategy => StrategyDefault,
    DetStrategy => Cofactor,
    MaxMinors => ((x,y) -> 5*x + 2*log_1.3(y)),
    PointOptions => optPoints
};

optIsRankAtLeast :=  {
    Verbose => false,
    DetStrategy => Rank,
    MaxMinors => null,
    Strategy => StrategyDefaultNonRandom,
    Threads => 1,
    PointOptions => optPoints
};

optChooseGoodMinors := {
    Verbose => false,
    Strategy => StrategyDefault,
    DetStrategy=>Cofactor,
    PeriodicCheckFunction => null,
    PointOptions => optPoints
};

optIsCodimAtLeast := {
    Verbose => false,
    PairLimit => 100,
    --DegreeFunction => ( (t,i) -> ceiling((i+1)*t))
    SPairsFunction => (i -> ceiling(i^1.5))
};


--************************************
-------------------------------------
--Choosing submatrices---------------
-------------------------------------
--************************************

chooseSubmatrixLargestDegree = method(Options => {});
--------------------------------------
--This command tries to find a n1xn1 submatrix with
--determinant of biggest possible degree.
--It does it by picking the largest element in the matrix,
--then removing that row and column, and repeating the process
--n1 times.
--It returns the list of rows and columns that determine the submatrix.
--------------------------------------


chooseSubmatrixLargestDegree(ZZ, Matrix) := opts -> (n1, M1) -> (
    rCt := numRows M1;
    cCt := numColumns M1;
    i := 0;
    j := 0;
    curM1 := M1;
    --degreeMatrx := matrix apply(entries M1, l1 -> apply(l1, i->degree i));
    curList := null;
    curMax := null;
    curCol := null;
    curRow := null;
    newCurCol := null;
    newCurRow := null;

    returnRowList := {};--maybe these should be mutable lists, we'll do that later...
    returnColList := {};
    --keepRowList := {};
    --keepColList := {};

    while (i < n1) do (
        --print concatenate("in loop, i =", toString(i));
        curList = flatten entries curM1;
        curMax = randomMaxPosition curList;
        --print (curList#curMax);
        curRow = curMax // cCt;
        curCol = curMax % cCt;
        curM1 = submatrix'(curM1, {curRow}, {curCol});
        rCt = rCt - 1;
        cCt = cCt - 1;
        --we need to adjust based on submatrices
        newCurRow = curRow;
        j = (#returnRowList) - 1;
        --and for columns
        newCurCol = curCol;
        j = (#returnColList) - 1;
        returnRowList = append(returnRowList, newCurRow);
        returnColList = append(returnColList, newCurCol);
        i = i + 1;
    );
    --print returnRowList;
    returnRowList = new MutableList from returnRowList;
    returnColList = new MutableList from returnColList;
    j = n1-1;
    --when we removed rows and columns, the numbering probably got off.
    --We are fixing that in the following two loops.
    while (j >= 0) do ( --compare entry j
        newCurRow = returnRowList#j;
        i = j-1;
        --print "here1";
        while  (i >= 0) do (--compare entry j to previous terms in the list
            --print "here2";
            if (newCurRow >= returnRowList#i) then (newCurRow = newCurRow + 1;);
            i = i-1;
        );
        returnRowList#j = newCurRow;
        j = j-1;
    );
    j = n1-1;
    while (j >= 0) do ( --compare entry j
        newCurCol = returnColList#j;
        i = j-1;
        while  (i >= 0) do (--compare entry j to previous terms in the list
            if (newCurCol >= returnColList#i) then (newCurCol = newCurCol + 1;);
            i = i-1;
        );
        returnColList#j = newCurCol;
        j = j-1;
    );
    return {new List from returnRowList, new List from returnColList};
);

--the following commands use chooseSubmatrixLargestDegree
--to return the actual submatrix.

chooseMinorLargestDegree = method(Options => {});
chooseMinorLargestDegree(ZZ, Matrix) := o -> (n1, M1) -> (
    bestDegree := chooseSubmatrixLargestDegree(n1, M1);
    minorRowList := bestDegree#0;
    minorColList := bestDegree#1;
    return (M1^minorRowList)_minorColList;
)

chooseMinorLargestDegree(ZZ, MutableMatrix) := o -> (n1, M1) -> (
    return new Matrix from chooseMinorLargestDegree(n1, matrix(M1), o);
  );

  --------------------------------------
  --This command finds an n1xn1 submatrix
  --This selects it randomly, but it makes sure there is a nonzero entry in each row and column.
  --------------------------------------


chooseRandomNonzeroSubmatrix = method(Options => {});

chooseRandomNonzeroSubmatrix(ZZ, Matrix) := opts -> (n1, M1) -> (
  rCt := numRows M1;
  cCt := numColumns M1;
  i := 0;
  j := 0;
  curM1 := M1;
  curList := null;
  curEntry := null;
  entryList := {};
  curCol := null;
  curRow := null;
  newCurCol := null;
  newCurRow := null;

  returnRowList := {};--maybe these should be mutable lists, we'll do that later...
  returnColList := {};

  while (i < n1) do (
      --print concatenate("in loop, i =", toString(i));
      --curList = flatten entries curM1;
      entryList = entries transpose matrix nonzeroEntries(curM1);
      if #entryList == 0 then return null;
      curEntry = entryList#(random(#entryList));
      --print (curList#curMax);
      curRow = curEntry#0;
      curCol = curEntry#1;
      curM1 = submatrix'(curM1, {curRow}, {curCol});
      rCt = rCt - 1;
      cCt = cCt - 1;
      --we need to adjust based on submatrices
      newCurRow = curRow;
      --print curRow;
      j = (#returnRowList) - 1;
      --and for columns
      newCurCol = curCol;
      j = (#returnColList) - 1;
      returnRowList = append(returnRowList, newCurRow);
      returnColList = append(returnColList, newCurCol);
      i = i + 1;
  );
  returnRowList = new MutableList from returnRowList;
  returnColList = new MutableList from returnColList;
  j = n1-1;
  --when we removed rows and columns, the numbering probably got off.
  --We are fixing that in the following two loops.
  while (j >= 0) do ( --compare entry j
      newCurRow = returnRowList#j;
      i = j-1;
      --print "here1";
      while  (i >= 0) do (--compare entry j to previous terms in the list
          --print "here2";
          if (newCurRow >= returnRowList#i) then (newCurRow = newCurRow + 1;);
          i = i-1;
      );
      returnRowList#j = newCurRow;
      j = j-1;
  );
  j = n1-1;
  while (j >= 0) do ( --compare entry j
      newCurCol = returnColList#j;
      i = j-1;
      while  (i >= 0) do (--compare entry j to previous terms in the list
          if (newCurCol >= returnColList#i) then (newCurCol = newCurCol + 1;);
          i = i-1;
      );
      returnColList#j = newCurCol;
      j = j-1;
  );
  return {new List from returnRowList, new List from returnColList};
);

--------------------------------------
--This command takes in a matrix, and replaces the zeros with high degree polynomials
--------------------------------------

myAmbient := R1 -> (
    try ambient R1 then ambient R1 else R1
);

replaceZeros= method(Options=>{Strategy=>null});

replaceZeros(Matrix):= Matrix => opts->(M2) -> (
    --Mute := mutableMatrix M2;
      --we aren't using a strategy that cares, forget about it
    if (not ((opts.Strategy === null) or (opts.Strategy === LexSmallest) or (opts.Strategy === LexSmallestTerm) or (opts.Strategy === GRevLexSmallest) or (opts.Strategy === GRevLexSmallestTerm) ) ) then (
        if (opts.Strategy === GRevLexLargest) or (opts.Strategy === LexLargest) or (opts.Strategy === Random) or (opts.Strategy=== RandomNonzero) or (opts.Strategy === Points) then (          
            return M2;
        )        
        else if not (((opts.Strategy)#LexSmallest > 0) or ((opts.Strategy)#LexSmallestTerm > 0) or ((opts.Strategy)#GRevLexSmallest > 0) or ((opts.Strategy)#GRevLexSmallestTerm > 0)) then (              
            return M2;
        );        
    );
    m := numRows M2;
    n := numColumns M2;
    M2ent := flatten entries M2;
    largeDeg := 1;
    if (#M2ent > 0) then largeDeg = 2*max(largeDeg, max(flatten apply(flatten entries M2, z->degree z)));

    largeGen:= null;
    if (instance(ring M2, PolynomialRing) or instance(ring M2, QuotientRing)) then (largeGen = (product gens myAmbient ring M2)^(2*largeDeg+2)) else (largeGen = (max(flatten entries M2))^2);
    if (sub(largeGen, ring M2) == 0) then (largeGen = (max(flatten entries M2))^2);
    if (sub(largeGen, ring M2) == 0) then (largeGen == sub(1, ring M2));
    largeGen = sub(largeGen, ring M2);
    unMute := matrix apply(entries M2, c -> apply(c, i->(if (i == 0) then largeGen else i)));

    return unMute;
);

selectSmallestTerms = method(Options=>{});
--this function takes a matrix and replaces each entry with the smallest monomial term
--this is useful when we want to find the minor with the smallest term
myTerms := f3 -> (
    try terms f3 then terms f3 else {f3}
);

selectSmallestTerms(Matrix) := Matrix => o->(M2) -> (
    entryList := entries M2;
    newEntryList := apply(entryList, myRow -> apply(myRow, f2 -> min myTerms f2));
    matrix newEntryList
);

chooseSubmatrixSmallestDegree = method(Options=>{});
    --------------------------------------
    --This command tries to find a n1xn1 submatrix with
    --determinant with a smallest term
    --Frequently this is used in concert with selectSmallestTerms which replaces
    --the entries of a matrix with their smallest term.
    --It does this by choosing the smallest (with respect to the monomial order)
    --term of the matrix, removing that row and column, and repeating
    --n1 times.
    --It returns the list of rows and columns that determine the submatrix
    --------------------------------------

chooseSubmatrixSmallestDegree(ZZ, Matrix) := o -> (n1, M3) -> (          
          M1 := new Matrix from M3;
          rCt := numRows M1;
          cCt := numColumns M1;
          i := 0;
          j := 0;
          curM1 := M1;
          curList := null;
          curMax := null;
          curCol := null;
          curRow := null;
          newCurCol := null;
          newCurRow := null;

          returnRowList := {};
          returnColList := {};

          while (i < n1) do (
              --print concatenate("in loop, i =", toString(i));
              curList = flatten entries curM1;
              curMax = randomMinPosition curList;
              curRow = curMax // cCt;
              curCol = curMax % cCt;
              curM1 = submatrix'(curM1, {curRow}, {curCol});
              rCt = rCt - 1;
              cCt = cCt - 1;
              newCurRow = curRow;
              j = (#returnRowList) - 1;
              newCurCol = curCol;
              j = (#returnColList) - 1;
              returnRowList = append(returnRowList, newCurRow);
              returnColList = append(returnColList, newCurCol);
              i = i + 1;
          );
          returnRowList = new MutableList from returnRowList;
          returnColList = new MutableList from returnColList;
          j = n1-1;
          while (j >= 0) do (
              newCurRow = returnRowList#j;
              i = j-1;
              while  (i >= 0) do (
                  if (newCurRow >= returnRowList#i) then (newCurRow = newCurRow + 1;);
                  i = i-1;
              );
              returnRowList#j = newCurRow;
              j = j-1;
          );
          j = n1-1;
          while (j >= 0) do (
              newCurCol = returnColList#j;
              i = j-1;
              while  (i >= 0) do (
                  if (newCurCol >= returnColList#i) then (newCurCol = newCurCol + 1;);
                  i = i-1;
              );
              returnColList#j = newCurCol;
              j = j-1;
          );
          return {new List from returnRowList, new List from returnColList};
      );

--these functions just return the submatrix the previous function identifies.

chooseMinorSmallestDegree = method(Options => {});
chooseMinorSmallestDegree(ZZ, MutableMatrix) := o -> (n1, M1) -> (
    return chooseMinorSmallestDegree(n1, matrix(M1), o);
  );

  chooseMinorSmallestDegree(ZZ, Matrix) := o -> (n1, M1) -> (
      bestDegree := chooseSubmatrixSmallestDegree(n1, M1);
      minorRowList := bestDegree#0;
      minorColList := bestDegree#1;
      return (M1^minorRowList)_minorColList;
  );

--the following command chooses a *random* maximum element of a list
randomMaxPosition = method(Options=>{});
randomMaxPosition(List) := o -> (L1) -> (
    newList := apply(#L1, i -> {L1#i, random((#L1)^2), i});
    newList2 := random(newList);
    j := maxPosition(newList2);
    return ((newList2#j)#2);
);

--the following command chooses a *random* minimum element of a list
randomMinPosition = method(Options=>{});
randomMinPosition(List) := o -> (L1) -> (
    newList := apply(#L1, i -> {L1#i, random((#L1)^2), i});
    newList2 := random(newList);
    j := minPosition(newList2);
    return ((newList2#j)#2);
);

--the following command chooses n1 *random* minimum element of a list
randomMinPositions = method(Options=>{});
randomMinPositions(ZZ, List) := o -> (n1, L1) -> (
    newList := apply(#L1, i -> {L1#i, random((#L1)^2), i});
    newList2 := random(newList);
    newList3 := sort(newList2);
    return apply(take(n1, newList3), z -> z#0);
);


--this function replaces one of the smallest terms in the matrix by a larger term,
-- hopefully then allowing us to identify the next smallest term
-- in fact, it chooses one of the entries we picked when
--identifying our smallest matrix and randomly increases it.
replaceSmallestTerm= method(Options=>{});
replaceSmallestTerm(List, MutableMatrix) := opts -> (submatrixS, M1) -> (
    ambR:= myAmbient ring(M1);
    rowListS := submatrixS#0;
    colListS := submatrixS#1;
    mutedSM := M1;
    M2 := sub(matrix M1, myAmbient ring M1);
    myRand := random(#rowListS);
    moddedRow := rowListS#(myRand);
    moddedCol := colListS#(myRand);
    val := (M1_(moddedRow, moddedCol))*(random(1, ambR));
    if (val == 0) then val = sub((max(flatten entries M2))^2, ring M1);
    mutedSM_(moddedRow, moddedCol) = val;
    return mutedSM;
);

--this works similarly to the above, but it doesn't choose a random term among
--the largest, it really picks the biggest one.   First we drop the extra terms,
--then we replace the term by a smaller degree term.
replaceLargestTerm= method(Options=>{});
replaceLargestTerm(List, MutableMatrix):= opts-> (submatrixL, M1)->(
    --submatrixL := chooseSubmatrixLargestDegree(FR, M1);
    rowListL := submatrixL#0;
    colListL := submatrixL#1;
    largeSubmatrix :=(M1^rowListL)_colListL;
    termLists := flatten entries monomials(M1_(rowListL#0, colListL#0));
    local myRand;

    muted := M1;
    if (#termLists > 1) then ( --drop the smallest degree term basically, and sum what's left
        minTerm := randomMinPosition(termLists);
        muted_(rowListL#0, colListL#0) = sum(drop(termLists, {minTerm, minTerm}));
    )
    else if (#termLists == 1) then (
        minTerm = new MutableList from factor(termLists#0);
        if (#minTerm > 0) then (
            myRand = random(#minTerm);
            minTerm#myRand = new Power from {(minTerm#myRand)#0, (minTerm#myRand#1) - 1};
            muted_(rowListL#0, colListL#0) = value (new Product from minTerm);
        )
        else (
            muted_(rowListL#0, colListL#0) = sub(0, ring M1);
        );
        j := minPosition
  )
  else(
      muted_(rowListL#0, colListL#0) = 0;
      );
  --replaced := new Matrix from muted;
  return muted;
);

--this just chooses a random submatrix
chooseRandomSubmatrix = method(Options=>{});

chooseRandomSubmatrix(ZZ, Matrix) := opts -> (n1, M1) ->
(
    rowL := random(toList(0..(numRows M1 - 1)));
    colL := random(toList(0..(numColumns M1 - 1)));
    rowL = sort take(rowL, n1);
    colL = take(colL, n1);
    return {rowL, colL};
    )


--this takes in a list of locations in a matrix determining a submatrix
--and returns the sorted list of rows and columns.
locationToSubmatrix = method();

locationToSubmatrix(List) := (L1) ->
(  {sort(L1#0), sort(L1#1)}  );


--in our functions, we choose smallest elements in our matrices, with
--respect to lots of different monomial orders, so we need code to
--change the polynomial ring order, randomly
reorderPolynomialRing = method(Options=>{});

reorderPolynomialRing(Symbol, Ring) := opts-> (myOrder, R1) -> (
    if instance(R1, PolynomialRing) then(
        if (debugLevel > 0) then print "reorderPolynomialRing: it is a polynomialRing";
        coeff := coefficientRing R1;
        genList := generators R1;
        newGenList := random(genList);
        return coeff[newGenList, MonomialOrder => myOrder];)
    else (return R1);
);

--this just gets the det of a submatrix, it has some strategy options
getDetOfSubmatrix = method(Options=>{DetStrategy=>Cofactor});

getDetOfSubmatrix(Matrix, List) := opts -> (M2, submat) -> (
    mRowListS := submat#0;
    mColListS := submat#1;
    smallestSubmatrix :=(M2^mRowListS)_mColListS;

    if (opts.DetStrategy === Recursive) then (
        J := recursiveMinors(#(submat#0), smallestSubmatrix);
        return first first entries gens J;
    );

    return determinant(smallestSubmatrix, Strategy=>opts.DetStrategy);-- take determinant
);

--this function is used in the choose nonzero matrix bit
nonzeroEntries = method(Options=>{});
nonzeroEntries (Matrix):= opts -> (M1) ->(
  entryList := flatten entries M1;
  numTerms := #entryList;

  n := numColumns M1;

  i:=0;
  thatTerm:= null;
  nonzeroRowList := {};
  nonzeroColList := {};

  nzRowNum := null;
  nzColNum := null;
  while (i<numTerms) do (
    if not (entryList#i ==0) then (
      nzRowNum = i//n;
      nzColNum = i%n;
      nonzeroRowList = append(nonzeroRowList, nzRowNum);
      nonzeroColList = append(nonzeroColList, nzColNum);
      );
    i=i+1;
    );
    return {nonzeroRowList, nonzeroColList};
  );



--this function checks Rn via reduction mod p
RnReductionP = method(Options=>optRn);
RnReductionP(ZZ, Ring, ZZ):= opts -> (n1, R1, p)-> (
    ambR := myAmbient R1;
    genList := generators(ambR);
    ambRing := ZZ/p[genList];

    Id := sub(ideal R1, ambRing);

    return regularInCodimension(n1, ambRing/Id, opts);
);

verifyStrategy := (passedStrat) -> (
        if (passedStrat === LexLargest) then return true;
        if (passedStrat === LexSmallestTerm) then return true;
        if (passedStrat === LexSmallest) then return true;
        if (passedStrat === GRevLexSmallestTerm) then return true;
        if (passedStrat === GRevLexSmallest) then return true;
        if (passedStrat === GRevLexLargest) then return true;
        if (passedStrat === Random) then return true;
        if (passedStrat === RandomNonzero) then return true;        
        if (passedStrat === Points) then return true;  
        --otherwise we verify it is a more complicated strategy
        instance(passedStrat, HashTable)         
        and (passedStrat #? LexLargest) 
        and (passedStrat #? LexSmallestTerm)
        and (passedStrat #? LexSmallest)
        and (passedStrat #? GRevLexSmallestTerm)
        and (passedStrat #? GRevLexSmallest)
        and (passedStrat #? GRevLexLargest)
        and (passedStrat #? Random)
        and (passedStrat #? RandomNonzero)    
        and (passedStrat #? Points)    
);

--an internal function which chooses a minor, based on chance
internalChooseMinor = method(Options=>optInternalChooseMinor);

internalChooseMinor(ZZ, Ideal, Matrix, Matrix) := opts -> (minorSize, I1, nonzeroM, M1) -> (
    --if (opts.Verbose or (debugLevel > 0)) then print "internalChooseMinor: starting.";
    passedStrat := opts.Strategy;
    tempHash := new MutableHashTable from {LexLargest => 0, LexSmallestTerm => 0, LexSmallest => 0, GRevLexSmallestTerm => 0, GRevLexSmallest => 0, GRevLexLargest => 0, Random => 0, RandomNonzero => 0, Points => 0};
    if (passedStrat === LexLargest) then (tempHash#LexLargest = 100; passedStrat = tempHash);
    if (passedStrat === LexSmallestTerm) then (tempHash#LexSmallestTerm = 100; passedStrat = tempHash);
    if (passedStrat === LexSmallest) then (tempHash#LexSmallest = 100; passedStrat = tempHash);
    if (passedStrat === GRevLexSmallestTerm) then (tempHash#GRevLexSmallestTerm = 100; passedStrat = tempHash);
    if (passedStrat === GRevLexSmallest) then (tempHash#GRevLexSmallest = 100; passedStrat = tempHash) ;
    if (passedStrat === GRevLexLargest) then (tempHash#GRevLexLargest = 100; passedStrat = tempHash);
    if (passedStrat === Random) then (tempHash#Random = 100; passedStrat = tempHash);
    if (passedStrat === RandomNonzero) then (tempHash#RandomNonzero = 100; passedStrat = tempHash);        
    if (passedStrat === Points) then (tempHash#Points = 100; passedStrat = tempHash);        

    totalPercent :=  passedStrat#LexSmallest + passedStrat#LexSmallestTerm + passedStrat#LexLargest + passedStrat#GRevLexSmallest + passedStrat#GRevLexSmallestTerm + passedStrat#GRevLexLargest + passedStrat#Random + passedStrat#RandomNonzero + passedStrat#Points;
    myRandom := random(totalPercent);
    submatrixS1 := null;
    ambR := ring I1;
    local R2;
    local f;
    local o;
    mutM2 := opts.MutableSmallest;
    mutM1 := opts.MutableLargest;
    local M2;
    --if any(flatten entries matrix mutM2, z->z==0) then error "internalChooseMinor: expected a matrix with no zero entries.";
    if (myRandom < passedStrat#LexSmallest) then (
        R2 = reorderPolynomialRing(Lex, ambR); --do the same with respect to a Lex ordering
        f = map(R2, ambR);
        M2 = f(nonzeroM);
        submatrixS1 = chooseSubmatrixSmallestDegree(minorSize, M2);
        if (opts.Verbose) or debugLevel > 1 then print "internalChooseMinor: Choosing LexSmallest";
    )
    else if (myRandom < passedStrat#LexSmallest + passedStrat#LexSmallestTerm) then (
        R2 = reorderPolynomialRing(Lex, ambR); --do the same with respect to a Lex ordering
        f = map(R2, ambR);
        M2 = f(nonzeroM);
        submatrixS1 = chooseSubmatrixSmallestDegree(minorSize, selectSmallestTerms M2);
        if (opts.Verbose) or debugLevel > 1 then print "internalChooseMinor: Choosing LexSmallestTerm";
    )
    else if (myRandom < passedStrat#LexSmallest + passedStrat#LexSmallestTerm + passedStrat#LexLargest) then
    (
        R2 = reorderPolynomialRing(Lex, ambR); --do the same with respect to a Lex ordering
        f = map(R2, ambR);
        M2 = f(M1);
        submatrixS1 = chooseSubmatrixLargestDegree(minorSize, M2);
        if (opts.Verbose) or debugLevel > 1 then print "internalChooseMinor: Choosing LexLargest";
    )
    else if (myRandom < passedStrat#LexSmallest + passedStrat#LexSmallestTerm + passedStrat#LexLargest + passedStrat#GRevLexSmallest) then
    (
        R2 = reorderPolynomialRing(GRevLex, ambR);
        f = map(R2, ambR);
        M2 = f(matrix mutM2); --put the matrix in the ring with the new order
        submatrixS1 = chooseSubmatrixSmallestDegree(minorSize, M2);
        mutM2 = replaceSmallestTerm(submatrixS1, mutM2);
        if (opts.Verbose) or debugLevel > 1 then print "internalChooseMinor: Choosing GRevLexSmallest";
    )
    else if (myRandom < passedStrat#LexSmallest + passedStrat#LexSmallestTerm + passedStrat#LexLargest + passedStrat#GRevLexSmallest + passedStrat#GRevLexSmallestTerm) then
    (
        R2 = reorderPolynomialRing(GRevLex, ambR);
        f = map(R2, ambR);
        M2 = f(matrix mutM2); --put the matrix in the ring with the new order
        submatrixS1 = chooseSubmatrixSmallestDegree(minorSize, selectSmallestTerms M2);
        mutM2 = replaceSmallestTerm(submatrixS1, mutM2);
        if (opts.Verbose) or debugLevel > 1 then print "internalChooseMinor: Choosing GRevLexSmallestTerm";
    )
    else if (myRandom < passedStrat#LexSmallest + passedStrat#LexSmallestTerm + passedStrat#LexLargest + passedStrat#GRevLexSmallest + passedStrat#GRevLexSmallestTerm + passedStrat#GRevLexLargest ) then (
        R2 = reorderPolynomialRing(GRevLex, ambR);
        f = map(R2, ambR);
        M2 = f(matrix mutM1); --put the matrix in the ring with the new order
        submatrixS1 = chooseSubmatrixLargestDegree(minorSize, M2);
        mutM1 = replaceLargestTerm(submatrixS1, mutM1);
        if (opts.Verbose) or debugLevel > 1 then print "internalChooseMinor: Choosing GRevLexLargest";
        --this needs to be written
    )
    else if (myRandom < passedStrat#LexSmallest + passedStrat#LexSmallestTerm + passedStrat#LexLargest + passedStrat#GRevLexSmallest + passedStrat#GRevLexSmallestTerm + passedStrat#GRevLexLargest + passedStrat#Random) then (
        submatrixS1 = chooseRandomSubmatrix(minorSize, M1);
        if (opts.Verbose) or debugLevel > 1 then print "internalChooseMinor: Choosing Random";
    )
    else if (myRandom < passedStrat#LexSmallest + passedStrat#LexSmallestTerm + passedStrat#LexLargest + passedStrat#GRevLexSmallest + passedStrat#GRevLexSmallestTerm + passedStrat#GRevLexLargest + passedStrat#Random + passedStrat#RandomNonzero) then (
        submatrixS1 = chooseRandomNonzeroSubmatrix(minorSize, M1);
        if (opts.Verbose) or debugLevel > 1 then print "internalChooseMinor: Choosing RandomNonZero";
    )
    else if (myRandom < passedStrat#LexSmallest + passedStrat#LexSmallestTerm + passedStrat#LexLargest + passedStrat#GRevLexSmallest + passedStrat#GRevLexSmallestTerm + passedStrat#GRevLexLargest + passedStrat#Random + passedStrat#RandomNonzero + passedStrat#Points) then (
        if (char ambR == 0) then (
            if (opts.Verbose) or debugLevel > 1 then print "internalChooseMinor: Choosing Points, but characteristic is zero, so defaulting to random instead.";
            submatrixS1 = chooseRandomSubmatrix(minorSize, M1); 
        )
        else (
            if (opts.Verbose) or debugLevel > 0 then print "internalChooseMinor: Choosing Points";                                    
            try (o = findANonZeroMinor(minorSize, M1, I1, new OptionTable from opts.PointOptions);) then submatrixS1 = {o#2, o#1} else ( submatrixS1 = chooseRandomSubmatrix(minorSize, M1); if (opts.Verbose) or debugLevel > 0 then print "internalChooseMinor: failed to find a point"; );            
        );
    );
    --if (opts.Verbose or (debugLevel > 0)) then print "internalChooseMinor: finished.";
    return submatrixS1;
)



regularInCodimension = method(Options=>optRn);

regularInCodimension(ZZ, Ring) := opts -> (n1, R1) -> (
    if (not verifyStrategy(opts.Strategy)) then error "regularInCodimension: Expected a valid strategy, a HashTable or MutableHashTable with expected Keys.";
    ambR := myAmbient R1;
    Id := ideal R1;
    R1a := R1;
    if (opts.Modulus > 0) then (
        p:=opts.Modulus;
        genList := generators(ambR);
        ambR = ZZ/p[genList];
        Id = sub(ideal R1, ambR);
        R1a = ambR/Id;
    );

    if not (isField coefficientRing ambR) then return "Ambient ring is not field";

    M1 := sub(jacobian Id, ambR);
    numberRelations := numColumns(M1);
    n := numgens ambR;
    r := dim R1a;
    fullRank := n-r;
    myRand := 0;
    Q := null; C := null; C2 := null; g := null;

    possibleMinors := binomial(n, fullRank)*binomial(numberRelations, fullRank);
    minNumberToCutDown := n1+1;
    local numberOfMinorsCompute;
    if instance (opts.MaxMinors, Function) then (
        numberOfMinorsCompute = opts.MaxMinors(minNumberToCutDown, possibleMinors); )
        --5*minNumberToCutDown + 8*ceiling(log_1.3(possibleMinors));)
    else if instance(opts.MaxMinors, Number) then (
        numberOfMinorsCompute = opts.MaxMinors;)
    else (
        numberOfMinorsCompute = 5*minNumberToCutDown + 8*ceiling(log_1.3(possibleMinors));
    );

    minTerm := sub(0, R1a);
    mutM1 := mutableMatrix(M1);
    nonzeroM := replaceZeros(M1, Strategy=>opts.Strategy); --
    mutM2 := mutableMatrix(nonzeroM); --for smallest grevlex computations

    searchedSet := new MutableHashTable from {}; --used to store which determinants have already been computed

    sumMinors := ideal(sub(0, ambR));

    quotient1 := ambR/(Id+sumMinors);
    d := dim(ideal quotient1);
    D := dim(ambR);

    if (opts.Verbose or debugLevel > 0) then print concatenate("regularInCodimension: ring dimension =", toString(d), ", there are ", toString(possibleMinors), " possible ", toString(fullRank), " by ", toString(fullRank), " minors, we will compute up to ", toString(numberOfMinorsCompute), " of them.");

    i := 0; --number of matrices considered so far
    k := 0; --how many minors found so far
    initToCompute := opts.MinMinorsFunction(minNumberToCutDown);
    j := initToCompute-1;
    --2*minNumberToCutDown+3;
    R2 := reorderPolynomialRing(GRevLex, ambR);
    f := map(R2, ambR);
    myRandom := 0;
    local M2;
    local submatrixS1;
    nextCodimCheck := opts.CodimCheckFunction(initToCompute);
    if (opts.Verbose or debugLevel > 0) then print concatenate("regularInCodimension: About to enter loop");
    while ( (r-d <= n1) and (i < numberOfMinorsCompute) and (#searchedSet < possibleMinors)) do (   
        --print ("i = " |toString(i) | ", nextCodimCheck = " | toString(nextCodimCheck) | ", codimCheck(i) = " | toString( (opts.CodimCheckFunction(i))));
        while (opts.CodimCheckFunction(i) < nextCodimCheck) and (i < numberOfMinorsCompute) do (
            --print (toString(i) | "," | toString(nextCodimCheck) );
            submatrixS1 = internalChooseMinor(fullRank, Id+sumMinors, nonzeroM, M1, Strategy=>opts.Strategy, Verbose=>opts.Verbose, MutableSmallest=>mutM2, MutableLargest=>mutM1, PointOptions=>opts.PointOptions);
            if  (not (submatrixS1 === null)) and (not (searchedSet#?(locationToSubmatrix(submatrixS1)))) then (
                searchedSet#(locationToSubmatrix(submatrixS1)) = true;
                sumMinors = sumMinors + ideal(getDetOfSubmatrix(M1, submatrixS1, DetStrategy=>opts.DetStrategy));
                k = k+1;
            );
            i = i+1;
        );
        --if (i == initToCompute) then j = i;
        if (opts.Verbose or debugLevel > 0) then print concatenate("regularInCodimension:  Loop step, about to compute dimension.  Submatrices considered: ", toString(i), ", and computed = ", toString(# keys searchedSet) );
        mutM2 = mutableMatrix(nonzeroM); --reset this matrix periodically
        mutM1 = mutableMatrix(M1);
        if (true === isCodimAtLeast((D - r) + n1 + 1, Id + sumMinors, PairLimit=>opts.PairLimit, SPairsFunction => opts.SPairsFunction)) then (
            d = r-n1 - 1;
            if (opts.Verbose or debugLevel > 0) then print concatenate("regularInCodimension:  singularLocus dimension verified by isCodimAtLeast");            
        );
        if (not opts.UseOnlyFastCodim) and (r-d <= n1) then (
            if (opts.Verbose or debugLevel > 0) then print concatenate("regularInCodimension:  isCodimAtLeast failed, computing codim.");            
            quotient1 = ambR/(Id+sumMinors);
            d = dim(ideal quotient1);
        );
        if (opts.Verbose or debugLevel > 0) then print concatenate("regularInCodimension:  partial singular locus dimension computed, = ", toString(d));
        --j = j+1;
        --while (opts.CodimCheckFunction(i) >= nextCodimCheck) do nextCodimCheck = nextCodimCheck+1;
        nextCodimCheck = ceiling(opts.CodimCheckFunction(i+1));
    );
    if (opts.Verbose or debugLevel > 0) then print concatenate("regularInCodimension:  Loop completed, submatrices considered = ", toString(i), ", and computed = ", toString(# keys searchedSet), ".  singular locus dimension appears to be = ", toString(d));
    if ( r-d > n1) then (return true);
    if  (r-d<n+1) and (#searchedSet <= possibleMinors) then (return null);
    if (#searchedSet == possibleMinors) and ((r-d)<n1+1) then (return false);
);



chooseGoodMinors = method(Options=>optChooseGoodMinors);

chooseGoodMinors(ZZ, ZZ, Matrix) := opts -> (howMany, minorSize, M1) -> (
    R1 := ring M1;
    ambR := myAmbient R1;
    chooseGoodMinors(howMany, minorSize, sub(M1, ambR), ideal R1, opts)
);

chooseGoodMinors(ZZ, ZZ, Matrix, Ideal) := opts -> (howMany, minorSize, M1, I1) -> (
    if (not verifyStrategy(opts.Strategy)) then error "chooseGoodMinors: Expected a valid strategy, a HashTable or MutableHashTable with expected Keys.";
    R1 := ring M1;
    if (howMany <= 0) then return trim ideal(sub(0, R1));
    ambR := myAmbient R1;
    Id := sub(I1, ambR) + ideal(R1);
    possibleMinors := binomial(numColumns M1, minorSize)*binomial(numRows M1, minorSize);
    M1 = sub(M1, ambR);
    mutM1 := mutableMatrix(M1);
    nonzeroM := replaceZeros(M1, Strategy=>opts.Strategy); --
    mutM2 := mutableMatrix(nonzeroM); --for smallest grevlex computations

    searchedSet := new MutableHashTable from {}; --used to store which determinants have already been computed
    i := 0; -- how many times through the loop we go
    k := 0; -- how many actual minors we found
    j := 3; -- controlling when to reset the grevlex matrix
    maxAttempts := 10*log_1.5(possibleMinors) + 10; --the absolute max number of attempts
    if (maxAttempts < howMany) then maxAttempts = howMany;
    --print maxAttempts;
    --print howMany;
    R2 := reorderPolynomialRing(GRevLex, ambR);
    f := map(R2, ambR);
    sumMinors := trim ideal(sub(0, ambR));
    local M2;
    local submatrixS1;
--first we try smallest submatrices with respect to several different monomial orders
    while ( (k < howMany) and (i < maxAttempts) and (#searchedSet < possibleMinors)) do (
        while (i <= j^1.5) and (k < howMany) and (i < maxAttempts) do (
            submatrixS1 = internalChooseMinor(minorSize, Id + sumMinors, nonzeroM, M1, Strategy=>opts.Strategy, Verbose=>opts.Verbose, MutableSmallest=>mutM2, MutableLargest=>mutM1, PointOptions => opts.PointOptions);

            if (not (submatrixS1 === null)) and (not searchedSet#?(locationToSubmatrix(submatrixS1))) then (
                searchedSet#(locationToSubmatrix(submatrixS1)) = true;
                sumMinors = sumMinors + ideal(getDetOfSubmatrix(M1, submatrixS1, DetStrategy=>opts.DetStrategy));
                k = k+1;
            );
            i = i+1;
        );
        if not (opts.PeriodicCheckFunction === null) then (
            --if there is a custom function
            if (opts.PeriodicCheckFunction)(sub(sumMinors, R1)) then break;
        );
        mutM2 = mutableMatrix(nonzeroM); --reset this matrix periodically
        mutM1 = mutableMatrix(M1);
        j = j+1;
      --reset the matrix before randomly traversing it again
    );
    if (opts.Verbose == true) or (debugLevel > 0) then print concatenate("chooseGoodMinors: found =", toString(k), ", attempted = ", toString(i));
    sub(sumMinors, R1)
)



projDim = method(Options=>optProjDim);

projDim(Module) := opts -> (N1) -> (    
    if (isHomogeneous N1) then return pdim N1;
    if (not verifyStrategy(opts.Strategy)) then error "projDim: Expected a valid strategy, a HashTable or MutableHashTable with expected Keys.";
    ambRing := ring (N1);
    myDim := #(first entries vars ambRing);
    if (not instance(ambRing, PolynomialRing)) then error "projDim: currently this only works for modules over polynomial rings";
    myRes := resolution minimalPresentation N1;
    myDiffs := myRes.dd;
    myLength := length myRes;
    firstRank := rank myRes_myLength;
    if (debugLevel > 0) or opts.Verbose then print concatenate("projDim: resolution computed!  length =", toString myLength, " rank =", toString firstRank);
    firstDiff := myDiffs_myLength;

    possibleMinors := binomial(numColumns firstDiff, firstRank)*binomial(numRows firstDiff, firstRank);

    local minorsCount; --how many minors?
    if instance(opts.MaxMinors, BasicList) then (
        minorsCount = (opts.MaxMinors)#0;)
    else if instance(opts.MaxMinors, ZZ) then (
        minorsCount = opts.MaxMinors;)
    else if instance(opts.MaxMinors, Function) then (
        if (debugLevel > 0) or opts.Verbose then print "projDim:  Using passed minors function.";
        minorsCount = (opts.MaxMinors)(myDim, possibleMinors);)        
    else (
        if (debugLevel > 0) or opts.Verbose then print "projDim:  Using default max minors function."; 
        minorsCount = 5*myDim + 2*log_1.3(possibleMinors); );
    if (debugLevel > 0) or opts.Verbose then print concatenate("projDim: going to try to find ", toString minorsCount, " minors.");
    
    goodMinorsOptions := new OptionTable from {Strategy=>opts.Strategy, Verbose=>opts.Verbose, PointOptions => opts.PointOptions, DetStrategy=>opts.DetStrategy, PeriodicCheckFunction => (J -> (dim J < 0))}; --just grab the options relevant to chooseGoodMinors
    theseMinors := chooseGoodMinors(ceiling(minorsCount), firstRank, firstDiff, goodMinorsOptions);
    curDim := dim theseMinors;
    if (debugLevel > 0) or opts.Verbose then print concatenate("projDim: first minors computed!  minors found =", toString(#first entries gens theseMinors), ", curDim =", toString(curDim));
    if (curDim >= 0) then (return myLength);

    i := myLength-1;
    if (debugLevel > 0) or opts.Verbose then print "projDim: computed dim, now starting loop.";
    while (i>opts.MinDimension) and (curDim <= -1) do (
        --print concatenate("in loop: ", toString(i));
        firstRank = (rank myRes_i)-firstRank;
        possibleMinors = binomial(numColumns myDiffs_i, firstRank)*binomial(numRows myDiffs_i, firstRank);
        
        if instance(opts.MaxMinors, BasicList) then (
            minorsCount = (opts.MaxMinors)#(myLength - i); )
        else if instance(opts.MaxMinors, ZZ) then (
            minorsCount = opts.MaxMinors; )
        else if instance(opts.MaxMinors, Function) then (
            minorsCount = (opts.MaxMinors)(myDim, possibleMinors);)         
        else ( minorsCount = minorsCount = 10*myDim + 2*log_1.3(possibleMinors); );
        theseMinors = chooseGoodMinors(ceiling(minorsCount), firstRank, myDiffs_i, goodMinorsOptions);
        if (debugLevel > 0) or opts.Verbose then print concatenate("projDim: in loop, about to compute dim.  current length =", toString myLength, " rank =", toString firstRank);
        curDim = dim theseMinors;
        if (curDim >= 0) then (return i);
        i=i-1;
    );
    return i;
);

isGBDone := (myGB) -> (
    --a temporary function for finding out if a gb computation is done.
    myStr := status myGB;
    return 0 < #select("status: done", myStr);
);



isCodimAtLeast = method(Options => optIsCodimAtLeast);

isCodimAtLeast(ZZ, Ideal) := opts -> (n1, I1) -> (
    R1 := ring I1;
    S1 := myAmbient R1;
    if (not isPolynomialRing(S1)) then error "isCodimAtLeast:  This requires an ideal in a polynomial ring, or in a quotient of a polynomial ring.";
    if n1 <= 0 then return true; --if for some reason we are checking codim 0.
    if (isMonomialIdeal I1) and (codim monomialIdeal I1 >= n1) then return true;
    if #first entries gens I1 == 0 then return false;  
    J1 := ideal R1;
    dAmb := codim J1;
    I2 := sub(I1, S1) + sub(J1, S1); --lift to the polynomial ring.
    --now we have an ideal in a polynomial ring.  The idea is that we should compute a partial Groebner basis.
    --and compute the codim of that.
    --But first, we just try a quick codim computation based on the ideal generators.
    monIdeal := null;
    if (#first entries gens I2 > 0) then (
        monIdeal = monomialIdeal(apply(first entries gens I2, t->leadTerm t));
        if (opts.Verbose or debugLevel > 2) then print concatenate("isCodimAtLeast: Computing codim of monomials based on ideal generators.");
        if (codim monIdeal - dAmb >= n1) then return true;
        if (opts.Verbose or debugLevel > 2) then print concatenate("isCodimAtLeast: Didn't work, going to find the partial Groebner basis.");
    );
    --now we set stuff up for the loop if that doesn't work.  
    vCount := # first entries vars S1;
 --   baseDeg := apply(sum(apply(first entries vars S1, t1 -> degree t1)), v -> ceiling(v/vCount)); --use this as the base degree to step by (probably we should use a different value)
    i := 1;
    --curLimit := baseDeg;
    local curLimit;
    local myGB;
    
    gensList := null;
    while (i <= opts.PairLimit) do(
        curLimit = opts.SPairsFunction(i);
--        curLimit = apply(baseDeg, tt -> (opts.SPairsFunction)(tt,i));
        if (opts.Verbose or debugLevel > 2) then print concatenate("isCodimAtLeast: about to compute gb PairLimit => ", toString curLimit);
        myGB = gb(I2, PairLimit=>curLimit);
        gensList = first entries leadTerm gb(I2, PairLimit=>curLimit);    
        if (#gensList > 0) then (
            monIdeal = monomialIdeal(first entries leadTerm myGB);
            if (opts.Verbose or debugLevel > 2) then print concatenate("isCodimAtLeast: computed gb, now computing codim ");
            if (codim monIdeal - dAmb >= n1) then return true;
        );
        if isGBDone(myGB) then i = opts.PairLimit;
        i = i + 1;
    );
    return null;
);

isDimAtMost = method(Options => optIsCodimAtLeast);

isDimAtMost(ZZ, Ideal) := opts -> (n1, I1) -> (
    d := dim ring I1;
    return isCodimAtLeast(d-n1, I1, opts);
);

-- Multithreaded function to determine whether the rank of a given matrix is greater than or equal to an input value
-- If multithreading is permitted and the processor has at least 4 threads, code executes multithreaded version. Else, executes isRankAtLeastSingle
-- Schedules three tasks, two that run getSubmatrixOfRank and one that runs rank. If one of the getSubmatrixOfRank calls returns first and results are inconclusive, waits for another task to complete.
-- If rank completes first, function returns whether the rank is greater than the input value.
-- This version currently only supports one thread as Macaulay2 becomes unstable when cancelling tasks. The code for more threads is already implemented and can be used once this functionality is fixed.
isRankAtLeast = method(Options => optIsRankAtLeast);

isRankAtLeast(ZZ, Matrix) := opts -> (n1, M0) -> (
    if (not verifyStrategy(opts.Strategy)) then error "isRankAtLeast: Expected a valid strategy, a HashTable or MutableHashTable with expected Keys.";    
  if (opts.Threads <= 2) or (not isGlobalSymbol "nanosleep") or (allowableThreads <= 3) then (
      if ((debugLevel > 0) or (opts.Verbose==true)) then print "isRankAtLeast: Going to single threaded version.";
      return isRankAtLeastSingle(n1, M0, opts); --single threaded version, implementation below.
  );
  if (M0 == 0) then return (n1 <= 0);

  if (n1 > numRows M0) or (n1 > numColumns M0) then return false;
  if (n1 == numRows M0) and (n1 == numColumns M0) then return (rank M0 == n1);
  if (not (opts.MaxMinors === null)) and (opts.MaxMinors <= 0) then return (rank M0 == n1);
  t1 := createTask(getSubmatrixOfRank, (n1, M0, MaxMinors=>infinity, Verbose => opts.Verbose, DetStrategy => opts.DetStrategy, Strategy => opts.Strategy, Threads => opts.Threads));
  t3 := createTask(rank, M0);
  schedule t1;
  schedule t3;
  r1 := isReady(t1);
  r3 := isReady(t3);
  while (r1==false and r3==false) do ( nanosleep(10000000); r1 = isReady(t1); r3 = isReady(t3));
  if ((opts.Verbose==true)  or (debugLevel > 0)) then print ("isRankAtLeast(multi threaded):  done with getSubmatrix 1:" | toString(r1) | ",  done with rank: "| toString(r3) );
  if (r1) then (tr := taskResult(t1); if (tr =!= null) then (cancelTask t3; return true;) else ( while (r3 == false) do (r3 = isReady(t3); nanosleep(10000000);); ))
  else if(r3) then (cancelTask t1; return taskResult(t3)>=n1);

  tr3 := taskResult(t3);
  return (tr3 >= n1);


  --if (n1 > numRows M0) or (n1 > numColumns M0) then return false;
  --if (n1 == numRows M0) and (n1 == numColumns M0) then return (rank M0 == n1);
  --if (not (opts.MaxMinors === null)) then (if (opts.MaxMinors <= 0) then return (rank M0 == n1););
  --t1 := createTask(getSubmatrixOfRank, (n1, M0, MaxMinors=>infinity, Verbose => opts.Verbose, DetStrategy => opts.DetStrategy, Strategy => opts.Strategy, Threads => opts.Threads));
  --t2 := createTask(getSubmatrixOfRank, (n1, M0, MaxMinors=>infinity, Verbose => opts.Verbose, DetStrategy => opts.DetStrategy, Strategy => opts.Strategy, Threads => opts.Threads));
  --t3 := createTask(rank, M0);
  --schedule t1;
  --schedule t2;
  --schedule t3;
  --r1 := isReady(t1);
  --r2 := isReady(t2);
  --r3 := isReady(t3);
  --while (r1==false and r2==false and r3==false) do ( nanosleep(10000000); r1 = isReady(t1); r2 = isReady(t2); r3 = isReady(t3));
  --if ((opts.Verbose==true)  or (debugLevel > 0)) then print ("isRankAtLeast(multi threaded):  done with getSubmatrix 1:" | toString(r1) | ", done with getSubmatrix 2:" | toString(r2) | ",  done with rank: "| toString(r3) );
  --if (r1) then (tr := taskResult(t1); if (tr =!= null) then ( cancelTask t2; cancelTask t3; return true;) else ( while (r2 == false or r3 == false) do (r2 = isReady(t2); r3 = isReady(t3); nanosleep(10000000);); ));
  --if (r2) then (tr2 := taskResult(t2); if (tr2 =!= null) then ( cancelTask t1; cancelTask t3; return true;) else ( while (r3 == false) do (r3 = isReady(t3); nanosleep(10000000);); ));

  --tr3 := taskResult(t3);
  --cancelTask t1; cancelTask t2;
  --return (tr3 >= n1);
);


-*
StrategyDefault = new OptionTable from {
    LexLargest => 0,
    LexSmallestTerm => 16,
    LexSmallest => 16,
    GRevLexSmallestTerm => 16,
    GRevLexSmallest => 16,
    GRevLexLargest => 0,
    Random => 16,
    RandomNonzero => 16,
    Points => 0*-


--this is an internal helper method that is called as a default for now, in the future will only ne used when the user has less than 3 available threads
isRankAtLeastSingle = method(Options => optIsRankAtLeast);

isRankAtLeastSingle(ZZ, Matrix) := opts -> (n1, M0) -> (
  if (n1 > numRows M0) or (n1 > numColumns M0) then return false;
  if (M0 == 0) then return (n1 <= 0);
  if (n1 == numRows M0) and (n1 == numColumns M0) then return (rank M0 == n1);
  if (not (opts.MaxMinors === null)) and (opts.MaxMinors <= 0) then return (rank M0 == n1);
  val := getSubmatrixOfRank(n1, M0, opts);
  if (val === null) then ( return (rank M0 >= n1); ) else return true;
  )

getSubmatrixOfRank = method(Options => optIsRankAtLeast);

getSubmatrixOfRank(ZZ, Matrix) := opts -> (n1, M0) -> (
    local nonzeroM;
    local mutM2;
    --print opts;
    if (not verifyStrategy(opts.Strategy)) then error "getSubmatrixOfRank: Expected a valid strategy, a HashTable or MutableHashTable with expected Keys.";
    if (n1 > numRows M0) or (n1 > numColumns M0) then return null;
    if (M0 == 0) then return null;
    R1 := ring M0;
    ambRing := null;
    Id := null;
    if (instance(R1, PolynomialRing) or instance(R1, QuotientRing)) then (ambRing = ambient(R1); Id = ideal(R1)) else (ambRing = R1; Id = ideal(0_R1));
    i := 0;
    M1 := sub(M0, ambRing);
    possibleMinors := binomial(numColumns M1, n1)*binomial(numRows M1, n1);
    attempts := min(possibleMinors, 2+log_10(possibleMinors));
    if not (opts.MaxMinors === null) then attempts = opts.MaxMinors;
    mutM1 := mutableMatrix(M1); --for largest grevlex computations
    
    --we now only do the replacement if we are calling a strategy that needs it, replaceZeros is now smart about that, but it needs to know the strategy
    nonzeroM = replaceZeros(M1, Strategy=>opts.Strategy);   
    mutM2 = mutableMatrix(nonzeroM); --for smallest grevlex computations


    internalMinorsOptions := new OptionTable from {Strategy=>opts.Strategy, Verbose=>opts.Verbose, PointOptions => opts.PointOptions}; --just grab the options relevant to chooseGoodMinors

    searchedSet := new MutableHashTable from {}; --used to store which ranks have already been computed

    if not (opts.MaxMinors === null) then (
        attempts = opts.MaxMinors;
    );
    subMatrix := null;
    val := null;
    if (debugLevel > 0) or opts.Verbose then print ("getSubmatrixOfRank: Trying to find a submatrix of rank at least: " | toString(n1) | " with attempts = " | toString(attempts) | ".  DetStrategy=>" | toString(opts.DetStrategy));
    while (i < attempts)  do (
        --if any(flatten entries matrix mutM2, z->z==0) then error "getSubmatrixOfRank: expected a matrix with no zero entries.";
        subMatrix = internalChooseMinor(n1,  Id, nonzeroM, M1, internalMinorsOptions++{MutableSmallest=>mutM2, MutableLargest=>mutM1});
        --if (debugLevel > 0) or opts.Verbose then print ("getSubmatrixOfRank: found subMatrix " | toString(subMatrix));
        if (not (subMatrix === null)) and (not (searchedSet#?(locationToSubmatrix(subMatrix)))) then (
            searchedSet#(locationToSubmatrix(subMatrix)) = true;
            if (opts.DetStrategy === Rank) then (
                mRowListS := subMatrix#0;
                mColListS := subMatrix#1;
                if (rank sub((M1^mRowListS)_mColListS, R1) >= n1) then (
                    if (debugLevel > 0) or opts.Verbose then print ("getSubmatrixOfRank: found one, in " | toString(i+1) | " attempts");
                    return {mRowListS, mColListS};
                );
            )
            else if (opts.DetStrategy === null) then ( --we use a default strategy
                val = sub(getDetOfSubmatrix(M1, subMatrix, DetStrategy=>opts.DetStrategy), R1);
                if (not (val == 0)) then (if (debugLevel > 0) or opts.Verbose then print ("getSubmatrixOfRank: found one, in " | toString(i+1) | " attempts"); return {subMatrix#0, subMatrix#1};);
            )
            else ( --we use the given strategy
                val = sub(getDetOfSubmatrix(M1, subMatrix, DetStrategy=>opts.DetStrategy), R1);
                if (not (val == 0)) then (if (debugLevel > 0) or opts.Verbose then print ("getSubmatrixOfRank: found one, in " | toString(i+1) | " attempts"); return {subMatrix#0, subMatrix#1};);
            );
        );
        i = i+1;
    );
    if (debugLevel > 0) or opts.Verbose then print ("getSubmatrixOfRank: failed to find a minor, perhaps increase MaxMinors");
    return null;
);

--**********************************************
--**Alternate minors command, multithreaded  ***
--**computes smaller minors before larger    ***
--**minors.  It stores them.  This is faster ***
--**for matrices where multiplications are   ***
--**expensive.  It uses more ram though.     ***
--**********************************************

recursiveMinors =method(Options=>{MinorsCache => true, Threads=>0, Verbose=>false});

recursiveMinors(ZZ, Matrix) := Ideal => opts -> (n, M1) -> (
    if not ((M1#cache)#?MinorsCache) then (M1.cache)#MinorsCache = new MutableHashTable from {};
    --if not ((M1#cache)#?MinorsCache) then print "hello?";
    --if (n == numColumns M1) then M1 = transpose M1;
    H1 := null;
    if ((M1#cache)#MinorsCache)#?n then (
        if (debugLevel > 0) or (opts.Verbose) then print "recursiveMinors: we found stored data, using it.";
        H1 = ((M1#cache)#MinorsCache)#n; )
    else (
        H1 = recursiveMinorsTableP(n, M1, opts);
        if (opts.MinorsCache == true) then (((M1.cache).MinorsCache)#n = H1;); --store data only if told to
    );
    ideal values H1
);


--this is the multithreaded internal function
recursiveMinorsTableP = method(Options=>{MinorsCache => true, Threads=>0, Verbose=>true});

recursiveMinorsTableP(ZZ, Matrix) := opts -> (n, M1) -> (
    if not ((M1#cache)#?MinorsCache) then (M1.cache)#MinorsCache = new MutableHashTable from {};
    if (opts.Threads <= 1) or (not isGlobalSymbol "nanosleep") then (
        if (debugLevel > 0) then print "recursiveMinorsP: Going to single threaded version.";
        return recursiveMinorsTable(n, M1);
    );
    rowCt := numRows M1;
    colCt := numColumns M1;
    gg := subsets(rowCt, n);
    ff := subsets(colCt, n);
    L := toList ((set gg)**(set ff));
    lenL := length L;
    R1 := {};
    if (debugLevel > 0) then    print ("recursiveMinorsTableP: about to recurse, minors of size" | toString(n));
    if (n > 2) then (
        if ((M1.cache).MinorsCache)#?(n-1) then (
            if (debugLevel > 0) then print "recursiveMinorsTable: we found stored data, using it to avoid recursion.";
            R1 = ((M1.cache).MinorsCache)#(n-1); )
        else (
            R1 = recursiveMinorsTableP(n-1, submatrix'(M1, {rowCt-1}, ),opts);
        );
    )
    else (
        if ((M1.cache).MinorsCache)#?n then (
            if (debugLevel > 0) then print "recursiveMinorsTableP: we found stored data, using it.";
            return ((M1.cache).MinorsCache)#n; )
        else (
            return new HashTable from  apply(L, i -> (i => det (M1^(i#0)_(i#1))) ); --this should be multithreaded too presumably...
        );
    );
    tempL := null;
    if (debugLevel > 0) then print "recursiveMinorsTableP: did recursion, going to do tasks";
    taskList := apply(opts.Threads, i -> (tempL =  take(L, {(i*(lenL-1))//(opts.Threads), ((i+1)*(lenL-1))//(opts.Threads)});    
        return createTask(temp, (tempL, n, M1, R1) ); ) );
    apply(taskList, t -> schedule t);
    if (debugLevel > 0) then print ("recursiveMinorsTableP: started " | toString(#taskList) | " tasks.");
    while true do (
        nanosleep 50000000;
        if all(taskList, t->isReady(t)) then break;
        );
    myList := flatten flatten apply(taskList, t -> taskResult(t));
    H := new HashTable from myList;
    apply(taskList, tt -> cancelTask(tt));
    return H;
)

--this is just a temporary function
temp = (Lis,n, M1, R1) -> (
    --print "temp started";
	K := apply(Lis, i -> (i => sum(0..n-1, curCol -> (-1)^(curCol)*M1_(i#0#(n-1), i#1#curCol)*R1#(toList drop(i#0, {n-1,n-1}), toList drop(i#1, {curCol, curCol})))));
	return K
);

--this is the single threaded function
recursiveMinorsTable = (n, M1) -> (
    if not ((M1#cache)#?MinorsCache) then (M1.cache)#MinorsCache = new MutableHashTable from {};
    rowCt := numRows M1;
    colCt := numColumns M1;
    gg := subsets(rowCt, n);
    ff := subsets(colCt, n);
    L := toList ((set gg)**(set ff));
    R := {};
    if (n > 2) then (
        if ((M1.cache).MinorsCache)#?(n-1) then (
            if (debugLevel > 0) then print "recursiveMinorsTable: we found stored data, using it to avoid recursion.";
            R = ((M1.cache).MinorsCache)#(n-1); )
        else (
            R = recursiveMinorsTable(n-1, submatrix'(M1, {rowCt-1}, )))
        )
    else (
        if ((M1.cache).MinorsCache)#?n then (
            if (debugLevel > 0) then print "recursiveMinorsTable: we found stored data, using it.";
            return ((M1.cache).MinorsCache)#n; )
        else (
            return new HashTable from  apply(L, i -> (i => det (M1^(i#0)_(i#1))) )
        );
    );
    H := new HashTable from  apply(L, i -> (i => sum(0..n-1, curCol -> (-1)^(curCol)*M1_(i#0#(n-1), i#1#curCol)*R#(toList drop(i#0, {n-1,n-1}), toList drop(i#1, {curCol, curCol}))))); --det (M^(i#0)_(i#1))

    return H;
)



beginDocumentation();

document {
    Key => FastMinors,
    Headline => "faster linear algebra, especially for computation of minors",
    EM "FastMinors", " is a package for computing relevant minors (determinants of submatrices) of matrices of polynomial functions quickly",
    BR{},BR{},
    "This package provides functionality for doing certain linear algebra operations over rings quickly.  There is also some multithreaded capability which is disabled by default.  Note this package was previously called ", TT "FastLinAlg.",
    BR{},BR{},
    BOLD "Tutorials:", BR{},
    "There are tutorials on how to use various strategies and options in ", TO "FastMinorsStrategyTutorial", " and ", TO "RegularInCodimensionTutorial", " which we recommend the user explore if they are interested in the more advanced features of the package.",
    BR{},BR{},
    BOLD "Useful functions:",BR{},
    UL {
	  {TO "chooseGoodMinors", " Tries to find interesting minors of a matrix."},
      {TO "isRankAtLeast", " Tries to show that a matrix has rank at least a given number by looking at submatrices"},
	  {TO "regularInCodimension", " checks whether a ring is regular in codimension n, but doesn't return false." },
	  {TO "projDim", " checks the projective dimension of a module and may give better answers than ", TO "pdim", " in the case that R is not homogeneous" },
      {TO "recursiveMinors", " provides a different strategy for computing minors of a matrix.  It is a cofactor strategy where the determinants of smaller minors are stored." },
      {TO "isCodimAtLeast", " provides a way for finding lower bounds for the codimension of an ideal, without actually computing the codimension."},
	},
    "Many of these functions have extensive options for finetuning their behavior, for instance by controlling how submatrices are chosen.  See the documentation for ", TO "StrategyDefault",
	BR{},BR{},
	BOLD "Acknowledgements:",BR{},BR{},
	"The authors would like to thank the anonymous referee, David Eisenbud, Eloisa Grifo, and Srikanth Iyengar for useful conversations and comments on the development of this package.", BR{},
    BR{},
    "Boyana Martinova received funding from the University of Utah Mathematics Department REU program and from the ACCESS program at the University of Utah, while developing this package.",
    BR{},
    "Marcus Robinson received funding from the NSF RTG grant 1246989 while developing this package.",
    BR{},
    "Karl Schwede received funding from NSF grant 1801849 and a fellowship from the Simons Foundation while developing this package.",
    BR{},
    "Yuhui (Wei) Yao received funding from the University of Utah Mathematics Department REU program, while developing this package"
}

doc ///
    Key
        FastMinorsStrategyTutorial
    Headline
        How to use and construct strategies for selecting submatrices in various functions
    Description
        Text
            We will work with the following example, the cone over a product of an elliptic curve and a genus 3 planar curve, embedded with a Segre embedding, so the cone over a surface, embedded in $P^8$ (defined by 31 equations).  The Jacobian is a relatively sparse $9 \times 31$ matrix and to compute the full Jacobian ideal we would need to compute the 61,847,604 minors of size $6 \times 6$.  
        Example
            S = ZZ/103[x_1..x_9];
            J = ideal(x_6*x_8-x_5*x_9,x_3*x_8-x_2*x_9,x_6*x_7-x_4*x_9,x_5*x_7-x_4*x_8,x_3*x_7-x_1*x_9,x_2*x_7-x_1*x_8,x_3*x_5-x_2*x_6,x_3*x_4-x_1*x_6,x_2*x_4-x_1*x_5,x_7^3-x_8^2*x_9-x_7*x_9^2,x_4*x_7^2-x_5*x_8*x_9-x_4*x_9^2, x_1*x_7^2-x_2*x_8*x_9-x_1*x_9^2,x_4^2*x_7-x_5^2*x_9-x_4*x_6*x_9,x_1*x_4*x_7-x_2*x_5*x_9-x_1*x_6*x_9,x_1^2*x_7-x_2^2*x_9-x_1*x_3*x_9,x_4^3-x_5^2*x_6-x_4*x_6^2,x_1*x_4^2-x_2*x_5*x_6-x_1*x_6^2,x_1^2*x_4-x_2^2*x_6-x_1*x_3*x_6,x_1^3-x_2^2*x_3-x_1*x_3^2,x_3^4+x_6^4-x_9^4,x_2*x_3^3+x_5*x_6^3-x_8*x_9^3,x_1*x_3^3+x_4*x_6^3-x_7*x_9^3,x_2^2*x_3^2+x_5^2*x_6^2-x_8^2*x_9^2,x_1*x_2*x_3^2+x_4*x_5*x_6^2-x_7*x_8*x_9^2,x_1^2*x_3^2+x_4^2*x_6^2-x_7^2*x_9^2,x_2^3*x_3+x_5^3*x_6-x_8^3*x_9,x_1*x_2^2*x_3+x_4*x_5^2*x_6-x_7*x_8^2*x_9,x_1^2*x_2*x_3+x_4^2*x_5*x_6-x_7^2*x_8*x_9,x_2^4+x_5^4-x_8^4,x_1*x_2^3+x_4*x_5^3-x_7*x_8^3,x_1^2*x_2^2+x_4^2*x_5^2-x_7^2*x_8^2)
            M = jacobian J
        Text
            {\bf getSubmatrixOfRank:}  We begin by exploring submatrices of this matrix, chosen using different strategies.  We try to select submatrices of rank 6 with the command @TO getSubmatrixOfRank@.  The {\tt Random} strategy for instance selects a smallest matrix.  On the other hand, {\tt GRevLexSmallest} (respectively {\tt GRevLexLargest}) tries to choose a matrix with minimal (respectively maximal) values with respect to a random {\tt GRevLex} order.  {\tt LexSmallest} and {\tt LexLargest} selects a smallest submatrix whose terms have smallest and largest value with respect to a random {\tt Lex} monomial order.  {\tt Points} first finds a point on $V(J)$ and then finds a submatrix where the matrix has full rank after being evaluated at that point.  For more details on how these strategies work, see @TO [getSubmatrixOfRank, Strategy]@.
        Example
            a = getSubmatrixOfRank(6, M**(S/J), Strategy=>Random)
            M^(a#0)_(a#1)           
            d = getSubmatrixOfRank(6, M**(S/J), MaxMinors=>100, Strategy=>LexSmallest)
            M^(d#0)_(d#1)
            e = getSubmatrixOfRank(6, M**(S/J), Strategy=>LexSmallestTerm)
            M^(e#0)_(e#1)                        
            f = getSubmatrixOfRank(6, M**(S/J), MaxMinors=>100, Strategy=>LexLargest)
            M^(f#0)_(f#1)
            g = getSubmatrixOfRank(6, M**(S/J), Strategy=>Points)
            M^(g#0)_(g#1)
        Text
            You can see that different strategies typically produce submatrices which can appear quite different in their complexity.
            We left off the strategies {\tt GRevLexLargest, GRevLexSmallest, GRevLexSmallestTerm} as they behave very poorly on this example (indeed, we had to increase {\tt MaxMinors}, the number of submatrices considered, for both {\tt LexSmallest} and {\tt LexLargest} as they did not find a submatrix in the default number of attempts).  Of course, one can view the matrix over $S$, instead of $S/J$, where it's easier to choose a submatrix.
        Example
            b = getSubmatrixOfRank(6, M, Strategy=>GRevLexSmallest)
            M^(b#0)_(b#1)
            c = getSubmatrixOfRank(6, M, Strategy=>GRevLexSmallestTerm)
            M^(c#0)_(c#1)
            h = getSubmatrixOfRank(6, M, Strategy=>LexLargest)
            M^(h#0)_(h#1)
        Text
            {\bf chooseGoodMinors:}  In many cases, we want to consider the determinant of the submatrices we just found, perhaps to append the determinant to an ideal.  The function @TO chooseGoodMinors@ helps us do that.  Note that {\tt chooseGoodMinors} does not check that the submatrix we are computing has full rank (except by computing its determinant and adding it to our working ideal).
        Example
            chooseGoodMinors(1, 6, M, J, Strategy=>Random)            
            chooseGoodMinors(1, 6, M, J, Strategy=>LexSmallest)
            chooseGoodMinors(1, 6, M, J, Strategy=>LexSmallestTerm)
            chooseGoodMinors(1, 6, M, J, Strategy=>LexLargest)
            chooseGoodMinors(1, 6, M, J, Strategy=>GRevLexSmallest)
            chooseGoodMinors(1, 6, M, J, Strategy=>GRevLexSmallestTerm)
            chooseGoodMinors(1, 6, M, J, Strategy=>GRevLexLargest)
            chooseGoodMinors(1, 6, M, J, Strategy=>Points)
        Text
            Here the $1$ passed to the function says how many minors to compute.  For instance, let's compute 8 minors for each of these strategies and see if that was enough to verify that the ring is regular in codimension 1.  In other words, if the dimension of $J$ plus the ideal of partial minors is $\leq 1$ (since $S/J$ has dimension 3).  
        Example
            time dim (J + chooseGoodMinors(8, 6, M, J, Strategy=>Random))
            time dim (J + chooseGoodMinors(8, 6, M, J, Strategy=>LexSmallest))
            time dim (J + chooseGoodMinors(8, 6, M, J, Strategy=>LexSmallestTerm))
            time dim (J + chooseGoodMinors(8, 6, M, J, Strategy=>LexLargest))
            time dim (J + chooseGoodMinors(8, 6, M, J, Strategy=>GRevLexSmallest))
            time dim (J + chooseGoodMinors(8, 6, M, J, Strategy=>GRevLexSmallestTerm))
            time dim (J + chooseGoodMinors(8, 6, M, J, Strategy=>GRevLexLargest))
            time dim (J + chooseGoodMinors(8, 6, M, J, Strategy=>Points))
        Text
            Indeed, in this example, even computing determinants of 1,000 random submatrices is not typically enough to verify that $V(J)$ is regular in codimension 1.  On the other hand, {\tt Points} is almost always quite effective at finding valuable submatrices, but can be quite slow.  In this particular example, we can see that {\tt LexSmallestTerm} also performs very well (and does it quickly).
            Since different strategies work better or worse on different examples, the default strategy actually mixes and matches various strategies.  The default strategy, which we now elucidate,         
        Example
            peek StrategyDefault
        Text
            says that we should use {\tt GRevLexSmallest, GRevLexSmallestTerm, LexSmallest, LexSmallestTerm, Random, RandomNonzero} all with equal probability (note {\tt RandomNonzero}, which we have not yet discussed chooses random submatrices where no row or column is zero, which is good for working in sparse matrices).  For instance, if we run:
        Example
            time chooseGoodMinors(20, 6, M, J, Strategy=>StrategyDefault, Verbose=>true);
        Text
            we can see different minors being chosen via different strategies.  
        Text
            Note, if one asks {\tt chooseGoodMinors} for more than one minor, then any time a {\tt Points} strategy is selected, the point is found on $J$ plus the ideal of all minors computed thus far. 
        Text
            Let us take a look at some other built-in strategies.  
        Example
            peek StrategyDefaultNonRandom
            peek StrategyDefaultWithPoints
            peek StrategyPoints
        Text
            {\tt StrategyDefaultNonRandom} is like {\tt StrategyDefault} but removes random submatrices (which can be suprisingly beneficial in some cases).  {\tt StrategyDefaultWithPoints} removes randomness but adds in points instead.  
        Text
            {\it A warning on chooseGoodMinors:}  The strategies {\tt LexSmallest} and {\tt LexSmallestTerm} will very frequently {\bf repeatedly} choose the same submatrix of the given matrix.  Hence if one tries to run {\tt chooseGoodMinors} and choose too many minors with such a strategy, one can get into a long loop (the function give up eventually, but only after doing way too much work).  The {\tt GRevLex} strategies periodically temporarily change the underlying matrix to avoid this sort of loop.
        Text
            {\bf Points:} Notice that {\tt Strategy => StrategyPoints} and {\tt Strategy => Points} do the same thing. We briefly describe how {\tt chooseGoodMinors} interacts with {\tt Points}.  Indeed {\tt Points} forms the ideal of minors computed so far (plus $J$), finds a point where that ideal vanishes (which can be slow), evaluates the matrix $M$ at that point, and then finally computes the corresponding determinant of the submatrix.  This submatrix will always produce a minor which shrinks our vanishing locus.  
        Text
            By default, the {\tt Points} strategy actually finds geometric points.  Which can be sometimes slower (but which are almost certain to exist, and are less likely to hang if the function has trouble finding a point).  For instance, we can control that as follows.
        Example            
            ptsStratGeometric = new OptionTable from (options chooseGoodMinors)#PointOptions;
            ptsStratGeometric#ExtendField --look at the default value
            time dim (J + chooseGoodMinors(1, 6, M, J, Strategy=>Points, PointOptions=>ptsStratGeometric))
            ptsStratRational = ptsStratGeometric++{ExtendField=>false} --change that value
            ptsStratRational.ExtendField --look at our changed value
            time dim (J + chooseGoodMinors(1, 6, M, J, Strategy=>Points, PointOptions=>ptsStratRational))
        Text
            Other options may also be passed to the @TO RandomPoints@ package via the @TO PointOptions@ option.
        Text
            {\bf regularInCodimension:}  It is reasonable to think that you should find a few minors (with one strategy or another), and see if perhaps the minors you have computed so far are enough to verify our ring is regular in codimension 1.  This is exactly what {\tt regularInCodimension} does.  One can control at a fine level how frequently new minors are computed, and how frequently the dimension of what we have computed so far is checked, by the option {\tt codimCheckFunction}.  For more on that, see @TO RegularInCodimensionTutorial@ and @TO regularInCodimension@.  Let us finish running {\tt regularInCodimension} on our example with several different strategies.
        Example
            time regularInCodimension(1, S/J, MaxMinors => 100, Strategy=>StrategyDefault)
            time regularInCodimension(1, S/J, MaxMinors => 100, Strategy=>StrategyDefaultNonRandom)
            time regularInCodimension(1, S/J, MaxMinors => 100, Strategy=>Random)
            time regularInCodimension(1, S/J, MaxMinors => 100, Strategy=>LexSmallest)
            time regularInCodimension(1, S/J, MaxMinors => 100, Strategy=>LexSmallestTerm)            
            time regularInCodimension(1, S/J, MaxMinors => 100, Strategy=>GRevLexSmallest)
            time regularInCodimension(1, S/J, MaxMinors => 100, Strategy=>GRevLexSmallestTerm)
            time regularInCodimension(1, S/J, MaxMinors => 100, Strategy=>Points)
            time regularInCodimension(1, S/J, MaxMinors => 100, Strategy=>StrategyDefaultWithPoints)
        Text
            If {\tt regularInCodimension} outputs nothing, then it couldn't verify that the ring was regular in that codimension.  We set {\tt MaxMinors => 100} to keep it from running too long with an ineffective strategy.  Again, even though {\tt GRevLexSmallest} and {\tt GRevLexSmallestTerm} are not effective in this particular example, in others they perform better than other strategies.  Note similar considerations also apply to @TO projDim@.
    SeeAlso
        [chooseGoodMinors, Strategy]
        RegularInCodimensionTutorial
///

doc ///
    Key
        RegularInCodimensionTutorial
    Headline
        A tutorial for how to use the advanced options of the regularInCodimension function
    Description
        Text
            In this tutorial we explore the different options of {\tt RegularInCodimension} (and related functions) on some cone singularities.  For the most part we will not talk about the {\tt Strategy} option, we have a separate tutorial for that @TO FastMinorsStrategyTutorial@.  
        Text
            We begin with the following ideal.
        Example
            S = ZZ/103[x_1..x_9];
            J = ideal(x_6*x_8-x_5*x_9,x_3*x_8-x_2*x_9,x_6*x_7-x_4*x_9,x_5*x_7-x_4*x_8,x_3*x_7-x_1*x_9,x_2*x_7-x_1*x_8,x_3*x_5-x_2*x_6,x_3*x_4-x_1*x_6,x_2*x_4-x_1*x_5,x_3^3-x_6^3-x_9^3,x_2*x_3^2-x_5*x_6^2-x_8*x_9^2,x_1*x_3^2-x_4*x_6^2-x_7*x_9^2,x_2^2*x_3-x_5^2*x_6-x_8^2*x_9,x_1*x_2*x_3-x_4*x_5*x_6-x_7*x_8*x_9,x_1^2*x_3-x_4^2*x_6-x_7^2*x_9,x_2^3-x_5^3-x_8^3,x_1*x_2^2-x_4*x_5^2-x_7*x_8^2,x_1^2*x_2-x_4^2*x_5-x_7^2*x_8,x_1^3-x_4^3-x_7^3);
            dim (S/J)
        Text
            It is the cone over $P^2 \times E$ where $E$ is an elliptic curve.  We have embedded it with a Segre embedding inside $P^8$.  In particular, this example is even regular in codimension 3.  
        Example
            time regularInCodimension(1, S/J) 
            time regularInCodimension(2, S/J) 
        Text
            We try to verify that $S/J$ is regular in codimension 1 or 2 by computing the ideal made up of a small number of minors of the Jacobian matrix.  
            In this example, instead of computing all relevant 1465128 minors to compute the singular locus, and then trying to compute the dimension of the ideal they generate, we instead compute a few of them.  {\tt regularInCodimension} returns {\tt true} if it verified the the ring with regular in codim 1 or 2 (respectively) and {\tt null} if not.  Because of the randomness that exists in terms of selecting minors, the execution time can actually vary quite a bit.   Let's take a look at what is occurring by using the {\tt Verbose} option.  We go through the output and explain what each line is telling us.
        Example
            time regularInCodimension(1, S/J, Verbose=>true) 
        Text
            {\bf MaxMinors.}  The first output says that we will compute up to 452.9 minors before giving up.  We can control that by setting the option {\tt MaxMinors}.  
        Example
            time regularInCodimension(1, S/J, MaxMinors=>10, Verbose=>true)
        Text
            There are other finer ways to control the MaxMinors option, but they will not be discussed in this tutorial.  See @TO regularInCodimension@.
        Text
            {\bf Selecting submatrices of the Jacobian.}  We also see output like: ``Choosing LexSmallest'' or ``Choosing Random''.  This is saying how we are selecting a given submatrix.  For instance, we can run:
        Example
            time regularInCodimension(1, S/J, MaxMinors=>10, Strategy=>StrategyRandom, Verbose=>true)
        Text
            and only random submatrices are chosen.  We discuss strategies for choosing submatrices much more generally in the @TO FastMinorsStrategyTutorial@.  
            Regardless, after a certain number of minors have been looked at, we see output lines like:  ``Loop step, about to compute dimension.  Submatrices considered: 7, and computed = 7''.  We only compute minors we haven't considered before.  So as we compute more minors, there can be a distinction between considered and computed.
        Text
            {\bf Computing minors vs considering the dimension of what has been computed.}  Periodically we compute the codimension of the partial ideal of minors we have computed so far.  There are two options to control this.  First, we can tell the function when to first compute the dimension of the working partial ideal of minors.
        Example
            time regularInCodimension(1, S/J, MaxMinors=>10, MinMinorsFunction => t->3, Verbose=>true)
        Text
            {\bf MinMinorsFunction.} We pass {\tt MinMinorsFunction} a function which sends the minimum number of minors needed to verify that something is regular in codimension $n$ (which is always $n+1$) to the number of minors to compute before computing the dimension of the partial ideal of minors for the first time.   You can see that three minors were computed in the above example before we attempt to compute codimension.
        Text
            {\bf CodimCheckFunction.} The option {\tt CodimCheckFunction} controls how frequently the dimension of the partial ideal of minors is computed.  For instance, setting {\tt CodimCheckFunction => t -> t/5} will say it should compute dimension after every 5 minors are examined.  In general, after the output of the CodimCheckFunction increases by an integer we compute the codimension again.  The default function has the space between computations grow exponentially.
        Example            
            time regularInCodimension(1, S/J, MaxMinors=>25, CodimCheckFunction => t->t/5, MinMinorsFunction => t->2, Verbose=>true)
        Text
            {\bf isCodimAtLeast and dim}.  We see the lines about the ``isCodimAtLeast failed''.  This means that {\tt isCodimAtLeast} was not enough on its own to verify that our ring is regular in codimension 1.  After this, ``partial singular locus dimension computed'' indicates we did a complete dimension computation of the partial ideal defining the singular locus.  How {\tt isCodimAtLeast} is called can be controlled via the options {\tt SPairsFunction} and {\tt PairLimit}, which are simply passed to @TO isCodimAtLeast@.  You can force the function to only use {\tt isCodimAtLeast} and not call dimension by setting {\tt UseOnlyFastCodim => true}.
        Example            
            time regularInCodimension(1, S/J, MaxMinors=>25, UseOnlyFastCodim => true, Verbose=>true)
        Text
            This can be useful if the function is hanging when trying to compute the dimension, but you may wish increase {\tt PairLimit}.
        Text
            {\bf Summary.}  If you expect that finding a submatrix or computing a minor is relatively costly from a time perspective, then it makes sense to compute the codimension more frequently.  If computing the codimension is relatively costly we recommend computing the codimension less frequently, or using the {\tt UseOnlyFastCodim => true} with a high {\tt PairLimit}.  For example, if using {\tt StrategyPoints}, then choosing a submatrix can be quite slow, however each submatrix is very ``valuable'', in that adding it to the ideal of minors so far is quite likely to reduce the dimension of the singular locus.
            
            One may also change how minors (determinants of the Jacobian submatrix) are computed by using the @TO DetStrategy@ option.  
    SeeAlso
        regularInCodimension
        FastMinorsStrategyTutorial
        DetStrategy
///

doc ///
    Key
        chooseGoodMinors
        (chooseGoodMinors, ZZ, ZZ, Matrix)
        (chooseGoodMinors, ZZ, ZZ, Matrix, Ideal)
        [chooseGoodMinors, Verbose]
        [chooseGoodMinors, PeriodicCheckFunction]
        PeriodicCheckFunction
    Headline
        returns an ideal generated by interesting minors in a matrix
    Usage
        chooseGoodMinors(count, minorSize, M)
        chooseGoodMinors(count, minorSize, M, I)
    Inputs
        count: ZZ
        minorSize: ZZ
        M: Matrix
        I: Ideal            
        PeriodicCheckFunction => Function
            a function whose argument is the ideal of minors computed so far and which outputs whether the chooseGoodMinors should continue
        PointOptions => List
            options to be passed to the RandomPoints package        
    Outputs
        : Ideal
    Description
        Text
            This returns an ideal generated by approximately {\tt count} minors of size {\tt minorSize} of the matrix M.  
        Example
            R = QQ[x, y, z];
            M = matrix{{x,y,0}, {y,z,0}, {0,0,0}}
            chooseGoodMinors(1, 2, M, Strategy=>StrategyDefaultNonRandom)
        Text
            The ideal {\tt I} is used in the {\tt Points} portion of the {\tt Strategy}.  Only points where that ideal vanishes will be considered.
        Text
            The option {\tt PeriodicCheckFunction} can be set to a function which will periodically evaluate the partially computed ideal of minors via the given function (which should return a boolean value).  If that function returns {\tt true} then {\tt chooseGoodMinors} will terminate.  For instance, one can set it to periodically check whether the dimension of the ideal is at most zero via {\tt PeriodicCheckFunction => (J -> dim J <= 0)}.
///

doc ///
    Key
        chooseSubmatrixSmallestDegree
        (chooseSubmatrixSmallestDegree, ZZ, Matrix)
    Headline
        returns coordinates for low degree submatrix of a matrix
    Usage
        chooseSubmatrixSmallestDegree(n1, M3)
    Inputs
        n1: ZZ
        M3: Matrix
    Outputs
        : List
    Description
        Text
            Returns a list containing the row and column coordinates in the original matrix for a specified sized submatrix of terms lower in degree than other terms of the matrix.
        Example
            R = QQ[x,y,z];
            M=matrix{{x^2,x^3,x^4},{y^4,y^2,y^3},{z^3,z^4,z^2}}
            chooseSubmatrixSmallestDegree(2, M)
        Text
            This returns a list with two entries, first a list of row indices, and then a list of column indices.  In this case, {\tt z^2} is the smallest element, and after removing that row and column from consideration, {\tt y^2} is smallest.  Thus we want rows 2 and 1 and columns 2 and 1.     
///

doc ///
    Key
        chooseSubmatrixLargestDegree
        (chooseSubmatrixLargestDegree, ZZ, Matrix)
    Headline
        returns coordinates for higher degree submatrix of a matrix
    Usage
        chooseSubmatrixLargestDegree(n1, M1)
    Inputs
        n1: ZZ
        M1: Matrix
    Outputs
        : List
    Description
        Text
            This returns a list containing two entries, the row and column coordinates in the original matrix for a specified sized submatrix.  
            In this case, the terms of that submatrix are likely higher in degree than other terms of the matrix.
        Example
            R = QQ[x,y,z];
            M=matrix{{x^2,x^3,x^4},{y^4,y^2,y^3},{z^3,z^4,z^2}}
            chooseSubmatrixLargestDegree(2, M)
        Text
            It returns a list with two entries, first a list of row indices, and then a list of column indices.  In this case, {\tt x^4} is the biggest element, and after removing that row and column from consideration, {\tt y^4} is biggest.  Thus we want rows 0 and 1 and columns 2 and 0.
///

doc ///
    Key
        chooseRandomSubmatrix
        (chooseRandomSubmatrix, ZZ, Matrix)
    Headline
        returns coordinates for a random submatrix
    Usage
        chooseRandomSubmatrix(n1, M1)
    Inputs
        n1: ZZ
        M1: Matrix
    Outputs
        : List
    Description
        Text
            This returns a list with two entries, first a list of random rows, and then a list of random columns.  These specify a random submatrix of the desired size, {\tt n1}.
        Example
            R = QQ[x,y,z];
            M=matrix{{x^2,x^3,x^4},{y^4,y^2,y^3},{z^3,z^4,z^2}};
            chooseRandomSubmatrix(2, M)
///

doc ///
    Key
        isRankAtLeast
        (isRankAtLeast, ZZ, Matrix)
        [isRankAtLeast, Verbose]
        [isRankAtLeast, Threads]
    Headline
        determines if the matrix has rank at least a number
    Usage
        isRankAtLeast(n1, M1)
    Inputs
        n1: ZZ
        M1: Matrix
    Outputs
        : Boolean
    Description
        Text
            This function tries to quickly determine whether the matrix has a given rank.
            {\tt isRankAtLeast} calls @TO getSubmatrixOfRank@.  If that function finds
            a submatrix of a certain rank, this returns true.  If that function
            fails to find a submatrix of a certain rank, this simply calls
            @TO rank@.  To control the number of times {\tt getSubmatrixOfRank} considers
            submatrices, use the option {\tt MaxMinors}.
        Example
            R = QQ[x,y];
            M = matrix{{x,y,2,0,2*x+y}, {0,0,1,0,x}, {x,y,0,0,y}};
            rank M
            isRankAtLeast(2, M)
            isRankAtLeast(3, M)
        Text
            The option {\tt Threads} can be used allow the function use multiple threads of execution.  If {\tt allowableThreads} is above 2 and {\tt Threads} is set above 1, then this function will try to simultaneously compute the rank of the matrix while looking for a submatrix of a certain rank.
    SeeAlso
        getSubmatrixOfRank
        [isRankAtLeast, Strategy]
///

doc ///
    Key
        getSubmatrixOfRank
        (getSubmatrixOfRank, ZZ, Matrix)
        [getSubmatrixOfRank, Verbose]
        [getSubmatrixOfRank, Threads]
    Headline
        tries to find a submatrix of the given rank
    Usage
        getSubmatrixOfRank(n1, M1)
    Inputs
        n1: ZZ
        M1: Matrix
    Outputs
        : List
            the first entry is a list of row indicies, the second is a list of column indices
    Description
        Text
            This function looks at submatrices of the given matrix, and tries to find
            one of the specified rank.  If it succeeds, it returns a list of two lists.
            The first is the list of row indices, the second is the list of columns, of the desired rank submatrix.
            If it fails to find such a matrix,
            the function returns {\tt null}.  The option {\tt MaxMinors} is used to
            control how many minors to consider.  If left {\tt null}, the number
            considered is based on the size of the matrix.
        Example
            R = QQ[x,y];
            M = matrix{{x,y,2,0,2*x+y}, {0,0,1,0,x}, {x,y,0,0,y}};
            l = getSubmatrixOfRank(2, M)
            (M^(l#0))_(l#1)
            l = getSubmatrixOfRank(2, M)
            (M^(l#0))_(l#1)
            getSubmatrixOfRank(3, M)
        Text
            The option {\tt Strategy} is used to used to control how the function computes
            the rank of the submatrices considered.  See @TO [getSubmatrixOfRank, Strategy]@.
            In the future, we hope to speed up the function to use multiple threads of execution, in which case the threading would be controlled by the option {\tt Threads}.
    SeeAlso
        isRankAtLeast
        [getSubmatrixOfRank, Strategy]
///

doc ///
    Key
        PointOptions
        [chooseGoodMinors, PointOptions]
        [getSubmatrixOfRank, PointOptions]
        [isRankAtLeast, PointOptions]
        [projDim, PointOptions]
        [regularInCodimension, PointOptions]
    Headline
        options to pass to functions in the package RandomPoints
    Description
        Text
            {\tt PointOptions} is an option in various functions in this package, which can store options to be passed to the function {\tt findANonZeroMinor} and other functions in {\tt RandomPoints}.  
        Example
            (options regularInCodimension)#PointOptions
            options findANonZeroMinor
        Text
            The default setting {\tt ExtendField => true} means that points whose residue field are finite extensions of the prime field are valid, and are used to study the matrix.  Furthermore, we have set {\tt Homogeneous=>false} by default which means the origin is treated as a valid point.  Setting {\tt ExtendField=>false} will sometimes speed up computation, but can also miss some important submatrices if that determinant (plus what has already been computed) defines a scheme with no or relatively few rational points.  In such a case, {\tt ExtendField => false} will typically substantially slow down computations.
    SeeAlso
        findANonZeroMinor
///

doc ///
    Key
        regularInCodimension
        (regularInCodimension, ZZ, Ring)
        [regularInCodimension, Verbose]
        [regularInCodimension, Modulus]
        [regularInCodimension, MinMinorsFunction]
        [regularInCodimension, CodimCheckFunction]
        [regularInCodimension, PairLimit]
        [regularInCodimension, UseOnlyFastCodim]
        [regularInCodimension, SPairsFunction]
        MinMinorsFunction
        CodimCheckFunction
        UseOnlyFastCodim
    Headline
        attempts to show that the ring is regular in codimension n
    Usage
        regularInCodimension(n, R)
    Inputs
        n: ZZ
        R: Ring
        Modulus => Number
            work modulo the given prime modulus
        PairLimit => Number
            passed to isCodimAtLeast
        SPairsFunction => Function
            passed to isCodimAtLeast
        UseOnlyFastCodim => Boolean
            tell the function not to use the built in dim command and only use isCodimAtLeast
        MinMinorsFunction => Function
            control how many minors are computed before computing codim
        MaxMinors => Function
            how many minors to consider before giving up
        CodimCheckFunction => Function
            control how many minors to compute in between calls to codim
    Outputs
        : 
            true, if the ring is regular in codimension n, false if it determines it is not, and null if no determination is made
    Description
        Text
            This function returns {\tt true} if R is regular in codimension {\tt n}, {\tt false} if it is not, and {\tt null} if it did not make a determination.
            It considers interesting minors of the jacobian matrix to try to verify that the ring is regular in codimension {\tt n}.
            It is frequently much faster at giving an affirmative answer than computing the dimension of the ideal of all minors of the Jacobian.
            We begin with a simple example which is R1, but not R2.
        Example
            R = QQ[x, y, z]/ideal(x*y-z^2);
            regularInCodimension(1, R)
            regularInCodimension(2, R)
        Text
            Next we consider a more interesting example that is R1 but not R2, and highlight the speed differences.  Note that {\tt regularInCodimension(2, R)} returns nothing, as the function did not determine whether the ring was regular in codimension {\tt n}.
        Example
            T = ZZ/101[x1,x2,x3,x4,x5,x6,x7];
            I =  ideal(x5*x6-x4*x7,x1*x6-x2*x7,x5^2-x1*x7,x4*x5-x2*x7,x4^2-x2*x6,x1*x4-x2*x5,x2*x3^3*x5+3*x2*x3^2*x7+8*x2^2*x5+3*x3*x4*x7-8*x4*x7+x6*x7,x1*x3^3*x5+3*x1*x3^2*x7+8*x1*x2*x5+3*x3*x5*x7-8*x5*x7+x7^2,x2*x3^3*x4+3*x2*x3^2*x6+8*x2^2*x4+3*x3*x4*x6-8*x4*x6+x6^2,x2^2*x3^3+3*x2*x3^2*x4+8*x2^3+3*x2*x3*x6-8*x2*x6+x4*x6,x1*x2*x3^3+3*x2*x3^2*x5+8*x1*x2^2+3*x2*x3*x7-8*x2*x7+x4*x7,x1^2*x3^3+3*x1*x3^2*x5+8*x1^2*x2+3*x1*x3*x7-8*x1*x7+x5*x7);
            S = T/I;
            dim S
            time regularInCodimension(1, S)
            time regularInCodimension(2, S)
        Text
            There are numerous examples where {\tt regularInCodimension} is several orders of magnitude faster that calls of {\tt dim singularLocus}.
        Text
            The following is a (pruned) affine chart on an Abelian surface obtained as a product of
	        two elliptic curves.  It is nonsingular, as our function verifies.
            If one does not prune it, then the {\tt dim singularLocus} call takes an enormous amount of time, otherwise the running times of {\tt dim singularLocus} and our function are frequently about the same.
        Example
            R = QQ[c, f, g, h]/ideal(g^3+h^3+1,f*g^3+f*h^3+f,c*g^3+c*h^3+c,f^2*g^3+f^2*h^3+f^2,c*f*g^3+c*f*h^3+c*f,c^2*g^3+c^2*h^3+c^2,f^3*g^3+f^3*h^3+f^3,c*f^2*g^3+c*f^2*h^3+c*f^2,c^2*f*g^3+c^2*f*h^3+c^2*f,c^3-f^2-c,c^3*h-f^2*h-c*h,c^3*g-f^2*g-c*g,c^3*h^2-f^2*h^2-c*h^2,c^3*g*h-f^2*g*h-c*g*h,c^3*g^2-f^2*g^2-c*g^2,c^3*h^3-f^2*h^3-c*h^3,c^3*g*h^2-f^2*g*h^2-c*g*h^2,c^3*g^2*h-f^2*g^2*h-c*g^2*h,c^3*g^3+f^2*h^3+c*h^3+f^2+c);
            dim(R)
            time (dim singularLocus (R))
            time regularInCodimension(2, R)
            time regularInCodimension(2, R)
            time regularInCodimension(2, R)
        Text
            The function works by choosing interesting looking submatrices, computing their determinants, and periodically (based on a logarithmic growth setting), computing the dimension of a subideal of the Jacobian.
            The option {\tt Verbose} can be used to see this in action.
        Example
            time regularInCodimension(2, S, Verbose=>true)
        Text
            The maximum number of minors considered can be controlled by the option {\tt MaxMinors}.  Alternatively, it can be controlled in a more precise way by passing a function to the option {\tt MaxMinors}.  This function should have two inputs; the first is minimum number of minors needed to determine whether the ring is regular in codimension n, and the second is the total number of minors available in the Jacobian.              
            The function {\tt regularInCodimension} does not recompute determinants, so {\tt MaxMinors} or is only an upper bound on the number of minors computed.            
        Example
            time regularInCodimension(2, S, Verbose=>true, MaxMinors=>30)
        Text
            This function has many options which allow you to fine tune the strategy used to find interesting minors.
            You can pass it a {\tt HashTable} specifying the strategy via the option {\tt Strategy}.  See @TO LexSmallest@ for how to construct this {\tt HashTable}.
            The default strategy is {\tt StrategyDefault}, which seems to work well on the examples we have explored.  However, caution must be exercised, because, even in the examples above, certain strategies work well while others do not.  In the Abelian surface example, {\tt LexSmallest} works very well,
            while {\tt LexSmallestTerm} does not even typically correctly identify the ring as nonsingular
            (this is because there are a small number of entries with nonzero constant terms, which are selected repeatedly).
            However, in our first example, the {\tt LexSmallestTerm} is much faster, and {\tt Random} does not perform well at all.
        Example
            StrategyCurrent#Random = 0;
            StrategyCurrent#LexSmallest = 100;
            StrategyCurrent#LexSmallestTerm = 0;
            time regularInCodimension(2, R, Strategy=>StrategyCurrent)
            time regularInCodimension(2, R, Strategy=>StrategyCurrent)
            time regularInCodimension(1, S, Strategy=>StrategyCurrent)
            time regularInCodimension(1, S, Strategy=>StrategyCurrent)
            StrategyCurrent#LexSmallest = 0;
            StrategyCurrent#LexSmallestTerm = 100;
            time regularInCodimension(2, R, Strategy=>StrategyCurrent)
            time regularInCodimension(2, R, Strategy=>StrategyCurrent)
            time regularInCodimension(1, S, Strategy=>StrategyCurrent)
            time regularInCodimension(1, S, Strategy=>StrategyCurrent)
            time regularInCodimension(1, S, Strategy=>StrategyRandom)
            time regularInCodimension(1, S, Strategy=>StrategyRandom)
        Text
            The minimum number of minors computed before checking the codimension can also be controlled by an option {\tt MinMinorsFunction}.  This is should be a function of a single variable, the number of minors computed.  Finally, via the option {\tt CodimCheckFunction}, you can pass the {\tt regularInCodimension} a function which controls how frequently the codimension of the partial Jacobian ideal is computed.  By default this is the floor of {\tt 1.3^k}.  
            Finally, passing the option {\tt Modulus => p} will do the computation after changing the coefficient ring to {\tt ZZ/p}.
        Text
            The options {\tt PairLimit} and {\tt SPairsFunction} are passed directly to {\tt isCodimAtLeast}.  You can turn off internal calls to {\tt codim/dim}, and only use {\tt isCodimAtLeast} by setting {\tt UseOnlyFastCodim => true}.
    SeeAlso
        isCodimAtLeast
///





document {
    Key => {"StrategyDefault", [regularInCodimension, Strategy], GRevLexLargest, GRevLexSmallest, GRevLexSmallestTerm, LexLargest, LexSmallest, LexSmallestTerm, Random, RandomNonzero, Points, [chooseGoodMinors, Strategy], [getSubmatrixOfRank, Strategy],  [isRankAtLeast, Strategy], [projDim, Strategy], "StrategyCurrent",  "StrategyDefaultNonRandom", "StrategyLexSmallest", "StrategyGRevLexSmallest", "StrategyRandom", "StrategyPoints", "StrategyDefaultWithPoints"},
    Headline => "strategies for choosing submatrices",
    "Many of the core functions of this package allow the user to fine tune the strategy used for selecting submatrices.  Different strategies yield markedly different performance or results on various examples.
    These are controlled by specifying a ", TT " Strategy => ", " option, pointing to a ", TT " HashTable", "which specifies several strategies should be used simultaneously, or to a symbol saying we should use only a single strategy.  For a more detailed look at this in an example please see ", TO FastMinorsStrategyTutorial, 
    "Before describing the available strategies, we beging by roughly outlining the different approaches.",
    UL {
        { BOLD "Heuristic submatrix selection:", " In this case, a submatrix is chosen via a greedy algorithm, looking for a submatrix with smallest (or largest) degree with respect to a random monomial order." }, 
        { BOLD "Submatrix selection via rational and geometric points:", " Here a rational or geometric point is found where a given ideal vanishes.  That point is plugged into the matrix and a submatrix of full rank is identified.   This approach currently only works over a finite field and is accomplished with the help of the package ", TO RandomPoints, "."},
        { BOLD "Random submatrix selection:", " This either chooses a completely random submatrix, or a submatrix which has no zero columns or rows."},
    },
    "There we highlight five pre-programmed strategies provided to the user.",
    UL {
        {TT "StrategyDefault", ": this uses a mix of heuristics and random submatrices."},
        {TT "StrategyRandom", ": this uses purely random submatrices."},
        {TT "StrategyDefaultNonRandom", ": this uses a mix of heuristics but no random submatrices."},
        {TT "StrategyPoints", ": this only uses rational / geometric points to find submatrices."},
        {TT "StrategyDefaultWithPoints", ": this uses a mix of heuristics and submatrices chosen with rational and geometric points."},        
    },
    "Below the details of how these strategies are constructed will be detailed below.  But first, we provide an example showing that these strategies can perform quite differently.  The following is the cone over the product of two elliptic curves.  We verify that this ring is regular in codimension 1 using different strategies.  Essentially, minors are computed until it is verified that the ring is regular in codimension 1.",
    EXAMPLE {
        "T=ZZ/7[a..i]/ideal(f*h-e*i,c*h-b*i,f*g-d*i,e*g-d*h,c*g-a*i,b*g-a*h,c*e-b*f,c*d-a*f,b*d-a*e,g^3-h^2*i-g*i^2,d*g^2-e*h*i-d*i^2,a*g^2-b*h*i-a*i^2,d^2*g-e^2*i-d*f*i,a*d*g-b*e*i-a*f*i,a^2*g-b^2*i-a*c*i,d^3-e^2*f-d*f^2,a*d^2-b*e*f-a*f^2,a^2*d-b^2*f-a*c*f,c^3+f^3-i^3,b*c^2+e*f^2-h*i^2,a*c^2+d*f^2-g*i^2,b^2*c+e^2*f-h^2*i,a*b*c+d*e*f-g*h*i,a^2*c+d^2*f-g^2*i,b^3+e^3-h^3,a*b^2+d*e^2-g*h^2,a^2*b+d^2*e-g^2*h,a^3+e^2*f+d*f^2-h^2*i-g*i^2);",         
        "elapsedTime regularInCodimension(1, T, Strategy=>StrategyDefault)",
    },
    "In this particular example, on one machine, we list average time to completion of each of the above strategies after 100 runs.",
    UL {
         {TT "StrategyDefault", ": 1.65 seconds"},
        {TT "StrategyRandom", ": 8.32 seconds"},
        {TT "StrategyDefaultNonRandom", ": 0.99 seconds"},
        {TT "StrategyPoints", ": 3.27 seconds"},
        {TT "StrategyDefaultWithPoints", ": 3.37"}, 
    },
    "Roughly speaking, heuristics tend to provide more information than random submatrices and so they work much faster since they consider far fewer submatrices.  Frequently also, computing random or rational points does have advantages as typically fewer still minors are needed (hence if computing minors is slow ", TT "StrategyPoints",  " is a good choice).  However, sometimes that non-trivial point computation will become stuck (in the above example, the median time for ", TT "StrategyPoints", " and ", TT "StrategyDefaultWithPoints", " was close to 1.5 seconds, but a couple runs in each case were orders of magnitude slower).",    
    BR{}, BR{},    
    {BOLD "Custom Strategies"}, BR{},
    "The user can create their own strategies as well, as we now explain.  In particular, the user can even customize the heuristics used.  See below for how to easily use only a single heuristic.
    To custom strategy is specified by a ", TT "HashTable", " which must have the following keys.",
    UL {
        {TT "GRevLexLargest", ": try to find submatrices where each row and column has a large entry with respect to a random ", TT "GRevLex", "order."},
        {TT "GRevLexSmallest", ": try to find submatrices where each row and column has a small entry with respect to a random ", TT "GRevLex", "order."},
        {TT "GRevLexSmallestTerm", ": find submatrices where each row and column has an entry with a small term with respect to a random ", TT "GRevLex", "order."},
        {TT "LexLargest", ": try to find submatrices where each row and column has a large entry with respect to a random ", TT "Lex", "order."},
        {TT "LexSmallest", ": try to find submatrices where each row and column has a small entry with respect to a random ", TT "Lex", "order."},
        {TT "LexSmallestTerm", ": find submatrices where each row and column has an entry with a small term with respect to a random ", TT "Lex", "order."},
        {TT "Random", ": find random submatrices "},
        {TT "RandomNonzero", ": find random submatrices that have nonzero rows and columns"},
        {TT "Points", ": find submatrices that are not singular at the given ideal by finding a point where that ideal vanishes, and evaluating the matrix at that point (via the package ", TO RandomPoints, ").  If working over a characteristic zero field, this will select random submatrices.  To access options for that package, set the ", TO PointOptions, " option."}
    },    
    "For example:", 
    EXAMPLE {
        "peek StrategyDefault",
    },    
    "Each such key should point to an integer.  The larger the integer, the more likely that such a minor will be chosen.  ",
	BR{},BR{},
    "Functions such as ", TO chooseGoodMinors, " will select a number of random submatrices based on the values of those keys.  For example, if ",
    TT "LexSmallest", " and ", TT "LexLargest", " are set to ", TT "50", " approximately the submatrics will be smallest with respect to ", TT "Lex",
    " and the other half will be largest with respect to ", TT "Lex.", "The values do not need to add up to 100.",
    BR{},BR{},
    "The heuristic functions all work by finding the optimal entry with respect to the given strategy, removing that row and column, and then choosing the next optimal entry.  ",
    "This is done until a submatrix of the desired size has been found.",
    BR{},BR{},
    "In some functions, the ", TT "GRevLex", " versions of this strategy will modify the working matrix in a loop, repeatedly lowering/raising the degree of elements",
    "so as to ensure that different choices are made.",
    BR{},BR{},
    "We briefly summarize the Strategies provided to the user by default (some of which we have seen in action above)",
    UL {
        {TT "StrategyDefault", ": 16% of the matrices are ", TT "LexSmallest", ", ", TT "LexSmallestTerm", ", ", TT "GRevLexSmallest", ", ", TT "GRevLexLargest", ", ", TT "Random", ", and ", TT "RandomNonZero",  " each"},
        {TT "StrategyDefaultNonRandom", ": 25% of the matrices are ", TT "LexSmallest", ", ", TT "LexSmallestTerm", ", ", TT "GRevLexSmallest", " and, ", TT "GRevLexLargest", " each"},
        {TT "StrategyLexSmallest", ": 50% of the matrices are ", TT "LexSmallest", " and 50% are ", TT "LexSmallestTerm"},
        {TT "StrategyGRevLexSmallest", ": 50% of the matrices are ", TT "GRevLexSmallest", " and 50% are ", TT "GRevLexLargest"},
        {TT "StrategyRandom", ": chooses 100% random submatrices."},
        {TT "StrategyPoints", ": choose all submatrices via Points."},
        {TT "StrategyDefaultWithPoints", ": like ", TT "StrategyDefault", " but replaces the ", TT "Random", " and ", "RandomNonZero", " submatrices as with matrices chosen as in Points."},
    },
    "Additionally, a ", TT "MutableHashTable", " named ", TT "StrategyCurrent", " is also exported.  It begins as the default strategy, but the user can modify it.", 
    BR{}, BR{},
    {BOLD "Using a single heuristic  "},
    "Alternatively, if the user only wants to use say ", TT "LexSmallestTerm", " they can set, ", TT "Strategy", " to point to that symbol, instead of a creating a custom strategy HashTable.  For example: ",
    EXAMPLE {              
        "elapsedTime regularInCodimension(1, T, Strategy=>LexSmallestTerm)",
    }
}


doc ///
    Key
        recursiveMinors
        (recursiveMinors, ZZ, Matrix)
        [recursiveMinors, MinorsCache]
        [recursiveMinors, Threads]
        [recursiveMinors,Verbose]
        MinorsCache
    Headline
        uses a recursive cofactor algorithm to compute the ideal of minors of a matrix
    Usage
        I = recursiveMinors(n, M, Threads=>t, MinorsCache=>b)
    Inputs
        n: ZZ
           the size of minors to compute
        M: Matrix           
        t: ZZ
           an optional input, which describes the number of threads to uses
        b: Boolean
           an optional input, which says whether to cache in input
    Outputs
        I: Ideal
           the ideal of minors of M
    Description
        Text
           Given a matrix $M$, this computes the ideal of determinants of size $n \times n$ submatrices.
           The {\tt recursiveMinors} function uses a recursive strategy, keeping track of the smaller minors computed so far, unlike the built-in {\tt Cofactor} strategy for {\tt minors}
        Example
           R = QQ[x,y];
           M = random(R^{5,5,5,5,5,5}, R^7);
           time I2 = recursiveMinors(4, M, Threads=>0);
           time I1 = minors(4, M, Strategy=>Cofactor);
           I1 == I2
    SeeAlso
        minors
///

doc ///
    Key
        projDim
        (projDim, Module)
        [projDim, Verbose]
        [projDim, MinDimension]
    Headline
        finds an upper bound for the projective dimension of a module
    Usage
        n = projDim(N, MinDimension=>d)
    Inputs
        N: Module
            a module over a polynomial ring
        d: ZZ
            the minimum projective dimension of the module
        MinDimension => Number
            stop after verifying the module has at most a certain projective dimension
        PointOptions => List
            options to be passed to the RandomPoints package        
        MaxMinors => 
            used to control how many minors are computed of the matrices in a projective resolution
    Outputs
        n: ZZ
           an upper bound for the projective dimension of N
    Description
        Text
            The function {\tt pdim} returns the length of a projective resolution.
            If the module passed is not homogeneous, then the projective resolution may not be minimal and so {\tt pdim} can
            give the wrong answer.  This function {\tt projDim} tries to improve this bound
            by considering ideals of appropriately sized minors of the resolution (starting from the end of the resolution and working backwards).
            Using the option {\tt MinDimension} (default value 0)
            gives a lower bound on the projective dimension, increasing it can thus improve the speed of computation.
        Example
            R = QQ[x,y];
            I = ideal((x^3+y)^2, (x^2+y^2)^2, (x+y^3)^2, (x*y)^2);
            pdim(module I)
            time projDim(module I, Strategy=>StrategyRandom)
            time projDim(module I, Strategy=>StrategyRandom, MinDimension => 1)
        Text
            The option {\tt MaxMinors} can be used to control how many minors are computed at each step.
            If this is not specified, the number of minors is a function of the dimension $d$ of the polynomial ring and the possible minors $c$. 
            Specifically it is {\tt 10 * d + 2 * log_1.3(c)}.    
            Otherwise the user can set the option {\tt MaxMinors => ZZ} to specify that a fixed integer is used for
            each step.  Alternatively, the user can control the number of minors computed at each step by setting the option {\tt MaxMinors => List}.  In this case, the list specifies how many minors to be computed at each step, (working backwards).  
            Finally, you can also set {\tt MaxMinors} to be a custom function of the dimension $d$ of the polynomial ring and the maximum number of minors.
    SeeAlso
        pdim
///


doc ///
    Key
        MinDimension
    Headline
        an option for projDim
    Description
        Text
            This option is used to tell the function {\tt projDim} not to look for projective dimension below the option value.
    SeeAlso
        projDim
///

doc ///
    Key
        Modulus
    Headline
        an option for regularInCodimension
    Description
        Text
            This option is used to tell the function to do the computation modulo a prime p.
    SeeAlso
        regularInCodimension
///

doc ///
    Key
        Threads
    Headline
        an option for various functions
    Description
        Text
            Increasing this function may tell various functions to multithread their operations.  You may also want to increase {\tt allowableThreads}.
    SeeAlso
        isRankAtLeast
        getSubmatrixOfRank
        recursiveMinors
///

doc ///
    Key
        MaxMinors
        [getSubmatrixOfRank, MaxMinors]
        [isRankAtLeast, MaxMinors]
        [regularInCodimension, MaxMinors]
        [projDim, MaxMinors]
    Headline
        an option to control depth of search
    Description
        Text
            This option controls how many minors various functions consider.  Increasing it will make certain functions search longer, but may make them give more useful outputs.  The functions {\tt projDim} and {\tt regularInCodimension} can also take in more complicated inputs.  See their documentation for details.
    SeeAlso
        regularInCodimension
        projDim
        isRankAtLeast
        getSubmatrixOfRank
///

doc ///
    Key
        reorderPolynomialRing
        (reorderPolynomialRing, Symbol, Ring)
    Headline
        produces an isomorphic polynomial ring with a different, randomized, monomial order
    Usage
        R1 = reorderPolynomialRing(orderType, R)
    Inputs
        R: Ring
            a polynomial ring
        orderType: Symbol
            a valid monomial order, such as {\tt GRevLex}
    Outputs
        S: Ring
           a polynomial ring with a new random monomial order
    Description
        Text
            This function takes a polynomial ring and produces a new polynomial ring with {\tt MonomialOrder} of type {\tt orderType}.
            The order of the variables is randomized.
        Example
            R = QQ[x,y,z,w];
            x > y and y > z and z > w
            use reorderPolynomialRing(GRevLex, R)
            x > y
            y > z
            z > w
///

doc ///
    Key
        DetStrategy
        Recursive
        Rank
        [chooseGoodMinors, DetStrategy]
        [getSubmatrixOfRank, DetStrategy]
        [isRankAtLeast, DetStrategy]
        [projDim, DetStrategy]
        [regularInCodimension, DetStrategy]        
    Headline
        DetStrategy is a strategy for allowing the user to choose how determinants (or rank), is computed
    Description
        Text
            Passing the option {\tt DetStrategy => Symbol} controls how certain functions compute determinants (or in some cases, rank).
            For all methods, {\tt Bareiss}, {\tt Cofactor} and {\tt Recursive} are valid options.  The first two come included with Macaulay2,
            the third is just a call to @TO recursiveMinors@.
        Text
            Additionally, for the methods {\tt getSubmatrixOfRank} and {\tt isRankAtLeast},
            one can also pass {\tt DetStrategy} the option {\tt Rank} (the default).  Not using {\tt Rank} tells
            the functions to check rank by computing the determinant, instead of using the internal
            {\tt rank} function.  This is not usually recommended, but sometimes it can be effective.
    SeeAlso
        recursiveMinors
        Bareiss
        Cofactor
        rank
///

doc ///
    Key
        isCodimAtLeast
        (isCodimAtLeast, ZZ, Ideal)
        [isCodimAtLeast, Verbose]
        [isCodimAtLeast, SPairsFunction]
        [isCodimAtLeast, PairLimit]
        SPairsFunction
    Headline
        returns true if we can quickly see whether the codim is at least a given number
    Usage
        isCodimAtLeast(n, I)
    Inputs
        n: ZZ
            an integer
        I: Ideal
            an ideal in a polynomial ring over a field, or a quotient ring
        SPairsFunction => Function
            a function to control how when the codimension of minors is computed, default is i->ceiling(1.5^i)
        PairLimit => Number
            the max value to be plugged into SPairsFunction
    Outputs
        : 
           {\tt true} if the codimension of I is at least n or {\tt null} if the function cannot tell whether the codimension is at least n        
    Description
        Text
            This computes a partial Groebner basis, takes the initial terms, and checks whether that (partial) initial ideal has codimension at least {\tt n}.  
            Consider the following example.  We create an ideal of 15 minors of the matrix {\tt myDiff} (a matrix constructed in a way typical of applications).  We would like to verify that the codimension of this ideal is at least 3.  The built-in {\tt codim} function typically does not terminate. However, {\tt isCodimAtLeast} is normally very fast.
        Example
            R = ZZ/127[x_1 .. x_(12)];
            P = minors(3,genericMatrix(R,x_1,3,4));
            C = res (R^1/(P^3));
            myDiff = C.dd_3;
            r = rank myDiff;
            J = chooseGoodMinors(15, r, myDiff, Strategy=>StrategyDefaultNonRandom);
            time isCodimAtLeast(3, J)
        Text
            The function works by computing {\tt gb(I, PairLimit=>f(i))} for successive values of {\tt i}.  Here {\tt f(i)} is a function that takes {\tt t}, some approximation of the base degree value
            of the polynomial ring (for example, in a standard graded polynomial ring, this is probably expected to be {\tt \{1\}}).  And {\tt i} is a counting variable.  
            You can provide your own function by calling {\tt isCodimAtLeast(n, I, SPairsFunction=>( (i) -> f(i) )}, the default function is {\tt SPairsFunction=>i->ceiling(1.5^i)}   Perhaps more commonly however, the user may want to 
            instead tell the function to compute for larger values of {\tt i}.  This is done via the option {\tt PairLimit}.  This is the max value of {\tt i} to be plugged into {\tt SPairsFunction} before the function gives up.  In other words, {\tt PairLimit=>5} will tell the function to check codimension 5 times.
        Example
            I = ideal(x_2^8*x_10^3-3*x_1*x_2^7*x_10^2*x_11+3*x_1^2*x_2^6*x_10*x_11^2-x_1^3*x_2^5*x_11^3,x_5^5*x_6^3*x_11^3-3*x_5^6*x_6^2*x_11^2*x_12+3*x_5^7*x_6*x_11*x_12^2-x_5^8*x_12^3,x_1^5*x_2^3*x_4^3-3*x_1^6*x_2^2*x_4^2*x_5+3*x_1^7*x_2*x_4*x_5^2-x_1^8*x_5^3,x_6^8*x_11^3-3*x_5*x_6^7*x_11^2*x_12+3*x_5^2*x_6^6*x_11*x_12^2-x_5^3*x_6^5*x_12^3,x_8^3*x_10^8-3*x_7*x_8^2*x_10^7*x_11+3*x_7^2*x_8*x_10^6*x_11^2-x_7^3*x_10^5*x_11^3,x_2^8*x_4^3-3*x_1*x_2^7*x_4^2*x_5+3*x_1^2*x_2^6*x_4*x_5^2-x_1^3*x_2^5*x_5^3,-x_6^3*x_11^8+3*x_5*x_6^2*x_11^7*x_12-3*x_5^2*x_6*x_11^6*x_12^2+x_5^3*x_11^5*x_12^3,-x_6^3*x_7^3*x_9^5+3*x_4*x_6^2*x_7^2*x_9^6-3*x_4^2*x_6*x_7*x_9^7+x_4^3*x_9^8,x_8^8*x_10^3-3*x_7*x_8^7*x_10^2*x_11+3*x_7^2*x_8^6*x_10*x_11^2-x_7^3*x_8^5*x_11^3,x_2^5*x_3^3*x_11^3-3*x_2^6*x_3^2*x_11^2*x_12+3*x_2^7*x_3*x_11*x_12^2-x_2^8*x_12^3);
            time isCodimAtLeast(5, I, PairLimit => 5, Verbose=>true)            
            time isCodimAtLeast(5, I, PairLimit => 200, Verbose=>false)            
        Text
            Notice in the first case the function returned {\tt null}, because the depth of search was not high enough.  It only computed {\tt codim} 5 times.  The second returned true, but it did so as soon as the answer was found (and before we hit the {\tt PairLimit} limit).
///

doc ///
    Key
        isDimAtMost
        (isDimAtMost, ZZ, Ideal)
        [isDimAtMost, Verbose]
        [isDimAtMost, SPairsFunction]
        [isDimAtMost, PairLimit]
    Headline
        returns true if we can quickly see whether the dim is at most a given number
    Usage
        isDimAtMost(n, I)
    Inputs
        n: ZZ
            an integer
        I: Ideal
            an ideal in a polynomial ring over a field, or a quotient ring of such
    Outputs
        : 
           {\tt true} if the dimension of I is at most n or {\tt null} if the function cannot tell whether the dimension is at most n
    Description
        Text
            This simply calls {\tt isCodimAtLeast}, passing options as described there.
    SeeAlso
        isCodimAtLeast
///

TEST /// --check #0 (regularInCodimension)
R = QQ[x,y,z]/ideal(x^2-y*z);
assert(regularInCodimension(1, R)=== true);
///

TEST /// --check #1 (regularInCodimension)
T = ZZ/101[x1,x2,x3,x4,x5,x6,x7];
I =  ideal(x5*x6-x4*x7,x1*x6-x2*x7,x5^2-x1*x7,x4*x5-x2*x7,x4^2-x2*x6,x1*x4-x2*x5,x2*x3^3*x5+3*x2*x3^2*x7+8*x2^2*x5+3*x3*x4*x7-8*x4*x7+x6*x7,x1*x3^3*x5+3*x1*x3^2*x7+8*x1*x2*x5+3*x3*x5*x7-8*x5*x7+x7^2,x2*x3^3*x4+3*x2*x3^2*x6+8*x2^2*x4+3*x3*x4*x6-8*x4*x6+x6^2,x2^2*x3^3+3*x2*x3^2*x4+8*x2^3+3*x2*x3*x6-8*x2*x6+x4*x6,x1*x2*x3^3+3*x2*x3^2*x5+8*x1*x2^2+3*x2*x3*x7-8*x2*x7+x4*x7,x1^2*x3^3+3*x1*x3^2*x5+8*x1^2*x2+3*x1*x3*x7-8*x1*x7+x5*x7);
assert((regularInCodimension(1,T/I) === true) or (regularInCodimension(1,T/I) === true));
///

TEST /// --check #2 (ProjDim)
R = QQ[x,y];
I = ideal((x^3+y)^2, (x^2+y^2)^2, (x+y^3)^2, (x*y)^2);
assert(projDim(module I, Strategy=>StrategyDefault)==1);
///

TEST /// --check #3 (regularInCodimension, Modulus)
R = (QQ)[YY_1, YY_2, YY_3, YY_4, YY_5, YY_6, YY_7, YY_8, YY_9];
J =  ideal(YY_8^2-YY_7*YY_9,YY_6*YY_8-YY_5*YY_9,YY_3*YY_8-YY_2*YY_9,YY_2*YY_8-YY_1*YY_9,YY_6*YY_7-YY_5*YY_8,YY_3*YY_7-YY_1*YY_9,YY_2*YY_7-YY_1*YY_8,YY_6^2-YY_4*YY_9,YY_5*YY_6-YY_4*YY_8,YY_4*YY_6+YY_1*YY_8-10*YY_1*YY_9-YY_
     2*YY_9+10*YY_3*YY_9,YY_3*YY_6-YY_8*YY_9-10*YY_9^2,YY_2*YY_6-YY_7*YY_9-10*YY_8*YY_9,YY_1*YY_6-YY_7*YY_8-10*YY_7*YY_9,YY_5^2-YY_4*YY_7,YY_4*YY_5+YY_1*YY_7-10*YY_1*YY_8-YY_1*YY_9+10*YY_2*YY_9,YY_3*YY_5-YY_7*YY_9-10*YY_8
     *YY_9,YY_2*YY_5-YY_7*YY_8-10*YY_7*YY_9,YY_1*YY_5-YY_7^2-10*YY_7*YY_8,YY_4^2+YY_7^2-YY_9^2,YY_3*YY_4-YY_5*YY_9-10*YY_6*YY_9,YY_2*YY_4-YY_5*YY_8-10*YY_5*YY_9,YY_1*YY_4-YY_5*YY_7-10*YY_5*YY_8,YY_2^2-YY_1*YY_3,YY_1*YY_2-
     10*YY_1*YY_3-YY_2*YY_3+10*YY_3^2+YY_4*YY_8+10*YY_4*YY_9,YY_1^2-YY_3^2+YY_4*YY_7+20*YY_4*YY_8-YY_4*YY_9);
assert((regularInCodimension(1, R/J, MaxMinors=>15)===null) and (regularInCodimension(1, R/J, MaxMinors=>15, Modulus=>7)===null));
///

TEST/// --check #4 (regularInCodimension)
T = (ZZ/101)[YY_1, YY_2, YY_3, YY_4, YY_5];
J = ideal(YY_2*YY_3+3*YY_3^2+43*YY_1*YY_4+YY_3*YY_4-43*YY_2*YY_5+50*YY_3*YY_5,YY_2^2-18*YY_3^2+YY_1*YY_4-18*YY_2*YY_4-39*YY_3*YY_4-YY_1*YY_5+8*YY_2*YY_5-47*YY_3*YY_5,YY_1^2+16*YY_1*YY_2-3*YY_1*YY_3-32*YY_3^2+23*YY_1*YY_4-42*YY_2*YY_4-43*YY_3*YY_4+19*YY_1*YY_5+20*YY_2*YY_5-34*YY_3*YY_5,YY_3^3+16*YY_1*YY_2*YY_4+8*YY_1*YY_3*YY_4-36*YY_3^2*YY_4-YY_1*YY_4^2-47*YY_3*YY_4^2+45*YY_1*YY_3*YY_5-31*YY_3^2*YY_5+7*YY_1*YY_4*YY_5+16*YY_2*YY_4*YY_5+37*YY_3*YY_4*YY_5-16*YY_1*YY_5^2+36*YY_2*YY_5^2+15*YY_3*YY_5^2,YY_1*YY_3^2-48*YY_1*YY_2*YY_4+21*YY_1*YY_3*YY_4-45*YY_3^2*YY_4+47*YY_1*YY_4^2+10*YY_2*YY_4^2-47*YY_3*YY_4^2+13*YY_4^3-25*YY_1*YY_2*YY_5-33*YY_1*YY_3*YY_5+45*YY_3^2*YY_5+24*YY_1*YY_4*YY_5-36*YY_2*YY_4*YY_5-41*YY_3*YY_4*YY_5+26*YY_4^2*YY_5-27*YY_1*YY_5^2+30*YY_2*YY_5^2-13*YY_3*YY_5^2-24*YY_4*YY_5^2-17*YY_5^3);
assert(regularInCodimension(1, T/J)===true);
--we should change these a bit
--assert(regularInCodimension(1, T/J, LexSmallest=>0, Random=>0)===true);
-- note: if we set LexLargest=>0 running just on LexSmallest then regularInCodimension returns null
-- same happens for all =>0, GRevLexSmallest=> nonzero
-- GRevLexLargest works
-- assert(regularInCodimension(1, T/J, LexLargest=>0, LexSmallest=>0, GRevLexLargest=>20, Random=>0)===true);
-- assert(regularInCodimension(1, T/J, LexLargest=>0, LexSmallest=>0)===true);
-- assert(regularInCodimension(1, T/J, LexLargest=>0, LexSmallest=>0, Random=>0, RandomNonzero=>20)===true);
///

TEST/// --check #5, this is an affine chart on an Abelian surface, it is nonsingular
S = QQ[c, f, g, h];
J = ideal(g^3+h^3+1,f*g^3+f*h^3+f,c*g^3+c*h^3+c,f^2*g^3+f^2*h^3+f^2,c*f*g^3+c*f*h^3+c*f,c^2*g^3+c^2*h^3+c^2,f^3*g^3+f^3*h^3+f^3,c*f^2*g^3+c*f^2*h^3+c*f^2,c^2*f*g^3+c^2*f*h^3+c^2*f,c^3-f^2-c,c^3*h-f^2*h-c*h,c^3*g-f^2*g-c*g,c^3*h^2-f^2*h^2-c*h^2,c^3*g*h-f^2*g*h-c*g*h,c^3*g^2-f^2*g^2-c*g^2,c^3*h^3-f^2*h^3-c*h^3,c^3*g*h^2-f^2*g*h^2-c*g*h^2,c^3*g^2*h-f^2*g^2*h-c*g^2*h,c^3*g^3+f^2*h^3+c*h^3+f^2+c);
assert((regularInCodimension(2, S/J) === true) or (regularInCodimension(2, S/J) === true));
///

TEST /// --check #6, we found this example by dehomogenizing a homongeneous example, pdim does not provide the correct answer (of course, it does if you rehomogenize)
S = QQ[t_0, t_1, t_2, t_3, t_4, t_5];
J = ideal(-t_2^3+2*t_1*t_2*t_3-t_0*t_3^2-t_1^2*t_4+t_0*t_2*t_4,-t_2^2*t_3+t_1*t_3^2+t_1*t_2*t_4-t_0*t_3*t_4-t_1^2*t_5+t_0*t_2*t_5,-t_2*t_3^2+t_2^2*t_4+t_1*t_3*t_4-t_0*t_4^2-t_1*t_2*t_5+t_0*t_3*t_5,-t_3^3+2*t_2*t_3*t_4-t_1*t_4^2-t_2^2*t_5+t_1*t_3*t_5,-t_2^2*t_4+t_1*t_3*t_4+t_1*t_2*t_5-t_0*t_3*t_5-t_1^2+t_0*t_2,-t_2*t_3*t_4+t_1*t_4^2+t_2^2*t_5-t_0*t_4*t_5-t_1*t_2+t_0*t_3,-t_3^2*t_4+t_2*t_4^2+t_2*t_3*t_5-t_1*t_4*t_5-t_2^2+t_1*t_3,-t_2*t_4^2+t_2*t_3*t_5+t_1*t_4*t_5-t_0*t_5^2-t_1*t_3+t_0*t_4,
    -t_3*t_4^2+t_3^2*t_5+t_2*t_4*t_5-t_1*t_5^2-t_2*t_3+t_1*t_4,-t_4^3+2*t_3*t_4*t_5-t_2*t_5^2-t_3^2+t_2*t_4);
assert(projDim(module J, MinDimension=>2) == 2)
///

TEST /// --check #7 --choose submatrix largest degree
    R = ZZ/7[x];
    M = matrix{ {x, x^3, x^9}, {x^2, x^8, x^10}, {x^4, x^5, x^6}};
        --the largest elements are in row 1, column 2, then row 2, column, 1. (answer should be {1,2} (rows) and {2,1} (cols))
    myList := chooseSubmatrixLargestDegree(2, M);
    assert(myList#0 == {1, 2});
    assert(myList#1 == {2, 1});
///

TEST /// --check #8 --choose submatrix smallest degree
    R = ZZ/7[x];
    M = matrix{ {x, x^3, x^9}, {x^2, x^8, x^10}, {x^4, x^5, x^6}};
        --the smallest elements are in row 0, column 0, then row 2, column, 1.  (answer should be {0,2} (rows) and {0, 1} (cols))
    myList := chooseSubmatrixSmallestDegree(2, M);
    assert(myList#0 == {0, 2});
    assert(myList#1 == {0, 1});
///

TEST /// --check #9 --choose submatrix largest degree
    R = ZZ/7[x];
    M = matrix{ {x^14, x, x^3, x^9}, {x^2, x^8, x^10, x^13}, {x^11 ,x^4,  x^5, x^6}, {x^7, x^12, x^15, 1}};
        --the largest elements are in row 3, column 2, then row 0, column 0, then row 1, column 3.  {3, 0, 1}, {2, 0, 3}
    myList := chooseSubmatrixLargestDegree(3, M);
    assert(myList#0 == {3,0,1});
    assert(myList#1 == {2,0,3});
///

TEST /// --check #10 --choose submatrix largest degree
    R = ZZ/7[x];
    M = matrix{ {x^14, x, x^3, x^9}, {x^2, x^8, x^10, x^13}, {x^11 ,x^4,  x^5, x^6}, {x^7, x^12, x^15, 1}};
        --the smallest elements are in row 3, column 3, then row 0, column 1, then row 1, column 0 {3, 0, 1}, {3, 1, 0}
    myList := chooseSubmatrixSmallestDegree(3, M);
    assert(myList#0 == {3,0,1});
    assert(myList#1 == {3,1,0});
///

TEST /// --check #11, --reorderPolynomialRing
    R = ZZ/7[x,y,z,w];
    S = reorderPolynomialRing(GRevLex, R);
    assert(#first entries vars S == 4);
    assert( not (R === S));
    R1 = QQ[u];
    S1 = reorderPolynomialRing(Lex, R1);
    assert(#first entries vars S1 == 1);
    assert( not (R1 === S1));
///

TEST ///--check #12, --isRankAtLeast
    R = ZZ/101[x,y,z];
    M = random(R^4, R^{-3, -4, -2, -3});
    assert(isRankAtLeast(3, M));
    assert(isRankAtLeast(3, M, Strategy=>StrategyRandom));
    assert(isRankAtLeast(3, M, Strategy=>StrategyGRevLexSmallest));
    assert(isRankAtLeast(3, M, Strategy=>StrategyLexSmallest));
///

TEST /// --check #13 (ProjDim)
R = QQ[x,y,z,w];
I = ideal(x^4,x*y,w^3, y^4);
f = map(R, R, {x+1, x+y+1, z+x-2, w+y+1});
g =  map(R, R, {sub(x+x^2+1, R), x+y+1, z+z^4+x-2, w+w^5+y+1});
assert(projDim(module f I, Strategy=>StrategyDefault)==2);
assert(projDim(module g I, Strategy=>StrategyDefault, MinDimension=>2)==2);
///

TEST /// --check #14 (isCodimAtLeast)
R = ZZ/7[x,y,z];
I = ideal(x^2+z,y);
assert(isCodimAtLeast(1, I))
assert(isCodimAtLeast(2, I))
assert(null === isCodimAtLeast(3, I))
///

TEST /// --check #15 (isDimAtMost)
R = ZZ/7[x,y,u,v];
I = ideal(x*u,x*v,y*u,y*v);
assert(null === isDimAtMost(1, I));
assert(true === isDimAtMost(2, I));
assert(true === isDimAtMost(3, I))
///

TEST /// --check #16 (checking various strategies)
T = ZZ/101[x1,x2,x3,x4,x5,x6,x7];
 I =  ideal(x5*x6-x4*x7,x1*x6-x2*x7,x5^2-x1*x7,x4*x5-x2*x7,x4^2-x2*x6,x1*x4-x2*x5,x2*x3^3*x5+3*x2*x3^2*x7+8*x2^2*x5+3*x3*x4*x7-8*x4*x7+x6*x7,x1*x3^3*x5+3*x1*x3^2*x7+8*x1*x2*x5+3*x3*x5*x7-8*x5*x7+x7^2,x2*x3^3*x4+3*x2*x3^2*x6+8*x2^2*x4+3*x3*x4*x6-8*x4*x6+x6^2,x2^2*x3^3+3*x2*x3^2*x4+8*x2^3+3*x2*x3*x6-8*x2*x6+x4*x6,x1*x2*x3^3+3*x2*x3^2*x5+8*x1*x2^2+3*x2*x3*x7-8*x2*x7+x4*x7,x1^2*x3^3+3*x1*x3^2*x5+8*x1^2*x2+3*x1*x3*x7-8*x1*x7+x5*x7);
 R=T/I;
assert(regularInCodimension(1, R, Strategy=>StrategyDefault));
assert(regularInCodimension(1, R, Strategy=>StrategyDefaultNonRandom));
assert(regularInCodimension(1, R, Strategy=>StrategyDefaultWithPoints, MinMinorsFunction => x->x));
assert(regularInCodimension(1, R, Strategy=>StrategyGRevLexSmallest));
assert(regularInCodimension(1, R, Strategy=>StrategyLexSmallest));
assert(regularInCodimension(1, R, Strategy=>StrategyRandom));
assert(regularInCodimension(1, R, Strategy=>StrategyPoints, MinMinorsFunction => x->x, CodimCheckFunction => x -> x));
///




end
