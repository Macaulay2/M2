

cartesianProductBags = theBags -> (
    ans := toList theBags#0;
    for i from 1 to #theBags-1 do (
        ans = ans ** toList theBags#i;
        );
    Bag (ans / deepSplice)
    )

maxSSYT = method(TypicalValue => YoungTableau)
maxSSYT (Partition,Partition) := (lam,mu) -> (
    tempT := youngTableau(lam,mu);
    (lam,mu) = standardize skewShape tempT;

    entryList := for entryIndex from 0 to sum toList lam - sum toList mu - 1 list (
        (i,j) := toPosition(entryIndex,tempT);
        theCol := tempT_j;
        theColAbove := theCol_(toList(0..i));
        #(delete(null,theColAbove))
        );

    youngTableau(lam,mu,entryList)
    )

minSSYT = method(TypicalValue => YoungTableau)
minSSYT (Partition,Partition,ZZ) := (lam,mu,maxEntry) -> (
    tempT := youngTableau(lam,mu);
    (lam,mu) = standardize skewShape tempT;
    
    entryList := for entryIndex from 0 to sum toList lam - sum toList mu - 1 list (
        (i,j) := toPosition(entryIndex,tempT);
        theCol := tempT_j;
        theColBelow := theCol_(toList((i+1)..(#theCol-1)));
        maxEntry - #(delete(null,theColBelow))
        );

    youngTableau(lam,mu,entryList)
    )

addOneSSYT = method(TypicalValue => YoungTableau)
addOneSSYT (YoungTableau,Sequence,Partition,Partition) := (T,thePosition,lam,mu) -> (
    (rowIndex,colIndex) := thePosition;
    entryList := new MutableList from entries T;

    maxRowIndex :=  max select(rowIndex..(#lam-1), i -> lam#i > colIndex and mu#i <= colIndex);
    for currRowIndex from rowIndex to maxRowIndex  do (
        for currColIndex from colIndex to lam#currRowIndex-1 do (
            theIndex := toIndex((currRowIndex,currColIndex),T);
            
            currBox := entryList#theIndex;
            
            isBoxLeft := currColIndex > mu#currRowIndex;
            leftBox := if isBoxLeft then (
                leftIndex := toIndex((currRowIndex,currColIndex-1),T);
                entryList#leftIndex
                ) else (
                0
                );
            
            isBoxAbove := currRowIndex >= 1 and currColIndex >= mu#(currRowIndex-1) and currColIndex < lam#(currRowIndex-1);
            aboveBox := if isBoxAbove then (
                aboveIndex := toIndex((currRowIndex-1,currColIndex),T);
                entryList#aboveIndex
                ) else (
                0
                );

            if thePosition == (currRowIndex,currColIndex) or currBox < leftBox or currBox <= aboveBox then (
                entryList#theIndex += 1;
                );
            );
        );

    youngTableau(lam,mu,toList entryList)
    )

allSemistandardTableaux = method(TypicalValue => Bag)
allSemistandardTableaux (Partition,Partition,ZZ) := (lam,mu,maxEntry) -> (
    (lam,mu) = standardize (lam,mu);
    (lamList,muList) := (toList lam, toList mu);

    if rsort lamList != lamList or rsort muList != muList then error "expected partitions to be weakly decreasing";
    
    if any(0..(#lam-1), i -> mu#i > lam#i) then return Bag {};

    T := youngTableau(lam,mu);

    if #lam == 0 then return Bag {youngTableau(new Partition from {})};
    if any(columnRange T,i -> #columnEntries(i,T) > maxEntry) then return Bag {};

    maxT := maxSSYT(lam,mu);
    minT := minSSYT(lam,mu,maxEntry);
    
    recurse := (anIndex,T) -> (
        canAddOneSSYT := (entries T)#anIndex < (entries minT)#anIndex;
        if canAddOneSSYT then (
            newT := addOneSSYT(T,toPosition(anIndex,T),lam,mu);

            flatten ({newT} | for i from 1 to -anIndex list recurse(-i,newT))
            ) else (
            {}
            )
        );

    ans := {maxT} | flatten parallelApply(1..(size T), theIndex -> recurse(-theIndex,maxT));
    --ans := {maxT} | flatten for theIndex from 1 to size T list recurse(-theIndex,maxT);

    Bag ans
    )
allSemistandardTableaux (Partition,Partition) := (lam,mu) -> (
    (lam,mu) = standardize (lam,mu);
    maxEntry := #lam;
    
    allSemistandardTableaux(lam,mu,maxEntry)
    )
allSemistandardTableaux (Partition,ZZ) := (lam,maxEntry) -> (
    mu := new Partition from {0};
    
    allSemistandardTableaux(lam,mu,maxEntry)
    )
allSemistandardTableaux Partition := lam -> (
    mu := new Partition from {0};
    (lam,mu) = standardize (lam,mu);
    maxEntry := #lam;
    
    allSemistandardTableaux(lam,mu,maxEntry)
    )
allSemistandardTableaux List := shapeList -> (
    cartesianProductBags for theShape in shapeList list allSemistandardTableaux theShape
    )

numSemistandardTableaux = method(TypicalValue => ZZ)
numSemistandardTableaux (Partition,ZZ) := (lam,n) -> (
    if n < #lam then return 0;
    
    numAppendedZeros := n - #lam;
    lam = new Partition from (toList(lam)|toList(numAppendedZeros:0));

    T := youngTableau(lam);

    theProd := product flatten for rowIndex from 0 to #lam-1 list (
        for colIndex from 0 to lam#rowIndex-1 list (
            n + boxContent(rowIndex,colIndex)
            )
        );

    theDiv := product flatten for rowIndex from 0 to #lam-1 list (
        for colIndex from 0 to lam#rowIndex-1 list (
            hookLength((rowIndex,colIndex),T)
            )
        );

    theProd//theDiv
    )
numSemistandardTableaux Partition := lam -> numSemistandardTableaux(trim lam,# trim lam)

randomSemistandardTableau = method(TypicalValue => YoungTableau)
randomSemistandardTableau (Partition,Partition,ZZ) := (lam,mu,maxEntry) -> (
    maxT := maxSSYT(lam,mu);
    minT := minSSYT(lam,mu,maxEntry);

    T := mutableYoungTableau(lam,mu,entries maxT);

    for thePosition in reverse positionList T do (
        (rowIndex,colIndex) := thePosition;

        hasBoxRight := colIndex < lam#rowIndex - 1;
        hasBoxBelow := rowIndex < #lam-1 and colIndex < lam#(rowIndex+1);
        entryRight := if hasBoxRight then T_(rowIndex,colIndex+1) else infinity;
        entryBelow := if hasBoxBelow then T_(rowIndex+1,colIndex) else infinity;

        minEntry := maxT_thePosition;
        maxEntry := min(minT_thePosition,entryRight,entryBelow - 1);
        --possibleEntries := toList(minEntry .. maxEntry);
        possibleEntries := join toSequence for i from minEntry to maxEntry list toList((2^(i-1)):i);
        
        T_thePosition = possibleEntries_(random(#possibleEntries));
        );

    youngTableau(lam,mu,new List from entries T)
    )
randomSemistandardTableau (Partition,ZZ) := (lam,maxEntry) -> randomSemistandardTableau(lam,new Partition from {},maxEntry)
randomSemistandardTableau (Partition,Partition) := (lam,mu) -> randomSemistandardTableau(lam,mu,#lam)
randomSemistandardTableau Partition := lam -> randomSemistandardTableau(lam,new Partition from {},#lam)

numStandardTableaux = method(TypicalValue => ZZ)
numStandardTableaux Partition := lam -> hookLength lam

allStandardTableaux = method(TypicalValue => Bag)
allStandardTableaux (Partition) := lam -> (
    lam = trim lam;
    n := sum toList lam;
    T := youngTableau lam;

    recurse := (indexList,mu) -> (
        if # mu == 0 then (
            entryList := for i from 0 to #indexList - 1 list (n - position(indexList, theIndex -> theIndex == i));
            return youngTableau(lam,entryList);
            );

        cornerList := positionList(youngTableau mu,isCorner);
        
        flatten for thePosition in cornerList list (
            (rowIndex,colIndex) := thePosition;
            muNew := trim new Partition from for i from 0 to #mu-1 list (
                if i == rowIndex then mu#i-1 else mu#i
                );

            recurse(indexList | {toIndex(thePosition,T)}, muNew)
            )
        );

    Bag recurse({},lam)
    )
-*
allStandardTableaux List := shapeList -> (
    cartesianProductBags for theShape in shapeList list allStandardTableaux theShape
    )
*-

randomStandardTableau = method(TypicalValue => YoungTableau)
randomStandardTableau Partition := lam -> (
    lam = trim lam;
    n := sum toList lam;
    T := youngTableau lam;

    mu := lam;
    indexList := {};
    for i from 1 to n do (
        cornerList := positionList(youngTableau mu,isCorner);
        randomIndex := random(#cornerList);
        thePosition := cornerList#randomIndex;

        (rowIndex,colIndex) := thePosition;
        mu = trim new Partition from for i from 0 to #mu-1 list (
            if i == rowIndex then mu#i-1 else mu#i
            );
        
        indexList = indexList | {toIndex(thePosition,T)};
        );

    entryList := for i from 0 to #indexList - 1 list (n - position(indexList, theIndex -> theIndex == i));
    return youngTableau(lam,entryList);
    )

numTabloids = method(TypicalValue => ZZ)
numTabloids (Partition,Partition) := (lam,mu) -> (
    if hasNegativeRow youngTableau(lam,mu) then error "error: expected lam >= mu";
    
    (lam,mu) = standardize (lam,mu);
    n := sum for i from 0 to #lam-1 list lam#i - mu#i;

    nNext := n;
    product for i from 0 to #lam-1 list (
        n = nNext;
        nNext = n - abs(lam#i-mu#i);
        binomial(n,abs(lam#i-mu#i))
        )
    )
numTabloids Partition := lam -> numTabloids(lam,new Partition from {})

allTabloids = method(TypicalValue => Bag)
allTabloids (Partition,Partition) := (lam,mu) -> (
    if hasNegativeRow youngTableau(lam,mu) then error "error: expected lam >= mu";
    
    (lam,mu) = standardize (lam,mu);
    n := sum for i from 0 to #lam-1 list lam#i - mu#i;

    recurse := (currentEntries, remainingEntries, rowIndex) -> (
        if rowIndex == #lam then return tabloid(lam,mu,currentEntries);
        
        flatten for theRow in subsets(remainingEntries, lam#rowIndex - mu#rowIndex) list (
            recurse(currentEntries | theRow, sort toList(set remainingEntries - set theRow), rowIndex + 1)
            )
        );

    Bag recurse({}, toList(1..n), 0)
    )
allTabloids Partition := lam -> allTabloids(lam,new Partition from {})

randomTabloid = method(TypicalValue => Tabloid)
randomTabloid (Partition,Partition) := (lam,mu) -> tabloid(lam,mu,random toList(1..(size youngTableau(lam,mu))))
randomTabloid Partition := lam -> randomTabloid(lam,new Partition from {})

allSubPartitions = method(TypicalValue => Bag)
allSubPartitions (Partition,Partition) := (lam,mu) -> (
    if not isWeaklyDecreasing(lam,mu) or not isNonnegative(lam,mu) then error "expected weakly decreasing, nonnegative partitions";
    
    (lam,mu) = standardize(lam,mu);
    T := youngTableau(lam,mu);

    if hasNegativeRow T then return Bag {};

    ans := {toList mu};

    for rowIndex in rowRange T do (
        tempList := flatten for thePartition in ans list (
             for j from 1 to lam#rowIndex-mu#rowIndex list (
                if rowIndex >= 1 and thePartition#rowIndex + j > thePartition#(rowIndex-1) then continue;
                ((toList(rowIndex:0))|{j}|(toList((#lam-rowIndex-1):0))) + thePartition
                )
            );
        ans = ans | tempList;
        );
    
    
    Bag for theList in ans list trim new Partition from theList
    )
allSubPartitions Partition := lam -> allSubPartitions(lam,new Partition from {})

toPartitionChain = method(TypicalValue => Sequence)
toPartitionChain YoungTableau := T -> (
    (lam,mu) := standardize skewShape T;

    minEntry := min entries T;
    maxEntry := max entries T;

    ans := {mu};
    
    for i from minEntry to maxEntry do (
        lamPrev := ans#-1;
        
        lamNew := new Partition from for rowIndex in rowRange T list (
            lamPrev#rowIndex + number(rowEntries(T,rowIndex), theBox -> theBox == i)
            );

        ans = append(ans, lamNew);
        );

    --apply(ans, trim)
    toSequence ans
    )
