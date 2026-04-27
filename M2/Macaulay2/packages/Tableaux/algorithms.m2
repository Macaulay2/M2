

maxSSYT = method(TypicalValue => SkewTableau)
maxSSYT (Partition,Partition) := (lam,mu) -> (
    tempT := skewTableau(lam,mu);
    (lam,mu) = standardize skewShape tempT;

    entryList := for entryIndex from 0 to sum toList lam - sum toList mu - 1 list (
        (i,j) := toPosition(entryIndex,tempT);
        theCol := tempT_j;
        theColAbove := theCol_(toList(0..i));
        #(delete(null,theColAbove))
        );

    skewTableau(lam,mu,entryList)
    )

minSSYT = method(TypicalValue => SkewTableau)
minSSYT (Partition,Partition,ZZ) := (lam,mu,maxEntry) -> (
    tempT := skewTableau(lam,mu);
    (lam,mu) = standardize skewShape tempT;
    
    entryList := for entryIndex from 0 to sum toList lam - sum toList mu - 1 list (
        (i,j) := toPosition(entryIndex,tempT);
        theCol := tempT_j;
        theColBelow := theCol_(toList((i+1)..(#theCol-1)));
        maxEntry - #(delete(null,theColBelow))
        );

    skewTableau(lam,mu,entryList)
    )

addOneSSYT = method(TypicalValue => SkewTableau)
addOneSSYT (SkewTableau,Sequence,Partition,Partition) := (T,thePosition,lam,mu) -> (
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

    skewTableau(lam,mu,toList entryList)
    )

allSemistandardTableaux = method(TypicalValue => List)
allSemistandardTableaux (Partition,Partition,ZZ) := (lam,mu,maxEntry) -> (
    (lam,mu) = standardize (lam,mu);
    (lamList,muList) := (toList lam, toList mu);

    if rsort lamList != lamList or rsort muList != muList then error "expected partitions to be weakly decreasing";
    
    if any(0..(#lam-1), i -> mu#i > lam#i) then return Bag {};

    T := skewTableau(lam,mu);

    if #lam == 0 then return Bag {skewTableau(new Partition from {})};
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

numSemistandardTableaux = method(TypicalValue => ZZ)
numSemistandardTableaux (Partition,ZZ) := (lam,n) -> (
    if n < #lam then return 0;
    
    numAppendedZeros := n - #lam;
    lam = new Partition from (toList(lam)|toList(numAppendedZeros:0));

    T := skewTableau(lam);

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
numSemistandardTableaux Partition := lam -> numSemistandardTableaux(truncate lam,# truncate lam)
