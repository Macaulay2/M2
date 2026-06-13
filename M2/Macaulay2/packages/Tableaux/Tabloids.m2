
Tabloid = new Type of YoungTableau

tabloid = method(TypicalValue => Tabloid)
tabloid (Partition,Partition,List) := (lam,mu,theList) -> (
    (lam',mu') := standardize (lam,mu);
    numBoxesNeeded := sum for i from 0 to #lam'-1 list abs(lam'#i-mu'#i);

    if sort theList != toList(1..numBoxesNeeded) then error "list must contain distinct entries in 1..n";

    new Tabloid from {
        "outerShape" => lam,
        "innerShape" => mu,
        values => theList
        }
    )
tabloid (Sequence,List) := (theShape,theList) -> (
    (lam,mu) := theShape;

    tabloid(lam,mu,theList)
    )
tabloid (Partition,List) := (lam,theList) -> (
    tabloid(lam,new Partition from {},theList)
    )
tabloid YoungTableau := T -> tabloid(skewShape T, entries T)

youngTableau Tabloid := T -> youngTableau(skewShape T, entries T)

toList Tabloid := T -> (
    T = representative T;
    
    rowPermutations := for rowIndex from 0 to numRows T - 1 list (
        theRow := rowEntries(T,rowIndex);
        
        permList := for thePerm in permutations (#theRow) list permutation for theNum in thePerm list theNum + 1;

        permutedRowList := for thePerm in permList list thePerm * theRow
        );

    entryLists := rowPermutations#0;
    for rowIndex from 1 to numRows T - 1 do entryLists = entryLists ** rowPermutations#rowIndex;
    entryLists = for theSeq in entryLists / deepSplice list join sequence theSeq;

    Bag for theList in entryLists list youngTableau(skewShape T, theList)
    )

representative = method(TypicalValue => Tabloid)
representative Tabloid := T -> youngTableau(skewShape T, flatten for i from 0 to numRows T - 1 list sort rowEntries(T,i))

Tabloid == Tabloid := (T1,T2) -> representative T1 == representative T2
