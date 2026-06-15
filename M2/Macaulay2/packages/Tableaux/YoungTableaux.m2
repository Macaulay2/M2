

YoungTableau = new Type of HashTable

youngTableau = method(TypicalValue => YoungTableau)
youngTableau (Partition,Partition,List) := (lam,mu,theList) -> (
    (lam',mu') := standardize (lam,mu);
    numBoxesNeeded := sum for i from 0 to #lam'-1 list abs(lam'#i-mu'#i);
    
    if (numBoxesNeeded != #theList) then error "partition sizes do not match with the length of the list";
    if any(theList, theElt -> theElt === null) then error "filling must not contain null entries";

    new YoungTableau from {
        "outerShape" => lam,
        "innerShape" => mu,
        values => theList
        }
    )
youngTableau (Sequence,List) := (theShape,theList) -> (
    (lam,mu) := theShape;

    youngTableau(lam,mu,theList)
    )
youngTableau (Partition,List) := (lam,theList) -> (
    youngTableau(lam,new Partition from {},theList)
    )
youngTableau (Partition,Partition) := (lam,mu) -> (
    (lam',mu') := standardize (lam,mu);
    numBoxesNeeded := sum for i from 0 to #lam'-1 list abs(lam'#i-mu'#i);
    
    youngTableau(lam,mu,toList(numBoxesNeeded:""))
    )
youngTableau Partition := lam -> (
    numBoxesNeeded := sum for i from 0 to #lam-1 list abs(lam#i);
    
    youngTableau(lam, new Partition from {}, toList(numBoxesNeeded:""))
    )

-- computing tableau shapes

skewShape = method(TypicalValue => Sequence)
skewShape YoungTableau := T -> (
    (T#"outerShape", T#"innerShape")
    )

outerShape = method(TypicalValue => Partition)
outerShape YoungTableau := T -> (
    T#"outerShape"
    )

innerShape = method(TypicalValue => Partition)
innerShape YoungTableau := T -> (
    T#"innerShape"
    )

shape = method(TypicalValue => Partition)
shape YoungTableau := T -> (
    if isSkew T then error "expected a non-skew tableau";

    outerShape T
    )

trim Partition := Partition => o -> lam -> (
    numTrailingZeros := # for i from 1 to #lam list (if lam#-i == 0 then 1 else break);

    lamShortened := (toList lam)_(toList(0..(#lam-1-numTrailingZeros)));

    new Partition from lamShortened
    )
trim (Partition,Partition) := (Partition,Partition) => o -> (lam,mu) -> (trim lam, trim mu)

-*
truncate Partition := theInput -> (
    lam := theInput#1;
    numTrailingZeros := # for i from 1 to #lam list (if lam#-i == 0 then 1 else break);

    lamShortened := (toList lam)_(toList(0..(#lam-1-numTrailingZeros)));

    new Partition from lamShortened
    )

truncate (Partition,Partition) := theInput -> (
    (lam,mu) := theInput#1;

    (truncate lam, truncate mu)
    )
*-

pad (Partition,Partition) := (lam,mu) -> (
    maxLength := max(#lam,#mu);

    lamPadded := toList(lam)|toList((maxLength - #lam):0);
    muPadded := toList(mu)|toList((maxLength - #mu):0);
    
    (new Partition from lamPadded, new Partition from muPadded)
    )

standardize = method(TypicalValue => Sequence)
standardize (Partition,Partition) := (lam,mu) -> (
    (lam,mu) = pad trim (lam,mu);
    if #lam == 0 then return (new Partition from {0}, new Partition from {0});

    (lam,mu)
    )

alignToZero = method(TypicalValue => YoungTableau)
alignToZero (Partition,Partition) := (lam,mu) -> (
    (lam,mu) = standardize (lam,mu);
    minPart := min(toList mu | toList lam);

    lamAligned := for thePart in lam list thePart - minPart;
    muAligned := for thePart in mu list thePart - minPart;

    (new Partition from lamAligned, new Partition from muAligned)
    )
alignToZero YoungTableau := T -> (
    (lamAligned,muAligned) := alignToZero standardize skewShape T;
    youngTableau(lamAligned, muAligned, entries T)
    )

hasNegativeRow = method(TypicalValue => Boolean)
hasNegativeRow YoungTableau := T -> (
    (lam,mu) := standardize skewShape T;
    
    any(0..(#lam-1), i -> mu#i > lam#i)
    )

-- swap parts when mu#i > lam#i
normalizeNegativeRows = method(TypicalValue => YoungTableau)
normalizeNegativeRows YoungTableau := T -> (
    if not hasNegativeRow T then return T;
    
    (lam,mu) := standardize skewShape T;

    lam' := new Partition from for i from 0 to #lam-1 list max(lam#i,mu#i);
    mu' := new Partition from for i from 0 to #lam-1 list min(lam#i,mu#i);
    
    youngTableau(lam',mu',entries T)
    )

listNegativeRows = method(TypicalValue => List)
listNegativeRows YoungTableau := T -> (
    (lam,mu) := standardize skewShape T;

    for i from 0 to #lam-1 list (mu#i > lam#i)
    )

-- retrieving rows and columns

numcols YoungTableau := T -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;
    max(max toList lam - min toList mu,0)
    )

numrows YoungTableau := T -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;
    #lam
    )

columnRange = method(TypicalValue => Sequence)
columnRange YoungTableau := T -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;
    (min (toList mu | toList lam)..(max (toList lam | toList mu) - 1))
    )

rowRange = method(TypicalValue => Sequence)
rowRange YoungTableau := T -> (0..(numRows T - 1))

rowEntries = method(TypicalValue => List)
rowEntries (ZZ,YoungTableau) := (rowIndex,T) -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;

    if rowIndex < 0 then (
        rowIndex = #lam + rowIndex;
        );
    if rowIndex >= #lam or rowIndex < 0 then error "index out of bounds";

    numBoxesAbove := sum for i from 0 to rowIndex-1 list (lam#i-mu#i);

    for i from numBoxesAbove to numBoxesAbove + lam#rowIndex - mu#rowIndex - 1 list ((entries T)#i)
    )
rowEntries (YoungTableau,ZZ) := (T,rowIndex) -> rowEntries(rowIndex,T)

YoungTableau^ZZ := (T,rowIndex) -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;

    if rowIndex < 0 then (
        rowIndex = #lam + rowIndex;
        );
    if rowIndex >= #lam or rowIndex < 0 then error "index out of bounds";

    colInds := columnRange T;

    theEntries := rowEntries(rowIndex,T);

    theRowOrderedLeft := for colIndex from min(colInds#0,0) to -1 list (
        if colIndex < mu#rowIndex or colIndex >= lam#rowIndex then null else (
            theEntries#(colIndex - mu#rowIndex)
            )
        );

    theRowOrderedRight := for colIndex from 0 to max(colInds#-1,0) list (
        if colIndex < mu#rowIndex or colIndex >= lam#rowIndex then null else (
            theEntries#(colIndex - mu#rowIndex)
            )
        );

    theRowOrderedRight|theRowOrderedLeft
    )

YoungTableau_Sequence := (T,thePosition)->(
    (rowIndex,colIndex) := thePosition;
    (lam,mu) := standardize skewShape normalizeNegativeRows T;

    if rowIndex < 0 or rowIndex >= numRows T then error("index "|toString(rowIndex)|" out of range");
    if colIndex < mu#rowIndex or colIndex >= lam#rowIndex then error "index "|toString(colIndex)|" out of range";

    T^rowIndex#colIndex
    )

YoungTableau_ZZ := (T,colIndex) -> (
    colInds := columnRange T;
    
    if colIndex < min(colInds#0,0) or colIndex > max(colInds#-1,0) then error "index out of bounds";
    
    for rowIndex from 0 to numRows T - 1 list T^rowIndex#colIndex
    )

columnEntries = method(TypicalValue => List)
columnEntries (ZZ,YoungTableau) := (colIndex,T) -> delete(null,T_colIndex)
columnEntries (YoungTableau,ZZ) := (T,colIndex) -> columnEntries(colIndex,T)

toPosition = method(TypicalValue => Sequence)
toPosition (ZZ,YoungTableau) := (theIndex,T) -> (
    if theIndex < 0 then (
        theIndex = size T + theIndex;
        );
    if theIndex < 0 or theIndex >= size T then error "index out of range";

    (lam,mu) := standardize skewShape normalizeNegativeRows T;
    
    numBoxesSeen := 0;
    for rowIndex from 0 to numRows T - 1 do (
        for colIndex from mu#rowIndex to lam#rowIndex - 1 do (
            if numBoxesSeen == theIndex then return (rowIndex,colIndex);
            numBoxesSeen = numBoxesSeen + 1;
            );
        );
    )
toPosition (YoungTableau,ZZ) := (T,theIndex) -> toPosition(theIndex,T)

toIndex = method(TypicalValue => ZZ)
toIndex (Sequence,YoungTableau) := (thePosition,T) -> (
    (rowIndex,colIndex) := thePosition;
    (lam,mu) := standardize skewShape normalizeNegativeRows T;

    if rowIndex < 0 or rowIndex >= #lam or colIndex < mu#rowIndex or colIndex >= lam#rowIndex then error "index out of range";

    numBoxesSeen := 0;
    for theRowIndex from 0 to numRows T - 1 do (
        for theColIndex from mu#theRowIndex to lam#theRowIndex - 1 do (
            if (theRowIndex,theColIndex) == (rowIndex,colIndex) then return numBoxesSeen;
            numBoxesSeen = numBoxesSeen + 1;
            );
        );
    )
toIndex (YoungTableau,Sequence) := (T,thePosition) -> toIndex(thePosition,T)

positionList = method(TypicalValue => List)
positionList YoungTableau := T -> (
    for i from 0 to size T - 1 list toPosition(i,T)
    )
positionList (YoungTableau,Function) := (T,f) -> (
    (lam,mu) := standardize skewShape T;
    
    if f === isCorner then (
        if isSkew T then error "Expected non-skew shape";
        return((for rowIndex from 0 to numRows T - 2 list (
            if lam#rowIndex > lam#(rowIndex + 1) then (rowIndex,lam#rowIndex - 1) else continue
            ))|{(numRows T - 1,lam#-1 - 1)});
        );

    select(positionList T,f)
    )

rowList = method(TypicalValue => List)
rowList YoungTableau := T -> for i from 0 to numRows T - 1 list rowEntries(T,i)

columnList = method(TypicalValue => List)
columnList YoungTableau := T -> for i from 0 to numColumns T - 1 list columnEntries(T,i)

-- drawing

isDrawnInner := false

drawInnerShape = method(TypicalValue => Boolean)
drawInnerShape Boolean := theBool -> (isDrawnInner = theBool; theBool)

net YoungTableau := T -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;

    if #lam == 0 or (size T == 0 and not isDrawnInner) then return "∅";

    isNegativeRow := listNegativeRows T;

    innerShapeString := if isDrawnInner then "■" else " ";

    (muSmallest, lamLargest) := (min(min toList mu,0), max(max toList lam,0));
    --colWidth := if #entries T == 0 then 1 else max for theBox in entries T list #toString(theBox);
    colWidth := if #entries T == 0 then 1 else max for theBox in entries T list width net theBox;
    colWidth = max {colWidth + 2,3};
    rowHeight := if #entries T == 0 then 1 else max for theBox in entries T list (depth net theBox + height net theBox);
    rowHeight = max {rowHeight,1};
    hasNegativeParts := any(toList(lam)|toList(mu), thePart -> thePart < 0);
    
    boxColumns := for colIndex from muSmallest to lamLargest-1 list (
        currCol := if colIndex >= mu#0 and colIndex < lam#0 then concatenate(colWidth:"─") else concatenate(colWidth:" ");
        for rowIndex from 0 to #lam-1 do (
            isBox := colIndex >= mu#rowIndex and colIndex < lam#rowIndex;
            isBoxBelow := rowIndex < #lam-1 and colIndex >= mu#(rowIndex+1) and colIndex < lam#(rowIndex+1);

            boxString := if isBox then (
                boxEntry := net((rowEntries(rowIndex,T))#(colIndex-mu#rowIndex));
                
                boxPadding := if isNegativeRow#rowIndex then "░" else " "; --w
                boxPadding = (verticalNet for i from 1 to rowHeight list boxPadding)^(height boxEntry - 1);
                
                if isNegativeRow#rowIndex and #boxEntry == 0 then boxEntry = "░";
                boxPadding|boxEntry|boxPadding
                
                ) else if (colIndex < 0 and colIndex >= lam#rowIndex) or (colIndex >= 0 and colIndex < mu#rowIndex) then (
                aString := " "|concatenate((colWidth-2):innerShapeString)|" ";
                verticalNet for i from 1 to rowHeight list aString
                
                ) else (
                --verticalNet for i from 1 to rowHeight list(" ")
                " "
                );

            belowString := if isBox or isBoxBelow then "─" else " ";
            belowString = concatenate(colWidth:belowString);
            
            currCol = currCol||boxString||belowString;
            );
        currCol
        );

    cornerChar := (leftUp,rightUp,leftDown,rightDown) -> (
        cornerHash := if instance(T, Tabloid) then (
            new HashTable from {
                "0000" => " ",
                "0001" => "┌",
                "0010" => "┐",
                "0011" => "─",
                "0100" => "└",
                "0101" => "├",
                "0110" => "┼",
                "0111" => "┴",
                "1000" => "┘",
                "1001" => "┼",
                "1010" => "┤",
                "1011" => "┴",
                "1100" => "─",
                "1101" => "┬",
                "1110" => "┬",
                "1111" => "─"
                }
            ) else (
            new HashTable from {
                "0000" => " ",
                "0001" => "┌",
                "0010" => "┐",
                "0011" => "┬",
                "0100" => "└",
                "0101" => "├",
                "0110" => "┼",
                "0111" => "┼",
                "1000" => "┘",
                "1001" => "┼",
                "1010" => "┤",
                "1011" => "┼",
                "1100" => "┴",
                "1101" => "┼",
                "1110" => "┼",
                "1111" => "┼"
                }
            );

        theKey := concatenate({leftUp,rightUp,leftDown,rightDown} / (i -> if i then toString(1) else toString(0)));
        
        cornerHash#theKey
        );

    sepColumns := for colIndex from muSmallest to lamLargest list (
        isBoxLeftUp := false;
        isBoxRightUp := false;
        isBoxRightDown := colIndex >= mu#0 and colIndex < lam#0;
        isBoxLeftDown := colIndex-1 >= mu#0 and colIndex-1 < lam#0;
        
        currColNet := if colIndex == 0 and hasNegativeParts then (
                "║"
                ) else (
                cornerChar(isBoxLeftUp,isBoxRightUp,isBoxLeftDown,isBoxRightDown)
                );
        
        for rowIndex from 0 to #lam-1 do (

            isBoxLeft := colIndex-1 >= mu#rowIndex and colIndex-1 < lam#rowIndex;
            isBoxRight := colIndex >= mu#rowIndex and colIndex < lam#rowIndex;

            isBoxLeftUp = isBoxLeft;
            isBoxRightUp = isBoxRight;
            isBoxRightDown = rowIndex < #lam-1 and colIndex >= mu#(rowIndex+1) and colIndex < lam#(rowIndex+1);
            isBoxLeftDown = rowIndex < #lam-1 and colIndex-1 >= mu#(rowIndex+1) and colIndex-1 < lam#(rowIndex+1);

            boxString := if colIndex == 0 and hasNegativeParts then (
                "║"
                ) else if (not instance(T, Tabloid) and (isBoxRight or isBoxLeft)) or
                              (instance(T, Tabloid) and (isBoxRight xor isBoxLeft)) then (
                "│"
                ) else (
                " "
                );
            boxString = verticalNet for i from 1 to rowHeight list boxString;

            belowString := if colIndex == 0 and hasNegativeParts then (
                "║"
                ) else (
                cornerChar(isBoxLeftUp,isBoxRightUp,isBoxLeftDown,isBoxRightDown)
                );
            
            currColNet = currColNet||boxString||belowString;
            );
        
        currColNet
        );
    
    ans := "";
    for theNet in mingle {sepColumns,boxColumns} do (
        ans = ans|theNet;
        );
    ans^1
    )

youngDiagram = method(TypicalValue => Net)
youngDiagram (Partition,Partition) := (lam,mu) -> net youngTableau(lam,mu)
youngDiagram Partition := lam -> youngDiagram(lam,new Partition from {0})
youngDiagram YoungTableau := T -> youngDiagram standardize skewShape T

ferrersDiagram = method(TypicalValue => Net)
ferrersDiagram (Partition,Partition) := (lam,mu) -> (
    boxChar := "●";
    negBoxChar := "○";

    T := youngTableau(lam,mu);
    (lam,mu) = standardize skewShape normalizeNegativeRows T;

    if #lam == 0 or (size T == 0 and not isDrawnInner) then return "∅";

    isNegativeRow := listNegativeRows T;

    ans := concatenate for colIndex in columnRange T list (
        isBox := colIndex >= mu#0 and colIndex < lam#0;

        if isBox and isNegativeRow#0 then (
            negBoxChar|" "
            ) else if isBox then (
            boxChar|" "
            ) else (
            "  "
            )
        );
    for rowIndex from 1 to #lam-1 do (
        rowString := concatenate for colIndex in columnRange T list (
            isBox := colIndex >= mu#rowIndex and colIndex < lam#rowIndex;

            if isBox and isNegativeRow#rowIndex then (
                negBoxChar|" "
                ) else if isBox then (
                boxChar|" "
                ) else (
                "  "
                )
            );
        ans = ans || rowString;
        );

    ans
    )
ferrersDiagram Partition := lam -> ferrersDiagram(lam,new Partition from {0})
ferrersDiagram YoungTableau := T -> ferrersDiagram standardize skewShape T

verticalNet = theTuple -> (
    ans := net theTuple#0;
    for i from 1 to #theTuple-1 do (
        ans = ans || net theTuple#i;
        );

    ans
    )

horizontalNet = theTuple -> (
    ans := net theTuple#0;
    for i from 1 to #theTuple-1 do (
        ans = ans | net theTuple#i;
        );

    ans
    )

-- getting data

tex YoungTableau := T -> (
    -- \usepackage{atableau}
    (lam,mu) := standardize skewShape normalizeNegativeRows T;
    
    isNegativeRow := listNegativeRows T;
    starString := if hasNegativeRow T then ", star style={fill=red!50}" else "";

    ans := if instance(T, Tabloid) then (
        "\\Tabloid[skew="|toString(toList mu)|starString|"]"
        ) else (
        "\\SkewTableau[skew border, skew boxes"|starString|"]"|toString(toList mu)
        );
    filling := for i from 0 to #lam-1 list (
        currRow := rowEntries(i,T);
        isRed := isNegativeRow#i;
        
        concatenate for theBox in currRow list (
            boxString := toString theBox;
            boxString = if #boxString == 1 then boxString else "{"|boxString|"}";
            if isRed then "*"|boxString else boxString
            )
        );
    ans|toString(filling)
    )

entries YoungTableau := T -> T#values

size YoungTableau := T -> # entries T

components YoungTableau := T -> (
    (lamOriginal,muOriginal) := standardize skewShape T;
    (lamOriginal,muOriginal) = (toList lamOriginal,toList muOriginal);
    
    (lam,mu) := standardize skewShape normalizeNegativeRows T;
    (lam,mu) = (toList lam,toList mu);

    isNegativeRow := listNegativeRows T;

    isConnectedToNextRow := for i from 0 to numrows T - 2 list (
        if (mu#i <= mu#(i+1) and mu#(i+1) < lam#i and lam#(i+1) > mu#(i+1)) or (mu#(i+1) <= mu#i and mu#i < lam#(i+1) and lam#i > mu#i) then (
            true
            ) else (
            false
            )
        );
    isConnectedToNextRow = append(isConnectedToNextRow,false);

    currComponentStart := 0;
    compRows := for i from 0 to numrows T - 1 list (
        if isConnectedToNextRow#i then continue else (
            compStart := currComponentStart;
            currComponentStart = i + 1;
            (compStart..i)
            )
        );

    for partIndices in compRows list (
        lam' := new Partition from lamOriginal_(toList partIndices);
        mu' := new Partition from muOriginal_(toList partIndices);
        (lam',mu') = alignToZero(lam',mu');
        entryList' := flatten for theIndex in partIndices list rowEntries(theIndex,T);
        if #entryList' == 0 then continue else youngTableau(lam',mu',entryList')
        )
    )

-- making new tableau

applyEntries = method(TypicalValue => YoungTableau)
applyEntries (YoungTableau,Function) := (T,f) -> (
    if not instance(T,Tabloid) then (
        youngTableau(trim skewShape T, apply(entries T, f))
        ) else (
        tabloid(trim skewShape T, apply(entries T, f))
        )
    )

applyPositions = method(TypicalValue => YoungTableau)
applyPositions (YoungTableau,Function) := (T,f) -> (
    if not instance(T,Tabloid) then (
        youngTableau(trim skewShape T, apply(positionList T, f))
        ) else (
        tabloid(trim skewShape T, apply(positionList T, f))
        )
    )

conjugate YoungTableau := T -> (
    if not isWeaklyDecreasing T or not isNonnegative T then error "expected shape to be weakly decreasing and nonnegative";
    (lam,mu) := standardize skewShape T;

    if #lam == 0 then return T;
    
    lam' := conjugate lam;
    mu' := conjugate mu;
    entryList' := flatten for colIndex from 0 to lam#0-1 list columnEntries(colIndex,T);

    youngTableau(lam',mu',entryList')
    )

verticalConcatenate = method(TypicalValue => YoungTableau)
verticalConcatenate List := tabList -> (
    lam := new Partition from flatten for T in tabList list toList((standardize skewShape T)#0);
    mu := new Partition from flatten for T in tabList list toList((standardize skewShape T)#1);
    entryList := flatten for T in tabList list toList entries T;

    youngTableau(lam, mu, entryList)
    )

YoungTableau || YoungTableau := (T1,T2) -> (
    verticalConcatenate {T1,T2}
    )

YoungTableau ++ YoungTableau := (T1,T2) -> (
    (lam1,mu1) := standardize skewShape T1;
    (lam2,mu2) := standardize skewShape T2;
    (entryList1, entryList2) := (entries T1, entries T2);

    lastMu1 := mu1#-1;
    firstLam2 := lam2#0;
    shiftAmount := firstLam2 - lastMu1;

    lam1 = for thePart in lam1 list (thePart + shiftAmount);
    mu1 = for thePart in mu1 list (thePart + shiftAmount);

    lam := new Partition from ((toList lam1)|(toList lam2));
    mu := new Partition from ((toList mu1)|(toList mu2));
    entryList := entryList1|entryList2;

    youngTableau(lam, mu, entryList)
    )

shift = method(TypicalValue => YoungTableau)
shift (YoungTableau,ZZ) := (T,firstRowAmount) -> (
    (lam,mu) := standardize skewShape T;
    
    lam = for i from 0 to #lam-1 list (lam#i+(i+firstRowAmount));
    mu = for i from 0 to #lam-1 list (mu#i+(i+firstRowAmount));

    youngTableau(new Partition from lam, new Partition from mu, entries T)
    )
shift YoungTableau := T -> (
    shift(T,0)
    )

unshift = method(TypicalValue => YoungTableau)
unshift (YoungTableau,ZZ) := (T,firstRowAmount) -> (
    (lam,mu) := standardize skewShape T;
    
    lam = for i from 0 to #lam-1 list (lam#i-(i+firstRowAmount));
    mu = for i from 0 to #lam-1 list (mu#i-(i+firstRowAmount));

    youngTableau(new Partition from lam, new Partition from mu, entries T)
    )
unshift YoungTableau := T -> (
    unshift(T,0)
    )

-- bools

isWeaklyDecreasing = method(TypicalValue => Boolean)
isWeaklyDecreasing Partition := lam -> rsort toList lam == toList lam
isWeaklyDecreasing (Partition,Partition) := (lam,mu) -> isWeaklyDecreasing lam and isWeaklyDecreasing mu
isWeaklyDecreasing YoungTableau := T -> isWeaklyDecreasing skewShape T

isNonnegative = method(TypicalValue => Boolean)
isNonnegative Partition := lam -> all(toList lam, thePart -> thePart >= 0)
isNonnegative (Partition,Partition) := (lam,mu) -> isNonnegative lam and isNonnegative mu
isNonnegative YoungTableau := T -> isNonnegative standardize skewShape T

isSkew = method(TypicalValue => Boolean)
isSkew YoungTableau := T -> (
    (lam,mu) := trim skewShape T;

    #mu != 0
    )

isNatural = method(TypicalValue => Boolean)
isNatural YoungTableau := T -> sort entries T == toList(1..(size T))

isColumnStrict = method(TypicalValue => Boolean)
isColumnStrict YoungTableau := T -> (
    isWeakRows := all(0..(numRows T - 1), i -> isSorted rowEntries(T,i));
    isWeakCols := all(0..(numColumns T - 1), j -> isSorted columnEntries(T,j));
    isUniqueCols := all(0..(numColumns T - 1), j -> max values tally columnEntries(T,j) == 1);

    isWeakRows and isWeakCols and isUniqueCols
    )

isRowStrict = method(TypicalValue => Boolean)
isRowStrict YoungTableau := T -> (
    isWeakCols := all(0..(numColumns T - 1), j -> isSorted columnEntries(T,j));
    isWeakRows := all(0..(numRows T - 1), i -> isSorted rowEntries(T,i));
    isUniqueRows := all(0..(numRows T - 1), i -> max values tally rowEntries(T,i) == 1);

    isWeakRows and isWeakCols and isUniqueRows
    )

isSemistandard = method(TypicalValue => Boolean)
isSemistandard YoungTableau := T -> (
    isColumnStrict T and isNonnegative T and isWeaklyDecreasing T
    )

isStandard = method(TypicalValue => Boolean)
isStandard YoungTableau := T -> (
    isNatural T and isSemistandard T
    )

isCorner = method(TypicalValue => Boolean)
isCorner (Sequence,Partition) := (thePosition,lam) -> (
    (rowIndex,colIndex) := thePosition;
    lam = trim lam;

    if not isWeaklyDecreasing lam then error "expected weakly decreasing shape";

    if rowIndex == #lam-1 then return(colIndex == lam#rowIndex - 1);

    colIndex == lam#rowIndex - 1 and lam#rowIndex > lam#(rowIndex + 1)
    )
isCorner (Sequence,YoungTableau) := (thePosition,T) -> (
    (rowIndex,colIndex) := thePosition;
    (lam,mu) := standardize skewShape normalizeNegativeRows T;

    isNonemptyRow := lam#rowIndex > mu#rowIndex;

    isNonemptyRow and isCorner(thePosition,lam)
    )

-- tableau calculations

boxContent = method(TypicalValue => ZZ)
boxContent (ZZ,ZZ) := (rowIndex,colIndex) -> colIndex - rowIndex

hookLength = method(TypicalValue => ZZ)
hookLength (Sequence,YoungTableau) := (thePosition,T) -> (
    (rowIndex,colIndex) := thePosition;
    (lam,mu) := standardize skewShape T;
    lam' := conjugate lam;

    1 + (lam#rowIndex - colIndex - 1) + (lam'#colIndex-rowIndex - 1)
    )
hookLength Partition := lam -> (
    if not isWeaklyDecreasing lam or not isNonnegative lam then error "expected shape to be weakly decreasing and nonnegative";

    T := youngTableau lam;
    
    (size T)! // product apply(positionList T,thePosition -> hookLength(thePosition,T))
    )

rowStabilizer = method(TypicalValue => List)
rowStabilizer YoungTableau := T -> (
    if not isNatural T then error "expected tableau entries in 1..n";
    if not isWeaklyDecreasing T or not isNonnegative T then error "expected shape to be weakly decreasing and nonnegative";

    n := size T;

    permutedRows := for theRow in rowList T list permutations theRow;

    extendedPerms := for permList in permutedRows list (
        theRow := permList#0;
        
        for thePerm in permList list (
            extendedPerm := new MutableList from toList(1..n);
            
            for i from 0 to #thePerm-1 do (
                extendedPerm#(theRow#i-1) = thePerm#i;
                );
            
            permutation toList extendedPerm
            )
        );

    ans := extendedPerms#0;
    for i from 1 to #extendedPerms-1 do ans = (ans ** extendedPerms#i) / splice;
    for theSeq in ans list extend(product toList theSeq,n)
    )

columnStabilizer = method(TypicalValue => List)
columnStabilizer YoungTableau := T -> rowStabilizer conjugate T

readingWord = method(TypicalValue => List)
readingWord YoungTableau := T -> (
    flatten for colIndex in columnRange T list reverse columnEntries(T,colIndex)
    )

-- operators

YoungTableau == YoungTableau := (T1,T2) -> (
    (lam1,mu1) := standardize skewShape T1;
    (lam2,mu2) := standardize skewShape T2;

    toList lam1 == toList lam2 and toList mu1 == toList mu2 and entries T1 == entries T2
    )

Permutation * YoungTableau := (p,T) -> applyEntries(T, theBox -> (if theBox > #p then theBox else p#(theBox-1)))
