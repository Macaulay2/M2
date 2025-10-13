

SkewTableau = new Type of HashTable

skewTableau = method(TypicalValue => SkewTableau)
skewTableau (Partition,Partition,List) := (lam,mu,theList) -> (
    (lam',mu') := standardize (lam,mu);
    numBoxesNeeded := sum for i from 0 to #lam'-1 list abs(lam'#i-mu'#i);
    
    if (numBoxesNeeded != #theList) then error "partition sizes do not match with the length of the list";
    if any(theList, theElt -> theElt === null) then error "filling must not contain null entries";

    new SkewTableau from {
        "outerShape" => lam,
        "innerShape" => mu,
        values => theList
        }
    )
skewTableau (Sequence,List) := (theShape,theList) -> (
    (lam,mu) := theShape;

    skewTableau(lam,mu,theList)
    )
skewTableau (Partition,List) := (lam,theList) -> (
    skewTableau(lam,new Partition from {},theList)
    )
skewTableau (Partition,Partition) := (lam,mu) -> (
    (lam',mu') := standardize (lam,mu);
    numBoxesNeeded := sum for i from 0 to #lam'-1 list abs(lam'#i-mu'#i);
    
    skewTableau(lam,mu,toList(numBoxesNeeded:""))
    )
skewTableau Partition := lam -> (
    numBoxesNeeded := sum for i from 0 to #lam-1 list abs(lam#i);
    
    skewTableau(lam, new Partition from {}, toList(numBoxesNeeded:""))
    )

skewShape = method(TypicalValue => Sequence)
skewShape SkewTableau := T -> (
    (T#"outerShape", T#"innerShape")
    )

truncate (Partition,Partition) := theInput -> (
    (lam,mu) := theInput#1;

    lamNumTrailingZeros := # for i from 1 to #lam list (if lam#-i == 0 then 1 else break);
    muNumTrailingZeros := # for i from 1 to #mu list (if mu#-i == 0 then 1 else break);

    lamShortened := (toList lam)_(toList(0..(#lam-1-lamNumTrailingZeros)));
    muShortened := (toList mu)_(toList(0..(#mu-1-muNumTrailingZeros)));

    (new Partition from lamShortened, new Partition from muShortened)
    )

pad (Partition,Partition) := (lam,mu) -> (
    maxLength := max(#lam,#mu);

    lamPadded := toList(lam)|toList((maxLength - #lam):0);
    muPadded := toList(mu)|toList((maxLength - #mu):0);
    
    (new Partition from lamPadded, new Partition from muPadded)
    )

standardize = method(TypicalValue => Sequence)
standardize (Partition,Partition) := (lam,mu) -> pad truncate (lam,mu)

alignToZero = method(TypicalValue => SkewTableau)
alignToZero (Partition,Partition) := (lam,mu) -> (
    (lam,mu) = standardize (lam,mu);
    minPart := min(toList mu | toList lam);

    lamAligned := for thePart in lam list thePart - minPart;
    muAligned := for thePart in mu list thePart - minPart;

    (new Partition from lamAligned, new Partition from muAligned)
    )
alignToZero SkewTableau := T -> (
    (lamAligned,muAligned) := alignToZero standardize skewShape T;
    skewTableau(lamAligned, muAligned, entries T)
    )

hasNegativeRow = method(TypicalValue => Boolean)
hasNegativeRow SkewTableau := T -> (
    (lam,mu) := standardize skewShape T;
    
    any(0..(#lam-1), i -> mu#i > lam#i)
    )

-- swap parts when mu#i > lam#i
normalizeNegativeRows = method(TypicalValue => SkewTableau)
normalizeNegativeRows SkewTableau := T -> (
    if not hasNegativeRow T then return T;
    
    (lam,mu) := standardize skewShape T;

    lam' := new Partition from for i from 0 to #lam-1 list max(lam#i,mu#i);
    mu' := new Partition from for i from 0 to #lam-1 list min(lam#i,mu#i);
    
    skewTableau(lam',mu',entries T)
    )

listNegativeRows = method(TypicalValue => List)
listNegativeRows SkewTableau := T -> (
    (lam,mu) := standardize skewShape T;

    for i from 0 to #lam-1 list (mu#i > lam#i)
    )

-- retrieving rows and columns

numcols SkewTableau := T -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;
    max(max toList lam - min toList mu,0)
    )

numrows SkewTableau := T -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;
    #lam
    )

columnRange = method(TypicalValue => Sequence)
columnRange SkewTableau := T -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;
    (min (toList mu | toList lam)..(max (toList lam | toList mu) - 1))
    )

rowRange = method(TypicalValue => Sequence)
rowRange SkewTableau := T -> (0..(numRows T - 1))

rowEntries = method(TypicalValue => List)
rowEntries (ZZ,SkewTableau) := (rowIndex,T) -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;

    if rowIndex < 0 then (
        rowIndex = #lam + rowIndex;
        );
    if rowIndex >= #lam or rowIndex < 0 then error "index out of bounds";

    numBoxesAbove := sum for i from 0 to rowIndex-1 list (lam#i-mu#i);

    for i from numBoxesAbove to numBoxesAbove + lam#rowIndex - mu#rowIndex - 1 list ((entries T)#i)
    )
rowEntries (SkewTableau,ZZ) := (T,rowIndex) -> rowEntries(rowIndex,T)

SkewTableau^ZZ := (T,rowIndex) -> (
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

SkewTableau_Sequence := (T,thePosition)->(
    (rowIndex,colIndex) := thePosition;
    (lam,mu) := standardize skewShape normalizeNegativeRows T;

    if rowIndex < 0 or rowIndex >= numRows T then error("index "|toString(rowIndex)|" out of range");
    if colIndex < mu#rowIndex or colIndex >= lam#rowIndex then error "index "|toString(colIndex)|" out of range";

    T^rowIndex#colIndex
    )

SkewTableau_ZZ := (T,colIndex) -> (
    colInds := columnRange T;
    
    if colIndex < min(colInds#0,0) or colIndex > max(colInds#-1,0) then error "index out of bounds";
    
    for rowIndex from 0 to numRows T - 1 list T^rowIndex#colIndex
    )

columnEntries = method(TypicalValue => List)
columnEntries (ZZ,SkewTableau) := (colIndex,T) -> delete(null,T_colIndex)
columnEntries (SkewTableau,ZZ) := (T,colIndex) -> columnEntries(colIndex,T)

toPosition = method(TypicalValue => Sequence)
toPosition (ZZ,SkewTableau) := (theIndex,T) -> (
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
toPosition (SkewTableau,ZZ) := (T,theIndex) -> toPosition(theIndex,T)

toIndex = method(TypicalValue => ZZ)
toIndex (Sequence,SkewTableau) := (thePosition,T) -> (
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
toIndex (SkewTableau,Sequence) := (T,thePosition) -> toIndex(thePosition,T)

positionList = method(TypicalValue => List)
positionList SkewTableau := T -> (
    for i from 0 to size T - 1 list toPosition(i,T)
    )

-- drawing

isDrawnInner := false

drawInnerShape = method(TypicalValue => Nothing)
drawInnerShape Boolean := theBool -> (isDrawnInner = theBool;)

net SkewTableau := T -> (
    (lam,mu) := standardize skewShape normalizeNegativeRows T;

    if #lam == 0 or (size T == 0 and not isDrawnInner) then return "∅";

    isNegativeRow := listNegativeRows T;

    innerShapeString := if isDrawnInner then "■" else " ";

    (muSmallest, lamLargest) := (min(min toList mu,0), max(max toList lam,0));
    colWidth := if #entries T == 0 then 1 else max for theBox in entries T list #toString(theBox);
    colWidth = max {colWidth + 2,3};
    hasNegativeParts := any(toList(lam)|toList(mu), thePart -> thePart < 0);
    
    boxColumns := for colIndex from muSmallest to lamLargest-1 list (
        currCol := if colIndex >= mu#0 and colIndex < lam#0 then concatenate(colWidth:"─") else concatenate(colWidth:" ");
        for rowIndex from 0 to #lam-1 do (
            isBox := colIndex >= mu#rowIndex and colIndex < lam#rowIndex;
            isBoxBelow := rowIndex < #lam-1 and colIndex >= mu#(rowIndex+1) and colIndex < lam#(rowIndex+1);

            boxString := if isBox then (
                boxPadding := if isNegativeRow#rowIndex then "░" else " ";
                boxEntry := toString((rowEntries(rowIndex,T))#(colIndex-mu#rowIndex));
                if isNegativeRow#rowIndex and #boxEntry == 0 then boxEntry = "░";
                boxPadding|boxEntry|boxPadding
                ) else if (colIndex < 0 and colIndex >= lam#rowIndex) or (colIndex >= 0 and colIndex < mu#rowIndex) then (
                " "|concatenate((colWidth-2):innerShapeString)|" "
                ) else (
                " "
                );

            belowString := if isBox or isBoxBelow then "─" else " ";
            belowString = concatenate(colWidth:belowString);
            
            currCol = currCol||boxString||belowString;
            );
        currCol
        );

    cornerChar := (leftUp,rightUp,leftDown,rightDown) -> (
        cornerHash := new HashTable from {
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
            };

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
                ) else if isBoxRight or isBoxLeft then (
                "│"
                ) else (
                " "
                );

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
youngDiagram (Partition,Partition) := (lam,mu) -> net skewTableau(lam,mu)
youngDiagram Partition := lam -> youngDiagram(lam,new Partition from {0})
youngDiagram SkewTableau := T -> youngDiagram standardize skewShape T

ferrersDiagram = method(TypicalValue => Net)
ferrersDiagram (Partition,Partition) := (lam,mu) -> (
    boxChar := "●";
    negBoxChar := "○";

    T := skewTableau(lam,mu);
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
ferrersDiagram SkewTableau := T -> ferrersDiagram standardize skewShape T

-- getting data

tex SkewTableau := T -> (
    -- \usepackage{atableau}
    (lam,mu) := standardize skewShape normalizeNegativeRows T;
    
    isNegativeRow := listNegativeRows T;
    starString := if hasNegativeRow T then ", star style={fill=red!50}" else "";
    
    ans := "\\SkewTableau[skew border, skew boxes"|starString|"] "|toString(toList mu);
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

entries SkewTableau := T -> T#values

size SkewTableau := T -> # entries T

components SkewTableau := T -> (
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
        if #entryList' == 0 then continue else skewTableau(lam',mu',entryList')
        )
    )

-- making new tableau

applyEntries = method(TypicalValue => SkewTableau)
applyEntries (SkewTableau,Function) := (T,f) -> skewTableau(truncate skewShape T, apply(entries T, f))

applyPositions = method(TypicalValue => SkewTableau)
applyPositions (SkewTableau,Function) := (T,f) -> skewTableau(truncate skewShape T, apply(positionList T, f))

conjugate SkewTableau := T -> (
    if not isWeaklyDecreasing T or not isNonnegative T then error "expected shape to be weakly decreasing and nonnegative";
    (lam,mu) := standardize skewShape T;

    if #lam == 0 then return T;
    
    lam' := conjugate lam;
    mu' := conjugate mu;
    entryList' := flatten for colIndex from 0 to lam#0-1 list columnEntries(colIndex,T);

    skewTableau(lam',mu',entryList')
    )

verticalConcatenate = method(TypicalValue => SkewTableau)
verticalConcatenate List := tabList -> (
    lam := new Partition from flatten for T in tabList list toList((standardize skewShape T)#0);
    mu := new Partition from flatten for T in tabList list toList((standardize skewShape T)#1);
    entryList := flatten for T in tabList list entries T;

    skewTableau(lam, mu, entryList)
    )

SkewTableau || SkewTableau := (T1,T2) -> (
    verticalConcatenate {T1,T2}
    )

SkewTableau ++ SkewTableau := (T1,T2) -> (
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

    skewTableau(lam, mu, entryList)
    )

shift = method(TypicalValue => SkewTableau)
shift (SkewTableau,ZZ) := (T,firstRowAmount) -> (
    (lam,mu) := standardize skewShape T;
    
    lam = for i from 0 to #lam-1 list (lam#i+(i+firstRowAmount));
    mu = for i from 0 to #lam-1 list (mu#i+(i+firstRowAmount));

    skewTableau(new Partition from lam, new Partition from mu, entries T)
    )
shift SkewTableau := T -> (
    shift(T,0)
    )

unshift = method(TypicalValue => SkewTableau)
unshift (SkewTableau,ZZ) := (T,firstRowAmount) -> (
    (lam,mu) := standardize skewShape T;
    
    lam = for i from 0 to #lam-1 list (lam#i-(i+firstRowAmount));
    mu = for i from 0 to #lam-1 list (mu#i-(i+firstRowAmount));

    skewTableau(new Partition from lam, new Partition from mu, entries T)
    )
unshift SkewTableau := T -> (
    unshift(T,0)
    )

-- bools

isWeaklyDecreasing = method(TypicalValue => Boolean)
isWeaklyDecreasing Partition := lam -> rsort toList lam == toList lam
isWeaklyDecreasing (Partition,Partition) := (lam,mu) -> isWeaklyDecreasing lam and isWeaklyDecreasing mu
isWeaklyDecreasing SkewTableau := T -> isWeaklyDecreasing skewShape T

isNonnegative = method(TypicalValue => Boolean)
isNonnegative Partition := lam -> all(toList lam, thePart -> thePart >= 0)
isNonnegative (Partition,Partition) := (lam,mu) -> isNonnegative lam and isNonnegative mu
isNonnegative SkewTableau := T -> isNonnegative standardize skewShape T

isSkew = method(TypicalValue => Boolean)
isSkew SkewTableau := T -> (
    (lam,mu) := truncate skewShape T;

    #mu != 0
    )

-- tableau calculations

boxContent = method(TypicalValue => ZZ)
boxContent (ZZ,ZZ) := (rowIndex,colIndex) -> colIndex - rowIndex

hookLength = method(TypicalValue => ZZ)
hookLength (Sequence,SkewTableau) := (thePosition,T) -> (
    (rowIndex,colIndex) := thePosition;
    (lam,mu) := standardize skewShape T;
    lam' := conjugate lam;

    1 + (lam#rowIndex - colIndex - 1) + (lam'#colIndex-rowIndex - 1)
    )

