

YoungTableau = new Type of SkewTableau

youngTableau = method(TypicalValue => YoungTableau)
youngTableau (Partition,List) := (lam,theList) -> (
    numBoxesNeeded := sum for i from 0 to #lam-1 list abs(lam#i);
    
    if (numBoxesNeeded != #theList) then error "partition sizes do not match with the length of the list";
    if any(theList, theElt -> theElt === null) then error "filling must not contain null entries";

    mu := new Partition from {};

    new YoungTableau from {
        "outerShape" => lam,
        "innerShape" => mu,
        values => theList
        }
    )
youngTableau Partition := lam -> (
    numBoxesNeeded := sum for i from 0 to #lam-1 list abs(lam#i);
    
    youngTableau(lam, toList(numBoxesNeeded:""))
    )

skewTableau YoungTableau := T -> new SkewTableau from T

shape = method(TypicalValue => Partition)
shape YoungTableau := T -> (
    T#"outerShape"
    )

truncate Partition := theInput -> (
    lam := theInput#1;
    numTrailingZeros := # for i from 1 to #lam list (if lam#-i == 0 then 1 else break);

    lamShortened := (toList lam)_(toList(0..(#lam-1-numTrailingZeros)));

    new Partition from lamShortened
    )
