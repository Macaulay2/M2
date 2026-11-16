
MutableYoungTableau = new Type of YoungTableau

mutableYoungTableau = method(TypicalValue => MutableYoungTableau)
mutableYoungTableau (Partition,Partition,List) := (lam,mu,theList) -> (
    (lam',mu') := standardize (lam,mu);
    numBoxesNeeded := sum for i from 0 to #lam'-1 list abs(lam'#i-mu'#i);
    
    if (numBoxesNeeded != #theList) then error "partition sizes do not match with the length of the list";
    if any(theList, theElt -> theElt === null) then error "filling must not contain null entries";

    new MutableYoungTableau from {
        "outerShape" => lam,
        "innerShape" => mu,
        values => new MutableList from theList
        }
    )
mutableYoungTableau (Sequence,List) := (theShape,theList) -> (
    (lam,mu) := theShape;

    mutableYoungTableau(lam,mu,theList)
    )
mutableYoungTableau (Partition,List) := (lam,theList) -> (
    mutableYoungTableau(lam,new Partition from {},theList)
    )
mutableYoungTableau (Partition,Partition) := (lam,mu) -> (
    (lam',mu') := standardize (lam,mu);
    numBoxesNeeded := sum for i from 0 to #lam'-1 list abs(lam'#i-mu'#i);
    
    mutableYoungTableau(lam,mu,toList(numBoxesNeeded:""))
    )
mutableYoungTableau Partition := lam -> (
    numBoxesNeeded := sum for i from 0 to #lam-1 list abs(lam#i);
    
    mutableYoungTableau(lam, new Partition from {}, toList(numBoxesNeeded:""))
    )

MutableYoungTableau_Sequence = (T,thePosition,newBox) -> (
    (entries T)#(toIndex(T,thePosition)) = newBox;

    newBox
    )
