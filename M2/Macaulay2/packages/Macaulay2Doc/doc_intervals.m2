doc ///
  Key
    left
  Headline
    left endpoint of an interval
  Usage
    x = left I
  Inputs
    I:RRi
  Outputs
    x:RR
  Description
    Text
      Returns the left endpoint of the input interval.
  SeeAlso
    right
    midpoint
    diameter
///

doc ///
  Key
    right
Headline
    right endpoint of an interval
Usage
    x = right I
Inputs
    I:RRi
Outputs
    x:RR
Description
  Text
    Returns the right endpoint of the input interval.
SeeAlso
    left
    midpoint
    diameter
///

doc ///
Key
    midpoint
Headline
    midpoint of an interval
Usage
    x = midpoint I
Inputs
    I:RRi
Outputs
    x:RR
Description
  Text
    Returns the midpoint (the average of the endpoints) of the input interval.
SeeAlso
    left
    right
    diameter
///

doc ///
Key
    diameter
    (diameter, RRi)
Headline
    diameter of an interval
Usage
    x = diameter I
Inputs
    I:RRi
Outputs
    x:RR
Description
  Text
    Returns the diameter (the difference between the endpoints) of the input interval.
SeeAlso
    left
    right
    midpoint
///

doc ///
Key
    (intersect, RRi)
    (intersect, RRi, RRi)
    [(intersect, RRi), Precision]
    [(intersect, RRi, RRi), Precision]
Headline
    intersection of input intervals
Usage
    J = intersect(I,...)
    J = intersect(I,...,Precision => prec)
Inputs
    I:RRi
    Precision => ZZ
        specifies the desired precision of the output, a value of {\tt -1} uses the minimum precision of the inputs.
Outputs
    J:RRi
Description
  Text
    Returns the intersection of any number of input intervals.
SeeAlso
    intersect
///

doc ///
Key
    (isMember, QQ, RRi)
    (isMember, ZZ, RRi)
    (isMember, RR, RRi)
Headline
    membership test in an interval
Usage
    isMember(x,I)
Inputs
    x:{QQ,ZZ,RR}
    I:RRi
Outputs
    :Boolean
Description
  Text
    Returns true if the input number is in the interval.
SeeAlso
    isEmpty
///

doc ///
Key
    (isEmpty, RRi)
Headline
    empty test for an interval
Usage
    x = isEmpty(I)
Inputs
    I:RRi
Outputs
    x:Boolean
Description
  Text
    Returns true if the input interval is empty, i.e., the left endpoint is to the right of the right endpoint.
SeeAlso
    isMember
///

doc ///
Key
    (isSubset, RRi, RRi)
Headline
    subset test for intervals
Usage
    x = isSubset(I,J)
Inputs
    I:RRi
    J:RRi
Outputs
    x:Boolean
Description
  Text
    Returns true if interval I is a subset of interval J.
SeeAlso
    isMember
///

undocumented{(span,RRi), (span, QQ), (span, RR), (span, ZZ)}

doc ///
Key
    span
Headline
    construct smallest interval
Description
  Text
    Returns the smallest interval containing the inputs (which can include intervals).  Typically, the returned interval is not empty.  See @TO (span, Sequence)@ and @TO (span, List)@
SeeAlso
    interval
    (span, Sequence)
    (span, List)
    toRRi
///

doc ///
Key
    (span, List)
    [(span,List),Precision]
Headline
    construct smallest interval
Usage
    I = span(L)
    I = span(L,Precision => prec)
Inputs
    L:List
        containing @TO Number@ (including @TO RRi@)
    Precision => ZZ
        specifies the desired precision of the output, a value of {\tt -1} uses the minimum precision of the inputs.
Outputs
    I:RRi
Description
  Text
    Returns the smallest interval containing the inputs (which can include intervals).  Typically, the returned interval is not empty.
SeeAlso
    interval
    (span, Sequence)
    toRRi
///

doc ///
Key
    (span, Sequence)
    [(span,Sequence),Precision]
Headline
    construct smallest interval
Usage
    I = span(S)
    I = span(S,Precision => prec)
Inputs
    S:Sequence
        containing @TO Number@ (including @TO RRi@)
    Precision => ZZ
        specifies the desired precision of the output, a value of {\tt -1} uses the minimum precision of the inputs.
Outputs
    I:RRi
Description
  Text
    Returns the smallest interval containing the inputs (which can include intervals).  Typically, the returned interval is not empty.
SeeAlso
    interval
    (span, List)
    toRRi
///

doc ///
Key
    interval
    (interval,Array)
    (interval,QQ)
    (interval,QQ,QQ)
    (interval,QQ,RR)
    (interval,QQ,ZZ)
    (interval,RR)
    (interval,RR,QQ)
    (interval,RR,RR)
    (interval,RR,ZZ)
    (interval,ZZ)
    (interval,ZZ,QQ)
    (interval,ZZ,RR)
    (interval,ZZ,ZZ)
    [interval,Precision]
Headline
    construct an interval
Usage
    I = interval(n)
    I = interval(l,r)
    I = interval([l,r])
    I = interval(n,Precition => prec)
    I = interval(l,r,Precition => prec)
    I = interval([l,r],Precition => prec)
Inputs
    n:Number
    l:Number
    r:Number
    Precision => ZZ
        specifies the desired precision of the output, a value of {\tt -1} uses the @TO "defaultPrecision"@ or the minimum precisions of the inputs.
Outputs
    I:RRi
Description
  Text
    Returns an interval as small as possible containing {\tt n} or from {\tt l} to {\tt r}.  Note that if {\tt l} is to the right of {\tt r}, the constructed interval is empty.
SeeAlso
    (span, List)
    (span, Sequence)
    toRRi
///

doc ///
Key
    toRRi
Headline
    construct an interval
Usage
    I = toRRi(n)
    I = toRRi(l,r)
    I = toRRi(prec,l,r)
Inputs
    n:Number
    l:Number
    r:Number
    prec:ZZ
Outputs
    I:RRi
Description
  Text
    Returns an interval as small as possible containing {\tt n} or from {\tt l} to {\tt r}.  Note that if {\tt l} is to the right of {\tt r}, the constructed interval is empty.  This is a more low-level function and @TO interval@ or span should be used instead.
SeeAlso
    (span, List)
    (span, Sequence)
    interval
///

doc ///
Key
    numericInterval
    (numericInterval, Constant)
    (numericInterval, ZZ, Constant)
Headline
    convert a constant to an interval
Usage
    I = numericInterval(c)
    I = numericInterval(prec,c)
Inputs
    c:Constant
    prec:ZZ
Outputs
    I:RRi
Description
  Text
    Constructs an interval containing {\tt c} of either the @TO "defaultPrecision"@ or precision {\tt prec}
SeeAlso
    (span, List)
    (span, Sequence)
    interval
    toRRi
///
