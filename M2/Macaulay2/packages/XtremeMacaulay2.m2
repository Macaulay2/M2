newPackage("XtremeMacaulay2",
    Headline => "a package for doing x-treme computations in Macaulay2!",
    Version => "0.1",
    Date => "May 19, 2026",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu (maybe?)",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance/"}},
    Keywords => {"Algebraic Geometry")

export {
    "xtremeList"
    }

xtremeList = method()
xtremeList ZZ := n -> (
    ret := {};
    for i from 0 to n - 1 do ret = append(ret, i);
    ret)

beginDocumentation()

doc ///
  Key
    XtremeMacaulay2
  Headline
    a package for doing x-treme computations in Macaulay2!
  Description
    Text
      Let's do some x-treme computations in Macaulay2!
  Subnodes
    xtremeList
///

doc ///
  Key
    xtremeList
    (xtremeList, ZZ)
  Headline
    construct an x-treme list
  Usage
    xtremeList n
  Inputs
    n:ZZ
  Outputs
    :List
  Description
    Text
      Given an integer $n$, we construct a list of $n$ elements using an
      "x-treme" algorithm.
    Example
      xtremeList 5
      xtremeList 50000;
///

TEST ///
assert Equation(#xtremeList 50000, 50000)
///
