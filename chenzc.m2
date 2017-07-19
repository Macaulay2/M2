newPackage(
        "FirstPackage",
        Version => "1.0",
        Date => "February 11, 2004",
        Authors => {{Name => "Jane Doe",
                  Email => "doe@math.uiuc.edu",
                  HomePage => "http://www.math.uiuc.edu/~doe/"}},
        Headline => "an example Macaulay2 package",
        DebuggingMode => true
        )
needsPackage"SimpleDoc"
export {firstFunction}
firstFunction = method(TypicalValue => String)
firstFunction ZZ := String => n -> if n == 1 then "Hello World!" else "D’oh!"
beginDocumentation()
doc ///
    Key
        FirstPackage
    Headline
        an example Macaulay2 package
    Description
        Text
            This package is a basic package to be used as an example.
    Caveat
Still trying to figure this out.
///
doc /// Key
        (firstFunction,ZZ)
    Headline
        a silly first function
    Usage
        f = firstFunction n
    Inputs
n:ZZ Outputs
        f:String
          a silly string, depending on the value of @TT "n"@
    Description
     Text
       Here we show an example.
     Example
       firstFunction 1
       firstFunction 0
firstFunction
2
///
TEST ///
    assert ( firstFunction 2 == "D’oh!" )
/// end
You can write anything you want down here... I like to keep examples
as I’m developing here.  Clean it up before submitting for
publication.
1. You must save a package as file with nothing but the package in it (you can put anything you want, actually, as long is it comes after an end in the file). The name of the file must match the name of the package. So for example, you must save this package as “FirstPackage.m2”
2. The discussion here is just about the overall structure of a package. There are examples of more sophisticated documentation nodes and tests that follow this discussion. Consider using that to help make changes to this document.
3. Debugging code is an important skill. Learning the meaning of error messages and how to use Macaulay 2’s debugging features go a long way with this. With this in mind, try introducing errors to the package. Think like a scientist — introduce them one at a time in a way that you know what the error is, and then see what Macaulay 2 tells you.
(a) Start with the pre-amble.
(b) Try not exporting the function — just export nothing.
(c) Introduce and error into the function. Here we can force with using the command error (check it out in the documentation). This can be useful when developing code. More on this in a moment.
(d) Now introduce errors into the documentation.
(e) Finally change the test so that the test is false and see what happens.
Macaulay 2 has some nice debugging features. We won’t go into them all now, but one key concept is that if an error occurs while loading a package, or while running functions from a package, Macaulay 2 moves into debugging mode which is indicated by a change of the input icon to having two i’s — for example ii4:. This mode allows access to locally defined variables in the function where the error occurred. If that function is called by another, access to the higher level function is accessed through the use of the command break. This allows you to experiment and play around with the internal structure of the function. Sometimes you might want to do this even when Macaulay 2 does not think there is an error. The function error is good for this. In this case you might want Macaulay 2 to continue the computation after you have had a chance to inspect things and the function “continue” does this.
Documentation Internal documentation is easy, just use two dashes, for example the line before the method, and after and then later are all internal comments as they start with two dashes.
-- Cellular decomposition of binomial ideals:
binomialCellularDecomposition = method (Options => {returnCellVars => false, verbose=>true})
binomialCellularDecomposition Ideal := Ideal => o -> I -> (
-- Based on code by Ignacio Ojeda and Mike Stillman
     R := ring I;
     n := numgens R;
     Answer := {};
     L := null;