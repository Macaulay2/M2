newPackage(
        "LiskeFirstPackage",
        Version => "1.0",
        Date => "February 11, 2004",
        Authors => {{Name => "Whitney Liske",
                  Email => "liske.2@nd.edu",
                  HomePage => "http://www.nd.edu/~wradil"}},
        Headline => "an example Macaulay2 package",
        DebuggingMode => true
        )
	
needsPackage"SimpleDoc"

export {"firstFunction"}

firstFunction = method(TypicalValue => String)
firstFunction ZZ := String => n -> if n == 1 then "Hello World!" else "D’oh!"


beginDocumentation()
doc ///
    Key
        LiskeFirstPackage
    Headline
        an example Macaulay2 package
    Description
        Text
            This package is a basic package to be used as an example.
    Caveat
	Still trying to figure this out.
///

doc ///
    Key
        (firstFunction,ZZ)
    Headline
        a silly first function
    Usage
        f = firstFunction n
    Inputs
	n:ZZ
    Outputs
        f:String
          a silly string, depending on the value of @TT "n"@
    Description
     Text
       Here we show an example.
     Example
       firstFunction 1
       firstFunction 0
///
TEST ///
    assert ( firstFunction 2 == "D’oh!" )
///

end
