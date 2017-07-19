newPackage(
        "ZhangchiFirstPackage",
        Version => "1.0",
        Date => "July  19, 2017",
        Authors => {{Name => "Zhangchi Chen",
                  Email => "zhangchi.chen@u-psud.fr",
                  HomePage => "empty"}},
        Headline => "an example Macaulay2 package",
        DebuggingMode => true
        )
	
needsPackage"SimpleDoc"

export {"firstFunction","secondFunction"}

firstFunction = method(TypicalValue => String)
firstFunction ZZ := String => n -> if n == 1 then "Hello World!" else "D’oh!"

secondFunction = method()
secondFunction Ring := ZZ => R-> dim R

end

beginDocumentation()
doc ///
    Key
        ZhangchiFirstPackage
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
       firstFunction 1 == "Hello Wold!"
       firstFunction 0 == "D'oh!"
///
doc ///
    Key
        (secondFunction,Ring)
    Headline
        a not so silly second function
    Usage
        f = secondFunction R
    Inputs
	R:Ring
    Outputs
        f:ZZ
          an interger, the dim of the Ring R
    Description
     Text
       Here we show an example.
     Example
       secondFunction ZZ == 1
       secondFunction ZZ[X] == 2
///
TEST ///
    assert ( firstFunction 2 == "D’oh!" )
///

end--

restart
needsPackage"ZhangchiFirstPackage"
help ZhangchiFirstPackage
help "firstFunction(ZZ)"
help firstFunction
help "firstFunction(ZZ)"
