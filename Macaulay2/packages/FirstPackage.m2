-- -*- coding: utf-8 -*-
newPackage(
	"FirstPackage",
    	Version => "1.0", 
    	Date => "February 11, 2004",
    	Authors => {{Name => "Jane Doe", 
		  Email => "doe@math.uiuc.edu", 
		  HomePage => "http://www.math.uiuc.edu/~doe/"}},
    	Headline => "an example Macaulay 2 package",
    	DebuggingMode => true
    	)

export {firstFunction}

firstFunction = method(TypicalValue => String)
firstFunction ZZ := String => n -> if n == 1 then "Hello World!" else "D'oh!"

beginDocumentation()
document { 
	Key => FirstPackage,
	Headline => "an example Macaulay 2 package",
	EM "FirstPackage", " is a basic package to be used as an example."
	}
document {
	Key => {(firstFunction,ZZ),firstFunction},
	Headline => "a silly first function",
	Usage => "firstFunction n",
	Inputs => { "n" },
	Outputs => {{ "a silly string, depending on the value of ", TT "n" }},
        SourceCode => {(firstFunction,ZZ)},
	EXAMPLE lines ///
	   firstFunction 1
	   firstFunction 0
     	///
	}

TEST ///
    assert ( firstFunction 2 == "D'oh!" )
///
