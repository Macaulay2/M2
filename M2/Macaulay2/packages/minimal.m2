-- this is a minimal example of a package

newPackage("minimal",
     Version => "1.0", 
     Date => "December 14, 2004",
     Author => "Daniel R. Grayson <dan@math.uiuc.edu>",
     HomePage => "http://www.math.uiuc.edu/~dan/Macaulay2/",
     Headline => "A minimal example of a Macaulay 2 package",
     DebuggingMode => false
     )
export (f, "g" => f)

f = x -> print "hi there!"

beginDocumentation()

TEST "assert( f() === null )"

document { Key => "minimal",
     Headline => "a minimal example of a Macaulay 2 package",
     "This package is just a minimal example of a package."
     }

document { Key => "f",
     Headline => "print a message",
     Usage => "f()",
     Inputs => { },
     Outputs => { },
     PARA,
     "This function will print a message.",
     EXAMPLE {
	  "f()"
	  }
     }
