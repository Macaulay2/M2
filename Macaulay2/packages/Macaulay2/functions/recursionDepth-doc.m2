document { 
     Key => recursionDepth,
     Headline => "the current recursion depth",
     Usage => "recursionDepth()",
     Outputs => { ZZ => "the current depth of recursion in the interpreter" },
     EXAMPLE lines ///
	  recursionDepth()
     	  f = x -> recursionDepth()
	  f()
	  g = x -> f()
	  g()
     ///,
     "Tail recursion optimization may be in effect, so there the answer may not always be what you expect.",
     EXAMPLE lines ///
     	  r = i -> if i == 100 then recursionDepth() else r(i+1)
	  r 0
     ///,
     SeeAlso => { "recursionLimit" }
     }
