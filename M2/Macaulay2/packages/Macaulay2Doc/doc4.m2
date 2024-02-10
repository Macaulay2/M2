-- -*- coding: utf-8 -*-
--		Copyright 1993-1998 by Daniel R. Grayson

document {
     Key => InverseMethod,
     Headline => "compute reciprocals",
     TT "InverseMethod", " -- a key used under which is stored a method
     for computing multiplicative inverses.",
     PARA{},
     "Internal routines for computing powers call upon that method when
     the exponent is negative."
     }

document {
     Key => { symbolBody, SymbolBody },
     Headline => "symbol bodies",
     PARA {
	  "A Macaulay2 symbol is much like a function closure, in that it comes equipped with a pointer to a frame
	  that contains a value for it.  That value can be recovered with the function ", TO "value", ", as follows."
	  },
     EXAMPLE lines ///
     f = x -> symbol x
     s = f 1
     t = f 2
     ///,
     PARA {
	  "The two symbols created in the example above have something in common -- they are created by the same bit
	  of code (the function ", TT "f", "), but they have different values."
	  },
     EXAMPLE lines ///
     value s
     value t
     ///,
     PARA {
	  "To allow the user to determine whether two symbols are created by the same bit of code, Macaulay2 has
	  the notion of symbol body, which parallel to the notion of function body.  It's essentially the symbol, but
	  without the pointer to the frame that contains the value."
	  },
     EXAMPLE lines ///
     symbolBody s
     symbolBody t
     symbolBody s === symbolBody t
     ///,
     PARA {
	  "All such symbol bodies are members of the class ", TT "SymbolBody", "."
	  },
     EXAMPLE lines ///
     class symbolBody s
     ///
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
