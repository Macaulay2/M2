--		Copyright 1997 by Daniel R. Grayson

Function @ List := (f,v) -> error ///
'@' is obsolete now - use '\' or '/' instead to apply a 
function to each element of a list.
///

unlist = X -> error "'unlist' has been replaced by toSequence"
unlist

elements = X -> error "'elements' has been replace by toList"
elements

document { "obsolete functions and symbols",
     "The following functions and symbols have become obsolete.",
     MENU {
	  TO "unlist",
	  TO "elements",
	  TO "syms",
	  TO "expand",
	  TO "evaluate",
	  }
     }

document { quote unlist,
     TT "unlist",
     PARA,
     "An obsolete function, replaced by ", TO "toSequence", "."
     }

document { quote elements,
     TT "elements",
     PARA,
     "An obsolete function, replaced by ", TO "toList", "."
     }

document { quote syms,
     TT "syms",
     PARA,
     "A symbol, replaced by ", TO "generatorSymbols", "."
     }

document { quote expand,
     TT "expand",
     PARA,
     "A function replaced by ", TO "value", "."
     }

document { quote evaluate,
     TT "evaluate",
     PARA,
     "A function replaced by ", TO "value", "."
     }
