--		Copyright 1997 by Daniel R. Grayson

Function @ List := (f,v) -> error ///
'@' is obsolete now - use '\' or '/' instead to apply a 
function to each element of a list.
///

unlist = X -> error "'unlist' has been replaced by toSequence"
unlist

elements = X -> error "'elements' has been replace by toList"
elements

document { "obsolete functions",
     "The following functions have become obsolete.",
     MENU {
	  TO "unlist",
	  TO "elements"
	  }
     }

document { quote unlist,
     TT "unlist",
     PARA,
     "An obsolete function, replace by ", TO "toSequence", "."
     }

document { quote elements,
     TT "elements",
     PARA,
     "An obsolete function, replace by ", TO "toList", "."
     }
