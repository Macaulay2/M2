--		Copyright 1997 by Daniel R. Grayson

document { "obsolete functions and symbols",
     "The following functions and symbols have become obsolete.",
     MENU {
	  TO "elements",
	  TO "evaluate",
	  TO "expand",
	  TO "seq",
	  TO "syms",
	  TO "unlist",
	  TO "verticalJoin",
	  }
     }

unlist = X -> error "'unlist' has been replaced by toSequence"
unlist

document { quote unlist,
     TT "unlist",
     PARA,
     "An obsolete function, replaced by ", TO "toSequence", "."
     }

elements = X -> error "'elements' has been replace by toList"
elements

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

expand = X -> error "'expand' has been replaced by 'value'"
expand

document { quote expand,
     TT "expand",
     PARA,
     "A function replaced by ", TO "value", "."
     }

evaluate = X -> error "'evaluate' has been replaced by 'value'"
evaluate

document { quote evaluate,
     TT "evaluate",
     PARA,
     "A function replaced by ", TO "value", "."
     }

seq = X -> error "'seq' has been replaced by 'singleton'"
seq

document { quote seq,
     TT "seq",
     PARA,
     "A function replaced by ", TO "singleton", "."
     }

verticalJoin = X -> error "'verticalJoin' has been replaced by 'stack'"
verticalJoin

document { quote verticalJoin,
     TT "verticalJoin",
     PARA,
     "A function replaced by ", TO "stack", "."
     }
