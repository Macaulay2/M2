-- This file contains the code to interface to the engine
-- (c) 1994  Michael E. Stillman

ggPush HashTable := n -> (ggINT, gg n.handle, ggderef)
ggPush Handle := n -> (ggINT, gg n, ggderef)

callgg = args -> (
     sendgg (apply(#args-1, i -> ggPush args#(i+1)), args#0)
     )
document { quote callgg,
     TT "callgg(f,x,y,...)", " -- calls the ", TO "engine", " with engine
     command string f, after pushing engine objects corresponding to
     x, y, ... onto the engine's stack."
     }

ggPush String := s -> concatenate(ggSTRING, gg (# s), s)
eePop = format -> convert(format, sendgg ggtonet)

ZZ.pop = eePopInt = () -> eePop ConvertInteger
ZZ.handle = newHandle ggZ
ggPush ZZ := i -> (ggINT, gg i)

eePopBool = () -> eePop ConvertInteger === 1
eePopIntarray = () -> eePop ConvertList ConvertInteger
eePromote = (f,R) -> (
     sendgg(ggPush R, ggPush f, ggpromote);
     R.pop())
eeLift = (f,R) -> (
     sendgg(ggPush R, ggPush f, gglift);
     R.pop())

-- these routines are used just for debugging
look  = new Command from (() -> (<< sendgg ggsee;))
document { quote look,
     TT "look()", " -- display item on the top of the engine's stack.",
     PARA,
     "It's a ", TO "Command", " so it may be entered simply
     as ", TT "look", " if it's alone on the command line.",
     PARA,
     "Used mainly for debugging the engine.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "sendgg ggPush R",
	  "look"
	  }
     }
     
engineStack = new Command from (() -> stack lines sendgg ggstack)
document { quote engineStack,
     TT "engineStack()", " -- returns a net containing a display of the contents
     of the engine's stack.",
     PARA,
     "It's a ", TO "Command", " so it may be entered simply
     as ", TT "engineStack", " if it's alone on the command line.",
     PARA,
     "Used mainly for debugging the engine.",
     EXAMPLE {
	  "ZZ/101[x,y,z]",
      	  "f = matrix {{x,y,z}}",
      	  "sendgg ggPush f",
      	  "engineStack"
	  },
     }
     
heap = new Command from (() -> (<< sendgg ggheap;))
document { quote heap,
     TT "heap()", " -- display the contents of the engine's heap.",
     PARA,
     "It's a ", TO "Command", " so it may be entered simply
     as ", TT "heap", " if it's alone on the command line.",
     PARA,
     "Used mainly for debugging the engine.",
     EXAMPLE {
	  "ZZ/101[x,y,z];",
      	  "matrix {{x,y,z}}",
      	  "heap"
	  },
     }
     
engineMemory = new Command from (() -> (<< sendgg ggmem;))
document { quote engineMemory,
     TT "engineMemory()", " -- display the memory usage of the engine.",
     PARA,
     "It's a ", TO "Command", " so it may be entered simply
     as ", TT "engineMemory", " if it's alone on the command line.",
     PARA,
     "Used mainly for debugging the engine.",
     EXAMPLE {
	  "ZZ/101[x,y,z];",
      	  "matrix {{x,y,z}}",
      	  "engineMemory"
	  }
     }

see = method()
see ZZ := i -> sendgg(ggPush i, ggderef, ggsee, ggpop)
see Handle := i -> sendgg(ggPush i, ggsee, ggpop)
see HashTable := (X) -> if X.?handle then see X.handle else error "not an engine object"

document { quote see,
     TT "see i", " -- return a string which displays the engine object whose handle 
     is the integer i.",
     BR,
     TT "see X", " -- return a string which displays the engine object corresponding 
     to the ring, matrix, module, or ring element X.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "see R",
      	  "see (x+1)^6",
      	  "see handle (x*y*z)",
      	  "see 0"
	  }
     }
-----------------------------------------------------------------------------

document { "engine", 
     "The engine is the part of the program that is dedicated to
     performing the computation of Groebner bases with Buchberger's
     algorithm.  It is coded directly in C++ for speed, and it communicates
     with the front-end interpreter through a bidirectional stream of bytes,
     so that in future implementations the engine may reside in a separate
     process on a distant machine.",
     MENU {
	  TO "engine communication protocol",
     	  TO "low level gb engine commands",
	  TO "high level gb engine commands"
	  },
     PARA,
     "The Macaulay 2 engine provides fast polynomial and matrix operations,
     and Groebner bases, syzygies, Hilbert functions, resolutions and
     other operations that we feel need to be implemented directly for
     efficiency reasons.",
     }

document { "high level gb engine commands",
     "Sending commands to the engine:",
     MENU {
	  TO "callgg",
	  TO "gg",
	  TO "ggPush",
	  TO "handle",
	  TO "sendToEngine",
	  TO "sendgg"
	  },
     "This class provides an interface to rings implemented by the engine.",
     MENU {
	  TO "EngineRing"
	  },
     "These routines take an element off the stack.",
     MENU {
	  TO "eePop",
	  TO "eePopBool",
	  TO "eePopInt",
	  TO "eePopIntarray",
	  TO "getMatrix"
	  },
     "These functions transfer ring elements to other rings.",
     MENU {
	  TO "eeLift",
	  TO "eePromote"
	  },
     "These functions are used mainly for debugging the engine.",
     MENU {
	  TO "look",
	  TO "engineMemory",
	  TO "engineStack",
	  TO "heap",
	  TO "see"
	  }
     }

document { quote eePopInt,
     TT "eePopInt()", " -- pop the integer from the top of the engine's stack,
     returning its value.",
     PARA,
     SEEALSO "high level gb engine commands"
     }

document { quote eePopIntarray,
     TT "eePopIntarray()", " -- pop the array of integers from the top of the engine's 
     stack, returning its value as a list of integers.",
     PARA,
     SEEALSO "high level gb engine commands"
     }

document { quote eePopBool,
     TT "eePopBool()", " -- pop a boolean value from the top of the engine's stack,
     returning true or false.",
     PARA,
     SEEALSO "high level gb engine commands"
     }

document { quote eePop,
     TT "eePop f", " -- take an engine conversion format string and use it
     to convert an object popped from the top of the engine's stack.",
     PARA,
     SEEALSO "high level gb engine commands"
     }

document { quote eePromote,
     TT "eePromote(f,R)", " -- promote a ring element ", TT "f", " to the
     ring ", TT "R", "."
     }

document { quote eeLift,
     TT "eeLift(f,R)", " -- lift a ring element ", TT "f", " to the
     ring ", TT "R", "."
     }
