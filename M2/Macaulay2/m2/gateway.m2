--		Copyright 1995 by Daniel R. Grayson

ScriptedFunction = new Type of MutableHashTable
ScriptedFunction ^ Thing := (G,i) -> (
     if G#?superscript then G#superscript i
     else error("no method for ", name G, "^", name i))
ScriptedFunction _ Thing := (G,i) -> (
     if G#?subscript then G#subscript i
     else error("no method for ", name G, "_", name i))

GlobalAssignHook ScriptedFunction := globalAssignFunction
GlobalReleaseHook ScriptedFunction := globalReleaseFunction

id = new ScriptedFunction from { 
     subscript => (
	  (x) -> (
	       r := lookup(id,class x);
	       if r =!= null then r x
	       else error ("no method 'id_' found for item of class ", name class x)))
     }

document { quote ScriptedFunction,
     TT "ScriptedFunction", " -- the class of all scripted functions,
     by which we mean those functions 'f' of one argument 'x' which
     accept their argument as a subscript 'f_x' or as a superscript 'f^x'.",
     PARA,
     "To create a new subscripted function use a statement of the following
     form.",
     PRE "     f = new ScriptedFunction from { subscript => (x) -> ... }",
     "To create a new superscripted function use a statement of the following
     form.",
     PRE "     f = new ScriptedFunction from { superscript => (x) -> ... }",
     "The subscript and superscript options can be combined to create a
     scripted function which accepts either a subscript or a superscript.",
     PARA,
     "A good example of a subscripted function is ", TO "identity", ".",
     PARA,
     SEEALSO "ScriptedFunctor"
     }

ScriptedFunctor = new Type of MutableHashTable
GlobalAssignHook ScriptedFunctor := globalAssignFunction
GlobalReleaseHook ScriptedFunctor := globalReleaseFunction
ScriptedFunctor ^ Thing := (G,i) -> (
     if G#?superscript 
     then G#superscript i
     else error("no method for ", name G, "^", name i)
     )
ScriptedFunctor _ Thing := (G,i) -> (
     if G#?subscript 
     then G#subscript i
     else error("no method for ", name G, "_", name i)
     )
ScriptedFunctor Thing := (G,X) -> (
     if G#?argument
     then G#argument X
     else error("no method for ", name G, " ", name X)
     )
protect argument
protect subscript
protect superscript

args := method()
args(Thing,Sequence) := (i,args) -> prepend(i,args)
args(Thing,Thing) := identity
args(Thing,Thing,Sequence) := (i,j,args) -> prepend(i,prepend(j,args))
args(Thing,Thing,Thing) := identity

HH = new ScriptedFunctor from {
     subscript => (
	  i -> new ScriptedFunctor from {
	       superscript => (
		    j -> new ScriptedFunctor from {
	       	    	 argument => (
			      X -> cohomology args(i,j,X)
			      )
	       	    	 }
		    ),
	       argument => (
		    X -> homology args(i,X)
		    )
	       }
	  ),
     superscript => (
	  j -> new ScriptedFunctor from {
	       subscript => (
		    i -> new ScriptedFunctor from {
	       	    	 argument => (
			      X -> homology args(j,i,X)
			      )
	       	    	 }
		    ),
	       argument => (
		    X -> cohomology args(j,X)
		    )
	       }
	  ),
     argument => homology
     }

cohomology(ZZ,Sequence) := (i,X) -> cohomology prepend(i,X)
  homology(ZZ,Sequence) := (i,X) ->   homology prepend(i,X)

document { quote ScriptedFunctor,
     TT "ScriptedFunctor", " -- the class of all functors which accept a 
     subscript or a superscript, the primary example of which is ", TO "HH", ".",
     SEEALSO {"subscript", "superscript", "argument"}
     }

document { quote argument,
     TT "argument", " -- a key used in scripted functors under which is
     stored the function that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { quote subscript,
     TT "subscript", " -- a key used in scripted functors under which is
     stored the function of one variable that accepts the subscript and
     returns a scripted functor that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { quote superscript,
     TT "superscript", " -- a key used in scripted functors under which is
     stored the function of one variable that accepts the superscript and
     returns a scripted functor that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { quote HH,
     TT "HH", " -- general homology and cohomology functor.",
     PARA,
     "Specific examples:",
     MENU {
	  (TO (homology, ZZ, ChainComplex), "      -- homology of a chain complex"),
	  (TO (homology, ChainComplex), "          -- total homology of a chain complex"),
	  (TO (homology, ZZ, ChainComplexMap), "   -- homology as a functor"),
	  (TO (homology, Matrix, Matrix), "        -- homology of a pair of maps"),
	  (TO (cohomology, ZZ, ChainComplex), "    -- cohomology of a chain complex"),
	  (TO (cohomology, ZZ, ChainComplexMap), " -- cohomology as a functor"),
	  (TO (cohomology, ZZ, Module), "          -- local cohomology"),
	  (TO (cohomology, ZZ, CoherentSheaf), "   -- sheaf cohomology"),
	  }
     }

TEST ("
     R=ZZ/101[a..d]
     C=resolution cokernel vars R
     D = C ++ C[1] ++ C[2]
     betti D
     assert( degree HH_1 D === 0 )
     assert( degree HH_0 D === 1 )
     assert( degree HH_-1 D === 1 )
     assert( degree HH_-2 D === 1 )
     ")

document { quote cohomology,
     TT "cohomology", " -- a method name available for computing expressions
     of the forms HH^i(X) and HH^i(M,N).",
     PARA,
     "If it is intended that i be of class ZZ, M be of class A, and N be of 
     class B, then the method can be installed with ",
     PRE "   cohomology(ZZ, A, B) := (i,M,N,opts) -> ...",
     SEEALSO {"homology", "ScriptedFunctor"}
     }

document { quote homology,
     TT "homology(f,g)", " -- computes the homology module (kernel f)/(image g).",
     BR, NOINDENT,
     TT "homology", " -- a method name available for computing expressions
     of the forms HH_i(X) and HH_i(M,N).",
     PARA,
     "If it is intended that i be of class ZZ, M be of class A, and N be of
     class B, then the method can be installed with ",
     PRE "   homology(ZZ, A, B) := (i,M,N,opts) -> ...",
     SEEALSO {"cohomology", "ScriptedFunctor"}
     }
