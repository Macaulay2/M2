--- status: Draft
--- author(s): MES
--- notes: 

document { 
     Key => {registerFinalizer},
     Headline => "register a string that will be displayed when an object is garbage collected",
     Usage => "registerFinalizer(x,str)",
     Inputs => {
	  "x" => Thing,
	  "str" => String
	  },
     Consequences => {
	  "A finalizer is registered with the garbage collector to print a string
	  when that object is collected as garbage"
	  },
     EXAMPLE lines ///
     	  R = QQ[a..d];
	  makeGB = (n) -> (g := gb((ideal vars R)^4); registerFinalizer(g, "gb("|n|")"););
	  for i from 1 to 10 do (makeGB i);
	  collectGarbage()
	  ///,
     Caveat => "This function should mainly be used for debugging.  Having a large number of finalizers
     might degrade the performance of the program.  Moreover, registering two or more objects that are members of a circular chain
     of pointers for finalization will result in a memory leak, with none of the objects in the chain
     being freed, even if nothing else points to any of them.",
     SeeAlso => {
	  collectGarbage
	  }
     }
