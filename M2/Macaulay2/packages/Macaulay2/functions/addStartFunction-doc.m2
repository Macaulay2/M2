--- status: done
--- author(s): dan
--- notes: 

document {
     Key => addStartFunction,
     Headline => "add a startup function",
     Usage => "addStartFunction f",
     Inputs => { "f" => Function },
     Consequences => {
	  {"When the program restarts, the function ", TT "f", " will be called, with no arguments."}
	  },
     "The program is said to restart if ", TO "dumpdata", " is used to save the state, and at
     some later time, ", TT "loaddata", " is used to restore it.",
     SeeAlso => {"addEndFunction", "loaddata", "dumpdata"}
     }
