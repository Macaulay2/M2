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
     SeeAlso => {"addEndFunction"}
     }
