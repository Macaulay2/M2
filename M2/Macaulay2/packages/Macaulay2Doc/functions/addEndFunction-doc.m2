--- status: done
--- author(s): dan
--- notes: 

document {
     Key => addEndFunction,
     Headline => "add an ending function",
     Usage => "addEndFunction f",
     Inputs => { "f" => Function },
     Consequences => {
	  {"When the program is about the exit, the function ", TT "f", " will be called, with no arguments."}
	  },     
     SeeAlso => {"addStartFunction"}
     }

