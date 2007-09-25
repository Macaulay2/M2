--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => wait,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

///
fork, wait, 

f = openInOut("!M2")
if fork() === 0
  then (print "in the child"; exit 0)  else (sleep 3; print "I'm the parent"; exit 0)

  

///