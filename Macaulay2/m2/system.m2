--		Copyright 1995 by Daniel R. Grayson

restart = new Command from ( 
     () -> (
	  runEndFunctions();
	  exec commandLine
	  )
     )

     