--		Copyright 1995 by Daniel R. Grayson

restart = Command ( 
     () -> (
	  runEndFunctions();
	  exec commandLine
	  )
     )

     