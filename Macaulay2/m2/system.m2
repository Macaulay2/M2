--		Copyright 1995-2001 by Daniel R. Grayson

restart = Command ( 
     () -> (
	  runEndFunctions();
	  exec commandLine
	  )
     )

setRandomSeed = method()
setRandomSeed ZZ := seed -> (callgg(ggrandomseed, seed);)
setRandomSeed String := seed -> setRandomSeed fold((i,j) -> 101*i + j, 0, ascii seed)
