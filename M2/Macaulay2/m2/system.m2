--		Copyright 1995-2002 by Daniel R. Grayson

-- This version of 'run' doesn't handle pipes or redirection, of course
-- but it's an advantage to have this facility without depending on an outside shell.
-- We comment it out because some systems don't have wordexp() in libc, upon which 
-- expandWord is based.
-- run = cmd -> if (pid := fork()) == 0 then exec expandWord cmd else wait pid

restart = Command ( 
     () -> (
	  runEndFunctions();
	  exec commandLine
	  )
     )

setRandomSeed = method()
setRandomSeed ZZ := seed -> (
     error "random seed not re-implemented yet";
     (callgg(ggrandomseed, seed);)
     )
setRandomSeed String := seed -> setRandomSeed fold((i,j) -> 101*i + j, 0, ascii seed)
