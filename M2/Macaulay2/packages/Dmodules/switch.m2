-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai 

---------------------------------------------------------------------------------
-- InfoLevel switch 
-- determines how often and of which depth remarks are made by D-routines
-- Suggested levels:
-- 	1: "still-alive" remarks as "localize: computing b-function..."
--     	    	      	   	     ^^^^^^^^
--         (should include a reference to the routine talking)
--     
-- 	2: benchmarks: "time = ..."
--      666: debugging info, reserved for developers.
---------------------------------------------------------------------------------
 
INFOLEVEL := 0

Dtrace  = method()
Dtrace ZZ := ZZ => level -> (t := INFOLEVEL;  INFOLEVEL = level; t)
getDtrace = () -> INFOLEVEL

-- prints Info 
-- format: pInfo(min_level, Thing)
pInfo = method();
pInfo(ZZ, Thing) := (minLevel, s) -> (
     if minLevel <= getDtrace() then print s ;
     << flush;
     ); 
pInfo(ZZ, List) := (minLevel, l) -> (
     if minLevel <= getDtrace() then (
	  scan(l, u-><<u); 
     	  << endl << flush;
	  )
     ); 
