--		Copyright 1993-1999 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

-- leave this at the end, to get a complete list of options
document { quote Options,
     TT "Options", " -- an option used with ", TO "method", " to specify
     names of optional arguments and their default values.",
     PARA,
     NOINDENT,
     TT "f = method(Options => w)", " -- creates a method which accepts
     optional arguments.  Here 'w' is a list ", TT "{A=>a,B=>b,...}", " of
     optional argument names A,B,... and corresponding default values a,b,...",
     PARA,
     "The methods installed for this method function should be written in
     the form ", TT "opts -> args -> (...)", ".  The argument ", TT "args", "
     will be assigned a hash table of type ", TO "OptionTable", " containing 
     the optional argument names and their values.  The default table will
     be stored in the ", TO "OptionsRegistry", " and can be recovered with the 
     function ", TO "options", ".",
     EXAMPLE {
	  "f = method(Options => {Slope => 1, Intercept => 1})",
      	  "f RR := o -> x -> o.Slope * x + o.Intercept",
      	  "f(5.,Slope=>100)",
	  "options f",
	  },
     PARA,
     "Here is a complete list of symbols which are used as names of options:
     ",
     between( ",
     ",
     	  (i -> TO i) \ rsort keys set flatten (keys \ values OptionsRegistry)
	  ),
     ".",
     SEEALSO "method"
     }

if phase > 1 then load "docloads.m2"

path = (
     if getenv "M2HOME" === "" 
     then { "." }
     else { "." , getenv "M2HOME" | "/packages" }
     )

setrecursionlimit 300

addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))


