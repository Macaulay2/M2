--		Copyright 1996 by Daniel R. Grayson

split := s -> flatten apply(lines(s,"/"), i -> lines(i,"\\")) -- sigh...

enquote := s -> "'" | s | "'"

M2HOME := substring(concatenate between(pathSeparator, drop(split("X" | currentDirectory()),-1)), 1)

(
  if version#"operating system" === "Windows-95-98-NT"
  then (
       dossify := s -> concatenate between("\\",lines(s,"/"));
       "../bin/M2.arg"
       << "'-e loaddata \"" << M2HOME << "/cache/Macaulay2-"
       << version#"architecture"
       << "-data\"'" << endl
       << "--" << endl
       -- << "'-e path = {\".\", \"" << M2HOME << "/m2\"}'" << endl
       << "'-e runStartFunctions()'" << endl
       << close;
       "../bin/M2.bat"
       << "@echo off" << endl
       << "set LFN=Y" << endl
       << dossify concatenate (M2HOME, "/bin/Macaulay2") 
       << " @" << concatenate (M2HOME, "/bin/M2.arg") 
       << " %1 %2 %3 %4 %5 %6 %7 %8 %9" << endl
       << close;
       );
  (
       args := new Manipulator from (
	    if version#"dumpdata" then (
		 o -> o
		 << "exec $M2HOME/bin/Macaulay2 "
		 << format concatenate( 
		      "-e loaddata ", format concatenate(
			   "$M2HOME/cache/Macaulay2-`uname -m | sed s=/=-=g `-data" 
			   )
		      )
		 << " --"
		 -- << " $TTY"
		 << " "
		 -- << enquote ( "-epath={" | format "." | "," | format (M2HOME | "/m2") | "}" )
		 << " "
		 << enquote "-e runStartFunctions()" 
		 )
	    else (
		 o -> o
		 << "exec "
		 << format "$M2HOME/bin/Macaulay2"
		 -- << " $TTY"
		 << " -ephase=1"
		 << " "
		 << format "$M2HOME/m2/setup.m2"
		 << " "
		 << "-ephase=0"
		 << " "
		 -- << enquote ( "-epath={" | format "." | "," | format (M2HOME | "/m2") | "}" )
		 << " "
		 << enquote "-e runStartFunctions()" 
		 )
	    );
       FILENAME := "../bin/M2";
       FILENAME
       << "#! /bin/sh" << endl
       << "M2HOME=" << M2HOME << endl
       << "export M2HOME" << endl
       -- << "if [ \"$EMACS\" = t ]; then TTY=-tty; else TTY=; fi" << endl
       << "if [ $# = 0 ]" << endl
       << "then " << args << endl
       << "else " << args << " " << format "$@" << endl
       << "fi" << endl << close;
       run concatenate ("chmod a+x ", FILENAME);
  )
)
