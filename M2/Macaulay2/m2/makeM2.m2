--		Copyright 1996 by Daniel R. Grayson

split := s -> flatten apply(separate("/",s), i -> separate("\\",i)) -- sigh...

M2HOME := concatenate between(pathSeparator, drop(split currentDirectory(), -1))

fix := s -> format concatenate s

(
  if version#"operating system" === "Windows-95-98-NT"
  then (
       dossify := s -> concatenate between("\\",lines("/",s));
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
       << dossify concatenate (M2HOME, "/libexec/Macaulay2") 
       << " @" << concatenate (M2HOME, "/bin/M2.arg") 
       << " %1 %2 %3 %4 %5 %6 %7 %8 %9" << endl
       << close;
       );
  (
       command := new Manipulator from (
	    if version#"dumpdata" then (
		 o -> o
		 )
	    else (
		 o -> o
		 )
	    );
       FILENAME := "../bin/M2";
       M2 := FILENAME
       << "#! /bin/sh" << endl
       << "M2HOME='" << M2HOME << "'" << endl
       -- << "export M2HOME" << endl
       << "EXE=" << fix "$M2HOME/libexec/Macaulay2" << endl
       << "DATA=" << fix "$M2HOME/libexec/Macaulay2-`uname -m |sed s=/=-=g`-data" << endl
       << "SETUP=" << fix "$M2HOME/m2/setup.m2" << endl;
       if getenv "SHARED" =!= "" then ( M2
	    << ///if [ "" = "$LD_LIBRARY_PATH" ]/// << endl
       	    << ///then LD_LIBRARY_PATH="$M2HOME/lib"/// << endl
       	    << ///else LD_LIBRARY_PATH="$M2HOME/lib:$LD_LIBRARY_PATH"/// << endl
       	    << ///fi/// << endl
       	    << ///export LD_LIBRARY_PATH/// << endl
       	    << ///# reminder: root ignores LD_LIBRARY_PATH/// << endl
	    );
       -- << "if [ \"$EMACS\" = t ]; then TTY=-tty; else TTY=; fi" << endl
       M2
       << "if [ -f \"$DATA\" ]" << endl
       << "then exec " << fix "$EXE"
		 << " " << fix "-e loaddata \"$DATA\""
		 << " -- "
		 << fix "-e runStartFunctions()" << " " << fix "$@" << endl
       << "else exec " << fix "$EXE"
		 << " -ephase=1 \"$SETUP\" -ephase=0 "
		 << fix "-e runStartFunctions()" << " " << fix "$@" << endl
       << "fi" << endl << close;
       run concatenate ("chmod a+x ", FILENAME);
  )
)
