--		Copyright 1996 by Daniel R. Grayson

M2HOME := concatenate between("/",drop(lines(currentDirectory(),"/"),-1))

(
  if version#"operating system" === "MS-DOS"
  then (
       dossify := s -> concatenate between("\\",lines(s,"/"));
       "../bin/M2.arg"
       << "'-e loaddata \"" << M2HOME << "/cache/Macaulay2-"
       << version # "ARCH"
       << ".data\"'" << endl
       << "--" << endl
       << "'-e path = append(path, \"" << M2HOME << "/m2\")'" << endl
       << "'-e runStartFunctions()'" << endl
       << close;
       "../bin/M2.bat"
       << "@echo off" << endl
       << "set LFN=Y" << endl
       << dossify concatenate (M2HOME, "/bin/Macaulay2") 
       << " @" << concatenate (M2HOME, "/bin/M2.arg") 
       << " %1 %2 %3 %4 %5 %6 %7 %8 %9" << endl
       << close;
  ) else (
       args := new Manipulator from (
	    if version#"dumpdata" then (
		 o -> o
		 << "exec $M2HOME/bin/Macaulay2 "
		 << format concatenate( 
		      "-e loaddata ", format concatenate(
			   "$M2HOME/cache/Macaulay2-`uname -m | sed s=/=-=g `.data" 
			   )
		      )
		 << " -- $TTY "
		 << format ( "-e path = append(path, "| format "$M2HOME/m2" | ")" )
		 << " "
		 << format "-e runStartFunctions()" 
		 )
	    else (
		 o -> o
		 << "exec "
		 << format "$M2HOME/bin/Macaulay2"
		 << " $TTY -ephase=1"
		 << " "
		 << format "$M2HOME/m2/setup.m2"
		 << " "
		 << "-ephase=0"
		 << " "
		 << format ( "-e path = append(path, " | format "$M2HOME/m2" | ")" )
		 << " "
		 << format "-e runStartFunctions()" 
		 )
	    );
       FILENAME := "../bin/M2";
       FILENAME
       << "#! /bin/sh" << endl
       << "M2HOME=" << M2HOME << endl
       << "if [ \"$EMACS\" = t ]; then TTY=-tty; else TTY=; fi" << endl
       << "if [ $# = 0 ]" << endl
       << "then " << args << endl
       << "else " << args << " " << format "$@" << endl
       << "fi" << endl << close;
       run concatenate ("chmod a+x ", FILENAME);
  )
)
