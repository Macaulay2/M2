--		Copyright 1996 by Daniel R. Grayson

M2HOME := concatenate between("/",drop(lines(currentDirectory(),"/"),-1))

(
  if version#"OS" === "MS-DOS"
  then (
       dossify := s -> concatenate between("\\",lines(s,"/"));
       "../bin/M2.arg"
       << "'-e loaddata \"" << M2HOME << "/cache/Macaulay2-"
       << version # "ARCH"
       << ".data\"'" << endl
       << "--" << endl
       << "'-e path = prepend(\"" << M2HOME << "/m2\", path)'" << endl
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
       FILENAME := "../bin/M2";
       args := new Manipulator from (
	    o -> o
	    << "exec $M2HOME/bin/Macaulay2 "
	    << format concatenate( 
		 "-e loaddata ", format concatenate(
		      "$M2HOME/cache/Macaulay2-`uname -m | sed s=/=-=g `.data" 
		      )
		 )
	    << " -- "
	    << format concatenate( "-e path = prepend(", format "$M2HOME/m2", ", path)" )
	    << " "
	    << format "-e runStartFunctions()" 
	    );
       FILENAME
       << "#! /bin/sh" << endl
       << "M2HOME=" << M2HOME << endl
       << "if [ $# = 0 ]" << endl
       << "then " << args << endl
       << "else " << args << " " << format "$@" << endl
       << "fi" << endl << close;
       run concatenate ("chmod a+x ", FILENAME);
  )
)
