if not version#"dumpdata" then error "can't dump data with this version of Macaulay 2"
phase = 0
scan(openFiles(), f -> (
	  flush stderr;
	  flush stdio;
	  if not (f === stdio or f === stdio or f === stderr)
	  then (
	       << "--closing file " << name f << "\n";
	       close f;
	       )))
collectGarbage()
fn := concatenate("../cache/Macaulay2-",
     try first lines get "!uname -m | sed s=/=-=g" 
     else if getenv "ARCH" != "" then concatenate between("-", lines(getenv "ARCH","/"))
     else version#"architecture", 
     ".data")
<< "dumping to " << fn << endl
dumpdata fn
