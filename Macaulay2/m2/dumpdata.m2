phase = 0
scan(openFiles(), f -> (
	  flush stderr;
	  flush stdout;
	  if not (f === stdout or f === stdin or f === stderr)
	  then (
	       << "--closing file " << name f << "\n";
	       close f;
	       )))
errorDepth (reloaded + 1)
collectGarbage()
fn := concatenate("../cache/Macaulay2-",
     try first lines get "!uname -m | sed s=/=-=g" else version#"ARCH", 
     ".data")
<< "dumping to " << fn << endl
dumpdata fn
