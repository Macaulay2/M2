dump = () -> (
     -- load dumpdata.m2 and then execute dump(), so dumpdata.m2 is closed when the dump occurs
     if not version#"dumpdata" then error "can't dump data with this version of Macaulay 2";
     -- if version#"operating system" === "Linux" then (
     --	  << "open files : " << stack lines get("!ls -l /proc/"|toString processID()|"/fd") << endl;
     --   );
     fn := concatenate("../libexec/Macaulay2-",
	  try first lines get "!uname -m | sed s=/=-=g" 
	  else version#"architecture", 
	  "-data");
     << "dumping to " << fn << endl << flush;
     runEndFunctions();
     erase symbol dump;
     phase = 0;
     collectGarbage();
     dumpdata fn;
     exit 0;
     )
