dump = filename -> (
     -- load dumpdata.m2 and then execute dump(), so dumpdata.m2 is closed when the dump occurs
     if not version#"dumpdata" then error "can't dump data with this version of Macaulay 2";
     arch := if getenv "M2ARCH" =!= "" then getenv "M2ARCH" else version#"architecture";
     fn := if class filename === String then "../" | filename else concatenate("Macaulay2-", arch, "-data");
     << "--dumping to " << fn << endl << flush;
     runEndFunctions();
     erase symbol dump;
     collectGarbage();
     dumpdata fn;
     << "--dumped to " << fn << endl << flush;
     exit 0;
     )
