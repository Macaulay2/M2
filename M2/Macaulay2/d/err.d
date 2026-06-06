--		Copyright 1994 by Daniel R. Grayson
use stdio;

warnings := 0;
export warning(msg:string):void := (
     warnings = warnings + 1;
     stdIO << flush;
     endLine(stdError);
     stderr << argv.0 << ": warning: " << msg << newline;
     if warnings > 1000 then (
	  stderr << "too many warnings, exiting" << endl;
	  exit(1);
	  );
     );
errors := 0;
export error(msg:string):void := (
     errors = errors + 1;
     stdIO << flush;
     endLine(stdError);
     stderr << argv.0 << ": error: " << msg << newline;
     if errors > 100 then (
	  stderr << "too many errors, exiting" << endl;
	  exit(1);
	  );
     );
export fatal(msg:string):exits := (
     stdIO << flush;
     endLine(stdError);
     stderr << argv.0 << ": fatal: " << msg << endl;
     exit(1));
export abort(msg:string):exits := (
     stdIO << flush;
     endLine(stdError);
     stderr << argv.0 << ": fatal: " << msg << endl;
     abort());
export syserr(msg:string):exits := (
     stdIO << flush;
     endLine(stdError);
     stderr << argv.0 << ": system error: " << msg;
     s := syserrmsg();
     if length(s) > 0 then stderr << ": " << s;
     stderr << endl;
     exit(1));
export warning(t:errmsg):void := warning(t.message);
export error(t:errmsg):void := error(t.message);
export fatal(t:errmsg):void := fatal(t.message);
export syserr(t:errmsg):void := syserr(t.message);
export (o:file) << (t:errmsg) : void := o << t.message;

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
-- End:
