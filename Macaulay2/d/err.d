--		Copyright 1994 by Daniel R. Grayson
use stdio;
use C;
use system;
use strings;
export usage(msg:string):void := ( 
     stderr << "usage: " << msg << endl ;
     exit(1));
warnings := 0;
export warning(msg:string):void := (
     warnings = warnings + 1;
     flush(stdout);
     stderr << argv.0 << ": warning: " << msg << endl;
     if warnings > 1000 then (
	  stderr << "too many warnings, exiting" << endl;
	  exit(1);
	  );
     );
errors := 0;
export error(msg:string):void := (
     errors = errors + 1;
     flush(stdout);
     stderr << argv.0 << ": error: " << msg << endl;
     if errors > 100 then (
	  stderr << "too many errors, exiting" << endl;
	  exit(1);
	  );
     );
export fatal(msg:string):void := (
     flush(stdout);
     stderr << argv.0 << ": fatal: " << msg << endl;
     exit(1);
     );
export abort(msg:string):void := (
     flush(stdout);
     stderr << argv.0 << ": fatal: " << msg << endl;
     abort();
     );
export syserr(msg:string):void := (
     flush(stdout);
     stderr << argv.0 << ": system error: " << msg;
     s := syserrmsg();
     if length(s) > 0 then stderr << ": " << s;
     stderr << endl;
     exit(1);
     );
export warning(t:errmsg):void := warning(t.message);
export error(t:errmsg):void := error(t.message);
export fatal(t:errmsg):void := fatal(t.message);
export syserr(t:errmsg):void := syserr(t.message);
export (o:file) << (t:errmsg) : void := o << t.message;

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d"
-- End:
