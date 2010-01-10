--		Copyright 1994-2000 by Daniel R. Grayson

use C;
use system;
use stdio;
use strings;
use varstrin;
use ctype;

tokenbuf := newvarstring(100);
export getLine(o:file):(string or errmsg) := (
     ch := 0;
     while (
	  ch = getc(o);
	  if iserror(ch) then return (string or errmsg)(errmsg("failed to read file : "+syserrmsg()));
	  !(isnewline(ch) || iseof(ch))
	  )
     do (
	  tokenbuf << char(ch);
	  );
     (string or errmsg)(takestring(tokenbuf)));

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
