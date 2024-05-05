--		Copyright 1994-2000 by Daniel R. Grayson
use tokens;
threadLocal tokenbuf := newvarstring(100);
export getLine(o:file):StringOrError := (
     ch := 0;
     while (
	  ch = getc(o);
	  if iserror(ch) then return StringOrError(errmsg("failed to read file : "+syserrmsg()));
	  !(isnewline(ch) || iseof(ch))
	  )
     do (
	  tokenbuf << char(ch);
	  );
     StringOrError(stringCell(takestring(tokenbuf))));

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d getline.o "
-- End:
