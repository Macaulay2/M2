--		Copyright 1994-2000 by Daniel R. Grayson

use system;
use stdio;
use varstrin;
use ctype;

tokenbuf := newvarstring(100);
export getline(o:file):string := (
     ch := 0;
     while (
	  ch = getc(o);
	  !(isnewline(ch) || ch == EOF)
	  )
     do (
	  tokenbuf << char(ch);
	  );
     takestring(tokenbuf));
