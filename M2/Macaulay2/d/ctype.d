--		Copyright 1994 by Daniel R. Grayson
use strings;
chartypes := new array(short) len 256 do provide short(0);
setchartype(c:char,t:int):void := chartypes.(int(uchar(c))) = short(t | int(chartypes.(int(uchar(c)))));
LOWER := 1;
UPPER := 2;
DIGIT := 4;
WHITE := 8;
NEWLINE := 16;
QUOTE := 32;
CTRL := 64;
DOLLAR := 128;
HEX := 256;
BINARY := 512;
SPACE := WHITE | NEWLINE;
ALPHA := UPPER | LOWER;
ALNUM := ALPHA | DIGIT | DOLLAR;

foreach c in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"  do setchartype(c,UPPER);
foreach c in "abcdefghijklmnopqrstuvwxyz"  do setchartype(c,LOWER);
foreach c in "0123456789"      	           do setchartype(c,DIGIT);
foreach c in "0123456789abcdefABCDEF"      do setchartype(c,HEX);
foreach c in "01"                          do setchartype(c,BINARY);
foreach c in " \t\r"	                   do setchartype(c,WHITE);
foreach c in "\n"                          do setchartype(c,NEWLINE);
foreach c in "$"                           do setchartype(c,DOLLAR);

for c from 128 to 225	       	    	   do setchartype(char(c),ALPHA);
for c from 227 to 255	       	    	   do setchartype(char(c),ALPHA);
					      setchartype('\'',ALPHA);
					      setchartype('\"',QUOTE);

chartype(c:int):int := if (c & ~255) == 0 then int(chartypes.c) else 0;
chartype(c:char):int := int(chartypes.(int(uchar(c))));

export isdigit    (c:int ):bool := (chartype(c) & DIGIT    ) != 0;
export ishex      (c:int ):bool := (chartype(c) & HEX      ) != 0;
export isbinary   (c:int ):bool := (chartype(c) & BINARY   ) != 0;
export isalpha    (c:int ):bool := (chartype(c) & ALPHA    ) != 0;
export isalnum    (c:int ):bool := (chartype(c) & ALNUM    ) != 0;
export iswhite    (c:int ):bool := (chartype(c) & WHITE    ) != 0;
export isspace    (c:int ):bool := (chartype(c) & SPACE    ) != 0;
export isnewline  (c:int ):bool := (chartype(c) & NEWLINE  ) != 0;
export isquote    (c:int ):bool := (chartype(c) & QUOTE    ) != 0;

export isdigit    (c:char):bool := (chartype(c) & DIGIT    ) != 0;
export ishex      (c:char):bool := (chartype(c) & HEX      ) != 0;
export isbinary   (c:char):bool := (chartype(c) & BINARY   ) != 0;
export isalpha    (c:char):bool := (chartype(c) & ALPHA    ) != 0;
export isalnum    (c:char):bool := (chartype(c) & ALNUM    ) != 0;
export iswhite    (c:char):bool := (chartype(c) & WHITE    ) != 0;
export isspace    (c:char):bool := (chartype(c) & SPACE    ) != 0;
export isnewline  (c:char):bool := (chartype(c) & NEWLINE  ) != 0;
export isquote    (c:char):bool := (chartype(c) & QUOTE    ) != 0;

export isalnum  (s:string):bool := (
     foreach c in s do if !isalnum(c) then return false;
     true);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
-- End:
