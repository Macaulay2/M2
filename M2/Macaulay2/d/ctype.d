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
ALNUMEXTRA := 128;
HEX := 256;
BINARY := 512;
SPACE := WHITE | NEWLINE;
ALPHA := UPPER | LOWER;
ALNUM := ALPHA | DIGIT | ALNUMEXTRA;

foreach c in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"  do setchartype(c,UPPER);
foreach c in "abcdefghijklmnopqrstuvwxyz"  do setchartype(c,LOWER);
foreach c in "0123456789"      	           do setchartype(c,DIGIT);
foreach c in "0123456789abcdefABCDEF"      do setchartype(c,HEX);
foreach c in "01"                          do setchartype(c,BINARY);
foreach c in " \t\r"	                   do setchartype(c,WHITE);
foreach c in "\n"                          do setchartype(c,NEWLINE);
foreach c in "$'"                          do setchartype(c,ALNUMEXTRA);

for c from 128 to 255                      do setchartype(char(c),ALPHA);
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

-- c = two bytes concatenated
export ismathoperator(c:int):bool := (
    (c & 0xffe0) == 0xc2a0 || -- latin-1 punctuation/symbols
    c == 0xc397            || -- multiplication sign
    c == 0xc3b7            || -- division sign
    (c & 0xfffc) == 0xe288 || -- mathematical operators
    (c & 0xfffc) == 0xe2a8 || -- supplemental mathematical operators
    c == 0xe29f            || -- misc. mathematical symbols A
    (c & 0xfffe) == 0xe2a6 || -- misc. mathematical symbols B
    (c & 0xfffc) == 0xe28c    -- misc. technical
    );

ismathoperator(c1:char, c2:char):bool := (
    ismathoperator((int(uchar(c1)) << 8) | int(uchar(c2))));

export isvalidsymbol(s:string):bool := (
     if !isalpha(s.0) then return false;
     if ismathoperator(s.0, s.1) && length(s) == utf8charlength(s.0)
     then return true;
     for i from 0 to length(s) - 1 do (
	 if !isalnum(s.i) || ismathoperator(s.i, s.(i + 1))
	 then return false);
     true);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
-- End:
