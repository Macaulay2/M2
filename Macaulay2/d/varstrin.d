--		Copyright 1994 by Daniel R. Grayson
use C;
use system;
use strings;
export varstring := {
     str:string,
     size:int						    -- the number of bytes in str that are used; if greater than length(str), blank padding is understood
     };
export newvarstring(n:int):varstring := (		    -- assume n > 0
     varstring( new string len n do provide ' ', 0)
     );
needatleast(n:int,v:varstring):void := (
     if length(v.str) < n then (
     	  v.str = new string len 2*n do (
	       foreach c in v.str do provide c;
	       while true do provide ' '
	       );
     	  );
     );
export blankfill(n:int,o:varstring):void := (		    -- add blanks if necessary to get a varstring of size at least n, lazily
     if o.size < n then (
     	  m := n;
	  if m > length(o.str) then m = length(o.str);	    -- blank padding is understood
	  for i from o.size to m-1 do o.str.i = ' ';
	  o.size = n;
	  );
     );
export (v:varstring) << (c:char) : varstring := (
     n := v.size + 1;
     if n > length(v.str) then needatleast(n,v);
     v.str.(v.size) = c;
     v.size = n;
     v
     );
export (v:varstring) << (s:string):varstring := (
     m := length(s);
     k := v.size;
     n := k + m;
     if n > length(v.str) then needatleast(n,v);
     foreach c at i in s do v.str.(k + i) = c;
     v.size = n;
     v
     );
export tostring(v:varstring):string := new string len v.size do (
     foreach c in v.str do provide c;
     while true do provide ' ';
     );
export toreversestring(v:varstring):string := (
     new string len v.size do (
	  for v.size - length(v.str) do provide ' ';
	  for i from v.size-1 to 0 by -1 do provide v.str.i;
	  )
     );
export empty(v:varstring):void := v.size = 0;
export takestring(v:varstring):string := (
     s := tostring(v);
     empty(v);
     s);
export takereversestring(v:varstring):string := (
     s := toreversestring(v);
     empty(v);
     s);
-- export addchar(v:varstring,c:char):void := v<<c;
-- export addstring(v:varstring,s:string):void := v<<s;

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d"
-- End:
