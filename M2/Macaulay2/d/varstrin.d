--		Copyright 1994 by Daniel R. Grayson
use C;
use system;
use strings;
export varstring := {
     str:string,
     size:int
     };
export newvarstring(i:int):varstring := varstring(
     new string len i do provide ' ', 
     0);
needatleast(i:int,v:varstring):void := (
     if length(v.str) < i then (
     	  v.str = new string len 2*i do (
	       foreach c in v.str do provide c;
	       while true do provide ' '
	       );
     	  );
     );
export (v:varstring) << (c:char) : varstring := (
     needatleast(v.size + 1,v);
     v.str.(v.size) = c;
     v.size = v.size + 1;
     v
     );
export (v:varstring) << (s:string):varstring := (foreach c in s do v << c;v);
export tostring(v:varstring):string := (
     new string len v.size do foreach c in v.str do provide c
     );
export toreversestring(v:varstring):string := (
     new string len v.size do for i from v.size-1 to 0 by -1 do provide v.str.i
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
