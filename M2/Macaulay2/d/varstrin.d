--		Copyright 1994 by Daniel R. Grayson
use strings;
export varstring := {
     str:string,
     width:int						    -- the number of bytes in str that are used; if greater than length(str), blank padding is understood
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
     if o.width < n then (
     	  m := n;
	  if m > length(o.str) then m = length(o.str);	    -- blank padding is understood
	  for i from o.width to m-1 do o.str.i = ' ';
	  o.width = n;
	  );
     );
export (v:varstring) << (c:char) : varstring := (
     n := v.width + 1;
     if n > length(v.str) then needatleast(n,v);
     v.str.(v.width) = c;
     v.width = n;
     v
     );
export (v:varstring) << (s:string):varstring := (
     m := length(s);
     k := v.width;
     n := k + m;
     if n > length(v.str) then needatleast(n,v);
     foreach c at i in s do v.str.(k + i) = c;
     v.width = n;
     v
     );
export tostring(v:varstring):string := new string len v.width do (
     foreach c in v.str do provide c;
     while true do provide ' ';
     );
export toreversestring(v:varstring):string := (
     new string len v.width do (
	  for v.width - length(v.str) do provide ' ';
	  for i from v.width-1 to 0 by -1 do provide v.str.i;
	  )
     );
export empty(v:varstring):void := v.width = 0;
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

export hash(s:varstring):int := (
     h := 0;
     n := s.width;
     if n > length(s.str) then n = length(s.str);
     for i from 0 to n-1 do h = 311*h + int(s.str.i);
     h & 0x7fffffff );
export putdigit(o:varstring,x:int):void := o << (x + if x<10 then '0' else 'a'-10) ;

export varstringarray := { a:array(string), n:int };
export newvarstringarray(m:int):varstringarray := varstringarray( new array(string) len m do provide "", 0 );
export append(v:varstringarray,s:string):void := (
     n := v.n;
     if length(v.a) == n then ( 
	  v.a = new array(string) len 2*n+1 do (
	       foreach t in v.a do provide t;
	       provide s;
	       while true do provide "";
	       );
	  v.n = n+1;)
     else (
     	  v.a.n = s;
     	  v.n = n+1;));
export extract(v:varstringarray):array(string) := new array(string) len v.n do foreach s in v.a do provide s;

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
-- End:
