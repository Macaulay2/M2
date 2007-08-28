--		Copyright 1994 by Daniel R. Grayson

use C;
use system;

import join(x:string,y:string):string;
import substr(x:string,start:int,leng:int):string;
import substr(x:string,start:int):string;
import enlarge(n:int,x:string):string;

export concatenate(x:array(string)):string := (
     n := 0;
     foreach y in x do n = n + length(y);
     new string len n do foreach y in x do foreach z in y do provide z);
export between(s:string,x:array(string)):string := (
     if length(x) == 0
     then ""
     else if length(x) == 1 then x.0
     else concatenate(new array(string) len 2*length(x)-1 do foreach y in x do (provide y; provide s)));

export (s:string) + (t:string) : string := join(s,t);
export (s:string) + (c:char) : string := join(s,string(c));
export (c:char) + (t:string) : string := join(string(c),t);

-- numdigits(x:int):int := ( n:=0; while x>0 do ( n=n+1; x=x/10 ); n);
digit(x:int):char := if x<10 then '0' + x else 'a' + (x - 10);
export reverse(s:string):string := (
	n := length(s);
	new string len n do ( n = n-1 ; provide s.n));
export newstring(i:int):string := new string len i do provide ' ';
export (s:string) === (t:string) : bool := (
     if s==t then return true;
     if length(s) != length(t) then return false;
     foreach c at i in s do if c != t.i then return false;
     return true;
     );
export strchr(s:string,c:char):bool := (
     foreach b in s do if b == c then return true;
     false);

export (s:string) < (t:string) : bool := strcmp(s,t) == -1;
export (s:string) >= (t:string) : bool := !(s<t);
export (s:string) > (t:string) : bool := t<s;
export (s:string) <= (t:string) : bool := !(t<s);
export hash(s:string):int := (
     h := 0;
     foreach c in s do h = 31*h + int(c);
     h & 0x7fffffff
     );

export index(s:string,offset:int):int := (
     i := offset;
     while i < length(s) do if '\n' == s.i then return i else i=i+1;
     -1);     
export index(s:string,offset:int,c:char):int := (
     i := offset;
     while i < length(s) do if c == s.i then return i else i=i+1;
     -1);     
export match(s:string,i:int,t:string):bool := (
     m := length(s);
     n := length(t);
     j := 0;
     while i < m && j < n do if s.i != t.j then return false else (i=i+1; j=j+1);
     true);
export index(s:string,offset:int,sep:string):int := (
     i := offset;
     while i < length(s) do if match(s,i,sep) then return i else i=i+1;
     -1);
export index(s:string,offset:int,c:char,d:char):int := (
     i := offset;
     while i+1 < length(s) do if c == s.i && d==s.(i+1) then return i else i=i+1;
     -1);     

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
