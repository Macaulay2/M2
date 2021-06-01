-- Copyright 1994 by Daniel R. Grayson

use getline;
use actors;
use actors2;
use struct;
use pthread;
use regex;

header "// required for toString routines
#include <engine.h>                         // for IM2_GB_to_string, rawMuta... // TODO: remove this one
#include <interface/NAG.h>                  // for rawHomotopyToString, rawP...
#include <interface/freemodule.h>           // for IM2_FreeModule_to_string
#include <interface/matrix.h>               // for IM2_Matrix_to_string
#include <interface/monoid.h>               // for IM2_Monoid_to_string
#include <interface/monomial-ordering.h>    // for IM2_MonomialOrdering_to_s...
#include <interface/mutable-matrix.h>       // for IM2_MutableMatrix_to_string
#include <interface/random.h>               // for system_randomint
#include <interface/ring.h>                 // for IM2_Ring_to_string
#include <interface/ringelement.h>          // for IM2_RingElement_to_string
#include <interface/ringmap.h>              // for IM2_RingMap_to_string";

header "
#ifndef WITH_PYTHON
#  define PyObject_Str(o)       0
#  define PyString_AS_STRING(o) 0
#  define Py_DECREF(o)          0
#endif";

internalName(s:string):string := (
     -- was "$" + s in 0.9.2
     s
     );

sleepfun(e:Expr):Expr := (
     when e
     is i:ZZcell do (
	  if isInt(i)
	  then toExpr(sleep(toInt(i)))
	  else WrongArgSmallInteger(1))
     else WrongArgZZ(1));
setupfun("sleep",sleepfun);

nanosleepfun(e:Expr):Expr := (
     when e
     is i:ZZcell do (
	  if isLong(i)
	  then toExpr(nanosleep(toLong(i)))
	  else WrongArgSmallInteger(1))
     else WrongArgZZ(1));
setupfun("nanosleep",nanosleepfun);

forkfun(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 0
	  then (
	       pid := fork();
	       toExpr(pid))
	  else WrongNumArgs(0))
     else WrongNumArgs(0));
setupfun("fork",forkfun);

getpidfun(e:Expr):Expr := (
     when e
     is o:file do toExpr(o.pid)
     is a:Sequence do (
	  if length(a) == 0
	  then toExpr(getpid())
	  else WrongNumArgs(0))
     else WrongNumArgs(0));
setupfun("processID",getpidfun);


getpgrpfun(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 0
	  then toExpr(getpgrp())
	  else WrongNumArgs(0))
     else WrongNumArgs(0));
setupfun("groupID",getpgrpfun);

setpgidfun(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2
	  then (
	       when a.0 is pid:ZZcell do
	       if !isInt(pid) then WrongArgSmallInteger(1) else
	       when a.1 is pgid:ZZcell do
	       if !isInt(pgid) then WrongArgSmallInteger(2) else (
		    r := setpgid(toInt(pid),toInt(pgid));
		    if r == ERROR
		    then buildErrorPacket("setGroupID: "+syserrmsg())
		    else nullE)
	       else WrongArgZZ(2)
	       else WrongArgZZ(1)
	       )
	  else WrongNumArgs(2))
     else WrongNumArgs(0));
setupfun("setGroupID",setpgidfun);

absfun(e:Expr):Expr := (
     when e
     is i:ZZcell do toExpr(abs(i.v))				    -- # typical value: abs, ZZ, ZZ
     is x:RRcell do toExpr(if sign(x.v) then -x.v else x.v)		    -- # typical value: abs, RR, RR
     is x:RRicell do toExpr(abs(x.v))            -- # typical value: abs, RRi, RRi
     is x:CCcell do toExpr(abs(x.v))				    -- # typical value: abs, CC, RR
     is r:QQcell do toExpr(abs(r.v))				    -- # typical value: abs, QQ, RR
     else WrongArg("a number, real or complex"));
setupfun("abs",absfun);

select(a:Sequence,f:Expr):Expr := (
     b := new array(bool) len length(a) do provide false;
     found := 0;
     foreach x at i in a do (
	  y := applyEE(f,x);
	  when y is err:Error do if err.message == breakMessage then return if err.value == dummyExpr then nullE else err.value else return y else nothing;
	  if y == True then (
	       b.i = true;
	       found = found + 1;
	       )
	  else if y != False then return buildErrorPacket("select: expected predicate to yield true or false");
	  );
     new Sequence len found do (
	  foreach p at i in b do if p then provide a.i));
foo := array(string)();
select(n:int,f:Expr):Expr := (
     b := new array(bool) len n do provide false;
     found := 0;
     for i from 0 to n-1 do (
	  y := applyEE(f,toExpr(i));
	  when y is err:Error do if err.message == breakMessage then return if err.value == dummyExpr then nullE else err.value else return y else nothing;
	  if y == True then (
	       b.i = true;
	       found = found + 1;
	       )
	  else if y != False then return buildErrorPacket("select: expected predicate to yield true or false");
	  );
     Expr(list(new Sequence len found do foreach p at i in b do if p then provide toExpr(i))));
select(e:Expr,f:Expr):Expr := (
     when e is obj:HashTable do (
	  if obj.Mutable then return WrongArg(0+1,"an immutable hash table");
	  u := newHashTable(obj.Class,obj.parent);
	  u.beingInitialized = true;
	  foreach bucket in obj.table do (
	       p := bucket;
	       while p != p.next do (
		    newvalue := applyEE(f,p.value);
		    when newvalue
		    is err:Error do if err.message == breakMessage then return if err.value == dummyExpr then nullE else err.value else return newvalue
		    else if newvalue == True
		    then (storeInHashTable(u,p.key,p.hash,p.value);)
	  	    else if newvalue != False then return buildErrorPacket("select: expected predicate to yield true or false");
		    p = p.next;
		    ));
	  Expr(sethash(u,obj.Mutable)))
     is a:Sequence do Expr(select(a,f))
     is b:List do (
	  c := select(b.v,f);
	  when c
	  is Error do c
	  is v:Sequence do list(b.Class,v)
	  else e			  -- shouldn't happen
	  )
     is n:ZZcell do (
	  if isInt(n)
	  then select(toInt(n),f)
	  else WrongArgSmallInteger(1)
	  )
     else WrongArg(0+1,"a list or a string"));
select(n:int,a:Sequence,f:Expr):Expr := (
     b := new array(bool) len length(a) do provide false;
     found := 0;
     foreach x at i in a do (
	  if found < n then (
	       y := applyEE(f,x);
	       when y is err:Error do if err.message == breakMessage then return if err.value == dummyExpr then nullE else err.value else return y else nothing;
	       if y == True then (
		    b.i = true;
		    found = found + 1;
		    )
	       else if y != False then return buildErrorPacket("select: expected predicate to yield true or false");
	       )
	  else b.i = false);
     new Sequence len found do (
	  foreach p at i in b do if p then provide a.i));
select(n:Expr,e:Expr,f:Expr,g:Expr,h:Expr):Expr := (
     when n
     is regexp:stringCell do (
	 when e is form:stringCell do
	 when f is text:stringCell do
	 when g is regexFlags:ZZcell do if !isInt(regexFlags) then WrongArgSmallInteger(4) else
	 when h is matchFlags:ZZcell do if !isInt(matchFlags) then WrongArgSmallInteger(5) else (
	     regexSelect(regexp.v, form.v, text.v, toInt(regexFlags), toInt(matchFlags))) -- see regex.dd
	 else WrongArgZZ(5)
	 else WrongArgZZ(4)
	 else WrongArgString(3)
	 else WrongArgString(2))
     is n:ZZcell do if isInt(n) then
     when e
     is obj:HashTable do (
	  if obj.Mutable then return WrongArg(1+1,"an immutable hash table");
	  u := newHashTable(obj.Class,obj.parent);
	  u.beingInitialized = true;
	  nval := toInt(n);
	  if nval > 0 then
	  foreach bucket in obj.table do (
	       p := bucket;
	       while nval > 0 && p != p.next do (
		    newvalue := applyEE(f,p.value);
		    when newvalue
		    is err:Error do if err.message == breakMessage then return if err.value == dummyExpr then nullE else err.value else return newvalue
		    else if newvalue == True
		    then (
			 storeInHashTable(u,p.key,p.hash,p.value);
			 nval = nval-1;
			 )
	  	    else if newvalue != False then return buildErrorPacket("select: expected predicate to yield true or false");
		    p = p.next;
		    ));
	  Expr(sethash(u,obj.Mutable)))
     is a:Sequence do Expr(select(toInt(n),a,f))
     is b:List do (
	  c := select(toInt(n),b.v,f);
	  when c
	  is Error do c
	  is v:Sequence do list(b.Class,v)
	  else e			  -- shouldn't happen
	  )
     else WrongArg(1+1,"a list")
     else WrongArg(0+1,"an integer or string")
     else WrongArgZZ(0+1));
select(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a) == 2 then select(a.0,a.1) else
     if length(a) == 3 then select(a.0,a.1,a.2,toExpr(defaultRegexFlags),toExpr(defaultMatchFlags)) else
     if length(a) == 5 then select(a.0,a.1,a.2,a.3,a.4)
     else WrongNumArgs(2,5)
     else WrongNumArgs(2,5));
setupfun("select", select).Protected = false; -- will be overloaded in m2/lists.m2 and m2/regex.m2

any(f:Expr,n:int):Expr := (
     for i from 0 to n-1 do (
	  v := applyEE(f,toExpr(i));
	  when v is err:Error do if err.message == breakMessage then return if err.value == dummyExpr then nullE else err.value else return v else nothing;
	  if v == True then return True;
	  if v != False then return buildErrorPacket("any: expected true or false");
	  );
     False);
any(f:Expr,obj:HashTable):Expr := (
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == p.next then break;
	       v := applyEEE(f,p.key,p.value);
	       when v is err:Error do if err.message == breakMessage then return if err.value == dummyExpr then nullE else err.value else return v else nothing;
	       if v == True then return True;
	       if v != False then return buildErrorPacket("any: expected true or false");
	       p = p.next;
	       ));
     False);
any(f:Expr,a:Sequence):Expr := (
     foreach x at i in a do (
	  y := applyEE(f,x);
	  when y is err:Error do if err.message == breakMessage then return if err.value == dummyExpr then nullE else err.value else return y else nothing;
	  if y == True then return True;
	  if y != False then return buildErrorPacket("any: expected true or false");
	  );
     False);
any(f:Expr,e:Expr):Expr := (
     when e
     is a:Sequence do Expr(any(f,a))
     is b:List do Expr(any(f,b.v))
     is i:ZZcell do if isInt(i) then Expr(any(f,toInt(i))) else WrongArgSmallInteger(1)
     is c:HashTable do
     if c.Mutable then WrongArg(1,"an immutable hash table") else
     Expr(any(f,c))
     else WrongArg("a list or a hash table"));
any(f:Expr,a:Sequence,b:Sequence):Expr := (
     if length(a) != length(b) then return buildErrorPacket("expected lists of the same length");
     foreach x at i in a do (
	  y := applyEEE(f,x,b.i);
	  when y is err:Error do if err.message == breakMessage then return if err.value == dummyExpr then nullE else err.value else return y else nothing;
	  if y == True then return True;
	  if y != False then return buildErrorPacket("any: expected true or false");
	  );
     False);
any(f:Expr,a:Sequence,y:Expr):Expr := (
     when y
     is c:Sequence do Expr(any(f,a,c))
     is d:List do Expr(any(f,a,d.v))
     else WrongArg("a basic list or sequence"));
any(f:Expr,x:Expr,y:Expr):Expr := (
     when x
     is a:Sequence do Expr(any(f,a,y))
     is b:List do Expr(any(f,b.v,y))
     else WrongArg("a basic list or sequence"));
any(e:Expr):Expr := (
     when e is a:Sequence do (
	  if length(a) == 2 then any(a.1,a.0)
	  else if length(a) == 3 then any(a.2,a.0,a.1)
	  else WrongNumArgs(2,3))
     else WrongNumArgs(2));
setupfun("any",any);

--find(f:Expr,obj:HashTable):Expr := (
--     foreach bucket in obj.table do (
--	  p := bucket;
--	  while true do (
--	       if p == bucketEnd then break;
--	       r := apply(f,p.key,p.value);
--	       if r != nullE then return r;
--	       p = p.next;
--	       ));
--     nullE);
--find(f:Expr,a:Sequence):Expr := (
--     i := 0;
--     while i < length(a) do (
--	  r := apply(f,a.i);
--	  if r != nullE then return r;
--	  i = i+1;
--	  );
--     nullE);
--find(e:Expr):Expr := (
--     when e is a:Sequence do (
--	  if length(a) != 2
--	  then WrongNumArgs(2)
--	  else (
--	       f := a.1;
--	       x := a.0;
--	       when x
--	       is a:Sequence do Expr(find(f,a))
--	       is b:List do Expr(find(f,b.v))
--	       is c:HashTable do
--	       if c.Mutable then WrongArg(1,"an immutable hash table") else
--	       Expr(find(f,c))
--	       else WrongArg(1+1,"a list")))
--     else WrongNumArgs(2));
--setupfun("find",find);

characters(e:Expr):Expr := (
     when e
     is s:stringCell do list(
	  new Sequence len length(s.v) do (
	       foreach c in s.v do provide chars.(int(uchar(c)))))
     else buildErrorPacket("expects a string"));
setupfun("characters",characters);

ascii(e:Expr):Expr := (
     if isIntArray(e)
     then toExpr((
	  v := toIntArray(e);
	  new string len length(v) do foreach i in v do provide char(i)))
     else
     when e is s:stringCell do list(
	  new Sequence len length(s.v) do (
	       foreach c in s.v do provide toExpr(int(uchar(c)))))
     is i:ZZcell do (
	  if isInt(i)
	  then toExpr(new string len 1 do provide char(toInt(i)))
	  else WrongArgSmallInteger(1))
     else buildErrorPacket("expects a string, a small integer, or an array of small integers"));
setupfun("ascii",ascii);

utf8(v:array(int)):Expr := (
     w := newvarstring(length(v)+10);
     foreach i in v do (
	  if (i &     ~0x7f) == 0 then w << char(i) else
	  if (i &    ~0x7ff) == 0 then w << char(0xc0 | (i >> 6)) << char(0x80 | (i & 0x3f)) else
	  if (i &   ~0xffff) == 0 then w << char(0xe0 | (i >> 12)) << char(0x80 | ((i >> 6) & 0x3f)) << char(0x80 | (i & 0x3f)) else
	  if (i & ~0x1fffff) == 0 then w << char(0xf0 | (i >> 18)) << char(0x80 | ((i >> 12) & 0x3f)) << char(0x80 | ((i >> 6) & 0x3f)) << char(0x80 | (i & 0x3f))
	  else ( return buildErrorPacket("encountered integer too large for utf-8 encoding"); w));
     toExpr(takestring(w)));

erru():Expr := buildErrorPacket("string ended unexpectedly during utf-8 decoding");
errb():Expr := buildErrorPacket("unexpected byte encountered in utf-8 decoding");
utf8(y:Expr):Expr := (
     if isIntArray(y) then utf8(toIntArray(y))
     else when y is ss:stringCell do (
	  s := ss.v;
	  n := length(s);
	  x := newvararrayint(n+10);
	  i := 0;
	  while i < n do (
	       c := int(uchar(s.i));
	       if (c & 0x80) == 0 then (
		    x << c;
		    i = i+1;
		    )
	       else if (c & 0xe0) == 0xc0 then (
		    if i+1 < n then (
		    	 d := int(uchar(s.(i+1)));
			 if (d & 0xc0) != 0x80 then return errb();
		    	 x << (((c & ~0xe0) << 6) | (d & ~0xc0));
			 i = i+2;
	       		 )
		    else return erru();
		    )
	       else if (c & 0xf0) == 0xe0 then (
		    if i+2 < n then (
		    	 d := int(uchar(s.(i+1)));
			 if (d & 0xc0) != 0x80 then return errb();
		    	 e := int(uchar(s.(i+2)));
			 if (e & 0xc0) != 0x80 then return errb();
		    	 x << (((c & ~0xf0) << 12) | ((d & ~0xc0) << 6) | (e & ~0xc0));
			 i = i+3;
			 )
		    else return erru();
		    )
	       else if (c & 0xf8) == 0xf0 then (
		    if i+3 < n then (
		    	 d := int(uchar(s.(i+1)));
			 if (d & 0xc0) != 0x80 then return errb();
		    	 e := int(uchar(s.(i+2)));
			 if (e & 0xc0) != 0x80 then return errb();
		    	 f := int(uchar(s.(i+3)));
			 if (f & 0xc0) != 0x80 then return errb();
		    	 x << (((c & ~0xf8) << 18) | ((d & ~0xc0) << 12) | ((e & ~0xc0) << 6) | (f & ~0xc0));
			 i = i+4;
			 )
		    else return erru();
		    )
	       else return errb();
	       );
	  a := takearrayint(x);
     	  Expr(list(new Sequence len length(a) do foreach i in a do provide toExpr(i))))
     is i:ZZcell do (
	  if !isInt(i) then return WrongArgSmallInteger();
	  Expr(utf8(array(int)(toInt(i)))))
     else buildErrorPacket("expects a string, a small integer, or an array of small integers"));
setupfun("utf8",utf8);

errucheck():Expr := buildErrorPacket("string ended unexpectedly during utf-8 checking");
errbcheck():Expr := buildErrorPacket("unexpected byte encountered in utf-8 checking");
utf8check(y:Expr):Expr := (
     when y is ss:stringCell do (
	  s := ss.v;
	  n := length(s);
	  i := 0;
	  while i < n do (
	       c := int(uchar(s.i));
	       if (c & 0x80) == 0 then (
		    i = i+1;
		    )
	       else if (c & 0xe0) == 0xc0 then (
		    if i+1 < n then (
		    	 d := int(uchar(s.(i+1)));
			 if (d & 0xc0) != 0x80 then return errbcheck();
			 i = i+2;
	       		 )
		    else return errucheck();
		    )
	       else if (c & 0xf0) == 0xe0 then (
		    if i+2 < n then (
		    	 d := int(uchar(s.(i+1)));
			 if (d & 0xc0) != 0x80 then return errbcheck();
		    	 e := int(uchar(s.(i+2)));
			 if (e & 0xc0) != 0x80 then return errbcheck();
			 i = i+3;
			 )
		    else return errucheck();
		    )
	       else if (c & 0xf8) == 0xf0 then (
		    if i+3 < n then (
		    	 d := int(uchar(s.(i+1)));
			 if (d & 0xc0) != 0x80 then return errbcheck();
		    	 e := int(uchar(s.(i+2)));
			 if (e & 0xc0) != 0x80 then return errbcheck();
		    	 f := int(uchar(s.(i+3)));
			 if (f & 0xc0) != 0x80 then return errbcheck();
			 i = i+4;
			 )
		    else return errucheck();
		    )
	       else return errbcheck();
	       );
     	  nullE)
     else buildErrorPacket("expects a string"));
setupfun("utf8check",utf8check);

export checknoargs(e:Expr):Expr := (
     when e
     is v:Sequence do (
	  if length(v) == 0 then e else WrongNumArgs(0)
	  )
     else WrongNumArgs(0)
     );

randomint(e:Expr):Expr := (
     when checknoargs(e) is f:Error do return Expr(f) else nothing;
     toExpr(randomint()));
setupfun("randomint",randomint);

outseq(x:file,y:Sequence):Expr := (
     foreach z in y do (
	  when z
	  is w:List do (outseq(x,w.v);)
	  is Nothing do nothing
	  is y:SymbolClosure do (
	       x << if y.frame == globalFrame
	       then y.symbol.word.name
	       else internalName(y.symbol.word.name);
	       )
	  is y:ZZcell do (
	       if isInt(y)
	       then for toInt(y) do x << ' '
	       else return WrongArg(2,"a string, or list or sequence of strings and small integers, or null"))
	  is w:Sequence do (outseq(x,w);)
	  is s:stringCell do (x << s.v;)
	  else return WrongArg(2,"a string, or list or sequence of strings and integers, or null"));
     Expr(x));
outstringfun(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then (
	       when a.0
	       is x:file do (
		    when a.1
		    is y:stringCell do Expr(x << y.v)
		    is Nothing do Expr(x)
		    is y:SymbolClosure do (
			 x << (
			      if y.frame == globalFrame
	  		      then y.symbol.word.name
	  		      else internalName(y.symbol.word.name)
			      );
			 Expr(x)
			 )
		    is y:List do outseq(x,y.v)
		    is n:Net do Expr(x << n)
		    is y:ZZcell do (
			 if isInt(y)
			 then (
			      for toInt(y) do x << ' ';
			      Expr(x))
			 else WrongArg(2,"a sequence or list of strings, nets, or nulls"))
		    is y:Sequence do outseq(x,y)
		    else WrongArg(2,"a sequence or list of strings, nets, or nulls"))
	       else WrongArg(1,"a file"))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
setupfun("printString",outstringfun);

lenseq(y:Sequence):int := (
     l := 0;
     foreach z in y do (
	  when z
	  is w:List do (
	       m := lenseq(w.v);
	       if m == -1 then return -1;
	       l = l + m;
	       )
	  is Nothing do nothing
	  is y:SymbolClosure do l = l + length(y.symbol.word.name)
	  is y:ZZcell do (
	       if isInt(y)
	       then (if toInt(y)>0 then l = l + toInt(y);)
	       else return -1)
	  is w:Sequence do (
	       m := lenseq(w);
	       if m == -1 then return -1;
	       l = l + m;
	       )
	  is s:stringCell do (
	       l = l + length(s.v);
	       )
	  else return -1);
     l);
stringlenfun(e:Expr):Expr := (
     when e
     is y:stringCell do toExpr(length(y.v))
     is Nothing do zeroE
     is y:SymbolClosure do toExpr(length(y.symbol.word.name))
     is y:List do (
	  l := lenseq(y.v);
	  if l == -1
	  then WrongArg(1,"a string, or list or sequence of strings and integers, or null")
	  else toExpr(l)
	  )
     is y:ZZcell do e
     is y:Sequence do (
	  l := lenseq(y);
	  if l == -1
	  then WrongArg(1,"a string, or list or sequence of strings and integers, or null")
	  else toExpr(l)
	  )
     else WrongArg(1,"a string, or list or sequence of strings and integers, or null"));
setupfun("stringlen",stringlenfun);

stringcat2(a:Sequence,s:string,i:int):int := ( -- returns next available index
     foreach x in a do (
	  when x
	  is a:Sequence do i = stringcat2(a,s,i)
     	  is y:SymbolClosure do foreach c in y.symbol.word.name do (s.i = c; i = i+1;)
	  is a:List do i = stringcat2(a.v,s,i)
	  is n:ZZcell do for toInt(n) do (s.i = ' '; i = i+1;)
	  is t:stringCell do foreach c in t.v do (s.i = c; i = i+1;)
	  else nothing;
	  );
     i);
export stringcatseq(a:Sequence):Expr := (
     l := lenseq(a);
     if l == -1
     then WrongArg("strings, integers, or symbols")
     else (
	  s := new string len l do provide ' ';
	  stringcat2(a,s,0);
	  toExpr(s)));
stringcatfun(e:Expr):Expr := (
     when e
     is a:Sequence do stringcatseq(a)
     is a:List do stringcatseq(a.v)
     is Nothing do emptyString
     is y:SymbolClosure do toExpr(y.symbol.word.name)
     is n:ZZcell do (
	  if isInt(n) then (
	       m := max(toInt(n), 0);
	       toExpr(new string len m do provide ' ')
	       )
	  else buildErrorPacket("encountered a large integer")
	  )
     is stringCell do e
     else WrongArg("a sequence or list of strings, integers, or symbols"));
setupfun("concatenate",stringcatfun);

errorfun(e:Expr):Expr := (
     e = stringcatfun(e);
     when e
     is s:stringCell do buildErrorPacket(s.v)
     else buildErrorPacket("expects a string or sequence of strings as its argument"));
setupfun("error",errorfun).Protected = false;		    -- this will be replaced by a toplevel function that calls this one

mingleseq(a:Sequence):Expr := (
     n := length(a);
     b := new array(Sequence) len n do provide emptySequence;
     newlen := 0;
     for i from 0 to n-1 do (
	  when a.i
	  is d:Sequence do ( newlen = newlen + length(d); b.i = d; )
	  is d:List do ( newlen = newlen + length(d.v); b.i = d.v; )
	  else return WrongArg(i+1,"a list or sequence"));
     list ( new Sequence len newlen do
     	  for j from 0 to newlen-1 do for i from 0 to n-1 do
     	  if j < length(b.i) then provide b.i.j));
minglefun(e:Expr):Expr := (
     when e
     is a:Sequence do mingleseq(a)
     is a:List do mingleseq(a.v)
     is Error do e
     else WrongArg("a list or sequence"));
setupfun("mingle", minglefun);

packlist(v:Sequence,n:int):Expr := (
     d := length(v);
     i := 0;
     list(new Sequence len (d + n - 1) / n do
     	  provide list(
	       new Sequence len if n < d-i then n else d-i do (
		    j := i;
		    i = i+1;
	       	    provide v.j))));
packfun(e:Expr):Expr := (
     when e
     is a:Sequence do (
     	  if length(a) == 2 then (
	       when a.0
	       is n:ZZcell do (
		    if isInt(n)
		    then (
			 nn := toInt(n);
			 if nn > 0 then (
			      when a.1
			      is x:Sequence do packlist(x,nn)
			      is x:List do packlist(x.v,nn)
			      else WrongArg(1,"a list or sequence")
			      )
			 else if nn == 0 then (
			      when a.1
			      is x:Sequence do if length(x) == 0 then emptyList else WrongArg(1,"a positive integer")
			      is x:List do if length(x.v) == 0 then emptyList else WrongArg(1,"a positive integer")
			      else WrongArg(1,"a list or sequence")
			      )
			 else WrongArg(1,"a positive integer")
			 )
		    else WrongArgSmallInteger(1)
		    )
	       is x:Sequence do (
		    when a.1
		    is n:ZZcell do (
			 if isInt(n)
			 then (
			      nn := toInt(n);
			      if nn > 0
			      then packlist(x,nn)
			      else WrongArg(2,"a positive integer"))
			 else WrongArgSmallInteger(2))
		    else WrongArgZZ(2))
	       is x:List do (
		    when a.1
		    is n:ZZcell do (
			 if isInt(n)
			 then (
			      nn := toInt(n);
			      if nn > 0
			      then packlist(x.v,nn)
			      else WrongArg(2,"a positive integer"))
			 else WrongArgSmallInteger(2))
		    else WrongArgZZ(2))
	       else WrongArg(1,"a list or sequence"))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
setupfun("pack", packfun);

getenvfun(e:Expr):Expr := (
     when e
     is s:stringCell do toExpr(getenv(s.v))
     else WrongArgString());
setupfun("getenv",getenvfun);

getfun(e:Expr):Expr := (
     when e
     is f:file do (
	  if f.infd == -1
	  then WrongArg("an open input file")
	  else (
	       when readfile(f.infd)
	       is s:string do (
		    stat := closeIn(f); -- the user may close the output side of a pipe first, so we can find out now if the process exited normally
		    when stat is m:errmsg do buildErrorPacket(m.message)
		    else toExpr(s))
	       else buildErrorPacket("unable to read file: "+syserrmsg())))
     is filename:stringCell do (
	  when get(filename.v)
	  is e:errmsg do buildErrorPacket(e.message)
	  is s:stringCell do toExpr(s.v))
     else WrongArg("a string as filename"));
setupfun("get",getfun);

readprompt := "";
readpromptfun():string := readprompt;

isReadyFun(e:Expr):Expr := (
     when e
     -- # typical value: isReady, Task, Boolean
     is tc:TaskCell do toExpr(taskDone(tc.body.task) && !tc.body.resultRetrieved)
     -- # typical value: isReady, File, Boolean
     is f:file do toExpr (
	  f.input && !f.eof && ( f.insize > f.inindex || isReady(f.infd) > 0 )
	  ||
	  f.listener && (
	       f.connection != -1
	       ||
	       ( sd := acceptNonblocking(f.listenerfd); f.connection = sd; sd != -1 )
	       )
	  )
     else WrongArg("a file or a task"));
setupfun("isReady",isReadyFun);


isCanceledFun(e:Expr):Expr := (
     when e
     -- # typical value: isCanceled, Task, Boolean
     is tc:TaskCell do toExpr(!taskRunning(tc.body.task) && !taskKeepRunning(tc.body.task))
     else WrongArg("a task"));
setupfun("isCanceled",isCanceledFun);


atEOFfun(e:Expr):Expr := (
     when e
     is f:file do toExpr ( !f.input || f.eof || f.insize == f.inindex && isReady(f.infd) > 0
	  && 0 == filbuf(f) )
     else WrongArg("a file"));
setupfun("atEndOfFile",atEOFfun);

allInputFilesOrListeners(s:Sequence):bool := (
     foreach f in s do (
     	  when f is g:file do ( if !g.input && !g.listener then return false; )
     	  else return false;
	  );
     true);
allIntegers(s:Sequence):bool := (
     foreach f in s do (
     	  when f is g:ZZcell do ( if !isInt(g) || toInt(g) < 0 then return false; )
     	  else return false;
	  );
     true);
numberReadyOnes(s:Sequence):int := (
     n := 0;
     foreach f in s do (
     	  when f is g:file do ( if g.input && (g.insize > g.inindex) then n = n+1; )
     	  else nothing;
	  );
     n);
readyOnes(s:Sequence):array(int) := (
     new array(int) len numberReadyOnes(s) do
     foreach f at i in s do
     when f is g:file do ( if g.input && (g.insize > g.inindex) then provide i; )
     else nothing
     );
fdlist(s:Sequence):array(int) := (
     new array(int) len length(s) do
     foreach f in s do
     when f is g:file
     do provide if g.input then g.infd else if g.listener then g.listenerfd else if g.output then g.outfd else -1
     else nothing
     );
wait(s:Sequence):Expr := (
     if allIntegers(s) then (
	  stats := waitNoHang(new array(int) len length(s) do foreach pid in s do provide toInt(pid));
	  Expr(list(new Sequence len length(s) do foreach status in stats do provide toExpr(status))))
     else if !allInputFilesOrListeners(s) then WrongArg("a list of input files or listeners, or a list of small non-negative integers")
     else if numberReadyOnes(s) > 0 then list(toArrayExpr(readyOnes(s)))
     else list(toArrayExpr(select(fdlist(s))))
     );
wait(f:file):Expr := (
     if !f.input then WrongArg("an input file or list of input files")
     else if f.insize > f.inindex then nullE
     else ( filbuf(f); nullE ));
wait(e:Expr):Expr := (
     when e
     is f:List do wait(f.v)
     is v:Sequence do wait(v)
     is f:file do wait(f)
     is x:ZZcell do (
	  if isInt(x) then (
	       ret := wait(toInt(x));
	       if ret == ERROR
	       then buildErrorPacket("wait failed")
	       else toExpr(ret)
	       )
	  else WrongArgSmallInteger()
	  )
     else WrongArg("an integer, or an input file or list of input files")
     );
setupfun("wait",wait);

readE(f:file):Expr := (
     when read(f)
     is e:errmsg do buildErrorPacket(e.message)
     is s:stringCell do Expr(s));

readfun(e:Expr):Expr := (
     when e
     is f:file do readE(f)
     is p:stringCell do (
	  readprompt = p.v;
	  oldprompt := stdIO.prompt;
	  stdIO.prompt = readpromptfun;
	  r := getLine(stdIO);
	  stdIO.prompt = oldprompt;
	  when r
	  is e:errmsg do buildErrorPacket(e.message)
	  is s:stringCell do Expr(s)
	  )
     is s:Sequence do (
	  if length(s) == 0
	  then readE(stdIO)
	  else if length(s) == 2
	  then (
	       when s.0
	       is f:file do (
		    if ! f.input
		    then return WrongArg(1,"expected an input file");
		    when s.1
		    is n:ZZcell do (
			 if isInt(n)
			 then (
			      nn := toInt(n);
			      if nn < 0
			      then return WrongArg(2,"a positive integer");
			      if f.inindex < f.insize
			      then (
				   nn = min(nn,f.insize - f.inindex);
				   o := f.inindex;
				   f.inindex = f.inindex + nn;
				   toExpr(new string len nn do for i from 0 to nn-1 do provide f.inbuffer.(o+i))
				   )
			      else (
				   buf := new string len nn do provide ' ';
				   r := read(f.infd,buf,nn);
				   toExpr(new string len r do for i from 0 to r-1 do provide buf.i)
				   )
			      )
			 else WrongArgSmallInteger(2)
			 )
		    else WrongArgZZ(2)
		    )
	       else WrongArg(1,"a file")
	       )
	  else WrongNumArgs(0,1))
     else WrongArg(1,"a file or a string"));
setupfun("read",readfun);

substrfun(e:Expr):Expr := (
     when e is args:Sequence do
     if length(args) == 3 then
     when args.0
     is i:ZZcell do if !isInt(i) then WrongArgSmallInteger(1) else (
	  when args.1 is j:ZZcell do if !isInt(j) then WrongArgSmallInteger(2) else
	  when args.2 is s:stringCell do toExpr(substr(s.v,toInt(i),toInt(j)))
	  else WrongArgString(3)
	  else WrongArgZZ(2))
     is s:stringCell do (
	  when args.1 is i:ZZcell do if !isInt(i) then WrongArgSmallInteger(2) else
	  when args.2 is j:ZZcell do if !isInt(j) then WrongArgSmallInteger(3) else toExpr(substr(s.v,toInt(i),toInt(j)))
	  else WrongArgZZ(3)
	  else WrongArgZZ(2))
     else WrongArg(1,"a string or an integer")
     else if length(args) == 2 then
     when args.0
     is s:stringCell do (
	  when args.1
	  is i:ZZcell do if !isInt(i) then WrongArgSmallInteger(2) else toExpr(substr(s.v,toInt(i)))
	  else WrongArgZZ(2))
     is i:ZZcell do if !isInt(i) then WrongArgSmallInteger(1) else (
	  when args.1
	  is s:stringCell do toExpr(substr(s.v,toInt(i)))
	  else WrongArgString(2))
     is pair:Sequence do (
	  if length(pair) != 2 then WrongArg(1,"a sequence of length 2") else
	  when pair.0 is i:ZZcell do if !isInt(i) then WrongArg(1,"a pair of small integers") else (
	       when pair.1 is j:ZZcell do if !isInt(j) then WrongArg(1,"a pair of small integers") else
	       when args.1 is s:stringCell do toExpr(substr(s.v,toInt(i),toInt(j)))
	       else WrongArgString(2)
	       else WrongArg(1,"a pair of integers"))
	  else WrongArg(1,"a pair of integers"))
     else WrongArg(1,"a string, an integer, or a pair of integers")
     else WrongNumArgs(2,3)
     else WrongNumArgs(2,3));
setupfun("substring",substrfun);

tostring(n:MysqlConnection):string := tostring(Ccode(constcharstarOrNull, "
     #if WITH_MYSQL
       mysql_get_host_info(", n, ")
     #else
       \"not present\"
     #endif
"));

tostring(m:MysqlConnectionWrapper):string := (
     "<<MysqlConnection : " + ( when m.mysql is null do "closed" is n:MysqlConnection do tostring(n) ) + ">>"
     );

tostringfun(e:Expr):Expr := (
     when e
     is i:ZZcell do toExpr(tostring(i.v))
     is x:QQcell do toExpr(tostring(x.v))
     is stringCell do e
     is q:SymbolClosure do toExpr( if q.frame == globalFrame then q.symbol.word.name else internalName(q.symbol.word.name) )
     is f:file do toExpr(f.filename)
     is b:Boolean do toExpr(if b.v then "true" else "false")
     is Nothing do toExpr("null")
     is f:Database do toExpr(f.filename)
     is m:MysqlConnectionWrapper do toExpr(tostring(m))
     is res:MysqlResultWrapper do toExpr(
	  "<<MysqlResult : "
	  + tostring(Ccode(int, "\n # if WITH_MYSQL \n mysql_num_rows(", res.res, ") \n #else \n 0 \n #endif \n"))
	  + " by "
	  + tostring(Ccode(int, "\n # if WITH_MYSQL \n mysql_num_fields(", res.res, ") \n #else \n 0 \n #endif \n"))
	  + ">>")
     is fld:MysqlFieldWrapper do toExpr(
	  "<<MysqlField : "
	  + tostring(Ccode(constcharstarOrNull,"(\n #if WITH_MYSQL \n (", fld.fld, ")->name \n #else \n \"\" \n #endif \n )"))
	  + " : "
	  + tostring(Ccode(int,"\n # if WITH_MYSQL \n (", fld.fld, ")->type \n #else \n 0 \n #endif \n"))
	  + ">>")
     is Net do toExpr("<<net>>")
     is CodeClosure do toExpr("<<pseudocode>>")
     is functionCode do toExpr("<<a function body>>")
     is CompiledFunction do toExpr("<<a compiled function>>")
     is CompiledFunctionClosure do toExpr("<<a compiled function closure>>")
     is CompiledFunctionBody do toExpr("<<a compiled function body>>")
     is x:SymbolBody do toExpr(x.symbol.word.name)
     is FunctionClosure do toExpr("<<a function closure>>")
     is DictionaryClosure do toExpr("<<a dictionary>>")
     is NetFile do toExpr("<<a netfile>>")
     is x:RRcell do toExpr(tostringRR(x.v))
     is x:RRicell do toExpr(tostringRRi(x.v))
     is z:CCcell do toExpr(tostringCC(z.v))
     is Error do toExpr("<<an error message>>")
     is Sequence do toExpr("<<a sequence>>")
     is HashTable do toExpr("<<a hash table>>")
     is List do toExpr("<<a list>>")
     is s:SpecialExpr do tostringfun(s.e)
     is x:RawMonomialCell do toExpr(tostring(x.p))
     is x:RawFreeModuleCell do toExpr(Ccode(string, "IM2_FreeModule_to_string(",x.p,")" ))
     is x:RawMatrixCell do toExpr(Ccode(string, "IM2_Matrix_to_string(",x.p,")" ))
     is x:RawMutableMatrixCell do toExpr(Ccode(string, "IM2_MutableMatrix_to_string(",x.p,")" ))
     is x:RawMutableComplexCell do toExpr(Ccode(string, "rawMutableComplexToString(",x.p,")" ))
     -- NAG stuff begin
     is x:RawHomotopyCell do toExpr(Ccode(string, "rawHomotopyToString(",x.p,")" ))
     is x:RawSLEvaluatorCell do toExpr(Ccode(string, "rawSLEvaluatorToString(",x.p,")" ))
     is x:RawSLProgramCell do toExpr(Ccode(string, "rawSLProgramToString(",x.p,")" ))
     is x:RawStraightLineProgramCell do toExpr(Ccode(string, "rawStraightLineProgramToString(",x.p,")" ))
     is x:RawPathTrackerCell do toExpr(Ccode(string, "rawPathTrackerToString(",x.p,")" ))
     is x:RawPointArrayCell do toExpr(Ccode(string, "rawPointArrayToString(",x.p,")" ))
     -- NAG stuff end
     is x:RawRingMapCell do toExpr(Ccode(string, "IM2_RingMap_to_string(",x.p,")" ))
     is x:RawMonomialOrderingCell do toExpr(Ccode(string, "IM2_MonomialOrdering_to_string(",x.p,")" ))
     is x:RawMonoidCell do toExpr(Ccode(string, "IM2_Monoid_to_string(",x.p,")" ))
     is x:RawRingCell do toExpr(Ccode(string, "IM2_Ring_to_string(",x.p,")" ))
     is x:RawRingElementCell do toExpr( Ccode(string, "IM2_RingElement_to_string(",x.p,")" ) )
     is x:RawMonomialIdealCell do toExpr(
	  "<<raw monomial ideal>>"
	  -- Ccode(string, "IM2_MonomialIdeal_to_string(",x.p,")" )
	  )
     is c:RawComputationCell do toExpr(Ccode(string, "IM2_GB_to_string(",c.p,")" ))
     is po:pythonObjectCell do (
	  -- Expr("<<a python object>>")
	  str := Ccode(pythonObject,"PyObject_Str(",po.v,")");
	  r := toExpr(tostring(Ccode(constcharstarOrNull,"PyString_AS_STRING(",str,")")));
	  Ccode(void,"Py_DECREF(",str,")");
	  r)
     is x:xmlNodeCell do toExpr(toString(x.v))
     is xmlAttrCell do toExpr("<<libxml attribute>>")
     is x:TaskCell do (
--	  while !isInitialized(x) do nothing;
	  toExpr(
	       "<<task, "
	       + (
		    if x.body.resultRetrieved then "result delivered, task terminated"
		    else if taskDone(x.body.task) then "result available, task done"
		    else if taskRunning(x.body.task) && !taskKeepRunning(x.body.task) then "running, cancellation requested"
		    else if !taskKeepRunning(x.body.task) then "canceled"
		    else if !taskStarted(x.body.task) then "created"
		    else "running"
		    )
	       + ">>"
	       ))
    is x:fileOutputSyncState do toExpr("File Output Sync State")
);
setupfun("simpleToString",tostringfun);

connectionCount(e:Expr):Expr := (
     when e is f:file do if f.listener then toExpr(f.numconns)
     else WrongArg(1,"an open socket listening for connections")
     else WrongArg(1,"a file")
     );
setupfun("connectionCount", connectionCount);

format(e:Expr):Expr := (
     when e
     is s:stringCell do toExpr("\"" + present(s.v) + "\"")
     is RRcell do format(Expr(Sequence(e)))
     is RRicell do format(Expr(Sequence(e)))
     is CCcell do format(Expr(Sequence(e)))
     is args:Sequence do (
	  s := printingPrecision;
	  ac := printingAccuracy;
	  l := printingLeadLimit;
	  t := printingTrailLimit;
	  sep := printingSeparator;
	  n := length(args);
	  if n == 0 || n > 6 then return WrongNumArgs(1,6);
	  if n > 1 then when args.0 is p:ZZcell do if !isInt(p) then return WrongArgSmallInteger(2) else s = toInt(p)
	  is Nothing do nothing else return WrongArgZZ(1);
	  if n > 2 then when args.1 is p:ZZcell do if !isInt(p) then return WrongArgSmallInteger(2) else ac = toInt(p)
	  is Nothing do nothing else return WrongArgZZ(2);
	  if n > 3 then when args.2 is p:ZZcell do if !isInt(p) then return WrongArgSmallInteger(2) else l = toInt(p)
	  is Nothing do nothing else return WrongArgZZ(3);
	  if n > 4 then when args.3 is p:ZZcell do if !isInt(p) then return WrongArgSmallInteger(2) else t = toInt(p)
	  is Nothing do nothing else return WrongArgZZ(4);
	  if n > 5 then when args.4 is p:stringCell do sep = p.v else return WrongArgString(5);
	  when args.(n-1)
	  is x:RRcell do toExpr(concatenate(format(s,ac,l,t,sep,x.v)))
	  is z:CCcell do toExpr(format(s,ac,l,t,sep,false,false,z.v))
	  else WrongArgRR(n)
	  )
     else WrongArg("string, or real number, integer, integer, integer, string"));
setupfun("format", format).Protected = false; -- will be overloaded in m2/methods.m2

numfun(e:Expr):Expr := (
     when e
     is r:QQcell do toExpr(numerator(r.v))
     else WrongArg("a rational number"));
setupfun("numerator",numfun);
denfun(e:Expr):Expr := (
     when e
     is r:QQcell do toExpr(denominator(r.v))
     else WrongArg("a rational number"));
setupfun("denominator",denfun);

join(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  n := length(a);
	  if n == 0 then return e;
	  newlen := 0;
	  foreach x in a do (
	       when x
	       is b:Sequence do (newlen = newlen + length(b);)
	       is c:List do (newlen = newlen + length(c.v);)
	       else return WrongArg("lists or sequences");
	       );
	  z := new Sequence len newlen do (
	       foreach x in a do (
		    when x
		    is b:Sequence do foreach y in b do provide y
		    is c:List do foreach y in c.v do provide y
		    else nothing;
		    );
	       );
	  when a.0
	  is Sequence do Expr(z)
	  is c:List do list(c.Class,z,c.Mutable)
	  else nullE			  -- shouldn't happen anyway
	  )
     is c:List do if c.Mutable then Expr(copy(c)) else e
     else WrongArg("lists or sequences"));
setupfun("join",join);

instanceof(e:Expr):Expr := (
     when e
     is args:Sequence do (
	  when args.1
	  is y:HashTable do if ancestor(Class(args.0),y) then True else False
	  else WrongArg(2,"a hash table"))
     else WrongNumArgs(2));
setupfun("instance",instanceof);


hadseq := false;
deeplen(a:Sequence):int := (
     n := 0;
     foreach x in a do (
	  when x
	  is b:Sequence do (
	       hadseq = true;
	       n = n + deeplen(b);
	       )
	  else (
	       n = n+1;
	       );
	  );
     n);
deepseq := emptySequence;
deepindex := 0;
deepinsert(a:Sequence):int := (
     n := 0;
     foreach x in a do (
	  when x
	  is b:Sequence do n = n + deepinsert(b)
	  else (
	       deepseq.deepindex = x;
	       deepindex = deepindex+1;
	       n = n+1;
	       );
	  );
     n);
deepsplice(a:Sequence):Sequence := (
     -- warning: this returns its arg if no change is required.
     hadseq = false;
     newlen := deeplen(a);
     if hadseq then (
     	  deepseq = new Sequence len deeplen(a) do provide nullE;
     	  deepindex = 0;
     	  deepinsert(a);
     	  w := deepseq;
     	  deepseq = emptySequence;
     	  w)
     else a);
deepsplice(e:Expr):Expr := (
     when e
     is args:Sequence do Expr(deepsplice(args))
     is a:List do (
	  r := deepsplice(a.v);
	  if r == a.v
	  then (
	       if a.Mutable
	       then list(a.Class,copy(r),a.Mutable)
	       else e)
	  else list(a.Class,r,a.Mutable))
     else e);
setupfun("deepSplice",deepsplice);

horizontalJoin(s:Sequence):Expr := (
     s = deepsplice(s);
     ln := 0;
     foreach f at i in s do (
	  when f
	  is Nothing do nothing
	  is Net do (ln = ln + 1;)
	  is stringCell do (ln = ln + 1;)
	  else return WrongArg(i+1,"a net, string, or null"));
     v := new array(Net) len ln do (
	  foreach f in s do (
	       when f
	       is Nothing do nothing
	       is n:Net do provide n
	       is s:stringCell do provide toNet(s.v)
	       else nothing));
     Expr(HorizontalJoin(v)));
horizontalJoin(e:Expr):Expr := (
     when e
     is s:Sequence do horizontalJoin(s)
     is s:List do horizontalJoin(s.v)
     is n:Net do e
     is s:stringCell do Expr(toNet(s.v))
     is Nothing do horizontalJoin(emptySequence)
     else WrongArg("a net, a string, or a list or sequence of nets and strings"));
setupfun("horizontalJoin",horizontalJoin);

stack(s:Sequence):Expr := (
     s = deepsplice(s);
     ln := 0;
     foreach f at i in s do (
	  when f
	  is Nothing do nothing
	  is Net do (ln = ln + 1;)
	  is stringCell do (ln = ln + 1;)
	  else return WrongArg(i+1,"a net, string, or null"));
     v := new array(Net) len ln do (
	  foreach f in s do (
	       when f
	       is Nothing do nothing
	       is n:Net do provide n
	       is s:stringCell do provide toNet(s.v)
	       else nothing));
     Expr(VerticalJoin(v)));
stack(e:Expr):Expr := (
     when e
     is s:Sequence do stack(s)
     is s:List do stack(s.v)
     is n:Net do e
     is s:stringCell do Expr(toNet(s.v))
     is Nothing do stack(emptySequence)
     else WrongArg("a sequence of nets and strings"));
setupfun("stack",stack);

exec(a:Sequence):Expr := (
     newargv := new array(string) len length(a) do provide "";
     foreach x at i in a do (
	  when x
	  is s:stringCell do newargv.i = s.v
	  else return WrongArgString(i+1));
     exec(newargv);
     buildErrorPacket("exec failed"));
exec(e:Expr):Expr := (
     when e
     is a:Sequence do exec(a)
     is a:List do exec(a.v)
     is stringCell do exec(Sequence(e))
     else WrongArg( "a string or a sequence or list of strings"));
setupfun("exec",exec);

youngest(e:Expr):Expr := (
     when e
     is y:HashTable do if !y.Mutable then nullE else e
     is b:Sequence do (
	  if length(b) == 0
	  then nullE
	  else (
	       h := 0;
	       e = nullE;
	       foreach x in b do (
		    when x
		    is y:HashTable do if y.Mutable && y.hash>h then ( h = y.hash; e = x; ) else nothing
		    is y:file do if y.hash>h then ( h = y.hash; e = x; ) else nothing
		    is y:CompiledFunctionClosure do if y.hash>h then ( h = y.hash; e = x; ) else nothing
		    is y:SymbolClosure do if y.symbol.hash>h then ( h = y.symbol.hash; e = x; ) else nothing
		    else nothing;
		    );
	       e))
     else nullE);
setupfun("youngest", youngest);

toRR(e:Expr):Expr := (
     when e
     is x:ZZcell do toExpr(toRR(x.v,defaultPrecision))
     is x:QQcell do toExpr(toRR(x.v,defaultPrecision))
     is RRcell do e
     is x:RRicell do toExpr(midpointRR(x.v))
     is s:Sequence do (
	  if length(s) != 2 then WrongNumArgs(1,2) else
	  when s.0 is prec:ZZcell do (
	       if !isULong(prec.v) then WrongArgSmallUInteger(1)
	       else (
	       	    when s.1
     	       	    is x:ZZcell do toExpr(toRR(x.v,toULong(prec.v)))
	       	    is x:QQcell do toExpr(toRR(x.v,toULong(prec.v)))
     	       	    is x:RRcell do toExpr(toRR(x.v,toULong(prec.v)))
                 is x:RRicell do toExpr(midpointRR(x.v,toULong(prec.v)))
		    else WrongArg(1,"an integral, rational, or real number")
		    )
	       )
	  else WrongArgZZ(1)
	  )
     else WrongArg("an integral, rational, or real number, or a pair"));
setupfun("toRR",toRR);
                                                     
toRRi(e:Expr):Expr := (
    when e
    	 is x:ZZcell do toExpr(toRRi(x.v))
	     is x:QQcell do toExpr(toRRi(x.v))
	     is x:RRcell do toExpr(toRRi(x.v))
	     is x:RRicell do e
    	 is s:Sequence do (
	     if length(s) > 3 then WrongNumArgs(1,3) else
	     if length(s) == 2 then (
               when s.0 is x:ZZcell do (
               	       when s.1 is y:ZZcell do toExpr(toRRi(x.v,y.v))
                                is y:QQcell do toExpr(toRRi(x.v,y.v))
                                is y:RRcell do toExpr(toRRi(x.v,y.v))
                    	        else WrongArg(1,"a pair of integral, rational, or real numbers"))
                       is x:QQcell do (
               	       when s.1 is y:ZZcell do toExpr(toRRi(x.v,y.v))
                                is y:QQcell do toExpr(toRRi(x.v,y.v))
                                is y:RRcell do toExpr(toRRi(x.v,y.v))
                    	        else WrongArg(1,"a pair of integral, rational, or real numbers"))
                       is x:RRcell do (
               	       when s.1 is y:ZZcell do toExpr(toRRi(x.v,y.v))
                                is y:QQcell do toExpr(toRRi(x.v,y.v))
                                is y:RRcell do toExpr(toRRi(x.v,y.v))
                    	        else WrongArg(1,"a pair of integral, rational, or real numbers"))
                       else WrongArg(1,"a pair of integral, rational, or real numbers"))
	    else when s.0 is prec:ZZcell do (
	           when s.1 is x:ZZcell do (
                       when s.2 is y:ZZcell do toExpr(toRRi(x.v,y.v,toULong(prec.v)))
                                is y:QQcell do toExpr(toRRi(x.v,y.v,toULong(prec.v)))
                                is y:RRcell do toExpr(toRRi(x.v,y.v,toULong(prec.v)))
                                else WrongArg(1,"a pair of integral, rational, or real numbers, with a precision"))
                       is x:QQcell do (
                       when s.2 is y:ZZcell do toExpr(toRRi(x.v,y.v,toULong(prec.v)))
                                is y:QQcell do toExpr(toRRi(x.v,y.v,toULong(prec.v)))
                                is y:RRcell do toExpr(toRRi(x.v,y.v,toULong(prec.v)))
                                else WrongArg(1,"a pair of integral, rational, or real numbers, with a precision"))
                       is x:RRcell do (
                       when s.2 is y:ZZcell do toExpr(toRRi(x.v,y.v,toULong(prec.v)))
                                is y:QQcell do toExpr(toRRi(x.v,y.v,toULong(prec.v)))
                                is y:RRcell do toExpr(toRRi(x.v,y.v,toULong(prec.v)))
                                else WrongArg(1,"a pair of integral, rational, or real numbers, with a precision"))
	       else WrongArg(1,"a pair of integral, rational, or real numbers, with a precision"))
	    else buildErrorPacket(EngineError("The first argument should be an integer")))
   	 else WrongArg(1,"a pair or triple  of integral, rational, or real numbers"));
setupfun("toRRi",toRRi);
                                                     
rightRR(e:Expr):Expr := (
     when e
        is x:RRicell do toExpr(rightRR(x.v))
        else WrongArg("an interval"));
setupfun("right",rightRR);
                                                     
leftRR(e:Expr):Expr := (
     when e
        is x:RRicell do toExpr(leftRR(x.v))
        else WrongArg("an interval"));
setupfun("left",leftRR);
                                                     
widthRR(e:Expr):Expr := (
     when e
        is x:RRicell do toExpr(widthRR(x.v))
        else WrongArg("an interval"));
setupfun("diameter",widthRR).Protected = false;
                                                     
midpointRR(e:Expr):Expr := (
     when e
        is x:RRicell do toExpr(midpointRR(x.v))
        else WrongArg("an interval"));
setupfun("midpoint",midpointRR);
                                                     
isEmptyRRi(e:Expr):Expr := (
     when e
        is x:RRicell do toExpr(isEmpty(x.v))
        else WrongArg("an interval"));
setupfun("isEmptyRRi",isEmptyRRi);
                                                     
subsetRRi(e:Expr):Expr := (
     when e is s:Sequence do (
	    if length(s) > 3 then WrongNumArgs(1,3) else
	    if length(s) == 2 then (
               when s.0 is x:ZZcell do (
               	    when s.1 is y:ZZcell do (toExpr(x.v===y.v))
		    	             is y:QQcell do (toExpr(x.v===y.v))
			                 is y:RRcell do (toExpr(x.v===y.v))
                             is y:RRicell do (toExpr(contains(x.v,y.v)))
                    else WrongArg(1,"a pair of integral, rational, real numbers or intervals"))
               is x:QQcell do (
               	    when s.1 is y:ZZcell do (toExpr(x.v===y.v))
		    	             is y:QQcell do (toExpr(x.v===y.v))
			                 is y:RRcell do (toExpr(x.v===y.v))
                             is y:RRicell do (toExpr(contains(x.v,y.v)))
                    else WrongArg(1,"a pair of integral, rational, real numbers or intervals"))
               is x:RRcell do (
               	    when s.1 is y:ZZcell do (toExpr(x.v===y.v))
		    	             is y:QQcell do (toExpr(x.v===y.v))
			                 is y:RRcell do (toExpr(x.v===y.v))
                             is y:RRicell do (toExpr(contains(x.v,y.v)))
                    else WrongArg(1,"a pair of integral, rational, real numbers or intervals"))
                is x:RRicell do (
               	    when s.1 is y:ZZcell do (toExpr(x.v===y.v))
		    	             is y:QQcell do (toExpr(x.v===y.v))
			                 is y:RRcell do (toExpr(x.v===y.v))
                             is y:RRicell do (toExpr(contains(x.v,y.v)))
                    else WrongArg(1,"a pair of integral, rational, real numbers or intervals"))
                else WrongArg(1,"a pair of integral, rational, real numbers or intervals")) else
            WrongArg(1,"a pair of integral, rational, real numbers or intervals"))
         else WrongArg(1,"a pair of integral, rational, real numbers or intervals"));
setupfun("subsetRRi",subsetRRi);
                                                     
intersectRRi(e:Expr):Expr := (
    when e
    	 is s:Sequence do (
	     if length(s) > 3 then WrongNumArgs(1,3) else
	     if length(s) == 2 then (
               when s.0 is x:RRicell do (
               	       when s.1 is y:RRicell do toExpr(intersectRRi(x.v,y.v))
                    	        else WrongArg(1,"a pair of intervals"))
                       else WrongArg("a pair of intervals"))
	    else when s.0 is prec:ZZcell do (
	           when s.1 is x:RRicell do (
                       when s.2 is y:RRicell do toExpr(intersectRRi(x.v,y.v,toULong(prec.v)))
                                else WrongArg(1,"a pair of intervals"))
	       else WrongArg(1,"a pair of intervals"))
	    else WrongArg(1,"a pair of intervals"))
   	 else WrongArg("a pair of intervals"));
setupfun("intersectRRi",intersectRRi);

toCC(e:Expr):Expr := (
     when e
     is x:ZZcell do toExpr(toCC(x.v,defaultPrecision)) -- # typical value: toCC, ZZ, CC
     is x:QQcell do toExpr(toCC(x.v,defaultPrecision)) -- # typical value: toCC, QQ, CC
     is x:RRcell do toExpr(toCC(x.v)) -- # typical value: toCC, RR, CC
     is CCcell do e -- # typical value: toCC, CC, CC
     is s:Sequence do (
	  if length(s) == 2 then (
	       when s.0 is prec:ZZcell do (
		    if !isULong(prec.v) then WrongArgSmallUInteger(1)
		    else (
			 when s.1
			 is x:ZZcell do toExpr(toCC(x.v,toULong(prec.v))) -- # typical value: toCC, ZZ, ZZ, CC
			 is x:QQcell do toExpr(toCC(x.v,toULong(prec.v))) -- # typical value: toCC, ZZ, QQ, CC
			 is x:RRcell do toExpr(toCC(x.v,toULong(prec.v))) -- # typical value: toCC, ZZ, RR, CC
			 is x:CCcell do toExpr(toCC(x.v,toULong(prec.v))) -- # typical value: toCC, ZZ, CC, CC
			 else WrongArg("a rational number, real number, or an integer")
			 )
		    )
	       is x:RRcell do (
		    when s.1 is y:RRcell do toExpr(toCC(x.v,y.v))	    -- # typical value: toCC, RR, RR, CC
		    else WrongArgRR()
		    )
	       else WrongArgZZ(1)
	       )
	  else if length(s) == 3 then (
	       when s.0 is prec:ZZcell do (
		    -- # typical value: toCC, ZZ, ZZ, ZZ, CC
		    -- # typical value: toCC, ZZ, ZZ, QQ, CC
		    -- # typical value: toCC, ZZ, ZZ, RR, CC
		    -- # typical value: toCC, ZZ, QQ, ZZ, CC
		    -- # typical value: toCC, ZZ, QQ, QQ, CC
		    -- # typical value: toCC, ZZ, QQ, RR, CC
		    -- # typical value: toCC, ZZ, RR, ZZ, CC
		    -- # typical value: toCC, ZZ, RR, QQ, CC
		    -- # typical value: toCC, ZZ, RR, RR, CC
		    if !isULong(prec.v) then WrongArgSmallUInteger(1)
		    else toExpr(CC(
			      when s.1
			      is x:QQcell do toRR(x.v,toULong(prec.v))
			      is x:ZZcell do toRR(x.v,toULong(prec.v))
			      is x:RRcell do toRR(x.v,toULong(prec.v))
			      else (
				   return WrongArg("a rational number, real number, or an integer");
				   toRR(0,toULong(prec.v)) -- dummy
				   )
			      ,
			      when s.2
			      is x:QQcell do toRR(x.v,toULong(prec.v))
			      is x:ZZcell do toRR(x.v,toULong(prec.v))
			      is x:RRcell do toRR(x.v,toULong(prec.v))
			      else (
				   return WrongArg("a rational number, real number, or an integer");
				   toRR(0,toULong(prec.v)) -- dummy
				   ))))
	       else WrongArgZZ(1))
	  else WrongNumArgs(1,3))
     else WrongArg("a real or complex number, or 2 or 3 arguments"));
setupfun("toCC",toCC);

precision(e:Expr):Expr := (
     when e
     is x:RRcell do toExpr(precision(x.v))
     is x:RRicell do toExpr(precision(x.v))
     is x:CCcell do toExpr(precision(x.v))
     else WrongArgRR());
setupfun("precision0",precision);

-- locate:

positionRange := {filename:string, minline:int, mincol:int, maxline:int, maxcol:int};
threadLocal locatedCode := positionRange("",0,0,0,0);
lookat(p:Position):void := (
     if p == dummyPosition then return;
     locatedCode.filename = p.filename;
     if locatedCode.minline > int(p.line) then (
	  locatedCode.minline = int(p.line);
	  locatedCode.mincol = int(p.column);
	  )
     else if locatedCode.minline == int(p.line) && locatedCode.mincol > int(p.column) then (
	  locatedCode.mincol = int(p.column);
	  );
     if locatedCode.maxline < int(p.line) then (
	  locatedCode.maxline = int(p.line);
	  locatedCode.maxcol = int(p.column);
	  )
     else if locatedCode.maxline == int(p.line) && locatedCode.maxcol < int(p.column) then (
	  locatedCode.maxcol = int(p.column);
	  );
     );
locate(x:Token):void := lookat(position(x));
locate(e:Code):void := (
     when e
     is nullCode do nothing
     is v:adjacentCode do (lookat(v.position); locate(v.lhs); locate(v.rhs);)
     is v:arrayCode do foreach c in v.z do locate(c)
     is v:angleBarListCode do foreach c in v.t do locate(c)
     is v:Error do lookat(v.position)
     is v:semiCode do foreach c in v.w do locate(c)
     is v:binaryCode do (lookat(v.position); locate(v.lhs); locate(v.rhs);)
     is v:forCode do ( lookat(v.position); locate(v.fromClause); locate(v.toClause); locate(v.whenClause); locate(v.listClause); locate(v.doClause); )
     is v:functionCode do (locate(v.arrow);locate(v.body);)
     is v:globalAssignmentCode do (lookat(v.position); locate(v.rhs);)
     is v:globalMemoryReferenceCode do lookat(v.position)
     is v:threadMemoryReferenceCode do lookat(v.position)
     is v:globalSymbolClosureCode do lookat(v.position)
     is v:threadSymbolClosureCode do lookat(v.position)
     is v:ifCode do ( lookat(v.position); locate(v.predicate); locate(v.thenClause); locate(v.elseClause); )
     is v:integerCode do lookat(v.position)
     is v:listCode do foreach c in v.y do locate(c)
     is v:localAssignmentCode do (lookat(v.position); locate(v.rhs);)
     is v:localMemoryReferenceCode do lookat(v.position)
     is v:localSymbolClosureCode do lookat(v.position)
     is v:multaryCode do ( lookat(v.position); foreach c in v.args do locate(c);)
     is v:newCode do ( lookat(v.position); locate(v.newClause); )
     is v:newFromCode do ( lookat(v.position); locate(v.newClause); locate(v.fromClause); )
     is v:newLocalFrameCode do locate(v.body)
     is v:newOfCode do ( lookat(v.position); locate(v.newClause); locate(v.ofClause); )
     is v:newOfFromCode do ( lookat(v.position); locate(v.newClause); locate(v.ofClause); locate(v.fromClause); )
     is v:parallelAssignmentCode do (lookat(v.position); locate(v.rhs);)
     is v:realCode do lookat(v.position)
     is v:sequenceCode do foreach c in v.x do locate(c)
     is v:stringCode do nothing
     is v:ternaryCode do ( lookat(v.position); locate(v.arg1); locate(v.arg2); locate(v.arg3);)
     is v:tryCode do ( lookat(v.position); locate(v.code); locate(v.thenClause); locate(v.elseClause); )
     is v:catchCode do ( lookat(v.position); locate(v.code); )
     is v:unaryCode do (lookat(v.position); locate(v.rhs);)
     is v:whileDoCode do ( lookat(v.position); locate(v.predicate); locate(v.doClause); )
     is v:whileListCode do ( lookat(v.position); locate(v.predicate); locate(v.listClause); )
     is v:whileListDoCode do ( lookat(v.position); locate(v.predicate); locate(v.listClause); locate(v.doClause); )
     );
locate0():void := (
     locatedCode.filename = "-*unknown file name*-";
     locatedCode.minline = 1000000;
     locatedCode.maxline = 0;
     );
locate1():void := (
     if locatedCode.minline == 1000000 then (
	  locatedCode.minline = 0;
	  locatedCode.mincol = 0;
	  locatedCode.maxcol = 0;
	  ));
locate2(c:Code):Expr := (
     locate1();
     p := codePosition(c);
     Expr(Sequence(
	       toExpr(verifyMinimizeFilename(locatedCode.filename)),
	       toExpr(locatedCode.minline),
	       toExpr(locatedCode.mincol),
	       toExpr(locatedCode.maxline),
	       toExpr(locatedCode.maxcol),
	       toExpr(int(p.line)),
	       toExpr(int(p.column)))));
locate(e:Expr):Expr := (
     when e
     is Nothing do nullE
     is Sequence do locate(lookupfun(e))
     is CompiledFunction do nullE
     is CompiledFunctionClosure do nullE
     is s:SymbolClosure do (
	  p := s.symbol.position;
	  if p == dummyPosition
	  then nullE
	  else Expr(
	       Sequence(
		    toExpr(verifyMinimizeFilename(p.filename)),
		    toExpr(int(p.line)),toExpr(int(p.column)),
		    toExpr(int(p.line)),toExpr(int(p.column)+length(s.symbol.word.name)),
		    toExpr(int(p.line)),toExpr(int(p.column))
		    )))
     is c:CodeClosure do (
	  locate0();
	  locate(c.code);
	  locate2(c.code))
     is s:SpecialExpr do locate(s.e)
     is f:functionCode do (
	  locate0();
	  locate(f.body);
	  locate2(f.body))
     is f:FunctionClosure do (
	  locate0();
	  locate(f.model.arrow);
	  locate(f.model.body);
	  locate2(f.model.body))
     else WrongArg("a function, symbol, sequence, or null"));
setupfun("locate", locate).Protected = false; -- will be overloaded in m2/methods.m2


powermod(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 3 then
     when s.0 is base:ZZcell do
     when s.1 is exp:ZZcell do
     when s.2 is mod:ZZcell do
     toExpr(powermod(base.v,exp.v,mod.v))
     else WrongArgZZ(3)
     else WrongArgZZ(2)
     else WrongArgZZ(1)
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("powermod",powermod);

partsRR(x:Expr):Expr := (
     when x is xxx:RRcell do (
	  xx := xxx.v;
	  p := Ccode(ulong,"(unsigned long)(",xx,")->_mpfr_prec");
	  sz := 8 * Ccode(int,"sizeof(*(",xx,")->_mpfr_d)");
	  n := (p+sz-1)/sz;
	  numbits := n * sz;
	  sgn := toExpr(Ccode(long,"(long)(",xx,")->_mpfr_sign"));
	  expt := toExpr(Ccode(long,"(long)(",xx,")->_mpfr_exp"));
	  m := zeroZZ;
	  for i from int(n)-1 to 0 by -1 do (
	       m = (m << sz) + toInteger(Ccode(ulong,"(unsigned long)(",xx,")->_mpfr_d[",i,"]"));
	       );
	  Expr(Sequence(toExpr(p),sgn,expt,toExpr(m),toExpr(numbits))))
     else WrongArg("a real number"));
setupfun("partsRR",partsRR);
                                                     
-- Should there be an RRi version of partsRR?

segmentationFault(e:Expr):Expr := (segmentationFault();e);
setupfun("segmentationFault",segmentationFault);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d actors4.o "
-- End:
