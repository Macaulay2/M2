--		Copyright 1994 by Daniel R. Grayson
use common;
use util;

reverse(e:Expr):Expr := (
     when e
     is a:Sequence do Expr(reverse(a))
     is a:List do Expr(reverse(a))
     is s:stringCell do toExpr(reverse(s.v))
     else WrongArg("a list, sequence, or string"));
setupfun("reverse",reverse);
export seq(e:Expr):Expr := Expr(Sequence(e));
-- setupfun("singleton",seq);
export splice(a:Sequence):Sequence := (
     -- warning - this function may return its argument without copying
     hadseq := false;
     newlen := length(a);
     if newlen == 0 then return a;
     if newlen == 1 then (
	  when a.0
	  is s:Sequence do return s
	  else return a; );
     foreach i in a do (
	  when i is ii:Sequence do (
	       hadseq = true; 
	       newlen = newlen + length(ii) - 1; )
     	  else nothing;
	  );
     if hadseq
     then new Sequence len newlen do
     foreach i in a do 
     when i is ii:Sequence 
     do foreach j in ii do provide j
     else provide i
     else a);
export splice(e:Expr):Expr := (
     when e
     is v:Sequence do Expr(splice(v))
     is a:List do list(
	  a.Class,
	  if a.Mutable then (
	       r := splice(a.v);
	       if r == a.v then copy(r) else r
	       )
	  else splice(a.v),
	  a.Mutable)
     else e);
setupfun("splice",splice);
export accumulate(
     f0:function():Expr, f1:function(Expr):Expr,
     f2:function(Expr,Expr):Expr, e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 0 then f0()
	  else if length(a) == 1 then f1(a.0)
	  else (
	       g := a.0;
	       for i from 1 to length(a)-1 do (
		    g = f2(g,a.i);
		    when g is Error do return g else nothing;
		    );
	       g))
     else f1(e));

export map(f:function(Expr):Expr,a:Sequence):Sequence := (
     new Sequence len length(a) do foreach x in a do provide f(x));
export join(v:Sequence,w:Sequence):Sequence := (
     new Sequence len length(v) + length(w) do (
	  foreach x in v do provide x;
	  foreach y in w do provide y));
export subarray(v:Sequence,start:int,leng:int):Sequence := (
     new Sequence len leng at i do provide v.(start+i));
export subarray(v:Sequence,leng:int):Sequence := subarray(v,0,leng);

export isInteger(e:Expr):bool := when e is ZZcell do true else false;
export isInt(e:Expr):bool := when e is i:ZZcell do isInt(i) else false;
export isIntArray(e:Sequence):bool := (
     foreach x in e do if !isInt(x) then return false;
     true);
export isIntArray(e:Expr):bool := (
     when e
     is a:Sequence do isIntArray(a)
     is b:List do isIntArray(b.v)
     else false);     
export toInt(e:Expr):int := (
     -- This is getting used incorrectly in interface2.dd, so a user error could be labelled an internal error.
     -- We should have no internal errors.
     -- To fix it, we should have this function, and similar ones, return union types that have to be tested.
     when e 
     is i:ZZcell do toInt(i)
     else (
	  fatal("internal error");
	  0	     	       	    -- just to satisfy noisy compilers
	  ));
export toIntArray(e:Sequence):array(int) := (
     new array(int) len length(e) do foreach x in e do provide toInt(x));
export toIntArray(e:Expr):array(int) := (
     when e
     is a:Sequence do toIntArray(a)
     is b:List do toIntArray(b.v)
     else (
	  fatal("internal error: toIntArray expected an array of ints");
	  array(int)()	   	     	  -- just to satisfy noisy compilers
	  )
     );
export toArrayExpr(v:array(int)):Sequence := (
     new Sequence len length(v) do foreach i in v do provide Expr(ZZcell(toInteger(i)))
     );

export newlist(classs:HashTable,v:Sequence):List := (
     x := List(classs,v,0,false);
     x.hash = hash(x);
     x);
export basictype(o:HashTable):HashTable := (
     while true do (
	  if o.parent == thingClass then return o;
	  o = o.parent;
	  ));


-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d struct.o "
-- End:
