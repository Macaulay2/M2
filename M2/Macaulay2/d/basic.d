--		Copyright 1994 by Daniel R. Grayson
use C;
use system;
use stdio;
use stdiop;
use engine;
use binding;
use strings;
use nets;
use varnets;
use tokens;
use err;
use stdio;
use gmp;

export hash(e:Expr):int := (
     when e
     is x:HashTable do x.hash
     is x:SymbolClosure do x.symbol.hash
     is x:Database do x.hash
     is x:Integer do hash(x)
     is b:Boolean do if b.v then 444777 else 777333
     is Nothing do 333889
     is x:List do x.hash
     is f:functionCode do f.hash
     is CodeClosure do 73889				    -- improve this later!
     is x:DictionaryClosure do x.dictionary.hash	    -- there may be many dictionary closures with the same dictionary and different frames, too bad
     is x:Rational do hash(x)
     is x:RR do hash(x)
     is x:CC do hash(x)
     is x:Sequence do (
	  -- the numbers here are the same as in binary lookup() in objects.d!!
	  h := 27449;
	  foreach y in x do h = h * 27457 + hash(y);
	  h)
     is x:string do hash(x)				    -- for strings, keep internal and external hash the same
     is n:Net do hash(n)
     is n:NetFile do hash(n)
     is x:file do x.hash
     is f:FunctionClosure do f.model.hash
     is x:Error do (
	  929+hash(x.message)+12963*(
	       hash(x.position.filename) 
	       + 1299791 * (int(x.position.line) + 
		    1299811 * int(x.position.column))))
     is x:RawMonomial do int(Ccode(ulong, "IM2_Monomial_hash((Monomial*)",x,")" ))
     is x:RawMonomialOrdering do int(Ccode(ulong, "IM2_MonomialOrdering_hash((MonomialOrdering*)",x,")" ))
     is x:RawMonoid do int(Ccode(ulong, "IM2_Monoid_hash((Monoid*)",x,")" ))
     is x:RawMatrix do int(Ccode(int, "IM2_Matrix_hash((Matrix*)",x,")" ))
     is x:RawMutableMatrix do int(Ccode(int, "IM2_MutableMatrix_hash((MutableMatrix*)",x,")" ))
     is x:RawRing do int(Ccode(ulong, "IM2_Ring_hash((Ring*)",x,")" ))
     is x:RawComputation do int(Ccode(ulong, "IM2_GB_hash((Computation*)",x,")" ))
     is x:RawFreeModule do (
	  0
	  -- int(Ccode(ulong, "IM2_FreeModule_hash((FreeModule*)",x,")" ))
	  )
     is x:RawRingMap do (
	  0
	  -- int(Ccode(ulong, "IM2_RingMap_hash((RingMap*)",x,")" ))
	  )
     is x:RawRingElement do 12345
     is x:RawMonomialIdeal do 12346
     is s:SpecialExpr do s.class.hash + 221 * hash(s.e)
     is x:CompiledFunction do x.hash
     is x:CompiledFunctionClosure do x.hash
     is f:CompiledFunctionBody do 12347
     );
export hash(x:List):int := (
     h := x.class.hash + 23407;
     foreach y in x.v do h = h * 1299833 + hash(y);
     h);
export sethash(x:List,mutable:bool):List := (
     if mutable 
     then (
	  x.mutable = true;
	  x.hash = nextHash();
	  )
     else (
	  x.mutable = false;
	  x.hash = hash(x);
	  );
     x);
export setmutability(x:List,mutable:bool):List := (
     if x.mutable == mutable 
     then x
     else if mutable 
     then (
	  x.mutable = true;
	  x.hash = nextHash();
	  x
	  )
     else (
	  x.mutable = false;
	  x.hash = hash(x);
	  x
	  ));
export hash(x:HashTable):int := (
     h := x.parent.hash + x.class.hash * 231 + 32455;
     foreach bucket in x.table do (
	  p := bucket;
	  while p != p.next do (
	       j := 48892373 + p.hash;
	       h = h + j * j * hash(p.value);
	       p = p.next;
	       ));
     h);
export sethash(o:HashTable,mutable:bool):HashTable := (
     if mutable 
     then (
	  o.mutable = true;
	  o.hash = nextHash();
	  )
     else (
	  o.mutable = false;
	  o.hash = hash(o);
	  );
     o);
export setmutability(o:HashTable,mutable:bool):HashTable := (
     if o.mutable == mutable 
     then o
     else if mutable
     then (
	  o.mutable = true;
	  o.hash = nextHash();
	  o)
     else (
	  o.mutable = false;
	  o.hash = hash(o);
	  o));
export copy(v:Sequence):Sequence := (
     new Sequence len length(v) do foreach i in v do provide i);
export copy(table:array(KeyValuePair)):array(KeyValuePair) := (
     new array(KeyValuePair) len length(table) do (
	  foreach bucket in table do provide (
	       if bucket == bucketEnd 
	       then bucketEnd
	       else if bucket.next == bucketEnd
	       then KeyValuePair(
		    bucket.key, bucket.hash, bucket.value,bucketEnd)
	       else KeyValuePair(
		    bucket.key, bucket.hash, bucket.value,
		    KeyValuePair(
			 bucket.next.key, bucket.next.hash,
			 bucket.next.value, (
			      newbucket := bucketEnd;
			      p := bucket.next.next;
			      while p != bucketEnd do (
				   newbucket = KeyValuePair(
					p.key,p.hash,p.value,newbucket);
				   p = p.next;
				   );
			      newbucket))))));
export copy(obj:HashTable):HashTable := HashTable(
     copy(obj.table), 
     obj.class, 
     obj.parent, 
     obj.numEntries,
     obj.hash,
     obj.mutable);
export copy(a:List):List := List(
     a.class, 
     new Sequence len length(a.v) do foreach i in a.v do provide i,
     a.hash,
     a.mutable);
export reverse(a:Sequence):Sequence := (
     n := length(a);
     new Sequence len n do (n = n-1; provide a.n));
export reverse(a:List):List := sethash( 
     List( a.class, reverse(a.v), 0, a.mutable), a.mutable 
     );
copy(f:Frame):Frame := (
     if f.frameID == 0			  -- the global dictionary?
     then f
     else Frame(
	  copy(f.outerFrame),
	  f.frameID,
	  f.valuesUsed,
	  false,
	  new Sequence len length(f.values) do foreach e in f.values do provide e));
export seq():Expr := emptySequenceE;
export seq(e:Expr,f:Expr):Expr := Expr(Sequence(e,f));
export seq(e:Expr,f:Expr,g:Expr):Expr := Expr(Sequence(e,f,g));
export list(a:Sequence):Expr := (
     r := List(listClass,a,0,false);
     r.hash = hash(r);
     Expr(r));     
export list(class:HashTable,a:Sequence):Expr := (
     r := List(class,a,0,false);
     r.hash = hash(r);
     Expr(r));     
export list(class:HashTable,a:Sequence,mutable:bool):Expr := (
     r := List(class,a,0,mutable);
     r.hash = hash(r);
     Expr(r));     
export list(class:HashTable,e:Expr):Expr := (
     when e
     is a:Sequence do list(class,a)
     else list(class,Sequence(e)));
export emptyList := list(Sequence());
export list():Expr := emptyList;
export list(e:Expr):Expr := list(Sequence(e));
export list(e:Expr,f:Expr):Expr := list(Sequence(e,f));
export list(e:Expr,f:Expr,g:Expr):Expr := list(Sequence(e,f,g));
export list(e:Expr,f:Expr,g:Expr,h:Expr):Expr := list(Sequence(e,f,g,h));

export Array(a:Sequence):Expr := (
     r := List(arrayClass,a,0,false);
     r.hash = hash(r);
     Expr(r));
export Array(e:Expr):Expr := (
     when e
     is a:Sequence do Array(a)
     else Array(Sequence(e)));
export emptyArray := Array(Sequence());
export Array():Expr := emptyArray;
export Array(e:Expr,f:Expr):Expr := Array(Sequence(e,f));
export Array(e:Expr,f:Expr,g:Expr):Expr := Array(Sequence(e,f,g));
export Array(e:Expr,f:Expr,g:Expr,h:Expr):Expr := Array(Sequence(e,f,g,h));

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
