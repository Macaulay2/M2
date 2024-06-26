--		Copyright 1994 by Daniel R. Grayson

use expr;

header "#include <engine.h>"; -- required for raw hash functions

export hash(e:Expr):hash_t := (
     when e
     is x:HashTable do x.hash
     is x:SymbolClosure do x.symbol.hash -- should add in a hash code for the frame
     is s:SymbolBody do s.symbol.hash
     is x:Database do x.hash
     is x:ZZcell do hash(x.v)
     is b:Boolean do if b.v then hash_t(444777) else hash_t(777333)
     is Nothing do hash_t(333889)
     is x:List do x.hash
     is f:functionCode do f.hash
     is MysqlConnectionWrapper do hash_t(237489) -- improve this later!
     is MysqlFieldWrapper do hash_t(23748) -- improve this later!
     is MysqlResultWrapper do hash_t(2374) -- improve this later!
     is CodeClosure do hash_t(73889)       -- improve this later!
     is x:DictionaryClosure do x.dictionary.hash -- there may be many dictionary closures with the same dictionary and different frames, too bad
     is x:QQcell do hash(x.v)
     is x:RRcell do hash(x.v)
     is x:RRicell do hash(x.v)
     is x:CCcell do hash(x.v)
     is x:Sequence do (
	  -- the numbers here are the same as in binary lookup() in objects.d!!
	  h := hash_t(27449);
	  foreach y in x do h = h * 27457 + hash(y);
	  h)
     is x:stringCell do hash(x.v)				    -- for strings, keep internal and external hash the same
     is n:Net do hash(n)
     is n:NetFile do hash(n)
     is x:file do x.hash
     is f:FunctionClosure do f.hash
     is x:Error do (
	  929+hash(x.message)+12963*(
	       hash(x.position.filename) 
	       + 1299791 * (int(x.position.line) + 
		    1299811 * int(x.position.column))))
     is x:RawMonomialCell do hash(x.p)
     is x:RawMonomialOrderingCell do Ccode(hash_t, "rawMonomialOrderingHash(",x.p,")" )
     is x:RawMonoidCell do Ccode(hash_t, "rawMonoidHash(",x.p,")" )
     is x:RawMatrixCell do Ccode(hash_t, "rawMatrixHash(",x.p,")" )
     is x:RawMutableMatrixCell do Ccode(hash_t, "rawMutableMatrixHash(",x.p,")" )
     is x:RawMutableComplexCell do Ccode(hash_t, "rawMutableComplexHash(",x.p,")" )
     -- NAG begin
     is x:RawHomotopyCell do Ccode(hash_t, "rawHomotopyHash(",x.p,")" )
     is x:RawSLEvaluatorCell do Ccode(hash_t, "rawSLEvaluatorHash(",x.p,")" )
     is x:RawSLProgramCell do Ccode(hash_t, "rawSLProgramHash(",x.p,")" )
     is x:RawStraightLineProgramCell do Ccode(hash_t, "rawStraightLineProgramHash(",x.p,")" )
     is x:RawPathTrackerCell do Ccode(hash_t, "rawPathTrackerHash(",x.p,")" )
     is x:RawPointArrayCell do Ccode(hash_t, "rawPointArrayHash(",x.p,")" )
     -- NAG end
     is x:RawRingCell do Ccode(hash_t, "rawRingHash(",x.p,")" )
     is x:RawComputationCell do Ccode(hash_t, "rawComputationHash(",x.p,")" )
     is x:RawFreeModuleCell do Ccode(hash_t, "rawFreeModuleHash(",x.p,")" )
     is x:RawRingMapCell do Ccode(hash_t, "rawRingMapHash(",x.p,")" )
     is x:RawRingElementCell do Ccode(hash_t, "rawRingElementHash(",x.p,")" )
     is x:RawMonomialIdealCell do Ccode(hash_t, "rawMonomialIdealHash(",x.p,")" )
     is s:SpecialExpr do s.Class.hash + 221 * hash(s.e)
     is x:CompiledFunction do x.hash
     is x:CompiledFunctionClosure do x.hash
     is f:CompiledFunctionBody do hash_t(12347)
     is po:pythonObjectCell do po.hash
     is xmlNodeCell do hash_t(123456)
     is xmlAttrCell do hash_t(123457)
     is t:TaskCell do t.body.hash
     is foss:fileOutputSyncState do hash_t(123458)
     -- cast to long first to avoid "different size" compiler warning
     is x:pointerCell do Ccode(hash_t, "(long)", x.v)
     is x:atomicIntCell do x.hash
     );

export hash(x:List):hash_t := (
     h := x.Class.hash + 23407;
     foreach y in x.v do h = h * 1299833 + hash(y);
     h);
export sethash(x:List,is_mutable:bool):List := (
     if is_mutable 
     then (
	  x.Mutable = true;
	  x.hash = nextHash();
	  )
     else (
	  x.Mutable = false;
	  x.hash = hash(x);
	  );
     x);
export copy(v:Sequence):Sequence := (
     new Sequence len length(v) do foreach i in v do provide i);
export copy(a:List):List := List(
     a.Class, 
     new Sequence len length(a.v) do foreach i in a.v do provide i,
     a.hash,
     a.Mutable);
export reverse(a:Sequence):Sequence := (
     n := length(a);
     new Sequence len n do (n = n-1; provide a.n));
export reverse(a:List):List := sethash( 
     List( a.Class, reverse(a.v), hash_t(0), a.Mutable), a.Mutable 
     );
export seq():Expr := emptySequenceE;
export seq(e:Expr,f:Expr):Expr := Expr(Sequence(e,f));
export seq(e:Expr,f:Expr,g:Expr):Expr := Expr(Sequence(e,f,g));
export list(a:Sequence):Expr := (
     r := List(listClass,a,hash_t(0),false);
     r.hash = hash(r);
     Expr(r));     
export list(classs:HashTable,a:Sequence):Expr := (
     r := List(classs,a,hash_t(0),false);
     r.hash = hash(r);
     Expr(r));     
export list(classs:HashTable,a:Sequence,is_mutable:bool):Expr := (
     r := List(classs,a,hash_t(0),is_mutable);
     r.hash = hash(r);
     Expr(r));     
export list(classs:HashTable,e:Expr):Expr := (
     when e
     is a:Sequence do list(classs,a)
     else list(classs,Sequence(e)));
export emptyList := list(Sequence());
export list():Expr := emptyList;
export list(e:Expr):Expr := list(Sequence(e));
export list(e:Expr,f:Expr):Expr := list(Sequence(e,f));
export list(e:Expr,f:Expr,g:Expr):Expr := list(Sequence(e,f,g));
export list(e:Expr,f:Expr,g:Expr,h:Expr):Expr := list(Sequence(e,f,g,h));

export Array(a:Sequence):Expr := (
     r := List(arrayClass,a,hash_t(0),false);
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

export AngleBarList(a:Sequence):Expr := (
     r := List(angleBarListClass,a,hash_t(0),false);
     r.hash = hash(r);
     Expr(r));
export emptyAngleBarList := AngleBarList(Sequence());

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d basic.o "
-- End:
