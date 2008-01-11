--		Copyright 1994 by Daniel R. Grayson

use C;
use system;
use strings;
use stdio;
use gmp;
use nets;
use varnets;
use engine;
use err;
use tokens;
use basic;
use convertr;
use common;
use struct;
use binding;
use ctype;
use util;

export enlarge(object:HashTable):void := (
     oldTable := object.table;
     newlen := 2*length(oldTable);
     mask := newlen - 1;
     newTable := new array(KeyValuePair) len newlen do provide bucketEnd;
     object.table = newTable;
     foreach x in oldTable do (
	  p := x;
	  while true do (
	       if p == p.next then break;
	       hmod := int(p.hash & mask);
	       newTable.hmod = KeyValuePair(p.key,p.hash,p.value,newTable.hmod);
	       p = p.next;)));
shrink(object:HashTable):void := (
     oldTable := object.table;
     newlen := length(oldTable)/2;
     mask := newlen - 1;
     newTable := new array(KeyValuePair) len newlen do provide bucketEnd;
     object.table = newTable;
     foreach x in oldTable do (
	  p := x;
	  while true do (
	       if p == p.next then break;
	       hmod := int(p.hash & mask);
	       newTable.hmod = KeyValuePair(p.key,p.hash,p.value,newTable.hmod);
	       p = p.next;)));
hashfun(e:Expr):Expr := Expr(toInteger(int(hash(e))));
setupfun("hash",hashfun);
export toExpr(h:int):Expr := Expr(toInteger(h));
export toExpr(h:ulong):Expr := Expr(toInteger(h));
mutablefun(e:Expr):Expr := Expr(toExpr(
     	  when e is o:HashTable do o.mutable
     	  is x:List do x.mutable
     	  is s:SymbolClosure do !s.symbol.protected
     	  is d:DictionaryClosure do !d.dictionary.protected
     	  is x:Database do x.mutable
     	  else false));
setupfun("mutable",mutablefun);
export equal(lhs:Expr,rhs:Expr):Expr;
export lookup1(object:HashTable,key:Expr,keyhash:int):Expr := (
     -- warning: can return notfoundE, which should not be given to the user
     keymod := int(keyhash & (length(object.table)-1));
     bucket := object.table.keymod;
     while bucket != bucket.next do (
	  if bucket.key == key || bucket.hash == keyhash && equal(bucket.key,key)==True then return bucket.value;
	  bucket = bucket.next;
	  );
     notfoundE);
export lookup1(object:HashTable,key:Expr):Expr := lookup1(object,key,hash(key));
export lookup1force(object:HashTable,key:Expr,keyhash:int):Expr := (
     keymod := int(keyhash & (length(object.table)-1));
     bucket := object.table.keymod;
     while bucket != bucket.next do (
	  if bucket.key == key || bucket.hash == keyhash && equal(bucket.key,key)==True then return bucket.value;
	  bucket = bucket.next;
	  );
     buildErrorPacket("key not found in hash table"));
export lookup1force(object:HashTable,key:Expr):Expr := lookup1force(object,key,hash(key));
export lookup1Q(object:HashTable,key:Expr,keyhash:int):bool := (
     keymod := int(keyhash & (length(object.table)-1));
     bucket := object.table.keymod;
     while bucket != bucket.next do (
	  if bucket.key == key || bucket.hash == keyhash && equal(bucket.key,key)==True then return true;
	  bucket = bucket.next;
	  );
     false);
export lookup1Q(object:HashTable,key:Expr):bool := lookup1Q(object,key,hash(key));
export lookup(object:HashTable,key:Expr,keyhash:int):Expr := (
     while true do (
     	  keymod := int(keyhash & (length(object.table)-1));
     	  bucket := object.table.keymod;
     	  while bucket != bucket.next do (
	       if bucket.key == key || bucket.hash == keyhash && equal(bucket.key,key)==True then return bucket.value;
	       bucket = bucket.next;
	       );
	  if object == thingClass then break;
	  object = object.parent;
	  );
     nullE);	  
export lookup(object:HashTable,key:Expr):Expr := lookup(object,key,hash(key));
export lookup(object:HashTable,key:SymbolClosure):Expr := (
     lookup(object,Expr(key),key.symbol.hash)
     );
export ancestor(o:HashTable,p:HashTable):bool := (
     while true do (
	  if o == p then return true;
	  if o == thingClass then return false;
	  o = o.parent;
	  ));
equal(x:HashTable,y:HashTable):Expr := (
     if x == y then return True;
     if x.hash != y.hash then return False;
     if x.class != y.class || x.parent != y.parent then return False;
     if x.hash == 0 && x.class == cacheTableClass then return True; -- cache tables have hash code 0
     if x.mutable || y.mutable then return False;
     if x.numEntries != y.numEntries || length(x.table) != length(y.table) then return False;
     foreach a at i in x.table do (
	  p := a;
	  q := y.table.i;
	  if p.next == q.next then (
	       -- p.next and q.next must both be bucketEnd
	       if p.hash != q.hash 
	       || p.key != q.key && False == equal(p.key,q.key) 
	       || p.value != q.value && False == equal(p.value,q.value) 
	       then return False;)
	  else (
	       plen := 0; pp := p; while pp != pp.next do (pp=pp.next; plen=plen+1);
	       qlen := 0; qq := q; while qq != qq.next do (qq=qq.next; qlen=qlen+1);
	       if plen != qlen then return False;
	       while true do (
		    if p == p.next then break;
		    z := q;
		    while true do (
			 if z.key == p.key
			 || z.hash == p.hash && equal(z.key,p.key)==True
			 then (
			      if z.value == p.value
			      || True == equal(z.value, p.value)
			      then break
			      else return False;
			      );
			 z = z.next;
			 if z == z.next then return False;
			 );
		    p = p.next;
		    )));
     True);

export equal(lhs:Expr,rhs:Expr):Expr := (
     if lhs == rhs then True else 
     when lhs
     is Error do lhs
     is x:List do (
	  when rhs
	  is y:List do (
     	       if x.hash != y.hash
	       || x.mutable 
	       || y.mutable
	       || length(x.v) != length(y.v)
	       || x.class != y.class && False == equal(x.class,y.class) 
	       then False
	       else (
		    foreach z at i in x.v do (
			 if equal(z,y.v.i) == False then return False;
			 );
		    True ) )
	  else False)
     is x:ZZ do (
	  when rhs 
	  is y:ZZ do (
	       if x === y then True else False
	       )
	  -- other cases needed soon
	  else False)
     is x:HashTable do (
	  when rhs
	  is y:HashTable do equal(x,y)
	  else False)		    
     is x:string do (
	  when rhs 
	  is y:string do if x === y then True else False
	  else False
	  )
     is x:Net do (
	  when rhs
	  is y:Net do if x === y then True else False
	  else False)
     is x:Sequence do (
	  when rhs
	  is y:Sequence do (
	       if length(x) != length(y)
	       then False
	       else (
		    foreach z at i in x do (
			 if equal(z,y.i) == False then return False;
			 );
		    True))
	  else False)
     is Boolean do False
     is Nothing do False
     is file do False
     is CompiledFunction do False
     is CompiledFunctionClosure do False
     is a:DictionaryClosure do (
	  when rhs
	  is b:DictionaryClosure do (
	       if a.frame == b.frame
	       && a.dictionary == b.dictionary		    -- strictly speaking, this second part is redundant
	       then True else False
	       )
	  else False)
     is x:QQ do (
	  when rhs
	  is y:QQ do (
	       if x === y then True else False
	       )
	  -- other cases needed soon
	  else False)
     is c:CodeClosure do (
	  when rhs
	  is d:CodeClosure do (
	       if c.frame == d.frame
	       && c.code == d.code
	       then True else False)
	  else False
	  )
     is x:RR do (
	  when rhs
	  is y:RR do (
	       if strictequality(x,y) then True else False
	       )
	  else False)
     is x:CC do (
	  when rhs
	  is y:CC do (
	       if strictequality(x,y) then True else False
	       )
	  else False)
     is x:SymbolClosure do (
	  when rhs 
	  is y:SymbolClosure do (
       	       if x === y
	       then True else False
	       )
	  else False
	  )
     is FunctionClosure do False
     is NetFile do False
     is x:RawMonomialOrdering do False
     is x:RawMonoid do False
     is x:RawMonomial do (
	  when rhs
	  is y:RawMonomial do (
	       if Ccode(bool, "IM2_Monomial_is_equal((Monomial *)",x,",(Monomial *)",y,")")
	       then True else False
	       )
	  else False
	  )
     is x:RawRing do False
     is x:RawMonomialIdeal do (
	  when rhs
	  is y:RawMonomialIdeal do (
	       r := Ccode(int, "IM2_MonomialIdeal_is_equal((MonomialIdeal *)",x,",(MonomialIdeal *)",y,")");
	       if r == -1 then engineErrorMessage() else toExpr(r == 1))
	  else False
	  )
     is x:RawRingElement do (
	  when rhs
	  is y:RawRingElement do (
	       if Ccode(bool, "IM2_RingElement_is_equal((RingElement *)",x,",(RingElement *)",y,")")
	       then True else False
	       )
	  else False
	  )
     is x:RawFreeModule do (
	  when rhs
	  is y:RawFreeModule do (
	       if Ccode(bool, "IM2_FreeModule_is_equal((FreeModule *)",x,",(FreeModule *)",y,")")
	       then True else False
	       )
	  else False
	  )
     is x:RawMatrix do (
	  when rhs
	  is y:RawMatrix do toExpr(Ccode(bool, "IM2_Matrix_is_equal((Matrix *)",x,",(Matrix *)",y,")"))
	  else False
	  )
     is x:RawMutableMatrix do False			    -- mutable matrices may not stay equal, so they aren't equal
     is x:RawComputation do (
	  when rhs
	  is y:RawComputation do (
	       False
	       -- toExpr(Ccode(bool, "IM2_GB_is_equal((Computation *)",x,",(Computation *)",y,")"))
	       )
	  else False
	  )
     is functionCode do False
     is f:CompiledFunctionBody do when rhs is g:CompiledFunctionBody do if f.fn == g.fn then True else False else False
     is s:SpecialExpr do when rhs is t:SpecialExpr do if s.class == t.class && equal(s.e,t.e) == True then True else False else False
     is x:RawRingMap do (
	  when rhs
	  is y:RawRingMap do toExpr(Ccode(bool, "IM2_RingMap_is_equal((RingMap *)",x,",(RingMap *)",y,")"))
	  else False
	  )
     is Database do False
     );
export remove(x:HashTable,key:Expr):Expr := (
     if !x.mutable then (
	  return buildErrorPacket("attempted to modify an immutable hash table");
	  );
     h := hash(key);
     hmod := int(h & (length(x.table)-1));
     p := x.table.hmod;
     prev := p;
     while p != p.next do (
	  if p.key == key || equal(p.key,key)==True 
	  then (
	       if prev == p then x.table.hmod = p.next
	       else prev.next = p.next;
	       x.numEntries = x.numEntries - 1;
	       if 8 * x.numEntries == 3 * length(x.table) -- SEE BELOW
	       && length(x.table) > 4
	       -- 4 is the length of a new hash table, see tokens.d, newHashTable()
	       then shrink(x);
	       return Expr(x));
	  prev = p;
	  p = p.next);
     Expr(x));
export storeInHashTable(x:HashTable,key:Expr,h:int,value:Expr):Expr := (
     if !x.mutable then return buildErrorPacket("attempted to modify an immutable hash table");
     hmod := int(h & (length(x.table)-1));
     p := x.table.hmod;
     while p != p.next do (
	  if p.key == key || equal(p.key,key)==True 
	  then (
	       p.value = value; 
	       return value);
	  p = p.next);
     if 4 * x.numEntries == 3 * length(x.table) -- SEE ABOVE
     then (
	  enlarge(x);
	  hmod = int(h & (length(x.table)-1));
	  );
     x.numEntries = x.numEntries + 1;
     x.table.hmod = KeyValuePair(key,h,value,x.table.hmod);
     value);
export storeInHashTable(x:HashTable,key:Expr,value:Expr):Expr := storeInHashTable(x,key,hash(key),value);
export storeInHashTableNoClobber(x:HashTable,key:Expr,h:int,value:Expr):Expr := (
     -- derived from storeInHashTable above!
     if !x.mutable then return buildErrorPacket("attempted to modify an immutable hash table");
     hmod := int(h & (length(x.table)-1));
     p := x.table.hmod;
     while p != p.next do (
	  if p.key == key || equal(p.key,key)==True 
	  then return buildErrorPacket("collision of keys in hash table");
	  p = p.next);
     if 4 * x.numEntries == 3 * length(x.table) -- SEE ABOVE
     then (
	  enlarge(x);
	  hmod = int(h & (length(x.table)-1));
	  );
     x.numEntries = x.numEntries + 1;
     x.table.hmod = KeyValuePair(key,h,value,x.table.hmod);
     value);
export storeInHashTableNoClobber(x:HashTable,key:Expr,value:Expr):Expr := storeInHashTableNoClobber(x,key,hash(key),value);
export storeInHashTableMustClobber(x:HashTable,key:Expr,h:int,value:Expr):Expr := (
     if !x.mutable then return buildErrorPacket("attempted to modify an immutable hash table");
     hmod := int(h & (length(x.table)-1));
     p := x.table.hmod;
     while p != p.next do (
	  if p.key == key || equal(p.key,key)==True 
	  then (
	       p.value = value; 
	       return value);
	  p = p.next);
     buildErrorPacket("encountered an unknown key or option"
	  + when key is s:SymbolClosure do ": " + s.symbol.word.name 
	  is s:string do ": " + s
	  else ""
	  ));
export storeInHashTableMustClobber(x:HashTable,key:Expr,value:Expr):Expr := (
     storeInHashTableMustClobber(x,key,hash(key),value)
     );

bucketsfun(e:Expr):Expr := (
     when e
     is dc:DictionaryClosure do (
	  d := dc.dictionary;
	  f := dc.frame;
	  Expr(
	       list(
		    new Sequence len length(d.symboltable.buckets) do (
			 foreach b in d.symboltable.buckets do (
			      n := 0;
			      c := b;
			      while ( when c is null do false is cell:SymbolListCell do (c = cell.next; true) ) do n = n+1;
			      c = b;
			      provide list(
				   new Sequence len n do
				   while (
					when c
					is null do false
					is cell:SymbolListCell do (
					     provide Expr(Sequence(Expr(cell.word.name),SymbolClosure(f,cell.entry)));
					     c = cell.next;
					     true)
					)
				   do nothing))))))
     is h:HashTable do list(
	  new Sequence len length(h.table) do (
	       foreach pp in h.table do (
		    n := 0;
		    p := pp;
		    while true do (
			 if p == p.next then break;
			 n = n+1;
			 p = p.next);
		    p = pp;
		    s := new Sequence len n do (
			 provide Expr(Sequence(p.key, p.value));
			 p = p.next);
		    provide list(s))))
     else WrongArg("a hash table"));
setupfun("buckets",bucketsfun);
export Parent(e:Expr):HashTable := (
     when e
     is obj:HashTable do obj.parent
     is s:SpecialExpr do Parent(s.e)
     else nothingClass);
export parentfun(e:Expr):Expr := Expr(Parent(e));
setupfun("parent",parentfun);

export isglobaldict(d:Dictionary):bool := !d.transient && d.frameID == 0;

export Class(e:Expr):HashTable := (
     when e 
     is obj:HashTable do obj.class
     is x:List do x.class
     is ZZ do ZZClass
     is CodeClosure do codeClass
     is functionCode do functionBodyClass
     is CompiledFunctionBody do compiledFunctionBodyClass
     is QQ do QQClass
     is file do fileClass
     is dc:DictionaryClosure do if isglobaldict(dc.dictionary) then globalDictionaryClass else localDictionaryClass
     is string do stringClass
     is FunctionClosure do functionClosureClass
     is Net do netClass
     is NetFile do netFileClass
     is Error do errorClass
     is Sequence do sequenceClass
     is CompiledFunction do compiledFunctionClass
     is CompiledFunctionClosure do compiledFunctionClosureClass
     is s:SymbolClosure do if s.symbol.word.parse == parseWORD then symbolClass else keywordClass
     is RR do RRClass
     is CC do CCClass
     is RawComputation do rawComputationClass
     is Nothing do nothingClass
     is Database do dbClass
     is e:SpecialExpr do e.class
     is Boolean do booleanClass
     is RawMonomial do rawMonomialClass
     is RawMonomialOrdering do rawMonomialOrderingClass
     is RawMonoid do rawMonoidClass
     is RawMonomialIdeal do rawMonomialIdealClass
     is RawRing do rawRingClass
     is RawRingElement do rawRingElementClass
     is RawRingMap do rawRingMapClass
     is RawFreeModule do rawFreeModuleClass
     is RawMatrix do rawMatrixClass
     is RawMutableMatrix do rawMutableMatrixClass
     );
classfun(e:Expr):Expr := Expr(Class(e));
setupfun("class",classfun);
-- these couldn't have been right
--export lookup(e:Expr,key:Expr,keyhash:int):Expr := (
--     when e
--     is obj:HashTable do lookup(obj,key,keyhash)
--     else lookup(Class(e),key,keyhash));
--export lookup(e:Expr,key:Expr):Expr := (
--     when e
--     is obj:HashTable do lookup(obj,key)
--     else lookup(Class(e),key));
setupconst("Type",Expr(typeClass));
setupconst("Thing",Expr(thingClass));
setupconst("HashTable",Expr(hashTableClass));
setupconst("Dictionary",Expr(dictionaryClass));
setupconst("LocalDictionary",Expr(localDictionaryClass));
setupconst("GlobalDictionary",Expr(globalDictionaryClass));
setupconst("Pseudocode",Expr(codeClass));
setupconst("FunctionBody",Expr(functionBodyClass));
setupconst("CompiledFunctionBody",Expr(compiledFunctionBodyClass));
setupconst("MutableHashTable",Expr(mutableHashTableClass));
setupconst("CacheTable",Expr(cacheTableClass));
setupconst("BasicList",Expr(basicListClass));
setupconst("List",Expr(listClass));
setupconst("MutableList",Expr(mutableListClass));
setupconst("ZZ",Expr(ZZClass));
setupconst("QQ",Expr(QQClass));
setupconst("RingFamily",Expr(ringFamilyClass));
setupconst("InexactNumberType",Expr(inexactNumberTypeClass));
setupconst("RR",Expr(RRClass));
setupconst("CC",Expr(CCClass));
setupconst("Number",Expr(numberClass));
setupconst("InexactNumber",Expr(inexactNumberClass));
setupconst("RawObject",Expr(rawObjectClass));
setupconst("RawMonomial",Expr(rawMonomialClass));
setupconst("RawMonomialOrdering",Expr(rawMonomialOrderingClass));
setupconst("RawMonoid",Expr(rawMonoidClass));
setupconst("RawRing",Expr(rawRingClass));
setupconst("RawFreeModule",Expr(rawFreeModuleClass));
setupconst("RawMatrix",Expr(rawMatrixClass));
setupconst("RawMutableMatrix",Expr(rawMutableMatrixClass));
setupconst("RawComputation",Expr(rawComputationClass));
setupconst("RawRingElement",Expr(rawRingElementClass));
setupconst("RawRingMap",Expr(rawRingMapClass));
setupconst("RawMonomialIdeal",Expr(rawMonomialIdealClass));
setupconst("File",Expr(fileClass));
setupconst("String",Expr(stringClass));
setupconst("Function",Expr(functionClass));
setupconst("FunctionClosure",Expr(functionClosureClass));
setupconst("CompiledFunction",Expr(compiledFunctionClass));
setupconst("CompiledFunctionClosure",Expr(compiledFunctionClosureClass));
setupconst("Symbol",Expr(symbolClass));
setupconst("Keyword",Expr(keywordClass));
setupconst("Time",Expr(timeClass));
setupconst("Option",Expr(optionClass));
setupconst("OptionTable",Expr(optionTableClass));
setupconst("Net",Expr(netClass));
setupconst("NetFile",Expr(netFileClass));
setupconst("true",True);
setupconst("false",False);
setupconst("null",nullE);
setupconst("Boolean",Expr(booleanClass));
setupconst("Database",Expr(dbClass));
setupconst("Sequence",Expr(sequenceClass));
setupconst("VisibleList",Expr(visibleListClass));
setupconst("Array",Expr(arrayClass));
--setupconst("Error",Expr(errorMessageClass));
--setupconst("MissingMethod",Expr(missingMethodClass));
setupconst("Ring",Expr(ringClass));
setupconst("Nothing",Expr(nothingClass));

varstringarray := { a:array(string), n:int };
newvarstringarray(m:int):varstringarray := varstringarray( new array(string) len m do provide "", 0 );
append(v:varstringarray,s:string):varstringarray := (
     n := v.n;
     if length(v.a) == n then varstringarray( 
	  new array(string) len 2*n+1 do (
	       foreach t in v.a do provide t;
	       provide s;
	       while true do provide "";
	       ),
	  n+1)
     else (
     	  v.a.n = s;
     	  v.n = n+1;
     	  v));
extract(v:varstringarray):array(string) := new array(string) len v.n do foreach s in v.a do provide s;

export completions(s:string):array(string) := (
     n := length(s);
     if n == 0 then return array(string)();		    -- don't complete the null string
     v := newvarstringarray(6);
     d := globalDictionary;
     while (
	  foreach bucket in d.symboltable.buckets do (
	       b := bucket;
	       while true do when b
	       is null do break
	       is q:SymbolListCell do (
		    t := q.word.name;
		    if isalnum(t.0) && n <= length(t) && 0 == strncmp(s,t,n) then v=append(v,t);
		    b = q.next; ));
	  d != d.outerDictionary) do d = d.outerDictionary;
     extract(v));

export commonAncestor(x:HashTable,y:HashTable):HashTable := (
     if x == y then return x;
     t := x;
     while t != thingClass do ( t.numEntries = - 1 - t.numEntries; t = t.parent; );
     a := thingClass;
     t = y;
     while t != thingClass do (
	  if t.numEntries < 0 then ( a = t; break; );
	  t = t.parent;
     	  );
     t = x;
     while t != thingClass do ( t.numEntries = - 1 - t.numEntries; t = t.parent; );
     a);
-----------------------------------------------------------------------------

assignmentMethodP(meth:Expr):bool := (
     when meth is s:Sequence do true 			-- something like (OP, symbol =), where OP is a symbol such as symbol +
     is sym:SymbolClosure do (
	  sym === LeftArrowS	    	-- symbol <- , which is an assignment operator in its own right  -- this should go away!!!
     	  ||
	  sym === GlobalAssignS	        -- global assignment, dispatched internally (it's like    GlobalAssignHook X = (X,x) -> ...    )
	  )
     else false);

ernary(n:int):string := (
     if n == 4 then "quaternary"
     else if n == 3 then "ternary"
     else if n == 2 then "binary"
     else if n == 1 then "unary"
     else tostring(n)+"-ary");

export typicalValues := newHashTable(mutableHashTableClass,nothingClass);
messx := "expected method to be a function or TYPE => function'";
installIt(numtypes:int,assgnmt:bool,h:HashTable,key:Expr,value:Expr):Expr := (
     when value
     is Error do value
     is fc:FunctionClosure do (
	  numparms := if assgnmt then numtypes+1 else numtypes;
	  if numtypes!=-1 && !fc.model.desc.restargs && fc.model.desc.numparms!=numparms
	  then buildErrorPacket( "expected method for " + ernary(numtypes)
	       + (if assgnmt then " assignment" else "") + " operator to be a function of "
	       + tostring(numparms) + if numparms == 1 then " variable" else " variables" )
	  else storeInHashTable(h,key,value))
     is CompiledFunction do storeInHashTable(h,key,value)
     is CompiledFunctionClosure do storeInHashTable(h,key,value)
     is s:SpecialExpr do if ancestor(s.class,functionClass) then storeInHashTable(h,key,value) else buildErrorPacket(messx)
     is x:List do (
	  if x.class == optionClass && length(x.v) == 2 then (
	       installIt(numtypes,assgnmt,h,key,x.v.1);
	       storeInHashTable(typicalValues,
		    if numtypes == 1 then Expr(Sequence(key,Expr(h))) else key,
		    x.v.0);
	       value)
	  else buildErrorPacket(messx))
     else buildErrorPacket(messx));
-----------------------------------------------------------------------------
-- nullary methods
export NullaryMethods := (
     x := newHashTable(mutableHashTableClass,nothingClass);
     sethash(x,true);
     x);
setupconst("nullaryMethods",Expr(NullaryMethods));
export lookup(e:Expr):Expr := (
     r := lookup1(NullaryMethods,Expr(Sequence(e)));
     if r == notfoundE then r = nullE;
     r);
export lookup0(e:Expr):Expr := (			    -- should be a length 1 sequence
     r := lookup1(NullaryMethods,e);
     if r == notfoundE then r = nullE;
     r);
export installMethod(meth:Expr,value:Expr):Expr := (
     when value is Error do value
     is fc:FunctionClosure do (
	  if !fc.model.desc.restargs && fc.model.desc.numparms!=0
	  then buildErrorPacket("expected method for nullary operator to be a function of 0 variables")
     	  else storeInHashTable(NullaryMethods,Expr(Sequence(meth)),value))
     is CompiledFunction do storeInHashTable(NullaryMethods,Expr(Sequence(meth)),value)
     is CompiledFunctionClosure do storeInHashTable(NullaryMethods,Expr(Sequence(meth)),value)
     is s:SpecialExpr do if ancestor(s.class,functionClass) then (
	  -- could check the function takes 0 arguments here...
	  storeInHashTable(NullaryMethods,Expr(Sequence(meth)),value)
	  ) else buildErrorPacket(messx)
     is x:List do (
	  if x.class == optionClass && length(x.v) == 2 then (
	       storeInHashTable(typicalValues,Expr(Sequence(meth)),x.v.0);
	       installMethod(meth,x.v.1);
	       value)
	  else buildErrorPacket(messx))
     else buildErrorPacket(messx));
-----------------------------------------------------------------------------
-- unary methods
export installMethod(meth:Expr,s:HashTable,value:Expr):Expr := installIt(1, assignmentMethodP(meth), s, meth, value);
key1 := Sequence(nullE,nullE);
key1E := Expr(key1);
export lookupUnaryValue(s:HashTable,meth:Expr,methhash:int):Expr := (
     -- warning: can return notfoundE, which should not be given to the user
     key1.0 = Expr(s);
     key1.1 = meth;
     -- the big numbers here are the same as in hash() for sequences in structure.d
     lookup1(s, key1E, (27449 * 27457 + s.hash) * 27457 + methhash));
-----------------------------------------------------------------------------
-- binary methods
export installMethod(meth:Expr,lhs:HashTable,rhs:HashTable,value:Expr):Expr := (
     installIt(2, assignmentMethodP(meth),
	  if lhs.hash > rhs.hash then lhs else rhs,
	  Expr(Sequence(meth,Expr(lhs),Expr(rhs))),
	  value));

export installMethod(s:SymbolClosure,X:HashTable,f:fun):Expr := (
     installMethod(Expr(s),X,Expr(CompiledFunction(f,nextHash())))
     );
export installMethod(s:SymbolClosure,X:HashTable,Y:HashTable,f:fun):Expr := (
     installMethod(Expr(s),X,Y,Expr(CompiledFunction(f,nextHash())))
     );

key2 := Sequence(nullE,nullE,nullE);
key2E := Expr(key2);
export lookupBinaryMethod(lhs:HashTable,rhs:HashTable,meth:Expr,methhash:int):Expr := (
     key2.0 = meth;
     -- the big numbers here are the same as in hash() for sequences in structure.d
     keyhash0 := 27449 * 27457 + methhash;
     while true do (			  -- loop through ancestors of lhs
	  key2.1 = Expr(lhs);
	  lefthash := lhs.hash;
	  keyhash1 := keyhash0 * 27457 + lefthash;
	  rhsptr := rhs;
	  while true do (		  -- loop through ancestors of rhs
	       key2.2 = Expr(rhsptr);
     	       righthash := rhsptr.hash;
	       keyhash := keyhash1 * 27457 + righthash;
	       s := lookup1(
		    if lefthash > righthash then lhs else rhsptr,
		    key2E, keyhash);
	       if s != notfoundE then return s;
	       if rhsptr == thingClass then break;
	       rhsptr = rhsptr.parent;
	       );
     	  if lhs == thingClass then break;
	  lhs = lhs.parent;
	  );
     nullE);
export lookupBinaryValue(lhs:HashTable,rhs:HashTable,meth:Expr,methhash:int):Expr := (
     -- warning: can return notfoundE, which should not be given to the user
     key2.0 = Expr(lhs);
     key2.1 = Expr(rhs);
     key2.2 = meth;
     -- the big numbers here are the same as in hash() for sequences in structure.d
     keyhash := ((27449 * 27457 + lhs.hash) * 27457 + rhs.hash) * 27457 + methhash;
     lookup1(if lhs.hash > rhs.hash then lhs else rhs, key2E, keyhash));
export lookupBinaryMethod(lhs:HashTable,rhs:HashTable,meth:Expr):Expr := (
     lookupBinaryMethod(lhs,rhs,meth,hash(meth)));
export lookupBinaryMethod(lhs:HashTable,rhs:HashTable,meth:SymbolClosure):Expr := (
     lookupBinaryMethod(lhs,rhs,Expr(meth),meth.symbol.hash));
export lookupBinaryValue(lhs:HashTable,rhs:HashTable,meth:Expr):Expr := (
     lookupBinaryValue(lhs,rhs,meth,hash(meth)));
export lookupBinaryValue(lhs:HashTable,rhs:HashTable,meth:SymbolClosure):Expr := (
     lookupBinaryValue(lhs,rhs,Expr(meth),meth.symbol.hash));
-----------------------------------------------------------------------------
-- ternary methods
export installMethod(meth:Expr,s1:HashTable,s2:HashTable,s3:HashTable,value:Expr):Expr := (
     installIt(3, assignmentMethodP(meth),
	  if s1.hash > s2.hash then (
	       if s1.hash > s3.hash then s1 else s3
	       )
	  else (
	       if s2.hash > s3.hash then s2 else s3
	       ),
	  Expr(Sequence(meth,Expr(s1),Expr(s2),Expr(s3))), 
	  value));
key3 := Sequence(nullE,nullE,nullE,nullE);
key3E := Expr(key3);
export lookupTernaryMethod(s1:HashTable,s2:HashTable,s3:HashTable,meth:Expr,methhash:int):Expr := (
     key3.0 = meth;
     -- the big numbers here are the same is in hash() for sequences in structure.d
     keyhash0 := 27449 * 27457 + methhash;
     while true do (			  -- loop through ancestors of s1
	  key3.1 = Expr(s1);
	  s1hash := s1.hash;
	  keyhash1 := keyhash0 * 27457 + s1hash;
	  s2ptr := s2;
	  while true do (		  -- loop through ancestors of s2
	       key3.2 = Expr(s2ptr);
     	       s2hash := s2ptr.hash;
	       keyhash2 := keyhash1 * 27457 + s2hash;
	       s3ptr := s3;
	       while true do (		  -- loop through ancestors of s3
		    key3.3 = Expr(s3ptr);
		    s3hash := s3ptr.hash;
		    keyhash3 := keyhash2  * 27457 + s3hash;
	       	    s := lookup1(
			 if s1hash > s2hash then (
			      if s1hash > s3hash then s1 else s3ptr
			      )
			 else (
			      if s2hash > s3hash then s2ptr else s3ptr
			      ),
		    	 key3E, keyhash3);
	       	    if s != notfoundE then (
			 key3.0 = nullE;
			 key3.1 = nullE;
			 key3.2 = nullE;
			 key3.3 = nullE;
			 return s;
			 );
		    if s3ptr == thingClass then break;
		    s3ptr = s3ptr.parent;
		    );
	       if s2ptr == thingClass then break;
	       s2ptr = s2ptr.parent;
	       );
     	  if s1 == thingClass then break;
	  s1 = s1.parent;
	  );
     key3.0 = nullE;
     key3.1 = nullE;
     key3.2 = nullE;
     key3.3 = nullE;
     nullE);
export lookupTernaryMethod(s1:HashTable,s2:HashTable,s3:HashTable,meth:Expr):Expr := (
     lookupTernaryMethod(s1,s2,s3,meth,hash(meth))
     );
-----------------------------------------------------------------------------
-- quaternary methods
export installMethod(meth:Expr,s1:HashTable,s2:HashTable,s3:HashTable,s4:HashTable,value:Expr):Expr := (
     installIt(4, assignmentMethodP(meth),
	  if s1.hash > s2.hash
	  then if s1.hash > s3.hash 
	  then if s1.hash < s4.hash then s4 else s1
	  else if s3.hash < s4.hash then s4 else s3
	  else if s2.hash > s3.hash 
	  then if s2.hash < s4.hash then s4 else s2 
	  else if s3.hash < s4.hash then s4 else s3,
	  Expr(Sequence(meth,Expr(s1),Expr(s2),Expr(s3),Expr(s4))), 
	  value));
key4 := Sequence(nullE,nullE,nullE,nullE,nullE);
key4E := Expr(key4);
export lookupQuaternaryMethod(s1:HashTable,s2:HashTable,s3:HashTable,s4:HashTable,meth:Expr,methhash:int):Expr := (
     key4.0 = meth;
     -- the big numbers here are the same is in hash() for sequences in structure.d
     keyhash0 := 27449 * 27457 + methhash;
     while true do (			  -- loop through ancestors of s1
	  key4.1 = Expr(s1);
	  s1hash := s1.hash;
	  keyhash1 := keyhash0 * 27457 + s1hash;
	  s2ptr := s2;
	  while true do (		  -- loop through ancestors of s2
	       key4.2 = Expr(s2ptr);
     	       s2hash := s2ptr.hash;
	       keyhash2 := keyhash1 * 27457 + s2hash;
	       s3ptr := s3;
	       while true do (		  -- loop through ancestors of s3
		    key4.3 = Expr(s3ptr);
		    s3hash := s3ptr.hash;
		    keyhash3 := keyhash2  * 27457 + s3hash;
		    s4ptr := s4;
		    while true do (		  -- loop through ancestors of s4
			 key4.4 = Expr(s4ptr);
			 s4hash := s4ptr.hash;
			 keyhash4 := keyhash3  * 27457 + s4hash;
			 s := lookup1(
			      if s1hash > s2hash then (
				   if s1hash > s3hash 
				   then if s4hash > s1hash then s4ptr else s1 
				   else if s4hash > s3hash then s4ptr else s3ptr
				   )
			      else (
				   if s2hash > s3hash 
				   then if s4hash > s2hash then s4ptr else s2ptr 
				   else if s4hash > s3hash then s4ptr else s3ptr
				   ),
			      key4E, keyhash4);
			 if s != notfoundE then ( key4.0 = nullE; key4.1 = nullE; key4.2 = nullE; key4.3 = nullE; return s; );
			 if s4ptr == thingClass then break;
			 s4ptr = s4ptr.parent;
			 );
		    if s3ptr == thingClass then break;
		    s3ptr = s3ptr.parent;
		    );
	       if s2ptr == thingClass then break;
	       s2ptr = s2ptr.parent;
	       );
     	  if s1 == thingClass then break;
	  s1 = s1.parent;
	  );
     key4.0 = nullE; key4.1 = nullE; key4.2 = nullE; key4.3 = nullE;
     nullE);
export lookupQuaternaryMethod(s1:HashTable,s2:HashTable,s3:HashTable,s4:HashTable,meth:Expr):Expr := lookupQuaternaryMethod(s1,s2,s3,s4,meth,hash(meth));
-----------------------------------------------------------------------------
installfun(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then installMethod(a.0,a.1)
	  else if length(a) == 3 then (
	       when a.1 is s:HashTable do
	       if s.mutable then installMethod(a.0,s,a.2)
	       else WrongArg(1+1,"a mutable hash table")
	       else WrongArg(1+1,"a hash table"))
	  else if length(a) == 4 then (
	       when a.1
	       is lhs:HashTable do
	       if lhs.mutable then
	       when a.2
	       is rhs:HashTable do
	       if rhs.mutable then installMethod(a.0,lhs,rhs,a.3)
	       else WrongArg(2+1,"a mutable hash table")
	       else WrongArg(2+1,"a hash table")
	       else WrongArg(1+1,"a mutable hash table")
	       else WrongArg(1+1,"a hash table"))
	  else if length(a) == 5 then (
	       when a.1
	       is s1:HashTable do
	       if s1.mutable then
	       when a.2
	       is s2:HashTable do
	       if s2.mutable then 
	       when a.3
	       is s3:HashTable do
	       if s3.mutable then installMethod(a.0,s1,s2,s3,a.4)
	       else WrongArg(3+1,"a mutable hash table")
	       else WrongArg(3+1,"a hash table")
	       else WrongArg(2+1,"a mutable hash table")
	       else WrongArg(2+1,"a hash table")
	       else WrongArg(1+1,"a mutable hash table")
	       else WrongArg(1+1,"a hash table"))
	  else WrongNumArgs(3,5))
     else WrongNumArgs(3,5));
setupfun("installMethod",installfun);
-----------------------------------------------------------------------------
export lookupfun(e:Expr):Expr := (
     when e 
     is a:Sequence do
     if length(a) == 1 then lookup0(e)
     else if length(a) == 2 then
     when a.1
     is s:HashTable do lookup(s,a.0)
     else nullE
     else if length(a)==3 then
     when a.1
     is lhs:HashTable do
     when a.2
     is rhs:HashTable do lookupBinaryMethod(lhs,rhs,a.0)
     else nullE
     else nullE
     else if length(a) == 4 then
     when a.1 is s1:HashTable do
     when a.2 is s2:HashTable do 
     when a.3 is s3:HashTable do lookupTernaryMethod(s1,s2,s3,a.0)
     else nullE
     else nullE
     else nullE
     else if length(a) == 5 then
     when a.1 is s1:HashTable do
     when a.2 is s2:HashTable do 
     when a.3 is s3:HashTable do
     when a.4 is s4:HashTable do lookupQuaternaryMethod(s1,s2,s3,s4,a.0)
     else nullE
     else nullE
     else nullE
     else nullE
     else nullE
     -- these next ones are sort of the 0-argument case of lookup
     is CompiledFunctionClosure do e
     is FunctionClosure do e
     is CompiledFunction do e
     is s:SpecialExpr do if ancestor(s.class,functionClass) then e else nullE
     -- but let's see if we can do without this one, since there aren't any nullary operators
     -- is SymbolClosure do e
     else nullE);
setupfun("lookup",lookupfun);	  

export toHashTableError(i:int):Expr := buildErrorPacket(
     "expected element at position "+tostring(i)+" to be a pair");
export toHashTable(v:Sequence):Expr := (
     o := newHashTable(hashTableClass,nothingClass);
     foreach e at i in v do (
	  when e
	  is Nothing do nothing
	  is pair:Sequence do (
	       if length(pair) == 2 
	       then (storeInHashTable(o,pair.0,pair.1);)
	       else return toHashTableError(i))
	  is z:List do (
	       pair := z.v;
	       if length(pair) == 2 
	       then (storeInHashTable(o,pair.0,pair.1);)
	       else return toHashTableError(i))
	  else return toHashTableError(i));
     sethash(o,false);
     Expr(o));

newtypeof(parent:HashTable):HashTable := newHashTable(typeClass,parent);
export Tally := newtypeof(hashTableClass);
setupconst("Tally",Expr(Tally));
export Set := newtypeof(Tally);
one := Expr(toInteger(1));
setupconst("Set",Expr(Set));
makeSet(v:Sequence):Expr := (
     o := newHashTable(Set,nothingClass);
     foreach e in v do storeInHashTable(o,e,one);
     sethash(o,false);
     Expr(o));
makeSet(e:Expr):Expr := (
     when e
     is v:Sequence do makeSet(v)
     is w:List do if ancestor(w.class,visibleListClass) then makeSet(w.v)
     else WrongArg("a visible list")
     else WrongArg("a visible list"));
setupfun("set",makeSet);

modify(object:HashTable,key:Expr,f:function(Expr):Expr,v:Expr):void := (
     keyhash:= hash(key);
     keymod := int(keyhash & (length(object.table)-1));
     bucket := object.table.keymod;
     while bucket != bucket.next do (
	  if bucket.key == key || bucket.hash == keyhash && equal(bucket.key,key)==True then (
	       bucket.value = f(bucket.value); -- notice: no error checking here
	       return;
	       );
	  bucket = bucket.next;
	  );
     storeInHashTable(object,key,keyhash,v);
     );
addone(i:Expr):Expr := when i is j:ZZ do Expr(j+1) else i;
makeTally(v:Sequence):Expr := (
     o := newHashTable(Tally,nothingClass);
     foreach e at i in v do modify(o,e,addone,one);
     sethash(o,false);
     Expr(o));
makeTally(e:Expr):Expr := (
     when e
     is v:Sequence do makeTally(v)
     is w:List do if ancestor(w.class,visibleListClass) then makeTally(w.v)
     else WrongArg("a visible list")
     else WrongArg("a list or sequence"));
setupfun("tally",makeTally);

export keys(o:HashTable):Expr := list(
     new Sequence len o.numEntries do
     foreach bucket in o.table do (
	  p := bucket;
	  while p != p.next do (
	       provide Expr(p.key);
	       p = p.next;
	       )
	  )
     );
export keys(f:Database):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     x := newHashTable(mutableHashTableClass,nothingClass);
     k := dbmfirst(f.handle);
     continue := true;
     while continue do (
	  when k
	  is key:string do (
	       storeInHashTable(x,Expr(key),True);
	       k = dbmnext(f.handle);
	       )
	  else continue = false;
	  );
     keys(x));
export keys(o:Dictionary):Expr := Expr(
     list(
	  new Sequence len o.symboltable.numEntries do
	  foreach bucket in o.symboltable.buckets do (
	       p := bucket;
	       while true do (
		    when p
		    is q:SymbolListCell do (
			 provide Expr(q.word.name);
			 p=q.next;)
		    else break;))));

export lookup(s:string,table:SymbolHashTable):(null or Symbol) := (
     if table == dummySymbolHashTable then error("dummy table used");
     entryList := table.buckets.( hash(s) & (length(table.buckets)-1) );
     while true do
     when entryList
     is null do return NULL
     is entryListCell:SymbolListCell do (
	  if 0 == strcmp(entryListCell.word.name, s)
	  then return entryListCell.entry;
	  entryList = entryListCell.next));

getvalue(x:Sequence,i:int):Expr := (
     if i < -length(x) || i >= length(x)
     then buildErrorPacket("array index "
	  + tostring(i)
	  + " out of bounds 0 .. "
	  + tostring(length(x)-1))
     else (
	  if i < 0
	  then x.(length(x) + i)
	  else x.i));
export subvalue(left:Expr,right:Expr):Expr := (
     -- don't change this without changing subvalueQ below
     when left is x:Sequence do (
	  when right is r:ZZ do (
	       if isInt(r) then getvalue(x,toInt(r))
	       else buildErrorPacket("array index "
		    + tostring(r)
		    + " out of bounds 0 .. "
		    + tostring(length(x)-1)))
	  else buildErrorPacket("expected subscript to be an integer"))
     is x:HashTable do lookup1force(x,right)
     is f:Database do (
	  when right
	  is key:string do (
	       if !f.isopen then return buildErrorPacket("database closed");
	       when dbmfetch(f.handle,key)
	       is a:string do Expr(a)
	       else buildErrorPacket("encountered missing value"))
	  else buildErrorPacket("expected a string as key to database"))
     is x:List do (
	  when right is r:ZZ do (
	       if isInt(r) then getvalue(x.v,toInt(r))
	       else buildErrorPacket("array index "
		    + tostring(r)
		    + " out of bounds 0 .. "
		    + tostring(length(x.v)-1)))
	  else buildErrorPacket("array index not an integer"))
     is dc:DictionaryClosure do (
	  when right is s:string do (
	       d := dc.dictionary;
	       f := dc.frame;
	       when lookup(s,d.symboltable)
	       is x:Symbol do Expr(SymbolClosure(f,x))
	       else buildErrorPacket("key not found in dictionary")
	       )
	  else buildErrorPacket("expected subscript to be a string")
	  )
     is x:string do (
	  when right is r:ZZ do (
	       if isInt(r) then (
		    rr := toInt(r);
		    if rr < 0 then rr = rr + length(x);
		    if rr < 0 || rr >= length(x) 
		    then buildErrorPacket("string index out of bounds")
		    else Expr(string(x.rr)))
	       else buildErrorPacket("string index out of bounds"))
	  else buildErrorPacket("expected subscript to be an integer"))
     is n:Net do (
	  x := n.body;
	  when right is r:ZZ do (
	       if isInt(r) then (
		    rr := toInt(r);
		    if rr < 0 then rr = rr + length(x);
		    if rr < 0 || rr >= length(x) 
		    then buildErrorPacket("net row index out of bounds")
		    else Expr(x.rr))
	       else buildErrorPacket("net row index out of bounds"))
	  else buildErrorPacket("expected subscript to be an integer"))
     else buildErrorPacket("expected a list, hash table, or sequence"));
export subvalueQ(left:Expr,right:Expr):Expr := (
     -- don't change this without changing subvalue above
     when left is x:Sequence do (
	  when right is r:ZZ do (
	       if isInt(r) then (
	       	    i := toInt(r);
		    if i < -length(x) || i >= length(x) then False else True
		    )
	       else False)
	  else False)
     is x:HashTable do if lookup1Q(x,right) then True else False
     is dc:DictionaryClosure do (
	  d := dc.dictionary;
	  when right is s:string do when lookup(s,d.symboltable) is Symbol do True else False
	  else buildErrorPacket("expected subscript to be a string")
	  )
     is x:Database do (
	  when right
	  is key:string do dbmquery(x,key)
	  else buildErrorPacket("expected a string as key to database"))
     is x:List do (
	  when right is r:ZZ do (
	       if isInt(r) then (
	       	    i := toInt(r);
		    if i < -length(x.v) || i >= length(x.v) then False else True
		    )
	       else False)
	  else False)
     is x:string do (
	  when right is r:ZZ do (
	       if isInt(r) then (
		    rr := toInt(r);
		    if rr < 0 || rr >= length(x) 
		    then False
		    else True)
	       else False)
	  else False)
     is n:Net do (
	  x := n.body;
	  when right is r:ZZ do (
	       if isInt(r) then (
		    rr := toInt(r);
		    if rr < 0 || rr >= length(x) 
		    then False
		    else True)
	       else False)
	  else False)
     is Nothing do False				    -- we think of "null" as a universal gadget with no parts
     else WrongArg(1,"null, a sequence, list, string, net, hashtable, database, or dictionary"));


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
