--		Copyright 1994 by Daniel R. Grayson

use C;
use system;
use strings;
use stdio;
use gmp;
use nets;
use engine;
use tokens;
use basic;
use convertr;
use struct;
use binding;

enlarge(object:HashTable):void := (
     oldTable := object.table;
     newlen := 2*length(oldTable);
     mask := newlen - 1;
     newTable := new array(KeyValuePair) len newlen do provide bucketEnd;
     object.table = newTable;
     foreach x in oldTable do (
	  p := x;
	  while true do (
	       if p == bucketEnd then break;
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
	       if p == bucketEnd then break;
	       hmod := int(p.hash & mask);
	       newTable.hmod = KeyValuePair(p.key,p.hash,p.value,newTable.hmod);
	       p = p.next;)));
hashfun(e:Expr):Expr := Expr(toInteger(int(hash(e))));
setupfun("hash",hashfun);
export toExpr(h:int):Expr := Expr(toInteger(int(h)));
mutablefun(e:Expr):Expr := Expr(toExpr(
     	  when e is o:HashTable do o.mutable
     	  is x:List do x.mutable
     	  is s:SymbolClosure do !s.symbol.protected
     	  is x:Database do x.mutable
     	  else false));
setupfun("mutable",mutablefun);
export equal(lhs:Expr,rhs:Expr):Expr;
export lookup1(object:HashTable,key:Expr,keyhash:int):Expr := (
     -- warning: can return notfoundE, which should not be given to the user
     keymod := int(keyhash & (length(object.table)-1));
     bucket := object.table.keymod;
     if bucket.key == key then return(bucket.value);
     if bucket.next.key == key then return(bucket.next.value);
     while bucket != bucketEnd do (
	  if bucket.key == key
	  || bucket.hash == keyhash && equal(bucket.key,key)==True
	  then return(bucket.value);
	  bucket = bucket.next;
	  );
     notfoundE);
export lookup1(object:HashTable,key:Expr):Expr := lookup1(object,key,hash(key));
export lookup1force(object:HashTable,key:Expr,keyhash:int):Expr := (
     keymod := int(keyhash & (length(object.table)-1));
     bucket := object.table.keymod;
     if bucket.key == key then return(bucket.value);
     if bucket.next.key == key then return(bucket.next.value);
     while bucket != bucketEnd do (
	  if bucket.key == key
	  || bucket.hash == keyhash && equal(bucket.key,key)==True
	  then return(bucket.value);
	  bucket = bucket.next;
	  );
     buildErrorPacket("key not found in hash table"));
export lookup1force(object:HashTable,key:Expr):Expr := lookup1force(object,key,hash(key));
export lookup1Q(object:HashTable,key:Expr,keyhash:int):bool := (
     keymod := int(keyhash & (length(object.table)-1));
     bucket := object.table.keymod;
     if bucket.key == key then return(true);
     if bucket.next.key == key then return(true);
     while bucket != bucketEnd do (
	  if bucket.key == key
	  || bucket.hash == keyhash && equal(bucket.key,key)==True
	  then return(true);
	  bucket = bucket.next;
	  );
     false);
export lookup1Q(object:HashTable,key:Expr):bool := lookup1Q(object,key,hash(key));
export lookup(object:HashTable,key:Expr,keyhash:int):Expr := (
     while true do (
     	  keymod := int(keyhash & (length(object.table)-1));
     	  bucket := object.table.keymod;
	  if bucket.key == key then return(bucket.value);
	  if bucket.next.key == key then return(bucket.next.value);
     	  while bucket != bucketEnd do (
	       if bucket.key == key
	       || bucket.hash == keyhash && equal(bucket.key,key)==True
	       then return(bucket.value);
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
	  if o == p then return(true);
	  if o == thingClass then return(false);
	  o = o.parent;
	  ));
equal(x:HashTable,y:HashTable):Expr := (
     if x==y then return(True);
     if x.hash != y.hash then return(False);
     if x.mutable 
     || y.mutable
     || x.numEntries != y.numEntries
     || length(x.table) != length(y.table)
     || x.class != y.class -- && False == equal(x.class,y.class) 
     || x.parent != y.parent -- && False == equal(x.parent,y.parent) 
     then return(False);
     if x.hash == 0					    -- cache tables has hash code 0
     && ancestor(x.class,cacheTableClass) then return (True);
     foreach a at i in x.table do (
	  p := a;
	  q := y.table.i;
	  if p.next == q.next then (
	       -- p.next and q.next must both be bucketEnd
	       if p.hash != q.hash 
	       || p.key != q.key && False == equal(p.key,q.key) 
	       || p.value != q.value && False == equal(p.value,q.value) 
	       then return(False);)
	  else (
	       plen := 0; pp := p; while pp != bucketEnd do (pp=pp.next; plen=plen+1);
	       qlen := 0; qq := q; while qq != bucketEnd do (qq=qq.next; qlen=qlen+1);
	       if plen != qlen then return(False);
	       while true do (
		    if p == bucketEnd then break;
		    z := q;
		    while true do (
			 if z.key == p.key
			 || z.hash == p.hash && equal(z.key,p.key)==True
			 then (
			      if z.value == p.value
			      || True == equal(z.value, p.value)
			      then break
			      else return(False);
			      );
			 z = z.next;
			 if z == bucketEnd then return(False);
			 );
		    p = p.next;
		    )));
     True);
export equal(lhs:Expr,rhs:Expr):Expr := (
     if lhs == rhs then True else 
     when lhs
     is Error do lhs
     is x:Real do (
	  when rhs 
	  is y:Real do Expr(toExpr(x.v==y.v))
     	  is err:Error do Expr(err)
	  else False
	  )
     is x:Complex do (
	  when rhs 
	  is y:Complex do Expr(toExpr(x.re==y.re && x.im==y.im))
     	  is err:Error do Expr(err)
	  else False
	  )
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
			 if equal(z,y.v.i) == False then return(False);
			 );
		    True ) )
	  else False)
     is x:Integer do (
	  when rhs 
	  is y:Integer do (
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
			 if equal(z,y.i) == False then return(False);
			 );
		    True))
	  else False)
     is Boolean do False
     is Nothing do False
     is file do False
     is CompiledFunction do False
     is CompiledFunctionClosure do False
     is x:Rational do (
	  when rhs
	  is y:Rational do (
	       if x === y then True else False
	       )
	  -- other cases needed soon
	  else False)
     is x:BigReal do (
	  when rhs
	  is y:BigReal do (
	       if x === y then True else False
	       )
	  else False)
     is x:BigComplex do (
	  when rhs
	  is y:BigComplex do (
	       if x === y then True else False
	       )
	  else False)
     is x:SymbolClosure do (
	  when rhs 
	  is y:SymbolClosure do (
       	       if x.symbol == y.symbol && x.frame == y.frame
	       then True else False
	       )
	  else False
	  )
     is FunctionClosure do False
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
	       if Ccode(bool, "IM2_MonomialIdeal_is_equal((MonomialIdeal *)",x,",(MonomialIdeal *)",y,")")
	       then True else False
	       )
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
     is x:RawVector do (
	  when rhs
	  is y:RawVector do (
	       if Ccode(bool, "IM2_Vector_is_equal((Vector *)",x,",(Vector *)",y,")")
	       then True else False
	       )
	  else False
	  )
     is x:LMatrixRR do (
	  when rhs
	  is y:LMatrixRR do (
	       if Ccode(bool, "LP_LMatrixRR_is_equal((LMatrixRR *)",x,",(LMatrixRR *)",y,")")
	       then True else False
	       )
	  else False
	  )
     is x:LMatrixCC do (
	  when rhs
	  is y:LMatrixCC do (
	       if Ccode(bool, "LP_LMatrixCC_is_equal((LMatrixCC *)",x,",(LMatrixCC *)",y,")")
	       then True else False
	       )
	  else False
	  )
     is x:RawMatrix do (
	  when rhs
	  is y:RawMatrix do (
	       toExpr(Ccode(bool, "IM2_Matrix_is_equal((Matrix *)",x,",(Matrix *)",y,")"))
	       )
	  else False
	  )
     is x:RawComputation do (
	  when rhs
	  is y:RawComputation do (
	       False
	       -- toExpr(Ccode(bool, "IM2_GB_is_equal((Computation *)",x,",(Computation *)",y,")"))
	       )
	  else False
	  )
     is x:RawMutableMatrix do False			    -- mutable things are unequal
     is x:RawRingMap do (
	  when rhs
	  is y:RawRingMap do toExpr(Ccode(bool, "IM2_RingMap_is_equal((RingMap *)",x,",(RingMap *)",y,")"))
	  else False
	  )
     is Database do False
     );
export remove(x:HashTable,key:Expr):Expr := (
     if !x.mutable then (
	  return(buildErrorPacket("attempted to modify an immutable hash table"));
	  );
     h := hash(key);
     hmod := int(h & (length(x.table)-1));
     p := x.table.hmod;
     prev := p;
     while p != bucketEnd do (
	  if p.key == key || equal(p.key,key)==True 
	  then (
	       if prev == p then x.table.hmod = p.next
	       else prev.next = p.next;
	       x.numEntries = x.numEntries - 1;
	       if 8 * x.numEntries == 3 * length(x.table) -- SEE BELOW
	       && length(x.table) > 4
	       -- 4 is the length of a new hash table, see tokens.d, newHashTable()
	       then shrink(x);
	       return(Expr(x)));
	  prev = p;
	  p = p.next);
     Expr(x));
export storeInHashTable(x:HashTable,key:Expr,h:int,value:Expr):Expr := (
     if !x.mutable then return(buildErrorPacket("attempted to modify an immutable hash table"));
     hmod := int(h & (length(x.table)-1));
     p := x.table.hmod;
     while p != bucketEnd do (
	  if p.key == key || equal(p.key,key)==True 
	  then (
	       p.value = value; 
	       return(value));
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
export storeInHashTable(x:HashTable,i:Code,rhs:Code):Expr := (
     ival := eval(i);
     when ival is Error do ival else (
	  val := eval(rhs);
	  when val is Error do val else storeInHashTable(x,ival,val)));
export storeInHashTableNoClobber(x:HashTable,key:Expr,h:int,value:Expr):Expr := (
     -- derived from storeInHashTable above!
     if !x.mutable then return(buildErrorPacket("attempted to modify an immutable hash table"));
     hmod := int(h & (length(x.table)-1));
     p := x.table.hmod;
     while p != bucketEnd do (
	  if p.key == key || equal(p.key,key)==True 
	  then return(buildErrorPacket("collision of keys in hash table"));
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
     if !x.mutable then return(buildErrorPacket("attempted to modify an immutable hash table"));
     hmod := int(h & (length(x.table)-1));
     p := x.table.hmod;
     while p != bucketEnd do (
	  if p.key == key || equal(p.key,key)==True 
	  then (
	       p.value = value; 
	       return(value));
	  p = p.next);
     buildErrorPacket("encountered an unknown key or option"));
export storeInHashTableMustClobber(x:HashTable,key:Expr,value:Expr):Expr := (
     storeInHashTableMustClobber(x,key,hash(key),value)
     );
export assignquotedobject(x:HashTable,i:Code,rhs:Code):Expr := (
     when i
     is var:variableCode do (
	  ival := Expr(makeSymbolClosure(var.v));
	  val := eval(rhs);
	  when val is Error do val else storeInHashTable(x,ival,val))
     else printErrorMessage(i,"'.' expected right hand argument to be a symbol")
     );
idfun(e:Expr):Expr := e;
setupfun("identity",idfun);
scanpairs(f:Expr,obj:HashTable):Expr := (
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == bucketEnd then break;
	       v := apply(f,p.key,p.value);
	       when v is Error do return(v) else nothing;
	       p = p.next;
	       ));
     nullE);
scanpairsfun(e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do
     if	       o.mutable
     then      WrongArg("an immutable hash table")
     else      scanpairs(a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("scanPairs",scanpairsfun);

mappairs(f:Expr,obj:HashTable):Expr := (
     newobj := newHashTable(obj.class,obj.parent);
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == bucketEnd then break;
	       v := apply(f,p.key,p.value);
	       when v 
	       is Error do return(v) 
	       is Nothing do nothing
	       is a:Sequence do (
		    if length(a) == 2
		    then (
			 ret := storeInHashTable(newobj,a.0,a.1);
			 when ret is Error do return(ret) else nothing;
			 )
		    else return(
			 buildErrorPacket(
			      "'applyPairs' expected return value to be a pair or 'null'"));
		    )
	       else return(
		    buildErrorPacket(
			 "'applyPairs' expected return value to be a pair or 'null'"));
	       p = p.next;
	       ));
     sethash(newobj,obj.mutable);
     Expr(newobj));
mappairsfun(e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do
     if        o.mutable 
     then      WrongArg("an immutable hash table")
     else      mappairs(a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("applyPairs",mappairsfun);

export mapkeys(f:Expr,obj:HashTable):Expr := (
     newobj := newHashTable(obj.class,obj.parent);
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == bucketEnd then break;
	       newkey := apply(f,p.key);
	       if newkey == nullE then return(buildErrorPacket("null key encountered")); -- remove soon!!!
	       when newkey is Error do return(newkey) else nothing;
	       storeInHashTableNoClobber(newobj,newkey,p.value);
	       p = p.next;
	       ));
     sethash(newobj,obj.mutable);
     Expr(newobj));
mapkeysfun(e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do        
     if        o.mutable
     then      WrongArg("an immutable hash table")
     else      mapkeys(a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("applyKeys",mapkeysfun);

export mapvalues(f:Expr,obj:HashTable):Expr := (
     u := newHashTable(obj.class,obj.parent);
     hadError := false;
     errm := nullE;
     u.numEntries = obj.numEntries;
     u.table = new array(KeyValuePair) len length(obj.table) do (
	  foreach bucket in obj.table do (
	       p := bucket;
	       q := bucketEnd;
	       while p != bucketEnd do (
		    newvalue := apply(f,p.value);
		    when newvalue is Error do (
			 errm = newvalue;
			 hadError = true;
			 while true do provide bucketEnd;
			 )
		    else nothing;
		    q = KeyValuePair(p.key,p.hash,newvalue,q);
		    p = p.next;
		    );
	       provide q;
	       );
	  );
     if hadError then return(errm);
     sethash(u,obj.mutable);
     Expr(u));
mapvaluesfun(e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do        
     if        o.mutable
     then      WrongArg("an immutable hash table")
     else      mapvalues(a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("applyValues",mapvaluesfun);

bucketsfun(e:Expr):Expr := (
     when e
     is h:HashTable do list(
	  new Sequence len length(h.table) do (
	       foreach pp in h.table do (
		    n := 0;
		    p := pp;
		    while true do (
			 if p == bucketEnd then break;
			 n = n+1;
			 p = p.next);
		    p = pp;
		    s := new Sequence len n do (
			 provide Expr(Sequence(p.key, p.value));
			 p = p.next);
		    provide list(s))))
     else WrongArg("a hash table"));
setupfun("buckets",bucketsfun);
merge(e:Expr):Expr := (
     when e is v:Sequence do (
	  if length(v) != 3 then return(WrongNumArgs(3));
	  g := v.2;
	  when v.0 is x:HashTable do
	  if x.mutable then WrongArg("an immutable hash table") else
	  when v.1 is y:HashTable do
	  if y.mutable then WrongArg("an immutable hash table") else 
	  if length(x.table) >= length(y.table) then (
	       z := copy(x);
	       z.mutable = true;
	       foreach bucket in y.table do (
		    q := bucket;
		    while q != bucketEnd do (
			 val := lookup1(z,q.key,q.hash);
			 if val != notfoundE then (
			      t := apply(g,val,q.value);
			      when t is Error do return(t) else nothing;
			      storeInHashTable(z,q.key,q.hash,t);
			      )
			 else (
			      storeInHashTable(z,q.key,q.hash,q.value);
			      );
			 q = q.next));
	       mut := false;
	       if x.class == y.class && x.parent == y.parent then (
		    z.class = x.class;
		    z.parent = x.parent;
		    mut = x.mutable;
		    )
	       else (
		    z.class = hashTableClass;
		    z.parent = nothingClass);
	       sethash(z,mut);
	       Expr(z))
	  else (
	       z := copy(y);
	       z.mutable = true;
	       foreach bucket in x.table do (
		    q := bucket;
		    while q != bucketEnd do (
			 val := lookup1(z,q.key,q.hash);
			 if val != notfoundE then (
			      t := apply(g,q.value,val);
			      when t is Error do return(t) else nothing;
			      storeInHashTable(z,q.key,q.hash,t);
			      )
			 else (
			      storeInHashTable(z,q.key,q.hash,q.value);
			      );
			 q = q.next));
	       mut := false;
	       if x.class == y.class && x.parent == y.parent then (
		    z.class = x.class;
		    z.parent = x.parent;
		    mut = x.mutable;
		    )
	       else (
		    z.class = hashTableClass;
		    z.parent = nothingClass;
		    );
	       sethash(z,mut);
	       Expr(z))
	  else WrongArg(2,"a hash table")
	  else WrongArg(1,"a hash table"))
     else WrongNumArgs(3));
setupfun("merge",merge);		  -- see objects.d
combine(f:Expr,g:Expr,h:Expr,x:HashTable,y:HashTable):Expr := (
     z := newHashTable(x.class,x.parent);
     foreach pp in x.table do (
	  p := pp;
	  while p != bucketEnd do (
	       foreach qq in y.table do (
		    q := qq;
		    while q != bucketEnd do (
			 pqkey := apply(f,p.key,q.key);
			 when pqkey is Error do return(pqkey) else nothing;
			 pqvalue := apply(g,p.value,q.value);
			 when pqvalue is Error do return(pqvalue) else nothing;
			 pqhash := hash(pqkey);
			 previous := lookup1(z,pqkey,pqhash);
			 r := storeInHashTable(z,pqkey,pqhash,
			      if previous == notfoundE
			      then pqvalue
			      else (
				   t := apply(h,previous,pqvalue);
				   when t is Error do return(t) else nothing;
				   t));
			 when r is Error do return(r) else nothing;
			 q = q.next);
		    );
	       p = p.next));
     sethash(z,x.mutable | y.mutable);
     z);
combine(e:Expr):Expr := (
     when e
     is v:Sequence do
     if length(v) == 5 then 
     when v.0 is x:HashTable do
     if x.mutable then WrongArg(1,"an immutable hash table") else
     when v.1 is y:HashTable do
     if y.mutable then WrongArg(2,"an immutable hash table") else
     combine(v.2,v.3,v.4,x,y)
     else WrongArg(1+1,"a hash table")
     else WrongArg(0+1,"a hash table")
     else WrongNumArgs(5)
     else WrongNumArgs(5));
setupfun("combine",combine);
export Parent(e:Expr):HashTable := when e is obj:HashTable do obj.parent else nothingClass;
export parentfun(e:Expr):Expr := Expr(Parent(e));
setupfun("parent",parentfun);
export Class(e:Expr):HashTable := (
     when e 
     is obj:HashTable do obj.class
     is x:List do x.class
     is Integer do integerClass
     is Rational do rationalClass
     is Real do doubleClass
     is Complex do complexClass
     is file do fileClass
     is string do stringClass
     is FunctionClosure do functionClass
     is Net do netClass
     is Error do errorClass
     is Sequence do sequenceClass
     is CompiledFunction do functionClass
     is CompiledFunctionClosure do functionClass
     is SymbolClosure do symbolClass
     is BigReal do bigRealClass
     is BigComplex do bigComplexClass
     is RawComputation do rawComputationClass
     is Nothing do nothingClass
     is Database do dbClass
     is Boolean do booleanClass
     is RawMonomial do rawMonomialClass
     is RawMonomialOrdering do rawMonomialOrderingClass
     is RawMonoid do rawMonoidClass
     is RawMonomialIdeal do rawMonomialIdealClass
     is RawRing do rawRingClass
     is RawRingElement do rawRingElementClass
     is RawRingMap do rawRingMapClass
     is RawFreeModule do rawFreeModuleClass
     is RawVector do rawVectorClass
     is RawMatrix do rawMatrixClass
     is RawMutableMatrix do rawMutableMatrixClass
     is LMatrixRR do LMatrixRRClass
     is LMatrixCC do LMatrixCCClass
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
setupconst("MutableHashTable",Expr(mutableHashTableClass));
setupconst("CacheTable",Expr(cacheTableClass));
setupconst("BasicList",Expr(basicListClass));
setupconst("List",Expr(listClass));
setupconst("MutableList",Expr(mutableListClass));
setupconst("ZZ",Expr(integerClass));
setupconst("QQ",Expr(rationalClass));
setupconst("RR",Expr(doubleClass));
setupconst("BigReal",Expr(bigRealClass));
setupconst("RawObject",Expr(rawObjectClass));
setupconst("RawMonomial",Expr(rawMonomialClass));
setupconst("RawMonomialOrdering",Expr(rawMonomialOrderingClass));
setupconst("RawMonoid",Expr(rawMonoidClass));
setupconst("RawRing",Expr(rawRingClass));
setupconst("RawFreeModule",Expr(rawFreeModuleClass));
setupconst("RawVector",Expr(rawVectorClass));
setupconst("RawMatrix",Expr(rawMatrixClass));
setupconst("RawMutableMatrix",Expr(rawMutableMatrixClass));
setupconst("RawRingElement",Expr(rawRingElementClass));
setupconst("RawRingMap",Expr(rawRingMapClass));
setupconst("RawMonomialIdeal",Expr(rawMonomialIdealClass));
setupconst("File",Expr(fileClass));
setupconst("String",Expr(stringClass));
setupconst("Function",Expr(functionClass));
setupconst("Symbol",Expr(symbolClass));
setupconst("Error",Expr(errorClass));
setupconst("Time",Expr(timeClass));
setupconst("Option",Expr(optionClass));
setupconst("Net",Expr(netClass));
setupconst("true",True);
setupconst("false",False);
setupconst("null",nullE);
setupconst("Boolean",Expr(booleanClass));
setupconst("Database",Expr(dbClass));
setupconst("Sequence",Expr(sequenceClass));
setupconst("VisibleList",Expr(visibleListClass));
setupconst("Array",Expr(arrayClass));
setupconst("SymbolTable",Expr(symboltableClass));
setupconst("Ring",Expr(ringClass));
setupconst("Nothing",Expr(nothingClass));
setupconst("LMatrixRR",Expr(LMatrixRRClass));
setupconst("LMatrixCC",Expr(LMatrixCCClass));
setupconst("CCC",Expr(complexClass));			    -- to be changed to CC later

assigntofun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left
     is q:SymbolClosure do (
	  if q.symbol.protected then (
	       printErrorMessage(lhs, "assignment to protected variable '" + q.symbol.word.name + "'")
	       )
	  else (
	       value := eval(rhs);
	       when value is Error do return(value) else nothing;
	       q.frame.values.(q.symbol.frameindex) = value;
	       value))
     is o:HashTable do (
	  if o.mutable then (
	       y := eval(rhs);
	       when y is p:HashTable do (
		    o.table = copy(p.table);
		    o.numEntries = p.numEntries;
		    left)
	       is Error do y
	       else printErrorMessage(rhs,"expected hash table on right"))
	  else printErrorMessage(lhs,"encountered read only hash table"))
     is l:List do (
	  if l.mutable then (
	       y := eval(rhs);
	       when y
	       is p:List do ( l.v = copy(p.v); left)
	       is s:Sequence do ( l.v = copy(s); left)
	       is Error do y
	       else printErrorMessage(rhs,"'<-' expected list or sequence on right"))
	  else printErrorMessage(lhs,"'<-' encountered read-only list"))
     is Error do left
     else printErrorMessage(lhs,"'<-' expected symbol or hash table on left")
     );
setup(LeftArrowW,assigntofun);

symbols(e:Expr):Expr := (
     o := newHashTable(symboltableClass,nothingClass);
     foreach bucket in globalScope.dictionary.hashTable do (
	  p := bucket;
	  while true do (
	       when p
	       is null do break
	       is q:SymbolListCell do (
		    storeInHashTable(o,
			 Expr(q.entry.word.name),
			 Expr(makeSymbolClosure(q.entry)));
		    p = q.next;
		    )));
     sethash(o,false);
     o);
setupfun("symbolTable",symbols);

export commonAncestor(x:HashTable,y:HashTable):HashTable := (
     if x == y then return(x);
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

-- methods

export unarymethod(rhs:Code,methodkey:SymbolClosure):Expr := (
     right := eval(rhs);
     when right is Error do right
     else (
	  method := lookup(Class(right),Expr(methodkey),methodkey.symbol.hash);
	  if method == nullE then MissingMethod(methodkey)
	  else apply(method,right)));

export unarymethod(right:Expr,methodkey:SymbolClosure):Expr := (
     method := lookup(Class(right),Expr(methodkey),methodkey.symbol.hash);
     if method == nullE then MissingMethod(methodkey)
     else apply(method,right));
-----------------------------------------------------------------------------
export typicalValues := newHashTable(mutableHashTableClass,nothingClass);
messx := "method should be 'function' or 'returntype => function'";
installIt(h:HashTable,key:Expr,value:Expr):Expr := (
     when value
     is Error do value
     is FunctionClosure do storeInHashTable(h,key,value)
     is CompiledFunction do storeInHashTable(h,key,value)
     is CompiledFunctionClosure do storeInHashTable(h,key,value)
     is x:List do (
	  if x.class == optionClass && length(x.v) == 2 then (
	       storeInHashTable(typicalValues,key,x.v.0);
	       installIt(h,key,x.v.1);
	       value)
	  else buildErrorPacket(messx))
     else buildErrorPacket(messx));
-----------------------------------------------------------------------------
-- unary methods
export installMethod(meth:Expr,s:HashTable,value:Expr):Expr := (
     when value is Error do value
     is FunctionClosure do storeInHashTable(s,meth,value)
     is CompiledFunction do storeInHashTable(s,meth,value)
     is CompiledFunctionClosure do storeInHashTable(s,meth,value)
     is x:List do (
	  if x.class == optionClass && length(x.v) == 2 then (
	       storeInHashTable(typicalValues,Expr(Sequence(meth,Expr(s))),x.v.0);
	       installMethod(meth,s,x.v.1);
	       value)
	  else buildErrorPacket(messx))
     else buildErrorPacket(messx));
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
     installIt(
	  if lhs.hash > rhs.hash then lhs else rhs,
	  Expr(Sequence(meth,Expr(lhs),Expr(rhs))),
	  value));

export installMethod(s:SymbolClosure,X:HashTable,Y:HashTable,f:fun):Expr := (
     installMethod(Expr(s),X,Y,Expr(CompiledFunction(f,nextHash())))
     );

export installValue(meth:Expr,lhs:HashTable,rhs:HashTable,value:Expr):Expr := (
     if ! rhs.mutable && !lhs.mutable
     then return(buildErrorPacket("value installation attempted, but neither hash tables is mutable"));
     storeInHashTable(
	  if !lhs.mutable then rhs
	  else if rhs.mutable then ( if lhs.hash > rhs.hash then lhs else rhs )
	  else lhs,
	  Expr(Sequence(Expr(lhs),Expr(rhs),meth)),
	  value));
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
	       if s != notfoundE then return(s);
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
     installIt( 
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
			 return(s);
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
export binarymethod(lhs:Code,rhs:Code,methodkey:SymbolClosure):Expr := (
     left := eval(lhs);
     when left is Error do left
     else (
	  right := eval(rhs);
	  when right is Error do right
	  else (
	       method := lookupBinaryMethod(Class(left),Class(right),Expr(methodkey),
		    methodkey.symbol.hash);
	       if method == nullE then MissingMethodPair(methodkey,left,right)
	       else apply(method,left,right))));
export binarymethod(left:Expr,rhs:Code,methodkey:SymbolClosure):Expr := (
     right := eval(rhs);
     when right is Error do right
     else (
	  method := lookupBinaryMethod(Class(left),Class(right),Expr(methodkey),
	       methodkey.symbol.hash);
	  if method == nullE then MissingMethodPair(methodkey,left,right)
	  else apply(method,left,right)));
export binarymethod(left:Expr,right:Expr,methodkey:SymbolClosure):Expr := (
     method := lookupBinaryMethod(Class(left),Class(right),Expr(methodkey),methodkey.symbol.hash);
     if method == nullE then MissingMethodPair(methodkey,left,right)
     else apply(method,left,right));
-----------------------------------------------------------------------------
installfun(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 3 then (
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
     if length(a)== 2 then
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
     else nullE
     is SymbolClosure do e
     is CompiledFunctionClosure do e
     is FunctionClosure do e
     is CompiledFunction do e
     else nullE);
setupfun("lookup",lookupfun);	  

toHashTableError(i:int):Expr := buildErrorPacket(
     "expected element at position "+tostring(i)+" to be a pair");
toHashTable(v:Sequence):Expr := (
     o := newHashTable(hashTableClass,nothingClass);
     foreach e at i in v do (
	  when e
	  is Nothing do nothing
	  is pair:Sequence do (
	       if length(pair) == 2 
	       then (storeInHashTable(o,pair.0,pair.1);)
	       else return(toHashTableError(i)))
	  is z:List do (
	       pair := z.v;
	       if length(pair) == 2 
	       then (storeInHashTable(o,pair.0,pair.1);)
	       else return(toHashTableError(i)))
	  else return(toHashTableError(i)));
     sethash(o,false);
     Expr(o));
toHashTable(e:Expr):Expr := (
     when e
     is w:List do toHashTable(w.v)
     else WrongArg("a list"));
setupfun("hashTable",toHashTable);

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
     is w:List do makeSet(w.v)
     else WrongArg("a list or sequence"));
setupfun("set",makeSet);

modify(object:HashTable,key:Expr,f:function(Expr):Expr,v:Expr):void := (
     keyhash:= hash(key);
     keymod := int(keyhash & (length(object.table)-1));
     bucket := object.table.keymod;
     if bucket.key == key then (
	  bucket.value = f(bucket.value); -- notice: no error checking here
	  return();
	  );
     if bucket.next.key == key then (
	  bucket.next.value = f(bucket.next.value);-- notice: no error checking here
	  return();
	  );
     while bucket != bucketEnd do (
	  if bucket.key == key
	  || bucket.hash == keyhash && equal(bucket.key,key)==True
	  then (
	       bucket.value = f(bucket.value); -- notice: no error checking here
	       return();
	       );
	  bucket = bucket.next;
	  );
     storeInHashTable(object,key,keyhash,v);
     );

addone(i:Expr):Expr := when i is j:Integer do Expr(j+1) else i;
makeTally(v:Sequence):Expr := (
     o := newHashTable(Tally,nothingClass);
     foreach e at i in v do modify(o,e,addone,one);
     sethash(o,false);
     Expr(o));
makeTally(e:Expr):Expr := (
     when e
     is v:Sequence do makeTally(v)
     is w:List do makeTally(w.v)
     else WrongArg("a list or sequence"));
setupfun("tally",makeTally);
