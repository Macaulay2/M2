--		Copyright 1994 by Daniel R. Grayson
use system; 
use convertr;
use binding;
use parser;
use lex;
use arith;
use nets;
use tokens;
use err;
use stdiop;
use ctype;
use stdio;
use varstrin;
use strings;
use C;
use actors;
use basic;
use struct;
use objects;

-----------------------------------------------------------------------------
-- Database stuff
dbmcheck(ret:int):Expr := (
     if ret == -1 then buildErrorPacket(dbmstrerror())
     else Expr(toInteger(ret)));
dbmopenin(filename:string):Expr := (
     mutable := false;
     handle := dbmopen(filename,mutable);
     if handle == -1 
     then buildErrorPacket(dbmstrerror() + " : " + filename)
     else Expr(Database(filename,nextHash(),handle,true,mutable)));
dbmopenout(filename:string):Expr := (
     mutable := true;
     handle := dbmopen(filename,mutable);
     if handle == -1 
     then buildErrorPacket(dbmstrerror() + " : " + filename)
     else Expr(Database(filename,nextHash(),handle,true,mutable)));
dbmclose(f:Database):Expr := (
     if !f.isopen then return(buildErrorPacket("database already closed"));
     dbmclose(f.handle);
     f.isopen = false;
     Expr(toInteger(0)));
dbmstore(f:Database,key:string,content:string):Expr := (
     if !f.isopen then return(buildErrorPacket("database closed"));
     if !f.mutable then return(buildErrorPacket("database not mutable"));
     ret := dbmstore(f.handle,key,content);
     if 0 == ret then Expr(content)
     else dbmcheck(ret));
dbmstore(f:Database,KEY:Code,CONTENT:Code):Expr := (
     Key := eval(KEY);
     when Key
     is Error do Key
     is key:string do (
	  Content := eval(CONTENT);
	  when Content
	  is Error do Content
	  is content:string do dbmstore(f,key,content)
	  is Nothing do (
	       if !f.isopen then return(buildErrorPacket("database closed"));
	       if !f.mutable then return(buildErrorPacket("database not mutable"));
	       if 0 == dbmdelete(f.handle,key)
	       then nullE
	       else buildErrorPacket(dbmstrerror() + " : " + f.filename))
	  else printErrorMessage(CONTENT,"expected a string or null"))
     else printErrorMessage(KEY,"expected a string"));
dbmquery(f:Database,key:string):Expr := (
     if !f.isopen then return(buildErrorPacket("database closed"));
     when dbmfetch(f.handle,key)
     is a:string do True
     else False);
dbmfirst(f:Database):Expr := (
     if !f.isopen then return(buildErrorPacket("database closed"));
     when dbmfirst(f.handle)
     is a:string do Expr(a)
     else nullE);
dbmfirst(e:Expr):Expr := (
     when e
     is f:Database do dbmfirst(f)
     else buildErrorPacket("expected a database"));
setupfun("firstkey",dbmfirst);
dbmnext(f:Database):Expr := (
     if !f.isopen then return(buildErrorPacket("database closed"));
     when dbmnext(f.handle)
     is a:string do Expr(a)
     else nullE);
dbmnext(e:Expr):Expr := (
     when e
     is f:Database do dbmnext(f)
     is Sequence do WrongNumArgs(1)
     else WrongArg(1,"a database"));
setupfun("nextkey",dbmnext);
dbmreorganize(f:Database):Expr := (
     if !f.isopen then return(buildErrorPacket("database closed"));
     if !f.mutable then return(buildErrorPacket("database not mutable"));
     dbmcheck(dbmreorganize(f.handle)));
dbmreorganize(e:Expr):Expr := (
     when e
     is f:Database do dbmreorganize(f)
     else buildErrorPacket("expected a database"));
setupfun("reorganize",dbmreorganize);
dbmopenin(e:Expr):Expr := (
     when e
     is a:string do dbmopenin(a)
     else buildErrorPacket("expected a string as filename"));
setupfun("openDatabase",dbmopenin);
dbmopenout(e:Expr):Expr := (
     when e
     is a:string do dbmopenout(a)
     else buildErrorPacket("expected a string as filename"));
setupfun("openDatabaseOut",dbmopenout);
-----------------------------------------------------------------------------

keys(o:HashTable):Expr := list(
     new Sequence len o.numEntries do
     foreach bucket in o.table do (
	  p := bucket;
	  while p != bucketEnd do (
	       provide Expr(p.key);
	       p = p.next;
	       )
	  )
     );

keys(f:Database):Expr := (
     if !f.isopen then return(buildErrorPacket("database closed"));
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
keys(e:Expr):Expr := (
     when e
     is f:Database do keys(f)
     is o:HashTable do keys(o)
     else WrongArg("a hash table"));
setupfun("keys",keys);
elements(e:Expr):Expr := (
     when e
     is o:HashTable do keys(o)
     is a:Sequence do list(a)
     is b:List do (
	  if b.class == listClass then e
	  else Expr(
	       sethash(
	       	    List(listClass, if b.mutable then copy(b.v) else b.v,
		    	 0, false),
	       	    false)))
     else WrongArg("a hash table, list, or sequence"));
setupfun("toList",elements);
values(e:Expr):Expr := (
     when e
     is o:HashTable do list(
	  new Sequence len o.numEntries do
	  foreach bucket in o.table do (
	       p := bucket;
	       while p != bucketEnd do (
		    provide Expr(p.value);
		    p = p.next;
		    )
	       )
	  )
     else WrongArg("a hash table"));
setupfun("values",values);

-- operators

timefun(a:Code):Expr := (
     v := etime();
     ret := eval(a);
     x := etime();
     y := etime();
     when ret
     is Error do ret
     else list(timeClass,Sequence(Expr(Real((x-v)-(y-x))),ret)));
setupop(timingS,timefun);
showtimefun(a:Code):Expr := (
     v := etime();
     ret := eval(a);
     x := etime();
     y := etime();
     stdout << "     -- used " << (x-v)-(y-x) << " seconds" << endl;
     ret);
setupop(timeS,showtimefun);
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
subvalue(left:Expr,right:Expr):Expr := (
     -- don't change this without changing subvalueQ below
     when left is x:Sequence do (
	  when right is r:Integer do (
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
	       if !f.isopen then return(buildErrorPacket("database closed"));
	       when dbmfetch(f.handle,key)
	       is a:string do Expr(a)
	       else buildErrorPacket("encountered missing value"))
	  else buildErrorPacket("expected a string as key to database"))
     is x:List do (
	  when right is r:Integer do (
	       if isInt(r) then getvalue(x.v,toInt(r))
	       else buildErrorPacket("array index "
		    + tostring(r)
		    + " out of bounds 0 .. "
		    + tostring(length(x.v)-1)))
	  else buildErrorPacket("array index not an integer"))
     is x:string do (
	  when right is r:Integer do (
	       if isInt(r) then (
		    rr := toInt(r);
		    if rr < 0 then rr = rr + length(x);
		    if rr < 0 || rr >= length(x) 
		    then buildErrorPacket("string index out of bounds")
		    else Expr(string(x.rr)))
	       else buildErrorPacket("string index out of bounds"))
	  else buildErrorPacket("expected subscript to be an integer"))
     else buildErrorPacket("expected a list, hash table, or sequence"));
subvalueQ(left:Expr,right:Expr):Expr := (
     -- don't change this without changing subvalue above
     when left is x:Sequence do (
	  when right is r:Integer do (
	       if isInt(r) then (
	       	    i := toInt(r);
		    if i < -length(x) || i >= length(x) then False else True
		    )
	       else False)
	  else False)
     is x:HashTable do if lookup1Q(x,right) then True else False
     is x:Database do (
	  when right
	  is key:string do dbmquery(x,key)
	  else False)
     is x:List do (
	  when right is r:Integer do (
	       if isInt(r) then (
	       	    i := toInt(r);
		    if i < -length(x.v) || i >= length(x.v) then False else True
		    )
	       else False)
	  else False)
     is x:string do (
	  when right is r:Integer do (
	       if isInt(r) then (
		    rr := toInt(r);
		    if rr < 0 || rr >= length(x) 
		    then False
		    else True)
	       else False)
	  else False)
     else False);
subvalue(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left is Error do left
     else (
      	  right := eval(rhs);
      	  when right is Error do right
      	  else subvalue(left,right)));
lengthFun(rhs:Code):Expr := (
     e := eval(rhs);
     when e
     is Error do e
     is x:HashTable do Expr(toInteger(x.numEntries))
     is x:Sequence do Expr(toInteger(length(x)))
     is x:List do Expr(toInteger(length(x.v)))
     is f:file do (
	  if f.input || f.output then (
	       r := fileLength(f);
	       if r == ERROR then buildErrorPacket("couldn't determine length of file")
	       else Expr(toInteger(r))
	       )
	  else buildErrorPacket("file not open")
	  )
     is s:string do Expr(toInteger(length(s)))
     else buildErrorPacket("expected a list, sequence, hash table, file, or string"));
setup(SharpS,lengthFun,subvalue);
subvalueQ(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left is Error do left
     else (
      	  right := eval(rhs);
      	  when right is Error do right
      	  else subvalueQ(left,right)));
setup(SharpQuestionS,subvalueQ);

assignvector(x:Sequence,i:Code,rhs:Code):Expr := (
     ival := eval(i);
     when ival
     is j:Integer do (
	  if isInt(j)
	  then (
	       k := toInt(j);
	       if k < -length(x) || k >= length(x)
	       then printErrorMessage(i,"subscript out of bounds 0 .. "+tostring(length(x)-1))
	       else (
		    val := eval(rhs);
		    when val is Error do val
		    else (
			 if k < 0
			 then x.(length(x) + k) = val
			 else x.k = val;
			 val)))
	  else printErrorMessage(i,"subscript out of bounds"))
     is Error do ival
     else printErrorMessage(i,"expected integer as subscript")
     );
assignfun(lhs:Code,rhs:Code):Expr := (
     when lhs
     is var:variableCode do (
	  if var.v.protected then (
	       printErrorMessage(lhs,"assignment to protected variable")
	       )
	  else (
	       value := eval(rhs);
	       when value is Error do return(value) else nothing;
	       frame(var.v.scopenum).values.(var.v.frameindex) = value;
	       value))
     else printErrorMessage(lhs,"left side of assignment should be symbol")
     );
AssignFun = assignfun;

globalassignfun(lhs:Code,rhs:Code):Expr := (
     when lhs
     is var:variableCode do (
	  if var.v.protected then (
	       printErrorMessage(lhs,"assignment to protected variable")
	       )
	  else (
	       value := eval(rhs);
	       when value is Error do return(value) else nothing;
	       f := frame(var.v.scopenum).values;
	       i := var.v.frameindex;
	       oldvalue := f.i;
	       method := lookup(Class(oldvalue),GlobalReleaseE);
	       if method != nullE then (
		    y := apply(method,Expr(makeSymbolClosure(var.v)),oldvalue);
		    when y 
		    is Error do return(y)
		    else nothing;
		    );
	       method = lookup(Class(value),GlobalAssignE);
	       if method != nullE then (
		    y := apply(method,Expr(makeSymbolClosure(var.v)),value);
		    when y 
		    is Error do return(y)
		    else nothing;
		    );
	       f.i = value;
	       value))
     else printErrorMessage(lhs,"left side of assignment should be symbol")
     );
GlobalAssignFun = globalassignfun;
assignelemfun(lhsarray:Code,lhsindex:Code,rhs:Code):Expr := (
     x := eval(lhsarray);
     when x
     is x:List do (
	  if x.mutable then assignvector(x.v,lhsindex,rhs)
	  else buildErrorPacket("assignment attempted to element of immutable list")
	  )
     is x:Sequence do buildErrorPacket("assignment attempted to element of sequence")
     is x:HashTable do storeInHashTable(x,lhsindex,rhs)
     is x:Database do dbmstore(x,lhsindex,rhs)
     else printErrorMessage(lhsarray,"expected a list, sequence, hash table, or database")
     );
AssignElemFun = assignelemfun;
assignquotedelemfun(lhsarray:Code,lhsindex:Code,rhs:Code):Expr := (
     x := eval(lhsarray);
     when x
     is x:HashTable do assignquotedobject(x,lhsindex,rhs)
     else printErrorMessage(lhsarray,"'.' expected left hand side to be a hash table")
     );
AssignQuotedElemFun = assignquotedelemfun;
ifthenfun(predicate:Code,thenclause:Code):Expr := (
     p := eval(predicate);
     when p is Error do p
     else if p == True then eval(thenclause)
     else if p == False then nullE
     else printErrorMessage(predicate,"expected true or false"));
IfThenFun = ifthenfun;
tryelsefun(primary:Code,alternate:Code):Expr := (
     oldSuppressErrors := SuppressErrors;
     SuppressErrors = true;
     p := eval(primary);
     if !SuppressErrors then p		  -- eval could have turned it off
     else (
     	  SuppressErrors = oldSuppressErrors;
	  when p is err:Error do (
	       if err.message == breakMessage || err.message == returnMessage then p
	       else eval(alternate)
	       )
	  else p));
TryElseFun = tryelsefun;
tryfun(primary:Code):Expr := (
     oldSuppressErrors := SuppressErrors;
     SuppressErrors = true;
     p := eval(primary);
     if !SuppressErrors then p		  -- eval could have turned it off
     else (
     	  SuppressErrors = oldSuppressErrors;
	  when p 
	  is err:Error do (
	       if err.message == breakMessage || err.message == returnMessage then p
	       else nullE)
	  else p));
TryFun = tryfun;
ifthenelsefun(predicate:Code,thenclause:Code,elseClause:Code):Expr := (
     p := eval(predicate);
     when p is Error do p
     else if p == True then eval(thenclause)
     else if p == False then eval(elseClause)
     else printErrorMessage(predicate,"expected true or false"));
IfThenElseFun = ifthenelsefun;

basictype(e:Expr):HashTable := basictype(Class(e));
basictypefun(e:Expr):Expr := Expr(basictype(e));
setupfun("basictype",basictypefun);

expected(type:string,returned:bool):Expr := buildErrorPacket(
     if returned 
     then "'new' expected method to return " + type
     else "expected " + type + " (in absence of a 'new' method)"
     );

wrongTarget():Expr := buildErrorPacket("'new' expected a type of list or hash table");

transform(e:Expr,class:HashTable,parent:HashTable,returned:bool):Expr := (
     basicType := basictype(class);
     when e
     is Error do e
     is o:HashTable do (
	  if basicType == hashTableClass then (
	       if o.class == class && o.parent == parent
	       then e
	       else (
	       	    mutable := ancestor(class,mutableHashTableClass);
		    x := HashTable(
			 if mutable || o.mutable then copy(o.table) else o.table,
			 class, parent, o.numEntries, 0, mutable);
		    if mutable then (
			 if !ancestor(class,cacheTableClass) then o.hash = nextHash();
			 )
		    else o.hash = hash(o);
		    Expr(o)))
	  else if basicType == basicListClass then expected("a list",returned)
	  else wrongTarget())
     is o:List do (
     	  if basicType == basicListClass then (
	       if parent != nothingClass
	       then buildErrorPacket("expected Nothing as parent for list")
	       else if o.class == class then e
	       else (
	       	    mutable := ancestor(class,mutableListClass);
		    Expr(
			 sethash(
			      List(class,
			      	   if mutable || o.mutable then copy(o.v) else o.v,
			      	   0,false),
			      mutable))))
	  else if basicType == hashTableClass 
	  then expected("a hash table",returned)
	  else wrongTarget())
     else expected(
	  if basicType == basicListClass
	  then "a list"
	  else if basicType == hashTableClass
	  then "a hash table"
	  else "a list or hash table",
	  returned
	  ));

transform(e:Expr,class:HashTable,returned:bool):Expr := (
     -- same as above, but no parent specified, so leave what s provided alone
     when e
     is Error do e
     is o:HashTable do (
     	  basicType := basictype(class);
	  if basicType == hashTableClass then (
	       if o.class == class then e
	       else (
	       	    mutable := ancestor(class,mutableHashTableClass);
		    Expr(
			 sethash(
			      HashTable(
				   if mutable || o.mutable 
				   then copy(o.table) else o.table,
				   class,o.parent,
				   o.numEntries,0,false),
			      mutable))))
	  else if basicType == basicListClass then expected("a list",returned)
	  else wrongTarget())
     is o:List do (
     	  basicType := basictype(class);
     	  if basicType == basicListClass then (
	       if o.class == class then e
	       else (
	       	    mutable := ancestor(class,mutableListClass);
		    Expr(
			 sethash(
			      List(class,
			      	   if mutable || o.mutable then copy(o.v) else o.v,
			      	   0,false),
			      mutable))))
	  else if basicType == hashTableClass 
	  then expected("a hash table",returned)
	  else wrongTarget())
     else if Class(e) == class then e
     else (
	  basicType := basictype(class);
	  expected(
	       if basicType == basicListClass then "a list"
	       else if basicType == hashTableClass then "a hash table"
	       else "a list or hash table",
	       returned)));
newclassfun(e:Expr):Expr := (
     when e
     is a:Sequence do
     if length(a) == 2
     then when a.0
     is class:HashTable do transform(a.1,class,false)
     else WrongArg(1,"a hash table")
     else if length(a) == 3
     then when a.0
     is class:HashTable do (
	  when a.1
	  is parent:HashTable do transform(a.2,class,parent,false)
	  else WrongArg(2,"a hash table"))
     else WrongArg(1,"a hash table")
     else WrongNumArgs(2,3)
     else WrongNumArgs(2,3));
setupfun("newClass",newclassfun);

makenew(class:HashTable,parent:HashTable):Expr := (
     basicType := basictype(class);
     if basicType == hashTableClass 
     then (
	  o := newHashTable(class,parent);
	  p := class;
	  while true do (
	       if p == hashTableClass then (
		    o.mutable = false;
		    o.hash = hash(o);
		    break;
		    );
	       if p == mutableHashTableClass then (
		    break;
		    );
	       if p == cacheTableClass then (
		    o.hash = 0;
		    break;
		    );
	       p = p.parent;
	       );
     	  Expr(o))
     else if basicType == basicListClass 
     then (
	  if parent != nothingClass
	  then buildErrorPacket("expected Nothing as parent for list")
	  else Expr(
	       sethash(
		    List(class,emptySequence,0,false),
		    ancestor(class,mutableHashTableClass))))
     else buildErrorPacket("basic type for 'new' method should have been BasicList or HashTable"));
makenew(class:HashTable):Expr := makenew(class,nothingClass);
-----------------------------------------------------------------------------

errt (newClassCode :Code):Expr := printErrorMessage(newClassCode ,"'new' expected a Type as prospective class");
errtt(newClassCode :Code):Expr := printErrorMessage(newClassCode ,"'new' expected a Type of Type as prospective class");
errp (newParentCode:Code):Expr := printErrorMessage(newParentCode,"'new' expected a Type as prospective parent");

newfun(newClassCode:Code):Expr := (
     classExpr := eval(newClassCode);
     when classExpr 
     is Error do classExpr
     is class:HashTable do (
	  if !ancestor(class.class,typeClass)
	  then errt(newClassCode)
	  else (
	       method := lookup(class,NewS);
	       if method != nullE
	       then transform(apply(method,Expr(class)),class,true)
	       else makenew(class)))
     else errt(newClassCode));
NewFun = newfun;
newoffun(newClassCode:Code,newParentCode:Code):Expr := (
     classExpr := eval(newClassCode);
     when classExpr 
     is Error do classExpr
     is class:HashTable do (
	  if !ancestor(class.class,typeClass)
	  || !ancestor(class,typeClass) 
	  then errtt(newClassCode)
	  else (
	       newParentExpr := eval(newParentCode);
	       when newParentExpr
	       is Error do newParentExpr
	       is parent:HashTable do (
		    if !ancestor(parent.class,typeClass)
		    then errp(newParentCode)
		    else (
			 method := lookupBinaryMethod(class,parent.class,NewOfS);
			 if method != nullE
			 then transform(apply(method,Expr(class),Expr(parent)),class,parent,true)
			 else makenew(class,parent)))
	       else errp(newParentCode)))
     else errtt(newClassCode));
NewOfFun = newoffun;
newfromfun(newClassCode:Code,newInitCode:Code):Expr := (
     classExpr := eval(newClassCode);
     when classExpr 
     is Error do classExpr
     is class:HashTable do (
	  if !ancestor(class.class,typeClass)
	  then errt(newClassCode)
	  else (
	       newInitExpr := eval(newInitCode);
	       when newInitExpr
	       is Error do newInitExpr
	       else (
		    method := lookupBinaryMethod(class,Class(newInitExpr),NewFromS);
		    if method != nullE
		    then transform(apply(method,Expr(class),newInitExpr),class,true)
		    else (
			 when newInitExpr
			 is p:List do (
			      if p.class == class
			      then Expr(if p.mutable then copy(p) else p)
			      else transform(newInitExpr,class,false))
			 is p:HashTable do (
			      if p.class == class
			      then Expr(if p.mutable then copy(p) else p)
			      else transform(newInitExpr,class,false))
			 else transform(newInitExpr,class,false)))))
     else errt(newClassCode));
NewFromFun = newfromfun;
newoffromfun(newClassCode:Code,newParentCode:Code,newInitCode:Code):Expr := (
     classExpr := eval(newClassCode);
     when classExpr 
     is Error do classExpr
     is class:HashTable do (
	  if !ancestor(class.class,typeClass)
	  && !ancestor(class,typeClass)
	  then errtt(newClassCode)
	  else (
	       newParentExpr := eval(newParentCode);
	       when newParentExpr
	       is Error do newParentExpr
	       is parent:HashTable do (
		    if !ancestor(parent.class,typeClass)
		    then errp(newParentCode)
		    else (
			 newInitExpr := eval(newInitCode);
			 when newInitExpr
			 is Error do newInitExpr
			 else (
			      method := lookupTernaryMethod(
				   class,parent.class,Class(newInitExpr),NewOfFromE,NewOfFromS.symbol.hash);
			      if method != nullE 
			      then transform(
				   apply(method,Expr(class),Expr(parent),newInitExpr),
				   class,parent,true)
			      else (when newInitExpr
				   is p:List do (
					if p.class == class && nothingClass == parent
					then Expr(if p.mutable then copy(p) else p)
					else transform(newInitExpr,class,parent,false))
				   is p:HashTable do (
					if p.class == class && p.parent == parent
					then Expr(if p.mutable then copy(p) else p)
					else transform(newInitExpr,class,false))
				   else transform(newInitExpr,class,parent,false)))))
	       else errp(newParentCode)))
     else errtt(newClassCode));
NewOfFromFun = newoffromfun;
-----------------------------------------------------------------------------
whiledofun(predicate:Code,body:Code):Expr := (
     while true do (
	  p := eval(predicate);
	  when p is err:Error 
	  do return(if err.message == breakMessage then err.value else p)
	  else if p == True then (
	       b := eval(body);
	       when b is err:Error 
	       do return(if err.message == breakMessage then err.value else b) 
	       else nothing;
	       )
	  else if p == False then break
	  else return(printErrorMessage(predicate,"expected true or false")));
     nullE);
WhileDoFun = whiledofun;

whilelistfun(predicate:Code,body:Code):Expr := (
     n := 1;
     r := new Sequence len n do provide nullE;
     i := 0;
     while true do (
	  p := eval(predicate);
	  when p is err:Error
	  do return(if err.message == breakMessage then err.value else p)
	  else if p == True then (
	       b := eval(body);
	       when b is err:Error 
	       do return(if err.message == breakMessage then err.value else b) 
	       else (
		    if i == n then (
			 n = 2*n;
			 r = new Sequence len n do (
			      foreach x in r do provide x;
			      while true do provide nullE;
			      );
			 );
		    r.i = b;
		    i = i+1;
		    );
	       )
	  else if p == False then break
	  else return(printErrorMessage(predicate,"expected true or false")));
     Expr(
	  list(
	       if i == 0 then emptySequence
	       else if i == n then r
	       else new Sequence len i do foreach x in r do provide x)));
WhileListFun = whilelistfun;

whilelistdofun(predicate:Code,listClause:Code,doClause:Code):Expr := (
     r := new Sequence len 1 do provide nullE;
     i := 0;
     while true do (
	  p := eval(predicate);
	  when p is err:Error do return(if err.message == breakMessage then err.value else p)
	  else if p == True then (
	       b := eval(listClause);
	       when b is err:Error
	       do return(if err.message == breakMessage then err.value else b)
	       else (
		    if i == length(r) then (
			 r = new Sequence len 2*length(r) do (
			      foreach x in r do provide x;
			      while true do provide nullE;
			      );
			 );
		    r.i = b;
		    i = i+1;
		    );
     	       c := eval(doClause);
	       when c is err:Error
	       do return(if err.message == breakMessage then err.value else c) 
	       else nothing;
	       )
	  else if p == False then break
	  else return(printErrorMessage(predicate,"expected true or false")));
     Expr(
	  list(
	       if i == 0 then emptySequence
	       else if i == length(r) then r
	       else new Sequence len i do foreach x in r do provide x)));
WhileListDoFun = whilelistdofun;

forfun(c:forCode):Expr := (
     fromClause := c.fromClause;
     toClause := c.toClause;
     predicate := c.whenClause;
     listClause := c.listClause;
     doClause := c.doClause;
     r := if listClause == dummyCode then emptySequence else new Sequence len 1 do provide nullE;
     i := 0;				    -- index in r
     j := 0;				    -- the value of the loop variable
     n := 0;				    -- the upper bound on j, if there is a toClause.
     if fromClause != dummyCode then (
     	  fromvalue := eval(fromClause);
     	  when fromvalue is f:Integer do (
	       if isInt(f) then j = toInt(f)
	       else return(printErrorMessage(fromClause,"expected a small integer"));
	       )
     	  else return(printErrorMessage(fromClause,"expected an integer")));
     if toClause != dummyCode then (
	  tovalue := eval(toClause);
	  when tovalue is f:Integer do (
	       if isInt(f) then n = toInt(f)
	       else return(printErrorMessage(toClause,"expected a small integer"));
	       )
	  else return(printErrorMessage(toClause,"expected an integer")));
     while true do (
	  if toClause != dummyCode && j > n then break;
	  localFrame.values.0 = toInteger(j);		    -- should be the frame spot for the loop var!
     	  j = j+1;
	  if predicate != dummyCode then (
	       p := eval(predicate);
	       when p is err:Error do (
		    if err.message == breakMessage then return(err.value)
		    else return(p)
		    )
	       else if p == False then break
	       else if p != True then return(printErrorMessage(predicate,"expected true or false"));
	       );
	  if listClause != dummyCode then (
	       b := eval(listClause);
	       when b is err:Error do (
		    if err.message == breakMessage then return(err.value)
		    else return(b)
		    )
	       else (
		    if i == length(r) then (
			 r = new Sequence len 2*length(r) do (
			      foreach x in r do provide x;
			      while true do provide nullE;
			      );
			 );
		    r.i = b;
		    i = i+1;
		    );
	       );
	  if doClause != dummyCode then (
	       b := eval(doClause);
	       when b is err:Error do (
		    if err.message == breakMessage then return(err.value)
		    else return(b)
		    )
	       else nothing;
	       );
	  );
     if listClause == dummyCode then nullE
     else Expr(
	  list(
	       if i == 0 then emptySequence
	       else if i == length(r) then r
	       else new Sequence len i do foreach x in r do provide x)));
ForFun = forfun;

setupconst("stdio",Expr(stdIO));
--setupconst("stdin",Expr(stdin));
--setupconst("stdout",Expr(stdout));
setupconst("stderr",Expr(stderr));
openfilesfun(e:Expr):Expr := (
     n := 0;
     ff := openfiles;
     while true do (
	  when ff
	  is null do break
	  is f:FileCell do (n=n+1; ff=f.next;));
     v := new Sequence len n do (
	  ff = openfiles;
	  while true do (
	       when ff
	       is null do break
	       is f:FileCell do (provide f.file; ff=f.next;));
	  );
     list(v));
setupfun("openFiles",openfilesfun);
openIn(filename:Expr):Expr := (
     when filename
     is f:file do (
	  when openIn(f)
	  is g:file do Expr(g)
	  is m:errmsg do buildErrorPacket(m.message))
     is f:string do (
	  when openIn(f)
	  is g:file do Expr(g)
	  is m:errmsg do buildErrorPacket(m.message))
     is Error do filename
     else WrongArg("a string"));
setupfun("openIn",openIn);
openOut(filename:Expr):Expr := (
     when filename
     is f:file do (
	  when openOut(f)
	  is g:file do Expr(g)
	  is m:errmsg do buildErrorPacket(m.message))
     is f:string do (
	  when openOut(f)
	  is g:file do Expr(g)
	  is m:errmsg do buildErrorPacket(m.message))
     is Error do filename
     else WrongArg("a string"));
setupfun("openOut",openOut);
openInOut(filename:Expr):Expr := (
     when filename
     is f:file do (
	  when openInOut(f)
	  is g:file do Expr(g)
	  is m:errmsg do buildErrorPacket(m.message))
     is f:string do (
	  when openInOut(f)
	  is g:file do Expr(g)
	  is m:errmsg do buildErrorPacket(m.message))
     is Error do filename
     else WrongArg("a string"));
setupfun("openInOut",openInOut);
openListener(filename:Expr):Expr := (
     when filename
     is f:string do (
	  when openListener(f)
	  is g:file do Expr(g)
	  is m:errmsg do buildErrorPacket(m.message))
     is Error do filename
     else WrongArg("a string"));
setupfun("openListener",openListener);
isOpenFile(e:Expr):Expr := (
     when e
     is f:file do toBoolean(f.listener || f.input || f.output)
     else False);
setupfun("isOpenFile",isOpenFile);
isInputFile(e:Expr):Expr := (
     when e
     is f:file do toBoolean(f.input)
     else False);
setupfun("isInputFile",isInputFile);
isOutputFile(e:Expr):Expr := (
     when e
     is f:file do toBoolean(f.output)
     else False);
setupfun("isOutputFile",isOutputFile);
isListener(e:Expr):Expr := (
     when e
     is f:file do toBoolean(f.listener)
     else False);
setupfun("isListener",isListener);
close(g:Expr):Expr := (
     when g
     is f:file do ( 
	  if !f.input && !f.output && !f.listener then return(buildErrorPacket("file already closed"));
	  if close(f) == 0 then g
	  else buildErrorPacket(if f.pid != 0 then "error return from child" else "error closing file"))
     is x:Database do dbmclose(x)
     else buildErrorPacket("expected a file or database"));
setupfun("close",close);
closeIn(g:Expr):Expr := (
     when g
     is f:file do ( 
	  if f.infd == -1 then return(buildErrorPacket("file already closed"));
	  if closeIn(f) == 0 then g
	  else buildErrorPacket(if f.pid != 0 then "error closing pipe" else "error closing file"))
     else buildErrorPacket("expected an open input file"));
setupfun("closeIn",closeIn);
closeOut(g:Expr):Expr := (
     when g
     is f:file do ( 
	  if f.infd == -1 && f.outfd == -1 then return(buildErrorPacket("file already closed"));
	  if closeOut(f) == 0 then g
	  else buildErrorPacket(if f.pid != 0 then "error closing pipe" else "error closing file"))
     else buildErrorPacket("expected an open output file"));
setupfun("closeOut",closeOut);
flush(g:Expr):Expr := (
     when g
     is f:file do (
	  if f.output
	  then (flush(f); g)
	  else WrongArg("an output file"))
     else WrongArg("a file"));
setupfun("flush",flush);
protect(e:Expr):Expr := (
     when e
     is q:SymbolClosure do (
	  if !q.symbol.protected then
	  if q.symbol.transientScope
	  then buildErrorPacket("can't protect a symbol with transient scope")
	  else (
	       q.symbol.protected = true; 
	       nullE
	       )
	  else nullE
	  )
     else WrongArg( "a symbol"));
setupfun("protect",protect);
flagSymbol(e:Expr):Expr := (
     when e
     is q:SymbolClosure do (
	  q.symbol.flagLookup = true; 
	  nullE
	  )
     else WrongArg("a symbol"));
setupfun("flag",flagSymbol);
--unprotect(e:Expr):Expr := (
--     when e
--     is q:SymbolClosure do (q.symbol.protected = false; nullE)
--     else WrongArg( "a symbol"));
--setupfun("unprotect",unprotect);
quoteF(rhs:Code):Expr := (
     when rhs
     is var:variableCode do Expr(makeSymbolClosure(var.v))
     else printErrorMessage(rhs,"expected a symbol"));
QuoteFun = quoteF;
-- setupfun("quote",quoteF);

export chars := new array(Expr) len 256 do (
     i := 0;
     while i<256 do (
	  provide Expr(string(char(i)));
	  i = i+1;
	  ));
getcfun(e:Expr):Expr := (
     when e
     is f:file do (
	  i := getc(f);
	  if i == -1 then Expr("") else chars.(i & 255))
     is Error do e
     else buildErrorPacket("expected an input file"));
setupfun("getc",getcfun);
leftshiftfun(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then (
	       when a.0 
	       is x:Integer do (
		    when a.1 is y:Integer do (
			 if isInt(y) 
			 then Expr(x << toInt(y))
			 else WrongArg(2,"a small integer"))
		    else WrongArg(2,"an integer"))
	       else  WrongArg(1,"an integer"))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(Expr(LessLessS),integerClass,integerClass,
     Expr(CompiledFunction(leftshiftfun,nextHash()))
     );

singleton(e:Expr):Expr := Expr(Sequence(e));
setupfun("singleton",singleton);
unSingleton(e:Expr):Expr := (
     when e
     is v:Sequence do if length(v) == 1 then v.0 else e
     else e);
setupfun("unSingleton",unSingleton);

sameFunctionBody(e:Expr):Expr := (
     when e is v:Sequence do
     if length(v) == 2 then
     when v.0 
     is f:FunctionClosure do (
	  when v.1
	  is g:FunctionClosure do toBoolean(f.model == g.model)
	  else False
	  )
     is f:CompiledFunctionClosure do (
	  when v.1
	  is g:CompiledFunctionClosure do toBoolean(f.fn == g.fn)
	  else False
	  )
     is f:CompiledFunction do toBoolean(v.0 == v.1)
     else False
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("sameFunctionBody", sameFunctionBody);
