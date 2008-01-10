--		Copyright 1994 by Daniel R. Grayson
use C;
use system; 
use convertr;
use binding;
use evaluate;
use common;
use parser;
use lex;
use gmp;
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
use util;

dbmfirst(e:Expr):Expr := (
     when e
     is f:Database do dbmfirst(f)
     else buildErrorPacket("expected a database"));
setupfun("firstkey",dbmfirst);
dbmnext(f:Database):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     when dbmnext(f.handle)
     is a:string do Expr(a)
     else nullE);
dbmnext(e:Expr):Expr := (
     when e
     is f:Database do dbmnext(f)
     is Sequence do WrongNumArgs(1)
     else WrongArg(1,"a database"));
setupfun("nextkey",dbmnext);
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

keys(e:Expr):Expr := (
     when e
     is o:DictionaryClosure do keys(o.dictionary)
     is f:Database do keys(f)
     is o:HashTable do keys(o)
     else WrongArg("a hash table, database, or dictionary"));
setupfun("keys",keys);
toList(e:Expr):Expr := (
     when e
     is o:HashTable do if ancestor(o.class,Set) then keys(o) else WrongArg("list, sequence, or set")
     -- is o:DictionaryClosure do keys(o.dictionary)
     is a:Sequence do list(a)
     is b:List do (
	  if b.class == listClass then e
	  else Expr(
	       sethash(
	       	    List(listClass, if b.mutable then copy(b.v) else b.v,
		    	 0, false),
	       	    false)))
     else WrongArg("list, sequence, or set"));
setupfun("toList",toList);
values(e:Expr):Expr := (
     when e
     is oc:DictionaryClosure do (
	  o := oc.dictionary;
	  f := oc.frame;
	  Expr(list(
		    new Sequence len o.symboltable.numEntries do
		    foreach bucket in o.symboltable.buckets do (
			 p := bucket;
			 while true do (
			      when p
			      is q:SymbolListCell do (
				   provide Expr(SymbolClosure(f,q.entry));
				   p=q.next;)
			      else break;
			      )))))
     is o:HashTable do list(
	  new Sequence len o.numEntries do
	  foreach bucket in o.table do (
	       p := bucket;
	       while p != p.next do (
		    provide Expr(p.value);
		    p = p.next; )))
     else WrongArg("a hash table or dictionary"));
setupfun("values",values);

pairs(e:Expr):Expr := (
     when e
     is oc:DictionaryClosure do (
	  o := oc.dictionary;
	  f := oc.frame;
	  Expr(list(
		    new Sequence len o.symboltable.numEntries do (
			 foreach bucket in o.symboltable.buckets do (
			      p := bucket;
			      while true do (
				   when p
				   is q:SymbolListCell do (
					provide Expr(Sequence(Expr(q.word.name),Expr(SymbolClosure(f,q.entry))));
					p=q.next;)
				   else break; ));
			 ))))
     is o:HashTable do list(
	  new Sequence len o.numEntries do
	  foreach bucket in o.table do (
	       p := bucket;
	       while p != p.next do (
		    provide Expr(Sequence(p.key,p.value));
		    p = p.next; )))
     else WrongArg("a hash table or a raw polynomial"));
setupfun("pairs",pairs);

-- operators

basictype(e:Expr):HashTable := basictype(Class(e));
basictypefun(e:Expr):Expr := Expr(basictype(e));
setupfun("basictype",basictypefun);

expected(type:string,returned:bool):Expr := buildErrorPacket(
     if returned 
     then "'new' expected method to return " + type
     else "expected " + type + " (in absence of a 'new' method)"
     );

wrongTarget():Expr := buildErrorPacket("'new' expected a type of list or hash table");

newClassParent(e:Expr,class:HashTable,parent:HashTable,returned:bool):Expr := (
     -- same as below, but parent specified
     basicType := basictype(class);
     when e
     is Error do e
     is o:HashTable do (
	  if basicType == hashTableClass then (
	       if o.class == class && o.parent == parent then e
	       else (
	       	    mutable := ancestor(class,mutableHashTableClass);
		    x := HashTable( if mutable || o.mutable then copy(o.table) else o.table, class, parent, o.numEntries, 0, mutable);
		    if mutable then (
			 if class != cacheTableClass then x.hash = nextHash(); -- cache tables are mutable and have hash code 0
			 )
		    else x.hash = hash(x);
		    Expr(x)))
	  else if basicType == basicListClass then expected("a list",returned)
	  else wrongTarget())
     is o:List do (
     	  if basicType == basicListClass then (
	       if parent != nothingClass
	       then buildErrorPacket("expected Nothing as parent for list")
	       else if o.class == class then e
	       else if class == sequenceClass then Expr(o.v)
	       else (
	       	    mutable := ancestor(class,mutableListClass);
		    Expr(sethash( List(class, if mutable || o.mutable then copy(o.v) else o.v, 0,false), mutable))))
	  else if basicType == hashTableClass then expected("a hash table",returned)
	  else wrongTarget())
     is v:Sequence do (
     	  if basicType == basicListClass then (
	       if parent != nothingClass then buildErrorPacket("expected Nothing as parent for list")
	       else if class == sequenceClass then Expr(v)
	       else (
	       	    mutable := ancestor(class,mutableListClass);
		    Expr( sethash( List(class, if mutable then copy(v) else v, 0,false), mutable))))
	  else if basicType == hashTableClass then expected("a hash table",returned)
	  else wrongTarget())
     is s:SpecialExpr do (
	  if s.class == class && Parent(s.e) == parent then e else newClassParent(s.e,class,parent,returned)
	  )
     else (
	  c := Class(e);
	  if c == class then e
	  else if !ancestor(class,c) then buildErrorPacket("expected new class to be a specialization of the old one")
	  else if Parent(e) != parent then buildErrorPacket("unable to set new parent")
	  else Expr(SpecialExpr(class,e))));

newClass(e:Expr,class:HashTable,returned:bool):Expr := (
     -- same as above, but no parent specified, so leave what s provided alone
     when e
     is Error do e
     is o:HashTable do (
	  basicType := basictype(class);
	  if basicType == hashTableClass then (
	       if o.class == class then e
	       else (
		    mutable := ancestor(class,mutableHashTableClass);
		    x := HashTable(
			 if mutable || o.mutable then copy(o.table) else o.table,
			 class, o.parent, o.numEntries, 0, mutable);
		    if mutable then (
			 if class != cacheTableClass then x.hash = nextHash();
			 )
		    else x.hash = hash(x);
		    Expr(x))
	       )
	  else if basicType == basicListClass then expected("a list",returned)
	  else wrongTarget())
     is o:List do (
	  basicType := basictype(class);
	  if basicType == basicListClass then (
	       if o.class == class then e
	       else if class == sequenceClass then Expr(o.v)
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
     is v:Sequence do (
	  basicType := basictype(class);
	  if basicType == basicListClass then (
	       if class == sequenceClass then Expr(v)
	       else (
		    mutable := ancestor(class,mutableListClass);
		    Expr( sethash( List(class, if mutable then copy(v) else v, 0,false), mutable))))
	  else if basicType == hashTableClass 
	  then expected("a hash table",returned)
	  else wrongTarget())
     is s:SpecialExpr do if s.class == class then e else newClass(s.e,class,returned)
     else (
	  c := Class(e);
	  if c == class then e
	  else if !ancestor(class,c) 
	  then buildErrorPacket("expected new class to be a specialization of the old one")
	  else Expr(SpecialExpr(class,e))));
newclassfun(e:Expr):Expr := (
     when e
     is a:Sequence do
     if length(a) == 2
     then when a.0
     is class:HashTable do newClass(a.1,class,false)
     else WrongArg(1,"a hash table")
     else if length(a) == 3
     then when a.0
     is class:HashTable do (
	  when a.1
	  is parent:HashTable do newClassParent(a.2,class,parent,false)
	  else WrongArg(2,"a hash table"))
     else WrongArg(1,"a hash table")
     else WrongNumArgs(2,3)
     else WrongNumArgs(2,3));
setupfun("newClass",newclassfun);

makenew(class:HashTable,parent:HashTable):Expr := (
     basicType := basictype(class);
     if basicType == hashTableClass 
     then if parent == sequenceClass then buildErrorPacket("can't make subclass of Sequence")
     else (
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
     else if basicType == basicListClass then (
	  if parent != nothingClass
	  then buildErrorPacket("expected Nothing as parent for list")
	  else if class == sequenceClass
	  then Expr(emptySequence)
	  else Expr(
	       sethash(
		    List(class,emptySequence,0,false),
		    ancestor(class,mutableListClass))))
     else if basicType == dictionaryClass then Expr(DictionaryClosure(globalFrame,newGlobalDictionary()))
     else buildErrorPacket("basic type for 'new' method should have been BasicList or HashTable"));
makenew(class:HashTable):Expr := makenew(class,nothingClass);
-----------------------------------------------------------------------------

errt(newClassCode :Code):Expr := printErrorMessageE(newClassCode ,"new: expected a hash table as prospective class");
errp(newParentCode:Code):Expr := printErrorMessageE(newParentCode,"new: expected a hash table as prospective parent");

newfun(newClassCode:Code):Expr := (
     classExpr := eval(newClassCode);
     when classExpr 
     is Error do classExpr
     is class:HashTable do (
	  method := lookup(class,NewS);
	  if method != nullE
	  then newClass(applyEE(method,Expr(class)),class,true)
	  else makenew(class))
     else errt(newClassCode));
NewFun = newfun;
newoffun(newClassCode:Code,newParentCode:Code):Expr := (
     classExpr := eval(newClassCode);
     when classExpr 
     is Error do classExpr
     is class:HashTable do (
	  newParentExpr := eval(newParentCode);
	  when newParentExpr
	  is Error do newParentExpr
	  is parent:HashTable do (
	       method := lookupBinaryMethod(class,parent,NewOfS);
	       if method != nullE
	       then newClassParent(applyEEE(method,Expr(class),Expr(parent)),class,parent,true)
	       else makenew(class,parent))
	  else errp(newParentCode))
     else errt(newClassCode));
NewOfFun = newoffun;
newfromfun(newClassCode:Code,newInitCode:Code):Expr := (
     classExpr := eval(newClassCode);
     when classExpr 
     is Error do classExpr
     is class:HashTable do (
	  newInitExpr := eval(newInitCode);
	  when newInitExpr
	  is Error do newInitExpr
	  else (
	       method := lookupBinaryMethod(class,Class(newInitExpr),NewFromS);
	       if method != nullE
	       then newClass(applyEEE(method,Expr(class),newInitExpr),class,true)
	       else (
		    when newInitExpr
		    is s:Sequence do newClass(newInitExpr,class,false)
		    is p:List do (
			 if p.class == class
			 then Expr(if p.mutable then copy(p) else p)
			 else newClass(newInitExpr,class,false))
		    is p:HashTable do (
			 if p.class == class
			 then Expr(if p.mutable then copy(p) else p)
			 else newClass(newInitExpr,class,false))
		    else newClass(newInitExpr,class,false))))
     else errt(newClassCode));
NewFromFun = newfromfun;
newoffromfun(newClassCode:Code,newParentCode:Code,newInitCode:Code):Expr := (
     classExpr := eval(newClassCode);
     when classExpr 
     is Error do classExpr
     is class:HashTable do (
	  newParentExpr := eval(newParentCode);
	  when newParentExpr
	  is Error do newParentExpr
	  is parent:HashTable do (
	       newInitExpr := eval(newInitCode);
	       when newInitExpr
	       is Error do newInitExpr
	       else (
		    method := lookupTernaryMethod(class,parent,Class(newInitExpr),NewOfFromE,NewOfFromS.symbol.hash);
		    if method != nullE 
		    then newClassParent(applyEEE(method,Expr(class),Expr(parent),newInitExpr),class,parent,true)
		    else (
			 when newInitExpr
			 is p:Sequence do newClassParent(newInitExpr,class,parent,false)
			 is p:List do (
			      if p.class == class && nothingClass == parent
			      then Expr(if p.mutable then copy(p) else p)
			      else newClassParent(newInitExpr,class,parent,false))
			 is p:HashTable do (
			      if p.class == class && p.parent == parent
			      then Expr(if p.mutable then copy(p) else p)
			      else newClassParent(newInitExpr,class,parent,false))
			 else newClassParent(newInitExpr,class,parent,false))))
	  else errp(newParentCode))
     else errt(newClassCode));
NewOfFromFun = newoffromfun;
-----------------------------------------------------------------------------

export stdioS  := setupconst("stdio", Expr(stdIO));
export stderrS := setupconst("stderr",Expr(stderr));

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
     else WrongArgString());
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
     else WrongArgString());
setupfun("openOut",openOut);
openOutAppend(filename:Expr):Expr := (
     when filename
     is f:string do (
	  when openOutAppend(f)
	  is g:file do Expr(g)
	  is m:errmsg do buildErrorPacket(m.message))
     is Error do filename
     else WrongArgString());
setupfun("openOutAppend",openOutAppend);
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
     else WrongArgString());
setupfun("openInOut",openInOut);
openListener(filename:Expr):Expr := (
     when filename
     is f:string do (
	  when openListener(f)
	  is g:file do Expr(g)
	  is m:errmsg do buildErrorPacket(m.message))
     is Error do filename
     else WrongArgString());
setupfun("openListener",openListener);
isOpen(e:Expr):Expr := (
     when e
     is f:file do toExpr(f.listener || f.input || f.output)
     is x:Database do toExpr(x.isopen)
     else WrongArg("a file or database"));
setupfun("isOpen",isOpen);
isInputFile(e:Expr):Expr := (
     when e
     is f:file do toExpr(f.input)
     else False);
setupfun("isInputFile",isInputFile);
isOutputFile(e:Expr):Expr := (
     when e
     is f:file do toExpr(f.output)
     else False);
setupfun("isOutputFile",isOutputFile);
isListener(e:Expr):Expr := (
     when e
     is f:file do toExpr(f.listener)
     else False);
setupfun("isListener",isListener);
close(g:Expr):Expr := (
     when g
     is f:file do ( 
	  if !f.input && !f.output && !f.listener then return buildErrorPacket("file already closed");
	  if close(f) == 0 then g
	  else buildErrorPacket(if f.pid != 0 then "error return from child" else "error closing file"))
     is x:Database do dbmclose(x)
     else buildErrorPacket("expected a file or database"));
setupfun("close",close).protected = false;
closeIn(g:Expr):Expr := (
     when g
     is f:file do ( 
	  if f.infd == -1 then return buildErrorPacket("file already closed");
	  if closeIn(f) == 0 then g
	  else buildErrorPacket(if f.pid != 0 then "error closing pipe" else "error closing file"))
     else buildErrorPacket("expected an open input file"));
setupfun("closeIn",closeIn).protected = false;
closeOut(g:Expr):Expr := (
     when g
     is f:file do ( 
	  if f.infd == -1 && f.outfd == -1 then return buildErrorPacket("file already closed");
	  if closeOut(f) == 0 then g
	  else buildErrorPacket(if f.pid != 0 then "error closing pipe" else "error closing file"))
     else buildErrorPacket("expected an open output file"));
setupfun("closeOut",closeOut).protected = false;
flush(g:Expr):Expr := (
     when g
     is f:file do (
	  if f.output
	  then (flush(f); g)
	  else WrongArg("an output file"))
     else WrongArg("a file"));
setupfun("flush",flush).protected = false;
protect(e:Expr):Expr := (
     when e
     is dc:DictionaryClosure do (
	  d := dc.dictionary;
	  okay := false;
	  t := globalDictionary;
	  while (
	       if t != d && !t.protected then okay = true;
	       t != t.outerDictionary ) do t = t.outerDictionary;
	  if !okay then buildErrorPacket("tried to protect last remaining visible dictionary")
	  else (
	       d.protected = true;
	       e))
     is q:SymbolClosure do (
	  q.symbol.protected = true; 
	  e)
     else WrongArg("a symbol or a dictionary"));
setupfun("protect",protect);
flagSymbol(e:Expr):Expr := (
     when e
     is q:SymbolClosure do (
	  q.symbol.flagLookup = !q.symbol.flagLookup;
	  Expr(Boolean(q.symbol.flagLookup)))
     else WrongArg("a symbol"));
setupfun("flagLookup",flagSymbol);

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
	       is x:ZZ do (
		    when a.1 is y:ZZ do (
			 if isInt(y) 
			 then Expr(x << toInt(y))
			 else WrongArgSmallInteger(2))
		    else WrongArgInteger(2))
	       is x:RR do (
		    when a.1 is y:ZZ do (
			 if isLong(y) 
			 then Expr(x << toLong(y))
			 else WrongArgSmallInteger(2))
		    else WrongArgInteger(2))
	       is x:CC do (
		    when a.1 is y:ZZ do (
			 if isLong(y) 
			 then Expr(x << toLong(y))
			 else WrongArgSmallInteger(2))
		    else WrongArgInteger(2))
	       else WrongArg(1,"an integral, real, or complex number"))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(Expr(LessLessS),ZZClass,ZZClass,
     Expr(CompiledFunction(leftshiftfun,nextHash()))
     );
installMethod(Expr(LessLessS),RRClass,ZZClass,
     Expr(CompiledFunction(leftshiftfun,nextHash()))
     );

rightshiftfun(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then (
	       when a.0 
	       is x:ZZ do (
		    when a.1 is y:ZZ do (
			 if isInt(y) 
			 then Expr(x >> toInt(y))
			 else WrongArgSmallInteger(2))
		    else WrongArgInteger(2))
	       is x:RR do (
		    when a.1 is y:ZZ do (
			 if isLong(y) 
			 then Expr(x >> toLong(y))
			 else WrongArgSmallInteger(2))
		    else WrongArgInteger(2))
	       is x:CC do (
		    when a.1 is y:ZZ do (
			 if isLong(y) 
			 then Expr(x >> toLong(y))
			 else WrongArgSmallInteger(2))
		    else WrongArgInteger(2))
	       else  WrongArgInteger(1))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(Expr(GreaterGreaterS),ZZClass,ZZClass,
     Expr(CompiledFunction(rightshiftfun,nextHash()))
     );

unSingleton(e:Expr):Expr := (
     when e
     is v:Sequence do if length(v) == 1 then v.0 else e
     else e);
setupfun("unsequence",unSingleton);

disassemble(e:Expr):Expr := (
     when e
     is f:FunctionClosure do Expr(tostring(Code(f.model)))
     is f:functionCode do Expr(tostring(Code(f)))
     is c:CodeClosure do Expr(tostring(c.code))
     is s:SpecialExpr do disassemble(s.e)
     else WrongArg("pseudocode or a function closure derived from Macaulay 2 code")
     );
setupfun("disassemble", disassemble);

pseudocode(e:Expr):Expr := (
     when e
     is f:FunctionClosure do Expr(CodeClosure(f.frame, Code(f.model)))
     is s:SpecialExpr do pseudocode(s.e)
     else WrongArg("a function closure derived from Macaulay 2 code")
     );
setupfun("pseudocode", pseudocode);

cpuTime(e:Expr):Expr := (
     when e
     is s:Sequence do if length(s) == 0 then toExpr(etime())
     else WrongNumArgs(0)
     else WrongNumArgs(0));
setupfun("cpuTime",cpuTime);

timefun(a:Code):Expr := (
     v := etime();
     ret := eval(a);
     x := etime();
     y := etime();
     when ret
     is Error do ret
     else list(timeClass,Sequence(toExpr((x-v)-(y-x)),ret)));
setupop(timingS,timefun);
showtimefun(a:Code):Expr := (
     v := etime();
     ret := eval(a);
     x := etime();
     y := etime();
     stdout << "     -- used " << (x-v)-(y-x) << " seconds" << endl;
     ret);
setupop(timeS,showtimefun);

realPart(e:Expr):Expr := (
     when e
     is ZZ do e
     is RR do e
     is z:CC do Expr(realPart(z))
     is QQ do e
     else WrongArg("a number"));
setupfun("realPart",realPart);
imaginaryPart(e:Expr):Expr := (
     when e
     is ZZ do Expr(toInteger(0))
     is RR do toExpr(0)
     is z:CC do Expr(imaginaryPart(z))
     is QQ do toExpr(0)
     else WrongArg("a number"));
setupfun("imaginaryPart",imaginaryPart);

Foo := { foo:void };					    -- make a new type of pointer that's innocuous and unusable
finalizer(x:Foo,msg:string):void := (
     stderr << "--finalization: " << msg << endl ;
     );
finalizerCount := 0;
registerFinalizer(e:Expr):Expr := (
     when e is s:Sequence do (
	  if length(s) != 2 then WrongNumArgs(2) else
 	  when s.1 is msg:string do (
	       msg = "[" + tostring(finalizerCount) + "]: " + msg;
	       finalizerCount = finalizerCount + 1;
	       Ccode(void, "GC_register_finalizer((void *)",e,".ptr_,(GC_finalization_proc)",finalizer,",",msg,",0,0)");
	       toExpr(finalizerCount))
     	  else WrongArgString(2))
     else WrongNumArgs(2));
setupfun("registerFinalizer",registerFinalizer);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
