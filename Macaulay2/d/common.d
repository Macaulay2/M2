--		Copyright 1994-2003 by Daniel R. Grayson

use C;
use system;
use binding;
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
use basic;

export setup(word:Word):void := (
     makeSymbol(word,dummyPosition,globalDictionary);
     );
export setup(word:Word,fn:unop):void := (
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.unary = fn;
     );
export setup(word:Word,fn:binop):void := (
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.binary = fn;
     );
export setup(word:Word,fun1:unop,fun2:binop):void := (
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.unary = fun1;
     e.binary = fun2;
     );
export setup(word:Word,fun1:unop,fun2:unop):void := (
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.unary = fun1;
     e.postfix = fun2;
     );
export setup(e:SymbolClosure,fn:unop):void := (
     e.symbol.unary = fn;
     );
export setuppostfix(e:SymbolClosure,fn:unop):void := (
     e.symbol.postfix = fn;
     );
export setup(e:SymbolClosure,fn:binop):void := (
     e.symbol.binary = fn;
     );
export setup(e:SymbolClosure,fun1:unop,fun2:binop):void := (
     e.symbol.unary = fun1;
     e.symbol.binary = fun2;
     );
export setup(e:SymbolClosure,fun1:unop,fun2:unop):void := (
     e.symbol.unary = fun1;
     e.symbol.postfix = fun2;
     );
export setupop(s:SymbolClosure,fun:unop):void := s.symbol.unary = fun;
export setupfun(name:string,fun:unop):Symbol := (
     word := makeUniqueWord(name,
	  parseinfo(precSpace,precSpace,precSpace,parsefuns(unaryop, defaultbinary)));
     entry := makeSymbol(word,dummyPosition,globalDictionary);
     entry.unary = fun;
     entry.protected = true;
     entry);     
export setupfun(name:string,value:fun):Symbol := (
     word := makeUniqueWord(name,parseWORD);
     entry := makeSymbol(word,dummyPosition,globalDictionary);
     globalFrame.values.(entry.frameindex) = Expr(CompiledFunction(value,nextHash()));
     entry.protected = true;
     entry);
export setupvar(name:string,value:Expr):Symbol := (
     word := makeUniqueWord(name,parseWORD);
     when lookup(word,globalDictionary)
     is null do (
     	  entry := makeSymbol(word,dummyPosition,globalDictionary);
     	  globalFrame.values.(entry.frameindex) = value;
	  entry)
     is entry:Symbol do (
	  -- we are doing it again after loading data with loaddata()
	  -- or we are reassigning to o or oo in interpret.d
     	  globalFrame.values.(entry.frameindex) = value;
	  entry));
export setupconst(name:string,value:Expr):Symbol := (
     s := setupvar(name,value);
     s.protected = true;
     s);
setup(commaW,dummyBinaryFun);


export returnMessage := "return value";
export breakMessage := "break value";

export buildErrorPacket(message:string):Expr := Expr(Error(dummyPosition,message,emptySequence,nullE));
export buildErrorPacket(message:string,report:Expr):Expr := Expr(Error(dummyPosition,message,report,nullE));

export quoteit(name:string):string := "'" + name + "'";
export NotYet(desc:string):Expr := buildErrorPacket(desc + " not implemented yet");
export WrongArg(desc:string):Expr := buildErrorPacket("expected " + desc);
export WrongArg(n:int,desc:string):Expr := (
     buildErrorPacket("expected argument " + tostring(n) + " to be " + desc));
export WrongArgInteger():Expr := WrongArg("an integer");
export WrongArgInteger(n:int):Expr := WrongArg(n,"an integer");
export WrongArgSmallInteger():Expr := WrongArg("a small integer");
export WrongArgSmallInteger(n:int):Expr := WrongArg(n,"a small integer");
export WrongArgString():Expr := WrongArg("a string");
export WrongArgString(n:int):Expr := WrongArg(n,"a string");
export WrongArgBoolean():Expr := WrongArg("true or false");
export WrongArgBoolean(n:int):Expr := WrongArg(n,"true or false");
export ArgChanged(name:string,n:int):Expr := (
     buildErrorPacket(quoteit(name) + " expected argument " + tostring(n)
	  + " not to change its type during execution"));
export WrongNumArgs(name:string,n:int):Expr := (
     if n == 0
     then buildErrorPacket(quoteit(name) + " expected no arguments")
     else if n == 1
     then buildErrorPacket(quoteit(name) + " expected " + tostring(n) + " argument")
     else buildErrorPacket(quoteit(name) + " expected " + tostring(n) + " arguments")
     );
export WrongNumArgs(n:int):Expr := buildErrorPacket(
     if n == 0 then "expected no arguments"
     else if n == 1 then "expected " + tostring(n) + " argument"
     else "expected " + tostring(n) + " arguments"
     );
export WrongNumArgs(name:string,m:int,n:int):Expr := (
     if n == m+1
     then buildErrorPacket(quoteit(name) + " expected " 
	  + tostring(m) + " or "
	  + tostring(n) + " arguments")
     else buildErrorPacket(quoteit(name) + " expected " 
	  + tostring(m) + " to "
	  + tostring(n) + " arguments"));
export WrongNumArgs(m:int,n:int):Expr := (
     if n == m+1
     then buildErrorPacket("expected " + tostring(m) + " or " + tostring(n) + " arguments")
     else buildErrorPacket("expected " + tostring(m) + " to " + tostring(n) + " arguments"));
export TooFewArgs(name:string,m:int):Expr := (
     if m == 1
     then buildErrorPacket(quoteit(name) + " expected at least 1 argument")
     else buildErrorPacket(quoteit(name) + " expected at least " 
	  + tostring(m) + " arguments"));
export TooManyArgs(name:string,m:int):Expr := (
     if m == 1
     then buildErrorPacket(quoteit(name) + " expected at most 1 argument")
     else buildErrorPacket(quoteit(name) + " expected at most " 
	  + tostring(m) + " arguments"));
export errorDepth := 0;
export printErrorMessage(e:Code,message:string):Expr := (
     p := codePosition(e);
     if int(p.loadDepth) >= errorDepth
     then (
     	  printErrorMessage(p,message);
     	  Expr(Error(p,message,emptySequence,nullE)))
     else buildErrorPacket(message));
export printErrorMessage(e:Code,message:string,report:Expr):Expr := (
     p := codePosition(e);
     if int(p.loadDepth) >= errorDepth
     then (
     	  printErrorMessage(p,message);
     	  Expr(Error(p,message,report,nullE)))
     else buildErrorPacket(message));

export backtr(z:Expr):Expr := (
     when z is err:Error do 
     if err.position == dummyPosition 
     || int(err.position.loadDepth) < errorDepth
     || SuppressErrors
     then z
     else buildErrorPacket("--backtrace--",err.report)
     else z);
export backtr(z:Expr,report:Expr):Expr := (
     when z is err:Error do (
	  err.report = Expr(Sequence(report,err.report));
	  backtr(z))
     else z);
export backtrFunction(z:Expr):Expr := (
     when z is err:Error do (
	  if err.message == returnMessage 
	  then err.value
	  else backtr(z))
     else z);
export backtrFunction(z:Expr,report:Expr):Expr := (
     when z is err:Error do (
	  if err.message == returnMessage 
	  then err.value
	  else backtr(z,report))
     else z);
export backtrLoop(z:Expr):Expr := (
     when z is err:Error do (
	  if err.message == breakMessage then err.value
	  else backtr(z))
     else z);
export backtrLoop(z:Expr,report:Expr):Expr := (
     when z is err:Error do (
	  if err.message == breakMessage then err.value
	  else backtr(z,report))
     else z);
export WrongNumArgs(c:Code,wanted:int,got:int):Expr := (
     printErrorMessage(c, "expected " + tostring(wanted) + " argument"
	  + (if wanted == 1 then "" else "s") + ", but got "
	  + tostring(got)));

export MissingMethod(name:string,method:string):Expr := (
     buildErrorPacket(quoteit(name) + " expected item to have a method for " + method));
export MissingMethod(method:SymbolClosure):Expr := (
     buildErrorPacket("expected a method for "+quoteit(method.symbol.word.name)));
export MissingMethodPair(method:string):Expr := (
     buildErrorPacket("expected pair to have a method for "+quoteit(method)));
export MissingMethodPair(method:SymbolClosure):Expr := (
     buildErrorPacket("expected pair to have a method for "+
	  quoteit(method.symbol.word.name)));
export MissingMethodPair(method:SymbolClosure,left:Expr,right:Expr):Expr := (
     backtr(
	  buildErrorPacket(
	       "expected pair to have a method for "
	       +quoteit(method.symbol.word.name)
	       ),
	  list(Expr(method),left,right)));

-----------------------------------------------------------------------------
-- Database stuff
export dbmcheck(ret:int):Expr := (
     if ret == -1 then buildErrorPacket(dbmstrerror())
     else Expr(toInteger(ret)));
export dbmopenin(filename:string):Expr := (
     mutable := false;
     handle := dbmopen(filename,mutable);
     if handle == -1 
     then buildErrorPacket(dbmstrerror() + " : " + filename)
     else Expr(Database(filename,nextHash(),handle,true,mutable)));
export dbmopenout(filename:string):Expr := (
     mutable := true;
     handle := dbmopen(filename,mutable);
     if handle == -1 
     then buildErrorPacket(dbmstrerror() + " : " + filename)
     else Expr(Database(filename,nextHash(),handle,true,mutable)));
export dbmclose(f:Database):Expr := (
     if !f.isopen then return buildErrorPacket("database already closed");
     dbmclose(f.handle);
     f.isopen = false;
     Expr(toInteger(0)));
export dbmstore(f:Database,key:string,content:string):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     if !f.mutable then return buildErrorPacket("database not mutable");
     ret := dbmstore(f.handle,key,content);
     if 0 == ret then Expr(content)
     else dbmcheck(ret));
export dbmquery(f:Database,key:string):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     when dbmfetch(f.handle,key)
     is a:string do True
     else False);
export dbmfirst(f:Database):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     when dbmfirst(f.handle)
     is a:string do Expr(a)
     else nullE);
export dbmreorganize(f:Database):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     if !f.mutable then return buildErrorPacket("database not mutable");
     dbmcheck(dbmreorganize(f.handle)));

