--		Copyright 1994-2003 by Daniel R. Grayson

use basic;
use binding;

export codePosition(c:Code):Position := (
     when c
     is f:adjacentCode do f.position
     is f:arrayCode do f.position
     is f:angleBarListCode do f.position
     is f:binaryCode do f.position
     is f:catchCode do f.position
     is f:Error do f.position
     is f:forCode do f.position
     is f:functionCode do position(f.arrow)
     is f:globalAssignmentCode do f.position
     is f:globalMemoryReferenceCode do f.position
     is f:globalSymbolClosureCode do f.position
     is f:ifCode do f.position
     is f:integerCode do f.position
     is f:listCode do f.position
     is f:localAssignmentCode do f.position
     is f:localMemoryReferenceCode do f.position
     is f:localSymbolClosureCode do f.position
     is f:multaryCode do f.position
     is f:newCode do f.position
     is f:newFromCode do f.position
     is f:newLocalFrameCode do codePosition(f.body)
     is f:newOfCode do f.position
     is f:newOfFromCode do f.position
     is f:nullCode do dummyPosition
     is f:parallelAssignmentCode do f.position
     is f:realCode do f.position
     is f:semiCode do f.position
     is f:sequenceCode do f.position
     is f:stringCode do dummyPosition
     is f:ternaryCode do f.position
     is f:threadMemoryReferenceCode do f.position
     is f:threadSymbolClosureCode do f.position
     is f:tryCode do f.position
     is f:unaryCode do f.position
     is f:whileDoCode do f.position
     is f:whileListCode do f.position
     is f:whileListDoCode do f.position
     );

export pos(c:Code):void := (					    -- for use in the debugger
     stdIO << codePosition(c) << endl;
     );

export tostring(c:Code):string := (
     when c
     is x:Error do concatenate(array(string)( "(error \"", x.message, "\")"))
     is x:semiCode do concatenate(array(string)( "(semi ", between(" ",new array(string) len length(x.w) do foreach s in x.w do provide tostring(s)), ")"))
     is x:arrayCode do concatenate(array(string)( "(array ", between(" ",new array(string) len length(x.z) do foreach s in x.z do provide tostring(s)), ")"))
     is x:angleBarListCode do concatenate(array(string)( "(angleBarList ", between(" ",new array(string) len length(x.t) do foreach s in x.t do provide tostring(s)), ")"))
     is x:binaryCode do concatenate(array(string)("(2-OP ",getBinopName(x.f)," ",tostring(x.lhs)," ",tostring(x.rhs),")"))
     is x:adjacentCode do concatenate(array(string)("(adjacent ",tostring(x.lhs)," ",tostring(x.rhs),")"))
     is x:forCode do concatenate(array(string)(
	       "(for", 
	       " in: ",tostring(x.inClause),
	       " from: ",tostring(x.fromClause),
	       " to: ",tostring(x.toClause),
	       " when: ",tostring(x.whenClause),
	       " list: ",tostring(x.listClause),
	       " do: ",tostring(x.doClause),
	       ")"))
     is x:whileListDoCode do concatenate(array(string)( "(while ",tostring(x.predicate), " list: ",tostring(x.listClause), " do: ",tostring(x.doClause), ")"))
     is x:newOfFromCode do concatenate(array(string)( "(new ",tostring(x.newClause), " of: ",tostring(x.ofClause), " from: ",tostring(x.fromClause), ")"))
     is x:newFromCode do concatenate(array(string)( "(new ",tostring(x.newClause), " from: ",tostring(x.fromClause), ")"))
     is x:newOfCode do concatenate(array(string)( "(new ",tostring(x.newClause), " of: ",tostring(x.ofClause), ")"))
     is x:newCode do concatenate(array(string)( "(new ",tostring(x.newClause), ")"))
     is x:whileDoCode do concatenate(array(string)( "(while ",tostring(x.predicate), " do: ",tostring(x.doClause), ")"))
     is x:whileListCode do concatenate(array(string)( "(while ",tostring(x.predicate), " list: ",tostring(x.listClause), ")"))
     is x:functionCode do concatenate(array(string)(
	       "(function restargs: ",tostring(x.desc.restargs),
	       " numparms: ",tostring(x.desc.numparms),
	       " framesize: ",tostring(x.desc.framesize),
	       " frameID: ",tostring(x.desc.frameID),
	       " ",tostring(x.body),")"))
     is x:globalAssignmentCode do concatenate(array(string)("(global-assign ",x.lhs.word.name," ",tostring(x.rhs),")"))
     is x:localAssignmentCode do concatenate(array(string)("(local-assign ",tostring(x.frameindex)," ",tostring(x.nestingDepth)," ",tostring(x.rhs),")"))
     is x:globalMemoryReferenceCode do concatenate(array(string)("(global-fetch ",tostring(x.frameindex),")"))
     is x:threadMemoryReferenceCode do concatenate(array(string)("(thread-fetch ",tostring(x.frameindex),")"))
     is x:globalSymbolClosureCode  do concatenate(array(string)("(global ",x.symbol.word.name,")"))
     is x:threadSymbolClosureCode  do concatenate(array(string)("(thread ",x.symbol.word.name,")"))
     is x:integerCode do tostring(x.x)
     is x:listCode do concatenate(array(string)( "(list ", between(" ",new array(string) len length(x.y) do foreach s in x.y do provide tostring(s)), ")"))
     is x:localMemoryReferenceCode do concatenate(array(string)("(fetch ",tostring(x.frameindex)," ",tostring(x.nestingDepth),")"))
     is x:localSymbolClosureCode do concatenate(array(string)("(local ",x.symbol.word.name," nestingDepth: ",tostring(x.nestingDepth),")"))
     is x:multaryCode do concatenate(array(string)( "(OP ",getMultopName(x.f)," ", between(" ",new array(string) len length(x.args) do foreach c in x.args do provide tostring(c)), ")" ))
     is x:newLocalFrameCode do concatenate(array(string)())
     is x:nullCode do "(null)"
     is x:parallelAssignmentCode do (
	  n := length(x.nestingDepth);
	  concatenate(
	       array(string)(
	       	    "(parallel-assign (",
		    between(" ",
			 new array(string) len length(x.nestingDepth) do 
			 for i from 0 to n-1 do 
			 if x.lhs.i == dummySymbol 
			 then provide concatenate(array(string)("(",tostring(x.frameindex.i)," ",tostring(x.nestingDepth.i),")"))
			 else provide join("'",x.lhs.i.word.name)),
		    ") ", tostring(x.rhs), ")" ) ) )
     is x:realCode do tostringRR(x.x)
     is x:sequenceCode do (
	  concatenate(array(string)(
		    "(sequence ",
		    between(" ",new array(string) len length(x.x) do foreach s in x.x do provide tostring(s)),
     	       	    ")")))
     is x:stringCode do concatenate(array(string)("\"",present(x.x),"\""))
     is x:ternaryCode do concatenate(array(string)("(3-OP ",getTernopName(x.f)," ",tostring(x.arg1)," ",tostring(x.arg2)," ",tostring(x.arg3),")"))
     is x:ifCode do concatenate(array(string)("(if ",tostring(x.predicate)," then: ",tostring(x.thenClause)," else: ",tostring(x.elseClause),")"))
     is x:tryCode do concatenate(array(string)("(try ",tostring(x.code)," ",tostring(x.thenClause)," ",tostring(x.elseClause),")"))
     is x:catchCode do concatenate(array(string)("(catch ",tostring(x.code),")"))
     is x:unaryCode do concatenate(array(string)("(1-OP ",getUnopName(x.f)," ",tostring(x.rhs),")"))
     );

export setup(word:Word):void := (
     makeSymbol(word,dummyPosition,globalDictionary);
     );
export setup(word:Word,fn:unop):void := (
     unopNameList = unopNameListCell(fn,word.name,unopNameList);
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.unary = fn;
     );
export setup(word:Word,fn:binop):void := (
     binopNameList = binopNameListCell(fn,word.name,binopNameList);
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.binary = fn;
     );
export setup(word:Word,fun1:unop,fun2:binop):void := (
     unopNameList = unopNameListCell(fun1,word.name,unopNameList);
     binopNameList = binopNameListCell(fun2,word.name,binopNameList);
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.unary = fun1;
     e.binary = fun2;
     );
export setup(word:Word,fun1:unop,fun2:unop):void := (
     unopNameList = unopNameListCell(fun1,word.name,unopNameList);
     unopNameList = unopNameListCell(fun2,word.name,unopNameList);
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.unary = fun1;
     e.postfix = fun2;
     );
export setup(e:SymbolClosure,fn:unop):void := (
     unopNameList = unopNameListCell(fn,e.symbol.word.name,unopNameList);
     e.symbol.unary = fn;
     );
export setuppostfix(e:SymbolClosure,fn:unop):void := (
     unopNameList = unopNameListCell(fn,e.symbol.word.name,unopNameList);
     e.symbol.postfix = fn;
     );
export setup(e:SymbolClosure,fn:binop):void := (
     binopNameList = binopNameListCell(fn,e.symbol.word.name,binopNameList);
     e.symbol.binary = fn;
     );
export setup(e:SymbolClosure,fun1:unop,fun2:binop):void := (
     unopNameList = unopNameListCell(fun1,e.symbol.word.name,unopNameList);
     binopNameList = binopNameListCell(fun2,e.symbol.word.name,binopNameList);
     e.symbol.unary = fun1;
     e.symbol.binary = fun2;
     );
export setup(e:SymbolClosure,fun1:unop,fun2:unop):void := (
     unopNameList = unopNameListCell(fun1,e.symbol.word.name,unopNameList);
     unopNameList = unopNameListCell(fun2,e.symbol.word.name,unopNameList);
     e.symbol.unary = fun1;
     e.symbol.postfix = fun2;
     );
export setupop(s:SymbolClosure,fun:unop):void := (
     unopNameList = unopNameListCell(fun,s.symbol.word.name,unopNameList);
     s.symbol.unary = fun;
     );
export setupfun(name:string,fun:unop):Symbol := (
     unopNameList = unopNameListCell(fun,name,unopNameList);
     word := makeUniqueWord(name,
	  parseinfo(precSpace,precSpace,precSpace,parsefuns(unaryop, defaultbinary)));
     entry := makeSymbol(word,dummyPosition,globalDictionary);
     entry.unary = fun;
     entry.Protected = true;
     entry);     
export setupfun(name:string,value:fun):Symbol := (
     word := makeUniqueWord(name,parseWORD);
     entry := makeSymbol(word,dummyPosition,globalDictionary);
     globalFrame.values.(entry.frameindex) = Expr(CompiledFunction(value,nextHash()));
     entry.Protected = true;
     entry);
export setupvar(name:string,value:Expr,thread:bool):Symbol := (
     word := makeUniqueWord(name,parseWORD);
     when lookup(word,globalDictionary)
     is null do (
     	  entry := makeSymbol(word,dummyPosition,globalDictionary,thread);
     	  (if thread then enlargeThreadFrame() else globalFrame).values.(entry.frameindex) = value;
	  entry)
     is entry:Symbol do (
	  -- we are doing it again after loading data with loaddata()
	  -- or we are reassigning to o or oo in interpret.d
     	  (if thread then enlargeThreadFrame() else globalFrame).values.(entry.frameindex) = value;
	  entry));
export setupvar(name:string,value:Expr):Symbol := setupvar(name,value,false);
export setupvarThread(name:string,value:Expr):Symbol := setupvar(name,value,true);
export setupconst(name:string,value:Expr):Symbol := (
     s := setupvar(name,value);
     s.Protected = true;
     s);
setup(commaS,dummyBinaryFun);

threadLocal export errorDepth := ushort(0);
export printErrorMessage0(c:Code,message:string):Error := (
     p := codePosition(c);
     e := Error(p,message,nullE,false,dummyFrame);
     if p.loadDepth >= errorDepth then printError(e);
     e);
export printErrorMessageE(c:Code,message:string):Expr := (
     p := codePosition(c);
     e := Error(p,message,nullE,false,dummyFrame);
     if p.loadDepth >= errorDepth then printError(e);
     Expr(e));
export printErrorMessageE(p:Position,message:string):Expr := ( -- for use when we have no code
     e := Error(p,message,nullE,false,dummyFrame);
     if p.loadDepth >= errorDepth then printError(e);
     Expr(e));
export printErrorMessageE(c:Token,message:string):Expr := ( -- for use when we have no code
     printErrorMessageE(position(c),message));

export returnFromFunction(z:Expr):Expr := when z is err:Error do if err.message == returnMessage then err.value else z else z;
export returnFromLoop(z:Expr):Expr     := when z is err:Error do if err.message == breakMessage  then if err.value == dummyExpr then nullE else err.value else z else z;

export WrongNumArgs(c:Code,wanted:int,got:int):Expr := (
     printErrorMessageE(c, "expected " + tostring(wanted) + " argument"
	  + (if wanted == 1 then "" else "s") + ", but got "
	  + tostring(got)));
export WrongNumArgs(c:Token,wanted:int,got:int):Expr := (
     printErrorMessageE(c, "expected " + tostring(wanted) + " argument"
	  + (if wanted == 1 then "" else "s") + ", but got "
	  + tostring(got)));

-----------------------------------------------------------------------------

hashfun(e:Expr):Expr := Expr(ZZcell(toInteger(hash(e))));
setupfun("hash",hashfun);

-----------------------------------------------------------------------------
-- Database stuff
export dbmcheck(ret:int):Expr := (
     if ret == -1 then buildErrorPacket(dbmstrerror())
     else Expr(ZZcell(toInteger(ret))));
export dbmopenin(filename:string):Expr := (
     mutable := false;
     filename = expandFileName(filename);
     handle := dbmopen(filename,mutable);
     if handle == -1 
     then buildErrorPacket(dbmstrerror() + " : " + filename)
     else Expr(Database(filename,nextHash(),handle,true,mutable)));
export dbmopenout(filename:string):Expr := (
     mutable := true;
     filename = expandFileName(filename);
     handle := dbmopen(filename,mutable);
     if handle == -1 
     then buildErrorPacket(dbmstrerror() + " : " + filename)
     else Expr(Database(filename,nextHash(),handle,true,mutable)));
export dbmclose(f:Database):Expr := (
     if !f.isopen then return buildErrorPacket("database already closed");
     dbmclose(f.handle);
     f.isopen = false;
     zeroE);
export dbmstore(f:Database,key:string,content:string):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     if !f.Mutable then return buildErrorPacket("database not mutable");
     ret := dbmstore(f.handle,key,content);
     if 0 == ret then Expr(stringCell(content))
     else dbmcheck(ret));
export dbmquery(f:Database,key:string):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     when dbmfetch(f.handle,key)
     is a:string do True
     else False);
export dbmfirst(f:Database):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     when dbmfirst(f.handle)
     is a:string do Expr(stringCell(a))
     else nullE);
export dbmreorganize(f:Database):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     if !f.Mutable then return buildErrorPacket("database not mutable");
     dbmcheck(dbmreorganize(f.handle)));
export keys(f:Database):Expr := (
     if !f.isopen then return buildErrorPacket("database closed");
     x := newvarstringarray(20);
     k := dbmfirst(f.handle);
     continue := true;
     while continue do (
	  when k
	  is key:string do (
	       append(x,key);
	       k = dbmnext(f.handle);
	       )
	  else continue = false;
	  );
     Expr(list(new Sequence len x.n do foreach s in x.a do provide Expr(stringCell(s)))));

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d common.o "
-- End:
