--		Copyright 1994-2003 by Daniel R. Grayson
use basic;
use binding;
use stdiop0;

export codePosition(c:Code):Position := ( -- TODO retire
    when c
    is f:nullCode                  do dummyPosition
    is f:realCode                  do f.position
    is f:stringCode                do f.position
    is f:integerCode               do f.position
    is f:globalMemoryReferenceCode do f.position
    is f:threadMemoryReferenceCode do f.position
    is f:localMemoryReferenceCode  do f.position
    is f:globalAssignmentCode      do f.position
    is f:localAssignmentCode       do f.position
    is f:parallelAssignmentCode    do f.position
    is f:augmentedAssignmentCode   do f.position
    is f:evaluatedCode             do f.position
    is f:globalSymbolClosureCode   do f.position
    is f:threadSymbolClosureCode   do f.position
    is f:localSymbolClosureCode    do f.position
    is f:unaryCode                 do f.position
    is f:binaryCode                do f.position
    is f:ternaryCode               do f.position
    is f:multaryCode               do f.position
    is f:sequenceCode              do f.position
    is f:listCode                  do f.position
    is f:arrayCode                 do f.position
    is f:angleBarListCode          do f.position
    is f:semiCode                  do f.position
    is f:newCode                   do f.position
    is f:newFromCode               do f.position
    is f:newOfCode                 do f.position
    is f:newOfFromCode             do f.position
    is f:forCode                   do f.position
    is f:whileDoCode               do f.position
    is f:whileListCode             do f.position
    is f:whileListDoCode           do f.position
    is f:ifCode                    do f.position
    is f:tryCode                   do f.position
    is f:adjacentCode              do f.position
    is f:functionCode              do f.position
    is f:catchCode                 do f.position
    is f:Error                     do f.position
    );

export setuppostfix(e:SymbolClosure,fn:unop):void := (
     unopNameList = unopNameListCell(fn,e.symbol,unopNameList);
     e.symbol.postfix = fn;
     );
export setup(e:SymbolClosure,fn:binop):void := (
     binopNameList = binopNameListCell(fn,e.symbol,binopNameList);
     e.symbol.binary = fn;
     );
export setup(e:SymbolClosure,fun1:unop,fun2:binop):void := (
     unopNameList = unopNameListCell(fun1,e.symbol,unopNameList);
     binopNameList = binopNameListCell(fun2,e.symbol,binopNameList);
     e.symbol.unary = fun1;
     e.symbol.binary = fun2;
     );
export setupop(s:SymbolClosure,fun:unop):void := (
     unopNameList = unopNameListCell(fun,s.symbol,unopNameList);
     s.symbol.unary = fun;
     );
export setup(e:SymbolClosure,fn:ternop):void := (
     ternopNameList = ternopNameListCell(fn,e.symbol,ternopNameList);
     );
export setup(e:SymbolClosure,fn:multop):void := (
     multopNameList = multopNameListCell(fn,e.symbol,multopNameList);
     );
export setupfun(name:string, value:function(Expr):Expr):Symbol := (
     word := makeUniqueWord(name,parseWORD);
     entry := makeSymbol(word,dummyPosition,globalDictionary);
     globalFrame.values.(entry.frameindex) = Expr(newCompiledFunction(value));
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
     printErrorMessageE(c.position,message));

-- for use in the debugger
export printPosition(c:Code):void := ( stdIO << codePosition(c) << endl; );

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
dbmopenhelper(filename:string,is_mutable:bool):Expr := (
    filename = expandFileName(filename);
    handle := dbmopen(filename,is_mutable);
    if handle == -1
    then buildErrorPacket(dbmstrerror() + " : " + filename)
    else (
	db := Database(filename,hash_t(0),handle,true,is_mutable);
	db.hash = hashFromAddress(Expr(db));
	Expr(db)));
export dbmopenin(filename:string):Expr := dbmopenhelper(filename, false);
export dbmopenout(filename:string):Expr := dbmopenhelper(filename, true);
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
