--		Copyright 1994-2000 by Daniel R. Grayson

newStartupMethod := true;				    -- for testing purposes

use C;
use system;
use actors;
use convertr;
use evaluate;
use common;
use binding;
use actors2;
use actors3;
use actors4;
use actors5;
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
use objects;
use basic;
use struct;
use texmacs;

import dirname(s:string):string;

currentFileName := setupvar("currentFileName", nullE);
currentFileDirectory := setupvar("currentFileDirectory", Expr("./"));
update(err:Error,prefix:string,f:Code):Expr := (
     if err.position == dummyPosition
     then printErrorMessage(f,prefix + ": " + err.message)
     else printErrorMessage(f,prefix + ": --backtrace-- ")
     );
stmtno := 0;
linefun(e:Expr):Expr := (
     when e
     is n:Integer do (
	  if isInt(n) then (
	       old := Expr(toInteger(stmtno));
	       nn := toInt(n);
	       if nn >= 0 then (
		    stmtno = nn;
	       	    old)
	       else WrongArg(1,"a non-negative integer"))
	  else WrongArgSmallInteger(1)
	  )
     is a:Sequence do (
	  if length(a) == 0
     	  then Expr(toInteger(stmtno))
     	  else WrongNumArgs(0))
     else WrongNumArgs(0)
     );
setupfun("lineNumber",linefun);
laststmtno := -1;
-- PrimaryPrompt := makeProtectedSymbolClosure("PrimaryPrompt");
-- SecondaryPrompt := makeProtectedSymbolClosure("SecondaryPrompt");
Print := makeProtectedSymbolClosure("Print");
NoPrint := makeProtectedSymbolClosure("NoPrint");
endInput := makeProtectedSymbolClosure("end");
(x:Position) === (y:Position) : bool := (
     x == y || x.filename === y.filename && x.line == y.line && x.column == y.column
     );
PrintOut(g:Expr,semi:bool,f:Code):Expr := (
     methodname := if semi then NoPrint else Print;
     method := lookup(Class(g),methodname);
     if method == nullE 
     then printErrorMessage(f,"no method for '" + methodname.symbol.word.name + "'")
     else apply(method,g)
     );
errorReportS := setupconst("errorReport",Expr(emptySequence));
errorReportS.protected = false;

num(x:CodeClosureList):int := (
     n := 0;
     while (
	  if x.code != dummyCodeClosure then n = n+1;
	  x != x.next
	  ) do x = x.next;
    n);
toExpr(x:CodeClosureList):Expr := Expr(list(
	  new Sequence len num(x)
	  do while (
	       if x.code != dummyCodeClosure then provide x.code;
	       x != x.next) do x = x.next));

readeval4(file:TokenFile,printout:bool,AbortIfError:bool,dictionary:Dictionary,returnLastvalue:bool):Expr := (
     lastvalue := nullE;
     while true do (
     	  if printout then stmtno = stmtno + 1;
	  interrupted = false;
	  interruptPending = false;
	  while peektoken(file,true).word == newlineW do (
	       -- laststmtno = -1; -- so there will be a new prompt after a blank line
	       -- but now we don't like so many extra prompts
	       interrupted = false;
	       interruptPending = false;
	       gettoken(file,true);
	       );
	  interrupted = false;
	  interruptPending = false;
	  parsed := parse(file,semicolonW.parse.precedence,true);
	  if equal(parsed,wordEOF) then return if returnLastvalue then lastvalue else nullE;
	  if parsed == errorTree then (
	       if fileError(file) then return buildErrorPacket(fileErrorMessage(file));
	       if AbortIfError then return buildErrorPacket("--backtrace--");
	       )
	  else (
	       s := gettoken(file,true);  -- get the semicolon
	       if !(s.word == semicolonW || s.word == newlineW)
	       then (
		    printErrorMessage(s,"syntax error");
		    if AbortIfError then return Expr(Error(position(s),"syntax error",dummyCodeClosureList,nullE));
		    )
	       else (
		    if localBind(parsed,dictionary) -- assign scopes to tokens, look up symbols
		    then (		  
			 f := convert(parsed); -- convert to runnable code
			 lastvalue = eval(f);	  -- run it
			 if lastvalue == endInput then return nullE;
			 when lastvalue is err:Error do (
			      if AbortIfError || err.message == returnMessage || err.message == continueMessage || err.message == breakMessage then return lastvalue;
			      if err.message == unwindMessage 
			      then lastvalue = nullE
			      else setGlobalVariable(errorReportS, toExpr(err.report));
			      )
			 else (
			      if printout then (
				   g := PrintOut(lastvalue,s.word == semicolonW,f);
				   when g is err:Error do (
					g = update(err,"at print",f);
					when g is err2:Error do (
					     setGlobalVariable(errorReportS, toExpr(err2.report));
					     )
					else nothing;
					if AbortIfError then return g;
					)
				   else nothing)))
		    else if isatty(file) 
		    then flush(file)
		    else return buildErrorPacket("error while loading file")))));
readeval3(file:TokenFile,printout:bool,AbortIfError:bool,dc:DictionaryClosure,returnLastvalue:bool):Expr := (
     saveLocalFrame := localFrame;
     localFrame = dc.frame;
      savecf := getGlobalVariable(currentFileName);
       savecd := getGlobalVariable(currentFileDirectory);
	setGlobalVariable(currentFileName,Expr(file.posFile.file.filename));
	setGlobalVariable(currentFileDirectory,Expr(dirname(file.posFile.file.filename)));
	ret := readeval4(file,printout,AbortIfError,dc.dictionary,returnLastvalue);
       setGlobalVariable(currentFileDirectory,savecd);
      setGlobalVariable(currentFileName,savecf);
     localFrame = saveLocalFrame;
     ret);
readeval(file:TokenFile,returnLastvalue:bool):Expr := readeval3(file,false,true,newStaticLocalDictionaryClosure(),returnLastvalue);

InputPrompt := makeProtectedSymbolClosure("InputPrompt");
InputContinuationPrompt := makeProtectedSymbolClosure("InputContinuationPrompt");

topLevelPrompt():string := (
     method := lookup(integerClass,if stmtno == laststmtno then InputContinuationPrompt else (laststmtno = stmtno; InputPrompt));
     if method == nullE then ""
     else when apply(method,toExpr(stmtno)) is s:string do s
     is n:Integer do if isInt(n) then blanks(toInt(n)) else ""
     else "\n<--bad prompt--> : " -- unfortunately, we are not printing the error message!
     );

loadprintstdin(stopIfError:bool,dc:DictionaryClosure):Expr := (
     when openTokenFile("-")
     is e:errmsg do buildErrorPacket(e.message)
     is file:TokenFile do (
	  setprompt(file,topLevelPrompt);
	  r := readeval3(file,true,stopIfError,dc,false);
	  file.posFile.file.eof = false; -- erase eof indication so we can try again (e.g., recursive calls to topLevel)
	  r));

loadprint(s:string,stopIfError:bool,dc:DictionaryClosure):Expr := (
     when openTokenFile(s)
     is errmsg do False
     is file:TokenFile do (
	  if file.posFile.file != stdin then file.posFile.file.echo = true;
	  setprompt(file,topLevelPrompt);
	  r := readeval3(file,true,stopIfError,dc,false);
	  t := (
	       if s === "-"			 -- whether it's stdin
	       then (
		    file.posFile.file.eof = false; -- erase eof indication so we can try again (e.g., recursive calls to topLevel)
		    0
		    )
	       else close(file));
	  when r is err:Error do (
	       if err.message == returnMessage then err.value else r
	       )
	  else (
	       if t == ERROR
	       then buildErrorPacket("error closing file") 
	       else nullE)));
load(s:string):Expr := (
     when openTokenFile(s)
     is e:errmsg do buildErrorPacket(e.message)
     is file:TokenFile do (
	  r := readeval(file,false);
	  t := if !(s==="-") then close(file) else 0;
	  when r is err:Error do (
	       if err.message == returnMessage then err.value else r
	       )
	  else (
	       if t == ERROR
	       then buildErrorPacket("error closing file") 
 	       else nullE)));

load(e:Expr):Expr := (
     when e
     is s:string do load(s)
     else buildErrorPacket("expected string as file name"));
setupfun("load",load).protected = false;

input(e:Expr):Expr := (
     when e
     is s:string do (
	  -- we should have a way of setting normal prompts while inputting
	  ret := loadprint(s,true,newStaticLocalDictionaryClosure());
	  laststmtno = -1;
	  ret)
     else buildErrorPacket("expected string as file name"));
setupfun("input",input).protected = false;

stringTokenFile(name:string,contents:string):TokenFile := (
     TokenFile(
	  makePosFile(
	  file(nextHash(),     	    	  -- hash
	       name,	 		  -- filename
	       0,			  -- pid
	       false,	       	    	  -- error
	       "",     	    	      	  -- message
	       false,	       	    	  -- listener
	       NOFD,   	    	          -- listenerfd
	       NOFD,	      	   	  -- connection
	       0,     	   	     	  -- numconns
	       true,			  -- input
	       NOFD,			  -- infd
	       false,			  -- inisatty
	       contents,		  -- inbuffer
	       0,			  -- inindex
	       length(contents),	  -- insize
	       true,			  -- eof
	       false,	  		  -- promptq
	       noprompt,		  -- prompt
     	       true,	       	    	  -- bol
	       false,			  -- echo
	       false,			  -- output
	       NOFD,			  -- outfd
	       false,			  -- outisatty
	       "",			  -- outbuffer
	       0,			  -- outindex
	       0,     	   	     	  -- outbol
	       false,	       	    	  -- hadNet
	       dummyNetList,   	      	  -- nets
	       0		          -- bytesWritten
	       )),
	  NULL));

interpreterDepth := 0;
interpreterDepthS := setupconst("interpreterDepth",toExpr(0));
incrementInterpreterDepth():void := (
     interpreterDepth = interpreterDepth + 1;
     setGlobalVariable(interpreterDepthS,toExpr(interpreterDepth)));
decrementInterpreterDepth():void := (
     interpreterDepth = interpreterDepth - 1;
     setGlobalVariable(interpreterDepthS,toExpr(interpreterDepth)));

export topLevel():bool := when loadprint("-",stopIfError,newStaticLocalDictionaryClosure()) is Error do false else true;

topLevel(dc:DictionaryClosure):bool := when loadprint("-",stopIfError,dc) is Error do false else true;
topLevel(f:Frame):bool := topLevel(newStaticLocalDictionaryClosure(localDictionaryClosure(f)));
topLevel(e:Expr):Expr := (
     incrementInterpreterDepth();
       ret := 
       when e is s:Sequence do (
	    if length(s) == 0 then toExpr(topLevel())
	    else WrongNumArgs(0,1)
	    )
       is dc:DictionaryClosure do toExpr(topLevel(dc))
       is sc:SymbolClosure do toExpr(topLevel(sc.frame))
       is fc:FunctionClosure do toExpr(topLevel(fc.frame))
       is cfc:CompiledFunctionClosure do toExpr(topLevel(emptyFrame))	    -- some values are there, but no symbols
       is CompiledFunction do toExpr(topLevel(emptyFrame))		    -- no values or symbols are there
       else WrongArg("a function, a symbol, or ()");
     decrementInterpreterDepth();
     ret);
setupfun("commandInterpreter",topLevel);

errorCodeS := setupconst("errorCode",nullE);
breakLoop(f:Frame,c:Code):Expr := (
     setDebuggingMode(false);
       oldBreakLoopCode := getGlobalVariable(errorCodeS);
       setGlobalVariable(errorCodeS,Expr(CodeClosure(f,c)));
	 incrementInterpreterDepth();
	   ret := loadprintstdin(stopIfError, newStaticLocalDictionaryClosure(localDictionaryClosure(f)));
	 decrementInterpreterDepth();
       setGlobalVariable(errorCodeS,oldBreakLoopCode);
     setDebuggingMode(true);
     ret);
breakLoopFun = breakLoop;

value(e:Expr):Expr := (
     when e
     is q:SymbolClosure do q.frame.values.(q.symbol.frameindex)
     is c:CodeClosure do eval(c.frame,c.code)
     is s:string do (
	  r := readeval(stringTokenFile("a string", s+newline),true);
	  when r 
	  is err:Error do (
	       if err.message == returnMessage then err.value 
	       else (
		    if err.position == dummyPosition
		    || int(err.position.loadDepth) < errorDepth
		    then r
		    else buildErrorPacket("--backtrace--")))
	  else r)
     else WrongArg(1,"a string, a symbol, or pseudocode"));
setupfun("value",value).protected = false;

export process():void := (
     laststmtno = -1;			  -- might have done dumpdata()
     localFrame = globalFrame;
     stdin .inisatty  =   0 != isatty(0) ;
     stdin.echo       = !(0 != isatty(0));
     stdout.outisatty =   0 != isatty(1) ;
     stderr.outisatty =   0 != isatty(2) ;
     setstopIfError(false);				    -- this is usually true after loaddata(), we want to reset it
     setloadDepth(loadDepth);				    -- loaddata() in M2lib.c increments it, so we have to reflect that at top level
     ret := (
	  when readeval(stringTokenFile("--startupString1--/layout.m2",startupString1),false)
	  is Error do (
	       if stopIfError
	       then 1					    -- probably can't happen, because layout.m2 doesn't set stopIfError
	       else (
		    if topLevel() 			    -- give a prompt for debugging
		    then 0 else 1))
	  else 
	  when readeval(stringTokenFile("--startupString2--/startup.m2",startupString2),false) -- startup.m2 calls topLevel and eventually returns
	  is Error do (
	       if stopIfError 
	       then 1
	       else (
		    if topLevel() 			    -- give a prompt for debugging
		    then 0 else 1))
	  else 0);
     value(Expr("exit " + tostring(ret)));		    -- try to exit the user's way
     exit(if ret == 0 then 1 else ret);			    -- if that doesn't work, try harder and indicate an error
     );
