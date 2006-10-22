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

fileExitHooks := setupvar("fileExitHooks", Expr(emptyList));
currentFileName := setupvar("currentFileName", nullE);
currentPosFile  := dummyPosFile;
currentFileDirectory := setupvar("currentFileDirectory", Expr("./"));
update(err:Error,prefix:string,f:Code):Expr := (
     if err.position == dummyPosition
     then printErrorMessageE(f,prefix + ": " + err.message)
     else printErrorMessageE(f,prefix + ": --backtrace update-- ")
     );
previousLineNumber := -1;

AfterEval := makeProtectedSymbolClosure("AfterEval");
BeforePrint := makeProtectedSymbolClosure("BeforePrint");
Print := makeProtectedSymbolClosure("Print");
NoPrint := makeProtectedSymbolClosure("NoPrint");
AfterPrint := makeProtectedSymbolClosure("AfterPrint");
AfterNoPrint := makeProtectedSymbolClosure("AfterNoPrint");
runmethod(methodname:SymbolClosure,g:Expr,f:Code):Expr := (
     method := lookup(Class(g),methodname);
     if method == nullE 
     then printErrorMessageE(f,"no method for '" + methodname.symbol.word.name + "'")
     else applyEE(method,g)
     );

endInput := makeProtectedSymbolClosure("end");

PrintOut(g:Expr,semi:bool,f:Code):Expr := (
     methodname := if semi then NoPrint else Print;
     method := lookup(Class(g),methodname);
     if method == nullE 
     then printErrorMessageE(f,"no method for '" + methodname.symbol.word.name + "'")
     else applyEE(method,g)
     );
readeval4(file:TokenFile,printout:bool,dictionary:Dictionary,returnLastvalue:bool,stopIfBreakReturnContinue:bool):Expr := (
     lastvalue := nullE;
     while true do (
     	  if printout then setLineNumber(lineNumber + 1);
	  clearAllFlags();
	  while peektoken(file,true).word == NewlineW do (
	       -- previousLineNumber = -1; -- so there will be a new prompt after a blank line
	       -- but now we don't like so many extra prompts
	       interruptedFlag = false;
	       interruptPending = false;
     	       determineExceptionFlag();
	       gettoken(file,true);
	       );
	  interruptedFlag = false;
	  interruptPending = false;
     	  determineExceptionFlag();
	  parsed := parse(file,SemicolonW.parse.precedence,true);
	  if equal(parsed,wordEOF) then return if returnLastvalue then lastvalue else nullE;
	  if parsed == errorTree then (
	       if fileError(file) then return buildErrorPacket(fileErrorMessage(file));
	       if stopIfError then return buildErrorPacket("--backtrace: parse error--");
	       )
	  else (
	       s := gettoken(file,true);  -- get the semicolon
	       if !(s.word == SemicolonW || s.word == NewlineW)
	       then (
		    printErrorMessage(s,"syntax error");
		    if stopIfError then return Expr(Error(position(s),"syntax error",nullE,false,dummyFrame));
		    )
	       else (
		    if localBind(parsed,dictionary) -- assign scopes to tokens, look up symbols
		    then (		  
			 f := convert(parsed); -- convert to runnable code
			 lastvalue = eval(f);	  -- run it
			 if lastvalue == endInput then return nullE;
			 when lastvalue is err:Error do (
			      if err.message == returnMessage || err.message == continueMessage || err.message == continueMessageWithArg 
			      || err.message == breakMessage || err.message == throwMessage then (
				   if stopIfBreakReturnContinue then return lastvalue;
				   printErrorMessage(err.position,"warning: unhandled " + err.message);
				   );
			      if err.message == unwindMessage then (
				   lastvalue = nullE;
				   )
			      else (
				   if !err.printed then (
					printErrorMessage(err.position,"warning: unprinted error message: " + err.message);
					);
			      	   if stopIfError then return lastvalue;
				   );
			      )
			 else (
			      if printout then (
				   -- result of AfterEval replaces lastvalue unless error, in which case 'null' replaces it
				   g := runmethod(AfterEval,lastvalue,f);
				   when g is err:Error
				   do (
					g = update(err,"after eval",f); 
					if stopIfError then return g;
					lastvalue = nullE;
					) 
				   else lastvalue = g;
				   -- result of BeforePrint is printed instead unless error
				   printvalue := nullE;
				   g = runmethod(BeforePrint,lastvalue,f);
				   when g is err:Error
				   do ( g = update(err,"before print",f); if stopIfError then return g; )
				   else printvalue = g;
				   -- result of Print is ignored
				   g = runmethod(if s.word == SemicolonW then NoPrint else Print,printvalue,f);
				   when g is err:Error do ( g = update(err,"at print",f); if stopIfError then return g; ) else nothing;
				   -- result of AfterPrint is ignored
				   g = runmethod(if s.word == SemicolonW then AfterNoPrint else AfterPrint,lastvalue,f);
				   when g is err:Error do ( g = update(err,"after print",f); if stopIfError then return g; ) else nothing;
				   )
			      )
			 )
		    else if isatty(file) 
		    then flush(file)
		    else return buildErrorPacket("error while loading file")))));

interpreterDepth := 0;
interpreterDepthS := setupvar("interpreterDepth",toExpr(0));
incrementInterpreterDepth():void := (
     interpreterDepth = interpreterDepth + 1;
     setGlobalVariable(interpreterDepthS,toExpr(interpreterDepth)));
decrementInterpreterDepth():void := (
     interpreterDepth = interpreterDepth - 1;
     setGlobalVariable(interpreterDepthS,toExpr(interpreterDepth)));


readeval3(file:TokenFile,printout:bool,stopIfError:bool,dc:DictionaryClosure,returnLastvalue:bool,stopIfBreakReturnContinue:bool):Expr := (
     saveLocalFrame := localFrame;
     localFrame = dc.frame;
      savecf := getGlobalVariable(currentFileName);
      savecd := getGlobalVariable(currentFileDirectory);
      savepf := currentPosFile;
	setGlobalVariable(currentFileName,Expr(file.posFile.file.filename));
	setGlobalVariable(currentFileDirectory,Expr(dirname(file.posFile.file.filename)));
        currentPosFile = file.posFile;
	ret := readeval4(file,printout,dc.dictionary,returnLastvalue,stopIfBreakReturnContinue);
      setGlobalVariable(currentFileDirectory,savecd);
      setGlobalVariable(currentFileName,savecf);
      currentPosFile = savepf;
     localFrame = saveLocalFrame;
     ret);
readeval(file:TokenFile,returnLastvalue:bool):Expr := (
     savefe := getGlobalVariable(fileExitHooks);
     setGlobalVariable(fileExitHooks,emptyList);
     ret := readeval3(file,false,true,newStaticLocalDictionaryClosure(file.posFile.file.filename),returnLastvalue,false);
     haderror := when ret is Error do True else False;
     hadexiterror := false;
     hook := getGlobalVariable(fileExitHooks);
     when hook is x:List do foreach f in x.v do (
	  r := applyEE(f,haderror);
	  when r is err:Error do (
	       if err.position == dummyPosition then stderr << "error in file exit hook: " << err.message << endl;
	       hadexiterror = true;
	       ) else nothing
	  )
     else nothing;
     setGlobalVariable(fileExitHooks,savefe);
     if hadexiterror then buildErrorPacket("error in file exit hook") else ret);

InputPrompt := makeProtectedSymbolClosure("InputPrompt");
InputContinuationPrompt := makeProtectedSymbolClosure("InputContinuationPrompt");

topLevelPrompt():string := (
     method := lookup(integerClass,if lineNumber == previousLineNumber then InputContinuationPrompt else (previousLineNumber = lineNumber; InputPrompt));
     if method == nullE then ""
     else when applyEE(method,toExpr(lineNumber)) is s:string do s
     is n:Integer do if isInt(n) then blanks(toInt(n)) else ""
     else "\n<--bad prompt--> : " -- unfortunately, we are not printing the error message!
     );

loadprintstdin(dc:DictionaryClosure,stopIfBreakReturnContinue:bool):Expr := (
     when openTokenFile("-")
     is e:errmsg do buildErrorPacket(e.message)
     is file:TokenFile do (
	  setprompt(file,topLevelPrompt);
	  r := readeval3(file,true,stopIfError,dc,false,stopIfBreakReturnContinue);
	  file.posFile.file.eof = false; -- erase eof indication so we can try again (e.g., recursive calls to topLevel)
	  r));

loadprint(filename:string,stopIfError:bool,dc:DictionaryClosure):Expr := (
     when openTokenFile(filename)
     is errmsg do False
     is file:TokenFile do (
	  if file.posFile.file != stdin then file.posFile.file.echo = true;
	  setprompt(file,topLevelPrompt);
	  r := readeval3(file,true,stopIfError,dc,false,false);
	  t := (
	       if filename === "-"			 -- whether it's stdin
	       then (
		    file.posFile.file.eof = false; -- erase eof indication so we can try again (e.g., recursive calls to topLevel)
		    0
		    )
	       else close(file));
	  when r is err:Error do (
	       if err.message == returnMessage || err.message == continueMessage || err.message == continueMessageWithArg
	       || err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else r
	       )
	  else (
	       if t == ERROR
	       then buildErrorPacket("error closing file") 
	       else nullE)));
load(filename:string):Expr := (
     when openTokenFile(filename)
     is e:errmsg do buildErrorPacket(e.message)
     is file:TokenFile do (
	  r := readeval(file,false);
	  t := if !(filename==="-") then close(file) else 0;
	  when r is err:Error do (
	       if err.message == returnMessage || err.message == continueMessage || err.message == continueMessageWithArg
	       || err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else r
	       )
	  else (
	       if t == ERROR
	       then buildErrorPacket("error closing file") 
 	       else nullE)));

load(e:Expr):Expr := (
     when e
     is s:string do load(s)
     else buildErrorPacket("expected string as file name"));
setupfun("simpleLoad",load);

currentLineNumber(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 0 then Expr(toInteger(int(currentPosFile.pos.line)))
     else WrongNumArgs(0)
     else WrongNumArgs(0));
setupfun("currentLineNumber",currentLineNumber);

input(e:Expr):Expr := (
     when e
     is filename:string do (
	  -- we should have a way of setting normal prompts while inputting
     	  incrementInterpreterDepth();
	  ret := loadprint(filename,true,newStaticLocalDictionaryClosure(filename));
     	  decrementInterpreterDepth();
	  previousLineNumber = -1;
	  ret)
     else buildErrorPacket("expected string as file name"));
setupfun("simpleInput",input);

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
	       0,			  -- echoindex
	       false,			  -- output
	       NOFD,			  -- outfd
	       false,			  -- outisatty
	       "",			  -- outbuffer
	       0,			  -- outindex
	       0,     	   	     	  -- outbol
	       false,	       	    	  -- hadNet
	       dummyNetList,   	      	  -- nets
	       0,		          -- bytesWritten
	       -1,		          -- lastCharOut
	       false                      -- readline
	       )),
	  NULL));

export topLevel():bool := (
     when loadprint("-",stopIfError,newStaticLocalDictionaryClosure()) 
     is err:Error do (
	  -- printErrorMessage(err);		    -- this message may not have been printed before (?)
	  false)
     else true
     );

commandInterpreter(dc:DictionaryClosure):Expr := loadprint("-",stopIfError,dc);
commandInterpreter(f:Frame):Expr := commandInterpreter(newStaticLocalDictionaryClosure(localDictionaryClosure(f)));
commandInterpreter(e:Expr):Expr := (
     saveLoadDepth := loadDepth;
     setLoadDepth(loadDepth+1);
     incrementInterpreterDepth();
       ret := 
       when e is s:Sequence do (
	    if length(s) == 0 then loadprint("-",stopIfError,newStaticLocalDictionaryClosure())
	    else WrongNumArgs(0,1)
	    )
       is x:DictionaryClosure do commandInterpreter(x)
       is x:SymbolClosure do commandInterpreter(x.frame)
       is x:CodeClosure do commandInterpreter(x.frame)
       is x:FunctionClosure do commandInterpreter(x.frame)
       is cfc:CompiledFunctionClosure do commandInterpreter(emptyFrame)	    -- some values are there, but no symbols
       is CompiledFunction do commandInterpreter(emptyFrame)		    -- no values or symbols are there
       is s:SpecialExpr do commandInterpreter(s.e)
       else WrongArg("a function, symbol, dictionary, pseudocode, or ()");
     decrementInterpreterDepth();
     setLoadDepth(saveLoadDepth);
     ret);
setupfun("commandInterpreter",commandInterpreter);

errorCodeS := setupvar("errorCode",nullE);
debugger(f:Frame,c:Code):Expr := (
     -- stdIO << "-- recursionDepth = " << recursionDepth << endl;
     oldrecursionDepth := recursionDepth;
     recursionDepth = 0;
     setDebuggingMode(false);
       oldDebuggerCode := getGlobalVariable(errorCodeS);
       setGlobalVariable(errorCodeS,Expr(CodeClosure(f,c)));
	 incrementInterpreterDepth();
	   if debuggerHook != nullE then (
		r := applyES(debuggerHook,emptySequence);
		when r is Error do return r else nothing;
		);
	   ret := loadprintstdin(newStaticLocalDictionaryClosure(localDictionaryClosure(f)),true);
	 decrementInterpreterDepth();
       setGlobalVariable(errorCodeS,oldDebuggerCode);
     setDebuggingMode(true);
     recursionDepth = oldrecursionDepth;
     ret);
debuggerFun = debugger;

currentString := setupvar("currentString", nullE);
value(e:Expr):Expr := (
     when e
     is q:SymbolClosure do q.frame.values.(q.symbol.frameindex)
     is c:CodeClosure do eval(c.frame,c.code)
     is s:string do (
      	  savecs := getGlobalVariable(currentString);
	  setGlobalVariable(currentString,Expr(s));
	  r := readeval(stringTokenFile("a string", s+newline),true);
	  setGlobalVariable(currentString,savecs);
	  when r 
	  is err:Error do (
	       if err.message == returnMessage || err.message == continueMessage || err.message == continueMessageWithArg 
	       || err.message == breakMessage then if err.value == dummyExpr then nullE else err.value 
	       else r)
	  else r)
     else WrongArg(1,"a string, a symbol, or pseudocode"));
setupfun("value",value).protected = false;

tmpbuf := new string len 100 do provide ' ' ;

internalCapture(e:Expr):Expr := (
     when e
     is s:string do (
     	  flush(stdIO);
	  oldfd := stdIO.outfd;
	  oldDebuggingMode := debuggingMode;
	  setDebuggingMode(false);
	  oldStderrE := getGlobalVariable(stderrS);
	  oldstderr := stderr;
	  stderr = stdIO;
	  setGlobalVariable(stderrS,getGlobalVariable(stdioS));
	  stdIO.outfd = NOFD;
	  oldbuf := stdIO.outbuffer;
	  stdIO.outbuffer = tmpbuf;
	  stringFile := stringTokenFile("a string", s+newline);
	  stringFile.posFile.file.echo = true;
	  oldLineNumber := lineNumber;
	  previousLineNumber = -1;
	  setLineNumber(0);
	  setprompt(stringFile,topLevelPrompt);
	  r := readeval3(stringFile,true,true,newStaticLocalDictionaryClosure(),false,false);
	  out := substr(stdIO.outbuffer,0,stdIO.outindex);
	  stdIO.outfd = oldfd;
	  stdIO.outbuffer = oldbuf;
	  stdIO.outindex = 0;
	  setGlobalVariable(stderrS,oldStderrE);
	  stderr = oldstderr;
	  setLineNumber(oldLineNumber);
	  setDebuggingMode(oldDebuggingMode);
	  previousLineNumber = -1;
	  Expr(Sequence( when r is err:Error do True else False, Expr(out) )))
     else WrongArg(1,"a string"));
setupfun("internalCapture",internalCapture);

normalExit := 0;
errorExit := 1;
interruptExit := 2;					    -- see also M2lib.c
failedExitExit := 3;

Exit(err:Error):void := exit(
     if err.message === interruptMessage then interruptExit
     else errorExit
     );

export process():void := (
     localFrame = globalFrame;
     previousLineNumber = -1;			  -- might have done dumpdata()
     stdin .inisatty  =   0 != isatty(0) ;
     stdin.echo       = !(0 != isatty(0));
     stdout.outisatty =   0 != isatty(1) ;
     stderr.outisatty =   0 != isatty(2) ;
     setstopIfError(false);				    -- this is usually true after loaddata(), we want to reset it
     setLoadDepth(loadDepth);				    -- loaddata() in M2lib.c increments it, so we have to reflect that at top level
     everytimeRun();
     ret := readeval(stringTokenFile("layout.m2",startupString1),false); -- we don't know the right directory!
     when ret is err:Error do (
	  if !err.printed then printError(err);		    -- just in case
	  if stopIfError
	  then Exit(err)	 -- probably can't happen, because layout.m2 doesn't set stopIfError
	  else if !topLevel()				    -- give a prompt for debugging
	  then Exit(err))
     else nothing;
     ret = readeval(stringTokenFile("startup.m2",startupString2),false); -- startup.m2 calls commandInterpreter and eventually returns -- we don't know the right directory!
     when ret is err:Error do (
	  if !err.printed then printError(err);		    -- just in case
	  if stopIfError 
	  then Exit(err)
	  else if !topLevel()				    -- give a prompt for debugging
	  then Exit(err))
     else nothing;
     when ret is n:Integer do (
	  if isInt(n) then (
     	       value(Expr("exit " + tostring(toInt(n))));   -- try to exit the user's way
     	       ))
     else nothing;
     value(Expr("exit 0"));				    -- try to exit the user's way
     exit(failedExitExit);		   -- if that doesn't work, try harder and indicate an error
     );

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
