--		Copyright 1994 by Daniel R. Grayson
use system;
use C;
use actors;
use convertr;
use binding;
use actors2;
use actors3;
use actors4;
use actors5;
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
use objects;
use basic;
use struct;

setvalue(x:Symbol,y:Expr):void := globalFrame.values.(x.frameindex) = y;
getvalue(x:Symbol):Expr := globalFrame.values.(x.frameindex);
currentFile := setupconst("currentFile", nullE);
update(err:Error,prefix:string,f:Code):Expr := (
     if err.position == dummyPosition
     then errorpos(f,prefix + ": " + err.message)
     else errorpos(f,prefix + ": --backtrace--")
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
	  else WrongArg(1,"a small integer")
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
EndOfInput := makeProtectedSymbolClosure("end");
(x:Position) === (y:Position) : bool := (
     x == y || x.filename === y.filename && x.line == y.line && x.column == y.column
     );
PrintOut(g:Expr,semi:bool,f:Code):Expr := (
     methodname := if semi then NoPrint else Print;
     method := lookup(Class(g),methodname);
     if method == nullE 
     then errorpos(f,"no method for '" + methodname.symbol.word.name + "'")
     else apply(method,g)
     );
errorReportS := setupconst("report",Expr(emptySequence));
errorReportS.protected = false;
readeval4(file:TokenFile,printout:bool,AbortIfError:bool,scope:Scope):Expr := (
     returnvalue := nullE;
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
	  if equal(parsed,wordEOF) then break;
	  returnvalue = nullE;
	  if parsed == errorTree then (
	       if AbortIfError then return(errorExpr("--backtrace--"));
	       )
	  else (
	       s := gettoken(file,true);  -- get the semicolon
	       if !(s.word == semicolonW || s.word == newlineW)
	       then (
		    errorpos(s.position,"syntax error");
		    if AbortIfError 
		    then return(Expr(Error(s.position,"syntax error",emptySequence)));
		    )
	       else (
		    if localBind(parsed,scope) -- assign scopes to tokens, look up
		    then (		  
			 f := convert(parsed); -- convert to runnable code
			 lastvalue = eval(f);	  -- run it
			 if lastvalue == EndOfInput then return(lastvalue);
			 when lastvalue is err:Error do (
			      setvalue(errorReportS, err.report);
			      if AbortIfError then return(lastvalue);
			      lastvalue = nullE; )
			 else (
			      if s.word != semicolonW then returnvalue = lastvalue;
			      if printout then (
				   g := PrintOut(lastvalue,s.word == semicolonW,f);
				   when g is err:Error do (
					g = update(err,"at print",f);
					when g is err2:Error do (
					     setvalue(errorReportS, err2.report);
					     )
					else nothing;
					if AbortIfError then return(g);
					)
				   else nothing; ) ) )
		    else if isatty(file) 
		    then flush(file)
		    else return(errorExpr("error while loading file")); ); );
	  lastvalue = nullE; );
     if isatty(file) then stdout << endl;
     returnvalue);
readeval3(file:TokenFile,printout:bool,AbortIfError:bool,scope:Scope):Expr := (
     save := getvalue(currentFile);
     setvalue(currentFile,Expr(file.posFile.file.filename));
     ret := readeval4(file,printout,AbortIfError,scope);
     setvalue(currentFile,save);
     ret);
     
readeval2(file:TokenFile,printout:bool,AbortIfError:bool):Expr := (
     -- wrap a new scope around the file
     saveLocalFrame := localFrame;
     scope := newScope(globalScope);	  -- don't nest the scopes of files loaded.
     scope.transient = false;
     localFrame = Frame(localFrame,scope.seqno,
	  new Sequence len scope.framesize do provide nullE);
     ret := readeval3(file,printout,AbortIfError,scope);
     localFrame = saveLocalFrame;
     ret);
readeval(file:TokenFile):Expr := readeval2(file,false,true);
export StopIfError := false;
readevalprint(file:TokenFile):Expr := readeval3(file,true,StopIfError,globalScope);
nprompt := false;
mpwprompt := false;
xprompt := false;
prompt(o:file):void := (
     if stmtno == laststmtno 
     then (
	  if !nprompt then o << " " << blanks(length(tostring(stmtno))) << "   ";
     	  )
     else (
     	  laststmtno = stmtno;
     	  if xprompt 
	  then (o << "\1";)
	  else if !nprompt then o << endl;		  -- double space
	  if mpwprompt then (o << "i" << stmtno << " : " << endl 
	                      << " " << blanks(length(tostring(stmtno))) << "   ";)
	  else if !nprompt then (o << "i" << stmtno << " : ";)
	  ));
loadprint(s:string):Expr := (
     when openTokenFile(s)
     is errmsg do False
     is file:TokenFile do (
	  if file.posFile.file != stdin then file.posFile.file.echo = true;
	  setprompt(file,prompt);
	  r := readevalprint(file);
	  t := if !(s==="-") then close(file) else 0;
	  when r is Error do r 
	  else (
	       if t == ERROR
	       then errorExpr("error closing file") 
	       else True)));
load(s:string):Expr := (
     when openTokenFile(s)
     is errmsg do False
     is file:TokenFile do (
	  r := readeval(file);
	  t := if !(s==="-") then close(file) else 0;
	  when r is Error do r
	  else (
	       if t == ERROR
	       then errorExpr("error closing file") 
 	       else True)));

load(e:Expr):Expr := (
     when e
     is s:string do load(s)
     else errorExpr("expected string as file name"));
setupfun("load",load);

input(e:Expr):Expr := (
     when e
     is s:string do (
	  oldxprompt := xprompt;
	  xprompt = false;
	  ret := loadprint(s);
	  xprompt = oldxprompt;
	  laststmtno = -1;
	  ret)
     else errorExpr("expected string as file name"));
setupfun("input",input);

stringTokenFile(name:string,contents:string):TokenFile := (
     TokenFile(
	  makePosFile(
	  file(nextHash(),     	    	  -- hash
	       name,	 		  -- filename
	       0,			  -- pid
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

usageMessage():void := (
     usage(argv.0 + " [options...] filename ..." + newline 
	  + "    --              ignore previous arguments after reloading data" + newline
	  + "    \"-e x\"          evaluate expression x" + newline
	  + "    -h              print this usage message" + newline
	  + "    -n              print no input prompts" + newline
	  + "    -mpwprompt      print a prompt suitable for use using MPW on the Macintosh" + newline
	  + "    -q              don't load init file" + newline
	  + "    -s              stop if an error occurs" + newline
	  + "    -silent         don't print the startup banner" + newline
	  + "    -tty            assume stdin and stdout are ttys" + newline
	  + "    -x              examples-prompt mode" + newline
	  ));

export process2():void := (
     when loadprint("-") is Error do exit(2) else nothing;
     );

export process():void := (
     laststmtno = -1;			  -- might have done dumpdata()
     localFrame = globalFrame;
     stdin .inisatty  =   0 != isatty(0) ;
     stdin.echo       = !(0 != isatty(0));
     stdout.outisatty =   0 != isatty(1) ;
     stderr.outisatty =   0 != isatty(2) ;
     phase := 0;
     foreach arg at i in args do (
	  if arg === "--" then phase = phase + 1
	  else if reloaded == phase then (
	       if arg === "-s" then ( 
		    StopIfError = true; 
		    )
	       else if arg.0 == '-' && arg.1 == 'e' then (
		    when readeval(
			 stringTokenFile(
			      "command line argument " + tostring (1+i) + " ",
			      substr(arg,2)+newline))
		    is Error do exit(2)
		    else nothing;
		    )	       
	       else if arg === "-x" then xprompt = true
	       else if arg === "-tty" then (
		    stdin.inisatty = true;
		    stdin.echo = false;
		    stdout.outisatty = true;
		    stderr.outisatty = true;
		    )
	       else if arg === "-silent" then nothing
	       else if arg === "" then nothing
	       else if arg === "-n" then nprompt = true
       	       else if arg === "-mpwprompt" then mpwprompt = true
	       else if arg === "-q" then nothing  -- pass through to top level
	       else if arg === "-h" then (
		    usageMessage();
		    exit(1))
	       else if arg.0 == '-' then (
		    stderr << "unknown option " << arg << endl;
		    usageMessage();
		    exit(1))
	       else (
		    r := load(arg);
		    when r
		    is Error do exit(2)
		    else if r == False 
		    then (
			 error("can't open file " + arg);
			 exit(1);
			 )
		    )));
     process2();
     exit(0);
     );
value(e:Expr):Expr := (
     when e
     is q:SymbolClosure do q.frame.values.(q.symbol.frameindex)
     is s:string do (
	  r := readeval(stringTokenFile("a string", s+newline));
	  when r 
	  is err:Error do (
	       if err.position == dummyPosition
	       || int(err.position.reloaded) < ErrorDepth
	       then r
	       else errorExpr("--backtrace--"))
	  else r)
     else WrongArg(1,"a string or a symbol"));
setupfun("value",value);

