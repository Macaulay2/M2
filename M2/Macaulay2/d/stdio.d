--		Copyright 1994 by Daniel R. Grayson
use nets;
use interrupts;
use errio;
use gmp;
use expr;
use stdio0;

header "#include \"../system/m2fileinterface.h\"";
--provide a constant representation of default buffer size for a file
--this must be set the same as the bufsize in stdio0.d
bufsize ::= 4 * 1024;

export newm2cfile(foss:fileOutputSyncState) ::= Ccode(m2cfile,"M2File_New(",lvalue(foss),")");

--provide a default constructor for files
export newFile(	
        filename:string,	-- name of file
	pid:int,	        -- pid if it's a pipe or pair of pipes to a child process, else 0
        error:bool,             -- a system call returned ERROR
	errorMessage:string,    -- the error message associated to the system call that returned ERROR
	-- listener stuff
        listener:bool,	   	-- is a listener
	listenerfd:int,	    	-- file descriptor of listener, or -1
	connection:int,	   	-- file descriptor of accepted connection, not made into file yet
	numconns:int,	        -- count connections accepted
     	-- input file stuff
     	input:bool,	        -- is input file
	infd:int,		-- file descriptor or -1
        inisatty:bool,
	inbuffer:string,        -- buffer
	inindex:int,		-- inbuffer.inindex is the first available char
        insize:int,		-- inbuffer.(insize-1) is the last char
				-- we always have inindex <= insize
	eof:bool,		-- last read got 0 characters (still might have some chars in inbuffer)
        promptq:bool,           -- whether to prompt and to reward for input
	prompt:function():string, -- function to call to get prompt string when input is required
        reward:function():string, -- function to call to get reward string when input has been obtained
	fulllines:bool,		-- whether to read at least up to the next newline each time input is required
        bol:bool,     	   	-- at the beginning of a line, and a prompt is needed
	echo:bool,     	   	-- whether to echo characters read to corresponding output file
	echoindex:int,	        --   inbuffer.echoindex is the next character to echo
     	-- output file stuff
     	output:bool,	        -- is output file
	outfd:int,		-- file descriptor or -1
        outisatty:bool,
	outbuffer:string,	-- buffer
	                        -- outbuffer . 0 is the first char in the buffer
	outindex:int,	        -- outbuffer.(outindex-1) is the last char
	outbol:int,	        -- outbuffer.outbol is the index of the first char of the current line
	     	       	        -- The text after this point may be combined with
				-- subsequently printed nets.
        hadNet:bool,		-- whether a Net is present, in which case the
	     	       	        -- buffer will be empty
	nets:NetList,	        -- list of nets, to be printed after the outbuffer
        bytesWritten:int,       -- bytes written so far
	lastCharOut:int,        -- when outbuffer empty, last character written, or -1 if none
        readline:bool,          -- input handled by readline()
	fileThreadState:int     -- state of thread handling 
):file := ( 
foss := newFileOutputSyncState(outbuffer,outindex,outbol,hadNet,nets,bytesWritten,lastCharOut,false);
--foss:= newDefaultFileOutputSyncState();
m2f := newm2cfile(foss);
Ccode(void,"M2File_SetThreadMode(",lvalue(m2f),",",fileThreadState,")");
file(nextHash(), filename,pid,error,errorMessage,listener,listenerfd,connection,numconns,input,infd,inisatty,inbuffer,inindex,insize,eof,
promptq,prompt,reward,fulllines,bol,echo,echoindex,readline,output,outfd,outisatty,foss,newMutex,m2f)

);

export getFileFOSS(o:file):fileOutputSyncState := (
    return Ccode(fileOutputSyncState,"M2File_GetState(",lvalue(o.cfile),")");
);
export releaseFileFOSS(o:file):void := (
    Ccode(void,"M2File_ReleaseState(",lvalue(o.cfile),")");
);
export startFileInput(o:file):void := (
    Ccode(void,"M2File_StartInput(",lvalue(o.cfile),")");
);
export endFileInput(o:file):void := (
    Ccode(void,"M2File_EndInput(",lvalue(o.cfile),")");
);
export startFileOutput(o:file):void := (
    Ccode(void,"M2File_StartOutput(",lvalue(o.cfile),")");
);
export endFileOutput(o:file):void := (
    Ccode(void,"M2File_EndOutput(",lvalue(o.cfile),")");
);
export setFileThreadState(o:file, state:int):void :=
(
	Ccode(void,"M2File_SetThreadMode(",lvalue(o.cfile),",state)")
);

export syscallErrorMessage(msg:string):string := msg + " failed: " + syserrmsg();
export fileErrorMessage(o:file,msg:string):string := (
     o.error = true;
     o.errorMessage = syscallErrorMessage(msg);
     o.errorMessage);
export fileError(o:file):bool := o.error;
export clearFileError(o:file):void := (
     o.error = false;
     o.errorMessage = "";
     );
export fileErrorMessage(o:file):string := o.errorMessage;
export noprompt():string := "";
newbuffer():string := new string len bufsize do provide ' ';
export stdError := newFile(
     -- contrast with stderr, defined in errio.d
     -- intended just for top level use where nets might be printed
     "stderr", 0, 
     false, "",
     false,NOFD,NOFD,0,
     false,NOFD  ,false,          "",        0,0,false,false,noprompt,noprompt,false,true,false,0,
     true, STDERR,0!=isatty(2), newbuffer(), 0,0,false,dummyNetList,0,-1,false,1);
-- we give a way for other threads to know whether stder is there
export dummyfile := newFile("dummy",0, 
     false, "",
     false,NOFD,NOFD,0,
     false,NOFD,false,        "",          0,0,false,false,noprompt,noprompt,false,true,false,0,
     false,NOFD,false,        "",	    	 0,0,false,dummyNetList,0,-1,false,0);
export stdIO  := newFile("stdio",  0, 
     false, "",
     false, NOFD,NOFD,0,
     true,  STDIN ,0!=isatty(0), newbuffer(), 0,0,false,false,noprompt,noprompt,false,true,false,0,
     true,  STDOUT,0!=isatty(1), newbuffer(), 0,0,false,dummyNetList,0,-1,false,1);

export interpreterDepth := 0;
export lineNumber := 0;
texmacsprompt():string := (
     s := "";
     for i from 1 to interpreterDepth do s = s + "i";
     "\2prompt#" + s + tostring(lineNumber) + " : \5\5"
     );
texmacsreward():string := "\2verbatim:";

init():void := (
     stdIO.readline = stdIO.infd == STDIN && stdIO.inisatty ;
     foreach arg in args do (
	  if arg === "--texmacs" then (
	       stdIO.echo = false;
	       stdIO.inisatty = false;
	       stdIO.outisatty = false;
	       stdIO.readline = false;
	       stdIO.promptq = true;
	       stdIO.prompt = texmacsprompt;
	       stdIO.reward = texmacsreward;
	       stdIO.fulllines = true;
	       )
	  else
	  if arg === "--no-readline" then (
	       stdIO.readline = false
	       )
	  else
	  if arg === "--no-tty" then (
	       stdIO.inisatty = false; 
	       stdIO.outisatty = false;
	       )
	  else
	  if arg === "--read-only-files" then (
	       readonlyfiles = true;
	       )
	  );
     -- strange but true: readline refuses to display CTRL-A when it occurs in a prompt
     );

everytime(init);

export errmsg := {+ message:string };
export FileCell := {file:file,next:(null or FileCell)};
export openfiles := (null or FileCell)(NULL);
addfile(o:file):file := (
     openfiles = FileCell(o,openfiles);
     o);
rmfile(o:file):void := (
     prevcell := (null or FileCell)(NULL);
     x := openfiles;
     while true do (
	  when x is null do break
	  is thiscell:FileCell do (
	       if thiscell.file == o then (
	       	    when prevcell is null do openfiles = thiscell.next
	       	    is prevcell:FileCell do prevcell.next = thiscell.next
		    )
	       else prevcell = x;
	       x = thiscell.next;
	       )));
addfile(stdIO);
addfile(stdError);
opensocket(filename:string,input:bool,output:bool,listener:bool):(file or errmsg) := (
     if readonlyfiles then return (file or errmsg)(errmsg("--read-only-files: opening a socket not permitted"));
     host0 := substr(filename,1);
     serv := "2500";		  -- Macaulay 2 default port
     foreach c at j in filename do if c == ':' then (
	  host0 = substr(filename,1,j-1);
	  if j+1 < length(filename) then serv = substr(filename,j+1);
	  break;
	  );
     so := NOFD;
     sd := NOFD;
     if length(host0) == 0 || listener then (
	  so = openlistener(host0,serv);
     	  if so == ERROR then return (file or errmsg)(errmsg("can't open listener : "+syserrmsg()));
	  if input || output then (
	       sd = acceptBlocking(so);
     	       if sd == ERROR then (
		    close(so);
		    return (file or errmsg)(errmsg("can't open socket: "+syserrmsg()));
		    );
	       )
	  )
     else (
	  sd = opensocket(host0,serv);
     	  if sd == ERROR then return (file or errmsg)(errmsg("can't open socket : "+syserrmsg()));
	  );
     (file or errmsg)(addfile(newFile(filename, 0,
	  false, "",
	  listener, so, NOFD, if listener then 0 else 1,
	  input, if input then sd else NOFD, false, if input then newbuffer() else "", 
	  0, 0, false, false,noprompt,noprompt,false, true, false,0,
	  output, if output then sd else NOFD, false, if output then newbuffer() else "",
	  0, 0, false, dummyNetList,0,-1,false,0))));
accept(f:file,input:bool,output:bool):(file or errmsg) := (
     if readonlyfiles then return (file or errmsg)(errmsg("--read-only-files: accepting a connection not permitted"));
     if !f.listener then return (file or errmsg)(errmsg("expected a listener"));
     sd := NOFD;
     if f.connection != NOFD
     then (
	  sd = f.connection;
	  f.connection = NOFD;
	  )
     else (
     	  sd = acceptBlocking(f.listenerfd);
     	  if sd == ERROR
	  then return (file or errmsg)(errmsg(fileErrorMessage(f,"accepting connection")));
	  );
     f.numconns = f.numconns + 1;
     (file or errmsg)(addfile(newFile(f.filename, 0,
	  false, "",
	  false, NOFD,NOFD,f.numconns,
	  input, if input then sd else NOFD, false, if input then newbuffer() else "", 
	  0, 0, false, false,noprompt,noprompt,false, true, false, 0,
	  output, if output then sd else NOFD, false, if output then newbuffer() else "",
	  0, 0, false, dummyNetList,0,-1,false,0))));

openpipe(filename:string,input:bool,output:bool):(file or errmsg) := (
     if readonlyfiles then return (file or errmsg)(errmsg("--read-only-files: opening a pipe not permitted"));
     toChild := array(int)(NOFD,NOFD);
     fromChild := array(int)(NOFD,NOFD);
     if output && pipe(toChild) == ERROR || input && pipe(fromChild) == ERROR 
     then (
	  if  toChild.0 != NOFD then close(toChild.0);
	  if  toChild.1 != NOFD then close(toChild.1);
	  return errmsg("can't make pipe : "+syserrmsg());
	  );
     arglist := tocharstarstar(array(string)("/bin/sh","-c",substr(filename,1)));
     pid := fork();
     -- Don't call GC_malloc after forking; it causes problems when there are multiple threads, because
     -- a call by GC_malloc to pthread_mutex_lock hangs.  That's why we compute "arglist" above.
     if pid == -1 then (
	  if  toChild.0 != NOFD then close(toChild.0);
	  if  toChild.1 != NOFD then close(toChild.1);
	  return errmsg("can't fork : "+syserrmsg());
	  );
     listener := false;
     if pid == 0 then (
	  close(STDIN);
	  if output then (
	       close(toChild.1);
	       if toChild.0 != STDIN then (
		    dup2(toChild.0, STDIN);
		    close(toChild.0);
		    ));
	  if input then (
	       close(fromChild.0);
	       if fromChild.1 != STDOUT then (
		    dup2(fromChild.1, STDOUT);
		    close(fromChild.1);
		    ));
	  execstar(arglist);
	  write(2,"exec of /bin/sh failed!\n",24);
	  exit(1);
	  );
     if input then close(fromChild.1);
     if output then close(toChild.0);
     (file or errmsg)(addfile(newFile(filename, pid, 
	  false, "",
	  listener, NOFD,NOFD,0,
	  input, if input then fromChild.0 else NOFD, false, if input then newbuffer() else "", 
	  0, 0, false, false,noprompt,noprompt,false, true, false, 0,
	  output, if output then toChild.1 else NOFD, false, if output then newbuffer() else "",
	  0, 0, false, dummyNetList,0,-1,false,0))));
export openIn(f:file):(file or errmsg) := accept(f,true,false);
export openOut(f:file):(file or errmsg) := accept(f,false,true);
export openInOut(f:file):(file or errmsg) := accept(f,true,true);
export expandFileName(filename:string):string := (
     if length(filename) > 0 
     && filename.0 == '~' 
     && ( length(filename) == 1 || length(filename) >= 2 && filename.1 == '/'  )
     then (
	  h := getenv("HOME");
	  if length(h) == 0 then filename else h + substr(filename,1)
	  )
     else filename);
export openIn(filename:string):(file or errmsg) := (
     if filename === "-"
     then (file or errmsg)(stdIO)
     else if length(filename) > 0 && filename . 0 == '$'
     then opensocket(filename,true,false,false)
     else if length(filename) > 0 && filename . 0 == '!'
     then openpipe(filename,true,false)
     else (
	  filename = expandFileName(filename);
     	  fd := openin(filename);
     	  if fd == ERROR
     	  then (file or errmsg)(errmsg(syscallErrorMessage("opening input file \""+filename+ "\"")))
     	  else (file or errmsg)(addfile(newFile(filename, 0, 
	  	    false, "",
		    false, NOFD,NOFD,0,
		    true,  fd, 0 != isatty(fd), newbuffer(), 0, 0, false, false,noprompt,noprompt,false,true,false,0,
		    false, NOFD, false,           "",          0, 0, false, dummyNetList,0,-1,false,0)))));
export openOut(filename:string):(file or errmsg) := (
     if readonlyfiles then return (file or errmsg)(errmsg("--read-only-files: opening an output file not permitted"));
     if filename === "-"
     then (file or errmsg)(stdIO)
     else if length(filename) > 0 && filename . 0 == '$'
     then opensocket(filename,false,true,false)
     else if length(filename) > 0 && filename . 0 == '!'
     then openpipe(filename,false,true)
     else (
	  filename = expandFileName(filename);
     	  fd := openout(filename);
     	  if fd == ERROR
     	  then (file or errmsg)(errmsg(syscallErrorMessage("opening output file \""+filename+"\"")))
     	  else (file or errmsg)(addfile(newFile(filename, 0, 
	  	    false, "",
		    false, NOFD,NOFD,0,
		    false, NOFD, false,           "",          0, 0, false,false,noprompt,noprompt,false,true,false,0,
		    true,  fd, 0 != isatty(fd), newbuffer(), 0, 0, false,dummyNetList,0,-1,false,0)))));
export openOutAppend(filename:string):(file or errmsg) := (
     if readonlyfiles then return (file or errmsg)(errmsg("--read-only-files: opening an output file not permitted"));
     filename = expandFileName(filename);
     fd := openoutappend(filename);
     if fd == ERROR
     then (file or errmsg)(errmsg(syscallErrorMessage("opening output (append) file \""+filename+"\"")))
     else (file or errmsg)(addfile(newFile(filename, 0, 
	       false, "",
	       false, NOFD,NOFD,0,
	       false, NOFD, false,           "",          0, 0, false,false,noprompt,noprompt,false,true,false,0,
	       true,  fd, 0 != isatty(fd), newbuffer(), 0, 0, false,dummyNetList,0,-1,false,0))));
export openInOut(filename:string):(file or errmsg) := (
     if readonlyfiles then return (file or errmsg)(errmsg("--read-only-files: opening an output file not permitted"));
     if filename === "-"
     then (file or errmsg)(stdIO)
     else if length(filename) > 0 && filename . 0 == '$'
     then opensocket(filename,true,true,false)
     else if length(filename) > 0 && filename . 0 == '!'
     then openpipe(filename,true,true)
     else (file or errmsg)(errmsg("can't open file "+filename+" for both input and output")));
export openListener(filename:string):(file or errmsg) := (
     if readonlyfiles then return (file or errmsg)(errmsg("--read-only-files: opening a listener not permitted"));
     if length(filename) > 0 && filename . 0 == '$'
     then opensocket(filename,false,false,true)
     else (file or errmsg)(errmsg("openListener: expected file name starting with '$'")));
export flushinput(o:file):void := (
     o.inindex = 0;
     o.insize = 0;
     o.bol = true;
     );

simpleflush(o:file):int := (				    -- write the entire buffer to file or enlarge the buffer
     if o.outfd == NOFD then return ERROR;
     startFileOutput(o);
     foss := getFileFOSS(o);
     if foss.capturing then (
     	  if foss.outindex == length(foss.outbuffer)
     	  then foss.outbuffer = enlarge(length(foss.outbuffer),foss.outbuffer);
	  )
     else (
     	  foss.outbol = 0;
	  off := 0;
	  n := 0;
	  while n >= 0 && off < foss.outindex && !test(interruptedFlag) do (
	       n = write(o.outfd,foss.outbuffer,foss.outindex-off,off);
	       if n > 0 then (
		    off = off + n;
		    foss.lastCharOut = int(foss.outbuffer.(off-1));
		    foss.bytesWritten = foss.bytesWritten + n));
	  if 0 < off then (
	       for k from off to foss.outindex-1 do foss.outbuffer.(k-off) = foss.outbuffer.k;
	       foss.outindex = foss.outindex - off);
	  if n == -1 then (
	       fileErrorMessage(o,"writing");
	       releaseFileFOSS(o);
	       endFileOutput(o);
	       return -1);
	  if test(interruptedFlag) then (
	       foss.outindex = 0;				    -- erase the output buffer after an interrupt
	       releaseFileFOSS(o);
	       endFileOutput(o);
	       return ERROR);
	  );
     releaseFileFOSS(o);
     endFileOutput(o);
     NOERROR);

simpleout(o:file,x:string):int := (
     foss := getFileFOSS(o);
     i := 0;						    -- bytes of x transferred so far
     m := length(x);
     j := foss.outindex;
     n := length(foss.outbuffer);
     while i < m do (
	  if j == n then (
	       if simpleflush(o) == ERROR then (releaseFileFOSS(o); return ERROR);
	       j = foss.outindex;
               n = length(foss.outbuffer);
	       );
	  b := m-i;					    -- number of bytes to transfer this time
	  if b > n-j then b = n-j;
	  for k from 0 to b-1 do foss.outbuffer.(j+k) = x.(i+k);
	  i = i + b;
	  j = j + b;
	  foss.outindex = j;
	  foss.outbol = j;				    -- is this right?
	  );
     releaseFileFOSS(o);
     NOERROR);

flushnets(o:file):int := (
     foss := getFileFOSS(o);
     if foss.hadNet then (
	  n := HorizontalJoin(foss.nets);
	  foss.hadNet = false;
	  foss.nets = dummyNetList;
	  lastone := length(n.body)-1;
	  foreach s at i in n.body do (
	       if ERROR == simpleout(o,s) then (releaseFileFOSS(o); return ERROR);
	       if i != lastone then (
		    if ERROR == simpleout(o,newline) then (releaseFileFOSS(o); return ERROR);
		    );
	       ); 
	  );
     releaseFileFOSS(o);
     NOERROR);

export flush(o:file):int := (
     foss := getFileFOSS(o);
     if foss.hadNet then if ERROR == flushnets(o) then (releaseFileFOSS(o); return ERROR);
     foss.outbol = foss.outindex;
     releaseFileFOSS(o);
     simpleflush(o));

cleanUp(o:file):void := (
     if !o.listener && !o.input && !o.output then (
	  rmfile(o);
	  );
     );

export closeListener(o:file):(errmsg or null) := (
     if o.listenerfd == NOFD then return errmsg("close: listener not open");
     haderror := close(o.listenerfd) == ERROR;
     o.listenerfd = NOFD;
     o.listener = false;
     cleanUp(o); 
     if haderror then return errmsg("closing listener: " + syserrmsg());
     null());

export closeIn(o:file):(errmsg or null) := (
     stat := 0;
     if o.infd == NOFD then return errmsg("close: file not open");
     if o == stdIO then return null();			    -- silently refuse to close stdIO
     if o.input then flushinput(o);
     haderror := false;
     haderror = haderror || o.infd != o.outfd && close(o.infd) == ERROR;
     haderror = haderror || o.outfd == NOFD && (
	  o.pid != 0 && (stat = wait(o.pid); o.pid = 0; 0 != stat) ||
	  o.listenerfd != NOFD && close(o.listenerfd) == ERROR
	  );
     o.infd = NOFD;
     o.input = false;
     cleanUp(o);
     if haderror then (
	  if stat != 0 then return errmsg("close: process exited with code " + tostring(stat));
	  return errmsg("closing input file: " + syserrmsg());
	  );
     null());

export closeOut(o:file):(errmsg or null) := (
     stat := 0;
     if o.outfd == NOFD then return errmsg("close: file not open");
     haderror := false;
     haderror = haderror || flush(o) == ERROR;
     haderror = haderror || o.infd != o.outfd && close(o.outfd) == ERROR;
     haderror = haderror || o.infd == NOFD && (
	  o.pid != 0 && (stat = wait(o.pid); o.pid = 0; 0 != stat) ||
	  o.listenerfd != NOFD && close(o.listenerfd) == ERROR
	  );
     o.outfd = NOFD;
     o.output = false;
     cleanUp(o);
     if haderror then (
	  if stat != 0 then return errmsg("close: process exited with code " + tostring(stat));
	  return errmsg("closing output file: " + syserrmsg());
	  );
     null());

export close(o:file):(errmsg or null) := (
     if !o.input && !o.output && !o.listener then return (errmsg or null)(errmsg("close: file not open"));
     stat := (errmsg or null)(null());
     if o.input then (
	  stat = closeIn(o);
	  );
     if o.output then (
	  r := closeOut(o);
	  when r is errmsg do when stat is null do stat = r else nothing else nothing;
	  );
     if o.listener then (
	  r := closeListener(o);
	  when r is errmsg do when stat is null do stat = r else nothing else nothing;
	  );
     stat);

closem():void := (
     f := openfiles;
     while true do when f is null do break is fileCell:FileCell do (
	  flush(fileCell.file);
	  f = fileCell.next;
	  );
     );

atend(closem);

export (o:file) << (n:Net) : file := (
     foss := getFileFOSS(o);
     if o.output then (
	  if !foss.hadNet then (
	       m := foss.outindex - foss.outbol;
	       if m < 0 then Ccode(returns,"puts(\"internal error: beginning of line marker not within buffer\"); abort();");
	       if m > 0 then ( 
		    -- remove the first part of the line from the buffer and add it, as a net, to the (currently empty) list of nets
		    s := toNet(new string len m do for i from foss.outbol to foss.outindex - 1 do provide foss.outbuffer.i);
		    -- Ccode(void,"printf(\"adding a string of length %d starting at %d to the list of nets\\n\",", m, ",", foss.outbol, ");");
		    foss.nets = NetList(foss.nets,s);
		    foss.outindex = foss.outbol;
		    );
     	       foss.hadNet = true;
	       );
     	  foss.nets = NetList(foss.nets,n);
	  );
     releaseFileFOSS(o);
     o);

export (o:file) << (c:char) : file := (
     if test(interruptedFlag) then return o;
     foss := getFileFOSS(o);
     if o.output then (
	  if foss.hadNet then (
     	       foss.nets = NetList(foss.nets,toNet(c));
	       )
	  else (
	       if foss.outindex == length(foss.outbuffer)
	       && ERROR == flush(o) then (releaseFileFOSS(o); return o);
	       foss.outbuffer.(foss.outindex) = c;
	       foss.outindex = foss.outindex + 1;
	       );
	  );
     releaseFileFOSS(o);
     o
     );

export (o:file) << (x:string) : file := (
     foss := getFileFOSS(o);
     if o.output then (
	  if foss.hadNet then (
	       o << toNet(x);
	       )
	  else (
     	       foreach c in x do o << c;
	       );
	  );
     releaseFileFOSS(o);
     o );

endlfun(o:file):int := (
     if o.output then (
     	  foss := getFileFOSS(o);
	  if foss.hadNet then if ERROR == flushnets(o) then (releaseFileFOSS(o); return ERROR);
	  o << newline;
	  foss.outbol = foss.outindex;
     	  releaseFileFOSS(o);
	  if o.outisatty || o == stdError then simpleflush(o) else NOERROR)
     else (
	  stderr << "not an output file" << newline;
	  ERROR));

maybeprompt(o:file):void := (
     o.bol = false;
     if o.promptq then stdIO << o.prompt(); -- does this ever really happen?  Experiments show not.  Some logic might be wrong.
     );

octal(c:char):string := (
     i := int(c) & 0xff;
     if i < 8
     then new string len 1 do provide ('0'+i)
     else if i < 64 then new string len 2 do (provide '0'+i/8; provide '0'+i%8)
     else new string len 3 do (provide '0'+i/64; provide '0'+(i/8)%8; provide '0'+i%8)
     );

export present(c:char):string := (
     c = c & 0xff;
     if c == '\r' then "\\r" else
     if c == '\f' then "\\f" else
     if c == '\t' then "\\t" else
     if c == '\n' then "\\n" else
     if c == '\"' then "\\\"" else
     if c == '\\' then "\\\\" else
     if c < char(32) || c == char(127) 
     then '\\' + octal(c)
     else string(c)
     );

export present(x:string):string := (
     fixesneeded := 0;
     foreach cc in x do (
	  c := cc; 
	  if c == char(0) || c == '\t' || c == '\b' || c == '\r' || c == '\"' || c == '\\' 
	  then fixesneeded = fixesneeded + 1 
	  );
     if fixesneeded != 0 then (
	  new string len length(x)+fixesneeded do foreach cc in x do (
	       c := cc;
	       if c == char(0) then (provide '\\'; provide '0';)
	       else if c == '\r' then (provide '\\'; provide 'r';)
	       else if c == '\b' then (provide '\\'; provide 'b';)
	       else if c == '\t' then (provide '\\'; provide 't';)
	       else (
		    if c == '\"' || c == '\\' then provide '\\';
	       	    provide c;
		    )
	       ))
     else x);

export presentn(x:string):string := ( -- fix newlines, also
     fixesneeded := 0;
     foreach cc in x do (
	  c := cc; 
	  if c == char(0) || c == '\t' || c == '\b' || c == '\r' || c == '\"' || c == '\\' || c == '\n' 
	  then fixesneeded = fixesneeded + 1 
	  );
     if fixesneeded != 0 then (
	  new string len length(x)+fixesneeded do foreach cc in x do (
	       c := cc;
	       if c == char(0) then (provide '\\'; provide '0';)
	       else if c == '\r' then (provide '\\'; provide 'r';)
	       else if c == '\n' then (provide '\\'; provide 'n';)
	       else if c == '\b' then (provide '\\'; provide 'b';)
	       else if c == '\t' then (provide '\\'; provide 't';)
	       else (
		    if c == '\"' || c == '\\' then provide '\\';
	       	    provide c;
		    )
	       ))
     else x);

export filbuf(o:file):int := (
--      if o.fulllines then (
-- 	  flush(stdIO);
-- 	  stderr << "--filbuf (fulllines, bol=" << (if o.bol then "true" else "false")
-- 	  << ", promptq=" << (if o.promptq then "true" else "false")
-- 	  << ", reward()=" << present(o.reward())
-- 	  << "): ";
-- 	  for i from o.inindex to o.insize-1 do (
-- 	       if o.inbuffer.i == '\n' then stderr << "\\n" else stderr << o.inbuffer.i;
-- 	       );
-- 	  stderr << '\n';
-- 	  flush(stderr);
--      	  );
     -- returns number of bytes added to buffer, or ERROR if a system call had an error
     if ! o.input then return 0;
     if o.inindex > 0
     then (
	  o.insize = o.insize - o.inindex;
	  o.echoindex = o.echoindex - o.inindex;
	  if o.echoindex < 0 then o.echoindex = 0;
	  for i from 0 to o.insize - 1 do o.inbuffer.i = o.inbuffer.(i+o.inindex);
	  o.inindex = 0;
	  );
     if o.fulllines then (
	  for i from 0 to o.insize-1 do if o.inbuffer.i == '\n' then return 0;
	  );
     r := 0;     
     while true do (
	  n := length(o.inbuffer) - o.insize;
	  if o.readline then (
	       initReadlineVariables();
	       flush(stdIO);
	       if test(interruptedFlag) then return ERROR;
	       startFileInput(o);
	       r = readline(o.inbuffer,n,o.insize,o.prompt());
	       endFileInput(o);
	       )
	  else (
	       if o.bol then maybeprompt(o);
	       flush(stdIO);
	       if test(interruptedFlag) then return ERROR;
	       r = (
		    if o.infd == NOFD 
		    then 0 -- take care of "string files" made by stringTokenFile in interp.d
		    else read(o.infd,o.inbuffer,n,o.insize)));
	  if r == ERROR then (
	       fileErrorMessage(o,"read");
	       return r;
	       )
	  else if r == 0 then (
	       o.eof = true;
	       o.bol = true;
	       if o.promptq then (
		    stdIO << newline;
		    flush(stdIO);
		    );
	       return r;
	       )
	  else (
	       oldsize := o.insize;
	       newsize := o.insize + r;
	       o.insize = newsize;
	       if o.fulllines then (
		    for i from newsize-1 to oldsize by -1 do if o.inbuffer.i == '\n' then (
			 if o.promptq then (
			      o << o.reward();
			      flush(o);
			      );
			 return r;
			 );
	  	    if o.inindex == 0 && o.insize == length(o.inbuffer)
	  	    then o.inbuffer = new string len 2*length(o.inbuffer) do ( foreach c in o.inbuffer do provide c; while true do provide ' ' );
		    )
	       else return r;
	       )));

putdigit(o:file,x:int):void := o << (x + if x<10 then '0' else 'a'-10) ;

putneg(o:file,x:int):void := (
     if x<0 then (
	  q := x/10;
	  r := x%10;
	  if r>0 then (r=r-10;q=q+1);
     	  putneg(o,q);
     	  putdigit(o,-r)));

export (o:file) << (x:int) : file :=  (
   if x==0
   then putdigit(o,0)
   else (
	if x<0 
	then ( 
	     o << '-';
	     putneg(o,x);
	     )
	else putneg(o,-x);
	);
   o);

export (o:file) << (x:short) : file := o << int(x);

export (o:file) << (x:ushort) : file := o << int(x);

export (o:file) << (x:uchar) : file := o << int(x);

export (o:file) << (b:bool) : file := (
     o << if b then "true" else "false");

digits(o:varstring,x:double,a:int,b:int):void := (
     x = x + 0.5 * pow(10.,double(1-a-b));
     if x >= 10. then (x = x/10.; a = a+1; b = if b==0 then 0 else b-1);
     while a > 0 do (
	  putdigit(o,int(x));
	  x = 10. * (x - double(int(x)));
	  a = a-1;
	  );
     o << '.';
     lim := pow(10.,double(-b+1));
     while b > 0 do (
	  if x < lim then break;
	  putdigit(o,int(x));
	  x = 10. * (x - double(int(x)));
	  lim = lim * 10.;
	  b = b-1;
	  ));

export finite(x:double):bool := x==x && x-x == x-x;

export isinf(x:double):bool := x==x && x-x != x-x;

export isnan(x:double):bool := x!=x;

export tostring(x:bool):string := if x then "true" else "false";

export tostring5(
     x:double,						-- the number to format
     s:int,					-- number of significant digits
     l:int,					   -- max number leading zeroes
     t:int,				    -- max number extra trailing digits
     e:string			     -- separator between mantissa and exponent
     ) : string := (
     o := newvarstring(25);
     if isinf(x) then return "infinity";
     if isnan(x) then return "NotANumber";
     if x==0. then return "0.";
     if x<0. then (o << '-'; x=-x);
     oldx := x;
     i := 0;
     if x >= 1. then (
     	  until x < 10000000000. do ( x = x/10000000000.; i = i + 10 );
     	  until x < 100000. do ( x = x/100000.; i = i + 5 );
     	  until x < 100. do ( x = x/100.; i = i + 2 );
     	  until x < 10. do ( x = x/10.; i = i + 1 );
	  )
     else (
     	  until x >= 1./10000000000. do ( x = x*10000000000.; i = i - 10 );
     	  until x >= 1./100000. do ( x = x*100000.; i = i - 5 );
     	  until x >= 1./100. do ( x = x*100.; i = i - 2 );
     	  until x >= 1. do ( x = x*10.; i = i - 1 );
	  );
     -- should rewrite this so the format it chooses is the one that takes the least space, preferring not to use the exponent when it's a tie
     if i<0 then (
	  if -i <= l 
	  then digits(o,oldx,1,s-i-1)
	  else (digits(o,x,1,s-1); o << e << tostring(i);))
     else if i+1 > s then (
	  if i+1-s <= t
	  then digits(o,x,i+1,0)
	  else (digits(o,x,1,s-1); o << e << tostring(i);))
     else digits(o,x,i+1,s-i-1);
     tostring(o));

export tostringRR(x:double) : string := tostring5(x,6,5,5,"e");

export (o:file) << (x:double) : file := o << tostringRR(x);

nl := if length(newline) > 0 then newline.(length(newline)-1) else '\n';

export getc(o:file):int := (
     startFileInput(o);
     if !o.input then (endFileInput(o); return EOF;);
     if o.inindex == o.insize then (
	  r := filbuf(o);
	  if r == 0 then (endFileInput(o); return EOF;)
	  else if r == ERROR then (endFileInput(o); return ERROR;);
	  )
     else if o.bol && !o.readline then maybeprompt(o);
     c := o.inbuffer.(o.inindex);
     o.inindex = o.inindex + 1;
     if o.echo && o.echoindex < o.inindex then (
	  while o.echoindex < o.insize && (
	       e := o.inbuffer.(o.echoindex);
	       stdIO << e; 
	       o.echoindex = o.echoindex + 1;
     	       e != '\n'
	       )
	  do nothing;
	  flush(stdIO);
	  );
     if c == nl then (
	  o.bol = true;
	  if o.echo then flush(stdIO);
	  );
     x:=int(uchar(c));
     endFileInput(o);
     x
);
export StringOrError := stringCell or errmsg;

export read(o:file):StringOrError := (
     startFileInput(o);
     if o.inindex == o.insize then (
	  r := filbuf(o);
	  if r == ERROR then (endFileInput(o); return StringOrError(errmsg(fileErrorMessage(o))));
	  )
     else if o.bol && !o.readline then maybeprompt(o);
     s := substrAlwaysCopy(o.inbuffer,o.inindex,o.insize);
     o.insize = 0;
     o.inindex = 0;
     if o.echo then (
	  stdIO << s;
	  flush(stdIO);
	  );
     sc:=stringCell(s);
     endFileInput(o);
     sc
);

export peek(o:file,offset:int):int := (
     startFileInput(o);
     if !o.input then (endFileInput(o); return EOF;);
     if offset >= bufsize then (endFileInput(o); return ERROR;);		    -- tried to peek too far
     if o.inindex+offset >= o.insize then (
	  if o.eof then (endFileInput(o); return EOF;);
     	  while (
	       r := filbuf(o);
	       if r == ERROR then (endFileInput(o); return ERROR;);
	       o.inindex+offset >= o.insize
	       )
	  do (
	       if o.eof then (endFileInput(o); return EOF);
	       );
	  );
     x := int(uchar(o.inbuffer.(o.inindex+offset)));
     endFileInput(o);
     x
);

export peek(o:file):int := peek(o,0);

someblanks := new array(string) len 20 at n do provide new string len n do provide ' ';

export blanks(n:int):string := if n < length(someblanks) then someblanks.n else new string len n do provide ' ';

padto(s:string,n:int):string := (
     if n<0
     then (
	  n = -n;
     	  if length(s) >= n
     	  then s
     	  else blanks(n-length(s)) + s
	  )
     else (
     	  if length(s) >= n
     	  then s
     	  else s + blanks(n-length(s))
	  )
     );

export (v:varstring) << (i:int) : varstring := v << tostring(i);

export (o:file) << (s:string, n:int) : file := o << padto(s,n);

export (o:file) << (i:int, n:int) : file := o << (tostring(i),n);

export setprompt(o:file,prompt:function():string):void := ( o.promptq = true; o.prompt = prompt; o.reward=noprompt;);

export unsetprompt(o:file):void := ( o.promptq = false; o.prompt = noprompt; o.reward=noprompt; );

export clean(o:file):void := flush(o);

export get(filename:string):StringOrError := (
     when openIn(filename)
     is x:errmsg do StringOrError(x)
     is f:file do (
	  when readfile(f.infd)
	  is null do (
	       close(f);
	       StringOrError(errmsg(fileErrorMessage(f,"read"))))
	  is s:string do (
	       r := close(f);
	       when r is m:errmsg
	       do StringOrError(m)
	       else StringOrError(stringCell(s)))));

export Manipulator := {fun:function(file):int};

export (o:file) << (m:Manipulator) : file := (
     m.fun(o);				  -- ignoring error here
     o
     );

export endl := Manipulator(endlfun);

export Flush := Manipulator(flush);

export (o:BasicFile) << (m:Manipulator) : int := (
     if m == endl then o << basicEndl
     else if m == Flush then o << basicFlush
     else -1);

export fchmod(o:file,mode:int):int := (
     if o.input && o.infd != -1 then if -1 == fchmod(o.infd,mode) then return -1;
     if o.output && o.outfd != -1 then if -1 == fchmod(o.outfd,mode) then return -1;
     0);

lastCharWritten(o:file):int := (
     foss := getFileFOSS(o);
     if foss.outindex > 0 then 
         (releaseFileFOSS(o); int(foss.outbuffer.(foss.outindex-1)))
     else 
         (releaseFileFOSS(o); foss.lastCharOut)
);

export atEndOfLine(o:file):bool := ( c := lastCharWritten(o); c == int('\n') || c == -1);

export endLine(o:file):void := (
     if !atEndOfLine(o) || !atEndOfLine(stdIO) then o << '\n';
     -- usually o == stderr != stdIO
     -- we might put a needless one out in the case where o.outfd == stdIO.outfd, oh well.
     );

export (o:file) << (x:long) : file :=  o << tostring(x);

export (o:file) << (x:ulong) : file :=  o << tostring(x);

export setIOSynchronized(e:Expr):Expr :=(
     when e
     is a:Sequence do (
	  if length(a) == 0
	  then (setFileThreadState(stdIO,1); setFileThreadState(stdError,1); nullE)
	  else WrongNumArgs(0))
     else WrongNumArgs(0)
);

export setIOExclusive(e:Expr):Expr :=(
     when e
     is a:Sequence do (
	  if length(a) == 0
	  then (setFileThreadState(stdIO,2); setFileThreadState(stdError,2); nullE)
	  else WrongNumArgs(0))
     else WrongNumArgs(0)
);

export setIOUnSynchronized(e:Expr):Expr :=(
     when e
     is a:Sequence do (
	  if length(a) == 0
	  then (setFileThreadState(stdIO,0); setFileThreadState(stdError,0); nullE)
	  else WrongNumArgs(0))
     else WrongNumArgs(0)
);


-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d stdio.o "
-- End:
