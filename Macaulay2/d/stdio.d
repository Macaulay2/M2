--		Copyright 1994 by Daniel R. Grayson
use C;
use system;
use strings;
use varstrin;
use nets;

export HashCounter := 1000000;
export nextHash():int := (
     HashCounter = HashCounter + 1;
     HashCounter);

export ERROR := -1;
export NOFD := -1;
export EOF := -2;					    -- end of file
export NULL := null();
export STDIN := 0;
export STDOUT := 1;
export STDERR := 2;
bufsize := 4 * 1024;

export iseof      (c:int ):bool := c == EOF;
export iserror    (c:int ):bool := c == ERROR;

export file := {
        -- general stuff
     	hash:int,     	   	-- hash code
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
        promptq:bool,           -- whether to prompt for input
	prompt:function():string, -- function to call to get prompt string when input is required
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
	outbol:int,	        -- outbuffer.outbol = first char of the current line
	     	       	        -- The text after this point may be combined with
				-- subsequently printed nets.
        hadNet:bool,		-- whether a Net is present, in which case the
	     	       	        -- buffer will be empty
	nets:NetList,	        -- list of nets, to be printed after the outbuffer
        bytesWritten:int,       -- bytes written so far
	lastCharOut:int,        -- when outbuffer empty, last character written, or -1 if none
        readline:bool           -- input handled by readline()
	};

export syscallErrorMessage(msg:string):string := msg + " failed: " + syserrmsg();
export fileErrorMessage(o:file,msg:string):string := (
     o.error = true;
     o.errorMessage = syscallErrorMessage(msg);
     o.errorMessage);
export fileError(o:file):bool := o.error;
export fileErrorMessage(o:file):string := o.errorMessage;
export noprompt():string := "";
newbuffer():string := new string len bufsize do provide ' ';
export dummyfile := file(nextHash(), "dummy",0, 
     false, "",
     false,NOFD,NOFD,0,
     false,NOFD,false,        "",          0,0,false,false,noprompt,true,false,0,
     false,NOFD,false,        "",	    	 0,0,false,dummyNetList,0,-1,false);
export stdIO  := file(nextHash(),  "stdio",  0, 
     false, "",
     false, NOFD,NOFD,0,
     true,  STDIN ,0!=isatty(0), newbuffer(), 0,0,false,false,noprompt,true,false,0,
     true,  STDOUT,0!=isatty(1), newbuffer(), 0,0,false,dummyNetList,0,-1,false);

init():void := (
     stdIO.readline = stdIO.infd == STDIN && stdIO.inisatty ;
     foreach arg in args do if arg === "-x" || arg === "--no-readline" then stdIO.readline = false;
     -- strange but true: readline refuses to display CTRL-A when it occurs in a prompt
     );

everytime(init);

export stdin  := stdIO;
export stdout := stdIO;
export stderr := file(nextHash(), "stderr", 0, 
     false, "",
     false,NOFD,NOFD,0,
     false,NOFD  ,false,          "",        0,0,false,false,noprompt,true,false,0,
     true, STDERR,0!=isatty(2), newbuffer(), 0,0,false,dummyNetList,0,-1,false);
export errmsg := { message:string };
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
	       if thiscell.file == o then 
	       when prevcell is null do openfiles = thiscell.next
	       is prevcell:FileCell do prevcell.next = thiscell.next
	       else prevcell = x;
	       x = thiscell.next;
	       )));
addfile(stdout);
addfile(stderr);
opensocket(filename:string,input:bool,output:bool,listener:bool):(file or errmsg) := (
     host := substr(filename,1);
     serv := "2500";		  -- Macaulay 2 default port
     foreach c at j in filename do if c == ':' then (
	  host = substr(filename,1,j-1);
	  serv = substr(filename,j+1);
	  break;
	  );
     so := NOFD;
     sd := NOFD;
     if length(host) == 0 then (
	  so = openlistener(serv);
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
	  sd = opensocket(host,serv);
     	  if sd == ERROR then return (file or errmsg)(errmsg("can't open socket : "+syserrmsg()));
	  );
     (file or errmsg)(addfile(file(nextHash(), filename, 0,
	  false, "",
	  listener, so, NOFD, if listener then 0 else 1,
	  input, if input then sd else NOFD, false, if input then newbuffer() else "", 
	  0, 0, false, false,noprompt, true, false,0,
	  output, if output then sd else NOFD, false, if output then newbuffer() else "",
	  0, 0, false, dummyNetList,0,-1,false))));
accept(f:file,input:bool,output:bool):(file or errmsg) := (
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
     (file or errmsg)(addfile(file(nextHash(), f.filename, 0,
	  false, "",
	  false, NOFD,NOFD,f.numconns,
	  input, if input then sd else NOFD, false, if input then newbuffer() else "", 
	  0, 0, false, false,noprompt, true, false, 0,
	  output, if output then sd else NOFD, false, if output then newbuffer() else "",
	  0, 0, false, dummyNetList,0,-1,false))));
openpipe(filename:string,input:bool,output:bool):(file or errmsg) := (
     toChild := array(int)(NOFD,NOFD);
     fromChild := array(int)(NOFD,NOFD);
     if output && pipe(toChild) == ERROR || input && pipe(fromChild) == ERROR 
     then (
	  if  toChild.0 != NOFD then close(  toChild.0);
	  if  toChild.1 != NOFD then close(  toChild.1);
	  return errmsg("can't make pipe : "+syserrmsg());
	  );
     pid := fork();
     if pid == -1 then (
	  if  toChild.0 != NOFD then close(  toChild.0);
	  if  toChild.1 != NOFD then close(  toChild.1);
	  return errmsg("can't fork : "+syserrmsg());
	  );
     listener := false;
     if pid == 0 then (
	  close(STDIN);
	  if output then (
	       close(  toChild.1);
	       if toChild.0 != STDIN then (dup2(toChild.0, STDIN); close(toChild.0););
	       );
	  if input then (
	       close(fromChild.0);
	       if fromChild.1 != STDOUT then (dup2(fromChild.1, STDOUT); close(fromChild.1););
	       );
	  exec(array(string)("/bin/sh","-c",substr(filename,1)));
	  write(2,"exec of /bin/sh failed!\n",24);
	  exit(1);
	  );
     if input then close(fromChild.1);
     if output then close(toChild.0);
     (file or errmsg)(addfile(file(nextHash(), filename, pid, 
	  false, "",
	  listener, NOFD,NOFD,0,
	  input, if input then fromChild.0 else NOFD, false, if input then newbuffer() else "", 
	  0, 0, false, false,noprompt, true, false, 0,
	  output, if output then toChild.1 else NOFD, false, if output then newbuffer() else "",
	  0, 0, false, dummyNetList,0,-1,false))));
export openIn(f:file):(file or errmsg) := accept(f,true,false);
export openOut(f:file):(file or errmsg) := accept(f,false,true);
export openInOut(f:file):(file or errmsg) := accept(f,true,true);
export openIn(filename:string):(file or errmsg) := (
     if filename === "-"
     then (file or errmsg)(stdin)
     else if length(filename) > 0 && filename . 0 == '$'
     then opensocket(filename,true,false,false)
     else if length(filename) > 0 && filename . 0 == '!'
     then openpipe(filename,true,false)
     else (
     	  fd := openin(filename);
     	  if fd == ERROR
     	  then (file or errmsg)(errmsg(syscallErrorMessage("opening input file '"+filename+ "'")))
     	  else (file or errmsg)(addfile(file(nextHash(), filename, 0, 
	  	    false, "",
		    false, NOFD,NOFD,0,
		    true,  fd, 0 != isatty(fd), newbuffer(), 0, 0, false, false,noprompt,true,false,0,
		    false, NOFD, false,           "",          0, 0, false, dummyNetList,0,-1,false)))));
export openOut(filename:string):(file or errmsg) := (
     if filename === "-"
     then (file or errmsg)(stdout)
     else if length(filename) > 0 && filename . 0 == '$'
     then opensocket(filename,false,true,false)
     else if length(filename) > 0 && filename . 0 == '!'
     then openpipe(filename,false,true)
     else (
     	  fd := openout(filename);
     	  if fd == ERROR
     	  then (file or errmsg)(errmsg(syscallErrorMessage("opening output file '"+filename+"'")))
     	  else (file or errmsg)(addfile(file(nextHash(), filename, 0, 
	  	    false, "",
		    false, NOFD,NOFD,0,
		    false, NOFD, false,           "",          0, 0, false,false,noprompt,true,false,0,
		    true,  fd, 0 != isatty(fd), newbuffer(), 0, 0, false,dummyNetList,0,-1,false)))));
export openOutAppend(filename:string):(file or errmsg) := (
     fd := openoutappend(filename);
     if fd == ERROR
     then (file or errmsg)(errmsg(syscallErrorMessage("opening output (append) file '"+filename+"'")))
     else (file or errmsg)(addfile(file(nextHash(), filename, 0, 
	       false, "",
	       false, NOFD,NOFD,0,
	       false, NOFD, false,           "",          0, 0, false,false,noprompt,true,false,0,
	       true,  fd, 0 != isatty(fd), newbuffer(), 0, 0, false,dummyNetList,0,-1,false))));
export openInOut(filename:string):(file or errmsg) := (
     if filename === "-"
     then (file or errmsg)(stdIO)
     else if length(filename) > 0 && filename . 0 == '$'
     then opensocket(filename,true,true,false)
     else if length(filename) > 0 && filename . 0 == '!'
     then openpipe(filename,true,true)
     else (file or errmsg)(errmsg("can't open file "+filename+" for both input and output")));
export openListener(filename:string):(file or errmsg) := (
     if length(filename) > 0 && filename . 0 == '$'
     then opensocket(filename,false,false,true)
     else (file or errmsg)(errmsg("can't open file "+filename+" as listener")));
export flushinput(o:file):void := (
     o.inindex = 0;
     o.insize = 0;
     o.bol = true;
     );

simpleflush(o:file):int := (				    -- write to file or enlarge the buffer
     o.outbol = 0;
     if o.outindex == 0 then return 0;
     if o.outfd != -1 then (
	  if interruptedFlag then return -1;
	  if write(o.outfd,o.outbuffer,o.outindex) == -1 then (
	       fileErrorMessage(o,"writing");
	       return -1;
	       );
	  o.lastCharOut = int(o.outbuffer.(o.outindex-1));
     	  o.bytesWritten = o.bytesWritten + o.outindex;
	  o.outindex = 0;
	  )
     else if o.outindex == length(o.outbuffer) then (
	  o.outbuffer = enlarge(length(o.outbuffer),o.outbuffer);
	  );
     0);
simpleout(o:file,c:char):int := (
     if o.outindex == length(o.outbuffer) && simpleflush(o) == ERROR then return ERROR;
     o.outbuffer.(o.outindex) = c;
     o.outindex = o.outindex + 1;
     0);
simpleout(o:file,x:string):int := (
     i := 0;						    -- bytes of x transferred so far
     m := length(x);
     j := o.outindex;
     n := length(o.outbuffer);
     while i < m do (
	  if j == n then (
	       if simpleflush(o) == ERROR then return ERROR;
	       j = o.outindex;
	       );
	  b := m-i;					    -- number of bytes to transfer this time
	  if b > n-j then b = n-j;
	  for k from 0 to b-1 do o.outbuffer.(j+k) = x.(i+k);
	  i = i + b;
	  j = j + b;
	  o.outindex = j;
	  );
     0);
flushnets(o:file):int := (
     if o.hadNet then (
	  n := HorizontalJoin(o.nets);
	  o.hadNet = false;
	  o.nets = dummyNetList;
	  lastone := length(n.body)-1;
	  foreach s at i in n.body do (
	       if ERROR == simpleout(o,s) then return ERROR;
	       if i != lastone then (
		    if ERROR == simpleout(o,newline) then return ERROR;
		    );
	       ); 
	  );
     0);
export flush(o:file):int := (
     if o.hadNet then if ERROR == flushnets(o) then return ERROR;
     simpleflush(o));
cleanUp(o:file):void := (
     if !o.listener && !o.input && !o.output then (
	  rmfile(o);
	  );
     );
export closeListener(o:file):int := (
     if o.listenerfd == NOFD then return ERROR;
     haderror := close(o.listenerfd) == ERROR;
     o.listenerfd = NOFD;
     o.listener = false;
     cleanUp(o); 
     if haderror then ERROR else 0);
export closeIn(o:file):int := (
     if o.infd == NOFD then return ERROR;
     if o == stdin then return 0;			    -- silently refuse to close stdin
     if o.input then flushinput(o);
     haderror := false;
     haderror = haderror || o.infd != o.outfd && close(o.infd) == ERROR;
     haderror = haderror || o.outfd == NOFD && (
	  o.pid != 0 && 0 != wait(o.pid) ||
	  o.listenerfd != NOFD && close(o.listenerfd) == ERROR
	  );
     o.infd = NOFD;
     o.input = false;
     cleanUp(o);
     if haderror then ERROR else 0     
     );
export closeOut(o:file):int := (
     if o.outfd == NOFD then return ERROR;
     haderror := false;
     haderror = haderror || flush(o) == ERROR;
     haderror = haderror || o.infd != o.outfd && close(o.outfd) == ERROR;
     haderror = haderror || o.infd == NOFD && (
	  o.pid != 0 && 0 != wait(o.pid) ||
	  o.listenerfd != NOFD && close(o.listenerfd) == ERROR
	  );
     o.outfd = NOFD;
     o.output = false;
     cleanUp(o);
     if haderror then ERROR else 0     
     );
export close(o:file):int := (
     if o.input && closeIn(o) == ERROR
     || o.output && closeOut(o) == ERROR
     || o.listener && closeListener(o) == ERROR 
     then ERROR else 0
     );
closem():void := (
     f := openfiles;
     while true do when f is null do break is fileCell:FileCell do (
	  flush(fileCell.file);
	  f = fileCell.next;
	  );
     );
atend(closem);
export (o:file) << (n:Net) : file := (
     if o.output then (
	  if !o.hadNet then (
	       if o.outindex != o.outbol then (
		    o.nets = NetList(o.nets,
			 toNet(
			      new string len o.outindex - o.outbol do
			      for i from o.outbol to o.outindex - 1 do
			      provide o.outbuffer.i
			      ));
		    o.outindex = o.outbol;
		    );
     	       o.hadNet = true;
	       );
     	  o.nets = NetList(o.nets,n);
	  );
     o);
export (o:file) << (c:char) : file := (
     if o.output then (
	  if o.hadNet then (
     	       o.hadNet = true;
     	       o.nets = NetList(o.nets,toNet(c));
	       )
	  else (
	       if o.outindex == length(o.outbuffer) 
	       then flush(o);		  -- possible error ignored!
	       o.outbuffer.(o.outindex) = c;
	       o.outindex = o.outindex + 1;
	       );
	  );
     o
     );

export (o:file) << (x:string) : file := (
     if o.output then (
	  if o.hadNet then (
	       o << toNet(x);
	       )
	  else (
     	       foreach c in x do o << c;
	       );
	  );
     o );

endlfun(o:file):int := (
     if o.output then (
	  if o.hadNet then if ERROR == flushnets(o) then return ERROR;
	  o << newline;
	  if o.outisatty || o == stderr 
	  then (
	       if ERROR == simpleflush(o) then return ERROR;
	       )
	  else (
	       o.outbol = o.outindex;
	       );
	  );
     0);

maybeprompt(o:file):void := (
     o.bol = false;
     if o.promptq then stdout << o.prompt();
     );

export filbuf(o:file):int := (
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
     n := length(o.inbuffer) - o.insize;
     r := (
	  if o.readline then (
	       flush(stdout);
	       readline(o.inbuffer,n,o.insize,o.prompt()))
	  else (
     	       if o.bol then maybeprompt(o);
	       flush(stdout);
	       read(o.infd,o.inbuffer,n,o.insize)));
     if r == ERROR then (fileErrorMessage(o,"read");)
     else if r == 0 then (
	  o.eof = true;
	  o.bol = true;
	  if o.promptq then (
	       stdout << newline;
	       flush(stdout);
	       );
	  )
     else o.insize = o.insize + r;
     r);

putdigit(o:file,x:int):void := o << (x + if x<10 then '0' else 'a'-10) ;
putdigit(o:varstring,x:int):void := o << (x + if x<10 then '0' else 'a'-10) ;
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
     foreach cc in x do (c := cc; if c == char(0) || c == '\t' || c == '\r' || c == '\"' || c == '\\' then fixesneeded = fixesneeded + 1 );
     if fixesneeded != 0 then (
	  new string len length(x)+fixesneeded do foreach cc in x do (
	       c := cc;
	       if c == char(0) then (provide '\\'; provide '0';)
	       else if c == '\r' then (provide '\\'; provide 'r';)
	       else if c == '\t' then (provide '\\'; provide 't';)
	       else (
		    if c == '\"' || c == '\\' then provide '\\';
	       	    provide c;
		    )
	       ))
     else x);
export tostring(i:int):string := (
     if i==0 then return "0";
     s := newvarstring(25);
     sign := i<0;
     if sign then i=-i;
     while i>0 do (
	  s << "0123456789".(i%10);
	  i = i/10;
	  );
     if sign then s << '-';
     toreversestring(s));
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

export printingPrecision := 6;
export printingLeadLimit := 5;
export printingTrailLimit := 5;
export printingSeparator := "*10^";
export tostringRR(x:double) : string := tostring5(x,printingPrecision,printingLeadLimit,printingTrailLimit,printingSeparator);
export (o:file) << (x:double) : file := o << tostringRR(x);

nl := if length(newline) > 0 then newline.(length(newline)-1) else '\n';

export getc(o:file):int := (
     if !o.input then return EOF;
     if o.inindex == o.insize then (
	  r := filbuf(o);
	  if r == 0 then return EOF
	  else if r == ERROR then return ERROR;
	  )
     else if o.bol && !o.readline then maybeprompt(o);
     c := o.inbuffer.(o.inindex);
     o.inindex = o.inindex + 1;
     if o.echo && o.echoindex < o.inindex then (
	  while o.echoindex < o.insize && (
	       e := o.inbuffer.(o.echoindex);
	       stdout << e; 
	       o.echoindex = o.echoindex + 1;
     	       e != '\n'
	       )
	  do nothing;
	  flush(stdout);
	  );
     if c == nl then (
	  o.bol = true;
	  if o.echo then flush(stdout);
	  );
     int(uchar(c)));
export read(o:file):(string or errmsg) := (
     if o.inindex == o.insize then (
	  r := filbuf(o);
	  if r == ERROR then return (string or errmsg)(errmsg(fileErrorMessage(o)));
	  )
     else if o.bol && !o.readline then maybeprompt(o);
     s := substr(o.inbuffer,o.inindex,o.insize);
     o.insize = 0;
     o.inindex = 0;
     if o.echo then (
	  stdout << s;
	  flush(stdout);
	  );
     s);
export peek(o:file,offset:int):int := (
     if !o.input then return EOF;
     if offset >= bufsize then return ERROR;		    -- tried to peek too far
     if o.inindex+offset >= o.insize then (
	  if o.eof then return EOF;
     	  while (
	       r := filbuf(o);
	       if r == ERROR then return ERROR;
	       o.inindex+offset >= o.insize
	       )
	  do (
	       if o.eof then return EOF;
	       );
	  );
     int(uchar(o.inbuffer.(o.inindex+offset))));
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
export setprompt(o:file,prompt:function():string):void := ( o.promptq = true; o.prompt = prompt; );
export unsetprompt(o:file):void := ( o.promptq = false; o.prompt = noprompt; );
export clean(o:file):void := flush(o);

export get(filename:string):(string or errmsg) := (
     when openIn(filename)
     is x:errmsg do (string or errmsg)(x)
     is f:file do (
	  when readfile(f.infd)
	  is null do (
	       close(f);
	       (string or errmsg)(errmsg(fileErrorMessage(f,"read")))
	       )
	  is s:string do (
	       r := close(f);
	       if r == ERROR then (string or errmsg)(errmsg(fileErrorMessage(f,"close")))
	       else if r != 0 && length(filename) > 0 && filename . 0 == '!'
	       then (string or errmsg)(errmsg("process exit code " + tostring(r)))
	       else (string or errmsg)(s))));

export Manipulator := {fun:function(file):int};
export (o:file) << (m:Manipulator) : file := (
     m.fun(o);				  -- ignoring error here
     o
     );
export endl := Manipulator(endlfun);
export Flush := Manipulator(flush);

export fchmod(o:file,mode:int):int := (
     if o.input && o.infd != -1 then if -1 == fchmod(o.infd,mode) then return -1;
     if o.output && o.outfd != -1 then if -1 == fchmod(o.outfd,mode) then return -1;
     0);

lastCharWritten(o:file):int := if o.outindex > 0 then int(o.outbuffer.(o.outindex-1)) else o.lastCharOut;
export atEndOfLine(o:file):bool := ( c := lastCharWritten(o); c == int('\n') || c == -1);
export endLine(o:file):void := (
     if !atEndOfLine(o) || !atEndOfLine(stdout) then o << '\n';
     -- usually o == stderr != stdout
     -- we might put a needless one out in the case where o.outfd == stdout.outfd, oh well.
     );

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
