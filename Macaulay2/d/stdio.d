--		Copyright 1994 by Daniel R. Grayson
use system;
use strings;
use C;
use varstrin;
use nets;

export HashCounter := 1000000;
export nextHash():int := (
     HashCounter = HashCounter + 1;
     HashCounter);

export ERROR := -1;
export NOFD := -1;
export EOF := -1;
export NULL := null();
export STDIN := 0;
export STDOUT := 1;
export STDERR := 2;
bufsize := 4 * 1024;
export file := {
     	hash:int,     	   	-- hash code
	filename:string,	-- name of file
	pid:int,	        -- pid if it's a pipe or pair of pipes to a child process, else 0
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
	prompt:function(file):void, -- function to call to prompt for input
        bol:bool,     	   	-- at the beginning of a line, and prompt not yet given
	echo:bool,     	   	-- whether to echo characters read to corresponding output file
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
	nets:NetList	        -- list of nets, to be printed after the outbuffer
	};
export noprompt(o:file):void := nothing;
newbuffer():string := new string len bufsize do provide ' ';
export dummyfile := file(nextHash(), "dummy",0, 
     false,NOFD,NOFD,0,
     false,NOFD,false,        "",          0,0,false,noprompt,true,false,
     false,NOFD,false,        "",	    	 0,0,false,dummyNetList);
export stdIO  := file(nextHash(),  "stdio",  0, 
     false, NOFD,NOFD,0,
     true,  STDIN ,0!=isatty(0), newbuffer(), 0,0,false,noprompt,true,false,
     true,  STDOUT,0!=isatty(1), newbuffer(), 0,0,false,dummyNetList);
export stdin  := stdIO;
export stdout := stdIO;
export stderr := file(nextHash(), "stderr", 0, 
     false,NOFD,NOFD,0,
     false,NOFD  ,false,          "",        0,0,false,noprompt,true,false,
     true, STDERR,0!=isatty(2), newbuffer(), 0,0,false,dummyNetList);
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
     	  if so == ERROR then return((file or errmsg)(errmsg("can't open listener : "+syserrmsg())));
	  if input || output then (
	       sd = acceptBlocking(so);
     	       if sd == ERROR then (
		    close(so);
		    return((file or errmsg)(errmsg("can't open socket: "+syserrmsg())));
		    );
	       )
	  )
     else (
	  sd = opensocket(host,serv);
     	  if sd == ERROR then return((file or errmsg)(errmsg("can't open socket : "+syserrmsg())));
	  );
     (file or errmsg)(addfile(file(nextHash(), filename, 0,
	  listener, so, NOFD, if listener then 0 else 1,
	  input, if input then sd else NOFD, false, if input then newbuffer() else "", 
	  0, 0, false, noprompt, true, false,
	  output, if output then sd else NOFD, false, if output then newbuffer() else "",
	  0, 0, false, dummyNetList))));
accept(f:file,input:bool,output:bool):(file or errmsg) := (
     if !f.listener then return((file or errmsg)(errmsg("expected a listener")));
     sd := NOFD;
     if f.connection != NOFD
     then (
	  sd = f.connection;
	  f.connection = NOFD;
	  )
     else (
     	  sd = acceptBlocking(f.listenerfd);
     	  if sd == ERROR then return((file or errmsg)(errmsg("can't accept connection : "+syserrmsg())));
	  );
     f.numconns = f.numconns + 1;
     (file or errmsg)(addfile(file(nextHash(), f.filename, 0,
	  false, NOFD,NOFD,f.numconns,
	  input, if input then sd else NOFD, false, if input then newbuffer() else "", 
	  0, 0, false, noprompt, true, false,
	  output, if output then sd else NOFD, false, if output then newbuffer() else "",
	  0, 0, false, dummyNetList))));
openpipe(filename:string,input:bool,output:bool):(file or errmsg) := (
     toChild := array(int)(NOFD,NOFD);
     fromChild := array(int)(NOFD,NOFD);
     if pipe(toChild) == ERROR || pipe(fromChild) == ERROR then return(errmsg("can't make pipe : "+syserrmsg()));
     pid := fork();
     listener := false;
     if pid == 0 then (
	  close(  toChild.1);
	  close(fromChild.0);
	  dup2(   toChild.0, 0);
	  dup2( fromChild.1, 1);
	  if      toChild.0 != 0 then close(toChild.0);
	  if    fromChild.1 != 1 then close(fromChild.1);
	  exec(array(string)("/bin/sh","-c",substr(filename,1)));
	  write(2,"exec of /bin/sh failed!\n",24);
	  exit(1);
	  );
     close(fromChild.1);
     close(toChild.0);
     if !input then close(fromChild.0);
     if !output then close(toChild.1);
     (file or errmsg)(addfile(file(nextHash(), filename, pid, 
	  listener, NOFD,NOFD,0,
	  input, if input then fromChild.0 else NOFD, false, if input then newbuffer() else "", 
	  0, 0, false, noprompt, true, false,
	  output, if output then toChild.1 else NOFD, false, if output then newbuffer() else "",
	  0, 0, false, dummyNetList))));
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
     	  then (file or errmsg)(errmsg("can't open input file "+filename+ " : "+syserrmsg()))
     	  else (file or errmsg)(addfile(file(nextHash(), filename, 0, 
		    false, NOFD,NOFD,0,
		    true,  fd, 0 != isatty(fd), newbuffer(), 0, 0, false, noprompt,true,false,
		    false, NOFD, false,           "",          0, 0, false, dummyNetList)))));
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
     	  then (file or errmsg)(errmsg("can't open output file "+filename+" : "+syserrmsg()))
     	  else (file or errmsg)(addfile(file(nextHash(), filename, 0, 
		    false, NOFD,NOFD,0,
		    false, NOFD, false,           "",          0, 0, false,noprompt,true,false,
		    true,  fd, 0 != isatty(fd), newbuffer(), 0, 0, false,dummyNetList)))));
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
     );

simpleflush(o:file):int := (
     r := if o.outindex > 0 then write(o.outfd,o.outbuffer,o.outindex) else 0;
     o.outindex = 0;
     o.outbol = 0;
     r);
simpleout(o:file,c:char):int := (
     r := if o.outindex == length(o.outbuffer) then simpleflush(o) else 0;
     o.outbuffer.(o.outindex) = c;
     o.outindex = o.outindex + 1;
     r);
simpleout(o:file,x:string):int := (
     foreach c in x do (
	  if ERROR == simpleout(o,c) then return(ERROR);
	  );
     0);
flushnets(o:file):int := (
     if o.hadNet then (
	  i := 0;
	  p := o.nets;
	  while p.next != p do (
	       i = i+1;
	       p = p.next;
	       );
	  s := new array(Net) len i do (
	       p = o.nets;
	       while true do (
		    provide p.this;
		    p = p.next;
		    );
	       );
	  for j from 0 to (i-1)/2 do (
	       k := i-1-j;
	       t := s.j;
	       s.j = s.k;
	       s.k = t;
	       );
	  n := HorizontalJoin(s);
	  o.hadNet = false;
	  o.nets = dummyNetList;
	  lastone := length(n.body)-1;
	  foreach s at i in n.body do (
	       if ERROR == simpleout(o,s) then return(ERROR);
	       if i != lastone then (
		    if ERROR == simpleout(o,newline) then return(ERROR);
		    );
	       ); 
	  );
     0);
export flush(o:file):int := (
     if o.hadNet then if ERROR == flushnets(o) then return(ERROR);
     simpleflush(o));
cleanUp(o:file):void := (
     if !o.listener && !o.input && !o.output then (
	  rmfile(o);
	  );
     );
export closeListener(o:file):int := (
     if o.listenerfd == NOFD then return(ERROR);
     haderror := close(o.listenerfd) == ERROR;
     o.listenerfd = NOFD;
     o.listener = false;
     cleanUp(o); 
     if haderror then ERROR else 0);
export closeIn(o:file):int := (
     if o.infd == NOFD then return(ERROR);
     if o == stdin && o.inisatty then return(ERROR);
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
     if o.outfd == NOFD then return(ERROR);
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
export filbuf(o:file):bool := (
     -- returns true if it managed to get some more characters
     if ! o.input then return(false);
     if o.inindex > 0
     then (
	  o.insize = o.insize - o.inindex;
	  for i from 0 to o.insize - 1 do o.inbuffer.i = o.inbuffer.(i+o.inindex);
	  o.inindex = 0;
	  );
     n := length(o.inbuffer)-o.insize;
     if n == 0 then return(false);
     r := read(o.infd,o.inbuffer,n,o.insize);
     if r > 0 then ( o.insize = o.insize + r; true)
     else if r == 0 then ( o.eof = true; false )
     else false
     );
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
export (o:file) << (b:bool) : file := (
     o << if b then "true" else "false");
octal(c:char):string := (
     i := int(c) & 0xff;
     if i < 8
     then new string len 1 do provide ('0'+i)
     else if i < 64 then new string len 2 do (provide('0'+i/8); provide('0'+i%8))
     else new string len 3 do (provide('0'+i/64); provide('0'+(i/8)%8); provide('0'+i%8))
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
     needsfixing := false;
     foreach cc in x do (
	  c := cc;
	  if c < char(32) || c == '\"' || c == char(127) || c == '\\'
	  then (
	       needsfixing = true;
	       break;
	       );
	  );
     if needsfixing then (
	  v := newvarstring(12);
	  foreach cc in x do (
	       c := cc;
	       -- warning : c is a signed char
	       if c == '\r' then v << "\\r" 
	       else if c == '\f' then v << "\\f" 
	       else if c == '\t' then v << "\\t" 
	       else if c == '\n' then v << "\\n" 
	       else if c == '\"' then v << "\\\"" 
	       else if c == '\\' then v << "\\\\" 
	       else if c == char(127) then v << "\\177"
	       else if c < char(32) then (
		    v << '\\';
		    i := int(c) & 0xff;
		    if i < 8 then v << '0'+i
		    else if i < 64 then v << '0'+i/8 << '0'+i%8
		    else v << '0'+i/64 << '0'+(i/8)%8 << '0'+i%8
		    )
	       else v << c
	       );
	  tostring(v))
     else x);
export tostring(i:int):string := (
     if i==0 then return("0");
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
export tostring(x:double) : string := (
     o := newvarstring(25);
     if isinf(x) then return("infinity");
     if isnan(x) then return("NotANumber");
     if x==0. then return("0.");
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
     s := 6;				  -- number of significant digits
     l := 5;				  -- max number leading zeroes
     t := 5;				  -- max number extra trailing digits
     j := 0;				  -- power of 10 afterwards
     if i<0 then (
	  if -i <= l 
	  then digits(o,oldx,1,s-i-1)
	  else (digits(o,x,1,s-1); o << " 10^" << tostring(i);))
     else if i+1 > s then (
	  if i+1-s <= t
	  then digits(o,x,i+1,0)
	  else (digits(o,x,1,s-1); o << " 10^" << tostring(i);))
     else digits(o,x,i+1,s-i-1);
     tostring(o));
export (o:file) << (x:double) : file := o << tostring(x);
endlfun(o:file):int := (
     if o.output then (
	  if o.hadNet then if ERROR == flushnets(o) then return(ERROR);
	  o << newline;
	  if o.outisatty || o == stderr 
	  then (
	       if ERROR == simpleflush(o) then return(ERROR);
	       )
	  else (
	       o.outbol = o.outindex;
	       );
	  );
     0);

nl := if length(newline) > 0 then newline.(length(newline)-1) else '\n';

prepare(o:file):void := (
     o.bol = false;
     o.prompt(stdout);
     flush(stdout);
     if o.echo then (
	  filbuf(o);
	  i := o.inindex;
	  c := ' ';
	  while i < o.insize && (c = o.inbuffer.i; c != '\n' && c != '\r') do (stdout << c; i = i+1);
	  endlfun(stdout);
	  flush(stdout)));
export getc(o:file):int := (
     if !o.input then return(EOF);
     if o.bol then prepare(o);
     if o.inindex == o.insize && !filbuf(o) then return(EOF); -- but it might have been interrupted, too
     c := o.inbuffer.(o.inindex);
     o.inindex = o.inindex + 1;
     o.bol = c == nl;
     int(uchar(c)));
export read(o:file):string := (
     if o.inindex == o.insize then filbuf(o);
     s := substr(o.inbuffer,o.inindex,o.insize);
     o.insize = 0;
     o.inindex = 0;
     s);
export peek(o:file,offset:int):int := (
     if !o.input then return(EOF);
     if offset >= bufsize then return(EOF);		    -- tried to peek too far
     if o.inindex+offset >= o.insize then (
	  if o.eof then return(EOF);
	  if o.bol then prepare(o);
     	  while (
	       filbuf(o);
	       o.inindex+offset >= o.insize
	       )
	  do (
	       if o.eof then return(EOF);
	       );
	  );
     int(uchar(o.inbuffer.(o.inindex+offset))));
export peek(o:file):int := peek(o,0);
export blanks(n:int):string := new string len n do provide(' ');
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
export setprompt(o:file,prompt:function(file):void):void := ( o.prompt = prompt; );
export clean(o:file):void := flush(o);

import readfile(fd:int):string;
export get(filename:string):(string or errmsg) := (
     when openIn(filename)
     is x:errmsg do (string or errmsg)(x)
     is f:file do (
	  s := readfile(f.infd);
	  r := close(f);
	  if r == ERROR
	  then (string or errmsg)(errmsg("failed to close file : "+syserrmsg()))
	  else if r != 0 && length(filename) > 0 && filename . 0 == '!'
	  then (string or errmsg)(errmsg("process exit code " + tostring(r)))
	  else (string or errmsg)(s)));

export Manipulator := {fun:function(file):int};
export (o:file) << (m:Manipulator) : file := (
     m.fun(o);				  -- ignoring error here
     o
     );
export endl := Manipulator(endlfun);

element(s:Cstring, i:int):char ::= Ccode( char, "(((char *)", s, ")[", i, "])" );
export (o:file) << (s:Cstring) : file := (
     i := 0;
     while element(s,i) != char(0) do (
	  o << element(s,i);
	  i = i+1;
	  );
     o
     );
