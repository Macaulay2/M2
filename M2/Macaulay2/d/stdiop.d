--		Copyright 1994 by Daniel R. Grayson
-- a flavor of input stream where line numbers and
-- column numbers are maintained properly
-- Also, if a \ comes at the end of the line, then it swallows the next
-- line up to a nonspace

use system;
use strings;
use stdio;
use ctype;

export Position := {filename:string, line:ushort, column:uchar, reloaded:uchar};
export dummyPosition := Position("--dummy file name--",ushort(0),uchar(0),uchar(reloaded));
export minimizeFilename(filename:string):string := (
     ofilename := filename;
     s := getcwd();
     if !(s === "/") then s = s + "/";
     i := 0;
     while i < length(s) && i < length(filename) && s.i == filename.i do i = i+1;
     while i > 0 && (s.(i-1) != '/' || filename.(i-1) != '/') do i = i-1;
     filename = substr(filename,i);
     while i < length(s) do (if s.i == '/' then filename = "../" + filename; i = i+1);
     if length(ofilename) <= length(filename) then ofilename else filename
     );

export tostring(w:Position) : string := (
	 errfmt(minimizeFilename(w.filename),int(w.line),int(w.column + 1))
     );
export (o:file) << (w:Position) : file := (
     o << tostring(w)
     );
export SuppressErrors := false;
export errorpos(position:Position,message:string):void := (
     if !SuppressErrors then (
     	  flush(stdout);
     	  stderr << position << ' ' << message << endl;
	  );
     );
export (o:file) << (p:(null or Position)):file := (
     when p is null do o is w:Position do o << w
     );
export copy(p:Position):Position := Position(
     p.filename, p.line, p.column, uchar(reloaded));
export PosFile := {file:file, pos:Position};
export makePosFile(o:file):PosFile := PosFile(o, 
     Position(o.filename, ushort(1), uchar(0), uchar(reloaded)));
export peek(o:PosFile, offset:int):int := peek(o.file,offset);
export peek(o:PosFile):int := peek(o.file);
export isatty(o:PosFile):bool := o.file.isatty;
export close(o:PosFile):void := close(o.file);
export setprompt(o:PosFile,prompt:function():void):void := (
     setprompt(o.file,prompt)
     );
export fopeninp(filename:string):(PosFile or errmsg) := (
     when fopenin(filename)
     is f:file do (PosFile or errmsg)(
	  PosFile(f,Position(
		    if f.filename.0 == '/'
		    then f.filename
		    else getcwd() + '/' + f.filename,
		    ushort(1),uchar(0),uchar(reloaded))))
     is s:errmsg do (PosFile or errmsg)(s)
     );
roundup(n:uchar,d:int):uchar := uchar(((int(n)+d-1)/d)*d);
tabwidth := 8;
export getc(o:PosFile):int := (
     c := getc(o.file);
     if c != EOF then (
 	  if isnewline(c) then (
	       o.pos.line = o.pos.line + 1;
	       o.pos.column = uchar(0);
	       )
	  else if c == int('\t')
	  then o.pos.column = roundup(o.pos.column+1,tabwidth)
	  else if c == int('\\')
	  then (
	       o.pos.column = o.pos.column + 1;
	       if isnewline(peek(o))
	       then (
		    getc(o);
		    while iswhite(peek(o)) do c = getc(o);
		    )
	       )
	  else (
	       o.pos.column = o.pos.column + (
		    if c < int(' ') || c == 127
		    then 2
		    else 1);
	       )
	  );
     c
     );
export flush(o:PosFile):void := flushinput(o.file);
