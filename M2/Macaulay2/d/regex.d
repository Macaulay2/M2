use common;
use util;

header "#include <engine.h>";

import noErrorMessage:string;
import regexmatchErrorMessage:string;
import regexmatch(pattern:string, start:int, range:int, text:string, ignorecase:bool):array(int);
import regexreplace(pattern:string, replacement: string, text:string, errflag:string, ignorecase:bool):string;
import regexselect(pattern:string, replacement: string, text:string, errflag:array(string), ignorecase:bool):array(string);

toPairs(r:array(int)):Expr := Expr(
     list (
	  new Sequence len length(r)/2 at i do
	  provide new Sequence len 2 at j do
	  provide toExpr(r.(2*i+j))
	  )
     );

regexmatch(e:Expr):Expr := (
     ignorecase := false;
     when e is a:Sequence do
     if length(a) == 2 then
     when a.0 is regexp:stringCell do
     when a.1 is text:stringCell do (
	  r := regexmatch(regexp.v,0,length(text.v),text.v,ignorecase);
	  if regexmatchErrorMessage != noErrorMessage then buildErrorPacket("regex: "+regexmatchErrorMessage)
     	  else if length(r) != 0 then toPairs(r)
	  else nullE)
     else WrongArgString(2)
     else WrongArgString(1)
     else if length(a) == 3 then
     when a.0 is regexp:stringCell do
     when a.1 is start:ZZcell do if !isInt(start) then WrongArgSmallInteger(2) else
     when a.2 is text:stringCell do (
	  istart := toInt(start);
	  r := regexmatch(regexp.v,istart,length(text.v)-istart,text.v,ignorecase);
	  if length(r) != 0 then toPairs(r)
	  else if regexmatchErrorMessage == noErrorMessage
	  then nullE
	  else buildErrorPacket("regex: "+regexmatchErrorMessage))
     else WrongArgString(3)
     else WrongArgZZ(2)
     else WrongArgString(1)
     else if length(a) == 4 then
     when a.0 is regexp:stringCell do
     when a.1 is start:ZZcell do if !isInt(start) then WrongArgSmallInteger(2) else
     when a.2 is range:ZZcell do if !isInt(range) then WrongArgSmallInteger(3) else
     when a.3 is text:stringCell do (
	  r := regexmatch(regexp.v,toInt(start),toInt(range),text.v,ignorecase);
	  if length(r) != 0 then toPairs(r)
	  else if regexmatchErrorMessage == noErrorMessage
	  then nullE
	  else buildErrorPacket("regex: "+regexmatchErrorMessage))
     else WrongArgString(4)
     else WrongArgZZ(3)
     else WrongArgZZ(2)
     else WrongArgString(1)
     else WrongNumArgs(2,3)
     else WrongNumArgs(2,3));
setupfun("regex",regexmatch);

foo := "foo";
replace(e:Expr):Expr := (
     ignorecase := false;
     when e is a:Sequence do
     if length(a) == 3 then
     when a.0 is regexp:stringCell do
     when a.1 is replacement:stringCell do
     when a.2 is text:stringCell do (
	  r := regexreplace(regexp.v,replacement.v,text.v,foo,ignorecase);
	  if r == foo then buildErrorPacket("replace: "+regexmatchErrorMessage)
	  else toExpr(r))
     else WrongArgString(3)
     else WrongArgString(2)
     else WrongArgString(1)
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("replaceStrings",replace);



rawRegexSearcher(e:Expr):Expr := (
    ignorecase := false;
    when e is s:Sequence do
    if length(s) == 2 then (
	when s.0 is regexp:stringCell do
	when s.1 is text:stringCell do (
            r := Ccode(arrayint,
                "rawRegexSearch(",
                regexp.v, ",",
                0, ",",
                length(text.v), ",",
                text.v, ",",
                ignorecase,
		")");
            if length(r) != 0 then toPairs(r)
            else nullE)
        else WrongArgString(2)
	else WrongArgString(1))
    else if length(s) == 3 then (
	when s.0 is regexp:stringCell do
	when s.1 is start:ZZcell do if !isInt(start) then WrongArgSmallInteger(2) else
	when s.2 is text:stringCell do (
            istart := toInt(start);
            r := regexmatch(regexp.v,istart,length(text.v)-istart,text.v,ignorecase);
            if length(r) != 0 then toPairs(r)
            else if regexmatchErrorMessage == noErrorMessage
            then nullE
            else buildErrorPacket("regex: "+regexmatchErrorMessage))
	else WrongArgString(3)
	else WrongArgZZ(2)
	else WrongArgString(1))
    else if length(s) == 4 then (
	when s.0 is regexp:stringCell do
	when s.1 is start:ZZcell do if !isInt(start) then WrongArgSmallInteger(2) else
	when s.2 is range:ZZcell do if !isInt(range) then WrongArgSmallInteger(3) else
	when s.3 is text:stringCell do (
            r := regexmatch(regexp.v,toInt(start),toInt(range),text.v,ignorecase);
            if length(r) != 0 then toPairs(r)
            else if regexmatchErrorMessage == noErrorMessage
            then nullE
            else buildErrorPacket("regex: "+regexmatchErrorMessage))
	else WrongArgString(4)
	else WrongArgZZ(3)
	else WrongArgZZ(2)
	else WrongArgString(1))
    else WrongNumArgs(2,4)
    else WrongNumArgs(2,4));
setupfun("rawRegexSearch", rawRegexSearcher);
