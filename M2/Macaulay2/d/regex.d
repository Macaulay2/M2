use common;
use util;

header "#include <engine.h>";

import noErrorMessage:string;
import regexmatchErrorMessage:string;
import regexreplace(pattern:string, replacement: string, text:string, errflag:string, ignorecase:bool):string;
import regexselect(pattern:string, replacement: string, text:string, errflag:array(string), ignorecase:bool):array(string);

toPairs(r:array(int)):Expr := Expr(
     list (
	  new Sequence len length(r)/2 at i do
	  provide new Sequence len 2 at j do
	  provide toExpr(r.(2*i+j))
	  )
     );

rawRegex(e:Expr):Expr := (
    when e is s:Sequence do
    if length(s) == 2 then (
	when s.0 is regexp:stringCell do
	when s.1 is text:stringCell do (
            r := Ccode(arrayint,
                "rawRegexSearch(", regexp.v, ",", 0, ",", length(text.v), ",", text.v, ",", 0, ")");
            if length(r) != 0 then toPairs(r)
            else nullE)
        else WrongArgString(2)
	else WrongArgString(1))
    else if length(s) == 3 then (
	when s.0 is regexp:stringCell do
	when s.1 is text:stringCell do
	when s.2 is flags:ZZcell do if !isInt(flags) then WrongArgSmallInteger(3) else (
            r := Ccode(arrayint,
                "rawRegexSearch(", regexp.v, ",", 0, ",", length(text.v), ",", text.v, ",", toInt(flags), ")");
            if length(r) != 0 then toPairs(r)
            else nullE)
	else WrongArgZZ(3)
        else WrongArgString(2)
	else WrongArgString(1))
    else if length(s) == 4 then (
	when s.0 is regexp:stringCell do
	when s.1 is start:ZZcell do if !isInt(start) then WrongArgSmallInteger(2) else
	when s.2 is text:stringCell do
	when s.3 is flags:ZZcell do if !isInt(flags) then WrongArgSmallInteger(4) else (
            istart := toInt(start);
            r := Ccode(arrayint,
                "rawRegexSearch(", regexp.v, ",", istart, ",", length(text.v)-istart, ",", text.v, ",", toInt(flags), ")");
            if length(r) != 0 then toPairs(r)
            else nullE)
	else WrongArgZZ(4)
	else WrongArgString(3)
	else WrongArgZZ(2)
	else WrongArgString(1))
    else if length(s) == 5 then (
	when s.0 is regexp:stringCell do
	when s.1 is start:ZZcell do if !isInt(start) then WrongArgSmallInteger(2) else
	when s.2 is range:ZZcell do if !isInt(range) then WrongArgSmallInteger(3) else
	when s.3 is text:stringCell do
	when s.4 is flags:ZZcell do if !isInt(flags) then WrongArgSmallInteger(5) else (
	    istart := toInt(start);
	    irange := toInt(range);
            r := Ccode(arrayint,
                "rawRegexSearch(", regexp.v, ",", istart, ",", irange, ",", text.v, ",", toInt(flags), ")");
            if length(r) != 0 then toPairs(r)
            else nullE)
	else WrongArgZZ(5)
	else WrongArgString(4)
	else WrongArgZZ(3)
	else WrongArgZZ(2)
	else WrongArgString(1))
    else WrongNumArgs(2,5)
    else WrongNumArgs(2,5));
setupfun("regex", rawRegex).Protected = false; -- will be overloaded in m2/regex.m2

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
