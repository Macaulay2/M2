--		Copyright 1995 by Daniel R. Grayson

use system; 
use convertr;
use binding;
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
use C;
use actors;
use actors2;
use struct;
use objects;
use GB;

import OpenLink(args:array(string)):int;
import CloseLink(link:int):int;
import EndMsgReset(link:int):int;
import PutSint32(link:int,n:int):int;
import RawPutSint32(link:int,n:int):int;
import PutString(link:int,s:string):int;
import RawPutString(link:int,s:string):int;
import PutIdentifier(link:int,s:string):int;
import RawPutIdentifier(link:int,s:string):int;
import PutListOperator(link:int,leng:int):int;
import PutCommonOperator(link:int,dict:int,oper:int,numannot:int,leng:int):int;
import PutOperatorPacket(link:int,dict:int,oper:string,numannot:int,leng:int):int;
import PutAnnotationPacket(link:int,dict:int,atype:int,aflags:int):int;
import PutCommonMetaOperatorPacket(link:int,dict:int,oper:int,numannot:int,numchildren:int):int;
import PutCommonMetaTypePacket(link:int,dict:int,oper:int,numannot:int):int;

openlink1(a:Sequence):Expr := (
     newargv := new array(string) len length(a) do provide "";
     foreach x at i in a do (
	  when x
	  is s:string do newargv.i = s
	  else return(WrongArg(i+1,"a string")));
     Expr(toInteger(OpenLink(newargv))));

openlink(e:Expr):Expr := (
     when e
     is w:List do openlink1(w.v)
     is w:Sequence do openlink1(w)
     is s:string do openlink1(Sequence(s))
     else WrongArg( "a string or a sequence or list of strings"));

setupfun("openLink",openlink);

WritePacketS := makeProtectedSymbolClosure("WritePacket");
WritePacketE := Expr(WritePacketS);

writePacket(h:int,x:Expr):Expr := (
     when x
     is s:SymbolClosure do Expr(toInteger(PutIdentifier(h,s.symbol.word.name)))
     is s:string do Expr(toInteger(PutString(h,s)))
     is z:List do (
	  method := lookupBinaryMethod(integerClass,z.class,WritePacketS);
	  if method != nullE
	  then apply(method,Expr(toInteger(h)),x)
	  else (
	       PutListOperator(h,length(z.v));
	       foreach i in z.v do (
		    r := writePacket(h,i);
		    when r is Error do return(r) else nothing;
		    r);
	       Expr(toInteger(0))))
     is j:Integer do (
	  if isInt(j)
	  then Expr(toInteger(PutSint32(h,toInt(j))))
	  else WrongArg(2,"a small integer"))
     else (
	  method := lookupBinaryMethod(integerClass,Class(x),WritePacketS);
     	  if method != nullE 
     	  then apply(method,Expr(toInteger(h)),x)
     	  else errorExpr("no method found for WritePacket")
	  ));

writePacket(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then
	  when a.0
	  is link:Integer do (
	       if isInt(link) 
	       then writePacket(toInt(link),a.1)
	       else WrongArg(1,"a small integer"))
     	  else WrongArg(1,"an integer")
	  else WrongNumArgs(2)
	  )
     else WrongNumArgs(2));
setupfun("writePacket",writePacket);

writeRawPacket(h:int,x:Expr):Expr := (
     when x
     is s:SymbolClosure do Expr(toInteger(RawPutIdentifier(h,s.symbol.word.name)))
     is s:string do Expr(toInteger(RawPutString(h,s)))
     is z:Sequence do (
	  foreach i in z do (
	       r := writeRawPacket(h,i);
	       when r is Error do return(r) else nothing;
	       r);
	  Expr(toInteger(0)))
     is j:Integer do (
	  if isInt(j)
	  then Expr(toInteger(RawPutSint32(h,toInt(j))))
	  else WrongArg(2,"a small integer"))
     else WrongArg(2,"a small integer"));

writeRawPacket(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then
	  when a.0
	  is link:Integer do (
	       if isInt(link) 
	       then writeRawPacket(toInt(link),a.1)
	       else WrongArg(1,"a small integer"))
     	  else WrongArg(1,"an integer")
	  else WrongNumArgs(2)
	  )
     else WrongNumArgs(2));
setupfun("writeRawPacket",writeRawPacket);

writeMessage(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then
	  when a.0
	  is i:Integer do (
	       if isInt(i) 
	       then (
		    h := toInt(i);
		    r := writePacket(h,a.1);
		    EndMsgReset(h);
		    r)
	       else WrongArg(0+1,"a small integer"))
     	  else WrongArg(0+1,"an integer")
	  else WrongNumArgs(2)
	  )
     else WrongNumArgs(2));
setupfun("writeMessage",writeMessage);

closelink(e:Expr):Expr := (
     when e
     is link:Integer do (
	  if isInt(link) 
	  then Expr(toInteger(CloseLink(toInt(link))))
	  else WrongArg(0+1,"a small integer"))
     else WrongArg(0+1,"an integer"));
setupfun("closeLink",closelink);	       

putCommonOperatorPacket(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a) == 5 then
     when a.0 is link:Integer do
     if isInt(link) then
     when a.1 is dict:Integer do
     if isInt(dict) then
     when a.2 is oper:Integer do
     if isInt(oper) then
     when a.3 is numannot:Integer do
     if isInt(numannot) then
     when a.4 is leng:Integer do 
     if isInt(leng) then Expr(toInteger(PutCommonOperator(
		    toInt(link),
		    toInt(dict),
		    toInt(oper),
		    toInt(numannot),
		    toInt(leng))))
     else WrongArg(5,"a small integer")
     else WrongArg(5,"an integer")
     else WrongArg(4,"a small integer")
     else WrongArg(4,"an integer")
     else WrongArg(3,"a small integer")
     else WrongArg(3,"an integer")
     else WrongArg(2,"a small integer")
     else WrongArg(2,"an integer")
     else WrongArg(1,"a small integer")
     else WrongArg(1,"an integer")
     else WrongNumArgs(4)
     else WrongNumArgs(4));
setupfun("PutCommonOperatorPacket",putCommonOperatorPacket);

putAnnotationPacket(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a) == 4 then
     when a.0 is link:Integer do
     if isInt(link) then
     when a.1 is dict:Integer do
     if isInt(dict) then
     when a.2 is atype:Integer do
     if isInt(atype) then 
     when a.3 is annotflags:Integer do
     if isInt(annotflags) then
     Expr(toInteger(PutAnnotationPacket(
		    toInt(link),
		    toInt(dict),
		    toInt(atype),
		    toInt(annotflags) -- MPAnnotRequired, MPAnnotValuated, or MPAnnotTreeScope
		    )))
     else WrongArg(4,"a small integer")
     else WrongArg(4,"an integer")
     else WrongArg(3,"a small integer")
     else WrongArg(3,"an integer")
     else WrongArg(2,"a small integer")
     else WrongArg(2,"an integer")
     else WrongArg(1,"a small integer")
     else WrongArg(1,"an integer")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("PutAnnotationPacket",putAnnotationPacket);

putCommonMetaOperatorPacket(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a) == 5 then
     when a.0 is link:Integer do
     if isInt(link) then
     when a.1 is dict:Integer do
     if isInt(dict) then
     when a.2 is oper:Integer do
     if isInt(oper) then
     when a.3 is numannot:Integer do 
     if isInt(numannot) then
     when a.4 is numchildren:Integer do 
     if isInt(numchildren) then
     Expr(toInteger(PutCommonMetaOperatorPacket(
		    toInt(link),
		    toInt(dict),
		    toInt(oper),
		    toInt(numannot),
		    toInt(numchildren))))
     else WrongArg(5,"a small integer")
     else WrongArg(5,"an integer")
     else WrongArg(4,"a small integer")
     else WrongArg(4,"an integer")
     else WrongArg(3,"a small integer")
     else WrongArg(3,"an integer")
     else WrongArg(2,"a small integer")
     else WrongArg(2,"an integer")
     else WrongArg(1,"a small integer")
     else WrongArg(1,"an integer")
     else WrongNumArgs(4)
     else WrongNumArgs(4));
setupfun("PutCommonMetaOperatorPacket",putCommonMetaOperatorPacket);

putCommonMetaTypePacket(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a) == 5 then
     when a.0 is link:Integer do
     if isInt(link) then
     when a.1 is dict:Integer do
     if isInt(dict) then
     when a.2 is oper:Integer do
     if isInt(oper) then
     when a.3 is numannot:Integer do 
     if isInt(numannot) then
     when a.4 is numchildren:Integer do 
     if isInt(numchildren) then
     Expr(toInteger(PutCommonMetaTypePacket(
		    toInt(link),
		    toInt(dict),
		    toInt(oper),
		    toInt(numannot))))
     else WrongArg(5,"a small integer")
     else WrongArg(5,"an integer")
     else WrongArg(4,"a small integer")
     else WrongArg(4,"an integer")
     else WrongArg(3,"a small integer")
     else WrongArg(3,"an integer")
     else WrongArg(2,"a small integer")
     else WrongArg(2,"an integer")
     else WrongArg(1,"a small integer")
     else WrongArg(1,"an integer")
     else WrongNumArgs(4)
     else WrongNumArgs(4));
setupfun("PutCommonMetaTypePacket",putCommonMetaTypePacket);

putOperatorPacket(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a) == 5 then
     when a.0 is link:Integer do
     if isInt(link) then
     when a.1 is dict:Integer do
     if isInt(dict) then
     when a.2 is oper:string do
     when a.3 is numannot:Integer do
     if isInt(numannot) then
     when a.4 is leng:Integer do 
     if isInt(leng) then Expr(toInteger(PutOperatorPacket(
		    toInt(link),
		    toInt(dict),
		    oper,
		    toInt(numannot),
		    toInt(leng))))
     else WrongArg(5,"a small integer")
     else WrongArg(5,"an integer")
     else WrongArg(4,"a small integer")
     else WrongArg(4,"an integer")
     else WrongArg(3,"a string")
     else WrongArg(2,"a small integer")
     else WrongArg(2,"an integer")
     else WrongArg(1,"a small integer")
     else WrongArg(1,"an integer")
     else WrongNumArgs(4)
     else WrongNumArgs(4));
setupfun("PutOperatorPacket",putOperatorPacket);
