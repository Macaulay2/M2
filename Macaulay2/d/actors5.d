--		Copyright 1995 by Daniel R. Grayson


use system; 
use convertr;
use binding;
use nets;
use parser;
use lex;
use arith;
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
use basic;
use struct;
use objects;
use GB;
use actors4;

installMethod(s:SymbolClosure,X:HashTable,Y:HashTable,f:fun):Expr := (
     installMethod(Expr(s),X,Y,Expr(CompiledFunction(f,nextHash())))
     );

cvtlen := 0;
cvtstr := "";
cvtpos := 0;
getlength():int := (
     c := int(uchar(cvtstr.cvtpos));
     cvtpos = cvtpos + 1;
     if (c & 0x80) == 0
     then c
     else (
     	  ln := c & 0x7f;
     	  if cvtpos >= cvtlen then return(ln); -- actually an error
	  c = int(uchar(cvtstr.cvtpos));
	  cvtpos = cvtpos + 1;
	  if (c & 0x80) == 0
	  then (ln << 7) + c
	  else (
	       ln = (ln << 7) + (c & 0x7f);
     	       if cvtpos >= cvtlen then return(ln); -- actually an error
	       c = int(uchar(cvtstr.cvtpos));
	       cvtpos = cvtpos + 1;
	       if (c & 0x80) == 0
	       then (ln << 7) + c
	       else (
		    ln = (ln << 7) + (c & 0x7f);
     	       	    if cvtpos >= cvtlen then return(ln); -- actually an error
		    c = int(uchar(cvtstr.cvtpos));
		    cvtpos = cvtpos + 1;
		    (ln << 7) + (0x7f & c)))));
getlength6():int := (
     c := int(uchar(cvtstr.cvtpos));
     cvtpos = cvtpos + 1;
     if (c & 0x80) == 0
     then (c & 0x3f)
     else (
     	  ln := c & 0x3f;
     	  if cvtpos >= cvtlen then return(ln); -- actually an error
	  c = int(uchar(cvtstr.cvtpos));
	  cvtpos = cvtpos + 1;
	  if (c & 0x80) == 0
	  then (ln << 7) + c
	  else (
	       ln = (ln << 7) + (c & 0x7f);
     	       if cvtpos >= cvtlen then return(ln); -- actually an error
	       c = int(uchar(cvtstr.cvtpos));
	       cvtpos = cvtpos + 1;
	       if (c & 0x80) == 0
	       then (ln << 7) + c
	       else (
		    ln = (ln << 7) + (c & 0x7f);
     	       	    if cvtpos >= cvtlen then return(ln); -- actually an error
		    c = int(uchar(cvtstr.cvtpos));
		    cvtpos = cvtpos + 1;
		    (ln << 7) + (0x7f & c)))));
getmantissa():int := (
     c := int(uchar(cvtstr.cvtpos));
     cvtpos = cvtpos + 1;
     if (c & 0x80) == 0
     then (c & 0x3f)
     else (
     	  ln := c & 0x3f;
     	  if cvtpos >= cvtlen then return(ln); -- actually an error
	  c = int(uchar(cvtstr.cvtpos));
	  cvtpos = cvtpos + 1;
	  if (c & 0x80) == 0
	  then (ln << 7) + c
	  else (
	       ln = (ln << 7) + (c & 0x7f);
     	       if cvtpos >= cvtlen then return(ln); -- actually an error
	       c = int(uchar(cvtstr.cvtpos));
	       cvtpos = cvtpos + 1;
	       if (c & 0x80) == 0
	       then (ln << 7) + c
	       else (
		    ln = (ln << 7) + (c & 0x7f);
     	       	    if cvtpos >= cvtlen then return(ln); -- actually an error
		    c = int(uchar(cvtstr.cvtpos));
		    cvtpos = cvtpos + 1;
		    (ln << 7) + (0x7f & c)))));
getinteger():Expr := (
     if cvtpos + 1 > cvtlen 
     then return(errorExpr("encountered end of string prematurely"));
     sgn := 0 != (0x40 & int(uchar(cvtstr.cvtpos)));
     oldpos := cvtpos;
     i := getlength6();
     x := toInteger(i);
     if (
	  ( cvtpos == oldpos + 4 && 0 == (int(cvtstr.(cvtpos-1)) & 0x80) )
	  ||
	  cvtpos < oldpos + 4
	  )
     then if sgn then -Expr(x) else Expr(x)
     else (
	  ln := getlength();
	  if cvtpos + ln > cvtlen 
	  then return(errorExpr("encountered end of string prematurely"));
	  while ln > 0 do (
	       c := 0xff & int(cvtstr.cvtpos);
	       x = (x << 8) + c;
	       cvtpos = cvtpos + 1;
	       ln = ln - 1;
	       );
	  if sgn then -Expr(x) else Expr(x)));
ConvertInteger := makeProtectedSymbolClosure("ConvertInteger");
convert(format:Expr):Expr := (
     when format
     is a:Sequence do (
	  l := length(a);
	  if l==0 then return(errorExpr("encountered a sequence of length 0"));
	  f := a.0;
	  if l==1 then (
	       if cvtpos >= cvtlen
	       then errorExpr("encountered end of string prematurely")
	       else (
		    haderror := false;
		    ln := getlength();
		    r := nullE;
		    t := new Sequence len ln do (
			 r = convert(f);
			 when r is Error do (
			      haderror = true;
			      while true do provide nullE;
			      )
			 else provide r;
			 );
		    if haderror then r else Expr(t)))
	  else when f 
	  is j:Integer do (
	       if !isInt(j)
	       then errorExpr("expected repetition count to be a small integer")
	       else (
		    ln := toInt(j);
		    if ln >= 0
		    then (
			 -- ln >= 0 means ln is the exact rep ct
			 r := nullE;
			 haderror := false;
			 ln = ln * (l-1);
			 if cvtpos + ln > cvtlen
			 then return(errorExpr("encountered end of string prematurely"));
			 t := new Sequence len ln do (
			      k := 1;
			      while k < l do (
				   r = convert(a.k);
				   k = k+1;
				   when r is Error do (
					haderror = true;
					while true do provide nullE;
					)
				   else provide r;
				   ));
			 if haderror then r else Expr(t))
		    else errorExpr("expected repetition count nonnegative")))
	  is c:FunctionClosure do (
	       haderror := false;
	       if l == 2
	       then (
		    r := convert(a.1);
		    when r 
		    is Error do r
		    is v:Sequence do apply(c,v)
		    else apply(c,r)
		    )
	       else (
	       	    r := nullE;
	       	    k := 1;
		    t := new Sequence len l-1 do (
			 r = convert(a.k);
			 when r is Error do (
			      haderror = true;
			      while true do provide nullE;
			      )
			 else provide r;
			 k = k+1;
			 );
		    if haderror then r else apply(c,t)
		    )
	       )
	  is c:CompiledFunctionClosure do (
	       haderror := false;
	       if l == 2
	       then (
		    r := convert(a.1);
		    when r is Error do r else c.fn(r,c.env)
		    )
	       else (
	       	    r := nullE;
		    k := 1;
		    t := new Sequence len l-1 do (
			 r = convert(a.k);
			 when r is Error do (
			      haderror = true;
			      while true do provide nullE;
			      )
			 else provide r;
			 k = k+1;
			 );
		    if haderror then r else c.fn(Expr(t),c.env)
		    )
	       )
	  is c:CompiledFunction do (
	       haderror := false;
	       if l == 2
	       then (
		    r := convert(a.1);
		    when r is Error do r else c.fn(r)
		    )
	       else (
	       	    r := nullE;
		    k := 1;
		    t := new Sequence len l-1 do (
			 r = convert(a.k);
			 when r is Error do (
			      haderror = true;
			      while true do provide nullE;
			      )
			 else provide r;
			 k = k+1;
			 );
		    if haderror then r else c.fn(Expr(t))
		    )
	       )
	  else errorExpr("encountered invalid format")
	  )
     is w:SymbolClosure do (
	  if w.symbol == ConvertInteger.symbol then getinteger()
	  else errorExpr("encountered a unrecognized format symbol")
	  )
     is c:FunctionClosure do apply(c,emptySequence)
     is f:CompiledFunction do f.fn(Expr(emptySequence))
     is f:CompiledFunctionClosure do f.fn(Expr(emptySequence),f.env)
     else errorExpr("expected a valid format item"));
convertfun(e:Expr):Expr := (
     when e
     is a:Sequence do
     if length(a) == 2 then (
	  fmt := a.0;
	  when a.1 
	  is str:string do (
	       savecvtpos := cvtpos;
	       savecvtstr := cvtstr;
	       savecvtlen := cvtlen;
	       cvtstr = str;
	       cvtlen = length(str);
	       cvtpos = 0;
	       r := convert(fmt);
	       when r
	       is Error do nothing
	       else (
		    if cvtlen != cvtpos
	       	    then r = errorExpr("did not exhaust its input")
		    );
	       cvtpos = savecvtpos;
	       cvtstr = savecvtstr;
	       cvtlen = savecvtlen;
	       r)
	  else WrongArg(1+1,"a string"))
     else WrongNumArgs(2)
     else WrongArg(0+1,"an integer"));
setupfun("convert",convertfun);
formatseq(v:Sequence):Expr := (
     foreach x in v do (
	  when x 
	  is Integer do nothing
	  else return(WrongArg(1,"a list or sequence of integers")));
     b := new Sequence len length(v)+1 do (
	  provide converttonet(toInteger(length(v)));
	  foreach x in v do (
	       when x
	       is j:Integer do provide converttonet(j)
	       else nothing	  -- won t occur
	       );
	  );
     Expr(stringcatseq(b)));
formatfun(e:Expr):Expr := (
     when e
     is i:Integer do Expr(converttonet(i))
     is h:Handle do Expr(converttonet(toInteger(h.handle)))
     is v:Sequence do formatseq(v)
     is l:List do formatseq(l.v)
     else WrongArg(0+1,"an integer, a list of integers, or a handle"));
setupfun("gg",formatfun);

-- getParsing(o:file):void := (
--      o
--      << endl << endl
--      << "word      precedence scope unaryStrength" << endl << endl
--      << ("<WORDS>",12)
--      << (parseWORD.precedence,-7)
--      << (parseWORD.binaryStrength,-7)
--      << (parseWORD.unaryStrength,-7) << endl;
--      foreach hashListX in hashTable do (
-- 	  hashList := hashListX;
-- 	  while true do
-- 	  when hashList
-- 	  is null do break
-- 	  is hashCell:WordListCell do (
-- 	       if hashCell.word.parse != parseWORD 
-- 	       then (
-- 		    o << (hashCell.word.name,12)
-- 		    << (hashCell.word.parse.precedence,-7) 
-- 		    << (hashCell.word.parse.binaryStrength,-7) 
-- 		    << (hashCell.word.parse.unaryStrength,-7) 
-- 		    << endl;
-- 		    );
-- 	       hashList = hashCell.next;
-- 	       );
-- 	  );
--      );
getParsing(e:Expr):Expr := (
     when e
     is s:SymbolClosure
     do (
	  x := s.symbol.word.parse;
	  list( Expr(toInteger(x.precedence)), Expr(toInteger(x.binaryStrength)), Expr(toInteger(x.unaryStrength))))
     else nullE);
setupfun("getParsing",getParsing);

dumpdatafun(e:Expr):Expr := (
     when e
     is s:string do (
	  o := stdin.insize;
	  p := stdin.eof;
	  q := stdin.inindex;
	  stdin.insize = 0;
	  stdin.eof = false;
	  stdin.inindex = 0;
	  oldErrorDepth := ErrorDepth;
	  ErrorDepth = reloaded + 1;
	  r := dumpdata(s);
	  ErrorDepth = oldErrorDepth;
	  stdin.insize = o;
	  stdin.eof = p;
	  stdin.inindex = q;
	  if 0 == r then nullE
	  else errorExpr("failed to dump data to '" + s + "'"))
     else WrongArg(0+1,"a string")
     );
setupfun("dumpdata",dumpdatafun);

loaddatafun(e:Expr):Expr := (
     when e
     is s:string do (
	  loaddata(s);			  -- should not return
	  errorExpr("failed to load data from '" + s + "'")
	  )
     else WrongArg(0+1,"a string")
     );
setupfun("loaddata",loaddatafun);

LongDoubleArrowFun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,LongDoubleArrowS);
setup(LongDoubleArrowS,LongDoubleArrowFun);

-- doublePointerfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,DoubleArrowS);
optionFun(lhs:Code,rhs:Code):Expr := (
     l := eval(lhs);
     when l is Error do l
     else (
     	  r := eval(rhs);
     	  when r is Error do r
	  else list(optionClass,Sequence(l,r))));
setup(DoubleArrowS,optionFun);

prependfun(e:Expr):Expr := (
     when e 
     is a:Sequence do (
	  if length(a) != 2
	  then WrongNumArgs(2)
	  else (
	       elem := a.0;
	       lst := a.1;
	       when lst
	       is y:Sequence do Expr(
		    new Sequence len length(y) + 1 do (
			 provide elem;
			 foreach t in y do provide t;
			 )		    
		    )
	       is y:List do (
		    r := List(
			 y.class,
			 new Sequence len length(y.v) + 1 do (
			      provide elem;
			      foreach t in y.v do provide t;
			      ),
			 0,y.mutable);
		    sethash(r,y.mutable);
		    Expr(r))
	       else WrongArg(1+1,"a list or sequence")
	       )
	  )
     else WrongNumArgs(2));
setupfun("prepend",prependfun);

appendfun(e:Expr):Expr := (
     when e 
     is a:Sequence do (
	  if length(a) != 2
	  then WrongNumArgs(2)
	  else (
	       elem := a.1;
	       lst := a.0;
	       when lst
	       is y:Sequence do Expr(
		    new Sequence len length(y) + 1 do (
			 foreach t in y do provide t;
			 provide elem;
			 )		    
		    )
	       is y:List do (
		    r := List(
			 y.class,
			 new Sequence len length(y.v) + 1 do (
			      foreach t in y.v do provide t;
			      provide elem;
			      ),
			 0,y.mutable);
		    sethash(r,y.mutable);
		    Expr(r))
	       else WrongArg(0+1,"a list or sequence")
	       )
	  )
     else WrongNumArgs(2));
setupfun("append",appendfun);

exitfun(e:Expr):Expr := (
     when e
     is Integer do (
	  if isInt(e) 
	  then (
	       exit(toInt(e));
	       nullE)
	  else WrongArg(1,"a small integer"))
     else WrongArg(1,"an integer"));
setupfun("exit",exitfun);

applythem(obj:HashTable,fn:FunctionClosure):void := (
     apply(fn,Expr(obj));
     );
RegisterFinalizer( obj:Handle, fn:function(Handle,int):void):void ::= 
     Ccode( void,
     	  "GC_REGISTER_FINALIZER(__subfront__(",
     	  h,
	  "),(GC_finalization_proc)", 
	  fn,
	  ",(void *)(", 
	  0,
	  "),0,0)" 
	  );

 -- see memdebug.h
 -- FixUp( obj:Handle ):void ::= Ccode( void, "((void *) ", obj, ") += sizeof(front)" );
    FixUp( obj:Handle ):void ::= Ccode( void, "(", obj, " = __addfront__(", obj, "))" );

--RegisterFinalizerFun(e:Expr):Expr := (
--     when e
--     is a:sequence do
--     if length(a) == 2
--     then (
--	  when a.0
--	  is obj:HashTable do (
--	       when a.1 
--	       is fn:FunctionClosure do (
--		    RegisterFinalizer(obj,applythem,fn);
--	       	    nullE
--		    )
--	       else WrongArg(2,"a user function")
--	       )
--	  else WrongArg(1,"a hash table")
--	  )
--     else WrongNumArgs(2)
--     else WrongNumArgs(2));
--setupfun("register",RegisterFinalizerFun);

freeHandle(obj:Handle,i:int):void := (
     FixUp(obj);
     gbforget(obj.handle);
     obj.handle = -1;			  -- mainly for debugging
     );
toHandle(e:Expr):Expr := (
     when e
     is i:Integer do (
	  if isInt(i) 
	  then (
	       h := Handle(toInt(i));
	       RegisterFinalizer(h,freeHandle);
	       Expr(h)
	       )
	  else WrongArg(1,"a small integer")
	  )
     else WrongArg(1,"an integer")
     );
setupfun("toHandle",toHandle);

match(subject:string,i:int,pattern:string,j:int):bool := (
     while true do (
	  if j == length(pattern) 
	  then return(i == length(subject))
	  else if pattern.j == '*' then (
	       if match(subject,i,pattern,j+1)
	       then return(true)
	       else if i < length(subject)
	       then i = i+1
	       else return(false)
	       )
	  else if i < length(subject) && subject.i == pattern.j
	  then (
	       i=i+1; 
	       j=j+1
	       )
	  else return(false)));
matchfun(e:Expr):Expr := (
     when e
     is a:Sequence do
     if length(a) == 2 then
     when a.0 
     is subject:string do
     when a.1
     is pattern:string do
     if match(subject,0,pattern,0) then True else False
     else WrongArg(2,"a string")
     else WrongArg(1,"a string")
     else WrongNumArgs(2)
     else WrongNumArgs(2)
     );
setupfun("match",matchfun);

lookupCountFun(e:Expr):Expr := (
     when e
     is s:SymbolClosure do Expr(toInteger(s.symbol.lookupCount))
     else WrongArg(1,"a symbol")
     );
setupfun("lookupCount",lookupCountFun);

use GC;
CollectGarbage(e:Expr):Expr := (
     gcollect();
     nullE);
setupfun("collectGarbage",CollectGarbage);

gcdump(e:Expr):Expr := (
     dump();
     nullE);
setupfun("gcDump",gcdump);

integermod(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then (
	       when a.0
     	       is x:Integer do (
	  	    when a.1
	  	    is y:Integer do (
	       		 if y === 0
	       		 then errorExpr("division by zero")
	       		 else Expr(x % y))
	  	    else WrongArg(2,"an integer"))
     	       else WrongArg(1,"an integer"))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(PercentS,integerClass,integerClass,integermod);

modC(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,PercentS);
setup(PercentS,modC);

AtAtfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,AtAtS);
setup(AtAtS,AtAtfun);

StarStarfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,StarStarS);
setup(StarStarS,StarStarfun);

-- doublecolonfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,ColonColonS);
-- setup(ColonColonS,doublecolonfun);

doubleplusfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,PlusPlusS);
setup(PlusPlusS,doubleplusfun);

lesslessfun2(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,LessLessS);
lesslessfun1(rhs:Code):Expr := unarymethod(rhs,LessLessS);
setup(LessLessS,lesslessfun1,lesslessfun2);

greatergreaterfun2(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,GreaterGreaterS);
setup(GreaterGreaterS,greatergreaterfun2);

barfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,BarS);
setup(BarS,barfun);

SlashHatfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,SlashHatS);
setup(SlashHatS,SlashHatfun);

colonfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,ColonS);
setup(ColonS,colonfun);

ampersandfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,AmpersandS);
setup(AmpersandS,ampersandfun);

hathatfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,HatHatS);
setup(HatHatS,hathatfun);

Tildefun(rhs:Code):Expr := unarymethod(rhs,TildeS);
setuppostfix(TildeS,Tildefun);

underscorefun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,UnderscoreS);
setup(UnderscoreS,underscorefun);

dotfun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left is Error do left
     is x:HashTable do (
	  when rhs
	  is r:exprCode do lookup1force(x, r.v)
	  else errorpos(rhs,"expected a symbol"))
     else WrongArg(1,"a hash table")
     );
setup(DotS,dotfun);

dotQfun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left is Error do left
     is x:HashTable do (
	  when rhs
	  is r:exprCode do if lookup1Q(x,r.v) then True else False
	  else errorpos(rhs,"expected a symbol"))
     else False);
setup(DotQuestionS,dotQfun);

atfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,AtS);
setup(AtS,atfun);

leftDividefun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,LeftDivideS);
setup(LeftDivideS,leftDividefun);

import WindowWidth(fd:int):int;
fileWidth(e:Expr):Expr := (
     when e
     is o:file do (
	  if o.infd == -1 && o.outfd == -1
	  then WrongArg("an open file")
	  else Expr(toInteger(WindowWidth(if o.infd != -1 then o.infd else o.outfd)))
	  )
     else WrongArg("a file"));
setupfun("fileWidth",fileWidth);

horizontalJoin(s:Sequence):Expr := (
     s = deepsplice(s);
     ln := 0;
     foreach f at i in s do (
	  when f 
	  is Nothing do nothing
	  is n:Net do (ln = ln + 1;)
	  is s:string do (ln = ln + 1;)
	  else return(WrongArg(i+1,"a net, string, or null")));
     v := new array(Net) len ln do (
	  foreach f in s do (
	       when f 
	       is Nothing do nothing
	       is n:Net do provide n
	       is s:string do provide toNet(s)
	       else nothing));
     Expr(HorizontalJoin(v)));
horizontalJoin(e:Expr):Expr := (
     when e
     is s:Sequence do horizontalJoin(s)
     is s:List do horizontalJoin(s.v)
     is n:Net do e
     is s:string do Expr(toNet(s))
     is Nothing do horizontalJoin(emptySequence)
     else WrongArg("a net, a string, or a list or sequence of nets and strings"));
setupfun("horizontalJoin",horizontalJoin);

stack(s:Sequence):Expr := (
     s = deepsplice(s);
     ln := 0;
     foreach f at i in s do (
	  when f 
	  is Nothing do nothing
	  is n:Net do (ln = ln + 1;)
	  is s:string do (ln = ln + 1;)
	  else return(WrongArg(i+1,"a net, string, or null")));
     v := new array(Net) len ln do (
	  foreach f in s do (
	       when f 
	       is Nothing do nothing
	       is n:Net do provide n
	       is s:string do provide toNet(s)
	       else nothing));
     Expr(VerticalJoin(v)));
stack(e:Expr):Expr := (
     when e
     is s:Sequence do stack(s)
     is s:List do stack(s.v)
     is n:Net do e
     is s:string do Expr(toNet(s))
     is Nothing do stack(emptySequence)
     else WrongArg("a sequence of nets and strings"));
setupfun("stack",stack);

raisef(e:Expr):Expr := (
     when e
     is s:Sequence do (
	  if length(s) == 2 then
	  when s.0 is n:Net do
	  when s.1 is i:Integer do
	  if isInt(i) then Expr(RaiseNet(n,toInt(i)))
	  else WrongArgSmallInteger()
	  else WrongArgInteger()
	  else WrongArg("a net")
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
setupfun("raise",raisef);

replicate(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then (
	       when a.0 is n:Integer do (
		    if isInt(n) then (
			 x := a.1;
			 m := toInt(n);
			 if m<0 then m=0;
			 Expr(new Sequence len m do provide x))
		    else WrongArgSmallInteger(1))
	       else WrongArgInteger(1))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(ColonS,integerClass,thingClass,replicate);

bitorfun(e:Expr):Expr := (
     when e is a:Sequence do (
     	  if length(a) == 2 then (
     	       when a.0 is x:Integer do (
     		    when a.1 is y:Integer do Expr(x | y) 
     		    else WrongArgInteger(2)
		    )
	       else WrongArgInteger(1))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(BarS,integerClass,integerClass,bitorfun);

bitandfun(e:Expr):Expr := (
     when e is a:Sequence do (
     	  if length(a) == 2 then (
	       when a.0 is x:Integer do (
		    when a.1 is y:Integer do Expr(x & y)
		    else WrongArgInteger(2))
	       else WrongArgInteger(1))
 	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(AmpersandS,integerClass,integerClass,bitandfun);

examine(e:Expr):Expr := (
     when e
     is sc:SymbolClosure do (
	  f := sc.frame;
	  s := sc.symbol;
	  stdout
	  << s.position
	  << " word.name:" << present(s.word.name)
	  << " scopenum:" << s.scopenum
	  << " frameindex:" << s.frameindex
	  << " lookupCount:" << s.lookupCount
	  << " protected:" << s.protected 
	  << " transientScope:" << s.transientScope
	  << endl
	  << "frames bound for scopes:";
	  while f.scopenum >= 0 do (
	       stdout << " " << f.scopenum;
	       f = f.next;
	       );
	  stdout << endl;
	  nullE)
     is fc:FunctionClosure do (
	  f := fc.frame;
	  model := fc.model;
	  desc := model.desc;
	  stdout
	  << "restargs:" << desc.restargs << ", "
	  << "scopenum:" << desc.scopenum << ", "
	  << "framesize:" << desc.framesize << ", "
	  << "numparms:" << desc.numparms << ", "
	  << "hasClosure:" << desc.hasClosure << endl
	  << "frames bound for scopes:";
	  while f.scopenum >= 0 do (
	       stdout << " " << f.scopenum;
	       f = f.next;
	       );
	  stdout << endl; 
	  nullE)
     is s:Sequence do (
	  if length(s) == 0 then (
	       f := localFrame;
	       stdout << "frames currently bound for scopes:";
	       while f.scopenum >= 0 do (
		    stdout << " " << f.scopenum;
		    f = f.next;
		    );
	       stdout << endl;
	       nullE)
	  else WrongNumArgs(1))
     else WrongArg("(), a function, or a symbol"));
setupfun("examine",examine);
     
listFrame(s:Sequence):Expr := Expr(
     List(mutableListClass,
	  if s == globalFrame.values 
	  then emptySequence				    -- some variables in the global frame are protected!
	  else s,
	  nextHash(),
	  true));	  

frame(e:Expr):Expr := (
     when e
     is sc:SymbolClosure do Expr(listFrame(sc.frame.values))
     is fc:FunctionClosure do Expr(listFrame(fc.frame.values))
     is cfc:CompiledFunctionClosure do Expr(listFrame(cfc.env))
     is CompiledFunction do Expr(listFrame(emptySequence))
     else WrongArg("a function"));
setupfun("frame", frame);

netWidth(e:Expr):Expr := (
     when e
     is n:Net do Expr(toInteger(n.width))
     else WrongArg("a net"));
setupfun("netWidth",netWidth);

netHeight(e:Expr):Expr := (
     when e
     is n:Net do Expr(toInteger(n.height))
     else WrongArg("a net"));
setupfun("netHeight",netHeight);

netDepth(e:Expr):Expr := (
     when e
     is n:Net do Expr(toInteger(length(n.body)-n.height))
     else WrongArg("a net"));
setupfun("netDepth",netDepth);

netRows(e:Expr):Expr := (
     when e
     is n:Net do list(new Sequence len length(n.body) do foreach s in n.body do provide Expr(s))
     is s:string do seq(e)
     else WrongArg("a net"));
setupfun("netRows",netRows);

alarm(e:Expr):Expr := (
     when e is i:Integer do 
     if isInt(i)
     then Expr(toInteger(alarm(toInt(i))))
     else WrongArgSmallInteger()
     else WrongArgInteger()
     );
setupfun("alarm",alarm);

endlfun(e:Expr):Expr := (
     when e is o:file do 
     if o.output 
     then Expr(o << endl)
     else WrongArg("an output file")
     else WrongArg("a file")
     );
setupfun("endl",endlfun);

import CCVERSION:string;
import VERSION:string;
import OS:string;
import ARCH:string;
import NODENAME:string;
import REL:string;
import DATE:string;
import TIME:string;
import LIBFACVERSION:string;
import GCVERSION:string;
import FACTORYVERSION:string;
import DUMPDATA:bool;
import FACTORY:bool;
import MP:bool;
setupconst("newline", Expr(newline));

x := newHashTable(hashTableClass,nothingClass);
storeInHashTable(x,Expr("VERSION"),Expr(VERSION));
storeInHashTable(x,Expr("architecture"),Expr(ARCH));
storeInHashTable(x,Expr("operating system"),Expr(OS));
storeInHashTable(x,Expr("operating system release"),Expr(REL));
storeInHashTable(x,Expr("compiler"),Expr(CCVERSION));
storeInHashTable(x,Expr("compile time"),Expr(DATE+" "+TIME));
storeInHashTable(x,Expr("compile node name"),Expr(NODENAME));
storeInHashTable(x,Expr("dumpdata"),Expr(if DUMPDATA then True else False));
storeInHashTable(x,Expr("factory"),Expr(if FACTORY then True else False));
storeInHashTable(x,Expr("mp"),Expr(if MP then True else False));
storeInHashTable(x,Expr("gc version"),Expr(GCVERSION));
if FACTORY then (
     storeInHashTable(x,Expr("libfac version"),Expr(LIBFACVERSION));
     storeInHashTable(x,Expr("factory version"),Expr(FACTORYVERSION));
     );
sethash(x,false);
setupconst("version", Expr(x));

removefun(e:Expr):Expr := (
     when e
     is args:Sequence do (
	  if length(args) != 2
	  then WrongNumArgs(2)
	  else (
	       when args.0
	       is o:HashTable do (
		    ret := remove(o,args.1);
		    when ret is Error do ret else nullE)
	       else WrongArg(1,"a hash table")))
     else WrongNumArgs(2));
setupfun("remove",removefun);

erase(s:Symbol,dictionary:Dictionary):bool := (
     i := s.word.hash & (length(dictionary.hashTable)-1);
     entryList := dictionary.hashTable.i;
     when entryList
     is entryListCell:SymbolListCell do (
	  if entryListCell.entry == s
	  then (
	       dictionary.hashTable.i = entryListCell.next;
	       return(true);
	       );
	  lastCell := entryListCell;
	  entryList = entryListCell.next;
	  while true do (
	       when entryList
	       is entryListCell:SymbolListCell do (
	  	    if entryListCell.entry == s
	  	    then (
		    	 lastCell.next = entryListCell.next;
		    	 return(true);
		    	 );
	  	    lastCell = entryListCell;
	       	    entryList = entryListCell.next;
		    )
	       is null do return(false);
	       );
	  )
     is null do return(false);
     false
     );
erase(e:Expr):Expr := (
     when e is s:SymbolClosure do (
	  --if s.symbol.protected
	  --then errorExpr("attempt to erase a protected symbol")
	  --else 
	  if erase(s.symbol,globalScope.dictionary)
	  then e
	  else WrongArg("a global symbol"))
     else WrongArg("a symbol")
     );
setupfun("erase", erase);

factorInt(n:int):Expr := (
     facs := newHashTable(Tally,nothingClass);
     if n == 0 then (
	  storeInHashTable(facs,Expr(toInteger(n)),Expr(toInteger(1)));
	  )
     else (
	  d := 2;
	  hadone := false;
	  while (n % d) == 0 && n != -1 && n != 1 do (
	       key := Expr(toInteger(d));
	       storeInHashTable(facs,key, 
		    if hadone 
		    then lookup1(facs,key) + Expr(toInteger(1))
		    else Expr(toInteger(1)));
	       n = n / d;
	       hadone = true;
	       );
	  if n < 0 then (
	       n = -n;
	       storeInHashTable(facs,Expr(toInteger(-1)),Expr(toInteger(1)));
	       );
	  d = 3;
	  while n > 1 do (
	       hadodd := false;
	       while n > 1 && (n % d) == 0 do (
	       	    key := Expr(toInteger(d));
		    storeInHashTable(facs,key, 
			 if hadodd
			 then lookup1(facs,key) + Expr(toInteger(1))
			 else Expr(toInteger(1)));
		    n = n / d;
		    hadodd = true;
		    );
	       d = d+2;
	       if d > n/d then d = n;
	       );
	  );
     Expr(facs));

factorInteger(e:Expr):Expr := (
     when e is i:Integer do (
	  if isInt(i) then factorInt(toInt(i))
	  else WrongArgSmallInteger())
     else WrongArgSmallInteger());
setupfun("factorInteger",factorInteger);

setSpin(e:Expr):Expr := (
     when e is i:Integer do (
	  if isInt(i) then Expr(toInteger(setspinspan(toInt(i))))
	  else WrongArgSmallInteger())
     else WrongArgSmallInteger());
setupfun("setSpin",setSpin);

--rebind(e:Expr):Expr := (
--     when e is args:Sequence do (
--	  if length(args) == 2 then (
--	       when args.0 is f:CompiledFunctionClosure do (
--		    when args.1 is env:Sequence do (
--			 Expr(CompiledFunctionClosure(f.fn,nextHash(),env)))
--		    else Expr(CompiledFunctionClosure(f.fn,nextHash(),Sequence(args.1))))
--	       else WrongArg(1,"a compiled function closure"))
--	  else WrongNumArgs(2))
--     else WrongNumArgs(2));
--setupfun("rebind",rebind);

-- method functions for use in closures
method1(e:Expr,env:Sequence):Expr := (
     -- env.0 : the primary method function, used as key for lookup
     -- env.1 : the function to call if no method found
     f := lookup(Class(e),env.0);
     apply(if f == nullE then env.1 else f,e)
     );
newmethod1(e:Expr):Expr := (
     env := Sequence(nullE,e);
     cfc := Expr(CompiledFunctionClosure(method1,nextHash(),env));
     env.0 = cfc;
     cfc);
setupfun("newmethod1",newmethod1);

method123(e:Expr,env:Sequence):Expr := (
     -- env.0 : the primary method function, used as key for lookup
     -- env.1 : the function to call if no method found
     when e is args:Sequence do (
	  if length(args) == 2 then (
	       f := lookupBinaryMethod(Class(args.0),Class(args.1),env.0);
	       if f == nullE then f = env.1;
	       apply(f, args))
	  else if length(args) == 3 then (
	       f := lookupTernaryMethod(Class(args.0),Class(args.1),Class(args.2),env.0);
	       if f == nullE then f = env.1;
	       apply(f, args))
	  else if length(args) == 1 then (
	       f := lookup(Class(args.0),env.0);
	       if f == nullE then f = env.1;
	       apply(f, args.0))
	  else apply(env.1, args))
     else (
	  f := lookup(Class(e),env.0);
	  if f == nullE then f = env.1;
     	  apply(f,e)));

anonymousClass := newHashTable(thingClass,thingClass);
method123c(e:Expr,env:Sequence):Expr := (
     -- ClassArgument version
     -- env.0 : the primary method function, used as key for lookup
     -- env.1 : the function to call if no method found
     -- env.2 : a list {false,true,...} telling whether to treat
     --         the corresponding argument as the type.  Otherwise, use
     --         its class.  Assumed to be 'false' for arguments off
     --         the end of the list.
     when env.2 is u:List do (
	  useClass := u.v;
	  when e is args:Sequence do
	  if length(args) == 2 then (
	       a0 := (
		    if length(useClass) <= 0 || useClass.0 == False
		    then Class(args.0)
		    else when args.0 is o:HashTable do o else anonymousClass);
	       a1 := (
		    if length(useClass) <= 1 || useClass.1 == False
		    then Class(args.1)
		    else when args.1 is o:HashTable do o else anonymousClass);
	       f := lookupBinaryMethod(a0,a1,env.0);
	       if f == nullE then f = env.1;
	       apply(f, args))
	  else if length(args) == 3 then (
	       a0 := (
		    if length(useClass) <= 0 || useClass.0 == False
		    then Class(args.0)
		    else when args.0 is o:HashTable do o else anonymousClass);
	       a1 := (
		    if length(useClass) <= 1 || useClass.1 == False
		    then Class(args.1)
		    else when args.1 is o:HashTable do o else anonymousClass);
	       a2 := (
		    if length(useClass) <= 2 || useClass.2 == False
		    then Class(args.2)
		    else when args.2 is o:HashTable do o else anonymousClass);
	       f := lookupTernaryMethod(a0,a1,a2,env.0);
	       if f == nullE then f = env.1;
	       apply(f, args))
	  else if length(args) == 1 then (
	       a0 := (
		    if length(useClass) <= 0 || useClass.0 == False
		    then Class(args.0)
		    else when args.0 is o:HashTable do o else anonymousClass);
	       f := lookup(a0,env.0);
	       if f == nullE then f = env.1;
	       apply(f, args.0))
	  else apply(env.1, args)
	  else (
	       a0 := (
		    if length(useClass) <= 0 || useClass.0 == False
		    then Class(e)
		    else when e is o:HashTable do o else anonymousClass);
	       f := lookup(Class(e),env.0);
	       if f == nullE then f = env.1;
	       apply(f,e)))
     else errorExpr("invalid list"));
newmethod123c(e:Expr):Expr := (
     when e is env:Sequence do (
	  -- env.0 : the primary method function, used as key for lookup
	  -- env.1 : the function to call if no method found
	  -- env.2 : a list {false,true,...} telling whether to treat
	  --         the corresponding argument as the type.  Otherwise, use
	  --         its class.  Assumed to be 'false' for arguments off
	  --         the end of the list.
	  if length(env) == 3
	  then (
	       when env.0
	       is Nothing do nothing
	       is CompiledFunction do nothing
	       is CompiledFunctionClosure do nothing
	       is FunctionClosure do nothing
	       else return(WrongArg(1,"a function"));
	       when env.1
	       is CompiledFunction do nothing
	       is CompiledFunctionClosure do nothing
	       is FunctionClosure do nothing
	       else return(WrongArg(2,"a function"));
	       when env.2 is u:List do (
		    useClass := u.v;
		    foreach i in useClass do if !(i == True || i == False) 
		    then return( WrongArg(3,"a list of boolean values") );
		    allFalse := true;
		    foreach i in useClass do if i != False then allFalse = false;
		    cfc :=
		    if allFalse
		    then Expr(CompiledFunctionClosure(method123,nextHash(),env))
		    else Expr(CompiledFunctionClosure(method123c,nextHash(),env));
		    if env.0 == nullE then env.0 = cfc;
		    cfc)
	       else WrongArg(3,"a list of boolean values")
	       )
	  else WrongNumArgs(3))
     else WrongNumArgs(3));
setupfun("newmethod123c",newmethod123c);

drop(v:Sequence,b:Expr):Expr := (
     when b
     is n:Integer do (
	  if isInt(n) then (
	       m := toInt(n);
	       if m < 0 then (
		    m = -m;
		    if m >= length(v) 
		    then Expr(emptySequence)
		    else Expr(new Sequence len length(v)-m at i do provide v.i))
	       else if m == 0 then Expr(v)
	       else (
		    if m >= length(v) 
		    then Expr(emptySequence)
		    else Expr(new Sequence len length(v)-m at i do provide v.(i+m))))
	  else WrongArgSmallInteger())
     is w:List do (
	  if length(w.v) == 2 then (
	       when w.v.0
	       is ii:Integer do (
		    if isInt(ii) then (
			 i := toInt(ii);
			 if i < 0 then i = 0;
			 when w.v.1
			 is jj:Integer do (
			      if isInt(jj) then (
				   j := toInt(jj);
				   if j > length(v)-1 then j = length(v)-1;
				   if i > j 
				   then Expr(v)
				   else Expr(
					new Sequence len length(v)-(j-i+1) do (
					     for k from 0 to i-1 
					     do provide v.k;
					     for k from j+1 to length(v) 
					     do provide v.k)))
			      else WrongArg("a list of small integers"))
			 else WrongArg("a list of integers"))
		    else WrongArg("a list of small integers"))
	       else WrongArg("a list of integers"))
	  else WrongArg("a list of two integers"))
     else WrongArg(2,"an integer or list of integers"));
drop(e:Expr):Expr := (
     when e
     is args:Sequence do 
     if length(args) == 2 then (
	  when args.0
	  is x:List do (
	       vv := drop(x.v,args.1);
	       when vv
	       is v:Sequence do (
	       	    if v == x.v && x.mutable then v = copy(v);
	       	    list(x.class,v,x.mutable))
	       else vv)
	  is v:Sequence do drop(v,args.1)
	  else WrongArg(1,"a list or sequence"))
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("drop",drop);

take(v:Sequence,b:Expr):Expr := (
     when b
     is n:Integer do (
	  if isInt(n) then (
	       m := toInt(n);
	       if m < 0 then (
		    m = -m;
		    if m >= length(v) 
		    then Expr(v)
		    else (
			 k := length(v)-m;
			 Expr(new Sequence len m at i do provide v.(i+k))
			 )
		    )
	       else if m == 0 then Expr(emptySequence)
	       else (
		    if m >= length(v) 
		    then Expr(v)
		    else Expr(new Sequence len m at i do provide v.i)))
	  else WrongArgSmallInteger())
     is w:List do (
	  if length(w.v) == 2 then (
	       when w.v.0
	       is ii:Integer do (
		    if isInt(ii) then (
			 i := toInt(ii);
			 if i < 0 then i = 0;
			 when w.v.1
			 is jj:Integer do (
			      if isInt(jj) then (
				   j := toInt(jj);
				   if j > length(v)-1 then j = length(v)-1;
				   if i > j 
				   then Expr(emptySequence)
				   else Expr(
					new Sequence len j-i+1 do (
					     for k from i to j do provide v.k)))
			      else WrongArg("a list of small integers"))
			 else WrongArg("a list of integers"))
		    else WrongArg("a list of small integers"))
	       else WrongArg("a list of integers"))
	  else WrongArg("a list of two integers"))
     else WrongArg(2,"an integer or list of integers"));
take(e:Expr):Expr := (
     when e
     is args:Sequence do 
     if length(args) == 2 then (
	  when args.0
	  is x:List do (
	       vv := take(x.v,args.1);
	       when vv
	       is v:Sequence do (
	       	    if v == x.v && x.mutable then v = copy(v);
	       	    list(x.class,v,x.mutable))
	       else vv)
	  is v:Sequence do take(v,args.1)
	  else WrongArg(1,"a list or sequence"))
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("take",take);

anyhex(s:string):bool := (
     foreach c in s do if c == '+' || c == '%' then return(true);
     false);
lengthUnhexed(s:string):int := (
     n := 0;
     foreach c in s do if c == '%' then n=n-1 else n=n+1;
     if n < 0 then 0 else n);
hex(c:char):int := (
     i := int(c);
     if i >= int('0') && i <= int('9') then i - int('0')
     else if i >= int('A') && i <= int('F') then i + (10 - int('A'))
     else if i >= int('a') && i <= int('f') then i + (10 - int('a'))
     else 0);
unhex(s:string):string := (
     if !anyhex(s) then s
     else new string len lengthUnhexed(s) do (
	  hexing := 0;
	  hexval := 0;
	  foreach c in s 
	  do   if hexing == 2 then (hexing = 1; hexval = hex(c); ) 
	  else if hexing == 1 then (hexing = 0; provide char(hexval * 16 + hex(c))) 
	  else if c == '%'    then (hexing = 2;)
	  else if c == '+'    then (provide ' ';)
	  else (provide c;);
	  while true do provide ' '			    -- shouldn't happen
	  ));
unhex(e:Expr):Expr := (
     when e
     is s:string do Expr(unhex(s))
     else WrongArg("a string"));
setupfun("unhex",unhex);

echo(f:file,v:bool):Expr := (
     if f.input then ( f.echo = v; nullE)
     else WrongArg("an input file"));
echoOn(e:Expr):Expr := (
     when e
     is s:Sequence do
     if length(s) == 0 then echo(stdIO,true)
     else WrongArg("a file or ()")
     is f:file do echo(f,true)
     else WrongArg("a file or ()")
     );
setupfun("echoOn",echoOn);
echoOff(e:Expr):Expr := (
     when e
     is s:Sequence do
     if length(s) == 0 then echo(stdIO,false)
     else WrongArg("a file or ()")
     is f:file do echo(f,false)
     else WrongArg("a file or ()")
     );
setupfun("echoOff",echoOff);
kill(e:Expr):Expr := (
     when e 
     is f:file do (
	  if f.pid != 0 then (
	       if kill(f.pid,9) == ERROR
	       then errorExpr("can't kill process")
	       else (
		    if ERROR != wait(f.pid) then f.pid = 0;
		    nullE
		    )
	       )
	  else WrongArg("a file with a process associated to it")
	  )
     else WrongArg("a file"));
setupfun("kill",kill);

setEcho(e:Expr):Expr := (
     when e is f:file do (
	  f.echo = true;
	  nullE
	  )
     else nullE);
setupfun("setEcho",setEcho);
clearEcho(e:Expr):Expr := (
     when e is f:file do (
	  f.echo = false;
	  nullE
	  )
     else nullE);
setupfun("clearEcho",clearEcho);

setupconst("typicalValues", Expr(typicalValues));
setupconst("binaryOperators",Expr(new array(Expr) len length(opsWithBinaryMethod) do (
     foreach s in opsWithBinaryMethod do provide Expr(s))));
setupconst("prefixOperators",Expr(new array(Expr) len length(opsWithUnaryMethod) do (
     foreach s in opsWithUnaryMethod do provide Expr(s))));
setupconst("postfixOperators",Expr(new array(Expr) len length(opsWithPostfixMethod) do (
     foreach s in opsWithPostfixMethod do provide Expr(s))));
setupconst("otherOperators",Expr(new array(Expr) len length(opsOther) do (
     foreach s in opsOther do provide Expr(s))));
