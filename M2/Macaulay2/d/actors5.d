--		Copyright 1995 by Daniel R. Grayson

use C;
use system; 
use util;
use evaluate;
use common;
use evaluate;
use common;
use binding;
use nets;
use parser;
use lex;
use gmp;
use tokens;
use err;
use stdiop;
use ctype;
use stdio;
use varstrin;
use strings;
use actors;
use actors2;
use basic;
use struct;
use objects;
use actors4;
use engine;

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
	  olderrorDepth := errorDepth;
	  errorDepth = loadDepth + 1;
	  r := dumpdata(s);
	  errorDepth = olderrorDepth;
	  stdin.insize = o;
	  stdin.eof = p;
	  stdin.inindex = q;
	  if 0 == r then nullE
	  else buildErrorPacket("failed to dump data to '" + s + "'"))
     else WrongArgString(0+1)
     );
setupfun("dumpdata",dumpdatafun);

loaddatafun(e:Expr):Expr := (
     when e
     is s:string do (
	  loaddata(s);			  -- should not return
	  buildErrorPacket("failed to load data from '" + s + "'")
	  )
     else WrongArgString(0+1)
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
	  else WrongArgSmallInteger(1))
     else WrongArgInteger(1));
setupfun("exit",exitfun).protected = false;

applythem(obj:HashTable,fn:FunctionClosure):void := (
     apply(fn,Expr(obj));
     );

-- match(subject:string,i:int,pattern:string,j:int):bool := (
--      while true do (
-- 	  if j == length(pattern) 
-- 	  then return i == length(subject)
-- 	  else if pattern.j == '*' then (
-- 	       if match(subject,i,pattern,j+1)
-- 	       then return true
-- 	       else if i < length(subject)
-- 	       then i = i+1
-- 	       else return false
-- 	       )
-- 	  else if i < length(subject) && subject.i == pattern.j
-- 	  then (
-- 	       i=i+1; 
-- 	       j=j+1
-- 	       )
-- 	  else return false));
-- matchfun(e:Expr):Expr := (
--      when e
--      is a:Sequence do
--      if length(a) == 2 then
--      when a.0 
--      is subject:string do
--      when a.1
--      is pattern:string do
--      if match(subject,0,pattern,0) then True else False
--      else WrongArgString(2)
--      else WrongArgString(1)
--      else WrongNumArgs(2)
--      else WrongNumArgs(2)
--      );
-- setupfun("match",matchfun);

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

--gcdump(e:Expr):Expr := (
--     dump();
--     nullE);
--setupfun("gcDump",gcdump);

integermod(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is x:Integer do 
     when a.1 is y:Integer do 
     if y === 0 then buildErrorPacket("division by zero")
     else Expr(x % y)
     else WrongArgInteger(2)
     else WrongArgInteger(1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
installMethod(PercentS,integerClass,integerClass,integermod);

modC(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,PercentS);
setup(PercentS,modC);

AtAtfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,AtAtS);
setup(AtAtS,AtAtfun);

StarStarfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,StarStarS);
setup(StarStarS,StarStarfun);

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

PowerStarStarfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,PowerStarStarS);
setup(PowerStarStarS,PowerStarStarfun);

colonfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,ColonS);
setup(ColonS,colonfun);

ampersandfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,AmpersandS);
setup(AmpersandS,ampersandfun);

hathatfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,HatHatS);
setup(HatHatS,hathatfun);

Tildefun(rhs:Code):Expr := unarymethod(rhs,TildeS);
setuppostfix(TildeS,Tildefun);

ParenStarParenfun(rhs:Code):Expr := unarymethod(rhs,ParenStarParenS);
setuppostfix(ParenStarParenS,ParenStarParenfun);

underscorefun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,UnderscoreS);
setup(UnderscoreS,underscorefun);

dotfun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left is Error do left
     is x:HashTable do (
	  when rhs
	  is r:globalSymbolClosureCode do lookup1force(x, Expr(SymbolClosure(globalFrame,r.symbol)))
	  else printErrorMessage(rhs,"expected a symbol"))
     else WrongArg(1,"a hash table")
     );
setup(DotS,dotfun);

dotQfun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left is Error do left
     is x:HashTable do (
	  when rhs
	  is r:globalSymbolClosureCode do if lookup1Q(x,Expr(SymbolClosure(globalFrame,r.symbol))) then True else False
	  else printErrorMessage(rhs,"expected a symbol"))
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
	  else return WrongArg(i+1,"a net, string, or null"));
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
	  else return WrongArg(i+1,"a net, string, or null"));
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

showFrames(f:Frame):void := (
     stdout << " frames bound :";
     while (
	  stdout << " " << f.frameID;
	  if f.notrecyclable then stdout << " (NR)";
	  stdout << " [" << f.valuesUsed;
	  if f.valuesUsed != length(f.values) then stdout << "<" << length(f.values);
	  stdout << "]";
	  f != f.outerFrame ) do (
	  stdout << ",";
	  f = f.outerFrame;
	  );
     stdout << endl;
     );

examine(e:Expr):Expr := (
     when e
     is sc:SymbolClosure do (
	  f := sc.frame;
	  s := sc.symbol;
	  stdout
	  << "symbol : " << present(s.word.name) << endl
	  << " position : " << s.position << endl
	  << " frameID : " << s.frameID << endl
	  << " frameindex : " << s.frameindex << endl
	  << " lookupCount : " << s.lookupCount << endl
	  << " protected : " << s.protected << endl;
     	  showFrames(f);
          if s.frameID != f.frameID then stdout << " -- warning: incorrect frameID on first frame" << endl;
	  nullE)
     is fc:FunctionClosure do (
	  f := fc.frame;
	  model := fc.model;
	  desc := model.desc;
	  stdout
	  << " restargs : " << desc.restargs << endl
	  << " frameID : " << desc.frameID << endl
	  << " framesize : " << desc.framesize << endl
	  << " numparms : " << desc.numparms << endl;
     	  showFrames(f);
	  nullE)
     is fn:CompiledFunction do (
	  stdout
	  << " hash : " << fn.hash << endl;
	  nullE)
     is fnc:CompiledFunctionClosure do (
	  stdout
	  << " hash : " << fnc.hash << endl
	  << " env : [" << length(fnc.env) << "]" << endl;
	  nullE)
     is dc:DictionaryClosure do (
	  f := dc.frame;
	  d := dc.dictionary;
	  stdout
	  << " hash : " << d.hash << endl
	  << " frameID : " << d.frameID << endl
	  << " framesize : " << d.framesize << endl
	  << " transient : " << d.transient << endl
	  << " protected : " << d.protected << endl
	  << " symboltable size : " << d.symboltable.numEntries << endl;
     	  showFrames(f);
          if d.frameID != f.frameID then stdout << " -- warning: incorrect frameID on first frame" << endl;
	  nullE)
     is s:Sequence do (
	  if length(s) == 0 then (
     	       showFrames(localFrame);
	       nullE)
	  else WrongNumArgs(1))
     else WrongArg("(), a function, or a symbol"));
setupfun("examine",examine);

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

unstack(e:Expr):Expr := (
     when e
     is n:Net do list(new Sequence len length(n.body) do foreach s in n.body do provide Expr(s))
     is s:string do list(e)
     else WrongArg("a net"));
setupfun("unstack",unstack);

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
setupfun("endl",endlfun).protected = false;

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
import GMPVERSION:string;
import NTLVERSION:string;
import FACTORYVERSION:string;
import DUMPDATA:bool;
import startupString1:string;
import startupString2:string;
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
storeInHashTable(x,Expr("gc version"),Expr(GCVERSION));
storeInHashTable(x,Expr("gmp version"),Expr(GMPVERSION));
storeInHashTable(x,Expr("ntl version"),Expr(NTLVERSION));
storeInHashTable(x,Expr("libfac version"),Expr(LIBFACVERSION));
storeInHashTable(x,Expr("factory version"),Expr(FACTORYVERSION));
sethash(x,false);
setupconst("version", Expr(x));
setupconst("startupString1", Expr(startupString1));
setupconst("startupString2", Expr(startupString2));

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

erase(e:Expr):Expr := (
     when e is t:SymbolClosure do (
	  s := t.symbol;
	  d := globalDictionary;
	  if t.frame != globalFrame then return WrongArg("a global symbol");
	  while (
	       table := d.symboltable;
	       i := s.word.hash & (length(table.buckets)-1);
	       entryList := table.buckets.i;
	       when entryList
	       is entryListCell:SymbolListCell do (
		    if entryListCell.entry == s
		    then (
     	       	    	 if d.protected then return buildErrorPacket("symbol is in a protected dictionary");
			 table.numEntries = table.numEntries - 1;
			 table.buckets.i = entryListCell.next;
			 return nullE;
			 );
		    lastCell := entryListCell;
		    entryList = entryListCell.next;
		    while true do (
			 when entryList
			 is entryListCell:SymbolListCell do (
			      if entryListCell.entry == s
			      then (
     	       	    	 	   if d.protected then return buildErrorPacket("symbol is in a protected dictionary");
				   table.numEntries = table.numEntries - 1;
				   lastCell.next = entryListCell.next;
				   return nullE;
				   );
			      lastCell = entryListCell;
			      entryList = entryListCell.next;
			      )
			 is null do break;
			 );
		    )
	       is null do nothing;
	       d != d.outerDictionary ) do d = d.outerDictionary;
	  buildErrorPacket("symbol has already been erased"))
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
     else buildErrorPacket("invalid list"));
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
	       else return WrongArg(1,"a function");
	       when env.1
	       is CompiledFunction do nothing
	       is CompiledFunctionClosure do nothing
	       is FunctionClosure do nothing
	       else return WrongArg(2,"a function");
	       when env.2 is u:List do (
		    useClass := u.v;
		    foreach i in useClass do if !(i == True || i == False) 
		    then return  WrongArg(3,"a list of boolean values") ;
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
     foreach c in s do if c == '+' || c == '%' then return true;
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
     else WrongArgString());
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
	       then buildErrorPacket("can't kill process")
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

readlinkfun(e:Expr):Expr := (
     when e is filename:string do (
	  v := readlink(filename);
	  if length(v) == 0 then nullE else Expr(v))
     else WrongArgString());
setupfun("readlink",readlinkfun);

realpathfun(e:Expr):Expr := (
     when e is filename:string do (
	  v := realpath(filename);
	  if length(v) == 0 then nullE else Expr(v))
     else WrongArgString());
setupfun("realpath",realpathfun);

setupconst("typicalValues", Expr(typicalValues));
setupconst("binaryOperators",Expr(new array(Expr) len length(opsWithBinaryMethod) do (
     foreach s in opsWithBinaryMethod do provide Expr(s))));
setupconst("prefixOperators",Expr(new array(Expr) len length(opsWithUnaryMethod) do (
     foreach s in opsWithUnaryMethod do provide Expr(s))));
setupconst("postfixOperators",Expr(new array(Expr) len length(opsWithPostfixMethod) do (
     foreach s in opsWithPostfixMethod do provide Expr(s))));
setupconst("otherOperators",Expr(new array(Expr) len length(opsOther) do (
     foreach s in opsOther do provide Expr(s))));

fileExists(e:Expr):Expr := (
     when e is name:string do toExpr(fileExists(name))
     else WrongArgString()
     );
setupfun("fileExists",fileExists);

removeDirectory(e:Expr):Expr := (
     when e is name:string do
     if rmdir(name) == ERROR 
     then buildErrorPacket(syscallErrorMessage("removing a directory"))
     else nullE
     else WrongArgString()
     );
setupfun("removeDirectory",removeDirectory);

removeFile(e:Expr):Expr := (
     when e is name:string do
     if unlink(name) == ERROR 
     then buildErrorPacket(syscallErrorMessage("removing a file"))
     else nullE
     else WrongArgString()
     );
setupfun("removeFile",removeFile);

readDirectory(e:Expr):Expr := (
     when e is name:string do (
	  r := readDirectory(name);
	  when r is null do buildErrorPacket("can't read directory '" + name + "' : " + syserrmsg())
	  is x:array(string) do Expr(list(new Sequence len length(x) do foreach i in x do provide Expr(i))))
     else WrongArgString());
setupfun("readDirectory",readDirectory);

isDirectory(e:Expr):Expr := (
     when e is name:string do (
	  r := isDirectory(name);
	  if r == -1 then buildErrorPacket("can't see file '" + name + "' : " + syserrmsg())
	  else if r == 1 then True else False)
     else WrongArgString()
     );
setupfun("isDirectory",isDirectory);

isRegularFile(e:Expr):Expr := (
     when e is name:string do (
	  r := isRegularFile(name);
	  if r == -1 then buildErrorPacket("can't see file '" + name + "' : " + syserrmsg())
	  else if r == 1 then True else False)
     else WrongArgString()
     );
setupfun("isRegularFile",isRegularFile);

unlinkfun(e:Expr):Expr := (
     when e is name:string do
     if -1 == unlink(name) then buildErrorPacket("failed to unlink " + name + " : " + syserrmsg()) else nullE
     else WrongArgString());
setupfun("unlink",unlinkfun);

fileTime(e:Expr):Expr := (
     when e is name:string do (
	  r := fileTime(name);
	  if r == -1
	  then buildErrorPacket("can't see file '" + name + "' : " + syserrmsg())
	  else Expr(toInteger(r))
	  )
     else WrongArgString());
setupfun("fileTime",fileTime);

mkdir(e:Expr):Expr := (
     when e is name:string do (
	  r := mkdir(name);
	  if r == -1 then buildErrorPacket("can't make directory '" + name + "' : " + syserrmsg())
	  else nullE)
     else WrongArgString());
setupfun("mkdir",mkdir);

--import setFactorySeed(s:int):void;
--setFactorySeed(e:Expr):Expr := (
--     when e is s:Integer do (
--	  if isInt(s) then (
--	       setFactorySeed(toInt(s));
--	       nullE
--	       )
--	  else WrongArgSmallInteger(0+1)
--	  )
--     else WrongArgInteger(0+1)
--     );
--setupfun("setFactorySeed",setFactorySeed);

wrap(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is wid:Integer do
     if !isInt(wid) then WrongArgSmallInteger(1) else (
	  width := toInt(wid);
	  if width <= 0 then WrongArg(1,"a positive integer") else 
	  when s.1
	  is t:Net do Expr(wrap(width,'-',t))
	  is str:string do if length(str) <= width then Expr(str) else Expr(wrap(width,'-',toNet(str)))
	  else WrongArg(2,"a net"))
     else WrongArgInteger(1)
     else WrongNumArgs(2)
     );
setupfun("wrap",wrap);

minimizeFilename(e:Expr):Expr := (
     when e is s:string do Expr(minimizeFilename(s))
     else WrongArgString()
     );
setupfun("minimizeFilename",minimizeFilename);

relativizeFilename(e:Expr):Expr := (
     when e
     is t:Sequence do
     if length(t) == 2 then
     when t.0 is cwd:string do
     when t.1 is filename:string do Expr(relativizeFilename(cwd,filename))
     else WrongArgString(2)
     else WrongArgString(1)
     else WrongArg("a string or a pair of strings")
     is filename:string do Expr(relativizeFilename(filename))
     else WrongArg("a string or a pair of strings")
     );
setupfun("relativizeFilename",relativizeFilename);

isGlobalSymbol(e:Expr):Expr := (
     when e is s:string do (
	  when globalLookup(makeUniqueWord(s,parseWORD))
	  is x:Symbol do True
	  is null do False
	  )
     else WrongArgString());
setupfun("isGlobalSymbol",isGlobalSymbol);

getsym(d:Dictionary,s:string):Expr := (
     w := makeUniqueWord(s,parseWORD);
     when globalLookup(w) is x:Symbol do Expr(SymbolClosure(globalFrame,x))
     is null do (
	  t := makeSymbol(w,dummyPosition,d);
	  globalFrame.values.(t.frameindex)));

getGlobalSymbol(e:Expr):Expr := (
     when e 
     is s:string do getsym(globalDictionary,s)
     is z:Sequence do if length(z) != 2 then WrongNumArgs(2) else (
	  when z.0
	  is dc:DictionaryClosure do (
	       d := dc.dictionary;
	       if d.transient || d.frameID != 0 then WrongArg(1,"a global dictionary") else
	       when z.1
	       is s:string do getsym(d,s)
	       else WrongArgString(2)
	       )
	  else WrongArg(1,"a dictionary")
	  )
     else WrongArgString());
setupfun("getGlobalSymbol",getGlobalSymbol);

expandWord(e:Expr):Expr := (
     when e is word:string do (
	  when wordexp(word)
	  is null do buildErrorPacket("failed to expand word")
	  is r:array(string) do toExpr(r)
	  )
     else WrongArgString());
setupfun("expandWord",expandWord);

history(e:Expr):Expr := (
     when e
     is s:Sequence do (
	  if length(s) == 0 then (
	       r := history();
	       Expr(list(new Sequence len length(r) do foreach s in r do provide Expr(s))))
     	  else WrongNumArgs(0))
     else WrongNumArgs(0));
setupfun("history",history);

toPairs(r:array(int)):Expr := Expr( 
     list (
	  new Sequence len length(r)/2 at i do 
	  provide new Sequence len 2 at j do 
	  provide toExpr(r.(2*i+j))
	  )
     );

regexmatch(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a) == 2 then
     when a.0 is regexp:string do
     when a.1 is text:string do toPairs(regexmatch(regexp,text))
     else WrongArgString(2)
     else WrongArgString(1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("matches",regexmatch);
     
listFrame(s:Sequence):Expr := Expr(List(mutableListClass, s, nextHash(), true));	  
listFrame(f:Frame):Expr := if f.frameID == 0 then listFrame(emptySequence) else listFrame(f.values); -- refuse to defeat the protection of global variables
frame(e:Expr):Expr := (
     when e
     is s:Sequence do 
     if length(s) == 0 then Expr(listFrame(localFrame)) else WrongNumArgs(1,2)
     is sc:SymbolClosure do Expr(listFrame(sc.frame))
     is fc:FunctionClosure do Expr(listFrame(fc.frame))
     is cfc:CompiledFunctionClosure do Expr(listFrame(cfc.env))
     is CompiledFunction do Expr(listFrame(emptySequence))
     else WrongNumArgs(1,2));
setupfun("frame", frame);

numFrames(f:Frame):int := (
     n := 0;
     while f.frameID > 0
     && ( n = n+1; f != f.outerFrame )
     do f = f.outerFrame;
     n);

listFrames(f:Frame):Expr := Expr(
     list(
	  new Sequence len numFrames(f) do (
	       while f.frameID > 0
	       && (provide listFrame(f) ; f != f.outerFrame )
	       do f = f.outerFrame)));     

frames(e:Expr):Expr := (
     when e
     is a:Sequence do if length(a) == 0 then listFrames(localFrame) else WrongNumArgs(0,1) 
     is sc:SymbolClosure do Expr(listFrames(sc.frame))
     is fc:FunctionClosure do Expr(listFrames(fc.frame))
     is cfc:CompiledFunctionClosure do Expr(list(listFrame(cfc.env)))
     is CompiledFunction do Expr(list(listFrame(emptySequence)))
     else WrongArg("a function, a symbol, or ()"));
setupfun("frames", frames);

newDictionaryFun(e:Expr):Expr := (
     when e is a:Sequence do if length(a) == 0 then Expr(DictionaryClosure(globalFrame,newGlobalDictionary()))
     else WrongNumArgs(0)
     else WrongNumArgs(0));
setupfun("newDictionary", newDictionaryFun);


localDictionaries(f:Frame):Expr := Expr(
     list(
	  new Sequence len numFrames(f) do (
	       while f.frameID > 0
	       && ( provide Expr(localDictionaryClosure(f)); f != f.outerFrame )
	       do f = f.outerFrame)));

localDictionaries(e:Expr):Expr := (
     when e
     is x:Sequence do if length(x) != 0 then WrongNumArgs(0,1) else localDictionaries(noRecycle(localFrame))
     is dc:DictionaryClosure do localDictionaries(dc.frame)
     is sc:SymbolClosure do localDictionaries(sc.frame)
     is fc:FunctionClosure do localDictionaries(fc.frame)
     is cfc:CompiledFunctionClosure do localDictionaries(emptyFrame)	    -- some values are there, but no symbols
     is CompiledFunction do localDictionaries(emptyFrame)			    -- no values or symbols are there
     else WrongArg("a function, a symbol, or ()"));
setupfun("localDictionaries", localDictionaries);


-----------------------------------------------------------------------------

globalDictionaryList():Expr := (		    -- get the current globalDictionary list
     g := globalDictionary;
     n := 0;
     while ( n = n+1; g != g.outerDictionary ) do g = g.outerDictionary;
     g = globalDictionary;
     Expr(list(new Sequence len n do while true do ( provide Expr(DictionaryClosure(globalFrame,g)); g = g.outerDictionary ))));

globalDictionaries(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 0 then globalDictionaryList()
     	  else WrongNumArgs(0))
     is t:List do (					    -- set the current globalDictionary list
	  s := t.v;
	  n := length(s);
	  if n == 0 then return WrongArg("expected a nonempty list of globalDictionaries");
          sawUnprotected := false;
	  sawM2dict := false;
	  foreach x in s do 
	  when x is dc:DictionaryClosure do (
	       d := dc.dictionary;
	       if d == Macaulay2Dictionary then sawM2dict = true;
	       if !d.protected then sawUnprotected = true;
	       if d.frameID != 0 || d.transient then return WrongArg("expected a list of global dictionaries")
	       )
	  else return WrongArg("expected a list of dictionaries");
	  if !sawM2dict then return WrongArg("expected a list of dictionaries containing Macaulay2Dictionary");
          if !sawUnprotected then return WrongArg("expected a list of dictionaries, not all protected");
     	  a := new array(Dictionary) len n do foreach x in s do when x is d:DictionaryClosure do provide d.dictionary else nothing;
     	  a.(n-1).outerDictionary = a.(n-1);
     	  for i from 0 to n-2 do a.i.outerDictionary = a.(i+1);
	  globalDictionary = a.0;
	  e)
     else WrongNumArgs(0));
globalDictionariesS := setupvar("globalDictionaries",globalDictionaryList());
storeGlobalDictionaries(e:Expr):Expr := (			    -- called with (symbol,newvalue)
     when e
     is s:Sequence do if length(s) != 2 then WrongNumArgs(2) else (
	  sym := s.0;
	  if !(sym === globalDictionariesS)
	  then buildErrorPacket("global assignment hook encountered unknown symbol")
	  else globalDictionaries(s.1))
     else WrongNumArgs(2));
storeInHashTable(globalAssignmentHooks,Expr(SymbolClosure(globalFrame,globalDictionariesS)),Expr(CompiledFunction(storeGlobalDictionaries,nextHash())));

export setGlobalVariable(x:Symbol,y:Expr):void := globalFrame.values.(x.frameindex) = y;
export getGlobalVariable(x:Symbol):Expr := globalFrame.values.(x.frameindex);

export stopIfError := false;

debuggingModeS := setupvar("debuggingMode",toExpr(debuggingMode));
debugLevelS := setupvar("debugLevel",toExpr(debugLevel));
loadDepthS := setupvar("loadDepth",toExpr(loadDepth));
recursionLimitS := setupvar("recursionLimit",toExpr(recursionlimit));
errorDepthS := setupvar("errorDepth",toExpr(errorDepth));
stopIfErrorS := setupvar("stopIfError",toExpr(stopIfError));
syms := SymbolSequence(debuggingModeS,loadDepthS,errorDepthS,recursionLimitS,stopIfErrorS,debugLevelS);

export setDebuggingMode(b:bool):void := (
     debuggingMode = b;
     setGlobalVariable(debuggingModeS,toExpr(b));
     );
export setloadDepth(b:int):void := (
     loadDepth = b;
     setGlobalVariable(loadDepthS,toExpr(b));
     );
export setstopIfError(b:bool):void := (
     stopIfError = b;
     setGlobalVariable(stopIfErrorS,toExpr(b));
     );

msg := "global assignment hook encountered unknown symbol/value combination";
store(e:Expr):Expr := (			    -- called with (symbol,newvalue)
     when e
     is s:Sequence do if length(s) != 2 then WrongNumArgs(2) else (
	  sym := s.0;
	  when s.1
	  is b:Boolean do (
	       if sym === debuggingModeS then (debuggingMode = b.v; e)
	       else if sym === stopIfErrorS then (stopIfError = b.v; e)
	       else buildErrorPacket(msg))
	  is i:Integer do 
	  if !isInt(i) then buildErrorPacket(msg)
	  else (
	       n := toInt(i);
	       if sym === loadDepthS then (loadDepth = n; e)
	       else if sym === errorDepthS then (errorDepth = n; e)
	       else if sym === debugLevelS then (debugLevel = n; e)
	       else if sym === recursionLimitS then (recursionlimit = n; e)
	       else buildErrorPacket(msg))
	  else buildErrorPacket(msg))
     else WrongNumArgs(2));
storeE := Expr(CompiledFunction(store,nextHash()));
foreach s in syms do storeInHashTable(globalAssignmentHooks,Expr(SymbolClosure(globalFrame,s)),storeE);
storeE = nullE;
syms = SymbolSequence();
