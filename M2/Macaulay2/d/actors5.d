--		Copyright 1995,2010 by Daniel R. Grayson
use actors;
use actors2;

header "#include <interface/random.h>";

getParsing(e:Expr):Expr := (
     when e
     is s:SymbolClosure
     do (
	  x := s.symbol.word.parse;
	  list( toExpr(x.precedence), toExpr(x.binaryStrength), toExpr(x.unaryStrength)))
     else nullE);
setupfun("getParsing",getParsing);

LongDoubleRightArrowFun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,LongDoubleRightArrowS);
setup(LongDoubleRightArrowS,LongDoubleRightArrowFun);

LongLongDoubleRightArrowFun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,LongLongDoubleRightArrowS);
setup(LongLongDoubleRightArrowS,LongLongDoubleRightArrowFun);

LongDoubleLeftArrowFun1(rhs:Code):Expr := unarymethod(rhs,LongDoubleLeftArrowS);
LongDoubleLeftArrowFun2(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,LongDoubleLeftArrowS);
setup(LongDoubleLeftArrowS,LongDoubleLeftArrowFun1,LongDoubleLeftArrowFun2);

LongLongDoubleLeftArrowFun1(rhs:Code):Expr := unarymethod(rhs,LongLongDoubleLeftArrowS);
LongLongDoubleLeftArrowFun2(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,LongLongDoubleLeftArrowS);
setup(LongLongDoubleLeftArrowS,LongLongDoubleLeftArrowFun1,LongLongDoubleLeftArrowFun2);

LongBiDoubleArrowFun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,LongBiDoubleArrowS);
setup(LongBiDoubleArrowS,LongBiDoubleArrowFun);

binaryDeductionFun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,DeductionS);
unaryDeductionFun(rhs:Code):Expr := unarymethod(rhs,DeductionS);
setup(DeductionS,unaryDeductionFun,binaryDeductionFun);

-- doublePointerfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,DoubleArrowS);
optionFun(lhs:Code,rhs:Code):Expr := (
    -- # typical value: symbol =>, Thing, Thing, Option
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
			 y.Class,
			 new Sequence len length(y.v) + 1 do (
			      provide elem;
			      foreach t in y.v do provide t;
			      ),
			 hash_t(0),y.Mutable);
		    Expr(sethash(r,y.Mutable)))
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
			 y.Class,
			 new Sequence len length(y.v) + 1 do (
			      foreach t in y.v do provide t;
			      provide elem;
			      ),
			 hash_t(0),y.Mutable);
		    Expr(sethash(r,y.Mutable)))
	       else WrongArg(0+1,"a list or sequence")
	       )
	  )
     else WrongNumArgs(2));
setupfun("append",appendfun);

exitfun(e:Expr):Expr := (
     when e
     is ZZcell do (
	  if isInt(e) 
	  then (
	       exit(toInt(e));
	       nullE			     -- just to satisfy noisy compilers
	       )
	  else WrongArgSmallInteger(1))
     else WrongArgZZ(1));
setupfun("exit",exitfun).Protected = false;

lookupCountFun(e:Expr):Expr := (
     when e
     is s:SymbolClosure do toExpr(s.symbol.lookupCount)
     else WrongArg(1,"a symbol")
     );
setupfun("lookupCount",lookupCountFun);

integermod(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is x:ZZcell do 
     when a.1 is y:ZZcell do 
     if y.v === 0 then a.0
     else toExpr(x.v % y.v)
     else WrongArgZZ(2)
     else WrongArgZZ(1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
installMethod(PercentS,ZZClass,ZZClass,integermod);

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

BarUnderscorefun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,BarUnderscoreS);
setup(BarUnderscoreS,BarUnderscorefun);

UnderscoreGreaterfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,UnderscoreGreaterS);
setup(UnderscoreGreaterS,UnderscoreGreaterfun);

UnderscoreGreaterEqualfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,UnderscoreGreaterEqualS);
setup(UnderscoreGreaterEqualS,UnderscoreGreaterEqualfun);

UnderscoreLessfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,UnderscoreLessS);
setup(UnderscoreLessS,UnderscoreLessfun);

UnderscoreLessEqualfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,UnderscoreLessEqualS);
setup(UnderscoreLessEqualS,UnderscoreLessEqualfun);

PowerGreaterfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,PowerGreaterS);
setup(PowerGreaterS,PowerGreaterfun);

PowerGreaterEqualfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,PowerGreaterEqualS);
setup(PowerGreaterEqualS,PowerGreaterEqualfun);

PowerLessfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,PowerLessS);
setup(PowerLessS,PowerLessfun);

PowerLessEqualfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,PowerLessEqualS);
setup(PowerLessEqualS,PowerLessEqualfun);

PowerStarStarfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,PowerStarStarS);
setup(PowerStarStarS,PowerStarStarfun);

colonfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,ColonS);
setup(ColonS,colonfun);

ampersandfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,AmpersandS);
setup(AmpersandS,ampersandfun);

hathatfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,HatHatS);
setup(HatHatS,hathatfun);

interpunctfun(lhs:Code, rhs:Code):Expr := binarymethod(lhs, rhs, InterpunctS);
setup(InterpunctS, interpunctfun);

boxtimesfun(lhs:Code, rhs:Code):Expr := binarymethod(lhs, rhs, BoxTimesS);
setup(BoxTimesS, boxtimesfun);

shuffleproductfun(lhs:Code, rhs:Code):Expr := binarymethod(lhs, rhs, ShuffleProductS);
setup(ShuffleProductS, shuffleproductfun);

Tildefun(rhs:Code):Expr := unarymethod(rhs,TildeS);
setuppostfix(TildeS,Tildefun);

PowerTildefun(rhs:Code):Expr := unarymethod(rhs,PowerTildeS);
setuppostfix(PowerTildeS,PowerTildefun);

UnderscoreTildefun(rhs:Code):Expr := unarymethod(rhs,UnderscoreTildeS);
setuppostfix(UnderscoreTildeS,UnderscoreTildefun);

ParenStarParenfun(rhs:Code):Expr := unarymethod(rhs,ParenStarParenS);
setuppostfix(ParenStarParenS,ParenStarParenfun);

UnderscoreStarfun(rhs:Code):Expr := unarymethod(rhs,UnderscoreStarS);
setuppostfix(UnderscoreStarS,UnderscoreStarfun);

PowerStarfun(rhs:Code):Expr := unarymethod(rhs,PowerStarS);
setuppostfix(PowerStarS,PowerStarfun);

--PowerSharpfun(rhs:Code):Expr := unarymethod(rhs,PowerSharpS);
--setuppostfix(PowerSharpS,PowerSharpfun);

--UnderscoreSharpfun(rhs:Code):Expr := unarymethod(rhs,UnderscoreSharpS);
--setuppostfix(UnderscoreSharpS,UnderscoreSharpfun);

Exclamationfun(rhs:Code):Expr := unarymethod(rhs,ExclamationS);
setuppostfix(ExclamationS,Exclamationfun);

PowerExclamationfun(rhs:Code):Expr := unarymethod(rhs,PowerExclamationS);
setuppostfix(PowerExclamationS,PowerExclamationfun);

UnderscoreExclamationfun(rhs:Code):Expr := unarymethod(rhs,UnderscoreExclamationS);
setuppostfix(UnderscoreExclamationS,UnderscoreExclamationfun);

factorial(x:Expr):Expr := (
     when x
     is x:RRcell do toExpr(factorial(x.v))
     is x:QQcell do toExpr(factorial(toRR(x.v)))
     is x:ZZcell do (
	  if !isULong(x.v) then return WrongArgSmallInteger();
	  n := toULong(x.v);
	  if n<2 then return oneE;
	  toExpr(factorial(n)))
     else WrongArg("integral or real number"));
setupfun("factorial",factorial);

installMethod(ExclamationS,RRClass,factorial);
installMethod(ExclamationS,ZZClass,factorial);
installMethod(ExclamationS,QQClass,factorial);

underscorefun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,UnderscoreS);
setup(UnderscoreS,underscorefun);

dotfun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left is Error do left
     is x:HashTable do (
	  when rhs
	  is r:globalSymbolClosureCode do lookup1force(x, Expr(SymbolClosure(globalFrame,r.symbol)))
	  else printErrorMessageE(rhs,"expected a symbol"))
     else WrongArgHashTable(1)
     );
setup(DotS,dotfun);

dotQfun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left is Error do left
     is x:HashTable do (
	  when rhs
	  is r:globalSymbolClosureCode do if lookup1Q(x,Expr(SymbolClosure(globalFrame,r.symbol))) then True else False
	  else printErrorMessageE(rhs,"expected a symbol"))
     else False);
setup(DotQuestionS,dotQfun);

atfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,AtS);
setup(AtS,atfun);

leftDividefun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,LeftDivideS);
setup(LeftDivideS,leftDividefun);

header "
#ifdef HAVE_SYS_IOCTL_H
 #include <sys/ioctl.h>
#endif
#ifdef HAVE_TERMIOS_H
 #include <termios.h>
#endif
";

WindowWidth(fd:int):int := Ccode(returns,"
   #ifdef HAVE_SYS_IOCTL_H
     struct winsize x = {0};
     ioctl(1,TIOCGWINSZ,&x);	/* see /usr/include/$SYSTEM/termios.h */
     return x.ws_col;
   #else
     return -1;
   #endif
");
WindowHeight(fd:int):int := Ccode(returns,"
   #ifdef HAVE_SYS_IOCTL_H
     struct winsize x = {0};
     ioctl(1,TIOCGWINSZ,&x);	/* see /usr/include/$SYSTEM/termios.h */
     return x.ws_row;
   #else
     return -1;
   #endif
");

fileWidth(e:Expr):Expr := (
     when e
     is o:file do (
	  if o.infd == -1 && o.outfd == -1
	  then WrongArg("an open file")
	  else toExpr(WindowWidth(if o.infd != -1 then o.infd else o.outfd)))
     else WrongArg("a file"));
setupfun("fileWidth",fileWidth);
fileHeight(e:Expr):Expr := (
     when e
     is o:file do (
	  if o.infd == -1 && o.outfd == -1
	  then WrongArg("an open file")
	  else toExpr(WindowHeight(if o.infd != -1 then o.infd else o.outfd)))
     else WrongArg("a file"));
setupfun("fileHeight",fileHeight);

raisef(e:Expr):Expr := (
     when e
     is s:Sequence do (
	  if length(s) == 2 then
	  when s.0 is n:Net do
	  when s.1 is i:ZZcell do
	  if isInt(i) then Expr(RaiseNet(n,toInt(i)))
	  else WrongArgSmallInteger()
	  else WrongArgZZ()
	  else WrongArg("a net")
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
setupfun("raise",raisef);

replicate(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then (
	       when a.0 is n:ZZcell do (
		    if isInt(n) then (
			 x := a.1;
			 m := toInt(n);
			 if m<0 then m=0;
			 Expr(new Sequence len m do provide x))
		    else WrongArgSmallInteger(1))
	       else WrongArgZZ(1))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(ColonS,ZZClass,thingClass,replicate);

bitorfun(e:Expr):Expr := (
     when e is a:Sequence do (
     	  if length(a) == 2 then (
     	       when a.0 is x:ZZcell do (
     		    when a.1 is y:ZZcell do toExpr(x.v | y.v) 
     		    else WrongArgZZ(2)
		    )
	       else WrongArgZZ(1))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(BarS,ZZClass,ZZClass,bitorfun);

bitandfun(e:Expr):Expr := (
     when e is a:Sequence do (
     	  if length(a) == 2 then (
	       when a.0 is x:ZZcell do (
		    when a.1 is y:ZZcell do toExpr(x.v & y.v)
		    else WrongArgZZ(2))
	       else WrongArgZZ(1))
 	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(AmpersandS,ZZClass,ZZClass,bitandfun);

newNetFile(e:Expr):Expr := (
     when e is a:Sequence do (
	  if length(a) == 0 then Expr(newNetFile())
	  else WrongNumArgs(0))
     else WrongNumArgs(0));
setupfun("newNetFile",newNetFile);

getNetFile(e:Expr):Expr := (
     when e is n:NetFile do (
	  v := tonets(n);
	  Expr(new Sequence len length(v) do foreach n in v do provide Expr(n)))
     else WrongArg("a net file"));
setupfun("getNetFile",getNetFile);

NetFileAppend(e:Expr):Expr := (
     when e is a:Sequence do (
	  if length(a) == 2 then (
	       when a.0 is n:NetFile do (
		    when a.1 is s:stringCell do Expr(n << s.v)
		    is x:Net do Expr(n << x)
		    else WrongArg(2,"a string or a net"))
	       else WrongArg(1,"a net file"))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
installMethod(LessLessS,netFileClass,stringClass,NetFileAppend);
installMethod(LessLessS,netFileClass,netClass,NetFileAppend);
		   
address(f:Frame):ulong := Ccode(ulong,"((unsigned long)(intptr_t)",f,")");

showFrames(f:Frame):void := (
     stdIO << " frames bound :";
     while (
	  stdIO << " " << f.frameID << " @" << address(f);
	  if f.notrecyclable then stdIO << " (NR)";
	  stdIO << " [" << f.valuesUsed;
	  if f.valuesUsed != length(f.values) then stdIO << " of " << length(f.values);
	  stdIO << "]";
	  f != f.outerFrame ) do (
	  stdIO << ",";
	  f = f.outerFrame;
	  );
     stdIO << endl;
     );

showsym(s:Symbol):void := (
     stdIO
     << " symbol : " << present(s.word.name) << endl
     << " position : " << s.position << endl
     << " frameID : " << s.frameID << endl
     << " frameindex : " << s.frameindex << endl
     << " lookupCount : " << s.lookupCount << endl
     << " protected : " << s.Protected << endl
     << " thread : " << s.thread << endl;
     );

examine(e:Expr):Expr := (
     when e
     is sc:SymbolClosure do (
	  f := sc.frame;
	  s := sc.symbol;
	  stdIO
	  << "symbol closure :" << endl;
     	  showsym(s);
     	  showFrames(f);
          if s.frameID != f.frameID then stdIO << " -- warning: incorrect frameID on first frame" << endl;
	  nullE)
     is sb:SymbolBody do (
	  stdIO << "symbol body :" << endl;
	  showsym(sb.symbol);
	  nullE)
     is c:PseudocodeClosure do (
	  f := c.frame;
     	  showFrames(f);
	  nullE)
     is fc:FunctionClosure do (
	  f := fc.frame;
	  model := fc.model;
	  desc := model.desc;
	  stdIO
     	  << "function closure :" << endl
     	  << " body hash : " << model.hash << endl
	  << " restargs : " << desc.restargs << endl
	  << " frameID : " << desc.frameID << endl
	  << " framesize : " << desc.framesize << endl
	  << " numparms : " << desc.numparms << endl;
     	  showFrames(f);
	  nullE)
     is model:functionCode do (
	  desc := model.desc;
	  stdIO
     	  << "function body :" << endl
     	  << " hash : " << model.hash << endl
	  << " restargs : " << desc.restargs << endl
	  << " frameID : " << desc.frameID << endl
	  << " framesize : " << desc.framesize << endl
	  << " numparms : " << desc.numparms << endl;
	  nullE)
     is fn:CompiledFunction do (
	  stdIO
	  << "compiled function:" << endl
	  << " hash : " << fn.hash << endl;
	  nullE)
     is fnc:CompiledFunctionClosure do (
	  stdIO
	  << "compiled function closure:" << endl
	  << " hash : " << fnc.hash << endl
	  << " env : [" << length(fnc.env) << "]" << endl;
	  nullE)
     is dc:DictionaryClosure do (
	  f := dc.frame;
	  d := dc.dictionary;
	  stdIO
	  << "dictionary closure:" <<endl
	  << " hash : " << d.hash << endl
	  << " frameID : " << d.frameID << endl
	  << " framesize : " << d.framesize << endl
	  << " transient : " << d.transient << endl
	  << " protected : " << d.Protected << endl
	  << " local creation allowed : " << d.LocalCreationAllowed << endl
	  << " symboltable size : " << d.symboltable.numEntries << endl;	-- TODO: lockRead() ?
     	  showFrames(f);
          if d.frameID != f.frameID then stdIO << " -- warning: incorrect frameID on first frame" << endl;
	  nullE)
     is s:Sequence do (
	  if length(s) == 0 then (
     	       showFrames(localFrame);
	       nullE)
	  else WrongNumArgs(1))
     is s:List do (
	  stdIO
	  << "basic list:" << endl
	  << " length: " << length(s.v) << endl
	  << " hash: " << s.hash << endl
	  << " mutable: " << s.Mutable << endl;
	  nullE)
     is s:SpecialExpr do (
	  stdIO
	  << "special expr:" << endl;
	  examine(s.e);
	  nullE)
     is x:xmlNodeCell do (examine(x.v); nullE)
     else WrongArg("(), a function, a symbol, or a basic list"));
setupfun("examine",examine);

numparms(e:Expr):Expr := (
     when e
     is fc:FunctionClosure do toExpr(if fc.model.desc.restargs then -1 else fc.model.desc.numparms)
     is fn:CompiledFunction do toExpr(-1)
     is fnc:CompiledFunctionClosure do toExpr(-1)
     is s:SpecialExpr do numparms(s.e)
     else WrongArg("a function"));
setupfun("numparms",numparms);

utf8substrfun(e:Expr):Expr := (
     when e is args:Sequence do
	  if length(args) != 3 then WrongNumArgs(3) else
	       when args.0 is s:stringCell do
		   when args.1 is start:ZZcell do
		       when args.2 is width:ZZcell do
		       toExpr(utf8substr(s.v,toInt(start),toInt(width)))
		       else WrongArgSmallInteger(3)
		   else WrongArgSmallInteger(2)
	       else WrongArg(1,"a string")
     else WrongNumArgs(3)
);
setupfun("utf8substring",utf8substrfun);

utf8charactersfun(e:Expr):Expr := (
     when e
     is s:stringCell do toExpr(utf8characters(s.v))
     else WrongArg("a string"));
setupfun("characters",utf8charactersfun);



stringWidth(e:Expr):Expr := (
     when e
     is s:stringCell do toExpr(utf8width(s.v))
     else WrongArg("a string"));
setupfun("stringWidth",stringWidth);

netWidth(e:Expr):Expr := (
     when e
     is n:Net do toExpr(n.width)
     else WrongArg("a net"));
setupfun("netWidth",netWidth);

netHeight(e:Expr):Expr := (
     when e
     is n:Net do toExpr(n.height)
     else WrongArg("a net"));
setupfun("netHeight",netHeight);

netDepth(e:Expr):Expr := (
     when e
     is n:Net do toExpr(length(n.body)-n.height)
     else WrongArg("a net"));
setupfun("netDepth",netDepth);

unstack(e:Expr):Expr := (
     when e
    -- # typical value: unstack, Net, List
     is n:Net do list(new Sequence len length(n.body) do foreach s in n.body do provide toExpr(s))
    -- # typical value: unstack, String, List
     is stringCell do list(e)
     else WrongArg("a net"));
setupfun("unstack",unstack);

alarm(e:Expr):Expr := (
     when e is i:ZZcell do 
     if isInt(i)
     then toExpr(int(alarm(uint(toInt(i)))))
     else WrongArgSmallInteger()
     else WrongArgZZ());
setupfun("alarm",alarm);

endlfun(e:Expr):Expr := (
     when e 
     is o:file do if o.output then Expr(o << endl) else WrongArg("an output file")
     is n:NetFile do Expr(endlnetfile(n))
     else WrongArg("a file or a net file")
     );
setupfun("endl",endlfun).Protected = false;
setupconst("newline", toExpr(newline));

remove(x:List,i:int):Expr:= (
     n := length(x.v);
     if !x.Mutable then buildErrorPacket("expected a mutable list")
     else if i >= n || i < -n then ArrayIndexOutOfBounds(i, n - 1)
     else (
	  if i < 0 then i = n + i;
	  ret := x.v.i;
	  for j from i to n - 2 do x.v.j = x.v.(j + 1);
	  Ccode(void, x.v, "->len = ", n - 1);
	  ret));

removefun(e:Expr):Expr := (
     when e
     is args:Sequence do (
	  if length(args) != 2
	  then WrongNumArgs(2)
	  else (
	       when args.0
	       is x:List do (
		    when args.1 is i:ZZcell do
		    if isInt(i) then remove(x,toInt(i)) else WrongArgSmallInteger(2)
		    else WrongArgZZ(2))
	       is f:Database do (
		    when args.1 is key:stringCell do (
	       		 if !f.isopen then return buildErrorPacket("database closed");
	       		 if !f.Mutable then return buildErrorPacket("database not mutable");
			 ret := dbmfetch(f.handle,key.v);
	       		 if 0 == dbmdelete(f.handle,key.v) then (
			     when ret
			     is s:string do toExpr(s)
			     is null do nullE)
	       		 else buildErrorPacket(dbmstrerror() + " : " + f.filename))
		    else WrongArgString(2))
	       is o:HashTable do remove(o,args.1)
	       else WrongArg(1,"a hash table or database")))
     else WrongNumArgs(2));
setupfun("remove",removefun);

erase(e:Expr):Expr :=
     when e is t:SymbolClosure do (
	  s := t.symbol;
	  d := globalDictionary;
	  if t.frame != globalFrame then return WrongArg("a global symbol");
	  found := false;
	  while (
	       table := d.symboltable;
	       lockRead(table.mutex);
	       i := s.word.hash & (length(table.buckets)-1);
	       entryList := table.buckets.i;
	       when entryList
	       is entryListCell:SymbolListCell do
		    if entryListCell.entry == s
		    then (
			 found = true;
			 if ! d.Protected then (
			      table.numEntries = table.numEntries - 1;
			      table.buckets.i = entryListCell.next);
			 )
		    else (
			 lastCell := entryListCell;
			 entryList = entryListCell.next;
			 while true do (
			      when entryList
			      is entryListCell:SymbolListCell do (
				   if entryListCell.entry == s
				   then (
					found = true;
					if ! d.Protected then (
					     table.numEntries = table.numEntries - 1;
					     lastCell.next = entryListCell.next);
					break);
				   lastCell = entryListCell;
				   entryList = entryListCell.next;
				   )
			      is null do break;
			      );
			 )
	       is null do nothing;
	       unlock(table.mutex);
	       ! found && d != d.outerDictionary ) do d = d.outerDictionary;
	  if ! found then buildErrorPacket("symbol has already been erased: "+s.word.name)
	  else if d.Protected then buildErrorPacket("symbol is in a protected dictionary")
	  else nullE)
     else WrongArg("a symbol");
setupfun("erase", erase);

factorInt(n:int):Expr := (
     facs := newHashTable(Tally,nothingClass);
     if n == 0 then (
	  storeInHashTable(facs,toExpr(n),oneE);
	  )
     else (
	  d := 2;
	  hadone := false;
	  while (n % d) == 0 && n != -1 && n != 1 do (
	       key := toExpr(d);
	       storeInHashTable(facs,key, 
		    if hadone 
		    then lookup1(facs,key) + oneE
		    else oneE);
	       n = n / d;
	       hadone = true;
	       );
	  if n < 0 then (
	       n = -n;
	       storeInHashTable(facs,minusoneE,oneE);
	       );
	  d = 3;
	  while n > 1 do (
	       hadodd := false;
	       while n > 1 && (n % d) == 0 do (
	       	    key := toExpr(d);
		    storeInHashTable(facs,key, 
			 if hadodd
			 then lookup1(facs,key) + oneE
			 else oneE);
		    n = n / d;
		    hadodd = true;
		    );
	       d = d+2;
	       if d > n/d then d = n;
	       );
	  );
     Expr(facs));

factorInteger(e:Expr):Expr := (
     when e is i:ZZcell do (
	  if isInt(i) then factorInt(toInt(i))
	  else WrongArgSmallInteger())
     else WrongArgSmallInteger());
setupfun("factorInteger",factorInteger);

-- method functions for use in closures
method1(e:Expr,env:Sequence):Expr := (
     -- env.0 : the primary method function, used as key for lookup
     -- env.1 : the function to call if no method found
     f := lookup(Class(e),env.0);
     if recursionDepth > recursionLimit then return InternalRecursionLimit();
     recursionDepth = recursionDepth + 1;
     r := applyEE(if f == nullE then env.1 else f,e);
     recursionDepth = recursionDepth - 1;
     r);
method1c(e:Expr,env:Sequence):Expr := (
     -- env.0 : the primary method function, used as key for lookup
     -- env.1 : the function to call if no method found
     when e is c:HashTable do (
     	  f := lookup(c,env.0);
     	  applyEE(if f == nullE then env.1 else f,e))
     else WrongArg("a class")
     );
newmethod1(e:Expr):Expr := (
     when e is s:Sequence do if length(s) != 3 then WrongNumArgs(3) else (
	  f := s.0;
	  output := s.1;
	  when s.2 is typ:HashTable do (
	       env := Sequence(nullE,f);
	       cfc := Expr(CompiledFunctionClosure(if output == True then method1c else method1,nextHash(),env));
	       if typ != compiledFunctionClosureClass then cfc = SpecialExpr(typ,cfc);
	       env.0 = cfc;
	       cfc)
	  else WrongArg(3,"a type of CompiledFunctionClosure")
	  )
     else WrongNumArgs(2));
setupfun("newmethod1",newmethod1);

applyEOS(f:Expr,o:Expr,s:Sequence):Expr := (
     if o == nullE then applyES(f,s)
     else (
     	  g := applyEE(f,o);
     	  when g is Error do g else applyES(g,s)));

applyEOE(f:Expr,o:Expr,x:Expr):Expr := (
     if o == nullE then applyEE(f,x)
     else (
     	  g := applyEE(f,o);
     	  when g is Error do g else applyEE(g,x)));

method1234(e:Expr,env:Sequence):Expr := (
     -- env.0 : the primary method function, used as key for lookup
     -- env.1 : the function to call if no method found
     when e is args:Sequence do (
	  if length(args) == 2 then (
	       f := lookupBinaryMethod(Class(args.0),Class(args.1),env.0);
	       if f == nullE then f = env.1;
	       applyES(f, args))
	  else if length(args) == 3 then (
	       f := lookupTernaryMethod(Class(args.0),Class(args.1),Class(args.2),env.0);
	       if f == nullE then f = env.1;
	       applyES(f, args))
	  else if length(args) == 1 then (
	       f := lookup(Class(args.0),env.0);
	       if f == nullE then f = env.1;
	       applyEE(f, args.0))
	  else if length(args) == 0 then (
	       f := lookup(env.0);
	       if f == nullE then f = env.1;
	       applyES(f, emptySequence))
	  else if length(args) == 4 then (
	       f := lookupQuaternaryMethod(Class(args.0),Class(args.1),Class(args.2),Class(args.3),env.0);
	       if f == nullE then f = env.1;
	       applyES(f, args))
	  else applyES(env.1, args))
     else (
	  f := lookup(Class(e),env.0);
	  if f == nullE then applyEE(env.1,e)
	  else applyEE(f,e)));

method1234o(e:Expr,env:Sequence):Expr := (
     -- e is (opt,arg);
     -- env.0 : the primary method function, used as key for lookup
     -- env.1 : the function to call if no method found
     when e is s:Sequence do if length(s) != 2 then return WrongNumArgs(2) else (
	  opt := s.0;
	  arg := s.1;
	  when arg is args:Sequence do (
	       if length(args) == 2 then (
		    f := lookupBinaryMethod(Class(args.0),Class(args.1),env.0);
		    if f == nullE then applyES(env.1, args)
		    else applyEOS(f, opt, args))
	       else if length(args) == 3 then (
		    f := lookupTernaryMethod(Class(args.0),Class(args.1),Class(args.2),env.0);
		    if f == nullE then applyES(env.1, args)
		    else applyEOS(f, opt, args))
	       else if length(args) == 1 then (
		    f := lookup(Class(args.0),env.0);
		    if f == nullE then applyEE(env.1, args.0)
		    else applyEOE(f, opt, args.0))
	       else if length(args) == 0 then (
		    f := lookup(env.0);
		    if f == nullE then applyES(env.1, args)
		    else applyEOS(f, opt, args))
	       else if length(args) == 4 then (
		    f := lookupQuaternaryMethod(Class(args.0),Class(args.1),Class(args.2),Class(args.3),env.0);
		    if f == nullE then applyES(env.1, args)
		    else applyEOS(f, opt, args))
	       else applyES(env.1, args))
	  else (
	       f := lookup(Class(arg),env.0);
	       if f == nullE then applyEE(env.1,arg)
	       else applyEOE(f,opt,arg)) )
     else WrongNumArgs(2));

method1234p(e:Expr,env:Sequence):Expr := ( -- just like method1234o, except we call the method function f as f(opt,arg)
     -- e is (opt,arg);
     -- env.0 : the primary method function, used as key for lookup
     -- env.1 : the function to call if no method found
     when e is s:Sequence do if length(s) != 2 then return WrongNumArgs(2) else (
	  opt := s.0;
	  arg := s.1;
	  when arg is args:Sequence do (
	       if length(args) == 2 then (
		    f := lookupBinaryMethod(Class(args.0),Class(args.1),env.0);
		    if f == nullE then applyES(env.1, args) else applyES(f,s))
	       else if length(args) == 3 then (
		    f := lookupTernaryMethod(Class(args.0),Class(args.1),Class(args.2),env.0);
		    if f == nullE then applyES(env.1, args) else applyES(f,s))
	       else if length(args) == 1 then (
		    f := lookup(Class(args.0),env.0);
		    if f == nullE then applyEE(env.1, args.0) else applyEOE(f, opt, args.0))
	       else if length(args) == 0 then (
		    f := lookup(env.0);
		    if f == nullE then applyES(env.1, args) else applyES(f,s))
	       else if length(args) == 4 then (
		    f := lookupQuaternaryMethod(Class(args.0),Class(args.1),Class(args.2),Class(args.3),env.0);
		    if f == nullE then applyES(env.1, args) else applyES(f,s))
	       else applyES(env.1, args))
	  else (
	       f := lookup(Class(arg),env.0);
	       if f == nullE then applyEE(env.1,arg) else applyES(f,s)) )
     else WrongNumArgs(2));

method1234c(e:Expr,env:Sequence):Expr := (
     -- ClassArgument version
     -- env.0 : the primary method function, used as key for lookup
     -- env.1 : the function to call if no method found
     -- env.2 : the function to call with the (i,args) if i-th output argument isn't even a hashtable, or (-1,) if args is not a sequence
     -- env.3 : a list {false,true,...} telling whether to treat
     --         the corresponding argument as the type.  Otherwise, use
     --         its class.  Assumed to be 'false' for arguments off
     --         the end of the list.
     -- env.4 :  null means: we dispatch on e  , and call the resulting method function f as f e
     --	    	 true means: we dispatch on e.1, and call the resulting method function f as (f e.0) e.1
     --         false means: we dispatch on e.1, and call the resulting method function f as f e
     opt := nullE;
     arg := e;
     disp:= e;
     if env.4 != nullE then (
     	  when e is s:Sequence do
	  if length(s) != 2 then return WrongNumArgs(2)
	  else (
	       if env.4 == True then ( disp = s.1; opt = s.0; arg = s.1; ) else
	       if env.4 == False then ( disp = s.1; ) else
	       return WrongArg(5,"true, false, or null");
	       )
	  else return WrongNumArgs(2));
     when env.3 is u:List do (
	  outputs := u.v;
	  when disp is dispseq:Sequence do
	  if length(dispseq) == 2 then (
	       a0 := (
		    if length(outputs) <= 0 || outputs.0 == False
		    then Class(dispseq.0)
		    else when dispseq.0 is o:HashTable do o else return applyEE(env.2, Expr(Sequence(zeroE,disp))));
	       a1 := (
		    if length(outputs) <= 1 || outputs.1 == False
		    then Class(dispseq.1)
		    else when dispseq.1 is o:HashTable do o else return applyEE(env.2, Expr(Sequence(oneE,disp))));
	       f := lookupBinaryMethod(a0,a1,env.0);
	       if f == nullE then applyES(env.1, dispseq) else applyEOE(f,opt,arg))
	  else if length(dispseq) == 3 then (
	       a0 := (
		    if length(outputs) <= 0 || outputs.0 == False
		    then Class(dispseq.0)
		    else when dispseq.0 is o:HashTable do o else return applyEE(env.2, Expr(Sequence(zeroE,disp))));
	       a1 := (
		    if length(outputs) <= 1 || outputs.1 == False
		    then Class(dispseq.1)
		    else when dispseq.1 is o:HashTable do o else return applyEE(env.2, Expr(Sequence(oneE,disp))));
	       a2 := (
		    if length(outputs) <= 2 || outputs.2 == False
		    then Class(dispseq.2)
		    else when dispseq.2 is o:HashTable do o else return applyEE(env.2, Expr(Sequence(twoE,disp))));
	       f := lookupTernaryMethod(a0,a1,a2,env.0);
	       if f == nullE then applyES(env.1, dispseq) else applyEOE(f,opt,arg))
	  else if length(dispseq) == 1 then (
	       a0 := (
		    if length(outputs) <= 0 || outputs.0 == False
		    then Class(dispseq.0)
		    else when dispseq.0 is o:HashTable do o else return applyEE(env.2, Expr(Sequence(zeroE,disp))));
	       f := lookup(a0,env.0);
	       if f == nullE then applyEE(env.1, dispseq.0) else applyEOE(f,opt,arg))
	  else if length(dispseq) == 4 then (
	       a0 := (
		    if length(outputs) <= 0 || outputs.0 == False
		    then Class(dispseq.0)
		    else when dispseq.0 is o:HashTable do o else return applyEE(env.2, Expr(Sequence(zeroE,disp))));
	       a1 := (
		    if length(outputs) <= 1 || outputs.1 == False
		    then Class(dispseq.1)
		    else when dispseq.1 is o:HashTable do o else return applyEE(env.2, Expr(Sequence(oneE,disp))));
	       a2 := (
		    if length(outputs) <= 2 || outputs.2 == False
		    then Class(dispseq.2)
		    else when dispseq.2 is o:HashTable do o else return applyEE(env.2, Expr(Sequence(twoE,disp))));
	       a3 := (
		    if length(outputs) <= 3 || outputs.3 == False
		    then Class(dispseq.3)
		    else when dispseq.3 is o:HashTable do o else return applyEE(env.2, Expr(Sequence(threeE,disp))));
	       f := lookupQuaternaryMethod(a0,a1,a2,a3,env.0);
	       if f == nullE then applyES(env.1, dispseq) else applyEOE(f,opt,arg))
	  else if length(dispseq) == 0 then (
	       f := lookup(env.0);
	       if f == nullE then applyES(env.1, dispseq) else applyEOE(f,opt,arg))
	  else applyES(env.1, dispseq)			    -- it's too long!
	  else (
	       a0 := (
		    if length(outputs) <= 0 || outputs.0 == False
		    then Class(disp)
		    else when disp is o:HashTable do o else return applyEE(env.2, Expr(Sequence(minusoneE,disp))));
	       f := lookup(a0,env.0);
	       if f == nullE then applyEE(env.1,disp) else applyEOE(f,opt,arg)))
     else buildErrorPacket("env.3: not a list"));
newmethod1234c(e:Expr):Expr := (
     when e is env:Sequence do (
	  -- see above for description of elements of env, except that env.0 is supplanted by us if it is null or a type of CompiledFunctionClosure
	  if length(env) == 5
	  then (
	       when env.0
	       is Nothing do nothing
	       is typ:HashTable do (
		    if !ancestor(typ,compiledFunctionClosureClass) then return WrongArg(1,"a function or a type of CompiledFunctionClosure");
		    )
	       is CompiledFunction do nothing
	       is CompiledFunctionClosure do nothing
	       is FunctionClosure do nothing
	       is s:SpecialExpr do if ancestor(s.Class,functionClass) then nothing else return WrongArg(1,"a function")
	       else return WrongArg(1,"a function or null");
	       when env.1
	       is CompiledFunction do nothing
	       is CompiledFunctionClosure do nothing
	       is FunctionClosure do nothing
	       is s:SpecialExpr do if ancestor(s.Class,functionClass) then nothing else return WrongArg(2,"a function")
	       else return WrongArg(2,"a function");
	       when env.2
	       is CompiledFunction do nothing
	       is CompiledFunctionClosure do nothing
	       is FunctionClosure do nothing
	       is s:SpecialExpr do if ancestor(s.Class,functionClass) then nothing else return WrongArg(3,"a function")
	       else return WrongArg(3,"a function");
	       when env.3 is u:List do (
		    useClass := u.v;
		    foreach i in useClass do if !(i == True || i == False) 
		    then return  WrongArg(4,"a list of boolean values") ;
		    allFalse := true;
		    foreach i in useClass do if i != False then allFalse = false;
		    env = copy(env);
		    cfc :=
		    if allFalse
		    then (
			 if env.4 == nullE
		    	 then Expr(CompiledFunctionClosure(method1234,nextHash(),env))
			 else if env.4 == True
			 then Expr(CompiledFunctionClosure(method1234o,nextHash(),env))
		    	 else Expr(CompiledFunctionClosure(method1234p,nextHash(),env))
			 )
		    else Expr(CompiledFunctionClosure(method1234c,nextHash(),env));
		    if env.0 == nullE then env.0 = cfc
		    else when env.0 is typ:HashTable do (
			 if typ != compiledFunctionClosureClass then cfc = SpecialExpr(typ,cfc);
			 env.0 = cfc;
			 )
		    else nothing;
		    cfc)
	       else WrongArg(4,"a list of boolean values")
	       )
	  else WrongNumArgs(5))
     else WrongNumArgs(5));
setupfun("newmethod1234c",newmethod1234c);

drop(v:Sequence,b:Expr):Expr := (
     when b
     is n:ZZcell do (
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
	       is ii:ZZcell do (
		    if isInt(ii) then (
			 i := toInt(ii);
			 if i < 0 then i = 0;
			 when w.v.1
			 is jj:ZZcell do (
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
	       	    if v == x.v && x.Mutable then v = copy(v);
	       	    list(x.Class,v,x.Mutable))
	       else vv)
	  is v:Sequence do drop(v,args.1)
	  else WrongArg(1,"a list or sequence"))
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("drop",drop);

take(v:Sequence,b:Expr):Expr := (
     when b
     is n:ZZcell do (
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
	       is ii:ZZcell do (
		    if isInt(ii) then (
			 i := toInt(ii);
			 if i < 0 then i = 0;
			 when w.v.1
			 is jj:ZZcell do (
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
	       	    if v == x.v && x.Mutable then v = copy(v);
	       	    list(x.Class,v,x.Mutable))
	       else vv)
	  is v:Sequence do take(v,args.1)
	  else (
	      iter := getIterator(args.0);
	      if iter == nullE
	      then return WrongArg(1, "a list, sequence, or iterable object");
	      nextfunc := getNextFunction(iter);
	      if nextfunc == nullE
	      then return buildErrorPacket(
		  "no method for applying next to iterator");
	      start := 0;
	      stop := 0;
	      when args.1
	      is n:ZZcell do (
		  if !isInt(n) then return WrongArgSmallInteger(2);
		  stop = toInt(n);
		  if stop < 0 then return WrongArg(2, "a nonnegative integer");
		  if stop == 0 then return list())
	      is x:List do (
		  ok := true;
		  if length(x.v) == 2 then (
		      when x.v.0
		      is a:ZZcell do (
			  when x.v.1
			  is b:ZZcell do (
			      if !isInt(a) || !isInt(b)
			      then ok = false
			      else (
				  start = toInt(a);
				  stop = toInt(b) + 1))
			  else ok = false)
		      else ok = false)
		  else ok = false;
		  if !ok
		  then return WrongArg(2, "a list of two small integers"))
	      else return WrongArg(2, "an integer or list of integers");
	      if stop < start then return list();
	      j := 0;
	      y := nullE;
	      while (j < start && y != StopIterationE)
	      do (
		  y = applyEE(nextfunc, iter);
		  when y
		  is Error do return returnFromFunction(y)
		  else nothing;
		  j = j + 1);
	      r := new Sequence len stop - start do provide nullE;
	      while (
		  y = applyEE(nextfunc, iter);
		  when y
		  is Error do return returnFromFunction(y)
		  else nothing;
		  y != StopIterationE)
	      do (
		  r.(j - start) = y;
		  j = j + 1;
		  if j == stop then break);
	      list(
		      if j < start then emptySequence
		      else if j == stop then r
		      else new Sequence len j - start do (
			  foreach x in r do provide x))))
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
     is s:stringCell do toExpr(unhex(s.v))
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
header "#include <signal.h>";
kill(pid:int,sig:int) ::= Ccode(int,"
     #ifdef HAVE_KILL
      kill(",pid,",",sig,")
     #else
      -1
     #endif
     ");
kill(e:Expr):Expr := (
     when e 
     is pid:ZZcell do if !isInt(pid) then WrongArgSmallInteger() else (
	  id := toInt(pid);
	  if kill(id,9) == ERROR then buildErrorPacket("can't kill process")
	  else if wait(id) == ERROR then buildErrorPacket("process killed, but wait fails") 
	  else nullE
	  )
     is f:file do (
	  if f.pid != 0 then (
	       if kill(f.pid,9) == ERROR then buildErrorPacket("can't kill process")
	       else if wait(f.pid) == ERROR  then buildErrorPacket("process killed, but wait fails")
	       else (
		    f.pid = 0;
		    nullE
		    )
	       )
	  else WrongArg("a file with a process associated to it")
	  )
     else WrongArg("a file"));
setupfun("kill",kill);

limitFiles(e:Expr):Expr := (
     when e is n:ZZcell do
     if isInt(n)
     then if limitFiles(toInt(n)) == 0 then nullE
     else buildErrorPacket(syserrmsg())
     else WrongArgSmallInteger(0)
     else WrongArgZZ(0));
setupfun("limitFiles",limitFiles);

limitProcesses(e:Expr):Expr := (
     when e is n:ZZcell do
     if isInt(n)
     then if limitProcesses(toInt(n)) == 0 then nullE
     else buildErrorPacket(syserrmsg())
     else WrongArgSmallInteger(0)
     else WrongArgZZ(0));
setupfun("limitProcesses",limitProcesses);

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
     when e is filename:stringCell do (
	  v := readlink(expandFileName(filename.v));
	  if length(v) == 0 then nullE else toExpr(v))
     else WrongArgString());
setupfun("readlink",readlinkfun);

realpathfun(e:Expr):Expr := (
     when e is f:stringCell do (
	  if f.v === "stdio" || f.v === "currentString" then return e;
     	  when realpath(expandFileName(f.v))
     	  is null do buildErrorPacket(syscallErrorMessage("realpath"))
     	  is p:string do toExpr(p)
     	  )
     else WrongArgString());
setupfun("realpath",realpathfun);

setupconst("typicalValues", Expr(typicalValues));
setupconst("flexibleBinaryOperators", Expr(new Sequence len length(opsWithBinaryMethod)   do foreach s in opsWithBinaryMethod do provide Expr(s)));
setupconst("flexiblePrefixOperators", Expr(new Sequence len length(opsWithUnaryMethod)    do foreach s in opsWithUnaryMethod do provide Expr(s)));
setupconst("flexiblePostfixOperators",Expr(new Sequence len length(opsWithPostfixMethod)  do foreach s in opsWithPostfixMethod do provide Expr(s)));
setupconst("fixedBinaryOperators",    Expr(new Sequence len length(fixedBinaryOperators)  do foreach s in fixedBinaryOperators do provide Expr(s)));
setupconst("fixedPrefixOperators",    Expr(new Sequence len length(fixedPrefixOperators)  do foreach s in fixedPrefixOperators do provide Expr(s)));
setupconst("fixedPostfixOperators",   Expr(new Sequence len length(fixedPostfixOperators) do foreach s in fixedPostfixOperators do provide Expr(s)));
setupconst("augmentedAssignmentOperators", Expr(new Sequence len length(augmentedAssignmentOperators) do foreach s in augmentedAssignmentOperators do provide Expr(s)));

fileExists(e:Expr):Expr := (
     when e is name:stringCell do toExpr(fileExists(expandFileName(name.v)))
     else WrongArgString()
     );
setupfun("fileExists",fileExists);

fileReadable(e:Expr):Expr := (
     when e is name:stringCell do toExpr(fileReadable(expandFileName(name.v)))
     else WrongArgString()
     );
-- # typical value: fileReadable, String, Boolean
setupfun("fileReadable",fileReadable);

fileWritable(e:Expr):Expr := (
     when e is name:stringCell do toExpr(fileWritable(expandFileName(name.v)))
     else WrongArgString()
     );
-- # typical value: fileWritable, String, Boolean
setupfun("fileWritable",fileWritable);

fileExecutable(e:Expr):Expr := (
     when e is name:stringCell do toExpr(fileExecutable(expandFileName(name.v)))
     else WrongArgString()
     );
-- # typical value: fileExecutable, String, Boolean
setupfun("fileExecutable",fileExecutable);

removeDirectory(e:Expr):Expr := (
     when e is name:stringCell do
     if rmdir(expandFileName(name.v)) == ERROR 
     then buildErrorPacket(syscallErrorMessage("removing a directory"))
     else nullE
     else WrongArgString()
     );
setupfun("removeDirectory",removeDirectory);

removeFile(e:Expr):Expr := (
     when e is name:stringCell do
     if unlink(expandFileName(name.v)) == ERROR 
     then buildErrorPacket(syscallErrorMessage("removing a file"))
     else nullE
     else WrongArgString()
     );
setupfun("removeFile",removeFile);

readDirectory(e:Expr):Expr := (
     when e is filename0:stringCell do (
	  filename := expandFileName(filename0.v);
	  r := readDirectory(filename);
	  when r is null do buildErrorPacket("can't read directory '" + filename + "' : " + syserrmsg())
	  is x:array(string) do Expr(list(new Sequence len length(x) do foreach i in x do provide toExpr(i))))
     else WrongArgString());
setupfun("readDirectory",readDirectory);

isDirectory(e:Expr):Expr := (
     when e is filename0:stringCell do (
	  filename := filename0.v;
	  filename = expandFileName(filename);
	  if !fileExists(filename) then return False;
	  if filename.(length(filename) - 1) != '/'
	  then filename = filename + "/";
	  r := isDirectory(filename);
	  if r == -1 then buildErrorPacket("can't see file '" + filename + "' : " + syserrmsg())
	  else if r == 1 then True else False)
     else WrongArgString()
     );
setupfun("isDirectory",isDirectory);

isRegularFile(e:Expr):Expr := (
     when e is filename0:stringCell do (
	  filename := filename0.v;
     	  filename = expandFileName(filename);
	  r := isRegularFile(filename);
	  if r == -1 then buildErrorPacket("can't see file '" + filename + "' : " + syserrmsg())
	  else if r == 1 then True else False)
     else WrongArgString()
     );
setupfun("isRegularFile",isRegularFile);

linkfun(e:Expr):Expr := (
     when e is s:Sequence do if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is oldfilename0:stringCell do
     when s.1 is newfilename0:stringCell do (
	  oldfilename := oldfilename0.v;
	  newfilename := newfilename0.v;
	  oldfilename = expandFileName(oldfilename);
	  newfilename = expandFileName(newfilename);
     	  if -1 == link(oldfilename,newfilename)
	  then buildErrorPacket("failed to link file " + oldfilename + " to " + newfilename + " : " + syserrmsg()) else nullE     
	  )
     else WrongArgString(2)
     else WrongArgString(1)
     else WrongNumArgs(2));
setupfun("linkFile",linkfun);

symlinkfun(e:Expr):Expr := (
     when e is s:Sequence do if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is oldfilename0:stringCell do
     when s.1 is newfilename0:stringCell do (
	  oldfilename := oldfilename0.v;
	  newfilename := newfilename0.v;
	  oldfilename = expandFileName(oldfilename);
	  newfilename = expandFileName(newfilename);
     	  if -1 == symlink(oldfilename,newfilename)
	  then buildErrorPacket("failed to symbolically link file " + oldfilename + " to " + newfilename + " : " + syserrmsg()) else nullE
	  )
     else WrongArgString(2)
     else WrongArgString(1)
     else WrongNumArgs(2));
setupfun("symlinkFile",symlinkfun);

fileTime(e:Expr):Expr := (
     when e is filename0:stringCell do (
	  filename := filename0.v;
	  filename = expandFileName(filename);
	  r := fileTime(filename);
	  if r == -1
	  then buildErrorPacket("can't see file '" + filename + "' : " + syserrmsg())
	  else toExpr(r))
     is args:Sequence do if length(args) != 2 then WrongNumArgs(2) else (
	  when args.0 is modtime:ZZcell do if !isInt(modtime) then WrongArgSmallInteger(2) else (
	       when args.1 is filename0:stringCell do (
		    filename := filename0.v;
	  	    filename = expandFileName(filename);
		    r := setFileTime(filename,toInt(modtime));
		    if r == -1 then buildErrorPacket("can't set modification time of file '" + filename + "' : " + syserrmsg())
	  	    else nullE)
	       else WrongArgString(1))
	  else WrongArgZZ(2))
     else WrongArg("string, or integer and string"));
setupfun("fileTime",fileTime);

haveNoTimeInitialized := false;
haveNoTime := false;
currentTime(e:Expr):Expr := (
     if !haveNoTimeInitialized then (
	  haveNoTimeInitialized = true;
	  foreach s in argv do if s === "--no-time" then haveNoTime = true);
     when e is a:Sequence do
     if length(a) == 0 then toExpr(if haveNoTime then 0 else currentTime())
     else WrongNumArgs(0)
     else WrongNumArgs(0));
setupfun("currentTime",currentTime);

mkdir(e:Expr):Expr := (
     when e is filename0:stringCell do (
	  filename := filename0.v;
	  filename = expandFileName(filename);
	  r := mkdir(filename);
	  if r == -1 then buildErrorPacket("can't make directory \"" + filename + "\": " + syserrmsg())
	  else nullE)
     else WrongArgString());
setupfun("mkdir",mkdir);

export printWidth := 0;

wrap(e:Expr):Expr := (
     wid := printWidth;
     sep := char(0);
     net := dummyNet;
     when e
     is s:Sequence do (
	  if length(s) == 3 then (
	       when s.0 
	       is Wid:ZZcell do if !isInt(Wid.v) then return WrongArgSmallInteger(1) else wid = toInt(Wid.v)
	       is Sep:stringCell do if length(Sep.v) == 0 then sep = char(0) else if length(Sep.v) == 1 then sep = Sep.v.0 else return WrongArg(1,"a string of length 0 or 1")
	       else return WrongArg(1,"a string or an integer");
	       when s.1
	       is Wid:ZZcell do if !isInt(Wid.v) then return WrongArgSmallInteger(1) else wid = toInt(Wid.v)
	       is Sep:stringCell do if length(Sep.v) == 0 then sep = char(0) else if length(Sep.v) == 1 then sep = Sep.v.0 else return WrongArg(1,"a string of length 0 or 1")
	       else return WrongArg(1,"a string or an integer");
	       when s.2
	       is t:Net do net = t 
	       is s:stringCell do net = toNet(s.v)
	       else return WrongArg(3,"a net or a string");
	       )
	  else if length(s) == 2 then (
	       when s.0 
	       is Wid:ZZcell do if !isInt(Wid.v) then return WrongArgSmallInteger(1) else wid = toInt(Wid.v)
	       is Sep:stringCell do if length(Sep.v) == 0 then sep = char(0) else if length(Sep.v) == 1 then sep = Sep.v.0 else return WrongArg(1,"a string of length 0 or 1")
	       else return WrongArg(1,"a string or an integer");
	       when s.1
	       is t:Net do net = t 
	       is s:stringCell do net = toNet(s.v)
	       else return WrongArg(2,"a net or a string");
	       )
	  else return WrongNumArgs(1,3))
     is s:stringCell do net = toNet(s.v)
     is n:Net do net = n
     else return WrongArg("a string or a net");
     if wid <= 0 then return Expr(net);
     Expr(wrap(wid,sep,net)));
setupfun("wrap",wrap);

dillyDallyFun(e:Expr):Expr := (				    -- for debugging interrupts in compiled code
     while true do (
	  sleep(1);
	  if test(interruptedFlag) then return buildErrorPacket("dillyDally: interrupted");
	  ));
setupfun("dillyDally",dillyDallyFun);

minimizeFilename(e:Expr):Expr := (
     when e is s:stringCell do toExpr(minimizeFilename(s.v))
     else WrongArgString()
     );
setupfun("minimizeFilename",minimizeFilename);

relativizeFilename(e:Expr):Expr := (
     when e
     is t:Sequence do
     if length(t) == 2 then
     when t.0 is cwd:stringCell do
     when t.1 is filename:stringCell do toExpr(relativizeFilename(cwd.v,filename.v))
     else WrongArgString(2)
     else WrongArgString(1)
     else WrongArg("a string or a pair of strings")
     is filename:stringCell do toExpr(relativizeFilename(filename.v))
     else WrongArg("a string or a pair of strings"));
setupfun("relativizeFilename",relativizeFilename);

nodecount := 0;
countnodes(n:LexNode):void := (
     when n.word is w:Word do nodecount = nodecount + 1 else nothing;
     when n.next is m:LexNode do countnodes(m) else nothing;
     when n.further is m:LexNode do countnodes(m) else nothing;
     );
countnodes(baseLexNode);
operatorNames := new Sequence len nodecount do provide nullE;
nodecount = 0;
fillnodes(n:LexNode):void := (
     when n.word is w:Word do (
	  operatorNames.nodecount = toExpr(w.name);
	  nodecount = nodecount + 1;
	  ) else nothing;
     when n.next is m:LexNode do fillnodes(m) else nothing;
     when n.further is m:LexNode do fillnodes(m) else nothing;
     );
fillnodes(baseLexNode);
setupconst("operatorNames",Expr(operatorNames));

createSymbol(w:Word, d:Dictionary, s:string):Expr := (
    if !isvalidsymbol(s) then buildErrorPacket("invalid symbol")
    else if d.Protected then (
	buildErrorPacket("attempted to create symbol in protected dictionary"))
    else (
	t := makeSymbol(w, tempPosition, d);
	globalFrame.values.(t.frameindex)));

getglobalsym(d:Dictionary,s:string):Expr := (
     w := makeUniqueWord(s,parseWORD);
     when lookup(w,d.symboltable) is x:Symbol do Expr(SymbolClosure(globalFrame,x))
     is null do createSymbol(w, d, s));

getglobalsym(s:string):Expr := (
     w := makeUniqueWord(s,parseWORD);
     when globalLookup(w)
     is x:Symbol do Expr(SymbolClosure(if x.thread then threadFrame else globalFrame,x))
     is null do createSymbol(w, globalDictionary, s));

getGlobalSymbol(e:Expr):Expr := (
     when e 
     is s:stringCell do getglobalsym(s.v)
     is z:Sequence do if length(z) != 2 then WrongNumArgs(2) else (
	  when z.0
	  is dc:DictionaryClosure do (
	       d := dc.dictionary;
	       if !isglobaldict(d) then WrongArg(1,"a global dictionary") else
	       when z.1
	       is s:stringCell do getglobalsym(d,s.v)
	       else WrongArgString(2)
	       )
	  else WrongArg(1,"a dictionary")
	  )
     else WrongArg("a string or a dictionary and a string"));
setupfun("getGlobalSymbol",getGlobalSymbol);

isglobalsym(s:string):Expr := when globalLookup(makeUniqueWord(s,parseWORD)) is x:Symbol do True is null do False;

isGlobalSymbol(e:Expr):Expr := when e is s:stringCell do isglobalsym(s.v) else WrongArgString();
setupfun("isGlobalSymbol",isGlobalSymbol);

-- history(e:Expr):Expr := (
--      when e
--      is s:Sequence do (
-- 	  if length(s) == 0 then (
-- 	       r := history();
-- 	       Expr(list(new Sequence len length(r) do foreach s in r do provide Expr(s))))
--      	  else WrongNumArgs(0))
--      else WrongNumArgs(0));
-- setupfun("history",history);

listFrame(s:Sequence):Expr := Expr(List(mutableListClass, s, nextHash(), true));	  
listFrame(f:Frame):Expr := if f.frameID == 0 then listFrame(emptySequence) else listFrame(f.values); -- refuse to defeat the protection of global variables
frame(e:Expr):Expr := (
     when e
     is s:Sequence do 
     if length(s) == 0 then Expr(listFrame(localFrame)) else WrongNumArgs(1,2)
     is sc:SymbolClosure do Expr(listFrame(sc.frame))
     is c:PseudocodeClosure do Expr(listFrame(c.frame))
     is fc:FunctionClosure do Expr(listFrame(fc.frame))
     is cfc:CompiledFunctionClosure do Expr(listFrame(cfc.env))
     is CompiledFunction do Expr(listFrame(emptySequence))
     is s:SpecialExpr do frame(s.e)
     else WrongNumArgs(1,2));
setupfun("frame", frame);

numFrames(f:Frame):int := (
     n := 0;
     while ( n = n+1; f != f.outerFrame ) do f = f.outerFrame;
     n);

listFrames(f:Frame):Expr := Expr( list( new Sequence len numFrames(f) do while (provide listFrame(f) ; f != f.outerFrame ) do f = f.outerFrame));     

frames(e:Expr):Expr := (
     when e
    -- # typical value: frames, Sequence, List
     is a:Sequence do if length(a) == 0 then listFrames(localFrame) else WrongNumArgs(0,1) 
    -- # typical value: frames, Symbol, List
     is sc:SymbolClosure do Expr(listFrames(sc.frame))
    -- # typical value: frames, PseudocodeClosure, List
    is c:PseudocodeClosure do Expr(listFrames(c.frame))
    -- # typical value: frames, FunctionClosure, List
     is fc:FunctionClosure do Expr(listFrames(fc.frame))
    -- # typical value: frames, CompiledFunctionClosure, List
     is cfc:CompiledFunctionClosure do Expr(list(listFrame(cfc.env)))
    -- # typical value: frames, CompiledFunction, List
     is CompiledFunction do Expr(list(listFrame(emptySequence)))
     is s:SpecialExpr do frames(s.e)
     else WrongArg("a function, a symbol, or ()"));
setupfun("frames", frames);

localDictionaries(f:Frame):Expr := Expr( list( new Sequence len numFrames(f) do ( while ( provide Expr(localDictionaryClosure(f)); f != f.outerFrame ) do f = f.outerFrame)));

localDictionaries(e:Expr):Expr := (
     when e
    -- # typical value: localDictionaries, Sequence, List
     is x:Sequence do if length(x) != 0 then WrongNumArgs(0,1) else localDictionaries(noRecycle(localFrame))
    -- # typical value: localDictionaries, Dictionary, List
     is x:DictionaryClosure do localDictionaries(x.frame)
    -- # typical value: localDictionaries, Symbol, List
     is x:SymbolClosure do localDictionaries(x.frame)
    -- # typical value: localDictionaries, PseudocodeClosure, List
    is x:PseudocodeClosure do localDictionaries(x.frame)
    -- # typical value: localDictionaries, FunctionClosure, List
     is x:FunctionClosure do localDictionaries(x.frame)
    -- # typical value: localDictionaries, CompiledFunctionClosure, List
     is CompiledFunctionClosure do localDictionaries(emptyFrame)	    -- some values are there, but no symbols
    -- # typical value: localDictionaries, CompiledFunction, List
     is CompiledFunction do localDictionaries(emptyFrame)			    -- no values or symbols are there
     is s:SpecialExpr do localDictionaries(s.e)
     else WrongArg("a function, a symbol, or ()"));
setupfun("localDictionaries", localDictionaries);


-----------------------------------------------------------------------------

globalDictionaryList():Expr := (		    -- get the current globalDictionary list
     g := globalDictionary;
     n := 0;
     while ( n = n+1; g != g.outerDictionary ) do g = g.outerDictionary;
     g = globalDictionary;
     Expr(list(new Sequence len n do while true do ( provide Expr(DictionaryClosure(globalFrame,g)); g = g.outerDictionary ))));

dictionaryPath(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 0 then globalDictionaryList()
     	  else WrongNumArgs(0))
     is t:List do (					    -- set the current globalDictionary list
	  s := t.v;
	  n := length(s);
	  if n == 0 then return WrongArg("a nonempty list of dictionaries");
          sawUnprotected := false;
	  foreach x in s do 
	  when x is dc:DictionaryClosure do (
	       d := dc.dictionary;
	       if !d.Protected then sawUnprotected = true;
	       if d.frameID != 0 || d.transient then return WrongArg("a list of global dictionaries")
	       )
	  else return WrongArg("a list of dictionaries");
	  for i from 0 to n-2 do for j from i+1 to n-1 do if s.i == s.j then return WrongArg("a list of dictionaries with no duplicate entries");
          if !sawUnprotected then return WrongArg("a list of dictionaries, not all protected");
     	  a := new array(Dictionary) len n do foreach x in s do when x is d:DictionaryClosure do provide d.dictionary else nothing;
     	  a.(n-1).outerDictionary = a.(n-1);
     	  for i from 0 to n-2 do a.i.outerDictionary = a.(i+1);
	  globalDictionary = a.0;
	  e)
     else WrongNumArgs(0));
dictionaryPathS := setupvar("dictionaryPath",globalDictionaryList());
setupfun("internalDictionaryPathFunction",dictionaryPath);
storeGlobalDictionaries(e:Expr):Expr := (			    -- called with (symbol,newvalue)
     when e
     is s:Sequence do if length(s) != 2 then WrongNumArgs(2) else (
	  sym := s.0;
	  if !(sym === dictionaryPathS)
	  then buildErrorPacket("global assignment hook encountered unknown symbol")
	  else dictionaryPath(s.1))
     else WrongNumArgs(2));
storeInHashTable(
     globalAssignmentHooks,
     Expr(SymbolBody(dictionaryPathS)),
     Expr(newCompiledFunction(storeGlobalDictionaries)));

getcwdfun(e:Expr):Expr := (				    -- this has to be a function, because getcwd may fail
    when e is s:Sequence do if length(s) != 0 then WrongNumArgs(0) else (
	dir := getcwd();
	if dir != "" then Expr(stringCell(dir))
	else buildErrorPacket("can't get current working directory: " + syserrmsg()))
     else WrongNumArgs(0));
setupfun("currentDirectory",getcwdfun);

changeDirectory(dir:string):Expr := (
    if chdir(expandFileName(dir)) == -1
    then buildErrorPacket(syscallErrorMessage("changing directory"))
    else getcwdfun(emptySequenceE));

changeDirectory(e:Expr):Expr := (
    when e
    is filename:stringCell do changeDirectory(filename.v)
    is a:Sequence do (
	if length(a) == 0
	then changeDirectory("~")
	else WrongArg("a string or ()"))
    else WrongArg("a string or ()"));
setupfun("changeDirectory",changeDirectory);

export debuggerHook := nullE;

backtraceS := dummySymbol;
debugLevelS := dummySymbol;
engineDebugLevelS := dummySymbol;
debuggingModeS := dummySymbol;
errorDepthS := dummySymbol;
gbTraceS := dummySymbol;
numTBBThreadsS := dummySymbol;
numericalAlgebraicGeometryTraceS := dummySymbol;
debuggerHookS := dummySymbol;
lineNumberS := dummySymbol;
allowableThreadsS := dummySymbol;
loadDepthS := dummySymbol;
randomSeedS := dummySymbol;
randomHeightS := dummySymbol;
printingPrecisionS := dummySymbol;
printingAccuracyS := dummySymbol;
printingLeadLimitS := dummySymbol;
printingTrailLimitS := dummySymbol;
printingSeparatorS := dummySymbol;
defaultPrecisionS := dummySymbol;
recursionLimitS := dummySymbol;
nLimitS := dummySymbol;
stopIfErrorS := dummySymbol;
handleInterruptsS := dummySymbol;
printWidthS := dummySymbol;
notifyS := dummySymbol;
readonlyfilesS := dummySymbol;

setupconst("minExponent",toExpr(minExponent));
setupconst("maxExponent",toExpr(maxExponent));

StandardS := makeProtectedSymbolClosure("Standard");
export StandardE := Expr(StandardS);
export topLevelMode := Expr(StandardS);
topLevelModeS := dummySymbol;

initialRandomSeed := zeroZZ;
initialRandomHeight := toInteger(10);

maxAllowableThreadsS := setupvar("maxAllowableThreads",toExpr(0)); -- the value returned by getMaxAllowableThreads may not be initialized yet
export setMaxAllowableThreads():void := (
     setGlobalVariable(maxAllowableThreadsS, toExpr(Ccode(int, "getMaxAllowableThreads()")));
     );

syms := SymbolSequence(
     (  backtraceS = setupvar("backtrace",toExpr(backtrace));  backtraceS  ),
     (  debugLevelS = setupvarThread("debugLevel",toExpr(debugLevel));  debugLevelS  ),
     (  engineDebugLevelS = setupvarThread("engineDebugLevel",toExpr(engineDebugLevel));  engineDebugLevelS  ),
     (  debuggingModeS = setupvarThread("debuggingMode",toExpr(debuggingMode));  debuggingModeS  ),
     (  defaultPrecisionS = setupvar("defaultPrecision",toExpr(defaultPrecision));  defaultPrecisionS  ),
     (  errorDepthS = setupvar("errorDepth",toExpr(errorDepth));  errorDepthS  ),
     (  gbTraceS = setupvar("gbTrace",toExpr(gbTrace));  gbTraceS  ), 
     (  numTBBThreadsS = setupvar("numTBBThreads",toExpr(numTBBThreads));  numTBBThreadsS  ),
     (  numericalAlgebraicGeometryTraceS = setupvar("numericalAlgebraicGeometryTrace",toExpr(numericalAlgebraicGeometryTrace));  numericalAlgebraicGeometryTraceS  ),
     (  debuggerHookS = setupvar("debuggerHook",debuggerHook);  debuggerHookS  ),
     (  lineNumberS = setupvar("lineNumber",toExpr(lineNumber));  lineNumberS  ),
     (  allowableThreadsS = setupvar("allowableThreads",toExpr(Ccode( int, " getAllowableThreads() " )));  allowableThreadsS  ),
     (  loadDepthS = setupvarThread("loadDepth",toExpr(loadDepth));  loadDepthS  ),
     (  printingPrecisionS = setupvar("printingPrecision",toExpr(printingPrecision));  printingPrecisionS  ),
     (  printingAccuracyS = setupvar("printingAccuracy",toExpr(printingAccuracy));  printingAccuracyS  ),
     (  printingLeadLimitS = setupvar("printingLeadLimit",toExpr(printingLeadLimit));  printingLeadLimitS ),
     (  printingTrailLimitS = setupvar("printingTrailLimit",toExpr(printingTrailLimit));  printingTrailLimitS  ),
     (  printingSeparatorS = setupvar("printingSeparator",toExpr(printingSeparator));  printingSeparatorS  ),
     (  randomSeedS = setupvar("randomSeed",toExpr(initialRandomSeed));  randomSeedS  ),
     (  randomHeightS = setupvar("randomHeight",toExpr(initialRandomHeight));  randomHeightS  ),
     (  recursionLimitS = setupvar("recursionLimit",toExpr(recursionLimit));  recursionLimitS  ),
     (  stopIfErrorS = setupvarThread("stopIfError",toExpr(stopIfError));  stopIfErrorS  ),
     (  handleInterruptsS = setupvar("handleInterrupts",toExpr(handleInterrupts));  handleInterruptsS  ),
     (  printWidthS = setupvar("printWidth",toExpr(printWidth));  printWidthS  ),
     (  notifyS = setupvar("notify",toExpr(notify));  notifyS  ),
     (  topLevelModeS = setupvar("topLevelMode",topLevelMode); topLevelModeS  )
     );

export setDebuggingMode(b:bool):void := (
     debuggingMode = b;
     setGlobalVariable(debuggingModeS,toExpr(b));
     );
export setLoadDepth(b:ushort):void := (
     loadDepth = b;
     setGlobalVariable(loadDepthS,toExpr(b));
     );
export setErrorDepth(b:ushort):void := (
     errorDepth = b;
     setGlobalVariable(errorDepthS,toExpr(b));
     );
export setLineNumber(b:int):void := (
     if b < 0 then b = 1;				    -- start over
     lineNumber = b;
     setGlobalVariable(lineNumberS,toExpr(b));
     );

export setAllowableThreadsFun(b:int):void := (
     Ccode( void, "
     {
	 extern void setAllowableThreads(int);
	 setAllowableThreads(", b, ");
     }
     " );
     setGlobalVariable(allowableThreadsS,toExpr(b));
     );
export setstopIfError(b:bool):void := (
     stopIfError = b;
     setGlobalVariable(stopIfErrorS,toExpr(b));
     );
export sethandleInterrupts(b:bool):void := (
     handleInterrupts = b;
     handleInterruptsSetup(b);
     setGlobalVariable(handleInterruptsS,toExpr(b));
     );
threadLocal resetvars := (
     -- These are the thread local variables that got re-initialized in tokens.d:
     -- Actually, this is no good!  If the user assigns to one of these variables, the "top level" version
     -- of the value will be global, even though the bottom level is thread local.
     setGlobalVariable(debuggingModeS,toExpr(debuggingMode));
     setGlobalVariable(debugLevelS,toExpr(debugLevel));
     setGlobalVariable(engineDebugLevelS,toExpr(engineDebugLevel));
     setGlobalVariable(stopIfErrorS,toExpr(stopIfError));
     setGlobalVariable(loadDepthS,toExpr(loadDepth));
     0
     );
msg := "internal assignment hook encountered unknown symbol/value combination";
store(e:Expr):Expr := (			    -- called with (symbol,newvalue)
     when e
     is s:Sequence do if length(s) != 2 then WrongNumArgs(2) else (
	  sym := s.0;
	  if sym === topLevelModeS then (topLevelMode = s.1; e)
	  else if ancestor(Class(s.1),functionClass) then (
	       if sym === debuggerHookS then (debuggerHook = s.1; e)
	       else buildErrorPacket(msg))
	  else when s.1
	  is Nothing do (
	       if sym === debuggerHookS then (debuggerHook = s.1; e)
	       else buildErrorPacket(msg))
	  is b:Boolean do (
	       n := b.v;
	       if sym === debuggingModeS then (debuggingMode = n; e)
	       else if sym === stopIfErrorS then (stopIfError = n; e)
	       else if sym === handleInterruptsS then (handleInterrupts = n; handleInterruptsSetup(n); e)
	       else if sym === backtraceS then (backtrace = n; e)
	       else if sym === notifyS then (notify = n; e)
	       else buildErrorPacket(msg))
	  is s:stringCell do (
	       if sym === printingSeparatorS then (printingSeparator = s.v; e)
	       else buildErrorPacket(msg))
	  is i:ZZcell do (
	       if sym === randomSeedS then ( Ccode(void, "rawSetRandomSeed(", "", i.v, ")"); e )
	       else if sym === randomHeightS then ( Ccode(void, "rawSetRandomMax(", i.v, ")"); e )
	       else if sym === defaultPrecisionS then (
		    if !isULong(i.v) then return buildErrorPacket(msg);
		    prec := toULong(i.v);
		    if prec < minprec || prec > maxprec
		    then return buildErrorPacket("value for defaultPrecision out of range");
		    defaultPrecision = prec;
		    e)
	       else if sym === loadDepthS then (
		    if !isUShort(i.v) then return buildErrorPacket("loadDepth: expected integer in range 0 .. 255");
		    loadDepth = toUShort(i.v);
		    e)
	       else if sym === errorDepthS then (
		    if !isUShort(i.v) then return buildErrorPacket("errorDepth: expected integer in range 0 .. 255");
		    errorDepth = toUShort(i.v);
		    e)
	       else if isInt(i) then (
		    n := toInt(i);
		    if sym === debugLevelS then (debugLevel = n; e)
		    else if sym === engineDebugLevelS then (engineDebugLevel = n; e)
		    else if sym === recursionLimitS then (recursionLimit = n; e)
		    else if sym === lineNumberS then (lineNumber = n; e)
		    else if sym === numTBBThreadsS then (
			 if n < 0 then return buildErrorPacket("numTBBThreads cannot be set to negative value");
             numTBBThreads = n;
             e)
		    else if sym === allowableThreadsS then (
			 if n < 1 || n > Ccode( int, " getMaxAllowableThreads() " ) 
			 then return buildErrorPacket("allowableThreads: expected integer in range 1 .. " + tostring(Ccode( int, " getMaxAllowableThreads() " )));
			 setAllowableThreadsFun(n);
			 e)
		    else if sym === printingPrecisionS then (
			 if n < 0 then return buildErrorPacket("printingPrecision can not be set to negative value");
			 printingPrecision = n;
			 e)
		    else if sym === printingAccuracyS then (
			 if n < -1 then return buildErrorPacket("printingAccuracy can not be set to value < -1");
			 printingAccuracy = n;
			 e)
		    else if sym === printingLeadLimitS then (printingLeadLimit = n; e)
		    else if sym === printingTrailLimitS then (printingTrailLimit = n; e)
		    else if sym === gbTraceS then (gbTrace = n; e)
		    else if sym === numericalAlgebraicGeometryTraceS then (numericalAlgebraicGeometryTrace = n; e)
		    else if sym === printWidthS then (printWidth = n; e)
		    else buildErrorPacket(msg))
	       else buildErrorPacket(
		    if sym === debugLevelS
		    || sym === engineDebugLevelS
		    || sym === lineNumberS
		    || sym === allowableThreadsS
		    || sym === printingPrecisionS
		    || sym === printingAccuracyS
		    || sym === printingLeadLimitS
		    || sym === printingTrailLimitS
		    || sym === gbTraceS
            || sym === numTBBThreadsS
		    || sym === numericalAlgebraicGeometryTraceS
		    || sym === printWidthS
		    then (when sym is s:SymbolClosure do s.symbol.word.name else "") + ": expected a small integer"
		    else msg))
	  else buildErrorPacket(msg))
     else WrongNumArgs(2));
storeE := Expr(newCompiledFunction(store));
foreach s in syms do storeInHashTable(
     globalAssignmentHooks,
     Expr(SymbolBody(s)),
     storeE);
storeE = nullE;
syms = SymbolSequence();

export fileDictionaries := newHashTableWithHash(mutableHashTableClass,nothingClass);
setupconst("fileDictionaries",Expr(fileDictionaries));

export newStaticLocalDictionaryClosure(filename:string):DictionaryClosure := (
     d := newStaticLocalDictionaryClosure();
     storeInHashTable(fileDictionaries,toExpr(relativizeFilename(filename)),Expr(d));
     d);

fileMode(e:Expr):Expr := (
     when e is s:Sequence do (
	  if length(s) != 2 
	  then WrongNumArgs(2) 
	  else (
	       when s.1 
	       is o:file do (
	  	    when s.0
		    is mode:ZZcell do (
	  		 if !isInt(mode)
			 then WrongArgSmallInteger(1)
			 else (
	       		      r := fchmod(o,toInt(mode));
	       		      if r == -1
			      then buildErrorPacket(syscallErrorMessage("fchmod")) 
			      else nullE))
	  	    else WrongArgZZ(1))
	       is filename:stringCell do (
		    when s.0 
		    is mode:ZZcell do (
			 if !isInt(mode)
			 then WrongArgSmallInteger(1)
			 else (
			      r := chmod(expandFileName(filename.v),toInt(mode));
			      if r == -1
			      then buildErrorPacket(syscallErrorMessage("chmod"))
			      else nullE))
		    else WrongArgZZ(1))
	       else WrongArg(2,"a file")))
     is f:file do (
	  fd := -1;
	  if f.input then fd = f.infd
	  else if f.output then fd = f.outfd
	  else if f.listener then fd = f.listenerfd
	  else return WrongArg("an open file");
	  r := fileModeFD(fd);
	  if r == -1 then buildErrorPacket(syscallErrorMessage("fstat"))
	  else toExpr(r))
     is fn:stringCell do (
	  r := fileMode(expandFileName(fn.v));
	  if r == -1 then buildErrorPacket(syscallErrorMessage("stat"))
	  else toExpr(r))
     else WrongArg("string, integer and string or file"));
setupfun("fileMode",fileMode);

rawRandomInitializeFun(e:Expr):Expr := (
     when e is s:Sequence do if length(s) == 0 then (
	  Ccode(void, "rawRandomInitialize()");
	  nullE
	  )
     else WrongNumArgs(0)
     else WrongNumArgs(0));
setupfun("rawRandomInitialize",rawRandomInitializeFun);

recursionDepthFun(e:Expr):Expr := (
     when e is s:Sequence do if length(s) == 0 then toExpr(recursionDepth)
     else WrongNumArgs(0)
     else WrongNumArgs(0));
setupfun("recursionDepth",recursionDepthFun);

--does this make sense in the world of threaded files?  How do you define the output length in thread exclusive mode?  
fileLength(e:Expr):Expr := (
     when e
     is f:file do (
	  if f.input && f.infd != -1 then (
	       ret := fileLength(f.infd);
	       if ret == ERROR
	       then Expr(buildErrorPacket(syscallErrorMessage("getting the length of a file")))
	       else toExpr(ret))
	  else if f.output then (
	       foss := getFileFOSS(f);
	       r := toExpr(foss.bytesWritten + foss.outindex);
	       releaseFileFOSS(f);
	       r)
     	  else buildErrorPacket("file not open"))
     is f:stringCell do (
	  filename := f.v;
	  filename = expandFileName(filename);
	  ret := fileLength(filename);
	  if ret == ERROR
	  then Expr(buildErrorPacket(syscallErrorMessage("length of a file: \"" + present(filename) + "\"")))
     	  else toExpr(ret))
     else WrongArg("a string or a file"));     
setupfun("fileLength",fileLength);

functionBody(e:Expr):Expr := (
     when e is f:FunctionClosure do Expr(f.model)
     is f:CompiledFunction do e
     is f:CompiledFunctionClosure do Expr(CompiledFunctionBody(f.fn))
     is s:SpecialExpr do functionBody(s.e)
     else WrongArg("a function")
     );
setupfun("functionBody",functionBody);

symbolBody(e:Expr):Expr := when e is s:SymbolClosure do Expr(SymbolBody(s.symbol)) else WrongArg("a symbol");
setupfun("symbolBody",symbolBody);

dumpNodes(e:Expr):Expr := (dumpNodes(); nullE);
setupfun("dumpNodes",dumpNodes);

toExternalString(e:Expr):Expr := (
     when e
     is x:RRcell do toExpr(toExternalString(x.v))
     is x:RRicell do toExpr(toExternalString(x.v))
     is x:CCcell do toExpr(toExternalString(x.v))
     else WrongArg("a real or complex number")
     );
setupfun("toExternalString0",toExternalString);

header "
/* added in bdwgc 8 */
#if GC_VERSION_MAJOR < 8
unsigned long GC_get_full_gc_total_time(void) {return 0;}
#endif
#define DEF_GC_FN0(s)	static void * s##_0(void *client_data) { (void) client_data; return (void *) (long) s(); }
DEF_GC_FN0(GC_get_full_gc_total_time)
DEF_GC_FN0(GC_get_free_space_divisor)
";

export gcTime():double := Ccode(double, "0.001 * (unsigned long) GC_call_with_alloc_lock(GC_get_full_gc_total_time_0, NULL)");

GCstats(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 0 then (
	  h := newHashTable(hashTableClass,nothingClass);
	  h.beingInitialized = true;
	  foreach eq in envp do if match(eq,0,"GC_") then (
	       j := index(eq,0,'=');
	       if j != -1 then (
		    keyS := substr(eq,0,j);
		    valS := substr(eq,j+1);
		    storeInHashTable(h,toExpr(keyS),toExpr(valS))));
	  Ccode(void, "
	       struct GC_prof_stats_s stats;
	       GC_get_prof_stats(&stats, sizeof stats);");
	  -- M2 developers may (possibly) care about e.g.: GC_LOG_FILE GC_FIND_LEAK GC_IGNORE_GCJ_INFO GC_LARGE_ALLOC_WARN_INTERVAL
	  --      GC_RETRY_SIGNALS GC_UNMAP_THRESHOLD GC_FORCE_UNMAP_ON_GCOLLECT GC_BACKTRACES GC_PRINT_ADDRESS_MAP GC_DONT_GC
	  --      GC_USE_ENTIRE_HEAP GC_TRACE etc.
	  storeInHashTable(h,toExpr("heapSize"),toExpr(Ccode(long, "stats.heapsize_full - stats.unmapped_bytes")));
	  storeInHashTable(h,toExpr("bytesAlloc"),toExpr(Ccode(long, "stats.bytes_allocd_since_gc + stats.allocd_bytes_before_gc")));
	  storeInHashTable(h,toExpr("numGCs"),toExpr(Ccode(long, "stats.gc_no")));
	  storeInHashTable(h,toExpr("numGCThreads"),toExpr(Ccode(long, "stats.markers_m1 + 1")));
	  storeInHashTable(h,toExpr("gcCpuTimeSecs"),toExpr(gcTime()));
	  storeInHashTable(h,toExpr("GC_free_space_divisor"),	-- the set value in memory
	       toExpr(Ccode(long, "(long) GC_call_with_alloc_lock(GC_get_free_space_divisor_0, NULL)")));
	  Expr(sethash(h,false)))
     else WrongNumArgs(0)
     else WrongNumArgs(0));
setupfun("GCstats",GCstats);

header "extern void set_gftable_dir(char *); /* defined in library factory, as patched by us */";
setFactoryGFtableDirectory(e:Expr):Expr := (
     when e is d:stringCell do (
     	  Ccode(void,"set_gftable_dir(", tocharstar(d.v), ")");
	  nullE)
     else WrongArgString());
setupfun("setFactoryGFtableDirectory",setFactoryGFtableDirectory);

serialNumber(e:Expr):Expr := (
     when e 
     is s:SymbolClosure do toExpr(s.symbol.serialNumber)
     is o:HashTable do if o.Mutable then toExpr(o.hash) else WrongArg("hash table to be mutable")
     is o:List do if o.Mutable then toExpr(o.hash) else WrongArg("list to be mutable")
     is o:DictionaryClosure do toExpr(o.dictionary.hash)
     is t:TaskCell do toExpr(t.body.serialNumber)
     else WrongArg("a symbol or a mutable hash table or list"));
setupfun("serialNumber",serialNumber);

header "extern void TS_Test();";
threadTest(e:Expr):Expr := (
     Ccode(void, "TS_Test()");
     nullE);
setupfun("threadTest",threadTest);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d actors5.o "
-- End:
