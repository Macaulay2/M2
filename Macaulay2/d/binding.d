--		Copyright 1994 by Daniel R. Grayson
use system;
use stdio;
use arith;
use nets;
use tokens;
use strings;
use parser;
use lex;
use ctype;
use err;
use stdiop;

export defaultparsefuns := parsefuns(defaultunary, defaultbinary);
foreach p in array(parseinfo)(
     parseEOL, parseEOF, parseERRMSG, parseWORD
     ) do p.funs = defaultparsefuns;
prec := 0;
step := 2;
bump():void := prec = prec + step;
bump(i:int):void := prec = prec + i*step;
unary(s:string):Word := (
     install(s,unique(s,parseinfo(prec,prec,prec,
		    parsefuns(unaryop,defaultbinary)))));
unaryword(s:string):Word := (
     unique(s,parseinfo(prec,prec,prec,
		    parsefuns(unaryop,defaultbinary))));
biunary(s:string):Word := (
     install(s,unique(s,parseinfo(prec,prec,prec,
		    parsefuns(unaryop,postfixop)))));
postfix(s:string):Word := (
     install(s,unique(s,parseinfo(prec,prec,prec,
		    parsefuns(errorunary,postfixop)))));
unaryleft(s:string):Word := (
     install(s,unique(s,parseinfo(prec,prec,prec,
		    parsefuns(unaryop,binaryop))))
     );
unaryright(s:string):Word := (
     install(s,unique(s,parseinfo(prec,prec-step,prec,
		    parsefuns(unaryop,binaryop))))
     );
binaryleft(s:string):Word := (
     install(s,unique(s,parseinfo(prec,prec,prec,
		    parsefuns(errorunary,binaryop)))));
binaryleftword(s:string):Word := (
     unique(s,
	  parseinfo(prec,prec,prec, parsefuns(errorunary,binaryop))));
nleft(s:string):Word := (
     install(s,unique(s,parseinfo(prec,prec,prec,
		    parsefuns(errorunary,nbinaryop)))));
nleftword(s:string):Word := (
     unique(s,parseinfo(prec,prec,prec,
		    parsefuns(errorunary,nbinaryop))));
nunaryleft(s:string):Word := (
     install(s,unique(s,parseinfo(prec,prec,prec,
		    parsefuns(nunaryop,nbinaryop)))));
binaryright(s:string,binary:function(ParseTree,Token,TokenFile,int,bool):ParseTree):Word := (
     install(s,unique(s,parseinfo(prec,prec-step,prec,
		    parsefuns(errorunary,binary)))));
binaryright(s:string):Word := binaryright(s,binaryop);
specialprec := -1;			  -- filled in below
export special(s:string,f:function(Token,TokenFile,int,bool):ParseTree):Word := (
     unique(s, parseinfo(precObject,specialprec,specialprec,
	       parsefuns(f, defaultbinary))));
token(s:string):Word := unique(s,
     parseinfo(prec,prec,prec, parsefuns(errorunary,errorbinary)));
export parens(binaryleft:string,binaryright:string,prec:int,scope:int,strength:int):Word := (
     l := unique(binaryleft, parseinfo(prec,scope,strength,leftparen));
     binaryleft = l.name;
     install(binaryleft,l);
     r := unique(binaryright,parseinfo(scope,0,0,rightparen));
     binaryright = r.name;
     install(binaryright,r);
     addmatch(binaryleft,binaryright);
     l
     );

HadError := false;
unknown := Position("--unknown position--",ushort(0),uchar(0),uchar(reloaded));
export treePosition(e:ParseTree):Position := (
     while true do (
	  when e
	  is dummy do return(dummyPosition)
	  is token:Token do return(token.position)
	  is adjacent:Adjacent do e = adjacent.lhs
	  is binary:Binary do return(binary.operator.position)
	  is a:Arrow do return(a.operator.position)
	  is unary:Unary do return(unary.operator.position)
	  is postfix:Postfix do return(postfix.operator.position)
	  is a:Quote do return(a.operator.position)
	  is a:GlobalQuote do return(a.operator.position)
	  is a:LocalQuote do return(a.operator.position)
	  is ee:parenthesized do return(ee.left.position)
	  is ee:parentheses do return(ee.left.position)
     	  is i:IfThen do return(i.iftoken.position)
	  is i:TryElse do return(i.trytoken.position)
	  is i:Try do return(i.trytoken.position)
     	  is i:IfThenElse do return(i.iftoken.position)
     	  is w:While do return(w.whiletoken.position)
     	  is s:startScope do e = s.body
	  is n:New do return(n.newtoken.position)
	  )
     );
export makeErrorTree(e:ParseTree,message:string):void := (
     HadError = true;
     errorpos(treePosition(e),message);
     );
export makeErrorTree(e:Token,message:string):void := (
     HadError = true;
     errorpos(e.position,message);
     );
append(hashTable:SymbolHashTable,entry:Symbol):void := (
     h := entry.word.hash & (length(hashTable)-1);
     when hashTable.h
     is null do hashTable.h = SymbolListCell(entry,NULL)
     is e:SymbolListCell do (
	  while true do (
	       when e.next 
	       is f:SymbolListCell do e = f
	       is null do (
		    e.next = SymbolListCell(entry,NULL);
		    break))));
enlarge(dictionary:Dictionary):void := (
     newTable := newSymbolHashTable(2*length(dictionary.hashTable));
     foreach e in dictionary.hashTable do (
	  entryList := e;
	  while true do
	  when entryList
	  is null do break
	  is entryListCell:SymbolListCell do (
	       append(newTable, entryListCell.entry);
	       entryList = entryListCell.next;
	       )
	  );
     dictionary.hashTable = newTable;
     );
insert(entry:Symbol,dictionary:Dictionary):void := (
     dictionary.numEntries = dictionary.numEntries + 1;
     if 3 * dictionary.numEntries > 2 * length(dictionary.hashTable) + 1
     then enlarge(dictionary);
     h := entry.word.hash & (length(dictionary.hashTable)-1);
     dictionary.hashTable.h = SymbolListCell(entry,dictionary.hashTable.h);
     );
export makeSymbol(word:Word,position:Position,scope:Scope):Symbol := (
     frameindex := scope.framesize;
     scope.framesize = scope.framesize + 1;
     entry := Symbol(
	  word, 
	  nextHash(), 
	  position,
	  dummyUnaryFun,dummyPostfixFun,dummyBinaryFun,
	  scope.seqno, 
	  frameindex,
	  1,				  -- first lookup is now
	  false,			  -- not protected
	  scope.transient,
	  false
	  );
     if scope == globalScope then (
	  -- this allows the global frame to grow
	  if globalScope.framesize > length(globalFrame.values) then (
	       globalFrame.values = (
		    new Sequence len 2 * globalScope.framesize + 1 do (
			 foreach y in globalFrame.values do provide y;
			 while true do provide nullE;
			 )));
	  globalFrame.values.frameindex = Expr(SymbolClosure(globalFrame,entry));
	  )
     else if scope.seqno == localFrame.scopenum then (
	  -- this should take care of scopes which span a file,
	  -- and have a single frame which ought to be allowed to grow
	  if scope.framesize > length(localFrame.values) then (
	       localFrame.values = (
		    new Sequence len 2 * scope.framesize + 1 do (
			 foreach y in localFrame.values do provide y;
			 while true do provide nullE;
			 )));
	  localFrame.values.frameindex = nullE;
	  );
     insert(entry,scope.dictionary);
     entry
     );
export makeProtectedSymbolClosure(w:Word):SymbolClosure := (
     entry := makeSymbol(w,dummyPosition,globalScope);
     entry.protected = true;
     SymbolClosure(globalFrame,entry));
export makeProtectedSymbolClosure(s:string):SymbolClosure := (
     makeProtectedSymbolClosure(unique(s,parseWORD))
     );

bump();
     export pEOF := prec;
     parseEOF.precedence = pEOF;
     parseEOF.scope = pEOF;
bump();
     pRPAREN := prec;
bump(2);
     semicolonW = nleft(";");
     export semicolonS := makeProtectedSymbolClosure(semicolonW);
     newlineW = nleftword("--newline--");
     export newlineS := makeProtectedSymbolClosure(newlineW);
bump();
     export commaW := nunaryleft(",");
bump(2);
     specialprec = prec;
     doW = token("do");
     elseW = token("else");
     thenW = token("then");
bump();
     export ColonEqualW := binaryright(":=");
     export EqualW := binaryright("=");
     export LeftArrowW := binaryright("<-");
     export RightArrowW := binaryright("->",arrowop);
     makeProtectedSymbolClosure(RightArrowW);
     export DoubleArrowS := makeProtectedSymbolClosure(binaryright("=>"));
     export LongDoubleArrowS := makeProtectedSymbolClosure(binaryright("==>"));
bump();
     newscope := prec;
     ofW = token("of");
     fromW = token("from");
bump();
     export LessLessS := makeProtectedSymbolClosure(unaryleft("<<"));
bump();
     export GreaterGreaterS := makeProtectedSymbolClosure(binaryright(">>"));
bump();
     export orS := makeProtectedSymbolClosure(binaryleftword("or"));
bump();
     export andS := makeProtectedSymbolClosure(binaryleftword("and"));
bump();
     export notS := makeProtectedSymbolClosure(unaryword("not"));
bump();
     export LessS := makeProtectedSymbolClosure(binaryleft("<"));
     export GreaterS := makeProtectedSymbolClosure(binaryleft(">"));
     export LessEqualS := makeProtectedSymbolClosure(binaryleft("<="));
     export GreaterEqualS := makeProtectedSymbolClosure(binaryleft(">="));
     export EqualEqualEqualS := makeProtectedSymbolClosure(binaryleft("==="));
     export EqualEqualS := makeProtectedSymbolClosure(binaryleft("=="));
     export QuestionS := makeProtectedSymbolClosure(binaryleft("?"));
     export NotEqualEqualEqualS := makeProtectedSymbolClosure(binaryleft("=!="));
     export NotEqualS := makeProtectedSymbolClosure(binaryleft("!="));
bump();
     export BarBarS := makeProtectedSymbolClosure(binaryleft("||"));
bump();
     export AmpersandAmpersandS := makeProtectedSymbolClosure(binaryleft("&&"));
bump();
     export ColonS := makeProtectedSymbolClosure(binaryright(":"));
bump();
     -- export ColonColonS := makeProtectedSymbolClosure(binaryleft("::"));
     export ColonColonS := ColonS;
bump();
     export BarS := makeProtectedSymbolClosure(binaryleft("|"));
bump();
     export HatHatS := makeProtectedSymbolClosure(binaryleft("^^"));
bump();
     export AmpersandS := makeProtectedSymbolClosure(binaryleft("&"));
bump();
     export TildeS := makeProtectedSymbolClosure(postfix("~"));
bump();
     export DotDotS := makeProtectedSymbolClosure(binaryleft(".."));
bump();
     export MinusS := makeProtectedSymbolClosure(unaryleft("-"));
     export PlusS := makeProtectedSymbolClosure(binaryleft("+"));
     export PlusPlusS := makeProtectedSymbolClosure(binaryleft("++"));
     export pPLUS := prec;
bump();
     export StarStarS := makeProtectedSymbolClosure(binaryleft("**"));
bump();
     export BracketS := makeProtectedSymbolClosure(parens("[","]",prec,pRPAREN,prec-1));
bump();
     export BackslashBackslashS := makeProtectedSymbolClosure(binaryright("\\\\"));
bump();
     export StarS := makeProtectedSymbolClosure(unaryleft("*"));
     export DivideS := makeProtectedSymbolClosure(binaryleft("/"));
     export LeftDivideS := makeProtectedSymbolClosure(binaryright("\\"));
     export PercentS := makeProtectedSymbolClosure(binaryleft("%"));
     export SlashSlashS := makeProtectedSymbolClosure(binaryleft("//"));
bump();
     export SharpSharpS := makeProtectedSymbolClosure(binaryright("##"));
     export AtS := makeProtectedSymbolClosure(binaryright("@"));
bump();
     export AdjacentS:=makeProtectedSymbolClosure(binaryright(" "));

     precObject = prec;
     parseWORD.precedence = prec;
     parseWORD.scope = prec;
     parseWORD.strength = prec;

     special("if",unaryif);
     special("try", unarytry);

     unique("new", parseinfo(precObject,newscope,newscope, 
	       parsefuns(unarynew, defaultbinary)));

     special("while",unarywhile);

     special("symbol",unaryquote);
     special("global",unaryglobal);
     special("local",unarylocal);

     export paren := parens("(",")",prec,pRPAREN,prec);
     export brace := parens("{","}",prec,pRPAREN,prec);
     export keywordprec := prec;
bump();
     export AtAtS := makeProtectedSymbolClosure(binaryleft("@@"));
bump();
     export SlashHatS := makeProtectedSymbolClosure(binaryleft("/^"));
     export PowerS := makeProtectedSymbolClosure(binaryleft("^"));
     export UnderscoreS := makeProtectedSymbolClosure(binaryleft("_"));
     export SharpS := makeProtectedSymbolClosure(unaryleft("#"));
     SharpS.symbol.word.parse.strength = precObject-step;
     export SharpQuestionS := makeProtectedSymbolClosure(binaryleft("#?"));
     export DotS := makeProtectedSymbolClosure(binaryleft("."));
     export DotQuestionS := makeProtectedSymbolClosure(binaryleft(".?"));
bump();
     export ExclamationS := makeProtectedSymbolClosure(postfix("!"));
bump();
     paren.parse.strength = prec;
     BracketS.symbol.word.parse.strength = prec;
     brace.parse.strength = prec;
     parseEOL.precedence = prec;
     parseEOL.scope = prec;

export GlobalAssignS := makeProtectedSymbolClosure("GlobalAssignHook");
export GlobalAssignE := Expr(GlobalAssignS);

export GlobalReleaseS := makeProtectedSymbolClosure("GlobalReleaseHook");
export GlobalReleaseE := Expr(GlobalReleaseS);

export EqualE := Expr(EqualEqualS);
export LessE := Expr(LessS);
export GreaterE := Expr(GreaterS);

export NewS := makeProtectedSymbolClosure("NewMethod");
export NewE := Expr(NewS);

export NewOfS := makeProtectedSymbolClosure("NewOfMethod");
export NewOfE := Expr(NewOfS);

export NewFromS := makeProtectedSymbolClosure("NewFromMethod");
export NewFromE := Expr(NewFromS);

export NewOfFromS := makeProtectedSymbolClosure("NewOfFromMethod");
export NewOfFromE := Expr(NewOfFromS);

export InverseS := makeProtectedSymbolClosure("InverseMethod");
export InverseE := Expr(InverseS);

export makeSymbol(token:Token):Symbol := (
     e := makeSymbol(token.word,token.position,token.scope);
     token.entry = e;
     e);
makeSymbol(e:ParseTree,scope:Scope):void := (
     when e
     is token:Token do (
	  token.scope = scope;
	  makeSymbol(token);)
     else makeErrorTree(e,"expected single identifier"));
-----------------------------------------------------------------------------
cleanGlobalFrame():void := globalFrame.values = emptySequence;
atend(cleanGlobalFrame);
-----------------------------------------------------------------------------
lookupCountIncrement := 1;
lookup(word:Word,dictionary:Dictionary):(null or Symbol) := (
     if dictionary == dummyDictionary then error("dummy dictionary used");
     entryList := dictionary.hashTable.(
	  word.hash & (length(dictionary.hashTable)-1)
	  );
     while true do
     when entryList
     is null do return(NULL)
     is entryListCell:SymbolListCell do (
	  if entryListCell.entry.word == word 
	  then (
	       e := entryListCell.entry;
	       e.lookupCount = e.lookupCount + lookupCountIncrement;
	       return(e);
	       );
	  entryList = entryListCell.next));
lookup(
     word:Word,criterion:function(Symbol):bool,dictionary:Dictionary
     ):(null or Symbol) := (
     if dictionary == dummyDictionary then error("dummy dictionary used");
     entryList := dictionary.hashTable.(
	  word.hash & (length(dictionary.hashTable)-1)
	  );
     while true do
     when entryList
     is null do return(NULL)
     is entryListCell:SymbolListCell do (
	  if entryListCell.entry.word == word
	  then if criterion(entryListCell.entry)
	  then (
	       e := entryListCell.entry;
	       e.lookupCount = e.lookupCount + lookupCountIncrement;
	       return(e);
	       );
	  entryList = entryListCell.next));
export lookup(word:Word,scope:Scope):(null or Symbol) := (
     while true do (
	  when lookup(word,scope.dictionary)
	  is null do (
	       if scope.outerScope == scope 
	       then return(NULL)
	       else scope = scope.outerScope;
	       )
	  is e:Symbol do return(e)));
lookup(
     word:Word,
     criterion:function(Symbol):bool,  	  -- used for overloading
     scope:Scope
     ):(null or Symbol) := (
     while true do (
	  when lookup(word,criterion,scope.dictionary)
	  is null do (
	       if scope.outerScope == scope
	       then return(NULL)
	       else scope = scope.outerScope
	       )
	  is e:Symbol do return(e)));
lookup(token:Token,forcedef:bool):void := (
     n := length(token.word.name);
     if n >= 1 && isdigit(token.word.name.0) 
     || n >= 2 && token.word.name.0 == '.' && isdigit(token.word.name.1)
     then nothing
     else (
     	  when lookup(token.word,token.scope)
     	  is entry:Symbol do (
	       token.entry = entry;
	       if entry.flagLookup then (
		    errorpos(token.position,"flagged symbol encountered");
		    );
	       )
     	  else (
	       if forcedef
	       then (
	       	    token.scope = globalScope;
	       	    makeSymbol(token);
		    )
	       else (
	       	    errorpos(token.position,"undefined token " + token.word.name);
	       	    HadError=true;))));
lookup(token:Token):void := lookup(token,true);
lookuponly(token:Token):void := lookup(token,false);
-----------------------------------------------------------------------------
bind(token:Token,scope:Scope):void := (
     token.scope = scope;
     lookup(token););
bindop(token:Token,scope:Scope):void := (
     token.scope = scope;
     lookuponly(token););
export bind(e:ParseTree,scope:Scope):void;
bindFormalParm(e:ParseTree,scope:Scope,desc:functionDescription):void := (
     when e
     is Token do (
	  makeSymbol(e,scope);
	  desc.numparms = desc.numparms + 1;
	  )
     else makeErrorTree(e,"syntax error"));
bindFormalParmList(e:ParseTree,scope:Scope,desc:functionDescription):void := (
     when e 
     is binary:Binary do (
	  if binary.operator.word == commaW
	  then (
	       bindFormalParmList(binary.lhs,scope,desc);
	       bindop(binary.operator,scope);
	       bindFormalParm(binary.rhs,scope,desc);)
	  else bindFormalParm(e,scope,desc))
     else bindFormalParm(e,scope,desc));
bindParenParmList(e:ParseTree,scope:Scope,desc:functionDescription):void := (
     when e 
     is t:Token do (
	  bindFormalParm(e,scope,desc);
	  desc.restargs = true;
	  )
     is p:parenthesized do (
	  bindFormalParmList(p.contents,scope,desc)
	  )
     is p:parentheses do nothing
     else makeErrorTree(e,"expected parenthesized argument list or symbol"));

export opsWithBinaryMethod := array(SymbolClosure)(
     LessLessS, GreaterGreaterS, EqualEqualS, QuestionS, BarBarS, LongDoubleArrowS,
     AmpersandAmpersandS, ColonS, BarS, HatHatS, AmpersandS, DotDotS, MinusS, PlusS, PlusPlusS,
     StarStarS, StarS, BackslashBackslashS, DivideS, LeftDivideS, PercentS, SlashSlashS, AtS, 
     AdjacentS, AtAtS, SlashHatS, PowerS, UnderscoreS);
export opsWithUnaryMethod := array(SymbolClosure)( StarS, MinusS, LessLessS );
export opsWithPostfixMethod := array(SymbolClosure)( TildeS );

opHasBinaryMethod(o:Symbol):bool := (
     foreach s in opsWithBinaryMethod do if s.symbol == o then return(true);
     return(false);
     );
opHasUnaryMethod(o:Symbol):bool := (
     foreach s in opsWithUnaryMethod do if s.symbol == o then return(true);
     return(false);
     );
opHasPostfixMethod(o:Symbol):bool := (
     foreach s in opsWithPostfixMethod do if s.symbol == o then return(true);
     return(false);
     );
bindassignment(assn:Binary,scope:Scope,colon:bool):void := (
     bindop(assn.operator,scope);
     body := assn.rhs;
     when assn.lhs
     is token:Token do (
	  if colon 
	  then (
	       lookupCountIncrement = 0;
	       r := lookup(token.word,scope);
	       lookupCountIncrement = 1;
	       when r
	       is entry:Symbol do (
		    if scope.seqno == entry.scopenum
		    then errorpos(token.position,
			 "warning: local declaration of " 
			 + token.word.name
			 + " shields variable with same name"
			 );
		    )
	       else nothing;
	       token.scope = scope;
	       makeSymbol(token);
	       )
	  else bind(token,scope);
	  bind(body,scope);)
     is a:Adjacent do (
	  bind(a.lhs,scope);
	  bind(a.rhs,scope);
	  bind(body,scope);
	  )
     is unary:Unary do (
	  bindop(unary.operator,scope);
	  bind(unary.rhs,scope);
	  bind(body,scope);
	  if colon
	  then (
	       if ! opHasUnaryMethod(unary.operator.entry)
	       then makeErrorTree(assn.operator, "can't assign a method for this unary operator");
	       )
	  else (
	       if ! opHasUnaryMethod(unary.operator.entry)
	       then makeErrorTree(assn.operator, "can't assign a value for this unary operator")
	       )
	  )
     is unary:Postfix do (
	  bind(unary.lhs,scope);
	  bindop(unary.operator,scope);
	  bind(body,scope);
	  if colon
	  then (
	       if ! opHasPostfixMethod(unary.operator.entry)
	       then makeErrorTree(assn.operator, "can't assign a method for this postfix operator");
	       )
	  else (
	       if ! opHasPostfixMethod(unary.operator.entry)
	       then makeErrorTree(assn.operator, "cna't assign a value for this postfix operator")
	       )
	  )
     is binary:Binary do (
	  bind(binary.lhs,scope);
	  bindop(binary.operator,scope);
	  bind(binary.rhs, if binary.operator.word == DotS.symbol.word then globalScope else scope );
	  bind(body,scope);
	  if colon then (
	       if ! opHasBinaryMethod(binary.operator.entry)
	       then makeErrorTree( assn.operator, "can't assign a method for this binary operator");
	       )
	  else (
	       if !(binary.operator.word == DotS.symbol.word
		    || 
		    binary.operator.word == SharpS.symbol.word
		    ||
		    opHasBinaryMethod(binary.operator.entry))
	       then makeErrorTree( assn.operator, "can't assign a value for this binary operator");
	       )
	  )
     is n:New do (
	  if colon then (
	       bind(n.newclass,scope);
	       bind(n.newparent,scope);
	       bind(n.newinitializer,scope);
	       bind(body,scope))
	  else makeErrorTree(assn.operator, 
	       "left hand side of assignment inappropriate"))
     else makeErrorTree(assn.operator, 
	  "left hand side of assignment inappropriate"));
bindnewscope(e:ParseTree,scope:Scope):ParseTree := (
     n := newScope(scope);
     bind(e,n);
     ParseTree(startScope(n,e)));
SawClosure := false;
export bind(e:ParseTree,scope:Scope):void := (
     when e
     is s:startScope do bind(s.body,scope)
     is i:IfThen do (
	  bind(i.predicate,scope);
	  -- i.thenclause = bindnewscope(i.thenclause,scope);
	  bind(i.thenclause,scope);
	  )
     is i:IfThenElse do (
	  bind(i.predicate,scope);
	  -- i.thenclause = bindnewscope(i.thenclause,scope);
	  bind(i.thenclause,scope);
	  -- i.elseclause = bindnewscope(i.elseclause,scope);
	  bind(i.elseclause,scope);
	  )
     is token:Token do (
	  if token.word.typecode == TCid then bind(token,scope);
	  )
     is adjacent:Adjacent do (
	  bind(adjacent.lhs,scope); 
	  bind(adjacent.rhs,scope))
     is binary:Binary do (
	  if binary.operator.word == EqualW
	  then bindassignment(binary,scope,false)
	  else if binary.operator.word == ColonEqualW
	  then bindassignment(binary,scope,true)
	  else if binary.operator.word == orS.symbol.word
	       || binary.operator.word == andS.symbol.word
	  then (
	       bind(binary.lhs,scope);
	       bindop(binary.operator,scope);
	       -- binary.rhs = bindnewscope(binary.rhs,scope);
	       bind(binary.rhs,scope);
	       )
	  else if binary.operator.word == DotS.symbol.word
	  then (
	       bind(binary.lhs,scope);
	       bindop(binary.operator,scope);
	       bind(binary.rhs,globalScope);
	       when binary.rhs
	       is token:Token do (
		    if token.word.typecode != TCid
		    then makeErrorTree(binary.operator, "expected a symbol to right of '.'" );
		    )
	       else makeErrorTree(binary.operator, "expected a symbol to right of '.'" );
	       )
	  else if binary.operator.word == DotQuestionS.symbol.word
	  then (
	       bind(binary.lhs,scope);
	       bindop(binary.operator,scope);
	       bind(binary.rhs,globalScope);
	       when binary.rhs
	       is token:Token do (
		    if token.word.typecode != TCid
		    then makeErrorTree(binary.operator, "expected a symbol to right of '.?'" );
		    )
	       else makeErrorTree(binary.operator, "expected a symbol to right of '.?'" );
	       )
	  else (
	       bind(binary.lhs,scope);
	       bindop(binary.operator,scope);
	       bind(binary.rhs,scope);
	       );
	  )
     is q:LocalQuote do (
	  bind(q.operator,scope);
	  tok := q.rhs;
	  tok.scope = scope;
	  r := lookup(tok.word,scope.dictionary);
	  when r
	  is entry:Symbol do ( tok.entry = entry; )
	  else ( makeSymbol(tok); );
	  SawClosure = true; 
	  )
     is q:GlobalQuote do (
	  bind(q.operator,scope);
	  bind(q.rhs,globalScope);
	  )
     is q:Quote do (
	  bind(q.operator,scope);
	  bind(q.rhs,scope);
	  if q.rhs.entry.scopenum != globalFrame.scopenum 
	  then SawClosure = true; 
	  )
     is a:Arrow do (
	  SawClosure = false;
	  newscop := newScope(scope);
	  a.desc = functionDescription(newscop.seqno,0,0,false,false);
	  bindParenParmList(a.lhs,newscop,a.desc);
	  bind(a.rhs,newscop);
	  a.desc.framesize = newscop.framesize;
	  a.desc.hasClosure = SawClosure;
	  SawClosure = true; )
     is unary:Unary do (
	  bindop(unary.operator,scope);
	  bind(unary.rhs,scope);)
     is postfix:Postfix do (
	  bind(postfix.lhs,scope);
	  bindop(postfix.operator,scope);)
     is ee:parenthesized do bind(ee.contents,scope)
     is parentheses do nothing
     is dummy do nothing
     is w:While do (
	  bind(w.predicate,scope);
	  -- w.body = bindnewscope(w.body,scope);
	  bind(w.body,scope);
	  )
     is n:New do (
     	  bind(n.newclass,scope);
     	  bind(n.newparent,scope);
     	  bind(n.newinitializer,scope);)
     is i:TryElse do (
	  -- i.primary = bindnewscope(i.primary,scope);
	  bind(i.primary,scope);
	  -- i.alternate = bindnewscope(i.alternate,scope);
	  bind(i.alternate,scope);
	  )
     is i:Try do (
	  bind(i.primary,scope);
	  )
     );
export bind(e:ParseTree):bool := (
     HadError = false;
     bind(e,globalScope);
     !HadError
     );
export localBind(e:ParseTree,scope:Scope):bool := (
     HadError = false;
     bind(e,scope);
     !HadError
     );
