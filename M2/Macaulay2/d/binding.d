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

-----------------------------------------------------------------------------
-- first, the global symbol dictionary and functions for making symbols
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
export makeEntry(word:Word,position:Position,scope:Scope):Symbol := (
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
			 ))))
     else if scope.seqno == localFrame.scopenum then (
	  -- this should take care of scopes which span a file,
	  -- and have a single frame which ought to be allowed to grow
	  if scope.framesize > length(localFrame.values) then (
	       localFrame.values = (
		    new Sequence len 2 * scope.framesize + 1 do (
			 foreach y in localFrame.values do provide y;
			 while true do provide nullE;
			 )));
	  -- localFrame.values.frameindex = nullE;
	  );
     insert(entry,scope.dictionary);
     entry);
export makeSymbol(word:Word,position:Position,scope:Scope):Symbol := (
     s := makeEntry(word,position,scope);
     if scope == globalScope && isalnum(word.name)
     then globalFrame.values.(s.frameindex) = Expr(SymbolClosure(globalFrame,s));
     s);
export makeProtectedSymbolClosure(w:Word):SymbolClosure := (
     entry := makeSymbol(w,dummyPosition,globalScope);
     entry.protected = true;
     when globalFrame.values.(entry.frameindex)
     is s:SymbolClosure do s
     else SymbolClosure(globalFrame,entry));
makeKeyword(w:Word):SymbolClosure := (
     -- keywords differ from symbols in that their initial value is null
     entry := makeEntry(w,dummyPosition,globalScope);
     entry.protected = true;
     SymbolClosure(globalFrame,entry));
export makeProtectedSymbolClosure(s:string):SymbolClosure := makeProtectedSymbolClosure(makeUniqueWord(s,parseWORD));
makeKeyword(s:string):SymbolClosure := makeKeyword(makeUniqueWord(s,parseWORD));
-----------------------------------------------------------------------------
prec := 0;
bump():void := prec = prec + 1;

-- helper functions for setting up words with various methods for parsing them
foreach p in array(parseinfo)( parseEOF, parseWORD ) do p.funs = parsefuns(defaultunary, defaultbinary);
unary(s:string)         :Word := install(s,makeUniqueWord(s,
	  parseinfo(prec,nopr  ,prec,parsefuns(unaryop   ,defaultbinary))));
unaryword(s:string)     :Word :=           makeUniqueWord(s,
          parseinfo(prec,nopr  ,prec,parsefuns(unaryop   ,defaultbinary)));
biunary(s:string)       :Word := install(s,makeUniqueWord(s,
          parseinfo(prec,nopr  ,prec,parsefuns(unaryop   ,postfixop))));
postfix(s:string)       :Word := install(s,makeUniqueWord(s,
          parseinfo(prec,nopr  ,nopr,parsefuns(errorunary,postfixop))));
unaryleft(s:string)     :Word := install(s,makeUniqueWord(s,
          parseinfo(prec,prec  ,prec,parsefuns(unaryop   ,binaryop))));
unaryright(s:string)    :Word := install(s,makeUniqueWord(s,
          parseinfo(prec,prec-1,prec,parsefuns(unaryop   ,binaryop))));
binaryleft(s:string)    :Word := install(s,makeUniqueWord(s,
          parseinfo(prec,prec  ,nopr,parsefuns(errorunary,binaryop))));
binaryleftword(s:string):Word :=           makeUniqueWord(s,
          parseinfo(prec,prec  ,nopr,parsefuns(errorunary,binaryop)));
nleft(s:string)         :Word := install(s,makeUniqueWord(s,
          parseinfo(prec,prec  ,nopr,parsefuns(errorunary,nbinaryop))));
nleftword(s:string)     :Word :=           makeUniqueWord(s,
          parseinfo(prec,prec  ,nopr,parsefuns(errorunary,nbinaryop)));
nunaryleft(s:string)    :Word := install(s,makeUniqueWord(s,
          parseinfo(prec,prec  ,prec,parsefuns(nnunaryop ,nbinaryop))));
token(s:string)         :Word :=           makeUniqueWord(s,
          parseinfo(prec,nopr  ,prec,parsefuns(errorunary,errorbinary)));
binaryright(s:string,binary:function(ParseTree,Token,TokenFile,int,bool):ParseTree)
                        :Word := install(s,makeUniqueWord(s,
          parseinfo(prec,prec-1,nopr,parsefuns(errorunary,binary))));
binaryright(s:string)   :Word := binaryright(s,binaryop);

-- Now the symbols and operators:

-- Keep in mind that a "Word" is determined by a string token, and has attributes 
-- that determine how it is parsed, but a "Symbol" or "SymbolClosure" is a Word together
-- with a binding done in a particular way depending on the current scope.  The symbols
-- created below are all in the global scope.

     parseEOF.precedence = prec;
     parseEOF.binaryStrength = prec;
     precRightParen := prec;
bump();
     semicolonW = nleft(";");
     export semicolonS := makeKeyword(semicolonW);
     newlineW = nleftword("<NEWLINE>");			    -- no symbol for this one needed
bump();
     export commaW := nunaryleft(","); makeKeyword(commaW);
bump();
     wide := prec;
     elseW = token("else"); makeKeyword(elseW);
     thenW = token("then"); makeKeyword(thenW);
     doW = token("do"); makeKeyword(doW);
     listW = token("list"); makeKeyword(listW);
bump();
     export ColonEqualW := binaryright(":="); makeKeyword(ColonEqualW);
     export EqualW := binaryright("="); makeKeyword(EqualW);
     export LeftArrowW := binaryright("<-"); makeKeyword(LeftArrowW);
     export RightArrowW := binaryright("->",arrowop); makeKeyword(RightArrowW);
     export DoubleArrowS := makeKeyword(binaryright("=>"));
     export LongDoubleArrowS := makeKeyword(binaryright("==>"));
bump();
     narrow := prec;
     whenW = token("when"); makeKeyword(whenW);
     ofW = token("of"); makeKeyword(ofW);
     fromW = token("from"); makeKeyword(fromW);
     toW = token("to"); makeKeyword(toW);
bump();
     export LessLessS := makeKeyword(unaryleft("<<"));
bump();
     export GreaterGreaterS := makeKeyword(binaryright(">>"));
bump();
     export orS := makeKeyword(binaryleftword("or"));
bump();
     export andS := makeKeyword(binaryleftword("and"));
bump();
     export notS := makeKeyword(unaryword("not"));
bump();
     export LessS := makeKeyword(binaryleft("<"));
     export GreaterS := makeKeyword(binaryleft(">"));
     export LessEqualS := makeKeyword(binaryleft("<="));
     export GreaterEqualS := makeKeyword(binaryleft(">="));
     export EqualEqualEqualS := makeKeyword(binaryleft("==="));
     export EqualEqualS := makeKeyword(binaryleft("=="));
     export QuestionS := makeKeyword(binaryleft("?"));
     export NotEqualEqualEqualS := makeKeyword(binaryleft("=!="));
     export NotEqualS := makeKeyword(binaryleft("!="));
bump();
     export BarBarS := makeKeyword(binaryleft("||"));
bump();
     export AmpersandAmpersandS := makeKeyword(binaryleft("&&"));
bump();
     export ColonS := makeKeyword(binaryright(":"));
bump();
     export BarS := makeKeyword(binaryleft("|"));
bump();
     export HatHatS := makeKeyword(binaryleft("^^"));
bump();
     export AmpersandS := makeKeyword(binaryleft("&"));
bump();
     export TildeS := makeKeyword(postfix("~"));
bump();
     export DotDotS := makeKeyword(binaryleft(".."));
bump();
     export MinusS := makeKeyword(unaryleft("-"));
     export PlusS := makeKeyword(binaryleft("+"));
     export PlusPlusS := makeKeyword(binaryleft("++"));
bump();
     export StarStarS := makeKeyword(binaryleft("**"));
bump();
     precBracket := prec;
bump();
     export BackslashBackslashS := makeKeyword(binaryright("\\\\"));
bump();
     export StarS := makeKeyword(unaryleft("*"));
     export DivideS := makeKeyword(binaryleft("/"));
     export LeftDivideS := makeKeyword(binaryright("\\"));
     export PercentS := makeKeyword(binaryleft("%"));
     export SlashSlashS := makeKeyword(binaryleft("//"));
bump();
     export SharpSharpS := makeKeyword(binaryright("##"));
     export AtS := makeKeyword(binaryright("@"));
bump();
     export AdjacentS:=makeKeyword(binaryright(" "));
     precSpace = prec;
     parseWORD.precedence = prec; parseWORD.binaryStrength = prec; parseWORD.unaryStrength = prec;
bump();
     export AtAtS := makeKeyword(binaryleft("@@"));
bump();
     export SlashHatS := makeKeyword(binaryleft("/^"));
     export PowerS := makeKeyword(binaryleft("^"));
     export UnderscoreS := makeKeyword(binaryleft("_"));
     export SharpS := makeKeyword(unaryleft("#"));
     SharpS.symbol.word.parse.unaryStrength = precSpace-1;
     export SharpQuestionS := makeKeyword(binaryleft("#?"));
     export DotS := makeKeyword(binaryleft("."));
     export DotQuestionS := makeKeyword(binaryleft(".?"));
bump();
     export ExclamationS := makeKeyword(postfix("!"));
-----------------------------------------------------------------------------
parens(left:string,right:string,prec:int,binaryStrength:int,unaryStrength:int):Word := (
     l := makeUniqueWord(left,
	  parseinfo(prec          ,nopr,unaryStrength,parsefuns(unaryparen, defaultbinary)));
     r := makeUniqueWord(right,
          parseinfo(precRightParen,nopr,nopr,         parsefuns(errorunary, errorbinary  )));
     left = l.name;
     right = r.name;
     install(left,l);
     install(right,r);
     addmatch(left,right);
     makeKeyword(l);
     makeKeyword(r);
     l);

     export leftparen   := parens("(",")",precSpace,  precRightParen,precRightParen);
     export leftbrace   := parens("{","}",precSpace,  precRightParen,precRightParen);
     export leftbracket := parens("[","]",precBracket,precRightParen,precRightParen);
-----------------------------------------------------------------------------
bump();
special(s:string,f:function(Token,TokenFile,int,bool):ParseTree,prec:int):SymbolClosure := (
     makeKeyword(makeUniqueWord(s, parseinfo(precSpace, nopr, prec, parsefuns(f, defaultbinary)))));

     special("new",unarynew,narrow);
     special("for",unaryfor,narrow);

     export timeS := special("time",unaryop,wide);
     export timingS := special("timing",unaryop,wide);
     export shieldS := special("shield",unaryop,wide);

     export returnS := special("return",nunaryop,narrow);
     export breakS := special("break",nunaryop,narrow);

     special("while",unarywhile,wide);
     special("if",unaryif,wide);

     special("try",unarytry,wide);

     special("symbol",unaryquote,prec);
     special("global",unaryglobal,prec);
     special("local",unarylocal,prec);
-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------
export makeSymbol(token:Token):Symbol := (
     e := makeSymbol(token.word,token.position,token.scope);
     token.entry = e;
     e);
HadError := false;
export makeErrorTree(e:ParseTree,message:string):void := (
     HadError = true;
     printErrorMessage(treePosition(e),message);
     );
export makeErrorTree(e:Token,message:string):void := (
     HadError = true;
     printErrorMessage(e.position,message);
     );
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
		    printErrorMessage(token.position,"flagged symbol encountered");
		    );
	       )
     	  else (
	       if forcedef
	       then (
	       	    token.scope = globalScope;
	       	    makeSymbol(token);
		    )
	       else (
	       	    printErrorMessage(token.position,"undefined token " + token.word.name);
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
     is t:Token do (
	  if t.word.typecode == TCid then makeSymbol(e,scope)
	  else makeErrorTree(t,"expected symbol");
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
bindSingleParm(e:ParseTree,scope:Scope):void := (
     when e 
     is t:Token do (
	  if t.word.typecode == TCid then makeSymbol(e,scope)
	  else makeErrorTree(t,"expected symbol")
	  )
     else makeErrorTree(e,"expected symbol"));
bindParenParmList(e:ParseTree,scope:Scope,desc:functionDescription):void := (
     when e 
     is t:Token do (
	  bindFormalParm(e,scope,desc);
	  desc.restargs = true;
	  )
     is p:Parentheses do (
	  bindFormalParmList(p.contents,scope,desc)
	  )
     is p:EmptyParentheses do nothing
     else makeErrorTree(e,"expected parenthesized argument list or symbol"));

export opsWithBinaryMethod := array(SymbolClosure)(
     LessLessS, GreaterGreaterS, EqualEqualS, QuestionS, BarBarS, LongDoubleArrowS,
     AmpersandAmpersandS, ColonS, BarS, HatHatS, AmpersandS, DotDotS, MinusS, PlusS, PlusPlusS,
     StarStarS, StarS, BackslashBackslashS, DivideS, LeftDivideS, PercentS, SlashSlashS, AtS, 
     AdjacentS, AtAtS, SlashHatS, PowerS, UnderscoreS);
export opsWithUnaryMethod := array(SymbolClosure)( StarS, MinusS, LessLessS );
export opsWithPostfixMethod := array(SymbolClosure)( TildeS );
export opsOther := array(SymbolClosure)( SharpS, DoubleArrowS, orS, andS, notS, LessS,
     GreaterS, LessEqualS, GreaterEqualS, EqualEqualEqualS, EqualEqualS,
     QuestionS, NotEqualEqualEqualS, NotEqualS, SharpSharpS, SharpQuestionS, DotS, DotQuestionS,
     ExclamationS );

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
		    then printErrorMessage(token.position,
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
     ParseTree(StartScope(n,e)));
SawClosure := false;
export bind(e:ParseTree,scope:Scope):void := (
     when e
     is s:StartScope do bind(s.body,scope)
     is i:IfThen do (
	  bind(i.predicate,scope);
	  -- i.thenclause = bindnewscope(i.thenclause,scope);
	  bind(i.thenclause,scope);
	  )
     is i:IfThenElse do (
	  bind(i.predicate,scope);
	  -- i.thenclause = bindnewscope(i.thenclause,scope);
	  bind(i.thenclause,scope);
	  -- i.elseClause = bindnewscope(i.elseClause,scope);
	  bind(i.elseClause,scope);
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
     is ee:Parentheses do bind(ee.contents,scope)
     is EmptyParentheses do nothing
     is dummy do nothing
     is w:WhileDo do (
	  bind(w.predicate,scope);
	  -- w.body = bindnewscope(w.body,scope);
	  bind(w.doClause,scope);
	  )
     is w:For do (
	  newscop := newScope(scope);
	  bindSingleParm(w.variable,newscop);
	  bind(w.fromClause,scope);
	  bind(w.toClause,scope);
	  bind(w.whenClause,newscop);
	  bind(w.listClause,newscop);
	  bind(w.doClause,newscop);
	  w.scope = newscop;
	  )
     is w:WhileList do (
	  bind(w.predicate,scope);
	  bind(w.listClause,scope);
	  )
     is w:WhileListDo do (
	  bind(w.predicate,scope);
	  bind(w.listClause,scope);
	  bind(w.doClause,scope);
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
