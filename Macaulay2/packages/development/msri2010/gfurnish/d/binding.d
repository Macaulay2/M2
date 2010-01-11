--		Copyright 1994 by Daniel R. Grayson
use C;
use system;
use stdio;
use gmp;
use nets;
use tokens;
use strings;
use parser;
use lex;
use ctype;
use err;
use stdiop;

-----------------------------------------------------------------------------
-- first, the global symbol table and functions for making symbols
append(buckets:array(SymbolList),word:Word, entry:Symbol):void := (
     h := word.hash & (length(buckets)-1);
     when buckets.h
     is null do buckets.h = SymbolListCell(word,entry,NULL)
     is e:SymbolListCell do (
	  while true do (
	       when e.next 
	       is f:SymbolListCell do e = f
	       is null do (
		    e.next = SymbolListCell(word,entry,NULL);
		    break))));
enlarge(table:SymbolHashTable):void := (
     newbuckets := new array(SymbolList) len 2*length(table.buckets) do provide NULL;
     foreach e in table.buckets do (
	  entryList := e;
	  while true do
	  when entryList
	  is null do break
	  is entryListCell:SymbolListCell do (
	       append(newbuckets, entryListCell.word, entryListCell.entry);
	       entryList = entryListCell.next;
	       )
	  );
     table.buckets = newbuckets;
     );

-- warning: these routines have similar code
export insert(entry:Symbol,table:SymbolHashTable):Symbol := (
     table.numEntries = table.numEntries + 1;
     if 3 * table.numEntries > 2 * length(table.buckets) + 1
     then enlarge(table);
     h := entry.word.hash & (length(table.buckets)-1);
     table.buckets.h = SymbolListCell(entry.word,entry,table.buckets.h);
     entry);
export insert(table:SymbolHashTable, newname:Word, entry:Symbol):Symbol := ( -- warning -- unsafe -- check that the dictionary of the symbol is the same as this dictionary
     table.numEntries = table.numEntries + 1;
     if 3 * table.numEntries > 2 * length(table.buckets) + 1
     then enlarge(table);
     h := newname.hash & (length(table.buckets)-1);
     table.buckets.h = SymbolListCell(newname,entry,table.buckets.h);
     entry);

enlarge(f:Frame):int := (
     n := f.valuesUsed;
     f.valuesUsed = n + 1;
     if f.valuesUsed > length(f.values) then (
	  f.values = new Sequence len 2 * length(f.values) + 1 do (
	       foreach value in f.values do provide value;
	       while true do provide nullE));
     n);
export makeEntry(localInterpState:threadLocalInterp,word:Word,position:Position,dictionary:Dictionary):Symbol := (
     while dictionary.protected do (
	  if dictionary == dictionary.outerDictionary then (
	       -- shouldn't occur
	       -- "dictionaries" in actors5.d and "protect" in actors2.d enforce this!
	       error("internal error: global dictionaries all protected");
	       return dummySymbol;
	       );
	  dictionary = dictionary.outerDictionary;
	  );
     frameindex := 0;
     if dictionary.frameID == 0 then (
	  -- this allows the global frame to grow
	  frameindex = enlarge(globalFrame))
     else if dictionary.frameID == localInterpState.localFrame.frameID then (
	  -- this should take care of scopes which span a file,
	  -- or the dictionary for a break loop
	  -- and have a single frame which ought to be allowed to grow
	  frameindex = enlarge(localInterpState.localFrame) )
     else (
	  -- this is a dynamic frame, not allocated yet
	  frameindex = dictionary.framesize;
	  dictionary.framesize = dictionary.framesize + 1;
	  );
     insert(
	  Symbol(
	       word, 
	       nextHash(), 
	       position,
	       dummyUnaryFun,dummyPostfixFun,dummyBinaryFun,
	       dictionary.frameID, 
	       frameindex,
	       1,				-- first lookup is now
	       false,				      -- not protected
	       false
	       ),
	  dictionary.symboltable));
export makeSymbol(localInterpState:threadLocalInterp,word:Word,position:Position,dictionary:Dictionary):Symbol := (
     entry := makeEntry(localInterpState,word,position,dictionary);
     if dictionary.frameID == 0 && isalnum(word.name)
     then globalFrame.values.(entry.frameindex) = Expr(SymbolClosure(globalFrame,entry));
     entry);
export makeProtectedSymbolClosure(localInterpState:threadLocalInterp,w:Word):SymbolClosure := (
     entry := makeSymbol(localInterpState,w,dummyPosition,globalDictionary);
     entry.protected = true;
     when globalFrame.values.(entry.frameindex)
     is s:SymbolClosure do s
     else SymbolClosure(globalFrame,entry));
makeKeyword(localInterpState:threadLocalInterp,w:Word):SymbolClosure := (
     -- keywords differ from symbols in that their initial value is null
     entry := makeEntry(localInterpState,w,dummyPosition,globalDictionary);
     entry.protected = true;
     sc := SymbolClosure(globalFrame,entry);
     globalFrame.values.(entry.frameindex) = Expr(sc);
     sc);
export bindThreadLocalInterp := threadLocalInterp(dummyFrame);
export makeProtectedSymbolClosure(s:string):SymbolClosure := makeProtectedSymbolClosure(bindThreadLocalInterp,makeUniqueWord(s,parseWORD));
makeKeyword(s:string):SymbolClosure := makeKeyword(bindThreadLocalInterp,makeUniqueWord(s,parseWORD));
-----------------------------------------------------------------------------
prec := 0;
bump():void := prec = prec + 2;

-- helper functions for setting up words with various methods for parsing them
parseWORD.funs                 = parsefuns(defaultunary, defaultbinary);
unary(s:string)         :Word := install(s,makeUniqueWord(s, parseinfo(prec,nopr  ,prec,parsefuns(unaryop   ,defaultbinary))));
unaryword(s:string)     :Word :=           makeUniqueWord(s, parseinfo(prec,nopr  ,prec,parsefuns(unaryop   ,defaultbinary)));
biunary(s:string)       :Word := install(s,makeUniqueWord(s, parseinfo(prec,nopr  ,prec,parsefuns(unaryop   ,postfixop))));
postfix(s:string)       :Word := install(s,makeUniqueWord(s, parseinfo(prec,nopr  ,nopr,parsefuns(errorunary,postfixop))));
unarybinaryleft(s:string)     :Word := install(s,makeUniqueWord(s, parseinfo(prec,prec  ,prec,parsefuns(unaryop   ,binaryop))));
unarybinaryright(s:string)    :Word := install(s,makeUniqueWord(s, parseinfo(prec,prec-1,prec,parsefuns(unaryop   ,binaryop))));
binaryleft(s:string)    :Word := install(s,makeUniqueWord(s, parseinfo(prec,prec  ,nopr,parsefuns(errorunary,binaryop))));
binaryleftword(s:string):Word :=           makeUniqueWord(s, parseinfo(prec,prec  ,nopr,parsefuns(errorunary,binaryop)));
nleft (s:string)        :Word := install(s,makeUniqueWord(s, parseinfo(prec,prec  ,nopr,parsefuns(errorunary,nbinaryop))));
nright(s:string)        :Word := install(s,makeUniqueWord(s, parseinfo(prec,prec-1,nopr,parsefuns(errorunary,nbinaryop))));
nleftword(s:string)     :Word :=           makeUniqueWord(s, parseinfo(prec,prec  ,nopr,parsefuns(errorunary,nbinaryop)));
nunarybinaryleft(s:string)    :Word := install(s,makeUniqueWord(s, parseinfo(prec,prec  ,prec,parsefuns(nnunaryop ,nbinaryop))));
token(s:string)         :Word :=           makeUniqueWord(s, parseinfo(prec,nopr  ,prec,parsefuns(errorunary,errorbinary)));
binaryright(s:string,binary:function(ParseTree,Token,TokenFile,int,bool):ParseTree):Word
                              := install(s,makeUniqueWord(s, parseinfo(prec,prec-1,nopr,parsefuns(errorunary,binary))));
binaryrightword(s:string):Word:=           makeUniqueWord(s, parseinfo(prec,prec-1,nopr,parsefuns(errorunary,binaryop)));
binaryright(s:string)   :Word := binaryright(s,binaryop);
parens(left:string,right:string,leftprec:int,rightprec:int,unaryStrength:int):Word := (
     l := makeUniqueWord(left,
	  parseinfo(leftprec ,nopr,unaryStrength,parsefuns(unaryparen, defaultbinary)));
     r := makeUniqueWord(right,
          parseinfo(rightprec,nopr,nopr,         parsefuns(errorunary, errorbinary  )));
     left = l.name;
     right = r.name;
     install(left,l);
     install(right,r);
     addmatch(left,right);
     makeKeyword(bindThreadLocalInterp,l);
     makeKeyword(bindThreadLocalInterp,r);
     l);
special(s:string,f:function(Token,TokenFile,int,bool):ParseTree,lprec:int,rprec:int):SymbolClosure := (
     makeKeyword(bindThreadLocalInterp,makeUniqueWord(s, parseinfo(lprec, nopr, rprec, parsefuns(f, defaultbinary)))));

-- Now the symbols and operators:

-- Keep in mind that a "Word" is determined by a string token, and has attributes 
-- that determine how it is parsed, but a "Symbol" or "SymbolClosure" is a Word together
-- with a binding done in a particular way depending on the current dictionary.  The symbols
-- created below are all in the global dictionary.

-- new operators must be:
--   set up as an "actor" with "setup()"
--   added to the export list in ../m2/exports.m2
--   added to the table binaryOperatorFunctions in ../m2/expressions.m2
--   added to the list of operators in the documentation node "operators" in ../packages/Macaulay2Doc/ov_language.m2
--   documented with a suitable headline, such as:
--     	    "a unary operator"
--     	    "a binary operator"
--     	    "a unary and binary operator"
--     	    "a unary postfix operator"

bump();
     wordEOF = nleftword("{*end of file*}");
     makeKeyword(bindThreadLocalInterp,wordEOF);
bump();
     wordEOC = nleftword("{*end of cell*}");
     makeKeyword(bindThreadLocalInterp,wordEOC);
bump();
     precRightParen := prec;
bump();
     export SemicolonW := nright(";");
     export SemicolonS := makeKeyword(bindThreadLocalInterp,SemicolonW);
     NewlineW = nleftword("{*newline*}");
bump();
     export CommaW := nunarybinaryleft(","); export commaS := makeKeyword(bindThreadLocalInterp,CommaW);
bump();
     wide := prec;
     elseW = token("else"); makeKeyword(bindThreadLocalInterp,elseW);
     thenW = token("then"); makeKeyword(bindThreadLocalInterp,thenW);
     doW = token("do"); makeKeyword(bindThreadLocalInterp,doW);
     listW = token("list"); makeKeyword(bindThreadLocalInterp,listW);
bump();
     export ColonEqualW := binaryright(":="); export ColonEqualS := makeKeyword(bindThreadLocalInterp,ColonEqualW);
     export EqualW := binaryright("="); export EqualS := makeKeyword(bindThreadLocalInterp,EqualW);
     export LeftArrowW := binaryright("<-"); export LeftArrowS := makeKeyword(bindThreadLocalInterp,LeftArrowW);
     export RightArrowW := binaryright("->",arrowop); export RightArrowS := makeKeyword(bindThreadLocalInterp,RightArrowW);
     export DoubleArrowS := makeKeyword(bindThreadLocalInterp,binaryright("=>"));
     export GreaterGreaterS := makeKeyword(bindThreadLocalInterp,binaryright(">>"));
bump();
     whenW = token("when"); makeKeyword(bindThreadLocalInterp,whenW);
     ofW = token("of"); makeKeyword(bindThreadLocalInterp,ofW);
     inW = token("in"); makeKeyword(bindThreadLocalInterp,inW);
     fromW = token("from"); makeKeyword(bindThreadLocalInterp,fromW);
     toW = token("to"); makeKeyword(bindThreadLocalInterp,toW);
     narrow := prec;
bump();
     export LessLessS := makeKeyword(bindThreadLocalInterp,unarybinaryleft("<<"));	    -- also binary
bump();
     export DeductionS := makeKeyword(bindThreadLocalInterp,unarybinaryright("|-"));	    -- also binary
bump();
     export LongLongDoubleRightArrowS := makeKeyword(bindThreadLocalInterp,binaryright("===>"));
     export LongLongDoubleLeftArrowS := makeKeyword(bindThreadLocalInterp,unarybinaryright("<==="));
bump();
     export LongBiDoubleArrowS := makeKeyword(bindThreadLocalInterp,binaryright("<==>"));
bump();
     export LongDoubleRightArrowS := makeKeyword(bindThreadLocalInterp,binaryright("==>"));
     export LongDoubleLeftArrowS := makeKeyword(bindThreadLocalInterp,unarybinaryright("<==")); -- also binary
bump();
     export orS := makeKeyword(bindThreadLocalInterp,binaryrightword("or"));
bump();
     export andS := makeKeyword(bindThreadLocalInterp,binaryrightword("and"));
bump();
     export notS := makeKeyword(bindThreadLocalInterp,unaryword("not"));
-- binary predicates on terms:
bump();
     export incomparableS := makeProtectedSymbolClosure("incomparable");
     export LessS := makeKeyword(bindThreadLocalInterp,unarybinaryright("<"));
     export GreaterS := makeKeyword(bindThreadLocalInterp,unarybinaryright(">"));
     export LessEqualS := makeKeyword(bindThreadLocalInterp,unarybinaryright("<="));
     export GreaterEqualS := makeKeyword(bindThreadLocalInterp,unarybinaryright(">="));
     export EqualEqualEqualS := makeKeyword(bindThreadLocalInterp,binaryright("==="));
     export EqualEqualS := makeKeyword(bindThreadLocalInterp,binaryright("=="));
     export QuestionS := makeKeyword(bindThreadLocalInterp,unarybinaryright("?"));
     export NotEqualEqualEqualS := makeKeyword(bindThreadLocalInterp,binaryright("=!="));
     export NotEqualS := makeKeyword(bindThreadLocalInterp,binaryright("!="));
-- operations on terms that yield terms:
bump();
     export BarBarS := makeKeyword(bindThreadLocalInterp,binaryleft("||"));
bump();
     export ColonS := makeKeyword(bindThreadLocalInterp,binaryright(":"));
bump();
     export BarS := makeKeyword(bindThreadLocalInterp,binaryleft("|"));
bump();
     export HatHatS := makeKeyword(bindThreadLocalInterp,binaryleft("^^"));
bump();
     export AmpersandS := makeKeyword(bindThreadLocalInterp,binaryleft("&"));
bump();
     export DotDotS := makeKeyword(bindThreadLocalInterp,binaryleft(".."));
     export DotDotLessS := makeKeyword(bindThreadLocalInterp,binaryleft("..<"));
bump();
     export MinusS := makeKeyword(bindThreadLocalInterp,unarybinaryleft("-"));	    -- also binary
     export PlusS := makeKeyword(bindThreadLocalInterp,unarybinaryleft("+"));	    -- also binary
     export PlusPlusS := makeKeyword(bindThreadLocalInterp,binaryleft("++"));
bump();
     export StarStarS := makeKeyword(bindThreadLocalInterp,binaryleft("**"));
bump();
     precBracket := prec;
     export leftbracket := parens("[","]",precBracket, precRightParen, precRightParen);
bump();
     export BackslashBackslashS := makeKeyword(bindThreadLocalInterp,binaryright("\\\\"));
     export StarS := makeKeyword(bindThreadLocalInterp,unarybinaryleft("*"));	    -- also binary
     export DivideS := makeKeyword(bindThreadLocalInterp,binaryleft("/"));
     export LeftDivideS := makeKeyword(bindThreadLocalInterp,binaryright("\\"));
     export PercentS := makeKeyword(bindThreadLocalInterp,binaryleft("%"));
     export SlashSlashS := makeKeyword(bindThreadLocalInterp,binaryleft("//"));
bump();
     export AtS := makeKeyword(bindThreadLocalInterp,binaryright("@"));
bump();
     precSpace = prec;
     export AdjacentS:=makeKeyword(bindThreadLocalInterp,binaryright("SPACE"));
     export leftparen   := parens("(",")",precSpace, precRightParen, precRightParen);
     export leftbrace   := parens("{","}",precSpace, precRightParen, precRightParen);
     parseWORD.precedence = prec; parseWORD.binaryStrength = nopr; parseWORD.unaryStrength = nopr;
     export timeS := special("time",unaryop,precSpace,wide);
     export timingS := special("timing",unaryop,precSpace,wide);
     export shieldS := special("shield",unaryop,precSpace,wide);
     export throwS := special("throw",nunaryop,precSpace,wide);
     export returnS := special("return",nunaryop,precSpace,wide);
     export breakS := special("break",nunaryop,precSpace,wide);
     export continueS := special("continue",nunaryop,precSpace,wide);
     export stepS := special("step",nunaryop,precSpace,wide);
     -- export codePositionS := special("codePosition",unaryop,precSpace,narrow);
     special("new",unarynew,precSpace,narrow);
     special("for",unaryfor,precSpace,narrow);
     special("while",unarywhile,precSpace,wide);
     special("if",unaryif,precSpace,wide);
     special("try",unarytry,precSpace,wide);
     special("catch",unarycatch,precSpace,wide);
bump();
     export ParenStarParenS := makeKeyword(bindThreadLocalInterp,postfix("(*)"));
bump();
     export AtAtS := makeKeyword(bindThreadLocalInterp,binaryleft("@@"));
bump();
     export TildeS := makeKeyword(bindThreadLocalInterp,postfix("~"));
     export UnderscoreStarS := makeKeyword(bindThreadLocalInterp,postfix("_*"));
     export PowerStarS := makeKeyword(bindThreadLocalInterp,postfix("^*"));
bump();
     export PowerS := makeKeyword(bindThreadLocalInterp,binaryleft("^"));
     export PowerStarStarS := makeKeyword(bindThreadLocalInterp,binaryleft("^**"));
     export UnderscoreS := makeKeyword(bindThreadLocalInterp,binaryleft("_"));
     export SharpS := makeKeyword(bindThreadLocalInterp,unarybinaryleft("#")); SharpS.symbol.word.parse.unaryStrength = precSpace-1;
     export SharpQuestionS := makeKeyword(bindThreadLocalInterp,binaryleft("#?"));
     export DotS := makeKeyword(bindThreadLocalInterp,binaryleft("."));
     export DotQuestionS := makeKeyword(bindThreadLocalInterp,binaryleft(".?"));
bump();
     export ExclamationS := makeKeyword(bindThreadLocalInterp,postfix("!"));
bump();
     special("symbol",unarysymbol,precSpace,prec);
     special("global",unaryglobal,precSpace,prec);
     special("local",unarylocal,precSpace,prec);
-----------------------------------------------------------------------------
export GlobalAssignS := makeProtectedSymbolClosure("GlobalAssignHook");
export GlobalAssignE := Expr(GlobalAssignS);

export GlobalReleaseS := makeProtectedSymbolClosure("GlobalReleaseHook");
export GlobalReleaseE := Expr(GlobalReleaseS);

export EqualE := Expr(EqualS);
export LeftArrowE := Expr(LeftArrowS);

export EqualEqualE := Expr(EqualEqualS);
export LessE := Expr(LessS);
export GreaterE := Expr(GreaterS);
export incomparableE := Expr(incomparableS);

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
export makeSymbol(localInterpState:threadLocalInterp,token:Token):Symbol := (
     e := makeSymbol(localInterpState,token.word,position(token),token.dictionary);
     token.entry = e;
     e);
HadError := false;
export makeErrorTree(e:ParseTree,message:string):void := (
     HadError = true;
     printErrorMessage(treePosition(e),message);
     );
export makeErrorTree(e:Token,message:string):void := (
     HadError = true;
     printErrorMessage(e,message);
     );
makeSymbol(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary):void := (
     when e
     is token:Token do (
	  token.dictionary = dictionary;
	  makeSymbol(localInterpState,token);)
     else makeErrorTree(e,"expected single identifier"));
-----------------------------------------------------------------------------
cleanGlobalFrame():void := globalFrame.values = emptySequence;
atend(cleanGlobalFrame);
-----------------------------------------------------------------------------
lookupCountIncrement := 1;
export lookup(word:Word,table:SymbolHashTable):(null or Symbol) := (
     if table == dummySymbolHashTable then error("dummy symbol table used");
     entryList := table.buckets.(
	  word.hash & (length(table.buckets)-1)
	  );
     while true do
     when entryList
     is null do return NULL
     is entryListCell:SymbolListCell do (
	  if entryListCell.word == word 
	  then (
	       e := entryListCell.entry;
	       e.lookupCount = e.lookupCount + lookupCountIncrement;
	       return e;
	       );
	  entryList = entryListCell.next));

export globalLookup(w:Word):(null or Symbol) := (
     d := globalDictionary;
     while (
	  when lookup(w,d.symboltable) is null do nothing is e:Symbol do return e;
	  d != d.outerDictionary ) do d = d.outerDictionary;
     NULL);
export lookup(w:Word,d:Dictionary):(null or Symbol) := (
     while (
	  when lookup(w,d.symboltable) is null do nothing is e:Symbol do return e;
	  d != d.outerDictionary ) do d = d.outerDictionary;
     globalLookup(w));
lookup(localInterpState:threadLocalInterp,token:Token,forcedef:bool):void := (
     n := length(token.word.name);
     if n >= 1 && isdigit(token.word.name.0) 
     || n >= 2 && token.word.name.0 == '.' && isdigit(token.word.name.1)
     then nothing
     else (
     	  when lookup(token.word,token.dictionary)
     	  is entry:Symbol do (
	       token.entry = entry;
	       if entry.flagLookup then (
		    printErrorMessage(token,"flagged symbol encountered");
		    HadError=true;
		    );
	       )
     	  else (
	       if forcedef
	       then (
		    -- undefined variables are defined as global and static here
	       	    token.dictionary = globalDictionary;
	       	    makeSymbol(localInterpState,token);
		    )
	       else (
	       	    printErrorMessage(token,"undefined token " + token.word.name);
	       	    HadError=true;))));
lookup(localInterpState:threadLocalInterp,token:Token):void := lookup(localInterpState,token,true);
lookuponly(localInterpState:threadLocalInterp,token:Token):void := lookup(localInterpState,token,false);
-----------------------------------------------------------------------------
export opsWithBinaryMethod := array(SymbolClosure)(
     LessLessS, GreaterGreaterS, EqualEqualS, QuestionS, BarBarS, 
     LongBiDoubleArrowS, DeductionS,
     LongDoubleRightArrowS, LongLongDoubleRightArrowS,
     LongDoubleLeftArrowS, LongLongDoubleLeftArrowS,
     ColonS, BarS, HatHatS, AmpersandS, DotDotS, DotDotLessS, MinusS, PlusS, PlusPlusS, StarStarS, StarS, BackslashBackslashS, DivideS, LeftDivideS, PercentS, SlashSlashS, AtS, 
     AdjacentS, AtAtS, PowerS, UnderscoreS, PowerStarStarS, orS, andS);
export opsWithUnaryMethod := array(SymbolClosure)(
     StarS, MinusS, PlusS, LessLessS, 
     LongDoubleLeftArrowS, LongLongDoubleLeftArrowS, 
     notS, DeductionS, QuestionS,LessS,GreaterS,LessEqualS,GreaterEqualS);
export opsWithPostfixMethod := array(SymbolClosure)( TildeS, ParenStarParenS, UnderscoreStarS, PowerStarS ,ExclamationS );

-- ":=" "=" "<-" "->"  "=>" "===" "=!=" "!=" "#" "#?" "." ".?" ";" "," "<" ">" "<=" ">="
export fixedBinaryOperators := array(SymbolClosure)(ColonEqualS,EqualS,LeftArrowS,RightArrowS,DoubleArrowS,EqualEqualEqualS,NotEqualEqualEqualS,NotEqualS,SharpS,SharpQuestionS,
     DotS,DotQuestionS,SemicolonS,commaS,LessS,GreaterS,LessEqualS,GreaterEqualS);

-- "#" "," "<" ">" "<=" ">="
export fixedPrefixOperators := array(SymbolClosure)(commaS,SharpS);

-- ";" ","
export fixedPostfixOperators := array(SymbolClosure)(SemicolonS,commaS);

-----------------------------------------------------------------------------
bind(localInterpState:threadLocalInterp,token:Token,dictionary:Dictionary):void := (
     token.dictionary = dictionary;
     lookup(localInterpState,token););
bindop(localInterpState:threadLocalInterp,token:Token,dictionary:Dictionary):void := (
     token.dictionary = dictionary;
     lookuponly(localInterpState,token););
export bind(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary):void;
bindFormalParm(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary,desc:functionDescription):void := (
     when e
     is t:Token do (
	  if t.word.typecode == TCid then makeSymbol(localInterpState,e,dictionary)
	  else makeErrorTree(t,"expected symbol");
	  desc.numparms = desc.numparms + 1;
	  )
     else makeErrorTree(e,"syntax error: expected function parameter"));
bindFormalParmList(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary,desc:functionDescription):void := (
     when e 
     is binary:Binary do (
	  if binary.operator.word == CommaW
	  then (
	       bindFormalParmList(localInterpState,binary.lhs,dictionary,desc);
	       bindop(localInterpState,binary.operator,dictionary);
	       bindFormalParm(localInterpState,binary.rhs,dictionary,desc);)
	  else makeErrorTree(e,"syntax error: expected function parameter list"))
     else bindFormalParm(localInterpState,e,dictionary,desc));
bindSingleParm(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary):void := (
     when e 
     is t:Token do (
	  if t.word.typecode == TCid then makeSymbol(localInterpState,e,dictionary)
	  else makeErrorTree(t,"expected symbol")
	  )
     else makeErrorTree(e,"expected symbol"));
bindParenParmList(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary,desc:functionDescription):void := (
     when e 
     is t:Token do (
	  bindFormalParm(localInterpState,e,dictionary,desc);
	  desc.restargs = true;
	  )
     is p:Parentheses do (
	  bindFormalParmList(localInterpState,p.contents,dictionary,desc)
	  )
     is p:EmptyParentheses do nothing
     else makeErrorTree(e,"expected parenthesized argument list or symbol"));
opHasBinaryMethod(o:Symbol):bool := (
     foreach s in opsWithBinaryMethod do if s.symbol == o then return true;
     return false;
     );
opHasUnaryMethod(o:Symbol):bool := (
     foreach s in opsWithUnaryMethod do if s.symbol == o then return true;
     return false;
     );
opHasPostfixMethod(o:Symbol):bool := (
     foreach s in opsWithPostfixMethod do if s.symbol == o then return true;
     return false;
     );
bindTokenLocally(localInterpState:threadLocalInterp,token:Token,dictionary:Dictionary):void := (
     lookupCountIncrement = 0;
     r := lookup(token.word,dictionary);
     lookupCountIncrement = 1;
     when r
     is entry:Symbol do (
	  if dictionary.frameID == entry.frameID
	  then printWarningMessage(token, "local declaration of " + token.word.name + " shields variable with same name" );
	  )
     else nothing;
     token.dictionary = dictionary;
     makeSymbol(localInterpState,token);
     );
bindToken(localInterpState:threadLocalInterp,token:Token,dictionary:Dictionary,colon:bool):void := (
     if colon then bindTokenLocally(localInterpState,token,dictionary) else bind(localInterpState,token,dictionary);
     );
bindParallelAssignmentItem(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary,colon:bool):void := (
     when e
     is token:Token do (
	  if token.word.typecode != TCid then makeErrorTree(token,"syntax error: parallel assignment expected symbol")
	  else bindToken(localInterpState,token,dictionary,colon);
	  )
     else makeErrorTree(e,"syntax error: parallel assignment expected symbol"));
bindParallelAssignmentList(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary,colon:bool):void := (
     when e
     is binary:Binary do (
	  if binary.operator.word == CommaW
	  then (
	       bindParallelAssignmentList(localInterpState,binary.lhs,dictionary,colon);
	       bindop(localInterpState,binary.operator,dictionary);
	       bindParallelAssignmentItem(localInterpState,binary.rhs,dictionary,colon);
	       )
     	  else makeErrorTree(e,"syntax error: parallel assignment expected symbol list")
	  )
     else bindParallelAssignmentItem(localInterpState,e,dictionary,colon));
bindassignment(localInterpState:threadLocalInterp,assn:Binary,dictionary:Dictionary,colon:bool):void := (
     bindop(localInterpState,assn.operator,dictionary);
     body := assn.rhs;
     when assn.lhs
     is p:Parentheses do (
	  bindParallelAssignmentList(localInterpState,p.contents,dictionary,colon);
	  bind(localInterpState,body,dictionary);
	  )
     is token:Token do (
	  if token.word.typecode != TCid then (
	       makeErrorTree(assn.operator, "expected a symbol to left of '"+assn.operator.entry.word.name+"'");
	       return;
	       );
	  bindToken(localInterpState,token,dictionary,colon);
	  bind(localInterpState,body,dictionary);
	  )
     is a:Adjacent do (
	  bind(localInterpState,a.lhs,dictionary);
	  bind(localInterpState,a.rhs,dictionary);
	  bind(localInterpState,body,dictionary);
	  )
     is unary:Unary do (
	  bindop(localInterpState,unary.operator,dictionary);
	  bind(localInterpState,unary.rhs,dictionary);
	  bind(localInterpState,body,dictionary);
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
	  bind(localInterpState,unary.lhs,dictionary);
	  bindop(localInterpState,unary.operator,dictionary);
	  bind(localInterpState,body,dictionary);
	  if colon
	  then (
	       if ! opHasPostfixMethod(unary.operator.entry)
	       then makeErrorTree(assn.operator, "can't assign a method for this postfix operator");
	       )
	  else (
	       if ! opHasPostfixMethod(unary.operator.entry)
	       then makeErrorTree(assn.operator, "can't assign a value for this postfix operator")
	       )
	  )
     is binary:Binary do (
	  bind(localInterpState,binary.lhs,dictionary);
	  bindop(localInterpState,binary.operator,dictionary);
	  bind(localInterpState,binary.rhs, if binary.operator.word == DotS.symbol.word then globalDictionary else dictionary );
	  bind(localInterpState,body,dictionary);
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
	       if binary.operator.word == DotS.symbol.word then (
		    when binary.rhs is t:Token do (
			 if t.word.typecode != TCid
			 then makeErrorTree(assn.operator, "expected a symbol to right of '.'");
			 )
		    else makeErrorTree(assn.operator, "expected a symbol to right of '.'");
		    );
	       )
	  )
     is n:New do (
	  if colon then (
	       bind(localInterpState,n.newclass,dictionary);
	       bind(localInterpState,n.newparent,dictionary);
	       bind(localInterpState,n.newinitializer,dictionary);
	       bind(localInterpState,body,dictionary))
	  else makeErrorTree(assn.operator, 
	       "left hand side of assignment inappropriate"))
     else makeErrorTree(assn.operator, 
	  "left hand side of assignment inappropriate"));
bindnewdictionary(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary):ParseTree := (
     n := newLocalDictionary(dictionary);
     bind(localInterpState,e,n);
     ParseTree(StartDictionary(n,e)));
export bind(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary):void := (
     when e
     is s:StartDictionary do bind(localInterpState,s.body,dictionary)
     is i:IfThen do (
	  bind(localInterpState,i.predicate,dictionary);
	  -- i.thenclause = bindnewdictionary(i.thenclause,dictionary);
	  bind(localInterpState,i.thenclause,dictionary);
	  )
     is i:IfThenElse do (
	  bind(localInterpState,i.predicate,dictionary);
	  -- i.thenclause = bindnewdictionary(i.thenclause,dictionary);
	  bind(localInterpState,i.thenclause,dictionary);
	  -- i.elseClause = bindnewdictionary(i.elseClause,dictionary);
	  bind(localInterpState,i.elseClause,dictionary);
	  )
     is token:Token do (
	  if token.word.typecode == TCid then bind(localInterpState,token,dictionary);
	  )
     is adjacent:Adjacent do (
	  bind(localInterpState,adjacent.lhs,dictionary); 
	  bind(localInterpState,adjacent.rhs,dictionary))
     is binary:Binary do (
	  if binary.operator.word == EqualW
	  then bindassignment(localInterpState,binary,dictionary,false)
	  else if binary.operator.word == ColonEqualW
	  then bindassignment(localInterpState,binary,dictionary,true)
	  else if binary.operator.word == DotS.symbol.word
	  then (
	       bind(localInterpState,binary.lhs,dictionary);
	       bindop(localInterpState,binary.operator,dictionary);
	       bind(localInterpState,binary.rhs,globalDictionary);
	       when binary.rhs
	       is token:Token do (
		    if token.word.typecode != TCid
		    then makeErrorTree(binary.operator, "expected a symbol to right of '.'" );
		    )
	       else makeErrorTree(binary.operator, "expected a symbol to right of '.'" );
	       )
	  else if binary.operator.word == DotQuestionS.symbol.word
	  then (
	       bind(localInterpState,binary.lhs,dictionary);
	       bindop(localInterpState,binary.operator,dictionary);
	       bind(localInterpState,binary.rhs,globalDictionary);
	       when binary.rhs
	       is token:Token do (
		    if token.word.typecode != TCid
		    then makeErrorTree(binary.operator, "expected a symbol to right of '.?'" );
		    )
	       else makeErrorTree(binary.operator, "expected a symbol to right of '.?'" );
	       )
	  else (
	       bind(localInterpState,binary.lhs,dictionary);
	       bindop(localInterpState,binary.operator,dictionary);
	       bind(localInterpState,binary.rhs,dictionary);
	       );
	  )
     is q:LocalQuote do (
	  bind(localInterpState,q.operator,dictionary);
	  tok := q.rhs;
	  tok.dictionary = dictionary;
	  r := lookup(tok.word,dictionary.symboltable);
	  when r
	  is entry:Symbol do ( tok.entry = entry; )
	  else ( makeSymbol(localInterpState,tok); );
	  )
     is q:GlobalQuote do (
	  bind(localInterpState,q.operator,dictionary);
	  bind(localInterpState,q.rhs,globalDictionary);
	  )
     is q:Quote do (
	  bind(localInterpState,q.operator,dictionary);
	  bind(localInterpState,q.rhs,dictionary);
	  )
     is a:Arrow do (
	  newdict := newLocalDictionary(dictionary);
	  a.desc = functionDescription(newdict.frameID,0,0,false);
	  bindParenParmList(localInterpState,a.lhs,newdict,a.desc);
	  bind(localInterpState,a.rhs,newdict);
	  a.desc.framesize = newdict.framesize;
	  )
     is unary:Unary do (
	  bindop(localInterpState,unary.operator,dictionary);
	  bind(localInterpState,unary.rhs,dictionary);)
     is postfix:Postfix do (
	  bind(localInterpState,postfix.lhs,dictionary);
	  bindop(localInterpState,postfix.operator,dictionary);)
     is ee:Parentheses do bind(localInterpState,ee.contents,dictionary)
     is EmptyParentheses do nothing
     is dummy do nothing
     is w:WhileDo do (
	  bind(localInterpState,w.predicate,dictionary);
	  -- w.body = bindnewdictionary(w.body,dictionary);
	  bind(localInterpState,w.doClause,dictionary);
	  )
     is w:For do (
	  newdict := newLocalDictionary(dictionary);
	  bindSingleParm(localInterpState,w.variable,newdict);
	  bind(localInterpState,w.inClause,dictionary);
	  bind(localInterpState,w.fromClause,dictionary);
	  bind(localInterpState,w.toClause,dictionary);
	  bind(localInterpState,w.whenClause,newdict);
	  bind(localInterpState,w.listClause,newdict);
	  bind(localInterpState,w.doClause,newdict);
	  w.dictionary = newdict;
	  )
     is w:WhileList do (
	  bind(localInterpState,w.predicate,dictionary);
	  bind(localInterpState,w.listClause,dictionary);
	  )
     is w:WhileListDo do (
	  bind(localInterpState,w.predicate,dictionary);
	  bind(localInterpState,w.listClause,dictionary);
	  bind(localInterpState,w.doClause,dictionary);
	  )
     is n:New do (
     	  bind(localInterpState,n.newclass,dictionary);
     	  bind(localInterpState,n.newparent,dictionary);
     	  bind(localInterpState,n.newinitializer,dictionary);)
     is i:TryElse do (
	  -- i.primary = bindnewdictionary(i.primary,dictionary);
	  bind(localInterpState,i.primary,dictionary);
	  -- i.alternate = bindnewdictionary(i.alternate,dictionary);
	  bind(localInterpState,i.alternate,dictionary);
	  )
     is i:TryThenElse do (
	  bind(localInterpState,i.primary,dictionary);
	  bind(localInterpState,i.sequel,dictionary);
	  bind(localInterpState,i.alternate,dictionary);
	  )
     is i:Try do (
	  bind(localInterpState,i.primary,dictionary);
	  )
     is i:Catch do (
	  bind(localInterpState,i.primary,dictionary);
	  )
     );
export localBind(localInterpState:threadLocalInterp,e:ParseTree,dictionary:Dictionary):bool := (
     HadError = false;
     bind(localInterpState,e,dictionary);
     !HadError
     );

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
