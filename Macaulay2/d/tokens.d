--		Copyright 1994 by Daniel R. Grayson
--- here we define the recursive types involved in parsing

use system;
use err;
use stdio;
use stdiop;
use strings;
use nets;
use arith;

export parsefuns := {
     unary:function(Token,TokenFile,int,bool):ParseTree,
     binary:function(ParseTree,Token,TokenFile,int,bool):ParseTree
     };
export parseinfo := {
     precedence:int,
     scope:int,
     strength:int,
     funs:parsefuns
     };
export TCnone := 0;			-- for artificial words: dummyWord, wordEOF
export TCid := 1;			-- identifiers and operators
export TCint := 2;
export TCdouble := 3;
export TCstring := 4;
export Word := {		-- a word
     name:string,		--   the string representing it in this language
     typecode:int,		--   TCid, TCint, TCdouble, or TCstring
     hash:int,	    		--   the hash value
     parse:parseinfo		--   parsing information
     };
export Symbol := {		    -- dictionary entry for a symbol
     word:Word,			    --   the word
     hash:int,			    --   just a counter, unique, unchanging
     position:Position,	    	    --   the position where the definition was made
     unary:unop,
     postfix:unop,
     binary:binop,
     scopenum:int,		    -- seqno of scope containing it
     frameindex:int,		    -- index within the frame of its value
     lookupCount:int,		    -- number of times looked up
     protected:bool,	            -- whether protected against assignment
     transientScope:bool,     	    -- whether its scope will have more than one frame
     flagLookup:bool		    -- whether to warn when symbol is used
     };
export SymbolListCell := {entry:Symbol, next:SymbolList};
export SymbolList := null or SymbolListCell;
export SymbolHashTable := array(SymbolList);-- length always a power of 2
export newSymbolHashTable(size:int):SymbolHashTable := (
     new array(SymbolList) len size do provide NULL
     );
export Dictionary := { 
     hashTable:SymbolHashTable, 
     numEntries:int 
     };
export Scope := {
     dictionary:Dictionary,
     outerScope:Scope,
     seqno:int,			-- -1 for dummy, 0 for global, then 1,2,3,...
     framesize:int,
     transient:bool	        -- whether there can be multiple frames
     	       	    	        -- for the global scope and file scopes : no
				-- for function closures : yes
     };
export Token := {		-- a word, as encountered in the input
     word:Word,			--   the word
     position:Position,		--   the location where it was encountered
     scope:Scope,		--   the scope in which it was encountered
     entry:Symbol,     	  	--   the dictionary entry
     followsNewline:bool        --   whether it followed white space with a newline in it
     };

-- ParseTree

export Adjacent := {lhs:ParseTree, rhs:ParseTree};
export For := {
     fortoken:Token, variable:ParseTree,
     fromclause:ParseTree, toclause:ParseTree, whenclause:ParseTree, listclause:ParseTree, doclause:ParseTree,
     scope:Scope					    -- filled in later
     };
export WhileDo := {
     whiletoken:Token, predicate:ParseTree,
     dotoken:Token, doclause:ParseTree};
export WhileList := {
     whiletoken:Token, predicate:ParseTree,
     listtoken:Token, listclause:ParseTree};
export WhileListDo := {
     whiletoken:Token, predicate:ParseTree,
     listtoken:Token, listclause:ParseTree,
     dotoken:Token, doclause:ParseTree
     };
export TryElse := {
     trytoken:Token, primary:ParseTree,
     elsetoken:Token, alternate:ParseTree};
export Try := {
     trytoken:Token, primary:ParseTree};
export IfThen := {
     iftoken:Token, predicate:ParseTree, 
     thentoken:Token, thenclause:ParseTree};
export IfThenElse := {
     iftoken:Token, predicate:ParseTree, 
     thentoken:Token, thenclause:ParseTree, 
     elsetoken:Token, elseclause:ParseTree};
export New := {
     newtoken:Token, newclass:ParseTree,
     oftoken:Token, newparent:ParseTree,
     fromtoken:Token, newinitializer:ParseTree};
export Arrow := {lhs:ParseTree, operator:Token, rhs:ParseTree, desc:functionDescription};
export Quote := {operator:Token, rhs:Token};
export GlobalQuote := {operator:Token, rhs:Token, global:void};
export LocalQuote := {operator:Token, rhs:Token, local:void};
export Binary := {lhs:ParseTree, operator:Token, rhs:ParseTree};
export Unary  := {operator:Token, rhs:ParseTree};
export Postfix:= {lhs:ParseTree, operator:Token};
export ArrayParseTree := array(ParseTree);
export parenthesized := { left:Token, contents:ParseTree, right:Token };
export parentheses := { left:Token, right:Token };
export dummy := {position:Position};
export startScope := {scope:Scope, body:ParseTree};
export ParseTree := (
     Token or Adjacent or Binary or Unary or Postfix or parenthesized 
     or parentheses or IfThen or IfThenElse or startScope 
     or Quote or GlobalQuote or LocalQuote
     or TryElse or Try or WhileDo or For or WhileList or WhileListDo or Arrow or New or dummy );

-- misc

export TokenFile := {
     posFile:PosFile,
     last:(null or Token)
     };
export isatty(f:TokenFile):bool := isatty(f.posFile);

export Real := {v:double};
export CompiledFunction := {fn:fun,hash:int};
export CompiledFunctionClosure := {
     fn:function(Expr,Sequence):Expr,
     hash:int,
     env:Sequence
     };

-- Expr

export Sequence := array(Expr);
export Frame := {
     next:Frame, 
     scopenum:int,			  -- seqno of corresponding scope
     values:Sequence
     };
export dummyFrame := Frame(self,-1,Sequence());   -- self pointer depended on by structure.d:apply()
accountfor(sizeof(dummyFrame));
accountfor(sizeof(dummyFrame.values));
export FunctionClosure := { frame:Frame, model:functionCode };
export SymbolClosure := {frame:Frame, symbol:Symbol};
export List := {
     class:HashTable,
     v:Sequence,
     hash:int,
     mutable:bool
     };
export Error := {position:Position, message:string, report:Expr};
export Handle := {
     handle:int
     };
export Database := {
     filename:string,
     hash:int,
     handle:int,
     isopen:bool,
     mutable:bool
     };

export Boolean := {v:bool};
export Nothing := {nothing:void};

export Expr := (
     Real or Boolean or file or string or FunctionClosure or Error or Sequence
     or CompiledFunction or CompiledFunctionClosure
     or SymbolClosure or List or Rational or Integer 
     or HashTable or Handle or Database or Nothing or Net
     );
export fun := function(Expr):Expr;

export True := Expr(Boolean(true));	  -- don't make new ones!
export False := Expr(Boolean(false));	  -- use toBoolean instead
export toBoolean(v:bool):Expr := if v then True else False;

export nullE := Expr(Nothing());
export notfoundE := Expr(Nothing());			    -- internal use only, not visible to user

-- Code

export exprCode := {v:Expr,position:Position};
export variableCode := {v:Symbol,position:Position};
export CodeSequence := array(Code);
export unaryCode := {f:unop,rhs:Code,position:Position};
export binaryCode := {f:binop,lhs:Code,rhs:Code,position:Position};
export ternaryCode := {f:ternop,arg1:Code,arg2:Code,arg3:Code,position:Position};
export multaryCode := {f:multop, args:CodeSequence, position:Position};
export forCode := {fromclause:Code,toclause:Code, whenclause:Code,listclause:Code,doclause:Code,
     scope:Scope, position:Position} ;
export unop := function(Code):Expr;
export binop := function(Code,Code):Expr;
export ternop := function(Code,Code,Code):Expr;
export multop := function(CodeSequence):Expr;
export openScopeCode := {scope:Scope, body:Code};
export functionDescription := {
     scopenum:int,		    -- seqno of scope
     framesize:int,
     numparms:int,		    -- number of formal parameters
     restargs:bool,		    -- whether last parm gets rest of args
     hasClosure:bool		    -- whether a closure occurs inside
     };
export dummyDesc := functionDescription(-1,0,0,false,false);
export functionCode := { 
     parms:Code,			  -- just for display purposes
     body:Code, 
     desc:functionDescription
     };
export Code := (exprCode or variableCode 
     or unaryCode or binaryCode 
     or ternaryCode or multaryCode or forCode
     or CodeSequence or openScopeCode or functionCode
     );

-- scopes

export newDictionary():Dictionary := (
     Dictionary( new SymbolHashTable len 10 do provide NULL, 0));
export ScopeList := null or ScopeListCell;
export ScopeListCell := {scope:Scope, next:ScopeList};
export allScopes := ScopeList(NULL);
export numScopes := 0;
export globalScope := (
     s := Scope(newDictionary(),self,numScopes,0,false);
     numScopes = numScopes + 1;
     allScopes = ScopeListCell(s,allScopes);
     s
     );
export globalFrame := Frame(dummyFrame,globalScope.seqno,Sequence());
export localFrame := globalFrame;
export newScope(scope:Scope):Scope := (
     s := Scope(newDictionary(),scope,numScopes,0,true);
     numScopes = numScopes + 1;
     allScopes = ScopeListCell(s,allScopes);
     s
     );

-- hash tables for exprs

export KeyValuePair := {key:Expr, hash:int, value:Expr, next:KeyValuePair};
export HashTable := {
     table:array(KeyValuePair), -- length is always a power of 2, initially 2^0 == 1
     class:HashTable,
     parent:HashTable,
     numEntries:int,
     hash:int,
     mutable:bool
     };

-- dummies

dummyunary(w:Token,o:TokenFile,prec:int,obeylines:bool):ParseTree := (
     error("unary dummy used"); 
     w);
dummybinary(w:ParseTree,v:Token,o:TokenFile,prec:int,obeylines:bool):ParseTree := (
     error("binary dummy used"); 
     w);
makedummy():parseinfo := parseinfo(0,0,0,
     parsefuns(dummyunary,dummybinary));
export dummyWord    := Word("--dummy word--",TCnone,0,makedummy());
export dummyDictionary := newDictionary();

export dummyScope := (
     s := Scope(dummyDictionary,self,numScopes,0,false);
     numScopes = numScopes + 1;
     s);

export dummyTree    := ParseTree(dummy(dummyPosition));
export dummyCode := Code(exprCode(nullE,dummyPosition)); -- was Code(CodeSequence());
export emptySequence := Sequence();
export dummyUnaryFun(c:Code):Expr := (
     error("dummy unary function called");
     nullE);
export dummyPostfixFun(c:Code):Expr := (
     error("dummy postfix function called");
     nullE);
export dummyBinaryFun(c:Code,d:Code):Expr := (
     error("dummy binary function called");
     nullE);
export dummyTernaryFun(c:Code,d:Code,e:Code):Expr := (
     error("dummy ternary function called");
     nullE);
export emptynullE := Expr(Sequence());
export bucketEnd := KeyValuePair(emptynullE,0,emptynullE, self);
accountfor(sizeof(bucketEnd));
accountfor(sizeof(emptynullE));
export thingClass := HashTable(
     array(KeyValuePair)(bucketEnd,bucketEnd,bucketEnd,bucketEnd),
     self,self,0,(HashCounter = HashCounter + 1;HashCounter),
     true);
export dummySymbol   := Symbol(
     dummyWord,nextHash(),dummyPosition,
     dummyUnaryFun,dummyPostfixFun,dummyBinaryFun,
     globalScope.seqno,-1,1,true,true,false
     );
export dummyToken   := Token(dummyWord,dummyPosition,globalScope,dummySymbol,false);
export parseEOF     := makedummy();
export parseWORD    := makedummy();
export parseERRMSG  := makedummy();

export ExprEmptySequence := Expr(emptySequence);
-----------------------------------------------------------------------------
dummyclean(x:HashTable):void := nothing;
export cleanfun := dummyclean;			  -- filled in later
--export clean(x:HashTable):void := cleanfun(x);
-----------------------------------------------------------------------------
export newHashTable(class:HashTable,parent:HashTable):HashTable := (
     HashCounter = HashCounter + 1;
     HashTable(
	  array(KeyValuePair)(bucketEnd,bucketEnd,bucketEnd,bucketEnd),
	  -- we start with four empty buckets.  It is important for the 
	  -- enlarge/shrink code in objects.d that the number of buckets
	  -- here (four) is a power of two!
	  class,parent,0,HashCounter,
	  true				  -- mutable by default!
	  ));
export hashTableClass := newHashTable(thingClass,thingClass);
export mutableHashTableClass := newHashTable(thingClass,hashTableClass);
export typeClass := newHashTable(mutableHashTableClass,mutableHashTableClass);
       thingClass.class = typeClass;
       typeClass.class = typeClass;
       mutableHashTableClass.class = typeClass;
       hashTableClass.class = typeClass;
       newtypeof(parent:HashTable):HashTable := newHashTable(typeClass,parent);
       newbasictype():HashTable := newtypeof(thingClass);
export basicListClass := newbasictype();
export mutableListClass := newtypeof(basicListClass);
export timeClass := newtypeof(basicListClass);
export optionClass := newtypeof(basicListClass);
export fileClass := newbasictype();
export functionClass := newbasictype();
export symbolClass := newbasictype();
export errorClass := newbasictype();
export handleClass := newbasictype();
export netClass := newbasictype();
export stringClass := newtypeof(netClass);
export booleanClass := newbasictype();
export dbClass := newbasictype();
export symboltableClass := newtypeof(hashTableClass);

export visibleListClass := newtypeof(basicListClass);
export listClass := newtypeof(visibleListClass);
export sequenceClass := newtypeof(visibleListClass);
export arrayClass := newtypeof(visibleListClass);

export ringClass := newtypeof(typeClass);
       newbasicringtype():HashTable := newHashTable(ringClass,thingClass);
export integerClass := newbasicringtype();
export fieldClass := newtypeof(ringClass);
       newbasicfieldtype():HashTable := newHashTable(fieldClass,thingClass);
export rationalClass := newbasicfieldtype();
export doubleClass := newbasicfieldtype();
export nothingClass := newbasictype();

export (x:SymbolClosure) === (y:SymbolClosure) : bool := (
     x == y || x.symbol == y.symbol && x.frame == y.frame
     );
export (x:Symbol) === (y:SymbolClosure) : bool := x == y.symbol;
export (x:SymbolClosure) === (y:Symbol) : bool := x.symbol == y;
export (x:SymbolClosure) === (y:Expr):bool := (
     x == y || (
	  when y
	  is z:SymbolClosure do x.symbol == z.symbol && x.frame == z.frame
	  else false
	  ));
export (x:Expr) === (y:SymbolClosure):bool := (
     y == x || (
	  when x
	  is z:SymbolClosure do y.symbol == z.symbol && y.frame == z.frame
	  else false
	  ));
export (x:Symbol) === (y:Expr):bool := (
     when y is z:SymbolClosure do x == z.symbol else false
     );
export (x:Expr) === (y:Symbol):bool := (
     when x is z:SymbolClosure do y == z.symbol else false
     );
