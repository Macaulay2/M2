--		Copyright 1994 by Daniel R. Grayson
--- here we define the recursive types involved in parsing

use C;
use system;
use err;
use stdio;
use stdiop;
use strings;
use nets;
use gmp;
use engine;

export parsefuns := {
     unary:function(Token,TokenFile,int,bool):ParseTree,
     binary:function(ParseTree,Token,TokenFile,int,bool):ParseTree
     };
export parseinfo := {
     precedence:int,
     binaryStrength:int,
     unaryStrength:int,
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
export Symbol := {		    -- symbol table entry for a symbol
     word:Word,			    --   the word
     hash:int,			    --   just a counter, unique, unchanging
     position:Position,	    	    --   the position where the definition was made
     unary:unop,
     postfix:unop,
     binary:binop,
     frameID:int,		    -- seqno of frame for dictionary containing it, 0 indicates the globalFrame
     frameindex:int,		    -- index within the frame of its value
     lookupCount:int,		    -- number of times looked up
     protected:bool,	            -- whether protected against assignment by the user
     transient:bool,                -- whether its value could ever change, even if protected, e.g., commandLine, or by rebinding in a transient dictionary
     flagLookup:bool		    -- whether to warn when symbol is used
     };
export SymbolListCell := {entry:Symbol, next:SymbolList};
export SymbolList := null or SymbolListCell;
export SymbolHashTable := { 
     buckets:array(SymbolList),	 -- length always a power of 2
     numEntries:int
     };
export Dictionary := {
     hash:int,						    -- assigned sequentially
     symboltable:SymbolHashTable,
     outerDictionary:Dictionary,          -- next outer dictionary, or pointer to self if none
     frameID:int,	        -- -1 for dummy, 0 for global, then 1,2,3,...
     framesize:int,	        -- one for each symbol, even the erased ones; for transient frames only
     transient:bool,	        -- whether there can be multiple frames
     	       	    	        -- for the global dictionary and file scopes : no
				-- for function closures : yes
     protected:bool             -- whether symbols can be added; closing a package protects it
     };
export Token := {		-- a word, as encountered in the input
     word:Word,			--   the word
     position:Position,		--   the location where it was encountered
     dictionary:Dictionary,	--   the dictionary active at the time it was encountered
     entry:Symbol,     	  	--   the symbol table entry, found in the dictionary above, or one for wider lexical scope
     followsNewline:bool        --   whether it followed white space with a newline in it
     };

-- ParseTree

export Adjacent := {lhs:ParseTree, rhs:ParseTree};
export For := {
     forToken:Token, variable:ParseTree,
     fromClause:ParseTree, toClause:ParseTree, whenClause:ParseTree, listClause:ParseTree, doClause:ParseTree,
     dictionary:Dictionary					    -- filled in later
     };
export WhileDo := {
     whileToken:Token, predicate:ParseTree,
     dotoken:Token, doClause:ParseTree};
export WhileList := {
     whileToken:Token, predicate:ParseTree,
     listtoken:Token, listClause:ParseTree};
export WhileListDo := {
     whileToken:Token, predicate:ParseTree,
     listtoken:Token, listClause:ParseTree,
     dotoken:Token, doClause:ParseTree
     };
export TryElse := {
     tryToken:Token, primary:ParseTree,
     elseToken:Token, alternate:ParseTree};
export Try := { tryToken:Token, primary:ParseTree};
export IfThen := { ifToken:Token, predicate:ParseTree, thenclause:ParseTree };
export IfThenElse := { ifToken:Token, predicate:ParseTree, thenclause:ParseTree, elseClause:ParseTree};
export New := { newtoken:Token, newclass:ParseTree, newparent:ParseTree, newinitializer:ParseTree};
export Arrow := {lhs:ParseTree, operator:Token, rhs:ParseTree, desc:functionDescription};
export Quote := {operator:Token, rhs:Token};
export GlobalQuote := {operator:Token, rhs:Token, global:void};
export LocalQuote := {operator:Token, rhs:Token, local:void};
export Binary := {lhs:ParseTree, operator:Token, rhs:ParseTree};
export Unary  := {operator:Token, rhs:ParseTree};
export Postfix:= {lhs:ParseTree, operator:Token};
export ArrayParseTree := array(ParseTree);
export Parentheses := { left:Token, contents:ParseTree, right:Token };
export EmptyParentheses := { left:Token, right:Token };
export dummy := {position:Position};
export StartDictionary := {dictionary:Dictionary, body:ParseTree};
export ParseTree := (
     Token or Adjacent or Binary or Unary or Postfix or Parentheses 
     or EmptyParentheses or IfThen or IfThenElse or StartDictionary 
     or Quote or GlobalQuote or LocalQuote
     or TryElse or Try or WhileDo or For or WhileList or WhileListDo or Arrow or New or dummy );

-- misc

export TokenFile := {
     posFile:PosFile,
     last:(null or Token)
     };
export fileErrorMessage(f:TokenFile):string := fileErrorMessage(f.posFile);
export fileError(f:TokenFile):bool := fileError(f.posFile);
export isatty(f:TokenFile):bool := isatty(f.posFile);

export CompiledFunction := {fn:fun,hash:int};
export CompiledFunctionClosure := {
     fn:function(Expr,Sequence):Expr,
     hash:int,
     env:Sequence
     };

-- Expr

export Sequence := array(Expr);
export Frame := {
     outerFrame:Frame, 
     frameID:int,			  -- seqno of corresponding dictionary
     valuesUsed:int,      -- sigh, we really need this only for static frames
     values:Sequence
     };
export FrameLocation := {
     frame:Frame,
     frameindex:int
     };
export DictionaryClosure := {
     frame:Frame,					    -- every symbol in the dictionary has the same frameID as this frame does
     dictionary:Dictionary
     };
export FunctionClosure := { frame:Frame, model:functionCode };
export SymbolClosure := {
     frame:Frame,      -- this is a frame whose frameID is the same as that of the symbol
     symbol:Symbol
     };
export List := {
     class:HashTable,
     v:Sequence,
     hash:int,
     mutable:bool
     };
export Error := {position:Position, message:string, report:Expr, value:Expr};
export Database := {
     filename:string,
     hash:int,
     handle:int,
     isopen:bool,
     mutable:bool
     };

export Boolean := {v:bool};
export Nothing := {nothing:void};
export Real := {v:double};
export Complex := { re:double, im:double };

export Expr := (
     BigComplex or
     BigReal or
     Boolean or
     CompiledFunction or
     CompiledFunctionClosure or
     Complex or
     Database or
     DictionaryClosure or 
     Error or
     FunctionClosure or
     HashTable or
     Integer or
     LMatrixRR or
     LMatrixCC or     
     List or
     Net or
     Nothing or
     Rational or
     RawComputation or
     RawFreeModule or
     RawMatrix or
     RawMonoid or
     RawMonomial or
     RawMonomialIdeal or
     RawMonomialOrdering or
     RawMutableMatrix or
     RawRing or
     RawRingElement or
     RawRingMap or
     RawVector or
     Real or
     Sequence or
     SymbolClosure or
     file or
     string
     );
export fun := function(Expr):Expr;

export True := Expr(Boolean(true));	  -- don't make new ones!
export False := Expr(Boolean(false));	  -- use toExpr instead
export toExpr(v:bool):Expr := if v then True else False;

export nullE := Expr(Nothing());
export notfoundE := Expr(Nothing());			    -- internal use only, not visible to user

-- Code

export localSymbolClosureCode := {
     nestingDepth:int,
     symbol:Symbol,
     position:Position
     };
export globalSymbolClosureCode := {
     symbol:Symbol,
     position:Position
     };
export localMemoryReferenceCode := {
     nestingDepth:int,
     frameindex:int,
     position:Position
     };
export globalMemoryReferenceCode := {
     frameindex:int,
     position:Position
     };
export localAssignmentCode := {
     nestingDepth:int,
     frameindex:int,
     rhs:Code,
     position:Position
     };
export globalAssignmentCode := {
     lhs:Symbol,
     rhs:Code,
     position:Position
     };

export SymbolSequence := array(Symbol);
export parallelAssignmentCode := {
     nestingDepth:array(int), -- spots corresponding to local variables are filled with -1
     frameindex:array(int),
     lhs:SymbolSequence, -- spots corresponding to local variables are filled with dummySymbol
     rhs:Code,
     position:Position};

export nullCode := {};
export realCode := {x:double,position:Position};
export integerCode := {x:Integer,position:Position};
export stringCode := {x:string,position:Position};
export unaryCode := {f:unop,rhs:Code,position:Position};
export binaryCode := {f:binop,lhs:Code,rhs:Code,position:Position};
export ternaryCode := {f:ternop,arg1:Code,arg2:Code,arg3:Code,position:Position};

export CodeSequence := array(Code);
export sequenceCode := {x:CodeSequence, position:Position};
export listCode     := {y:CodeSequence, position:Position};
export arrayCode    := {z:CodeSequence, position:Position};
export multaryCode := {f:multop, args:CodeSequence, position:Position};
export forCode := {fromClause:Code,toClause:Code, whenClause:Code,listClause:Code,doClause:Code,
     dictionary:Dictionary, position:Position} ;
export unop := function(Code):Expr;
export binop := function(Code,Code):Expr;
export ternop := function(Code,Code,Code):Expr;
export multop := function(CodeSequence):Expr;
export openDictionaryCode := {
     dictionary:Dictionary,				    -- soon to be obsolete
     body:Code
     };
export functionDescription := {
     frameID:int,		    -- seqno of dictionary
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
export Code := (
     nullCode
     or realCode
     or stringCode
     or integerCode
     or globalMemoryReferenceCode
     or localMemoryReferenceCode
     or globalAssignmentCode
     or localAssignmentCode
     or parallelAssignmentCode
     or globalSymbolClosureCode 
     or localSymbolClosureCode
     or unaryCode or binaryCode 
     or ternaryCode or multaryCode or forCode
     or sequenceCode
     or listCode
     or arrayCode
     or openDictionaryCode				    -- soon obsolete
     or functionCode
     );

-- scopes

export newSymbolHashTable():SymbolHashTable := SymbolHashTable( 
     new array(SymbolList) 
     len 8						    -- must be a power of 2, for our hashing to work
     do provide NULL,
     0);

export dummyFrame := Frame(self,
     -1,						    -- negative frame id's are ignored and give warning messages
     0,
     Sequence());

dummySymbolFrameIndex := 0;
globalFramesize := dummySymbolFrameIndex+1;
export globalFrame := Frame(self, 0, globalFramesize, 
     Sequence(
	  nullE						    -- one value for dummySymbol
	  ));

export Macaulay2Dictionary := Dictionary(nextHash(),newSymbolHashTable(),self,0,globalFramesize,false,false);

export LocalDictionaryList := {
     dictionary:Dictionary,
     next:LocalDictionaryList				    -- pointer to self indicates end
     };
     
allDictionaries := LocalDictionaryList(Macaulay2Dictionary,self);

export newGlobalDictionary():Dictionary := (
     d := Dictionary(nextHash(),newSymbolHashTable(),self,0,0,false,false);
     allDictionaries = LocalDictionaryList(d,allDictionaries);
     d);

export globalDictionary := Macaulay2Dictionary;

numLocalDictionaries := 0;
export localFrame := dummyFrame;

export dummySymbolHashTable := newSymbolHashTable();

export dummyDictionary := (
     numLocalDictionaries = numLocalDictionaries + 1;
     Dictionary(nextHash(),dummySymbolHashTable,self,numLocalDictionaries,0,false,true));

export getLocalDictionary(frameID:int):Dictionary := (
     p := allDictionaries;
     while (
	  if p.dictionary.frameID == frameID then return(p.dictionary);
	  p != p.next) do p = p.next;
     error("internal error: local dictionary with frameID " + tostring(frameID) + " not found");
     dummyDictionary);
export localDictionaryClosure(f:Frame):DictionaryClosure := DictionaryClosure(f,getLocalDictionary(f.frameID));

export newLocalDictionary(dictionary:Dictionary):Dictionary := (
     numLocalDictionaries = numLocalDictionaries + 1;
     d := Dictionary(nextHash(),newSymbolHashTable(),dictionary,numLocalDictionaries,0,true,false);
     allDictionaries = LocalDictionaryList(d,allDictionaries);
     d);
export newStaticLocalDictionary():Dictionary := (
     numLocalDictionaries = numLocalDictionaries + 1;
     d := Dictionary(nextHash(),newSymbolHashTable(),self,numLocalDictionaries,
	  0,      -- 0 for the global frame containing the static symbols' values
	  false,  -- the first local dictionary is usually (?) non-transient
	  false
	  );
     allDictionaries = LocalDictionaryList(d,allDictionaries);
     d);
export emptyLocalDictionary := newStaticLocalDictionary();

export newLocalFrame(d:Dictionary):Frame := Frame(self, d.frameID, d.framesize, new Sequence len d.framesize do provide nullE);
export newLocalFrame(outerFrame:Frame,d:Dictionary):Frame := Frame(outerFrame, d.frameID, d.framesize, new Sequence len d.framesize do provide nullE);
export newLocalDictionaryClosure(d:Dictionary):DictionaryClosure := DictionaryClosure(newLocalFrame(d),d);
export newStaticLocalDictionaryClosure():DictionaryClosure := (
     d := newStaticLocalDictionary();
     allDictionaries = LocalDictionaryList(d,allDictionaries);
     DictionaryClosure(newLocalFrame(d),d));

export newStaticLocalDictionaryClosure(dc:DictionaryClosure):DictionaryClosure := (
     d := newLocalDictionary(dc.dictionary);
     d.transient = false;
     allDictionaries = LocalDictionaryList(d,allDictionaries);
     f := newLocalFrame(dc.frame,d);
     DictionaryClosure(f,d));

export emptyFrame := newLocalFrame(emptyLocalDictionary);
export emptyDictionaryClosure := DictionaryClosure(emptyFrame,emptyLocalDictionary);

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

-- more dummies

dummyunary(w:Token,o:TokenFile,prec:int,obeylines:bool):ParseTree := (
     error("unary dummy used"); 
     w);
dummybinary(w:ParseTree,v:Token,o:TokenFile,prec:int,obeylines:bool):ParseTree := (
     error("binary dummy used"); 
     w);
export nopr := -1;						    -- represents unused precedence
export newParseinfo():parseinfo := parseinfo(nopr,nopr,nopr,parsefuns(dummyunary,dummybinary));
export dummyWord    := Word("--dummy word--",TCnone,0,newParseinfo());

export dummyTree    := ParseTree(dummy(dummyPosition));
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
export emptySequenceE := Expr(emptySequence);
export bucketEnd := KeyValuePair(nullE,0,nullE,self);
export dummySymbol := Symbol(
     dummyWord,nextHash(),dummyPosition,
     dummyUnaryFun,dummyPostfixFun,dummyBinaryFun,
     Macaulay2Dictionary.frameID,dummySymbolFrameIndex,1,
     false,						    -- not protected, so we can use it in parallelAssignmentFun
     true,false
     );
dummySymbolClosure := SymbolClosure(globalFrame,dummySymbol);
globalFrame.values.dummySymbolFrameIndex = Expr(dummySymbolClosure);
export dummyCode := Code(globalMemoryReferenceCode(0,dummyPosition));
export dummyToken   := Token(dummyWord,dummyPosition,Macaulay2Dictionary,dummySymbol,false);
export parseEOF     := newParseinfo();
export parseWORD    := newParseinfo();

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
export thingClass := HashTable(
     array(KeyValuePair)(bucketEnd,bucketEnd,bucketEnd,bucketEnd),
	  -- we start with four empty buckets.  It is important for the 
	  -- enlarge/shrink code in objects.d that the number of buckets
	  -- here (four) is a power of two!
     self,self,0,(HashCounter = HashCounter + 1;HashCounter),
     true);
export hashTableClass := newHashTable(thingClass,thingClass);
export mutableHashTableClass := newHashTable(thingClass,hashTableClass);
export typeClass := newHashTable(mutableHashTableClass,mutableHashTableClass);
       thingClass.class = typeClass;
       typeClass.class = typeClass;
       mutableHashTableClass.class = typeClass;
       hashTableClass.class = typeClass;
       newtypeof(parent:HashTable):HashTable := newHashTable(typeClass,parent);
       newbasictype():HashTable := newtypeof(thingClass);
export cacheTableClass := newtypeof(mutableHashTableClass);
export basicListClass := newbasictype();
export mutableListClass := newtypeof(basicListClass);
export timeClass := newtypeof(basicListClass);
export optionClass := newtypeof(basicListClass);
export fileClass := newbasictype();
export functionClass := newbasictype();
export symbolClass := newbasictype();
export errorClass := newbasictype();
export netClass := newbasictype();
export stringClass := newtypeof(netClass);
export booleanClass := newbasictype();
export dictionaryClass := newbasictype();
export dbClass := newbasictype();

export visibleListClass := newtypeof(basicListClass);
export listClass := newtypeof(visibleListClass);
export sequenceClass := newtypeof(visibleListClass);
export arrayClass := newtypeof(visibleListClass);

export ringClass := newtypeof(typeClass);
       newbasicringtype():HashTable := newHashTable(ringClass,thingClass);
export integerClass := newbasicringtype();
export rationalClass := newbasicringtype();
export bigRealClass := newbasicringtype();
export bigComplexClass := newbasicringtype();
export doubleClass := newbasicringtype();
export complexClass := newbasicringtype();
export rawObjectClass := newbasictype();		    -- RawObject
export rawMonomialClass := newtypeof(rawObjectClass);	    -- RawMonomial
export rawMonomialOrderingClass := newtypeof(rawObjectClass); -- RawMonomialOrdering
export rawMonoidClass := newtypeof(rawObjectClass);	    -- RawMonoid
export rawMonomialIdealClass := newtypeof(rawObjectClass);  -- RawMonomialIdeal
export rawRingClass := newtypeof(rawObjectClass);	    -- RawRing
export rawRingElementClass := newtypeof(rawObjectClass);    -- RawRingElement
export rawRingMapClass := newtypeof(rawObjectClass);	    -- RawRingMap
export rawFreeModuleClass := newtypeof(rawObjectClass);	    -- RawFreeModule
export rawVectorClass := newtypeof(rawObjectClass);	    -- RawVector
export rawMatrixClass := newtypeof(rawObjectClass);	    -- RawMatrix
export rawMutableMatrixClass := newtypeof(rawObjectClass);  -- RawMutableMatrix
export rawComputationClass := newtypeof(rawObjectClass);	    -- RawComputation
export LMatrixRRClass := newtypeof(rawObjectClass);         -- LMatrixRRClass
export LMatrixCCClass := newtypeof(rawObjectClass);         -- LMatrixCCClass
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
