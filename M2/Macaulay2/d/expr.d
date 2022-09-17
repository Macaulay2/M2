--This file contains functions that operate on Exprs.
--It also contains misc dummy declarations.
--Finally classes are declared at the end of this file.
--Functions in this file should not use stdio so that expr can be used by stdio.

use pthread0;
use gmp;
use xml;
use engine;
use varnets;
use parse;
use stdio0;
use strings;
--Note: scclib.c uses interrupts, so use it here so that it gets included in expr-exports.h
use interrupts;

export threadLocal debugLevel := 0;
export threadLocal engineDebugLevel := 0;

--Current function depth
threadLocal export recursionDepth := 0;
--Maximum function depth before triggering errors
threadLocal export recursionLimit := 300;


threadCounter := 0;
threadLocal HashCounter := ( threadCounter = threadCounter + 1; 1000000 + 3 + (threadCounter-1) * 10000 );

export nextHash():int := (
     HashCounter = HashCounter + 1;
     if HashCounter < 0 -- check for integer overflow
     then Ccode(void, " fprintf(stderr, \" *** hash code serial number counter overflow (too many mutable objects created)\\n\"); abort(); ");
     HashCounter);

export NULL ::= null();

-- scopes

export newSymbolHashTable():SymbolHashTable := SymbolHashTable( 
     new array(SymbolList) 
     len 8						    -- must be a power of 2, for our hashing to work
     do provide NULL,
     0,uninitializedSpinLock);

export dummyFrame := Frame(self,
     -1,						    -- negative frame id's are ignored and give warning messages
     0,
     true,
     Sequence());

export dummySymbolFrameIndex := 0;
export globalFrameID := 0;
globalFramesize := dummySymbolFrameIndex+1;
export globalFrame := Frame(self, 0, globalFramesize, true, 
     Sequence(
	  nullE						    -- one value for dummySymbol
	  ));
export threadFrameID := 0;
export threadFramesize := 0;
export threadLocal threadFrame  := Frame(self, threadFrameID, 0, true, Sequence());
export enlarge(f:Frame):int := (
     n := f.valuesUsed;
     f.valuesUsed = n + 1;
     if f.valuesUsed > length(f.values) then (
	  f.values = new Sequence len 2 * length(f.values) + 1 do (
	       foreach value in f.values do provide value;
	       while true do provide nullE));
     n);
export enlargeThreadFrame():Frame := (
     if threadFramesize > length(threadFrame.values) then (
	  threadFrame.values = new Sequence len 2 * threadFramesize + 1 do (
	       foreach value in threadFrame.values do provide value;
	       while true do provide nullE));
     threadFrame);
export setGlobalVariable(x:Symbol,y:Expr):void := (if x.thread then enlargeThreadFrame() else globalFrame).values.(x.frameindex) = y;
export getGlobalVariable(x:Symbol):Expr := (if x.thread then enlargeThreadFrame() else globalFrame).values.(x.frameindex);
export globalDictionary := 
export Macaulay2Dictionary := Dictionary(nextHash(),
     newSymbolHashTable(),self,globalFrameID,
     globalFramesize,false,false,false);
export completions(s:string):array(string) := (
     n := length(s);
     if n == 0 then return array(string)();		    -- don't complete the null string
     v := newvarstringarray(6);
     d := globalDictionary;
     while (
	  foreach bucket in d.symboltable.buckets do (
	       b := bucket;
	       while true do when b
	       is null do break
	       is q:SymbolListCell do (
		    t := q.word.name;
		    if isalnum(t.0) && n <= length(t) && 0 == strncmp(s,t,n) then append(v,t);
		    b = q.next; ));
	  d != d.outerDictionary) do d = d.outerDictionary;
     extract(v));
export DictionaryList := {
     dictionary:Dictionary,
     next:DictionaryList				    -- pointer to self indicates end
     };
export allDictionaries := DictionaryList(Macaulay2Dictionary,self);
record(d:Dictionary):Dictionary := (
     allDictionaries = DictionaryList(d,allDictionaries);
     d);     
export newGlobalDictionary():Dictionary := record(Dictionary(nextHash(),newSymbolHashTable(),self,0,0,false,false,false));
numLocalDictionaries := threadFrameID;
export threadLocal localFrame := dummyFrame;
export dummySymbolHashTable := newSymbolHashTable();
export dummyDictionary := (
     numLocalDictionaries = numLocalDictionaries + 1;
     Dictionary(nextHash(),dummySymbolHashTable,self,numLocalDictionaries,0,false,true,false));
export newLocalDictionary(dictionary:Dictionary):Dictionary := (
     numLocalDictionaries = numLocalDictionaries + 1;
     record(Dictionary(nextHash(),newSymbolHashTable(),dictionary,numLocalDictionaries,0,true,false,false)));
export newStaticLocalDictionary():Dictionary := (
     numLocalDictionaries = numLocalDictionaries + 1;
     record(
	  Dictionary(nextHash(),newSymbolHashTable(),self,numLocalDictionaries,
	       0,			     -- 0 for the global frame containing the static symbols' values
	       false,			  -- the first local dictionary is usually (?) non-transient
	       false,
	       false
	       )));
export emptyLocalDictionary := newStaticLocalDictionary();

export newLocalFrame(d:Dictionary):Frame := Frame(self, d.frameID, d.framesize, false, new Sequence len d.framesize do provide nullE);
export newLocalFrame(outerFrame:Frame,d:Dictionary):Frame := Frame(outerFrame, d.frameID, d.framesize, false, new Sequence len d.framesize do provide nullE);
export newStaticLocalDictionaryClosure():DictionaryClosure := (
     d := record(newStaticLocalDictionary());
     DictionaryClosure(noRecycle(newLocalFrame(d)),d));

export newStaticLocalDictionaryClosure(dc:DictionaryClosure):DictionaryClosure := (
     d := record(newLocalDictionary(dc.dictionary));
     d.transient = false;
     DictionaryClosure(noRecycle(newLocalFrame(dc.frame,d)),d));

export emptyFrame := newLocalFrame(emptyLocalDictionary);
emptyFrame.notrecyclable = true;

export dictionaryDepth(d:Dictionary):int := (
     i := 0;
     while d.outerDictionary != d do (i = i+1; d = d.outerDictionary);
     i);
export isglobaldict(d:Dictionary):bool := !d.transient && d.frameID == 0;



-----------------------------------------------------------------------------

export bucketEnd := KeyValuePair(nullE,0,nullE,self);
--Dummy Symbol needs to go in tokens because it needs error support.
--To preserve existing hashes of types we use this workaround and preallocate it a hash number
export dummySymbolHash := nextHash();
export newHashTable(Class:HashTable,parent:HashTable):HashTable := (
     ht:= HashTable(
	  array(KeyValuePair)(bucketEnd,bucketEnd,bucketEnd,bucketEnd),
	  -- we start with four empty buckets.  It is important for the 
	  -- enlarge/shrink code in hashtable.dd that the number of buckets
	  -- (here four) is a power of two
	  Class,parent,0,nextHash(),
	  true,				  -- mutable by default; careful: other routines depend on this
	  false,
	  uninitializedSpinLock
	  ); init(ht.mutex); ht);


--More dummy declarations


dummyunary(w:Token,o:TokenFile,prec:int,obeylines:bool):ParseTree := (
     anywhereError("unary dummy used"); 
     w);
dummybinary(w:ParseTree,v:Token,o:TokenFile,prec:int,obeylines:bool):ParseTree := (
     anywhereError("binary dummy used"); 
     w);
export nopr := -1;						    -- represents unused precedence
export newParseinfo():parseinfo := parseinfo(nopr,nopr,nopr,parsefuns(dummyunary,dummybinary));
export dummyUnaryFun(c:Code):Expr := (
     anywhereError("dummy unary function called");
     nullE);
export dummyPostfixFun(c:Code):Expr := (
     anywhereError("dummy postfix function called");
     nullE);
export dummyBinaryFun(c:Code,d:Code):Expr := (
     anywhereError("dummy binary function called");
     nullE);
export dummyTernaryFun(c:Code,d:Code,e:Code):Expr := (
     anywhereError("dummy ternary function called");
     nullE);

export emptySequence := Sequence();

export emptySequenceE := Expr(emptySequence);

export dummySymbol := Symbol(
     Word("-*dummy symbol*-",TCnone,0,newParseinfo()),dummySymbolHash,dummyPosition,
     dummyUnaryFun,dummyPostfixFun,dummyBinaryFun,
     Macaulay2Dictionary.frameID,dummySymbolFrameIndex,1,
     false,						    -- not protected, so we can use it in parallelAssignmentFun
     false,
     false,
     0
     );
dummySymbolClosure := SymbolClosure(globalFrame,dummySymbol);
globalFrame.values.dummySymbolFrameIndex = Expr(dummySymbolClosure);
export dummyCode := Code(nullCode());
export NullCode := Code(nullCode());
export dummyCodeClosure := CodeClosure(dummyFrame,dummyCode);
export dummyToken   := Token(
     Word("-*dummy token*-",TCnone,0,newParseinfo()),
     dummyPosition.filename,
     dummyPosition.line,
     dummyPosition.column,
     dummyPosition.loadDepth,
     Macaulay2Dictionary,dummySymbol,false);

export parseWORD    := newParseinfo();			    -- parsing functions filled in later

export dummyWord    := Word("-*dummy word*-",TCnone,0,newParseinfo());

export dummyTree    := ParseTree(dummy(dummyPosition));

---- class declarations


export thingClass := (
     ht:= HashTable(
          array(KeyValuePair)(bucketEnd,bucketEnd,bucketEnd,bucketEnd),
          -- we start with four empty buckets.  It is important for the 
	  -- enlarge/shrink code in objects.d that the number of buckets
	  -- here (four) is a power of two
          self,self,0,nextHash(),
          true,false, uninitializedSpinLock);
	  init(ht.mutex); ht);

export hashTableClass := newHashTable(thingClass,thingClass);
export mutableHashTableClass := newHashTable(thingClass,hashTableClass);
export typeClass := newHashTable(mutableHashTableClass,mutableHashTableClass);
       thingClass.Class = typeClass;
       typeClass.Class = typeClass;
       mutableHashTableClass.Class = typeClass;
       hashTableClass.Class = typeClass;
       newtypeof(parent:HashTable):HashTable := newHashTable(typeClass,parent);
       newbasictype():HashTable := newtypeof(thingClass);
export cacheTableClass := newtypeof(mutableHashTableClass);
export basicListClass := newbasictype();
export mutableListClass := newtypeof(basicListClass);
export timeClass := newtypeof(basicListClass);
export optionClass := newtypeof(basicListClass);
export optionTableClass := newtypeof(hashTableClass);
export fileClass := newbasictype();
export functionClass := newbasictype();
export functionClosureClass := newtypeof(functionClass);
export compiledFunctionClass := newtypeof(functionClass);
export compiledFunctionClosureClass := newtypeof(functionClass);
export symbolClass := newbasictype();
export keywordClass := newtypeof(symbolClass);
export codeClass := newbasictype();
export mysqlConnectionClass := newbasictype();
export mysqlFieldClass := newbasictype();
export mysqlResultClass := newbasictype();
export functionBodyClass := newbasictype();
export compiledFunctionBodyClass := newbasictype();
export errorClass := newbasictype();
export netClass := newbasictype();
export netFileClass := newbasictype();
export stringClass := newtypeof(netClass);
export booleanClass := newbasictype();
export dictionaryClass := newbasictype();
export localDictionaryClass := newtypeof(dictionaryClass);
export globalDictionaryClass := newtypeof(dictionaryClass);
export dbClass := newbasictype();

export visibleListClass := newtypeof(basicListClass);
export listClass := newtypeof(visibleListClass);
export sequenceClass := newtypeof(visibleListClass);
export arrayClass := newtypeof(visibleListClass);
export errorMessageClass := newtypeof(basicListClass);
export missingMethodClass := newtypeof(errorMessageClass);

export ringClass := newtypeof(typeClass);
export ringElementClass := newtypeof(basicListClass);
export numberClass := newtypeof(thingClass);
export inexactNumberClass := newtypeof(numberClass);

       newnumbertype():HashTable := newHashTable(ringClass,numberClass);
export ZZClass := newnumbertype();
export QQClass := newnumbertype();

export ringFamilyClass := newtypeof(typeClass);
export inexactNumberTypeClass := newtypeof(ringFamilyClass);
       newbignumbertype():HashTable := newHashTable(inexactNumberTypeClass,inexactNumberClass);
export RRClass := newbignumbertype();
export CCClass := newbignumbertype();

export rawObjectClass := newbasictype();		    -- RawObject
export rawMonomialClass := newtypeof(rawObjectClass);	    -- RawMonomial
export rawMonomialOrderingClass := newtypeof(rawObjectClass); -- RawMonomialOrdering
export rawMonoidClass := newtypeof(rawObjectClass);	    -- RawMonoid
export rawMonomialIdealClass := newtypeof(rawObjectClass);  -- RawMonomialIdeal
export rawRingClass := newtypeof(rawObjectClass);	    -- RawRing
export rawRingElementClass := newtypeof(rawObjectClass);    -- RawRingElement
export rawRingMapClass := newtypeof(rawObjectClass);	    -- RawRingMap
export rawFreeModuleClass := newtypeof(rawObjectClass);	    -- RawFreeModule
export rawMatrixClass := newtypeof(rawObjectClass);	    -- RawMatrix
export rawMutableMatrixClass := newtypeof(rawObjectClass);	    -- RawMutableMatrix
export rawStraightLineProgramClass := newtypeof(rawObjectClass);    -- RawStraightLineProgram
export rawComputationClass := newtypeof(rawObjectClass);	    -- RawComputation
export nothingClass := newbasictype(); -- we are testing, in basictests/hashcodes.m2, that the hash code of this one doesn't change
export rawPathTrackerClass := newtypeof(rawObjectClass);    -- RawPathTracker
export pythonObjectClass := newbasictype();
export xmlNodeClass := newbasictype();
export xmlAttrClass := newbasictype();
export taskClass := newbasictype();
export symbolBodyClass := newbasictype();
export fileOutputSyncStateClass := newbasictype();
-- NAG begin
export rawHomotopyClass := newtypeof(rawObjectClass);    -- RawHomotopy
export rawSLEvaluatorClass := newtypeof(rawObjectClass);    -- RawSLEvaluator
export rawSLProgramClass := newtypeof(rawObjectClass);    -- RawSLProgram
export rawPointArrayClass := newtypeof(rawObjectClass);    -- RawPointArray
-- NAG end
export rawMutableComplexClass := newtypeof(rawObjectClass);	    -- RawMutableComplex
export angleBarListClass := newtypeof(visibleListClass);
export RRiClass := newbignumbertype();
export pointerClass := newbasictype();
-- all new types, dictionaries, and classes go just above this line, if possible, so hash codes don't change gratuitously!


--Error Handling 
export buildErrorPacket(message:string):Expr := Expr(Error(dummyPosition,message,nullE,false,dummyFrame));
export buildErrorPacketErrno(msg:string,errnum:int):Expr := buildErrorPacket( msg + ": " + strerror(errnum) );

export quoteit(name:string):string := "'" + name + "'";
export NotYet(desc:string):Expr := buildErrorPacket(desc + " not implemented yet");
export WrongArg(desc:string):Expr := buildErrorPacket("expected " + desc);
export WrongArg(n:int,desc:string):Expr := (
     buildErrorPacket("expected argument " + tostring(n) + " to be " + desc));
export WrongArgZZ():Expr := WrongArg("an integer");
export WrongArgZZ(n:int):Expr := WrongArg(n,"an integer");
export WrongArgRR():Expr := WrongArg("a real number");
export WrongArgRR(n:int):Expr := WrongArg(n,"a real number");
export WrongArgRRorRRi():Expr := WrongArg("a real number or interval");
export WrongArgRRorRRi(n:int):Expr := WrongArg(n,"a real number or interval");
export WrongArgSmallInteger():Expr := WrongArg("a small integer");
export WrongArgSmallInteger(n:int):Expr := WrongArg(n,"a small integer");
export WrongArgSmallUInteger():Expr := WrongArg("a small non-negative integer");
export WrongArgSmallUInteger(n:int):Expr := WrongArg(n,"a small non-negative integer");
export WrongArgString():Expr := WrongArg("a string");
export WrongArgString(n:int):Expr := WrongArg(n,"a string");
export WrongArgBoolean():Expr := WrongArg("true or false");
export WrongArgBoolean(n:int):Expr := WrongArg(n,"true or false");
export WrongArgMutableMatrix(n:int):Expr := WrongArg(n,"a raw mutable matrix");
export WrongArgMutableMatrix():Expr := WrongArg("a raw mutable matrix");
export WrongArgMatrix(n:int):Expr := WrongArg(n,"a raw matrix");
export WrongArgMatrix():Expr := WrongArg("a raw matrix");
export ArgChanged(name:string,n:int):Expr := (
     buildErrorPacket(quoteit(name) + " expected argument " + tostring(n)
	  + " not to change its type during execution"));
export WrongNumArgs(name:string,n:int):Expr := (
     if n == 0
     then buildErrorPacket(quoteit(name) + " expected no arguments")
     else if n == 1
     then buildErrorPacket(quoteit(name) + " expected " + tostring(n) + " argument")
     else buildErrorPacket(quoteit(name) + " expected " + tostring(n) + " arguments")
     );
export WrongNumArgs(n:int):Expr := buildErrorPacket(
     if n == 0 then "expected no arguments"
     else if n == 1 then "expected " + tostring(n) + " argument"
     else "expected " + tostring(n) + " arguments"
     );
export WrongNumArgs(name:string,m:int,n:int):Expr := (
     if n == m+1
     then buildErrorPacket(quoteit(name) + " expected " 
	  + tostring(m) + " or "
	  + tostring(n) + " arguments")
     else buildErrorPacket(quoteit(name) + " expected " 
	  + tostring(m) + " to "
	  + tostring(n) + " arguments"));
export WrongNumArgs(m:int,n:int):Expr := (
     if n == m+1
     then buildErrorPacket("expected " + tostring(m) + " or " + tostring(n) + " arguments")
     else buildErrorPacket("expected " + tostring(m) + " to " + tostring(n) + " arguments"));
export TooFewArgs(name:string,m:int):Expr := (
     if m == 1
     then buildErrorPacket(quoteit(name) + " expected at least 1 argument")
     else buildErrorPacket(quoteit(name) + " expected at least " 
	  + tostring(m) + " arguments"));
export TooManyArgs(name:string,m:int):Expr := (
     if m == 1
     then buildErrorPacket(quoteit(name) + " expected at most 1 argument")
     else buildErrorPacket(quoteit(name) + " expected at most " 
	  + tostring(m) + " arguments"));


export MissingMethod(name:string,method:string):Expr := buildErrorPacket(quoteit(name) + " expected item to have a method for " + method);
export MissingMethod(method:SymbolClosure):Expr := buildErrorPacket("expected a method for "+quoteit(method.symbol.word.name));
export MissingMethodPair(method:string):Expr := buildErrorPacket("expected pair to have a method for "+quoteit(method));
export MissingMethodPair(method:SymbolClosure):Expr := buildErrorPacket("expected pair to have a method for " + quoteit(method.symbol.word.name));
export MissingMethodPair(method:SymbolClosure,left:Expr,right:Expr):Expr := buildErrorPacket( "expected pair to have a method for " + quoteit(method.symbol.word.name) );
export MissingMethodPair(methodname:string,left:Expr,right:Expr):Expr := buildErrorPacket( "expected pair to have a method for " + quoteit(methodname) );
export MissingAssignmentMethod(method:Expr,left:Expr):Expr := (
     when method is sc:SymbolClosure do buildErrorPacket("expected object to have an assignment method for " + quoteit(sc.symbol.word.name))
     else buildErrorPacket("expected object to have an assignment method"));
export MissingAssignmentMethodPair(method:Expr,left:Expr,right:Expr):Expr := (
     when method is sc:SymbolClosure do buildErrorPacket("expected pair to have an assignment method for " + quoteit(sc.symbol.word.name))
     else buildErrorPacket("expected pair to have an assignment method"));
