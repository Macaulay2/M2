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
--Note: scclib.c uses interrupts, so use it here so that it gets included in expr-exports.h
use interrupts;

export threadLocal debugLevel := 0;
export threadLocal engineDebugLevel := 0;

threadCounter := 0;
threadLocal HashCounter := ( threadCounter = threadCounter + 1; 1000000 + (threadCounter-1) * 10000 );
export nextHash():int := (
     HashCounter = HashCounter + 1;
     HashCounter);

--STDERR file ash workaround to preserve numbering scheme
nextHash();
--Dummy file hash workaround to preserve numbering scheme
nextHash();
--STDIO file hash workaround to preserve numbering scheme
nextHash();

export NULL ::= null();


-- scopes

export newSymbolHashTable():SymbolHashTable := SymbolHashTable( 
     new array(SymbolList) 
     len 8						    -- must be a power of 2, for our hashing to work
     do provide NULL,
     0);

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
     globalFramesize,false,false);
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
		    if isalnum(t.0) && n <= length(t) && 0 == strncmp(s,t,n) then v=append(v,t);
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
export newGlobalDictionary():Dictionary := record(Dictionary(nextHash(),newSymbolHashTable(),self,0,0,false,false));
numLocalDictionaries := threadFrameID;
export threadLocal localFrame := dummyFrame;
export dummySymbolHashTable := newSymbolHashTable();
export dummyDictionary := (
     numLocalDictionaries = numLocalDictionaries + 1;
     Dictionary(nextHash(),dummySymbolHashTable,self,numLocalDictionaries,0,false,true));
export newLocalDictionary(dictionary:Dictionary):Dictionary := (
     numLocalDictionaries = numLocalDictionaries + 1;
     record(Dictionary(nextHash(),newSymbolHashTable(),dictionary,numLocalDictionaries,0,true,false)));
export newStaticLocalDictionary():Dictionary := (
     numLocalDictionaries = numLocalDictionaries + 1;
     record(
	  Dictionary(nextHash(),newSymbolHashTable(),self,numLocalDictionaries,
	       0,			     -- 0 for the global frame containing the static symbols' values
	       false,			  -- the first local dictionary is usually (?) non-transient
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
export rawPathTrackerClass := newtypeof(rawObjectClass);    -- RawStraightLineProgram
export pythonObjectClass := newbasictype();
export xmlNodeClass := newbasictype();
export xmlAttrClass := newbasictype();
export threadClass := newbasictype();
export symbolBodyClass := newbasictype();
-- all new types, dictionaries, and classes go just above this line, if possible, so hash codes don't change gratuitously!
