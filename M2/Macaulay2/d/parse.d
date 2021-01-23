--This file contains declarations for the parser
--It also contains declarations for expressions that are strictly necessary for the parser
--This is necessary because Expr must be declared in this file for functions that return Expr to be declared properly
--Functions that merely operate on Exprs should go in expr.d
--Functions in this file should not use stdio so that parse can be used by stdio.


--Error function predeclaration
declarations "
#ifdef __cplusplus
extern \"C\" {
#endif
struct M2_string_struct;
extern void err_abort(struct M2_string_struct*);
extern void err_fatal(struct M2_string_struct*);
extern void err_error(struct M2_string_struct*);
#ifdef __cplusplus
};
#endif
";


use nets;
use gmp;
use xml;
use engine;
use varnets;
use strings1;
use stdio0;
use stdiop0;
use pthread0;


export anywhereError(s:string) ::= Ccode(voidPointer,"err_error(",s,")");
export anywhereAbort(s:string) ::= Ccode(exits,"err_abort(",s,")");

-- Typedefs for functions of various numbers of arguments
export unop := function(Code):Expr;
export binop := function(Code,Code):Expr;
export binopExpr := function(Expr,Expr):Expr;
export ternop := function(Code,Code,Code):Expr;
export multop := function(CodeSequence):Expr;


export TokenFile := {+
     posFile:PosFile,
     nexttoken:(null or Token)
     };

export parseinfo := {
     precedence:int,
     binaryStrength:int,
     unaryStrength:int,
     funs:parsefuns
     };

export parsefuns := {
     unary:function(Token,TokenFile,int,bool):ParseTree,
     binary:function(ParseTree,Token,TokenFile,int,bool):ParseTree
     };


export TCnone := 0;			-- for artificial words: dummyWord, wordEOF, wordEOC
export TCid := 1;			-- identifiers and operators
export TCint := 2;
export TCRR := 3;
export TCstring := 4;
export Word := {		-- a word, one for each name made by makeUniqueWord()
     name:string,		--   the string representing it in this language
     typecode:int,		--   TCid, TCint, TCRR, or TCstring
     hash:int,	    		--   the hash value
     parse:parseinfo		--   parsing information
     };
export Symbol := {		    -- symbol table entry for a symbol
     word:Word,			    --   the word
     hash:int,			    --   based on the hash code of word, unchanging
     position:Position,	    	    --   the position where the definition was made
     unary:unop,
     postfix:unop,
     binary:binop,
     frameID:int,		    -- seqno of frame for dictionary containing it
				    -- 0 for the globalFrame
     frameindex:int,		    -- index within the frame of its value
     lookupCount:int,		    -- number of times looked up
     Protected:bool,	            -- whether protected against assignment by the user
     flagLookup:bool,		    -- whether to warn when symbol is used
     thread:bool,		    -- whether to use threadFrame instead of globalFrame
     serialNumber:int		    -- a counter, used to tell the age for the Serialization package
     };
export SymbolListCell := {word:Word, entry:Symbol, next:SymbolList};
export SymbolList := null or SymbolListCell;
export SymbolHashTable := { 
     buckets:array(SymbolList),	 -- length always a power of 2
     numEntries:int,
     mutex:SpinLock -- Modification mutex: lock before changing
     };

export Dictionary := {
     hash:int,						    -- assigned sequentially
     symboltable:SymbolHashTable,
     outerDictionary:Dictionary,          -- next outer dictionary, or pointer to self if none
     	       	    	        -- these pointers are munged by dictionaryPath, which is non-re-entrant!
     frameID:int,	        -- -1 for dummy, 0 for global, then 1,2,3,...
     framesize:int,	        -- one for each symbol; for transient frames only
     				--    because all global dictionaries share globalFrame and threadFrame
     transient:bool,	        -- whether there can be multiple frames
     	       	    	        -- for the global or thread dictionary and for file scopes : no
				-- for function closures : yes
     Protected:bool,             -- whether symbols can be added; closing a package protects it
     LocalCreationAllowed:bool	-- whether symbols can be added by code encountered in a local scope
     };

export Token := {+		-- a word, as encountered in the input
     word:Word,			--   the word
     filename:string, line:ushort, column:ushort, loadDepth:ushort, -- position:Position, --   the location where it was encountered
     dictionary:Dictionary,	--   the dictionary active at the time it was encountered
     entry:Symbol,     	  	--   the symbol table entry, found in the dictionary above, or one for wider lexical scope
     followsNewline:bool        --   whether it followed white space with a newline in it
     };

-- ParseTree

export Adjacent := {+lhs:ParseTree, rhs:ParseTree};
export For := {+ forToken:Token, variable:ParseTree, inClause:ParseTree, fromClause:ParseTree, toClause:ParseTree, whenClause:ParseTree, listClause:ParseTree, doClause:ParseTree, 
     dictionary:Dictionary 					    -- filled in later
     };
export WhileDo := {+ whileToken:Token, predicate:ParseTree, dotoken:Token, doClause:ParseTree};
export WhileList := {+ whileToken:Token, predicate:ParseTree, listtoken:Token, listClause:ParseTree};
export WhileListDo := {+ whileToken:Token, predicate:ParseTree, listtoken:Token, listClause:ParseTree, dotoken:Token, doClause:ParseTree };
export TryElse := {+ tryToken:Token, primary:ParseTree, elseToken:Token, alternate:ParseTree};
export TryThenElse := {+ tryToken:Token, primary:ParseTree, thenToken:Token, sequel:ParseTree, elseToken:Token, alternate:ParseTree};
export Try := {+ tryToken:Token, primary:ParseTree};
export Catch := {+ catchToken:Token, primary:ParseTree};
export IfThen := {+ ifToken:Token, predicate:ParseTree, thenclause:ParseTree };
export IfThenElse := {+ ifToken:Token, predicate:ParseTree, thenclause:ParseTree, elseClause:ParseTree};
export New := {+ newtoken:Token, newclass:ParseTree, newparent:ParseTree, newinitializer:ParseTree};
export Arrow := {+lhs:ParseTree, Operator:Token, rhs:ParseTree, desc:functionDescription};
export Quote := {+Operator:Token, rhs:Token};
export GlobalQuote := {+Operator:Token, rhs:Token, global:void};
export ThreadQuote := {+Operator:Token, rhs:Token, thread:void};
export LocalQuote := {+Operator:Token, rhs:Token, local:void};
export Binary := {+lhs:ParseTree, Operator:Token, rhs:ParseTree};
export Unary  := {+Operator:Token, rhs:ParseTree};
export Postfix:= {+lhs:ParseTree, Operator:Token};
export ArrayParseTree := array(ParseTree);
export Parentheses := {+ left:Token, contents:ParseTree, right:Token };
export EmptyParentheses := {+ left:Token, right:Token };
export dummy := {+position:Position};
export StartDictionary := {+dictionary:Dictionary, body:ParseTree};
export ParseTree := (
     Token or Adjacent or Binary or Unary or Postfix or Parentheses 
     or EmptyParentheses or IfThen or IfThenElse or StartDictionary 
     or Quote or GlobalQuote or ThreadQuote or LocalQuote
     or TryThenElse or TryElse or Try or Catch or WhileDo or For or WhileList or WhileListDo or Arrow or New or dummy );


-- Code

export localSymbolClosureCode := {+
     nestingDepth:int,
     symbol:Symbol,
     position:Position
     };
export globalSymbolClosureCode := {+
     symbol:Symbol,
     position:Position
     };
export threadSymbolClosureCode := {+
     symbol:Symbol,
     position:Position,
     x:void						    -- just to distinguish it
     };
export localMemoryReferenceCode := {+
     nestingDepth:int,
     frameindex:int,
     position:Position
     };
export globalMemoryReferenceCode := {+
     frameindex:int,
     position:Position
     };
export threadMemoryReferenceCode := {+
     frameindex:int,
     position:Position,
     x:void						    -- just to distinguish it
     };
export localAssignmentCode := {+
     nestingDepth:int,
     frameindex:int,
     rhs:Code,
     position:Position
     };
export globalAssignmentCode := {+
     lhs:Symbol,
     rhs:Code,
     position:Position
     };
export ifCode := {+ predicate:Code, thenClause:Code, elseClause:Code, position:Position };
export tryCode := {+ code:Code, thenClause:Code, elseClause:Code, position:Position };
export catchCode := {+ code:Code, position:Position };

export SymbolSequence := array(Symbol);
export parallelAssignmentCode := {+
     nestingDepth:array(int), -- spots corresponding to global and thread variables are filled with -1
     frameindex:array(int),
     lhs:SymbolSequence, -- spots corresponding to local variables are filled with dummySymbol
     rhs:Code,
     position:Position};

export nullCode := {+};
export realCode := {+x:RR,position:Position};
export integerCode := {+x:ZZ,position:Position};
export stringCode := {+x:string};
export unaryCode := {+f:unop,rhs:Code,position:Position};
export binaryCode := {+f:binop,lhs:Code,rhs:Code,position:Position};
export adjacentCode := {+lhs:Code,rhs:Code,position:Position};
export whileDoCode := {+predicate:Code,doClause:Code,position:Position};
export whileListCode := {+predicate:Code,listClause:Code,position:Position};
export whileListDoCode := {+predicate:Code,listClause:Code,doClause:Code,position:Position};

export ternaryCode := {+f:ternop,arg1:Code,arg2:Code,arg3:Code,position:Position};

export newOfFromCode := {+newClause:Code,ofClause:Code,fromClause:Code,position:Position};
export newFromCode   := {+newClause:Code,fromClause:Code,position:Position};
export newOfCode     := {+newClause:Code,ofClause:Code,position:Position};
export newCode       := {+newClause:Code,position:Position};

export CodeSequence     := tarray(Code);
export sequenceCode     := {+x:CodeSequence, position:Position};
export listCode         := {+y:CodeSequence, position:Position};
export arrayCode        := {+z:CodeSequence, position:Position};
export angleBarListCode := {+t:CodeSequence, position:Position};
export semiCode         := {+w:CodeSequence, position:Position};
export multaryCode      := {+f:multop, args:CodeSequence, position:Position};
export forCode          := {+inClause:Code, fromClause:Code, toClause:Code, whenClause:Code, listClause:Code, doClause:Code, frameID:int, framesize:int, position:Position} ;

export newLocalFrameCode := {+
     frameID:int,
     framesize:int,
     body:Code
     };
export functionDescription := {
     frameID:int,		    -- seqno of dictionary
     framesize:int,
     numparms:int,		    -- number of formal parameters
     restargs:bool		    -- whether last parm gets rest of args
     };
export dummyDesc := functionDescription(-1,0,0,false);
export functionCode := {+
     arrow:Token,			  -- just for display purposes
     body:Code, 
     desc:functionDescription,
     hash:int
     };
export Code := (
     nullCode or realCode or stringCode or integerCode 
     or globalMemoryReferenceCode or threadMemoryReferenceCode or localMemoryReferenceCode 
     or globalAssignmentCode or localAssignmentCode 
     or globalSymbolClosureCode or threadSymbolClosureCode or localSymbolClosureCode
     or parallelAssignmentCode 
     or unaryCode or binaryCode or ternaryCode or multaryCode or forCode
     or sequenceCode or listCode or arrayCode or angleBarListCode or semiCode
     or newCode or newFromCode or newOfCode or newOfFromCode
     or whileDoCode or whileListCode or whileListDoCode
     or ifCode or tryCode or adjacentCode or functionCode or catchCode
     or Error						    -- for tail recursion
     or newLocalFrameCode				    -- soon obsolete
     );
export CodeClosure := {+ frame:Frame, code:Code };



--misc


export CompiledFunction := {+fn:fun,hash:int};
export CompiledFunctionClosure := {+
     fn:function(Expr,Sequence):Expr,
     hash:int,
     env:Sequence
     };
export CompiledFunctionBody := {+
     fn:function(Expr,Sequence):Expr			    -- it's hard to make hash codes for these things!
     };



-- Expr

export Sequence := tarray(Expr);
export Frame := {
     outerFrame:Frame, 
     frameID:int,	    -- seqno of corresponding dictionary
			    -- 0 for the globalFrame, and 1 for the thread local threadFrame
     valuesUsed:int,        -- sigh, we really need this only for static frames
     	       	    	    -- we don't need it for the thread local threadFrame, but use it as a high water mark
     notrecyclable:bool,    -- if the frame should not be recycled back into the recyclebin in evaluate.d
     values:Sequence
     };


export FrameLocation := {
     frame:Frame,
     frameindex:int
     };
export DictionaryClosure := {+
     frame:Frame,      -- every symbol in the dictionary has the same frameID as this frame does
     dictionary:Dictionary
     };
export FunctionClosure := {+ frame:Frame, model:functionCode };
export SymbolClosure := {+
     frame:Frame,      -- this is a frame whose frameID is the same as that of the symbol
     symbol:Symbol
     };
export SymbolBody := {+
     symbol:Symbol
     };
export List := {+
     Class:HashTable,
     v:Sequence,
     hash:int,
     Mutable:bool
     };

export Error := {+
     position:Position,
     message:string,
     value:Expr,					    -- we put dummyExpr here for "break;", or put x here for "break x;"
     printed:bool,
     frame:Frame
     };

export Database := {+
     filename:string,
     hash:int,
     handle:int,
     isopen:bool,
     Mutable:bool
     };
export SpecialExpr := {+					    -- this allows specialization of arbitrary types, like functions
     Class:HashTable,	       	    	      	   	    -- the declared class of e, a specialization of the "real" class of e
     e:Expr
     };
export Boolean := {+v:bool};
export Nothing := {+nothing:void};

export MysqlConnection := Pointer "struct st_mysql *";
export MysqlConnectionWrapper := {+mysql:MysqlConnection or null};
export MysqlResult := Pointer "struct st_mysql_res *";
export MysqlResultWrapper := {+connection:MysqlConnectionWrapper, res:MysqlResult};
export MysqlField  := Pointer "struct st_mysql_field *";
export MysqlFieldWrapper  := {+res:MysqlResultWrapper, fld:MysqlField};

export pythonObject := Pointer "struct _object *";
export pythonObjectCell := {+v:pythonObject};

export TaskCellBody := {+
     hash:int,
     serialNumber:int,
     task:taskPointer, resultRetrieved:bool,
     fun:Expr, arg:Expr, returnValue:Expr  };
export TaskCell := {+ body:TaskCellBody };


export Expr := (
     CCcell or
     RRcell or
     Boolean or
     CodeClosure or
     CompiledFunction or
     CompiledFunctionBody or
     CompiledFunctionClosure or
     Database or
     DictionaryClosure or 
     Error or
     functionCode or
     FunctionClosure or
     HashTable or
     ZZcell or
     List or
     Net or
     NetFile or
     Nothing or
     QQcell or
     RawComputationCell or
     RawFreeModuleCell or
     RawMatrixCell or
     RawMonoidCell or
     RawMonomialCell or
     RawMonomialIdealCell or
     RawMonomialOrderingCell or
     RawMutableMatrixCell or
     RawMutableComplexCell or
     -- NAG begin
     RawHomotopyCell or
     RawSLEvaluatorCell or
     RawSLProgramCell or
     RawStraightLineProgramCell or
     RawPathTrackerCell or
     RawPointArrayCell or
     -- NAG end
     RawRingCell or
     RawRingElementCell or
     RawRingMapCell or
     MysqlConnectionWrapper or
     MysqlResultWrapper or
     MysqlFieldWrapper or
     Sequence or
     SpecialExpr or
     SymbolClosure or
     SymbolBody or
     file or
     stringCell or
     pythonObjectCell or
     xmlNodeCell or xmlAttrCell or
     TaskCell or 
     fileOutputSyncState
     );
export fun := function(Expr):Expr;

--Unique True expression
export True := Expr(Boolean(true));	  -- don't make new ones!
--Unique False Expression
export False := Expr(Boolean(false));	  -- use toExpr instead
--Conversion from C boolean value to Expression
export toExpr(v:bool):Expr := if v then True else False;

export zeroE := Expr(zeroZZcell);
export  oneE := Expr( oneZZcell);
export  minusoneE := Expr( minusoneZZcell);

--Internal "null" expressions that should never be visible to user
export nullE := Expr(Nothing());
export notfoundE := Expr(Nothing());
export dummyExpr := Expr(Nothing());



-- Expr Functions 

export noRecycle(f:Frame):Frame := (
     g := f;
     while !g.notrecyclable && (
	  g.notrecyclable = true;
	  g != g.outerFrame
	  ) do g = g.outerFrame;
     f);




-- hash tables for exprs

export KeyValuePair := {key:Expr, hash:int, value:Expr, next:KeyValuePair};
export HashTable := {+
     table:array(KeyValuePair), -- length is always a power of 2, initially 2^0 == 1
     Class:HashTable,
     parent:HashTable,
     numEntries:int,
     hash:int,
     Mutable:bool,
     beingInitialized:bool,-- if true, no need to lock a mutex while modifying it
     mutex:SpinLock
     };

--This unfortunately needs to be here as it references Hash Tabe which needs expr.  

export m2cfile := Pointer "struct M2File*";	

-- TODO: note: Excessive padding in 'struct parse_file_struct' (34 padding bytes, where 2 is optimal).
-- Optimal fields order: filename, errorMessage, inbuffer, prompt, reward, unsyncOutputState, cfile, threadSyncMutex, hash, pid, listenerfd, connection, numconns, infd, inindex, insize, echoindex, outfd, type_, error, listener, input, inisatty, eof, promptq, fulllines, bol, echo, readline, output, outisatty, consider reordering the fields or adding explicit padding members

export file := {+
        -- general stuff
     	hash:int,     	   	-- hash code
	filename:string,	-- name of file
	pid:int,	        -- pid if it's a pipe or pair of pipes to a child process, else 0
        error:bool,             -- a system call returned ERROR
	errorMessage:string,    -- the error message associated to the system call that returned ERROR
	-- listener stuff
        listener:bool,	   	-- is a listener
	listenerfd:int,	    	-- file descriptor of listener, or -1
	connection:int,	   	-- file descriptor of accepted connection, not made into file yet
	numconns:int,	        -- count connections accepted
     	-- input file stuff
     	input:bool,	        -- is input file
	infd:int,		-- file descriptor or -1
        inisatty:bool,
	inbuffer:string,        -- buffer
	inindex:int,		-- inbuffer.inindex is the first available char
        insize:int,		-- inbuffer.(insize-1) is the last char
				-- we always have inindex <= insize
	eof:bool,		-- last read got 0 characters (still might have some chars in inbuffer)
        promptq:bool,           -- whether to prompt and to reward for input
	prompt:function():string, -- function to call to get prompt string when input is required
        reward:function():string, -- function to call to get reward string when input has been obtained
	fulllines:bool,		-- whether to read at least up to the next newline each time input is required
        bol:bool,     	   	-- at the beginning of a line, and a prompt is needed
	echo:bool,     	   	-- whether to echo characters read to corresponding output file
	echoindex:int,	        --   inbuffer.echoindex is the next character to echo
        readline:bool,           -- input handled by readline()
     	output:bool,	        -- is output file
	outfd:int,		-- file descriptor or -1
        outisatty:bool,
	unsyncOutputState:fileOutputSyncState, -- default sync state to use for unsync output
	 -- Mutex for syncronization and for buffering 
	 -- Lock before output in sync output mode
	threadSyncMutex:ThreadMutex,
	-- C structure for this file that provides for thread support
	cfile:m2cfile
	};

export PosFile := {+ file:file, lastchar:int, filename:string, line:ushort, column:ushort };

