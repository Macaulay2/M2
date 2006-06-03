newPackage ( "Parsing",
     Authors => {
	  { Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/" }
	  },
     Date => "June, 2006",
     Version => "1.0",
     Headline => "a framework for creating recursive descent parsers",
     DebuggingMode => true
     )

-- a parser, p, is a type of function 
-- the input stream is divided into tokens, t
-- a token can be anything, except "null"
-- the parser is called repeatedly, like this: "p t", one token at a time
--   the return value is a new parser, which replaces the old, and is ready to accept the next token (functional programming)
--   or the return value is "null", in which case there was a syntax error
-- when the input is done, we call p one more time like this: "p null"
--   the return value from "p null" is null if the parser didn't reach a terminal state
--   otherwise the return value is the parsed and possibly evaluated result
-- a function that accepts input in its original form and separates it into tokens (by lexical analysis), will
--   be called a lexical analyzer.
--   Our analyzers are functional -- call one with the original input, and it returns a function of 0 arguments (with state, non-functional)
--   that keeps returning tokens until none are left.
--   Actually, it's better for the analyzer to return a pair: "(pos,token)", where "pos" is a string indicating the position where the token was found in the input
--   a position will be a sort of thing which can be converted to string with toString and can be sorted with "?", "<", ">"
-- tokens should be convertible to strings with "toString", for printing syntax error messages
-- so, a complete system consists of an engine that accepts three things:
--      a lexical analyzer
--      a parser
--      the input in its original form
--   it accepts a parser, and runs all the tokens through the parser, issues an error message if there was a syntax error or, at
--      at the end of the input, the parser didn't reach a terminal state
-- a "machine" will be a function that has been formed from an analyzer and a parser, and is ready to receive raw input, analyze it, parse it, and return the result
-- it should be easy to use "regex" to write a lexical analyzer that separates a string into word-sized tokens
-- the easiest lexical analyzer separates a string into characters, optionally discarding the white space
-- what about error recovery?  How does yacc do it?  Currently a syntax error can't be recovered from. 

export Parser ; Parser = new SelfInitializingType of FunctionClosure
export Analyzer ; Analyzer = new SelfInitializingType of FunctionClosure

-- character sets and converters
space = set characters " \t\f\n\r"
alpha = set characters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
digit = hashTable {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
-- analyzers
chkstring := string -> if not instance(string, String) then error "analyzer expected a string";
Analyzer#"characters" = Analyzer( 
     string -> (
	  chkstring string;
	  i := 0;
	  () -> if string#?i then ( c := string#i; i = i+1; (i,c))))
Analyzer#"non-space characters" = Analyzer(
     string -> (
	  chkstring string;
	  i := 0;
	  () -> while string#?i do (c := string#i; i = i+1; if not space#?c then return (i,c))) )
-- machine makers
Parser ++ Analyzer := (p,a) -> s -> (
     a' := a s;
     while null =!= ((pos,t) := a'()) do if null === (p = p t) then error("syntax error at token '",toString t,"', position ",toString pos);
     if null === (r := p null) then error "parser unfinished, at end of input";
     r)
-- result symbols
export nil						    -- something for nullParser to return
-- parsers
export deadParser     ; deadParser = Parser (c -> null)
export terminalParser ; terminalParser = val -> new Parser from (c -> if c === null then val)
export nullParser     ; nullParser = terminalParser nil
export letterParser   ; letterParser = Parser (c -> if alpha#?c then b -> if b === null then c)
export spaceParser    ; spaceParser = Parser (c -> if c === null then nil else if space#?c then spaceParser)

export natNumberP
natNumberP = (() -> (
     	  f := n -> new Parser from (c -> if c === null then n else if digit#?c then f (10*n + digit#c)); 
     	  new Parser from (c -> if digit#?c then f digit#c)
     	  )) ()

-- parser makers
export orP
Parser | Parser := orP = x -> (
     assert( x =!= null );
     if instance(x, Function) then return x;
     if #x == 0 then return deadParser;
     Parser (c -> (
	  if c === null then return for p in x do if (t := p null) =!= null then return t;
	  y := select(apply(x, p -> p c), p -> p =!= null);
	  if #y > 0 then orP y)))
export andP
Parser * Parser := andP = x -> (
     assert( x =!= null );
     if instance(x, Function) then return x;
     if #x == 0 then return nullParser;
     f := (past,current,future) -> new Parser from (c -> (
	  if c === null then (
	       val := current null;
	       if val === null then return;
	       ret := append(past,val);
	       if #future == 0 then ret else (f(ret, future#0, drop(future,1))) c)
	  else (
	       q := current c;
	       if q =!= null then return f(past,q,future);
	       val = current null;
	       if val === null then return;
	       if #future > 0 then (f(append(past,val), future#0, drop(future,1))) c)));
     f((),x#0,drop(x,1)))
*Parser := p -> (
     assert( p =!= null );
     local g;
     f := (past,current) -> new Parser from (c -> (
	  if c === null then (
	       if current === null then past
	       else if (val := current null) =!= null then append(past,val)
	       )
	  else (
	       if current === null then (f(past,p)) c
	       else (
	       	    q := current c;
	       	    if q =!= null then return f(past,q);
	       	    if (val = current null) =!= null then (f(append(past,val),)) c))));
     f((),))
export munge; munge = fun -> f := parser -> new Parser from (c -> ( p -> if p =!= null then (if c === null then fun else f) p ) parser c)
+ Parser := parser -> parser * *parser
export futureParser; futureParser = parserSymbol -> Parser (c -> (value parserSymbol) c)

beginDocumentation()

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
