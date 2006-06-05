newPackage ( "Parsing",
     Authors => {
	  { Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/" }
	  },
     Date => "June, 2006",
     Version => "1.0",
     Headline => "a framework for creating recursive descent parsers",
     DebuggingMode => true
     )

export Parser
Parser = new SelfInitializingType of FunctionClosure
Parser.synonym = "parser"
	  
export Analyzer
Analyzer = new SelfInitializingType of FunctionClosure
Analyzer.synonym = "lexcical analyzer"

-- character sets and converters
space = set characters " \t\f\n\r"
alpha = set characters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
digit = hashTable {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
-- trivial lexical analyzers
chkstring := string -> if not instance(string, String) then error "analyzer expected a string";
export charAnalyzer
charAnalyzer = Analyzer( 
     string -> (
	  chkstring string;
	  i := 0;
	  () -> if string#?i then ( r := (i,string#i); i = i+1; r)))
export nonspaceAnalyzer
nonspaceAnalyzer = Analyzer(
     string -> (
	  chkstring string;
	  i := 0;
	  () -> while string#?i do (c := string#i; i = i+1; if not space#?c then return (i-1,c))) )
-- machine makers
Parser ++ Analyzer := (p0,a0) -> s -> (
     p := p0;
     a := a0 s;
     while null =!= ((pos,t) := a())
     do if t === null then error("unrecognized lexical item at position ", toString pos) 
     else if null === (p = p t) then error("syntax error at token '",toString t,"', position ",toString pos);
     if null === (r := p null) then error "parser unfinished, at end of input";
     r)
-- result symbols
export nil						    -- something useless for a parser to return to indicate acceptance
-- parsers
export deadParser     ; deadParser = Parser (c -> null)
export terminalParser ; terminalParser = val -> new Parser from (c -> if c === null then val)
export nullParser     ; nullParser = terminalParser nil
export letterParser   ; letterParser = Parser (c -> if alpha#?c then b -> if b === null then c)
export spaceParser    ; spaceParser = Parser (c -> if c === null then nil else if space#?c then spaceParser)
export futureParser   ; futureParser = parserSymbol -> new Parser from (c -> (value parserSymbol) c)

-- parser makers
export orP
orP = x -> (
     if instance(x, Function) then return x;
     if #x == 0 then return deadParser;
     Parser (c -> (
	  if c === null then (
	       for p in x do if (t := p null) =!= null then break t
	       )
	  else (
	       y := select(apply(x, p -> p c), p -> p =!= null);
	       if #y > 0 then orP y))))

Parser | Parser := (p,q) -> new Parser from (
     c -> (
	  p' := p c;
	  q' := q c;
	  if p' === null then q'
	  else if q' === null then p'
	  else if c === null then p' else p'|q'))

export optP
optP = parser -> parser | nullParser

export transform; 
transform = fun -> f := p -> new Parser from ( c -> (
	  p' := p c;
	  if p' =!= null then if c === null then fun p' else f p'
	  ))

export andP
andP = x -> (						    -- we don't do backtracking: the first parser absorbs as much as it can, and then the second one takes over, etc.
     if instance(x, Function) then return x;
     if #x == 0 then return nullParser;
     f := (past,current,future) -> new Parser from (c -> (
	  if c === null then (
	       val := current null;
	       if val === null then return;
	       val' := apply(future,p -> p null);	    -- wastes some time here if not all in terminal states
	       if any(val', v -> v === null) then return;
	       join(past,1:val,val'))
	  else (
	       q := current c;
	       if q =!= null then return f(past,q,future);
	       val = current null;
	       if val === null then return;
	       if #future > 0 then (f(append(past,val), future#0, drop(future,1))) c)));
     f((),x#0,drop(x,1)))

Parser @ Parser := (p,q) -> new Parser from (
     c -> (
	  if c =!= null then (
	       if (p' := p c) =!= null 
	       then p' @ q 
	       else if (val1 := p null) =!= null then if (q' := q c) =!= null then (transform (val2 -> (val1,val2))) q'
	       )
	  else if (val1 = p null) =!= null and (val2 := q null) =!= null then (val1,val2)
	  ))

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
+ Parser := parser -> parser @ *parser

-- some simple parsers that accept one character at time

export fixedStringParser 
fixedStringParser = s -> ( f := n -> new Parser from (c -> if c === null then if n === #s then s else null else if s#?n and c === s#n then f(n+1) else null)) 0

export natNumberP
natNumberP = (() -> (
     	  f := n -> new Parser from (c -> if c === null then n else if digit#?c then f (10*n + digit#c)); 
     	  new Parser from (c -> if digit#?c then f digit#c)
     	  )) ()

export optionalSignParser
optionalSignParser = Parser(c -> if c === null then 1 else if c === "-" then terminalParser(-1) else if c === "+" then terminalParser 1)

export ZZParser
ZZParser = (transform times) (optionalSignParser @ natNumberP)

export QQParser
QQParser = (transform ((num,sl,den) -> num/den)) andP(ZZParser,fixedStringParser "/",natNumberP)

beginDocumentation()

document { Key => Parsing,
     Headline => "a framework for building parsers",
     PARA {
          TO "Parsing", " is a package the provides a framework for building parsers.  It introduces ", TO "Parser", ", a type of
          function that parses a sequence of tokens, and ", TO "Analyzer", ", a type of function that accepts input for the parser in
          its original form and separates it into a stream of tokens.  A parser can be combined with an analyzer (see ", TO "(symbol ++, Parser, Analyzer)", ",
	  to produce a complete system for accepting input and parsing it."
     	  }
     }
document { Key => Parser,
     Headline => "the class of all parsers",
     PARA {
	  "A parser is a type of function that parses a sequence of tokens.  Tokens can be anything except ", TO "null", ".
	  A parser ", TT "p", " is called repeatedly, like this: ", TT "p t", ", one token at a time.  The return value that
	  indicates acceptance of the input token is a new parser, which replaces the old, and is ready to accept the next token; the
	  original parser ", TT "p", " should not change its internal state.  The return value that indicates rejection of the input
	  token is ", TO "null", ".  Rejection may be interpreted as a syntax error."
	  },
     PARA {
	  "When the input stream is exhausted, we call ", TT "p", " one more time like this: ", TT "p null", ".  The return value is ", TO "null", "
	  if the parser is not in a terminal state.  Otherwise the return value is the parsed (and possibly evaluated) result."
	  }
     }

document { Key => Analyzer,
     Headline => "the class of all lexical analyzers",
     PARA {
	  "A function that accepts input in its original form and separates it into tokens, will be called ", ofClass Analyzer, ".
	  These analyzers are functional: call one with the original input, and it returns a function of 0 arguments (with changeable internal state)
	  that keeps returning tokens each time it is called until none are left.
	  Actually, the analyzer is to return a pair: ", TT "(pos,token)", ", where ", TT "pos", " is a string indicating the position where ", TT "token", " was found in the input.
	  A position will be a sort of thing which can be converted to string with ", TO "toString", " (for printing error messages) and can be sorted."
	  }
     }

document { Key => charAnalyzer,
     Headline => "a lexical analyzer that provides characters from a string one at a time",
     EXAMPLE lines ///
     	  a = charAnalyzer "abc"
	  peek a()
	  peek a()
	  peek a()
	  peek a()
	  (fixedStringParser "abc" ++ charAnalyzer) "abc"
     ///
     }

document { Key => (symbol ++, Parser, Analyzer),
     Headline => "combine a parser with a lexical analyzer to make a complete system",
     Usage => "m = p ++ a",
     Inputs => { "p", "a" },
     Outputs => { "m" => Function => { "a function that will provide its argument to ", TT "a", " for lexical analysis and will
	       send the resulting stream of tokens through ", TT "p", " for parsing.  Appropriate error messages are printed as necessary."
	       }},
     EXAMPLE lines ///
	 (ZZParser ++ charAnalyzer) "123456789"
	 (fixedStringParser "abc" ++ charAnalyzer) "abc"
     ///
     }

document { Key => nonspaceAnalyzer,
     Headline => "a lexical analyzer that provides non-white-space characters from a string one at a time",
     EXAMPLE lines ///
         a = nonspaceAnalyzer " a b c "
	 peek a()
	 peek a()
	 peek a()
	 peek a()
	 (fixedStringParser "abc" ++ nonspaceAnalyzer) " a b c "
     ///
     }

document { Key => nil,
     Headline => "a symbol a parser to return to indicate acceptance of the empty string of tokens",
     }

document { Key => deadParser,
     Headline => "a parser which accepts no tokens and is not in a terminal state",
     EXAMPLE lines ///
     	  peek deadParser "a"
     	  peek deadParser null
     ///
     }

document { Key => terminalParser,
     Headline => "produce a parser in a terminal state",
     Usage => "terminalParser v",
     Inputs => { "v" },
     Outputs => { { "a parser in a terminal state with parsed value ", TT "v" }},
     EXAMPLE lines ///
     	  p = terminalParser x
	  p "a"
	  p null
     ///
     }

document { Key => nullParser,
     Headline => "",
     }

document { Key => letterParser,
     Headline => "",
     }

document { Key => spaceParser,
     Headline => "",
     }

document { Key => futureParser,
     Headline => "",
     }

document { Key => {orP, (symbol |, Parser, Parser)},
     Headline => "",
     }

document { Key => {andP, (symbol @, Parser, Parser)},
     Headline => "",
     }

document { Key => transform,
     Headline => "",
     }

document { Key => (symbol *, Parser),
     Headline => "",
     }

document { Key => (symbol +, Parser),
     Headline => "",
     }

document { Key => fixedStringParser,
     Headline => "",
     }

document { Key => optionalSignParser,
     Headline => "",
     }

document { Key => ZZParser,
     Headline => "",
     }

document { Key => QQParser,
     Headline => "",
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
