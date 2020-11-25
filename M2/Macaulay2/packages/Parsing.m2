-- -*- coding: utf-8 -*-
newPackage ( "Parsing",
     Authors => {
	  { Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/" }
	  },
     Date => "June, 2006",
     Version => "1.0",
     Headline => "a framework for creating recursive descent parsers",
     Keywords => {"Miscellaneous"},
     DebuggingMode => false
     )

export "constParser" 

export "Parser"
Parser = new SelfInitializingType of FunctionClosure
Parser.synonym = "parser"
	  
export "Analyzer"
Analyzer = new SelfInitializingType of FunctionClosure
Analyzer.synonym = "lexical analyzer"

-- character sets and converters
space = set characters " \t\f\n\r"
alpha = set characters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
digit = hashTable {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
-- trivial lexical analyzers
chkstring := string -> if not instance(string, String) then error "analyzer expected a string";
export "charAnalyzer"
charAnalyzer = Analyzer( 
     string -> (
	  chkstring string;
	  i := 0;
	  () -> if string#?i then ( r := (i,string#i); i = i+1; r)))
export "nonspaceAnalyzer"
nonspaceAnalyzer = Analyzer(
     string -> (
	  chkstring string;
	  i := 0;
	  () -> while string#?i do (c := string#i; i = i+1; if not space#?c then return (i-1,c))) )
-- machine makers
Parser : Analyzer := (p0,a0) -> s -> (
     p := p0;
     a := a0 s;
     while null =!= (k := a())
     do (
	  (pos,t) := k;
	  if t === null then error("unrecognized lexical item at position ", toString pos) 
     	  else if null === (p = p t) then error("syntax error at token '",toString t,"', position ",toString pos)
	  );
     if null === (r := p null) then error "parser unfinished, at end of input";
     r)
-- result symbols
export "nil"						    -- something useless for a parser to return to indicate acceptance
-- parsers
export "deadParser"
deadParser = Parser (c -> null)
export "terminalParser"
terminalParser = val -> new Parser from (c -> if c === null then val)
export "nullParser"
nullParser = terminalParser nil
export "letterParser"
letterParser = Parser (c -> if alpha#?c then new Parser from (b -> if b === null then c))
export "futureParser"
futureParser = parserSymbol -> new Parser from (c -> (value parserSymbol) c)

-- parser makers
export "orP"
orP = x -> (
     if instance(x, Function) then return x;
     if instance(x, String) then return constParser x;
     if #x == 0 then return deadParser;
     x = apply(x, s -> if instance(s,String) then constParser s else s);
     Parser (c -> (
	  if c === null then (
	       for p in x do if (t := p null) =!= null then break t
	       )
	  else (
	       y := select(apply(x, p -> p c), p -> p =!= null);
	       if #y > 0 then orP y))))

Parser | String := (p,s) -> p | constParser s
String | Parser := (s,p) -> constParser s | p
Parser | Parser := (p,q) -> new Parser from ( c -> (
	  p' := p c;
	  q' := q c;
	  if p' === null then q'
	  else if q' === null then p'
	  else if c === null then p' else p'|q'))

export "optP"
optP = parser -> parser | nullParser

Function % Parser := (fun,p) -> new Parser from ( c -> if (p' := p c) =!= null then if c === null then fun p' else fun % p' )

export "andP"
andP = x -> (						    -- we don't do backtracking: the first parser absorbs as much as it can, and then the second one takes over, etc.
     if instance(x, Parser) then return x;
     if instance(x, String) then return constParser x;
     if #x == 0 then return nullParser;
     x = apply(x, s -> if instance(s,String) then constParser s else s);
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


String @ Parser := (s,p) -> constParser s @ p
Parser @ String := (p,s) -> p @ constParser s
Parser @ Parser := (p,q) -> new Parser from (
     c -> (
	  if c =!= null then (
	       if (p' := p c) =!= null 
	       then p' @ q 
	       else if (val1 := p null) =!= null then if (q' := q c) =!= null then (val2 -> (val1,val2)) % q'
	       )
	  else if (val1 = p null) =!= null and (val2 := q null) =!= null then (val1,val2)
	  ))

*Parser := p -> (
     f := (vals,current) -> new Parser from (c -> (
	       if c === null then (
		    if current === null then vals
		    else if (val := current null) =!= null then append(vals,val)
		    )
	       else (
		    if current === null then (f(vals,p)) c
		    else (
			 q := current c;
			 if q =!= null then return f(vals,q);
			 if (val = current null) =!= null then (f(append(vals,val),)) c))))
     ) ((),)
	  
+Parser := p -> prepend % p @ *p

-- some simple parsers that accept one character at time

constParser = s -> ( f := n -> new Parser from (c -> if c === null then if n === #s then s else null else if s#?n and c === s#n then f(n+1) else null)) 0

export "NNParser"
NNParser = (() -> (
     	  f := n -> new Parser from (c -> if c === null then n else if digit#?c then f (10*n + digit#c)); 
     	  new Parser from (c -> if digit#?c then f digit#c)
     	  )) ()

export "optionalSignParser"
optionalSignParser = Parser(c -> if c === null then 1 else if c === "-" then terminalParser(-1) else if c === "+" then terminalParser 1)

export "ZZParser"
ZZParser = times % optionalSignParser @ NNParser

export "QQParser"
QQParser = ((num,sl,den) -> num/den) % andP(ZZParser,constParser "/",NNParser)

beginDocumentation()

document { Key => Parsing,
     Headline => "a framework for building parsers",
     PARA {
          TO "Parsing", " is a package the provides a framework for building parsers.  It introduces ", TO "Parser", ", a type of
          function that parses a sequence of tokens, and ", TO "Analyzer", ", a type of function that accepts input for the parser in
          its original form and separates it into a stream of tokens.  A parser can be combined with an analyzer: see ", TO (symbol :, Parser, Analyzer), ",
	  to produce a complete system for accepting input and parsing it."
     	  },
     PARA {
	  "See the package ", TO "Classic::Classic", " for a good example of  the use of this framework."
	  },
     Subnodes => {
	  TO Analyzer,
	  TO Parser,
	  TO (symbol :, Parser, Analyzer)
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
	  },
     Subnodes => {
	  "simple parsers",
	  TO deadParser,
	  TO terminalParser,
	  TO nullParser,
	  TO futureParser,
	  TO letterParser,
	  TO constParser,
	  TO optionalSignParser,
	  TO NNParser,
	  TO ZZParser,
	  TO QQParser,
	  "making new parsers from old ones",
	  TO optP,
	  TO orP,
	  TO andP,
	  TO (symbol *, Parser),
	  TO (symbol +, Parser),
	  TO (symbol %, Function, Parser)
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
	  },
     Subnodes => {
	  TO charAnalyzer,
	  TO nonspaceAnalyzer
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
	  (constParser "abc" : charAnalyzer) "abc"
     ///,
     SeeAlso => {constParser, (symbol :, Parser, Analyzer)}
     }

document { Key => (symbol :, Parser, Analyzer),
     Headline => "combine a parser with a lexical analyzer to make a complete system",
     Usage => "m = p : a",
     Inputs => { "p", "a" },
     Outputs => { "m" => Function => { "a function that will provide its argument to ", TT "a", " for lexical analysis and will
	       send the resulting stream of tokens through ", TT "p", " for parsing.  Appropriate error messages are printed as necessary."
	       }},
     EXAMPLE lines ///
	 (ZZParser : charAnalyzer) "123456789"
	 (constParser "abc" : charAnalyzer) "abc"
     ///,
     SeeAlso => {ZZParser, constParser, charAnalyzer, (symbol :, Parser, Analyzer)}
     }

document { Key => nonspaceAnalyzer,
     Headline => "a lexical analyzer that provides non-white-space characters from a string one at a time",
     EXAMPLE lines ///
         a = nonspaceAnalyzer " a b c "
	 peek a()
	 peek a()
	 peek a()
	 peek a()
	 (constParser "abc" : nonspaceAnalyzer) " a b c "
     ///,
     SeeAlso => {constParser, (symbol :, Parser, Analyzer)}
     }

document { Key => nil,
     Headline => "a symbol a parser may return to indicate acceptance of the empty string of tokens",
     SeeAlso => { nullParser }
     }

document { Key => deadParser,
     Headline => "a parser which accepts no tokens and is not in a terminal state",
     SourceCode => deadParser,
     EXAMPLE lines ///
     	  peek deadParser "a"
     	  peek deadParser null
     ///
     }

document { Key => terminalParser,
     Headline => "produce a parser in a terminal state",
     Usage => "terminalParser v",
     SourceCode => terminalParser,
     Inputs => { "v" },
     Outputs => { { "a parser in a terminal state with parsed value ", TT "v" }},
     EXAMPLE lines ///
     	  p = terminalParser x
	  p "a"
	  p null
     ///
     }

document { Key => nullParser,
     Headline => "a terminal parser that returns the value nil",
     SourceCode => nullParser,
     SeeAlso => { nil }
     }

document { Key => letterParser,
     Headline => "a parser that accepts a single letter and returns it",
     SourceCode => letterParser,
     EXAMPLE lines ///
     	  p = letterParser "a"
	  p "b"
	  p null
     ///
     }

document { Key => futureParser,
     Headline => "forward reference to a parser not defined yet",
     Usage => "futureParser p",
     Inputs => { "p" => Symbol },
     Outputs => { Parser => { "a parser that will pass its input to the value of the symbol ", TT "p", " at that time" }},
     SourceCode => futureParser,
     PARA "This function makes recursive parsers possible.",
     EXAMPLE lines ///
     	  p = futureParser q
	  m = p : charAnalyzer
	  q = constParser "abc"
	  m "abc"
     ///
     }

document { Key => {orP, (symbol |, Parser, Parser), (symbol |, Parser, String), (symbol |, String, Parser)},
     Headline => "parsing alternatives",
     Usage => "r = orP(p,q,...)",
     Inputs => { { TT "(p,q,...)", ", a sequence of parsers (of type ", TO "Parser", ")" }},
     Outputs => { "r" => Parser => { "a parser that accepts any sequence of tokens acceptable to one of the parsers ", TT "p,q,..."}},
     SourceCode => {(symbol |, Parser, Parser)},
     PARA {
	  "An abbreviation for ", TT "orP(p,q)", " is ", TT "p|q", "."
	  },
     PARA {
	  "In case of ambiguity, the value returned by the left-most accepting parser is provided."
	  },
     PARA {
	  "If one of the arguments is ", ofClass String, " then ", TO "constParser", " is used to convert it into a parser."
	  },
     PARA {
	  "In an efficient grammar, the first token presented to ", TT "r", " will be acceptable to at most one of the input parsers,
	  and then the parser returned by ", TT "r", " will be the parser returned by the single accepting input parser."
	  },
     EXAMPLE lines ///
     	  (constParser "abc" | constParser "def" : charAnalyzer) "abc"
     	  (constParser "abc" | constParser "def" : charAnalyzer) "def"
     	  (constParser "abc" | "def" : charAnalyzer) "def"
     ///,
     SeeAlso => {constParser, charAnalyzer, (symbol :, Parser, Analyzer)}
     }

document { Key => {andP, (symbol @, Parser, Parser), (symbol @, String, Parser), (symbol @, Parser, String)},
     Headline => "parser conjunction",
     Usage => "r = andP(p,q,...)",
     Inputs => { { TT "(p,q,...)", ", a sequence of parsers (of type ", TO "Parser", ")" }},
     Outputs => { "r" => Parser => { "a parser that accepts as many tokens as ", TT "p", " will accept, then as many as ", TT "q", " will
	       accept, and so on.  The return value is the sequence of values returned by each of the input parsers."}},
     SourceCode => {(symbol @, Parser, Parser)},
     PARA {
	  "An abbreviation for ", TT "andP(p,q)", " is ", TT "p@q", "."
	  },
     PARA {
	  "If one of the arguments is ", ofClass String, " then ", TO "constParser", " is used to convert it into a parser."
	  },
     EXAMPLE lines ///
     	  (constParser "abc" @ constParser "def" : charAnalyzer) "abcdef"
     ///,
     SeeAlso => {constParser,(symbol :, Parser, Analyzer)}
     }

document { Key => (symbol %, Function, Parser),
     Headline => "transform the value returned by a parser",
     Usage => "f % p",
     Inputs => { "f" => Function, "p" => Parser },
     Outputs => { Parser => { "a parser that feeds its tokens to ", TT "p", " and filters the value returned by ", TT "p", " through ", TT "f" }},
     SourceCode => {(symbol %, Function, Parser)},
     EXAMPLE lines ///
	  (* constParser "abc" : charAnalyzer) "abcabcabc"
	  (concatenate % * constParser "abc" : charAnalyzer) "abcabcabc"
     	  (constParser "abc" : charAnalyzer) "abc"
     	  ((s -> concatenate("[",s,"]")) % constParser "abc" : charAnalyzer) "abc"
     ///,
     SeeAlso => {constParser, charAnalyzer, (symbol :, Parser, Analyzer)}
     }

document { Key => (symbol *, Parser),
     Headline => "repetition of a parser",
     Usage => "*p",
     Inputs => { "p" => Parser },
     Outputs => { Parser => { "a parser that will feed its tokens through ", TT "p", ", and then when further tokens are not accepted, it will
	       start over with a fresh copy of ", TT "p", ".  The value returned is the sequence of values returned by each instance of ", TT "p", "."
	       }},
     SourceCode => {(symbol *, Parser)},
     EXAMPLE lines ///
     	  (* constParser "abc" : charAnalyzer) "abcabcabc"
     ///,
     SeeAlso => {constParser, charAnalyzer, (symbol :, Parser, Analyzer)}
     }

document { Key => (symbol +, Parser),
     Headline => "repetition of a parser at least once",
     Usage => "+p",
     Inputs => { "p" => Parser },
     Outputs => { Parser => { "a parser that will feed its tokens through ", TT "p", ", and then when further tokens are not accepted, it will
	       start over with a fresh copy of ", TT "p", ".  The value returned is the sequence of values returned by each instance of ", TT "p", "."
	       }},
     SourceCode => {(symbol +, Parser)},
     EXAMPLE lines ///
     	  (+ constParser "abc" : charAnalyzer) "abcabcabc"
     ///,
     SeeAlso => {constParser, charAnalyzer, (symbol :, Parser, Analyzer)}
     }

document { Key => constParser,
     Headline => "produce a parser that accepts a fixed string, one character at a time",
     Usage => "constParser s",
     Inputs => { "s" => String },
     Outputs => { Parser => { "a parser that accepts (and returns) the string s, one character at a time" } },
     EXAMPLE lines ///
     	  constParser "abc"
	  oo "a"
	  oo "a"
	  ooo "b"
	  oo "c"
	  oo null
	  (constParser "abc" : charAnalyzer) "abc"
     ///,
     SeeAlso => {(symbol :, Parser, Analyzer)}
     }

document { Key => optionalSignParser,
     Headline => "a parser that accepts an optional plus sign or minus sign",
     PARA {
	  "The return value is either ", TT "1", " or ", TT "-1", ", depending on the indicated sign."
	  },
     SourceCode => optionalSignParser,
     EXAMPLE lines ///
     	  (optionalSignParser @ constParser "abc" : charAnalyzer) "abc"
     	  (optionalSignParser @ constParser "abc" : charAnalyzer) "+abc"
     	  (optionalSignParser @ constParser "abc" : charAnalyzer) "-abc"
     ///,
     SeeAlso => {(symbol @,Parser,Parser),constParser,charAnalyzer,(symbol :, Parser, Analyzer)}
     }

document { Key => NNParser,
     Headline => "a parser that accepts (and returns) a natural number, one character at a time",
     SourceCode => NNParser,
     EXAMPLE lines ///
     	  NNParser "1"
	  oo null
     	  ooo "2"
	  oo null
     	  ooo "3"
	  oo null
     	  (NNParser : charAnalyzer) "123456789123456789123456789"
	  class oo
     ///,
     SeeAlso => {charAnalyzer,(symbol :, Parser, Analyzer)}
     }

document { Key => ZZParser,
     Headline => "a parser that accepts (and returns) an integer, one character at a time",
     SourceCode => ZZParser,
     EXAMPLE lines ///
     	  (ZZParser : charAnalyzer) "123456789"
     	  (ZZParser : charAnalyzer) "-123456789"
	  class oo
     ///,
     SeeAlso => {charAnalyzer,(symbol :, Parser, Analyzer)}
     }

document { Key => QQParser,
     Headline => "a parser that accepts (and returns) a rational number, one character at a time",
     SourceCode => QQParser,
     PARA "The denominator must be present.",
     EXAMPLE lines ///
     	  (QQParser : charAnalyzer) "-123456789/54321"
     ///,
     SeeAlso => {charAnalyzer,(symbol :, Parser, Analyzer)}
     }
	  
document { Key => optP,
     Headline => "making a parser optional",
     Usage => "optP p",
     Inputs => { "p" => Parser },
     Outputs => { Parser => {"a parser that accepts tokens accepted by p and returns the value returned by p, or accepts the empty sequence of tokens and returns
	  the symbol ", TT "nil", ".  It prefers to do the former."}},
     PARA { "After the first token, the parser is no slower than ", TT "p", " would have been." },
     EXAMPLE lines ///
     	  (optP constParser "abc" : charAnalyzer) "abc"
     	  (optP constParser "abc" : charAnalyzer) ""
     ///,
     SeeAlso => {constParser, charAnalyzer, (symbol :, Parser, Analyzer)}
     }
	  

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Parsing pre-install"
-- End:
