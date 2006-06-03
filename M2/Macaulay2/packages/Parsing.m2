newPackage ( "Parsing",
     Authors => {
	  { Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/" }
	  },
     Date => "June, 2006",
     Version => "0.9",
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
export Parsers ; Parsers = new MutableHashTable
export Analyzer ; Analyzer = new SelfInitializingType of FunctionClosure
export Analyzers ; Analyzers = new MutableHashTable
export Machine ; Machine = new SelfInitializingType of FunctionClosure
 

-- character sets and converters
space = set characters " \t\f\n\r"
alpha = set characters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
digit = hashTable {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
-- analyzers
chkstring := string -> if not instance(string, String) then error "analyzer expected a string";
Analyzers#"characters" = Analyzer( 
     string -> (
	  chkstring string;
	  i := 0;
	  () -> if string#?i then ( c := string#i; i = i+1; (i,c))))
Analyzers#"non-space characters" = Analyzer(
     string -> (
	  chkstring string;
	  i := 0;
	  () -> while string#?i do (c := string#i; i = i+1; if not space#?c then return (i,c))) )
-- machines
Parser ++ Analyzer := (p,a) -> s -> (
     a' := a s;
     while null =!= ((pos,t) := a'()) do if null === (p = p t) then error("syntax error at token '",toString t,"', position ",toString pos);
     if null === (r := p null) then error "parser unfinished, at end of input";
     r)
-- parsers
deadP = Parser (c -> null)
doneP = val -> new Parser from (c -> if c === null then val)
nullP = doneP nil
-- parser makers
Parser | Parser := orP = x -> (
     assert( x =!= null );
     if instance(x, Function) then return x;
     if #x == 0 then return deadP;
     Parser (c -> (
	  if c === null then return for p in x do if (t := p null) =!= null then return t;
	  y := select(apply(x, p -> p c), p -> p =!= null);
	  if #y > 0 then orP y)))
Parser * Parser := andP = x -> (
     assert( x =!= null );
     if instance(x, Function) then return x;
     if #x == 0 then return nullP;
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
* Parser := p -> (
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
munge = fun -> f := parser -> new Parser from (c -> ( p -> if p =!= null then (if c === null then fun else f) p ) parser c)
+ Parser := parser -> parser * *parser
futureP = parserSymbol -> Parser (c -> (value parserSymbol) c)

-- Macaulay classic parsing
letterP = Parser (c -> if alpha#?c then b -> if b === null then c)
spaceP = Parser (c -> if c === null then nil else if space#?c then spaceP)
symbolP = (munge (x -> ( if not isGlobalSymbol x then error("symbol ",x," undefined"); getGlobalSymbol x ))) letterP
natNumberP = (() -> (
     	  f := n -> new Parser from (c -> if c === null then n else if digit#?c then f (10*n + digit#c)); 
     	  new Parser from (c -> if digit#?c then f digit#c)
     	  )) ()
constP = s -> ( f := n -> new Parser from (c -> if c === null then if n === #s then s else null else if s#?n and c === s#n then f(n+1) else null)) 0
optP = parser -> parser | nullP
ifNone = default -> parser -> parser | doneP default
optSignP = Parser(c -> if c === null then 1 else if c === "-" then doneP(-1) else if c === "+" then doneP 1)
integerP = (munge times) (optSignP * natNumberP)
ratNumberP = (munge ((num,sl,den) -> num/den)) andP(integerP,constP "/",natNumberP)
listP = comma -> parser -> (munge splice) (parser * * (munge last) (constP comma * parser))
variableP = (munge value) symbolP
intP = natNumberP | variableP
subscriptP = (munge ((lb,x,rb) -> x)) andP( constP "[", (munge unsequence) (listP ",") intP, constP "]" )
ringVariableP = (munge ((x,n) -> if n === nil then value x else x_n)) (symbolP * optP subscriptP)
numberP = integerP | ratNumberP
powerP = (munge ((x,n) -> if n === nil then x else x^n)) ((futureP parenExprP | ringVariableP) * optP natNumberP)
monomialP = (munge (times @@ deepSplice)) (optSignP * (numberP * *powerP | +powerP ))
polyP = (munge (plus @@ deepSplice)) (+monomialP) | doneP 0
parenExprP = (munge ((lp,x,rp) -> x)) andP(constP "(", futureP parenExprP | polyP, constP ")")
listPolyP = (munge toList) (listP ",") polyP
arrayPolyP = (munge toList) (listP ";") listPolyP
export poly ; poly = method()
poly String :=  polyP ++ Analyzers#"non-space characters"
Ideal.Classic = listPolyP ++ Analyzers#"non-space characters"
ideal String := ideal @@ Ideal.Classic
matrix String := opts -> s -> matrix((arrayPolyP ++ Analyzers#"non-space characters") s, opts)

beginDocumentation()

document { 
     Key => {poly,(poly,String)},
     Headline => "make a polynomial using classic Macaulay-like syntax",
     Usage => "poly s",
     Inputs => {
	  "s" => String
	  },
     Outputs => {
	  RingElement
	  },
     "The classic Macaulay polynomial format is useful for fast entry
     of polynomials, ideals, and matrices.  Only ring variables which are
     single letters, or single letters indexed by a sequence of numbers can
     be handled with this parser.",
     PARA{},
     "The rules for creating polynomials using the classic parser include:",
     UL {
	  "Spaces and newline characters are completely ignored.",

	  "A variable is either (1) a single letter, (2) a subscripted variable, or
	  (3) a polynomial enclosed in parentheses.",

	  "Subscripted variables, e.g. y_(1,1) are written using brackets, as in
	  y[1,1].  Instead of explicit numbers for subscripted variables, Macaulay2
	  user variables which have an integer value may be used.",

	  "Coefficients are either integers or rational numbers, starting with
	  a + or - (which may be omitted for the first monomial).  Over finite
	  fields, the division is performed in that field.",

	  "A monomial is written without symbols for multiplication or exponentiation,
	  where the (optional) coefficient comes first.",

	  "A polynomial is a collection of monomials one after the other.",
	  },
     "Except for indices for subscripted variables, integers must be explicitly given.
     All of the variables used must already belong to a specific ring.  If in doubt, 
     first do ", TO (use,Ring), ".",
     EXAMPLE {
	  ///R = ZZ/32003[a..d,x_1..x_4,y_(1,1)..y_(2,2)];///,
	  ///poly"a2b-3ab-1"///,
	  ///poly"a2y[1,1]3-3ab-1"///,
	  ///poly"(a+b)(a+2c)"///,
	  ///poly"(a+b+c)3-1"///,
	  ///poly"3/4a2b-3ab-1"///,
	  ///poly"a5+5a4b+10a3b2+10a2b3+5ab4+b5
	        -10a4c-40a3bc-60a2b2c-40ab3c-10b4c
		+40a3c2+120a2bc2+120ab2c2+40b3c2
		-80a2c3-160abc3-80b2c3+80ac4+80bc4-32c5"///,
	  ///poly"(a+(c+y[1,1])2)2-1"///
	  },
     Caveat => {"Ring variables which are not single characters, or are not
	  indexed by a sequence of integers, cannot be input using this function."
	  },
     SeeAlso => {(ideal,String),(matrix,String)}
     }

document { 
     Key => (ideal,String),
     Headline => "make an ideal using classic Macaulay-like syntax",
     Usage => "ideal s",
     Inputs => {
	  "s" => "in the form: \"f1,f2,...,fr\""
	  },
     Outputs => {
	  Ideal
	  },
     "Creates an ideal using an abbreviated format.
     Each polynomial has the form described in ", TO (poly,String), 
     ".  The polynomials are separated by commas.
     Spaces and newline characters are ignored. ",
     EXAMPLE {
	  "R = ZZ/32003[a..d,x_1..x_4];",
	  ///I = ideal"a+b2-1,(a+b)(c+d),x[1]-x[2]3"///
	  },
     Caveat => {"Ring variables which are not single characters, or are not
	  indexed by a sequence of integers, cannot be input using this function."
	  },
     SeeAlso => {(poly,String), (matrix,String)}
     }

document { 
     Key => (matrix,String),
     Headline => "make a matrix using classic Macaulay-like syntax",
     Usage => "matrix s",
     Inputs => {
	  "s" => "in the form: \"f1,f2,...,fr;g1,...,gr;...;h1,...,hr\""
	  },
     Outputs => {
	  Matrix
	  },
     "Creates a matrix using an abbreviated format.
     Each polynomial has the form described in ", TO (poly,String), 
     ".  The rows of the matrix are separated by semicolons, and
     within each row, the polynomials are separated by commas.  Any
     entry which is missing is assumed to be 0.
     Spaces and newline
     characters are ignored. ",
     EXAMPLE {
	  "R = ZZ/32003[a..d,x_1..x_4];",
	  ///N = matrix"a,b,c,d;x[1],x[2],x[3],x[4]"///,
	  ///M = matrix"ad-2c3,(a-b)2+1,ab;,abc-1,"///,
	  },
     Caveat => {"Ring variables which are not single characters, or are not
	  indexed by a sequence of integers, cannot be input using this function."
	  },
     SeeAlso => {(poly,String), (ideal,String)}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
