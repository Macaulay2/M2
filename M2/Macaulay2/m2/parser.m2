--		Copyright 2006 by Daniel R. Grayson

-- new way to parse 

Parser = new SelfInitializingType of FunctionClosure

space := set characters " \t\f\n\r"
parse = (parser,string) -> (
     scan(length characters string, i -> (
	       c := string#i;
	       if not space#?c then (			    -- for classic Macaulay parsing, we ignore all spaces
		    p := parser c;
		    if p === null then error("syntax error at character '",c,"', position ",i);
		    parser = p)));
     val := parser null;
     if val === null then error "parser unfinished, at end of string";
     val
     )
deadP = Parser (c -> null)
doneP = val -> new Parser from (c -> if c === null then val)
nullP = doneP nil
orP = x -> (
     assert( x =!= null );
     if instance(x, Function) then return x;
     if #x == 0 then return deadP;
     Parser (c -> (
	  if c === null then return for p in x do if (t := p null) =!= null then return t;
	  y := select(apply(x, p -> p c), p -> p =!= null);
	  if #y > 0 then orP y)))
Parser | Parser := orP
andP = x -> (
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
Parser * Parser := andP
repP = p -> (
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
+ Parser := repP
munge = fun -> f := parser -> new Parser from (c -> ( p -> if p =!= null then (if c === null then fun else f) p ) parser c)
futureP = parserSymbol -> Parser (c -> (value parserSymbol) c)
alpha := set characters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
letterP = Parser (c -> if alpha#?c then b -> if b === null then c)
spaceP = Parser (c -> if c === null then nil else if space#?c then spaceP)
symbolP = (munge (x -> ( if not isGlobalSymbol x then error("symbol ",x," undefined"); getGlobalSymbol x ))) letterP
digit := hashTable {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
natNumberP = Parser ( f := n -> new Parser from (c -> if c === null then n else if digit#?c then f (10*n + digit#c)); new Parser from (c -> if digit#?c then f digit#c))
constP = s -> ( f := n -> new Parser from (c -> if c === null then if n === #s then s else null else if s#?n and c === s#n then f(n+1) else null)) 0
optP = parser -> parser | nullP
ifNone = default -> parser -> parser | doneP default
repParser1 = parser -> parser * +parser
optSignP = Parser(c -> if c === null then 1 else if c === "-" then doneP(-1) else if c === "+" then doneP 1)
integerP = (munge times) (optSignP * natNumberP)
ratNumberP = (munge ((num,sl,den) -> num/den)) andP(integerP,constP "/",natNumberP)
listP = comma -> parser -> (munge splice) (parser * repP (munge last) (constP comma * parser))
variableP = (munge value) symbolP
intP = natNumberP | variableP
subscriptP = (munge ((lb,x,rb) -> x)) andP( constP "[", (munge unsequence) (listP ",") intP, constP "]" )
ringVariableP = (munge ((x,n) -> if n === nil then value x else x_n)) (symbolP * optP subscriptP)
numberP = orP(integerP,ratNumberP)
powerP = (munge ((x,n) -> if n === nil then x else x^n)) (orP(futureP parenExprP,ringVariableP) * optP natNumberP)
monomialP = (munge (times @@ deepSplice)) (optSignP * (numberP * repP powerP | repParser1 powerP ))
polyP = (munge (plus @@ deepSplice)) repParser1 monomialP | doneP 0
parenExprP = (munge ((lp,x,rp) -> x)) andP(constP "(", futureP parenExprP | polyP, constP ")")
listPolyP = (munge toList) (listP ",") polyP
arrayPolyP = (munge toList) (listP ";") listPolyP

poly = method()
poly String := (s) -> parse(polyP, s)
ideal String := (s) -> ideal parse(listPolyP, s)
matrix String := o -> (s) -> matrix parse(arrayPolyP, s)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
