--		Copyright 1994 by Daniel R. Grayson
use err;
use system;
use strings;
use varstrin;
use lex;
use stdio;
use stdiop;
use arith;
use nets;
use tokens;


export parseInt(s:string):Integer := (
     i := toInteger(0);
     foreach c in s do (
	  if c == '"'
	  then nothing
	  else i = 10 * i + (c - '0')
	  );
     i);
export parseDouble(s:string):double := (
     point := false;
     x := 0.0;
     y := 1.0;
     foreach c in s do (
	  if c == '"'
	  then nothing
	  else if c == '.'
	  then point = true
	  else if point
	  then (
	       x = x + (y * (c - '0'))/10;
	       y = y / 10;
	       )
	  else x = 10 * x + (c - '0')
	  );
     x);
export parseString(s:string):string := (
     v := newvarstring(length(s)-2);
     i := 1;
     while true do (
	  if s.i == '"' then break;
	  if s.i == '\\' then (
	       i = i+1;
	       c := s.i;
	       if c == 'n' then v << '\n'
	       else if c == 'r' then v << '\r'
	       else if c == 't' then v << '\t'
	       else if c == 'f' then v << '\f'
	       else if '0' <= c && c < '8' then (
		    j := c - '0';
		    c = s.(i+1);
		    if '0' <= c && c < '8' then (
			 i = i+1;
			 j = 8 * j +  (c - '0');
			 c = s.(i+1);
			 if '0' <= c && c < '8' then (
			      i = i+1;
			      j = 8 * j +  (c - '0');
			      );
			 );
		    v << char(j)
		    )
	       else v << c
	       )
	  else v << s.i;
	  i = i+1;
	  );
     tostring(v)
     );

export parseChar(s:string):char := (
     if s.1 == '\\' then (
	  c := s.2;
	  if c == 'n' then '\n'
	  else if c == 'r' then '\r'
	  else if c == 't' then '\t'
	  else if c == 'f' then '\f'
	  else if '0' <= c && c < '8' then (
	       j := c - '0';
	       c = s.3;
	       if '0' <= c && c < '8' then (
		    j = 8 * j +  (c - '0');
		    c = s.4;
		    if '0' <= c && c < '8' then (
			 j = 8 * j +  (c - '0');
			 );
		    );
	       char(j)
	       )
	  else c
	  )
     else s.1
     );



export thenW := dummyWord;		  -- filled in by keywords.d
export elseW := dummyWord;		  -- filled in by keywords.d
export ofW := dummyWord;		  -- filled in by keywords.d
export doW := dummyWord;		  -- filled in by keywords.d
export fromW := dummyWord;		  -- filled in by keywords.d
export debug := false;
export tracefile := dummyfile;
export openTokenFile(filename:string):(TokenFile or errmsg) := (
     when fopeninp(filename)
     is f:PosFile do (TokenFile or errmsg)(TokenFile(f,NULL))
     is s:errmsg  do (TokenFile or errmsg)(s)
     );
export setprompt(file:TokenFile,prompt:function():void):void := (
     setprompt(file.posFile,prompt)
     );
export flush(file:TokenFile):void := flush(file.posFile);
export close(file:TokenFile):void := close(file.posFile);
export gettoken(file:TokenFile,obeylines:bool):Token := (
     when file.last
     is null do gettoken(file.posFile,obeylines)
     is w:Token do (
	  file.last = NULL;
     	  w
	  )
     );
export peektoken(file:TokenFile,obeylines:bool):Token := (
     when file.last
     is null do (
	  w := gettoken(file,obeylines);
	  file.last = w;
	  w
	  )
     is w:Token do w
     );
level := 0;
export errorTree := ParseTree(dummy(dummyPosition));
skip(file:TokenFile,prec:int):void := (
     while peektoken(file,false).word.parse.precedence > prec 
     do gettoken(file,false)
     );
accumulate(e:ParseTree,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     if e == errorTree then return(errorTree);
     ret := e;
     while true do (
	  token := peektoken(file,obeylines);
	  if token == errorToken then (
	       gettoken(file,obeylines);
	       ret = errorTree;
	       break;
	       );
	  if token.word.parse.precedence <= prec then break;
	  gettoken(file,obeylines);
	  ret = token.word.parse.funs.binary(ret,token,file,prec,obeylines);
	  if ret == errorTree then break;
	  );
     ret
     );
export defaultunary(token1:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     ret := ParseTree(token1);
     ret = accumulate(ret,file,prec,obeylines);
     ret
     );
export parse(file:TokenFile,prec:int,obeylines:bool):ParseTree;
export nparse(file:TokenFile,prec:int,obeylines:bool):ParseTree;
export unaryop(token1:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     ret := parse(file,max(prec,token1.word.parse.strength),obeylines);
     if ret != errorTree
     then ret = accumulate(ParseTree(Unary(token1,ret)),file,prec,obeylines);
     ret
     );
export nunaryop(token1:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     ret := nparse(file,token1.word.parse.scope,obeylines);
     if ret != errorTree
     then ret = accumulate(ParseTree(Unary(token1,ret)),file,prec,obeylines);
     ret
     );
export precObject := 0;		-- filled in later by keywords.d
export defaultbinary(
     lhs:ParseTree, token2:Token, file:TokenFile, prec:int, obeylines:bool):ParseTree := (
     e := token2.word.parse.funs.unary(token2,file,precObject-1,obeylines);
     ret := if e == errorTree then e else ParseTree(Adjacent(lhs,e));
     ret
     );
export postfixop(
     lhs:ParseTree, token2:Token, file:TokenFile, prec:int, obeylines:bool):ParseTree := (
     e := ParseTree(Postfix(lhs,token2));
     ret := accumulate(e,file,prec,obeylines);
     ret
     );
export parse(file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     token := gettoken(file,false);
     if token == errorToken then return(errorTree);
     ret := token.word.parse.funs.unary(token,file,prec,obeylines);
     if ret == errorTree then (
	  if isatty(file) then flush(file) else skip(file,prec));
     ret
     );
export nparse(file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     token := peektoken(file,obeylines);
     if token == errorToken then return(errorTree);
     ret := (
	  if token == errorToken
	  then errorTree
	  else if token.word.parse.precedence > prec
     	  then (
     	       gettoken(file,obeylines);
	       token.word.parse.funs.unary(token,file,prec,obeylines)
	       )
     	  else ParseTree(dummy(token.position))
	  );
     if ret == errorTree then (
	  if isatty(file) then flush(file) else skip(file,prec));
     ret
     );
export binaryop(
     lhs:ParseTree, token2:Token, 
     file:TokenFile, prec:int, obeylines:bool):ParseTree := (
     e := parse(file,token2.word.parse.scope,obeylines);
     ret := if e == errorTree then e else ParseTree(Binary(lhs, token2, e));
     ret
     );
export nbinaryop(
     lhs:ParseTree, token2:Token, 
     file:TokenFile, prec:int, obeylines:bool):ParseTree := (
     e := nparse(file,token2.word.parse.scope,obeylines);
     ret := if e == errorTree then e else ParseTree(Binary(lhs, token2, e));
     ret
     );
export arrowop(
     lhs:ParseTree, token2:Token, 
     file:TokenFile, prec:int, obeylines:bool):ParseTree := (
     e := parse(file,token2.word.parse.scope,obeylines);
     ret := if e == errorTree then e else ParseTree(
	  Arrow(lhs, token2, e, dummyDesc)
	  );
     ret
     );
export errorunary(token1:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     errorpos(token1.position,"syntax error at " + token1.word.name);
     errorTree
     );
export errorbinary(lhs:ParseTree, token2:Token, 
     file:TokenFile, prec:int,obeylines:bool):ParseTree := (
     errorpos(token2.position,"syntax error at " + token2.word.name);
     errorTree
     );
MatchPair := {left:string, right:string, next:(null or MatchPair)};
matchlist := (null or MatchPair)(NULL);
export addmatch(left:string, right:string):void := (
     matchlist = MatchPair(left,right,matchlist);
     );
matcher(left:string):string := (
     rest := matchlist;
     while true do
     when rest
     is null do break	    	 	-- shouldn t happen
     is matchPair:MatchPair do (
	  if matchPair.left == left then return(matchPair.right);
	  rest = matchPair.next;	  
	  );
     ""
     );
match(left:string,right:string):bool := (
     rest := matchlist;
     while true do
     when rest
     is null do return(false)
     is matchPair:MatchPair do (
	  if matchPair.left == left
	  then if matchPair.right == right
	  then return(true);
	  rest = matchPair.next;
	  ));
export varexprlist := {
     list:array(ParseTree),
     size:int
     };
export newvarexprlist(i:int):varexprlist := varexprlist(
     new array(ParseTree) len i do provide dummyTree, 
     0);
needatleast(i:int,v:varexprlist):void := (
     if length(v.list) < i then (
     	  v.list = new array(ParseTree) len 2*i do (
	       foreach e in v.list do provide e;
	       while true do provide dummyTree;
	       );
     	  );
     );
export (v:varexprlist) << (e:ParseTree) : varexprlist := (
     needatleast(v.size + 1,v);
     v.list.(v.size) = e;
     v.size = v.size + 1;
     v
     );
export toexprlist(v:varexprlist):ArrayParseTree := (
     new array(ParseTree) len v.size do foreach e in v.list do provide e
     );
singleparen(left:Token, file:TokenFile, prec:int):ParseTree:=(
     rightparen := matcher(left.word.name);
     if rightparen == peektoken(file,false).word.name then (
	  return(ParseTree(parentheses(left,gettoken(file,false)))));
     e := parse(file,left.word.parse.scope,false);
     if e == errorTree then return(e);
     right := gettoken(file,false);
     if rightparen != right.word.name then (
	  errorpos(right.position, "expected \"" + rightparen + "\"");
	  errorpos(left.position," -- to match this");
	  errorTree)
     else ParseTree(parenthesized(left,e,right)));
unaryparen(left:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     rightparen := matcher(left.word.name);
     ret := singleparen(left,file,prec);
     --ret = accumulate(ret,file,prec+1,obeylines); -- f(x) g(x) version
     ret = accumulate(ret,file,prec,obeylines);     -- f g h x version
     ret
     );
export binarybracket(
     lhs:ParseTree, token2:Token, 
     file:TokenFile, prec:int,obeylines:bool):ParseTree := (
     e := singleparen(token2,file,token2.word.parse.scope);
     ret := if e == errorTree then e else ParseTree(Adjacent(lhs,e));
     ret
     );
export leftparen  := parsefuns(unaryparen, defaultbinary);
export rightparen := parsefuns(errorunary, errorbinary);
export unarywhile(
     iftoken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     predicate := parse(file,iftoken.word.parse.scope,false);
     if predicate == errorTree then return(errorTree);
     dotoken := gettoken(file,false);
     if dotoken == errorToken then return(errorTree);
     if dotoken.word != doW then (
	  errorpos(dotoken.position,"syntax error : expected 'do'");
	  return(errorTree));
     body := parse(file,dotoken.word.parse.scope,obeylines);
     if body == errorTree then return(errorTree);
     r := ParseTree(While(iftoken,predicate,dotoken,body));
     accumulate(r,file,prec,obeylines));
unstringToken(q:Token):Token := (
     if q.word.typecode == TCstring 
     then Token(unique(parseString(q.word.name),q.word.parse),q.position,q.scope,q.entry)
     else q);
export unaryquote(
     quotetoken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     arg := gettoken(file,true);
     if arg == errorToken then return(errorTree);
     r := ParseTree(Quote(quotetoken,unstringToken(arg)));
     accumulate(r,file,prec,obeylines));
export unaryglobal(
     quotetoken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     arg := gettoken(file,true);
     if arg == errorToken then return(errorTree);
     r := ParseTree(GlobalQuote(quotetoken,unstringToken(arg)));
     accumulate(r,file,prec,obeylines));
export unarylocal(
     quotetoken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     arg := gettoken(file,true);
     if arg == errorToken then return(errorTree);
     r := ParseTree(LocalQuote(quotetoken,unstringToken(arg)));
     accumulate(r,file,prec,obeylines));
export unaryif(iftoken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     predicate := parse(file,iftoken.word.parse.scope,false);
     if predicate == errorTree then return(predicate);
     thentoken := gettoken(file,false);
     if thentoken == errorToken then return(errorTree);
     if thentoken.word != thenW then (
	  errorpos(thentoken.position,"syntax error : expected 'then'");
	  return(errorTree));
     thenclause := parse(file,thentoken.word.parse.scope,obeylines);
     if thenclause == errorTree then return(errorTree);
     if peektoken(file,obeylines).word == elseW then (
     	  elsetoken := gettoken(file,obeylines);
	  if elsetoken == errorToken then return(errorTree);
	  elseclause := parse(file,elsetoken.word.parse.scope,obeylines);
     	  if elseclause == errorTree then return(errorTree);
	  ret := ParseTree(IfThenElse(iftoken,predicate,
		    thentoken,thenclause,elsetoken,elseclause));
	  ret = accumulate(ret,file,prec,obeylines);
     	  ret
	  )
     else (
	  ret := ParseTree(IfThen(iftoken,predicate,thentoken,thenclause));
	  ret = accumulate(ret,file,prec,obeylines);
	  ret
	  )
     );
export unarytry(trytoken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree:=(
     primary := parse(file,trytoken.word.parse.scope,obeylines);
     if primary == errorTree then return(primary);
     if peektoken(file,obeylines).word == elseW then (
	  elsetoken := gettoken(file,false);
	  if elsetoken == errorToken then return(errorTree);
	  if elsetoken.word != elseW then (
	       errorpos(elsetoken.position,"syntax error : expected 'else'");
	       return(errorTree));
	  elseclause := parse(file,elsetoken.word.parse.scope,obeylines);
	  if elseclause == errorTree then return(errorTree);
	  ret := ParseTree(TryElse(trytoken,primary,elsetoken,elseclause));
	  ret = accumulate(ret,file,prec,obeylines);
	  ret)
     else (
	  ret := ParseTree(Try(trytoken,primary));
	  ret = accumulate(ret,file,prec,obeylines);
	  ret)
     );
export unarynew(newtoken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     newclass := parse(file,newtoken.word.parse.scope,obeylines);
     if newclass == errorTree then return(errorTree);
     oftoken := dummyToken;
     newparent := dummyTree;
     if peektoken(file,obeylines).word == ofW then (
	  oftoken = gettoken(file,obeylines);
	  newparent = parse(file,newtoken.word.parse.scope,obeylines);
	  if newparent == errorTree then return(errorTree);
	  );
     fromtoken := dummyToken;
     newinitializer := dummyTree;
     if peektoken(file,obeylines).word == fromW then (
	  fromtoken = gettoken(file,obeylines);
	  newinitializer = parse(file,newtoken.word.parse.scope,obeylines);
	  if newinitializer == errorTree then return(errorTree);
	  );
     ret := ParseTree(New(newtoken,newclass,oftoken,newparent,fromtoken,newinitializer));
     ret = accumulate(ret,file,prec,obeylines);
     ret);
