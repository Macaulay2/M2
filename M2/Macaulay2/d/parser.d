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
	  if c == '\"'
	  then nothing
	  else i = 10 * i + (c - '0')
	  );
     i);
export parseDouble(s:string):double := (
     point := false;
     x := 0.0;
     y := 1.0;
     foreach c in s do (
	  if c == '\"'
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
	  if s.i == '\"' then break;
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



export thenW := dummyWord;		  -- filled in by binding.d
export whenW := dummyWord;		  -- filled in by binding.d
export elseW := dummyWord;		  -- filled in by binding.d
export ofW := dummyWord;		  -- filled in by binding.d
export doW := dummyWord;		  -- filled in by binding.d
export listW := dummyWord;		  -- filled in by binding.d
export fromW := dummyWord;		  -- filled in by binding.d
export toW := dummyWord;		  -- filled in by binding.d
export debug := false;
export tracefile := dummyfile;
export openTokenFile(filename:string):(TokenFile or errmsg) := (
     when openPosIn(filename)
     is f:PosFile do (TokenFile or errmsg)(TokenFile(f,NULL))
     is s:errmsg  do (TokenFile or errmsg)(s)
     );
export setprompt(file:TokenFile,prompt:function(file):void):void := (
     setprompt(file.posFile,prompt)
     );
export flush(file:TokenFile):void := flush(file.posFile);
export close(file:TokenFile):int := close(file.posFile);
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
export errorunary(token1:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     errorpos(token1.position,"syntax error at '" + token1.word.name + "'");
     errorTree
     );
export errorbinary(lhs:ParseTree, token2:Token, file:TokenFile, prec:int,obeylines:bool):ParseTree := (
     errorpos(token2.position,"syntax error at '" + token2.word.name + "'");
     errorTree
     );
export defaultunary(token1:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     accumulate(ParseTree(token1),file,prec,obeylines)
     );
export parse(file:TokenFile,prec:int,obeylines:bool):ParseTree;
export nparse(file:TokenFile,prec:int,obeylines:bool):ParseTree;
export unaryop(token1:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     ret := parse(file,max(prec,token1.word.parse.unaryStrength),obeylines);
     if ret == errorTree then ret
     else accumulate(ParseTree(Unary(token1,ret)),file,prec,obeylines));
export nunaryop(token1:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     ret := nparse(file,token1.word.parse.unaryStrength,obeylines);
     if ret == errorTree then ret
     else accumulate(ParseTree(Unary(token1,ret)),file,prec,obeylines));
export nnunaryop(token1:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     if token1.word.parse.precedence <= prec
     then errorunary(token1,file,prec,obeylines)
     else (
	  ret := nparse(file,token1.word.parse.unaryStrength,obeylines);
	  if ret == errorTree then ret
	  else accumulate(ParseTree(Unary(token1,ret)),file,prec,obeylines)));
export precSpace := 0;		-- filled in later by binding.d
export defaultbinary(lhs:ParseTree, token2:Token, file:TokenFile, prec:int, obeylines:bool):ParseTree := (
     if token2.followsNewline then (
     	  errorpos(token2.position,"missing semicolon or comma on previous line?");
     	  errorTree)
     else (
     	  ret := token2.word.parse.funs.unary(token2,file,precSpace-1,obeylines);
     	  if ret == errorTree then ret else ParseTree(Adjacent(lhs,ret))));
export postfixop(lhs:ParseTree, token2:Token, file:TokenFile, prec:int, obeylines:bool):ParseTree := (
     accumulate(ParseTree(Postfix(lhs,token2)),file,prec,obeylines));
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
     	       token = gettoken(file,obeylines);
	       token.word.parse.funs.unary(token,file,prec,obeylines)
	       )
     	  else ParseTree(dummy(token.position))
	  );
     if ret == errorTree then (
	  if isatty(file) then flush(file) else skip(file,prec));
     ret
     );
export binaryop(lhs:ParseTree, token2:Token, file:TokenFile, prec:int, obeylines:bool):ParseTree := (
     ret := parse(file,token2.word.parse.binaryStrength,obeylines);
     if ret == errorTree then ret 
     else ParseTree(Binary(lhs, token2, ret)));
export nbinaryop(lhs:ParseTree, token2:Token, file:TokenFile, prec:int, obeylines:bool):ParseTree := (
     ret := nparse(file,token2.word.parse.binaryStrength,obeylines);
     if ret == errorTree then ret else ParseTree(Binary(lhs, token2, ret)));
export arrowop(lhs:ParseTree, token2:Token, file:TokenFile, prec:int, obeylines:bool):ParseTree := (
     e := parse(file,token2.word.parse.binaryStrength,obeylines);
     if e == errorTree then e else ParseTree(Arrow(lhs, token2, e, dummyDesc)));
MatchPair := {left:string, right:string, next:(null or MatchPair)};
matchList := (null or MatchPair)(NULL);
export addmatch(left:string, right:string):void := (
     matchList = MatchPair(left,right,matchList);
     );
matcher(left:string):string := (
     rest := matchList;
     while true do
     when rest is null do break	    	 	-- should not happen
     is matchPair:MatchPair do (
	  if matchPair.left == left then return(matchPair.right);
	  rest = matchPair.next;	  
	  );
     ""
     );
match(left:string,right:string):bool := (
     rest := matchList;
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
export unaryparen(left:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     rightparen := matcher(left.word.name);
     if rightparen == peektoken(file,false).word.name
     then accumulate(ParseTree(EmptyParentheses(left,gettoken(file,false))),file,prec,obeylines)
     else (
	  e := parse(file,left.word.parse.unaryStrength,false);
	  if e == errorTree then return(e);
	  right := gettoken(file,false);
	  if rightparen == right.word.name
	  then accumulate(ParseTree(Parentheses(left,e,right)),file,prec,obeylines)
	  else (
	       errorpos(right.position, "expected \"" + rightparen + "\"");
	       errorpos(left.position," ... to match this");
	       errorTree)));
export unarywhile(
     whileToken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     predicate := parse(file,whileToken.word.parse.unaryStrength,false);
     if predicate == errorTree then return(errorTree);
     token2 := gettoken(file,false);
     if token2 == errorToken then return(errorTree);
     if token2.word == doW then (
	  doClause := parse(file,doW.parse.unaryStrength,obeylines);
	  if doClause == errorTree then return(errorTree);
	  r := ParseTree(WhileDo(whileToken,predicate,token2,doClause));
	  accumulate(r,file,prec,obeylines))
     else if token2.word == listW then (
	  listClause := parse(file,listW.parse.unaryStrength,obeylines);
	  if listClause == errorTree then return(errorTree);
	  if peektoken(file,obeylines).word == doW then (
	       doToken := gettoken(file,obeylines);
	       if doToken == errorToken then return(errorTree);
	       doClause := parse(file,doW.parse.unaryStrength,obeylines);
	       if doClause == errorTree then return(errorTree);
	       ret := ParseTree(WhileListDo(whileToken,predicate,token2,listClause,doToken,doClause));
	       accumulate(ret,file,prec,obeylines))
	  else (
	       ret := ParseTree(WhileList(whileToken,predicate,token2,listClause));
	       accumulate(ret,file,prec,obeylines)))
     else (
	  errorpos(token2.position,"syntax error : expected 'do' or 'list'");
	  errorpos(whileToken.position," ... to match this 'while'");
	  errorTree));

export unaryfor(
     forToken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     var := parse(file,forToken.word.parse.unaryStrength,false);
     if var == errorTree then return(errorTree);
     fromClause := dummyTree;
     toClause := dummyTree;
     whenClause := dummyTree;
     listClause := dummyTree;
     doClause := dummyTree;
     token2 := gettoken(file,false);
     if token2 == errorToken then return(errorTree);
     if token2.word == fromW then (
	  fromClause = parse(file,fromW.parse.unaryStrength,false);
	  if fromClause == errorTree then return(errorTree);
     	  token2 = gettoken(file,false);
	  );
     if token2.word == toW then (
	  toClause = parse(file,toW.parse.unaryStrength,false);
	  if toClause == errorTree then return(errorTree);
     	  token2 = gettoken(file,false);
	  );
     if token2.word == whenW then (
	  whenClause = parse(file,whenW.parse.unaryStrength,false);
	  if whenClause == errorTree then return(errorTree);
     	  token2 = gettoken(file,false);
	  );
     if token2.word == doW then (
	  doClause = parse(file,doW.parse.unaryStrength,obeylines);
	  if doClause == errorTree then return(errorTree);
	  r := ParseTree(For( forToken, var, fromClause, toClause, whenClause, listClause,doClause, dummyScope ));
	  accumulate(r,file,prec,obeylines))
     else if token2.word == listW then (
	  listClause = parse(file,listW.parse.unaryStrength,obeylines);
	  if listClause == errorTree then return(errorTree);
	  if peektoken(file,obeylines).word == doW then (
	       gettoken(file,obeylines);
	       doClause = parse(file,doW.parse.unaryStrength,obeylines);
	       if doClause == errorTree then return(errorTree);
	       );
	  r := ParseTree(For(forToken, var, fromClause, toClause,whenClause, listClause, doClause, dummyScope));
	  accumulate(r,file,prec,obeylines))
     else (
	  errorpos(token2.position,"syntax error : expected 'do' or 'list'");
	  errorpos(forToken.position," ... to match this 'for'");
	  errorTree));

unstringToken(q:Token):Token := (
     if q.word.typecode == TCstring 
     then Token(unique(parseString(q.word.name),q.word.parse),q.position,q.scope,q.entry,q.followsNewline)
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
export unaryif(ifToken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     predicate := parse(file,ifToken.word.parse.unaryStrength,false);
     if predicate == errorTree then return(predicate);
     thenToken := gettoken(file,false);
     if thenToken == errorToken then return(errorTree);
     if thenToken.word != thenW then (
	  errorpos(thenToken.position,"syntax error : expected 'then'");
	  errorpos(ifToken.position," ... to match this 'if'");
	  return(errorTree));
     thenClause := parse(file,thenW.parse.unaryStrength,obeylines);
     if thenClause == errorTree then return(errorTree);
     if peektoken(file,obeylines).word == elseW then (
     	  elseToken := gettoken(file,obeylines);
	  if elseToken == errorToken then return(errorTree);
	  elseClause := parse(file,elseW.parse.unaryStrength,obeylines);
     	  if elseClause == errorTree then return(errorTree);
	  ret := ParseTree(IfThenElse(ifToken,predicate,thenClause,elseClause));
	  accumulate(ret,file,prec,obeylines))
     else (
	  ret := ParseTree(IfThen(ifToken,predicate,thenClause));
	  accumulate(ret,file,prec,obeylines))
     );
export unarytry(tryToken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     primary := parse(file,tryToken.word.parse.unaryStrength,obeylines);
     if primary == errorTree then return(primary);
     if peektoken(file,obeylines).word == elseW then (
	  elseToken := gettoken(file,false);
	  if elseToken == errorToken then return(errorTree);
	  if elseToken.word != elseW then (
	       errorpos(elseToken.position,"syntax error : expected 'else'");
	       errorpos(tryToken.position," ... to match this 'try'");
	       return(errorTree));
	  elseClause := parse(file,elseW.parse.unaryStrength,obeylines);
	  if elseClause == errorTree then return(errorTree);
	  accumulate(ParseTree(TryElse(tryToken,primary,elseToken,elseClause)),file,prec,obeylines))
     else accumulate(ParseTree(Try(tryToken,primary)),file,prec,obeylines));
export unarynew(newtoken:Token,file:TokenFile,prec:int,obeylines:bool):ParseTree := (
     newclass := parse(file,newtoken.word.parse.unaryStrength,obeylines);
     if newclass == errorTree then return(errorTree);
     ofToken := dummyToken;
     newparent := dummyTree;
     if peektoken(file,obeylines).word == ofW then (
	  ofToken = gettoken(file,obeylines);
	  newparent = parse(file,ofW.parse.unaryStrength,obeylines);
	  if newparent == errorTree then return(errorTree);
	  );
     fromToken := dummyToken;
     newinitializer := dummyTree;
     if peektoken(file,obeylines).word == fromW then (
	  fromToken = gettoken(file,obeylines);
	  newinitializer = parse(file,fromW.parse.unaryStrength,obeylines);
	  if newinitializer == errorTree then return(errorTree);
	  );
     accumulate(ParseTree(New(newtoken,newclass,newparent,newinitializer)),file,prec,obeylines));

export treePosition(e:ParseTree):Position := (
     while true do (
	  when e
	  is dummy do return(dummyPosition)
	  is token:Token do return(token.position)
	  is adjacent:Adjacent do e = adjacent.rhs
	  is binary:Binary do return(binary.operator.position)
	  is a:Arrow do return(a.operator.position)
	  is unary:Unary do return(unary.operator.position)
	  is postfix:Postfix do return(postfix.operator.position)
	  is a:Quote do return(a.operator.position)
	  is a:GlobalQuote do return(a.operator.position)
	  is a:LocalQuote do return(a.operator.position)
	  is ee:Parentheses do return(ee.left.position)
	  is ee:EmptyParentheses do return(ee.left.position)
     	  is i:IfThen do return(i.ifToken.position)
	  is i:TryElse do return(i.tryToken.position)
	  is i:Try do return(i.tryToken.position)
     	  is i:IfThenElse do return(i.ifToken.position)
     	  is w:For do return(w.forToken.position)
     	  is w:WhileDo do return(w.whileToken.position)
     	  is w:WhileList do return(w.whileToken.position)
     	  is w:WhileListDo do return(w.whileToken.position)
     	  is s:StartScope do e = s.body
	  is n:New do return(n.newtoken.position)
	  )
     );
