--		Copyright 1994 by Daniel R. Grayson

use C;
use system; 
use convertr;
use engine;
use binding;
use evaluate;
use common;
use parser;
use lex;
use gmp;
use nets;
use tokens;
use err;
use stdiop;
use ctype;
use stdio;
use varstrin;
use strings;
use C;
use actors;
use basic;
use struct;
use objects;
-----------------------------------------------------------------------------
isOption(e:Expr):bool := (
     when e
     is b:List do b.class == optionClass && length(b.v) == 2 
     is q:HashTable do q.class == optionTableClass
     else false
     );
numOptions(w:Sequence):int := (
     n := 0;
     foreach x in w do if isOption(x) then n = n+1;
     n);
override(h:HashTable,v:Sequence,numopts:int):Expr := (
     numargs := length(v) - numopts;
     newargs := nullE;
     if numargs == 0 then (newargs = emptySequenceE;)
     else if numargs == 1 then foreach x in v do (if !isOption(x) then newargs = x)
     else (
	  newargs = Expr(
	       new Sequence len numargs do (
	       	    foreach x in v do if !isOption(x) then provide x));
	  );
     z := copy(h);
     z.mutable = true;
     foreach x in v do if isOption(x) then (
	  when x is b:List do (
	       key := b.v.0;
	       keyhash := hash(key);
	       r := storeInHashTableMustClobber(z,key,keyhash,b.v.1);
	       when r is Error do return r else nothing;
	       )
	  is y:HashTable do (
	       foreach bucket in y.table do (
		    q := bucket;
		    while q != q.next do (
			 r := storeInHashTableMustClobber(z,q.key,q.hash,q.value);
			 when r is Error do return r else nothing;
			 q = q.next)))
	  else nothing;					    -- shouldn't occur
	  );
     sethash(z,h.mutable);
     Expr(Sequence(Expr(z),newargs)));
emptyOptionTable := newHashTable(optionTableClass,nothingClass);
override(v:Sequence,numopts:int):Expr := (
     numargs := length(v) - numopts;
     newargs := nullE;
     if numargs == 0 then (newargs = emptySequenceE;)
     else if numargs == 1 then foreach x in v do (if !isOption(x) then newargs = x)
     else (
	  newargs = Expr(
	       new Sequence len numargs do (
	       	    foreach x in v do if !isOption(x) then provide x));
	  );
     z := copy(emptyOptionTable);
     z.mutable = true;
     foreach x in v do if isOption(x) then (
	  when x is b:List do (
	       key := b.v.0;
	       keyhash := hash(key);
	       r := storeInHashTable(z,key,keyhash,b.v.1);
	       when r is Error do return r else nothing;
	       )
	  is y:HashTable do (
	       foreach bucket in y.table do (
		    q := bucket;
		    while q != q.next do (
			 r := storeInHashTable(z,q.key,q.hash,q.value);
			 when r is Error do return r else nothing;
			 q = q.next)))
	  else nothing;					    -- shouldn't occur
	  );
     sethash(z,false);
     Expr(Sequence(Expr(z),newargs)));
override(e:Expr):Expr := (
     when e is args:Sequence do (
	  if length(args) == 2 then (
	       when args.0
	       is h:HashTable do (
		    if h.mutable then WrongArg("an immutable hash table")
		    else when args.1 is v:Sequence do override(h,v,numOptions(v))
		    else override(h,Sequence(args.1),if isOption(args.1) then 1 else 0)
		    )
	       is Nothing do (
		    when args.1 is v:Sequence do override(v,numOptions(v))
		    else override(Sequence(args.1),if isOption(args.1) then 1 else 0)
		    )
	       else WrongArg(1,"a hashtable"))
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
setupfun("override",override);
-----------------------------------------------------------------------------
equalmethod(x:Expr,y:Expr):Expr := (
     method := lookupBinaryMethod(Class(x),Class(y),EqualEqualS);
     if method == nullE 
     then MissingMethodPair(EqualEqualS,x,y)
     else applyEEE(method,x,y));
EqualEqualfun(x:Expr,y:Expr):Expr := (
     -- some cases, where the types are equal, call immediately for strict equality
     -- some cases call for simple recursive routines
     if x == y then True else
     when x
     is xx:ZZ do (
	  when y 
	  is yy:ZZ do toExpr(yy === xx)			    -- # typical value: symbol ==, ZZ, ZZ, Boolean
	  is yy:QQ do toExpr(yy === xx)			    -- # typical value: symbol ==, ZZ, QQ, Boolean
	  is yy:RR do toExpr(yy === xx)			    -- # typical value: symbol ==, ZZ, RR, Boolean
	  is yy:CC do toExpr(yy === xx)			    -- # typical value: symbol ==, ZZ, CC, Boolean
	  else equalmethod(x,y)
	  )
     is xx:SymbolClosure do (
	  when y is yy:SymbolClosure do toExpr(xx === yy)             -- # typical value: symbol ==, Symbol, Symbol, Boolean
	  else equalmethod(x,y)
     	  )
     is xx:QQ do (
	  when y
	  is yy:ZZ do toExpr(xx === yy)			    -- # typical value: symbol ==, QQ, ZZ, Boolean
	  is yy:QQ do toExpr(xx === yy)			    -- # typical value: symbol ==, QQ, QQ, Boolean
	  is yy:RR do toExpr(xx === yy)			    -- # typical value: symbol ==, QQ, RR, Boolean
	  is yy:CC do toExpr(xx === yy)			    -- # typical value: symbol ==, QQ, CC, Boolean
	  else equalmethod(x,y)
	  )
     is xx:RR do (
	  when y
	  is yy:ZZ do toExpr(xx === yy)			    -- # typical value: symbol ==, RR, ZZ, Boolean
	  is yy:QQ do toExpr(xx === yy)			    -- # typical value: symbol ==, RR, QQ, Boolean
	  is yy:RR do toExpr(xx === yy)			    -- # typical value: symbol ==, RR, RR, Boolean
	  is yy:CC do toExpr(xx === yy)			    -- # typical value: symbol ==, RR, CC, Boolean
	  else equalmethod(x,y)
	  )
     is xx:CC do (
	  when y
	  is yy:ZZ do toExpr(xx === yy)			    -- # typical value: symbol ==, CC, ZZ, Boolean
	  is yy:QQ do toExpr(xx === yy)			    -- # typical value: symbol ==, CC, QQ, Boolean
	  is yy:RR do toExpr(xx === yy)			    -- # typical value: symbol ==, CC, RR, Boolean
	  is yy:CC do toExpr(xx === yy)			    -- # typical value: symbol ==, CC, CC, Boolean
	  else equalmethod(x,y)
	  )
     is xx:Boolean do (
	  when y is yy:Boolean do if xx == yy then True else False -- # typical value: symbol ==, Boolean, Boolean, Boolean
	  else equalmethod(x,y)
     	  )
     is xx:Net do (
	  when y is yy:Net do toExpr(xx === yy)	-- # typical value: symbol ==, Net, Net, Boolean
	  else equalmethod(x,y)
     	  )
     is xx:string do (
	  when y 
	  is yy:string do toExpr(xx === yy)	 -- # typical value: symbol ==, String, String, Boolean
	  else equalmethod(x,y))
     is s:Sequence do when y is t:Sequence do (				 -- # typical value: symbol ==, Sequence, Sequence, Boolean
	  if length(s) != length(t) then return False;
	  for i from 0 to length(s)-1 do (
	       ret := EqualEqualfun(s.i,t.i);
	       when ret is Error do return ret else nothing;
	       if ret == False then return False;
	       );
	  True
	  ) else equalmethod(x,y)
     else equalmethod(x,y));
listComparison(e:Expr):Expr := (
     when e 
     is args:Sequence do if length(args) != 2 then WrongNumArgs(2) else (
	  when args.0 is a:List do
	  when args.1 is b:List do (
	       if a.class != b.class then return False;
	       s := a.v;
	       t := b.v;
	       if length(s) != length(t) then return False;
	       for i from 0 to length(s)-1 do (
		    ret := EqualEqualfun(s.i,t.i);
		    when ret is Error do return ret else nothing;
		    if ret == False then return False;
		    );
	       True
	       )
	  else WrongArg(2,"a visible list")
	  else WrongArg(1,"a visible list")
	  )
     else WrongNumArgs(2));
installMethod(EqualEqualS,visibleListClass,visibleListClass,listComparison);
EqualEqualfun(lhs:Code,rhs:Code):Expr := (
     x := eval(lhs);
     when x is Error do x
     else (
     	  y := eval(rhs);
	  when y is Error do y else EqualEqualfun(x,y)));
setup(EqualEqualS,EqualEqualfun);
not(z:Expr):Expr := (
     when z is Error do z 
     else if z == True then False 
     else if z == False then True
     else buildErrorPacket("expected true or false"));

NotEqualfun(lhs:Code,rhs:Code):Expr := (
     x := eval(lhs);
     when x is Error do x
     else (
     	  y := eval(rhs);
     	  when y is Error do y
	  else notFun(EqualEqualfun(x,y))));
setup(NotEqualS,NotEqualfun);

compare(left:Expr,right:Expr):Expr := (
     if left == right then EqualEqualE else
     when left
     is x:ZZ do (
	  when right
	  is y:ZZ do
	  if x < y then LessE else if x > y then GreaterE else EqualEqualE
	  is y:QQ do
	  if x < y then LessE else if x > y then GreaterE else EqualEqualE
	  is y:RR do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:CC do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
     	  is Error do right
	  else binarymethod(left,right,QuestionS))
     is x:string do (
	  when right
	  is y:string do (
	       c := strnumcmp(x,y);			    -- should use same method as for symbols below
	       if c == 1 then GreaterE
	       else if c == -1 then LessE
     	       else EqualEqualE
	       )
	  is y:Net do (
	       c := netcmp(x,y);
	       if c == 1 then GreaterE
	       else if c == -1 then LessE
     	       else EqualEqualE
	       )
     	  is Error do right
	  else binarymethod(left,right,QuestionS))
     is x:SymbolClosure do (
	  when right
	  is y:SymbolClosure do (
	       c := strnumcmp(x.symbol.word.name,y.symbol.word.name); -- should use same method as for strings above
	       if c == 1 then GreaterE
	       else if c == -1 then LessE
	       else (
		    if x.symbol.hash < y.symbol.hash then LessE
		    else if x.symbol.hash > y.symbol.hash then GreaterE
		    else (
			 -- if we had a Sequence number stored in each frame
			 -- we could make the rest of the comparison well-defined
			 EqualEqualE
			 )
		    )
	       )
	  else binarymethod(left,right,QuestionS))
     is x:QQ do (
	  when right
	  is y:ZZ do 
	  if x < y then LessE else if x > y then GreaterE else EqualEqualE
	  is y:QQ do
	  if x < y then LessE else if x > y then GreaterE else EqualEqualE
	  is y:RR do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:CC do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
     	  is Error do right
	  else binarymethod(left,right,QuestionS))
     is x:RR do (
	  when right
	  is y:ZZ do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:QQ do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:RR do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:CC do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
     	  is Error do right
	  else binarymethod(left,right,QuestionS))
     is x:CC do (
	  when right
	  is y:ZZ do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:QQ do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:RR do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:CC do (
	       r := compare(x,y);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
     	  is Error do right
	  else binarymethod(left,right,QuestionS))
     is x:Net do (
	  when right
	  is y:Net do (
	       c := netcmp(x,y);
	       if c == 1 then GreaterE
	       else if c == -1 then LessE
     	       else EqualEqualE
	       )
	  is y:string do (
	       c := netcmp(x,y);
	       if c == 1 then GreaterE
	       else if c == -1 then LessE
     	       else EqualEqualE
	       )
     	  is Error do right
	  else binarymethod(left,right,QuestionS))
     is s:Sequence do (
	  when right
	  is t:Sequence do (
	       ls := length(s);
	       lt := length(t);
	       i := 0;
	       while true do (
		    if i == ls 
		    then if i == lt then return EqualEqualE
		    else return LessE
		    else if i == lt then return GreaterE
		    else (
			 c := compare(s.i,t.i);
			 if !(c === EqualEqualS) then return c);
		    i = i+1);
	       nullE)
	  else binarymethod(left,right,QuestionS))
     is Error do left
     else (
	  when right
	  is Error do right
	  else binarymethod(left,right,QuestionS)));
compareop(lhs:Code,rhs:Code):Expr := (
     x := eval(lhs);
     when x
     is Error do x
     else (
	  y := eval(rhs);
	  when y
	  is Error do y
	  else compare(x,y)));
unaryQuestionFun(rhs:Code):Expr := unarymethod(rhs,QuestionS);
setup(QuestionS,unaryQuestionFun,compareop);

whichway := GreaterS;
sortlist := emptySequence;
subsort(l:int,r:int):Expr := (
     b := r+1-l;
     a := randomint() % b;
     if a < 0 then a = a+b;
     p := l + a;
     pivot := sortlist.p;
     sortlist.p = sortlist.l;
     sortlist.l = pivot;
     i := l+1;
     j := r;
     while i <= j do (
	  -- spots 1 .. i-1 contain elements less or equal to the pivot
	  -- spots j+1 .. r contain elements greater or equal to the pivot
	  -- when i > j we've partitioned all the elements into two parts
	  if (
	       c := compare(sortlist.i,pivot);
	       when c is Error do return c else nothing;
	       !(c === whichway)) then i = i+1
	  else if (
	       c := compare(pivot, sortlist.j);
	       when c is Error do return c else nothing;
	       !(c === whichway)) then j = j-1
	  else (
	       tmp := sortlist.i;
	       sortlist.i = sortlist.j;
	       sortlist.j = tmp;
	       i = i+1;
	       j = j-1));
     if l+1 < j then subsort(l+1,j);
     if j+1 < r then subsort(j+1,r);
     for k from l+1 to j do sortlist.(k-1) = sortlist.k;
     sortlist.j = pivot;
     nullE);
basicsort(s:Sequence,ww:SymbolClosure):Expr := (
     if length(s) <= 1 then return Expr(s);
     savesortlist := sortlist;
     savewhichway := whichway;
     sortlist = new Sequence len length(s) do foreach x in s do provide x;
     whichway = ww;
     ret := subsort(0,length(s)-1);
     when ret is Error do nothing else ret = Expr(sortlist);
     whichway = savewhichway;
     sortlist = savesortlist;
     ret);
basicsort2(e:Expr,ww:SymbolClosure):Expr := (
     answer :=
     when e is s:Sequence do (
	  if length(s) <= 1 then e else basicsort(s,ww))
     is t:List do (
	   if ancestor(t.class, listClass) then (
		if length(t.v) <= 1 then e else (
		     r := basicsort(t.v,ww);
		     when r is b:Sequence do list(t.class,b) else r))
      	   else WrongArg("a list or sequence"))
     else WrongArg("a list or sequence");
     answer);
sortfun(e:Expr):Expr := basicsort2(e,GreaterS);
rsortfun(e:Expr):Expr := basicsort2(e,LessS);
setupfun("internalsort",sortfun);
setupfun("internalrsort",rsortfun);

lessfun1(rhs:Code):Expr := unarymethod(rhs,LessS);
lessfun2(lhs:Code,rhs:Code):Expr := (
     e := compareop(lhs,rhs);
     when e 
     is Error do e
     else if LessS.symbol === e then True else False
     );
setup(LessS,lessfun1,lessfun2);

greaterequalfun1(rhs:Code):Expr := unarymethod(rhs,GreaterEqualS);
greaterequalfun2(lhs:Code,rhs:Code):Expr := (
     e := compareop(lhs,rhs);
     when e 
     is Error do e
     else if GreaterS.symbol === e || EqualEqualS.symbol === e then True else False
     );
setup(GreaterEqualS,greaterequalfun1,greaterequalfun2);

greaterfun1(rhs:Code):Expr := unarymethod(rhs,GreaterS);
greaterfun2(lhs:Code,rhs:Code):Expr := (
     e := compareop(lhs,rhs);
     when e 
     is Error do e
     else if GreaterS.symbol === e then True else False
     );
setup(GreaterS,greaterfun1,greaterfun2);

lessequalfun1(rhs:Code):Expr := unarymethod(rhs,LessEqualS);
lessequalfun2(lhs:Code,rhs:Code):Expr := (
     e := compareop(lhs,rhs);
     when e 
     is Error do e
     else if LessS.symbol === e || EqualEqualS.symbol === e then True else False
     );
setup(LessEqualS,lessequalfun1,lessequalfun2);

mergepairs(xx:Expr,yy:Expr,f:Expr):Expr := (
     when xx is xl:List do
     when yy is yl:List do (
	  x := xl.v;
	  y := yl.v;
	  n := 0;
	  z := new Sequence len length(x)+length(y) do provide nullE;
	  i := 0;
	  j := 0;
	  while true do (
	       if i >= length(x) then (
		    while j < length(y) do (z.n = y.j; j = j+1; n = n+1; );
		    break;
		    )
	       else if j >= length(y) then (
		    while i < length(x) do (z.n = x.i; i = i+1; n = n+1; );
		    break;
		    );
	       when x.i
	       is xi:Sequence do 
	       if length(xi) != 2
	       then return WrongArg(1,"a list of pairs")
	       else
	       when y.j
	       is yj:Sequence do
	       if length(yj) != 2
	       then return WrongArg(2,"a list of pairs")
	       else (
		    c := compare(xi.0,yj.0);
		    when c is Error do return c else nothing;
		    if GreaterS.symbol === c then (
			 z.n = yj;
			 j = j+1;
			 n = n+1;
			 )
		    else if LessS.symbol === c then (
			 z.n = xi;
			 i = i+1;
			 n = n+1;
			 )
		    else (
			 z.n = Sequence(xi.0, applyEEE(f,xi.1,yj.1));
			 i = i+1;
			 j = j+1;
			 n = n+1;
			 ))
	       else return WrongArg(2,"a list of pairs")
	       else return WrongArg(1,"a list of pairs"));
	  if n < length(x)+length(y)
	  then z = new Sequence len n do foreach a in z do provide a;
	  Expr(sethash(List(commonAncestor(xl.class,yl.class), z,0,false),xl.mutable | yl.mutable)))
     else WrongArg(2,"a list")
     else WrongArg(1,"a list"));
mergepairsfun(e:Expr):Expr := (
     when e
     is a:Sequence do
     if length(a) == 3 then
     mergepairs(a.0,a.1,a.2)
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("mergePairs",mergepairsfun);

--rmergepairs(xx:Expr,yy:Expr,f:Expr):Expr := (
--     when xx is xl:List do
--     when yy is yl:List do (
--	  x := xl.v;
--	  y := yl.v;
--	  n := 0;
--	  z := new Sequence len length(x)+length(y) do provide nullE;
--	  i := 0;
--	  j := 0;
--	  while true do (
--	       if i >= length(x) then (
--		    while j < length(y) do (z.n = y.j; j = j+1; n = n+1; );
--		    break;
--		    )
--	       else if j >= length(y) then (
--		    while i < length(x) do (z.n = x.i; i = i+1; n = n+1; );
--		    break;
--		    );
--	       when x.i
--	       is xi:Sequence do 
--	       if length(xi) != 2
--	       then return WrongArg(1,"a list of pairs")
--	       else
--	       when y.j
--	       is yj:Sequence do
--	       if length(yj) != 2
--	       then return WrongArg(2,"a list of pairs")
--	       else (
--		    c := compare(xi.0,yj.0);
--		    when c is Error do return c else nothing;
--		    if LessS.symbol === c then (
--			 z.n = yj;
--			 j = j+1;
--			 n = n+1;
--			 )
--		    else if GreaterS.symbol === c then (
--			 z.n = xi;
--			 i = i+1;
--			 n = n+1;
--			 )
--		    else (
--			 z.n = Sequence(xi.0, apply(f,xi.1,yj.1));
--			 i = i+1;
--			 j = j+1;
--			 n = n+1;
--			 ))
--	       else return WrongArg(2,"a list of pairs")
--	       else return WrongArg(1,"a list of pairs"));
--	  if n < length(x)+length(y)
--	  then z = new Sequence len n do foreach a in z do provide a;
--	  Expr(sethash(List(commonAncestor(xl.class,yl.class), z,0,false),xl.mutable | yl.mutable)))
--     else WrongArg(2,"a list")
--     else WrongArg(1,"a list"));
--rmergepairsfun(e:Expr):Expr := (
--     when e
--     is a:Sequence do
--     if length(a) == 3 then
--     rmergepairs(a.0,a.1,a.2)
--     else WrongNumArgs(3)
--     else WrongNumArgs(3));
--setupfun("rmergepairs",rmergepairsfun);
bitxorfun(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a) == 2 then
     when a.0
     is x:ZZ do
     when a.1
     is y:ZZ do Expr(x ^^ y)
     else WrongArgZZ(2)
     else WrongArgZZ(1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("xor",bitxorfun);
semicolonfun(lhs:Code,rhs:Code):Expr := when eval(lhs) is err:Error do Expr(err) else eval(rhs);
setup(SemicolonS,semicolonfun);
starfun(rhs:Code):Expr := unarymethod(rhs,StarS);
timesfun(lhs:Code,rhs:Code):Expr := (
     l := eval(lhs);
     when l is Error do l
     else (
     	  r := eval(rhs);
     	  when r is Error do r
	  else l*r));
setup(StarS,starfun,timesfun);

-- functions

sin(e:Expr):Expr := (
     when e
     is x:RR do Expr(sin(x))				    -- # typical value: sin, RR, RR
     is x:ZZ do Expr(sin(toRR(x)))			    -- # typical value: sin, ZZ, RR
     is x:QQ do Expr(sin(toRR(x)))			    -- # typical value: sin, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("sin",sin);
cos(e:Expr):Expr := (
     when e
     is x:RR do Expr(cos(x))				    -- # typical value: cos, RR, RR
     is x:ZZ do Expr(cos(toRR(x)))			    -- # typical value: cos, ZZ, RR
     is x:QQ do Expr(cos(toRR(x)))			    -- # typical value: cos, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("cos",cos);
tan(e:Expr):Expr := (
     when e
     is x:RR do Expr(tan(x))				    -- # typical value: tan, RR, RR
     is x:ZZ do Expr(tan(toRR(x)))			    -- # typical value: tan, ZZ, RR
     is x:QQ do Expr(tan(toRR(x)))			    -- # typical value: tan, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("tan",tan);
acos(e:Expr):Expr := (
     when e
     is x:RR do Expr(acos(x))				    -- # typical value: acos, RR, RR
     is x:ZZ do Expr(acos(toRR(x)))			    -- # typical value: acos, ZZ, RR
     is x:QQ do Expr(acos(toRR(x)))			    -- # typical value: acos, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("acos",acos);
sec(e:Expr):Expr := (
     when e
     is x:RR do Expr(sec(x))				    -- # typical value: sec, RR, RR
     is x:ZZ do Expr(sec(toRR(x)))			    -- # typical value: sec, ZZ, RR
     is x:QQ do Expr(sec(toRR(x)))			    -- # typical value: sec, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("sec",sec);
csc(e:Expr):Expr := (
     when e
     is x:RR do Expr(csc(x))				    -- # typical value: csc, RR, RR
     is x:ZZ do Expr(csc(toRR(x)))			    -- # typical value: csc, ZZ, RR
     is x:QQ do Expr(csc(toRR(x)))			    -- # typical value: csc, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("csc",csc);
cot(e:Expr):Expr := (
     when e
     is x:RR do Expr(cot(x))				    -- # typical value: cot, RR, RR
     is x:ZZ do Expr(cot(toRR(x)))			    -- # typical value: cot, ZZ, RR
     is x:QQ do Expr(cot(toRR(x)))			    -- # typical value: cot, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("cot",cot);
sech(e:Expr):Expr := (
     when e
     is x:RR do Expr(sech(x))				    -- # typical value: sech, RR, RR
     is x:ZZ do Expr(sech(toRR(x)))			    -- # typical value: sech, ZZ, RR
     is x:QQ do Expr(sech(toRR(x)))			    -- # typical value: sech, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("sech",sech);
csch(e:Expr):Expr := (
     when e
     is x:RR do Expr(csch(x))				    -- # typical value: csch, RR, RR
     is x:ZZ do Expr(csch(toRR(x)))			    -- # typical value: csch, ZZ, RR
     is x:QQ do Expr(csch(toRR(x)))			    -- # typical value: csch, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("csch",csch);
coth(e:Expr):Expr := (
     when e
     is x:RR do Expr(coth(x))				    -- # typical value: coth, RR, RR
     is x:ZZ do Expr(coth(toRR(x)))			    -- # typical value: coth, ZZ, RR
     is x:QQ do Expr(coth(toRR(x)))			    -- # typical value: coth, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("coth",coth);
asin(e:Expr):Expr := (
     when e
     is x:RR do Expr(asin(x))				    -- # typical value: asin, RR, RR
     is x:ZZ do Expr(asin(toRR(x)))			    -- # typical value: asin, ZZ, RR
     is x:QQ do Expr(asin(toRR(x)))			    -- # typical value: asin, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("asin",asin);
log1p(e:Expr):Expr := (
     when e
     is x:RR do Expr(log1p(x))				    -- # typical value: log1p, RR, RR
     is x:ZZ do Expr(log1p(toRR(x)))			    -- # typical value: log1p, ZZ, RR
     is x:QQ do Expr(log1p(toRR(x)))			    -- # typical value: log1p, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("log1p",log1p);
expm1(e:Expr):Expr := (
     when e
     is x:RR do Expr(expm1(x))				    -- # typical value: expm1, RR, RR
     is x:ZZ do Expr(expm1(toRR(x)))			    -- # typical value: expm1, ZZ, RR
     is x:QQ do Expr(expm1(toRR(x)))			    -- # typical value: expm1, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("expm1",expm1);
eint(e:Expr):Expr := (
     when e
     is x:RR do Expr(eint(x))				    -- # typical value: eint, RR, RR
     is x:ZZ do Expr(eint(toRR(x)))			    -- # typical value: eint, ZZ, RR
     is x:QQ do Expr(eint(toRR(x)))			    -- # typical value: eint, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("eint",eint);
Gamma(e:Expr):Expr := (
     when e
     is x:RR do Expr(Gamma(x))				    -- # typical value: Gamma, RR, RR
     is x:ZZ do Expr(Gamma(toRR(x)))			    -- # typical value: Gamma, ZZ, RR
     is x:QQ do Expr(Gamma(toRR(x)))			    -- # typical value: Gamma, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("Gamma",Gamma);
--lngamma(e:Expr):Expr := (
--     when e
--     is x:RR do Expr(lngamma(x))			    -- typical value: lngamma, RR, RR
--     is x:ZZ do Expr(lngamma(toRR(x)))			    -- typical value: lngamma, ZZ, RR
--     is x:QQ do Expr(lngamma(toRR(x)))			    -- typical value: lngamma, QQ, RR
--     else buildErrorPacket("expected a number")
--     );
--setupfun("lngamma",lngamma);
export lgamma(x:RR):Expr := (
     z := newRR(precision(x));
     i := 0;
     Ccode( void, "mpfr_lgamma((__mpfr_struct *)", z, ",&",i,",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     Expr(Sequence(z,toInteger(i))));
lgamma(e:Expr):Expr := (
     when e
     is x:RR do Expr(lgamma(x))				    -- # typical value: lgamma, RR, RR
     is x:ZZ do Expr(lgamma(toRR(x)))			    -- # typical value: lgamma, ZZ, RR
     is x:QQ do Expr(lgamma(toRR(x)))			    -- # typical value: lgamma, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("lgamma",lgamma);
zeta(e:Expr):Expr := (
     when e
     is x:RR do Expr(zeta(x))				    -- # typical value: zeta, RR, RR
     is x:ZZ do (					    -- # typical value: zeta, ZZ, RR
	  if isULong(x)
	  then Expr(zeta(toULong(x),defaultPrecision))
	  else Expr(zeta(toRR(x,defaultPrecision)))
	  )
     is x:QQ do Expr(zeta(toRR(x)))			    -- # typical value: zeta, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("zeta",zeta);
erf(e:Expr):Expr := (
     when e
     is x:RR do Expr(erf(x))				    -- # typical value: erf, RR, RR
     is x:ZZ do Expr(erf(toRR(x)))			    -- # typical value: erf, ZZ, RR
     is x:QQ do Expr(erf(toRR(x)))			    -- # typical value: erf, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("erf",erf);
erfc(e:Expr):Expr := (
     when e
     is x:RR do Expr(erfc(x))				    -- # typical value: erfc, RR, RR
     is x:ZZ do Expr(erfc(toRR(x)))			    -- # typical value: erfc, ZZ, RR
     is x:QQ do Expr(erfc(toRR(x)))			    -- # typical value: erfc, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("erfc",erfc);
BesselJ(n:long,x:RR):RR := (
     if n == long(0) then j0(x)
     else if n == long(1) then j1(x)
     else jn(n,x));
BesselJ(e:Expr):Expr := (
     when e is s:Sequence do (
	  when s.0 is n:ZZ do if !isLong(n) then WrongArg(1,"a small integer") else (
	       when s.1 
	       is x:RR do Expr(BesselJ(toLong(n),x))	    -- # typical value: BesselJ, ZZ, RR, RR
	       is x:ZZ do Expr(BesselJ(toLong(n),toRR(x)))  -- # typical value: BesselJ, ZZ, ZZ, RR
	       is x:QQ do Expr(BesselJ(toLong(n),toRR(x)))  -- # typical value: BesselJ, ZZ, QQ, RR
	       else WrongArg(2,"a number"))
	  else WrongArgZZ(1))
     else WrongNumArgs(2));
setupfun("BesselJ",BesselJ);
BesselY(n:long,x:RR):RR := (
     if n == long(0) then y0(x)
     else if n == long(1) then y1(x)
     else yn(n,x));
BesselY(e:Expr):Expr := (
     when e is s:Sequence do (
	  when s.0 is n:ZZ do if !isLong(n) then WrongArg(1,"a small integer") else (
	       when s.1 
	       is x:RR do Expr(BesselY(toLong(n),x))	    -- # typical value: BesselY, ZZ, RR, RR
	       is x:ZZ do Expr(BesselY(toLong(n),toRR(x)))  -- # typical value: BesselY, ZZ, ZZ, RR
	       is x:QQ do Expr(BesselY(toLong(n),toRR(x)))  -- # typical value: BesselY, ZZ, QQ, RR
	       else WrongArg(2,"a number"))
	  else WrongArgZZ(1))
     else WrongNumArgs(2));
setupfun("BesselY",BesselY);
atan(e:Expr):Expr := (
     when e
     is x:RR do Expr(atan(x))				    -- # typical value: atan, RR, RR
     is x:ZZ do Expr(atan(toRR(x)))			    -- # typical value: atan, ZZ, RR
     is x:QQ do Expr(atan(toRR(x)))	       -- # typical value: atan, QQ, RR
     is a:Sequence do if length(a) != 2
     then WrongNumArgs(1,2)
     else
     when a.1
     is y:RR do (
	  when a.0
	  is x:RR do Expr(atan2(y,x))			            -- # typical value: atan, RR, RR, RR
	  is x:ZZ do Expr(atan2(y,toRR(x,precision(y))))	    -- # typical value: atan, ZZ, RR, RR
	  is x:QQ do Expr(atan2(y,toRR(x,precision(y))))	    -- # typical value: atan, QQ, RR, RR
	  else WrongArg(1,"a number"))
     is y:ZZ do (
	  when a.0
	  is x:RR do Expr(atan2(toRR(y,precision(x)),x))    -- -- # typical value: atan, RR, ZZ, RR
	  is x:ZZ do Expr(atan2(toRR(y),toRR(x)))	       -- # typical value: atan, ZZ, ZZ, RR
	  is x:QQ do Expr(atan2(toRR(y),toRR(x)))	       -- # typical value: atan, QQ, ZZ, RR
	  else WrongArg(1,"a number"))
     is y:QQ do (
	  when a.0
	  is x:RR do Expr(atan2(toRR(y,precision(x)),x))    -- # typical value: atan, RR, QQ, RR
	  is x:ZZ do Expr(atan2(toRR(y),toRR(x)))	    -- # typical value: atan, ZZ, QQ, RR
	  is x:QQ do Expr(atan2(toRR(y),toRR(x)))	    -- # typical value: atan, QQ, QQ, RR
     	  else WrongArg(1,"a number"))
     else WrongArg(2,"a number")
     else buildErrorPacket("expected a number or a pair of numbers")
     );
setupfun("atan",atan);
cosh(e:Expr):Expr := (
     when e
     is x:RR do Expr(cosh(x))				    -- # typical value: cosh, RR, RR
     is x:ZZ do Expr(cosh(toRR(x)))			    -- # typical value: cosh, ZZ, RR
     is x:QQ do Expr(cosh(toRR(x)))			    -- # typical value: cosh, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("cosh",cosh);
sinh(e:Expr):Expr := (
     when e
     is x:RR do Expr(sinh(x))				    -- # typical value: sinh, RR, RR
     is x:ZZ do Expr(sinh(toRR(x)))			    -- # typical value: sinh, ZZ, RR
     is x:QQ do Expr(sinh(toRR(x)))			    -- # typical value: sinh, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("sinh",sinh);
tanh(e:Expr):Expr := (
     when e
     is x:RR do Expr(tanh(x))				    -- # typical value: tanh, RR, RR
     is x:ZZ do Expr(tanh(toRR(x)))			    -- # typical value: tanh, ZZ, RR
     is x:QQ do Expr(tanh(toRR(x)))			    -- # typical value: tanh, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("tanh",tanh);
exp(e:Expr):Expr := (
     when e
     is x:CC do Expr(exp(x))			-- # typical value: exp, CC, CC
     is x:RR do Expr(exp(x))			-- # typical value: exp, RR, RR
     is x:ZZ do Expr(exp(toRR(x)))			    -- # typical value: exp, ZZ, RR
     is x:QQ do Expr(exp(toRR(x)))			    -- # typical value: exp, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("exp",exp);
log(e:Expr):Expr := (
     when e
     is a:Sequence do if length(a) != 2 then WrongNumArgs(1,2) 
     else (
	  when a.0
	  is b:RR do (
	       when a.1
	       is x:CC do Expr(log(b,x))			            -- # typical value: log, RR, CC, CC
	       is x:RR do (			            -- # typical value: log, RR, RR, CC
     	       	    if b>0 && x>0 then Expr(log(b,x)) else Expr(logc(b,x))
		    )
	       is x:ZZ do (	    -- # typical value: log, RR, ZZ, RR
		    y := toRR(x,precision(b));
		    if b < 0 || y < 0 then Expr(logc(b,y)) else Expr(log(b,y)))
	       is x:QQ do (	    -- # typical value: log, RR, QQ, RR
		    y := toRR(x,precision(b));
		    if b < 0 || y < 0 then Expr(logc(b,y)) else Expr(log(b,y)))
	       else WrongArg(1,"a number"))
	  is b:ZZ do (
	       when a.1
	       is x:CC do ( -- # typical value: log, ZZ, CC, CC
		    Expr(log(toRR(b,precision(x)),x))
		    )
	       is x:RR do (    -- -- # typical value: log, ZZ, RR, RR
		    c := toRR(b,precision(x));
		    if c>0 && x>0 then Expr(log(c,x)) else Expr(logc(c,x))		    
		    )
	       is x:ZZ do (	       -- # typical value: log, ZZ, ZZ, RR
		    if b>0 && x>0 then Expr(log(toRR(b),toRR(x))) else Expr(logc(toRR(b),toRR(x)))
		    )
	       is x:QQ do (	       -- # typical value: log, ZZ, QQ, RR
		    if b>0 && x>0 then Expr(log(toRR(b),toRR(x))) else Expr(logc(toRR(b),toRR(x)))
		    )
	       else WrongArg(1,"a number"))
	  is b:QQ do (
	       when a.1
	       is x:CC do ( -- # typical value: log, QQ, CC, CC
		    Expr(log(toRR(b,precision(x)),x))
		    )
	       is x:RR do (    -- -- # typical value: log, QQ, RR, RR
		    c := toRR(b,precision(x));
		    if c>0 && x>0 then Expr(log(c,x)) else Expr(logc(c,x))		    
		    )
	       is x:ZZ do (	       -- # typical value: log, QQ, ZZ, RR
		    if b>0 && x>0 then Expr(log(toRR(b),toRR(x))) else Expr(logc(toRR(b),toRR(x)))
		    )
	       is x:QQ do (	       -- # typical value: log, QQ, QQ, RR
		    if b>0 && x>0 then Expr(log(toRR(b),toRR(x))) else Expr(logc(toRR(b),toRR(x)))
		    )
	       else WrongArg(1,"a number"))
	  else WrongArg(2,"a number"))
     is x:CC do Expr(log(x))				    -- # typical value: log, CC, CC
     is x:RR do if x<0 then Expr(logc(x)) else Expr(log(x))				    -- # typical value: log, RR, RR
     is x:ZZ do if x<0 then Expr(logc(toRR(x))) else Expr(log(toRR(x)))			    -- # typical value: log, ZZ, RR
     is x:QQ do if x<0 then Expr(logc(toRR(x))) else Expr(log(toRR(x)))			    -- # typical value: log, QQ, RR
     else WrongArg("a number or a pair of numbers")
     );
setupfun("log",log);
agm(e:Expr):Expr := (
     when e
     is a:Sequence do if length(a) != 2 then WrongNumArgs(2) 
     else (
	  when a.0
	  is x:RR do (
	       when a.1
	       is y:RR do Expr(agm(x,y))			            -- # typical value: agm, RR, RR, RR
	       is y:ZZ do Expr(agm(x,toRR(y,precision(x))))	    -- # typical value: agm, RR, ZZ, RR
	       is y:QQ do Expr(agm(x,toRR(y,precision(x))))	    -- # typical value: agm, RR, QQ, RR
	       else WrongArg(1,"a number"))
	  is x:ZZ do (
	       when a.1
	       is y:RR do Expr(agm(toRR(x,precision(y)),y))    -- -- # typical value: agm, ZZ, RR, RR
	       is y:ZZ do Expr(agm(toRR(x),toRR(y)))	       -- # typical value: agm, ZZ, ZZ, RR
	       is y:QQ do Expr(agm(toRR(x),toRR(y)))	       -- # typical value: agm, ZZ, QQ, RR
	       else WrongArg(1,"a number"))
	  is x:QQ do (
	       when a.1
	       is y:RR do Expr(agm(toRR(x,precision(y)),y))    -- # typical value: agm, QQ, RR, RR
	       is y:ZZ do Expr(agm(toRR(x),toRR(y)))	    -- # typical value: agm, QQ, ZZ, RR
	       is y:QQ do Expr(agm(toRR(x),toRR(y)))	    -- # typical value: agm, QQ, QQ, RR
	       else WrongArg(1,"a number"))
	  else WrongArg(2,"a number"))
     else WrongNumArgs(2)
     );
setupfun("agm",agm);
abs(x:double):double := if x < 0. then -x else x;
floor(e:Expr):Expr := (
     when e
     is x:RR do Expr(floor(x))				    -- # typical value: floor, RR, ZZ
     is x:QQ do Expr(floor(x))				    -- # typical value: floor, QQ, ZZ
     is ZZ do e				    -- # typical value: floor, ZZ, ZZ
     else buildErrorPacket("expected an integral, rational, or real number")
     );
setupfun("floor",floor);

round0(e:Expr):Expr := (
     when e
     is x:RR do Expr(round(x))
     else buildErrorPacket("expected a real number")
     );
setupfun("round0",round0);

run(e:Expr):Expr := (
     when e
     is x:string do Expr(toInteger(run(x)))
     else buildErrorPacket("expected a string")
     );
setupfun("run",run);

sqrt(a:Expr):Expr := (
     when a
     is x:ZZ do Expr(sqrt(toRR(x)))	       -- # typical value: sqrt, ZZ, RR
     is x:QQ do Expr(sqrt(toRR(x)))	       -- # typical value: sqrt, QQ, RR
     is x:RR do Expr(sqrt(x))				    -- # typical value: sqrt, RR, RR
     is x:CC do Expr(sqrt(x))				    -- # typical value: sqrt, CC, CC
     is Error do a
     else WrongArgRR());
setupfun("sqrt",sqrt);
map(a1:Sequence,a2:Sequence,f:Expr):Expr := (
     newlen := length(a1);
     if newlen != length(a2) then return WrongArg("lists of the same length");
     if newlen == 0 then return emptySequenceE;
     haderror := false;
     if recursionDepth > recursionLimit then return RecursionLimit();
     errret := nullE;
     when f is fc:FunctionClosure do (
	  previousFrame := fc.frame;
	  model := fc.model;
	  desc := model.desc;
	  body := model.body;
	  frameID := desc.frameID;
	  numparms := desc.numparms;
	  framesize := desc.framesize;
	  -- since the function closure has no code inside it that makes
	  -- a closure, we can re-use its frame.
	  if desc.restargs then (	  -- x -> ...
	       saveLocalFrame := localFrame;
	       values := new Sequence len framesize do provide nullE;
	       localFrame = Frame(previousFrame,frameID,framesize,false,values);
	       ret := new Sequence len newlen do (
		    for i from 0 to newlen-1 do (
			 values.0 = Sequence(a1.i,a2.i);
     			 recursionDepth = recursionDepth + 1;
			 tmp := eval(body);
     			 recursionDepth = recursionDepth - 1;
			 when tmp is err:Error do (
			      if err.message == returnMessage then provide err.value
			      -- else if err.message == continueMessageWithArg then provide err.value
			      -- else if err.message == continueMessage then newlen = newlen - 1
			      else (
				   errret = tmp;
				   while true do provide nullE;
				   )
			      )
			 else provide tmp;
			 if localFrame.notrecyclable then (
			      values = new Sequence len framesize do provide nullE;
			      localFrame = Frame(previousFrame,frameID,framesize,false,values);
			      )
			 else for i from 1 to framesize - 1 do values.i = nullE;
			 );
		    -- while true do provide nullE;
		    );
	       localFrame = saveLocalFrame;
	       if errret != nullE then return errret;
	       -- if newlen < length(ret) then ret = new Sequence len newlen do foreach x in ret do provide x;
	       Expr(ret)
	       )
	  else (				  -- (x,y) -> ...
	       if numparms != 2 then WrongNumArgs(model.arrow,numparms,2)
	       else (
		    saveLocalFrame := localFrame;
		    values := new Sequence len framesize do provide nullE;
		    localFrame = Frame(previousFrame,frameID,framesize,false,values);
		    ret := new Sequence len newlen do (
			 for i from 0 to newlen - 1 do (
			      values.0 = a1.i;
			      values.1 = a2.i;
     			      recursionDepth = recursionDepth + 1;
			      tmp := eval(body);
     			      recursionDepth = recursionDepth - 1;
			      when tmp is err:Error do (
				   if err.message == returnMessage then provide err.value
				   -- else if err.message == continueMessageWithArg then provide err.value
				   -- else if err.message == continueMessage then newlen = newlen - 1
				   else (
					errret = tmp;
					while true do provide nullE;
					)
				   )
			      else provide tmp;
			      if localFrame.notrecyclable then (
				   values = new Sequence len framesize do provide nullE;
				   localFrame = Frame(previousFrame,frameID,framesize,false,values);
				   )
			      else (
				   -- it would be faster to do a byte copy here:
				   for i from 2 to framesize - 1 do values.i = nullE;
				   );
			      );
			 -- while true do provide nullE;
			 );
		    localFrame = saveLocalFrame;
		    if errret != nullE then return errret;
	       	    -- if newlen < length(ret) then ret = new Sequence len newlen do foreach x in ret do provide x;
		    Expr(ret)
		    )
	       )
	  )
     is cf:CompiledFunction do (	  -- compiled code
	  fn := cf.fn;
	  ret := new Sequence len newlen at i do (
	       recursionDepth = recursionDepth + 1;
	       tmp := fn(Expr(Sequence(a1.i,a2.i)));
	       recursionDepth = recursionDepth - 1;
	       when tmp is Error do (
		    errret = tmp;
		    while true do provide nullE; )
	       else provide tmp;
	       );
	  if errret != nullE then errret else Expr(ret)
	  )
     is cf:CompiledFunctionClosure do (	  -- compiled code closure
	  fn := cf.fn;
	  env := cf.env;
	  ret := new Sequence len newlen at i do (
	       recursionDepth = recursionDepth + 1;
	       tmp := fn(Expr(Sequence(a1.i,a2.i)),env);
	       recursionDepth = recursionDepth - 1;
	       when tmp is Error do (
		    errret = tmp;
		    while true do provide nullE; )
	       else provide tmp;
	       );
	  if errret != nullE then errret else Expr(ret)
	  )
     is s:SpecialExpr do map(a1,a2,s.e)
     else WrongArg(2,"a function"));
map(a:Sequence,f:Expr):Expr := (
     newlen := length(a);
     if newlen == 0 then return emptySequenceE;
     haderror := false;
     recursionDepth = recursionDepth + 1;
     if recursionDepth > recursionLimit then return RecursionLimit();
     errret := nullE;
     when f is fc:FunctionClosure do (
	  previousFrame := fc.frame;
	  model := fc.model;
	  desc := model.desc;
	  body := model.body;
	  frameID := desc.frameID;
	  numparms := desc.numparms;
	  framesize := desc.framesize;
	  if desc.restargs then (	  -- x -> ...
	       saveLocalFrame := localFrame;
	       values := new Sequence len framesize do provide nullE;
	       localFrame = Frame(previousFrame,frameID,framesize,false,values);
	       ret := new Sequence len newlen do (
		    foreach arg in a do (
			 values.0 = arg;
			 tmp := eval(body);
			 when tmp is err:Error do (
			      if err.message == returnMessage then provide err.value
			      -- else if err.message == continueMessageWithArg then provide err.value
			      -- else if err.message == continueMessage then newlen = newlen - 1
			      else (
				   errret = tmp;
				   while true do provide nullE;
				   )
			      )
			 else provide tmp;
		    	 if localFrame.notrecyclable then (
			      values = new Sequence len framesize do provide nullE;
			      localFrame = Frame(previousFrame,frameID,framesize,false,values);
			      )
			 else for i from 1 to framesize - 1 do values.i = nullE;
			 );
		    -- while true do provide nullE;
		    );
	       localFrame = saveLocalFrame;
	       recursionDepth = recursionDepth - 1;
	       if errret != nullE then return errret;
	       -- if newlen < length(ret) then ret = new Sequence len newlen do foreach x in ret do provide x;
	       Expr(ret)
	       )
	  else (				  -- (x,y) -> ...
	       if numparms == 1 then (
		    saveLocalFrame := localFrame;
		    values := new Sequence len framesize do provide nullE;
		    localFrame = Frame(previousFrame,frameID,framesize,false,values);
		    ret := new Sequence len newlen do (
			 if framesize == 1 then (
			      foreach arg in a do (
				   when arg is args:Sequence do (
					if 1 == length(args) then values.0 = args.0
					else (
					     errret = WrongNumArgs(model.arrow,numparms,length(args));
					     while true do provide nullE;
					     )
					)
				   else values.0 = arg;
				   tmp := eval(body);
				   when tmp is err:Error do (
					if err.message == returnMessage then provide err.value
			      		-- else if err.message == continueMessageWithArg then provide err.value
			      		-- else if err.message == continueMessage then newlen = newlen - 1
					else (
					     errret = tmp;
					     while true do provide nullE;
					     )
					)
				   else provide tmp;
		    		   if localFrame.notrecyclable then (
					values = new Sequence len framesize do provide nullE;
					localFrame = Frame(previousFrame,frameID,framesize,false,values);
					);
				   )
			      )
			 else (
			      foreach arg in a do (
				   when arg is args:Sequence do (
					if 1 == length(args) then values.0 = args.0
					else (
					     errret = WrongNumArgs(model.arrow,numparms,length(args));
					     while true do provide nullE;
					     )
					)
				   else values.0 = arg;
				   tmp := eval(body);
				   when tmp is err:Error do (
					if err.message == returnMessage then provide err.value
			      		-- else if err.message == continueMessageWithArg then provide err.value
			      		-- else if err.message == continueMessage then newlen = newlen - 1
					else (
					     errret = tmp;
					     while true do provide nullE;
					     )
					)
				   else provide tmp;
		    		   if localFrame.notrecyclable then (
					values = new Sequence len framesize do provide nullE;
					localFrame = Frame(previousFrame,frameID,framesize,false,values);
					)
				   else for i from 1 to framesize - 1 do values.i = nullE;
				   )
			      );
			 -- while true do provide nullE;
			 );
		    localFrame = saveLocalFrame;
		    recursionDepth = recursionDepth - 1;
		    if errret != nullE then return errret;
	       	    -- if newlen < length(ret) then ret = new Sequence len newlen do foreach x in ret do provide x;
		    Expr(ret)
		    )
	       else (
		    if framesize == 0 then (
			 saveLocalFrame := localFrame;
			 localFrame = previousFrame;
			 ret := new Sequence len newlen do (
			      foreach arg in a do (
				   when arg is args:Sequence do (
					if 0 != length(args) then (
					     errret = WrongNumArgs(model.arrow,0,length(args));
					     while true do provide nullE;
					     )
					)
				   else (
					errret = WrongNumArgs(model.arrow,numparms,1);
					while true do provide nullE;
					);
				   tmp := eval(body);
				   when tmp is err:Error do (
					if err.message == returnMessage then provide err.value
			      		-- else if err.message == continueMessageWithArg then provide err.value
			      		-- else if err.message == continueMessage then newlen = newlen - 1
					else (
					     errret = tmp;
					     while true do provide nullE;
					     )
					)
				   else provide tmp;
				   );
			      -- while true do provide nullE;
			      );
			 localFrame = saveLocalFrame;
			 recursionDepth = recursionDepth - 1;
			 if errret != nullE then return errret;
	       	    	 -- if newlen < length(ret) then ret = new Sequence len newlen do foreach x in ret do provide x;
			 Expr(ret)
			 )
		    else (	  -- framesize != 0
			 saveLocalFrame := localFrame;
			 values := new Sequence len framesize do provide nullE;
			 localFrame = Frame(previousFrame,frameID,framesize,false,values);
			 ret := new Sequence len newlen do (
			      foreach arg in a do (
				   when arg is args:Sequence do (
					if numparms == length(args) then (
					     foreach x at i in args do values.i = x;
					     )
					else (
					     errret=WrongNumArgs(model.arrow,numparms,length(args));
					     while true do provide nullE;
					     )
					)
				   else (
					errret = WrongNumArgs(model.arrow,numparms,1);
					while true do provide nullE;
					);
				   tmp := eval(body);
				   when tmp is err:Error do (
					if err.message == returnMessage then provide err.value
			      		-- else if err.message == continueMessageWithArg then provide err.value
			      		-- else if err.message == continueMessage then newlen = newlen - 1
					else (
					     errret = tmp;
					     while true do provide nullE;
					     )
					)
				   else provide tmp;
		    		   if localFrame.notrecyclable then (
					values = new Sequence len framesize do provide nullE;
					localFrame = Frame(previousFrame,frameID,framesize,false,values);
     	       	    	      	   	)
				   else for i from numparms to framesize - 1 do values.i = nullE;
				   );
			      -- while true do provide nullE;
			      );
			 localFrame = saveLocalFrame;
			 recursionDepth = recursionDepth - 1;
    			 if errret != nullE then return errret;
	       	    	 -- if newlen < length(ret) then ret = new Sequence len newlen do foreach x in ret do provide x;
			 Expr(ret)))))
     is cf:CompiledFunction do (	  -- compiled code
	  fn := cf.fn;
	  ret := new Sequence len newlen do (
	       foreach arg in a do (
		    tmp := fn(arg);
		    when tmp is Error do (
			 errret = tmp;
			 while true do provide nullE; )
		    else provide tmp; );
	       while true do provide nullE;
	       );
	  recursionDepth = recursionDepth - 1;
	  if errret != nullE then errret else Expr(ret)
	  )
     is cf:CompiledFunctionClosure do (	  -- compiled code closure
	  fn := cf.fn;
	  env := cf.env;
	  ret := new Sequence len newlen do (
	       foreach arg in a do (
		    tmp := fn(arg,env);
		    when tmp is Error do (
			 errret = tmp;
			 while true do provide nullE; )
		    else provide tmp; ));
	  recursionDepth = recursionDepth - 1;
	  if errret != nullE then errret else Expr(ret)
	  )
     is s:SpecialExpr do (
	  ret := map(a,s.e);
	  recursionDepth = recursionDepth - 1;
	  ret)
     else WrongArg(2,"a function")
     );
map(newlen:int,f:Expr):Expr := (
     if newlen <= 0 then return emptyList;
     haderror := false;
     errret := nullE;
     recursionDepth = recursionDepth + 1;
     saveLocalFrame := localFrame;
     ret := new Sequence len newlen do (
	  if recursionDepth > recursionLimit then (
	       errret = RecursionLimit();
	       while true do provide nullE; )
	  else when f is fc:FunctionClosure do (
	       previousFrame := fc.frame;
	       model := fc.model;
	       desc := model.desc;
	       body := model.body;
	       frameID := desc.frameID;
	       numparms := desc.numparms;
	       framesize := desc.framesize;
	       if numparms != 1 then (
		    errret = WrongNumArgs(model.arrow,numparms,1);
		    while true do provide nullE;
		    )
	       else (
		    values := new Sequence len framesize do provide nullE;
		    localFrame = Frame(previousFrame,frameID,framesize,false,values);
		    if framesize == 1 then (
			 for i from 0 to newlen-1 do (
			      values.0 = toInteger(i);
			      tmp := eval(body);
			      when tmp is err:Error do (
				   if err.message == returnMessage then provide err.value
				   -- else if err.message == continueMessageWithArg then provide err.value
				   -- else if err.message == continueMessage then newlen = newlen - 1
				   else (
					errret = tmp;
					while true do provide nullE;
					)
				   )
			      else provide tmp;
		    	      if localFrame.notrecyclable then (
				   values = new Sequence len framesize do provide nullE;
				   localFrame = Frame(previousFrame,frameID,framesize,false,values);
				   );				   
			      )
			 )
		    else (
			 for i from 0 to newlen-1 do (
			      values.0 = toInteger(i);
			      tmp := eval(body);
			      when tmp is err:Error do (
				   if err.message == returnMessage then provide err.value
				   -- else if err.message == continueMessageWithArg then provide err.value
				   -- else if err.message == continueMessage then newlen = newlen - 1
				   else (
					errret = tmp;
					while true do provide nullE;
					)
				   )
			      else provide tmp;
		    	      if localFrame.notrecyclable then (
				   values = new Sequence len framesize do provide nullE;
				   localFrame = Frame(previousFrame,frameID,framesize,false,values);
				   )
			      else for i from 1 to framesize - 1 do values.i = nullE;
			      )
			 )
		    )
	       )
	  is cf:CompiledFunction do (	  -- compiled code
	       fn := cf.fn;
	       for i from 0 to newlen-1 do (
		    tmp := fn(Expr(toInteger(i)));
		    when tmp is Error do (
			 errret = tmp;
			 while true do provide nullE; )
		    else provide tmp;))
	  is cf:CompiledFunctionClosure do (	  -- compiled code closure
	       fn := cf.fn;
	       env := cf.env;
	       for i from 0 to newlen-1 do (
		    tmp := fn(Expr(toInteger(i)),env);
		    when tmp is Error do (
			 errret = tmp;
			 while true do provide nullE; )
		    else provide tmp;))
	  is s:SpecialExpr do (
	       fn := s.e;
	       for i from 0 to newlen-1 do (
		    tmp := applyEE(fn,Expr(toInteger(i)));
		    when tmp is Error do (
			 errret = tmp;
			 while true do provide nullE; )
		    else provide tmp;))
	  else errret = WrongArg(2,"a function");
	  while true do provide nullE;
	  );
     localFrame = saveLocalFrame;
     recursionDepth = recursionDepth - 1;
     when errret
     is err:Error do if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else errret
     else (
	  -- if newlen < length(ret) then ret = new Sequence len newlen do foreach x in ret do provide x;
	  list(ret)
	  ));

map(e:Expr,f:Expr):Expr := (
     when e
     is a:Sequence do (
	  b := map(a,f);
	  when b
	  is err:Error do if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else b
	  else b
	  )
--     is obj:HashTable do (
--	  if obj.mutable then return WrongArg("an immutable hash table");
--	  if ancestor(obj.class,Tally) then mapkeys(f,obj) else mapvalues(f,obj))
     is b:List do (
	  c := map(b.v,f);
	  when c is err:Error do if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else c
	  is v:Sequence do list(b.class,v,b.mutable)
	  else nullE			  -- will not happen
	  )
     is i:ZZ do (
	  if !isInt(i)
	  then WrongArgSmallInteger()
	  else map(toInt(i),f))
     else WrongArg(1,"a list, sequence, or an integer"));
map(e1:Expr,e2:Expr,f:Expr):Expr := (
     when e1
     is a1:Sequence do (
	  when e2
	  is a2:Sequence do (
	       c := map(a1,a2,f);
	       when c
	       is err:Error do if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else c
	       else c
	       )
	  is b2:List do (
	       c := map(a1,b2.v,f);
	       when c is err:Error do if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else c
	       is v:Sequence do list(b2.class,v,b2.mutable)
	       else nullE		  -- will not happen
	       )
	  else WrongArg(2,"a list or sequence"))
     is b1:List do (
	  when e2
	  is a2:Sequence do (
	       c := map(b1.v,a2,f);
	       when c is err:Error do if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else c
	       is v:Sequence do list(b1.class,v,b1.mutable)
	       else nullE		  -- will not happen
	       )
	  is b2:List do (
	       mutable := b1.mutable;
	       class := b1.class;
	       if class != b2.class then (
		    mutable = false;
		    class = listClass;
		    );
	       c := map(b1.v,b2.v,f);
	       when c is err:Error do if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else c
	       is v:Sequence do list(class,v,mutable)
	       else nullE		  -- will not happen
	       )
	  else WrongArg(2,"a list or sequence"))
     else WrongArg(1,"a list or sequence"));
map(e:Expr):Expr := (
     when e is a:Sequence do (
	  if length(a) == 2
	  then map(a.0,a.1)
	  else if length(a) == 3
	  then map(a.0,a.1,a.2)
	  else WrongNumArgs(2,3))
     else WrongNumArgs(2,3));
setupfun("apply",map);

scan(n:int,f:Expr):Expr := (
     if n <= 0 then return nullE;
     if recursionDepth > recursionLimit then RecursionLimit()
     else when f is fc:FunctionClosure do (
     	  recursionDepth = recursionDepth + 1;
     	  saveLocalFrame := localFrame;
	  previousFrame := fc.frame;
	  model := fc.model;
	  desc := model.desc;
	  body := model.body;
	  frameID := desc.frameID;
	  numparms := desc.numparms;
	  framesize := desc.framesize;
	  if numparms != 1 then (
     	       recursionDepth = recursionDepth - 1;
	       return WrongNumArgs(model.arrow,numparms,1);
	       );
	  if framesize == 1 then (
	       values := new Sequence len framesize do provide nullE;
	       localFrame = Frame(previousFrame,frameID,framesize,false,values);
	       for i from 0 to n-1 do (
		    values.0 = toInteger(i);
		    tmp := eval(body);
		    when tmp is err:Error do (
			 if err.message != returnMessage then (
			      recursionDepth = recursionDepth - 1;
			      localFrame = saveLocalFrame;
			      return returnFromLoop(tmp); 
			      )
			 )
		    else nothing;
		    if localFrame.notrecyclable then (
			 values = new Sequence len framesize do provide nullE;
			 localFrame = Frame(previousFrame,frameID,framesize,false,values);
			 );
		    );
	       localFrame = saveLocalFrame;
	       recursionDepth = recursionDepth - 1;
	       nullE)
	  else (
	       values := new Sequence len framesize do provide nullE;
	       localFrame = Frame(previousFrame,frameID,framesize,false,values);
	       for i from 0 to n-1 do (
		    values.0 = toInteger(i);
		    tmp := eval(body);
		    when tmp is err:Error do (
			 if err.message != returnMessage then (
			      recursionDepth = recursionDepth - 1;
			      localFrame = saveLocalFrame;
			      return returnFromLoop(tmp); 
			      )
			 )
		    else nothing;
		    if localFrame.notrecyclable then (
			 values = new Sequence len framesize do provide nullE;
			 localFrame = Frame(previousFrame,frameID,framesize,false,values);
			 )
		    else for i from 1 to framesize - 1 do values.i = nullE;
		    );
	       localFrame = saveLocalFrame;
	       recursionDepth = recursionDepth - 1;
	       nullE))
     is cf:CompiledFunction do (	  -- compiled code
     	  recursionDepth = recursionDepth + 1;
     	  saveLocalFrame := localFrame;
	  fn := cf.fn;
	  for i from 0 to n-1 do (
	       tmp := fn(Expr(toInteger(i)));
	       when tmp is Error do (
		    recursionDepth = recursionDepth - 1;
		    localFrame = saveLocalFrame;
		    return tmp; 
		    )
	       else nothing; 
	       );
	  localFrame = saveLocalFrame;
	  recursionDepth = recursionDepth - 1;
	  nullE)
     is cf:CompiledFunctionClosure do (	  -- compiled code closure
     	  recursionDepth = recursionDepth + 1;
     	  saveLocalFrame := localFrame;
	  fn := cf.fn;
	  env := cf.env;
	  for i from 0 to n-1 do (
	       tmp := fn(Expr(toInteger(i)),env);
	       when tmp is Error do (
		    recursionDepth = recursionDepth - 1;
		    localFrame = saveLocalFrame;
		    return tmp; 
		    )
	       else nothing; 
	       );
	  localFrame = saveLocalFrame;
	  recursionDepth = recursionDepth - 1;
	  nullE)
     is s:SpecialExpr do scan(n,s.e)
     else WrongArg(2,"a function"));

scan(a:Sequence,f:Expr):Expr := (
     oldlen := length(a);
     if oldlen == 0 then return nullE;
     recursionDepth = recursionDepth + 1;
     saveLocalFrame := localFrame;
     if recursionDepth > recursionLimit then (
	  recursionDepth = recursionDepth - 1;
	  return RecursionLimit();
	  );
     when f is fc:FunctionClosure do (
	  previousFrame := fc.frame;
	  model := fc.model;
	  desc := model.desc;
	  body := model.body;
	  frameID := desc.frameID;
	  numparms := desc.numparms;
	  framesize := desc.framesize;
	  if desc.restargs then (	  -- x -> ...
	       values := new Sequence len framesize do provide nullE;
	       localFrame = Frame(previousFrame,frameID,framesize,false,values);
	       foreach arg in a do (
		    values.0 = arg;
		    tmp := eval(body);
		    when tmp is err:Error do (
			 if err.message != returnMessage then (
			      recursionDepth = recursionDepth - 1;
			      localFrame = saveLocalFrame;
			      return returnFromLoop(tmp);
			      )
			 )
		    else nothing;
		    if localFrame.notrecyclable then (
			 values = new Sequence len framesize do provide nullE;
			 localFrame = Frame(previousFrame,frameID,framesize,false,values);
			 )
		    else for i from 1 to framesize - 1 do values.i = nullE;
		    )
	       )
	  else (				  -- (x,y) -> ...
	       if numparms == 1 then (
		    values := new Sequence len framesize do provide nullE;
		    localFrame = Frame(previousFrame,frameID,framesize,false,values);
		    if framesize == 1 then (
			 foreach arg in a do (
			      when arg is args:Sequence do (
				   if 1 == length(args) then values.0 = args.0
				   else (
					recursionDepth = recursionDepth - 1;
					localFrame = saveLocalFrame;
					return WrongNumArgs(model.arrow,numparms,length(args));
					)
				   )
			      else values.0 = arg;
			      tmp := eval(body);
			      when tmp is err:Error do (
				   if err.message != returnMessage then (
					recursionDepth = recursionDepth - 1;
					localFrame = saveLocalFrame;
					return returnFromLoop(tmp);
					)
				   )
			      else nothing;
		    	      if localFrame.notrecyclable then (
				   values = new Sequence len framesize do provide nullE;
				   localFrame = Frame(previousFrame,frameID,framesize,false,values);
				   );
			      )
			 )
		    else (
			 foreach arg in a do (
			      when arg is args:Sequence do (
				   if 1 == length(args) then values.0 = args.0
				   else (
					recursionDepth = recursionDepth - 1;
					localFrame = saveLocalFrame;
					return WrongNumArgs(model.arrow,numparms,length(args));
					)
				   )
			      else values.0 = arg;
			      tmp := eval(body);
			      when tmp is err:Error do (
				   if err.message != returnMessage then (
					recursionDepth = recursionDepth - 1;
					localFrame = saveLocalFrame;
					return returnFromLoop(tmp);
					)
				   )
			      else nothing;
		    	      if localFrame.notrecyclable then (
				   values = new Sequence len framesize do provide nullE;
				   localFrame = Frame(previousFrame,frameID,framesize,false,values);
				   )
			      else for i from 1 to framesize - 1 do values.i = nullE;
			      )
			 )
		    )
	       else (
		    if framesize == 0 then (
			 localFrame = previousFrame;
			 foreach arg in a do (
			      when arg is args:Sequence do (
				   if 0 != length(args) then (
					recursionDepth = recursionDepth - 1;
					localFrame = saveLocalFrame;
					return WrongNumArgs(model.arrow,0,length(args));
					)
				   )
			      else (
				   recursionDepth = recursionDepth - 1;
				   localFrame = saveLocalFrame;
				   return WrongNumArgs(model.arrow,numparms,1);
				   );
			      tmp := eval(body);
			      when tmp is err:Error do (
				   if err.message != returnMessage then (
					recursionDepth = recursionDepth - 1;
					localFrame = saveLocalFrame;
					return returnFromLoop(tmp);
					)
				   )
			      else nothing))
		    else (	  -- framesize != 0
			 values := new Sequence len framesize do provide nullE;
			 localFrame = Frame(previousFrame,frameID,framesize,false,values);
			 foreach arg in a do (
			      when arg is args:Sequence do (
				   if numparms == length(args) then (
					foreach x at i in args do values.i = x;
					)
				   else (
					recursionDepth = recursionDepth - 1;
					localFrame = saveLocalFrame;
					return WrongNumArgs(model.arrow,numparms,length(args));
					)
				   )
			      else (
				   recursionDepth = recursionDepth - 1;
				   localFrame = saveLocalFrame;
				   return WrongNumArgs(model.arrow,numparms,1);
				   );
			      tmp := eval(body);
			      when tmp is err:Error do (
				   if err.message != returnMessage then (
					recursionDepth = recursionDepth - 1;
					localFrame = saveLocalFrame;
					return returnFromLoop(tmp);
					)
				   )
			      else nothing;
		    	      if localFrame.notrecyclable then (
				   values = new Sequence len framesize do provide nullE;
				   localFrame = Frame(previousFrame,frameID,framesize,false,values);
				   )
			      else for i from numparms to framesize - 1 do values.i = nullE;
			      )))))
     is cf:CompiledFunction do (	  -- compiled code
	  fn := cf.fn;
	  foreach arg in a do (
	       tmp := fn(arg);
	       when tmp is Error do (
		    recursionDepth = recursionDepth - 1;
		    localFrame = saveLocalFrame;
		    return tmp;
		    )
	       else nothing; ))
     is cf:CompiledFunctionClosure do (	  -- compiled code
	  fn := cf.fn;
	  env := cf.env;
	  foreach arg in a do (
	       tmp := fn(arg,env);
	       when tmp is Error do (
		    recursionDepth = recursionDepth - 1;
		    localFrame = saveLocalFrame;
		    return tmp;
		    )
	       else nothing; ))
     is s:SpecialExpr do (
	  fn := s.e;
	  foreach arg in a do (
	       tmp := applyEE(fn,arg);
	       when tmp is Error do (
		    recursionDepth = recursionDepth - 1;
		    localFrame = saveLocalFrame;
		    return tmp;
		    )
	       else nothing; ))
     else (
	  recursionDepth = recursionDepth - 1;
	  localFrame = saveLocalFrame;
	  return WrongArg(2,"a function");
	  );
     localFrame = saveLocalFrame;
     recursionDepth = recursionDepth - 1;
     nullE);

-- scan(a:Sequence,f:Expr):Expr := (
--      foreach x in a do (
-- 	  y := apply(f,x);
-- 	  when y is Error do return y else nothing;
-- 	  );
--      nullE);

scan(a1:Sequence,a2:Sequence,f:Expr):Expr := (
     newlen := length(a1);
     if newlen != length(a2) then return WrongArg("lists of the same length");
     if newlen == 0 then return nullE;
     haderror := false;
     recursionDepth = recursionDepth + 1;
     if recursionDepth > recursionLimit then return RecursionLimit();
     when f is fc:FunctionClosure do (
	  previousFrame := fc.frame;
	  model := fc.model;
	  desc := model.desc;
	  body := model.body;
	  frameID := desc.frameID;
	  numparms := desc.numparms;
	  framesize := desc.framesize;
	  if desc.restargs then (	  -- x -> ...
	       saveLocalFrame := localFrame;
	       values := new Sequence len framesize do provide nullE;
	       localFrame = Frame(previousFrame,frameID,framesize,false,values);
	       for i from 0 to newlen - 1 do (
		    values.0 = Sequence(a1.i,a2.i);
		    tmp := eval(body);
		    when tmp is err:Error do (
			 if err.message != returnMessage then (
			      -- stash
			      localFrame = saveLocalFrame;
			      recursionDepth = recursionDepth - 1;
			      return returnFromLoop(tmp);
			      )
			 )
		    else nothing;
		    if localFrame.notrecyclable then (
			 values = new Sequence len framesize do provide nullE;
			 localFrame = Frame(previousFrame,frameID,framesize,false,values);
			 )
		    else for i from 1 to framesize - 1 do values.i = nullE;
		    );
	       -- stash
	       localFrame = saveLocalFrame;
	       recursionDepth = recursionDepth - 1;
	       nullE)
	  else (				  -- (x,y) -> ...
	       if numparms != 2 then WrongNumArgs(model.arrow,numparms,2)
	       else (
		    saveLocalFrame := localFrame;
		    values := new Sequence len framesize do provide nullE;
		    localFrame = Frame(previousFrame,frameID,framesize,false,values);
		    for i from 0 to newlen - 1 do (
			 values.0 = a1.i;
			 values.1 = a2.i;
			 tmp := eval(body);
			 when tmp is err:Error do (
			      if err.message != returnMessage then (
				   -- stash
				   localFrame = saveLocalFrame;
				   recursionDepth = recursionDepth - 1;
				   return returnFromLoop(tmp);
				   )
			      )
			 else nothing;
		    	 if localFrame.notrecyclable then (
			      values = new Sequence len framesize do provide nullE;
			      localFrame = Frame(previousFrame,frameID,framesize,false,values);
			      )
			 else (
			      -- it would be faster to do a byte copy here:
			      for i from 2 to framesize - 1 do values.i = nullE;
			      );
			 );
		    localFrame = saveLocalFrame;
		    recursionDepth = recursionDepth - 1;
		    nullE ) ) )
     is cf:CompiledFunction do (	  -- compiled code
	  fn := cf.fn;
	  for i from 0 to newlen - 1 do (
	       tmp := fn(Expr(Sequence(a1.i,a2.i)));
	       when tmp is Error do (
	  	    recursionDepth = recursionDepth - 1;
		    return tmp;
		    )
	       else nothing;
	       );
	  recursionDepth = recursionDepth - 1;
	  nullE
	  )
     is cf:CompiledFunctionClosure do (	  -- compiled code closure
	  fn := cf.fn;
	  env := cf.env;
	  for i from 0 to newlen - 1 do (
	       tmp := fn(Expr(Sequence(a1.i,a2.i)),env);
	       when tmp is Error do (
	  	    recursionDepth = recursionDepth - 1;
		    return tmp;
		    )
	       else nothing;
	       );
	  recursionDepth = recursionDepth - 1;
	  nullE)
     is s:SpecialExpr do (
	  ret := scan(a1,a2,s.e);
	  recursionDepth = recursionDepth - 1;
	  ret)
     else WrongArg(2,"a function")
     );
scan(e1:Expr,e2:Expr,f:Expr):Expr := (
     when e1
     is a1:Sequence do (
	  when e2
	  is a2:Sequence do scan(a1,a2,f)
	  is b2:List do scan(a1,b2.v,f)
	  else WrongArg(2,"a list or sequence"))
     is b1:List do (
	  when e2
	  is a2:Sequence do scan(b1.v,a2,f)
	  is b2:List do scan(b1.v,b2.v,f)
	  else WrongArg(2,"a list or sequence"))
     else WrongArg(1,"a list or sequence"));
scan(e:Expr,f:Expr):Expr := (
     when e
     is a:Sequence do scan(a,f)
     is b:List do scan(b.v,f)
     is i:ZZ do (
	  if !isInt(i)
	  then WrongArgSmallInteger(1)
	  else scan(toInt(i),f))
     else buildErrorPacket("scan expects a list"));
scan(e:Expr):Expr := (
     when e is a:Sequence do (
	  if length(a) == 2
	  then scan(a.0,a.1)
	  else if length(a) == 3
	  then scan(a.0,a.1,a.2)
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
setupfun("scan",scan);
gcd(x:Expr,y:Expr):Expr := (
     when x
     is a:ZZ do (
	  when y
	  is b:ZZ do Expr(gcd(a,b))
	  else buildErrorPacket("expected an integer"))
     else buildErrorPacket("expected an integer"));
gcdfun(e:Expr):Expr := accumulate(plus0,plus1,gcd,e);
setupfun("gcd0",gcdfun);

toSequence(e:Expr):Expr := (
     when e
     is Sequence do e
     is b:List do (
	  if b.mutable
	  then Expr(new Sequence len length(b.v) do foreach i in b.v do provide i)
	  else Expr(b.v)
	  )
     else WrongArg("a list or sequence"));
setupfun("toSequence",toSequence);

sequencefun(e:Expr):Expr := (
     when e
     is a:Sequence do e
     else Expr(Sequence(e)));
setupfun("sequence",sequencefun);

-- iteratedApply(lhs:Code,rhs:Code):Expr := (
--      -- f ## (x,y,z) becomes ((f x) y) z
--      f := eval(lhs);
--      when f is Error do f else (
-- 	  arg := eval(rhs);
-- 	  when arg
-- 	  is Error do arg
-- 	  is args:Sequence do (
-- 	       foreach x in args do (
-- 		    f = apply(f,x);
-- 		    when f is Error do return f else nothing;
-- 		    );
-- 	       f)
-- 	  else apply(f,arg)));
-- setup(SharpSharpS,iteratedApply);
iteratedApply(e:Expr):Expr := (
     -- uncurry(f,(x,y,z)) becomes ((f x) y) z
     when e is s:Sequence do if length(s) != 2 then WrongNumArgs(2) else (
	  f := s.0;
	  arg := s.1;
	  when arg
	  is args:Sequence do (
	       foreach x in args do (
		    f = applyEE(f,x);
		    when f is Error do return f else nothing;
		    );
	       f)
	  else applyEE(f,arg))
     else WrongNumArgs(2));
setupfun("uncurry",iteratedApply);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
