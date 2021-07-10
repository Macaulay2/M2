--		Copyright 1994,2010 by Daniel R. Grayson

use evaluate;
use actors;

isOption(e:Expr):bool := (
     when e
     is b:List do b.Class == optionClass && length(b.v) == 2 
     is q:HashTable do q.Class == optionTableClass
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
     z.Mutable = true;
     z.beingInitialized = true;
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
     Expr(Sequence(Expr(sethash(z,h.Mutable)),newargs)));
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
     z.Mutable = true;
     z.beingInitialized = true;
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
     Expr(Sequence(Expr(sethash(z,false)),newargs)));
override(e:Expr):Expr := (
     when e is args:Sequence do (
	  if length(args) == 2 then (
	       when args.0
	       is h:HashTable do (
		    if h.Mutable then WrongArg("an immutable hash table")
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
     when x
     is xx:ZZcell do (
	  when y 
	  is yy:ZZcell do toExpr(yy.v === xx.v)			    -- # typical value: symbol ==, ZZ, ZZ, Boolean
	  is yy:QQcell do toExpr(yy.v === xx.v)			    -- # typical value: symbol ==, ZZ, QQ, Boolean
	  is yy:RRcell do toExpr(yy.v === xx.v)			    -- # typical value: symbol ==, ZZ, RR, Boolean
      is yy:RRicell do toExpr(yy.v === xx.v)			-- # typical value: symbol ==, ZZ, RRi, Boolean
	  is yy:CCcell do toExpr(yy.v === xx.v)			    -- # typical value: symbol ==, ZZ, CC, Boolean
	  else equalmethod(x,y)
	  )
     is xx:SymbolClosure do (
	  when y is yy:SymbolClosure do toExpr(xx === yy)             -- # typical value: symbol ==, Symbol, Symbol, Boolean
	  else equalmethod(x,y)
     	  )
     is xx:QQcell do (
	  when y
	  is yy:ZZcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, QQ, ZZ, Boolean
	  is yy:QQcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, QQ, QQ, Boolean
	  is yy:RRcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, QQ, RR, Boolean
      is yy:RRicell do toExpr(yy.v === xx.v)			-- # typical value: symbol ==, QQ, RRi, Boolean
	  is yy:CCcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, QQ, CC, Boolean
	  else equalmethod(x,y)
	  )
     is xx:RRcell do (
	  when y
	  is yy:ZZcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, RR, ZZ, Boolean
	  is yy:QQcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, RR, QQ, Boolean
	  is yy:RRcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, RR, RR, Boolean
      is yy:RRicell do toExpr(yy.v === xx.v)			-- # typical value: symbol ==, RR, RRi, Boolean
	  is yy:CCcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, RR, CC, Boolean
	  else equalmethod(x,y)
	  )
      is xx:RRicell do (
          when y is yy:RRicell do toExpr(xx.v === yy.v)  -- # typical value: symbol ==, RRi, RRi, Boolean
                 is yy:ZZcell do toExpr(xx.v === yy.v)   -- # typical value: symbol ==, RRi, ZZ, Boolean
                 is yy:QQcell do toExpr(xx.v === yy.v)   -- # typical value: symbol ==, RRi, QQ, Boolean
                 is yy:RRcell do toExpr(xx.v === yy.v)   -- # typical value: symbol ==, RRi, RR, Boolean
          else buildErrorPacket(EngineError("equality not implemented")))
     is xx:CCcell do (
	  when y
	  is yy:ZZcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, CC, ZZ, Boolean
	  is yy:QQcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, CC, QQ, Boolean
	  is yy:RRcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, CC, RR, Boolean
	  is yy:CCcell do toExpr(xx.v === yy.v)			    -- # typical value: symbol ==, CC, CC, Boolean
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
     is xx:stringCell do (
	  when y 
	  is yy:stringCell do toExpr(xx.v === yy.v)	 -- # typical value: symbol ==, String, String, Boolean
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
	       if a.Class != b.Class then return False;
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

NotEqualfun(lhs:Code,rhs:Code):Expr := (
     x := eval(lhs);
     when x is Error do x
     else (
     	  y := eval(rhs);
     	  when y is Error do y
	  else notFun(EqualEqualfun(x,y))));
setup(NotEqualS,NotEqualfun);

binarycomparison(left:Expr,right:Expr):Expr := (
     result := binarymethod(left,right,QuestionS);
     when result is Error do result else
     if result === GreaterS || result === LessS || result === EqualEqualS || result === incomparableS
     then result
     else buildErrorPacket("expected result of comparison to be one of the following symbols: <, >, ==, incomparable"));

compare(left:Expr,right:Expr):Expr := (
    if left == right then EqualEqualE
    else
     when left
     is x:ZZcell do (
	  when right
	  is y:ZZcell do
	  if x.v < y.v then LessE else if x.v > y.v then GreaterE else EqualEqualE
	  is y:QQcell do
	  if x.v < y.v then LessE else if x.v > y.v then GreaterE else EqualEqualE
	  is y:RRcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
      is y:RRicell do (
	       if flagged() then incomparableE else
	       if (compare(x.v,rightRR(y.v)) == 0) && (compare(x.v,leftRR(y.v)) == 0) then EqualEqualE
              else if compare(x.v,leftRR(y.v)) < 0 then LessE
              else if compare(x.v,rightRR(y.v)) > 0 then GreaterE
              else if compare(x.v,rightRR(y.v)) >= 0 then GreaterEqualE
              else if compare(x.v,leftRR(y.v)) <= 0 then LessEqualE
              else incomparableE
	       )
	  is y:CCcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
     	  is Error do right
	  else binarycomparison(left,right))
     is x:stringCell do (
	  when right
	  is y:stringCell do (
	       c := strnumcmp(x.v,y.v);			    -- should use same method as for symbols below
	       if c == 1 then GreaterE
	       else if c == -1 then LessE
     	       else EqualEqualE
	       )
	  is y:Net do (
	       c := netcmp(x.v,y);
	       if c == 1 then GreaterE
	       else if c == -1 then LessE
     	       else EqualEqualE
	       )
     	  is Error do right
	  else binarycomparison(left,right))
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
	  else binarycomparison(left,right))
     is x:QQcell do (
	  when right
	  is y:ZZcell do 
	  if x.v < y.v then LessE else if x.v > y.v then GreaterE else EqualEqualE
	  is y:QQcell do
	  if x.v < y.v then LessE else if x.v > y.v then GreaterE else EqualEqualE
	  is y:RRcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
      is y:RRicell do (
	       if flagged() then incomparableE else
	       if (compare(x.v,rightRR(y.v)) == 0) && (compare(x.v,leftRR(y.v)) == 0) then EqualEqualE
              else if compare(x.v,leftRR(y.v)) < 0 then LessE
              else if compare(x.v,rightRR(y.v)) > 0 then GreaterE
              else if compare(x.v,rightRR(y.v)) >= 0 then GreaterEqualE
              else if compare(x.v,leftRR(y.v)) <= 0 then LessEqualE
              else incomparableE
	       )
	  is y:CCcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
     	  is Error do right
	  else binarycomparison(left,right))
     is x:RRcell do (
	  when right
	  is y:ZZcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:QQcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:RRcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
      is y:RRicell do (
	       if flagged() then incomparableE else
	       if (compare(x.v,rightRR(y.v)) == 0) && (compare(x.v,leftRR(y.v)) == 0) then EqualEqualE
              else if compare(x.v,leftRR(y.v)) < 0 then LessE
              else if compare(x.v,rightRR(y.v)) > 0 then GreaterE
              else if compare(x.v,rightRR(y.v)) >= 0 then GreaterEqualE
              else if compare(x.v,leftRR(y.v)) <= 0 then LessEqualE
              else incomparableE
	       )
	  is y:CCcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
     	  is Error do right
	  else binarycomparison(left,right))
     is x:RRicell do (
            when right is y:RRicell do (
                if flagged() then incomparableE
                else if ((leftRR(x.v) === leftRR(y.v)) && (rightRR(x.v) === rightRR(y.v))) then EqualEqualE
                else if compare(rightRR(x.v),leftRR(y.v)) < 0  then LessE
                else if compare(rightRR(x.v),leftRR(y.v)) <= 0 then LessEqualE
                else if compare(leftRR(x.v),rightRR(y.v)) > 0 then GreaterE
                else if compare(leftRR(x.v),rightRR(y.v)) >= 0 then GreaterEqualE
                else incomparableE
                )
            is y:RRcell do (
	            if flagged() then incomparableE else
	            if (compare(y.v,rightRR(x.v)) == 0) && (compare(y.v,leftRR(x.v)) == 0) then EqualEqualE
                else if compare(rightRR(x.v),y.v) < 0 then LessE
                else if compare(leftRR(x.v),y.v) > 0 then GreaterE
                else if compare(leftRR(x.v),y.v) >= 0 then GreaterEqualE
                else if compare(rightRR(x.v),y.v) <= 0 then LessEqualE
                else incomparableE
	            )
            is y:QQcell do (
	            if flagged() then incomparableE else
	            if (compare(y.v,rightRR(x.v)) == 0) && (compare(y.v,leftRR(x.v)) == 0) then EqualEqualE
                else if compare(rightRR(x.v),y.v) < 0 then LessE
                else if compare(leftRR(x.v),y.v) > 0 then GreaterE
                else if compare(leftRR(x.v),y.v) >= 0 then GreaterEqualE
                else if compare(rightRR(x.v),y.v) <= 0 then LessEqualE
                else incomparableE
	            )
            is y:ZZcell do (
	            if flagged() then incomparableE else
	            if (compare(y.v,rightRR(x.v)) == 0) && (compare(y.v,leftRR(x.v)) == 0) then EqualEqualE
                else if compare(rightRR(x.v),y.v) < 0 then LessE
                else if compare(leftRR(x.v),y.v) > 0 then GreaterE
                else if compare(leftRR(x.v),y.v) >= 0 then GreaterEqualE
                else if compare(rightRR(x.v),y.v) <= 0 then LessEqualE
                else incomparableE
	            )
            is Error do right
            else buildErrorPacket(EngineError("comparison not implemented")))
     is x:CCcell do (
	  when right
	  is y:ZZcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:QQcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:RRcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
	  is y:CCcell do (
	       r := compare(x.v,y.v);
	       if flagged() then incomparableE else
	       if r < 0 then LessE else if r > 0 then GreaterE else EqualEqualE
	       )
     	  is Error do right
	  else binarycomparison(left,right))
     is x:Net do (
	  when right
	  is y:Net do (
	       c := netcmp(x,y);
	       if c == 1 then GreaterE
	       else if c == -1 then LessE
     	       else EqualEqualE
	       )
	  is y:stringCell do (
	       c := netcmp(x,y.v);
	       if c == 1 then GreaterE
	       else if c == -1 then LessE
     	       else EqualEqualE
	       )
     	  is Error do right
	  else binarycomparison(left,right))
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
	  else binarycomparison(left,right))
     is Error do left
     else (
	  when right
	  is Error do right
	  else binarycomparison(left,right)));
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
threadLocal sortlist := emptySequence;
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
	   if ancestor(t.Class, listClass) then (
		if length(t.v) <= 1 then e else (
		     r := basicsort(t.v,ww);
		     when r is b:Sequence do list(t.Class,b) else r))
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
     else (
        L := eval(lhs);
        when L
           is x:RRicell do (
                if leftRR(x.v) === rightRR(x.v) then (
                    if GreaterS.symbol === e || EqualEqualS.symbol === e || GreaterEqualS.symbol === e then True else False)
                else if GreaterS.symbol === e || GreaterEqualS.symbol === e then True else False)
           else if GreaterS.symbol === e || EqualEqualS.symbol === e || GreaterEqualS.symbol === e then True else False)
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
     else (
        L := eval(lhs);
        when L
           is x:RRicell do (
                if leftRR(x.v) === rightRR(x.v) then (
                    if LessS.symbol === e || EqualEqualS.symbol === e || LessEqualS.symbol === e then True else False)
                else if LessS.symbol === e || LessEqualS.symbol === e then True else False)
           else if LessS.symbol === e || EqualEqualS.symbol === e || LessEqualS.symbol === e then True else False)
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
	  Expr(sethash(List(commonAncestor(xl.Class,yl.Class), z,0,false),xl.Mutable | yl.Mutable)))
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

fusepairs(xx:Expr,yy:Expr,f:Expr):Expr := (
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
		    while j < length(y) do (
			  when y.j
			  is yj:Sequence do
			  if length(yj) != 2
			  then return WrongArg(2,"a list of pairs")
			  else (
				t := applyEEE(f,nullE,yj.1);
				when t is err:Error do (
				     if err.message != continueMessage then return t;
				     )
				else (
				     z.n = Sequence(yj.0, t);
				     n = n+1;
				     );
				j = j+1;
			       )
			  else return WrongArg(2,"a list of pairs");
			  );
		    break;
		    )
	       else if j >= length(y) then (
		    while i < length(x) do (
			  when x.i
			  is xi:Sequence do
			  if length(xi) != 2
			  then return WrongArg(1,"a list of pairs")
			  else (
				t := applyEEE(f,xi.1,nullE);
				when t is err:Error do (
				     if err.message != continueMessage then return t;
				     )
				else (
				     z.n = Sequence(xi.0, t);
				     n = n+1;
				     );
				i = i+1;
			       )
			  else return WrongArg(1,"a list of pairs");
			  );
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
				t := applyEEE(f,nullE,yj.1);
				when t is err:Error do (
				     if err.message != continueMessage then return t;
				     )
				else (
				     z.n = Sequence(yj.0, t);
				     n = n+1;
				     );
				j = j+1;
			 )
		    else if LessS.symbol === c then (
				t := applyEEE(f,xi.1,nullE);
				when t is err:Error do (
				     if err.message != continueMessage then return t;
				     )
				else (
				     z.n = Sequence(xi.0, t);
				     n=n+1;
				     );
				i = i+1;
			 )
		    else (
			 t := applyEEE(f,xi.1,yj.1);
			 when t is err:Error do (
			      if err.message != continueMessage then return t;
			      )
			 else (
			      z.n = Sequence(xi.0, t);
			      n = n+1;
			      );
			 i = i+1;
			 j = j+1;
			 )
			 )
	       else return WrongArg(2,"a list of pairs")
	       else return WrongArg(1,"a list of pairs"));
	  if n < length(x)+length(y)
	  then z = new Sequence len n do foreach a in z do provide a;
	  Expr(sethash(List(commonAncestor(xl.Class,yl.Class), z,0,false),xl.Mutable | yl.Mutable)))
     else WrongArg(2,"a list")
     else WrongArg(1,"a list"));
fusepairsfun(e:Expr):Expr := (
     when e
     is a:Sequence do
     if length(a) == 3 then
     fusepairs(a.0,a.1,a.2)
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("fusePairs",fusepairsfun);

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
--	  Expr(sethash(List(commonAncestor(xl.Class,yl.Class), z,0,false),xl.Mutable | yl.Mutable)))
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
     is x:ZZcell do
     when a.1
     is y:ZZcell do toExpr(x.v ^^ y.v)
     else WrongArgZZ(2)
     else WrongArgZZ(1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("bitxorfun",bitxorfun);
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
     is x:CCcell do toExpr(sin(x.v))				    -- # typical value: sin, CC, CC
     is x:RRcell do toExpr(sin(x.v))				    -- # typical value: sin, RR, RR
     is x:RRicell do toExpr(sin(x.v))				    -- # typical value: sin, RRi, RRi
     is x:ZZcell do toExpr(sin(toRR(x.v)))			    -- # typical value: sin, ZZ, RR
     is x:QQcell do toExpr(sin(toRR(x.v)))			    -- # typical value: sin, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("sin",sin);
cos(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(cos(x.v))				    -- # typical value: cos, CC, CC
     is x:RRcell do toExpr(cos(x.v))				    -- # typical value: cos, RR, RR
     is x:RRicell do toExpr(cos(x.v))				    -- # typical value: cos, RRi, RRi
     is x:ZZcell do toExpr(cos(toRR(x.v)))			    -- # typical value: cos, ZZ, RR
     is x:QQcell do toExpr(cos(toRR(x.v)))			    -- # typical value: cos, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("cos",cos);
tan(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(tan(x.v))				    -- # typical value: tan, CC, CC
     is x:RRcell do toExpr(tan(x.v))				    -- # typical value: tan, RR, RR
     is x:RRicell do toExpr(tan(x.v))				    -- # typical value: tan, RRi, RRi
     is x:ZZcell do toExpr(tan(toRR(x.v)))			    -- # typical value: tan, ZZ, RR
     is x:QQcell do toExpr(tan(toRR(x.v)))			    -- # typical value: tan, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("tan",tan);
acos(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(acos(x.v))				    -- # typical value: acos, CC, CC
     is x:RRcell do (
	  if x.v > 1 || x.v < -1
	  then toExpr(acos(toCC(x.v)))
	  else toExpr(acos(x.v))				    -- # typical value: acos, RR, RR
	  )
     is x:RRicell do (
	  if x.v <= 1 && x.v >= -1
	  then toExpr(acos(x.v))                    -- # typical value: acos, RRi, RRi
      else buildErrorPacket("Must be between -1 and 1")
	  )
     is x:QQcell do (
	  if x.v > 1 || x.v < -1
	  then toExpr(acos(toCC(x.v)))
	  else toExpr(acos(toRR(x.v))) -- # typical value: acos, QQ, RR
	  )
     is x:ZZcell do (
	  if x.v > 1 || x.v < -1
	  then toExpr(acos(toCC(x.v)))
	  else toExpr(acos(toRR(x.v)))				    -- # typical value: acos, ZZ, RR
	  )
     else buildErrorPacket("expected a number")
     );
setupfun("acos",acos);
sec(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(sec(x.v))				    -- # typical value: sec, CC, CC
     is x:RRcell do toExpr(sec(x.v))				    -- # typical value: sec, RR, RR
     is x:RRicell do toExpr(sec(x.v))				    -- # typical value: sec, RRi, RRi
     is x:ZZcell do toExpr(sec(toRR(x.v)))			    -- # typical value: sec, ZZ, RR
     is x:QQcell do toExpr(sec(toRR(x.v)))			    -- # typical value: sec, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("sec",sec);
csc(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(csc(x.v))				    -- # typical value: csc, CC, CC
     is x:RRcell do toExpr(csc(x.v))				    -- # typical value: csc, RR, RR
     is x:RRicell do toExpr(csc(x.v))				    -- # typical value: csc, RRi, RRi
     is x:ZZcell do toExpr(csc(toRR(x.v)))			    -- # typical value: csc, ZZ, RR
     is x:QQcell do toExpr(csc(toRR(x.v)))			    -- # typical value: csc, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("csc",csc);
cot(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(cot(x.v))				    -- # typical value: cot, CC, CC
     is x:RRcell do toExpr(cot(x.v))				    -- # typical value: cot, RR, RR
     is x:RRicell do toExpr(cot(x.v))				    -- # typical value: cot, RRi, RRi
     is x:ZZcell do toExpr(cot(toRR(x.v)))			    -- # typical value: cot, ZZ, RR
     is x:QQcell do toExpr(cot(toRR(x.v)))			    -- # typical value: cot, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("cot",cot);
sech(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(sech(x.v))				    -- # typical value: sech, CC, CC
     is x:RRcell do toExpr(sech(x.v))				    -- # typical value: sech, RR, RR
     is x:RRicell do toExpr(sech(x.v))				    -- # typical value: sech, RRi, RRi
     is x:ZZcell do toExpr(sech(toRR(x.v)))			    -- # typical value: sech, ZZ, RR
     is x:QQcell do toExpr(sech(toRR(x.v)))			    -- # typical value: sech, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("sech",sech);
csch(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(csch(x.v))				    -- # typical value: csch, CC, CC
     is x:RRcell do toExpr(csch(x.v))				    -- # typical value: csch, RR, RR
     is x:RRicell do toExpr(csch(x.v))				    -- # typical value: csch, RRi, RRi
     is x:ZZcell do toExpr(csch(toRR(x.v)))			    -- # typical value: csch, ZZ, RR
     is x:QQcell do toExpr(csch(toRR(x.v)))			    -- # typical value: csch, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("csch",csch);
coth(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(coth(x.v))				    -- # typical value: coth, CC, CC
     is x:RRcell do toExpr(coth(x.v))				    -- # typical value: coth, RR, RR
     is x:RRicell do toExpr(coth(x.v))				    -- # typical value: coth, RRi, RRi
     is x:ZZcell do toExpr(coth(toRR(x.v)))			    -- # typical value: coth, ZZ, RR
     is x:QQcell do toExpr(coth(toRR(x.v)))			    -- # typical value: coth, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("coth",coth);
asin(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(asin(x.v))				    -- # typical value: asin, CC, CC
     is x:RRcell do (
	  if x.v > 1 || x.v < -1
	  then toExpr(asin(toCC(x.v)))
	  else toExpr(asin(x.v))				    -- # typical value: asin, RR, RR
	  )
     is x:RRicell do (
	  if x.v <= 1 && x.v >= -1
	  then toExpr(asin(x.v))                    -- # typical value: asin, RRi, RRi
	  else buildErrorPacket("Must be between -1 and 1")
	  )
     is x:QQcell do (
	  if x.v > 1 || x.v < -1
	  then toExpr(asin(toCC(x.v)))
	  else toExpr(asin(toRR(x.v)))				    -- # typical value: asin, QQ, RR
	  )
     is x:ZZcell do (
	  if x.v > 1 || x.v < -1
	  then toExpr(asin(toCC(x.v)))
	  else toExpr(asin(toRR(x.v)))			    -- # typical value: asin, ZZ, RR
	  )
     else buildErrorPacket("expected a number")
     );
setupfun("asin",asin);
log1p(e:Expr):Expr := (
     when e
     is x:RRcell do toExpr(log1p(x.v))				    -- # typical value: log1p, RR, RR
     is x:RRicell do toExpr(log1p(x.v))				    -- # typical value: log1p, RRi, RRi
     is x:ZZcell do toExpr(log1p(toRR(x.v)))			    -- # typical value: log1p, ZZ, RR
     is x:QQcell do toExpr(log1p(toRR(x.v)))			    -- # typical value: log1p, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("log1p",log1p);
expm1(e:Expr):Expr := (
     when e
     is x:RRcell do toExpr(expm1(x.v))				    -- # typical value: expm1, RR, RR
     is x:RRicell do toExpr(expm1(x.v))				    -- # typical value: expm1, RRi, RRi
     is x:ZZcell do toExpr(expm1(toRR(x.v)))			    -- # typical value: expm1, ZZ, RR
     is x:QQcell do toExpr(expm1(toRR(x.v)))			    -- # typical value: expm1, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("expm1",expm1);
eint(e:Expr):Expr := (
     when e
     is x:RRcell do toExpr(eint(x.v))				    -- # typical value: eint, RR, RR
     is x:ZZcell do toExpr(eint(toRR(x.v)))			    -- # typical value: eint, ZZ, RR
     is x:QQcell do toExpr(eint(toRR(x.v)))			    -- # typical value: eint, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("eint",eint);
Gamma(e:Expr):Expr := (
     when e
     is x:RRcell do toExpr(Gamma(x.v))				    -- # typical value: Gamma, RR, RR
     is x:ZZcell do toExpr(Gamma(toRR(x.v)))			    -- # typical value: Gamma, ZZ, RR
     is x:QQcell do toExpr(Gamma(toRR(x.v)))			    -- # typical value: Gamma, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("Gamma",Gamma);
export lgamma(x:RR):Expr := (
     z := newRRmutable(precision(x));
     i := 0;
     Ccode( void, "mpfr_lgamma((mpfr_ptr)", z, ",&",i,",(mpfr_srcptr)", x, ", GMP_RNDN)" );
     Expr(Sequence(toExpr(moveToRR(z)),toExpr(i))));
lgamma(e:Expr):Expr := (
     when e
     is x:RRcell do lgamma(x.v)				    -- # typical value: lgamma, RR, RR
     is x:ZZcell do lgamma(toRR(x.v))			    -- # typical value: lgamma, ZZ, RR
     is x:QQcell do lgamma(toRR(x.v))			    -- # typical value: lgamma, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("lgamma",lgamma);
zeta(e:Expr):Expr := (
     when e
     is x:RRcell do toExpr(zeta(x.v))				    -- # typical value: zeta, RR, RR
     is x:ZZcell do (					    -- # typical value: zeta, ZZ, RR
	  if isULong(x.v)
	  then toExpr(zeta(toULong(x.v),defaultPrecision))
	  else toExpr(zeta(toRR(x.v)))
	  )
     is x:QQcell do toExpr(zeta(toRR(x.v)))			    -- # typical value: zeta, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("zeta",zeta);
erf(e:Expr):Expr := (
     when e
     is x:RRcell do toExpr(erf(x.v))				    -- # typical value: erf, RR, RR
     is x:ZZcell do toExpr(erf(toRR(x.v)))			    -- # typical value: erf, ZZ, RR
     is x:QQcell do toExpr(erf(toRR(x.v)))			    -- # typical value: erf, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("erf",erf);
erfc(e:Expr):Expr := (
     when e
     is x:RRcell do toExpr(erfc(x.v))				    -- # typical value: erfc, RR, RR
     is x:ZZcell do toExpr(erfc(toRR(x.v)))			    -- # typical value: erfc, ZZ, RR
     is x:QQcell do toExpr(erfc(toRR(x.v)))			    -- # typical value: erfc, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("erfc",erfc);
BesselJ(n:long,x:RR):RR := (
     if n == long(0) then j0(x)
     else if n == long(1) then j1(x)
     else jn(n,x));
BesselJ(e:Expr):Expr := (
     when e is s:Sequence do (
	  when s.0 is n:ZZcell do if !isLong(n.v) then WrongArg(1,"a small integer") else (
	       when s.1 
	       is x:RRcell do toExpr(BesselJ(toLong(n.v),x.v))	    -- # typical value: BesselJ, ZZ, RR, RR
	       is x:ZZcell do toExpr(BesselJ(toLong(n.v),toRR(x.v)))  -- # typical value: BesselJ, ZZ, ZZ, RR
	       is x:QQcell do toExpr(BesselJ(toLong(n.v),toRR(x.v)))  -- # typical value: BesselJ, ZZ, QQ, RR
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
	  when s.0 is n:ZZcell do if !isLong(n.v) then WrongArg(1,"a small integer") else (
	       when s.1 
	       is x:RRcell do toExpr(BesselY(toLong(n.v),x.v))	    -- # typical value: BesselY, ZZ, RR, RR
	       is x:ZZcell do toExpr(BesselY(toLong(n.v),toRR(x.v)))  -- # typical value: BesselY, ZZ, ZZ, RR
	       is x:QQcell do toExpr(BesselY(toLong(n.v),toRR(x.v)))  -- # typical value: BesselY, ZZ, QQ, RR
	       else WrongArg(2,"a number"))
	  else WrongArgZZ(1))
     else WrongNumArgs(2));
setupfun("BesselY",BesselY);
atan2(yy:Expr,xx:Expr):Expr := (
     when yy
     is y:RRcell do (
	  when xx
	  is x:RRcell do toExpr(atan2(y.v,x.v))			            -- # typical value: atan2, RR, RR, RR
	  is x:RRicell do toExpr(atan2(toRRi(y.v),x.v))			            -- # typical value: atan2, RR, RRi, RRi
	  is x:ZZcell do toExpr(atan2(y.v,toRR(x.v,precision(y.v))))	    -- # typical value: atan2, RR, ZZ, RR
	  is x:QQcell do toExpr(atan2(y.v,toRR(x.v,precision(y.v))))	    -- # typical value: atan2, RR, QQ, RR
	  else WrongArg(1,"a number"))
     is y:ZZcell do (
	  when xx
	  is x:RRcell do toExpr(atan2(toRR(y.v,precision(x.v)),x.v))    -- -- # typical value: atan2, ZZ, RR, RR
	  is x:RRicell do toExpr(atan2(toRRi(y.v,precision(x.v)),x.v))			            -- # typical value: atan2, ZZ, RRi, RRi
	  is x:ZZcell do toExpr(atan2(toRR(y.v),toRR(x.v)))	       -- # typical value: atan2, ZZ, ZZ, RR
	  is x:QQcell do toExpr(atan2(toRR(y.v),toRR(x.v)))	       -- # typical value: atan2, ZZ, QQ, RR
	  else WrongArg(1,"a number"))
     is y:QQcell do (
	  when xx
	  is x:RRcell do toExpr(atan2(toRR(y.v,precision(x.v)),x.v))    -- # typical value: atan2, QQ, RR, RR
	  is x:RRicell do toExpr(atan2(toRRi(y.v,precision(x.v)),x.v))			            -- # typical value: atan2, QQ, RRi, RRi
	  is x:ZZcell do toExpr(atan2(toRR(y.v),toRR(x.v)))	    -- # typical value: atan2, QQ, ZZ, RR
	  is x:QQcell do toExpr(atan2(toRR(y.v),toRR(x.v)))	    -- # typical value: atan2, QQ, QQ, RR
     	  else WrongArg(1,"a number"))
     is y:RRicell do (
	  when xx
	  is x:RRcell do toExpr(atan2(y.v,toRRi(x.v)))			            -- # typical value: atan2, RRi, RR, RRi
	  is x:RRicell do toExpr(atan2(y.v,x.v))			            -- # typical value: atan2, RRi, RRi, RRi
	  is x:ZZcell do toExpr(atan2(y.v,toRRi(x.v,precision(y.v))))	    -- # typical value: atan2, RRi, ZZ, RRi
	  is x:QQcell do toExpr(atan2(y.v,toRRi(x.v,precision(y.v))))	    -- # typical value: atan2, RRi, QQ, RRi
	  else WrongArg(1,"a number"))
     else WrongArg(2,"a number")
     );
atan(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(atan(x.v))				    -- # typical value: atan, CC, CC
     is x:RRcell do toExpr(atan(x.v))				    -- # typical value: atan, RR, RR
     is x:RRicell do toExpr(atan(x.v))				    -- # typical value: atan, RRi, RRi
     is x:ZZcell do toExpr(atan(toRR(x.v)))			    -- # typical value: atan, ZZ, RR
     is x:QQcell do toExpr(atan(toRR(x.v)))	       -- # typical value: atan, QQ, RR
     is a:Sequence do if length(a) == 2 then buildErrorPacket("atan(x,y) has been replaced by atan2(y,x)")
     else WrongNumArgs(1)
     else buildErrorPacket("expected a number or a pair of numbers")
     );
setupfun("atan",atan);
atan2(e:Expr):Expr := (
     when e is s:Sequence do if length(s) == 2 then atan2(s.0,s.1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("atan2",atan2);
cosh(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(cosh(x.v))				    -- # typical value: cosh, CC, CC
     is x:RRcell do toExpr(cosh(x.v))				    -- # typical value: cosh, RR, RR
     is x:RRicell do toExpr(cosh(x.v))				    -- # typical value: cosh, RRi, RRi
     is x:ZZcell do toExpr(cosh(toRR(x.v)))			    -- # typical value: cosh, ZZ, RR
     is x:QQcell do toExpr(cosh(toRR(x.v)))			    -- # typical value: cosh, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("cosh",cosh);
sinh(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(sinh(x.v))				    -- # typical value: sinh, CC, CC
     is x:RRcell do toExpr(sinh(x.v))				    -- # typical value: sinh, RR, RR
     is x:RRicell do toExpr(sinh(x.v))				    -- # typical value: sinh, RRi, RRi
     is x:ZZcell do toExpr(sinh(toRR(x.v)))			    -- # typical value: sinh, ZZ, RR
     is x:QQcell do toExpr(sinh(toRR(x.v)))			    -- # typical value: sinh, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("sinh",sinh);
tanh(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(tanh(x.v))				    -- # typical value: tanh, CC, CC
     is x:RRcell do toExpr(tanh(x.v))				    -- # typical value: tanh, RR, RR
     is x:RRicell do toExpr(tanh(x.v))				    -- # typical value: tanh, RRi, RRi
     is x:ZZcell do toExpr(tanh(toRR(x.v)))			    -- # typical value: tanh, ZZ, RR
     is x:QQcell do toExpr(tanh(toRR(x.v)))			    -- # typical value: tanh, QQ, RR
     else buildErrorPacket("expected a number")
     );
setupfun("tanh",tanh);
exp(e:Expr):Expr := (
     when e
     is x:CCcell do toExpr(exp(x.v))
     is x:RRcell do toExpr(exp(x.v))
     is x:RRicell do toExpr(exp(x.v))
     is x:ZZcell do toExpr(exp(toRR(x.v)))
     is x:QQcell do toExpr(exp(toRR(x.v)))
     else buildErrorPacket("expected a number")
     );
setupfun("exp'",exp);
log(e:Expr):Expr := (
     when e
     is a:Sequence do if length(a) != 2 then WrongNumArgs(1,2) 
     else (
	  when a.0
	  is b:RRcell do (
	       when a.1
	       is x:CCcell do toExpr(log(b.v,x.v))			            -- # typical value: log, RR, CC, CC
	       is x:RRcell do (			            -- # typical value: log, RR, RR, CC
     	       	    if b.v>0 && x.v>0 then toExpr(log(b.v,x.v)) else toExpr(logc(b.v,x.v))
		    )
	       is x:RRicell do (                     -- # typical value: log, RR, RRi, RRi
     	       	    if b.v>0 && x.v>=0 then toExpr(log(toRRi(b.v,precision(b.v)),x.v))
                    else
                        buildErrorPacket("Not defined")
		    )
	       is x:ZZcell do (	    -- # typical value: log, RR, ZZ, RR
		    y := toRR(x.v,precision(b.v));
		    if b.v < 0 || y < 0 then toExpr(logc(b.v,y)) else toExpr(log(b.v,y)))
	       is x:QQcell do (	    -- # typical value: log, RR, QQ, RR
		    y := toRR(x.v,precision(b.v));
		    if b.v < 0 || y < 0 then toExpr(logc(b.v,y)) else toExpr(log(b.v,y)))
	       else WrongArg(1,"a number"))
	  is b:ZZcell do (
	       when a.1
	       is x:CCcell do ( -- # typical value: log, ZZ, CC, CC
		    toExpr(log(toRR(b.v,precision(x.v)),x.v))
		    )
	       is x:RRcell do (    -- -- # typical value: log, ZZ, RR, RR
		    c := toRR(b.v,precision(x.v));
		    if c>0 && x.v>0 then toExpr(log(c,x.v)) else toExpr(logc(c,x.v))		    
		    )
	       is x:RRicell do (      -- # typical value: log, ZZ, RRi, RRi
     	       	    if b.v>0 && x.v>=0 then toExpr(log(toRRi(b.v,precision(x.v)),x.v))
                    else
                        buildErrorPacket("Not defined")
		    )
	       is x:ZZcell do (	       -- # typical value: log, ZZ, ZZ, RR
		    if b.v>0 && x.v>0 then toExpr(log(toRR(b.v),toRR(x.v))) else toExpr(logc(toRR(b.v),toRR(x.v)))
		    )
	       is x:QQcell do (	       -- # typical value: log, ZZ, QQ, RR
		    if b.v>0 && x.v>0 then toExpr(log(toRR(b.v),toRR(x.v))) else toExpr(logc(toRR(b.v),toRR(x.v)))
		    )
	       else WrongArg(1,"a number"))
	  is b:QQcell do (
	       when a.1
	       is x:CCcell do ( -- # typical value: log, QQ, CC, CC
		    toExpr(log(toRR(b.v,precision(x.v)),x.v))
		    )
	       is x:RRcell do (    -- -- # typical value: log, QQ, RR, RR
		    c := toRR(b.v,precision(x.v));
		    if c>0 && x.v>0 then toExpr(log(c,x.v)) else toExpr(logc(c,x.v))		    
		    )
	       is x:RRicell do (      -- # typical value: log, QQ, RRi, RRi
     	       	    if b.v>0 && x.v>=0 then toExpr(log(toRRi(b.v,precision(x.v)),x.v))
                    else
                        buildErrorPacket("Not defined")
		    )
	       is x:ZZcell do (	       -- # typical value: log, QQ, ZZ, RR
		    if b.v>0 && x.v>0 then toExpr(log(toRR(b.v),toRR(x.v))) else toExpr(logc(toRR(b.v),toRR(x.v)))
		    )
	       is x:QQcell do (	       -- # typical value: log, QQ, QQ, RR
		    if b.v>0 && x.v>0 then toExpr(log(toRR(b.v),toRR(x.v))) else toExpr(logc(toRR(b.v),toRR(x.v)))
		    )
	       else WrongArg(1,"a number"))
    is b:RRicell do (
	       when a.1
                is x:RRcell do (    -- # typical value: log, RRi, RR, RRi
     	       	    if b.v>0 && x.v>=0 then toExpr(log(b.v,toRRi(x.v,precision(x.v))))
                                else
                        buildErrorPacket("Not defined")
		         )
	       is x:RRicell do (        -- # typical value: log, RRi, RRi, RRi
     	       	    if b.v>0 && x.v>=0 then toExpr(log(b.v,x.v))
                    else
                        buildErrorPacket("Not defined")
		    )
	       is x:ZZcell do (          -- # typical value: log, RRi, ZZ, RRi
		    y := toRRi(x.v,precision(b.v));
		            if b.v>0 && x.v>=0 then toExpr(log(b.v,y))
                    else
                        buildErrorPacket("Not defined")
		    )
	       is x:QQcell do (          -- # typical value: log, RRi, QQ, RRi
		    y := toRRi(x.v,precision(b.v));
		            if b.v>0 && y>=0 then toExpr(log(b.v,y))
                    else
                        buildErrorPacket("Not defined")
                           )
	       else WrongArg(1,"a number"))
	  else WrongArg(2,"a number"))
     is x:CCcell do toExpr(log(x.v))				    -- # typical value: log, CC, CC
     is x:RRcell do if isNegative(x.v) then toExpr(logc(x.v)) else toExpr(log(x.v))				    -- # typical value: log, RR, RR
    is x:RRicell do if x.v >= 0 then toExpr(log(x.v))  -- # typical value: log, RRi, RRi
                     else buildErrorPacket("Not defined")
     is x:ZZcell do if x.v<0 then toExpr(logc(toRR(x.v))) else toExpr(log(toRR(x.v)))			    -- # typical value: log, ZZ, RR
     is x:QQcell do if x.v<0 then toExpr(logc(toRR(x.v))) else toExpr(log(toRR(x.v)))			    -- # typical value: log, QQ, RR
     else WrongArg("a number or a pair of numbers")
     );
setupfun("log",log);
agm(e:Expr):Expr := (
     when e
     is a:Sequence do if length(a) != 2 then WrongNumArgs(2) 
     else (
	  when a.0
	  is x:CCcell do (
	       when a.1
	       is y:CCcell do toExpr(agm(x.v,y.v)) -- # typical value: agm, CC, CC, CC
	       is y:RRcell do toExpr(agm(x.v,toCC(y.v)))			            -- # typical value: agm, CC, RR, CC
	       is y:ZZcell do toExpr(agm(x.v,toCC(y.v,precision(x.v))))	    -- # typical value: agm, CC, ZZ, CC
	       is y:QQcell do toExpr(agm(x.v,toCC(y.v,precision(x.v))))	    -- # typical value: agm, CC, QQ, CC
	       else WrongArg(1,"a number"))
	  is x:RRcell do (
	       when a.1
	       is y:CCcell do toExpr(agm(toCC(x.v),y.v)) -- # typical value: agm, RR, CC, CC
	       is y:RRcell do toExpr(agm(x.v,y.v))			            -- # typical value: agm, RR, RR, RR
	       is y:ZZcell do toExpr(agm(x.v,toRR(y.v,precision(x.v))))	    -- # typical value: agm, RR, ZZ, RR
	       is y:QQcell do toExpr(agm(x.v,toRR(y.v,precision(x.v))))	    -- # typical value: agm, RR, QQ, RR
	       else WrongArg(1,"a number"))
	  is x:ZZcell do (
	       when a.1
	       is y:CCcell do toExpr(agm(toCC(x.v,precision(y.v)),y.v))    -- -- # typical value: agm, ZZ, CC, CC
	       is y:RRcell do toExpr(agm(toRR(x.v,precision(y.v)),y.v))    -- -- # typical value: agm, ZZ, RR, RR
	       is y:ZZcell do toExpr(agm(toRR(x.v),toRR(y.v)))	       -- # typical value: agm, ZZ, ZZ, RR
	       is y:QQcell do toExpr(agm(toRR(x.v),toRR(y.v)))	       -- # typical value: agm, ZZ, QQ, RR
	       else WrongArg(1,"a number"))
	  is x:QQcell do (
	       when a.1
	       is y:CCcell do toExpr(agm(toCC(x.v,precision(y.v)),y.v))    -- # typical value: agm, QQ, CC, CC
	       is y:RRcell do toExpr(agm(toRR(x.v,precision(y.v)),y.v))    -- # typical value: agm, QQ, RR, RR
	       is y:ZZcell do toExpr(agm(toRR(x.v),toRR(y.v)))	    -- # typical value: agm, QQ, ZZ, RR
	       is y:QQcell do toExpr(agm(toRR(x.v),toRR(y.v)))	    -- # typical value: agm, QQ, QQ, RR
	       else WrongArg(1,"a number"))
	  else WrongArg(2,"a number"))
     else WrongNumArgs(2)
     );
setupfun("agm",agm);
-- abs(x:double):double := if x.v < 0. then -x.v else x.v;
floor(e:Expr):Expr := (
     when e
     is x:RRcell do (
	  if isnan(x.v) then buildErrorPacket("encountered NotANumber in conversion to integer") else
	  if isinf(x.v) then buildErrorPacket("encountered infinite real number in conversion to integer") else
	  toExpr(floor(x.v))
	  )
     is x:RRicell do (
	  if isnan(x.v) then buildErrorPacket("encountered NotANumber in conversion to integer") else
	  if isinf(x.v) then buildErrorPacket("encountered infinite real number in conversion to integer") else
	  toExpr(floor(x.v))
	  )
     is x:CCcell do (
	  if isnan(x.v) then buildErrorPacket("encountered NotANumber in conversion to integer") else
	  if isinf(x.v) then buildErrorPacket("encountered infinite real number in conversion to integer") else
	  toExpr(floor(x.v.re))
	  )
     is x:QQcell do toExpr(floor(x.v))
     is ZZcell do e
     else buildErrorPacket("expected an integral, rational, or real number")
     );
setupfun("floor0",floor);

round0(e:Expr):Expr := (
     when e
     is x:RRcell do (
	  if isnan(x.v) then buildErrorPacket("encountered NotANumber in conversion to integer") else
	  if isinf(x.v) then buildErrorPacket("encountered infinite real number in conversion to integer") else
	  toExpr(round(x.v)))
     is x:CCcell do (
	  if isnan(x.v) then buildErrorPacket("encountered NotANumber in conversion to integer") else
	  if isinf(x.v) then buildErrorPacket("encountered infinite real number in conversion to integer") else
	  toExpr(round(x.v.re)))
    is x:RRicell do (
	  if isnan(x.v) then buildErrorPacket("encountered NotANumber in conversion to integer") else
	  if isinf(x.v) then buildErrorPacket("encountered infinite real number in conversion to integer") else
	  toExpr(round(x.v)))
     else buildErrorPacket("expected a real number")
     );
setupfun("round0",round0);

run(e:Expr):Expr := (
     when e
     is x:stringCell do toExpr(run(x.v))
     else buildErrorPacket("expected a string")
     );
setupfun("run",run);

header "#include <math.h>";

sqrt(a:Expr):Expr := (
     when a
     is x:ZZcell do (
	  if x.v < 0
	  then toExpr(toCC(0,sqrt(-toRR(x.v))))
	  else toExpr(sqrt(toRR(x.v)))			       -- # typical value: sqrt, ZZ, CC
	  )
     is x:QQcell do (
	  if x.v < 0
	  then toExpr(toCC(0,sqrt(-toRR(x.v))))
	  else toExpr(sqrt(toRR(x.v)))			       -- # typical value: sqrt, QQ, CC
	  )
     is x:RRcell do (
	  if x.v < 0
	  then toExpr(toCC(0,sqrt(-x.v)))
	  else toExpr(sqrt(x.v))			       -- # typical value: sqrt, RR, CC
	  )
     is x:RRicell do (
	  if leftRR(x.v) >= 0
	  then toExpr(sqrt(x.v))                   -- # typical value: sqrt, RRi, RRi
	  else buildErrorPacket("Not implemented")
	  )
     is x:CCcell do toExpr(sqrt(x.v))				    -- # typical value: sqrt, CC, CC
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
			      values.0 = toExpr(i);
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
			      values.0 = toExpr(i);
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
		    tmp := fn(toExpr(i));
		    when tmp is Error do (
			 errret = tmp;
			 while true do provide nullE; )
		    else provide tmp;))
	  is cf:CompiledFunctionClosure do (	  -- compiled code closure
	       fn := cf.fn;
	       env := cf.env;
	       for i from 0 to newlen-1 do (
		    tmp := fn(toExpr(i),env);
		    when tmp is Error do (
			 errret = tmp;
			 while true do provide nullE; )
		    else provide tmp;))
	  is s:SpecialExpr do (
	       fn := s.e;
	       for i from 0 to newlen-1 do (
		    tmp := applyEE(fn,toExpr(i));
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
--	  if obj.Mutable then return WrongArg("an immutable hash table");
--	  if ancestor(obj.Class,VirtualTally) then mapkeys(f,obj) else mapvalues(f,obj))
     is b:List do (
	  c := map(b.v,f);
	  when c is err:Error do if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else c
	  is v:Sequence do list(b.Class,v,b.Mutable)
	  else nullE			  -- will not happen
	  )
     is i:ZZcell do (
	  if !isInt(i)
	  then WrongArgSmallInteger()
	  else map(toInt(i),f))
     is s:stringCell do map(strtoseq(s), f)
     else WrongArg(1,"a list, sequence, integer, or string"));
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
	       is v:Sequence do list(b2.Class,v,b2.Mutable)
	       else nullE		  -- will not happen
	       )
	  is s2:stringCell do map(a1, strtoseq(s2), f)
	  else WrongArg(2,"a list, sequence, or string"))
     is b1:List do (
	  when e2
	  is a2:Sequence do (
	       c := map(b1.v,a2,f);
	       when c is err:Error do if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else c
	       is v:Sequence do list(b1.Class,v,b1.Mutable)
	       else nullE		  -- will not happen
	       )
	  is b2:List do (
	       mutable := b1.Mutable;
	       class := b1.Class;
	       if class != b2.Class then (
		    mutable = false;
		    class = listClass;
		    );
	       c := map(b1.v,b2.v,f);
	       when c is err:Error do if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else c
	       is v:Sequence do list(class,v,mutable)
	       else nullE		  -- will not happen
	       )
	  is s2:stringCell do map(b1.v, strtoseq(s2), f)
	  else WrongArg(2,"a list, sequence, or string"))
     is s1:stringCell do (
	  when e2
	  is a2:Sequence do map(strtoseq(s1), a2, f)
	  is b2:List do map(strtoseq(s1), b2.v, f)
	  is s2:stringCell do map(strtoseq(s1), strtoseq(s2), f)
	  else WrongArg(2, "a list, sequence, or string"))
     else WrongArg(1,"a list, sequence, or string"));
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
		    values.0 = toExpr(i);
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
		    values.0 = toExpr(i);
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
	       tmp := fn(toExpr(i));
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
	       tmp := fn(toExpr(i),env);
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
     is i:ZZcell do (
	  if !isInt(i)
	  then WrongArgSmallInteger(1)
	  else scan(toInt(i),f))
     is s:stringCell do scan(strtoseq(s), f)
     else buildErrorPacket("scan expects a list, sequence, integer, or string"));
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
     is a:ZZcell do (
	  when y
	  is b:ZZcell do toExpr(gcd(a.v,b.v))
	  else buildErrorPacket("expected an integer"))
     else buildErrorPacket("expected an integer"));
gcdfun(e:Expr):Expr := accumulate(plus0,plus1,gcd,e);
setupfun("gcd0",gcdfun);

toSequence(e:Expr):Expr := (
     when e
     is Sequence do e
     is b:List do (
	  if b.Mutable
	  then Expr(new Sequence len length(b.v) do foreach i in b.v do provide i)
	  else Expr(b.v)
	  )
     is s:stringCell do Expr(strtoseq(s))
     else WrongArg("a list, sequence, or string"));
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
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d actors3.o "
-- End:
