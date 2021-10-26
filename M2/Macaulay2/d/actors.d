--		Copyright 1994 by Daniel R. Grayson

use evaluate;
use struct;

export plus0():Expr := zeroE;
export times0():Expr := oneE;
export plus1(e:Expr) : Expr := e;
times1 := plus1;

export (lhs:Expr) + (rhs:Expr) : Expr := (
     when lhs
     is x:ZZcell do (
	  when rhs
	      is y:ZZcell do toExpr(x.v + y.v)			    -- # typical value: symbol +, ZZ, ZZ, ZZ
     	  is y:QQcell do toExpr(x.v + y.v)			    -- # typical value: symbol +, ZZ, QQ, QQ
     	  is y:RRcell do toExpr(y.v + x.v)			    -- # typical value: symbol +, ZZ, RR, RR
          is y:RRicell do toExpr(y.v + x.v)             -- # typical value: symbol +, ZZ, RRi, RRi
     	  is y:CCcell do toExpr(toRR(x.v,precision(y.v.re)) + y.v)	    -- # typical value: symbol +, ZZ, CC, CC
	      is Error do rhs
	      else binarymethod(lhs,rhs,PlusS))
     is x:QQcell do (
	  when rhs
	      is y:ZZcell do toExpr(x.v + y.v)			    -- # typical value: symbol +, QQ, ZZ, QQ
     	  is y:QQcell do toExpr(x.v + y.v)			    -- # typical value: symbol +, QQ, QQ, QQ
     	  is y:RRcell do toExpr(y.v + x.v)			    -- # typical value: symbol +, QQ, RR, RR
          is y:RRicell do toExpr(y.v + x.v)             -- # typical value: symbol +, QQ, RRi, RRi
     	  is y:CCcell do toExpr(toRR(x.v,precision(y.v.re)) + y.v)	    -- # typical value: symbol +, QQ, CC, CC
	      is Error do rhs
	  else binarymethod(lhs,rhs,PlusS))
     is x:RawRingElementCell do (
	  when rhs
	  is y:RawRingElementCell do (			    -- # typical value: symbol +, RawRingElement, RawRingElement, RawRingElement
	       when x.p+y.p
	       is t:RawRingElement do toExpr(t)
	       is null do buildErrorPacket(EngineError("polynomial addition failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,PlusS))
     is x:RRcell do (
	  when rhs
	      is y:ZZcell do toExpr(x.v + y.v)			    -- # typical value: symbol +, RR, ZZ, RR
     	  is y:QQcell do toExpr(x.v + y.v)			    -- # typical value: symbol +, RR, QQ, RR
     	  is y:RRcell do toExpr(x.v + y.v)			    -- # typical value: symbol +, RR, RR, RR
	      is y:RRicell do toExpr(y.v + x.v)             -- # typical value: symbol +, RR, RRi, RRi
     	  is y:CCcell do toExpr(x.v + y.v)			    -- # typical value: symbol +, RR, CC, CC
	      is Error do rhs
	      else binarymethod(lhs,rhs,PlusS))
     is x:RRicell do (
        when rhs
	     is y:ZZcell do toExpr(x.v + y.v)               -- # typical value: symbol +, RRi, ZZ, RRi
	     is y:QQcell do toExpr(x.v + y.v)               -- # typical value: symbol +, RRi, QQ, RRi
	     is y:RRcell do toExpr(x.v + y.v)               -- # typical value: symbol +, RRi, RR, RRi
         is y:RRicell do toExpr(x.v + y.v)              -- # typical value: symbol +, RRi, RRi, RRi
	     is Error do rhs
	     else binarymethod(lhs,rhs,PlusS))
     is x:CCcell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v + toRR(y.v,precision(x.v.re)))	    -- # typical value: symbol +, CC, ZZ, CC
     	  is y:QQcell do toExpr(x.v + toRR(y.v,precision(x.v.re)))	    -- # typical value: symbol +, CC, QQ, CC
     	  is y:RRcell do toExpr(x.v + y.v)			    -- # typical value: symbol +, CC, RR, CC
     	  is y:CCcell do toExpr(x.v + y.v)			    -- # typical value: symbol +, CC, CC, CC
	  is Error do rhs
	  else binarymethod(lhs,rhs,PlusS))
     is x:RawMatrixCell do (
	  when rhs
	  is y:RawMatrixCell do (				    -- # typical value: symbol +, RawMatrix, RawMatrix, RawMatrix
	       when x.p+y.p
	       is t:RawMatrix do Expr(RawMatrixCell(t))
	       is null do buildErrorPacket(EngineError("matrix addition failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,PlusS))
     is x:RawMutableMatrixCell do (
	  when rhs
	  is y:RawMutableMatrixCell do (			    -- # typical value: symbol +, RawMutableMatrix, RawMutableMatrix, RawMutableMatrix
	       when x.p+y.p
	       is t:RawMutableMatrix do Expr(RawMutableMatrixCell(t))
	       is null do buildErrorPacket(EngineError("mutable matrix addition failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,PlusS))
     is x:RawMonomialIdealCell do (
	  when rhs
	  is y:RawMonomialIdealCell do (			    -- # typical value: symbol +, RawMonomialIdeal, RawMonomialIdeal, RawMonomialIdeal
	       when x.p+y.p
	       is t:RawMonomialIdeal do toExpr(t)
	       is null do buildErrorPacket(EngineError("monomial ideal addition failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,PlusS))
     is Error do lhs
     else binarymethod(lhs,rhs,PlusS));
plus(e:Expr):Expr := accumulate(plus0,plus1,op+,e);
setupfun("plus",plus);

plusfun1(rhs:Code):Expr := (
     r := eval(rhs);
     when r
     is Error do r
     is ZZcell do r						    -- # typical value: symbol +, ZZ, ZZ
     is RRcell do r						    -- # typical value: symbol +, RR, RR
     is RRicell do r                        -- # typical value: symbol +, RRi, RRi
     is CCcell do r						    -- # typical value: symbol +, CC, CC
     is QQcell do r						    -- # typical value: symbol +, QQ, QQ
     is RawRingElementCell do r				    -- # typical value: symbol +, RawRingElement, RawRingElement
     is RawMatrixCell do r					    -- # typical value: symbol +, RawMatrix, RawMatrix
     is RawMutableMatrixCell do r				    -- # typical value: symbol +, RawMutableMatrix, RawMutableMatrix
     else unarymethod(rhs,PlusS));
plusfun(lhs:Code,rhs:Code):Expr := (
     l := eval(lhs);
     when l is Error do l
     else (
     	  r := eval(rhs);
     	  when r is Error do r
	  else l+r));
setup(PlusS,plusfun1,plusfun);
export - (rhs:Expr) : Expr := (
     when rhs
     is x:ZZcell do toExpr(-x.v)				    -- # typical value: symbol -, ZZ, ZZ                            
     is x:RRcell do toExpr(-x.v)				    -- # typical value: symbol -, RR, RR
     is x:RRicell do toExpr(-x.v)                   -- # typical value: symbol -, RRi, RRi
     is x:CCcell do toExpr(-x.v)				    -- # typical value: symbol -, CC, CC                            
     is x:QQcell do toExpr(-x.v)				    -- # typical value: symbol -, QQ, QQ                            
     is x:RawRingElementCell do toExpr(-x.p)			    -- # typical value: symbol -, RawRingElement, RawRingElement    
     is x:RawMatrixCell do (				    -- # typical value: symbol -, RawMatrix, RawMatrix              
	  when -x.p is y:RawMatrix do toExpr(y) else buildErrorPacket(EngineError("polynomial minus failed"))
	  )
     is x:RawMutableMatrixCell do toExpr(-x.p)                      -- # typical value: symbol -, RawMutableMatrix, RawMutableMatrix
     is Error do rhs
     else (
	  method := lookup(Class(rhs),MinusS);
	  if method == nullE
	  then buildErrorPacket("no method found")
	  else applyEE(method,Expr(rhs))));
minusfun1(rhs:Code):Expr := - eval(rhs);

export (lhs:Expr) - (rhs:Expr) : Expr := (
     when lhs
     is x:ZZcell do (
	  when rhs
	      is y:ZZcell do toExpr(x.v - y.v)			    -- # typical value: symbol -, ZZ, ZZ, ZZ
     	  is y:QQcell do toExpr(x.v - y.v)			    -- # typical value: symbol -, ZZ, QQ, QQ
	      is y:RRcell do toExpr(toRR(x.v,precision(y.v)) - y.v)		    -- # typical value: symbol -, ZZ, RR, RR
          is y:RRicell do toExpr(toRRi(x.v,precision(y.v)) - y.v)       -- # typical value: symbol -, ZZ, RRi, RRi
	      is y:CCcell do toExpr(toRR(x.v,precision(y.v.re)) - y.v)	    -- # typical value: symbol -, ZZ, CC, CC
	      is Error do rhs
	      else binarymethod(lhs,rhs,MinusS))
     is x:QQcell do (
      when rhs
	      is y:ZZcell do toExpr(x.v - y.v)			    -- # typical value: symbol -, QQ, ZZ, QQ
     	  is y:QQcell do toExpr(x.v - y.v)			    -- # typical value: symbol -, QQ, QQ, QQ
	      is y:RRcell do toExpr(toRR(x.v,precision(y.v)) - y.v)		    -- # typical value: symbol -, QQ, RR, RR
          is y:RRicell do toExpr(toRRi(x.v,precision(y.v)) - y.v)       -- # typical value: symbol -, QQ, RRi, RRi
	      is y:CCcell do toExpr(toRR(x.v,precision(y.v.re)) - y.v)	    -- # typical value: symbol -, QQ, CC, CC
	      is Error do rhs
	      else binarymethod(lhs,rhs,MinusS))
     is x:RawRingElementCell do (
	  when rhs
	      is y:RawRingElementCell do (			    -- # typical value: symbol -, RawRingElement, RawRingElement, RawRingElement
	       when x.p-y.p
	       is t:RawRingElement do toExpr(t)
	       is null do buildErrorPacket(EngineError("polynomial subtraction failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,MinusS))
     is x:RRcell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v - y.v)			    -- # typical value: symbol -, RR, ZZ, RR
     	  is y:QQcell do toExpr(x.v - y.v)			    -- # typical value: symbol -, RR, QQ, RR
	  is y:RRcell do toExpr(x.v - y.v)			    -- # typical value: symbol -, RR, RR, RR
      is y:RRicell do toExpr(toRRi(x.v) - y.v)     -- # typical value: symbol -, RR, RRi, RRi
	  is y:CCcell do toExpr(x.v - y.v)			    -- # typical value: symbol -, RR, CC, CC
	  is Error do rhs
	  else binarymethod(lhs,rhs,MinusS))
    is x:RRicell do (
      when rhs
	       is y:ZZcell do toExpr(x.v - y.v) -- # typical value: symbol -, RRi, ZZ, RRi
	       is y:QQcell do toExpr(x.v - y.v) -- # typical value: symbol -, RRi, QQ, RRi
	       is y:RRcell do toExpr(x.v - y.v) -- # typical value: symbol -, RRi, RR, RRi
           is y:RRicell do toExpr(x.v - y.v) -- # typical value: symbol -, RRi, RRi, RRi
	       is Error do rhs
	       else binarymethod(lhs,rhs,MinusS))
     is x:CCcell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v - toRR(y.v,precision(x.v.re)))	    -- # typical value: symbol -, CC, ZZ, CC
     	  is y:QQcell do toExpr(x.v - toRR(y.v,precision(x.v.re)))	    -- # typical value: symbol -, CC, QQ, CC
	  is y:RRcell do toExpr(x.v - y.v)			    -- # typical value: symbol -, CC, RR, CC
	  is y:CCcell do toExpr(x.v - y.v)			    -- # typical value: symbol -, CC, CC, CC
	  is Error do rhs
	  else binarymethod(lhs,rhs,MinusS))
     is x:RawMatrixCell do (
	  when rhs
	  is y:RawMatrixCell do (				    -- # typical value: symbol -, RawMatrix, RawMatrix, RawMatrix
	       when x.p-y.p
	       is t:RawMatrix do toExpr(t)
	       is null do buildErrorPacket(EngineError("matrix subtraction failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,MinusS))
     is x:RawMutableMatrixCell do (				    -- # typical value: symbol -, RawMutableMatrix, RawMutableMatrix, RawMutableMatrix
	  when rhs
	  is y:RawMutableMatrixCell do (
	       when x.p-y.p
	       is t:RawMutableMatrix do toExpr(t)
	       is null do buildErrorPacket(EngineError("matrix subtraction failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,MinusS))
     is x:RawMonomialIdealCell do (				    -- # typical value: symbol -, RawMonomialIdeal, RawMonomialIdeal, RawMonomialIdeal
	  when rhs
	  is y:RawMonomialIdealCell do (
	       when x.p-y.p
	       is t:RawMonomialIdeal do toExpr(t)
	       is null do buildErrorPacket(EngineError("monomial ideal difference failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,PlusS))
     is Error do lhs
     else binarymethod(lhs,rhs,MinusS));
minusfun2(lhs:Code,rhs:Code):Expr := (
     l := eval(lhs);
     when l is Error do l
     else (
     	  r := eval(rhs);
     	  when r is Error do r
	  else l-r));
setup(MinusS,minusfun1,minusfun2);
minusfun(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 1 then -a.0
	  else WrongNumArgs(1))
     else -e);
setupfun("minus",minusfun);
differencefun(e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then a.0-a.1
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
setupfun("difference",differencefun);

export (lhs:Expr) * (rhs:Expr) : Expr := (
     when lhs
     is x:ZZcell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v * y.v)			    -- # typical value: symbol *, ZZ, ZZ, ZZ
     	  is y:QQcell do toExpr(x.v * y.v)			    -- # typical value: symbol *, ZZ, QQ, QQ
     	  is y:RRcell do toExpr(toRR(x.v,precision(y.v)) * y.v)		    -- # typical value: symbol *, ZZ, RR, RR
          is y:RRicell do toExpr(y.v * x.v)     -- # typical value: symbol *, ZZ, RRi, RRi
     	  is y:CCcell do toExpr(x.v * y.v)	    -- # typical value: symbol *, ZZ, CC, CC
	  is Error do rhs
	  else binarymethod(lhs,rhs,StarS))
     is x:QQcell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v * y.v)			    -- # typical value: symbol *, QQ, ZZ, QQ
     	  is y:QQcell do toExpr(x.v * y.v)			    -- # typical value: symbol *, QQ, QQ, QQ
     	  is y:RRcell do toExpr(y.v * x.v)			    -- # typical value: symbol *, QQ, RR, RR
          is y:RRicell do toExpr(y.v * x.v)             -- # typical value: symbol *, QQ, RRi, RRi
     	  is y:CCcell do toExpr(y.v * toRR(x.v,precision(y.v.re)))	    -- # typical value: symbol *, QQ, CC, CC
	  is Error do rhs
	  else binarymethod(lhs,rhs,StarS))
     is x:RawRingElementCell do (
	  when rhs
	  is y:RawRingElementCell do (			    -- # typical value: symbol *, RawRingElement, RawRingElement, RawRingElement
	       when x.p*y.p
	       is t:RawRingElement do toExpr(t)
	       is null do buildErrorPacket(EngineError("polynomial multiplication failed"))
	       )
	  is y:RawMatrixCell do (				    -- # typical value: symbol *, RawRingElement, RawMatrix, RawMatrix
	       when x.p*y.p
	       is t:RawMatrix do toExpr(t)
	       is null do buildErrorPacket(EngineError("matrix scalar multiplication failed"))
	       )
	  is y:RawMutableMatrixCell do (			    -- # typical value: symbol *, RawRingElement, RawMutableMatrix, RawMatrix
	       when x.p*y.p
	       is t:RawMutableMatrix do toExpr(t)
	       is null do buildErrorPacket(EngineError("matrix scalar multiplication failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,StarS))
     is x:RRcell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v * y.v)			    -- # typical value: symbol *, RR, ZZ, RR
     	  is y:QQcell do toExpr(x.v * y.v)			    -- # typical value: symbol *, RR, QQ, RR
     	  is y:RRcell do toExpr(x.v * y.v)			    -- # typical value: symbol *, RR, RR, RR
          is y:RRicell do toExpr(y.v * x.v)             -- # typical value: symbol *, RR, RRi, RRi
     	  is y:CCcell do toExpr(x.v * y.v)			    -- # typical value: symbol *, RR, CC, CC
	  is Error do rhs
	  else binarymethod(lhs,rhs,StarS))
      is x:RRicell do (
      when rhs
	       is y:ZZcell do toExpr(x.v * y.v)  -- # typical value: symbol *, RRi, ZZ, RRi
	       is y:QQcell do toExpr(x.v * y.v)  -- # typical value: symbol *, RRi, QQ, RRi
	       is y:RRcell do toExpr(x.v * y.v)  -- # typical value: symbol *, RRi, RR, RRi
           is y:RRicell do toExpr(x.v * y.v) -- # typical value: symbol *, RRi, RRi, RRi
	       is Error do rhs
	       else binarymethod(lhs,rhs,StarS))
    is x:CCcell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v * y.v)	    -- # typical value: symbol *, CC, ZZ, CC
     	  is y:QQcell do toExpr(x.v * toRR(y.v,precision(x.v.re)))	    -- # typical value: symbol *, CC, QQ, CC
     	  is y:RRcell do toExpr(y.v * x.v)			    -- # typical value: symbol *, CC, RR, CC
     	  is y:CCcell do toExpr(y.v * x.v)			    -- # typical value: symbol *, CC, CC, CC
	  is Error do rhs
	  else binarymethod(lhs,rhs,StarS))
     is x:RawMonomialCell do (
	  when rhs
	  is y:RawMonomialCell do (				    -- # typical value: symbol *, RawMonomialIdeal, RawMonomial, RawMonomial
	       when x.p*y.p
	       is z:RawMonomial do toExpr(z)
	       is null do buildErrorPacket(EngineError("monomial multiplication overflow"))
	       )
	  else binarymethod(lhs,rhs,StarS))
     is x:RawMonomialIdealCell do (				    -- # typical value: symbol *, RawMonomialIdeal, RawMonomialIdeal, RawMonomialIdeal
	  when rhs
	  is y:RawMonomialIdealCell do (
	       when x.p*y.p
	       is z:RawMonomialIdeal do toExpr(z)
	       is null do buildErrorPacket(EngineError("monomial ideal multiplication failed"))
	       )
	  else binarymethod(lhs,rhs,StarS))
     is x:RawMatrixCell do (				    -- # typical value: symbol *, RawMatrix, RawRingElement, RawRingElement
	  when rhs
	  is y:RawRingElementCell do (
	       when x.p*y.p
	       is t:RawMatrix do toExpr(t)
	       is null do buildErrorPacket(EngineError("matrix scalar multiplication failed"))
	       )
	  is y:RawMatrixCell do (				    -- # typical value: symbol *, RawMatrix, RawMatrix, RawMatrix
	       when x.p*y.p
	       is t:RawMatrix do toExpr(t)
	       is null do buildErrorPacket(EngineError("matrix multiplication failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,StarS))
     is x:RawMutableMatrixCell do (
	  when rhs
	  is y:RawRingElementCell do (			    -- # typical value: symbol *, RawMutableMatrix, RawRingElement, RawMutableMatrix
	       when x.p*y.p
	       is t:RawMutableMatrix do toExpr(t)
	       is null do buildErrorPacket(EngineError("matrix scalar multiplication failed"))
	       )
	  is y:RawMutableMatrixCell do (			    -- # typical value: symbol *, RawMutableMatrix, RawMutableMatrix, RawMutableMatrix
	       when x.p*y.p
	       is t:RawMutableMatrix do toExpr(t)
	       is null do buildErrorPacket(EngineError("matrix multiplication failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,StarS))
     is Error do lhs
     else (
	  when rhs is Error do rhs
	  else binarymethod(lhs,rhs,StarS)));
times(e:Expr):Expr := accumulate(times0,times1,op*,e);
setupfun("times",times);

export (lhs:Expr) / (rhs:Expr) : Expr := (
     when lhs
     is x:ZZcell do (
	  when rhs
	  is y:ZZcell do (					    -- # typical value: symbol /, ZZ, ZZ, ZZ
	       if y.v === 0
	       then buildErrorPacket("division by zero")
	       else toExpr(x.v / y.v))
     	  is y:QQcell do (					    -- # typical value: symbol /, ZZ, QQ, QQ
	       if y.v === 0
	       then buildErrorPacket("division by zero")
	       else toExpr(x.v / y.v))
     	  is y:RRcell do (					    -- # typical value: symbol /, ZZ, RR, RR
	       toExpr(toRR(x.v,precision(y.v)) / y.v))
          is y:RRicell do (toExpr(x.v / y.v))   -- # typical value: symbol /, ZZ, RRi, RRi
     	  is y:CCcell do (					    -- # typical value: symbol /, ZZ, CC, CC
	       toExpr(x.v / y.v))
	  is Error do rhs
	  else binarymethod(lhs,rhs,DivideS))
     is x:QQcell do (
	  when rhs
	  is y:ZZcell do (					    -- # typical value: symbol /, QQ, ZZ, QQ
	       if y.v === 0
	       then buildErrorPacket("division by zero")
	       else toExpr(x.v / y.v))
     	  is y:QQcell do (					    -- # typical value: symbol /, QQ, QQ, QQ
	       if y.v === 0
	       then buildErrorPacket("division by zero")
	       else toExpr(x.v / y.v))
     	  is y:RRcell do (					    -- # typical value: symbol /, QQ, RR, RR
	       toExpr(toRR(x.v,precision(y.v)) / y.v))
          is y:RRicell do (toExpr(x.v / y.v))   -- # typical value: symbol /, QQ, RRi, RRi
     	  is y:CCcell do (					    -- # typical value: symbol /, QQ, CC, CC
	       if y.v === 0 then buildErrorPacket("division by zero") else
	       toExpr(toRR(x.v,precision(y.v.re)) / y.v))
	  is Error do rhs
	  else binarymethod(lhs,rhs,DivideS))
     is x:RRcell do (
	  when rhs
	  is y:ZZcell do (					    -- # typical value: symbol /, RR, ZZ, RR
	       if y.v === 0
	       then buildErrorPacket("division by zero")
	       else toExpr(x.v / y.v))
     	  is y:QQcell do (					    -- # typical value: symbol /, RR, QQ, RR
	       if y.v === 0
	       then buildErrorPacket("division by zero")
	       else toExpr(x.v / y.v))
     	  is y:RRcell do (					    -- # typical value: symbol /, RR, RR, RR
	       toExpr(x.v / y.v))
          is y:RRicell do (toExpr(x.v / y.v))   -- # typical value: symbol /, RR, RRi, RRi
     	  is y:CCcell do (					    -- # typical value: symbol /, RR, CC, CC
	       toExpr(x.v / y.v))
	  is Error do rhs
	  else binarymethod(lhs,rhs,DivideS))
    is x:RRicell do (
      when rhs
	       is y:ZZcell do (                     -- # typical value: symbol /, RRi, ZZ, RRi
	         if y.v === 0
	         then buildErrorPacket("division by zero")
	         else toExpr(x.v / y.v))
           is y:QQcell do (                      -- # typical value: symbol /, RRi, QQ, RRi
	         if y.v === 0
	         then buildErrorPacket("division by zero")
	         else toExpr(x.v / y.v))
           is y:RRcell do (toExpr(x.v / y.v))    -- # typical value: symbol /, RRi, RR, RRi
           is y:RRicell do (toExpr(x.v / y.v))   -- # typical value: symbol /, RRi, RRi, RRi
	       is Error do rhs
	       else binarymethod(lhs,rhs,DivideS))
     is x:CCcell do (
	  when rhs
	  is y:ZZcell do (					    -- # typical value: symbol /, CC, ZZ, CC
	       if y.v === 0
	       then buildErrorPacket("division by zero")
	       else toExpr(x.v / toRR(y.v,precision(x.v.re)))
	       )
     	  is y:QQcell do (					    -- # typical value: symbol /, CC, QQ, CC
	       if y.v === 0
	       then buildErrorPacket("division by zero")
	       else toExpr(x.v / toRR(y.v,precision(x.v.re)))
	       )
     	  is y:RRcell do (					    -- # typical value: symbol /, CC, RR, CC
	       toExpr(x.v / y.v))
     	  is y:CCcell do (					    -- # typical value: symbol /, CC, CC, CC
	       toExpr(x.v / y.v))
	  is Error do rhs
	  else binarymethod(lhs,rhs,DivideS))
     is x:RawMonomialCell do (
	  when rhs
	  is y:RawMonomialCell do (				    -- # typical value: symbol /, RawMonomial, RawMonomial, RawMonomial
	       when x.p/y.p
	       is z:RawMonomial do toExpr(z)
	       is null do buildErrorPacket(EngineError("monomial division overflow"))
	       )
	  else binarymethod(lhs,rhs,DivideS))
     is Error do lhs
     else binarymethod(lhs,rhs,DivideS));
divideC(lhs:Code,rhs:Code):Expr := (
     l := eval(lhs);
     when l is Error do l
     else (
     	  r := eval(rhs);
     	  when r is Error do r
	  else l/r));
setup(DivideS,divideC);
export (lhs:Expr) // (rhs:Expr) : Expr := (
     when lhs
     is x:ZZcell do (
	  when rhs
	  is y:ZZcell do (					    -- # typical value: symbol //, ZZ, ZZ, ZZ
	       if y.v === 0
	       then zeroE
	       else toExpr(x.v//y.v)
	       )
     	  is Error do rhs
	  else binarymethod(lhs,rhs,SlashSlashS))
     is x:RawRingElementCell do (
	  when rhs
	  is y:RawRingElementCell do (			    -- # typical value: symbol //, RawRingElement, RawRingElement, RawRingElement
	       when x.p//y.p
	       is t:RawRingElement do toExpr(t)
	       is null do buildErrorPacket(EngineError("polynomial division failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,SlashSlashS))
     is Error do lhs
     else binarymethod(lhs,rhs,SlashSlashS));
quotientC(lhs:Code,rhs:Code):Expr := (
     l := eval(lhs);
     when l is Error do l
     else (
     	  r := eval(rhs);
     	  when r is Error do r
	  else l//r));
setup(SlashSlashS,quotientC);

BackslashBackslashFun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,BackslashBackslashS);
setup(BackslashBackslashS,BackslashBackslashFun);

adjacentFun(lhs:Code,rhs:Code):Expr := eval(Code(adjacentCode(lhs,rhs,codePosition(rhs))));
setup(AdjacentS,adjacentFun);
BinaryPowerMethod(x:Expr,y:Expr):Expr := (
     when y is i0:ZZcell do (
	  i := i0.v;
	  if i === 0 then (
	       onex := lookup(Class(x),oneE);
	       if onex == nullE then (
		    return buildErrorPacket("missing unit element")
		    )
	       else return onex;
	       );
	  if i < 0 then (
	       i = -i;
	       inver := lookup(Class(x),InverseS);
	       if inver == nullE then return MissingMethod("^","InverseMethod");
	       x = applyEE(inver,x);
	       );
	  if !isInt(i) then return buildErrorPacket("'^' expects a small integer exponent");
	  n := toInt(i);
	  w := x;
	  z := nullE;	  -- meaning 1
	  while true do (
	       if (n & 1) != 0 then (
		    if z == nullE then z=w
		    else (
			 z=z*w;
			 when z is Error do return z else nothing;
			 ));
	       n = n >> 1;
	       if n == 0 then break;
	       w = w*w;
	       when w is Error do return w else nothing;
	       );
	  z)
     else WrongArgZZ(2));
BinaryPowerMethodFun(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a)==2 then
     BinaryPowerMethod(a . 0, a . 1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("BinaryPowerMethod",BinaryPowerMethodFun);
SimplePowerMethod(x:Expr,y:Expr):Expr := (
     when y is i0:ZZcell do (
	  i := i0.v;
	  if i === 0 then (
	       onex := lookup(Class(x),oneE);
	       if onex == nullE
	       then return buildErrorPacket("missing unit element")
	       else return onex;
	       );
	  if i <= 0 then (
	       i = -i;
	       inver := lookup(Class(x),InverseS);
	       if inver == nullE then return MissingMethod("^","InverseMethod");
	       x = applyEE(inver,x);
	       );
	  if !isInt(i) then return buildErrorPacket("'^' expects a small integer exponent");
	  n := toInt(i);
	  z := x;
	  while n>1 do (
	       z = x*z;
	       n = n-1;
	       );
	  z)
     else WrongArgZZ(2));
SimplePowerMethodFun(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a)==2 then
     SimplePowerMethod(a . 0, a . 1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("SimplePowerMethod",SimplePowerMethodFun);

export (lhs:Expr) ^ (rhs:Expr) : Expr := (
     when lhs
     is Error do lhs
     is x:ZZcell do (
	  when rhs
	  is y:ZZcell do (
	       if !isNegative(y.v) then toExpr(x.v^y.v) 
	       else if x.v === 1 then toExpr(x.v)
	       else if x.v === -1 then (
		    if int(y.v%ushort(2)) == 0
		    then oneE
		    else minusoneE)
	       else if isZero(x.v) then buildErrorPacket("division by zero")
	       else (
	       	    den := x.v^-y.v;
		    if isNegative(den)
		    then toExpr(newQQCanonical(minusoneZZ,-den))
		    else toExpr(newQQCanonical(     oneZZ, den))))
	  is y:QQcell do (
	       d := denominator(y.v);
	       if d === 1 then toExpr(x.v^numerator(y.v))
	       else if isNegative(x.v)
	       then if isOdd(d) then (
		    if isOdd(numerator(y.v))
		    then toExpr(-toRR(-x.v)^toRR(y.v))
		    else toExpr( toRR(-x.v)^toRR(y.v))
		    )
	       else toExpr(toCC(x.v)^toRR(y.v))
	       else toExpr(toRR(x.v)^toRR(y.v))
	       )
	  is y:RRcell do (
	       if isULong(x.v) then toExpr(toULong(x.v) ^ y.v)
	       else if isNegative(x.v)
	       then toExpr(toCC(x.v,precision(y.v))^y.v)
	       else toExpr(toRR(x.v,precision(y.v))^y.v)
	       )
	  is y:RRicell do (
	       if isULong(x.v) then toExpr(toULong(x.v) ^ y.v)
	       else if isNegative(x.v)
	       then buildErrorPacket("negative base not implemented")
	       else toExpr(toRR(x.v,precision(y.v))^y.v)
	       )
	  is y:CCcell do toExpr(toRR(x.v,precision(y.v))^y.v)
     	  is Error do rhs
	  else binarymethod(lhs,rhs,PowerS))
     is x:QQcell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v^y.v)
	  is y:QQcell do (
	       d := denominator(y.v);
	       if d === 1 then toExpr(x.v^numerator(y.v))
	       else if isNegative(x.v)
	       then if isOdd(d) then (
		    if isOdd(numerator(y.v))
		    then toExpr(-toRR(-x.v)^toRR(y.v))
		    else toExpr( toRR(-x.v)^toRR(y.v))
		    )
	       else toExpr(toCC(x.v)^toRR(y.v))
	       else toExpr(toRR(x.v)^toRR(y.v))
	       )
	  is y:RRcell do (
	       if isNegative(x.v)
	       then toExpr(toCC(x.v,precision(y.v))^y.v)
	       else toExpr(toRR(x.v,precision(y.v))^y.v))
      is y:RRicell do (
	       if isNegative(x.v)
	       then buildErrorPacket("negative base not implemented")
	       else toExpr(toRR(x.v,precision(y.v))^y.v))
	  is y:CCcell do toExpr(toRR(x.v,precision(y.v))^y.v)
     	  is Error do rhs
	  else binarymethod(lhs,rhs,PowerS))
     is x:RRcell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v^y.v)
	  is y:QQcell do (
	       d := denominator(y.v);
	       if d === 1 then toExpr(x.v^numerator(y.v))
	       else if isNegative(x.v)
	       then if isOdd(d) then (
		    if isOdd(numerator(y.v))
		    then toExpr(-(-x.v)^toRR(y.v,precision(x.v)))
		    else toExpr( (-x.v)^toRR(y.v,precision(x.v)))
		    )
	       else toExpr(toCC(x.v)^toRR(y.v,precision(x.v)))
	       else toExpr(x.v^toRR(y.v,precision(x.v)))
	       )
	  is y:RRcell do (
	       if isNegative(x.v)
	       then toExpr(toCC(x.v)^y.v)
	       else toExpr(x.v^y.v)
	       )
      is y:RRicell do (
	       if isNegative(x.v)
	       then buildErrorPacket("negative base not implemented")
	       else toExpr(x.v^y.v)
	       )
	  is y:CCcell do toExpr(x.v^y.v)
     	  is Error do rhs
	  else binarymethod(lhs,rhs,PowerS))
    is x:RRicell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v^y.v)
	  is y:QQcell do (
	       d := denominator(y.v);
	       if d === 1 then toExpr(x.v^numerator(y.v))
	       else if x.v >= 0 then toExpr(x.v^(toRRi(y.v)))
           else buildErrorPacket("negative base not implemented")
           )
	  is y:RRcell do (
	       if x.v >= 0
	       then toExpr(x.v^y.v)
	       else buildErrorPacket("negative base not implemented")
	       )
      is y:RRicell do (
	       if x.v >= 0
	       then toExpr(x.v^y.v)
	       else buildErrorPacket("negative base not implemented")
	       )
      else binarymethod(lhs,rhs,PowerS))
     is x:CCcell do (
	  when rhs
	  is y:ZZcell do toExpr(x.v^y.v)
	  is y:QQcell do (
	       if denominator(y.v) === 1 then toExpr(x.v^numerator(y.v))
	       else if denominator(y.v) === 2 then toExpr(sqrt(x.v)^numerator(y.v))
	       else toExpr(x.v^toRR(y.v,precision(x.v))))
	  is y:RRcell do toExpr(x.v^y.v)
	  is y:CCcell do toExpr(x.v^y.v)
     	  is Error do rhs
	  else binarymethod(lhs,rhs,PowerS))
     is x:RawRingElementCell do (
	  when rhs
	  is y:ZZcell do (
	       when x.p^y.v
	       is t:RawRingElement do toExpr(t)
	       is null do buildErrorPacket(EngineError("polynomial power failed"))
	       )
	  is Error do rhs
	  else binarymethod(lhs,rhs,StarS))
     is x:RawMonomialCell do (
	  when rhs
	  is y:ZZcell do
	  if isInt(y.v)
	  then (
	       when x.p ^ toInt(y.v)
	       is z:RawMonomial do toExpr(z)
	       is null do buildErrorPacket(EngineError("monomial power overflow"))
	       )
	  else WrongArgSmallInteger(2)
	  else binarymethod(lhs,rhs,DivideS))
     else binarymethod(lhs,rhs,PowerS));
powerC(lhs:Code,rhs:Code):Expr := (
     l := eval(lhs);
     when l is Error do l
     else (
     	  r := eval(rhs);
     	  when r is Error do r
	  else l^r));
setup(PowerS,powerC);
powerfun(e:Expr):Expr := (
     when e is a:Sequence do
     if length(a) == 2 then a.0^a.1
     else WrongNumArgs("^",2)
     else WrongNumArgs("^",2));
setupfun("power",powerfun);
logorfun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left
     is Error do left
     else (
	  if left == True then True
	  else if left == False then (
	       right := eval(rhs);
	       when right
	       is Error do right
	       else (
		    if right == True then True
		    else if right == False then False
		    else binarymethod(left,right,orS)))
	  else binarymethod(left,rhs,orS)));
setup(orS,logorfun);
logxorfun(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,xorS);
setup(xorS,logxorfun);
BarBarF(lhs:Code,rhs:Code):Expr := binarymethod(lhs,rhs,BarBarS);
setup(BarBarS,BarBarF);
logandfun(lhs:Code,rhs:Code):Expr := (
     a := eval(lhs);
     when a
     is Error do a
     else (
	  if a == False then False
	  else if a == True then (
	       b := eval(rhs);
	       when b
	       is Error do b
	       else (
		    if b == True then True
		    else if b == False then False
		    else binarymethod(a,b,andS)))
	  else binarymethod(a,rhs,andS)));
setup(andS,logandfun);
export notFun(rhs:Code):Expr := (
     a := eval(rhs);
     when a
     is Error do a
     else if a == True then False
     else if a == False then True
     else unarymethod(a,notS));
setup(notS,notFun);
EqualEqualEqualfun(lhs:Code,rhs:Code):Expr := (
     x := eval(lhs);
     when x is Error do x
     else (
     	  y := eval(rhs);
     	  when y is Error do y
	  else equal(x,y)));
setup(EqualEqualEqualS,EqualEqualEqualfun);
quicknot(z:Expr):Expr := (
     when z is Error do z else if z == True then False else True
     );
notEqualEqualEqualfun(lhs:Code,rhs:Code):Expr := quicknot(EqualEqualEqualfun(lhs,rhs));
setup(NotEqualEqualEqualS,notEqualEqualEqualfun);
smallintarrays0 := new array(Expr) len 20 at i do (
     provide Expr(new Sequence len i+1 at k do provide toExpr(k)));
smallintarrays1 := new array(Expr) len 20 at i do (
     provide Expr(new Sequence len i at k do provide toExpr(1+k)));
DotDotfun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left
     is Error do left
     is xx:ZZcell do (
	  x := xx.v;
	  right := eval(rhs);
	  when right
	  is Error do right
	  is yy:ZZcell do (
	       y := yy.v;
	       if isInt(x) && isInt(y) then (
	  	    i := toInt(x);
		    j := toInt(y);
		    if i>j then emptySequenceE
		    else if i==0 && j<length(smallintarrays0)
		    then smallintarrays0.j
		    else if i==1 && j<length(smallintarrays1)
		    then smallintarrays1.j
		    else Expr(new Sequence len j-i+1 at k do provide toExpr(i+k)))
	       else (
		    z := y-x;
		    if z <= 0 then emptySequenceE
		    else if isInt(z) then (
			 m := toInt(z);
			 Expr(new Sequence len m+1 at k do provide toExpr(x+k)))
		    else printErrorMessageE(rhs,"range too large")))
	  else binarymethod(left,right,DotDotS))
     else binarymethod(left,rhs,DotDotS));
setup(DotDotS,DotDotfun);

DotDotLessFun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left
     is Error do left
     is xx:ZZcell do (
	  x := xx.v;
	  right := eval(rhs);
	  when right
	  is Error do right
	  is yy:ZZcell do (
	       y := yy.v;
	       if isInt(x) && isInt(y) then (
	  	    i := toInt(x);
		    j := toInt(y) - 1;
		    if i>j then emptySequenceE
		    else if i==0 && j<length(smallintarrays0)
		    then smallintarrays0.j
		    else if i==1 && j<length(smallintarrays1)
		    then smallintarrays1.j
		    else (
			 if j-i+1 > 100000
			 then buildErrorPacket("ZZ .. ZZ: very long sequence requested")
			 else Expr(new Sequence len j-i+1 at k do provide toExpr(i+k))))
	       else (
		    z := y-x;
		    if z <= 0 then emptySequenceE
		    else if isInt(z) then (
			 m := toInt(z)-1;
			 Expr(new Sequence len m+1 at k do provide toExpr(x+k)))
		    else printErrorMessageE(rhs,"range too large")))
	  else binarymethod(left,right,DotDotLessS))
     else binarymethod(left,rhs,DotDotLessS));
setup(DotDotLessS,DotDotLessFun);

assignNewFun(newclass:Code,rhs:Code):Expr := (
     c := eval(newclass);
     when c
     is Error do c
     is o:HashTable do installMethod(NewE,o,eval(rhs))
     else printErrorMessageE(newclass,"expected a hash table as prospective class"));
AssignNewFun = assignNewFun;
assignNewOfFun(newclass:Code,newparent:Code,rhs:Code):Expr := (
     c := eval(newclass);
     when c is Error do c
     is cc:HashTable do (
	  p := eval(newparent);
	  when p is Error do p
	  is pp:HashTable do installMethod(NewOfE,cc,pp,eval(rhs))
	  else printErrorMessageE(newparent,"expected a hash table as prospective parent"))
     else printErrorMessageE(newclass,"expected a hash table as prospective class"));
AssignNewOfFun = assignNewOfFun;
assignNewFromFun(newclass:Code,newinitializer:Code,rhs:Code):Expr := (
     c := eval(newclass);
     when c is Error do c
     is cc:HashTable do (
	  i := eval(newinitializer);
	  when i is Error do i
	  is ii:HashTable do installMethod(NewFromE,cc,ii,eval(rhs))
     	  else printErrorMessageE(newinitializer,"expected a hash table"))
     else printErrorMessageE(newclass,"expected a hash table as prospective class"));
AssignNewFromFun = assignNewFromFun;
assignNewOfFromFun(args:CodeSequence):Expr := (
     newclass := args.0;
     newparent := args.1;
     newinitializer := args.2;
     rhs := args.3;
     c := eval(newclass);
     when c 
     is Error do c 
     is cc:HashTable do (
	  p := eval(newparent);
	  when p
	  is Error do p
	  is pp:HashTable do (
	       i := eval(newinitializer);
	       when i 
	       is Error do i 
	       is ii:HashTable do (
	       	    r := eval(rhs);
	       	    when r 
		    is Error do r
	       	    else installMethod(NewOfFromE,cc,pp,ii,r))
     	       else printErrorMessageE(newinitializer,"expected a hash table as class to initialize from"))
	  else printErrorMessageE(newparent,"expected a hash table as prospective parent"))
     else printErrorMessageE(newclass,"expected a hash table as prospective class")
     );
AssignNewOfFromFun = assignNewOfFromFun;
installFun2(a:Expr,args:CodeSequence):Expr := (
     opr := eval(args.0);
     when opr 
     is Error do opr
     is oper:SymbolClosure do (
	  if oper === AdjacentS then (
	       b := eval(args.2);
	       when b
	       is Error do b
	       is bcd:Sequence do (
		    if length(bcd) == 2 then (
			 when bcd.0
			 is bb:HashTable do
			 when bcd.1
			 is cc:HashTable do installMethod(a,bb,cc,eval(args.3))
			 else buildErrorPacket("expected second parameter to be a hash table")
			 else buildErrorPacket("expected first parameter to be a hash table")
			 )
		    else if length(bcd) == 3 then (
			 when bcd.0 is bb:HashTable do
			 when bcd.1 is cc:HashTable do
			 when bcd.2 is dd:HashTable do installMethod(a,bb,cc,dd,eval(args.3)) 
			 else buildErrorPacket("expected third parameter to be a hash table")
			 else buildErrorPacket("expected second parameter to be a hash table")
			 else buildErrorPacket("expected first parameter to be a hash table")
			 )
		    else if length(bcd) == 4 then (
			 when bcd.0 is bb:HashTable do
			 when bcd.1 is cc:HashTable do
			 when bcd.2 is dd:HashTable do
			 when bcd.3 is ee:HashTable do installMethod(a,bb,cc,dd,ee,eval(args.3)) 
			 else buildErrorPacket("expected fourth parameter to be a hash table")
			 else buildErrorPacket("expected third parameter to be a hash table")
			 else buildErrorPacket("expected second parameter to be a hash table")
			 else buildErrorPacket("expected first parameter to be a hash table")
			 )
		    else buildErrorPacket("expected 1, 2, 3, or 4 parameter types"))
	       is bb:HashTable do installMethod(a,bb,eval(args.3))
	       else buildErrorPacket("expected right hand parameter to be a hash table or sequence"))
	  else buildErrorPacket("expected adjacency operator ' ' on left"))
     else buildErrorPacket("expected operator to be a symbol"));
installMethodFun2(arg1:Expr,args:CodeSequence):Expr := (
     when arg1 
     is Error do arg1
     is CompiledFunction do installFun2(arg1,args)
     is CompiledFunctionClosure do installFun2(arg1,args)
     is FunctionClosure do installFun2(arg1,args)
     is s:SpecialExpr do if ancestor(s.Class,functionClass) then installFun2(arg1,args) else buildErrorPacket("expected right hand parameter to be a type of function")
     is aa:HashTable do (
	  if aa.parent == nothingClass
	  then (
	       installFun2(arg1,args)	  -- handle, e.g., Ext(ZZ, Module, Module) := (i,M,N) -> ...
	       )
	  else (
	       b := eval(args.2);
	       when b is Error do b 
	       is bb:HashTable do (
		    opr := eval(args.0);
		    when opr is Error do opr
		    else installMethod(opr,aa,bb,eval(args.3)))
	       else buildErrorPacket("expected right hand parameter to be a hash table")))
     else buildErrorPacket("expected left hand parameter to be a function, type, or a hash table"));
installMethodFun(args:CodeSequence):Expr := installMethodFun2(eval(args.1),args);
InstallMethodFun = installMethodFun;

mess1 := "objects on left hand side of assignment are not types (use ':=' instead?)";

-- this new version just looks up a method for user code, which *could* do the same thing
installValueFun(args:CodeSequence):Expr := (
     oper := eval(args.0);
     when oper is Error do return oper else nothing;
     x := eval(args.1);
     when x is Error do return x else nothing;
     y := eval(args.2);
     when y is Error do return y else nothing;
     meth := lookupBinaryMethod(Class(x),Class(y),Expr(Sequence(oper,EqualE))); -- i.e., x*y=z is looked up under ((symbol *,symbol =),class x,class y)
     if meth == nullE then return MissingAssignmentMethodPair(oper,x,y);
     z := eval(args.3);
     applyEEEE(meth,x,y,z));
-- this old version was used for stashing values somewhere
-- installValueFun(args:CodeSequence):Expr := (
--      a := eval(args.1);
--      when a is Error do a
--      is aa:HashTable do (
-- 	  b := eval(args.2);
-- 	  when b is Error do b 
-- 	  is bb:HashTable do (
-- 	       opr := eval(args.0);
-- 	       when opr is Error do opr
-- 	       else (
-- 		    x := eval(args.3);
-- 		    when x is Error do x
-- 		    else installValue(opr,aa,bb,x)
-- 		    )
-- 	       )
-- 	  else buildErrorPacket(mess1))
--      else buildErrorPacket(mess1));
InstallValueFun = installValueFun;

unaryInstallMethodFun(meth:Code,argtype:Code,body:Code):Expr := (
     f := eval(meth);
     when f is Error do f
     else (
	  t := eval(argtype);
	  when t is Error do t 
	  else when t is T:HashTable do (
	       b := eval(body);
	       when b is Error do b
	       else installMethod(f,T,b)
	       )
	  else printErrorMessageE(argtype,"expected a hash table")));
UnaryInstallMethodFun = unaryInstallMethodFun;

unaryInstallValueFun(meth:Code,lhs:Code,rhs:Code):Expr := (
     oper := eval(meth);
     when oper is Error do return oper else nothing;
     y := eval(lhs);
     when y is Error do return y else nothing;
     method := lookup(Class(y),Expr(Sequence(oper,EqualE))); -- i.e., *y=z is looked up under ((symbol *,symbol =),class y)
     if method == nullE then return MissingAssignmentMethod(oper,y);
     z := eval(rhs);
     applyEEE(method,y,z));
-- this old version was used for stashing values somewhere
-- unaryInstallValueFun(meth:Code,argtype:Code,body:Code):Expr := (
--      Argtype := eval(argtype);
--      when Argtype is Error 
--      do Argtype 
--      else when Argtype is
--      o:HashTable do (
-- 	  methv := eval(meth);
-- 	  when methv is Error do methv else (
-- 	       bodyv := eval(body);
-- 	       when bodyv is Error do bodyv else (
-- 	  	    storeInHashTable(o,
-- 			 Expr(Sequence(methv)),  -- distinguishing feature of "values"
-- 			      	   	  -- so after -x the answer can be stored in x#(seq quote -)
-- 			 bodyv)
-- 		    )
-- 	       )
-- 	  )
--      else printErrorMessageE(argtype,"expected a hash table")
--      );
UnaryInstallValueFun = unaryInstallValueFun;

flatten(a:Sequence):Sequence := (
     -- warning - this function may return its argument without copying
     hadlist := false;
     newlen := length(a);
     if newlen == 0 then return a;
     if newlen == 1 then (
	  when a.0
	  is x:List do (
	       if ancestor(x.Class,listClass) 
	       then return x.v
	       else return a
	       )
	  else return a;
	  );
     foreach i in a do (
     	  when i is ii:List do (
	       if ancestor(ii.Class,listClass) then (
	       	    hadlist = true; 
	       	    newlen = newlen + length(ii.v) - 1;
	       	    ))
     	  else nothing;
	  );
     if hadlist then (
	  new Sequence len newlen do (
     	       foreach i in a do (
     	       	    when i is ii:List do (
			 if ancestor(ii.Class,listClass) then (
     	       	    	      foreach j in ii.v do provide j
			      )
			 else provide i
			 )
     	       	    else provide i)))
     else a);
flatten(e:Expr):Expr := (
     when e
     is v:Sequence do Expr(flatten(v))
     is a:List do list(
	  a.Class,
	  if a.Mutable then (
	       r := flatten(a.v);
	       if r == a.v then copy(r) else r
	       )
	  else flatten(a.v),
	  a.Mutable)
     else WrongArg("a list or sequence"));
setupfun("flatten",flatten);

subvalue(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left is Error do left
     else (
      	  right := eval(rhs);
      	  when right is Error do right
      	  else subvalue(left,right)));
lengthFun(rhs:Code):Expr := (
     e := eval(rhs);
     when e
     is Error do e
     is x:HashTable do toExpr(x.numEntries)
     is x:Sequence do toExpr(length(x))
     is dc:DictionaryClosure do toExpr(dc.dictionary.symboltable.numEntries)
     is x:List do toExpr(length(x.v))
     is s:stringCell do toExpr(length(s.v))
     is n:Net do toExpr(length(n.body))
     else buildErrorPacket("expected a list, sequence, hash table, or string"));
setup(SharpS,lengthFun,subvalue);
subvalueQ(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left is Error do left
     else (
      	  right := eval(rhs);
      	  when right is Error do right
      	  else subvalueQ(left,right)));
setup(SharpQuestionS,subvalueQ);

isFinite(e:Expr):Expr := (
     -- # typical value: isFinite, Number, Boolean
     when e
     is x:ZZcell do True
     is x:QQcell do True
     is x:RRcell do toExpr(isfinite(x.v))
     is x:RRicell do toExpr(isfinite(x.v))
     is x:CCcell do toExpr(isfinite(x.v))
     else WrongArg("a number")
     );
setupfun("isFinite",isFinite);

isANumber(e:Expr):Expr := (
     -- # typical value: isANumber, Number, Boolean
     when e
     is x:ZZcell do True
     is x:QQcell do True
     is x:RRcell do toExpr(!isnan(x.v))
     is x:RRicell do toExpr(!isnan(x.v))
     is x:CCcell do toExpr(!isnan(x.v))
     else WrongArg("a number")
     );
setupfun("isANumber",isANumber);

isInfinite(e:Expr):Expr := (
     -- # typical value: isInfinite, Number, Boolean
     when e
     is x:ZZcell do False
     is x:QQcell do False
     is x:RRcell do toExpr(isinf(x.v))
     is x:RRicell do toExpr(isinf(x.v))
     is x:CCcell do toExpr(isinf(x.v))
     else WrongArg("a number")
     );
setupfun("isInfinite",isInfinite);

gcIsVisible(e:Expr):Expr := (
     Ccode(void, "assert(GC_is_visible(",e,"))");
     nullE);
setupfun("gcIsVisible",gcIsVisible);

CollectGarbage(e:Expr):Expr := (
     Ccode(void,"GC_gcollect()");
     nullE);
setupfun("collectGarbage",CollectGarbage);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d actors.o "
-- End:
