--		Copyright 1994 by Daniel R. Grayson

use C;
use system; 
use convertr;
use evaluate;
use common;
use binding;
use parser;
use lex;
use engine;
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
use basic;
use struct;
use objects;

export plus0():Expr := Expr(toInteger(0));
export times0():Expr := Expr(toInteger(1));
export plus1(e:Expr) : Expr := e;
times1 := plus1;


RealPlus(localInterpState:threadLocalInterp,lhs:Expr,rhs:Expr):Expr := (
     when lhs
     is x:ZZ do (
	  when rhs
	  is y:ZZ do Expr(x + y)			    -- # typical value: symbol +, ZZ, ZZ, ZZ
     	  is y:QQ do Expr(x + y)			    -- # typical value: symbol +, ZZ, QQ, QQ
     	  is y:RR do Expr(y + x)			    -- # typical value: symbol +, ZZ, RR, RR
     	  is y:CC do Expr(toRR(x,precision(y.re)) + y)	    -- # typical value: symbol +, ZZ, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PlusS))
     is x:QQ do (
	  when rhs
	  is y:ZZ do Expr(x + y)			    -- # typical value: symbol +, QQ, ZZ, QQ
     	  is y:QQ do Expr(x + y)			    -- # typical value: symbol +, QQ, QQ, QQ
     	  is y:RR do Expr(y + x)			    -- # typical value: symbol +, QQ, RR, RR
     	  is y:CC do Expr(toRR(x,precision(y.re)) + y)	    -- # typical value: symbol +, QQ, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PlusS))
     is x:RawRingElement do (
	  when rhs
	  is y:RawRingElement do (			    -- # typical value: symbol +, RawRingElement, RawRingElement, RawRingElement
	       when x+y
	       is t:RawRingElement do Expr(t)
	       is null do buildErrorPacket(EngineError("polynomial addition failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PlusS))
     is x:RR do (
	  when rhs
	  is y:ZZ do Expr(x + y)			    -- # typical value: symbol +, RR, ZZ, RR
     	  is y:QQ do Expr(x + y)			    -- # typical value: symbol +, RR, QQ, RR
     	  is y:RR do Expr(x + y)			    -- # typical value: symbol +, RR, RR, RR
     	  is y:CC do Expr(x + y)			    -- # typical value: symbol +, RR, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PlusS))
     is x:CC do (
	  when rhs
	  is y:ZZ do Expr(x + toRR(y,precision(x.re)))	    -- # typical value: symbol +, CC, ZZ, CC
     	  is y:QQ do Expr(x + toRR(y,precision(x.re)))	    -- # typical value: symbol +, CC, QQ, CC
     	  is y:RR do Expr(x + y)			    -- # typical value: symbol +, CC, RR, CC
     	  is y:CC do Expr(x + y)			    -- # typical value: symbol +, CC, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PlusS))
     is x:RawMatrix do (
	  when rhs
	  is y:RawMatrix do (				    -- # typical value: symbol +, RawMatrix, RawMatrix, RawMatrix
	       when x+y
	       is t:RawMatrix do Expr(t)
	       is null do buildErrorPacket(EngineError("matrix addition failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PlusS))
     is x:RawMutableMatrix do (
	  when rhs
	  is y:RawMutableMatrix do (			    -- # typical value: symbol +, RawMutableMatrix, RawMutableMatrix, RawMutableMatrix
	       when x+y
	       is t:RawMutableMatrix do Expr(t)
	       is null do buildErrorPacket(EngineError("mutable matrix addition failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PlusS))
     is x:RawMonomialIdeal do (
	  when rhs
	  is y:RawMonomialIdeal do (			    -- # typical value: symbol +, RawMonomialIdeal, RawMonomialIdeal, RawMonomialIdeal
	       when x+y
	       is t:RawMonomialIdeal do Expr(t)
	       is null do buildErrorPacket(EngineError("monomial ideal addition failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PlusS))
     is Error do lhs
     else binarymethod(localInterpState,lhs,rhs,PlusS));

export (lhs:Expr) + (rhs:Expr) : Expr := RealPlus(threadLocalInterpState,lhs,rhs);

plus(localInterpState:threadLocalInterp,e:Expr):Expr := accumulate(plus0,plus1,op+,e);
setupfun("plus",plus);

plusfun1(localInterpState:threadLocalInterp,rhs:Code):Expr := (
     r := eval(localInterpState,rhs);
     when r
     is Error do r
     is ZZ do r						    -- # typical value: symbol +, ZZ, ZZ
     is RR do r						    -- # typical value: symbol +, RR, RR
     is CC do r						    -- # typical value: symbol +, CC, CC
     is QQ do r						    -- # typical value: symbol +, QQ, QQ
     is RawRingElement do r				    -- # typical value: symbol +, RawRingElement, RawRingElement
     is RawMatrix do r					    -- # typical value: symbol +, RawMatrix, RawMatrix
     is RawMutableMatrix do r				    -- # typical value: symbol +, RawMutableMatrix, RawMutableMatrix
     else unarymethod(localInterpState,rhs,PlusS));
plusfun(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     l := eval(localInterpState,lhs);
     when l is Error do l
     else (
     	  r := eval(localInterpState,rhs);
     	  when r is Error do r
	  else l+r));
setup(PlusS,plusfun1,plusfun);


RealMinus(localInterpState:threadLocalInterp,rhs:Expr):Expr := (
     when rhs
     is x:ZZ do Expr(-x)				    -- # typical value: symbol -, ZZ, ZZ                            
     is x:RR do Expr(-x)				    -- # typical value: symbol -, RR, RR                            
     is x:CC do Expr(-x)				    -- # typical value: symbol -, CC, CC                            
     is x:QQ do Expr(-x)				    -- # typical value: symbol -, QQ, QQ                            
     is x:RawRingElement do Expr(-x)			    -- # typical value: symbol -, RawRingElement, RawRingElement    
     is x:RawMatrix do (				    -- # typical value: symbol -, RawMatrix, RawMatrix              
	  when -x is y:RawMatrix do Expr(y) else buildErrorPacket(EngineError("polynomial minus failed"))
	  )
     is x:RawMutableMatrix do Expr(-x)                      -- # typical value: symbol -, RawMutableMatrix, RawMutableMatrix
     is Error do rhs
     else (
	  method := lookup(Class(rhs),MinusS);
	  if method == nullE
	  then buildErrorPacket("no method found")
	  else applyEE(localInterpState,method,Expr(rhs))));

export - (rhs:Expr) : Expr := RealMinus(threadLocalInterpState, rhs);

minusfun1(localInterpState:threadLocalInterp,rhs:Code):Expr := - eval(localInterpState,rhs);

RealMinus(localInterpState:threadLocalInterp,lhs:Expr,rhs:Expr):Expr := (
     when lhs
     is x:ZZ do (
	  when rhs
	  is y:ZZ do Expr(x - y)			    -- # typical value: symbol -, ZZ, ZZ, ZZ
     	  is y:QQ do Expr(x - y)			    -- # typical value: symbol -, ZZ, QQ, QQ
	  is y:RR do Expr(toRR(x,precision(y)) - y)		    -- # typical value: symbol -, ZZ, RR, RR
	  is y:CC do Expr(toRR(x,precision(y.re)) - y)	    -- # typical value: symbol -, ZZ, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,MinusS))
     is x:QQ do (
	  when rhs
	  is y:ZZ do Expr(x - y)			    -- # typical value: symbol -, QQ, ZZ, QQ
     	  is y:QQ do Expr(x - y)			    -- # typical value: symbol -, QQ, QQ, QQ
	  is y:RR do Expr(toRR(x,precision(y)) - y)		    -- # typical value: symbol -, QQ, RR, RR
	  is y:CC do Expr(toRR(x,precision(y.re)) - y)	    -- # typical value: symbol -, QQ, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,MinusS))
     is x:RawRingElement do (
	  when rhs
	  is y:RawRingElement do (			    -- # typical value: symbol -, RawRingElement, RawRingElement, RawRingElement
	       when x-y
	       is t:RawRingElement do Expr(t)
	       is null do buildErrorPacket(EngineError("polynomial subtraction failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,MinusS))
     is x:RR do (
	  when rhs
	  is y:ZZ do Expr(x - y)			    -- # typical value: symbol -, RR, ZZ, RR
     	  is y:QQ do Expr(x - y)			    -- # typical value: symbol -, RR, QQ, RR
	  is y:RR do Expr(x - y)			    -- # typical value: symbol -, RR, RR, RR
	  is y:CC do Expr(x - y)			    -- # typical value: symbol -, RR, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,MinusS))
     is x:CC do (
	  when rhs
	  is y:ZZ do Expr(x - toRR(y,precision(x.re)))	    -- # typical value: symbol -, CC, ZZ, CC
     	  is y:QQ do Expr(x - toRR(y,precision(x.re)))	    -- # typical value: symbol -, CC, QQ, CC
	  is y:RR do Expr(x - y)			    -- # typical value: symbol -, CC, RR, CC
	  is y:CC do Expr(x - y)			    -- # typical value: symbol -, CC, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,MinusS))
     is x:RawMatrix do (
	  when rhs
	  is y:RawMatrix do (				    -- # typical value: symbol -, RawMatrix, RawMatrix, RawMatrix
	       when x-y
	       is t:RawMatrix do Expr(t)
	       is null do buildErrorPacket(EngineError("matrix subtraction failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,MinusS))
     is x:RawMutableMatrix do (				    -- # typical value: symbol -, RawMutableMatrix, RawMutableMatrix, RawMutableMatrix
	  when rhs
	  is y:RawMutableMatrix do (
	       when x-y
	       is t:RawMutableMatrix do Expr(t)
	       is null do buildErrorPacket(EngineError("matrix subtraction failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,MinusS))
     is x:RawMonomialIdeal do (				    -- # typical value: symbol -, RawMonomialIdeal, RawMonomialIdeal, RawMonomialIdeal
	  when rhs
	  is y:RawMonomialIdeal do (
	       when x-y
	       is t:RawMonomialIdeal do Expr(t)
	       is null do buildErrorPacket(EngineError("monomial ideal difference failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PlusS))
     is Error do lhs
     else binarymethod(localInterpState,lhs,rhs,MinusS));


export (lhs:Expr) - (rhs:Expr) : Expr := RealMinus(threadLocalInterpState,lhs,rhs);

minusfun2(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     l := eval(localInterpState,lhs);
     when l is Error do l
     else (
     	  r := eval(localInterpState,rhs);
     	  when r is Error do r
	  else l-r));
setup(MinusS,minusfun1,minusfun2);
minusfun(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 1 then -a.0
	  else WrongNumArgs(1))
     else -e);
setupfun("minus",minusfun);
differencefun(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 2 then a.0-a.1
	  else WrongNumArgs(2))
     else WrongNumArgs(2));
setupfun("difference",differencefun);
one := toInteger(1);
minusone := toInteger(-1);

RealTimes(localInterpState:threadLocalInterp,lhs:Expr,rhs:Expr):Expr := (
     when lhs
     is x:ZZ do (
	  when rhs
	  is y:ZZ do Expr(x * y)			    -- # typical value: symbol *, ZZ, ZZ, ZZ
     	  is y:QQ do Expr(x * y)			    -- # typical value: symbol *, ZZ, QQ, QQ
     	  is y:RR do Expr(toRR(x,precision(y)) * y)		    -- # typical value: symbol *, ZZ, RR, RR
     	  is y:CC do Expr(x * y)	    -- # typical value: symbol *, ZZ, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,StarS))
     is x:QQ do (
	  when rhs
	  is y:ZZ do Expr(x * y)			    -- # typical value: symbol *, QQ, ZZ, QQ
     	  is y:QQ do Expr(x * y)			    -- # typical value: symbol *, QQ, QQ, QQ
     	  is y:RR do Expr(y * x)			    -- # typical value: symbol *, QQ, RR, RR
     	  is y:CC do Expr(y * toRR(x,precision(y.re)))	    -- # typical value: symbol *, QQ, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,StarS))
     is x:RawRingElement do (
	  when rhs
	  is y:RawRingElement do (			    -- # typical value: symbol *, RawRingElement, RawRingElement, RawRingElement
	       when x*y
	       is t:RawRingElement do Expr(t)
	       is null do buildErrorPacket(EngineError("polynomial multiplication failed"))
	       )
	  is y:RawMatrix do (				    -- # typical value: symbol *, RawRingElement, RawMatrix, RawMatrix
	       when x*y
	       is t:RawMatrix do Expr(t)
	       is null do buildErrorPacket(EngineError("matrix scalar multiplication failed"))
	       )
	  is y:RawMutableMatrix do (			    -- # typical value: symbol *, RawRingElement, RawMutableMatrix, RawMatrix
	       when x*y
	       is t:RawMutableMatrix do Expr(t)
	       is null do buildErrorPacket(EngineError("matrix scalar multiplication failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,StarS))
     is x:RR do (
	  when rhs
	  is y:ZZ do Expr(x * y)			    -- # typical value: symbol *, RR, ZZ, RR
     	  is y:QQ do Expr(x * y)			    -- # typical value: symbol *, RR, QQ, RR
     	  is y:RR do Expr(x * y)			    -- # typical value: symbol *, RR, RR, RR
     	  is y:CC do Expr(x * y)			    -- # typical value: symbol *, RR, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,StarS))
     is x:CC do (
	  when rhs
	  is y:ZZ do Expr(x * y)	    -- # typical value: symbol *, CC, ZZ, CC
     	  is y:QQ do Expr(x * toRR(y,precision(x.re)))	    -- # typical value: symbol *, CC, QQ, CC
     	  is y:RR do Expr(y * x)			    -- # typical value: symbol *, CC, RR, CC
     	  is y:CC do Expr(y * x)			    -- # typical value: symbol *, CC, CC, CC
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,StarS))
     is x:RawMonomial do (
	  when rhs
	  is y:RawMonomial do (				    -- # typical value: symbol *, RawMonomialIdeal, RawMonomial, RawMonomial
	       when x*y
	       is z:RawMonomial do Expr(z)
	       is null do buildErrorPacket(EngineError("monomial multiplication overflow"))
	       )
	  else binarymethod(localInterpState,lhs,rhs,StarS))
     is x:RawMonomialIdeal do (				    -- # typical value: symbol *, RawMonomialIdeal, RawMonomialIdeal, RawMonomialIdeal
	  when rhs
	  is y:RawMonomialIdeal do (
	       when x*y
	       is z:RawMonomialIdeal do Expr(z)
	       is null do buildErrorPacket(EngineError("monomial ideal multiplication failed"))
	       )
	  else binarymethod(localInterpState,lhs,rhs,StarS))
     is x:RawMatrix do (				    -- # typical value: symbol *, RawMatrix, RawRingElement, RawRingElement
	  when rhs
	  is y:RawRingElement do (
	       when x*y
	       is t:RawMatrix do Expr(t)
	       is null do buildErrorPacket(EngineError("matrix scalar multiplication failed"))
	       )
	  is y:RawMatrix do (				    -- # typical value: symbol *, RawMatrix, RawMatrix, RawMatrix
	       when x*y
	       is t:RawMatrix do Expr(t)
	       is null do buildErrorPacket(EngineError("matrix multiplication failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,StarS))
     is x:RawMutableMatrix do (
	  when rhs
	  is y:RawRingElement do (			    -- # typical value: symbol *, RawMutableMatrix, RawRingElement, RawMutableMatrix
	       when x*y
	       is t:RawMutableMatrix do Expr(t)
	       is null do buildErrorPacket(EngineError("matrix scalar multiplication failed"))
	       )
	  is y:RawMutableMatrix do (			    -- # typical value: symbol *, RawMutableMatrix, RawMutableMatrix, RawMutableMatrix
	       when x*y
	       is t:RawMutableMatrix do Expr(t)
	       is null do buildErrorPacket(EngineError("matrix multiplication failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,StarS))
     is Error do lhs
     else (
	  when rhs is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,StarS)));

export (lhs:Expr) * (rhs:Expr) : Expr := RealTimes(threadLocalInterpState,lhs,rhs);

times(localInterpState:threadLocalInterp,e:Expr):Expr := accumulate(times0,times1,op*,e);
setupfun("times",times);

RealDivide(localInterpState:threadLocalInterp,lhs:Expr,rhs:Expr):Expr := (
     when lhs
     is x:ZZ do (
	  when rhs
	  is y:ZZ do (					    -- # typical value: symbol /, ZZ, ZZ, ZZ
	       if y === 0
	       then buildErrorPacket("division by zero")
	       else Expr(x / y))
     	  is y:QQ do (					    -- # typical value: symbol /, ZZ, QQ, QQ
	       if y === 0
	       then buildErrorPacket("division by zero")
	       else Expr(x / y))
     	  is y:RR do (					    -- # typical value: symbol /, ZZ, RR, RR
	       Expr(toRR(x,precision(y)) / y))
     	  is y:CC do (					    -- # typical value: symbol /, ZZ, CC, CC
	       Expr(x / y))
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,DivideS))
     is x:QQ do (
	  when rhs
	  is y:ZZ do (					    -- # typical value: symbol /, QQ, ZZ, QQ
	       if y === 0
	       then buildErrorPacket("division by zero")
	       else Expr(x / y))
     	  is y:QQ do (					    -- # typical value: symbol /, QQ, QQ, QQ
	       if y === 0
	       then buildErrorPacket("division by zero")
	       else Expr(x / y))
     	  is y:RR do (					    -- # typical value: symbol /, QQ, RR, RR
	       -- if y === 0 then buildErrorPacket("division by zero") else
	       Expr(toRR(x,precision(y)) / y))
     	  is y:CC do (					    -- # typical value: symbol /, QQ, CC, CC
	       if y === 0 then buildErrorPacket("division by zero") else
	       Expr(toRR(x,precision(y.re)) / y))
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,DivideS))
     is x:RR do (
	  when rhs
	  is y:ZZ do (					    -- # typical value: symbol /, RR, ZZ, RR
	       if y === 0
	       then buildErrorPacket("division by zero")
	       else Expr(x / y))
     	  is y:QQ do (					    -- # typical value: symbol /, RR, QQ, RR
	       if y === 0
	       then buildErrorPacket("division by zero")
	       else Expr(x / y))
     	  is y:RR do (					    -- # typical value: symbol /, RR, RR, RR
	       -- if y === 0 then buildErrorPacket("division by zero") else
	       Expr(x / y))
     	  is y:CC do (					    -- # typical value: symbol /, RR, CC, CC
	       -- if y === 0 then buildErrorPacket("division by zero") else
	       Expr(x / y))
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,DivideS))
     is x:CC do (
	  when rhs
	  is y:ZZ do (					    -- # typical value: symbol /, CC, ZZ, CC
	       if y === 0
	       then buildErrorPacket("division by zero")
	       else Expr(x / toRR(y,precision(x.re)))
	       )
     	  is y:QQ do (					    -- # typical value: symbol /, CC, QQ, CC
	       if y === 0
	       then buildErrorPacket("division by zero")
	       else Expr(x / toRR(y,precision(x.re)))
	       )
     	  is y:RR do (					    -- # typical value: symbol /, CC, RR, CC
	       -- if y === 0 then buildErrorPacket("division by zero") else
	       Expr(x / y))
     	  is y:CC do (					    -- # typical value: symbol /, CC, CC, CC
	       -- if y === 0 then buildErrorPacket("division by zero") else
	       Expr(x / y))
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,DivideS))
     is x:RawMonomial do (
	  when rhs
	  is y:RawMonomial do (				    -- # typical value: symbol /, RawMonomial, RawMonomial, RawMonomial
	       when x/y
	       is z:RawMonomial do Expr(z)
	       is null do buildErrorPacket(EngineError("monomial division overflow"))
	       )
	  else binarymethod(localInterpState,lhs,rhs,DivideS))
     is Error do lhs
     else binarymethod(localInterpState,lhs,rhs,DivideS));

export (lhs:Expr) / (rhs:Expr) : Expr := RealDivide(threadLocalInterpState,lhs,rhs);

divideC(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     l := eval(localInterpState,lhs);
     when l is Error do l
     else (
     	  r := eval(localInterpState,rhs);
     	  when r is Error do r
	  else l/r));
setup(DivideS,divideC);
RealQuotient(localInterpState:threadLocalInterp,lhs:Expr,rhs:Expr):Expr := (
     when lhs
     is x:ZZ do (
	  when rhs
	  is y:ZZ do (					    -- # typical value: symbol //, ZZ, ZZ, ZZ
	       if y === 0
	       then Expr(toInteger(0))
	       else Expr(x//y)
	       )
     	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,SlashSlashS))
     is x:RawRingElement do (
	  when rhs
	  is y:RawRingElement do (			    -- # typical value: symbol //, RawRingElement, RawRingElement, RawRingElement
	       when x//y
	       is t:RawRingElement do Expr(t)
	       is null do buildErrorPacket(EngineError("polynomial division failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,SlashSlashS))
     is Error do lhs
     else binarymethod(localInterpState,lhs,rhs,SlashSlashS));
export (lhs:Expr) // (rhs:Expr) : Expr := RealQuotient(threadLocalInterpState,lhs,rhs);
quotientC(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     l := eval(localInterpState,lhs);
     when l is Error do l
     else (
     	  r := eval(localInterpState,rhs);
     	  when r is Error do r
	  else l//r));
setup(SlashSlashS,quotientC);

BackslashBackslashFun(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := binarymethod(localInterpState,lhs,rhs,BackslashBackslashS);
setup(BackslashBackslashS,BackslashBackslashFun);

adjacentFun(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := eval(localInterpState,Code(adjacentCode(lhs,rhs,codePosition(rhs))));
setup(AdjacentS,adjacentFun);

doublepower(x:double,n:int):double := (
     if n == 0 then return 1.0;
     if n < 0 then (x = 1./x; n = -n;);
     y := x;
     z := 1.0;
     while true do (
	  if (n & 1) != 0 then z = z * y;
	  n = n >> 1;
	  if n == 0 then break;
	  y = y * y;
	  );
     z
     );
OneE := Expr(toInteger(1));
BinaryPowerMethod(localInterpState:threadLocalInterp,x:Expr,y:Expr):Expr := (
     when y is i:ZZ do (
	  if i === 0 then (
	       onex := lookup(Class(x),OneE);
	       if onex == nullE then (
		    return buildErrorPacket("missing unit element")
		    )
	       else return onex;
	       );
	  if i < 0 then (
	       i = -i;
	       inver := lookup(Class(x),InverseS);
	       if inver == nullE then return MissingMethod("^","InverseMethod");
	       x = applyEE(localInterpState,inver,x);
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
BinaryPowerMethodFun(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when e is a:Sequence do
     if length(a)==2 then
     BinaryPowerMethod(localInterpState, a . 0, a . 1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("BinaryPowerMethod",BinaryPowerMethodFun);
SimplePowerMethod(localInterpState:threadLocalInterp,x:Expr,y:Expr):Expr := (
     when y is i:ZZ do (
	  if i === 0 then (
	       onex := lookup(Class(x),OneE);
	       if onex == nullE
	       then return buildErrorPacket("missing unit element")
	       else return onex;
	       );
	  if i <= 0 then (
	       i = -i;
	       inver := lookup(Class(x),InverseS);
	       if inver == nullE then return MissingMethod("^","InverseMethod");
	       x = applyEE(localInterpState,inver,x);
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
SimplePowerMethodFun(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when e is a:Sequence do
     if length(a)==2 then
     SimplePowerMethod(localInterpState,a . 0, a . 1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("SimplePowerMethod",SimplePowerMethodFun);

RealPower(localInterpState:threadLocalInterp,lhs:Expr,rhs:Expr):Expr := (
     when lhs
     is Error do lhs
     is x:ZZ do (
	  when rhs
	  is y:ZZ do (
	       if !isNegative(y) then Expr(x^y) 
	       else if x === 1 then Expr(x)
	       else if x === -1 then (
		    if int(y%ushort(2)) == 0
		    then Expr(toInteger(1))
		    else Expr(toInteger(-1)))
	       else if isZero(x) then buildErrorPacket("division by zero")
	       else Expr(newRationalCanonical(toInteger(1),x^-y)))
	  is y:QQ do (
	       d := denominator(y);
	       if d === 1 then Expr(x^numerator(y))
	       else if isNegative(x)
	       then if isOdd(d) then (
		    if isOdd(numerator(y))
		    then Expr(-toRR(-x)^toRR(y))
		    else Expr( toRR(-x)^toRR(y))
		    )
	       else Expr(toCC(x)^toRR(y))
	       else Expr(toRR(x)^toRR(y))
	       )
	  is y:RR do (
	       if isULong(x) then Expr(toULong(x) ^ y)
	       else if isNegative(x)
	       then Expr(toCC(x,precision(y))^y)
	       else Expr(toRR(x,precision(y))^y)
	       )
	  is y:CC do Expr(toRR(x,precision(y))^y)
     	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PowerS))
     is x:QQ do (
	  when rhs
	  is y:ZZ do Expr(x^y)
	  is y:QQ do (
	       d := denominator(y);
	       if d === 1 then Expr(x^numerator(y))
	       else if isNegative(x)
	       then if isOdd(d) then (
		    if isOdd(numerator(y))
		    then Expr(-toRR(-x)^toRR(y))
		    else Expr( toRR(-x)^toRR(y))
		    )
	       else Expr(toCC(x)^toRR(y))
	       else Expr(toRR(x)^toRR(y))
	       )
	  is y:RR do (
	       if isNegative(x)
	       then Expr(toCC(x,precision(y))^y)
	       else Expr(toRR(x,precision(y))^y))
	  is y:CC do Expr(toRR(x,precision(y))^y)
     	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PowerS))
     is x:RR do (
	  when rhs
	  is y:ZZ do Expr(x^y)
	  is y:QQ do (
	       d := denominator(y);
	       if d === 1 then Expr(x^numerator(y))
	       else if isNegative(x)
	       then if isOdd(d) then (
		    if isOdd(numerator(y))
		    then Expr(-(-x)^toRR(y,precision(x)))
		    else Expr( (-x)^toRR(y,precision(x)))
		    )
	       else Expr(toCC(x)^toRR(y,precision(x)))
	       else Expr(x^toRR(y,precision(x)))
	       )
	  is y:RR do (
	       if isNegative(x)
	       then Expr(toCC(x)^y)
	       else Expr(     x ^y)
	       )
	  is y:CC do Expr(x^y)
     	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PowerS))
     is x:CC do (
	  when rhs
	  is y:ZZ do Expr(x^y)
	  is y:QQ do (
	       if denominator(y) === 1 then Expr(x^numerator(y))
	       else if denominator(y) === 2 then Expr(sqrt(x)^numerator(y))
	       else Expr(x^toRR(y,precision(x))))
	  is y:RR do Expr(x^y)
	  is y:CC do Expr(x^y)
     	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,PowerS))
     is x:RawRingElement do (
	  when rhs
	  is y:ZZ do (
	       when x^y
	       is t:RawRingElement do Expr(t)
	       is null do buildErrorPacket(EngineError("polynomial power failed"))
	       )
	  is Error do rhs
	  else binarymethod(localInterpState,lhs,rhs,StarS))
     is x:RawMonomial do (
	  when rhs
	  is y:ZZ do
	  if isInt(y)
	  then (
	       when x ^ toInt(y)
	       is z:RawMonomial do Expr(z)
	       is null do buildErrorPacket(EngineError("monomial power overflow"))
	       )
	  else WrongArgSmallInteger(2)
	  else binarymethod(localInterpState,lhs,rhs,DivideS))
     else binarymethod(localInterpState,lhs,rhs,PowerS));

export (lhs:Expr) ^ (rhs:Expr) : Expr := RealPower(threadLocalInterpState,lhs,rhs);
powerC(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     l := eval(localInterpState,lhs);
     when l is Error do l
     else (
     	  r := eval(localInterpState,rhs);
     	  when r is Error do r
	  else l^r));
setup(PowerS,powerC);
powerfun(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when e is a:Sequence do
     if length(a) == 2 then a.0^a.1
     else WrongNumArgs("^",2)
     else WrongNumArgs("^",2));
setupfun("power",powerfun);
logorfun(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     left := eval(localInterpState,lhs);
     when left
     is Error do left
     else (
	  if left == True then True
	  else if left == False then (
	       right := eval(localInterpState,rhs);
	       when right
	       is Error do right
	       else (
		    if right == True then True
		    else if right == False then False
		    else binarymethod(localInterpState,left,right,orS)))
	  else binarymethod(localInterpState,left,rhs,orS)));
setup(orS,logorfun);
BarBarF(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := binarymethod(localInterpState,lhs,rhs,BarBarS);
setup(BarBarS,BarBarF);
logandfun(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     a := eval(localInterpState,lhs);
     when a
     is Error do a
     else (
	  if a == False then False
	  else if a == True then (
	       b := eval(localInterpState,rhs);
	       when b
	       is Error do b
	       else (
		    if b == True then True
		    else if b == False then False
		    else binarymethod(localInterpState,a,b,andS)))
	  else binarymethod(localInterpState,a,rhs,andS)));
setup(andS,logandfun);
export notFun(localInterpState:threadLocalInterp,a:Expr):Expr := if a == True then False else if a == False then True else unarymethod(localInterpState,a,notS);
export notFun(localInterpState:threadLocalInterp,rhs:Code):Expr := (
     a := eval(localInterpState,rhs);
     when a
     is Error do a
     else if a == True then False
     else if a == False then True
     else unarymethod(localInterpState,a,notS));
setup(notS,notFun);
EqualEqualEqualfun(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     x := eval(localInterpState,lhs);
     when x is Error do x
     else (
     	  y := eval(localInterpState,rhs);
     	  when y is Error do y
	  else equal(x,y)));
setup(EqualEqualEqualS,EqualEqualEqualfun);
quicknot(z:Expr):Expr := (
     when z is Error do z else if z == True then False else True
     );
notEqualEqualEqualfun(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := quicknot(EqualEqualEqualfun(localInterpState,lhs,rhs));
setup(NotEqualEqualEqualS,notEqualEqualEqualfun);
smallintarrays0 := new array(Expr) len 20 at i do (
     provide Expr(new Sequence len i+1 at k do provide toInteger(k)));
smallintarrays1 := new array(Expr) len 20 at i do (
     provide Expr(new Sequence len i at k do provide toInteger(1+k)));
DotDotfun(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     left := eval(localInterpState,lhs);
     when left
     is Error do left
     is x:ZZ do (
	  right := eval(localInterpState,rhs);
	  when right
	  is Error do right
	  is y:ZZ do (
	       if isInt(x) && isInt(y) then (
	  	    i := toInt(x);
		    j := toInt(y);
		    if i>j then emptySequenceE
		    else if i==0 && j<length(smallintarrays0)
		    then smallintarrays0.j
		    else if i==1 && j<length(smallintarrays1)
		    then smallintarrays1.j
		    else (
			 if j-i+1 > 100000
			 then buildErrorPacket("ZZ .. ZZ: very long sequence requested")
			 else Expr(new Sequence len j-i+1 at k do provide toInteger(i+k))))
	       else (
		    z := y-x;
		    if z <= 0 then emptySequenceE
		    else if isInt(z) then (
			 m := toInt(z);
			 Expr(new Sequence len m+1 at k do provide x+k))
		    else printErrorMessageE(rhs,"range too large")))
	  else binarymethod(localInterpState,left,right,DotDotS))
     else binarymethod(localInterpState,left,rhs,DotDotS));
setup(DotDotS,DotDotfun);

DotDotLessFun(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     left := eval(localInterpState,lhs);
     when left
     is Error do left
     is x:ZZ do (
	  right := eval(localInterpState,rhs);
	  when right
	  is Error do right
	  is y:ZZ do (
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
			 else Expr(new Sequence len j-i+1 at k do provide toInteger(i+k))))
	       else (
		    z := y-x;
		    if z <= 0 then emptySequenceE
		    else if isInt(z) then (
			 m := toInt(z)-1;
			 Expr(new Sequence len m+1 at k do provide x+k))
		    else printErrorMessageE(rhs,"range too large")))
	  else binarymethod(localInterpState,left,right,DotDotLessS))
     else binarymethod(localInterpState,left,rhs,DotDotLessS));
setup(DotDotLessS,DotDotLessFun);

assignNewFun(localInterpState:threadLocalInterp,newclass:Code,rhs:Code):Expr := (
     c := eval(localInterpState,newclass);
     when c
     is Error do c
     is o:HashTable do installMethod(NewE,o,eval(localInterpState,rhs))
     else printErrorMessageE(newclass,"expected a hash table as prospective class"));
AssignNewFun = assignNewFun;
assignNewOfFun(localInterpState:threadLocalInterp,newclass:Code,newparent:Code,rhs:Code):Expr := (
     c := eval(localInterpState,newclass);
     when c is Error do c
     is cc:HashTable do (
	  p := eval(localInterpState,newparent);
	  when p is Error do p
	  is pp:HashTable do installMethod(NewOfE,cc,pp,eval(localInterpState,rhs))
	  else printErrorMessageE(newparent,"expected a hash table as prospective parent"))
     else printErrorMessageE(newclass,"expected a hash table as prospective class"));
AssignNewOfFun = assignNewOfFun;
assignNewFromFun(localInterpState:threadLocalInterp,newclass:Code,newinitializer:Code,rhs:Code):Expr := (
     c := eval(localInterpState,newclass);
     when c is Error do c
     is cc:HashTable do (
	  i := eval(localInterpState,newinitializer);
	  when i is Error do i
	  is ii:HashTable do installMethod(NewFromE,cc,ii,eval(localInterpState,rhs))
     	  else printErrorMessageE(newinitializer,"expected a hash table"))
     else printErrorMessageE(newclass,"expected a hash table as prospective class"));
AssignNewFromFun = assignNewFromFun;
assignNewOfFromFun(localInterpState:threadLocalInterp,args:CodeSequence):Expr := (
     newclass := args.0;
     newparent := args.1;
     newinitializer := args.2;
     rhs := args.3;
     c := eval(localInterpState,newclass);
     when c 
     is Error do c 
     is cc:HashTable do (
	  p := eval(localInterpState,newparent);
	  when p
	  is Error do p
	  is pp:HashTable do (
	       i := eval(localInterpState,newinitializer);
	       when i 
	       is Error do i 
	       is ii:HashTable do (
	       	    r := eval(localInterpState,rhs);
	       	    when r 
		    is Error do r
	       	    else installMethod(NewOfFromE,cc,pp,ii,r))
     	       else printErrorMessageE(newinitializer,"expected a hash table as class to initialize from"))
	  else printErrorMessageE(newparent,"expected a hash table as prospective parent"))
     else printErrorMessageE(newclass,"expected a hash table as prospective class")
     );
AssignNewOfFromFun = assignNewOfFromFun;
installFun2(localInterpState:threadLocalInterp,a:Expr,args:CodeSequence):Expr := (
     opr := eval(localInterpState,args.0);
     when opr 
     is Error do opr
     is oper:SymbolClosure do (
	  if oper === AdjacentS then (
	       b := eval(localInterpState,args.2);
	       when b
	       is Error do b
	       is bcd:Sequence do (
		    if length(bcd) == 2 then (
			 when bcd.0
			 is bb:HashTable do
			 when bcd.1
			 is cc:HashTable do installMethod(a,bb,cc,eval(localInterpState,args.3))
			 else buildErrorPacket("expected second parameter to be a hash table")
			 else buildErrorPacket("expected first parameter to be a hash table")
			 )
		    else if length(bcd) == 3 then (
			 when bcd.0 is bb:HashTable do
			 when bcd.1 is cc:HashTable do
			 when bcd.2 is dd:HashTable do installMethod(a,bb,cc,dd,eval(localInterpState,args.3)) 
			 else buildErrorPacket("expected third parameter to be a hash table")
			 else buildErrorPacket("expected second parameter to be a hash table")
			 else buildErrorPacket("expected first parameter to be a hash table")
			 )
		    else if length(bcd) == 4 then (
			 when bcd.0 is bb:HashTable do
			 when bcd.1 is cc:HashTable do
			 when bcd.2 is dd:HashTable do
			 when bcd.3 is ee:HashTable do installMethod(a,bb,cc,dd,ee,eval(localInterpState,args.3)) 
			 else buildErrorPacket("expected fourth parameter to be a hash table")
			 else buildErrorPacket("expected third parameter to be a hash table")
			 else buildErrorPacket("expected second parameter to be a hash table")
			 else buildErrorPacket("expected first parameter to be a hash table")
			 )
		    else buildErrorPacket("expected 1, 2, 3, or 4 parameter types"))
	       is bb:HashTable do installMethod(a,bb,eval(localInterpState,args.3))
	       else buildErrorPacket("expected right hand parameter to be a hash table or sequence"))
	  else buildErrorPacket("expected adjacency operator ' ' on left"))
     else buildErrorPacket("expected operator to be a symbol"));
installMethodFun2(localInterpState:threadLocalInterp,arg1:Expr,args:CodeSequence):Expr := (
     when arg1 
     is Error do arg1
     is CompiledFunction do installFun2(localInterpState,arg1,args)
     is CompiledFunctionClosure do installFun2(localInterpState,arg1,args)
     is FunctionClosure do installFun2(localInterpState,arg1,args)
     is s:SpecialExpr do if ancestor(s.class,functionClass) then installFun2(localInterpState,arg1,args) else buildErrorPacket("expected right hand parameter to be a type of function")
     is aa:HashTable do (
	  if aa.parent == nothingClass
	  then (
	       installFun2(localInterpState,arg1,args)	  -- handle, e.g., Ext(ZZ, Module, Module) := (i,M,N) -> ...
	       )
	  else (
	       b := eval(localInterpState,args.2);
	       when b is Error do b 
	       is bb:HashTable do (
		    opr := eval(localInterpState,args.0);
		    when opr is Error do opr
		    else installMethod(opr,aa,bb,eval(localInterpState,args.3)))
	       else buildErrorPacket("expected right hand parameter to be a hash table")))
     else buildErrorPacket("expected left hand parameter to be a function, type, or a hash table"));
installMethodFun(localInterpState:threadLocalInterp,args:CodeSequence):Expr := installMethodFun2(localInterpState,eval(localInterpState,args.1),args);
InstallMethodFun = installMethodFun;

mess1 := "objects on left hand side of assignment are not types (use ':=' instead?)";

-- this new version just looks up a method for user code, which *could* do the same thing
installValueFun(localInterpState:threadLocalInterp,args:CodeSequence):Expr := (
     oper := eval(localInterpState,args.0);
     when oper is Error do return oper else nothing;
     x := eval(localInterpState,args.1);
     when x is Error do return x else nothing;
     y := eval(localInterpState,args.2);
     when y is Error do return y else nothing;
     meth := lookupBinaryMethod(Class(x),Class(y),Expr(Sequence(oper,EqualE))); -- i.e., x*y=z is looked up under ((symbol *,symbol =),class x,class y)
     if meth == nullE then return MissingAssignmentMethodPair(oper,x,y);
     z := eval(localInterpState,args.3);
     applyEEE(localInterpState,meth,x,y,z));
-- this old version was used for stashing values somewhere
-- installValueFun(args:CodeSequence):Expr := (
--      a := eval(localInterpState,args.1);
--      when a is Error do a
--      is aa:HashTable do (
-- 	  b := eval(localInterpState,args.2);
-- 	  when b is Error do b 
-- 	  is bb:HashTable do (
-- 	       opr := eval(localInterpState,args.0);
-- 	       when opr is Error do opr
-- 	       else (
-- 		    x := eval(localInterpState,args.3);
-- 		    when x is Error do x
-- 		    else installValue(opr,aa,bb,x)
-- 		    )
-- 	       )
-- 	  else buildErrorPacket(mess1))
--      else buildErrorPacket(mess1));
InstallValueFun = installValueFun;

unaryInstallMethodFun(localInterpState:threadLocalInterp,meth:Code,argtype:Code,body:Code):Expr := (
     f := eval(localInterpState,meth);
     when f is Error do f
     else (
	  t := eval(localInterpState,argtype);
	  when t is Error do t 
	  else when t is T:HashTable do (
	       b := eval(localInterpState,body);
	       when b is Error do b
	       else installMethod(f,T,b)
	       )
	  else printErrorMessageE(argtype,"expected a hash table")));
UnaryInstallMethodFun = unaryInstallMethodFun;

unaryInstallValueFun(localInterpState:threadLocalInterp,meth:Code,lhs:Code,rhs:Code):Expr := (
     oper := eval(localInterpState,meth);
     when oper is Error do return oper else nothing;
     y := eval(localInterpState,lhs);
     when y is Error do return y else nothing;
     method := lookup(Class(y),Expr(Sequence(oper,EqualE))); -- i.e., *y=z is looked up under ((symbol *,symbol =),class y)
     if method == nullE then return MissingAssignmentMethod(oper,y);
     z := eval(localInterpState,rhs);
     applyEEE(localInterpState,method,y,z));
-- this old version was used for stashing values somewhere
-- unaryInstallValueFun(meth:Code,argtype:Code,body:Code):Expr := (
--      Argtype := eval(localInterpState,argtype);
--      when Argtype is Error 
--      do Argtype 
--      else when Argtype is
--      o:HashTable do (
-- 	  methv := eval(localInterpState,meth);
-- 	  when methv is Error do methv else (
-- 	       bodyv := eval(localInterpState,body);
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
	       if ancestor(x.class,listClass) 
	       then return x.v
	       else return a
	       )
	  else return a;
	  );
     foreach i in a do (
     	  when i is ii:List do (
	       if ancestor(ii.class,listClass) then (
	       	    hadlist = true; 
	       	    newlen = newlen + length(ii.v) - 1;
	       	    ))
     	  else nothing;
	  );
     if hadlist then (
	  new Sequence len newlen do (
     	       foreach i in a do (
     	       	    when i is ii:List do (
			 if ancestor(ii.class,listClass) then (
     	       	    	      foreach j in ii.v do provide j
			      )
			 else provide i
			 )
     	       	    else provide i)))
     else a);
flatten(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when e
     is v:Sequence do Expr(flatten(v))
     is a:List do list(
	  a.class,
	  if a.mutable then (
	       r := flatten(a.v);
	       if r == a.v then copy(r) else r
	       )
	  else flatten(a.v),
	  a.mutable)
     else WrongArg("a list or sequence"));
setupfun("flatten",flatten);

subvalue(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     left := eval(localInterpState,lhs);
     when left is Error do left
     else (
      	  right := eval(localInterpState,rhs);
      	  when right is Error do right
      	  else subvalue(left,right)));
lengthFun(localInterpState:threadLocalInterp,rhs:Code):Expr := (
     e := eval(localInterpState,rhs);
     when e
     is Error do e
     is x:HashTable do Expr(toInteger(x.numEntries))
     is x:Sequence do Expr(toInteger(length(x)))
     is dc:DictionaryClosure do Expr(toInteger(dc.dictionary.symboltable.numEntries))
     is x:List do Expr(toInteger(length(x.v)))
     is s:string do Expr(toInteger(length(s)))
     is n:Net do Expr(toInteger(length(n.body)))
     else buildErrorPacket("expected a list, sequence, hash table, or string"));
setup(SharpS,lengthFun,subvalue);
subvalueQ(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     left := eval(localInterpState,lhs);
     when left is Error do left
     else (
      	  right := eval(localInterpState,rhs);
      	  when right is Error do right
      	  else subvalueQ(left,right)));
setup(SharpQuestionS,subvalueQ);

isFinite(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when e
     is x:ZZ do True
     is x:QQ do True
     is x:RR do toExpr(isfinite(x))
     is x:CC do toExpr(isfinite(x))
     else WrongArg("a number")
     );
setupfun("isFinite",isFinite);

isANumber(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when e
     is x:ZZ do True
     is x:QQ do True
     is x:RR do toExpr(!isnan(x))
     is x:CC do toExpr(!isnan(x))
     else WrongArg("a number")
     );
setupfun("isANumber",isANumber);

isInfinite(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when e
     is x:ZZ do False
     is x:QQ do False
     is x:RR do toExpr(isinf(x))
     is x:CC do toExpr(isinf(x))
     else WrongArg("a number")
     );
setupfun("isInfinite",isInfinite);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
