-- Copyright 2009 by Daniel R. Grayson
use gmp;
use tokens;
use common;

toExpr(x:array(ZZ)):Expr := new Sequence len length(x) do foreach i in x do provide Expr(i);
toExpr(x:array(array(ZZ))):Expr := new Sequence len length(x) do foreach i in x do provide toExpr(i);

import factorint(x:ZZ,flags:long):array(array(ZZ));
pfactorint(e:Expr):Expr := (
     when e
     is x:ZZ do toExpr(factorint(x,long(0)))
     is s:Sequence do if length(s) != 2 then WrongNumArgs(1,2) else
     when s.0 is x:ZZ do
     when s.1 is flags:ZZ do 
     if !isInt(flags) then WrongArgSmallInteger(2)
     else toExpr(factorint(x,long(toInt(flags))))
     else WrongArgSmallInteger(2)
     else WrongArgSmallInteger(1)
     else WrongArgZZ());
setupfun("Pari$factorint",pfactorint);

import isprime(x:ZZ):bool;
pisprime(e:Expr):Expr := when e is x:ZZ do toExpr(isprime(x)) else WrongArgZZ();
setupfun("Pari$isprime",pisprime);

import ispseudoprime(x:ZZ,flags:long):bool;
pispseudoprime(e:Expr):Expr := (
     when e
     is x:ZZ do toExpr(ispseudoprime(x,long(0)))
     is s:Sequence do if length(s) != 2 then WrongNumArgs(1,2) else
     when s.0 is x:ZZ do
     when s.1 is flags:ZZ do 
     if !isInt(flags) then WrongArgSmallInteger(2)
     else toExpr(ispseudoprime(x,long(toInt(flags))))
     else WrongArgSmallInteger(2)
     else WrongArgSmallInteger(1)
     else WrongArgZZ());
setupfun("Pari$ispseudoprime",pispseudoprime);

import test():void;
ptest(e:Expr):Expr := (test();nullE);
setupfun("Pari$test",ptest);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d pari.oo "
-- End:
