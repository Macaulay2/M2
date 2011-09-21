-- Copyright 2009 by Daniel R. Grayson
use common;
use util;

import factorint(x:ZZ,flags:long):array(array(ZZ));
pfactorint(e:Expr):Expr := (
     when e
     is x:ZZcell do toExpr(factorint(x.v,long(0)))
     is s:Sequence do if length(s) != 2 then WrongNumArgs(1,2) else
     when s.0 is x:ZZcell do
     when s.1 is flags:ZZcell do 
     if !isInt(flags) then WrongArgSmallInteger(2)
     else toExpr(factorint(x.v,long(toInt(flags))))
     else WrongArgSmallInteger(2)
     else WrongArgSmallInteger(1)
     else WrongArgZZ());
setupfun("Pari$factorint",pfactorint);

import isprime(x:ZZ):bool;
pisprime(e:Expr):Expr := when e is x:ZZcell do toExpr(isprime(x.v)) else WrongArgZZ();
setupfun("Pari$isprime",pisprime);

import ispseudoprime(x:ZZ,flags:long):bool;
pispseudoprime(e:Expr):Expr := (
     when e
     is x:ZZcell do toExpr(ispseudoprime(x.v,long(0)))
     is s:Sequence do if length(s) != 2 then WrongNumArgs(1,2) else
     when s.0 is x:ZZcell do
     when s.1 is flags:ZZcell do 
     if !isInt(flags) then WrongArgSmallInteger(2)
     else toExpr(ispseudoprime(x.v,long(toInt(flags))))
     else WrongArgSmallInteger(2)
     else WrongArgSmallInteger(1)
     else WrongArgZZ());
setupfun("Pari$ispseudoprime",pispseudoprime);

import test():void;
ptest(e:Expr):Expr := (test();nullE);
setupfun("Pari$test",ptest);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d pari.o "
-- End:
