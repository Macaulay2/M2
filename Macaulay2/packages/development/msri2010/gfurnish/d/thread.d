use C;
use system; 
use util;
use convertr;
use binding;
use nets;
use parser;
use lex;
use gmp;
use engine;
use util;
use tokens;
use err;
use stdiop;
use ctype;
use stdio;
use varstrin;
use strings;
use basic;
use struct;
use objects;
use evaluate;
use common;

export exprCell := { e:Expr };

export rawLaunchThread(localInterpState:threadLocalInterp,e:Expr):Expr := (Ccode(void, "rawTestThread(",exprCell(e),")"); return nullE; );
setupfun("rawLaunchThread", rawLaunchThread);

export rawRunSequence(localInterpState:threadLocalInterp,ec:exprCell):void := (

      stdout << "test" << endl;
      a := ec.e;
      when a is seq:Sequence do (
       	    if length(seq)==2 then (
       	       applyEE(localInterpState,seq.0 ,seq.1);

            );
      )
      else (nullE;)
);
--setupfun("rawRunSequence",rawRunSequence);


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
