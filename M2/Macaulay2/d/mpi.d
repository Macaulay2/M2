use util;
use struct;

header "
extern int MPInumberOfProcesses();
extern int MPImyProcessNumber();
extern int MPIsendString();
";
export numberOfProcesses(e:Expr):Expr := toExpr(Ccode(int, "MPInumberOfProcesses()"));
setupfun("numberOfProcesses",numberOfProcesses);
export myProcessNumber(e:Expr):Expr := toExpr(Ccode(int, "MPImyProcessNumber()"));
setupfun("myProcessNumber",myProcessNumber);

-- input: (s,p) 
--   sends string s to processor p 
export sendString(e:Expr):Expr := ( 
    when e is seq:Sequence do
    if length(seq) != 2 then WrongNumArgs(2) else
    when seq.0 is s:stringCell do     
    when seq.1 is p:ZZcell do -- if p<0 or p>numberOfProcesses() then WrongArgSmallInteger(1) else -- !!! more meaningful error
    toExpr( Ccode( int, "MPIsendString(", s.v, ",", toInt(p), ")" ))
    else WrongArgZZ(2)
    else WrongArgString(1)
    else WrongNumArgs(2)
    );
setupfun("sendString",sendString);
