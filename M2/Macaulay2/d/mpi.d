use util;
use struct;

-- defined in M2lib.c
header "
#include \"M2/config.h\"
#ifdef WITH_MPI
#include \"mpi.h\"
#endif
int MPInumberOfProcesses();
int MPImyProcessNumber();
int MPIsendString(M2_string s, int p, int tag);
int MPIinterrupt(int p);
M2_string MPIreceiveString(int p, int* tagPtr);
";

MPIanyTag := Ccode(int, "
#ifdef WITH_MPI
MPI_ANY_TAG
#else
-1
#endif
");

export numberOfProcesses(e:Expr):Expr := (
    when e is seq:Sequence do
    if length(seq) != 0 then WrongNumArgs(0) else
    toExpr(Ccode(int, "MPInumberOfProcesses()"))
    else WrongNumArgs(0)    
    );    
setupfun("numberOfProcesses",numberOfProcesses);

export myProcessNumber(e:Expr):Expr := (
    when e is seq:Sequence do
    if length(seq) != 0 then WrongNumArgs(0) else
    toExpr(Ccode(int, "MPImyProcessNumber()"))
    else WrongNumArgs(0)    
    );
setupfun("myProcessNumber",myProcessNumber);

-- input: (s,tag,p) 
--   sends string `s` tagged with `tag` to processor `p` 
export sendStringWithTagMPI(e:Expr):Expr := ( 
    when e is seq:Sequence do
    if length(seq) != 3 then WrongNumArgs(3) else
    when seq.0 is s:stringCell do     
    when seq.1 is p:ZZcell do -- 
    when seq.2 is tag:ZZcell do -- if p<0 or p>numberOfProcesses() then WrongArgSmallInteger(1) else -- !!! more meaningful error
    toExpr( Ccode( int, "MPIsendString(", s.v, ",", toInt(p), ",", toInt(tag), ")" ))
    else WrongArgZZ(3)
    else WrongArgZZ(2)
    else WrongArgString(1)
    else WrongNumArgs(3)
    );
setupfun("sendStringWithTagMPI",sendStringWithTagMPI);

-- input: p 
--   receive a string from processor p 
export receiveStringMPI(e:Expr):Expr := ( 
    when e is p:ZZcell do (
	tagInt := MPIanyTag;
	message := toExpr(Ccode(string, "MPIreceiveString(", toInt(p), ",&", tagInt, ")"));
	Expr(Sequence(toExpr(tagInt), message))
	)
    else WrongArgZZ(1)    
    );
setupfun("receiveStringMPI",receiveStringMPI);

-- input: p 
--   interrupt process p
export interruptMPI(e:Expr):Expr := ( 
    when e
    is p:ZZcell do toExpr(Ccode(int, "MPIinterrupt(", toInt(p), ")"))
    else WrongArgZZ()
    );
setupfun("interruptMPI",interruptMPI);
