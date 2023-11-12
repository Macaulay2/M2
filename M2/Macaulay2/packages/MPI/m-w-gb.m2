-* 
mpirun -np 3 M2 -q --stop --silent MPI/m-w-gb.m2

After the example is executed, the MASTER becomes interactive.
*-
needsPackage "MPI"
master = 0
numberOfWorkers = numberOfProcesses()-1
broadcast = s -> for i from 1 to numberOfWorkers do (
    sendString(s,i);
    receiveString i;
    )
myID = myProcessNumber()
loadPackage "RandomIdeals";
if myID == master then (
    addEndFunction(()->(for i from 1 to numberOfWorkers do sendString("exit 0",i); -*sleep 1*-));
    R = (ZZ/911)[a,b];
    m = matrix{apply(100,i->randomMonomial(10,R))};
    I = randomIdeal({11,13},m);
    assert(numberOfWorkers>1);
    broadcast("R = (ZZ/911)[a,b];");
    broadcast("I = "|toString I|";");
    sendString("groebnerBasis I",1);
    sendString("groebnerBasis(I,Strategy=>\"F4\")",2);
    G1 = value receiveString 1;
    G2 = value receiveString 2;
    assert(G1==G2);
--     broadcast "end"
    ) else ( -- WORKER -------------------------------------------
    notDone := true;
    while notDone do (
	s := receiveString master;
	<< "-- " << myID << " received: " << s << endl;
	notDone = (s!="end");
	r := value s;
	<< "-- " << myID << " result: " << r << endl;
	sendString(toString r, master)
	)
    )
