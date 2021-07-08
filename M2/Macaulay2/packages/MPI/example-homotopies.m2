-* 
mpirun -np 4 ./M2 --script MPI/example-homotopies.m2
*-

debug Core
master = 0
myID = myProcessNumber()
nWorkers = numberOfProcesses() - 1

broadcast = method()
broadcast String := List => s -> for i from 1 to nWorkers do (
    sendString(s,i);
    receiveString i;
    )

needsPackage "NumericalAlgebraicGeometry"; -- can't do locally???
if myID!=master then (
    notDone := true;
    while notDone do (
        s := receiveString master;
        << "-- " << myID << " received: " << s << endl;
        notDone = (s!="end");
        r := value s;
        << "-- " << myID << " result: " << r << endl;
        sendString(toString r, master)
     	)
    ) else (
    --broadcast ///needsPackage "NumericalAlgebraicGeometry"///;
    n = 4;
    R = CC[x_1..x_n];    
    broadcast ("n = " | toString n | "; R = CC[x_1..x_n]");
    T = for i from 1 to n list random(2, R) + random(1, R) - 1;
    (S,solsS) = totalDegreeStartSystem T;
    broadcast("S = "|toString S); 
    broadcast("T = "|toString T); 
    blockSize = #solsS//nWorkers + 1;
    for i from 1 to nWorkers do 
    sendString("coordinates\\track(S,T,"|toString\\coordinates\take(solsS,{(i-1)*blockSize, min(i*blockSize-1,#solsS-1)})|")", i);
    solsT = flatten for i from 1 to nWorkers list value receiveString i;
    sleep 1;
    << "master: received " << # solsT << " solutions" << endl << endl;
    << netList solsT << endl;
    sleep 1;
    broadcast "end";
    sleep 2;
    );
end--

