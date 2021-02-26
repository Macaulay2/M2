-* 
mpirun -np 4 ./M2 --script MPI/example2.m2
*-

debug Core
master = 0
myID = myProcessNumber()
nWorkers = numberOfProcesses() - 1

broadcast = method()
broadcast String := List => (str) -> (
    )
if myID!=master then (
    notDone := true;
    while notDone do (
        s := receiveString master;
        << "-- " << myID << " received: " << s << endl;
        notDone = (s!="end");
        r := value s;
        << "-- " << myID << " result: " << r << endl;
        sendString(toString r, master)
     )) else (

     for i from 1 to nWorkers do (
         sendString(///needsPackage "NumericalAlgebraicGeometry"///, i);
         receiveString i;
         );
     R = CC[x_1..x_10];
     S = for i from 1 to 10 list x_i^2-1;
     T = for i from 1 to 10 list random(2, R) + random(1, R) - 1;
     

ringStr = "R = CC[x_1..x_10];";
startSysStr = "S = for i from 1 to 10 list x_i^2-1;";
actualSysStr = "T = for i from 1 to 10 list random(2, R) + random(1, R) - 1"
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};



    -- write the code for master below
    -- broadcast the ring:
    sendString("R = ZZ/101[a..d];", 1);
    sendString("R = ZZ/101[a..d];", 2);
    sendString("R = ZZ/101[a..d];", 3);
    
    << "from 1: " << receiveString 1 << endl;
    << "from 2: " << receiveString 2 << endl;
    << "from 3: " << receiveString 3 << endl;

    sendString("gens gb ideal random(R^1, R^{-2,-2,-2,-2})", 1);
    sendString("gens gb ideal random(R^1, R^{-2,-2,-2,-2})", 2);
    sendString("gens gb ideal random(R^1, R^{-2,-2,-2,-2})", 3);
    
    << "from 1: " << receiveString 1 << endl;
    << "from 2: " << receiveString 2 << endl;
    << "from 3: " << receiveString 3 << endl;

    sendString("end",1);
    sendString("end",2);
    sendString("end",3);

    sleep 1
    )
end--

