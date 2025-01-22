-* 
mpirun -np 4 ./M2 --script MPI/example2.m2
*-
debug Core
master = 0
myID = myProcessNumber()
notDone = true

if myID!=master then (
    while notDone do (
        s := receiveStringMPI master;
        << "-- " << myID << " received: " << s << endl;
        notDone = (s!="end");
        r := value s;
        << "-- " << myID << " result: " << r << endl;
        sendStringMPI(toString r, master)
     )) else (
    -- write the code for master below
    -- broadcast the ring:
    sendStringMPI("R = ZZ/101[a..d];", 1);
    sendStringMPI("R = ZZ/101[a..d];", 2);
    sendStringMPI("R = ZZ/101[a..d];", 3);
    
    << "from 1: " << receiveStringMPI 1 << endl;
    << "from 2: " << receiveStringMPI 2 << endl;
    << "from 3: " << receiveStringMPI 3 << endl;

    sendStringMPI("gens gb ideal random(R^1, R^{-2,-2,-2,-2})", 1);
    sendStringMPI("gens gb ideal random(R^1, R^{-2,-2,-2,-2})", 2);
    sendStringMPI("gens gb ideal random(R^1, R^{-2,-2,-2,-2})", 3);
    
    << "from 1: " << receiveStringMPI 1 << endl;
    << "from 2: " << receiveStringMPI 2 << endl;
    << "from 3: " << receiveStringMPI 3 << endl;

    sendStringMPI("end",1);
    sendStringMPI("end",2);
    sendStringMPI("end",3);

    sleep 1
    )
end--

