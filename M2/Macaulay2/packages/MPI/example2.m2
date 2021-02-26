-* 
mpirun -np 4 ./M2 --script MPI/example2.m2
*-
debug Core
master = 0
myID = myProcessNumber()
notDone = true

if myID!=master then (
    while notDone do (
        s := receiveString master;
        << "-- " << myID << " received: " << s << endl;
        notDone = (s!="end");
        r := value s;
        << "-- " << myID << " result: " << r << endl;
        sendString(toString r, master)
     )) else (
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

