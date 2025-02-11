-* 
Run on 4 "processors":

mpirun -np 4 ./M2 --script MPI/master-worker-MPI.m2

After the example is executed, the MASTER becomes interactive.
*-
needsPackage "MPI"
master = 0
numberOfWorkers = numberOfProcesses()-1
myID = myProcessNumber()
notDone = true
if myID!=master then while true do (
    (tag,s) := receiveStringMPI master;
    << "-- " << myID << " received: " << s << endl;
    r := value s;
    << "-- " << myID << " result: " << r << endl;
    sendStringMPI(toString r, master);
    ) 
addEndFunction(()->(for i from 1 to numberOfWorkers do sendStringMPI("exit 0",i); -*sleep 1*-));

-- write the code for master below
for i from 1  to numberOfWorkers do ( 
    s = toString i | "+" | toString i;
    << "-- " << myID << " sent: " << s << endl;
    sendStringMPI(s,i);
    )

<< "-- " << myID << " received (from all workers): " << endl;  
<< broadcastReceive() << endl;



