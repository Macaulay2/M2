-* 
Run on 4 "processors":

mpirun -np 4 ./M2 --script MPI/test-interruption.m2

After the example is executed, the MASTER becomes interactive.
*-
needsPackage "MPI"
master = 0
numberOfWorkers = numberOfProcesses()-1
myID = myProcessNumber()
notDone = true
if myID!=master then while true do (
    (tag,s) := receiveStringMPI master;
    << "-- " << myID << " received(tag): " << tag << endl;
    << "-- " << myID << " received(str): " << s << endl;
    r := value s;
    << "-- " << myID << " result: " << r << endl;
    sendStringMPI(toString r, master);
    ) 
addEndFunction(()->(for i from 1 to numberOfWorkers do sendStringMPI("exit 0",i); -*sleep 1*-));

-- write the code for master below
broadcastSend "Good'night"
print broadcastReceive()
broadcastSend ///
R = ZZ/101 [x_1..x_20];
res coker vars R
///
for i from 1  to numberOfWorkers do (
    sleep 5;
    interruptMPI i;
    << "-- interrupting " << i << endl; 
    )
print broadcastReceive()
