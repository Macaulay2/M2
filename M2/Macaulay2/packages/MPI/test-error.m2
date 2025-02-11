-* 
Run on 4 "processors":

mpirun -np 4 ./M2 --script MPI/test-error.m2

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
    local r;
    try (r = value s) then err := false else (err = true; r = "!!!ERROR!!!");	
    << "-- " << myID << " result: " << r << endl;
    (if err then sendErrorMPI else sendStringMPI) (toString r, master);
    ) 
addEndFunction(()->(for i from 1 to numberOfWorkers do sendStringMPI("exit 0",i); -*sleep 1*-));

-- write the code for master below
broadcastSend "error \"!!!\""
print broadcastReceive()
