-* 
mpirun -np 2 ./M2 --script ../../Macaulay2/packages/MPI/master-worker-MPI.m2
*-
debug Core
master = 0
myID = myProcessNumber()
notDone = true
if myID!=master then while notDone do (
    s := receiveString master;
    << "-- " << myID << " received: " << s << endl;
    notDone = (s=="end");
    r := value s;
    << "-- " << myID << " result: " << r << endl;
    sendString(toString r, master)
    ) else (
-- write the code for master below
s = "2+2";
<< "-- " << myID << " sent: " << s << endl;
sendString(s,1);
r = receiveString 1;
<< "-- " << myID << " received: " << r << endl;
sendString("end",1);
sleep 1 
    )
end

