-* 
mpirun -np 4 ./M2 --script  MPI/example-homotopies.m2
mpirun -np 4 ./M2 --q --stop --silent  MPI/example-homotopies.m2
*-
-- COMMON part
needsPackage "MPI"
master = 0
myID = myProcessNumber()
nWorkers = numberOfProcesses() - 1
needsPackage "NumericalAlgebraicGeometry";
n = 12
R = CC[x_1..x_n]    
setRandomSeed 0
T = for i from 1 to n list random(2, R) + random(1, R) + 1
(S,solsS) = totalDegreeStartSystem T;

if myID!=master then ( -- WORKER part
    while true do (
        s := receiveString master;
        r := value s; 
        sendString(toString r, master)
     	) -- e.g., when s == "exit 0" the process quits
    )
-- MASTER part 
addEndFunction(()->broadcastSend "exit 0") 
blockSize = #solsS//nWorkers + 1
startTime := currentTime()
for i from 1 to nWorkers do sendString(
    "coordinates\\track(S,T,"|toString\\coordinates\take(
	solsS,{(i-1)*blockSize, min(i*blockSize-1,#solsS-1)}
	)|")", i)
solsT = flatten for i from 1 to nWorkers list (
    value receiveString i
    )
print("#solutions: "|#solsT)
print("wall timing: " | (currentTime() - startTime) | " seconds") 
