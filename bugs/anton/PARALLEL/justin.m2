restart
needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y]
(F, G) = ({x - y}, {x - 2*y})
allowableThreads = 8
(N, m) = (2^3, 10)

elapsedTime sols = flatten apply(m, j -> flatten(
     solList = pack(apply(N, i -> (a = random CC; point{{a, a}})), ceiling(N/allowableThreads));
     threadList = apply(solList, paths -> schedule(track, (F, G, paths)));
     while not all(threadList, isReady) do sleep 1;
     threadList/taskResult));
 
