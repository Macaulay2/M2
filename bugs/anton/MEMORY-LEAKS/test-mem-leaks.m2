getRSS = () -> (
    s := get ("!ps -o rss -p" | processID());
    value last lines s    
    )

testF = (n,f) -> (
    setRandomSeed "a";
    collectGarbage();
    before := getRSS();
    t := first elapsedTiming for i to n do (
    	f();
    	); 
    collectGarbage();
    after := getRSS();
    (leakPer,timePer) := ((after-before)*1024.0/n, t*1000.0/n);
    << "-- elapsed time = " << t << endl; 
    << "-- leaks " << leakPer << " bytes, takes " << timePer << " ms. (per call)" << endl; 
    (leakPer,timePer)
    )
