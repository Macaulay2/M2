A = matrix{{2^10000}}
f = () -> (
    B := A*A;
    a := B_(0,0);
    )
g = () -> (
    count := 0;	   
    while true do (
	count = count + 1;
	if count % 10000 === 0 then collectGarbage();
	f();	
	)    
    )
end

restart
load "../../../bugs/anton/reallocate-heap-to-GC-heap/ZZ-test.m2"
