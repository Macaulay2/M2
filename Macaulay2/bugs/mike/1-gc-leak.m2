-- This problem seems to be a GC problem

kk = ZZ/5
R = kk[vars(0..10-1)];
B = flatten entries basis(2,R);
J = ideal (h*i-2*d*j,g*i+2*i^2,d*g+2*h*j,f^2+2*a*i,b*f-2*g^2,c^2+h*j,b*c+2*c*f)
--betti res(J = randomSparseIdeal(B,2,7))
H = () -> (
     I := ideal flatten entries gens J;
     C := res I;
     )

H1 = () -> (
     I := ideal flatten entries gens J;
     C := res I;
     collectGarbage();
     )

run "ulimit -v"

trial = 1

if trial == 1 then (

    time for i from 1 to 1000 do H();
    collectGarbage()
    -- at  590.22 MB, #slabs = 66781 (debug version) -- 105.52 sec
    -- at  626.66 MB, #slabs = ?? (optimized version) -- 51.17  sec
    -- on octopus, optimized: 849 MB, 51.76 sec

     ) else (

    -- start over --
    time for i from 1 to 1000 do H1();
    collectGarbage()
    -- at 124.72 MB, #slabs = 1324 (debug version) -- 211.79 sec
    -- at 125.41 MB, #slabs = ?? (debug version) -- 121.61 sec
    -- on octopus: killed it after 450 seconds, it was using over 600 MB

     )

run ("ps u " | toString processID())
