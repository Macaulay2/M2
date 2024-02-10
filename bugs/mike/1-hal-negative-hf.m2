-- This is now working.  Maybe make it into a slow/ test...
-- However, it shows that computing basis instead of hilbertFunction can be much better...

-- hilbertFunction is producing incorrect values, and is taking a huge amount
-- of time too.  HirzOmegaTable is the original.  HirzOmegaTable2 changes hilbertFunction
-- to numgens source basis, which seems to work here.
-- from email Hal to Mike, 1 May 2008.

HirzOmegaTable = (a,b)->(
    R=ZZ/31991[x_1..x_4, Degrees=>{{1,0},{-2,1},{1,0},{0,1}}, Heft=>{1,3}];
    OM = ker matrix{{x_1,-2*x_2,x_3,0},{0,x_2,0,x_4}};
    k=16*max(abs(a),abs(b));
    A=coker matrix{{(x_3*x_4)^k,(x_1*x_4)^k,(x_1*x_2)^k,(x_2*x_3)^k}};
    print "H^0";
    for i from -a to a do
                   (for j from -b to b do <<" " << hilbertFunction({i,j},OM);
                   print " ");
    E2=Ext^2(A,OM);
    print "H^1";
    for i from -a to a do
                   (for j from -b to b do <<" " << hilbertFunction({i,j},E2);
                    print " ");
    E3=Ext^3(A,OM);
    print "H^2";
    for i from -a to a do
                   (for j from -b to b do <<" " << hilbertFunction({i,j},E3);
                   print " "))
--Print cohomology tables for Omega^1(a,b) on Hirz surf F_2,
--for a grid of size a,b.

HirzOmegaTable2 = (a,b)->(
    R=ZZ/31991[x_1..x_4, Degrees=>{{1,0},{-2,1},{1,0},{0,1}}, Heft=>{1,3}];
    OM = ker matrix{{x_1,-2*x_2,x_3,0},{0,x_2,0,x_4}};
    k=16*max(abs(a),abs(b));
    A=coker matrix{{(x_3*x_4)^k,(x_1*x_4)^k,(x_1*x_2)^k,(x_2*x_3)^k}};
    print "H^0";
    for i from -a to a do
                   (for j from -b to b do <<" " << numgens source basis({i,j},OM);
                   print " ");
    time E2=Ext^2(A,OM);
    print "H^1";
    for i from -a to a do
                   (for j from -b to b do <<" " << numgens source basis({i,j},E2);
                    print " ");
    time E3=Ext^3(A,OM);
    print "H^2";
    for i from -a to a do
                   (for j from -b to b do <<" " << numgens source basis({i,j},E3);
                   print " "))

end
restart
load "1-hal-negative-hf.m2"
-- This one produces negative values for the Hilbert function of H^1 !!
time HirzOmegaTable(2,2)
H^0
0 0 0 0 0
0 0 0 0 4
0 0 0 1 7
0 0 0 2 10
0 0 1 4 14
H^1
0 0 -58 -59 -62

-- This one seems fine, and time wise is sort of OK.
time HirzOmegaTable2(2,2)

{* -- output
H^0
 0 0 0 0 1 
 0 0 0 0 4 
 0 0 0 1 7 
 0 0 0 2 10 
 0 0 1 4 14 
     -- used 0.007822 seconds
H^1
 0 0 3 4 3 
 0 0 2 2 2 
 1 1 2 1 1 
 2 2 2 0 0 
 3 4 3 0 0 
     -- used 0.001345 seconds
H^2
 14 4 1 0 0 
 10 2 0 0 0 
 7 1 0 0 0 
 4 0 0 0 0 
 1 0 0 0 0 
*}
