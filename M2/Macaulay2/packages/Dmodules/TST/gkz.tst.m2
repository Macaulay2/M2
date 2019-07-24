-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

Dtrace 1
pInfo(1, "testing gkz...")

------------------------------------
-- A = {{1,1,1},{0,1,2}}; custom W
------------------------------------
x = symbol x;
W = makeWeylAlgebra(QQ[x_0..x_2]);
A = matrix{{1,1,1},{0,1,2}};

for b in {{1,2}, {-1,3}, {1/2, 1/3}} do (
    gkzIdl = 
    gkz(A,b,W);
    correctGkzIdl = ideal( dx_1^2 - dx_0*dx_2,
			   x_0*dx_0 + x_1*dx_1 + x_2*dx_2 - b_0,
			   x_1*dx_1 + 2*x_2*dx_2 - b_1 );
    assert(gkzIdl == correctGkzIdl);
    );


---------------------------------------
-- A = {{1,1,1},{0,1,2}}; no W; global
---------------------------------------
A = matrix{{1,1,1},{0,1,2}};
u = symbol u;
x = symbol x;
D = symbol D;
u_1 = x_1 = "test1";
u_2 = x_2 = "test2";
u_3 = x_3 = "test3";
Du_1 = D_1 = "test4";
Du_2 = D_2 = "test5";
Du_3 = D_3 = "test6";
for b in {{1,2}, {-1,3}, {1/2, 1/3}} do (
    gkzIdl = gkz(A, b);
    
    -- -- All of x_1, x_2, x_3, D_1, D_2, D_3 should belong to the ring of gkzIdl
    -- assert( all(1..3, i -> ring x_i === ring gkzIdl) );
    -- assert( all(1..3, i -> ring D_i === ring gkzIdl) );
    
    assert(u_1  == "test1");
    assert(u_2 == "test2");
    assert(u_3 == "test3");
    assert(Du_1 == "test4");
    assert(Du_2 == "test5");
    assert(Du_3 == "test6");
    assert(x_1  == "test1");
    assert(x_2 == "test2");
    assert(x_3 == "test3");
    assert(D_1 == "test4");
    assert(D_2 == "test5");
    assert(D_3 == "test6");
    
    W' = ring gkzIdl;
    correctGkzIdl = ideal( W'_4^2 - W'_3*W'_5,
			   W'_0*W'_3 + W'_1*W'_4 + W'_2*W'_5 - b_0,
			   W'_1*W'_4 + 2*W'_2*W'_5 - b_1);
    assert(gkzIdl == correctGkzIdl);
    );

---------------------------------------
-- A = {{1,1,1},{0,1,2}}; no W; local
---------------------------------------
A = matrix{{1,1,1},{0,1,2}};
--dummyNames = apply(1..6, i -> "test" | i);
u = symbol u;
x = symbol x;
D = symbol D;
u_1 = x_1 = "test1";
u_2 = x_2 = "test2";
u_3 = x_3 = "test3";
Du_1 = D_1 = "test4";
Du_2 = D_2 = "test5";
Du_3 = D_3 = "test6";

for b in {{1,2}, {-1,3}, {1/2, 1/3}} do (
    gkzIdl = gkz(A, b, Vars=>Local);
    
    -- Make sure the local stuff worked
    assert(u_1  == "test1");
    assert(u_2 == "test2");
    assert(u_3 == "test3");
    assert(Du_1 == "test4");
    assert(Du_2 == "test5");
    assert(Du_3 == "test6");
    assert(x_1  == "test1");
    assert(x_2 == "test2");
    assert(x_3 == "test3");
    assert(D_1 == "test4");
    assert(D_2 == "test5");
    assert(D_3 == "test6");
    
    W' = ring gkzIdl;
    correctGkzIdl = ideal( W'_4^2 - W'_3*W'_5,
			   W'_0*W'_3 + W'_1*W'_4 + W'_2*W'_5 - b_0,
			   W'_1*W'_4 + 2*W'_2*W'_5 - b_1);
    assert(gkzIdl == correctGkzIdl);
    );

---------------------------------------
-- A = {{1,1},{0,1}}; W;
---------------------------------------
W = makeWeylAlgebra(QQ[x,y]);
A = matrix{{1,1},{0,1}};
b = {0,0};
gkzIdl = gkz(A, b, W);
correctGkzIdl = ideal(x*dx + y*dy, y*dy);
assert(gkzIdl == correctGkzIdl);