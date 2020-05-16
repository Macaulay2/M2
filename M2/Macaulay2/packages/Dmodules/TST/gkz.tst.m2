-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

Dtrace 1
pInfo(1, "testing gkz...")


Dtrace 1
pInfo(1, "testing gkz...")

------------------------------------
-- A = {{1,1,1},{0,1,2}}; custom W
------------------------------------
x = symbol x;
dx = symbol dx;
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
-- A = {{1,1,1},{0,1,2}}; no W
---------------------------------------
A = matrix{{1,1,1},{0,1,2}};
x = symbol x;
D = symbol D;
x_1 = "test1";
x_2 = "test2";
x_3 = "test3";
D_1 = "test4";
D_2 = "test5";
D_3 = "test6";
for b in {{1,2}, {-1,3}, {1/2, 1/3}} do (
    gkzIdl = gkz(A, b);
    
    -- Confirm that gkz didn't change x_i's or D_i's
    assert(x_1 == "test1");
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
x = symbol x;
y = symbol y;
dx = symbol dx;
dy = symbol dy;
W = makeWeylAlgebra(QQ[x,y]);
A = matrix{{1,1},{0,1}};
b = {0,0};
gkzIdl = gkz(A, b, W);
correctGkzIdl = ideal(x*dx + y*dy, y*dy);
assert(gkzIdl == correctGkzIdl);




pInfo(1, "testing eulerOperators...");

----------------------------
-- A = {{1,1},{0,1}}; no b
----------------------------
x = symbol x;
y = symbol y;
dx = symbol dx;
dy = symbol dy;
W = makeWeylAlgebra(QQ[x,y]);
A = matrix{{1,1},{0,1}};
correctEuls = {x*dx + y*dy, y*dy};
assert(eulerOperators(A, W) == correctEuls);

----------------------------
-- A = {{1,1},{0,1}}; b
----------------------------
x = symbol x;
y = symbol y;
dx = symbol dx;
dy = symbol dy;
W = makeWeylAlgebra(QQ[x,y]);
A = matrix{{1,1},{0,1}};
b = {1,2}
correctEuls = {x*dx + y*dy - 1, y*dy - 2};
assert(eulerOperators(A, b, W) == correctEuls);



pInfo(1, "testing toricIdealPartials...");

----------------------------
-- A = {{1,1,1},{0,1,2}};
----------------------------
x = symbol x;
y = symbol y;
z = symbol z;
dx = symbol dx;
dy = symbol dy;
dz = symbol dz;
W = makeWeylAlgebra(QQ[x,y,z]);
A = matrix{{1,1,1},{0,1,2}};
I = toricIdealPartials(A, W);
R = ring I;
correctI = ideal(R_1^2 - R_0*R_2);
assert(I == correctI);