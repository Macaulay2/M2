restart
gbTrace=3
debug Core
R = QQFlint
R = QQ
R = ZZFlint
m = mutableMatrix(R, 100, 100)
a = 2^100
for i from 0 to 99 do for j from 0 to 99 do m_(i,j) = a + random 10000;
collectGarbage()
for i from 1 to 2000 do (n := transpose m; collectGarbage());
for i from 1 to 2000 do (n := transpose m;);

for i from 1 to 100 do (m := mutableMatrix(R, 100, 100); fillMatrix m; m*m;)
