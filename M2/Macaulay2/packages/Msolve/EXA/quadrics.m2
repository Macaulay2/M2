restart
needsPackage "Msolve"
FF = ZZ/nextPrime 2^30 -- M2 segfaults
FF = ZZ/nextPrime 2^20 -- works!
FF = QQ
R = FF[a..o,x,y]
F = poly "x2 + axy + by2 + cx + dy + e"
G = poly "x2 + fxy + gy2 + hx + iy + j"
H = poly "x2 + kxy + ly2 + mx + ny + o"
eMsolve = eliminationIdeal({x,y},ideal{F,G,H}); 
numgens eMsolve
size eMsolve_0
