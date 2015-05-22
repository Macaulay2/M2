S = ZZ/32003[u,v,w,x,y,z];
I = minors(2,genericSymmetricMatrix(S,u,3));
X = variety I;
R = ring X;
Omega = cotangentSheaf X;
OmegaDual = dual Omega;
assert ( module Omega == cokernel map((R)^{{-1},{-1},{-1}},(R)^{{-2},{-2},{-2}},{{w, v, u}, {-y, -x, -v}, {z, y, w}}))
assert ( module OmegaDual == image map((R)^{{1},{1},{1}},(R)^8,{{y, x, v, 0, 0, -z, -y, -w}, {w, v, u, z, y, 0, 0, 0}, {0, 0, 0, y, x, w, v, u}}) )
