S = ZZ/32003[u,v,w,x,y,z];
I = minors(2,genericSymmetricMatrix(S,u,3));
X = variety I;
R = ring X;
Omega = cotangentSheaf X;
OmegaDual = dual Omega;
assert ( module Omega == cokernel map(R^{3:-1}, , {{z, y, w}, {w, v, u}, {-y, -x, -v}} ))
assert ( module OmegaDual == image map(R^{3:1}, , {{y, x, w, 0, v, 0, u, 0}, {0, 0, -z, y, -y, x, -w, v}, {z, y, 0, w, 0, v, 0, u}}) )
