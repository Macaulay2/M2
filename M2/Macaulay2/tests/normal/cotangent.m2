S = ZZ/32003[u,v,w,x,y,z];
I = minors(2,genericSymmetricMatrix(S,u,3));
X = variety I;
R = ring X;
Omega = cotangentSheaf X;
OmegaDual = dual Omega;
assert ( module Omega == cokernel map(R^{3:-1}, , {{z, y, w}, {w, v, u}, {-y, -x, -v}} ))
assert ( module OmegaDual == image map(R^{3:1}, , {{y, x, w, 0, v, 0, u, 0}, {0, 0, -z, y, -y, x, -w, v}, {z, y, 0, w, 0, v, 0, u}}) )

-- -- make sure it works in weighted projective space
-- K3 = Proj(QQ[x,y,z,s,Degrees=>{4,6,1,1}]/(y^2 - x^3 - z^8*x + z^12 + s^12))
-- assert ( rank cotangentSheaf K3 === 2 )
