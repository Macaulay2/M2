-- From bug report from Brian Pike, 3 Nov 2012 (google group)
R=QQ[x,y,z,MonomialOrder=>RevLex,Global=>false]
A=map(R^3,R^{{-1},{-2},{-3},{-3}},{{2*x, x^2, 4*x*y, 4*y^2+16*x*z}, {3*y, 0, 4*y^2-x^3, 16*y*z-3*x^2*y}, {4*z, 3*y^2-4*x*z, -2*x^2*y, -6*x*y^2}})
N=map(R^3,R^{{0},{-1},{-2},{-2}},{{0, 0, -4*x, -8*y}, {-3, 0, -8*y, -16*z+3*x^2}, {0, -6*y, 2*x^2, 12*x*y}})
NA=gens intersect(image(N),image(A));
                
(mat1,mat2) = quotientRemainder(NA,N)
assert(mat2 == 0)
assert(N * mat1 == NA)
end

-- the following was used to debug this, as the error was in rawGBMatrixLift         
f = NA
g = N         
G = gb(g, ChangeMatrix => true)
debug Core
gbTrace=4
rawGBMatrixLift(raw G, raw f);

