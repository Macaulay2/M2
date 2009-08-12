-- This is an example that Bernd Sturmfels asked about
-- and it would be nice to improve its performance

end
d = 5;
c = 10-d;

R = ZZ/101[x11,x12,x13,x14,x22,x23,x24,x33,x34,x44];
A = random(ZZ^c,ZZ^10)
B = gens kernel A

Xvec = matrix {{x11,x12,x13,x14,x22,x23,x24,x33,x34,x44}};

X = matrix{ {x11,x12,x13,x14},
           {x12,x22,x23,x24},
           {x13,x23,x33,x34},
           {x14,x24,x34,x44}};

Xad = matrix {
{ det(submatrix(X,{1,2,3},{1,2,3}))},
{-det(submatrix(X,{1,2,3},{0,2,3}))},
{det(submatrix(X,{1,2,3},{0,1,3}))},
{-det(submatrix(X,{1,2,3},{0,1,2}))},
{det(submatrix(X,{0,2,3},{0,2,3}))},
{-det(submatrix(X,{0,2,3},{0,1,3}))},
{det(submatrix(X,{0,2,3},{0,1,2}))},
{det(submatrix(X,{0,1,3},{0,1,3}))},
{-det(submatrix(X,{0,1,3},{0,1,2}))},
{det(submatrix(X,{0,1,2},{0,1,2}))}};

Model1  = ideal(A * Xad) ;

gbTrace=3
time gens gb(Model1, Algorithm=>LinearAlgebra);


time Model = Model1:minors(3,X);

MLE = Model +  ideal(Xvec * B  + random(ZZ^1,ZZ^d));
betti mingens Model
d,{dim Model, degree Model}, {dim MLE, degree MLE}
