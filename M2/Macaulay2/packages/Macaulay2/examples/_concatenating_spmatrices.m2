R = ZZ/32003[a..f];
M = genericMatrix(R,a,3,2)
N = matrix{{d^2,a*d},{b*c,b*d},{a,c}}
M|N
P = matrix{{d^2,a*d,e*f},{b*c,b*d,b*e},{a,c,d}}
transpose(M)||P
matrix{{id_(R^3),M,P},{random(R^1,R^3),random(R^1,R^3),random(R^1,R^2)}}
