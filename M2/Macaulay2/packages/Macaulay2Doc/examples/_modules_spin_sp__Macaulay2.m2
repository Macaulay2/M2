R = QQ[a..f];
m = matrix{{a,b,d,e},{b,c,e,f}}
M = coker m
N = image m
K = kernel m
presentation M -- this is just the original matrix
presentation N -- this one requires computation
ideal(a,b)*N
a*N + b*N
N0 = image (a**N_{1}|N_{2}-N_{3})
N_{1}
a ** N_{1}
a ** N_{1} | N_{2}-N_{3}
N0 = image(a ** N_{1} | N_{2}-N_{3})
isHomogeneous N0
Nbar = N/N0
I = ideal(a^2, a*b, c^2)
J = module I
I == ideal J
codim I
codim J
C = res I
C.dd
betti C
C = res Nbar
betti C
C.dd
R = QQ[a..h];
J = ideal(a*c+b*d,a*e+b*f,a*g+b*h)
betti res J
use ring M
M
N = a*M
M/N
generators N
relations N
presentation N
trim N
minimalPresentation N
prune N
ambient N
ambient N == target generators N
ambient N == target relations N
super N
super N == coker relations N
cover N
cover N == source generators N
A = QQ[x,y]/(y^2-x^3)
M = module ideal(x,y)
F = map(A^1,M,matrix{{y,x^2}})
source F == M
target F == A^1
matrix F
inducedMap(A^1,M)
G = F // inducedMap(A^1,M)
source G
target G
isWellDefined G
R = QQ[x,y,z,w]
M = ideal(x,y,z)/ideal(x^2,y^2,z*w)
N = z*M
M/N
M
ambient M
N = z*M
ambient(M/N)
super M
super N
image generators M
inducedMap(M,M) == id_M
inducedMap(super M,M) == map(super id_M) -- the map (P+Q)/Q --> R^n/Q, where M=(P+Q)/Q.
inducedMap(super M,ambient M) -- the quotient map R^n --> R^n/Q
inducedMap(M,N) -- the inclusion map
inducedMap(M/N,M) -- the projection map
inducedMap(M/N,N) -- the zero map
inducedMap(M,M/N,Verify => false)
inducedMap(M/N,x*M)
inducedMap(M/N,M) * inducedMap(M,x*M) == inducedMap(M/N,x*M)
A = QQ[x,y,Degrees=>{2,3}]/(y^2-x^3)
M = module ideal(x,y)
H = Hom(M,M)
F = homomorphism(H_{0})
G = homomorphism(H_{1})
source F == M
target F == M
ker F
coker F
m = matrix{{x,y},{y,x}}
Hom(m,A^2)
Hom(A^2,m)
m ** m
(coker m) ** (coker m)
M = coker m
M2 = prune(M ** M)
A = QQ[a,b,c]
A ** A
B = oo
a == B_3
a == B_0
tensor(A,A,Variables=>{a,b,c,d,e,f})
