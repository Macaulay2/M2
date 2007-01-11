-- Yanked from test/engine/raw-localgb.m2
--------------------------
-- singular example #18 --
--------------------------
kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
I = ideal(x-x^2)
gens gb I
x % I

-- This one is a problem
kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = z^4*t^3*y + t^7
H = x^5 + y^4
M = matrix{{F,G,H}}
gbTrace=10
time gens(G = gb M);


kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*z^4*t^3*y + 3*t^7
H = 2*x^5 + 6*z^2*y^2 + 2*y^4
M = matrix{{F,G,H}}
gbTrace=3
time gens gb M;

kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*x^2*z^4*t^3*y + 3*t^7
H = 2*x^5 + 6*z^2*y^2 + 2*y^4
M = matrix{{F,G,H}}
gbTrace=3
time gens gb M;


kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*x^2*z^4*t^3*y + 3*t^7
H = 2*x^8 + 6*z^2*y^2*t + 2*y^5
M = matrix{{F,G,H}}
gbTrace=3
time gens gb M;


restart
kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7
H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5
M = matrix{{F,G,H}}
gbTrace=3
time gens gb(M);
gb1 = gb(M,DegreeLimit=>27);
gbs = reverse flatten entries gens gb1
-- spair [6,1] is 
gbs#1
gbs#4
sp = y*gbs#4-z^15*gbs#1 -- this one should reduce to 0, but the following line doesn't seem to finish..
sp % gb1

time gens gb(M,BasisElementLimit =>20);
leadTerm gbSnapshot M

F1 = F
F2 = 1/3_kk * G
F3 = 1/2_kk * (H - 6*t*y*F1)

F1
F2
F3

s1 = y^5*F1-t*z*F3 - 12*t^3*y*z * F1
 + 3*t*y^2*z^2 * F1 - 9*t*y*z^3*F1 - 4*t*y^5*F1

kk = ZZ/101
R = kk[h,t,x,y,z,MonomialOrder=>Eliminate 1]
F = homogenize(4*t^2*z+6*z^3*t+3*z^3+t*z,h)
G = homogenize(5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7,h)
H = homogenize(6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5,h)

F1 = F
F2 = 1/3_kk * G
F3 = 1/2_kk * (H - 6*t*y*
     
L = new MutableHashTable from {" 1 " => F1, " 2 " => F2}
see L
see = method()
see HashTable := see MutableHashTable := (H) -> netList apply(pairs H, toList)

addto = (L, F) -> (i := 1+#values L; L#(" "|i|" ") = F; L)

L = new MutableHashTable 
see addto(L, F1)
see addto(L,F2)
F3 = 1/2_kk*(H - 6*h^2*t*y*F1)
see addto(L,F3)     

-- spair(F1,F3) is it automatic that this will reduce to zero?
F4' = t*z*F3-h*y^5*F1 + 12*h*t^3*y*z*F1 - 3*h*t*y^2*z^2*F1 + 9*h*t*y*z^3*F1 + 4*t*y^5*F1 - 48*t^4*y*z*F1 + 12*t^2*y^2*z^2*F1
F4' = -1/3_kk * F4'
  -- at this point, it needs to be deferred:
F4'' = -1/34_kk * (h*F4' - z^3*F3 + 6*h*t^2*y*z^3*F1 + 39*t^2*y^5*F1 + 37*t^5*y*z*F1  + 16*t^3*y^2*z^2*F1 + 2*t*F4' - 12*t^3*y*z^3*F1 )
h*F4' - x^8*F1 - h*F4' + x^8*F1 == 0
-- spair(F1,F2)
p1 = z*F2-h^4*t^6*F1 + 4*h^3*t^7*F1 + 3*h^3*t^5*z^2*F1 - 16*h^2*t^8*F1 - 18*h^2*t^6*z^2*F1 -9*h^2*t^4*z^4*F1 - 37*h*t^9*F1 - 5*h*t^7*z^2*F1 - 29*h*t^5*z^4*F1 + 32*h*t^2*x^2*y*z^4*F1 + 27*h*t^3*z^6*F1 + 47*t^10*F1
p1 = -1/14_kk *( p1 + 25*t^8*z^2*F1 +37*t^6*z^4*F1 - 27*t^3*x^2*y*z^4*F1 + 33*t^4*z^6*F1 + 5*t*x^2*y*z^6*F1 + 20*t^2*z^8*F1 )
p2 = h*p1-t^11*F1 + 23*t^9*z^2*F1 + 21*t^7*z^4*F1 + 50*t^4*x^2*y*z^4*F1 + 41*t^5*z^6*F1 + 31*t^2*x^2*y*z^6*F1 + 46*t^3*z^8*F1 - 35*x^2*y*z^8*F1 - 39*t*z^10*F1 + 4*t*p1
p2 = 1/49_kk * p2

- 21*h*t^4*z^4*F1 + 21*t^7*z^4*F1 + 50*t^4*x^2*y*z^4*F1 + 41*t^5*z^6*F1 + 31*t^2*x^2*y*z^6*F1 + 46*t^3*z^8*F1 - 35*x^2*y*z^8*F1 - 39*t*z^11*F1 + 39*t*z^11*F1

-- homogeneous version
R = ZZ/101[h,t,x,y,z]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7
H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5
M = matrix{{F,G,H}}
M = homogenize(M,h)
hf = poincare coker M
R = ZZ/101[h,t,x,y,z,MonomialOrder=>Eliminate 1]
M = substitute(M,R)
installHilbertFunction(M,hf)
gbTrace=3
time gens gb M;
mingens ideal substitute(leadTerm gens gb M, h=>1)
transpose gens gb M

-- in singular.  This is very fast.
ring R = 101, (t,x,y,z),ds;
poly F = 4*t^2*z+6*z^3*t+3*z^3+t*z;
poly G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7;
poly H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5;
ideal I = F,G,H;
timer=1;
std(I);

--------------------------------------------------
A = QQ[x,y]
I = ideal"x10+x9y2,y8-x2y7"
gens gb I

A = QQ[x,y,MonomialOrder=>Weights=>2:-1,Global=>false]
I = ideal"x10+x9y2,y8-x2y7"
gens gb I
