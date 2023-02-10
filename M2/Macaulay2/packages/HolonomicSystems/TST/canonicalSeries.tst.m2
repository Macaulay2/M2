Dtrace 1
pInfo(1, "testing canonicalSeries...")

------------------------------------
-- 
------------------------------------
W = makeWA(QQ[x_1,x_2])
for b in {0,1} do(
    F1 = ideal(x_1*dx_1*(x_1*dx_1+b), x_1*dx_1*(x_2*dx_2+b),  
	x_2*dx_2*(x_1*dx_1+b), x_2*dx_2*(x_2*dx_2+b));
    assert(isTorusFixed F1 == true)
    )
for b in {0,1} do(
    F2 = ideal(x_1*dx_1*x_2*dx_2-1, x_1*dx_1-b*x_2*dx_2-b+1)
    assert(isTorusFixed F2 == true)
    )


----
W = makeWeylAlgebra(QQ[x,y])
vars W
thetax = x*dx
thetay = y*dy
P1 = thetax^2*(thetax-2)-x*(thetax+thetay+1)*(thetax+2)*(thetax+3)
P2 = thetay^2*(thetay-3)-y*(thetax+thetay+1)*(thetay+2)*(thetay+3)
I = ideal(P1,P2)
w = {1,11}
inw(I,flatten{-w|w})

S = QQ[t_1,t_2]
distraction(I,S)
cssExptsMult(I,w)
--{{4, {0, 0}}, {2, {2, 0}}, {2, {0, 3}}, {1, {2, 3}}}
--matches SST Ex 2.5.13

-----

A = matrix{{1,1,1,1,1},{1,1,0,-1,0},{0,1,1,-1,0}}
beta = {1,0,0}
I = gkz(A,beta)
w = {1,1,1,1,0}
S = QQ[t_1..t_5]
isTorusFixed I --false
J = inw(I,flatten{-w|w}) 
isTorusFixed J --true
distraction(J,S) == ideal(t_1 +t_2 +t_3+t_4 +t_5 -1, t_1 +t_2 -t_4, t_2 +t_3 -t_4, t_1*t_3, t_2*t_4)
cssExptsMult(I,w) --{{4, {0, 0, 0, 0, 1}}}
--matches Ex 2.6.4





path = prepend("~/Desktop/Workshop-2019-Minneapolis/M2/Macaulay2/packages/", path)
installPackage "Dmodules"
