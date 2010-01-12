-- given an Ideal I find a point P on V(I),
-- find arcs starting from P on V(I)
-- interpolate hypersurfaces vanishing on all arcs

restart
K = ZZ/7

-- work in IP^4
n=4
R = K[x_0..x_n]

-- precision
maxe = 7
E = apply(maxe,i->K[e]/ideal(e^(i+1)))

-- make an Ideal
F = random(2,R);
G = random(3,R);
H = random(2,R)

betti (I=ideal (F*G,G*H))
-- time decompose I;
-- takes a long time

-- find Points
L = for i from 1 to 20 list (
     r = random(K^1,K^(n+1));
     if sub(I,r) == 0 then r else continue
     )


-- random kernel element
-- M: a matrix over K
-- returns a random element s with M*s == 0
rker = (M) -> (
     sM = syz M;
     sM*random(source sM,K^1)
     )

-- returns the last kernel Element (the one presumably involving the bottom row)
lastker = (M) -> (
     sM = syz M;
     r = rank source sM;
     sM_{r-1} + sM_{0..r-2}*random(K^(r-1),K^1)
     )


oneStep = (mI,JP,k,P) -> (
     erI := sub(mI,sub(P,E#(k+1)));
     erIK := sub(erI,matrix{{1_K}});
     corr0 := transpose lastker transpose (JP||erIK);
     if corr0_{rank source corr0-1} != matrix{{1_K}} then error"Variety is singular in P";
     corr = corr0_{0..n};
     Pplus = sub(P,E#(k+1))+sub(e^(k+1),E#(k+1))*sub(corr,E#(k+1));
     if 0 != sub(I,Pplus) then error "Not zero";
     (mI,JP,k+1,Pplus)
     )

maxStep = (I,P) -> (
     J := jacobian I;
     JP := sub(J,P);
     betti (mI := gens I);
     -- first Lift
     P1 := sub(P,E#1)+sub(e,E#1)*sub(transpose rker(transpose JP),E#1);
     now := (mI,JP,1,P1);
     for i from 2 to maxe-1 do now = oneStep(now);
     now#3)     

-- degree of interpolated polynomials
deg = 3
mons = basis(deg,R)
-- this calculation of needed arcs is only OK, if 
-- precision is large enough. Have to think about the relation
numArcs = (rank source mons//(maxe-1)) + 2

-- choose a point on the variety
PL = L#1

-- make a number of random arcs
-- collect their coefficients in a Matrix
time M = fold((a,b)->(a||b),apply(numArcs, i->(
	       arc = maxStep(I,PL);
	       sub((coefficients sub(mons,arc))#1,K)
	       )));

-- calculate the ideal of cubics vanishing on all arcs
time betti (I1 = ideal mingens ideal(mons*syz M))

-- remove component from Ideal
betti(I : I1)
