restart
load (currentFileDirectory|"../code/solveViaMonodromy.m2")
needsPackage "Permanents"

FF = CC

-- S: number of players
-- n_i: number of strategies for player i
-- N = n_1 + ... + n_S - S

-- we assume that each player (S players total) has the same number of strategies 
-- compute BKK bound for NxN system
bkkBound = method()
bkkBound (ZZ, ZZ) := (S, n) -> (
    N := S*(n-1);
    -- A is an NxN matrix of 1's and 0's
    A := matrix flatten toList(apply(0..S-1,i -> toList (
		if i == 0 then (
		    for j from 1 to n-1 list join(toList(n-1: 0_CC), toList(N-((n-1)*(i+1)): 1_CC))
		    ) 
		else if  i == S-1 then (
		    for j from 1 to n-1 list join(toList((n-1)*i: 1_CC), toList(n-1: 0_CC))
		    ) 
		else (
		    for j from 1 to n-1 list 
		    join(toList((n-1)*i: 1_CC), toList(n-1: 0_CC), toList(N-(n-1)*(i+1): 1_CC)))
		))
	);
    permA := glynn A;
    Bound := permA/((n-1)!^S);
    Bound
    )

TEST ///
bkkBound(3,3)
bkkBound(4,2)
bkkBound(5,3)
///

-- For S players, each with n strategies, we construct n-1 equations per player
-- We use p_(1,n) = 1-p_(1,1)-...-p_(1,n-1)
-- Polynomial system of N equations with N unknowns

-- 3 players 3 strategies each

S = 3
n = 3
N = S*(n-1)

R = FF[a_(1,1,1,1)..a_(S,n,n,n)][p_(1,1)..p_(S,n-1)]

P = symbol P

p_(1,3) = 1 - p_(1,1) - p_(1,2)
p_(2,3) = 1 - p_(2,1) - p_(2,2)
p_(3,3) = 1 - p_(3,1) - p_(3,2)

P11 = for j from 1 to n list (
    	for i from 1 to n list (a_(1,1,j,i)*p_(2,j)*p_(3,i)))
P_(1,1) = sum flatten P11   

P12 = for j from 1 to n list (
    	for i from 1 to n list (a_(1,2,j,i)*p_(2,j)*p_(3,i)))
P_(1,2) = sum flatten P12 

P13 = for j from 1 to n list (
    	for i from 1 to n list (a_(1,3,j,i)*p_(2,j)*p_(3,i)))
P_(1,3) = sum flatten P13	    

P21 = for j from 1 to n list (
    	for i from 1 to n list (a_(2,1,j,i)*p_(1,j)*p_(3,i)))
P_(2,1) = sum flatten P21   

P22 = for j from 1 to n list (
    	for i from 1 to n list (a_(2,2,j,i)*p_(1,j)*p_(3,i)))
P_(2,2) = sum flatten P22

P23 = for j from 1 to n list (
    	for i from 1 to n list (a_(2,3,j,i)*p_(1,j)*p_(3,i)))
P_(2,3) = sum flatten P23 	    

P31 = for j from 1 to n list (
    	for i from 1 to n list (a_(3,1,j,i)*p_(1,j)*p_(2,i)))
P_(3,1) = sum flatten P31   

P32 = for j from 1 to n list (
    	for i from 1 to n list (a_(3,2,j,i)*p_(1,j)*p_(2,i)))
P_(3,2) = sum flatten P32 

P33 = for j from 1 to n list (
    	for i from 1 to n list (a_(3,3,j,i)*p_(1,j)*p_(2,i)))
P_(3,3) = sum flatten P33 	    

-- 6 equations with 6 unknowns to set up polySystem
P_1 = P_(1,1) - P_(1,2)
P_2 = P_(1,1) - P_(1,3)
P_3 = P_(2,1) - P_(2,2)
P_4 = P_(2,1) - P_(2,3)
P_5 = P_(3,1) - P_(3,2)
P_6 = P_(3,1) - P_(3,3)


Q = matrix{{P_1}, {P_2}, {P_3}, {P_4}, {P_5}, {P_6}}
G = polySystem Q
peek G
L = apply(toList(1..numgens R), i -> random(0.,1.))
SubList := apply(toList(0..numgens R-1), i -> (gens R)#i => L#i)
C = coefficientRing ring G
M = sub(sub(G.PolyMap, SubList), C)
N = numericalIrreducibleDecomposition ideal M
c0 = first (first components N).Points
pre0 = point{apply(SubList, i -> i#1)}
(c0,pre0)

elapsedTime sols = twoNodes(transpose G.PolyMap,c0,{pre0},5)   
J = first sols
E = J.Edges
for e in E do (
    print peek e.Correspondence12;
    print peek e.Correspondence21;
    )
V = J.Vertices
for v in V do (
    print peek v.PartialSols;
    )




--ignore below

pol1 = "- 1.122525605*p3 - 0.1202590019*p3*p2 + 1.078803537*p2" \
         + " + 0.7696590455*p4 + 0.1628390188*p4*p3*p2 - 0.1163105815*p4*p3" \
         + " - 0.5333928492*p4*p2 - 0.6823118878;"
    pol2 = "- 0.4266067331*p3 + 0.9756757996*p4 + 0.646954510E-01*p4*p3" \
         + "- 0.2142068753*p3*p1 + 2.814522341*p1 + 0.3980853635*p4*p3*p1" \
         + "- 2.546266040*p4*p1 - 1.157918209;"
    pol3 = "2.023260100*p2 - 0.1342402078*p4 - 0.3904810713*p4*p2" \
         + " + 0.4274147938*p1 + 0.5269506245*p4*p1 - 3.069473137*p2*p1" \
         + " + 0.3284270487*p4*p2*p1 - 0.9128973443;"
    pol4 = "- 0.6256820158*p3 - 0.1614530476*p3*p2 + 1.080721123*p2" \
         + " + 1.366746822*p3*p1 - 2.347725424*p1 + 0.5941402017*p2*p1" \
         + " - 2.233884240*p3*p2*p1 + 1.026687422;"


