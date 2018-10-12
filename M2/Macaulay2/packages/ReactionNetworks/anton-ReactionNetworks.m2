needsPackage "NumericalAlgebraicGeometry"
needsPackage "Graphs"

-- define type

ReactionNetwork = new Type of Digraph

-- constructor: should probably have several input methods



termInp = (a,inp,out) -> if member(a,inp/last) then (     
    p := position(inp/last,x->x==a);
    - first inp#p * product(inp,b->cc_(last b)^(first b)) 
    ) else 0
termOut = (a,inp,out) -> if member(a,out/last) then (     
    p := position(out/last,x->x==a);
    first out#p * product(inp,b->cc_(last b)^(first b)) 
    ) else 0

peek Digraph

ancestors Digraph

R = QQ[k_1..k_6,a..d]
G = digraph ({x,y,z}, matrix(R,{{0,1,1},{1,0,1},{1,1,0}}))
adjacencyMatrix G

reactionNetwork = (K,C,R) -> (    
    -- K is the parameter ring
    -- C is a list of pairs (species, ?)
    -- R is a list of reaction equations, formatted ({(specie, coefficient), ... } => {(specie, coefficient), ...}, fwdrate, bckwd rate)
    --c := local c;
    RING := K[apply(C,i->cc_(first i))];
    F := for i in C list (
	(a,af) := i;
	(if af == infinity then 0 else af - cc_a) + sum(R,reaction->(
		(inp'out,k1,k2) := reaction;
		r1 = first inp'out;
		r2 = last inp'out;
		k1 * (termInp(a,r1,r2) + termOut(a,r1,r2)) +
		k2 * (termInp(a,r2,r1) + termOut(a,r2,r1))
		))  
	)
    ) 
end
restart
load "chemical-networks.m2"
F = reactionNetwork (QQ[a,b,k1,k2], {(A,a),(B,b)}, {
	({(2,A),(1,B)} => {(3,A)},k1,k2)
	})
F = reactionNetwork (QQ[a,b,k1,k2], {(A,a),(B,b)}, {
	({(1,A),(2,B)} => {(3,A)},k1,k2)
	})

-- same sign
F = reactionNetwork (QQ[a,b,c,k1,k2,k3], {(A,a),(B,b),(C,c)}, {
	({(1,A),(1,B)} => {(1,C)},k1,k2),
	({(1,A)} => {(2,B)},k1,0)
	})
F = reactionNetwork (CC[a,b,c,d,e,k1,k2,k3,k1',k2',k3'], {(A,a),(B,b),(C,c),(D,d),(E,e)}, {
	({(1,A),(1,B)} => {(1,D)},k1,k1'),
	({(1,B),(1,C)} => {(1,E)},k2,k2'),
	({(1,C)} => {(1,A)},k3,k3')
	})

det jacobian matrix {F}

setRandomSeed 0
for x to 100000 do
(
-*
F = reactionNetwork (CC_53, {(A,random RR),(B,random RR),(C,random RR),(D,random RR),(E,random RR)}, {
	({(1,A),(1,B)} => {(1,D)},random RR,random RR),
	({(1,B),(1,C)} => {(1,E)},random RR,random RR),
	({(1,C)} => {(2,A)},random RR,random RR)
	});
*-
F = reactionNetwork (CC_53, {(A,1),(B,1)}, {
	({(1,A),(3,B)} => {(3,A)},random RR,random RR)
	});
s = select(realPoints solveSystem F, p->all(coordinates p, x->x>0));
print( x=>s );
if #s > 1 then error "found more than one!"
)

-- Specialize concentrations ----------------------------------------
restart
load "chemical-networks.m2"
leftA = 2
leftB = 1
rightA = 3
F = reactionNetwork (QQ[a,b,k1,k2], {(A,a),(B,b)}, {
	({(leftA,A),(leftB,B)} => {(rightA,A)},k1,k2)
	})
M = matrix {F}
M1 = sub(M, apply(gens ring M, v->v=>2))
eliminate({k1,k2},ideal M1)

F = reactionNetwork (QQ[k1,k2], {(A,1),(B,1)}, {
	({(leftA,A),(leftB,B)} => {(rightA,A)},k1,k2)
	})
M = matrix {F}
M = (last flattenRing ring M) M
RM = ring M

-- critical point for the distance function (from a random point cc)
setRandomSeed 0
n = 2
cc's = take(gens RM, n)
S = jacobian M | transpose (
    matrix{apply(cc's, cc->cc-1-(1/10)*random QQ)}
    | 
    map(RM^1,RM^(numgens RM-n),0)
    )
minorsS = minors(n+1,S)
I = minorsS + ideal M
dim I
J = saturate(I,product cc's)
decompose J

-- specialize k, not cc
k's = drop(gens RM,n)
S = jacobian M | transpose (
    map(RM^1,RM^(n),0)
    |
    matrix{apply(k's, k->k-1-(1/10)*random QQ)}
    )
minorsS = minors(n+1,S)
I = minorsS + ideal M
dim I
J = saturate(I,product cc's)
decompose J
sols = solveSystem polySystem J
realPoints sols
--------------------------
restart
load "chemical-networks.m2"
F = reactionNetwork (QQ[k1,k2,k3,k4], {(A,1),(B,1),(C,1)}, {
	({(1,A),(1,B)} => {(1,C)},k1,k2),
	({(1,A)} => {(2,B)},k3,k4)
	})
M = matrix {F}
M = (last flattenRing ring M) M
RM = ring M

-- critical point for the distance function (from a random point cc)
n = 3
cc's = take(gens RM, n)
S = jacobian M | transpose (
    matrix{apply(cc's, cc->cc-1-(1/10)*random QQ)}
    | 
    map(RM^1,RM^(numgens RM-n),0)
    )
minorsS = minors(n+1,S)
I = minorsS + ideal M
dim I
J = saturate(I,product cc's)
decompose J

-- specialize k, not cc
k's = drop(gens RM,n)
S = jacobian M | transpose (
    map(RM^1,RM^(n),0)
    |
    matrix{apply(k's, k->k-1+(1/10)*random QQ)}
    )
minorsS = minors(n+1,S)
I = minorsS + ideal M
dim I
J = saturate(I,product cc's)
decompose J
