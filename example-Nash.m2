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
    R := FF[a_(1,1)..a_(N,N)][p_(1,1)..p_(S,n)];
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

///


TEST ///
S=3
n=3
N = S*(n-1)
R = FF[a_(1,1)..a_(N,N)][p_(1,1)..p_(S,n)]
A = genericMatrix(coefficientRing R, a_(1,1), N, N)
S=3
n=3
N = S*(n-1)
Q = matrix flatten toList(apply(0..S-1,i -> toList (
    if i == 0 then (
	for j from 1 to n-1 list join(toList(n-1: 0_CC), toList(N-((n-1)*(i+1)): 1_CC))
	) 
	else if  i == S-1 then (
	for j from 1 to n-1 list join(toList((n-1)*i: 1_CC), toList(n-1: 0_CC))
	) 
	else (
	for j from 1 to n-1 list join(toList((n-1)*i: 1_CC), toList(n-1: 0_CC), toList(N-(n-1)*(i+1): 1_CC)))
	)
    ))
det Q
glynn Q
///
