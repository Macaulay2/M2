restart
load "chemical-networks.m2"

-- warning: switch bac to QQ for dimension / degree and other symb. computations to make sense
FF = QQ

F = reactionNetwork (FF[k_1..k_6], {(A,infinity),(B,infinity),(C,infinity),(D,infinity),(E,infinity)}, {
	({(1,A)} => {(2,B)},k_2,k_1), 
	({(1,A), (1,C)} => {(1,D)},k_4,k_3),
        ({(1,A), (1,C)} => {(1,B),(1,E)},0,k_5),
	({(1,B), (1,E)} => {(1,D)},0,k_6)
	}) 
peek F

netList F

-- I consists of steady-state invariants
I = ideal F

R = ring I
S = FF[gens R]
coefficientRing R
coefficientRing S
-- random setting of k_i for homotopy
-- lets us compute dimension
kstart = apply(gens coefficientRing R, k -> k => random FF)
J = sub(sub(I, kstart), S)
dim J

-- now add the stoichiometric equations in
M = matrix {{-1,2,0,0,0},{1,0,1,-1,0},{0,1,0,-1,1},{1,-1,1,0,-1}}
St = ideal(random(FF^1, FF^2) - vars S * sub(gens ker M,FF))
J = J + St

-- compute dimension of ideal w/ stoichiometric eqns added
help 
netList flatten entries gens J
dim J 


-- now, what happens when k_5 ==0
-- TODO: write function to automate later
-- zeroOut = (Paramlist, I) -> apply(Paramlist 

J' = sub(sub(sub(I, {k_2 => 0, k_6 => 0}), kstart), S)
coefficientRing ring J'
J' = J' + St

dim J'
degree J
degree J'
degree J


needsPackage "NumericalAlgebraicGeometry"

-- randmat lets us square up overdetermined systems J, J'
randmat = random(FF^5, FF^7)
rank randmat

-- construct start system G
G = polySystem(randmat * transpose gens J )
peek G

-- we can solve G
sols = solveSystem polySystem J
netList sols
realPoints sols

degree J'
G' = polySystem(randmat * transpose gens J' )


-- solve zeroed out system using homotopy cont
sols' = track(G, G', sols)
netList sols'
peek sols'

-- sanity check for sols'
sols'' = solveSystem polySystem J'
sols''

help solveSystem

netList sortSolutions sols'
netList sortSolutions sols''

-- automate deletion of edge
-- graphs package, digraphs?
-- function which creates rxn network from graph
-- gens for stoichiometric space, separate from ss invariants desirable
-- y matrix, A matrix
-- check toric, check balanced, check endotactic?
-- multistationarity (real, pos >1)?
-- manipulations of CRNs: how to reduce,

needsPackage "Graphs"
ReactionNetwork = new Type of Graph
