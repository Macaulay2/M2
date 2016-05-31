
restart
load "chemical-networks.m2"

FF = CC

F = reactionNetwork (FF[k_1..k_6], {(A,infinity),(B,infinity),(C,infinity),(D,infinity),(E,infinity)}, {
	({(1,A)} => {(2,B)},k_2,k_1), 
	({(1,A), (1,C)} => {(1,D)},k_4,k_3),
        ({(1,A), (1,C)} => {(1,B),(1,E)},0,k_5),
	({(1,B), (1,E)} => {(1,D)},0,k_6)
	}) 
F

netList F
I = ideal F
R = ring I
S = FF[gens R]
kstart = apply(gens coefficientRing R, k -> k => random FF)
J = sub(sub(I, kstart), S)


dim J
M = matrix {{-1,2,0,0,0},{1,0,1,-1,0},{0,1,0,-1,1},{1,-1,1,0,-1}}

St = ideal(random(FF^1, FF^2) - vars S * sub(gens ker M,FF))

J = J + St

randmat = random(FF^5, FF^7)
rank randmat


G = polySystem(randmat * transpose gens J )
peek G


needsPackage "NumericalAlgebraicGeometry"
sols = solveSystem polySystem J
#sols
realPoints sols

J' = sub(sub(sub(I, k_6 => 0), kstart), S)
J' = J' + St
G' = polySystem(randmat * transpose gens J' )

sols' = track(G, G', sols)


dim J'
degree J'
sols'' = solveSystem polySystem J'
netList sortSolutions sols'
netList sortSolutions sols''

netList sols
realPoints sols

-- automate deletion of edge
-- graphs package, digraphs?
-- function which creates rxn network from graph
-- gens for stoichiometric space, separate from ss invariants desirable
-- y matrix, A matrix
-- check toric, check balanced, check endotactic?
-- multistationarity (real, pos >1)?
-- manipuations of CRNs: how to reduce,