debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

-- problem of 4 lines in P3 w.r.t. 2 random flags

-- the following creates random n-k x k matrices
Gs = createRandomFlagsForSimpleSchubert((2,4),{1},{1})

-- this creates random matrices for all
G1 = randomSchubertProblemInstance({{1},{1},{1},{1}},2,4)
-- since the flags afected are the id, rsort id, and two general
-- but only the first n-k part of them, we do the following
G1 = drop(G1,2)
--IDOP = rsort id_(FFF^4)

GforSimple = apply(G1, g->(
	submatrix(transpose last g,{0..4-2-1}, )
	))

Sols = solveSimpleSchubert((2,4),{1},{1}, GforSimple)

-- to translate into a SchubertProblem of the shape of LR-homotopies
E = skewSchubertVariety((2,4),{1},{1})
Sols = apply(Sols ,s-> transpose sub(E,matrix{s}) )

SchbPblm = {
    ({1},id_(FFF^4)),
    ({1},rsort id_(FFF^4))
    }|G1
scan(Sols, s-> checkIncidenceSolution(s, SchbPblm))


------------------------------
-- problem (2,1),(1,1),1^3 =  in G(2,6)

l = {2,1};
m = {1,1};
conds = {l,m}|toList(3:{1})
Gs = randomSchubertProblemInstance(conds, 2,6)

G = drop(Gs,2)

GforSimple = apply(G, g->(
	transpose (last g)_{0..6-2-1}
	))

Sols = solveSimpleSchubert((2,6),{2,1},{1,1}, GforSimple)

E = skewSchubertVariety((2,6),{2,1},{1,1})
Sols = apply(Sols ,s-> transpose sub(E,matrix{s}) )

-- *** NOTE when we transpose the solutions
--          we are inverting the order in which
--          we solve the first two Schubert conditions
--          (see transpose E)
SchbPblm = {
    ({1,1},id_(FFF^6)),
    ({2,1},rsort id_(FFF^6))
    }|G
scan(Sols, s-> checkIncidenceSolution(s, SchbPblm))


