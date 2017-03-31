-- Degree increases by 2 for each added piece; Dimension increases by 5
-- Make random substitution into one function
-- Can we automate the gluing of multiple of the same network?


-- for A 
restart
needsPackage "ReactionNetworks"
needsPackage "MonodromySolver"
load "realroots.m2"

A = oneSiteModificationA()
R = createRing(A, QQ)

F = join(subRandomInitVals A, subRandomReactionRates A)
I = ideal F
netList flatten entries mingens I

A.ConcentrationRates

S = QQ[value(A.ConcentrationRates)#2, 
    value(A.ConcentrationRates)#5,
    value(A.ConcentrationRates)#0, 
    value(A.ConcentrationRates)#3,
    value(A.ConcentrationRates)#4, 
    value(A.ConcentrationRates)#1, 
    MonomialOrder => {Eliminate 1,Lex}]

J = sub(I,S)

gb(J, BasisElementLimit=>1)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>2)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>3)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>4)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>5)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>6)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>7)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>8)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>9)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>10)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>11)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>12)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>13)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>14)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>15)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>16)
netList flatten entries gbSnapshot J


E = eliminate(drop(toList(apply(0..length A.ConcentrationRates-1, i-> 
		value(A.ConcentrationRates#i))),{2,2}), I)

degree I
dim sub(I, QQ[A.ConcentrationRates]) 

--make a ring that only has conc rates as vars
Rx = CC[A.ConcentrationRates]
--create a random linear combination of equations to get a square system
J = sub(I,Rx)
F2 = flatten entries gens J
M' = matrix{F2},
B = random(CC^9, CC^6)
M = M'*B
--compute mixed volume
computeMixedVolume (flatten entries M)


-- 2 copies of A
restart
needsPackage "ReactionNetworks"
needsPackage "MonodromySolver"
load "realroots.m2"

A = oneSiteModificationA()
A' = sub(oneSiteModificationA(), {"S_0" => "S_1", "S_1" => "S_2"})
C'' = glue(A,A')
R = createRing(C'', QQ)

F = join(subRandomInitVals C'', subRandomReactionRates C'')
I = ideal F
netList flatten entries mingens I

S = QQ[value(C''.ConcentrationRates)#2, 
    value(C''.ConcentrationRates)#5,
    value(C''.ConcentrationRates)#0, 
    value(C''.ConcentrationRates)#3,
    value(C''.ConcentrationRates)#6, 
    value(C''.ConcentrationRates)#4,
    value(C''.ConcentrationRates)#1, 
    MonomialOrder => {Eliminate 1,Lex}]
J = sub(I,S)
netList flatten entries gens gb J

gb(J, BasisElementLimit=>1)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>2)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>3)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>4)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>5)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>6)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>7)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>8)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>9)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>10)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>11)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>12)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>13)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>14)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>15)
netList flatten entries gbSnapshot J
gb(J, BasisElementLimit=>16)
netList flatten entries gbSnapshot J

E = eliminate(drop(toList(apply(0..length C''.ConcentrationRates-1, i-> 
		value(C''.ConcentrationRates#i))),{6,6}), I)

degree I
dim sub(I, QQ[C''.ConcentrationRates]) 

--make a ring that only has conc rates as vars
Rx = QQ[C''.ConcentrationRates]
--create a random linear combination of equations to get a square system
J = sub(I,Rx)
F2 = flatten entries gens J
M' = matrix{F2},
B = random(QQ^10, QQ^7)
M = M'*B
--compute mixed volume
computeMixedVolume (flatten entries M)

--quotient ring
A = S/J
B = basis A
dim A
value(C''.ConcentrationRates)#3//basis A
--dim A = degree J when J is prime (We can prove J is prime)


-- 3 copies of A
restart
needsPackage "ReactionNetworks"
needsPackage "MonodromySolver"


A = oneSiteModificationA()
A' = sub(oneSiteModificationA(), {"S_0" => "S_1", "S_1" => "S_2"})
A'' = sub(oneSiteModificationA(), {"S_0" => "S_2", "S_1" => "S_3"})
C'' = glue(A,A')
C' = glue(C'',A'')
R = createRing(C', QQ)

F = join(subRandomInitVals C', subRandomReactionRates C')
I = ideal F
netList flatten entries mingens I

S = QQ[value(C'.ConcentrationRates)#2, 
    value(C'.ConcentrationRates)#5,
    value(C'.ConcentrationRates)#0, 
    value(C'.ConcentrationRates)#3,
    value(C'.ConcentrationRates)#6,
    value(C'.ConcentrationRates)#7, 
    value(C'.ConcentrationRates)#4,
    value(C'.ConcentrationRates)#1, 
    MonomialOrder => {Eliminate 1,Lex}]
J = sub(I,S)
netList flatten entries gens J


E = eliminate(drop(toList(apply(0..length C'.ConcentrationRates-1, i-> 
		value(C'.ConcentrationRates#i))),{2,2}), I)

degree I
dim sub(I, QQ[C'.ConcentrationRates]) 

--make a ring that only has conc rates as vars
Rx = CC[C'.ConcentrationRates]
--create a random linear combination of equations to get a square system
J = sub(I,Rx)
F2 = flatten entries gens J
M' = matrix{F2},
B = random(CC^11, CC^8)
M = M'*B
--compute mixed volume
computeMixedVolume (flatten entries M)


{*
new ring with new monomial order
RNew = QQ[C'.ConcentrationRates, MonomialOrder => {Weights => 
	{1,0,0,1,0,0,1,1}}]
*}


-- 4 copies of A
restart
needsPackage "ReactionNetworks"
needsPackage "MonodromySolver"

A = oneSiteModificationA()
A' = sub(oneSiteModificationA(), {"S_0" => "S_1", "S_1" => "S_2"})
A'' = sub(oneSiteModificationA(), {"S_0" => "S_2", "S_1" => "S_3"})
A'''=sub(oneSiteModificationA(), {"S_0" => "S_3", "S_1" => "S_4"})
C'' = glue(A,A')
C' = glue(C'',A'')
D = glue (C',A''')
R = createRing(D, QQ)

F = join(subRandomInitVals D, subRandomReactionRates D)
I = ideal F
netList flatten entries mingens I

S = QQ[value(D.ConcentrationRates)#2, 
    value(D.ConcentrationRates)#5,
    value(D.ConcentrationRates)#0, 
    value(D.ConcentrationRates)#3,
    value(D.ConcentrationRates)#6,
    value(D.ConcentrationRates)#7, 
    value(D.ConcentrationRates)#8,
    value(D.ConcentrationRates)#4,
    value(D.ConcentrationRates)#1, 
    MonomialOrder => {Eliminate 1,Lex}]
J = sub(I,S)
netList flatten entries gens J

E = eliminate(drop(toList(apply(0..length D.ConcentrationRates-1, i-> 
		value(D.ConcentrationRates#i))),{2,2}), I)

degree I
dim sub(I, QQ[D.ConcentrationRates]) 

--make a ring that only has conc rates as vars
Rx = CC[D.ConcentrationRates]
--create a random linear combination of equations to get a square system
J = sub(I,Rx)
F2 = flatten entries gens J
M' = matrix{F2},
B = random(CC^12, CC^9)
M = M'*B
--compute mixed volume
computeMixedVolume (flatten entries M)



-- 5 copies of A
restart
needsPackage "ReactionNetworks"
needsPackage "MonodromySolver"
A = oneSiteModificationA()
A' = sub(oneSiteModificationA(), {"S_0" => "S_1", "S_1" => "S_2"})
A'' = sub(oneSiteModificationA(), {"S_0" => "S_2", "S_1" => "S_3"})
A'''=sub(oneSiteModificationA(), {"S_0" => "S_3", "S_1" => "S_4"})
B = sub(oneSiteModificationA(), {"S_0" => "S_4", "S_1" => "S_5"})
C'' = glue(A,A')
C' = glue(C'',A'')
D = glue (C',A''')
C = glue(D,B)
R = createRing(C, QQ)

F = join(subRandomInitVals C, subRandomReactionRates C)
I = ideal F
E = eliminate(drop(toList(apply(0..length C.ConcentrationRates-1, i-> 
		value(C.ConcentrationRates#i))),{2,2}), I)

degree I
dim sub(I, QQ[C.ConcentrationRates]) 

--make a ring that only has conc rates as vars
Rx = CC[C.ConcentrationRates]
--create a random linear combination of equations to get a square system
J = sub(I,Rx)
F2 = flatten entries gens J
M' = matrix{F2},
B = random(CC^13, CC^10)
M = M'*B
--compute mixed volume
computeMixedVolume (flatten entries M)

-- experiment
X = reactionNetwork "A+B <--> C, X <--> 2A+D, 2A+D <--> Y, D <--> C+W, B+D <--> Z"
R = createRing(X, QQ)
F = join(subRandomInitVals X, subRandomReactionRates X)
I = ideal F
dim sub(I, QQ[X.ConcentrationRates])
degree I
M=sub(matrix{F}, CC[X.ConcentrationRates])
solveSystem flatten entries M

XAugmented = reactionNetwork ({"A+B <--> C", "X <--> 2A+D", "2A+D <--> Y", 
    "D <--> C+W", "B+D <--> Z", "0 <--> A", "0 <--> B", "0 <--> C", 
    "0 <--> D", "0 <--> W", "0 <--> X", "0 <--> Y", "0 <--> Z"}, NullSymbol => "0")
RAugmented = createRing(XAugmented, QQ)
FAugmented = join(subRandomInitVals XAugmented, subRandomReactionRates XAugmented)
IAugmented = ideal FAugmented
dim sub(IAugmented, QQ[XAugmented.ConcentrationRates])
degree IAugmented


FF = QQ
CEforms1 = conservationEquations(C,FF)
CE1 =sub(CEforms1, apply(gens ring CEforms1, x -> x => 1)) - CEforms1
SSE1 = steadyStateEquations C     	   
T1 = (transpose CE1 || SSE1)
rM1 = sub(random(FF^10, FF^13),R)
G1 = polySystem(rM1 * T1)
