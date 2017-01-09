-- Degree increases by 2 for each added piece; Dimension increases by 5
-- Make random substitution into one function
-- Can we automate the gluing of multiple of the same network?


-- for A 
restart
needsPackage "ReactionNetworks"
needsPackage "MonodromySolver"

A = oneSiteModificationA()

R = createRing(A, QQ)
CE = conservationEquations A
L = toList(apply(0..length A.InitialValues-1, i-> random(QQ)))
Iv = toList(apply(0..length A.InitialValues-1, i-> 
		    value(A.InitialValues#i)))
S = toList(apply(0..length Iv-1, i-> Iv#i=>L#i))
F'' = toList apply(0..length CE-1, i-> sub(CE#i,S))

SS = flatten entries steadyStateEquations A
K = toList(apply(0..length A.ReactionRates-1, i-> random(QQ)))
Rr = toList(apply(0..length A.ReactionRates-1, i-> 
		    value(A.ReactionRates#i)))
P = toList(apply(0..length Rr-1, i-> Rr#i=>sub(K#i,R)))
F' = toList apply(0..length SS-1, i-> sub(SS#i,P))
netList F'

F = join(F', F'')
netList F
I = ideal F
gb I
mingens gb I
netList flatten entries mingens gb I

CR = A.ConcentrationRates
E = eliminate(drop(toList(apply(0..length A.ConcentrationRates-1, i-> 
		value(A.ConcentrationRates#i))),{2,2}), I)

degree I
dim I

--make a ring that only has conc rates as vars
A.ConcentrationRates
Rx = CC[A.ConcentrationRates]
--create a random linear combination of equations to 
--get a square system
M' = matrix{F2},
B = matrix (apply(9, i-> apply(6, i-> random CC))) --random matrix
M = M'*B
--look at ideal in new ring
J = sub(I,Rx)
F2 = flatten entries gens J
computeMixedVolume (flatten entries M)


-- for C'' (2 copies of A)
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

R = createRing(C'', QQ)
CE = conservationEquations C''
L = toList(apply(0..length C''.InitialValues-1, i-> random(QQ)))
Iv = toList(apply(0..length C''.InitialValues-1, i-> 
		    value(C''.InitialValues#i)))
S = toList(apply(0..length Iv-1, i-> Iv#i=>L#i))
F'' = toList apply(0..length CE-1, i-> sub(CE#i,S))

SS = flatten entries steadyStateEquations C''
K = toList(apply(0..length C''.ReactionRates-1, i-> random(QQ)))
Rr = toList(apply(0..length C''.ReactionRates-1, i-> 
		    value(C''.ReactionRates#i)))
P = toList(apply(0..length Rr-1, i-> Rr#i=>sub(K#i,R)))
F' = toList apply(0..length SS-1, i-> sub(SS#i,P))
netList F'

F = join(F', F'')
netList F
I = ideal F
gb I
mingens gb I

Li = flatten entries mingens gb I
D =drop(Li, {0,2})
H = ideal D
CR = C''.ConcentrationRates
E = eliminate(drop(toList(apply(0..length C''.ConcentrationRates-1, i-> 
		value(C''.ConcentrationRates#i))),{2,2}), H)

degree I
dim I

C''.ConcentrationRates
Rx = CC[C''.ConcentrationRates]
--create a random linear combination of equations to 
--get a square system
M' = matrix{F2},
B = matrix (apply(10, i-> apply(7, i-> random CC))) --random matrix
M = M'*B
--look at ideal in new ring
J = sub(I,Rx)
F2 = flatten entries gens J
computeMixedVolume (flatten entries M)


-- for C' (3 copies of A)
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

R = createRing(C', QQ)
CE = conservationEquations C'
L = toList(apply(0..length C'.InitialValues-1, i-> random(QQ)))
Iv = toList(apply(0..length C'.InitialValues-1, i-> 
		    value(C'.InitialValues#i)))
S = toList(apply(0..length Iv-1, i-> Iv#i=>L#i))
F'' = toList apply(0..length CE-1, i-> sub(CE#i,S))

SS = flatten entries steadyStateEquations C'
K = toList(apply(0..length C'.ReactionRates-1, i-> random(QQ)))
Rr = toList(apply(0..length C'.ReactionRates-1, i-> 
		    value(C'.ReactionRates#i)))
P = toList(apply(0..length Rr-1, i-> Rr#i=>sub(K#i,R)))
F' = toList apply(0..length SS-1, i-> sub(SS#i,P))
netList F'

F = join(F', F'')
I = ideal F

C'.ConcentrationRates
E = eliminate(drop(toList(apply(0..length C'.ConcentrationRates-1, i-> 
		value(C'.ConcentrationRates#i))),{2,2}), I)


degree I
dim I

--new ring with new monomial order
RNew = QQ[C'.ConcentrationRates, MonomialOrder => {Weights => 
	{1,0,0,1,0,0,1,1}}]

I' = sub(I,RNew)
gb I'
mingens gb I'
netList flatten entries mingens gb I'

C'.ConcentrationRates
Rx = CC[C'.ConcentrationRates]
--create a random linear combination of equations to 
--get a square system
M' = matrix{F2},
B = matrix (apply(11, i-> apply(8, i-> random CC))) --random matrix
M = M'*B
--look at ideal in new ring
J = sub(I,Rx)
F2 = flatten entries gens J
computeMixedVolume (flatten entries M)


-- for D (4 copies of A)
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

R = createRing(D, QQ)
CE = conservationEquations D
L = toList(apply(0..length D.InitialValues-1, i-> random(QQ)))
Iv = toList(apply(0..length D.InitialValues-1, i-> 
		    value(D.InitialValues#i)))
S = toList(apply(0..length Iv-1, i-> Iv#i=>L#i))
F'' = toList apply(0..length CE-1, i-> sub(CE#i,S))

SS = flatten entries steadyStateEquations D
K = toList(apply(0..length D.ReactionRates-1, i-> random(QQ)))
Rr = toList(apply(0..length D.ReactionRates-1, i-> 
		    value(D.ReactionRates#i)))
P = toList(apply(0..length Rr-1, i-> Rr#i=>sub(K#i,R)))
F' = toList apply(0..length SS-1, i-> sub(SS#i,P))

F = join(F', F'')
I = ideal F

D.ConcentrationRates
E = eliminate(drop(toList(apply(0..length D.ConcentrationRates-1, i-> 
		value(D.ConcentrationRates#i))),{2,2}), I)

degree I
dim I

D.ConcentrationRates
Rx = CC[D.ConcentrationRates]
--create a random linear combination of equations to 
--get a square system
M' = matrix{F2},
B = matrix (apply(12, i-> apply(9, i-> random CC))) --random matrix
M = M'*B
--look at ideal in new ring
J = sub(I,Rx)
F2 = flatten entries gens J
computeMixedVolume (flatten entries M)


-- for C (5 copies of A)
restart
needsPackage "ReactionNetworks"
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
CE = conservationEquations C
L = toList(apply(0..length C.InitialValues-1, i-> random(QQ)))
Iv = toList(apply(0..length C.InitialValues-1, i-> 
		    value(C.InitialValues#i)))
S = toList(apply(0..length Iv-1, i-> Iv#i=>L#i))
F'' = toList apply(0..length CE-1, i-> sub(CE#i,S))

SS = flatten entries steadyStateEquations C
K = toList(apply(0..length C.ReactionRates-1, i-> random(QQ)))
Rr = toList(apply(0..length C.ReactionRates-1, i-> 
		    value(C.ReactionRates#i)))
P = toList(apply(0..length Rr-1, i-> Rr#i=>sub(K#i,R)))
F' = toList apply(0..length SS-1, i-> sub(SS#i,P))

F = join(F', F'')
I = ideal F

C.ConcentrationRates
E = eliminate(drop(toList(apply(0..length C.ConcentrationRates-1, i-> 
		value(C.ConcentrationRates#i))),{2,2}), I)

degree I
dim I












describe R
numgens R



CEforms1 = matrix{conservationEquations(C,FF)}
CE1 =sub(CEforms1, apply(gens ring CEforms1, x -> x => 1)) - CEforms1
SSE1 = matrix {steadyStateEquations C}	       	   
T1 = transpose(CE1|SSE1)
rM1 = sub(random(FF^10, FF^13),R1)
G1 = polySystem(rM1 * T1)