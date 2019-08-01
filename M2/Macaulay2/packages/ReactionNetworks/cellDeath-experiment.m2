restart
needsPackage "ReactionNetworks"

K2 = reactionNetwork("Y+Z --> 2Z") 

K3 = reactionNetwork({"2Y+Z --> Y+2Z", "Y+2Z --> 3Z", "2Y+Z --> 3Z"})

K4 = reactionNetwork({"3Y+Z --> 2Y+2Z", "3Y+Z --> Y+3Z", "3Y+Z --> 4Z", "2Y+2Z --> Y+3Z", 
	"2Y+2Z --> 4Z", "Y+3Z --> 4Z"})

K5 = reactionNetwork({"4Y+Z-->3Y+2Z", "4Y+Z-->2Y+3Z", "4Y+Z-->Y+4Z", "4Y+Z-->5Z",
	"3Y+2Z-->2Y+3Z", "3Y+2Z-->Y+4Z", "3Y+2Z-->5Z", "2Y+3Z-->Y+4Z",
	"2Y+3Z-->5Z", "Y+4Z-->5Z"})


cellDeathIdeal = method()
cellDeathIdeal (ReactionNetwork, Ring) := (Rn, FF) -> (
    R := createRing (Rn, FF);
    F := join(subRandomInitVals Rn, subRandomReactionRates Rn);
    I := ideal F;
    S := FF[Rn.ConcentrationRates];
    J := sub(I, S)
    )
J = cellDeathIdeal(K5, QQ)


-- Edelstein Network
restart
needsPackage "ReactionNetworks"

E = reactionNetwork({"A<-->2A", "A+B<-->C", "C<-->B"})
R = createRing(E, QQ)
F = join(subRandomInitVals E, subRandomReactionRates E)
I = ideal F
S = QQ[E.ConcentrationRates]
J = sub(I,S)
netList flatten entries gens J
dim J
degree J


E2 = glue(E, reactionNetwork({"A+B<-->D", "D<-->B"}))
	    --, "A+B<-->E", "E<-->B"}))
R2 = createRing(E2, QQ)
F2 = join(subRandomInitVals E2, subRandomReactionRates E2)
I2 = ideal F2
S2 = QQ[E2.ConcentrationRates]
J2 = sub(I2,S2)
netList flatten entries gens J2
dim J2
degree J2

E3 = glue(E2, reactionNetwork({"A+B<-->E", "E<-->B"}))
R3 = createRing(E3, QQ)
F3 = join(subRandomInitVals E3, subRandomReactionRates E3)
I3 = ideal F3
S3 = QQ[E3.ConcentrationRates]
J3 = sub(I3,S3)
netList flatten entries gens J3
dim J3
degree J3

{*Q3 = QQ[value(E3.ConcentrationRates)#3,
    value(E3.ConcentrationRates)#0, value(E3.ConcentrationRates)#1,
    value(E3.ConcentrationRates)#2, value(E3.ConcentrationRates)#4,
    MonomialOrder => {Eliminate 1}]
K3 = sub(I3,Q3)
transpose mingens K3*}

---------------------- no conservation equations

E3 = glue(E, {"B<-->2B", "B+C<-->D", "D<-->C"})
R = createRing(E3, QQ)
F = join(subRandomInitVals E3, subRandomReactionRates E3)
I = ideal F
S = QQ[E3.ConcentrationRates]
J = sub(I,S)
dim J
degree J

E4 = glue(E3, {"C<-->2C", "C+D<-->F", "F<-->D"})
R = createRing(E4, QQ)
F = join(subRandomInitVals E4, subRandomReactionRates E4)
I = ideal F
S = QQ[E4.ConcentrationRates]
J = sub(I,S)
dim J
degree J


E5 = glue(E4, {"D<-->2D", "D+F<-->G", "G<-->F"})
R = createRing(E5, QQ)
F = join(subRandomInitVals E5, subRandomReactionRates E5)
I = ideal F
S = QQ[E5.ConcentrationRates]
J = sub(I,S)
dim J
degree J

E6 = glue(E5, {"F<-->2F", "F+G<-->H", "H<-->G"})
R = createRing(E6, QQ)
F = join(subRandomInitVals E6, subRandomReactionRates E6)
I = ideal F
S = QQ[E6.ConcentrationRates]
J = sub(I,S)
dim J
degree J

--one copy - 3 solutions
--two copies - 5 solutions
--n copies for n>2 - twice #solutions for (n-1) copies