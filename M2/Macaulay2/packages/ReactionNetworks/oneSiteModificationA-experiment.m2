-- Degree increases by 2 for each added piece; Dimension increases by 5
-- Make random substitution into one function
-- Can we automate the gluing of multiple of the same network?


-- for A 
restart
needsPackage "ReactionNetworks"
needsPackage "MonodromySolver"
--load "realroots.m2"

multipleModificationA = n -> (
    A := oneSiteModificationA();
    for i from 2 to n do 
    A = glue(A, sub(oneSiteModificationA(), {"S_0" => "S_"|(i-1), "S_1" => "S_"|i}));
    A
    )

{*
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
*}

{*
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
*}

end -------------------------------

restart
load "~/M2/M2/Macaulay2/packages/ReactionNetworks/oneSiteModificationA-experiment.m2"

FF = QQ
n = 60
An = multipleModificationA n
Rn = createRing(An, FF)
Fn = join(subRandomInitVals An, subRandomReactionRates An)
In = ideal Fn
Sn=FF[toList(apply(0..length An.ConcentrationRates-1,
	    i-> value(An.ConcentrationRates)#i))]
Jn = sub(In, Sn)
elapsedTime degree Jn 

Sn = FF[value(An.ConcentrationRates)#2,
    value(An.ConcentrationRates)#5,
    value(An.ConcentrationRates)#0, 
    value(An.ConcentrationRates)#3,
    toList(apply(6..n+4, i-> value(An.ConcentrationRates)#i)),
    value(An.ConcentrationRates)#4,
    value(An.ConcentrationRates)#1, 
    MonomialOrder => {Eliminate 1,Lex}]

netList (flatten entries gens Jn//unique)

    


gb(Jn, BasisElementLimit=>1)
netList flatten entries gbSnapshot Jn
gb(Jn, BasisElementLimit=>2)
netList flatten entries gbSnapshot Jn
gb(Jn, BasisElementLimit=>3)
netList flatten entries gbSnapshot Jn
gb(Jn, BasisElementLimit=>4)
netList flatten entries gbSnapshot Jn
gb(Jn, BasisElementLimit=>5)
netList flatten entries gbSnapshot Jn
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

--quotient ring
A = Sn/Jn
B = basis A
dim A
value(An.ConcentrationRates)#3//basis A
--dim A = degree J when J is prime (We can prove J is prime)


