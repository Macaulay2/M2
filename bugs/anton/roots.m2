testRoots = (F,prec) -> (
    r := roots(F,Precision=>prec);
    Rprec := CC_prec[gens ring F];
    assert(0 == clean_(2.^(-7*prec/10)) (sub(F,Rprec) - product(r,a->Rprec_0-a)))
    )
rings = {
    ZZ[x],QQ[x],
    RR_1000[x],CC_1000[x],
    RR[x],
    CC[x]
    }
polyR = ZZ[x]
F = (x^2-1)^3
--F = x^22-x-1
for R in rings do for prec in {53,100,1000} do (
    print (R,prec);
    testRoots(sub(F,R),prec);
    )
roots 0_polyR
roots 1_polyR
polyR = QQ[x]
time f = ((x-1/2)^100*(x-1/3)^50)
elapsedTime roots f 
end

needsPackage "NumericalAlgebraicGeometry"
roots RingElement := {Precision => 53, Unique => false} >> o -> p -> (
    << "calling our roots" << endl;
    sols := solveSystem({F}, Precision => o.Precision);
    sols1 := refine({F}, sols, Bits => o.Precision);
    sols1/coordinates/first//sort
    )

--  toList apply(rawRoots(raw p, o.Precision, o.Unique), r -> new CC from r)

end--

restart
load "roots.m2"

R = QQ[x]
F = (x^2-1)^3
r = roots F
prec=1000
r = roots(F,Precision=>prec)

R = RR[x]
R = CC[x]
R = RR_1000[x]

F = x^4-x^2-3*x-1
F = (x^22-1)
F = (x^2-1)^3
r = roots F
prec=1000
r = roots(F,Precision=>prec)
CC_prec[x]
p = product(r,a->x-a);
clean_(2.^(-prec)) p

R = QQ[x]
F = x^4-x^2-3*x-1
F = (x^2-1)^3
sols53 = roots F
for pt in oo list sub(F, x => pt)
sols100 = roots(F, Precision => 100)
sols200 = roots(F, Precision => 200)
sols400 = roots(F, Precision => 400)
sols800 = roots(F, Precision => 800)
sols100_0 - sols200_0
sols200_0 - sols400_0
sols400_0 - sols800_0
sols = solveSystem({F}, Precision => 100)
sols1 = sols/coordinates/first
for pt in sols1 list sub(F, x => pt)
refine({F}, sols, Bits=>100, ResidualTolerance=>10^-29)
refine({F}, sols, Bits=>200, ResidualTolerance=>10^-29)
refine({F}, sols, Bits=>400, ResidualTolerance=>10^-29)
peek sols_0
matrix sols_0
sols120 = refine({F}, sols, Bits=>120)
sols120/matrix
pt0 = first coordinates sols120_0
sub(F, x => pt0)
methods refine
