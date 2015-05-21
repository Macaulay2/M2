Hom1 = method()
Hom1(Module, Module) := (M,N) ->(
    --Hom(M,N) as a subquotient with ambient module Hom(cover M, ambient N)
--    AN := ambient N;
    pMstar := dual presentation M; --dual of the relation matrix of M
    DCM := source pMstar;    
    rN := relations N;
    dgM := dual gens M;
    r := DCM**rN; -- the relation matrix of Hom in CM**Ambient N
    
    gN := gens N;
    CN := source gN; --cover of N
    RN := source rN; 
    DRM := dual target pMstar;

    AG := DCM**CN ++ DRM**RN; -- ambient module of the generators of Hom
    g1 := syz map(DRM**ambient N, AG, (pMstar**gN) | (DRM**rN));

    
    subquotient( (DCM**gN) * (AG^[0]) * g1, r)
    )

Hom1(Module, Matrix) := (M,f) ->(
    --f is a map from CN to CN' that lifts to a map RN to RN'
    --so it induces a map source g1 --> source g1' 
    --that is the necessary map on generators of Hom1.
    
--Compute what's needed for M
    pMstar := dual presentation M; --dual of the relation matrix of M
    DCM := source pMstar;    
    DRM := dual target pMstar;
    dgM := dual gens M;

--Now for N
    N := source f;
    rN := relations N;
    RN := source rN; 
    r := DCM**rN; -- the relation matrix of Hom in CM**Ambient N
    gN := gens N;
    CN := source gN; --cover of N
    pN := rN//gens N;
    AG := DCM**CN ++ DRM**RN; -- ambient module of the generators of Hom
    g1 := syz map(DRM**ambient N, AG, (pMstar**gN) | (DRM**rN));
    MN := subquotient( (DCM**gN) * (AG^[0]) * g1, r);
--Now  for N'
    N' := target f;
    rN' := relations N';
    RN' := source rN'; 
    r' := DCM**rN'; -- the relation matrix of Hom in CM**Ambient N
    gN' := gens N';
    CN' := source gN'; --cover of N
    pN' := rN'//gens N';
    AG' := DCM**CN' ++ DRM**RN'; -- ambient module of the generators of Hom
    g1' := syz map(DRM**ambient N', AG', (pMstar**gN') | (DRM**rN'));
    MN' := subquotient( (DCM**gN') * (AG'^[0]) * g1', r');
    f1 :=map(RN', RN, ((matrix f)*pN)//pN');
    F := map(AG', AG, DCM**(matrix f)++DRM**f1);
    map(MN', MN, (F*g1)//g1')
    )


    
end
restart
load "bug-Hom.m2"
S = ZZ/101[a,b,c]
mm = ideal vars S
M = subquotient(gens mm, gens (mm^2))
Hom1(M,M)
prune oo
D = ker vars S -- second syz of k
inD = map(D, mm*D, gens(mm*D)//gens D) -- the inclusion of mm*D into D
ann coker inD == ideal(a,b,c)
--so the image of Hom(B,inD) should contain mm time every element of Hom(B,D); but
Hom1(D, inD)

ann (Hom(D,D)/image Hom(D, inD)) == ideal(a,b,c) -- it's actually (a,b)
ann (Hom1(D,D)/image Hom1(D, inD))== ideal(a,b,c) -- it's actually (a,b)
