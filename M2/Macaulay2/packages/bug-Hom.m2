Hom1 = method()
Hom1(Module, Module) := (M,N) ->(
    --Hom(M,N) as a subquotient with ambient module Hom(cover M, ambient N).

    --Key to notation:
    --CM = cover M, DCM = dual cover M
    --rN = relation matrix of N
    --gM = generator matrix of M
    --dgM = dual of gM
    
    pMstar := dual presentation M; --dual of the relation matrix of M
    DCM := source pMstar;    
    rN := relations N;
    dgM := dual gens M;
    r := DCM**rN; -- the relation matrix of Hom in DCM**Ambient N
    
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
    r := DCM**rN; -- the relation matrix of Hom in DCM**Ambient N
    gN := gens N;
    CN := source gN; --cover of N
    pN := rN//gens N;
    AG := DCM**CN ++ DRM**RN; -- ambient module of the generators of Hom
    g1 := syz map(DRM**ambient N, AG, (pMstar**gN) | (DRM**rN));
    MN := subquotient( (DCM**gN) * (AG^[0]) * g1, r); --Hom(M,N)
    
--Now  for N'
    N' := target f;
    rN' := relations N';
    RN' := source rN'; 
    r' := DCM**rN'; -- the relation matrix of Hom in DCM**Ambient N
    gN' := gens N';
    CN' := source gN'; --cover of N
    pN' := rN'//gens N';
    AG' := DCM**CN' ++ DRM**RN'; -- ambient module of the generators of Hom
    g1' := syz map(DRM**ambient N', AG', (pMstar**gN') | (DRM**rN'));
    MN' := subquotient( (DCM**gN') * (AG'^[0]) * g1', r'); --Hom(M,N')
    f1 :=map(RN', RN, ((matrix f)*pN)//pN');
    F := map(AG', AG, DCM**(matrix f)++DRM**f1);
    map(MN', MN, (F*g1)//g1')
    )


    
end
restart
load "bug-Hom.m2"


TEST///
R = ZZ/101[a,b,c]
M = ker vars R
N = ideal vars R * M
f = map(M, N, gens N//gens M) -- the inclusion of N into M
assert isHomogeneous f
assert isWellDefined f
M' = coker presentation M
N' = coker presentation N
f' = map(M',M,1) * f * map(N,N',1)
assert isHomogeneous f'
assert isWellDefined f'
assert(ann prune coker Hom(M',f') == ann prune coker Hom(f',M')) -- should be symmetric
assert (Hom1(R^1,M) == M)
    ///
TEST///
S = ZZ/101[a,b,c]
mm = ideal vars S
M = subquotient(gens mm, gens (mm^2))
assert(ann Hom(M,M) == mm)
assert(rank target gens prune Hom(M,M) == 9)

D = ker vars S -- second syz of k
inD = map(D, mm*D, gens(mm*D)//gens D) -- the inclusion of mm*D into D
assert(ann (Hom(D,D)/image Hom(D, inD))== ideal(a,b,c))
///


