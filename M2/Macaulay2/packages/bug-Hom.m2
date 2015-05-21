Hom1 = method()
Hom1(Module, Module) := (M,N) ->(
    --Hom(M,N) as a subquotient with ambient module Hom(cover M, ambient N)
--    ambN := ambient N;
    rMstar := dual relations M; --dual of the relation matrix of M
    DCM := source rMstar;    
    rN := relations N;    
    r := DCM**rN; -- the relation matrix of Hom
    
    gN := gens N;
    CN := source gN; --cover of N
    RN := source rN; 
    DRM := dual target rMstar;

    AG := DCM**CN ++ DRM**RN; -- ambient module of the generators of Hom
    g1 := syz map(DRM**CN, AG, (rMstar**CN) | (DRM**rN));

    
    subquotient( (DCM**gN) * (AG^[0]) * g1, r)
    )

end
restart
load "bug-Hom.m2"
S = ZZ/101[a,b,c]
mm = ideal vars S
M = subquotient(gens mm, gens (mm^2))
Hom1(M,M)

D = ker vars S -- second syz of k
inD = map(D, mm*D, gens(mm*D)//gens D) -- the inclusion of mm*D into D
ann coker inD == ideal(a,b,c)
--so the image of Hom(B,inD) should contain mm time every element of Hom(B,D); but
ann (Hom(D,D)/image Hom(D, inD)) === ideal(a,b,c) -- it's actually (a,b)


Hom1(D,D) == Hom(D,D)
