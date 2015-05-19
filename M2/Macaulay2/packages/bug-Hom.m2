Hom1 = method()
Hom1(Module, Module) := (M,N) ->(
    mstar := dual presentation M; --dual of the relation matrix of M
    n := presentation N; --relation matrix of N
    DCM := source mstar; -- dual of the cover of M
    DRM := target mstar; 
    CN := target n; -- cover of N
    RN := source n;
    r := (dual id_DCM)**n;
    p0 := (DCM**CN++DRM**RN)^[0];
    g1 := kernel(map (DRM**CN, DCM**CN++DRM**RN, mstar**id_CN | id_DRM**n));
    subquotient((target g1)^[0]*g1,r))

end
restart
load "bug-Hom.m2"
S = ZZ/101[a,b,c]
mm = ideal vars S
D = ker vars S -- second syz of k
inD = map(D, mm*D, gens(mm*D)//gens D) -- the inclusion of mm*D into D
ann coker inD == ideal(a,b,c)
--so the image of Hom(B,inD) should contain mm time every element of Hom(B,D); but
ann (Hom(D,D)/image Hom(D, inD)) === ideal(a,b,c) -- it's actually (a,b)


Hom1(D,D) == Hom(D,D)
