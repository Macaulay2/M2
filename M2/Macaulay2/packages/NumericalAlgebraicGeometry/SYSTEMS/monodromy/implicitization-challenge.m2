restart
needsPackage "NumericalImplicitization"
R = CC[a_(1,0,0)..a_(4,1,1),t_0..t_3],;
S = CC[b_1..b_4,c_1..c_4],;
J = product(apply(toList(1..4), i -> ideal(b_i,c_i))),;
mapList = apply(toList((0,0)..(1,1)), (j,k) -> map(R,S,join(toList(1..4)/(i -> a_(i,j,k)), toList(1..4)/(i -> 1_R)))),;
G = apply(mapList, m -> (m J)_*),;F = toList(0..15)/(i -> (t_0*((G#0)#i) + t_1*((G#2)#i))*(t_2*((G#1)#i) + t_3*((G#3)#i)));
time numericalImageDegree(F, ideal 0_R, maxRepetitiveMonodromies=>2)

restart -- dehomogenize (in a generic manner)
needsPackage "NumericalImplicitization"
R = CC[a_(1,0,0)..a_(4,1,1),t_0..t_3],;
S = CC[b_1..b_4,c_1..c_4],;
J = product(apply(toList(1..4), i -> ideal(b_i,c_i))),;
mapList = apply(toList((0,0)..(1,1)), (j,k) -> map(R,S,join(toList(1..4)/(i -> a_(i,j,k)), toList(1..4)/(i -> 1_R)))),;
G = apply(mapList, m -> (m J)_*),;F = toList(0..15)/(i -> (t_0*((G#0)#i) + t_1*((G#2)#i))*(t_2*((G#1)#i) + t_3*((G#3)#i)));
time numericalImageDegree(F, ideal(random CC*t_0+random CC*t_1-1,random CC*t_0+random CC*t_1-1), maxRepetitiveMonodromies=>2)

restart -- this is a higher degree slice of the previous affine cone
needsPackage "NumericalImplicitization"
R = CC[a_(1,0,0)..a_(4,1,1),t_1..t_2],;
S = CC[b_1..b_4,c_1..c_4],;
J = product(apply(toList(1..4), i -> ideal(b_i,c_i))),;
mapList = apply(toList((0,0)..(1,1)), (j,k) -> map(R,S,join(toList(1..4)/(i -> a_(i,j,k)), toList(1..4)/(i -> 1_R)))),;
G = apply(mapList, m -> (m J)_*),;F = toList(0..15)/(i -> (((G#0)#i) + t_1*((G#2)#i))*(t_2*((G#1)#i) + ((G#3)#i)));
time numericalImageDegree(F, ideal 0_R, maxRepetitiveMonodromies=>2)
