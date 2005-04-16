R = ZZ/32003[a..e];
I = ideal(random(3,R),random(3,R),random(3,R))
gens gb(I,PairLimit=>7);
g = gb(I,StopBeforeComputation => true);
leadTerm gens g
gbSnapshot = (I) -> gens gb(I,StopBeforeComputation => true);
leadTerm gbSnapshot(I)
