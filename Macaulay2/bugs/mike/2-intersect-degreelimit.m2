-- file name: intersection.m2
-- intersect needs a degree limit

-- allow intersect to use: DegreeLimit, Strategy=>Iterative

end

intersection = method(Dispatch => Thing, TypicalValue=>Ideal, Options => {DegreeLimit=>{}})

intersection Sequence := 
intersection List := o -> L -> (
     if not all(L, x -> instance(x,Ideal))
     then error "expected a list of ideals";
     B := directSum apply(L, generators);
     A := map(target B, 1, (i,j) -> 1);
     ideal syz(A|B, SyzygyRows => 1, DegreeLimit=>o.DegreeLimit)
     )

-- what about intersection of subspaces of polynomials of the same degree?
-- The above will do more work that required, since it will make
-- spairs that are not used...
end
restart
load "2-intersect-degreelimit.m2"

R = ZZ/32003[a..d]
I = (ideal random(R^1, R^{-2,-3,-5}))
J = (ideal random(R^1, R^{-2,-3,-6}))
K = ideal random(R^1, R^{-2,-3,-8})
time intersect(I,J,K);
betti oo
time intersection(I,J,K,DegreeLimit=>6);
time intersection(I,J,K,DegreeLimit=>7);
time intersection(I,J,K);
