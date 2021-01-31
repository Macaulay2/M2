end

-- according to Allen Knutson, this runs in less than 3GB for a week and then mysteriously just dies

-- I tried it and it used up 6 GB in less than 15 minutes and then died properly, out of memory

syze = 5; -- for 3 and 4, no segfault
R = QQ[t, x_(1,1) .. x_(syze,syze)];
X = matrix apply(syze, i->apply(syze, j->x_(i+1,j+1)));
SlowerPart = M -> matrix apply(syze, i->apply(syze, j->(if (i>j) then M_(i,j) else 0)));
brake = A -> {A-SlowerPart(A),SlowerPart(A)};
cpl = (p1,p2)->{p1_0 * p2_0, SlowerPart(p1_0*p2_1 + p1_1*p2_0)}
cp = (A,B)->(p := cpl(brake(A),brake(B)); p_0 + p_1)
Xcp3 = cp(X,cp(X,X)); 
I3 = ideal Xcp3;
gbTrace = 3;
sy = syz gens I3
