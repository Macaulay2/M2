
end

print generateAssertions ///
needsPackage "NormalToricVarieties"
rayList = {{1,0},{1,1},{0,1},{-1,0},{-1,-1},{0,-1}}
coneList = {{0,1},{1,2},{2,3},{3,4},{4,5},{5,0}}
BlP2= normalToricVariety(rayList,coneList)
projEmb = (X,lst)->(Ddiv=toricDivisor(lst,X);        if isAmple Ddiv then (             Ddeg =(weilToClass X)*(transpose matrix{lst});             Wdeg =flatten entries Ddeg;               L=sheaf(X,(ring X)^{Wdeg});             d = rank HH^0(X, L);             R=QQ[y_1..y_d];             kernel map(ring X, R, super basis(Wdeg, ring X)))        else print "not ample")
D={1,1,1,1,1,1}
I = projEmb(BlP2,D);  
transpose gens I
hilbertPolynomial(coker gens I, Projective=>false)
///
