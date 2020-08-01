restart
debugLevel = 1
debug needsPackage "Colon"

kk = ZZ/32003
S = kk[x_0,x_1,x_2,x_3,x_4, Degrees => {{1,0},{1,0},{0,1},{0,1},{0,1}}]
B0 = ideal(x_0,x_1)
B1 = ideal(x_2,x_3,x_4)
irr = intersect(B0,B1)

T = kk[t,x_0,x_1,x_2,x_3,x_4, Degrees => {1,1,1,0,0,0}]
T2 = kk[t,x_0,x_1,x_2,x_3,x_4, Degrees => {0,0,0,1,1,1}]

twoGen = J -> (
-- Input: saturated ideal
-- Output: first set of two generators that when saturated
--         give the same ideal as J
    for i from 0 to numgens(J)-2 do (
    	for j from i+1 to numgens(J)-1 do (
	    --print (i,j);
	    if saturate(ideal(J_i,J_j),irr) == J then (
	    	return (true,i,j);
	    	);
	    );
    	);
    return (false,0,0);
    )


threeGen = J -> (
-- Input: saturated ideal
-- Output: first set of three generators that when saturated
--         give the same ideal as J
    for i from 0 to numgens(J)-3 do (
    	for j from i+1 to numgens(J)-2 do (
	    for k from j+1 to numgens(J)-1 do (
	         --print (i,j,k);
	       if saturate(ideal(J_i,J_j,J_k),irr) == J then (
		   if length(res(ideal(J_i,J_j,J_k))) == (dim(ring(J)) - length(degree(J_0))) then
	    	    return (true,i,j,k);
	    	);
	    );
    	);
    );
    return (false,0,0,0);
    )

genSat = (J,n) -> (
-- Input: saturated ideal J and ZZ n
-- Output: all subsets of size n of the generators of J that
--         give the same saturated ideal as J
    use ring(J);
    lists := subsets(numgens(J),n);
    output = {};
    apply(lists, l -> (
	<< "doing " << l << endl;
	I := ideal(J_*_l);
	if elapsedTime saturate(I,irr) == J then (
	    output = append(output,l);
	         );
	     )
	 );
     output
	    )

genSat2 = (J,n) -> (
-- Input: saturated ideal J and ZZ n
-- Output: all subsets of size n of the generators of J that
--         give the same saturated ideal as J
    use ring(J);
    lists := subsets(numgens(J),n);
    output = {};
    apply(lists, l -> (
	<< "doing " << l << endl;
	I := ideal(J_*_l);
	if elapsedTime saturationByElimination(saturationByElimination(I,B0),B1) == J then (
	    output = append(output,l);
	         );
	     )
	 );
     output
	    )

genSat3 = (J,n) -> (
-- Input: saturated ideal J and ZZ n
-- Output: all subsets of size n of the generators of J that
--         give the same saturated ideal as J
    use ring(J);
    lists := subsets(numgens(J),n);
    output = {};
    apply(lists, l -> (
	<< "doing " << l << endl;
	I := ideal(J_*_l);
	if elapsedTime saturationByElimination(saturationByElimination(I,B1),B0) == J then (
	    output = append(output,l);
	         );
	     )
	 );
     output
	    )

paramCurve = method(Options => {PrintPolys => false})
paramCurve(ZZ,ZZ,ZZ) := paramCurve => opts -> (a,b,c) -> (
-- Input: the three degrees of polynomials that parametrize
--         a curve in PP1 x PP2
-- Option: print the polynomials by setting PrintPolys => true
-- Output: a bihomogeneous saturated ideal
    T = kk[t,x_0,x_1,x_2,x_3,x_4,Degrees=>{1,1,1,0,0,0}];
    f1 = sub(sum(apply(a+1,i->random(i,T))),{x_0=>1,x_1=>1,x_2=>1,x_3=>1,x_4=>1});
    g1 = sub(sum(apply(b+1,i->random(i,T))),{x_0=>1,x_1=>1,x_2=>1,x_3=>1,x_4=>1});
    g2 = sub(sum(apply(c+1,i->random(i,T))),{x_0=>1,x_1=>1,x_2=>1,x_3=>1,x_4=>1});
    I = ideal(x_1 - f1, x_3 - g1, x_4 - g2);
    I1 = eliminate(t,I);
    I2 = homogenize(I1,x_0);
    T2 = kk[t,x_0,x_1,x_2,x_3,x_4,Degrees=>{0,0,0,1,1,1}];
    I3 = sub(I2,T2);
    I4 = homogenize(I3,x_2);
    J = sub(I4,S);
    Jsat = saturate(J,irr);
    if dim Jsat != 3 then (
	error "Something went wrong. Ideal is wrong dimension")
    else if opts.PrintPolys == true then (
	return (Jsat,toString(f1), toString(g1), toString(g2));
	);
    Jsat
	)


paramRatCurve = method(Options => {PrintPolys => false})
paramRatCurve(List, List, List) := paramRatCurve => opts -> (a,b,c) -> (
-- Input: three lists that each consist of two integers
--         the first integer is the degree of the numerator
--         and the second integer is the degree of the denominator
--         of the rational function that parametrizes that
--         component of the curve
-- Option: print the polynomials by setting PrintPolys => true
-- Output: a bihomogeneous saturated ideal that corresponds  to a curve
    R = kk[z,t,x_0,x_1,x_2,x_3,x_4,Degrees=>{0,1,1,1,0,0,0}];
    f1num = sub(sum(apply(a_0+1,i->random(i,R))),{x_0=>1,x_1=>1,x_2=>1,x_3=>1,x_4=>1});
    g1num = sub(sum(apply(b_0+1,i->random(i,R))),{x_0=>1,x_1=>1,x_2=>1,x_3=>1,x_4=>1});
    g2num = sub(sum(apply(c_0+1,i->random(i,R))),{x_0=>1,x_1=>1,x_2=>1,x_3=>1,x_4=>1});
    f1den = sub(sum(apply(a_1+1,i->random(i,R))),{x_0=>1,x_1=>1,x_2=>1,x_3=>1,x_4=>1});
    g1den = sub(sum(apply(b_1+1,i->random(i,R))),{x_0=>1,x_1=>1,x_2=>1,x_3=>1,x_4=>1});
    g2den = sub(sum(apply(c_1+1,i->random(i,R))),{x_0=>1,x_1=>1,x_2=>1,x_3=>1,x_4=>1});
    I = ideal(f1den*x_1 - f1num, g1den*x_3 - g1num, g2den*x_4 - g2num,
	1 -z*f1den*g1den*g2den);
    I1' = eliminate(z,I);
    I1 = eliminate(t, I1');
    I2 = homogenize(I1,x_0);
    R2 = kk[t,x_0,x_1,x_2,x_3,x_4,Degrees=>{0,0,0,1,1,1}];
    I3 = sub(I2,R2);
    I4 = homogenize(I3,x_2);
    J = sub(I4,S);
    Jsat = saturate(J,irr);
    if dim Jsat != 3 then (
	error "Something went wrong. Ideal is wrong dimension")
    else if opts.PrintPolys == true then (
	return (Jsat,toString(f1num/f1den), toString(g1num/g1den), toString(g2num/g2den));
	);
    Jsat
	)

end--
restart
load(Colon#"auxiliary files"|"badsaturations.m2")

Jsat = paramCurve(2,4,4);
genSat(Jsat,3); --takes a long time

netList (Jsat_*)


J = paramCurve(2,3,4);
genSat(Jsat,3) -- also takes a long time



J = paramRatCurve({2,2},{3,3},{5,4});
genSat(J,2) -- also takes a long time


-- different possible ideas for saturating
sat1 = J -> saturate(saturate(J,B0),B1)
sat2 = J -> (
    J1 := intersect(saturate(J,x_0),saturate(J,x_1));
    intersect(for i from 2 to 4 list(saturate(J1,x_i)))
    )

--faster saturation of a module over a general toric variety
-- Input : M-module irr-irrelevant
-- Output: N- a irr-saturated module
-*moduleSat = (M,irr) -> (
    compts = decompose irr;
    for i from 0 to length(compts)-1 do (
	N = intersect(apply(numgens(compts_i), j-> saturate(M,compts_i_j)));
	M = N
	);
    N
    )
*-

isVirtual = (C, M, irr) -> (
    if not(genSat(image((presentation M).dd_1),irr) == genSat(image(C.dd_1),irr)) then return (false,0);
-- check pres M
    for i from 1 to length(C) do (
	if not(genSat(ann(HH_i(C)),irr) == 0) then return (false,i);
	);
    return true
    )

