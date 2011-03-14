diag = (f) ->(R = ring f;
              map(R^{(rank target f):1}, R^(rank target f), 
              (i,j) -> if i== j then f_(i,0) else 0))
--Take an n by 1 matrix, return the diagonal matrix.

osbd = (L) -> (b1:=#L;
               b2:=reverse subsets(L,b1-1);
               b3:=0;
               b4:=vars R;
               scan(b1, i->(
                   b3=b3+((-1)^i)*(det(diag transpose submatrix(b4,,b2#i),Strategy=>Cofactor))));
               b3)

osbds = (M)->(I := ideal (matrix{{0}}**R);
              scan(M, i->(I = I + ideal osbd(i)));
              ideal mingens I)

osbds2 = (M)->(t0=rank source vars R;
               s1=subsets(t0,4);
               I := ideal (matrix{{0}}**R);
               scan(s1, i->(I = I + ideal ((x_(i#1)*x_(i#2)*x_(i#3))-(x_(i#0)*x_(i#2)*x_(i#3))+(x_(i#0)*x_(i#1)*x_(i#3))-(x_(i#0)*x_(i#1)*x_(i#2)))));
             ideal mingens I)

osalg = (M)->(t0=min mingle M;
              t1=max mingle M;
              S1= ZZ/31991[a_(t0)..a_(t1), MonomialSize=>8];
              S = frac S1;
              R=S[x_(t0)..x_(t1),SkewCommutative=>true, MonomialSize=>8];
              J = (osbds M)+(osbds2 M);
              R/J)

--return the OS algebra for a hyperplane arrangement. 
--Modified to expect matroid input, and to throw in 
--the AUTOMATIC dependencies of which correspond to
--truncating down to a LINE arrangement. 

oshom = (M)->(A=osalg M; 
              out={};
              t4=4; --changed, special to line arrr.
              f = map(A^1,A^{-1},(vars R * transpose vars S1)**A);
              scan(t4, i->(out=append(out, entries inducedMap(image super basis(i+1,target f), image super basis(i+1,source f), f))));
              out)

EPY = (M)->(osmaps = oshom M;
             zs1 = ideal (matrix{{0}}**S1);
             zs2 = ideal (matrix{{1}}**S1);
             out={};
             s = map(S,A,vars S);
             sprime = map(S1,S,vars S1);
             f0=sprime(s(matrix osmaps#2));
             FA = coker f0**S1^{3})             

hessian={{0,3,7},{0,3,11},{0,7,11},
         {1,3,6},{1,3,9},{1,6,9},
         {2,3,8},{2,3,10},{2,8,10},
         {0,4,8},{0,4,9},{0,8,9},
         {1,4,7},{1,4,10},{1,7,10},
         {2,4,6},{2,4,11},{2,6,11},
         {0,5,6},{0,5,10},{0,6,10}, 
         {1,5,8},{1,5,11},{1,8,11},
         {2,5,7},{2,5,9},{2,7,9}}|subsets({0,1,2,3,4,5,6,7,8,9,10,11},4) 

K4 = {{0,1,2},{0,3,4},{2,3,5},{1,4,5}}

end
restart

load "/Users/mike/src/M2/Macaulay2/bugs/mike/1-schenck-os.m2"
load "1-schenck-os.m2"
time EPY hessian; -- 5.3 sec, habanero: 7.6 sec
time E2 = Ext^2(FA,S1); -- 12.5 sec, [habanero: 18.2 sec, 12.4 sec with MonomialSIze=>8]
time annihilator E2; -- 9038 seconds on habanero

time EPY K4;
time E2 = Ext^2(FA,S1); -- 12.5 sec, [habanero: 18.2 sec, 12.4 sec with MonomialSIze=>8]
time annihilator E2; -- 9038 seconds on habanero

time ann(Ext^2(EPY(K4),S1)
--             0   1
-- o6 = total: 1 427
--          0: 1   1
--          1: .   .
--          2: .  16
--          3: . 410

time ann(E2, Strategy=>Quotient);


time gens gb presentation E2; -- 13.63 sec on MBP, 2.4 GHz, MonomialSize=>8
E2a = matrix entries presentation E2;
time gens gb presentation E2; -- 13.63 sec on MBP, 2.4 GHz, MonomialSize=>8
 
time gens gb E2a; -- 13.63 sec on MBP, 2.4 GHz, MonomialSize=>8

E2a = matrix entries presentation E2;
time gens gb(E2a, Algorithm=>LinearAlgebra); -- appears to be very incorrect
 -- I think because the monomial order for modules being used is different
time gens gb(E2a, Algorithm=>Homogeneous2);

S2 = ZZ/31991[gens S1, MonomialSize=>8, MonomialOrder=>{Position=>Up}]
E2b = sub(E2a,S2)
time gens gb E2b; -- very bad....
S2 = ZZ/31991[gens S1, MonomialSize=>8, MonomialOrder=>{Position=>Down}]
E2b = sub(E2a,S2)
time gens gb E2b; -- very good: 4.6 sec
time gens gb(E2b, Algorithm=>Homogeneous2); -- 3.3 sec
M = coker E2b;
gbTrace=3
time ann M;

S2 = ZZ/31991[gens S1, MonomialSize=>8, MonomialOrder=>{numgens S1, Position=>Up}]
E2b = sub(E2a,S2);
time gens gb E2b; -- 11.1 sec (621 GB elems)
time gens gb(E2b, Algorithm=>LinearAlgebra); -- 0.74 sec


