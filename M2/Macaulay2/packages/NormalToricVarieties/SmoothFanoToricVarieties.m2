---------------------------------------------------------------------------
-- PURPOSE: create a database of smooth toric Fano varieties
-- PROGRAMMER : Gregory G. Smith 
-- UPDATE HISTORY : 17 May 2009
---------------------------------------------------------------------------
--path = append(path, homeDirectory | "Code/");
loadPackage "ToricQuivers"

filename = homeDirectory | "Code/smoothFanoToricVarieties.dbm"
dataBase = openDatabaseOut filename

nonfaces = X -> apply(first entries gens dual monomialIdeal X, r -> apply(support r, j -> index j))

-----------------------------------------------------------------------------------------------
-- SURFACES                       
-----------------------------------------------------------------------------------------------
-- 2:1 ---- PP^2
X = projectiveSpace(2);
dataBase#"2:1:r" = toString rays X
dataBase#"2:1:L" = toString nonfaces X
dataBase#"2:1:A" = toString degrees X

-- 2:2 ---- PP^1 * PP^1
X = projectiveSpace(1) * projectiveSpace(1);
dataBase#"2:2:r" = toString rays X
dataBase#"2:2:L" = toString nonfaces X
dataBase#"2:2:A" = toString degrees X

-- 2:3 ---- FF_1
X = hirzebruchSurface(1);
dataBase#"2:3:r" = toString rays X
dataBase#"2:3:L" = toString nonfaces X
dataBase#"2:3:A" = toString degrees X

-- 2:4 ---- blow-up of PP^2 at two points
V =  {{1,0},{0,1},{-1,1},{-1,0},{0,-1}};
P = matrix{{-1,1,-1,0,0},{-1,0,0,-1,0},{0,-1,1,-1,0},{0,-1,0,0,-1},{0,0,-1,1,-1}};
A = gradingGuess(P);
X = toricVariety(V,P,A);
dataBase#"2:4:r" = toString rays X
dataBase#"2:4:L" = toString nonfaces X
dataBase#"2:4:A" = toString degrees X

-- 2:5 ---- blow-up of PP^2 at three points
V = {{1,0},{0,1},{-1,1},{-1,0},{0,-1},{1,-1}};
PR = matrix{{-1,1,-1,0,0,0},{-1,0,0,-1,0,0},{-1,0,0,0,-1,1},{0,-1,1,-1,0,0},{0,-1,0,0,-1,0},{1,-1,0,0,0,-1},{0,0,-1,1,-1,0},{0,0,-1,0,0,-1},{0,0,0,-1,1,-1}};
A = (gradingGuess(PR))^{0,3,1,5}
X = toricVariety(V,PR,A);
dataBase#"2:5:r" = toString rays X
dataBase#"2:5:L" = toString nonfaces X
dataBase#"2:5:A" = toString degrees X


-----------------------------------------------------------------------------------------------
-- THREEFOLDS
-----------------------------------------------------------------------------------------------
-- 3:1 ---- PP^3
X = projectiveSpace(3);
dataBase#"3:1:r" = toString rays X
dataBase#"3:1:L" = toString nonfaces X
dataBase#"3:1:A" = toString degrees X

-- 3:2 ---- (Bat B_1): PP_Y(OO_Y oplus OO_Y(2)) over Y = PP^2
-- ODA: Theorem 1.32 page 59: (3^2*4^3)' with a = 2
V = {{1,0,0},{0,1,0},{-1,-1,-2},{0,0,1},{0,0,-1}};
PR = matrix{{-1,-1,-1,0,2},{0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"3:2:r" = toString rays X
dataBase#"3:2:L" = toString nonfaces X
dataBase#"3:2:A" = toString degrees X

-- 3:3 ---- (Bat B_2): PP_Y(OO_Y oplus OO_Y(1)) over Y = PP^2
V = {{1,0,0},{0,1,0},{-1,-1,-1},{0,0,1},{0,0,-1}};
PR = matrix{{-1,-1,-1,0,1},{0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"3:3:r" = toString rays X
dataBase#"3:3:L" = toString nonfaces X
dataBase#"3:3:A" = toString degrees X

-- 3:4 ---- (Bat B_3): PP(OO_Y oplus OO_Y oplus OO_Y(1)) over Y = PP^1
-- ODA: Theorem 1.32 page 59: (3^2*4^3)'' with b = 0 & c=1
V = {{1,0,0},{0,1,0},{-1,-1,0},{0,0,1},{0,1,-1}};
PR = matrix{{-1,-1,-1,0,0},{0,1,0,-1,-1}};
A = gradingGuess(PR)
X = toricVariety(V,PR,A);
dataBase#"3:4:r" = toString rays X
dataBase#"3:4:L" = toString nonfaces X
dataBase#"3:4:A" = toString degrees X

-- 3:5 ---- (Bat B_4): PP^2 * PP^1
X = projectiveSpace(2) * projectiveSpace(1);
dataBase#"3:5:r" = toString rays X
dataBase#"3:5:L" = toString nonfaces X
dataBase#"3:5:A" = toString degrees X

-- 3:6 ---- (Bat C_1): PP(OO_Y oplus OO_Y(1,1)) over Y = PP^1 * PP^1
V = {{1,0,0},{-1,0,1},{0,1,0},{0,-1,1},{0,0,1},{0,0,-1}};
PR = matrix{{-1,-1,0,0,1,0},{0,0,-1,-1,1,0}, {0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"3:6:r" = toString rays X
dataBase#"3:6:L" = toString nonfaces X
dataBase#"3:6:A" = toString degrees X

-- 3:7 ---- (Bat C_2): PP(OO_Y oplus OO_Y(l)) over Y = FF_1 where l^2 = 1
V = {{1,0,0},{-1,-1,1},{0,1,0},{0,-1,1},{0,0,1},{0,0,-1}};
PR = matrix{{-1,-1,0,1,0,0},{0,0,-1,-1,1,0},{0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"3:7:r" = toString rays X
dataBase#"3:7:L" = toString nonfaces X
dataBase#"3:7:A" = toString degrees X

-- 3:8 ---- (Bat C_3): PP^1 * PP^1 * PP^1
X = projectiveSpace(1) * projectiveSpace(1) * projectiveSpace(1);
dataBase#"3:8:r" = toString rays X
dataBase#"3:8:L" = toString nonfaces X
dataBase#"3:8:A" = toString degrees X

-- 3:9 ---- (Bat C_4): FF_1 * PP^1
X = hirzebruchSurface(1) * projectiveSpace(1);
dataBase#"3:9:r" = toString rays X
dataBase#"3:9:L" = toString nonfaces X
dataBase#"3:9:A" = toString degrees X

-- 3:10 ---- (Bat C_5): PP(OO_Y oplus OO_Y(1,-1)) over Y = PP^1 * PP^1 
V = {{1,0,0},{-1,0,1},{0,1,0},{0,-1,-1},{0,0,1},{0,0,-1}};
PR = matrix{{-1,-1,0,0,1,0},{0,0,-1,-1,0,1},{0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"3:10:r" = toString rays X
dataBase#"3:10:L" = toString nonfaces X
dataBase#"3:10:A" = toString degrees X

-- 3:11 ---- (Bat D_1): blow-up of PP^3
V = {{1,0,0},{0,1,0},{0,0,1},{-1,-1,2},{-1,-1,1},{0,0,-1}};
PR = matrix{{-1,-1,2,-1,0,0},{0,0,-1,0,0,-1},{-1,-1,1,0,-1,0},{0,0,0,-1,1,-1},{0,0,-1,1,-1,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"3:11:r" = toString rays X
dataBase#"3:11:L" = toString nonfaces X
dataBase#"3:11:A" = toString degrees X

-- 3:12 ---- (Bat D_2): blow-up of PP^1 on PP^2 * PP^1
V = {{1,0,0},{0,1,0},{0,0,1},{-1,0,-1},{1,-1,0},{0,-1,0}};
PR = matrix{{-1,0,-1,-1,0,0},{1,-1,0,0,-1,0},{0,0,-1,-1,-1,1},{-1,0,0,0,1,-1},{0,-1,0,0,0,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"3:12:r" = toString rays X
dataBase#"3:12:L" = toString nonfaces X
dataBase#"3:12:A" = toString degrees X

-- 3:13 ---- (Bat E_1): S_2-bundle over PP^1 where S_2 is PP^2 blown up at two points
V =  {{1,0,0},{0,1,0},{0,0,1},{-1,0,-1},{0,-1,0},{-1,0,0},{-1,1,0}};
PR = matrix{{-1,0,0,0,0,-1,0},{-1,1,0,0,0,0,-1},{0,-1,0,0,-1,0,0},{0,-1,0,0,0,-1,1},{0,0,0,0,-1,1,-1},{0,0,-1,-1,0,1,0}};
A = (gradingGuess(PR))^{1,2,0,3};
X = toricVariety(V,PR,A);
dataBase#"3:13:r" = toString rays X
dataBase#"3:13:L" = toString nonfaces X
dataBase#"3:13:A" = toString degrees X

-- 3:14 ---- (Bat E_2): S_2-bundle over PP^1 where S_2 is PP^2 blown up at two points
V = {{1,0,0},{0,1,0},{0,0,1},{-1,1,-1},{0,-1,0},{-1,0,0},{-1,1,0}};
PR = matrix{{-1,0,0,0,0,-1,0},{-1,1,0,0,0,0,-1},{0,-1,0,0,-1,0,0},{0,-1,0,0,0,-1,1},{0,0,0,0,-1,1,-1},{0,0,-1,-1,0,0,1}};
A = (gradingGuess(PR))^{1,2,0,3};
X = toricVariety(V,PR,A);
dataBase#"3:14:r" = toString rays X
dataBase#"3:14:L" = toString nonfaces X
dataBase#"3:14:A" = toString degrees X

-- 3:15 ---- (Bat E_3): S_2 * PP^1 where S_2 is PP^2 blown up at two points
V = {{1,0,0},{0,1,0},{0,0,1},{0,0,-1},{0,-1,0},{-1,0,0},{-1,1,0}};
PR = matrix{{-1,0,0,0,0,-1,0},{-1,1,0,0,0,0,-1},{0,-1,0,0,-1,0,0},{0,-1,0,0,0,-1,1},{0,0,0,0,-1,1,-1},{0,0,-1,-1,0,0,0}};
A = (gradingGuess(PR))^{1,2,0,3};
X = toricVariety(V,PR,A);
dataBase#"3:15:r" = toString rays X
dataBase#"3:15:L" = toString nonfaces X
dataBase#"3:15:A" = toString degrees X

-- 3:16 ---- (Bat E_4): S_2-bundle over PP^1 where S_2 is PP^2 blown up at two points
V = {{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{0,-1,0},{-1,0,0},{-1,1,0}};
PR = matrix{{-1,0,0,0,0,-1,0},{-1,1,0,0,0,0,-1},{0,-1,0,0,-1,0,0},{0,-1,0,0,0,-1,1},{0,0,0,0,-1,1,-1},{0,0,-1,-1,1,0,0}};
A = (gradingGuess(PR))^{1,2,0,3};
X = toricVariety(V,PR,A);
dataBase#"3:16:r" = toString rays X
dataBase#"3:16:L" = toString nonfaces X
dataBase#"3:16:A" = toString degrees X

-- 3:17 ---- (Bat F_1): S_3 * PP^1 where S_3 is PP^2 blown up at three points
V = {{1,0,0},{1,0,1},{0,0,1},{-1,0,0},{-1,0,-1},{0,0,-1},{0,1,0},{0,-1,0}};
PR = matrix{{0,0,-1,0,0,-1,0,0},{-1,1,-1,0,0,0,0,0},{1,-1,0,0,0,-1,0,0},{0,-1,1,-1,0,0,0,0},{0,-1,0,0,-1,0,0,0},{-1,0,0,-1,0,0,0,0},{0,0,0,-1,1,-1,0,0},{-1,0,0,0,-1,1,0,0},{0,0,-1,1,-1,0,0,0},{0,0,0,0,0,0,-1,-1}};
A = (gradingGuess(PR))^{0,4,2,6,1};
X = toricVariety(V,PR,A);
dataBase#"3:17:r" = toString rays X
dataBase#"3:17:L" = toString nonfaces X
dataBase#"3:17:A" = toString degrees X

-- 3:18 ---- (Bat F_2): S_3-bundle over PP^1 where S_3 is PP^2 blown up at three points
V = {{1,0,0},{1,0,1},{0,0,1},{-1,0,0},{-1,0,-1},{0,0,-1},{0,1,0},{1,-1,0}};
PR = matrix{{0,0,-1,0,0,-1,0,0},{-1,1,-1,0,0,0,0,0},{1,-1,0,0,0,-1,0,0},{0,-1,1,-1,0,0,0,0},{0,-1,0,0,-1,0,0,0},{-1,0,0,-1,0,0,0,0},{0,0,0,-1,1,-1,0,0},{-1,0,0,0,-1,1,0,0},{0,0,-1,1,-1,0,0,0},{1,0,0,0,0,0,-1,-1}};
A = (gradingGuess(PR))^{0,4,2,6,1};
X = toricVariety(V,PR,A);
dataBase#"3:18:r" = toString rays X
dataBase#"3:18:L" = toString nonfaces X
dataBase#"3:18:A" = toString degrees X


-----------------------------------------------------------------------------------------------
-- FOURFOLDS
-----------------------------------------------------------------------------------------------
-- 4:1 ---- PP^4
X = projectiveSpace(4);
dataBase#"4:1:r" = toString rays X
dataBase#"4:1:L" = toString nonfaces X
dataBase#"4:1:A" = toString degrees X

-- 4:2 ---- (Bat B_1): PP_Y(OO_Y oplus OO_Y(3)) over Y = PP^3
V =  {{1,0,0,0},{0,1,0,0},{0,0,1,0},{-1,-1,-1,3},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,-1,3,0},{0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:2:r" = toString rays X
dataBase#"4:2:L" = toString nonfaces X
dataBase#"4:2:A" = toString degrees X
  
-- 4:3 ---- (Bat B_2): PP_Y(OO_Y oplus OO_Y(2)) over Y = PP^3
V = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{-1,-1,-1,2},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,-1,2,0},{0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:3:r" = toString rays X
dataBase#"4:3:L" = toString nonfaces X
dataBase#"4:3:A" = toString degrees X

-- 4:4 ---- (Bat B_3): PP_Y(OO_Y oplus OO_Y(1)) over Y = PP^3
V = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{-1,-1,-1,1},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,-1,1,0},{0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:4:r" = toString rays X
dataBase#"4:4:L" = toString nonfaces X
dataBase#"4:4:A" = toString degrees X
 
-- 4:5 ---- (Bat B_4): PP^3 * PP^1 
X = projectiveSpace(3) * projectiveSpace(1);
dataBase#"4:5:r" = toString rays X
dataBase#"4:5:L" = toString nonfaces X
dataBase#"4:5:A" = toString degrees X

-- 4:6 ---- (Bat B_5): PP_Y(OO_Y oplus OO_Y oplus OO_Y oplus OO_Y(1)) over Y = PP^1
V = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{-1,-1,-1,0},{0,0,0,1},{1,0,0,-1}};
PR = matrix{{-1,-1,-1,-1,0,0},{1,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:6:r" = toString rays X
dataBase#"4:6:L" = toString nonfaces X
dataBase#"4:6:A" = toString degrees X

-- 4:7 ---- (Bat C_1): PP_Y(OO_Y oplus OO_Y oplus OO_Y(2)) over Y = PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{1,0,1,0},{1,0,0,1},{0,0,-1,-1}};
PR = matrix{{-1,-1,-1,0,0,0},{2,0,0,-1,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:7:r" = toString rays X
dataBase#"4:7:L" = toString nonfaces X
dataBase#"4:7:A" = toString degrees X

-- 4:8 ---- (Bat C_2): PP_Y(OO_Y oplus OO_Y oplus OO_Y(1)) over Y = PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{1,0,1,0},{0,0,0,1},{0,0,-1,-1}};
PR = matrix{{-1,-1,-1,0,0,0},{1,0,0,-1,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:8:r" = toString rays X
dataBase#"4:8:L" = toString nonfaces X
dataBase#"4:8:A" = toString degrees X

-- 4:9 ---- (Bat C_3): PP_Y(OO_Y oplus OO_Y(1) oplus OO_Y(1)) over Y = PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{1,0,1,0},{0,1,0,1},{0,0,-1,-1}};
PR = matrix{{-1,-1,-1,0,0,0},{1,1,0,-1,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:9:r" = toString rays X
dataBase#"4:9:L" = toString nonfaces X
dataBase#"4:9:A" = toString degrees X

-- 4:10 ---- (Bat C_4): PP^2 * PP^2
X = projectiveSpace(2) * projectiveSpace(2);
dataBase#"4:10:r" = toString rays X
dataBase#"4:10:L" = toString nonfaces X
dataBase#"4:10:A" = toString degrees X

-- 4:11 ---- (Bat E_1): blow up of PP_Y(OO_Y oplus OO_Y(2)) over Y = PP^3
V = {{1,0,0,0},{2,-1,-1,-1},{0,1,0,0},{0,0,1,0},{0,0,0,1},{3,-1,-1,-1},{-1,0,0,0}};
PR = matrix{{2,-1,-1,-1,-1,0,0},{3,0,-1,-1,-1,-1,0},{0,1,0,0,0,-1,-1},{-1,0,0,0,0,0,-1},{-1,-1,0,0,0,1,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:11:r" = toString rays X
dataBase#"4:11:L" = toString nonfaces X
dataBase#"4:11:A" = toString degrees X

-- 4:12 ---- (Bat E_2): blow up of PP_Y(OO_Y oplus OO_Y(1)) over Y = PP^3
V = {{1,0,0,0},{1,-1,-1,-1},{0,1,0,0},{0,0,1,0},{0,0,0,1},{2,-1,-1,-1},{-1,0,0,0}};
PR = matrix{{1,-1,-1,-1,-1,0,0},{2,0,-1,-1,-1,-1,0},{0,1,0,0,0,-1,-1},{-1,0,0,0,0,0,-1},{-1,-1,0,0,0,1,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:12:r" = toString rays X
dataBase#"4:12:L" = toString nonfaces X
dataBase#"4:12:A" = toString degrees X

-- 4:13 ---- (Bat E_3): blow up of PP^1 * PP^3
V = {{1,0,0,0},{0,-1,-1,-1},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,-1,-1,-1},{-1,0,0,0}};
PR = matrix{{0,-1,-1,-1,-1,0,0},{1,0,-1,-1,-1,-1,0},{0,1,0,0,0,-1,-1},{-1,0,0,0,0,0,-1},{-1,-1,0,0,0,1,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:13:r" = toString rays X
dataBase#"4:13:L" = toString nonfaces X
dataBase#"4:13:A" = toString degrees X

-- 4:14 ---- (Bat D_1): PP_Y(OO oplus OO(1,2)) over Y = PP^1*PP^1 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,2},{0,0,1,0},{0,0,-1,1},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,0,0,2,0},{0,0,0,-1,-1,1,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:14:r" = toString rays X
dataBase#"4:14:L" = toString nonfaces X
dataBase#"4:14:A" = toString degrees X

-- 4:15 ---- (Bat D_2): PP^1 bundle over PP_Y(OO oplus OO(2)) where Y = PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,2,0},{0,0,1,0},{0,0,-1,1},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,2,0,0,0},{0,0,0,-1,-1,1,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:15:r" = toString rays X
dataBase#"4:15:L" = toString nonfaces X
dataBase#"4:15:A" = toString degrees X

-- 4:16 ---- (Bat D_3): PP^1 bundle over PP_Y(OO oplus OO(1)) where Y = PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,1},{0,0,1,0},{0,0,-1,1},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,1,0,1,0},{0,0,0,-1,-1,1,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:16:r" = toString rays X
dataBase#"4:16:L" = toString nonfaces X
dataBase#"4:16:A" = toString degrees X

-- 4:17 ---- (Bat D_4): PP^1 * PP_Y(OO oplus OO(1,2)) over Y = PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,2},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,0,0,2,0},{1,0,0,-1,-1,0,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:17:r" = toString rays X
dataBase#"4:17:L" = toString nonfaces X
dataBase#"4:17:A" = toString degrees X

-- 4:18 ---- (Bat D_5): PP^1 * PP_Y(OO oplus OO(1,1)) over Y = PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,2,0},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,2,0,0,0},{0,0,0,-1,-1,0,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:18:r" = toString rays X
dataBase#"4:18:L" = toString nonfaces X
dataBase#"4:18:A" = toString degrees X

-- 4:19 ---- (Bat D_6): PP_Y(OO oplus OO oplus O(2)) where Y = PP^1 * PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,1},{0,0,1,0},{0,0,-1,1},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,0,0,1,0},{0,0,0,-1,-1,1,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:19:r" = toString rays X
dataBase#"4:19:L" = toString nonfaces X
dataBase#"4:19:A" = toString degrees X

-- 4:20 ---- (Bat D_7): PP_Y(OO oplus OO oplus O(1,1)) where Y = PP^1 * PP^1
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{1,0,0,-1}};
PR = matrix{{-1,-1,-1,0,0,0,0},{1,0,0,-1,-1,0,0},{1,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:20:r" = toString rays X
dataBase#"4:20:L" = toString nonfaces X
dataBase#"4:20:A" = toString degrees X

-- 4:21 ---- (Bat D_8): PP^1 bundle over PP_Y(OO oplus OO(1)) where Y = PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,0},{0,0,1,0},{0,0,-1,1},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,1,0,0,0},{0,0,0,-1,-1,1,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:21:r" = toString rays X
dataBase#"4:21:L" = toString nonfaces X
dataBase#"4:21:A" = toString degrees X

-- 4:22 ---- (Bat D_9): PP^1 bundle over PP_Y(OO oplus OO(1)) where Y = PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,1},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,1,0,1,0},{0,0,0,-1,-1,0,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:22:r" = toString rays X
dataBase#"4:22:L" = toString nonfaces X
dataBase#"4:22:A" = toString degrees X

-- 4:23 ---- (Bat D_10): PP^1 bundle over PP_Y(OO oplus OO oplus OO(1)) where Y = PP^1
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,1},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,0,0,1,0},{1,0,0,-1,-1,0,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:23:r" = toString rays X
dataBase#"4:23:L" = toString nonfaces X
dataBase#"4:23:A" = toString degrees X

-- 4:24 ---- (Bat D_11): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{0,0,1,-1}};
PR = matrix{{-1,-1,-1,0,0,0,0},{1,0,0,-1,-1,0,0},{0,0,0,1,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:24:r" = toString rays X
dataBase#"4:24:L" = toString nonfaces X
dataBase#"4:24:A" = toString degrees X

-- 4:25 ---- (Bat D_12): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,0},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,1,0,0,0},{0,0,0,-1,-1,0,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:25:r" = toString rays X
dataBase#"4:25:L" = toString nonfaces X
dataBase#"4:25:A" = toString degrees X

-- 4:26 ---- (Bat D_13): PP^1 * PP^1 * PP^2
X = projectiveSpace(1) * projectiveSpace(1) * projectiveSpace(2);
dataBase#"4:26:r" = toString rays X
dataBase#"4:26:L" = toString nonfaces X
dataBase#"4:26:A" = toString degrees X

-- 4:27 ---- (Bat D_14): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{1,0,0,-1}};
PR = matrix{{-1,-1,-1,0,0,0,0},{0,0,0,-1,-1,0,0},{1,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:27:r" = toString rays X
dataBase#"4:27:L" = toString nonfaces X
dataBase#"4:27:A" = toString degrees X

-- 4:28 ---- (Bat D_15): PP^2 * F_1 
X = projectiveSpace(2) * hirzebruchSurface(1);
dataBase#"4:28:r" = toString rays X
dataBase#"4:28:L" = toString nonfaces X
dataBase#"4:28:A" = toString degrees X

-- 4:29 ---- (Bat D_16):  
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,-1},{0,0,1,0},{0,0,-1,1},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,1,0,0,1},{0,0,0,-1,-1,1,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:29:r" = toString rays X
dataBase#"4:29:L" = toString nonfaces X
dataBase#"4:29:A" = toString degrees X

-- 4:30 ---- (Bat D_17): PP_Y(OO oplus OO(1,0) oplus OO(0,1)) where Y = PP^1 * PP^1
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{0,1,0,-1}};
PR = matrix{{-1,-1,-1,0,0,0,0},{1,0,0,-1,-1,0,0},{0,1,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:30:r" = toString rays X
dataBase#"4:30:L" = toString nonfaces X
dataBase#"4:30:A" = toString degrees X

-- 4:31 ---- (Bat D_18): PP_Y(OO oplus OO(-1,2)) where Y = PP^1 * PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,-2},{0,0,1,0},{0,0,-1,1},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,0,0,0,2},{0,0,0,-1,-1,1,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:31:r" = toString rays X
dataBase#"4:31:L" = toString nonfaces X
dataBase#"4:31:A" = toString degrees X

-- 4:32 ---- (Bat D_19): PP_Y(OO oplus OO(-1,1)) where Y = PP^1 * PP^2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,-1},{0,0,1,0},{0,0,-1,1},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{-1,-1,-1,0,0,0,1},{0,0,0,-1,-1,1,0},{0,0,0,0,0,-1,-1}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:32:r" = toString rays X
dataBase#"4:32:L" = toString nonfaces X
dataBase#"4:32:A" = toString degrees X

-- 4:33 ---- (Bat G_1): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,-1,0},{0,0,1,0},{0,0,0,1},{2,0,-1,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,-1},{1,-1,-1,-1,0,0,0},{2,0,0,-1,-1,-1,0},{0,1,1,0,-1,-1,-1},{-1,-1,-1,0,1,1,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:33:r" = toString rays X
dataBase#"4:33:L" = toString nonfaces X
dataBase#"4:33:A" = toString degrees X

-- 4:34 ---- (Bat G_2): blow up of a surface PP^1 * PP^1 on C_2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{0,0,0,1},{2,0,-1,-1},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,-1},{0,-1,-1,-1,0,0,1},{2,0,0,-1,-1,-1,0},{1,0,0,0,-1,-1,-1},{-1,-1,-1,0,0,0,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:34:r" = toString rays X
dataBase#"4:34:L" = toString nonfaces X
dataBase#"4:34:A" = toString degrees X

-- 4:35 ---- (Bat G_3): blow up of a curve PP^1 on C_3
V = {{1,0,0,0},{0,1,0,0},{0,-1,-1,0},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,-1},{0,-1,-1,-1,0,0,0},{1,0,0,-1,-1,-1,0},{0,1,1,0,-1,-1,-1},{-1,-1,-1,0,1,1,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:35:r" = toString rays X
dataBase#"4:35:L" = toString nonfaces X
dataBase#"4:35:A" = toString degrees X

-- 4:36 ---- (Bat G_4): blow up of a surface FF_1 on C_2
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,1,-1,-1},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,-1},{0,-1,-1,-1,0,0,1},{1,1,0,-1,-1,-1,0},{0,1,0,0,-1,-1,-1},{-1,-1,-1,0,0,0,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:36:r" = toString rays X
dataBase#"4:36:L" = toString nonfaces X
dataBase#"4:36:A" = toString degrees X

-- 4:37 ---- (Bat G_5): blow up of a surface PP^1 * PP^1 on C_3
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,-1},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,-1},{0,-1,-1,-1,0,0,1},{0,0,0,-1,-1,-1,0},{0,1,1,0,-1,-1,-1},{-1,-1,-1,0,0,0,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:37:r" = toString rays X
dataBase#"4:37:L" = toString nonfaces X
dataBase#"4:37:A" = toString degrees X

-- 4:38 ---- (Bat G_6): blow up of a surface PP^1 * PP^1 on C_4
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,-1},{0,-1,-1,-1,0,0,1},{1,0,0,-1,-1,-1,0},{0,0,0,0,-1,-1,-1},{-1,-1,-1,0,0,0,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:38:r" = toString rays X
dataBase#"4:38:L" = toString nonfaces X
dataBase#"4:38:A" = toString degrees X

-- 4:39 ---- (Bat H_1): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,2,-1,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,0,0,0}, {-1,1,0,0,0,0,0,-1},{0,-1,0,0,0,0,-1,1}, {0,0,-1,0,0,0,0,-1},{-1,0,0,0,0,0,-1,0}, {0,2,0,-1,-1,-1,0,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:39:r" = toString rays X
dataBase#"4:39:L" = toString nonfaces X
dataBase#"4:39:A" = toString degrees X

-- 4:40 ---- (Bat H_2): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,1,-1,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,0,0,0},{-1,1,0,0,0,0,0,-1},{0,-1,0,0,0,0,-1,1}, {0,0,-1,0,0,0,0,-1},{-1,0,0,0,0,0,-1,0}, {1,1,0,-1,-1,-1,0,0}};
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:40:r" = toString rays X
dataBase#"4:40:L" = toString nonfaces X
dataBase#"4:40:A" = toString degrees X

-- 4:421---- (Bat H_3): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{2,0,-1,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,0,0,0},{-1,1,0,0,0,0,0,-1},{0,-1,0,0,0,0,-1,1},{0,0,-1,0,0,0,0,-1},{-1,0,0,0,0,0,-1,0}, {2,0,0,-1,-1,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:41:r" = toString rays X
dataBase#"4:41:L" = toString nonfaces X
dataBase#"4:41:A" = toString degrees X

-- 4:42 ---- (Bat H_4): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,1,-1,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,0,0,0},{-1,1,0,0,0,0,0,-1},{0,-1,0,0,0,0,-1,1},{0,0,-1,0,0,0,0,-1},{-1,0,0,0,0,0,-1,0},{0,1,0,-1,-1,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:42:r" = toString rays X
dataBase#"4:42:L" = toString nonfaces X
dataBase#"4:42:A" = toString degrees X

-- 4:43 ---- (Bat H_5): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,0,0,0},{-1,1,0,0,0,0,0,-1},{0,-1,0,0,0,0,-1,1},{0,0,-1,0,0,0,0,-1},{-1,0,0,0,0,0,-1,0},{1,0,0,-1,-1,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:43:r" = toString rays X
dataBase#"4:43:L" = toString nonfaces X
dataBase#"4:43:A" = toString degrees X

-- 4:44 ---- (Bat H_6): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{2,-1,-1,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,0,0,0},{-1,1,0,0,0,0,0,-1},{0,-1,0,0,0,0,-1,1},{0,0,-1,0,0,0,0,-1},{-1,0,0,0,0,0,-1,0},{1,0,1,-1,-1,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:44:r" = toString rays X
dataBase#"4:44:L" = toString nonfaces X
dataBase#"4:44:A" = toString degrees X

-- 4:45 ---- (Bat H_7): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{2,-2,-1,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,0,0,0},{-1,1,0,0,0,0,0,-1},{0,-1,0,0,0,0,-1,1},{0,0,-1,0,0,0,0,-1},{-1,0,0,0,0,0,-1,0},{0,0,2,-1,-1,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:45:r" = toString rays X
dataBase#"4:45:L" = toString nonfaces X
dataBase#"4:45:A" = toString degrees X

-- 4:46 ---- (Bat H_8): PP^2 * S_2
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,0,0,0},{-1,1,0,0,0,0,0,-1},{0,-1,0,0,0,0,-1,1},{0,0,-1,0,0,0,0,-1},{-1,0,0,0,0,0,-1,0},{0,0,0,-1,-1,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:46:r" = toString rays X
dataBase#"4:46:L" = toString nonfaces X
dataBase#"4:46:A" = toString degrees X

-- 4:47 ---- (Bat H_9): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,-1,-1,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,0,0,0},{-1,1,0,0,0,0,0,-1},{0,-1,0,0,0,0,-1,1},{0,0,-1,0,0,0,0,-1},{-1,0,0,0,0,0,-1,0},{0,0,1,-1,-1,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:47:r" = toString rays X
dataBase#"4:47:L" = toString nonfaces X
dataBase#"4:47:A" = toString degrees X

-- 4:48 ---- (Bat H_10): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,-1,-1,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,0,0,0},{-1,1,0,0,0,0,0,-1},{0,-1,0,0,0,0,-1,1},{0,0,-1,0,0,0,0,-1},{-1,0,0,0,0,0,-1,0},{0,0,1,-1,-1,-1,1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:48:r" = toString rays X
dataBase#"4:48:L" = toString nonfaces X
dataBase#"4:48:A" = toString degrees X

-- 4:49 ---- (Bat L_1): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{1,0,0,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{1,-1,-1,0,0,0,0,0},{1,0,0,-1,-1,0,0,0},{1,0,0,0,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:49:r" = toString rays X
dataBase#"4:49:L" = toString nonfaces X
dataBase#"4:49:A" = toString degrees X

-- 4:50 ---- (Bat L_2): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{1,-1,-1,0},{0,0,0,1},{1,-1,0,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{1,-1,-1,0,0,0,0,0}, {0,0,1,-1,-1,0,0,0},{0,0,1,0,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:50:r" = toString rays X
dataBase#"4:50:L" = toString nonfaces X
dataBase#"4:50:A" = toString degrees X

-- 4:51 ---- (Bat L_3): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{0,0,1,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{1,-1,-1,0,0,0,0,0},{1,0,0,-1,-1,0,0,0},{0,0,0,1,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:51:r" = toString rays X
dataBase#"4:51:L" = toString nonfaces X
dataBase#"4:51:A" = toString degrees X

-- 4:52 ---- (Bat L_4): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{1,-1,-1,0},{0,0,0,1},{0,0,1,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{1,-1,-1,0,0,0,0,0},{0,0,1,-1,-1,0,0,0},{0,0,0,1,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:52:r" = toString rays X
dataBase#"4:52:L" = toString nonfaces X
dataBase#"4:52:A" = toString degrees X

-- 4:53 ---- (Bat L_5): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,0,0},{0,0,1,0},{0,-1,-1,0},{0,0,0,1},{0,-1,0,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{0,-1,-1,0,0,0,0,0},{0,0,1,-1,-1,0,0,0},{0,0,1,0,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:53:r" = toString rays X
dataBase#"4:53:L" = toString nonfaces X
dataBase#"4:53:A" = toString degrees X

-- 4:54 ---- (Bat L_6): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,0,0},{0,0,1,0},{0,-1,-1,0},{0,0,0,1},{0,0,1,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{0,-1,-1,0,0,0,0,0},{0,0,1,-1,-1,0,0,0},{0,0,0,1,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:54:r" = toString rays X
dataBase#"4:54:L" = toString nonfaces X
dataBase#"4:54:A" = toString degrees X

-- 4:55 ---- (Bat L_7): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,1,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{1,-1,-1,0,0,0,0,0},{0,0,0,-1,-1,0,0,0},{0,0,0,1,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:55:r" = toString rays X
dataBase#"4:55:L" = toString nonfaces X
dataBase#"4:55:A" = toString degrees X

-- 4:56 ---- (Bat L_8): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,0,0},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,0,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{0,-1,-1,0,0,0,0,0},{0,0,0,-1,-1,0,0,0},{0,0,0,0,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:56:r" = toString rays X
dataBase#"4:56:L" = toString nonfaces X
dataBase#"4:56:A" = toString degrees X

-- 4:57 ---- (Bat L_9): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,0,0},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,1,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{0,-1,-1,0,0,0,0,0},{0,0,0,-1,-1,0,0,0},{0,0,0,1,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:57:r" = toString rays X
dataBase#"4:57:L" = toString nonfaces X
dataBase#"4:57:A" = toString degrees X

-- 4:58 ---- (Bat L_10): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{1,-1,-1,0},{0,0,0,1},{0,1,0,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{1,-1,-1,0,0,0,0,0},{0,0,1,-1,-1,0,0,0},{0,1,0,0,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:58:r" = toString rays X
dataBase#"4:58:L" = toString nonfaces X
dataBase#"4:58:A" = toString degrees X

-- 4:59 ---- (Bat L_11): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,0,0},{0,0,1,0},{0,-1,-1,0},{0,0,0,1},{0,1,0,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{0,-1,-1,0,0,0,0,0},{0,0,1,-1,-1,0,0,0},{0,1,0,0,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:59:r" = toString rays X
dataBase#"4:59:L" = toString nonfaces X
dataBase#"4:59:A" = toString degrees X

-- 4:60 ---- (Bat L_12): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{-1,0,-1,0},{0,0,0,1},{0,0,1,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{1,-1,-1,0,0,0,0,0},{0,0,0,-1,-1,0,0,1},{0,0,0,1,0,-1,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:60:r" = toString rays X
dataBase#"4:60:L" = toString nonfaces X
dataBase#"4:60:A" = toString degrees X

-- 4:61 ---- (Bat L_13): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{-1,0,0,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{1,-1,-1,0,0,0,0,0},{1,0,0,-1,-1,0,0,0},{0,0,0,0,0,-1,-1,1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:61:r" = toString rays X
dataBase#"4:61:L" = toString nonfaces X
dataBase#"4:61:A" = toString degrees X

-- 4:62 ---- (Bat I_1): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,1,0},{0,0,1,0},{0,0,0,1},{2,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,1,0,0,0,0},{2,0,0,-1,-1,-1,0,0},{1,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:62:r" = toString rays X
dataBase#"4:62:L" = toString nonfaces X
dataBase#"4:62:A" = toString degrees X

-- 4:63 ---- (Bat I_2): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{2,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0},{2,0,0,-1,-1,-1,0,0},{1,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:63:r" = toString rays X
dataBase#"4:63:L" = toString nonfaces X
dataBase#"4:63:A" = toString degrees X

-- 4:64 ---- (Bat I_3): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,1,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0},{1,1,0,-1,-1,-1,0,0},{0,1,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:64:r" = toString rays X
dataBase#"4:64:L" = toString nonfaces X
dataBase#"4:64:A" = toString degrees X

-- 4:65 ---- (Bat I_4): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,0},{0,0,1,0},{0,0,0,1},{2,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,0,0,0,0,1},{2,0,0,-1,-1,-1,0,0},{1,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:65:r" = toString rays X
dataBase#"4:65:L" = toString nonfaces X
dataBase#"4:65:A" = toString degrees X

-- 4:66 ---- (Bat I_5): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,0,1},{0,0,1,0},{0,0,0,1},{2,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0}, {0,-1,-1,0,1,0,0,0},{2,0,0,-1,-1,-1,0,0},{1,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:66:r" = toString rays X
dataBase#"4:66:L" = toString nonfaces X
dataBase#"4:66:A" = toString degrees X

-- 4:67 ---- (Bat I_6): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,1,0},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,1,0,0,0,0},{1,0,0,-1,-1,-1,0,0},{0,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:67:r" = toString rays X
dataBase#"4:67:L" = toString nonfaces X
dataBase#"4:67:A" = toString degrees X

-- 4:68 ---- (Bat I_7): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,1},{2,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,0,0,0,0,0},{2,0,0,-1,-1,-1,0,0},{1,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:68:r" = toString rays X
dataBase#"4:68:L" = toString nonfaces X
dataBase#"4:68:A" = toString degrees X

-- 4:69 ---- (Bat I_8): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,1,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,0,0,0,1,0},{1,1,0,-1,-1,-1,0,0},{0,1,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:69:r" = toString rays X
dataBase#"4:69:L" = toString nonfaces X
dataBase#"4:69:A" = toString degrees X

-- 4:70 ---- (Bat I_9): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,0},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,0,0,0,0,1},{1,0,0,-1,-1,-1,0,0},{0,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:70:r" = toString rays X
dataBase#"4:70:L" = toString nonfaces X
dataBase#"4:70:A" = toString degrees X

-- 4:71 ---- (Bat I_10): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0},{1,0,0,-1,-1,-1,0,0},{0,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:71:r" = toString rays X
dataBase#"4:71:L" = toString nonfaces X
dataBase#"4:71:A" = toString degrees X

-- 4:72---- (Bat I_11): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,1},{1,1,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,0,0,0,0,0},{1,1,0,-1,-1,-1,0,0},{0,1,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:72:r" = toString rays X
dataBase#"4:72:L" = toString nonfaces X
dataBase#"4:72:A" = toString degrees X

-- 4:73 ---- (Bat I_12): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,0,0,0,1,0},{1,0,0,-1,-1,-1,0,0},{0,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:73:r" = toString rays X
dataBase#"4:73:L" = toString nonfaces X
dataBase#"4:73:A" = toString degrees X

-- 4:74 ---- (Bat I_13): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,0,0,0,0,0},{1,0,0,-1,-1,-1,0,0},{0,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:74:r" = toString rays X
dataBase#"4:74:L" = toString nonfaces X
dataBase#"4:74:A" = toString degrees X

-- 4:75 ---- (Bat I_14): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,0,1},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,0,1,0,0,0},{1,0,0,-1,-1,-1,0,0},{0,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:75:r" = toString rays X
dataBase#"4:75:L" = toString nonfaces X
dataBase#"4:75:A" = toString degrees X

-- 4:76 ---- (Bat I_15): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{0,0,0,1},{2,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{-1,0,0,1,0,0,0,-1},{0,0,0,-1,0,0,-1,1},{-1,0,0,0,0,0,-1,0},{0,-1,-1,0,0,0,1,0},{2,0,0,-1,-1,-1,0,0},{1,0,0,0,-1,-1,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:76:r" = toString rays X
dataBase#"4:76:L" = toString nonfaces X
dataBase#"4:76:A" = toString degrees X

-- 4:77 ---- (Bat M_1): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,1},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,0,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{-1,-1,-1,1,0,1,0,0},{0,1,1,-1,0,-1,0,-1},{0,0,0,-1,-1,0,0,0},{0,0,0,0,0,-1,-1,0},{0,-1,-1,0,-1,1,0,1},{0,-1,-1,1,0,0,-1,1}}
A = gradingGuess(PR)
X = toricVariety(V,PR,A);
dataBase#"4:77:r" = toString rays X
dataBase#"4:77:L" = toString nonfaces X
dataBase#"4:77:A" = toString degrees X

-- 4:78 ---- (Bat M_2): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,1},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{1,0,0,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{-1,-1,-1,1,0,1,0,0},{0,1,1,-1,0,-1,0,-1},{1,0,0,-1,-1,0,0,0},{1,0,0,0,0,-1,-1,0},{0,-1,-1,0,-1,1,0,0},{0,-1,-1,1,0,0,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:78:r" = toString rays X
dataBase#"4:78:L" = toString nonfaces X
dataBase#"4:78:A" = toString degrees X

-- 4:79 ---- (Bat M_3): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,1},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{-1,-1,-1,1,0,1,0,0},{0,1,1,-1,0,-1,0,-1},{1,0,0,-1,-1,0,0,0},{0,0,0,0,1,-1,-1,0},{0,-1,-1,0,-1,1,0,0},{0,-1,-1,0,0,0,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:79:r" = toString rays X
dataBase#"4:79:L" = toString nonfaces X
dataBase#"4:79:A" = toString degrees X

-- 4:80 ---- (Bat M_4): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,1,1},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{0,0,0,-1},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,-1},{-1,-1,-1,1,0,1,0,0},{0,1,1,-1,0,-1,0,-1},{1,0,0,-1,-1,0,0,0},{0,0,0,0,0,-1,-1,0},{0,-1,-1,0,-1,1,0,0},{0,-1,-1,1,0,0,-1,1}}
A = gradingGuess(PR)
X = toricVariety(V,PR,A);
dataBase#"4:80:r" = toString rays X
dataBase#"4:80:L" = toString nonfaces X
dataBase#"4:80:A" = toString degrees X

-- 4:81 ---- (Bat M_5): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,1},{0,0,1,0},{1,0,-1,-1},{0,0,0,1},{1,0,0,-1},{0,0,-1,-1}};
PR = matrix{{-1,0,0,0,1,0,0,-1},{-1,-1,-1,0,0,1,0,0},{0,0,0,-1,0,-1,0,-1},{0,0,0,-1,-1,0,1,0},{1,0,0,0,0,-1,-1,0},{0,-1,-1,0,-1,1,0,1},{0,-1,-1,0,0,0,-1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:81:r" = toString rays X
dataBase#"4:81:L" = toString nonfaces X
dataBase#"4:81:A" = toString degrees X

-- 4:82 ---- (Bat J_1): 
V = {{1,0,0,0},{0,1,0,0},{-1,-1,-1,0},{0,0,1,0},{0,0,0,1},{0,0,-1,-1},{-1,0,0,0},{-1,0,1,0}};
PR = matrix{{0,0,0,-1,0,0,-1,1},{-1,-1,-1,0,1,1,0,0},{0,1,1,0,-1,-1,-1,0},{-1,0,0,1,0,0,0,-1},{-1,0,0,0,0,0,-1,0},{0,0,0,-1,-1,-1,0,0},{0,0,0,0,-1,-1,1,-1},{0,-1,-1,-1,0,0,1,0},{0,-1,-1,0,0,0,2,-1}}
A = gradingGuess(PR)^{1,2,0,5}
X = toricVariety(V,PR,A);
dataBase#"4:82:r" = toString rays X
dataBase#"4:82:L" = toString nonfaces X
dataBase#"4:82:A" = toString degrees X     

-- 4:83 ---- (Bat J_2): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,-1,0},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{-1,0,0,0},{-1,0,1,0}}
PR = matrix{{0,0,0,-1,0,0,-1,1},{-1,-1,-1,0,1,1,0,0},{0,1,1,0,-1,-1,-1,0},{-1,0,0,1,0,0,0,-1},{-1,0,0,0,0,0,-1,0},{1,0,0,-1,-1,-1,0,0},{0,0,0,0,-1,-1,0,-1},{0,-1,-1,-1,0,0,0,0},{0,-1,-1,0,0,0,1,-1}}
A = gradingGuess(PR)^{4,5,3,1};
X = toricVariety(V,PR,A);
dataBase#"4:83:r" = toString rays X
dataBase#"4:83:L" = toString nonfaces X
dataBase#"4:83:A" = toString degrees X

-- 4:84 ---- (Bat Q_1): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,1,-1,0},{0,1,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,1,0,-1,0,-1,0,0,0},{0,1,0,0,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:84:r" = toString rays X
dataBase#"4:84:L" = toString nonfaces X
dataBase#"4:84:A" = toString degrees X

-- 4:85 ---- (Bat Q_2): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,1,-1,0},{0,0,1,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,1,0,-1,0,-1,0,0,0},{0,0,0,1,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:85:r" = toString rays X
dataBase#"4:85:L" = toString nonfaces X
dataBase#"4:85:A" = toString degrees X

-- 4:86 ---- (Bat Q_3): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,0,-1,0},{1,0,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{1,0,0,-1,0,-1,0,0,0},{1,0,0,0,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:86:r" = toString rays X
dataBase#"4:86:L" = toString nonfaces X
dataBase#"4:86:A" = toString degrees X

-- 4:87 ---- (Bat Q_4): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,1,-1,0},{1,0,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,1,0,-1,0,-1,0,0,0},{1,0,0,0,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:87:r" = toString rays X
dataBase#"4:87:L" = toString nonfaces X
dataBase#"4:87:A" = toString degrees X

-- 4:88 ---- (Bat Q_5): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,0,-1,0},{0,0,1,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{1,0,0,-1,0,-1,0,0,0},{0,0,0,1,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:88:r" = toString rays X
dataBase#"4:88:L" = toString nonfaces X
dataBase#"4:88:A" = toString degrees X

-- 4:89 ---- (Bat Q_6): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,0},{0,1,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,0,0,-1,0,-1,0,0,0},{0,1,0,0,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:89:r" = toString rays X
dataBase#"4:89:L" = toString nonfaces X
dataBase#"4:89:A" = toString degrees X

-- 4:90 ---- (Bat Q_7): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,0,-1,0},{-1,1,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{1,0,0,-1,0,-1,0,0,0},{0,0,0,0,-1,0,-1,1,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:90:r" = toString rays X
dataBase#"4:90:L" = toString nonfaces X
dataBase#"4:90:A" = toString degrees X

-- 4:91 ---- (Bat Q_8): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,0},{1,0,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,0,0,-1,0,-1,0,0,0},{1,0,0,0,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:91:r" = toString rays X
dataBase#"4:91:L" = toString nonfaces X
dataBase#"4:91:A" = toString degrees X

-- 4:92 ---- (Bat Q_9): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,0,-1,0},{1,-1,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{1,0,0,-1,0,-1,0,0,0},{0,0,1,0,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:92:r" = toString rays X
dataBase#"4:92:L" = toString nonfaces X
dataBase#"4:92:A" = toString degrees X

-- 4:93 ---- (Bat Q_10): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,0},{0,0,1,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,0,0,-1,0,-1,0,0,0},{0,0,0,1,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:93:r" = toString rays X
dataBase#"4:93:L" = toString nonfaces X
dataBase#"4:93:A" = toString degrees X

-- 4:94 ---- (Bat Q_11): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,0},{0,0,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,0,0,-1,0,-1,0,0,0},{0,0,0,0,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:94:r" = toString rays X
dataBase#"4:94:L" = toString nonfaces X
dataBase#"4:94:A" = toString degrees X

-- 4:95 ---- (Bat Q_12): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,1,-1,0},{1,-1,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,1,0,-1,0,-1,0,0,0},{0,0,1,0,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:95:r" = toString rays X
dataBase#"4:95:L" = toString nonfaces X
dataBase#"4:95:A" = toString degrees X

-- 4:96 ---- (Bat Q_13): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,-1,-1,0},{1,-1,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,0,1,-1,0,-1,0,0,0},{0,0,1,0,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:96:r" = toString rays X
dataBase#"4:96:L" = toString nonfaces X
dataBase#"4:96:A" = toString degrees X

-- 4:97 ---- (Bat Q_14): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,-1,-1,0},{0,0,1,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,0,1,-1,0,-1,0,0,0},{0,0,0,1,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:97:r" = toString rays X
dataBase#"4:97:L" = toString nonfaces X
dataBase#"4:97:A" = toString degrees X

-- 4:98 ---- (Bat Q_15): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,0},{1,-1,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,0,0,-1,0,-1,0,0,0},{0,0,1,0,-1,0,-1,0,0}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:98:r" = toString rays X
dataBase#"4:98:L" = toString nonfaces X
dataBase#"4:98:A" = toString degrees X

-- 4:99 ---- (Bat Q_16): 
V = {{1, 0, 0, 0}, {0, 1, 0, 0}, {1, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}, {1, 0, -1, 0}, {-1, 0, 0, -1}, {-1, 1, 0, 0}, {-1, 0, 0, 0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{1,0,0,-1,0,-1,0,0,0},{0,0,0,0,-1,0,-1,0,1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:99:r" = toString rays X
dataBase#"4:99:L" = toString nonfaces X
dataBase#"4:99:A" = toString degrees X

-- 4:100 ---- (Bat Q_17): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,-1,-1,0},{-1,0,0,-1},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{1,-1,-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,0,-1,0},{0,0,1,-1,0,-1,0,0,0},{0,0,0,0,-1,0,-1,0,1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:100:r" = toString rays X
dataBase#"4:100:L" = toString nonfaces X
dataBase#"4:100:A" = toString degrees X

-- 4:101 ---- (Bat K_1): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{2,0,-1,-1},{0,-1,0,0},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,1,0,-1},{0,0,0,0,0,0,-1,-1,1},{0,-1,0,0,0,0,-1,0,0},{-1,0,1,0,0,0,-1,0,0},{1,-1,-1,0,0,0,0,0,0},{0,0,-1,0,0,0,0,-1,0},{2,0,0,-1,-1,-1,0,0,0}}
PR * matrix V          
A = (gradingGuess(PR))^{0,1,4,5,6}
X = toricVariety(V,PR,A);
dataBase#"4:101:r" = toString rays X
dataBase#"4:101:L" = toString nonfaces X
dataBase#"4:101:A" = toString degrees X

-- 4:102 ---- (Bat K_2): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,1,-1,-1},{0,-1,0,0},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,1,0,-1},{0,0,0,0,0,0,-1,-1,1},{0,-1,0,0,0,0,-1,0,0},{-1,0,1,0,0,0,-1,0,0},{1,-1,-1,0,0,0,0,0,0},{0,0,-1,0,0,0,0,-1,0},{1,1,0,-1,-1,-1,0,0,0}}
A = (gradingGuess(PR))^{0,1,4,5,6};
X = toricVariety(V,PR,A);
dataBase#"4:102:r" = toString rays X
dataBase#"4:102:L" = toString nonfaces X
dataBase#"4:102:A" = toString degrees X

-- 4:103 ---- (Bat K_3): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{1,0,-1,-1},{0,-1,0,0},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,1,0,-1},{0,0,0,0,0,0,-1,-1,1},{0,-1,0,0,0,0,-1,0,0},{-1,0,1,0,0,0,-1,0,0},{1,-1,-1,0,0,0,0,0,0},{0,0,-1,0,0,0,0,-1,0},{1,0,0,-1,-1,-1,0,0,0}}
A = (gradingGuess(PR))^{0,1,4,5,6};
X = toricVariety(V,PR,A);
dataBase#"4:103:r" = toString rays X
dataBase#"4:103:L" = toString nonfaces X
dataBase#"4:103:A" = toString degrees X

-- 4:104 ---- (Bat K_4): 
V = {{1,0,0,0},{0,1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,-1},{0,-1,0,0},{-1,1,0,0},{-1,0,0,0}};
PR = matrix{{-1,0,0,0,0,0,0,0,-1},{-1,1,0,0,0,0,0,-1,0},{0,-1,0,0,0,0,0,1,-1},{0,0,-1,0,0,0,1,0,-1},{0,0,0,0,0,0,-1,-1,1},{0,-1,0,0,0,0,-1,0,0},{-1,0,1,0,0,0,-1,0,0},{1,-1,-1,0,0,0,0,0,0},{0,0,-1,0,0,0,0,-1,0},{0,0,0,-1,-1,-1,0,0,0}}
A = (gradingGuess(PR))^{0,1,4,5,6}
X = toricVariety(V,PR,A);
dataBase#"4:104:r" = toString rays X
dataBase#"4:104:L" = toString nonfaces X
dataBase#"4:104:A" = toString degrees X

-- 4:105 ---- (Bat R_1): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,1,-1},{0,0,1,0},{0,0,0,1},{0,0,-1,1},{1,0,0,-1},{-1,0,0,1},{-1,0,0,0}};
PR = matrix{{1,0,0,0,-1,0,-1,0,0},{-1,-1,-1,1,0,0,1,0,0},{0,1,1,-1,0,0,-1,0,-1},{-1,0,0,0,1,0,0,-1,0},{0,0,0,0,-1,0,0,1,-1},{-1,0,0,0,0,0,0,0,-1},{0,0,0,0,0,0,-1,-1,0},{0,-1,-1,1,0,0,0,-1,1},{0,-1,-1,1,-1,0,0,0,0},{0,0,0,-1,1,-1,0,0,0},{0,-1,-1,0,0,-1,0,0,0}}
A = (gradingGuess(PR))^{0,1,4,5,6};
X = toricVariety(V,PR,A);
dataBase#"4:105:r" = toString rays X
dataBase#"4:105:L" = toString nonfaces X
dataBase#"4:105:A" = toString degrees X

-- 4:106 ---- (Bat R_2): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,1,-1},{0,0,1,0},{0,0,0,1},{1,0,-1,0},{1,0,0,-1},{-1,0,0,1},{-1,0,0,0}};
PR = matrix{{1,0,0,0,-1,0,-1,0,0},{-1,-1,-1,1,0,0,1,0,0},{0,1,1,-1,0,0,-1,0,-1},{-1,0,0,0,1,0,0,-1,0},{0,0,0,0,-1,0,0,1,-1},{-1,0,0,0,0,0,0,0,-1},{0,0,0,0,0,0,-1,-1,0},{0,-1,-1,1,0,0,0,-1,1},{0,-1,-1,1,-1,0,0,0,0},{1,0,0,-1,0,-1,0,0,0},{0,-1,-1,0,0,-1,1,0,0}}
A = (gradingGuess(PR))^{0,1,4,5,6};
X = toricVariety(V,PR,A);
dataBase#"4:106:r" = toString rays X
dataBase#"4:106:L" = toString nonfaces X
dataBase#"4:106:A" = toString degrees X

-- 4:107 ---- (Bat R_3): 
V = {{1,0,0,0},{0,1,0,0},{0,-1,1,-1},{0,0,1,0},{0,0,0,1},{0,0,-1,0},{1,0,0,-1},{-1,0,0,1},{-1,0,0,0}};
PR = matrix{{1,0,0,0,-1,0,-1,0,0},{-1,-1,-1,1,0,0,1,0,0},{0,1,1,-1,0,0,-1,0,-1},{-1,0,0,0,1,0,0,-1,0},{0,0,0,0,-1,0,0,1,-1},{-1,0,0,0,0,0,0,0,-1},{0,0,0,0,0,0,-1,-1,0},{0,-1,-1,1,0,0,0,-1,1},{0,-1,-1,1,-1,0,0,0,0},{0,0,0,-1,0,-1,0,0,0},{0,-1,-1,0,0,-1,1,0,1}}
A = (gradingGuess(PR))^{0,1,2,5,6};
X = toricVariety(V,PR,A);
dataBase#"4:107:r" = toString rays X
dataBase#"4:107:L" = toString nonfaces X
dataBase#"4:107:A" = toString degrees X

-- 4:108 ---- (Bat Prop 3.4.1): 
V = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,-1,-1,0},{1,-1,-1,-1},{0,0,0,-1},{-1,0,0,0},{-1,1,0,0}};
PR = matrix{{1,-1,-1,0,0,-1,1,0,0},{-1,0,0,0,0,0,0,-1,0},{-1,1,0,0,0,0,0,0,-1},{0,0,0,-1,1,-1,0,0,0},{0,0,0,0,-1,1,-1,0,0},{0,-1,0,0,0,0,0,-1,1},{0,0,0,-1,0,0,-1,0,0},{1,-1,-1,0,-1,0,0,0,0},{0,0,-1,0,0,-1,1,0,-1},{0,0,-1,0,-1,0,0,0,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:108:r" = toString rays X
dataBase#"4:108:L" = toString nonfaces X
dataBase#"4:108:A" = toString degrees X

-- 4:109 ---- (Bat U_1): 
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{1,0,0,-1}};
PR = matrix{{0,0,-1,1,-1,0,0,0,0,0},{-1,1,-1,0,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0},{0,0,0,-1,1,-1,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0},{-1,0,0,0,-1,1,0,0,0,0},{1,-1,0,0,0,-1,0,0,0,0},{0,0,-1,0,0,-1,0,0,0,0},{1,0,0,0,0,0,-1,-1,0,0},{1,0,0,0,0,0,0,0,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,4,6,7}
X = toricVariety(V,PR,A);
dataBase#"4:109:r" = toString rays X
dataBase#"4:109:L" = toString nonfaces X
dataBase#"4:109:A" = toString degrees X

-- 4:110 ---- (Bat U_2): 
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{1,0,-1,-1}};
PR = matrix{{0,0,-1,1,-1,0,0,0,0,0},{-1,1,-1,0,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0},{0,0,0,-1,1,-1,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0},{-1,0,0,0,-1,1,0,0,0,0},{1,-1,0,0,0,-1,0,0,0,0},{0,0,-1,0,0,-1,0,0,0,0},{1,0,0,0,0,0,-1,-1,0,0},{0,0,0,0,0,0,0,1,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,4,6,7};
X = toricVariety(V,PR,A);
dataBase#"4:110:r" = toString rays X
dataBase#"4:110:L" = toString nonfaces X
dataBase#"4:110:A" = toString degrees X

-- 4:111 ---- (Bat U_3): 
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{0,1,0,-1}};
PR = matrix{{0,0,-1,1,-1,0,0,0,0,0},{-1,1,-1,0,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0},{0,0,0,-1,1,-1,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0},{-1,0,0,0,-1,1,0,0,0,0},{1,-1,0,0,0,-1,0,0,0,0},{0,0,-1,0,0,-1,0,0,0,0},{1,0,0,0,0,0,-1,-1,0,0},{0,1,0,0,0,0,0,0,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,4,6,7};
X = toricVariety(V,PR,A);
dataBase#"4:111:r" = toString rays X
dataBase#"4:111:L" = toString nonfaces X
dataBase#"4:111:A" = toString degrees X

-- 4:112 ---- (Bat U_4): 
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,-1,-1}};
PR = matrix{{0,0,-1,1,-1,0,0,0,0,0},{-1,1,-1,0,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0},{0,0,0,-1,1,-1,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0},{-1,0,0,0,-1,1,0,0,0,0},{1,-1,0,0,0,-1,0,0,0,0},{0,0,-1,0,0,-1,0,0,0,0},{0,0,0,0,0,0,-1,-1,0,0},{0,0,0,0,0,0,0,1,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,4,6,7};
X = toricVariety(V,PR,A);
dataBase#"4:112:r" = toString rays X
dataBase#"4:112:L" = toString nonfaces X
dataBase#"4:112:A" = toString degrees X

-- 4:113 ---- (Bat U_5): 
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{0,0,-1,1,-1,0,0,0,0,0},{-1,1,-1,0,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0},{0,0,0,-1,1,-1,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0},{-1,0,0,0,-1,1,0,0,0,0},{1,-1,0,0,0,-1,0,0,0,0},{0,0,-1,0,0,-1,0,0,0,0},{0,0,0,0,0,0,-1,-1,0,0},{0,0,0,0,0,0,0,0,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,4,6,7};
X = toricVariety(V,PR,A);
dataBase#"4:113:r" = toString rays X
dataBase#"4:113:L" = toString nonfaces X
dataBase#"4:113:A" = toString degrees X

-- 4:114 ---- (Bat U_6): 
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{0,0,0,-1}};
PR = matrix{{0,0,-1,1,-1,0,0,0,0,0},{-1,1,-1,0,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0},{0,0,0,-1,1,-1,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0},{-1,0,0,0,-1,1,0,0,0,0},{1,-1,0,0,0,-1,0,0,0,0},{0,0,-1,0,0,-1,0,0,0,0},{1,0,0,0,0,0,-1,-1,0,0},{0,0,0,0,0,0,0,0,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,4,6,7};
X = toricVariety(V,PR,A);
dataBase#"4:114:r" = toString rays X
dataBase#"4:114:L" = toString nonfaces X
dataBase#"4:114:A" = toString degrees X

-- 4:115 ---- (Bat U_7): 
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{-1,1,0,-1}};
PR = matrix{{0,0,-1,1,-1,0,0,0,0,0},{-1,1,-1,0,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0},{0,0,0,-1,1,-1,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0},{-1,0,0,0,-1,1,0,0,0,0},{1,-1,0,0,0,-1,0,0,0,0},{0,0,-1,0,0,-1,0,0,0,0},{1,0,0,0,0,0,-1,-1,0,0},{0,0,1,0,0,0,0,0,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,4,6,7};
X = toricVariety(V,PR,A);
dataBase#"4:115:r" = toString rays X
dataBase#"4:115:L" = toString nonfaces X
dataBase#"4:115:A" = toString degrees X

-- 4:116 ---- (Bat U_8): 
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{1,-1,0,0},{0,0,1,0},{1,0,-1,0},{0,0,0,1},{-1,0,0,-1}};
PR = matrix{{0,0,-1,1,-1,0,0,0,0,0},{-1,1,-1,0,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0},{0,0,0,-1,1,-1,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0},{-1,0,0,0,-1,1,0,0,0,0},{1,-1,0,0,0,-1,0,0,0,0},{0,0,-1,0,0,-1,0,0,0,0},{1,0,0,0,0,0,-1,-1,0,0},{0,0,0,1,0,0,0,0,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,4,6,7};
X = toricVariety(V,PR,A);
dataBase#"4:116:r" = toString rays X
dataBase#"4:116:L" = toString nonfaces X
dataBase#"4:116:A" = toString degrees X

-- 4:117 ---- (Bat Prop 3.5.8ii): 
V = {{1,0,0,0},{-1,0,0,0},{0,1,0,0},{0,-1,0,0},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,0,-1},{1,1,1,1},{-1,-1,-1,-1}};
PR = matrix{{-1,-1,0,0,0,0,0,0,0,0},{0,0,-1,-1,0,0,0,0,0,0},{-1,0,-1,0,-1,0,0,1,1,0},{0,-1,0,-1,0,-1,1,0,0,1},{0,0,0,0,-1,-1,0,0,0,0},{-1,0,-1,0,0,1,-1,0,1,0},{-1,0,0,1,-1,0,-1,0,1,0},{0,1,-1,0,-1,0,-1,0,1,0},{0,-1,0,-1,1,0,0,-1,0,1},{0,-1,1,0,0,-1,0,-1,0,1},{1,0,0,-1,0,-1,0,-1,0,1},{0,0,0,0,0,0,-1,-1,0,0},{0,-1,0,-1,1,0,1,0,-1,0},{0,-1,1,0,0,-1,1,0,-1,0},{1,0,0,-1,0,-1,1,0,-1,0},{0,-1,1,0,1,0,0,-1,-1,0},{1,0,0,-1,1,0,0,-1,-1,0},{1,0,1,0,0,-1,0,-1,-1,0},{-1,0,-1,0,0,1,0,1,0,-1},{-1,0,0,1,-1,0,0,1,0,-1},{0,1,-1,0,-1,0,0,1,0,-1},{-1,0,0,1,0,1,-1,0,0,-1},{0,1,-1,0,0,1,-1,0,0,-1},{0,1,0,1,-1,0,-1,0,0,-1},{0,0,0,0,0,0,0,0,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,4,6,10};
X = toricVariety(V,PR,A);
dataBase#"4:117:r" = toString rays X
dataBase#"4:117:L" = toString nonfaces X
dataBase#"4:117:A" = toString degrees X

-- 4:118 -- (Bat Prop 3.5.8iii): 
V = {{1,0,0,0},{-1,0,0,0},{0,1,0,0},{0,-1,0,0},{0,0,1,0},{0,0,-1,0},{0,0,0,1},{0,0,0,-1},{1,1,1,1}};
PR = matrix{{-1,-1,0,0,0,0,0,0,0},{0,0,-1,-1,0,0,0,0,0},{-1,0,-1,0,-1,0,0,1,1},{0,0,0,0,-1,-1,0,0,0},{-1,0,-1,0,0,1,-1,0,1},{-1,0,0,1,-1,0,-1,0,1},{0,1,-1,0,-1,0,-1,0,1},{0,0,0,0,0,0,-1,-1,0},{0,-1,0,-1,1,0,1,0,-1},{0,-1,1,0,0,-1,1,0,-1},{1,0,0,-1,0,-1,1,0,-1},{0,-1,1,0,1,0,0,-1,-1},{1,0,0,-1,1,0,0,-1,-1},{1,0,1,0,0,-1,0,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,3,6};
X = toricVariety(V,PR,A);
dataBase#"4:118:r" = toString rays X
dataBase#"4:118:L" = toString nonfaces X
dataBase#"4:118:A" = toString degrees X

-- 4:119 ---- (Bat ): S_2 * S_2 
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,1},{0,0,-1,0},{0,0,0,-1}};
PR = matrix{{-1,1,-1,0,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0},{0,0,-1,1,-1,0,0,0,0,0},{0,0,0,0,0,-1,1,-1,0,0},{0,0,0,0,0,-1,0,0,-1,0},{0,0,0,0,0,0,-1,1,-1,0},{0,0,0,0,0,0,-1,0,0,-1},{0,0,0,0,0,0,0,-1,1,-1}}
A = gradingGuess(PR);
X = toricVariety(V,PR,A);
dataBase#"4:119:r" = toString rays X
dataBase#"4:119:L" = toString nonfaces X
dataBase#"4:119:A" = toString degrees X

-- 4:120 ---- (Bat ): S_2 * S_3
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,1},{0,0,-1,0},{0,0,0,-1},{0,0,1,-1}};
PR = matrix{{-1,1,-1,0,0,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0,0},{0,0,-1,1,-1,0,0,0,0,0,0},{0,0,0,0,0,-1,1,-1,0,0,0},{0,0,0,0,0,-1,0,0,-1,0,0},{0,0,0,0,0,0,-1,1,-1,0,0},{0,0,0,0,0,-1,0,0,0,-1,1},{0,0,0,0,0,0,-1,0,0,-1,0},{0,0,0,0,0,0,0,-1,1,-1,0},{0,0,0,0,0,1,-1,0,0,0,-1},{0,0,0,0,0,0,0,-1,0,0,-1},{0,0,0,0,0,0,0,0,-1,1,-1}}
A = (gradingGuess(PR))^{0,1,2,3,4,5,6};
X = toricVariety(V,PR,A);
dataBase#"4:120:r" = toString rays X
dataBase#"4:120:L" = toString nonfaces X
dataBase#"4:120:A" = toString degrees X

-- 4:121 ---- (Bat ): S_3 * S_3
V = {{1,0,0,0},{0,1,0,0},{-1,1,0,0},{-1,0,0,0},{0,-1,0,0},{1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,1},{0,0,-1,0},{0,0,0,-1},{0,0,1,-1}};
PR = matrix{{-1,1,-1,0,0,0,0,0,0,0,0,0},{-1,0,0,-1,0,0,0,0,0,0,0,0},{0,-1,1,-1,0,0,0,0,0,0,0,0},{-1,0,0,0,-1,1,0,0,0,0,0,0},{0,-1,0,0,-1,0,0,0,0,0,0,0},{0,0,-1,1,-1,0,0,0,0,0,0,0},{1,-1,0,0,0,-1,0,0,0,0,0,0},{0,0,-1,0,0,-1,0,0,0,0,0,0},{0,0,0,-1,1,-1,0,0,0,0,0,0},{0,0,0,0,0,0,-1,1,-1,0,0,0},{0,0,0,0,0,0,-1,0,0,-1,0,0},{0,0,0,0,0,0,0,-1,1,-1,0,0},{0,0,0,0,0,0,-1,0,0,0,-1,1},{0,0,0,0,0,0,0,-1,0,0,-1,0},{0,0,0,0,0,0,0,0,-1,1,-1,0},{0,0,0,0,0,0,1,-1,0,0,0,-1},{0,0,0,0,0,0,0,0,-1,0,0,-1},{0,0,0,0,0,0,0,0,0,-1,1,-1}}
A = (gradingGuess(PR))^{0,1,2,3,6,7,8,9};
X = toricVariety(V,PR,A);
dataBase#"4:121:r" = toString rays X
dataBase#"4:121:L" = toString nonfaces X
dataBase#"4:121:A" = toString degrees X

-- 4:122 ---- (Bat Z_1): 
V = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{-1,-1,0,0},{0,-1,-1,-1},{1,0,-1,-1},{-1,0,0,1}};
PR = matrix{{-1,-1,0,0,-1,0,0,0},{-1,-1,0,0,0,-1,1,0},{0,-1,0,-1,-1,0,0,1},{0,-1,0,-1,0,-1,1,1},{0,0,-1,0,0,0,-1,-1},{1,0,-1,-1,1,-1,0,0},{-1,0,0,1,0,0,0,-1},{0,0,0,0,-1,1,-1,0},{1,0,-1,-1,0,0,-1,0},{0,0,-1,0,1,-1,0,-1}}
A = gradingGuess(PR)
rank A
X = toricVariety(V,PR,A);
dataBase#"4:122:r" = toString rays X
dataBase#"4:122:L" = toString nonfaces X
dataBase#"4:122:A" = toString degrees X

-- 4:123 ---- (Bat Z_2): 
V = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{-1,-1,0,0},{0,0,-1,-1},{1,1,-1,-1},{-1,0,0,1}};
PR = matrix{{-1,-1,0,0,-1,0,0,0},{-1,-1,0,0,0,-1,1,0},{0,-1,0,-1,-1,0,0,1},{0,-1,0,-1,0,-1,1,1},{-1,0,0,1,0,0,0,-1},{0,0,0,0,-1,1,-1,0},{0,1,-1,0,0,0,-1,-1},{0,0,-1,-1,0,-1,0,0},{1,1,-1,-1,0,0,-1,0},{0,1,-1,0,1,-1,0,-1}}
A = (gradingGuess(PR))^{0,1,2,3,4};
X = toricVariety(V,PR,A);
dataBase#"4:123:r" = toString rays X
dataBase#"4:123:L" = toString nonfaces X
dataBase#"4:123:A" = toString degrees X

-- 4:124 ---- missing from (Bat)
V = {{1,0,0,0},{0,1,0,0},{-1,-1,0,0},{0,0,1,0},{0,0,0,1},{0,0,-1,-1},{1,0,1,0},{0,1,0,1},{-1,-1,-1,-1}};
PR = matrix{{-1,-1,-1,0,0,0,0,0,0},{-1,0,0,-1,0,0,1,0,0},{0,-1,0,0,-1,0,0,1,0},{0,0,-1,0,0,-1,0,0,1},{0,0,0,-1,-1,-1,0,0,0},{0,-1,-1,1,0,0,-1,0,0},{1,0,0,0,-1,-1,-1,0,0},{-1,0,-1,0,1,0,0,-1,0},{0,1,0,-1,0,-1,0,-1,0},{0,0,-1,1,1,0,-1,-1,0},{1,1,0,0,0,-1,-1,-1,0},{-1,-1,0,0,0,1,0,0,-1},{0,0,1,-1,-1,0,0,0,-1},{0,-1,0,1,0,1,-1,0,-1},{1,0,1,0,-1,0,-1,0,-1},{-1,0,0,0,1,1,0,-1,-1},{0,1,1,-1,0,0,0,-1,-1},{0,0,0,0,0,0,-1,-1,-1}}
A = (gradingGuess(PR))^{0,1,2,4,6};
X = toricVariety(V,PR,A);
dataBase#"4:124:r" = toString rays X
dataBase#"4:124:L" = toString nonfaces X
dataBase#"4:124:A" = toString degrees X


close dataBase;
end
------------------------------------------------------------
restart
load "smoothFanoToricVarieties.m2"
loadPackage "ToricQuivers";

filename = homeDirectory | "Code/smoothFanoToricVarieties.dbm";
dataBase = openDatabase filename;
smoothFano = (d,l) -> (
     s1 := toString d | ":" | toString l | ":r";
     s2 := toString d | ":" | toString l | ":L";  
     s3 := toString d | ":" | toString l | ":A";
     sr := dataBase#s1;
     sL := dataBase#s2;
     sA := dataBase#s3;
     toricVariety(value sr, value sL, transpose matrix value sA));
all(1..5, i -> isSimplicial smoothFano(2,i))
all(1..5, i -> isSmooth smoothFano(2,i))
all(1..5, i -> isFano smoothFano(2,i))
all(1..18, i -> isSimplicial smoothFano(3,i))
all(1..18, i -> isSmooth smoothFano(3,i))
all(1..18, i -> isFano smoothFano(3,i))
all(1..124, i -> isSimplicial smoothFano(4,i))
all(1..124, i -> isSmooth smoothFano(4,i))
all(1..124, i -> isFano smoothFano(4,i))

apply(1..5, i -> (
	  X := smoothFano(2,i);
	  rank source basis(sum degrees X, ring X)))
apply(1..18, i -> (
	  X := smoothFano(3,i);
	  rank source basis(sum degrees X, ring X)))
apply(1..124, i -> (
	  X := smoothFano(4,i);
	  rank source basis(sum degrees X, ring X)))


--------------------------------------------------------------------------------
restart
needsPackage "FourierMotzkin";
needsPackage "NormalToricVarieties";
ff := currentFileDirectory | "NormalToricVarieties/dim5.txt"
i = -1;
getFano := memoize( () -> (
    if notify then stderr << "--loading file " << ff << endl;
    hashTable apply( lines get ff, x -> (
	x = value x;
	i = i +1;
	((5,i), x)))))

fout = currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties5.txt"
openOut fout
Y = projectiveSpace 5;
fout << 5 << "," << 0 << "," << rays Y << "," << max Y << endl;
for i from 1 to 865 do (
  L := (getFano())#(5,i);
  M := matrix {toList(#L:1)} || transpose matrix L;
  d := numRows M;
  X := normalToricVariety ((fourierMotzkin M)#0)^{1..d-1};
  fout << 5 << "," << i << "," << rays X << "," << max X << endl)
fout << close

L = (getFano())#(6,1)
M = matrix {toList(#L:1)} || transpose matrix L;
d = numRows M;
X = normalToricVariety ((fourierMotzkin M)#0)^{1..d-1};
rays X
max X
Y = projectiveSpace 5;
rays Y


ff5 := currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties5.txt"
getFano = memoize( () -> (
    if notify then stderr << "--loading file " << ff5 << endl;
    hashTable apply( lines get ff5, x -> (
	x = value x;
	((x#0,x#1),drop(x,2))))))

for i to 865 do (
  s := (getFano())#(5,i);
  X := normalToricVariety(s#0,s#1);
  << i << " : " << isSmooth X and isFano X << endl)
  
  
--------------------------------------------------------------------------------
restart
needsPackage "FourierMotzkin";
needsPackage "NormalToricVarieties";
  
ff := currentFileDirectory | "NormalToricVarieties/dim6.txt"
i = -1;
getFano := memoize( () -> (
    if notify then stderr << "--loading file " << ff << endl;
    hashTable apply( lines get ff, x -> (
	x = value x;
	i = i +1;
	((6,i), x)))))

fout = currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties6.txt"
openOut fout
Y = projectiveSpace 6;
fout << 6 << "," << 0 << "," << rays Y << "," << max Y << endl;
for i from 1 to 7621 do (
  L := (getFano())#(6,i);
  M := matrix {toList(#L:1)} || transpose matrix L;
  d := numRows M;
  X := normalToricVariety ((fourierMotzkin M)#0)^{1..d-1};
  fout << 6 << "," << i << "," << rays X << "," << max X << endl)
fout << close


ff6 := currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties6.txt"
getFano = memoize( () -> (
    if notify then stderr << "--loading file " << ff6 << endl;
    hashTable apply( lines get ff6, x -> (
	x = value x;
	((x#0,x#1),drop(x,2))))))

for i to 7621 do (
  s := (getFano())#(6,i);
  X := normalToricVariety(s#0,s#1);
  << i << " : " << isSmooth X and isFano X << endl)


--------------------------------------------------------------------------------
restart
uninstallPackage "NormalToricVarieties";
installPackage "NormalToricVarieties";
check "NormalToricVarieties"

for i to 43 do (
  j := random(20);
  X := smoothFanoToricVariety(5,10*i+j);
  assert(isSmooth X and isFano X))


for i to 37 do (
  j := random(200);
  X := smoothFanoToricVariety(6,100*i+j);
  << 100*i +j << endl;
  assert(isSmooth X and isFano X))
