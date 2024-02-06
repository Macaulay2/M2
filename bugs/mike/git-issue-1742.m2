;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

restart
needsPackage "MultiprojectiveVarieties"
X = PP_QQ^({2,1},{1,3})

restart
S = ZZ/101[a..d]
I = monomialIdeal (a^3, b^3, c^3, c*d^3)
J = monomialIdeal d
I:J
saturate(I,J)

needsPackage "NoetherianOperators"
R = QQ[x_1,x_2,x_3,x_4] 
         MM = matrix {{x_3,x_1,x_2},{x_1,x_2,x_4}} 
         P = minors(2,MM) 
         M = ideal{x_1^2,x_2^2,x_3^2,x_4^2}  
         Q = joinIdeals(P,M); 
         L1 = noetherianOperators(Q) -- A set of Noetherian operators 
         Q1 = getIdealFromNoetherianOperators(L1, P); 
         Q == Q1 
         L2 = noetherianOperators(M) -- Another set of Noetherian operators 

restart
needsPackage "MultiprojectiveVarieties"
t = gens ring PP_(ZZ/33331)^5; 
Phi = rationalMap {rationalMap {t_0,t_1,t_2},rationalMap {t_3,t_4,t_5}}; 
X = baseLocus Phi; 

restart
needsPackage "MultiprojectiveVarieties"
X = PP_QQ^({2,1},{1,3});
dim X

restart
needsPackage "MultiprojectiveVarieties"
ZZ/65521[x_0..x_4];
Psi = last graph rationalMap(projectiveVariety ideal(x_4,x_2^2-x_1*x_3,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2),Dominant=>true);
Phi = first graph Psi;

restart
needsPackage "MultiprojectiveVarieties"
Phi = inverse first graph rationalMap PP_QQ^(2,2);

needsPackage "SpecialFanoFourfolds"
x = gens ring PP_(ZZ/33331)^8;
X = specialGushelMukaiFourfold(
        ideal(x_6-x_7, x_5, x_3-x_4, x_1, x_0-x_4, x_2*x_7-x_4*x_8),
             ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8, x_0^2+x_0*x_1+x_1^2+x_0*x_2+2*x_0*x_3+x_1*x_3+x_2*x_3+x_3^2-x_0*x_4-x_1*x_4-2*x_2*x_4-x_3*x_4-2*x_4^2+x_0*x_5+x_2*x_5+x_5^2+2*x_0*x_6+x_1*x_6+2*x_2*x_6+x_3*x_6+x_5*x_6+x_6^2-3*x_4*x_7+2*x_5*x_7-x_7^2+x_1*x_8+x_3*x_8-3*x_4*x_8+2*x_5*x_8+x_6*x_8-x_7*x_8));
time toGrass X
show oo
