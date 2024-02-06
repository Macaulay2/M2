-- Functions needed to do the calculations in the proof 
-- of Proposition 5.3 in 
--
-- "Rationality of the moduli spaces of plane curves
--  of sufficiently large degree"
--
-- by Christian Boehning and Hans-Christian Graf v. Bothmer

-- some plausibility checks to test the functions of this
-- file can be found in "plausibility.m2"

-- the final calculation is in "proof.m2"

--------------------
-- The Covariants --
--------------------

-- Clebsch
-- L = {a,b,c,d} a list of 4 linear forms in the variables x_1,x_2,x_3
Clebsch = (L) -> (
     if #L != 4 then error "List does not contain 4 linearforms";
     use ring L#0;
     product apply(subsets(L,3),l->
     	  det diff(matrix{{x_1,x_2,x_3}},l#0||l#1||l#2)
     ))

-- Scorza
-- d = 3n+1 
-- L = {(c_0,l_0),...} List of coefficients and linear forms
-- calculates  
--   S_d(\sum c_i l_i^d)  
-- using multilinearity
Scorza = (n,L) -> (
     sum apply(subsets(L,4),CL -> (
	       c = apply(CL,cl -> cl#0);
	       l = apply(CL,cl -> cl#1);
	       24*c#0*c#1*c#2*c#3*l#0*l#1*l#2*l#3*(Clebsch(l))^n
     )))

-- Octa
-- d = 3n+2
-- L = {(c_0,l_0),...} List of coefficients and linear forms
-- calculates
--   T_d(\sum c_i l_i^d) 
-- using multilinearity
Octa = (n,L) -> (
     sum apply(subsets(L,4),CL -> (
	       c = apply(CL,cl -> cl#0);
	       l = apply(CL,cl -> cl#1);
	       24*c#0*c#1*c#2*c#3*l#0^2*l#1^2*l#2^2*l#3^2*(Clebsch(l))^n
     )))

--------------------------------------------
-- Points in the Image of L_S+g and L_T+g --
--------------------------------------------

-- the polynomial S_d(-(Cx+Dy)^d+g)
-- d = 3n+1
-- g = {(c_1,m_1)+\dots+(c_const,m_const)   
--        a list of coefficients and linear forms
-- CD = Cx+Dy    
--        a linear polynomial with x=x_1 and y=\lambda x_2 + \mu x_3
PolyScorza = (n,CD,g) -> Scorza(n,{(-1,CD)}|g)

-- the polynomial T_d(-(Cx+Dy)^d+g)
-- d = 3n+2
-- g = {(c_1,m_1)+\dots+(c_const,m_const)   
--        a list of coefficients and linear forms
-- CD = Cx+Dy    
--        a linear polynomial with x=x_1 and y=\lambda x_2 + \mu x_3
PolyOcta = (n,CD,g) -> Octa(n,{(-1,CD)}|g)

-- Coefficient Matrix of the Q_i
-- P is either S_d or T_d
-- d is either 3n+1 or 3n+2
-- e is either 4 or 8
-- The resulting matrix has
-- 	columns:  coefficients of C^iD^j
-- 	rows:	  coefficients of x_0^i x_1^j x_2^k
coeffMatrixQ = (P,e) -> (
     coeffCD := (coefficients(P,Variables => {C,D}))#1;
     contract(transpose gens (ideal(x_1,x_2,x_3))^e,transpose coeffCD)
     )
-- Warning: If calculating with precision, i.e D^prec=0 the
--          last column will correspond to the monomial C^0D^d
--          since Macaulay interprets this as 1 instead of 0

-- Coefficient Matrix of the R_i (multiplied by (prec-1)!)
-- reduced mod prec
-- d = 3n+1 or 3n+2
-- MQ the coefficient matrix of the Q_i
-- F_prec  a finite field
-- prec the precision (i.e. D^prec = 0)
coeffMatrixR = (MQ,d,Fprec) -> (
     prec = char Fprec;
     n = d//3;
     MR :=  matrix apply(rank target MQ,
	  row -> apply(prec,
	       col -> (
		    t = (d-col);
	       	    k = n-(t//3);
		    ((entries MQ)#row#col*(prec-1)!)//binomial(n,k)
		    )));
     -- testing if numbers were indeed divisible by the binomials
     Mtest := matrix{apply(rank source MR,
	       col->MR_{col}*binomial(n,n-(d-col)//3))};
     if Mtest != (prec-1)!*MQ_{0..prec-1} then error "Divisibility is not OK";
     sub(MR,Fprec)
     )     

-- Coefficient Matrix of the R_i for Scorza
ScorzaR = (n,CD,g,Fprec) -> (
     MQ := coeffMatrixQ(PolyScorza(n,CD,g),4);
     coeffMatrixR(MQ,3*n+1,Fprec)
     )
     
-- Coefficient Matrix of the R_i for Octa
OctaR = (n,CD,g,Fprec) -> (
     MQ := coeffMatrixQ(PolyOcta(n,CD,g),8);
     coeffMatrixR(MQ,3*n+2,Fprec)
     )
