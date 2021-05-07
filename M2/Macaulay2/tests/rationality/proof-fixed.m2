-- check if all prec*(prec-1) matrices in the proof
-- of Proposition 5.3 in 
--
-- "Rationality of the moduli spaces of plane curves
--  of sufficiently large degree"
--
-- by Christian Boehning and Hans-Christian Graf v. Bothmer
--
-- have full rank.


---------------------------------------------
-- the independence calculation for Scorza --
---------------------------------------------

-- restart

-- the precision
prec = 11
Fprec = ZZ/prec

-- calculate in ZZ/prec^3
RC = ZZ[x_1,x_2,x_3,C,D]/ideal(D^prec,prec^3*D^0)

-- define the covariants & some useful functions
load "ScorzaOcta-fixed.m2"

-- a random point
g = {(1,matrix {{  7*x_1+3*x_2 +9*x_3}}), 
     (1,matrix {{-10*x_1+  x_2 +4*x_3}}), 
     (1,matrix {{  8*x_1+4*x_2 +6*x_3}}), 
     (1,matrix {{    x_1+6*x_2-10*x_3}}), 
     (1,matrix {{  4*x_1-8*x_2-10*x_3}}), 
     (1,matrix {{ -3*x_1+7*x_2 -4*x_3}}), 
     (1,matrix {{       -3*x_2 +2*x_3}}), 
     (1,matrix {{  8*x_1-4*x_2 -4*x_3}}), 
     (1,matrix {{-10*x_1+4*x_2 +6*x_3}})}

-- the generic linear forms
CD1 = (C*x_1+D*x_2)
CD2 = (C*x_1+D*x_3)

-- calculate the ranks for one period
time tally apply(prec..prec^2,n->(
	  M1 := ScorzaR(n,CD1,g,Fprec);
	  M2 := ScorzaR(n,CD2,g,Fprec);
	  M = M1|M2;
	  r = rank M;
	  print (n,r);
	  r
	  ))
-- used 164.56 seconds
assert( oo === tally(111 : 15) ) -- Tally{15 => 111}
-- all do have full rank!

-------------------------------------------
-- the independence calculation for Octa --
-------------------------------------------

clearAll -- restart

-- the precision
prec = 19
Fprec = ZZ/prec

-- calculate in ZZ/prec^3
RC = ZZ[x_1,x_2,x_3,C,D]/ideal(D^prec,prec^3*D^0)

-- define the covariants and some useful functions
load "ScorzaOcta-fixed.m2"

-- a random point
--const = 9; g = apply(const,i->(1,matrix{{x_1,x_2,x_3}}*random(ZZ^3,ZZ^1)))
g = {(1,matrix {{  7*x_1+3*x_2 +9*x_3}}), 
     (1,matrix {{-10*x_1+  x_2 +4*x_3}}), 
     (1,matrix {{  8*x_1+4*x_2 +6*x_3}}), 
     (1,matrix {{    x_1+6*x_2-10*x_3}}), 
     (1,matrix {{  4*x_1-8*x_2-10*x_3}}), 
     (1,matrix {{ -3*x_1+7*x_2 -4*x_3}}), 
     (1,matrix {{       -3*x_2 +2*x_3}}), 
     (1,matrix {{  8*x_1-4*x_2 -4*x_3}}), 
     (1,matrix {{-10*x_1+4*x_2 +6*x_3}})}

-- the generic linear forms
CD1 = (C*x_1+D*x_2)
CD2 = (C*x_1+D*x_3)
CD3 = (C*x_1+D*(x_2+x_3))

-- calculate the ranks for one period
time tally apply(prec..prec^2,n->(
	  M1 := OctaR(n,CD1,g,Fprec);
	  M2 := OctaR(n,CD2,g,Fprec);
	  M3 := OctaR(n,CD3,g,Fprec);
	  M = M1|M2|M3;
	  r = rank M;
	  print (n,r);
	  r
	  ))
-- used 3081.47 seconds
assert( oo === tally( 343 : 45 ) ) -- Tally{45 => 343}
-- all do have full rank!
