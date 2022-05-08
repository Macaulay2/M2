newPackage(
   "Regularity",
   Version => "1.0",
   Date => "February 8, 2009",
   Authors => {
        {Name => "Alexandra Seceleanu", Email => "asecele2@uiuc.edu"},
        {Name => "Nathaniel Stapleton", Email => "nstaple2@math.uiuc.edu"}
        },
   Headline => "Castelnuovo-Mumford regularity of a homogeneous ideal",
   Keywords => {"Commutative Algebra"},
   DebuggingMode => false
   )
--=========================================================================--

-- This package is based on
-- [BG1] Bermejo, Gimenez "Saturation and Castelnuovo-mumford Regularity", 
--         Journal of Algebra 303/2006
-- [BG2] Bermejo, Gimenez "Computing the Castelnuovo-Mumford Regularity of some 
--         subschemes of P^n using quotients of monomial ideals",
--         Journal of Pure and Applied Algebra 164/2001

export{"mRegularity", "CM", "MonCurve"}


-- ========================================================================


-- HOMOLOGICAL FUNCTIONS: DEPTH FOR HOMOGENEOUS IDEALS IN NESTED POSITION

depthHomogMon = I-> (
     X := gens ring I;
     n := # X - 1;
     g := for i to #(flatten entries mingens I)-1 list support (flatten entries mingens I)_i;
     p := index min unique flatten g;
     return n-p
     )
    



--============================================================================    
    
    
    
-- RANDOM LINEAR TRANSFORMATIONS
-- this function produces a upper triangular liniar transformation with entries in k[X]

upTRT2 = (k,X,m) -> (
     Trans := {};
     D := {};
     for j to m-1 do (
	  U := {X_(#X-1-j)}|apply(X_{0..#X-2-j},i -> random(k)*i);
	  D = {sum(U)}|D  
    	  );
     return D;
     );
	
     
-- ========================================================================================


-- NESTED MONOMIAL IDEALS

-- By definition (see[BG1] Def 3.1)a monomial ideal I is called of nested type if 
-- for any prime ideal P associated to I there exists i such that P=(X_0,...,X_i)

-- Here we use the alternate characterisation in [BG1] Prop 3.2

isNested = (I,d) -> (
     R := ring I;
     n := dim R;
     X := gens R;
     S := {};
     flag := false;
     for i to d-1 do (
    	  f := map(R,R,X_{0..n-(i+2)}|{1}|X_{n-(i)..n-1});
	  S = S|{monomialIdeal f I};
      	  if i > 0 and S_i:S_(i-1) != ideal 1_R then return false;
	  );
     g := for i to #(flatten entries mingens I)-1 list support (flatten entries mingens I)_i;
     for j to n-d-1 do (
     	  flag = false;
	  for i to #g-1 do (
	       if X_{j} == g_i then (
		    flag = true;
		    break;
		    );
	       );
	  if flag == false then return false;
	  );
     return true;
     );

--==========================================================================================


-- SATIETY FOR MONOMIAL IDEALS
-- By definition, the satiety, or saturation index, of a homogeneous ideal I
-- is the least integer s such that, for all d>=s, the degree d part of the 
-- ideal I and the degree d part of the saturation of I with respect to the 
-- maximal ideal coincide

satMon = (I,X)-> (
     m := flatten entries mingens I;
     lamda := apply(entries transpose matrix flatten apply(m,exponents),max);
     l := {};
     for i to #X -1 do l = l|{X_i^(lamda_i+1)};
     gensIstar := flatten entries mingens (monomialIdeal l:monomialIdeal I);
     gensallvars :={};
     for i to #gensIstar-1 do if support gensIstar_i == X then gensallvars = gensallvars | {gensIstar_i}; --gensallvars contains the minimal gens of I that contain all variables
     if gensallvars == {} then return 0
     else return sum lamda +1 - (min apply( gensallvars,degree))_0;
     )


--========================================================================================================

-- REGULARITY

--regMonNested
-- INPUT: I= a monomial ideal in nested position
-- OUTPUT: the Castelnuovo-Mumford regularity of I

regMonNested = (I,d,dp) -> (
     R := ring I;
     k := coefficientRing R;
     X :=  flatten entries vars R;
     n := # X-1;
     p := n-dp;
     B := for q from p+1 to n list 0 ;
     u := map(R,R,X_{0..p}|B);
     T := u I; 
     l := for i from n-d+1 to p list (
	  B = for q from i+1 to n list 0;
	  J := (map(R,R,X_{0..(i-1)}|{1}|B)) I; 
	  satMon(J,X_{0..(i-1)})
	  );
     return max({satMon(T,X_{0..p})}|l)
     )

-- regMonCurve
-- INPUT: I= a monomial ideal whose zeros set is a projective curve
-- (usually I=monomialCurveIdeal(R,S) where S is a list of exponents in a
-- parametrisation)
-- OUTPUT: the Castelnuovo-Mumford regularity of I 

regMonCurve = (I,d) -> (
    R := ring I;
    X := gens R;
    n := #X-1;
    S := coefficientRing R [X_{1..n-1}];
    f := map (S,ring I);
    I = f I;
    m := ideal (apply (X,i-> f i));
    return (max apply((entries gens gb (I:m))_0,degree))_0
    )

delta = I -> min(flatten apply(flatten entries mingens I,degree))
     
-- mRegularity
-- INPUT: I a homogeneous ideal
-- OUTPUT: the Castelnuovo-Mumford regularity of I
mRegularity = method (TypicalValue=>ZZ,Options =>{CM => false, MonCurve => false, Verbose => false})
mRegularity (Ideal):= opts -> I -> (
     if not isHomogeneous I then (
	  stderr<<"--Error: not a homogeneous ideal!";
	  return -1;
	  );
     S := ring I;
     k := coefficientRing ring I;
     X := flatten entries vars ring I;
     R := k[X];
     g := map(R, S);
     I = g I;
     n := #X-1;
     X = apply(X, i ->g i);
     d := dim I; 
     if d==0 then (
	  j := ideal (X_{0..n});
	  return delta (I:j) +1;
	  );
     if opts.MonCurve == true then return regMonCurve(I,d);
     f := id_R;
     while not isNested(J := monomialIdeal leadTerm f I, d) do (
	  f = map(R,R,X_{0..n-d}|upTRT2(k,X,d));
	  	  );
     dp := depthHomogMon J;
     r := regMonNested (J,d,dp);
     use S;
     r
     );


     
     
     
--Tests

TEST ///
R = QQ[x_0..x_5]
I2 = ideal ( x_0^2+x_5^2, x_0^2+x_0*x_3+x_4^2, x_0^2+x_0*x_5+x_2*x_5, x_0^2-x_0*x_3-x_3*x_5, x_0^2-x_3*x_4, x_0*x_3)
assert(mRegularity I2 == 4)
///,

TEST ///
R=QQ[a,b,c,d,x_0..x_9,MonomialOrder =>  Eliminate 4]
i=ideal( x_0-a*b,x_1-a*c,x_2-a*d,x_3-b*c,x_4-b*d,x_5-c*d,x_6-a^2,x_7-b^2,x_8-c^2,x_9-d^2)
j=selectInSubring(1, gens gb i)
I=ideal flatten entries j -- this is the ideal of the Veronesean,
assert(mRegularity I == 3)
///, 
     



--=======================================================================================

beginDocumentation()	 

document {
     Key => Regularity,
     Headline => "compute Castelnuovo-Mumford regularity of a homogeneous ideal", 
     PARA {TT "Regularity", " is a package for computing the Castelnuovo-Mumford regularity
     of homogeneous ideals in a polynomial ring without having to compute a minimal 
     free resolution of the homogeneous ideal"},
     PARA {"This package is based on two articles by Bermejo and Gimenez: ", TT"Saturation and Castelnuovo-mumford Regularity", ", Journal of Algebra 303/2006
     and ", TT"Computing the Castelnuovo-Mumford Regularity of some subschemes of P^n using quotients of monomial ideals", ", Journal of Pure and Applied Algebra 164/2001."}  
}

document {
     Key => {mRegularity,(mRegularity,Ideal), MonCurve},
     Headline => "compute Castelnuovo-Mumford regularity",
     Usage => " mRegularity I",
     Inputs => {"I" => Ideal => {"a homogeneous ", TO Ideal},
	  CM => Boolean =>  {"a parameter that should be set to ",TO true ," if the ideal is known to be Cohen-Macaulay"},
	  MonCurve => Boolean =>{ " parameter that should be set to true if I is the ideal of a monomial curve"}
	  },     
     Outputs =>{ "the Castelnuovo-Mumford regularity of the given ideal, if it is homogeneous, and -1 otherwise"},
     PARA {"This package is based on two articles by Bermejo and Gimenez: ", TT"Saturation and Castelnuovo-mumford Regularity", ", Journal of Algebra 303/2006
     and ", TT"Computing the Castelnuovo-Mumford Regularity of some subschemes of P^n using quotients of monomial ideals", ", Journal of Pure and Applied Algebra 164/2001."}, 
     PARA {"computing the regularity of the defining ideal of the second Veronesean of P3"},
     EXAMPLE lines ///
     R=QQ[a,b,c,d,x_0..x_9,MonomialOrder =>  Eliminate 4]
     i=ideal( x_0-a*b,x_1-a*c,x_2-a*d,x_3-b*c,x_4-b*d,x_5-c*d,x_6-a^2,x_7-b^2,x_8-c^2,x_9-d^2)
     j=selectInSubring(1, gens gb i)
     I=ideal flatten entries j -- this is the ideal of the Veronesean,
     mRegularity I 
     ///,
     PARA {"This is an example where mRegularity is faster than regularity. Regularity takes approximately 190 seconds."},
     EXAMPLE lines ///
     R = QQ[x_0..x_5]
     I1 = ideal (x_0^2*x_1+x_0*x_1*x_2-x_0*x_4^2,-x_0*x_2^2+x_0^2*x_5,x_0^2*x_2-x_0*x_1*x_4,x_0^3-x_2^3+x_0*x_1*x_3,x_0^3+x_0^2*x_1-x_1*x_2^2-x_0*x_2*x_5,x_0^3+x_2^3-x_0*x_5^2)
     benchmark "mRegularity I1"
     ///,
     PARA {"This is an example where regularity is faster than mRegularity."},
     EXAMPLE lines ///
     R = QQ[x_0..x_5]
     I2 = ideal ( x_0^2+x_5^2, x_0^2+x_0*x_3+x_4^2, x_0^2+x_0*x_5+x_2*x_5, x_0^2-x_0*x_3-x_3*x_5, x_0^2-x_3*x_4, x_0*x_3);
     benchmark " mRegularity I2"
     time regularity I2  
     ///,
     PARA {"This symbol is provided by the package Regularity." },
     SeeAlso =>"regularity"
     },




end


--====================================================================================================================================

-- EXAMPLES:

-- mRegularity is faster then regularity

R = QQ[x_0..x_5]
I1 = ideal (x_0^2*x_1+x_0*x_1*x_2-x_0*x_4^2,-x_0*x_2^2+x_0^2*x_5,x_0^2*x_2-x_0*x_1*x_4,x_0^3-x_2^3+x_0*x_1*x_3,x_0^3+x_0^2*x_1-x_1*x_2^2-x_0*x_2*x_5,x_0^3+x_2^3-x_0*x_5^2)

benchmark "mRegularity I1" --> 1.5321 sec; Singular's similar method 9 seconds
time regularity I1 -- => 190.936 sec; Singular's regularity by computing the resolution 2668.46 sec
res I1


--       1      6      26      53      58      32      6
-- R  <-- R  <-- R   <-- R   <-- R   <-- R   <-- R  <-- 0

-- 0      1      2       3       4       5       6      7


--------------------------------------------------------------------------------
--this one is faster with resolutions 

I2 = ideal ( x_0^2+x_5^2, x_0^2+x_0*x_3+x_4^2, x_0^2+x_0*x_5+x_2*x_5, x_0^2-x_0*x_3-x_3*x_5, x_0^2-x_3*x_4, x_0*x_3);
benchmark " mRegularity I2" -- => 0.070 ; S <1 sec
time regularity I2  -- => 0.012 
time regularity res I2 --=> 0.016 this gives reg(R/I)
time regularity res (R^1/I2) -- =>0.016
time res I2 -- => 0.016

--      1      6      16      19      10      2
--     S  <-- S  <-- S   <-- S   <-- S   <-- S  <-- 0
--                                                   

-- NOTE: remove benchmark when timing regularity. (resolution may be cached)
---------------------------------------------------------------------------------
-- this one is much faster with resolutions

R=QQ[x_0..x_6];
I3 = ideal (x_0*x_1*x_3+x_0^2*x_4-x_0^2*x_5, x_1*x_3^2, x_0^2-x_1*x_4, x_0^3-x_1*x_2^2-x_0*x_4*x_5, x_0^2*x_3+x_0*x_3*x_6-x_0*x_5*x_6);

benchmark " mRegularity I3" -- => 14.73, sometimes out of memory; S=60.24 
  
time regularity I3     -- => 0.07
time regularity res I3 -- => 0.056 this yields reg (R/I)
time regularity (R^1/I3)-- => 0.064
time res I3              -- => 0.064

--     5      15      19      10      2
--    R  <-- R  <-- R   <-- R   <-- R   <-- R  <-- 0
--                                               
--    0      1      2       3       4       5      6


-------------------------------------------------------------------------------
-- comparable, but faster with mRegularity

R=QQ[x_0..x_5];
I4= ideal (x_0*x_2*x_3+x_0*x_1*x_4, x_0-x_1, x_0^3+x_0^2*x_1+x_0*x_3*x_4, x_0^3+x_0^2*x_4-x_2*x_4^2, x_0^3+x_0*x_1*x_2+x_0*x_2*x_3);

time  regularity I4     -- => 0.048
time regularity res I4 --=> 0.068
time mRegularity I4  -- => 0.045

time res I4           -- => 0.64

--     1      5      13      17      10      2
--   R  <-- R  <-- R   <-- R   <-- R   <-- R  <-- 0
--                                               
--   0      1      2       3       4       5      6



--=====================================================================
-- functions for Giulio Caviglia's idea (computing regularity by slicing with hyperplanes)

-- finding a hperplae that is not contained in any of the associated primes of Ass(inI)

findHyperplane = (I,n) -> (
     R := ring I;
     d := # gens R;
     inI := monomialIdeal apply(flatten entries gens gb I,leadTerm);
     as := apply(ass inI - set {monomialIdeal gens R}, t-> flatten entries gens t); -- returns a list of lists containing gens of the Ass(in I) 
     i := 1;
     while (not i>n) do (
	  subs := apply(subsets(d,i), s->apply(s, t->R_t));   
	  l := select(1,subs, s-> not any(as, t-> isSubset(s,t)));
	  if l != {} then return sum l#0 else i = i+1;	
     	  );
    return null
          )	    	      

slice = h -> (
     R := ring h;
     X := gens R;
     d := #X;
     z := index max support h;
     S := R/h;
     f := map(R, R, (for j to z-1 list X_j) |{X_(z)-h}|(for j from z+1 to d-1 list X_j));
     g := map(coefficientRing R[X-set{X_z}],R);
     return g*f
     )    
     
protect Length
protect Alarm     
fastReg = method (TypicalValue=>ZZ,Options =>{Alarm => 5,Length=>3})     
fastReg (Ideal):= opts -> I -> (     
     while true do (
	  alarm(2);
	  try return regularity I else (
	       h :=findHyperplane(I,3);
	       I = (slice h) (I);
	       )
	  )
     )      
     
     
--  h =findHyperplane(I,3)
--  I = (slice h) (I);
--  regularity I        



--===================================================================================

-- regCM

-- INPUT: I = a Cohen-Macaulay ideal in a polynomial ring
-- OUTPUT: the Castelnupovo Mumford regularity of I

regCM = (I,d) -> (
   R :=ring I;
   I = (normalize (I,d))I; -- attention! this normalization only guarantees that you get an ideal in NN position wrt the last d variables that ACTUALLY appear in I
   X := flatten entries vars R;
   n := #X-1;
   m := ideal (X_{0..n-d});
   inI := monomialIdeal leadTerm (I);
   return (max apply((entries gens gb (inI:m))_0,degree))_0 +1
   )
