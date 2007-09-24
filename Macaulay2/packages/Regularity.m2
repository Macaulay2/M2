newPackage(
   "Regularity",
   Version => "1.0",
   Date => "September 1, 2007",
   Authors => {
        {Name => "Alexandra Seceleanu", Email => "asecele2@uiuc.edu"},
        {Name => "Nathaniel Stapleton", Email => "nstaple2@math.uiuc.edu"}
        },
   Headline => "computes the Castelnuovo-Mumford regularity of a given homogeneous ideal",
   DebuggingMode => true
   )
--=========================================================================--

-- This package is based on
-- [BG1] Bermejo, Gimenez "Saturation and Castelnuovo-mumford Regularity", 
--         Journal of Algebra 303/2006
-- [BG2] Bermejo, Gimenez "Computing the Castelnuovo-Mumford Regularity of some 
--         subschemes of P^n using quotients of monomial ideals",
--         Journal of Pure and Applied Algebra 164/2001

export{mRegularity}


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
	  U = {X_(#X-1-j)}|apply(X_{0..#X-2-j},i -> random(k)*i);
	  D = {sum(U)}|D  
    	  );
     return D;
     );
	
     
-- =======================================================================================



-- NOETHER NORMALIZATION FOR HOMOGENEOUS IDEALS

randomSum = (U,V,k) -> (
     for j to #V - 1 do (
	  U = apply(U, i -> i + random(k)*V_j);
	  );
     return U;
     );


inverseSequence = (U,X) -> (
     N := {};
     for i to #X - 1 do (
	  for j to #U - 1 do (
	       if X_i == U_j then (
		    N = {X_j}|N;
		    break;
		    );
	       );
	  );
          return N;
     )



--a quicker algorithm for putting homogeneous ideals in noether position wrt x_n-d..x_n
normalize = (I,d) -> (
     R := ring I;
     k := coefficientRing R;
     X := gens R;
     di := nPosition (I,d);
     dimI = d - 1;
     algind := support (independentSets(I,Limit => 1))_0;
     f := map(R,R,reverse inverseSequence(X-set algind|algind,X));
     U := apply(algind,i->f i);
     V := apply(X-set algind,i->f i);
     g:= id_R;
     while di > 0 do (
	  g = map(R,R,V|randomSum(U,V,k));
     	  J = g f I;
	  di = dim(J+ideal(apply(algind,i->f i)));
	  );
     return g;
     );

--tells whether a homogeneous ideal is in noether postion with respect to x_n-d..x_n.
nPosition = (I,d) -> (
     R := ring I;
     X := flatten gens R;
     dimI = d - 1;
     algind := support (independentSets(I,Limit => 1))_0;
     di= dim(I + ideal(X_{((#X-1)-dimI)..(#X-1)}));
     return di;
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
    	  f = map(R,R,X_{0..n-(i+2)}|{1}|X_{n-(i)..n-1});
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

--regMonNested
-- INPUT: I= a monomial ideal in nested position
-- OUTPUT: the Castelnuovo-Mumford regularity of I

regMonNested = (I,d,dp) -> (
     R := ring I;
     k := coefficientRing R;
     X :=  flatten entries vars R;
     n := # X-1;
     p := n-dp;
     B = for q from p+1 to n list 0 ;
     u = map(R,R,X_{0..p}|B);
     T= u I; 
     l=for i from n-d+1 to p list (
	  B = for q from i+1 to n list 0;
	  J = (map(R,R,X_{0..(i-1)}|{1}|B)) I; 
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
     while not isNested(J = monomialIdeal leadTerm f I, d) do (
	  f = map(R,R,X_{0..n-d}|upTRT2(k,X,d));
	  	  );
     dp := depthHomogMon J;
     r = regMonNested (J,d,dp);
     use S;
     return r
     );

--=======================================================================================

beginDocumentation()	 

document {
     Key =>" Regularity", 
     {TT "Regularity", " is a package for computing the Castelnuovo-Mumford regularity of homogeneous ideals in a polynomial ring without having to compute a minimal free resolution of the homogeneous ideal"}  
}

document {
     Key => {mRegularity,(mRegularity,Ideal)},
     Headline => "Castelnuovo-Mumford regularity",
     Usage => " mRegularity I",
     Inputs => {"I" => Ideal => {"a homogeneous ", TO Ideal},
	  CM => Boolean =>  {"a parameter that should be set to ",TO true ," if the ideal is known to be Cohen-Macaulay"},
	  MonCurve => Boolean =>{ " parameter that should be set to true if I is the ideal of a monomial curve"}
	  },     
     Outputs =>{ "the Castelnuovo-Mumford regularity of the given ideal, if it is homogeneous, and -1 otherwise"},
     SeeAlso =>"regularity",
     "Example:",
     PARA {},
     "computing the regularity of the defining ideal of the second Veronesean of P3",
     EXAMPLE{
     "R=QQ[a,b,c,d,x_0..x_9,MonomialOrder =>  Eliminate 4];",
     "i=ideal( x_0-a*b,x_1-a*c,x_2-a*d,x_3-b*c,x_4-b*d,x_5-c*d,x_6-a^2,x_7-b^2,x_8-c^2,x_9-d^2);",
     "j=selectInSubring(1, gens gb i);",
     "I=ideal flatten entries j; -- this is the ideal of the Veronesean",
     "mRegularity I" 
     },
     PARA {"This symbol is provided by the package Regularity." }
     }
    end


--====================================================================================================================================


