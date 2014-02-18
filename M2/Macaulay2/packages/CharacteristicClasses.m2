-- -*- coding: utf-8 -*-
    

-----------------------------------------------------------------
-- Preamble
-----------------------------------------------------------------

newPackage(
	"CharacteristicClasses",
	Version => "1.0", 
    	Date => "February 18, 2014",
    	Authors => {{Name => "Christine Jost", 
		  Email => "chjo9357@mbox.su.se", 
		  HomePage => "http://people.su.se/~chjo9357/"}},
    	Headline => "Degrees of Chern classes and other characteristic classes",
    	DebuggingMode => false,
	Configuration => { 
	     "pathToBertini" => ""
	      }
    	)



---------------------------------------------------------------
-- Configuration of the Bertini path
---------------------------------------------------------------
 
-- Check the ~/.Macaulay2/init-CharacteristicClasses.m2 file for the absolute path.
bertini'path = (options CharacteristicClasses).Configuration#"pathToBertini";

if not instance(bertini'path,String) then error "expected configuration option pathToBertini to be a string."



----------------------------------------------------------------
-- Exported functions
----------------------------------------------------------------

-- The package provides the following functions:
-- segreClass:     computes Segre classes of closed subschemes of P^k, or rather the pushforward 
--                 of the total Segre class to the Chow ring of the ambient space
-- chernClass:     computes Chern classes of closed subschemes of P^k, or rather the pushforward 
--                 of the total Chern class to the Chow ring of the ambient space
-- CSMClass:	   computes Chern-Schwartz-MacPherson classes of closed subschemes of P^k, or 
--     	    	   rather the pushforward of the total Chern-Schwartz-MacPherson class to the Chow ring
--     	    	   of the ambient space
-- eulerChar:      computes the topological Euler characteristic of closed subschemes of P^k
--
-- All of these four functions have the option ResidualStrategy, which can be Symbolic or Bertini. 
-- The strategy Symbolic uses Groebner basis computations, the strategy Bertini uses numeric 
-- computations carried out by Bertini [2].  
--
-- bertiniCheck:   checks whether the option ResidualStrategy=>Bertini of the above four functions
--                  works properly 
-- 
export {
     "segreClass", 
     "chernClass", 
     "CSMClass",
     "eulerChar",
     "ResidualStrategy", 
     "Symbolic", 
     "Bertini",
     "bertiniCheck"
     }

-- The computation of the Segre classes is done by the internal function internalSegreClassList, which 
-- returns a list with the degrees of the Segre Classes and the dimension of the ambient space. The 
-- human-readable output as a polynomial in the Chow ring ZZ[H]/H^(k+1) of the ambient space P^k is
-- produced by the internal function output.
-- The user can choose to give the input as a homogeneous ideal in a polynomial ring or as a projective
-- variety. Furthermore, the user can give the symbol used for the Chow ring ZZ[H]/H^(k+1) as an 
-- optional input. The default symbol is H for hyperplane class.
segreClass = method(TypicalValue => RingElement, Options => {ResidualStrategy => Symbolic});
segreClass (Ideal, Symbol) :=  opts -> (I,hyperplaneClass) -> (
     (segreList, ambientDim) := internalSegreClassList(I, ResidualStrategy => opts.ResidualStrategy);
     return output (segreList, ambientDim, hyperplaneClass)
     )
segreClass Ideal := opts ->  I -> (     
     H := symbol H;
     return segreClass (I, H, ResidualStrategy => opts.ResidualStrategy)
     )
segreClass (ProjectiveVariety,Symbol) :=  opts ->  (projectiveVar,hyperplaneClass) -> (
     I := projectiveVar.ring.ideal;
     return segreClass(I, hyperplaneClass, ResidualStrategy => opts.ResidualStrategy)
     )
segreClass  ProjectiveVariety := opts -> projectiveVar -> (
     I := projectiveVar.ring.ideal;
     return segreClass(I, ResidualStrategy => opts.ResidualStrategy)
     )



-- Analogously to the computation of the Segre classes, the computation of the Chern classes is done by 
-- the internal function internalChernClassList, which returns a list with the degrees of the Chern 
-- Classes and the dimension of the ambient space. The human-readable output as a polynomial in the Chow ring 
-- ZZ[H]/H^(k+1) of the ambient space P^k is produced by the internal function output.
-- The user can choose to give the input as a homogeneous ideal in a polynomial ring or as a projective
-- variety. Furthermore, the user can give the symbol used for the Chow ring ZZ[H]/H^(k+1) as an 
-- optional input. The default symbol is H for hyperplane class.
chernClass = method(TypicalValue => RingElement,  Options => {ResidualStrategy=>Symbolic} );
chernClass (Ideal, Symbol) := opts -> (I,hyperplaneClass) -> (
     (chernList, ambientDim) := internalChernClassList(I, ResidualStrategy => opts.ResidualStrategy);
     return output (chernList, ambientDim, hyperplaneClass)
     )
chernClass Ideal :=  opts ->  I -> (  
     H := symbol H;   
     return chernClass (I, H, ResidualStrategy => opts.ResidualStrategy)
     )
chernClass (ProjectiveVariety,Symbol) :=  opts-> (projectiveVar, hyperplaneClass) -> (
     I := projectiveVar.ring.ideal;
     return chernClass(I, hyperplaneClass, ResidualStrategy => opts.ResidualStrategy)
     )
chernClass ProjectiveVariety := opts -> projectiveVar -> (
     I := projectiveVar.ring.ideal;
     return chernClass(I, ResidualStrategy => opts.ResidualStrategy)
     )

 

-- Analogously to the computation of the Chern and Segre classes, the computation of the Chern-Schwartz-MacPherson
--  classes is done by the internal function internalCSMlassList, which returns a list with the degrees 
-- of the Chern-Schwartz-MacPherson classes and the dimension of the ambient space. The human-readable 
-- output as a polynomial in the Chow ring ZZ[H]/H^(k+1) of the ambient space P^k is produced by the 
-- internal function output.
-- The user can choose to give the input as a homogeneous ideal in a polynomial ring or as a projective
-- variety. Furthermore, the user can give the symbol used for the Chow ring ZZ[H]/H^(k+1) as an 
-- optional input. The default symbol is H for hyperplane class.
CSMClass = method(TypicalValue => RingElement,  Options => {ResidualStrategy=>Symbolic} );
CSMClass (Ideal, Symbol) := opts -> (I,hyperplaneClass) -> (
     (csmList, ambientDim) := internalCSMClassList(I, ResidualStrategy => opts.ResidualStrategy);
     return output (csmList, ambientDim, hyperplaneClass)
     )
CSMClass Ideal :=  opts ->  I -> (  
     H := symbol H;   
     return CSMClass (I, H, ResidualStrategy => opts.ResidualStrategy)
     )
CSMClass (ProjectiveVariety,Symbol) :=  opts -> (projectiveVar, hyperplaneClass) -> (
     I := projectiveVar.ring.ideal;
     return CSMClass(I, hyperplaneClass, ResidualStrategy => opts.ResidualStrategy)
     )
CSMClass ProjectiveVariety := opts -> projectiveVar -> (
     I := projectiveVar.ring.ideal;
     return CSMClass(I, ResidualStrategy => opts.ResidualStrategy)
     )

 

-- The computation of the Euler characteristic is done by the internal function internalEuler.
-- The user can choose to give the input as a homogeneous ideal in a polynomial ring or as a projective
-- variety. 
eulerChar = method(TypicalValue => ZZ, Options => {ResidualStrategy => Symbolic});
eulerChar Ideal :=  opts -> (I) -> (
     return internalEuler(I, ResidualStrategy => opts.ResidualStrategy);
     )
eulerChar  ProjectiveVariety := opts -> projectiveVar -> (
     I := projectiveVar.ring.ideal;
     return internalEuler(I, ResidualStrategy => opts.ResidualStrategy);
     )

 
-- There is no test for the above functions using ResidualStrategy=>Bertini as Bertini does not need to
-- be installed on every system that runs Macaulay2. However, the function bertiniCheck()
-- checks whether the commands segreClass, chernClass, CSMClass and eulerChar work when using Bertini
-- instead of symbolic computations.
bertiniCheck = () -> (
    
    setRandomSeed 24;
    x := symbol x; y := symbol y; z := symbol z; w := symbol w;
    
    -- smooth example for segreClass and ChernClass
    R := QQ[x,y,z,w];
    I := minors(2,matrix{{x,y,z},{y,z,w}});
    totalSegre := segreClass(I, ResidualStrategy=>Bertini);
    assert( totalSegre == 3*( (ring(totalSegre))_0 )^2 - 10*( (ring(totalSegre))_0 )^3 );
    totalChern := chernClass(I, ResidualStrategy=>Bertini);
    assert( totalChern == 3*( (ring(totalChern))_0 )^2 + 2 * ((ring(totalChern))_0)^3 );
    
    -- singular example for CSMClass and eulerChar
    S := QQ[x,y,z];
    J := ideal(x^3 + x^2*z - y^2*z);
    totalCSM := CSMClass(J, ResidualStrategy=>Bertini);
    assert( totalCSM == 3*( (ring(totalCSM))_0 ) + 1*( (ring(totalCSM))_0 )^2 );
    eulerCharacteristic := eulerChar(J, ResidualStrategy=>Bertini);
    assert( eulerCharacteristic == 1 );
    
    print "Test passed for the option ResidualStrategy=>Bertini for the commands chernClass, segreClass, CSMClass and eulerChar.";
    
    )  


----------------------------------------------
-- Internal functions
----------------------------------------------



-- The functions internalSegreClassList, internalChernClassList and internalCSMClassList call 
-- other internal functions which do the actual work. 
internalSegreClassList = {ResidualStrategy => Symbolic} >> opts -> I -> (
     -- check that the input is a homogeneous ideal in a polynomial ring over a field
     checkUserInput(I, opts.ResidualStrategy);
     -- trim the ideal and make it an ideal over a ring only used internally
     localI := prepare I;
     -- compute the Segre classes
     return internalSegre(localI, ResidualStrategy => opts.ResidualStrategy);
     )
internalChernClassList = {ResidualStrategy => Symbolic} >> opts -> I -> (
     -- check that the input is a homogeneous ideal in a polynomial ring over a field
     checkUserInput(I,opts.ResidualStrategy);
     -- trim the ideal and make it an ideal over a ring only used internally
     localI := prepare I;
     -- compute the Chern classes
     return internalChern(localI, ResidualStrategy => opts.ResidualStrategy);
     )
internalCSMClassList = {ResidualStrategy => Symbolic} >> opts -> I -> (
     -- check that the input is a homogeneous ideal in a polynomial ring over a field
     checkUserInput(I,opts.ResidualStrategy);
     -- trim the ideal and make it an ideal over a ring only used internally
     localI := prepare I;
     -- compute the Chern-Schwartz-MacPherson classes
     return internalCSM(localI, ResidualStrategy => opts.ResidualStrategy);
     )
-- The function internalEuler checks and prepares the input, just as for example
-- internalSegreClassList. It then computes the Chern-Schwartz-MaxPherson-classes
-- of the input using internalCSM and returns the top Chern-Schwartz-MacPherson-
-- class, which equals the topological Euler characteristic
internalEuler = {ResidualStrategy => Symbolic} >> opts -> I -> (
     -- check that the input is a homogeneous ideal in a polynomial ring over a field
     checkUserInput(I,opts.ResidualStrategy);
     -- trim the ideal and make it an ideal over a ring only used internally
     localI := prepare I;
     -- compute the Chern-Schwartz-MacPherson classes and return the degree of the top class
     return last first internalCSM(localI, ResidualStrategy => opts.ResidualStrategy);
     )

-- The function internalSegre is one of the two main functions in this package which do the actual 
-- computation of the Segre classes. It uses the algorithm described in [1].
-- Computing the degrees of the residuals as defined in [1] is the heart of the algorithm. This
-- is done by the subroutine residualDegs.
-- Notation: This algorithm computes the degrees of the Segre classes s_0(Z,P^k), ..., s_n(Z,P^k) of an
-- n-dimensional closed subscheme Z of P^k. The subscheme Z is given by a homogeneous ideal I in the 
-- polynomial ring R.
-- Input:  I, a homogeneous ideal in a polynomial ring over a field
-- Output: segreList, a list containing the degrees of the Segre classes of Proj(R/I) = Z
--         ambientDim, the dimension k of the ambient space Proj(R)=P^k 
internalSegre = {ResidualStrategy => Symbolic} >> opts -> I -> (
    
     -- Obtain:
     -- the ring R 
     -- the dimension of the ambient space and
     -- the dimension n of Z
     R := ring I;
     ambientDim := dim Proj R;
     dimension := dim Proj(R/I) ;
     
     -- take care of the special cases I = (0) and I = (1)
     if I == ideal(0_R) then (
	  segreList := {1} | toList( ambientDim:0 );
	  return (segreList,ambientDim);
	  );
     if I == ideal(1_R) then (
	  segreList = {};
	  return (segreList,ambientDim);
	  ); 
        
     -- For the nonspecial cases, obtain:
     -- a list of the generators of I sorted by degree
     -- the maximal degree of the generators of I and
     -- a generator of I with minimal degree     
     
     gensI := flatten entries sort gens I;
     maxDeg := first max degrees I; 
     minDegGen := first gensI;
     
     -- initialize segreList as an empty list
     segreList= {};
    
     -- Pick random elements in I of degree maxdeg, one more than the dimension of the ambient space, store in the list f.
     f := for i from 1 to (ambientDim + 1) list sum( gensI, g -> g * random(maxDeg - first(degree(g)), R) );      
     
     -- Compute the degree of the residual of Z in the intersection of d hypersurfaces, where d = codimension of Z, ... , dimension of the ambient space.
     -- Depends on the strategy (Symbolic/Bertini).
     degR := residualDegs(f, ambientDim, dimension, minDegGen, ResidualStrategy => opts.ResidualStrategy);  
         
     
     -- The for loop computes the degrees of the Segre classes of Z using the degrees of the residuals
     for d from (ambientDim - dimension) to ambientDim do (
	  
     	  -- Using the degree of the residual, compute the degree of the pth Segre class, where p = d - codimension of Z.
	  p := d - (ambientDim - dimension);
	  degSegreClass := maxDeg^d - degR_(d - ambientDim + dimension) - sum( 0..(p-1), i -> binomial(d,p-i)*maxDeg^(p-i)*segreList_i );
	  
	  segreList = append(segreList, degSegreClass);
	    
	  );
     
     return (segreList, ambientDim);
     
     )


-- The function residualDegs is the other one of the two main functions in this package which do the actual 
-- computation of the Segre classes. It computes the degrees of the residuals as defined in [1].
-- The option ResidualStrategy determines which method is used to compute the degrees of the residuals.
-- Symbolic uses Groebner bases to compute the saturation of ideals.
-- Bertini uses the regenerative cascade as developed in [3] and implemented in Bertini [2].
residualDegs = {ResidualStrategy => Symbolic} >> opts -> (f, ambientDim, dimension,minDegGen) -> (
     
     R := ring first f;	  
     degR :={};
     
     if (opts.ResidualStrategy == Symbolic) then (
	  
  	  for d from (ambientDim - dimension) to ambientDim do (
	       -- Obtain the ideal J of the intersection of d hypersurfaces containing Z, where d = comdimension of Z, ..., dimension of the ambient space.
	       J := ideal(take(f,d));
	  
	       -- Compute the residual of Z in the intersection of the d hypersurfaces, using saturation. Compute the degree of the residual. 
	       -- Remark: Instead of saturating with the ideal I of the scheme Z, we saturate with a hypersurface containing Z of minimal degree.
	       --         This gives the same result with sufficiently high probability and speeds up calculations considerably.
	       residual := saturate(J,minDegGen);
	       -- Take care of the special case where the residual is the irrelevant ideal when computing the degree
	       degR = append(degR, if residual != ideal vars R then degree residual else 0);
	       ) 
	  );
     
     if (opts.ResidualStrategy == Bertini) then (
	  
	  -- write Bertini input file
	  
	  -- configuration 
	  outConfig := "CONFIG \n" | "OUTPUTLEVEL: 0; \n" | "TRACKTYPE: 1; \n" | "USEREGENERATION: 1; \n" | "MAXNORM: 1e8; \n" | "SECURITYMAXNORM: 1e8; \n" |"END; \n \n";
	  outVarGroup := "hom_variable_group ";
	  -- variables
	  variables := flatten entries vars R;
	  for i from 0 to (length(variables)-2) do outVarGroup = outVarGroup | toString(variables_i) | ", ";
	  outVarGroup = outVarGroup | toString(last variables) | "; \n";
	  -- functions
	  outFunctionDecl := "function "; 
	  for i from 0 to (length(f)-2) do outFunctionDecl = outFunctionDecl | "f" | toString(i) | ", ";
	  outFunctionDecl = outFunctionDecl | "f" | toString(length(f)-1) | "; \n \n";
	  outFunctions := "";
	  for i from 0 to (length(f)-1) do outFunctions = outFunctions | "f" | toString(i) | "=" | replace("ii","I",  toString(f_i) ) | "; \n";
	  outInput := "INPUT \n" | outVarGroup | outFunctionDecl |  outFunctions | "END; \n";
	 
     	  
	  out := outConfig | outInput;
	  
	  -- create input file, write it
	  filename := getFilename();
	  
          g := openOut(filename);
	  g << out;
	  close g;
	  
	  
	  -- run Bertini
	  execstr := "cd /tmp ;" | bertini'path | "bertini " | filename | " > " | getFilename();
	  ret := run(execstr);
	  if ret =!= 0 then  error("error occured while executing external program Bertini. Make sure that Bertini v1.3 or higher is installed and configured.");
	  
	  -- Read output file "regenSummary". Remove the first two lines and the last one. 
	  -- Furthermore remove the lines corresponding to codimensions less than the codimension of the variety,
	  -- these are not relevant. The degrees of the residuals are then the numbers in the 5th column.
	  degR = apply(drop(drop(lines(get "/tmp/regenSummary"),1 + ambientDim-dimension),-1), myString->value( (separate(" ", myString))_5 ) );
	  
	  -- If some the residuals are empty, we have to add zeros manually.
	  for i from 1 to dimension + 1 - #degR do degR = degR | {0};
	  	    	  	  
	 );
     
     degR
     
     );

getFilename = () -> (
     filename := temporaryFileName();
     while fileExists filename  do filename = temporaryFileName();
     rootPath | filename)


-- The function internalChern calls internalSegre to compute the Segre classes of the given subscheme of P^k. From these it computes the
-- Chern-Fulton classes using a simple formula (see for example [1]). The Chern-Fulton classes are identical to the Chern classes if the scheme 
-- is a smooth variety.
-- Input:  I, a homogeneous ideal in a polynomial ring over a field
-- Output: chernList, a list containing the degrees of the Chern classes of Proj(R/I)
--         ambientDim, the dimension k of the ambient space Proj(R)=P^k 
internalChern = {ResidualStrategy => Symbolic} >> opts -> I -> (
     
     -- Obtain:
     -- the ring R
     -- the dimension of the ambient space and
     -- the dimension n of Z
     R := ring I;
     ambientDim := dim Proj R;
     dimension := dim Proj(R/I) ;

     -- take care of the special cases I = (0) and I = (1) 
     if I == ideal(0_R) then (
	  chernList := apply(0..dimension, i-> binomial(dimension+1, i));
	  return (chernList,ambientDim);
	  );
     if I == ideal(1_R) then (
	  chernList = {};
	  return (chernList,ambientDim);
	  ); 

     (segreList,ambientDimDummy) := internalSegre(I, ResidualStrategy => opts.ResidualStrategy); 
     chernList = for i from 0 to dimension list sum( 0..i, p -> binomial( ambientDim + 1, i-p )*segreList_p );
     return  (chernList, ambientDim)
        
     )

-- The function internalCSM computes the Chern-Schwartz-MacPherson class of a projective variety given by
-- an ideal I, using an exclusion-inclusion principle and the function internalCSMhyp, which computes the
-- Chern-Schwartz-MacPherson classes of a hypersurface.
-- Input:  I, a homogeneous ideal in a polynomial ring over a field
-- Output: csmList, a list containing the degrees of the Chern-Schwartz-MacPherson classes of Proj(R/I)
--         ambientDim, the dimension k of the ambient space Proj(R)=P^k 
internalCSM = {ResidualStrategy => Symbolic} >> opts -> I -> (
     
     -- Compute the dimension of the ambient space 
     -- and the codimension of V(I)
     ambientDim := numgens ring I - 1;
     coDimension := ambientDim - (dim I - 1);
     
     -- obtain ring of ambient space and the dimension of I
     R := ring I;
     dimension := dim Proj(R/I) ;
     
     -- take care of the special cases I = (0) and I = (1) 
     if I == ideal(0_R) then (
	  csmList := apply(0..dimension, i-> binomial(dimension+1, i));
	  return (csmList,ambientDim);
	  );
     if I == ideal(1_R) then (
	  csmList = {};
	  return (csmList,ambientDim);
	  ); 
          
     -- compute the Chern-Schwartz-MacPherson class of V(I) from the Chern-Schwartz-MacPherson classes of
     -- hypersurfaces containing V(I), with the help of exclusion-inclusion
     csmList = toList( ambientDim+1:0 );
     for subset in drop(subsets first entries gens I, 1) do (
	  csmList = csmList + (-1)^(length subset - 1) * (internalCSMhyp( product subset, ResidualStrategy=>opts.ResidualStrategy) );
	  );
     -- remove leading zeros
     csmList = drop(csmList, coDimension);
          
     return  (csmList, ambientDim)
        
     )

-- The function internalCSMhyp computes the Chern-Schwartz-MacPherson class of a hypersurface
-- using the algorithm from [4].
-- Input:  p, a homogeneous element of a polynomial ring over a field
-- Output: csmList, a list containing the degrees of the Chern-Schwartz-MacPhersn classes of Proj(R/ideal(p)) 
internalCSMhyp = {ResidualStrategy => Symbolic} >> opts -> p -> (
     
     -- Compute:
     -- the ideal singP of the singular locus of V(p)
     -- the dimension of the ambient space,
     -- the dimension of the singular locus and
     -- the maximal degree maxDegSingP of its generators
     singP := ideal jacobian ideal p;
     ambientDim := numgens ring singP - 1;
     dimension := dim singP - 1;
     maxDegSingP := first max degrees singP;
     
     -- compute the integers s tilde related to the Segre classes of singP
     (s, ambientDimDummy) := internalSegre(singP, ResidualStrategy => opts.ResidualStrategy);
     stilde := {-1} | toList( (ambientDim - dimension - 1):0 ) | s;

     -- compute the shadow of the graph of singP
      g := {};
     for i from 0 to ambientDim do 
          g = g | {- stilde#i - sum(0..(i-1), j -> binomial(i,j) * (-maxDegSingP)^(i-j) * g_j) };
     
     -- compute the Chern-Schwartz-MacPherson classes of V(p) from the shadow of the graph of singP
     for i from 0 to ambientDim list 
          binomial(ambientDim+1, i) - sum(0..i, j-> (-1)^j * g#j * binomial(ambientDim-j, i-j))
     )


-- The function checkUserInput checks that the given ideal I is a homogeneous ideal in a polynomial ring over a field, with a suitable coefficient field.
checkUserInput = (I,residualStrategy) -> (
     
        
     -- Is the ring a polynomial ring?
     if not isPolynomialRing ring I then error "the ideal needs to be defined over a polynomial ring.";
     
     -- Is the ideal homogeneous?
     if not isHomogeneous I then error "the ideal has to be homogeneous.";
     
     -- Is the coefficient ring a field (to make dimension command work)?
     if not isField coefficientRing ring I then error "the coefficient ring needs to be a field.";
     
     -- The saturation part of the symbolic version will not work with real or complex coefficients.
     if  residualStrategy == Symbolic and any( {ComplexField,RealField}, myField -> instance( coefficientRing ring I, myField ) ) then error "the symbolic algorithm does not work with real or complex coefficients.";
  
     -- The numeric version only works with rational, real or complex coefficients.
     if  residualStrategy == Bertini and not( coefficientRing ring I === QQ or any( {ComplexField,RealField}, myField -> instance( coefficientRing ring I, myField ) ) )  then  error "the numeric algorithm only works with rational or complex coefficients."; 
      
     )


-- The function prepare does two things to prepare the later computations. At first, it trims the ideal I, taking away
-- nonnecessary generators. Then it creates a ring only used internally and an ideal in it isomorphic to I and returns this ideal. This 
-- step is done to avoid possible later conflicts in the choice of variables.
prepare = I -> (

     --trim I
     localI := trim I;     
     
     -- rename variables
     numGen := numgens ring localI;
     coeffRing := coefficientRing ring localI;
     z := symbol z;
     internalR := coeffRing[z_1 .. z_numGen];
     renamingMap := map(internalR, ring localI, {z_1 .. z_numGen});
     return renamingMap localI;
     )

-- The function output turns a list of degrees of characteristic classes into a polynomial in the Chow ring of the ambient space P^k.
-- This ring is generated by the hyperplane class.
-- Input:  segreList, a list {deg s_0, ..., deg s_n} of integers
--         ambientDim, the dimension k of ambient space P^k
--         hyperplaneClass, the symbol for the hyperplane class
-- Output: the polynomial (deg s_0)*hyperplaneClass^ambientDim + ... + (deg s_n)*hyperplaneClass^(ambientDim - n)
output = (segreList,ambientDim,hyperplaneClass) -> (
     -- produce the Chow ring ZZ[hyperplaneClass]/(hyperplaneClass^ambientDim+1)
     tempRing := ZZ[hyperplaneClass];
     outputRing := tempRing / ideal((tempRing_0)^(ambientDim+1));
     -- obtain the dimension n
     dimension := #segreList-1;
     -- create the polynomial (deg s_0)*hyperplaneClass^ambientDim + ... + (deg s_n)*hyperplaneClass^(ambientDim - n)
     return  sum(0..dimension, i -> segreList_i * (outputRing_0)^(ambientDim - dimension + i))
     )
 




----------------------------------------------
-- Documentation
---------------------------------------------



beginDocumentation()

doc ///
     Key
     	  CharacteristicClasses
     Headline
     	  Degrees of Chern classes and other characteristic classes of projective schemes
     Description
     	  Text
	       The package CharacteristicClasses provides commands to compute the degrees of the Chern classes, Chern-Schwartz-MacPherson classes and Segre classes of closed subschemes of projective space. 
	       Equivalently, it computes the pushforward of the respective classes to the Chow ring of projective space. The package can also compute the topological Euler characteristic of closed subvarieties and subschemes of projective space.
	       
	       Let X be an n-dimensional subscheme of projective space \PP^k. If X is smooth, then by definition the Chern classes of X are the Chern classes c_0(T_X), ..., c_n(T_X) 
	       of the tangent bundle T_X. The Chern classes are cycles in the Chow ring of X, i.e., linear combinations of subvarieties of X modulo rational equivalence. 
	       For a subvariety V of X, the degree of the cycle [V] is defined as the degree of the variety V. This extends linearly to linear combinations of cycles. 
	       Computing the degrees of the Chern classes of X is equivalent to computing the pushforward of the Chern classes to the Chow ring of \PP^k, which is the ring 
	       \ZZ[H]/(H^{k+1}), with H the hyperplane class. Also by definition, the Segre classes of the projective scheme X are the Segre classes s_0(X,\PP^k), ..., s_n(X,\PP^k) 
	       of X in \PP^k. For definition of the concepts used so far, see for example W. Fulton "Intersection Theory". Chern-Schwartz-MacPherson classes are a generalization of Chern classes of smooth schemes to possibly singular schemes with nice functorial properties.
	       
	       -- The functions computing characteristic classes in this package can have two different kinds of output. The functions chernClass, segreClass and CSMClass give back the pushforward of the total class 
	       -- to the Chow ring of P^k, whereas chernClassList, segreClassList and CSMClass List give a list of the degrees of the Chern, Segre and Chern-Schwartz-MacPherson classes, respectively. The scheme X can be 
	       -- given as either a homogeneous ideal in a polynomial ring over a field, or as projective variety. 
	       
	       This implementation uses the algorithms given in the  articles "Chern Numbers of Smooth Varieties via Homotopy Continuation and Intersection Theory" 
	       (S. Di Rocco, D. Eklund, C. Peterson, A.J. Sommese), "A method to compute Segre classes" (D. Eklund, C. Jost, C. Peterson) and
	       "An algorithm for computing the topological Euler characteristic of complex projective varieties" (C. Jost).
	       The main step in the algorithm is the computation of the residuals. This can be done symbolically, using
	       Gr&ouml;bner bases, and numerically, using the regenerative cascade implemented in Bertini. The regenerative
	       cascade is described in "Regenerative cascade homotopies for solving polynomial systems" by 
	       Jonathan Hauenstein, Andrew Sommese, and Charles Wampler. Bertini is developed
	       by Dan Bates, Jonathan Hauenstein, Andrew Sommese, and Charles Wampler.   
	       
	       Observe that the algorithm is a probabilistic algorithm and may give a wrong answer with a small but nonzero probability. Read more under 
	       @TO "probabilistic algorithm"@.
///



doc ///
     Key
     	  segreClass
	  [segreClass, ResidualStrategy]
	  (segreClass,Ideal)
	  (segreClass, ProjectiveVariety)
	  (segreClass, Ideal, Symbol)
	  (segreClass, ProjectiveVariety, Symbol)	  
     Headline
     	  Degrees of the Segre classes
     Usage
     	  segreClass I
	  segreClass X
     Inputs
     	  I:Ideal
	    a homogeneous ideal in a polynomial ring over a field, defining a closed subscheme X of \PP^k
	  X:ProjectiveVariety
	  ResidualStrategy => "Symbolic"
	    the strategy to compute the degrees of the residuals
     Outputs
     	  :RingElement
	   the pushforward of the total Segre class of the scheme X to the Chow ring \ZZ[H]/(H^{k+1}) of projective space \PP^k.
     Description
     	  Text
	       For an n-dimensional subscheme X of projective space \PP^k, this command computes the push-forward of the total Segre class s(X,\PP^k) of X in \PP^k to the Chow ring of \PP^k. The output is a polynomial in the hyperplane class, containing the degrees of the Segre classes s_0(X,\PP^k),...,s_n(X,\PP^k) as coefficients.
	  Example
	       setRandomSeed 72;
	       R = QQ[x,y,z]
	       segreClass ideal(x*y)
	       segreClass ideal(x^2*y,x*y^2)	  
	  Text
     	       We consider two singular curves in \PP^2, C_1 defined by \{xy=0\} \  and C_2 defined by \{x^2y=xy^2=0\}. The degrees of their Segre classes are s_0(C_1,\PP^2) = 2, s_1(C_1,\PP^2)=-4, and s_0(C_2, \PP^2)=2, s_1(C_2,\PP^2)=-3. Observe that the two curves have the same underlying space but a different scheme structure, which is detected by the Segre classes. It is also possible to provide the symbol for the hyperplane class in the Chow ring of \PP^k:
	  Example
	       segreClass( ideal(x*y), symbol t )
	  Text
	       All the examples were done using symbolic computations with Gr\"obner bases. Changing the
	       option @TO ResidualStrategy@ to Bertini will do the main computations numerically, provided
	       Bertini is  @TO2 {"configuring Bertini", "installed and configured"}@.
	       
	       Observe that the algorithm is a probabilistic algorithm and may give a wrong answer with a small but nonzero probability. Read more under 
	       @TO "probabilistic algorithm"@.
///
     

doc ///
     Key
     	  chernClass
	  [chernClass, ResidualStrategy]
	  (chernClass, Ideal)
	  (chernClass, ProjectiveVariety)
	  (chernClass, Ideal, Symbol)
	  (chernClass, ProjectiveVariety, Symbol)	  
     Headline
     	  computes degrees of the Chern classes
     Usage
     	  chernClass I
	  chernClass X
     Inputs
          I:Ideal
	    a homogeneous ideal in a polynomial ring over a field, defining a projective scheme X
	  X:ProjectiveVariety
	  ResidualStrategy => "Symbolic"
	    the strategy to compute the degrees of the residuals
     Outputs
     	  :RingElement
           the pushforward of the total Chern class of the scheme X to the Chow ring \ZZ[H]/(H^{k+1}) of projective space \PP^k.
Description
     	  Text
	       For a non-singular n-dimensional subscheme X of projective space \PP^k, this command computes the push-forward of the total Chern class of X to the Chow ring of \PP^k. The output is a polynomial in the hyperplane class, containing the degrees of the Chern classes c_0(T_X),...,c_n(T_X) as coefficients.
	  Example
	       setRandomSeed 438;
	       R = QQ[x,y,z,w]
	       A = matrix{{x,y,z},{y,z,w}}
	       chernClass minors(2,A)  	  
	  Text
	       The 2x2-minors of the matrix A form the ideal of the twisted cubic. It is well-known that its degree is 3 and its genus is 0. The calculations confirm that deg c_1 = 2-2g = 2 and deg  c_0 = 3. 
	       It is also possible to provide the symbol for the hyperplane class in the Chow ring of \PP^k:
	  Example
	       chernClass( minors(2,A), symbol t ) 
	  Text
	       All the examples were done using symbolic computations with Gr\"obner bases. Changing the
	       option @TO ResidualStrategy@ to Bertini will do the main computations numerically, provided
	       Bertini is @TO2 {"configuring Bertini", "installed and configured"}@ .  
	       
	       The command chernClass actually computes the push-forward of the total @EM {"Chern-Fulton class"}@ of the subscheme X of projective space \PP^k. The Chern-Fulton class is one of several generalizations of Chern classes to possibly singular subschemes of projective space. It is defined as c_{CF}(X) = c(T_{\PP^k}|_X) \cap s(X,\PP^k). For non-singular schemes, the Chern-Fulton class coincides with the Chern class of the tangent bundle. So for non-singular input, the command will compute just the usual Chern class.
	       
	       Observe that the algorithm is a probabilistic algorithm and may give a wrong answer with a small but nonzero probability. Read more under 
	       @TO "probabilistic algorithm"@.
///


doc ///
     Key
     	  CSMClass
	  [CSMClass, ResidualStrategy]
	  (CSMClass,Ideal)
	  (CSMClass, ProjectiveVariety)
	  (CSMClass, Ideal, Symbol)
	  (CSMClass, ProjectiveVariety, Symbol)	  
     Headline
     	  computes degrees of the Chern-Schwartz-MacPherson classes
     Usage
     	  CSMClass I
	  CSMClass X
     Inputs
          I:Ideal
	    a homogeneous ideal in a polynomial ring over a field, defining a projective scheme X
	  X:ProjectiveVariety
	  ResidualStrategy => "Symbolic"
	    the strategy to compute the degrees of the residuals
     Outputs
     	  :RingElement
	   the pushforward of the total Chern-Schwartz-MacPherson class of the scheme X to the Chow ring \ZZ[H]/(H^{k+1}) of projective space \PP^k.
     Description
     	  Text
	       For an n-dimensional subscheme X of projective space \PP^k, this command computes the push-forward of the total Chern-Schwartz-MacPherson class of X to the Chow ring of \PP^k. The output is a polynomial in the hyperplane class, containing the degrees of the Chern-Schwartz-MacPherson classes (c_{SM})_0(T_X),...,(c_{SM})_n(T_X) as coefficients.
	  Example
	       setRandomSeed 365;
	       R = QQ[x,y,z]
	       CSMClass ideal(x^3 + x^2*z - y^2*z)
	       chernClass ideal(x^3 + x^2*z - y^2*z)	  
	  Text
	       We compute the Chern-Schwartz-MacPherson class of the singular cubic x^3 + x^2z = y^2z. Observe that it does not agree with the Chern-Fulton class computed by the command @TO chernClass@.
	      It is also possible to provide the symbol for the hyperplane class in the Chow ring of \PP^k:
	  Example
	       CSMClass( ideal(x^3 + x^2*z - y^2*z), symbol t ) 
	  Text
	       All the examples were done using symbolic computations with Gr\"obner bases. Changing the
	       option @TO ResidualStrategy@ to Bertini will do the main computations numerically, provided
	       Bertini is @TO2 {"configuring Bertini", "installed and configured"}@ .  
	       
	       Observe that the algorithm is a probabilistic algorithm and may give a wrong answer with a small but nonzero probability. Read more under 
	       @TO "probabilistic algorithm"@.
///



doc ///
     Key
     	  eulerChar
	  [eulerChar, ResidualStrategy]
	  (eulerChar,Ideal)
	  (eulerChar, ProjectiveVariety)  
     Headline
     	  computes the topological Euler characteristic
     Usage
     	  eulerChar I
	  eulerChar X
     Inputs
          I:Ideal
	    a homogeneous ideal in a polynomial ring over a field, defining a projective scheme X
	  X:ProjectiveVariety
	  ResidualStrategy => "Symbolic"
	    the strategy to compute the degrees of the residuals
     Outputs
     	  :ZZ
	    the topological Euler characteristic of the scheme X.
	    
     Description
     	  Text
	      This command computes the topological Euler characteristic of closed subschemes of \PP^k, even singular ones. We compute the topological Euler characteristic of the singular cubic x^3 + x^2z = y^2z. 
	  Example
	       setRandomSeed 4386;
	       R = QQ[x,y,z]
	       eulerChar ideal(x^3 + x^2*z - y^2*z)     
	  Text
	       All the examples were done using symbolic computations with Gr\"obner bases. Changing the
	       option @TO ResidualStrategy@ to Bertini will do the main computations numerically, provided
	       Bertini is @TO2 {"configuring Bertini", "installed and configured"}@ .  
	       
	       Observe that the algorithm is a probabilistic algorithm and may give a wrong answer with a small but nonzero probability. Read more under 
	       @TO "probabilistic algorithm"@.
///

doc ///
     Key 
          ResidualStrategy
	  Symbolic
	  Bertini
     Description
     	  Text
	       The option ResidualStrategy determines which strategy is used to compute the residuals, which
	       is the main step in the computation done by the commands in this package. When choosing the default
	       Symbolic, Gr\"obner basis methods will be used. The computations can also be done numerically using 
	       the regenerative cascade implemented in Bertini. This is done by choosing the option Bertini and provided
	       Bertini is @TO2 {"configuring Bertini", "installed and configured"}@. Using Bertini may provide a speed-up 
	       or prevent running out of memory.
	  Example
	       setRandomSeed 367;
	       R = QQ[x,y,z,w]
	       chernClass( minors(2,matrix{{x,y,z},{y,z,w}}), ResidualStrategy=>Symbolic)  
///



   
doc ///
     Key
     	  "configuring Bertini"
     Description
     	  Text
	       Using the numeric version of any command in the package CharacteristicClasses needs version 1.3 or higher of Bertini
	       to be installed. Download and installation of Bertini are explained at the @HREF {"http://www.nd.edu/~sommese/bertini/","Bertini homepage"}@. 
	       
	       Bertini should be installed in a directory in the user's PATH. As an alternative you can tell
	       the package how to find Bertini. Usually, when the package is installed, a file called {\tt init-CharacteristicClasses.m2} is created automatically in the user's
	       @TO2 {"applicationDirectory", "application directory"}@. See also the option {\tt Configuration} under @TO "newPackage"@.
	       In the file {\tt init-CharacteristicClasses.m2}, replace {\tt ""} in  the line {\tt "pathToBertini" => ""}
	       by the path to Bertini in quotation marks, for example {\tt "pathToBertini" => "/usr/local/BertiniLinux64&#95;v1.3.1/"}. The / at the end is important.	
	       Windows users should use the path relative to the cygwin directory, for example {\tt "/usr/local/BertiniWindows32&#95;v1.3.1/"} if Bertini is installed under 
	       {\tt pathToTheCygwinDirectory&#92;cygwin&#92;usr&#92;local&#92;BertiniWindows32&#95;v1.3.1 }.
	       
	       To check whether Bertini is working properly with the functions in the package CharacteristicClasses, use @TO "bertiniCheck"@.

///

doc ///
    Key
    	bertiniCheck
    Headline
     	  checks whether the numerical version of the algorithms using Bertini works
    Usage
     	  bertiniCheck()
    Description
    	Text
		The functions @TO "chernClass"@, @TO segreClass@, @TO CSMClass@ and @TO eulerChar@ have the option @TO ResidualStrategy@,
		which can be either @TO Symbolic@ or @TO Bertini@. The option "Bertini" uses the external program Bertini, which might not
		be installed on the user's system. The function bertiniCheck checks whether Bertini is properly installed and configured. See
		also @TO "configuring Bertini"@.	    
///
  


doc ///
     Key
     	  "probabilistic algorithm"
     Description
     	  Text
	       The algorithm used for the computation of characteristic classes is probabilistic. Theoretically it calculates the classes correctly in all cases
	       outside a lower-dimensional subset, i.e., with probability 1. In the implementation, however, the probability of not computing the correct class
	       is strictly larger than zero, although small. Skeptical users should repeat calculations to increase the probability of computing the correct class.
	       
	       We illustrate the probabilistic behaviour with an example where the chosen random seed leads to a wrong result in the first calculation. 
	  Example
	       setRandomSeed 121;
   	       R = QQ[x,y,z,w]
   	       I = minors(2,matrix{{x,y,z},{y,z,w}})
   	       chernClass I  
     	       chernClass I  
	       chernClass I  	       
///

	  	       

   
--------------------------------------------------------
-- Tests
--------------------------------------------------------
 


 
TEST ///
   setRandomSeed 24
   R = QQ[x,y,z,w]
   I = minors(2,matrix{{x,y,z},{y,z,w}})
   totalSegre = segreClass I
   assert( totalSegre == 3*( (ring(totalSegre))_0 )^2 - 10*( (ring(totalSegre))_0 )^3 )
   totalChern = chernClass I
   assert( totalChern == 3*( (ring(totalChern))_0 )^2 + 2 * ((ring(totalChern))_0)^3 )
///

TEST ///
   setRandomSeed 657
   R = QQ[x,y,z]
   I = ideal(x^3 + x^2*z - y^2*z)
   totalCSM = CSMClass I
   assert( totalCSM == 3*( (ring(totalCSM))_0 ) + 1*( (ring(totalCSM))_0 )^2 )
///


-------------------------------------------------------
-- References
------------------------------------------------------
-- [1] A method to compute Segre classes (David Eklund, Christine Jost, Chris Peterson), Journal of Algebra and Its Applications 12(2), 2013
-- [2] Bertini: Software for Numerical Algebraic Geometry (Daniel J. Bates, Jonathan D. Hauenstein, Andrew J. Sommese, Charles W. Wampler), available at http://www.nd.edu/~sommese/bertini
-- [3] Regenerative cascade homotopies for solving polynomial systems (Jonathan D. Hauenstein, Andrew J. Sommese, Charles W. Wampler), Applied Mathematics and Computation 218(4), 2011
-- [4] An algorithm for computing the topological Euler characteristic of complex projective varieties (Christine Jost), submitted, arXiv:1301.4128 [math.AG]
