-- -*- coding: utf-8 -*-


-----------------------------------------------------------------
-- Preamble
-----------------------------------------------------------------

newPackage(
	"CharacteristicClasses",
	Version => "0.1", 
    	Date => "April 3, 2012",
    	Authors => {{Name => "Christine Jost", 
		  Email => "jost@math.su.se", 
		  HomePage => "http://www.math.su.se/~jost"}},
    	Headline => "Degrees of Chern and Segre classes",
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
-- if bertini'path == "" then bertini'path = rootPath | "/";

if not instance(bertini'path,String) then error "Expected configuration option pathToBertini to be a string."



----------------------------------------------------------------
-- Exported functions
----------------------------------------------------------------

-- The package provides the following functions:
-- segreClass:     computes Segre classes of closed subschemes of P^k, or rather the pushforward 
--                 of the total Segre class to the Chow ring of the ambient space
-- segreClassList: does the same as segreClass, but returns a list with coefficients instead of a 
--                 polynomial
-- chernClass:     computes Chern classes of closed subschemes of P^k, or rather the pushforward 
--                 of the total Chern class to the Chow ring of the ambient space
-- chernlassList:  does the same as chernClass, but returns a list with coefficients instead of a 
--                 polynomial
-- All functions have the option ResidualStrategy, which can be Symbolic or Bertini. 
-- The strategy Symbolic uses Groebner basis computations, the strategy Bertini uses numeric 
-- computations carried out by Bertini [2].  
export {segreClass, chernClass, segreClassList, chernClassList, ResidualStrategy, Symbolic, Bertini}



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

segreClassList = method(TypicalValue => List,  Options => {ResidualStrategy => Symbolic});
segreClassList  Ideal :=  opts ->I -> (
     (segreList, ambientDim) := internalSegreClassList(I, ResidualStrategy => opts.ResidualStrategy);
     return segreList
     )
segreClassList ProjectiveVariety := opts -> projectiveVar -> (
     I := projectiveVar.ring.ideal;
     return segreClassList(I, ResidualStrategy => opts.ResidualStrategy)
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

 
chernClassList = method(TypicalValue => List,  Options => {ResidualStrategy=>Symbolic});
chernClassList Ideal :=  opts ->  I -> (
     (chernList, ambientDim) := internalChernClassList(I, ResidualStrategy => opts.ResidualStrategy);
     return chernList
     )
chernClassList  ProjectiveVariety := opts -> projectiveVar -> (
     I := projectiveVar.ring.ideal;
     return chernClassList(I, ResidualStrategy => opts.ResidualStrategy)
     )




----------------------------------------------
-- Internal functions
----------------------------------------------



-- The functions internalSegreClassList and internalChernClassList call other internal functions 
-- which do the actual work. 
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
	  segreList := {};
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
	  execstr := "cd /tmp ;" | bertini'path | "bertini " | filename;
	  ret := run(execstr);
	  if ret =!= 0 then  error("Error occured while executing external program Bertini. Make sure that Bertini v1.3 or higher is installed and configured.");
	  
	  -- Read output file "regenSummary". Remove the first two lines and the last one. 
	  -- Furthermore remove the lines corresponding to codimensions less than the codimension of the variety,
	  -- these are not relevant. The degrees of the residuals are then the numbers in the 5th column.
	  degR = apply(drop(drop(lines(get "/tmp/regenSummary"),1 + ambientDim-dimension),-1), myString->value( (separate(" ", myString))_5 ) );
	  
	  -- If the residuals are empty, we have to add zeros manually.
	  numberOfMissingLines := dimension + 1 - #degR; 	 
	  if (numberOfMissingLines > 0) then for i from 1 to numberOfMissingLines do (degR = degR | {0}); 
	    	  	  
	 );
     
     degR
     
     );

getFilename = () -> (
     filename := temporaryFileName();
     while fileExists(filename) do filename = temporaryFileName();
     rootPath | filename)


-- The function internalChern calls internalSegre to compute the Segre classes of the given subscheme of P^k. From these it computes the
-- Chern-Fulton classes using a simple formula (see e.g. [1]). The Chern-Fulton classes are identical to the Chern classes if the scheme 
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



-- The function checkUserInput checks that the given ideal I is a homogeneous ideal in a polynomial ring over a field, with a suitable coefficient field.
checkUserInput = (I,residualStrategy) -> (
     
        
     -- Is the ring a polynomial ring?
     if not isPolynomialRing ring I then error "The ideal needs to be defined over a polynomial ring.";
     
     -- Is the ideal homogeneous?
     if not isHomogeneous I then error "The ideal has to be homogeneous.";
     
     -- Is the coefficient ring a field (to make dimension command work)?
     if not isField coefficientRing ring I then error "The coefficient ring needs to be a field.";
     
     -- The saturation part of the symbolic version will not work with real or complex coefficients.
     if ( (residualStrategy == Symbolic) and any( {ComplexField,RealField}, myField -> instance( coefficientRing ring I, myField ) ) ) then error "The symbolic algorithm does not work with real or complex coefficients.";
  
     -- The numeric version only works with rational, real or complex coefficients.
     if ( (residualStrategy == Bertini) and not( coefficientRing ring I === QQ or any( {ComplexField,RealField}, myField -> instance( coefficientRing ring I, myField ) ) ) ) then  error "The numeric algorithm only works with rational or complex coefficients."; 
      
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
     	  Degrees of Chern and Segre classes
     Description
     	  Text
	       The package CharacteristicClasses provides commands to compute the degrees of the Chern and Segre classes of subvarieties and subschemes of projective space. 
	       Equivalently, it computes the pushforward to projective space of the Chern and Segre classes.
	       
	       Let X be an n-dimensional subscheme of projective space P^k. If X is smooth, then by definition the Chern classes of X are the Chern classes c_0(T_X), ..., c_n(T_X) 
	       of the tangent bundle T_X. The Chern classes are cycles in the Chow ring of X, i.e., linear combinations of subvarieties of X modulo rational equivalence. 
	       For a subvariety V of X, the degree of the cycle [V] is defined as the degree of the variety V. This extends linearly to linear combinations of subvarieties. 
	       Computing the degrees of the Chern classes of X is equivalent to computing the pushforward of the Chern classes to the Chow ring of P^k, which is the ring 
	       ZZ[H]/(H^{k+1}), with H the hyperplane class. Also by definition, the Segre classes of the projective scheme X are the Segre classes s_0(X,P^k), ..., s_n(X,P^k) 
	       of X in P^k. For definition of the concepts used here, see e.g. W. Fulton "Intersection Theory".
	       The functions in this package can have two different kinds of output. The functions chernClass and segreClass give back the pushforward of the total Chern class 
	       to the Chow ring of P^k, whereas chernClassList and segreClassList give a list of the degrees of the Chern or Segre classes, respectively. The scheme X can be 
	       given as either a homogeneous ideal in a polynomial ring over a field, or as projective variety. 
	       
	       This implementation uses the algorithm given in the  articles "Chern Numbers of Smooth Varieties via Homotopy Continuation and Intersection Theory" 
	       (Sandra Di Rocco, David Eklund, Chris Peterson, Andrew J. Sommese) and "A method to compute Segre classes" (David Eklund, Christine Jost, Chris Peterson).
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
	    a homogeneous ideal in a polynomial ring over a field, defining a closed subscheme X of P^k
	  X:ProjectiveVariety
	    -- a projective variety
	  ResidualStrategy => "Symbolic"
	    the strategy to compute the degrees of the residuals
     Outputs
     	  :RingElement
	   the pushforward of the total Segre class of the scheme X to the Chow ring ZZ[H]/(H^{k+1}) of projective space P^k.
     Description
     	  Text
	       For an n-dimensional subscheme X of projective space P^k, this command computes the push-forward of the total Segre class s(X,P^k) of X in P^k to the Chow ring of P^k. The output is a polynomial in the hyperplane class, containing the degrees of the Segre classes s_0(X,P^k),...,s_n(X,P^k) as coefficients.
	  Example
	       R = QQ[x,y,z]
	       segreClass ideal(x*y)
	       segreClass ideal(x^2*y,x*y^2)	  
	  Text
     	       We consider two singular curves in P^2, C_1 defined by \{xy=0\} \  and C_2 defined by \{x^2y=xy^2=0\}. The degrees of their Segre classes are s_0(C_1,P^2) = 2, s_1(C_1,P^2)=-4, and s_0(C_2,P^2)=2, s_1(C_2,P^2)=-3. Observe that the two curves have the same underlying space but a different scheme structure, which is detected by the Segre classes. It is also possible to provide the symbol for the hyperplane class in the Chow ring of P^k:
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
	  (chernClass,Ideal)
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
	    -- a projective variety
	  ResidualStrategy => "Symbolic"
	    the strategy to compute the degrees of the residuals
     Outputs
     	  :RingElement
	   the pushforward of the total Chern class of the scheme X to the Chow ring ZZ[H]/(H^{k+1}) of projective space P^k.
     Description
     	  Text
	       For a non-singular n-dimensional subscheme X of projective space P^k, this command computes the push-forward of the total Chern class of X to the Chow ring of P^k. The output is a polynomial in the hyperplane class, containing the degrees of the Chern classes c_0(T_X),...,c_n(T_X) as coefficients.
	  Example
	       R = QQ[x,y,z,w]
	       A = matrix{{x,y,z},{y,z,w}}
	       chernClass minors(2,A)  	  
	  Text
	       The 2x2-minors of the matrix A form the ideal of the twisted cubic. It is well-known that its degree is 3 and its genus is 0. The calculations confirm that deg c_1 = 2-2g = 2 and deg  c_0 = 3. 
	       It is also possible to provide the symbol for the hyperplane class in the Chow ring of P^k:
	  Example
	       chernClass( minors(2,A), symbol t ) 
	  Text
	       All the examples were done using symbolic computations with Gr\"obner bases. Changing the
	       option @TO ResidualStrategy@ to Bertini will do the main computations numerically, provided
	       Bertini is @TO2 {"configuring Bertini", "installed and configured"}@ .  
	       
	       The command chernClass actually computes the push-forward of the total @EM {"Chern-Fulton class"}@ of the subscheme X of projective space P^k. The Chern-Fulton class is one of several generalizations of Chern classes to possibly singular subschemes of projective space. It is defined as c_{CF}(X) = c(T_{P^k}|_X) \cap s(X,P^k). For non-singular schemes, the Chern-Fulton class coincides with the Chern class of the tangent bundle. So for non-singular input, the command will compute just the usual Chern class.
	       
	       Observe that the algorithm is a probabilistic algorithm and may give a wrong answer with a small but nonzero probability. Read more under 
	       @TO "probabilistic algorithm"@.
///

doc ///
     Key
     	  segreClassList
	  [segreClassList, ResidualStrategy]
	  (segreClassList, Ideal)
	  (segreClassList, ProjectiveVariety)
     Headline
     	  computes degrees of the Segre classes
     Usage
     	  segreClassList I
	  segreClassList X
     Inputs
          I:Ideal
	    a homogeneous ideal in a polynomial ring over a field, defining a subscheme X of P^k
	  X:ProjectiveVariety
	    --a projective variety
	  ResidualStrategy => "Symbolic"
	    the strategy to compute the degrees of the residuals      
     Outputs
     	  :List
	   \{ deg s_0(X,P^k),..., deg s_n(X,P_K) \} of the degrees of the Segre classes of X in P^k
     Description
     	  Text
	       This function does the same as the function @TO segreClass@, but provides an output that is easier to read for a computer.
///



doc ///
     Key
     	  chernClassList
	  [chernClassList, ResidualStrategy]
	  (chernClassList, Ideal)
	  (chernClassList, ProjectiveVariety)
     Headline
     	  degrees of Chern classes
     Usage
     	  chernClassList I
	  chernClassList X
     Inputs
          I:Ideal
	    a homogeneous ideal in a polynomial ring over a field, defining a projective scheme X
	  X:ProjectiveVariety
	    --a projective variety
	  ResidualStrategy =>  "Symbolic" 
	    the strategy to compute the degrees of the residuals	    
     Outputs
     	  :List
	   \{ deg c_0(T_X),..., deg c_n(T_X) \} \ of the degrees of the Chern classes of X
     Description
     	  Text
	       This function does the same as the function @TO{chernClass}@, but provides an output that is easier to read for a computer.
///

doc ///
     Key 
          ResidualStrategy
	  Symbolic
	  Bertini
     --Headline
          --ResidualStrategy,
     Description
     	  Text
	       The option ResidualStrategy determines which strategy is used to compute the residuals, which
	       is the main step in the computation of the Chern and Segre classes. When choosing the default
	       Symbolic, Gr\"obner basis methods will be used. The computations can also be done numerically using 
	       the regenerative cascade implemented in Bertini. This is done by choosing the option Bertini and provided
	       Bertini is @TO2 {"configuring Bertini", "installed and configured"}@. Using Bertini will usually result in a considerable speed-up.	   
	  Example
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
	       
	       If Bertini cannot be called from the root Path (e.g. / on Unix-based system), you have to tell
	       the package how to find Bertini. When the package is installed, a file called {\tt init-CharacteristicClasses.m2} is created automatically.
	       Under Linux, the file is sought in the directory {\tt HOME/.Macaulay2/}, where {\tt HOME} is replaced by 
	       the path to the user's home directory. Under Mac OS X, the file is sought instead in the directory 
	       {\tt  HOME/Library/Application Support/Macaulay2/}. Windows users will have installed both Macaulay2 and Bertini under cygwin, so the file will be
	       sought in the directory  {\tt HOME/.Macaulay2/}, where {\tt HOME} is replaced by the path to the user's home directory in the cygwin directory.
	       
	       In the file {\tt init-CharacteristicClasses.m2}, replace {\tt ""} in  the line {\tt "pathToBertini" => ""}
	       by the path to Bertini in quotation marks, e.g. {\tt "pathToBertini" => "/usr/local/BertiniLinux64&#95;v1.3.1/"}. The / at the end is important.	
	       Windows users should use the path relative to the cygwin directory, e.g. {\tt "/usr/local/BertiniWindows32&#95;v1.3.1/"} if Bertini is installed under 
	       {\tt pathToTheCygwinDirectory&#92;cygwin&#92;usr&#92;local&#92;BertiniWindows32&#95;v1.3.1 }.

///


doc ///
     Key
     	  "probabilistic algorithm"
     Description
     	  Text
	       The algorithm used for the computation of characteristic classes is probabilistic. Theoretically it calculates the classes correctly in all cases
	       outside a lower-dimensional subset, i.e., with probability one. In the implementation, however, the probability of not computing the correct class
	       is strictly larger than zero, although small. Sceptical users should repeat calculations to increase the probability of computing the correct class.
	       
	       An example that is known to be susceptible to errors, especially if {\tt ResidualStrategy=>Bertini} is used, is the following.
	  Example
	       R=QQ[x,y,z,w]
	       I=ideal(x^3+y*z*w,x*y+z*w,x^5+y^5+z^5+w^5)
	  Text
	       To get a feeling, the user is recommended to compute characteristic classes of this scheme several times, using the two different strategies.
	       
///
	  	       

   
--------------------------------------------------------
-- Tests
--------------------------------------------------------
 

TEST ///
   setRandomSeed 121
   R = QQ[x,y,z,w]
   I = minors(2,matrix{{x,y,z},{y,z,w}})
   assert( segreClassList I == {3,-10} )
   assert( chernClassList I == {3,2} )
 ///
 
TEST ///
   R = QQ[x,y,z,w]
   I = minors(2,matrix{{x,y,z},{y,z,w}})
   totalSegre = segreClass I
   assert( totalSegre == 3*( (ring(totalSegre))_0 )^2 - 10*( (ring(totalSegre))_0 )^3 )
   totalChern = chernClass I
   assert( totalChern == 3*( (ring(totalChern))_0 )^2 + 2 * ((ring(totalChern))_0)^3 )
///


-- TEST ///
--    R = QQ[x,y,z,w]
--    I = minors(2,matrix{{x,y,z},{y,z,w}})
--    assert( segreClassList(I, ResidualStrategy=>Bertini) == {3,-10} )
--    assert( chernClassList(I, ResidualStrategy=>Bertini) == {3,2} )
--  ///
 
-- TEST ///
--    R = QQ[x,y,z,w]
--    I = minors(2,matrix{{x,y,z},{y,z,w}})
--    totalSegre = segreClass(I, ResidualStrategy=>Bertini)
--    assert( totalSegre == 3*( (ring(totalSegre))_0 )^2 - 10*( (ring(totalSegre))_0 )^3 )
--    totalChern = chernClass(I, ResidualStrategy=>Bertini)
--    assert( totalChern == 3*( (ring(totalChern))_0 )^2 + 2 * ((ring(totalChern))_0)^3 )
-- ///


-------------------------------------------------------
-- References
------------------------------------------------------
-- [1] A method to compute Segre classes (David Eklund, Christine Jost, Chris Peterson), 2011, available at arXiv:1109.5895v1 [math.AG]
-- [2] Bertini: Software for Numerical Algebraic Geometry (Daniel J. Bates, Jonathan D. Hauenstein, Andrew J. Sommese, Charles W. Wampler), available at http://www.nd.edu/~sommese/bertini
-- [3] Regenerative cascade homotopies for solving polynomial systems (Jonathan D. Hauenstein, Andrew J. Sommese, Charles W. Wampler), Applied Mathematics and Computation
