--*- coding: utf-8 -*-
---------------------------------------------------------------------------
-- PURPOSE: Calculating versal deformations and local Hilbert schemes
-- PROGRAMMER : Nathan Ilten
-- UPDATE HISTORY : June 2013
---------------------------------------------------------------------------
newPackage("VersalDeformations",
    Headline => "A package for calculating versal deformations and local Hilbert schemes",
    Version => "1.2",
    Date => "June 6, 2013",
    Authors => {
        {Name => "Nathan Owen Ilten",
	  HomePage => "http://math.berkeley.edu/~nilten/",
	  Email => "nilten@math.berkeley.edu"}},
    Configuration => {"DefaultDefParam"=>"t"},
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	 "journal URI" => "http://j-sag.org/",
	 "article title" => "Versal deformations and local Hilbert schemes",
	 "acceptance date" => "2012-06-05",
	 "published article URI" => "http://j-sag.org/Volume4/jsag-3-2012.pdf",
	 "published code URI" => "http://j-sag.org/Volume4/VersalDeformations.m2",
	 "repository code URI" => "svn://svn.macaulay2.com/Macaulay2/trunk/M2/Macaulay2/packages/VersalDeformations.m2",
	 "release at publication" => 14710,
	 "version at publication" => "1.0",
	 "volume number" => "4",
	 "volume URI" => "http://j-sag.org/Volume4/"
	 }    
    )

---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2013 Nathan Owen Ilten
--
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
---------------------------------------------------------------------------

defaultdefparam = (options VersalDeformations).Configuration#"DefaultDefParam"


export {"liftDeformation",
     "firstOrderDeformations",
     "CT",
     "cotangentCohomology1",
     "cotangentCohomology2",
     "normalMatrix",
     "versalDeformation",
     "localHilbertScheme",
     "PolynomialCheck",
     "SanityCheck",
     "HighestOrder",
     "SmartLift",
     "DefParam",
     "correctDeformation",
     "correctionMatrix",
     "CorrectionMatrix",
     "CacheName",
     "VersalDeformationResults"
      }

protect VersalDeformationResults
protect CacheName
protect SanityCheck
protect PolynomialCheck
protect HighestOrder
protect SmartLift
protect CorrectionMatrix
protect DefParam

     
     
----------------------------------------------------------------------------
-- Cotangent cohomology and normal modules
----------------------------------------------------------------------------     



CT=new ScriptedFunctor
CT#superscript=i->(if i==1 then return cotangentCohomology1; if i==2 then return cotangentCohomology2;
	error "Higher cotangent cohomology not yet implemented")

cotangentCohomology1=method(TypicalValue=>Matrix,Options=>{SourceRing=>null})

cotangentCohomology1Mod:=(F)->(
     A:=ring F/image F; --quotient ring
     Hom(ideal F,A)/(image ((transpose substitute(jacobian F,A)))))
     
cotangentCohomology1 Matrix:=opts->F->lift(ambient basis(cotangentCohomology1Mod F,opts), ring F)
cotangentCohomology1 (ZZ,Matrix):=opts->(deg,F)->lift(ambient basis(deg,cotangentCohomology1Mod(F),opts), ring F)
cotangentCohomology1 (List,Matrix):=opts->(deg,F)->lift(ambient basis(deg,cotangentCohomology1Mod(F),opts), ring F)
cotangentCohomology1 (ZZ,ZZ,Matrix):=opts->(lo,hi,F)->lift(ambient basis(lo,hi,cotangentCohomology1Mod(F),opts), ring F)
cotangentCohomology1 (InfiniteNumber,ZZ,Matrix):=opts->(lo,hi,F)->lift(ambient basis(lo,hi,cotangentCohomology1Mod(F),opts), ring F)
cotangentCohomology1 (ZZ,InfiniteNumber,Matrix):=opts->(lo,hi,F)->lift(ambient basis(lo,hi,cotangentCohomology1Mod(F),opts), ring F)

cotangentCohomology1 Ideal:=opts->I->lift(ambient basis(cotangentCohomology1Mod gens I,opts), ring I)
cotangentCohomology1 (ZZ,Ideal):=opts->(deg,I)->lift(ambient basis(deg,cotangentCohomology1Mod(gens I),opts), ring I)
cotangentCohomology1 (List,Ideal):=opts->(deg,I)->lift(ambient basis(deg,cotangentCohomology1Mod(gens I),opts), ring I)
cotangentCohomology1 (ZZ,ZZ,Ideal):=opts->(lo,hi,I)->lift(ambient basis(lo,hi,cotangentCohomology1Mod(gens I),opts), ring I)
cotangentCohomology1 (InfiniteNumber,ZZ,Ideal):=opts->(lo,hi,I)->lift(ambient basis(lo,hi,cotangentCohomology1Mod(gens I),opts), ring I)
cotangentCohomology1 (ZZ,InfiniteNumber,Ideal):=opts->(lo,hi,I)->lift(ambient basis(lo,hi,cotangentCohomology1Mod(gens I),opts), ring I)



cotangentCohomology2=method(TypicalValue=>Matrix,Options=>{SourceRing=>null})
cotangentCohomology2Mod:=F->(
     A:=ring F/image F;
     R:=gens ker F;
     kos:=koszul(2,F);
     (Hom((image R/image kos),A)/(image substitute(transpose R,A))))


cotangentCohomology2 Matrix:=opts->F->lift(ambient basis(cotangentCohomology2Mod F,opts),ring F)
cotangentCohomology2 (ZZ,Matrix):=opts->(deg,F)->lift(ambient basis(deg,cotangentCohomology2Mod(F),opts),ring F)
cotangentCohomology2 (List,Matrix):=opts->(deg,F)->lift(ambient basis(deg,cotangentCohomology2Mod(F),opts),ring F)
cotangentCohomology2 (ZZ,ZZ,Matrix):=opts->(lo,hi,F)->lift(ambient basis(lo,hi,cotangentCohomology2Mod(F),opts),ring F)
cotangentCohomology2 (InfiniteNumber,ZZ,Matrix):=opts->(lo,hi,F)->lift(ambient basis(lo,hi,cotangentCohomology2Mod(F),opts),ring F)
cotangentCohomology2 (ZZ,InfiniteNumber,Matrix):=opts->(lo,hi,F)->lift(ambient basis(lo,hi,cotangentCohomology2Mod(F),opts),ring F)

cotangentCohomology2 Ideal:=opts->I->lift(ambient basis(cotangentCohomology2Mod gens I,opts),ring I)
cotangentCohomology2 (ZZ,Ideal):=opts->(deg,I)->lift(ambient basis(deg,cotangentCohomology2Mod(gens I),opts),ring I)
cotangentCohomology2 (List,Ideal):=opts->(deg,I)->lift(ambient basis(deg,cotangentCohomology2Mod(gens I),opts),ring I)
cotangentCohomology2 (ZZ,ZZ,Ideal):=opts->(lo,hi,I)->lift(ambient basis(lo,hi,cotangentCohomology2Mod(gens I),opts),ring I)
cotangentCohomology2 (InfiniteNumber,ZZ,Ideal):=opts->(lo,hi,I)->lift(ambient basis(lo,hi,cotangentCohomology2Mod(gens I),opts),ring I)
cotangentCohomology2 (ZZ,InfiniteNumber,Ideal):=opts->(lo,hi,I)->lift(ambient basis(lo,hi,cotangentCohomology2Mod(gens I),opts),ring I)

normalMatrix=method(TypicalValue=>Matrix,Options=>{SourceRing=>null})
normalMatrix Matrix:=opts->F->lift(ambient basis(Hom(ideal F, ring F/ideal F),opts), ring F)
normalMatrix (ZZ,Matrix):=opts->(deg,F)->lift(ambient basis(deg,Hom(ideal F, ring F/ideal F),opts), ring F)
normalMatrix (List,Matrix):=opts->(deg,F)->lift(ambient basis(deg,Hom(ideal F, ring F/ideal F),opts), ring F)
normalMatrix (ZZ,ZZ,Matrix):=opts->(lo,hi,F)->lift(ambient basis(lo,hi,Hom(ideal F, ring F/ideal F),opts), ring F)
normalMatrix (InfiniteNumber,ZZ,Matrix):=opts->(lo,hi,F)->lift(ambient basis(lo,hi,Hom(ideal F, ring F/ideal F),opts), ring F)
normalMatrix (ZZ,InfiniteNumber,Matrix):=opts->(lo,hi,F)->lift(ambient basis(lo,hi.Hom(ideal F, ring F/ideal F),opts), ring F)

normalMatrix Ideal:=opts->I->lift(ambient basis(Hom(I, ring I/I),opts), ring I)
normalMatrix (ZZ,Ideal):=opts->(deg,I)->lift(ambient basis(deg,Hom(I, ring I/I),opts), ring I)
normalMatrix (List,Ideal):=opts->(deg,I)->lift(ambient basis(deg,Hom(I, ring I/I),opts), ring I)
normalMatrix (ZZ,ZZ,Ideal):=opts->(lo,hi,I)->lift(ambient basis(lo,hi,Hom(I, ring I/I),opts), ring I)
normalMatrix (InfiniteNumber,ZZ,Ideal):=opts->(lo,hi,I)->lift(ambient basis(lo,hi,Hom(I, ring I/I),opts), ring I)
normalMatrix (ZZ,InfiniteNumber,Ideal):=opts->(lo,hi,I)->lift(ambient basis(lo,hi.Hom(I, ring I/I),opts), ring I)
     
     

     
----------------------------------------------------------------------------
-- Stuff to lift deformation equation solutions
----------------------------------------------------------------------------
--auxiliary function to get lowest order degrees and terms of obstruction equations
lowestOrderTerms:=(F,G,C,n,d)->(
     if n==1 then (lowG:=map(source C_0,target F_0,0);
     	  lowDeg:=toList(d:0)) 
     else (lowDeg=apply(toList(0..(d-1)),i->min ({n-2}|positions(G,g->g^{i}!=0)));
    	  lowG=matrix(apply(toList(0..(d-1)),i->{(G_(lowDeg_i))^{i}})));
     (lowDeg,lowG))

--interchanges coefficients with variables
varSwap:=R->((coefficientRing coefficientRing R)[gens R][gens coefficientRing R])

liftDeformation=method (Options=>{SanityCheck=>true,Verbose=>0})


liftDeformation(List,List,List,List):= opts->(F,R,G,C)->(
     n:=#F-1; --order so far
     d:=numgens source C_0; --number of obstructions
     if n<1 then error "Need order at least one";
     -- find lowest order terms of obstruction equations
     if d>0 then (lowDeg,lowG):=lowestOrderTerms(F,G,C,n,d)
     else lowG=map(source C_0,target F_0,0); --unobstructed case
     T:=ring F_0;
     A:=T/ideal (F_0|transpose lowG); --setup a common ring
     if opts#Verbose > 3 then print "Calculating residual terms";
     fterms:=sum(apply(toList(1..n),i->F_i*R_(n+1-i))); --order n+1 terms
     eterms:=sum(apply(toList(1..(n-2)),i->C_i*G_(n-1-i))); -- terms from base equations
     rem:=substitute(((transpose fterms)+eterms),A); --reduce modulo our ideal
     if opts#Verbose >3 then print "Lifting Family";
     lfam:=rem//(substitute((transpose R_0),A));     
     FO:=F|{-transpose lift(lfam,T)}; --lift the family
     if opts#Verbose >3 then print "Calculating Obstruction Equations";
     obstructions:=rem-((sub(transpose R_0,A))*lfam);
     B:=varSwap T;
     clist:=coefficients(sub(C_0,B));
     coeff:=(coefficients(sub(obstructions,B),Monomials=>clist_0))_1;
     NG:=-sub(coeff//clist_1,T);     
     COC:=C_0;
     GO:=G|{NG};
     if opts#Verbose >3 then print "Lifting Relations and Coefficients";
     lrelco:=((fterms+FO_(n+1)*R_0)+transpose(eterms+COC*NG))//(F_0|transpose lowG);
     RO:=R|{-lift(lrelco^(toList(0..(numgens target R_0)-1)),T)}; --lift relations
     NC:=-transpose lift(lrelco^(toList((numgens target R_0)..(numgens target R_0)+d-1)),T);
     if n==1 then CO:={COC} else (
     	  COtriv:=C|{map(target C_0,source C_0,0)};
	  if d==0 then CO=COtriv
	  else CO={COC}|apply(toList(1..(n-1)),i->(
		    map(target C_0,source C_0,matrix apply(toList(0..(d-1)),j->(
			      if lowDeg_j==n-1-i then NC_j else (COtriv_i)_j)
			 )
		    )))
          ); --correct the entries of coefficient matrices
     if opts#Verbose>3 and opts#SanityCheck	   then print "Doing Sanity Check";
     if opts#SanityCheck then if not 
		(transpose(sum(apply(toList(0..(n+1)),i->FO_i*RO_(n+1-i))))+sum(apply(toList(0..(n-1)),i->CO_i*GO_(n-1-i))))==0 then error "Something is wrong"; 
     (FO,RO,GO,CO))


--auxiliary function to find the action of  T1 on liftings     
correctionMatrix=method()

correctionMatrix(Matrix,Matrix):=(F1,R1)->(
     T:=ring F1;
     params:=gens T;
     L:=apply(params,s->(
	sublist:=apply(params,r->(if r==s then  r=>1 else r=>0));
	{sub(sub(F1,sublist),T),sub(sub(R1,sublist),T)}));
     M:=matrix {apply(L,l->transpose(l_0*R1+F1*l_1))}; --changes to obstructions
     (M,L))


correctDeformation=method (Options=>{SanityCheck=>true,Verbose=>0})


correctDeformation(Sequence,Matrix,List):=  opts -> (S,M,L)->(
     (F,R,G,C):=S; --we have to do things this way since we can only have at most four arguments
     n:=#F-1; --order so far
     d:=numgens source C_0; --number of obstructions
     if n<2 then error "Need order at least two";
     -- find lowest order terms of obstruction equations
     (lowDeg,lowG):=lowestOrderTerms(F,G,C,n,d);
     T:=ring F_0;
     A:=T/ideal (F_0|transpose lowG); --setup a common ring
     if opts#Verbose > 3 then print "Calculating next order residual terms";
     fterms:=sum(apply(toList(1..n),i->F_i*R_(n+1-i))); --order n+1 terms
     eterms:=sum(apply(toList(1..(n-2)),i->C_i*G_(n-1-i))); -- terms from base equations
     rem:=substitute(((transpose fterms)+eterms),A); --reduce modulo our ideal
     if opts#Verbose >3 then print "Trying to kill obstructions";
     kobseq:=rem//(substitute((transpose R_0)|M,A));
     CM:=-lift(kobseq^(toList((numgens target R_0)..(numgens target R_0)+(numgens source M)-1)),T); --here is how to perturb F
     if opts#Verbose >3 then print "Adjusting family and relations";
     FC:=sum apply(toList(0..#L-1),i->CM_(i,0)*(L_i_0));
     RC:=sum apply(toList(0..#L-1),i->CM_(i,0)*(L_i_1));
     FO:=drop(F,-1)|{F_n+FC};
     RO:=drop(R,-1)|{R_n+RC};
     if opts#Verbose>3 and opts#SanityCheck	   then print "Doing Sanity Check";
     if opts#SanityCheck then if not 
		(transpose(sum(apply(toList(0..(n)),i->FO_i*RO_(n-i))))+sum(apply(toList(0..(n-2)),i->C_i*G_(n-2-i))))==0 then error "Something is wrong"; 
     (FO,RO))

correctDeformation(List,List,List,List):=  opts -> (F,R,G,C)->(
     (M,L):=correctionMatrix(F_1,R_1);
     correctDeformation((F,R,G,C),M,L))

--methodfunction for finding describing first order deformations and relations
firstOrderDeformations=method(Options=>{SanityCheck=>true,DefParam=>defaultdefparam})
firstOrderDeformations(Matrix,Matrix,Matrix):=  opts -> (F,R,T1)->(
     if T1==0 then return ({F,0*F},{R,0*R}); -- if rigid, nothing to do
     n:=numgens source T1; --number of deformation parameters
     defparam:=opts#DefParam; --deformation parameter name
--     T:=(coefficientRing ring F)[(gens ring F)|toList((value defparam)_1..(value defparam)_n),Degrees=>(degrees ring F)|((degrees T1)_1)];
--     T=T/sub(ideal ring F,T);
     T:=(ring F)[(value defparam)_1..(value defparam)_n,Join=>false,Degrees=>(apply((degrees T1)_1,i->-1*i))]; --setup ring with parameters
	--T:=(ring F)[(value defparam)_1..(value defparam)_n]; --setup ring with parameters
     FO:={substitute(F,T),(matrix{toList((value defparam)_1..(value defparam)_n)})*transpose substitute(T1,T)}; --first order family
     RO:={substitute(R,T),(-FO_1*substitute(R,T))//FO_0}; --first order relations
     if opts#SanityCheck then if not (FO_0*RO_1+FO_1*RO_0)==0 then error "Relations don't lift";
     (FO,RO))     

----------------------------------------------------------------------------
-- Iterated lifting methods
----------------------------------------------------------------------------
versalopts:={HighestOrder=>20,Verbose=>0,SanityCheck=>true, PolynomialCheck=>true,SmartLift=>true,CorrectionMatrix=>"auto",DefParam=>defaultdefparam,CacheName=>null}
versalDeformation=method(Options=>versalopts) 
localHilbertScheme=method(Options=>versalopts)


versalDeformation (List,List,List,List):= opts ->(f,r,g,c)->(
     cachename:=opts#CacheName;
     if cachename===null then cachename=(f_0).cache; 
     if not class coefficientRing ring f_0===PolynomialRing then (
	  print "No deformation parameters!";
	  return (f,r,g,c));
     ord:=-1+opts#HighestOrder;
     (F,R,G,C):=(f,r,g,c);
     if opts#SmartLift then (
	  if (opts#CorrectionMatrix==="auto") then (M,L):=correctionMatrix(F_1,R_1) else (M,L)=opts#CorrectionMatrix);
     i:=#F-2;
     polysol:=false;
     timestop:=false;
     if opts#Verbose >0 then print "Starting lifting";
     while (i<ord) do (
	  if opts#Verbose >1 then print ("Order "|toString(i+2));
	  (F,R,G,C)=(liftDeformation(F,R,G,C,Verbose=>opts#Verbose,SanityCheck=>opts#SanityCheck));
  	 if opts#SmartLift and numgens source C_0>0 then 
		(F,R)=correctDeformation((F,R,G,C),M,L,Verbose=>opts#Verbose,SanityCheck=>opts#SanityCheck);
	  i=i+1;
	  if opts#PolynomialCheck then ( --check if solution lifts to polynomial ring
	       if opts#Verbose>3 then print "Checking polynomial lifting";
	       if F_(-1)==0 and R_(-1)==0 and G_(-1)==0 then (
	       	    if transpose((sum F)*(sum R))+(sum C)*(sum G)==0 then (
			 i=ord;
			 polysol=true;
			 if opts#Verbose>0 then print "Solution is polynomial";
			 )
		    )
	       );
	cachename#VersalDeformationResults=(F,R,G,C);  
	);
     if not polysol and not timestop then print "Warning: calculation terminated since HighestOrder has been reached.";
     (F,R,G,C))

versalDeformation (Matrix,Matrix,Matrix):= opts ->(F0,T1,T2)->(
     cachename:=opts#CacheName;
     if cachename===null then cachename=(F0).cache; 
     ord:=-1+opts#HighestOrder;
     if opts#Verbose >1 then print "Calculating first order relations";
     (F,R):=firstOrderDeformations(F0,gens ker F0,T1,SanityCheck=>opts#SanityCheck,DefParam=>opts#DefParam);
     if opts#Verbose >1 then print "Calculating standard expressions for obstructions";
     A:=(ring F_0)/(image F_0);
     NT2:=lift(sub(T2,A)%sub(transpose R_0,A),ring F_0);
     C:={NT2};
     G:={};
     versalDeformation(F,R,G,C,HighestOrder=>opts#HighestOrder,Verbose=>opts#Verbose,
	  SanityCheck=>opts#SanityCheck, PolynomialCheck=>opts#PolynomialCheck,SmartLift=>opts#SmartLift,
	  CorrectionMatrix=>opts#CorrectionMatrix,CacheName=>cachename))
   
versalDeformation Matrix:=  opts ->F0->(
     if opts#Verbose > 0 then print "Calculating first order deformations and obstruction space";
     versalDeformation(F0,CT^1(F0),CT^2(F0),opts))

localHilbertScheme Matrix:=  opts ->F0->(
     if opts#Verbose > 0 then print "Calculating first order deformations and obstruction space";
     versalDeformation(F0,normalMatrix(0,F0),CT^2(0,F0),opts))

---------------------------------------
-- DOCUMENTATION
---------------------------------------


beginDocumentation()

document {
     Key => VersalDeformations,
     Headline => "calculating versal deformations and local Hilbert schemes",
     PARA{
     "This package provides tools for calculating tangent and obstruction spaces as well as
     power series solutions for deformation problems involving isolated singularities and projective schemes."},
 
     
     
    PARA{}, "A basic description of the package's approach to deformation problems can
    be found at the documentation node for ",TO versalDeformation,". 
    For details and mathematical background see ",
     
     UL {
	  {"Jan Stevens, ",EM "Computing Versal Deformations", ", Experimental Mathematics Vol. 4 No. 2, 1994."}
	 },

     PARA{"The numerous examples presented in the documentation nodes 
	  for ",TO versalDeformation," and ",TO localHilbertScheme," are classical
	  deformation problems, considered in the following articles:"},
     UL {
	  {"[Al97] Klaus Altmann, ",EM "The versal deformation of an isolated Gorenstein 
	       singularity",
	   ", Inventiones Mathematicae Vol. 128 No. 3, 443-479 1997."},
      {"[CS10] Dustin Cartwright and Bernd Sturmfels, ",EM "The Hilbert scheme of the diagonal
	   in a product of projective spaces", ", International Mathematics Research 
	   Notices Vol. 2010 No. 9, 1741-1771."},
	{"[PS85] Ragni Piene and Michael Schlessinger, ",EM "On the Hilbert scheme compactification
	of the space of twisted cubic curves", ", American Journal of Mathematics, Vol. 107
	No. 4, 761-774, 1985."},
	{"[Pi74] Henry Pinkham, ",EM "Deformations of algebraic varieties with G_m action",
	      ", Asterisque 20, 1974."},   
      },
 
      PARA{"The author thanks Jan Christophersen for helpful hints,
	   especially regarding the computation of ",TEX///$T^2$///,"."},
 }


document {
     Key =>{localHilbertScheme,(localHilbertScheme,Matrix),
	  [localHilbertScheme,PolynomialCheck],
	  [localHilbertScheme,HighestOrder],
	  [localHilbertScheme,SanityCheck],
	  [localHilbertScheme,SmartLift],
	  [localHilbertScheme,DefParam],
	  [localHilbertScheme,CacheName],
	  [localHilbertScheme,CorrectionMatrix]
	  },
     Headline => "computes a power series representation of the local Hilbert scheme",
     Usage=>"(F,R,G,C) = localHilbertScheme(F0)",
     Inputs=>{"F0" => Matrix,},
     Outputs=>{"F" => {ofClass List, " of matrices"},
		"R"=> {ofClass List, " of matrices"},
		"G"=> {ofClass List, " of matrices"},
		"C" =>{ofClass List, " of matrices"},},
     
     PARA{TT "F0"," should  be a matrix with homogeneous entries over some polynomial ring with one row."},

     
     PARA{"Each element of the sequence ", TT "(F,R,G,C)"," is a list of matrices
	  in increasing powers of the deformation parameter specified by ",TO DefParam,". Their
	  sums satisfy the deformation equation ",
	  TT "transpose ((sum F)*(sum R))+(sum C)*(sum G)==0"," up to powers of the deformation parameter equal to the
	  length of ",TT "F",". Furthermore,
	  ",TT "F_0=F0",", ",TT "R_0=gens ker F0",", ",TT "C_0=T^2(0,F_0)", " and ",TT "F_1"," consists 
	  of first order perturbations corresponding to ",TT "normalMatrix(0,F0)",". Thus,
	   ",TT "F"," and ",TT "G"," represent a universal family and
	  local analytic equations for the Hilbert scheme." 
	  }, 
	
	 	PARA{"Several options are available to control the termination of the calculation. 
	The calculation will terminate at the very latest after reaching order equal to 
	the option ",	TO HighestOrder,
	", which has default value ",TT "20",". If this order is reached, a warning message is generated.
	 If ",TO PolynomialCheck," is set to ",
        TO true,", as is the default, then the algorithm will check if the present solution lifts 
	to infinite order
	and terminate if this is the case. If ",TO SanityCheck," is set to ",TO true,", as is the
	default, then
	the algorithm will check that the present solution really does solve the deformation
	equation, and terminate with an error if this is not the case."},
	  
	PARA{"The option ",TO Verbose," may be
	used to control the verbosity of the output. Its value should be an integer, with higher values corresponding
	to more verbose output. Default value is ",TT "0","."}, 
	
	PARA{"The option ", TO SmartLift," is also available, which controls whether the algorithm
	     spends extra time trying to find liftings which introduce no new obstructions at the next 
	     highest order. By default, this option is enabled. The option ",TO CorrectionMatrix," may be used to control which liftings 
	     are considered."},	
	     
	PARA{"After each step of lifting, the solution ",TT "(F,R,G,C)"," to the deformation equation is cached. By default, it is stored in ",TT "F0.cache#VersalDeformationResults"," but may stored elsewhere by setting the option ",TO CacheName," to something other than ",TO null,"."}, 

 	PARA {"For example, consider a degenerate twisted cubic curve, see ",TO2 {VersalDeformations,"[PS85]"},
	     ":"},
     	EXAMPLE {"S=QQ[x,y,z,w];",
	"F0=matrix {{x*z,y*z,z^2,x^3}}",
	"(F,R,G,C)=localHilbertScheme(F0,Verbose=>2);"
	},
     	PARA {"Local equations for the Hilbert scheme are thus given by"},
	EXAMPLE {"T=ring first G;",
	     "sum G"},
	Caveat => {"The output may not be the local Hilbert scheme if standard comparison theorems 
	     do not hold for the ideal generated by ",TT "FO","."}, 
     }

document {
     Key =>{(versalDeformation,List,List,List,List)
	  	  },
     Usage=>"(F,R,G,C) = versalDeformation(F0)\n
     (F,R,G,C) = versalDeformation(F0,T1,T2)",
     Inputs=>{"f"=>List,"r"=>List,"g"=>List,"c"=>List},
     Outputs=>{"F" => {ofClass List, " of matrices"},
		"R"=> {ofClass List, " of matrices"},
		"G"=> {ofClass List, " of matrices"},
		"C" =>{ofClass List, " of matrices"},},
     Headline => "continues calculation of  a versal deformation",
     PARA {"Each element of the sequence ", TT "(F,R,G,C)"," is a list of matrices
	  in increasing powers of the deformation parameter. The input ",TT "(f,r,g,c)"," should be a valid solution to the deformation equation output in
     the form done by ",TO (versalDeformation,Matrix,Matrix,Matrix),". This function continues lifting this solution
     to higher order. All options described for ",TO (versalDeformation,Matrix)," may be used 
     to the same effect."},
	PARA{"After each step of lifting, the solution ",TT "(F,R,G,C)"," to the deformation equation is cached. By default, it is stored in ",TT "(f_0).cache#VersalDeformationResults"," but may stored elsewhere by setting the option ",TO CacheName," to something other than ",TO null,"."}, 

     PARA {"This function is especially useful for finding one-parameter families when the versal family
     is too complicated to calculate."},
             PARA {"We may consider the example of the versal deformation of a degree 12 toric Fano threefold:"},
     EXAMPLE {"S=QQ[x1,x2,x3,x4,x5,x6,y1,y2,z];",
	  "I=ideal {x1*x4-z^2,x2*x5-z^2,x3*x6-z^2,x1*x3-z*x2,x2*x4-z*x3,x3*x5-z*x4,x4*x6-z*x5,x5*x1-z*x6,x6*x2-z*x1,y1*y2-z^2};",
	"F0=gens I;",
	"(F,R,G,C)=versalDeformation(F0,CT^1(0,F0),CT^2(0,F0),HighestOrder=>2);"},
     PARA {"We stop the calculation at order 2, since in this case, the solution to the deformation
equation calculated by the lifting algorithm will not be polynomial. Equations for the tangent cone at the origin of a versal base space are"},
     EXAMPLE {"T=ring first G;",
	  "G_0"},
     PARA {"This decomposes into three components:"},
     EXAMPLE {"decompose ideal G_0"},
     PARA {"We now find a one-parameter deformation onto the component of highest dimension:"},
     EXAMPLE {"A=(coefficientRing ring F_0)[s];",
	"sublist=apply(gens T,v->(if v==t_19 or v==t_20  then return v=>s;v=>0));",
	"f=apply(F,i->sub(i,sublist));",
	"r=apply(R,i->sub(i,sublist));",
	"g=apply(G,i->sub(i,sublist));",
	"c=apply(C,i->sub(i,sublist));",
	"(F,R,G,C)=versalDeformation(f,r,g,c);",
	"sum F"}   
}

document{
     	 Key=>{versalDeformation},
	      Headline => "computes a power series representation of a versal deformation",
	      PARA{"Here we provide an overview of our approach to solving deformation problems.
	      For details on using the command ",TT "versalDeformation",", please see the documentation 
	      links below.  
	      The most basic use of the method is via ",TO (versalDeformation,Matrix),", which computes
	      the versal deformation of an isolated singularity. We restrict to this case in the following
	      overview."
	      },
	      PARA{TEX///First we fix some notation. Let $S$ be a polynomial ring over 
		   some field $k$, and let $I$ be an ideal of $S$ defining a 
		   scheme $X=$Spec $S/I$ with isolated singularities. 
		   Consider a free resolution of $S/I$:
$$
\ldots \to  S^l \to S^m \to S \to S/I\to 0
$$
with differentials $R^0:S^l\to S^m$ and $F^0:S^m\to S$.
///,
TEX///Let $\phi_i\in$Hom$(S^m/ $Im$ R^0,S)$ for $i=1,\ldots,n$ represent a basis of 
$T^1(S/I)\cong$Hom$(S^m/ $Im$ R^0,S)/ $Jac$ F^0$.
We introduce deformation parameters $t_1,\ldots,t_n$ with the ring $T=S[t_1,\ldots,t_n]$ and consider the map
$F^1: T^m\to T$ defined as 
$F^1=F^0+\sum t_i\phi_i.$
Let $a$ be the ideal generated by $t_1,\ldots,t_n$.
It follows that there is a map $R^1: T^l\to T^m$ with 
$R^1= R^0$ mod $a$ satisfying the first order deformation equation
$F^1R^1= 0$ mod $a^2$.///},
             
	      PARA{TEX///Our goal is to lift this equation to higher order, that is, for each $i>0$,
		    to find $F^i: T^m\to T$ with $F^i=F^{i-1}$ mod $a^{i}$ 
		    and $R^i: T^l\to T^m$ with $R^i= R^{i-1}$ mod $a^i$ satisfying 
		    $F^iR^i= 0$ mod $a^{i+1}$. In general, there are obstructions to doing this, 
		    governed by the $d$-dimensional $k$ vector space $T^2(S/I)$. Thus, we 
		    instead aim to solve
$$
	(F^iR^i)^{tr}+C^{i-2}G^{i-2}= 0
$$
mod $a^{i+1}$.
Here, $G^{i-2}: k[t]\to k[t]^d$ and $C^{i-2}: T^d\to T^l$ are congruent modulo 
$a^i$ to $G^{i-3}$ and $C^{i-3}$, respectively. Furthermore, we require that $G^i$ 
and $C^i$ vanish for $i<0$, and $C^0$ is of the form $V D$, where 
$V\in$Hom$(S^d,S^l)$ gives representatives of a basis for $T^2(S/I)$ and 
$D\in$Hom$(S^d,S^d)$ is a diagonal matrix.
The $G^i$ now give equations for the miniversal base space of $X$.///},

            PARA{TEX///Our implementation solves the above equation step by step.  
		 Given a solution $(F^i,R^i,G^{i-2},C^{i-2})$ modulo $a^{i+1}$, the package 
		 uses Macaulay2's built in matrix quotients to first solve for $F^{i+1}$ and 
		 $G^{i-1}$ (by working over the ring $T/I+$ Im $(G^{i-2})^{tr} +a^{i+2}$) 
		 and then solve for $R^{i+1}$ and $C^{i-1}$. For the actual computation, 
		 we avoid working over quotient rings involving high powers of 
		 $a$ by representing the $(F^i,R^i,G^{i-2},C^{i-2})$ as lists of matrices 
		 which keep track of the orders of the $t_j$ involved.///}
	 }
document {
     Key =>{(versalDeformation,Matrix,Matrix,Matrix),
	  	  },
     Usage=>"(F,R,G,C) = versalDeformation(F0)\n
     (F,R,G,C) = versalDeformation(F0,T1,T2)",
     Inputs=>{"F0" => Matrix, "T1" => Matrix, "T2" => Matrix},
     Outputs=>{"F" => {ofClass List, " of matrices"},
		"R"=> {ofClass List, " of matrices"},
		"G"=> {ofClass List, " of matrices"},
		"C" =>{ofClass List, " of matrices"},},
     Headline => "computes a power series representation of a versal deformation",
     
     PARA{TT "F0",", ",TT "T1",", and ",TT "T2"," should all be matrices over some common
	  polynomial ring, and ",TT "F0"," should have one row."}, 
     
     PARA{"Each element of the sequence ", TT "(F,R,G,C)"," is a list of matrices
	  in increasing powers of the deformation parameter specified by ",TO DefParam,". Their
	  sums satisfy the deformation equation ",
	  TT "transpose ((sum F)*(sum R))+(sum C)*(sum G)==0"," up to powers of the deformation parameter equal to the
	  length of ",TT "F",". Furthermore,
	  ",TT "F_0=F0",", ",TT "R_0=gens ker F0",", the columns of ",TT "C_0"," are multiples
	  of those of ",TT "T2", " and ",TT "F_1"," consists 
	  of first order perturbations corresponding to ",TT "T1",". Thus,
	  if ",TT "T1"," and ",TT "T2"," are tangent and obstruction spaces for some 
	  deformation functor, then ",TT "F"," and ",TT "G"," represent a versal family and
	  equations for a versal base space." 
	  }, 
	
	PARA{"Several options are available to control the termination of the calculation. 
	The calculation will terminate at the very latest after reaching order equal to 
	the option ",	TO HighestOrder,
	", which has default value ",TT "20",". If this order is reached, a warning message is generated.
	If ",TO PolynomialCheck," is set to ",
        TO true,", as is the default, then the algorithm will check if the present solution lifts 
	to infinite order
	and terminate if this is the case. If ",TO SanityCheck," is set to ",TO true,", as is the
	default, then
	the algorithm will check that the present solution really does solve the deformation
	equation, and terminate with an error if this is not the case."},
	  
	PARA{"The option ",TO Verbose," may be
	used to control the verbosity of the output. Its value should be an integer, with higher values corresponding
	to more verbose output. Default value is ",TT "0","."}, 
	
	PARA{"The option ", TO SmartLift," is also available, which controls whether the algorithm
	     spends extra time trying to find liftings which introduce no new obstructions at the next 
	     highest order. By default, this option is enabled. The option ",TO CorrectionMatrix," may be used to control which liftings 
	     are considered."},	
	PARA{"After each step of lifting, the solution ",TT "(F,R,G,C)"," to the deformation equation is cached. By default, it is stored in ",TT "F0.cache#VersalDeformationResults"," but may stored elsewhere by setting the option ",TO CacheName," to something other than ",TO null,"."}, 
	
	     
     
     PARA{"We may use this method to compute local multigraded Hilbert schemes. Here, we consider
	  the Borel fixed ideal for the multigraded Hilbert scheme of the diagonal in
	  a product of three projective planes, see ",TO2 {VersalDeformations,"[CS10]"},":"},
     EXAMPLE{
	  "S=QQ[x1,x2,x3,y1,y2,y3,z1,z2,z3,Degrees=>
	  {{1,0,0},{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1},{0,0,1}}];",
	  "I=ideal {y1*z2, x1*z2, y2*z1, y1*z1, x2*z1, x1*z1, x1*y2, x2*y1,
	   x1*y1, x2*y2*z2};",
	  "(F,R,G,C)=versalDeformation(gens I,normalMatrix({0,0,0},gens I),
	  CT^2({0,0,0},gens I),Verbose=>2);"},
     PARA {"Local equations for the multigraded Hilbert scheme  are"},
     EXAMPLE {"T=ring first G;",
	  "sum G"},
     PARA {"At this point, the multigraded Hilbert scheme has 7 irreducible components:"},
     EXAMPLE {"# primaryDecomposition ideal sum G"},	      
      }



document {
     Key =>{
	  (versalDeformation,Matrix),
	  [versalDeformation,SanityCheck],
   	  [versalDeformation,PolynomialCheck],
	  [versalDeformation,HighestOrder],
	  [versalDeformation,SmartLift],
	  [versalDeformation,CacheName],
  	  [versalDeformation,DefParam],
	    [versalDeformation,CorrectionMatrix],
  	 	  	  },
     Usage=>"(F,R,G,C) = versalDeformation(F0)",
     Inputs=>{"F0" => Matrix},
     Outputs=>{"F" => {ofClass List, " of matrices"},
		"R"=> {ofClass List, " of matrices"},
		"G"=> {ofClass List, " of matrices"},
		"C" =>{ofClass List, " of matrices"},},
     Headline => "computes a power series representation of a versal deformation",
     
     PARA{TT "F0", " should  be a matrix over some 
	  polynomial ring with one row."},
	  
     
     PARA{"Each element of the sequence ", TT "(F,R,G,C)"," is a list of matrices
	  in increasing powers of the deformation parameter specified by ",TO DefParam,". Their
	  sums satisfy the deformation equation ",
	  TT "transpose ((sum F)*(sum R))+(sum C)*(sum G)==0"," up to powers of the deformation parameter equal to the
	  length of ",TT "F",". Furthermore,
	  ",TT "F_0=F0",", ",TT "R_0=gens ker F0",", the columns of ",TT "C_0"," are multiples
	  of the those of ",TO cotangentCohomology2,TT "(F0)",",  and ",TT "F_1"," consists 
	  of first order perturbations corresponding to the basis of ",TO cotangentCohomology1,TT "(F0)",
	  ". Thus, ",TT "F"," and ",TT "G"," represent a versal family and
	  equations for a versal base space for the scheme cut out by the columns of ",TT "F0","." 
	  }, 
	
	PARA{"Several options are available to control the termination of the calculation. 
	The calculation will terminate at the very latest after reaching order equal to 
	the option ",	TO HighestOrder,
	", which has default value ",TT "20",". If this order is reached, a warning message is generated.
	If ",TO PolynomialCheck," is set to ",
        TO true,", as is the default, then the algorithm will check if the present solution lifts 
	to infinite order
	and terminate if this is the case. If ",TO SanityCheck," is set to ",TO true,", as is the
	default, then
	the algorithm will check that the present solution really does solve the deformation
	equation, and terminate with an error if this is not the case."},
	  
	PARA{"The option ",TO Verbose," may be
	used to control the verbosity of the output. Its value should be an integer, with higher values corresponding
	to more verbose output. Default value is ",TT "0","."}, 
	
	PARA{"The option ", TO SmartLift," is also available, which controls whether the algorithm
	     spends extra time trying to find liftings which introduce no new obstructions at the next 
	     highest order. By default, this option is enabled. The option ",TO CorrectionMatrix," may be used to control which liftings 
	     are considered."},	
	PARA{"After each step of lifting, the solution ",TT "(F,R,G,C)"," to the deformation equation is cached. By default, it is stored in ",TT "F0.cache#VersalDeformationResults"," but may stored elsewhere by setting the option ",TO CacheName," to something other than ",TO null,"."}, 
	
	     
     PARA {"For example, consider the cone over the rational normal curve of degree four, see ",TO2 {VersalDeformations,"[Pi74]"},":"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "F0=gens I",
	  "(F,R,G,C)=versalDeformation(F0,Verbose=>2);"},
     PARA {"Equations for a versal base space are"},
     EXAMPLE {"T=ring first G;",
	  "sum G"},
     PARA {"The versal family is given by"},
     EXAMPLE {"sum F"},
     
     PARA {"We may also consider the example of the cone over the del Pezzo surface of degree
	  six, see ",TO2 {VersalDeformations,"[Al97]"},":"},
     EXAMPLE {"S=QQ[x1,x2,x3,x4,x5,x6,z];",
	  "I=ideal {x1*x4-z^2,x2*x5-z^2,x3*x6-z^2,x1*x3-z*x2,x2*x4-z*x3,x3*x5-z*x4,x4*x6-z*x5,x5*x1-z*x6,x6*x2-z*x1};",
	"F0=gens I;",
	"(F,R,G,C)=versalDeformation(F0,Verbose=>2);"},
     PARA {"Equations for a versal base space are"},
     EXAMPLE {"T=ring first G;",
	  "sum G"},
     PARA {"The versal family is given by"},
     EXAMPLE {"sum F"}  
      } 

document {
     Key =>{liftDeformation,(liftDeformation,List,List,List,List),
	  [liftDeformation,SanityCheck]
     },
     Headline => "lift a solution of the deformation equation to the next order",
     Usage => "(F,R,G,C) = liftDeformation(f,r,g,c)",
     Inputs=> {"f"=>List,"r"=>List,"g"=>List,"c"=>List},
     Outputs=>{"F" => {ofClass List, " of matrices"},
		"R"=> {ofClass List, " of matrices"},
		"G"=> {ofClass List, " of matrices"},
		"C" =>{ofClass List, " of matrices"},},
     PARA{"Each element of the sequence ", TT "(f,r,g,c)"," is a list of matrices
	  in increasing powers of some deformation parameter. Their
	  sums satisfy the deformation equation ",
	  TT "transpose ( (sum f)*(sum r))+(sum c)*sum(g)==0"," up to powers of the deformation parameter equal to the
	  length of ",TT "f","." 
	    },
       
     PARA{"Each element of the output sequence ", TT "(F,R,G,C)"," is a list of matrices
	  in increasing powers of the deformation parameter. The first three matrices of the sequence
	  are gotten from ", TT "(f,r,g)"," by appending one
	  matrix to each list in the sequence,
	  and furthermore the columns of ",TT "C_0"," are multiples of those of ",TT "c_0",
	  ". The other matrices are chosen to satisfy
	  the deformation equation ",
	  TT "transpose ((sum F)*(sum R))+(sum C)*(sum G)==0"," up to powers of the deformation parameter equal to the
	  length of ",TT "F",", provided that there is such a solution. If ",TO SanityCheck," is set to ",TO true,", as is the
	default, then
	the algorithm will check that the lifted solution really does solve the deformation
	equation, and terminate with an error if this is not the case." 
	    },
     PARA {"For example, consider the cone over the rational normal curve of degree four, see ",TO2 {VersalDeformations,"[Pi74]"},":"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "F0=gens I",
	  "T1=cotangentCohomology1(F0);",
 	  "R0=gens ker F0;",
	  "(f,r)=firstOrderDeformations(F0,R0,T1);"
	  },
     PARA {"We now lift the first order deformations to second order:"},
     EXAMPLE{
      "A:=(ring f_0)/(image f_0);",  
      "T2=cotangentCohomology2(F0);",
      "NT2:=lift(sub(T2,A)%sub(transpose r_0,A),ring f_0);",
      "c={NT2};",
      "g={};",
      "(F,R,G,C)=liftDeformation(f,r,g,c);",
      "T=ring first F;",
      "sum F -- equations for family",
      "sum G -- base equations",},
     }

document {
     Key =>{firstOrderDeformations,(firstOrderDeformations,Matrix,Matrix,Matrix),
	  [firstOrderDeformations,SanityCheck],
     	  [firstOrderDeformations,DefParam]},
     Headline => "use tangent space to create first order peturbations and find relations",
     Usage => "(F,R) = firstOrderDeformations(F0,R0,T1)",
     Inputs => {"F0" =>Matrix, "R0"=>Matrix, "T1"=>Matrix},
     Outputs=>{"F" => {ofClass List, " of matrices"},
		"R"=> {ofClass List, " of matrices"},
		},
     PARA{TT "F0",", ",TT "R0",", and ",TT "T1"," should all be matrices over some common
	  polynomial ring, and ",TT "F0"," and ",TT "T1"," should have one row. ",TT "R0"," should be the 
	  first syzygy matrix of ",TT "F0"," and ",TT "T1"," should have the same number of
	  columns as ",TT "F0","."},
	  
     PARA{TT "F"," is a list of length two with ",TT "F_0=F0"," and ",TT "F_1"," the first
	  order perturbations corresponding to ",TT "T1",". ",TT "R"," is a list of length
	  two with ",TT "R_0=R0"," and ",TT "R_1"," such that
	  ",TT "F_0*R_1+F_1*R_0==0",". If ",TO SanityCheck," is set to ",TO true,", as is the
	default, then
	the algorithm will check that this equation is satisfied,
	 and terminate with an error if this is not the case."}, 
     PARA{"The parameters used in the perturbations may be specified by the option ",TO DefParam,"."},
         PARA {"For example, consider the cone over the rational normal curve of degree four, see ",TO2 {VersalDeformations,"[Pi74]"},":"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "F0=gens I",
	  "T1=cotangentCohomology1(F0);",
	  "R0=gens ker F0;",
	  "(F,R)=firstOrderDeformations(F0,R0,T1)"
	  },
     
     }

document {
     Key =>{correctDeformation,(correctDeformation,List,List,List,List),
	  [correctDeformation,SanityCheck],
	  (correctDeformation,Sequence,Matrix,List),
     	  },
     Headline => "correct lifting to avoid obstructions at next order",
     Usage => "(F,R) = correctDeformation(f,r,g,c)\n
     (F,R) = correctDeformation(S,M,L)",
     Inputs=> {"f"=>List,"r"=>List,"g"=>List,"c"=>List,
	  "(f,r,g,c)"=>Sequence,"M"=>Matrix,"L"=>List},
     Outputs=>{"F" => {ofClass List, " of matrices"},
     "R" => {ofClass List, " of matrices"},},
     PARA {"Each element of the sequence ", TT "(F,R)"," is a list of matrices
	  in increasing powers of the deformation parameter. ",TT "(f,r,g,c)"," should be as in the output of ", TO liftDeformation ," and ", TT "(M,L)",
	  "should be as in the output of ", TO correctionMatrix,". If the latter are omitted, they are replaced
	  by ", TT "(M,L)=correctionMatrix(f_1,r_1)","."},
     PARA {TT "correctDeformation"," perturbs the last entries of ",TT "f"," and ",TT "r"," such that  
     if possible, the next invocation of ", TO liftDeformation," will introduce no new terms in the 
     obstruction equations.  If ",TO SanityCheck," is set to ",TO true,", as is the
	default, then
	the algorithm will check that the corrected perturbation really does solve the deformation
	equation, and terminate with an error if this is not the case."},
     PARA {"For example, consider a degenerate twisted cubic curve, see ",TO2 {VersalDeformations,"[PS85]"},":"},
     EXAMPLE {"S=QQ[x,y,z,w];",
     "F0=matrix {{x*z,y*z,z^2,x^3}};",
     	"(f,r,g,c)=localHilbertScheme(F0,Verbose=>0,HighestOrder=>2,SmartLift=>false);",
	"(liftDeformation(f,r,g,c))_2",
	"(F,R)=correctDeformation(f,r,g,c);",
	"(liftDeformation(F,R,g,c))_2"},
     Caveat=>{"If the obstruction space is zero, this will generate an error."}    
	}
     
 document{
      Key =>{correctionMatrix,(correctionMatrix,Matrix,Matrix)},
      Headline =>"calculate how first order deformations perturb obstruction vector",
      Usage => "(M,L) = correctionMatrix(F1,R1)",
      Inputs => {"F1"=>Matrix,"R1"=>Matrix},
      Outputs => {"(M,L)"=>Sequence},
      PARA {TT "F1"," should be some first order perturbations of a one-row matrix with ", TT "R1",
	   " a lift of the corresponding relations, as in the output of ", TO firstOrderDeformations,". 
	   ",TT "M"," is a matrix representing the effect of these perturbations one order higher, and ",
	   TT "L", " gives a parameter-free version of the perturbations and lifted relations."}}     
     
     
document {
     Key =>{cotangentCohomology1,
	  (cotangentCohomology1,Matrix),(cotangentCohomology1,ZZ,Matrix),
	  (cotangentCohomology1,List,Matrix),(cotangentCohomology1,InfiniteNumber,ZZ,Matrix),
	  (cotangentCohomology1,ZZ,InfiniteNumber,Matrix),(cotangentCohomology1,ZZ,ZZ,Matrix),
	  (cotangentCohomology1,Ideal),(cotangentCohomology1,ZZ,Ideal),
	  (cotangentCohomology1,List,Ideal),(cotangentCohomology1,InfiniteNumber,ZZ,Ideal),
	  (cotangentCohomology1,ZZ,InfiniteNumber,Ideal),(cotangentCohomology1,ZZ,ZZ,Ideal),
	[cotangentCohomology1,SourceRing]},
     Headline => "calculate first cotangent cohomology",
     Usage => "T1 = cotangentCohomology1(F) \n
     T1 = cotangentCohomology1(deg,F) \n
     T1 = cotangentCohomology1(lo,hi,F)",
     Inputs => {"F" => {"a ",(TO Matrix)," or an ",(TO Ideal)},  "deg" => {"a ",(TO2 {List,"list"})," or ",(TO2 {ZZ,"integer"})},
	  "lo" => {"an ",(TO2 {ZZ,"integer"})," or -",(TO infinity)},
	  "hi" => {"an ",(TO2 {ZZ,"integer"})," or ",(TO infinity)}
	  },
     Outputs=>{"T1" => Matrix},
     PARA {"The matrix ",TT "F"," must have a single row. Inputing an ideal instead has the same effect as inputing ",TT "gens F",".  The output ",TT "T1"," is a matrix
	  over the same ring as ",TT "F"," whose columns form a basis for 
	  (a graded piece of) the first cotangent cohomology
	  module of ",TT "S/I",", where ",TT "S"," is the ring of ",TT "F"," and ",TT "I",
	  " is ideal generated by the columns of ",TT "F",". Selection
	  of graded pieces is done in the same manner as with ",TO basis,". If the selected
	  pieces are infinite dimensional, an error occurs. The optional argument ",TO SourceRing," may be used in the same fashing as with ",TO basis,"."},
         PARA {"This is ",ofClass MethodFunction,", which may also be accessed via the ",TO ScriptedFunctor," ",TO CT,"."},
	  PARA {"For example, consider the cone over the rational normal curve of degree four, see ",TO2 {VersalDeformations,"[Pi74]"},":"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "T1=cotangentCohomology1(I)"},
     PARA {"The first cotangent cohomology module, and thus the tangent space of the versal deformation,
	   is four dimensional."},
     }


document { Key
     =>{cotangentCohomology2,(cotangentCohomology2,Matrix),(cotangentCohomology2,ZZ,Matrix),
     (cotangentCohomology2,List,Matrix),(cotangentCohomology2,InfiniteNumber,ZZ,Matrix),
     (cotangentCohomology2,ZZ,InfiniteNumber,Matrix),(cotangentCohomology2,ZZ,ZZ,Matrix),
     (cotangentCohomology2,Ideal),(cotangentCohomology2,ZZ,Ideal),
     (cotangentCohomology2,List,Ideal),(cotangentCohomology2,InfiniteNumber,ZZ,Ideal),
     (cotangentCohomology2,ZZ,InfiniteNumber,Ideal),(cotangentCohomology2,ZZ,ZZ,Ideal),
	[cotangentCohomology2,SourceRing]},
     Headline => "calculate second cotangent cohomology",
      Usage => "T2 = cotangentCohomology2(F) \n
     T2 = cotangentCohomology2(deg,F) \n
     T2 = cotangentCohomology2(lo,hi,F)",
     Inputs => {"F" =>{"a ",(TO Matrix)," or an ",(TO Ideal)},  "deg" => {"a ",(TO2 {List,"list"})," or ",(TO2 {ZZ,"integer"})},
	  "lo" => {"an ",(TO2 {ZZ,"integer"})," or -",(TO infinity)},
	  "hi" => {"an ",(TO2 {ZZ,"integer"})," or ",(TO infinity)}
	  },
     Outputs=>{"T2" => Matrix},
     PARA {"The matrix ",TT "F"," must have a single row.  Inputing an ideal instead has the same effect as inputing ",TT "gens F",".  The output ",TT "T2"," is a matrix
	  over the same ring as ",TT "F"," whose columns form a basis for 
	  (a graded piece of) the second cotangent cohomology
	  module of ",TT "S/I",", where ",TT "S"," is the ring of ",TT "F"," and ",TT "I",
	  " is ideal generated by the columns of ",TT "F",". Selection
	  of graded pieces is done in the same manner as with ",TO basis,". If the selected
	  pieces are infinite dimensional, an error occurs. The optional argument ",TO SourceRing," may be used in the same fashing as with ",TO basis,"."},
     PARA {"This is ",ofClass MethodFunction,", which may also be accessed via the ",TO ScriptedFunctor," ",TO CT,"."},
     PARA {"For example, consider the cone over the rational normal curve of degree four, see ",TO2 {VersalDeformations,"[Pi74]"},":"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "T2=cotangentCohomology2(I)"},
     PARA {"The second cotangent cohomology module is three dimensional. Thus, the base space of the
	  versal deformation is cut out by (at most) three equations."
	   },
      PARA {"We also consider the graded example of a degenerate twisted cubic curve, see ",TO2 {VersalDeformations,"[PS85]"},":"},
     EXAMPLE {"S=QQ[x,y,z,w];",
	  "F=matrix {{x*z,y*z,z^2,x^3}}",
	  "T2=cotangentCohomology2(0,F)"},
     PARA {"The degree zero component of the second cotangent cohomology module
	  is four dimensional. Thus the Hilbert scheme is (locally analytically)
	   cut out by (at most) four equations."},
      }

document {
     Key =>{normalMatrix,(normalMatrix,Matrix),(normalMatrix,ZZ,Matrix),
	  (normalMatrix,List,Matrix),(normalMatrix,InfiniteNumber,ZZ,Matrix),
	  (normalMatrix,ZZ,InfiniteNumber,Matrix),(normalMatrix,ZZ,ZZ,Matrix),
	  (normalMatrix,Ideal),(normalMatrix,ZZ,Ideal),
	  (normalMatrix,List,Ideal),(normalMatrix,InfiniteNumber,ZZ,Ideal),
	  (normalMatrix,ZZ,InfiniteNumber,Ideal),(normalMatrix,ZZ,ZZ,Ideal),
		[normalMatrix,SourceRing]},
     Headline => "calculate normal module",
      Usage => "N = normalMatrix(F) \n
     N = normalMatrix(deg,F) \n
     N = normalMatrix(lo,hi,F)",
     Inputs => {"F" =>{"a ",(TO Matrix)," or an ",(TO Ideal)},  "deg" => {"a ",(TO2 {List,"list"})," or ",(TO2 {ZZ,"integer"})},
	  "lo" => {"an ",(TO2 {ZZ,"integer"})," or -",(TO infinity)},
	  "hi" => {"an ",(TO2 {ZZ,"integer"})," or ",(TO infinity)}
	  },
     Outputs=>{"N" => Matrix},
     PARA {"The matrix ",TT "F"," must have a single row.  Inputing an ideal instead has the same effect as inputing ",TT "gens F",".  The output ",TT "N"," is a matrix
	  over the same ring as ",TT "F"," whose columns form a basis for 
	  (a graded piece of) the normal module ",TT "Hom(I,S/I)",",
	  where ",TT "S"," is the ring of ",TT "F"," and ",TT "I",
	  " is ideal generated by the columns of ",TT "F",". Selection
	  of graded pieces is done in the same manner as with ",TO basis,". If the selected
	  pieces are infinite dimensional, an error occurs. The optional argument ",TO SourceRing," may be used in the same fashing as with ",TO basis,"."},
     PARA {"For example, consider a degenerate twisted cubic curve, see ",TO2 {VersalDeformations,"[PS85]"},":"},
     EXAMPLE {"S=QQ[x,y,z,w];",
	  "F=matrix {{x*z,y*z,z^2,x^3}}",
	  "N=normalMatrix(0,F)"},
     PARA {"The degree zero component of the normal module, and thus the tangent space of the Hilbert scheme,
	   is sixteen dimensional."},}

document {
     Key =>CT,
     Headline => "cotangent cohomology",
     PARA {TT "CT"," is a ",TO2{ScriptedFunctor,"scripted functor"}," providing an interface for cotangent cohomology
	  calculations. ",TT "CT^1"," is equivalent to ",TO cotangentCohomology1," and  
	   ",TT "CT^2"," is equivalent to ",TO cotangentCohomology2,"."}
     }

document {
     Key =>PolynomialCheck,
     Headline => "checks if power series solution terminates",
     PARA{TT "PolynomialCheck"," is the name of an optional argument. Its value is ", ofClass Boolean,", which determines whether 
	 or not to check if a solution of the deformation equation lifts trivially
	 to arbitrary order. Default value is ",TO true},
     }
document {
     Key =>SanityCheck,
     Headline => "checks if lifting solves deformation equation",
     PARA{TT "SanityCheck"," is the name of an optional argument. Its value is ",ofClass Boolean,", which determines whether 
	 or not to check if a supposed solution of the deformation equation
	 actually satisfies it. Default value is ",TO true,"."},     }

document {
     Key =>HighestOrder,
     Headline => "sets the order to which we compute",
     PARA{TT "HighestOrder"," is the name of an optional argument. Its value is an ",TO2(ZZ,"integer"),", which 
	  gives an upper bound on to what order
	  a solution of the deformation equation is lifted.
	 Default value is ",TT "20."},     }

document {
     Key =>SmartLift,
     Headline => "chooses lifting to avoid obstructions at next order",
     PARA{TT "SmartLift"," is the name of an optional argument whose value is ",ofClass Boolean,". If set to ",TO true,",
	   ",TO versalDeformation," or ",TO localHilbertScheme," will utilize the function
	   ",TO correctDeformation," in order to
	   choose liftings of the 
	   deformation equation at each step which, if possible, 
	  introduce no higher order terms to the obstruction equations. This may increase 
	  the time of calculation, but will hopefully result in nicer equations for the base space.
	  Default value is ",TO true},
	  
	PARA {"For example, consider a degenerate twisted cubic curve, see",TO2 {VersalDeformations,"[PS85]"},":"},
     	EXAMPLE {"S=QQ[x,y,z,w];",
	"F0=matrix {{x*z,y*z,z^2,x^3}}"},
   	PARA {"With the default setting ",TT "SmartLift=>true", " we get very
	     nice equations for the base space:"},
	EXAMPLE {
	"time (F,R,G,C)=localHilbertScheme(F0);",
	"T=ring first G;",
	"sum G"},
   	PARA {"With the setting ",TT "SmartLift=>false", " the calculation
	     is faster, but the equations are no longer homogeneous:"},
	EXAMPLE {
	"time (F,R,G,C)=localHilbertScheme(F0,SmartLift=>false);",
	"sum G"},
   	       }
	  
document {
     Key =>CorrectionMatrix,
     Headline => "determines the first order deformations used in correcting liftings",
     PARA{TT "CorrectionMatrix"," is the name of an optional argument, whose value is either ",ofClass String," with value auto or ",ofClass Sequence," of 
	  the form output by ",TO correctionMatrix,". The second term in the sequence is a list of two matrices which give some parameter-free first-order deformations and the corresponding lifted relations, respectively. The first term in the sequence is ",ofClass Matrix,", which describes the action on liftings of equations by the specified first-order deformations.  If set to auto, ",TO correctionMatrix," is used to calculate
	  the relevant sequence. The default value of ",TT "CorrectionMatrix"," is auto."}}

	document {
     Key =>CacheName,
     Headline => "determines hash table in which to cache solutions to the deformation equation",
     PARA{TT "CacheName"," is the name of an optional argument, whose value is either ",ofClass MutableHashTable," or ",TO null,". After each stage of lifting, the methods ",TO versalDeformation," and ",TO localHilbertScheme," will store the solution to the deformation equation 
in ",TT "CacheName#VersalDeformationResults",". If the value of ",TT "CacheName"," is ",TO null,"
 is as the default, then the solution is stored in the hash table described in the documenatation of ",TO versalDeformation," and ",TO localHilbertScheme,"."}}

document {
     Key =>VersalDeformationResults,
     Headline => "hash table key for cached solutions to the deformation equation",
     PARA{TT "VersalDeformationResults"," is the name of ",ofClass Symbol," used as a key for caching the output of ",TO versalDeformation," and ",TO localHilbertScheme,". See ",TO CacheName," for more information."}}



document {
    Key =>DefParam,
   Headline => "deformation parameter",
   PARA {TT "DefParam"," is the name of an optional arguemt. Its value is ",ofClass Symbol,", which specifies the name of the deformation parameter.
	Its default value is determined by the loadtime configuration ",TO Option," ",TT "DefaultDefParam",", which
	 has default value ",TT "t","."
	},
    PARA {"For example, we may use the deformation parameter ",TT "s",":"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "F0=gens I",
	  "(F,R,G,C)=versalDeformation(F0,DefParam=>s);",
	  "sum F"
	  },
  SeeAlso => {firstOrderDeformations,versalDeformation,localHilbertScheme}, 
         }

document{
     Key=>{[liftDeformation,Verbose],[versalDeformation,Verbose],
	  [localHilbertScheme,Verbose],[correctDeformation,Verbose]},
     Headline => "control the verbosity of output",
     PARA {TT "Verbose"," is the name of an optional argument. Its value is an integer, which specifies how verbose output of the above commands
	  should be. Default value is ",TT "0"," which gives the tersest possible output. Highest 
	  verbosity is attained with the value ",TT "4","."}
     }


TEST ///
S = QQ[a,b,c,d]
J = minors(2,matrix{{a,b,c,d^2},{b,c,d,a^3}})
(F,R,G,C)=versalDeformation(gens J,HighestOrder=>2,SmartLift=>false)
assert (sum G==map(target G_0,source G_0,sub(matrix {{-t_1*t_12-t_2*t_21}, {-t_10*t_12+t_12*t_21-t_2*t_22}, {t_10*t_21-t_21^2-t_1*t_22}, {t_12*t_21-t_13*t_21+t_1*t_23}, {t_12^2-t_12*t_13-t_2*t_23}, {t_12*t_22-t_13*t_22+t_10*t_23-t_21*t_23}},ring G_0)))
///

TEST ///
S = QQ[x_0..x_2,y_0..y_2]
J = minors(2,matrix{{x_0,x_1,x_2},{y_0,y_1,y_2}})
assert (CT^1(gens J)==0)
assert (CT^2(gens J)==0)
assert( numgens source normalMatrix(0,gens J)==24)
(F,R,G,C)=versalDeformation(gens J)
assert(sum F==sub(gens J,ring sum F))
assert(sum G==0)
///

TEST ///
S=QQ[x_0..x_4]
I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}})
F0=gens I
(F1,R1)=firstOrderDeformations(gens I,syz gens I,CT^1(gens I))
assert (sum F1==map(target F1_0,source F1_0,matrix {{x_1*t_1+x_0*t_2-x_1^2+x_0*x_2, x_0*t_4-x_1*x_2+x_0*x_3, -x_3*t_1-x_2*t_2+x_1*t_4-x_2^2+x_1*x_3, x_2*t_3-x_1*x_3+x_0*x_4, -x_4*t_1-x_3*t_2+x_3*t_3-x_2*x_3+x_1*x_4, x_4*t_3-x_3*t_4-x_3^2+x_2*x_4}}))
assert (sum R1==map(target R1_0,source R1_0,matrix {{t_4+x_3, x_2, 0, x_4, x_3, 0, 0, 0}, {-t_2-x_2, t_1-x_1, x_4, 0, -t_3, 0, x_4, x_3}, {x_1, x_0, -x_3, -t_3, 0, x_4, 0, -t_3}, {0, 0, -t_4-x_3, -t_2-x_2, t_1-x_1, 0, -t_4-x_3, -t_3-x_2}, {0, 0, x_2, x_1, x_0, -t_4-x_3, -t_3, 0}, {0, 0, t_1, 0, 0, t_2-t_3+x_2, x_1, x_0}}))
(F,R,G,C)=versalDeformation(F0)
assert (sum F==map(target F_0,source F_0,sub(matrix {{x_1*t_1+x_0*t_2-x_1^2+x_0*x_2, -t_1*t_3+x_0*t_4-x_1*x_2+x_0*x_3, -t_2*t_3+t_3^2-t_1*t_4-x_3*t_1-x_2*t_2+x_1*t_4-x_2^2+x_1*x_3, t_2*t_3-t_3^2+x_2*t_3-x_1*x_3+x_0*x_4, -x_4*t_1-x_3*t_2+x_3*t_3-x_2*x_3+x_1*x_4, x_4*t_3-x_3*t_4-x_3^2+x_2*x_4}},ring F_0)))
assert (sum R==map(target R_0,source R_0,sub(matrix {{t_4+x_3, x_2, 0, x_4, x_3, 0, 0, 0}, {-t_2-x_2, t_1-x_1, x_4, 0, -t_3, 0, x_4, x_3}, {x_1, x_0, -x_3, -t_3, 0, x_4, 0, -t_3}, {0, 0, -t_4-x_3, -t_2-x_2, t_1-x_1, 0, -t_4-x_3, -t_3-x_2}, {0, 0, x_2, x_1, x_0, -t_4-x_3, -t_3, 0}, {0, 0, t_1, 0, 0, t_2-t_3+x_2, x_1, x_0}},ring F_0)))
assert (sum G== map(target G_0,source G_0,sub(matrix {{t_2*t_3-t_3^2}, {-t_1*t_3}, {t_3*t_4}},ring F_0)))
assert (sum C==map(target C_0,source C_0,sub(matrix {{-t_1+x_1, t_3+x_2, 0}, {x_0, -t_1+x_1, 0}, {0, 0, t_2-t_3+x_2}, {t_2-t_3+x_2, x_3, -t_1+x_1}, {-t_1+x_1, t_3+x_2, x_0}, {0, 0, 0}, {0, 0, t_2-t_3+x_2}, {0, 0, -t_1+x_1}},ring F_0)))
(F2,R2,G2,C2)=liftDeformation(F1,R1,{},{sub(C_0,ring F1_0)},Verbose=>4) 
assert(F_2==sub(F2_2,ring F_0))
assert(R_2==sub(R2_2,ring F_0))
assert(G_0==sub(G2_0,ring F_0))
///

TEST ///
S=QQ[x,y,z,w]
F0=matrix {{x*z,y*z,z^2,x^3}}
(F,R,G,C)=localHilbertScheme(F0)
assert (sum F==map(target F_0,source F_0,sub(matrix {{w^2*t_5*t_10^2*t_16+2*w^2*t_7*t_10*t_11*t_16+2*w^2*t_2*t_11^2*t_16+w^2*t_10^2*t_12+w^2*t_10*t_11*t_13+x*w*t_5*t_10*t_16+y*w*t_7*t_10*t_16-(1/2)*w^2*t_8*t_10*t_16+y*w*t_2*t_11*t_16-w^2*t_3*t_11*t_16+x*w*t_7*t_11*t_16+2*x*w*t_10*t_12+y*w*t_10*t_13+x*w*t_11*t_13+w^2*t_10*t_14-y^2*t_2*t_16-y*w*t_3*t_16-w^2*t_4*t_16-(1/2)*x*w*t_8*t_16+z*w*t_10+x^2*t_12+x*y*t_13+x*w*t_14+x*z, -w^2*t_5*t_10*t_11*t_16+w^2*t_10*t_11*t_12+w^2*t_11^2*t_13+w^2*t_6*t_10*t_16-2*w^2*t_10^2*t_16+y*w*t_7*t_11*t_16+(1/2)*w^2*t_8*t_11*t_16+y*w*t_10*t_12+x*w*t_11*t_12+2*y*w*t_11*t_13+w^2*t_11*t_14+x*y*t_5*t_16+x*w*t_6*t_16+y^2*t_7*t_16+(1/2)*y*w*t_8*t_16-x*w*t_10*t_16+z*w*t_11+x*y*t_12+y^2*t_13+y*w*t_14+x^2*t_16+y*z, -w^2*t_5^2*t_10^2*t_16^2-2*w^2*t_5*t_10^2*t_12*t_16-4*w^2*t_7*t_10*t_11*t_12*t_16-4*w^2*t_2*t_11^2*t_12*t_16+2*w^2*t_5*t_10*t_11*t_13*t_16-2*w^2*t_6*t_7*t_10*t_16^2+w^2*t_5*t_8*t_10*t_16^2+4*w^2*t_7*t_10^2*t_16^2-2*y*w*t_2*t_5*t_11*t_16^2-2*w^2*t_2*t_6*t_11*t_16^2+4*w^2*t_2*t_10*t_11*t_16^2-w^2*t_10^2*t_12^2-2*w^2*t_10*t_11*t_12*t_13-w^2*t_11^2*t_13^2-2*x*w*t_5*t_10*t_12*t_16-2*y*w*t_7*t_10*t_12*t_16+w^2*t_8*t_10*t_12*t_16-2*y*w*t_2*t_11*t_12*t_16+2*w^2*t_3*t_11*t_12*t_16-2*x*w*t_7*t_11*t_12*t_16-2*w^2*t_6*t_10*t_13*t_16+4*w^2*t_10^2*t_13*t_16-2*y*w*t_7*t_11*t_13*t_16-w^2*t_8*t_11*t_13*t_16+y^2*t_2*t_5*t_16^2+y*w*t_3*t_5*t_16^2+y*w*t_2*t_6*t_16^2+w^2*t_3*t_6*t_16^2-x*y*t_5*t_7*t_16^2-x*w*t_6*t_7*t_16^2-y^2*t_7^2*t_16^2-y*w*t_7*t_8*t_16^2-(1/4)*w^2*t_8^2*t_16^2-2*y*w*t_2*t_10*t_16^2-2*w^2*t_3*t_10*t_16^2-2*x*w*t_2*t_11*t_16^2-2*x*w*t_10*t_12^2-2*y*w*t_10*t_12*t_13-2*x*w*t_11*t_12*t_13-2*y*w*t_11*t_13^2-2*w^2*t_10*t_12*t_14-2*w^2*t_11*t_13*t_14+w^2*t_10*t_12*t_15+w^2*t_11*t_13*t_15+2*y^2*t_2*t_12*t_16+2*y*w*t_3*t_12*t_16+x*w*t_8*t_12*t_16-2*x*y*t_5*t_13*t_16-2*x*w*t_6*t_13*t_16-2*y^2*t_7*t_13*t_16-y*w*t_8*t_13*t_16+2*x*w*t_10*t_13*t_16+x*y*t_2*t_16^2+x*w*t_3*t_16^2-x^2*t_7*t_16^2-x^2*t_12^2-2*x*y*t_12*t_13-y^2*t_13^2-2*x*w*t_12*t_14-2*y*w*t_13*t_14-w^2*t_14^2+x*w*t_12*t_15+y*w*t_13*t_15+w^2*t_14*t_15-2*x^2*t_13*t_16+z*w*t_15+z^2, -2*w^3*t_5*t_10^2*t_11-2*w^3*t_7*t_10*t_11^2-2*w^3*t_2*t_11^3-y*w^2*t_5*t_10^2+w^3*t_6*t_10^2-2*w^3*t_10^3-2*x*w^2*t_5*t_10*t_11-2*y*w^2*t_7*t_10*t_11+w^3*t_8*t_10*t_11-3*y*w^2*t_2*t_11^2+w^3*t_3*t_11^2-x*w^2*t_7*t_11^2+2*x*w^2*t_6*t_10+y*w^2*t_8*t_10+w^3*t_9*t_10-3*x*w^2*t_10^2+2*y*w^2*t_3*t_11+w^3*t_4*t_11+x*w^2*t_8*t_11-w^3*t_1*t_14+w^3*t_1*t_15+z*w^2*t_1+y^3*t_2+y^2*w*t_3+y*w^2*t_4+x^2*y*t_5+x^2*w*t_6+x*y^2*t_7+x*y*w*t_8+x*w^2*t_9+x^3}},ring F_0)))
assert (sum R==map(target R_0, source R_0,sub(matrix {{-w*t_11-y, -w*t_5*t_11*t_16+w*t_6*t_16-2*w*t_10*t_16+x*t_16, w*t_5*t_10*t_16+w*t_7*t_11*t_16+w*t_10*t_12+w*t_11*t_13-(1/2)*w*t_8*t_16+x*t_12+y*t_13+w*t_14-w*t_15-z, -w^2*t_5*t_10*t_11-w^2*t_7*t_11^2+w^2*t_1*t_5*t_16+w^2*t_6*t_10-2*w^2*t_10^2-x*w*t_5*t_11+w^2*t_8*t_11+w^2*t_1*t_12+x*w*t_6+y^2*t_7+y*w*t_8+w^2*t_9-x*w*t_10+x^2}, {w*t_10+x, w*t_10*t_12+w*t_11*t_13+x*t_5*t_16+y*t_7*t_16+(1/2)*w*t_8*t_16+x*t_12+y*t_13+w*t_14-w*t_15-z, w*t_7*t_10*t_16+2*w*t_2*t_11*t_16-y*t_2*t_16-w*t_3*t_16, -w^2*t_5*t_10^2-w^2*t_7*t_10*t_11-2*w^2*t_2*t_11^2-y*w*t_7*t_10-y*w*t_2*t_11+w^2*t_3*t_11+w^2*t_1*t_13+y^2*t_2+y*w*t_3+w^2*t_4+x^2*t_5}, {0, w*t_11+y, w*t_10+x, w^2*t_1}, {-t_16, -t_5*t_16^2-2*t_12*t_16, t_7*t_16^2+2*t_13*t_16, -w*t_7*t_11*t_16-w*t_10*t_12-w*t_11*t_13-x*t_5*t_16+(1/2)*w*t_8*t_16-x*t_12-y*t_13-w*t_14-z}},ring F_0)))
assert (sum G== map(target G_0,source G_0,sub(matrix {{t_1*t_16}, {t_9*t_16}, {-t_4*t_16}, {-2*t_14*t_16+t_15*t_16}},ring F_0)))
assert (sum C==map(target C_0,source C_0,sub(matrix {{(1/2)*w^3*t_15+z*w^2, w^3*t_10+x*w^2, 0, (1/2)*w^3*t_1}, {(1/2)*w^3*t_5*t_15*t_16+w^3*t_12*t_15+z*w^2*t_5*t_16+2*z*w^2*t_12, w^3*t_5*t_10*t_16+2*w^3*t_10*t_12+x*w^2*t_5*t_16+2*x*w^2*t_12, -2*w^3*t_11*t_12-y*w^2*t_5*t_16-w^3*t_6*t_16+2*w^3*t_10*t_16-2*y*w^2*t_12-x*w^2*t_16, -w^3*t_5*t_10*t_11+(1/2)*w^3*t_1*t_5*t_16+w^3*t_6*t_10-2*w^3*t_10^2+y*w^2*t_7*t_11+(1/2)*w^3*t_8*t_11+w^3*t_1*t_12+x*y*w*t_5+x*w^2*t_6+y^2*w*t_7+(1/2)*y*w^2*t_8-x*w^2*t_10+x^2*w}, {-(1/2)*w^3*t_7*t_15*t_16-w^3*t_13*t_15-z*w^2*t_7*t_16-2*z*w^2*t_13, -w^3*t_7*t_10*t_16-2*w^3*t_10*t_13-x*w^2*t_7*t_16-2*x*w^2*t_13, -w^3*t_5*t_10*t_16-w^3*t_10*t_12+w^3*t_11*t_13+y*w^2*t_7*t_16+(1/2)*w^3*t_8*t_16-x*w^2*t_12+y*w^2*t_13+(1/2)*w^3*t_15+z*w^2, w^3*t_5*t_10^2+2*w^3*t_7*t_10*t_11+2*w^3*t_2*t_11^2-(1/2)*w^3*t_1*t_7*t_16+x*w^2*t_5*t_10+y*w^2*t_7*t_10-(1/2)*w^3*t_8*t_10+y*w^2*t_2*t_11-w^3*t_3*t_11+x*w^2*t_7*t_11-w^3*t_1*t_13-y^2*w*t_2-y*w^2*t_3-(1/2)*w^3*t_4-(1/2)*x*w^2*t_8}, {-2*w^4*t_5*t_7*t_10*t_11*t_16-2*w^4*t_2*t_5*t_11^2*t_16+2*w^4*t_7*t_10*t_11*t_12+2*w^4*t_2*t_11^2*t_12-2*w^4*t_5*t_10*t_11*t_13-x*w^3*t_5^2*t_10*t_16-y*w^3*t_5*t_7*t_10*t_16+2*w^4*t_6*t_7*t_10*t_16-(1/2)*w^4*t_5*t_8*t_10*t_16-4*w^4*t_7*t_10^2*t_16+y*w^3*t_2*t_5*t_11*t_16+w^4*t_3*t_5*t_11*t_16+2*w^4*t_2*t_6*t_11*t_16-x*w^3*t_5*t_7*t_11*t_16-4*w^4*t_2*t_10*t_11*t_16-x*w^3*t_5*t_10*t_12+y*w^3*t_7*t_10*t_12-(1/2)*w^4*t_8*t_10*t_12+y*w^3*t_2*t_11*t_12-w^4*t_3*t_11*t_12+x*w^3*t_7*t_11*t_12-y*w^3*t_5*t_10*t_13+w^4*t_6*t_10*t_13-2*w^4*t_10^2*t_13-x*w^3*t_5*t_11*t_13+y*w^3*t_7*t_11*t_13+(1/2)*w^4*t_8*t_11*t_13-w^4*t_5*t_10*t_14-w^4*t_7*t_11*t_14+w^4*t_7*t_11*t_15+w^4*t_4*t_5*t_16-y*w^3*t_2*t_6*t_16-w^4*t_3*t_6*t_16+x*y*w^2*t_5*t_7*t_16+x*w^3*t_6*t_7*t_16+y^2*w^2*t_7^2*t_16+(1/2)*x*w^3*t_5*t_8*t_16+y*w^3*t_7*t_8*t_16+(1/4)*w^4*t_8^2*t_16+2*y*w^3*t_2*t_10*t_16+2*w^4*t_3*t_10*t_16+2*x*w^3*t_2*t_11*t_16-z*w^3*t_5*t_10+z*w^3*t_7*t_11-y^2*w^2*t_2*t_12-y*w^3*t_3*t_12+w^4*t_4*t_12-x^2*w^2*t_5*t_12-(1/2)*x*w^3*t_8*t_12+x*w^3*t_6*t_13+y^2*w^2*t_7*t_13+(1/2)*y*w^3*t_8*t_13-x*w^3*t_10*t_13-2*x*w^3*t_5*t_14+(1/2)*w^4*t_8*t_14+x*w^3*t_5*t_15-(1/2)*w^4*t_8*t_15-x*y*w^2*t_2*t_16-x*w^3*t_3*t_16+x^2*w^2*t_7*t_16-(1/2)*z*w^3*t_8+x^2*w^2*t_13, -w^4*t_5*t_10^2-w^4*t_7*t_10*t_11-2*w^4*t_2*t_11^2-y*w^3*t_7*t_10-y*w^3*t_2*t_11+w^4*t_3*t_11+y^2*w^2*t_2+y*w^3*t_3+w^4*t_4+x^2*w^2*t_5, 0, 0}},ring F_0)))
(F1,R1,G1,C1)=localHilbertScheme(F0,SmartLift=>false)
assert (sum F1==map(target F1_0,source F1_0,sub(matrix {{-w^2*t_5*t_10^2*t_16-w^2*t_7*t_10*t_11*t_16-w^2*t_2*t_11^2*t_16-w^2*t_10^2*t_12+y*w*t_7*t_10*t_16+w^2*t_8*t_10*t_16+y*w*t_2*t_11*t_16+w^2*t_3*t_11*t_16+y*w*t_10*t_13+w^2*t_10*t_14-y^2*t_2*t_16-y*w*t_3*t_16-w^2*t_4*t_16+z*w*t_10+x^2*t_12+x*y*t_13+x*w*t_14+x*z, -2*w^2*t_5*t_10*t_11*t_16-w^2*t_7*t_11^2*t_16-w^2*t_10*t_11*t_12-y*w*t_5*t_10*t_16+w^2*t_6*t_10*t_16-2*w^2*t_10^2*t_16+w^2*t_8*t_11*t_16-y*w*t_10*t_12+x*w*t_11*t_12+y*w*t_11*t_13+w^2*t_11*t_14+x*y*t_5*t_16+x*w*t_6*t_16+y^2*t_7*t_16+y*w*t_8*t_16-x*w*t_10*t_16+z*w*t_11+x*y*t_12+y^2*t_13+y*w*t_14+x^2*t_16+y*z, 2*w^2*t_5*t_7*t_10*t_11*t_16^2+w^2*t_7^2*t_11^2*t_16^2-2*w^2*t_7*t_10*t_11*t_12*t_16-4*w^2*t_2*t_11^2*t_12*t_16+4*w^2*t_5*t_10*t_11*t_13*t_16+2*w^2*t_7*t_11^2*t_13*t_16-2*w^2*t_6*t_7*t_10*t_16^2+4*w^2*t_7*t_10^2*t_16^2-2*y*w*t_2*t_5*t_11*t_16^2-2*w^2*t_2*t_6*t_11*t_16^2-w^2*t_7*t_8*t_11*t_16^2+4*w^2*t_2*t_10*t_11*t_16^2-w^2*t_10^2*t_12^2-2*y*w*t_7*t_10*t_12*t_16-2*y*w*t_2*t_11*t_12*t_16+2*w^2*t_3*t_11*t_12*t_16+2*y*w*t_5*t_10*t_13*t_16-2*w^2*t_6*t_10*t_13*t_16+4*w^2*t_10^2*t_13*t_16-2*w^2*t_8*t_11*t_13*t_16+y^2*t_2*t_5*t_16^2+y*w*t_3*t_5*t_16^2+y*w*t_2*t_6*t_16^2+w^2*t_3*t_6*t_16^2-x*y*t_5*t_7*t_16^2-x*w*t_6*t_7*t_16^2-y^2*t_7^2*t_16^2-y*w*t_7*t_8*t_16^2-2*y*w*t_2*t_10*t_16^2-2*w^2*t_3*t_10*t_16^2-2*x*w*t_2*t_11*t_16^2+2*x*w*t_10*t_12^2+2*y*w*t_10*t_12*t_13+2*w^2*t_10*t_12*t_14-w^2*t_10*t_12*t_15+2*y^2*t_2*t_12*t_16+2*y*w*t_3*t_12*t_16-2*x*y*t_5*t_13*t_16-2*x*w*t_6*t_13*t_16-2*y^2*t_7*t_13*t_16-2*y*w*t_8*t_13*t_16+2*x*w*t_10*t_13*t_16+x*y*t_2*t_16^2+x*w*t_3*t_16^2-x^2*t_7*t_16^2-x^2*t_12^2-2*x*y*t_12*t_13-y^2*t_13^2-2*x*w*t_12*t_14-2*y*w*t_13*t_14-w^2*t_14^2+x*w*t_12*t_15+y*w*t_13*t_15+w^2*t_14*t_15-2*x^2*t_13*t_16+z*w*t_15+z^2, w^3*t_5*t_10^2*t_11+w^3*t_7*t_10*t_11^2+w^3*t_2*t_11^3-w^3*t_6*t_10^2+w^3*t_10^3-w^3*t_8*t_10*t_11-w^3*t_3*t_11^2+2*w^3*t_1*t_10*t_12+w^3*t_1*t_11*t_13+w^3*t_9*t_10+w^3*t_4*t_11-w^3*t_1*t_14+w^3*t_1*t_15+z*w^2*t_1+y^3*t_2+y^2*w*t_3+y*w^2*t_4+x^2*y*t_5+x^2*w*t_6+x*y^2*t_7+x*y*w*t_8+x*w^2*t_9+x^3}},ring F1_0)))
assert (sum R1==map(target R1_0, source R1_0,sub(matrix {{-w*t_11-y, -w*t_5*t_11*t_16+w*t_6*t_16-2*w*t_10*t_16+x*t_16, -w*t_10*t_12+x*t_12+y*t_13+w*t_14-w*t_15-z, w^2*t_5*t_10*t_11+w^2*t_1*t_5*t_16-w^2*t_6*t_10+w^2*t_10^2-x*w*t_5*t_11+w^2*t_1*t_12+x*w*t_6+y^2*t_7+y*w*t_8+w^2*t_9-x*w*t_10+x^2}, {w*t_10+x, -w*t_5*t_10*t_16-w*t_7*t_11*t_16-w*t_10*t_12+x*t_5*t_16+y*t_7*t_16+w*t_8*t_16+x*t_12+y*t_13+w*t_14-w*t_15-z, w*t_7*t_10*t_16+2*w*t_2*t_11*t_16-y*t_2*t_16-w*t_3*t_16, w^2*t_7*t_10*t_11+w^2*t_2*t_11^2-y*w*t_7*t_10-w^2*t_8*t_10-y*w*t_2*t_11-w^2*t_3*t_11+w^2*t_1*t_13+y^2*t_2+y*w*t_3+w^2*t_4+x^2*t_5}, {0, w*t_11+y, w*t_10+x, w^2*t_1}, {-t_16, -t_5*t_16^2-2*t_12*t_16, t_7*t_16^2+2*t_13*t_16, w*t_5*t_10*t_16+w*t_10*t_12-x*t_5*t_16-x*t_12-y*t_13-w*t_14-z}},ring F1_0)))
assert (sum G1== map(target G1_0,source G1_0,sub(matrix {{t_1*t_16}, {2*t_5*t_10*t_11*t_16+t_7*t_11^2*t_16-2*t_6*t_10*t_16+3*t_10^2*t_16-t_8*t_11*t_16+t_9*t_16}, {-t_5*t_10^2*t_16-2*t_7*t_10*t_11*t_16-3*t_2*t_11^2*t_16+t_8*t_10*t_16+2*t_3*t_11*t_16-t_4*t_16}, {2*t_5*t_10*t_16^2+2*t_7*t_11*t_16^2+4*t_10*t_12*t_16+2*t_11*t_13*t_16-t_8*t_16^2-2*t_14*t_16+t_15*t_16}},ring F1_0)))
assert (sum C1==map(target C1_0,source C1_0,sub(matrix {{-w^3*t_5*t_10*t_16-w^3*t_7*t_11*t_16+(1/2)*w^3*t_8*t_16+(1/2)*w^3*t_15+z*w^2, w^3*t_10+x*w^2, 0, (1/2)*w^3*t_1}, {-2*w^3*t_7*t_11*t_12*t_16+w^3*t_5*t_11*t_13*t_16+w^3*t_8*t_12*t_16-w^3*t_5*t_14*t_16+w^3*t_5*t_15*t_16+w^3*t_12*t_15+z*w^2*t_5*t_16+2*z*w^2*t_12, w^3*t_5*t_10*t_16+2*w^3*t_10*t_12+x*w^2*t_5*t_16+2*x*w^2*t_12, -2*w^3*t_11*t_12-y*w^2*t_5*t_16-w^3*t_6*t_16+2*w^3*t_10*t_16-2*y*w^2*t_12-x*w^2*t_16, -2*w^3*t_5*t_10*t_11-w^3*t_7*t_11^2-y*w^2*t_5*t_10+w^3*t_6*t_10-2*w^3*t_10^2+w^3*t_8*t_11+w^3*t_1*t_12+x*y*w*t_5+x*w^2*t_6+y^2*w*t_7+y*w^2*t_8-x*w^2*t_10+x^2*w}, {-2*w^3*t_7*t_10*t_12*t_16+2*w^3*t_5*t_10*t_13*t_16+w^3*t_7*t_11*t_13*t_16-w^3*t_8*t_13*t_16+w^3*t_7*t_14*t_16-w^3*t_7*t_15*t_16-w^3*t_13*t_15-z*w^2*t_7*t_16-2*z*w^2*t_13, -w^3*t_7*t_10*t_16-2*w^3*t_10*t_13-x*w^2*t_7*t_16-2*x*w^2*t_13, -w^3*t_5*t_10*t_16-w^3*t_10*t_12+w^3*t_11*t_13+y*w^2*t_7*t_16+(1/2)*w^3*t_8*t_16-x*w^2*t_12+y*w^2*t_13+(1/2)*w^3*t_15+z*w^2, -(1/2)*w^3*t_5*t_10^2+(1/2)*w^3*t_2*t_11^2+y*w^2*t_7*t_10+(1/2)*w^3*t_8*t_10+y*w^2*t_2*t_11-w^3*t_1*t_13-y^2*w*t_2-y*w^2*t_3-(1/2)*w^3*t_4}, {w^4*t_5^2*t_10^2*t_16-w^4*t_5*t_7*t_10*t_11*t_16+w^4*t_2*t_5*t_11^2*t_16-w^4*t_7^2*t_11^2*t_16+3*w^4*t_7*t_10*t_11*t_12+5*w^4*t_2*t_11^2*t_12-3*w^4*t_5*t_10*t_11*t_13-w^4*t_7*t_11^2*t_13-y*w^3*t_5*t_7*t_10*t_16+2*w^4*t_6*t_7*t_10*t_16-w^4*t_5*t_8*t_10*t_16-4*w^4*t_7*t_10^2*t_16+y*w^3*t_2*t_5*t_11*t_16-w^4*t_3*t_5*t_11*t_16+2*w^4*t_2*t_6*t_11*t_16+w^4*t_7*t_8*t_11*t_16-4*w^4*t_2*t_10*t_11*t_16+2*x*w^3*t_5*t_10*t_12+y*w^3*t_7*t_10*t_12-w^4*t_8*t_10*t_12+y*w^3*t_2*t_11*t_12-3*w^4*t_3*t_11*t_12-2*y*w^3*t_5*t_10*t_13+w^4*t_6*t_10*t_13-2*w^4*t_10^2*t_13+x*w^3*t_5*t_11*t_13+w^4*t_8*t_11*t_13-w^4*t_5*t_10*t_15+w^4*t_4*t_5*t_16-y*w^3*t_2*t_6*t_16-w^4*t_3*t_6*t_16+x*y*w^2*t_5*t_7*t_16+x*w^3*t_6*t_7*t_16+y^2*w^2*t_7^2*t_16+y*w^3*t_7*t_8*t_16+2*y*w^3*t_2*t_10*t_16+2*w^4*t_3*t_10*t_16+2*x*w^3*t_2*t_11*t_16-2*z*w^3*t_5*t_10-y^2*w^2*t_2*t_12-y*w^3*t_3*t_12+w^4*t_4*t_12-x^2*w^2*t_5*t_12+x*w^3*t_6*t_13+y^2*w^2*t_7*t_13+y*w^3*t_8*t_13-x*w^3*t_10*t_13-2*x*w^3*t_5*t_14+x*w^3*t_5*t_15-x*y*w^2*t_2*t_16-x*w^3*t_3*t_16+x^2*w^2*t_7*t_16+x^2*w^2*t_13, w^4*t_7*t_10*t_11+w^4*t_2*t_11^2-y*w^3*t_7*t_10-w^4*t_8*t_10-y*w^3*t_2*t_11-w^4*t_3*t_11+y^2*w^2*t_2+y*w^3*t_3+w^4*t_4+x^2*w^2*t_5, 0, 0}},ring F1_0)))
FC=(correctDeformation(F1_{0,1,2},R1_{0,1,2},G1_{0},C1_{0}))_0
assert (sub(sum FC,ring F_0)==sum F_{0,1,2})
///
