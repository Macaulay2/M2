--*- coding: utf-8 -*-
---------------------------------------------------------------------------
-- PURPOSE: Calculating versal deformations and local Hilbert schemes
-- PROGRAMMER : Nathan Ilten
-- UPDATE HISTORY : June 2011
---------------------------------------------------------------------------
newPackage("VersalDeformations",
    Headline => "A package for calculating versal deformations and local Hilbert schemes",
    Version => "0.7",
    Date => "July 20, 2011",
    Authors => {
        {Name => "Nathan Owen Ilten",
	  HomePage => "http://people.cs.uchicago.edu/~nilten/",
	  Email => "nilten@cs.uchicago.edu"}},
    DebuggingMode => true,
    Configuration => {}
    )

---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2011 Nathan Owen Ilten
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



export {"liftDeformation",
     "firstOrderDeformations",
     "CT",
     "cotanComplexOne",
     "cotanComplexTwo",
     "normalModule",
     "versalDeformation",
     "localHilbertScheme",
     "PolynomialCheck",
     "SanityCheck",
     "HighestOrder",
     "TimeLimit",
     "SmartLift",
     "t",
     "correctDeformation",
     "correctionMatrix",
     "CorrectionMatrix"
     }

t = new IndexedVariableTable;

protect SanityCheck
protect TimeLimit
protect PolynomialCheck
protect HighestOrder
protect SmartLift
protect CorrectionMatrix

     
     
----------------------------------------------------------------------------
-- Cotangent cohomology and normal modules
----------------------------------------------------------------------------     

CT=new ScriptedFunctor
CT#superscript=i->(if i==1 then return cotanComplexOne; if i==2 then return cotanComplexTwo)

cotanComplexOne=method(TypicalValue=>Matrix)

cotanComplexOneMod:=(F)->(
     A:=ring F/image F; --quotient ring
     Hom(ideal F,A)/(image ((transpose substitute(jacobian F,A)))))
     
cotanComplexOne Matrix:=F->lift(ambient basis(cotanComplexOneMod F), ring F)
cotanComplexOne (ZZ,Matrix):=(deg,F)->lift(ambient basis(deg,cotanComplexOneMod(F)), ring F)
cotanComplexOne (List,Matrix):=(deg,F)->lift(ambient basis(deg,cotanComplexOneMod(F)), ring F)
cotanComplexOne (ZZ,ZZ,Matrix):=(lo,hi,F)->lift(ambient basis(lo,hi,cotanComplexOneMod(F)), ring F)
cotanComplexOne (InfiniteNumber,ZZ,Matrix):=(lo,hi,F)->lift(ambient basis(lo,hi,cotanComplexOneMod(F)), ring F)
cotanComplexOne (ZZ,InfiniteNumber,Matrix):=(lo,hi,F)->lift(ambient basis(lo,hi,cotanComplexOneMod(F)), ring F)


cotanComplexTwo=method(TypicalValue=>Matrix)
cotanComplexTwoMod:=F->(
     A:=ring F/image F;
     R:=gens ker F;
     kos:=koszul(2,F);
     (Hom((image R/image kos),A)/(image substitute(transpose R,A)))
     )

cotanComplexTwo Matrix:=F->lift(ambient basis(cotanComplexTwoMod F),ring F)
cotanComplexTwo (ZZ,Matrix):=(deg,F)->lift(ambient basis(deg,cotanComplexTwoMod(F)), ring F)
cotanComplexTwo (List,Matrix):=(deg,F)->lift(ambient basis(deg,cotanComplexTwoMod(F)), ring F)
cotanComplexTwo (ZZ,ZZ,Matrix):=(lo,hi,F)->lift(ambient basis(lo,hi,cotanComplexTwoMod(F)), ring F)
cotanComplexTwo (InfiniteNumber,ZZ,Matrix):=(lo,hi,F)->lift(ambient basis(lo,hi,cotanComplexTwoMod(F)), ring F)
cotanComplexTwo (ZZ,InfiniteNumber,Matrix):=(lo,hi,F)->lift(ambient basis(lo,hi,cotanComplexTwoMod(F)), ring F)

normalModule=method(TypicalValue=>Matrix)
normalModule Matrix:=F->lift(ambient basis(Hom(ideal F, ring F/ideal F)), ring F)
normalModule (ZZ,Matrix):=(deg,F)->lift(ambient basis(deg,Hom(ideal F, ring F/ideal F)), ring F)
normalModule (List,Matrix):=(deg,F)->lift(ambient basis(deg,Hom(ideal F, ring F/ideal F)), ring F)
normalModule (ZZ,ZZ,Matrix):=(lo,hi,F)->lift(ambient basis(lo,hi,Hom(ideal F, ring F/ideal F)), ring F)
normalModule (InfiniteNumber,ZZ,Matrix):=(lo,hi,F)->lift(ambient basis(lo,hi,Hom(ideal F, ring F/ideal F)), ring F)
normalModule (ZZ,InfiniteNumber,Matrix):=(lo,hi,F)->lift(ambient basis(lo,hi.Hom(ideal F, ring F/ideal F)), ring F)
     
----------------------------------------------------------------------------
-- Stuff to lift deformation equation solutions
----------------------------------------------------------------------------
--auxiliary function to get lowest order degrees and terms of obstruction equations
lowestOrderTerms:=(F,G,C,n,d)->(
     if n==1 then (lowG:=map(source C_0,target F_0,0);
     	  lowDeg:=toList(d:0)) 
     else (lowDeg=apply(toList(0..(d-1)),i->min ({n-2}|positions(G,g->not g^{i}==0)));
    	  lowG=matrix(apply(toList(0..(d-1)),i->{(G_(lowDeg_i))^{i}})));
     (lowDeg,lowG))

liftDeformation=method (TypicalValue=>Sequence,Options=>true)
liftopts:={SanityCheck=>true,Verbosity=>4}

liftDeformation(List,List,List,List):= liftopts >> opts -> (F,R,G,C)->(
     n:=#F-1; --order so far
     d:=numgens source C_0; --number of obstructions
     if n<1 then error "Need order at least one";
     -- find lowest order terms of obstruction equations
     if d>0 then (lowDeg,lowG):=lowestOrderTerms(F,G,C,n,d)
     else lowG=map(source C_0,target F_0,0); --unobstructed case
     T:=ring F_0;
     A:=T/ideal (F_0|transpose lowG); --setup a common ring
     if opts#Verbosity > 3 then print "Calculating residual terms";
     fterms:=sum(apply(toList(1..n),i->F_i*R_(n+1-i))); --order n+1 terms
     eterms:=sum(apply(toList(1..(n-2)),i->C_i*G_(n-1-i))); -- terms from base equations
     rem:=substitute(((transpose fterms)+eterms),A); --reduce modulo our ideal
     if opts#Verbosity >3 then print "Lifting Family and Equations";
     lfameq:=rem//(substitute((transpose R_0)|C_0,A));
     FO:=F|{-transpose lift(lfameq^(toList(0..(numgens target R_0)-1)),T)}; --lift the family
     NG:=-lift(lfameq^(toList((numgens target R_0)..(numgens target R_0)+(numgens source C_0)-1)),T); --lift equations
     --now we remove total space variables from the obstruction equations
     if d==0 then (CNG:=NG; COC:=C_0)
     else (
          ltv:=apply(toList(0..(d-1)),i->(
	       	    if not all(apply(G,g->g^{i}),h->h==0) then return 1;
	       	    if NG_(i,0)==0 then return 1;
	       	    leadCoefficient NG_(i,0))); --find correction factors
     	  CNG=matrix apply(toList(0..(d-1)),i->{NG^{i}//ltv_i});  --correct equations
     	  COC=matrix {apply(toList(0..(d-1)),i->ltv_i*(C_0)_{i})}; --correct coefficients
     	  );
     GO:=G|{CNG};
     if opts#Verbosity >3 then print "Lifting Relations and Coefficients";
     lrelco:=((fterms+FO_(n+1)*R_0)+transpose(eterms+COC*CNG))//(F_0|transpose lowG);
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
     if opts#Verbosity>3 and opts#SanityCheck	   then print "Doing Sanity Check";
     if opts#SanityCheck then if not (transpose(sum(apply(toList(0..(n+1)),i->FO_i*RO_(n+1-i))))+sum(apply(toList(0..(n-1)),i->CO_i*GO_(n-1-i))))==0 then error "Something is wrong"; 
     (FO,RO,GO,CO))

--auxiliary function to find the action of  T1 on liftings     
correctionMatrix=method(TypicalValue=>Sequence)

correctionMatrix(Matrix,Matrix):=(F1,R1)->(
     T:=ring F1;
     params:=gens T;
     L:=apply(params,s->(
	sublist:=apply(params,r->(if r==s then  r=>1 else r=>0));
	{sub(sub(F1,sublist),T),sub(sub(R1,sublist),T)}));
     M:=matrix {apply(L,l->transpose(l_0*R1+F1*l_1))}; --changes to obstructions
     (M,L))



correctDeformation=method (TypicalValue=>Sequence,Options=>true)
correctopts:={SanityCheck=>true,Verbosity=>4}

correctDeformation(Sequence,Matrix,List):= correctopts >> opts -> (S,M,L)->(
     (F,R,G,C):=S; --we have to do things this way since we can only have at most four arguments
     n:=#F-1; --order so far
     d:=numgens source C_0; --number of obstructions
     if n<2 then error "Need order at least two";
     -- find lowest order terms of obstruction equations
     (lowDeg,lowG):=lowestOrderTerms(F,G,C,n,d);
     T:=ring F_0;
     A:=T/ideal (F_0|transpose lowG); --setup a common ring
     if opts#Verbosity > 3 then print "Calculating next order residual terms";
     fterms:=sum(apply(toList(1..n),i->F_i*R_(n+1-i))); --order n+1 terms
     eterms:=sum(apply(toList(1..(n-2)),i->C_i*G_(n-1-i))); -- terms from base equations
     rem:=substitute(((transpose fterms)+eterms),A); --reduce modulo our ideal
     if opts#Verbosity >3 then print "Trying to kill obstructions";
     kobseq:=rem//(substitute((transpose R_0)|M,A));
     CM:=-lift(kobseq^(toList((numgens target R_0)..(numgens target R_0)+(numgens source M)-1)),T); --here is how to perturb F
     if opts#Verbosity >3 then print "Adjusting family and relations";
     FC:=sum apply(toList(0..#L-1),i->CM_(i,0)*(L_i_0));
     RC:=sum apply(toList(0..#L-1),i->CM_(i,0)*(L_i_1));
     FO:=drop(F,-1)|{F_n+FC};
     RO:=drop(R,-1)|{R_n+RC};
     if opts#Verbosity>3 and opts#SanityCheck	   then print "Doing Sanity Check";
     if opts#SanityCheck then if not (transpose(sum(apply(toList(0..(n)),i->FO_i*RO_(n-i))))+sum(apply(toList(0..(n-2)),i->C_i*G_(n-2-i))))==0 then error "Something is wrong"; 
     (FO,RO))

correctDeformation(List,List,List,List):= correctopts >> opts -> (F,R,G,C)->(
     (M,L):=correctionMatrix(F_1,R_1);
     correctDeformation((F,R,G,C),M,L))

--methodfunction for finding describing first order deformations and relations
firstOrderDeformations=method(TypicalValue=>Sequence,Options=>true)
firstOrderDeformations(Matrix,Matrix,Matrix):={SanityCheck=>true} >> opts -> (F,R,T1)->(
     if T1==0 then return ({F,0*F},{R,0*R}); -- if rigid, nothing to do
     n:=numgens source T1; --number of deformation parameters
     T:=(ring F)[t_1..t_n]; --setup ring with parameters
     FO:={substitute(F,T),(matrix{toList(t_1..t_n)})*transpose substitute(T1,T)}; --first order family
     RO:={substitute(R,T),(-FO_1*substitute(R,T))//FO_0}; --first order relations
     if opts#SanityCheck then if not (FO_0*RO_1+FO_1*RO_0)==0 then error "Relations don't lift";
     (FO,RO))     

----------------------------------------------------------------------------
-- Iterated lifting methods
----------------------------------------------------------------------------

versalDeformation=method(Options=> true,TypicalValue=>Sequence) 
localHilbertScheme=method(Options=>true,TypicalValue=>Sequence)

versalopts:={HighestOrder=>20,Verbosity=>2,TimeLimit=>infinity,SanityCheck=>true, PolynomialCheck=>true,SmartLift=>true,CorrectionMatrix=>"auto"}

versalDeformation (List,List,List,List):=versalopts >> opts ->(f,r,g,c)->(
     ord:=-1+opts#HighestOrder;
     (F,R,G,C):=(f,r,g,c);
     if opts#SmartLift then (
	  if (opts#CorrectionMatrix==="auto") then (M,L):=correctionMatrix(F_1,R_1) else (M,L)=opts#CorrectionMatrix);
     i:=#F-2;
     if opts#Verbosity >0 then print "Starting lifting";
     while (i<ord) do (
	  if opts#Verbosity >1 then print ("Order "|toString(i+2));
	  if opts#TimeLimit==infinity then (F,R,G,C)=(liftDeformation(F,R,G,C,Verbosity=>opts#Verbosity,SanityCheck=>opts#SanityCheck));
	  if not opts#TimeLimit== infinity then (F,R,G,C)=try(alarm opts#TimeLimit; liftDeformation(F,R,G,C,Verbosity=>opts#Verbosity,SanityCheck=>opts#SanityCheck)) else(if opts#Verbosity>0 then print "Time limit exceeded or something didn't lift";i=ord;(F,R,G,C));
  	 if opts#SmartLift and numgens source C_0>0 then (F,R)=correctDeformation((F,R,G,C),M,L,Verbosity=>opts#Verbosity,SanityCheck=>opts#SanityCheck);
	  i=i+1;
	  if opts#PolynomialCheck then ( --check if solution lifts to polynomial ring
	       if opts#Verbosity>3 then print "Checking polynomial lifting";
	       if F_(-1)==0 and R_(-1)==0 and G_(-1)==0 then (
	       	    if transpose((sum F)*(sum R))+(sum C)*(sum G)==0 then (
			 i=ord;
			 if opts#Verbosity>0 then print "Solution is polynomial";
			 )
		    )
	       );
	  );
     (F,R,G,C))

versalDeformation (Matrix,Matrix,Matrix):=versalopts >> opts ->(F0,T1,T2)->(
     ord:=-1+opts#HighestOrder;
     if opts#Verbosity >1 then print "Calculating first order relations";
     (F,R):=firstOrderDeformations(F0,gens ker F0,T1,SanityCheck=>opts#SanityCheck);
     C:={substitute(T2,ring F_0)};
     G:={};
     versalDeformation(F,R,G,C,opts))
   
versalDeformation Matrix:= versalopts >> opts ->F0->(
     if opts#Verbosity > 0 then print "Calculating first order deformations and obstruction space";
     versalDeformation(F0,CT^1(F0),CT^2(F0),opts))

localHilbertScheme Matrix:= versalopts >> opts ->F0->(
     if opts#Verbosity > 0 then print "Calculating first order deformations and obstruction space";
     versalDeformation(F0,normalModule(0,F0),CT^2(0,F0),opts))

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
	  {"Klaus Altmann, ",EM "The versal deformation of an isolated Gorenstein 
	       singularity",
	   ", Inventiones Mathematicae Vol. 128 No. 3, 1997."},
      {"Dustin Cartwright and Bernd Sturmfels, ",EM "The Hilbert scheme of the diagonal
	   in a product of projective spaces", ", International Mathematics Research 
	   Notices Vol. 2010 No. 9."},
	{"Ragni Piene and Michael Schlessinger, ",EM "On the Hilbert scheme compactification
	of the space of twisted cubic curves", ", American Journal of Mathematics, Vol. 107
	No. 4, 1985."},
	{"Henry Pinkham, ",EM "Deformations of variety with G_m action",
	      ", Asterisque 20, 1974."},   
      },
 
      PARA{"The author thanks Jan Christophersen for helpful hints,
	   especially regarding the computation of ",TEX///$T^2$///,"."},
 }


document {
     Key =>{localHilbertScheme,(localHilbertScheme,Matrix),
	  [(localHilbertScheme,Matrix),PolynomialCheck],
	  [(localHilbertScheme,Matrix),HighestOrder],
	  [(localHilbertScheme,Matrix),SanityCheck],
	  [(localHilbertScheme,Matrix),SmartLift],
	  [(localHilbertScheme,Matrix),CorrectionMatrix],
	  [(localHilbertScheme,Matrix),TimeLimit]},
     Headline => "computes a power series representation of the local Hilbert scheme",
     Usage=>"(F,R,G,C) = localHilbertScheme(F0)",
     Inputs=>{"F0" => Matrix,},
     Outputs=>{"(F,R,G,C)" => Sequence },
     
     PARA{TT "F0"," should  be a matrix with homogeneous entries over some polynomial ring with one row."},

     
     PARA{"Each element of the sequence ", TT "(F,R,G,C)"," is a list of matrices
	  in increasing powers of ",TO t,". Their
	  sums satisfy the deformation equation ",
	  TT "transpose ((sum F)*(sum R))+(sum C)*(sum G)==0"," up to powers of ",TO t," equal to the
	  length of ",TT "F",". Furthermore,
	  ",TT "F_0=F0",", ",TT "R_0=gens ker F0",", ",TT "C_0=T^2(0,F_0)", " and ",TT "F_1"," consists 
	  of first order perturbations corresponding to ",TT "normalModule(0,F0)",". Thus,
	   ",TT "F"," and ",TT "G"," represent a universal family and
	  local analytic equations for the Hilbert scheme." 
	  }, 
	
	PARA{"Several options are available to control the termination of the calculation. 
	The calculation will terminate at the very latest after reaching order equal to 
	the option ",	TO HighestOrder,
	". If any single lifting step takes longer than ", TO TimeLimit,
	" the algorithm will terminate earlier. If ",TO PolynomialCheck," is set to ",
        TO true,", then the algorithm will check if the present solution lifts 
	to infinite order
	and terminate if this is the case. If ",TO SanityCheck," is set to ",TO true,", then
	the algorithm will check that the present solution really does solve the deformation
	equation, and terminate if this is not the case. Finally, ",TO Verbosity," may be
	used to control the verbosity of the output."}, 
	
	PARA{"The option ", TO SmartLift," is also available, which controls whether the algorithm
	     spends extra time trying to find liftings which introduce no new obstructions at the next 
	     highest order. The option ",TO CorrectionMatrix," may be used to control which liftings 
	     are considered."},
	 
 	PARA {"For example, consider a degenerate twisted cubic curve:"},
     	EXAMPLE {"S=QQ[x,y,z,w];",
	"F0=matrix {{x*z,y*z,z^2,x^3}}",
	"(F,R,G,C)=localHilbertScheme(F0);"
	},
     	PARA {"Local equations for the Hilbert scheme are thus given by"},
	EXAMPLE {"sum G"},
	Caveat => {"The output may not be the local Hilbert scheme if standard comparison theorems 
	     do not hold for the ideal generated by ",TT "FO","."}, 
     }

document {
     Key =>{(versalDeformation,List,List,List,List),
	  [(versalDeformation,List,List,List,List),SanityCheck],
	  [(versalDeformation,List,List,List,List),PolynomialCheck],
	  [(versalDeformation,List,List,List,List),HighestOrder],
	  [(versalDeformation,List,List,List,List),SmartLift],
	  	  [(versalDeformation,List,List,List,List),TimeLimit],
		  	  [(versalDeformation,List,List,List,List),CorrectionMatrix]
	  	  },
     Usage=>"(F,R,G,C) = versalDeformation(F0)\n
     (F,R,G,C) = versalDeformation(F0,T1,T2)",
     Inputs=>{"f"=>List,"r"=>List,"g"=>List,"c"=>List},
     Outputs=>{"(F,R,G,C)" => Sequence },
     Headline => "continues calculation of  a versal deformation",
     PARA {"The input ",TT "(f,r,g,c)"," should be a valid solution to the deformation equation output in
     the form done by ",TO (versalDeformation,Matrix,Matrix,Matrix),". This function continues lifting this solution
     to higher order. All options described for ",TO (versalDeformation,Matrix,Matrix,Matrix)," may be used 
     to the same effect."},
     PARA {"This function is especially useful for finding one-parameter families when the versal family
     is too complicated to calculate."}
         }

document{
     	 Key=>{versalDeformation},
	      Headline => "computes a power series representation of a versal deformation",
	      PARA{"Here we provide an overview of our approach to solving deformation problems.
	      For details on using the command ",TT "versalDeformation",", please see the documentation 
	      links below.  For simplicity, we restrict to the case of the 
	      versal deformation of an isolated singularity, "
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
	  (versalDeformation,Matrix),
	  [(versalDeformation,Matrix),SanityCheck],
   	  [(versalDeformation,Matrix),PolynomialCheck],
	  [(versalDeformation,Matrix),HighestOrder],
	  [(versalDeformation,Matrix),SmartLift],
	  [(versalDeformation,Matrix),TimeLimit],
	    [(versalDeformation,Matrix),CorrectionMatrix],
  	  [(versalDeformation,Matrix,Matrix,Matrix),SanityCheck],
   	  [(versalDeformation,Matrix,Matrix,Matrix),PolynomialCheck],
	  [(versalDeformation,Matrix,Matrix,Matrix),HighestOrder],
	  [(versalDeformation,Matrix,Matrix,Matrix),SmartLift],
	   [(versalDeformation,Matrix,Matrix,Matrix),CorrectionMatrix],
	  [(versalDeformation,Matrix,Matrix,Matrix),TimeLimit]
	  	  },
     Usage=>"(F,R,G,C) = versalDeformation(F0)\n
     (F,R,G,C) = versalDeformation(F0,T1,T2)",
     Inputs=>{"F0" => Matrix, "T1" => Matrix, "T2" => Matrix},
     Outputs=>{"(F,R,G,C)" => Sequence },
     Headline => "computes a power series representation of a versal deformation",
     
     PARA{TT "F0",", ",TT "T1",", and ",TT "T2"," should all be matrices over some common
	  polynomial ring, and ",TT "F0"," should have one row. If ",TT "T1"," and ",TT "T2",
	  " are omitted, then bases of the first and second cotangent cohomology modules
     for ",TT "F0"," are used."},
     
     PARA{"Each element of the sequence ", TT "(F,R,G,C)"," is a list of matrices
	  in increasing powers of ",TO t,". Their
	  sums satisfy the deformation equation ",
	  TT "transpose ((sum F)*(sum R))+(sum C)*(sum G)==0"," up to powers of ",TO t," equal to the
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
	". If any single lifting step takes longer than ", TO TimeLimit,
	" the algorithm will terminate earlier. If ",TO PolynomialCheck," is set to ",
        TO true,", then the algorithm will check if the present solution lifts 
	to infinite order
	and terminate if this is the case. If ",TO SanityCheck," is set to ",TO true,", then
	the algorithm will check that the present solution really does solve the deformation
	equation, and terminate if this is not the case. Finally, ",TO Verbosity," may be
	used to control the verbosity of the output."},  
	
	PARA{"The option ", TO SmartLift," is also available, which controls whether the algorithm
	spends extra time trying to find liftings which introduce no new obstructions at the next 
	highest order.  The option ",TO CorrectionMatrix," may be used to control which liftings 
	     are considered."},
	     
     PARA {"For example, consider the cone over the rational normal curve of degree four:"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "F0=gens I",
	  "(F,R,G,C)=versalDeformation(F0);"},
     PARA {"Equations for a versal base space are"},
     EXAMPLE {"sum G"},
     PARA {"The versal family is given by"},
     EXAMPLE {"sum F"},
     
     PARA {"We may also consider the example of the cone over the del Pezzo surface of degree
	  six:"},
     EXAMPLE {"S=QQ[x1,x2,x3,x4,x5,x6,z];",
	  "I=ideal {x1*x4-z^2,x2*x5-z^2,x3*x6-z^2,x1*x3-z*x2,x2*x4-z*x3,x3*x5-z*x4,x4*x6-z*x5,x5*x1-z*x6,x6*x2-z*x1};",
	"F0=gens I;",
	"(F,R,G,C)=versalDeformation(F0);"},
     PARA {"Equations for a versal base space are"},
     EXAMPLE {"sum G"},
     PARA {"The versal family is given by"},
     EXAMPLE {"sum F"},  
     
     PARA{"We may also compute local multigraded Hilbert schemes. Here, we consider
	  the Borel fixed ideal for the multigraded Hilbert scheme of the diagonal in
	  a product of three projective planes:"},
     EXAMPLE{
	  "S=QQ[x1,x2,x3,y1,y2,y3,z1,z2,z3,Degrees=>
	  {{1,0,0},{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1},{0,0,1}}];",
	  "I=ideal {y1*z2, x1*z2, y2*z1, y1*z1, x2*z1, x1*z1, x1*y2, x2*y1,
	   x1*y1, x2*y2*z2};",
	  "(F,R,G,C)=versalDeformation(gens I,normalModule({0,0,0},gens I),
	  CT^2({0,0,0},gens I));"},
     PARA {"Local equations for the multigraded Hilbert scheme  are"},
     EXAMPLE {"sum G"},
     PARA {"At this point, the multigraded HIlbert scheme has 7 irreducible components:"},
     EXAMPLE {"# primaryDecomposition ideal sum G"},	      
      }

document {
     Key =>{liftDeformation,(liftDeformation,List,List,List,List),
	  [(liftDeformation,List,List,List,List),SanityCheck]},
     Headline => "lift a solution of the deformation equation to the next order",
     Usage => "(F,R,G,C) = liftDeformation(f,r,g,c)",
     Inputs=> {"f"=>List,"r"=>List,"g"=>List,"c"=>List},
     Outputs=>{"(F,R,G,C)"=>Sequence},
     PARA{"Each element of the sequence ", TT "(f,r,g,c)"," is a list of matrices
	  in increasing powers of ",TO t,". Their
	  sums satisfy the deformation equation ",
	  TT "transpose ( (sum f)*(sum r))+(sum c)*sum(g)==0"," up to powers of ",TO t," equal to the
	  length of ",TT "f","." 
	    },
       
     PARA{"Each element of the output sequence ", TT "(F,R,G,C)"," is a list of matrices
	  in increasing powers of ",TO t,". The first three matrices of the sequence
	  are gotten from ", TT "(f,r,g)"," by appending one
	  matrix to each list in the sequence,
	  and furthermore the columns of ",TT "C_0"," are multiples of those of ",TT "c_0",
	  ". The other matrices are chosen to satisfy
	  the deformation equation ",
	  TT "transpose ((sum F)*(sum R))+(sum C)*(sum G)==0"," up to powers of ",TO t," equal to the
	  length of ",TT "F",", provided that there is such a solution." 
	    },
     PARA {"For example, consider the cone over the rational normal curve of degree four:"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "F0=gens I",
	  "T1=cotanComplexOne(F0);",
 	  "R0=gens ker F0;",
	  "(f,r)=firstOrderDeformations(F0,R0,T1);"
	  },
     PARA {"We now lift the first order deformations to second order:"},
     EXAMPLE{
      "T2=cotanComplexTwo(F0);",
      "c={substitute(T2,ring f_0)};",
      "g={};",
      "(F,R,G,C)=liftDeformation(f,r,g,c);",
      "sum F -- equations for family",
      "sum G -- base equations",},
     }

document {
     Key =>{firstOrderDeformations,(firstOrderDeformations,Matrix,Matrix,Matrix),
	  [(firstOrderDeformations,Matrix,Matrix,Matrix),SanityCheck]},
     Headline => "use tangent space to create first order peturbations and find relations",
     Usage => "(F,R) = firstOrderDeformations(F0,R0,T1)",
     Inputs => {"F0" =>Matrix, "R0"=>Matrix, "T1"=>Matrix},
     Outputs => {"(F,R)" =>Sequence},
     PARA{TT "F0",", ",TT "R0",", and ",TT "T1"," should all be matrices over some common
	  polynomial ring, and ",TT "F0"," and ",TT "T1"," should have one row. ",TT "R0"," should be the 
	  first syzygy matrix of ",TT "F0"," and ",TT "T1"," should have the same number of
	  columns as ",TT "F0","."},
	  
     PARA{TT "F"," is a list of length two with ",TT "F_0=F0"," and ",TT "F_1"," the first
	  order perturbations corresponding to ",TT "T1",". ",TT "R"," is a list of length
	  two with ",TT "R_0=R0"," and ",TT "R_1"," such that
	  ",TT "F_0*R_1+F_1*R_0==0","."}, 
         PARA {"For example, consider the cone over the rational normal curve of degree four:"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "F0=gens I",
	  "T1=cotanComplexOne(F0);",
	  "R0=gens ker F0;",
	  "(F,R)=firstOrderDeformations(F0,R0,T1)"
	  },
     
     }

document {
     Key =>{correctDeformation,(correctDeformation,List,List,List,List),
	  [(correctDeformation,List,List,List,List),SanityCheck],
	  (correctDeformation,Sequence,Matrix,List),
     	  [(correctDeformation,Sequence,Matrix,List),SanityCheck]},
     Headline => "correct lifting to avoid obstructions at next order",
     Usage => "(F,R) = correctDeformation(f,r,g,c)\n
     (F,R) = correctDeformation(S,M,L)",
     Inputs=> {"f"=>List,"r"=>List,"g"=>List,"c"=>List,
	  "(f,r,g,c)"=>Sequence,"M"=>Matrix,"L"=>List},
     Outputs=>{"(F,R)"=>Sequence},
     PARA {TT "(f,r,g,c)"," should be as in the output of ", TO liftDeformation ," and ", TT "(M,L)",
	  "should be as in the output of ", TO correctionMatrix,". If the latter are omitted, they are replaced
	  by ", TT "(M,L)=correctionMatrix(f_1,r_1)","."},
     PARA {TT "correctDeformation"," perturbs the last entries of ",TT "f"," and ",TT "r"," such that  
     if possible, the next invocation of ", TO liftDeformation," will introduce no new terms in the 
     obstruction equations."},
     PARA {"For example, consider a degenerate twisted cubic curve:"},
     EXAMPLE {"S=QQ[x,y,z,w];",
     "F0=matrix {{x*z,y*z,z^2,x^3}};",
     	"(f,r,g,c)=localHilbertScheme(F0,Verbosity=>0,HighestOrder=>2,SmartLift=>false);",
	"(liftDeformation(f,r,g,c,Verbosity=>0))_2",
	"(F,R)=correctDeformation(f,r,g,c);",
	"(liftDeformation(F,R,g,c,Verbosity=>0))_2"},
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
     Key =>{cotanComplexOne,(cotanComplexOne,Matrix),(cotanComplexOne,ZZ,Matrix),
	  (cotanComplexOne,List,Matrix),(cotanComplexOne,InfiniteNumber,ZZ,Matrix),
	  (cotanComplexOne,ZZ,InfiniteNumber,Matrix),(cotanComplexOne,ZZ,ZZ,Matrix)},
     Headline => "calculate first cotangent cohomology",
     Usage => "T1 = cotanComplexOne(F) \n
     T1 = cotanComplexOne(deg,F) \n
     T1 = cotanComplexOne(lo,hi,F)",
     Inputs => {"F" =>Matrix,  "deg" => {"a ",(TO2 {List,"list"})," or ",(TO2 {ZZ,"integer"})},
	  "lo" => {"an ",(TO2 {ZZ,"integer"})," or -",(TO infinity)},
	  "hi" => {"an ",(TO2 {ZZ,"integer"})," or ",(TO infinity)}
	  },
     Outputs=>{"T1" => Matrix},
     PARA {"The matrix ",TT "F"," must have a single row.  The output ",TT "T1"," is a matrix
	  over the same ring as ",TT "F"," whose columns form a basis for 
	  (a graded piece of) the first cotangent cohomology
	  module of ",TT "S/I",", where ",TT "S"," is the ring of ",TT "F"," and ",TT "I",
	  " is ideal generated by the columns of ",TT "F",". Selection
	  of graded pieces is done in the same manner as with ",TO basis,". If the selected
	  pieces are infinite dimensional, an error occurs."},
     PARA {"For example, consider the cone over the rational normal curve of degree four:"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "F=gens I",
	  "T1=cotanComplexOne(F)"},
     PARA {"The first cotangent cohomology module, and thus the tangent space of the versal deformation,
	   is four dimensional."},
     }

document { Key
     =>{cotanComplexTwo,(cotanComplexTwo,Matrix),(cotanComplexTwo,ZZ,Matrix),
     (cotanComplexTwo,List,Matrix),(cotanComplexTwo,InfiniteNumber,ZZ,Matrix),
     (cotanComplexTwo,ZZ,InfiniteNumber,Matrix),(cotanComplexTwo,ZZ,ZZ,Matrix)},
     Headline => "calculate second cotangent cohomology",
      Usage => "T2 = cotanComplexTwo(F) \n
     T2 = cotanComplexTwo(deg,F) \n
     T2 = cotanComplexTwo(lo,hi,F)",
     Inputs => {"F" =>Matrix,  "deg" => {"a ",(TO2 {List,"list"})," or ",(TO2 {ZZ,"integer"})},
	  "lo" => {"an ",(TO2 {ZZ,"integer"})," or -",(TO infinity)},
	  "hi" => {"an ",(TO2 {ZZ,"integer"})," or ",(TO infinity)}
	  },
     Outputs=>{"T2" => Matrix},
     PARA {"The matrix ",TT "F"," must have a single row.  The output ",TT "T2"," is a matrix
	  over the same ring as ",TT "F"," whose columns form a basis for 
	  (a graded piece of) the second cotangent cohomology
	  module of ",TT "S/I",", where ",TT "S"," is the ring of ",TT "F"," and ",TT "I",
	  " is ideal generated by the columns of ",TT "F",". Selection
	  of graded pieces is done in the same manner as with ",TO basis,". If the selected
	  pieces are infinite dimensional, an error occurs."},
     PARA {"For example, consider the cone over the rational normal curve of degree four:"},
     EXAMPLE {"S=QQ[x_0..x_4];",
	  "I=minors(2,matrix {{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}});",
	  "F=gens I",
	  "T2=cotanComplexTwo(F)"},
     PARA {"The second cotangent cohomology module is three dimensional. Thus, the base space of the
	  versal deformation is cut out by (at most) three equations."
	   },
      PARA {"We also consider the graded example of a degenerate twisted cubic curve:"},
     EXAMPLE {"S=QQ[x,y,z,w];",
	  "F=matrix {{x*z,y*z,z^2,x^3}}",
	  "T2=cotanComplexTwo(0,F)"},
     PARA {"The degree zero component of the second cotangent cohomology module
	  is four dimensional. Thus the Hilbert scheme is (locally analytically)
	   cut out by (at most) four equations."},
      }

document {
     Key =>{normalModule,(normalModule,Matrix),(normalModule,ZZ,Matrix),
	  (normalModule,List,Matrix),(normalModule,InfiniteNumber,ZZ,Matrix),
	  (normalModule,ZZ,InfiniteNumber,Matrix),(normalModule,ZZ,ZZ,Matrix)},
     Headline => "calculate normal module",
      Usage => "N = normalModule(F) \n
     N = normalModule(deg,F) \n
     N = normalModule(lo,hi,F)",
     Inputs => {"F" =>Matrix,  "deg" => {"a ",(TO2 {List,"list"})," or ",(TO2 {ZZ,"integer"})},
	  "lo" => {"an ",(TO2 {ZZ,"integer"})," or -",(TO infinity)},
	  "hi" => {"an ",(TO2 {ZZ,"integer"})," or ",(TO infinity)}
	  },
     Outputs=>{"N" => Matrix},
     PARA {"The matrix ",TT "F"," must have a single row.  The output ",TT "N"," is a matrix
	  over the same ring as ",TT "F"," whose columns form a basis for 
	  (a graded piece of) the normal module ",TT "Hom(I,S/I)",",
	  where ",TT "S"," is the ring of ",TT "F"," and ",TT "I",
	  " is ideal generated by the columns of ",TT "F",". Selection
	  of graded pieces is done in the same manner as with ",TO basis,". If the selected
	  pieces are infinite dimensional, an error occurs."},
     PARA {"For example, consider a degenerate twisted cubic curve:"},
     EXAMPLE {"S=QQ[x,y,z,w];",
	  "F=matrix {{x*z,y*z,z^2,x^3}}",
	  "N=normalModule(0,F)"},
     PARA {"The degree zero component of the normal module, and thus the tangent space of the Hilbert scheme,
	   is sixteen dimensional."},}

document {
     Key =>CT,
     Headline => "cotangent cohomology",
     PARA {TT "CT"," is a ",TO2{ScriptedFunctor,"scripted functor"}," providing an interface for cotangent cohomology
	  calculations. ",TT "CT^1"," is equivalent to ",TO cotanComplexOne," and  
	   ",TT "CT^2"," is equivalent to ",TO cotanComplexTwo,"."}
     }

document {
     Key =>PolynomialCheck,
     Headline => "checks if power series solution terminates",
     PARA{TT "PolynomialCheck"," is a ",TO2(Boolean,"boolean")," which determines whether 
	 or not to check if a solution of the deformation equation lifts trivially
	 to arbitrary order. Default value is ",TO true},
     }

document {
     Key =>TimeLimit,
     Headline => "sets time limit for lifting calculation",
     PARA{TT "TimeLimit"," is either an ",TO2(ZZ,"integer")," or ",TO infinity," which 
	  gives an upper bound on the time in seconds allowed for lifting 
	  a solution of the deformation equation.
	 Default value is ",TT "infinity"},     
     Caveat=>{"If ",TT "HighestOrder"," is not set to ",TT "infinity",", errors may 
	  not be handled properly."},}
document {
     Key =>SanityCheck,
     Headline => "checks if lifting solves deformation equation",
     PARA{TT "SanityCheck"," is a ",TO2(Boolean,"boolean")," which determines whether 
	 or not to check if a supposed solution of the deformation equation
	 actually satisfies it. Default value is ",TO true},     }

document {
     Key =>HighestOrder,
     Headline => "sets the order to which we compute",
     PARA{TT "HighestOrder"," is an ",TO2(ZZ,"integer")," which 
	  gives an upper bound on to what order
	  a solution of the deformation equation is lifted.
	 Default value is ",TT "20."},     }

document {
     Key =>SmartLift,
     Headline => "chooses lifting to avoid obstructions at next order",
     PARA{TT "SanityCheck"," is a ",TO2(Boolean,"boolean"),". If set to ",TO true,",
	   ",TO versalDeformation," or ",TO localHilbertScheme," will utilize the function
	   ",TO correctDeformation," in order to
	   choose liftings of the 
	   deformation equation at each step which, if possible, 
	  introduce no higher order terms to the obstruction equations. This may increase 
	  the time of calculation, but will hopefully result in nicer equations for the base space.
	  Default value is ",TO true},
	  
	PARA {"For example, consider a degenerate twisted cubic curve:"},
     	EXAMPLE {"S=QQ[x,y,z,w];",
	"F0=matrix {{x*z,y*z,z^2,x^3}}"},
   	PARA {"With the default setting ",TT "SmartLift=>true", " we get very
	     nice equations for the base space:"},
	EXAMPLE {
	"time (F,R,G,C)=localHilbertScheme(F0,Verbosity=>0);",
	"sum G"},
   	PARA {"With the setting ",TT "SmartLift=>false", " the calculation
	     is faster, but the equations are no longer homogeneous:"},
	EXAMPLE {
	"time (F,R,G,C)=localHilbertScheme(F0,SmartLift=>false,Verbosity=>0);",
	"sum G"},
   	       }
	  
document {
     Key =>CorrectionMatrix,
     Headline => "determines the first order deformations used in correcting liftings",
     PARA{TT "CorrectionMatrix"," is either a ",TO String," with value auto or a ",TO Sequence," of 
	  the form output by ",TO correctionMatrix,". If set to auto, ",TO correctionMatrix," is used to calculate
	  the relevant sequence."}}
	
document {
     Key =>t,
     Headline => "deformation parameter",
     PARA {TT "t"," is used as a deformation parameter."},
     SeeAlso => {firstOrderDeformations,liftDeformation,versalDeformation,localHilbertScheme}, 
     }


TEST ///
S = QQ[a,b,c,d]
J = minors(2,matrix{{a,b,c,d^2},{b,c,d,a^3}})
(F,R,G,C)=versalDeformation(gens J,HighestOrder=>2,SmartLift=>false)
assert(sum G==map(target G_0,source G_0,matrix {{t_1*t_12+a^2*t_4*t_12+a*t_7*t_12-d*t_12*t_14-d^2*t_4*t_15-a*d*t_12*t_15-d^2*t_7*t_16-a^2*d*t_12*t_16-a^2*t_1*t_17-a*t_1*t_18-a^2*t_7*t_18+a*d*t_5*t_19+d*t_8*t_19+d^2*t_18*t_19+a*d^2*t_5*t_20+d^2*t_8*t_20+d^3*t_18*t_20+t_2*t_21+a*t_3*t_21+a*d*t_6*t_21+d*t_9*t_21+2*a^2*t_19*t_21+2*a^2*d*t_20*t_21+a^2*d*t_12*t_22+d^3*t_18*t_23+a^2*d*t_21*t_23}, {t_10*t_12+d*t_11*t_12-a^2*t_10*t_17-a*t_10*t_18-t_12*t_21+a^2*t_17*t_21+a*t_18*t_21+t_2*t_22+a*t_3*t_22+2*a^2*t_19*t_22+2*a^2*d*t_20*t_22-a^2*t_4*t_23-a*t_7*t_23}, {t_10*t_21+d*t_11*t_21-t_21^2-t_1*t_22}, {t_12*t_21-t_13*t_21+t_1*t_23}, {t_12^2-t_12*t_13-a^2*t_12*t_17+a^2*t_13*t_17-a*t_12*t_18+a*t_13*t_18+a*t_15*t_21+a^2*t_16*t_21-t_2*t_23-a*t_3*t_23-a^2*t_19*t_23}, {t_12*t_22-t_13*t_22+t_10*t_23-t_21*t_23}}))
///

TEST ///
S = QQ[x_0..x_2,y_0..y_2]
J = minors(2,matrix{{x_0,x_1,x_2},{y_0,y_1,y_2}})
assert (CT^1(gens J)==0)
assert (CT^2(gens J)==0)
assert( numgens source normalModule(0,gens J)==24)
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
assert (sum F==map(target F_0,source F_0,( matrix {{x_1*t_1+x_0*t_2-x_1^2+x_0*x_2, x_0*t_4-x_1*x_2+x_0*x_3, -t_1*t_4-x_3*t_1-x_2*t_2+x_1*t_4-x_2^2+x_1*x_3, t_2*t_3-t_3^2+x_2*t_3-x_1*x_3+x_0*x_4, t_3*t_4-x_4*t_1-x_3*t_2+x_3*t_3-x_2*x_3+x_1*x_4, x_4*t_3-x_3*t_4-x_3^2+x_2*x_4}})))
assert (sum R==map(target R_0, source R_0,matrix {{t_4+x_3, x_2, 0, x_4, x_3, 0, 0, 0}, {-t_2-x_2, t_1-x_1, x_4, 0, -t_3, 0, x_4, x_3}, {x_1, x_0, -x_3, -t_3, 0, x_4, 0, -t_3}, {0, 0, -t_4-x_3, -t_2-x_2, t_1-x_1, 0, -t_4-x_3, -t_3-x_2}, {0, 0, x_2, x_1, x_0, -t_4-x_3, -t_3, 0}, {0, 0, t_1, 0, 0, t_2-t_3+x_2, x_1, x_0}}))
assert (sum G== map(target G_0,source G_0,(matrix {{t_2*t_3-t_3^2}, {t_1*t_3}, {t_3*t_4}})))
assert (sum C==map(target C_0,source C_0,matrix {{0, 0, 0}, {0, 0, 0}, {x_3, -x_4, t_2-t_3}, {t_2+x_2, -x_3, -t_1}, {-t_1+x_1, -x_2, 0}, {-x_4, 0, t_4+x_3}, {0, -x_4, t_2+x_2}, {t_3, -x_3, -t_1+x_1}}))
(F2,R2,G2,C2)=liftDeformation(F1,R1,{},{sub(CT^2(gens I),ring F1_0)}) 
assert(F_2==sub(F2_2,ring F_0))
assert(R_2==sub(R2_2,ring F_0))
assert(G_0==sub(G2_0,ring F_0))
///

TEST ///
S=QQ[x,y,z,w]
F0=matrix {{x*z,y*z,z^2,x^3}}
(F,R,G,C)=localHilbertScheme(F0)
assert (sum F==map(target F_0,source F_0,matrix {{w^2*t_7*t_10*t_11*t_16+2*w^2*t_2*t_11^2*t_16-w^2*t_10^2*t_12+y*w*t_7*t_10*t_16+y*w*t_2*t_11*t_16-w^2*t_3*t_11*t_16+y*w*t_10*t_13+w^2*t_10*t_14-y^2*t_2*t_16-y*w*t_3*t_16-w^2*t_4*t_16+z*w*t_10+x^2*t_12+x*y*t_13+x*w*t_14+x*z, -2*w^2*t_5*t_10*t_11*t_16-w^2*t_7*t_11^2*t_16-w^2*t_10*t_11*t_12-y*w*t_5*t_10*t_16+w^2*t_6*t_10*t_16-2*w^2*t_10^2*t_16+w^2*t_8*t_11*t_16-y*w*t_10*t_12+x*w*t_11*t_12+y*w*t_11*t_13+w^2*t_11*t_14+x*y*t_5*t_16+x*w*t_6*t_16+y^2*t_7*t_16+y*w*t_8*t_16-x*w*t_10*t_16+z*w*t_11+x*y*t_12+y^2*t_13+y*w*t_14+x^2*t_16+y*z, 2*w^2*t_5*t_7*t_10*t_11*t_16^2+w^2*t_7^2*t_11^2*t_16^2+2*w^2*t_5*t_10^2*t_12*t_16-4*w^2*t_2*t_11^2*t_12*t_16+4*w^2*t_5*t_10*t_11*t_13*t_16+2*w^2*t_7*t_11^2*t_13*t_16-2*w^2*t_6*t_7*t_10*t_16^2+4*w^2*t_7*t_10^2*t_16^2-2*y*w*t_2*t_5*t_11*t_16^2-2*w^2*t_2*t_6*t_11*t_16^2-w^2*t_7*t_8*t_11*t_16^2+4*w^2*t_2*t_10*t_11*t_16^2+3*w^2*t_10^2*t_12^2+2*w^2*t_10*t_11*t_12*t_13-2*x*w*t_5*t_10*t_12*t_16-2*y*w*t_7*t_10*t_12*t_16-w^2*t_8*t_10*t_12*t_16-2*y*w*t_2*t_11*t_12*t_16+2*w^2*t_3*t_11*t_12*t_16-2*x*w*t_7*t_11*t_12*t_16-2*w^2*t_6*t_10*t_13*t_16+4*w^2*t_10^2*t_13*t_16-2*y*w*t_7*t_11*t_13*t_16-2*w^2*t_8*t_11*t_13*t_16-w^2*t_5*t_10*t_15*t_16-w^2*t_7*t_11*t_15*t_16+y^2*t_2*t_5*t_16^2+y*w*t_3*t_5*t_16^2+y*w*t_2*t_6*t_16^2+w^2*t_3*t_6*t_16^2-x*y*t_5*t_7*t_16^2-x*w*t_6*t_7*t_16^2-y^2*t_7^2*t_16^2-y*w*t_7*t_8*t_16^2-2*y*w*t_2*t_10*t_16^2-2*w^2*t_3*t_10*t_16^2-2*x*w*t_2*t_11*t_16^2-2*x*w*t_10*t_12^2-2*y*w*t_10*t_12*t_13-2*x*w*t_11*t_12*t_13-2*y*w*t_11*t_13^2-2*w^2*t_10*t_12*t_14-2*w^2*t_11*t_13*t_14-w^2*t_10*t_12*t_15-2*z*w*t_5*t_10*t_16-2*z*w*t_7*t_11*t_16+2*y^2*t_2*t_12*t_16+2*y*w*t_3*t_12*t_16+x*w*t_8*t_12*t_16-2*x*y*t_5*t_13*t_16-2*x*w*t_6*t_13*t_16-2*y^2*t_7*t_13*t_16-y*w*t_8*t_13*t_16+2*x*w*t_10*t_13*t_16+(1/2)*w^2*t_8*t_15*t_16+x*y*t_2*t_16^2+x*w*t_3*t_16^2-x^2*t_7*t_16^2-4*z*w*t_10*t_12-x^2*t_12^2-2*z*w*t_11*t_13-2*x*y*t_12*t_13-y^2*t_13^2-2*x*w*t_12*t_14-2*y*w*t_13*t_14-w^2*t_14^2+x*w*t_12*t_15+y*w*t_13*t_15+w^2*t_14*t_15+z*w*t_8*t_16-2*x^2*t_13*t_16+z*w*t_15+z^2, -2*w^3*t_5*t_10^2*t_11-2*w^3*t_7*t_10*t_11^2-2*w^3*t_2*t_11^3-y*w^2*t_5*t_10^2+w^3*t_6*t_10^2-2*w^3*t_10^3-2*x*w^2*t_5*t_10*t_11-2*y*w^2*t_7*t_10*t_11+w^3*t_8*t_10*t_11-3*y*w^2*t_2*t_11^2+w^3*t_3*t_11^2-x*w^2*t_7*t_11^2-2*w^3*t_1*t_10*t_12-w^3*t_1*t_11*t_13+2*x*w^2*t_6*t_10+y*w^2*t_8*t_10+w^3*t_9*t_10-3*x*w^2*t_10^2+2*y*w^2*t_3*t_11+w^3*t_4*t_11+x*w^2*t_8*t_11-w^3*t_1*t_14+w^3*t_1*t_15+z*w^2*t_1+y^3*t_2+y^2*w*t_3+y*w^2*t_4+x^2*y*t_5+x^2*w*t_6+x*y^2*t_7+x*y*w*t_8+x*w^2*t_9+x^3}}))
assert (sum R==map(target R_0, source R_0,matrix {{-w*t_11-y, -w*t_5*t_11*t_16+w*t_6*t_16-2*w*t_10*t_16+x*t_16, 2*w*t_5*t_10*t_16+2*w*t_7*t_11*t_16+3*w*t_10*t_12+2*w*t_11*t_13-w*t_8*t_16+x*t_12+y*t_13+w*t_14-w*t_15-z, -w^2*t_5*t_10*t_11-w^2*t_7*t_11^2+w^2*t_1*t_5*t_16+w^2*t_6*t_10-2*w^2*t_10^2-x*w*t_5*t_11+w^2*t_8*t_11+w^2*t_1*t_12+x*w*t_6+y^2*t_7+y*w*t_8+w^2*t_9-x*w*t_10+x^2}, {w*t_10+x, w*t_5*t_10*t_16+w*t_7*t_11*t_16+3*w*t_10*t_12+2*w*t_11*t_13+x*t_5*t_16+y*t_7*t_16+x*t_12+y*t_13+w*t_14-w*t_15-z, w*t_7*t_10*t_16+2*w*t_2*t_11*t_16-y*t_2*t_16-w*t_3*t_16, -w^2*t_5*t_10^2-w^2*t_7*t_10*t_11-2*w^2*t_2*t_11^2-y*w*t_7*t_10-y*w*t_2*t_11+w^2*t_3*t_11+w^2*t_1*t_13+y^2*t_2+y*w*t_3+w^2*t_4+x^2*t_5}, {0, w*t_11+y, w*t_10+x, w^2*t_1}, {-t_16, -t_5*t_16^2-2*t_12*t_16, t_7*t_16^2+2*t_13*t_16, w*t_5*t_10*t_16+w*t_10*t_12-x*t_5*t_16-x*t_12-y*t_13-w*t_14-z}}))
assert (sum G== map(target G_0,source G_0,matrix {{t_1*t_16}, {t_9*t_16}, {t_4*t_16}, {t_14*t_16-(1/2)*t_15*t_16}}))
assert (sum C==map(target C_0,source C_0,matrix {{-2*w^3*t_10*t_12-w^3*t_11*t_13+(1/2)*w^3*t_15+z*w^2, w^3*t_10+x*w^2, 0, -w^3*t_1}, {-2*w^3*t_5*t_10*t_12*t_16-w^3*t_5*t_11*t_13*t_16-4*w^3*t_10*t_12^2-2*w^3*t_11*t_12*t_13+(1/2)*w^3*t_5*t_15*t_16+w^3*t_12*t_15+z*w^2*t_5*t_16+2*z*w^2*t_12, w^3*t_5*t_10*t_16+2*w^3*t_10*t_12+x*w^2*t_5*t_16+2*x*w^2*t_12, 2*w^3*t_11*t_12+y*w^2*t_5*t_16+w^3*t_6*t_16-2*w^3*t_10*t_16+2*y*w^2*t_12+x*w^2*t_16, 2*w^3*t_5*t_10*t_11-w^3*t_1*t_5*t_16-2*w^3*t_6*t_10+4*w^3*t_10^2-2*y*w^2*t_7*t_11-w^3*t_8*t_11-2*w^3*t_1*t_12-2*x*y*w*t_5-2*x*w^2*t_6-2*y^2*w*t_7-y*w^2*t_8+2*x*w^2*t_10-2*x^2*w}, {2*w^3*t_7*t_10*t_12*t_16+w^3*t_7*t_11*t_13*t_16+4*w^3*t_10*t_12*t_13+2*w^3*t_11*t_13^2-(1/2)*w^3*t_7*t_15*t_16-w^3*t_13*t_15-z*w^2*t_7*t_16-2*z*w^2*t_13, -w^3*t_7*t_10*t_16-2*w^3*t_10*t_13-x*w^2*t_7*t_16-2*x*w^2*t_13, 2*w^3*t_5*t_10*t_16+w^3*t_7*t_11*t_16+3*w^3*t_10*t_12-y*w^2*t_7*t_16-w^3*t_8*t_16+x*w^2*t_12-y*w^2*t_13-(1/2)*w^3*t_15-z*w^2, -2*w^3*t_5*t_10^2-4*w^3*t_7*t_10*t_11-4*w^3*t_2*t_11^2+w^3*t_1*t_7*t_16-2*x*w^2*t_5*t_10-2*y*w^2*t_7*t_10+w^3*t_8*t_10-2*y*w^2*t_2*t_11+2*w^3*t_3*t_11-2*x*w^2*t_7*t_11+2*w^3*t_1*t_13+2*y^2*w*t_2+2*y*w^2*t_3+w^3*t_4+x*w^2*t_8}, {-3*w^4*t_5*t_7*t_10*t_11*t_16-2*w^4*t_2*t_5*t_11^2*t_16-w^4*t_7^2*t_11^2*t_16+w^4*t_5*t_10^2*t_12-w^4*t_7*t_10*t_11*t_12+2*w^4*t_2*t_11^2*t_12-w^4*t_5*t_10*t_11*t_13-w^4*t_7*t_11^2*t_13-y*w^3*t_5*t_7*t_10*t_16+2*w^4*t_6*t_7*t_10*t_16-4*w^4*t_7*t_10^2*t_16+y*w^3*t_2*t_5*t_11*t_16+w^4*t_3*t_5*t_11*t_16+2*w^4*t_2*t_6*t_11*t_16+w^4*t_7*t_8*t_11*t_16-4*w^4*t_2*t_10*t_11*t_16+y*w^3*t_7*t_10*t_12+w^4*t_8*t_10*t_12+y*w^3*t_2*t_11*t_12-w^4*t_3*t_11*t_12+2*x*w^3*t_7*t_11*t_12+w^4*t_6*t_10*t_13-2*w^4*t_10^2*t_13-x*w^3*t_5*t_11*t_13+2*y*w^3*t_7*t_11*t_13+w^4*t_8*t_11*t_13+w^4*t_7*t_11*t_15+w^4*t_4*t_5*t_16-y*w^3*t_2*t_6*t_16-w^4*t_3*t_6*t_16+x*y*w^2*t_5*t_7*t_16+x*w^3*t_6*t_7*t_16+y^2*w^2*t_7^2*t_16+y*w^3*t_7*t_8*t_16+2*y*w^3*t_2*t_10*t_16+2*w^4*t_3*t_10*t_16+2*x*w^3*t_2*t_11*t_16+2*z*w^3*t_7*t_11-y^2*w^2*t_2*t_12-y*w^3*t_3*t_12+w^4*t_4*t_12-x^2*w^2*t_5*t_12-x*w^3*t_8*t_12+x*w^3*t_6*t_13+y^2*w^2*t_7*t_13-x*w^3*t_10*t_13-2*x*w^3*t_5*t_14+x*w^3*t_5*t_15-(1/2)*w^4*t_8*t_15-x*y*w^2*t_2*t_16-x*w^3*t_3*t_16+x^2*w^2*t_7*t_16-z*w^3*t_8+x^2*w^2*t_13, -w^4*t_5*t_10^2-w^4*t_7*t_10*t_11-2*w^4*t_2*t_11^2-y*w^3*t_7*t_10-y*w^3*t_2*t_11+w^4*t_3*t_11+y^2*w^2*t_2+y*w^3*t_3+w^4*t_4+x^2*w^2*t_5, 0, 0}}))
(F1,R1,G1,C1)=localHilbertScheme(F0,SmartLift=>false)
assert (sum F1==map(target F1_0,source F1_0,matrix {{-w^2*t_5*t_10^2*t_16-w^2*t_7*t_10*t_11*t_16-w^2*t_2*t_11^2*t_16-w^2*t_10^2*t_12+y*w*t_7*t_10*t_16+w^2*t_8*t_10*t_16+y*w*t_2*t_11*t_16+w^2*t_3*t_11*t_16+y*w*t_10*t_13+w^2*t_10*t_14-y^2*t_2*t_16-y*w*t_3*t_16-w^2*t_4*t_16+z*w*t_10+x^2*t_12+x*y*t_13+x*w*t_14+x*z, -2*w^2*t_5*t_10*t_11*t_16-w^2*t_7*t_11^2*t_16-w^2*t_10*t_11*t_12-y*w*t_5*t_10*t_16+w^2*t_6*t_10*t_16-2*w^2*t_10^2*t_16+w^2*t_8*t_11*t_16-y*w*t_10*t_12+x*w*t_11*t_12+y*w*t_11*t_13+w^2*t_11*t_14+x*y*t_5*t_16+x*w*t_6*t_16+y^2*t_7*t_16+y*w*t_8*t_16-x*w*t_10*t_16+z*w*t_11+x*y*t_12+y^2*t_13+y*w*t_14+x^2*t_16+y*z, 2*w^2*t_5*t_7*t_10*t_11*t_16^2+w^2*t_7^2*t_11^2*t_16^2-2*w^2*t_7*t_10*t_11*t_12*t_16-4*w^2*t_2*t_11^2*t_12*t_16+4*w^2*t_5*t_10*t_11*t_13*t_16+2*w^2*t_7*t_11^2*t_13*t_16-2*w^2*t_6*t_7*t_10*t_16^2+4*w^2*t_7*t_10^2*t_16^2-2*y*w*t_2*t_5*t_11*t_16^2-2*w^2*t_2*t_6*t_11*t_16^2-w^2*t_7*t_8*t_11*t_16^2+4*w^2*t_2*t_10*t_11*t_16^2-w^2*t_10^2*t_12^2-2*y*w*t_7*t_10*t_12*t_16-2*y*w*t_2*t_11*t_12*t_16+2*w^2*t_3*t_11*t_12*t_16+2*y*w*t_5*t_10*t_13*t_16-2*w^2*t_6*t_10*t_13*t_16+4*w^2*t_10^2*t_13*t_16-2*w^2*t_8*t_11*t_13*t_16+y^2*t_2*t_5*t_16^2+y*w*t_3*t_5*t_16^2+y*w*t_2*t_6*t_16^2+w^2*t_3*t_6*t_16^2-x*y*t_5*t_7*t_16^2-x*w*t_6*t_7*t_16^2-y^2*t_7^2*t_16^2-y*w*t_7*t_8*t_16^2-2*y*w*t_2*t_10*t_16^2-2*w^2*t_3*t_10*t_16^2-2*x*w*t_2*t_11*t_16^2+2*x*w*t_10*t_12^2+2*y*w*t_10*t_12*t_13+2*w^2*t_10*t_12*t_14-w^2*t_10*t_12*t_15+2*y^2*t_2*t_12*t_16+2*y*w*t_3*t_12*t_16-2*x*y*t_5*t_13*t_16-2*x*w*t_6*t_13*t_16-2*y^2*t_7*t_13*t_16-2*y*w*t_8*t_13*t_16+2*x*w*t_10*t_13*t_16+x*y*t_2*t_16^2+x*w*t_3*t_16^2-x^2*t_7*t_16^2-x^2*t_12^2-2*x*y*t_12*t_13-y^2*t_13^2-2*x*w*t_12*t_14-2*y*w*t_13*t_14-w^2*t_14^2+x*w*t_12*t_15+y*w*t_13*t_15+w^2*t_14*t_15-2*x^2*t_13*t_16+z*w*t_15+z^2, w^3*t_5*t_10^2*t_11+w^3*t_7*t_10*t_11^2+w^3*t_2*t_11^3-w^3*t_6*t_10^2+w^3*t_10^3-w^3*t_8*t_10*t_11-w^3*t_3*t_11^2+2*w^3*t_1*t_10*t_12+w^3*t_1*t_11*t_13+w^3*t_9*t_10+w^3*t_4*t_11-w^3*t_1*t_14+w^3*t_1*t_15+z*w^2*t_1+y^3*t_2+y^2*w*t_3+y*w^2*t_4+x^2*y*t_5+x^2*w*t_6+x*y^2*t_7+x*y*w*t_8+x*w^2*t_9+x^3}}))
assert (sum R1==map(target R1_0, source R1_0,matrix {{-w*t_11-y, -w*t_5*t_11*t_16+w*t_6*t_16-2*w*t_10*t_16+x*t_16, -w*t_10*t_12+x*t_12+y*t_13+w*t_14-w*t_15-z, w^2*t_5*t_10*t_11+w^2*t_1*t_5*t_16-w^2*t_6*t_10+w^2*t_10^2-x*w*t_5*t_11+w^2*t_1*t_12+x*w*t_6+y^2*t_7+y*w*t_8+w^2*t_9-x*w*t_10+x^2}, {w*t_10+x, -w*t_5*t_10*t_16-w*t_7*t_11*t_16-w*t_10*t_12+x*t_5*t_16+y*t_7*t_16+w*t_8*t_16+x*t_12+y*t_13+w*t_14-w*t_15-z, w*t_7*t_10*t_16+2*w*t_2*t_11*t_16-y*t_2*t_16-w*t_3*t_16, w^2*t_7*t_10*t_11+w^2*t_2*t_11^2-y*w*t_7*t_10-w^2*t_8*t_10-y*w*t_2*t_11-w^2*t_3*t_11+w^2*t_1*t_13+y^2*t_2+y*w*t_3+w^2*t_4+x^2*t_5}, {0, w*t_11+y, w*t_10+x, w^2*t_1}, {-t_16, -t_5*t_16^2-2*t_12*t_16, t_7*t_16^2+2*t_13*t_16, w*t_5*t_10*t_16+w*t_10*t_12-x*t_5*t_16-x*t_12-y*t_13-w*t_14-z}}))
assert (sum G1== map(target G1_0,source G1_0,matrix {{t_1*t_16}, {2*t_5*t_10*t_11*t_16+t_7*t_11^2*t_16-2*t_6*t_10*t_16+3*t_10^2*t_16-t_8*t_11*t_16+t_9*t_16}, {t_5*t_10^2*t_16+2*t_7*t_10*t_11*t_16+3*t_2*t_11^2*t_16-t_8*t_10*t_16-2*t_3*t_11*t_16+t_4*t_16}, {-t_5*t_10*t_16^2-t_7*t_11*t_16^2-2*t_10*t_12*t_16-t_11*t_13*t_16+(1/2)*t_8*t_16^2+t_14*t_16-(1/2)*t_15*t_16}}))
assert (sum C1==map(target C1_0,source C1_0,matrix {{-w^3*t_5*t_10*t_16-w^3*t_7*t_11*t_16+(1/2)*w^3*t_8*t_16+(1/2)*w^3*t_15+z*w^2, w^3*t_10+x*w^2, 0, -w^3*t_1}, {-2*w^3*t_7*t_11*t_12*t_16+w^3*t_5*t_11*t_13*t_16+w^3*t_8*t_12*t_16-w^3*t_5*t_14*t_16+w^3*t_5*t_15*t_16+w^3*t_12*t_15+z*w^2*t_5*t_16+2*z*w^2*t_12, w^3*t_5*t_10*t_16+2*w^3*t_10*t_12+x*w^2*t_5*t_16+2*x*w^2*t_12, 2*w^3*t_11*t_12+y*w^2*t_5*t_16+w^3*t_6*t_16-2*w^3*t_10*t_16+2*y*w^2*t_12+x*w^2*t_16, 4*w^3*t_5*t_10*t_11+2*w^3*t_7*t_11^2+2*y*w^2*t_5*t_10-2*w^3*t_6*t_10+4*w^3*t_10^2-2*w^3*t_8*t_11-2*w^3*t_1*t_12-2*x*y*w*t_5-2*x*w^2*t_6-2*y^2*w*t_7-2*y*w^2*t_8+2*x*w^2*t_10-2*x^2*w}, {-2*w^3*t_7*t_10*t_12*t_16+2*w^3*t_5*t_10*t_13*t_16+w^3*t_7*t_11*t_13*t_16-w^3*t_8*t_13*t_16+w^3*t_7*t_14*t_16-w^3*t_7*t_15*t_16-w^3*t_13*t_15-z*w^2*t_7*t_16-2*z*w^2*t_13, -w^3*t_7*t_10*t_16-2*w^3*t_10*t_13-x*w^2*t_7*t_16-2*x*w^2*t_13, w^3*t_5*t_10*t_16+w^3*t_10*t_12-w^3*t_11*t_13-y*w^2*t_7*t_16-(1/2)*w^3*t_8*t_16+x*w^2*t_12-y*w^2*t_13-(1/2)*w^3*t_15-z*w^2, w^3*t_5*t_10^2-w^3*t_2*t_11^2-2*y*w^2*t_7*t_10-w^3*t_8*t_10-2*y*w^2*t_2*t_11+2*w^3*t_1*t_13+2*y^2*w*t_2+2*y*w^2*t_3+w^3*t_4}, {w^4*t_5^2*t_10^2*t_16-w^4*t_5*t_7*t_10*t_11*t_16+w^4*t_2*t_5*t_11^2*t_16-w^4*t_7^2*t_11^2*t_16+3*w^4*t_7*t_10*t_11*t_12+5*w^4*t_2*t_11^2*t_12-3*w^4*t_5*t_10*t_11*t_13-w^4*t_7*t_11^2*t_13-y*w^3*t_5*t_7*t_10*t_16+2*w^4*t_6*t_7*t_10*t_16-w^4*t_5*t_8*t_10*t_16-4*w^4*t_7*t_10^2*t_16+y*w^3*t_2*t_5*t_11*t_16-w^4*t_3*t_5*t_11*t_16+2*w^4*t_2*t_6*t_11*t_16+w^4*t_7*t_8*t_11*t_16-4*w^4*t_2*t_10*t_11*t_16+2*x*w^3*t_5*t_10*t_12+y*w^3*t_7*t_10*t_12-w^4*t_8*t_10*t_12+y*w^3*t_2*t_11*t_12-3*w^4*t_3*t_11*t_12-2*y*w^3*t_5*t_10*t_13+w^4*t_6*t_10*t_13-2*w^4*t_10^2*t_13+x*w^3*t_5*t_11*t_13+w^4*t_8*t_11*t_13-w^4*t_5*t_10*t_15+w^4*t_4*t_5*t_16-y*w^3*t_2*t_6*t_16-w^4*t_3*t_6*t_16+x*y*w^2*t_5*t_7*t_16+x*w^3*t_6*t_7*t_16+y^2*w^2*t_7^2*t_16+y*w^3*t_7*t_8*t_16+2*y*w^3*t_2*t_10*t_16+2*w^4*t_3*t_10*t_16+2*x*w^3*t_2*t_11*t_16-2*z*w^3*t_5*t_10-y^2*w^2*t_2*t_12-y*w^3*t_3*t_12+w^4*t_4*t_12-x^2*w^2*t_5*t_12+x*w^3*t_6*t_13+y^2*w^2*t_7*t_13+y*w^3*t_8*t_13-x*w^3*t_10*t_13-2*x*w^3*t_5*t_14+x*w^3*t_5*t_15-x*y*w^2*t_2*t_16-x*w^3*t_3*t_16+x^2*w^2*t_7*t_16+x^2*w^2*t_13, w^4*t_7*t_10*t_11+w^4*t_2*t_11^2-y*w^3*t_7*t_10-w^4*t_8*t_10-y*w^3*t_2*t_11-w^4*t_3*t_11+y^2*w^2*t_2+y*w^3*t_3+w^4*t_4+x^2*w^2*t_5, 0, 0}}))
FC=(correctDeformation(F1_{0,1,2},R1_{0,1,2},G1_{0},C1_{0}))_0
assert (sub(sum FC,ring F_0)==sum F_{0,1,2})
///