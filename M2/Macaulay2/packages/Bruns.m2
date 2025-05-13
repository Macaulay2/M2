---------------------------------------------------------------------------
-- PURPOSE : to compute the Brunsification of a module; that is,
-- produce an ideal with three generators whose 2nd syzygy module is 
-- isomorphic to a given module
-- PROGRAMMERs : Brunsification code written by David Eisenbud   
--     	    	 Documentation edited by Sonja Petrovic and Adam Van Tuyl.
-- UPDATE HISTORY : bruns2.0 created 28 June 2008
-- 	     	    updated 1 July 2008
--     	    	    Minor typos in documentation fixed: 7 Oct 2008 (SP)
--     	    	    Added function brunsIdeal 8 Oct 2008 (SP and AVT)
-- 
---------------------------------------------------------------------------

newPackage(
     "Bruns",
     Version => "2.0",
     Date => "June 28, 2008",
     Authors =>{{Name => "David Eisenbud",
	       Email => "de@msri.org",
	       HomePage=>"http://www.msri.org/~de"}},
     Headline => "make a 3-generator ideal with an \"any\" resolution",
     Keywords => {"Commutative Algebra"},
     PackageImports => {"OldChainComplexes"},
     DebuggingMode => false
     )

export{"bruns", 
       "brunsIdeal",
       "elementary", 
       "evansGriffith",
       "isSyzygy"}
       

--------------------------------------------------------------
-- bruns
-- returns a 3-generated ideal whose second syzygy module agrees
-- with the input
--------------------------------------------------------------

bruns = method(TypicalValue => Ideal)
bruns Matrix := f->(
     --given a matrix f, whose cokernel is a 2-rd syzygy,
     --bruns f returns a 3-generator ideal whose second syzygy is the image of f
     f1:=evansGriffith(f,2);
     FF:=res coker(transpose f1);
     g:=transpose FF.dd_2;
     --the row degrees of g are in the reverse order for bruns 1, so reverse them
     Lsource := flatten degrees source g;
     Ltar := flatten degrees target g;
     grev:=map((ring g)^(-reverse Ltar), (ring g)^(-Lsource), g^(reverse splice {0..#Ltar-1}));
     h:=evansGriffith(grev,1);
     KK:=res coker transpose h;
     ideal transpose KK.dd_2)

bruns Module := M->(
     --Given a module M that is at least a 3rd syzygy, 
     --the function returns a 3-generator ideal with 2nd syzygy
     --isomorphic to M.
     --It does this by constructing a matrix f whose image is M and 
     --whose cokernel is a second syzygy, and then calling 
     --bruns f.
     ff:=presentation M;
     ft:= syz transpose ff;
     bruns transpose ft)



--------------------------------------------------------------
-- brunsIdeal
-- returns a 3-generated ideal whose second syzygy module agrees
-- with the second syzygy module of the inputted ideal
--------------------------------------------------------------


brunsIdeal = method();
brunsIdeal Ideal := I->(
     --given ideal I, compute its 3rd syzygy module and brusify it!
    --S = ring I;
    F := res I;
    M := image F.dd_3;
    -- f=F.dd_3;
    bruns M
    )


--------------------------------------------------------------
-- elementary
-- a function used for brunsification
--------------------------------------------------------------

elementary=method(TypicalValue=>Matrix)
elementary(Matrix, ZZ, ZZ) := (f,k,m)->(
     --Takes a matrix f, an integer k whose value is strictly less than the 
     --number of rows of f, and a positive  integer m  The routine
     --adds rand multiples of the last row, whose coefficients are polynomials
     --in the first m variables,  to the k preceding rows
     --and drops the last row. For this to be effective, the target degrees of f
     --must be in ascending order.
     S:=ring f;
     L  :=-flatten degrees target f;
     b := #L;
     if k>=b then error("value of k is too large");
     L0 :=L_{b-1};
     L1 :=L_{0..b-2};
     L2 :=L_{0..b-2-k};
     L3 :=L_{b-1-k..b-2};
     m11 := map(S^L1,S^L1,(i,j)-> if i==j then 1_S else 0_(ring f));
     m12 := map(S^L2,S^L0,(i,j)->0_(ring f));
     Sk:=(coefficientRing S)[S_0..S_(m-1)];
     m22k:=random(Sk^L3, Sk^L0);
     m22:=substitute(m22k, S);
     --     g:=transpose gens ideal (vars S)_{0..m-1};
     --     m22 := random(S^L3, (target g)**S^L0) * (map((target g)**S^L0, (source g)**S^L0, g));
     --     m22 := random(S^L3,S^L0);
     --    error("debug");
     (m11|(m12||m22))*f
     )


--------------------------------------------------------------
-- evansGriffith
-- a function used by bruns in the brunsification process
--------------------------------------------------------------

evansGriffith = method(TypicalValue => Matrix)
evansGriffith(Matrix, ZZ) := (f,n)->(
     --f must be a matrix over a polynomial ring S
     --whose cokernel is an n-th syzygy.
     --The result f1=evansGriffith(f,n) 
     --is a matrix with the same source and kernel as f but
     --such that coker f1 is an nth syzygy of rank n.
     --rank target f1 = (rank f)+n. 
     --The routine reduces the target of f by elementary moves 
     --involving just n+1 variables.
     --The outcome is probabilistic, but if the routine fails, it 
     --gives an error message.
     N:=numgens ring f;
     f1:=transpose sort(transpose f, DegreeOrder=>Descending); -- this is f with rows sorted so that the degrees are ascending.
     if flatten degrees target f1 =!= sort flatten degrees target f then error("target degrees not ascending");
     --made change here: AVTSP
     if not isSyzygy(coker f1,n) then error("cokernel of input matrix is not an appropriate syzygy");
     r:=rank f1;
     b:=rank target f1;
     loopcount:=0;
     while r+n<b do(
	  j:=0;
	  ftemp:=elementary(f1,j,n+1);
	  while (rank ftemp =!= r or not isSyzygy(coker ftemp,n)) do(
	       if j<b-1 then j=j+1 
	            else (loopcount=loopcount+1; 
			 print(loopcount);
			 if loopcount>5 then error("Transformation is not random enough");
			 );
		    	       ftemp=elementary(f1,j, n+1));
	 b=b-1;
	 f1=ftemp);
    f1)



--------------------------------------------------------------
-- isSyzgy
-- checks if a module is a d-th syzygy
--------------------------------------------------------------

isSyzygy=method(TypicalValue=>Boolean)
isSyzygy(Module,ZZ) := (M,d)->(
     --tests whether coker f is a d-th syzygy
     --You would THINK that the LenghtLimit bounds below 
     --would SPEED things up, but
     --in fact they SLOW things a little, at least in the example below.
     --     F:=res (coker transpose f, LengthLimit => d+1);
     --     G:=res (coker transpose F.dd_(d+1), LengthLimit=>d+1);
     f := presentation M;
     F:=res coker transpose f;
     G:=res coker transpose F.dd_(d+1);
     value:=true;
     for i from 2 to d+1 do
         value = (value and sort flatten degrees G_i == sort(-flatten degrees F_(d+1-i)));
     value
     )


--- Contributions to Documentation by Sonja Petrovic and Adam Van Tuyl 
--- Snowbird, M2 Workshop, June-July 2008 

beginDocumentation()

---------------------------------------------------------
-- DOCUMENTATION Bruns
---------------------------------------------------------

doc ///
Key 
   Bruns
Headline 
   produces an ideal with three generators whose 2nd syzygy module is isomorphic to a given module
Description 
  Text
    {\em Bruns}  is a package of functions for transforming syzygies. 
    
    A well-known paper of Winfried Bruns, entitled  
    {\bf ''Jede'' freie Aufl\"osung ist freie Aufl\"osung eines drei-Erzeugenden Ideals }
    (J. Algebra 39 (1976), no. 2, 429-439),
    shows that every second syzygy module is the second syzygy module of an ideal with three generators.
    
    The general context of this result uses the theory of ''basic elements'', a
    commutative algebra version of the general position arguments of the algebraic
    geometers.   The ''Syzygy Theorem'' of Evans and Griffiths 
    ({\bf Syzygies.} London Mathematical Society Lecture Note Series, 106. Cambridge University Press, Cambridge, 1985)
    asserts that if a module M over a regular local ring S containing a field (the field is conjecturally not
    necessary), or a graded module over a polynomial ring S, is a k-th syzygy module but not a free module,
    then M has rank at least k. The theory of basic elements shows that if M is a k-th syzygy
    of rank >k, then for a ''sufficiently general'' element m of M the module M/Sm is again a k-th syzygy. 
    
    The idea of Bruns' theorem is that if M is a second syzygy module, then factoring out (rank M) - 2 general elements gives
    a second syzygy N of rank 2. It turns out that three general homomorphisms from M to S
    embed N in S^3 in such a way that the quotient S^3/N is an ideal generated by three elements.
    
    This package implements this method.
///

---------------------------------------------------------
-- DOCUMENTATION bruns
---------------------------------------------------------

doc ///
Key
  bruns
  (bruns,Module)
  (bruns,Matrix)
Headline
  Returns an ideal generated by three elements whose 2nd syzygy module is isomorphic to a given module
Usage
  j= bruns M or  j= bruns f
Inputs
  M:Module
    a second syzygy (graded) module
  f:Matrix
    whose cokernel is a second syzygy (graded) module
Outputs
  j:Ideal
    a homogeneous ideal generated by three elements whose second syzygy module is isomorphic to M, or image f
Description
  Text
    This function takes a graded module M over a polynomial ring S that
    is a second syzygy, and returns a three-generator ideal j whose second syzygy is M,
    so that the resolution of S/j, from the third step, is isomorphic to the resolution of M.
    Alternately {\tt bruns} takes
    a matrix whose cokernel is a second syzygy, and finds a 3-generator
    ideal whose second syzygy is the image of that matrix.
  Example
    kk=ZZ/32003
    S=kk[a..d]
    i=ideal(a^2,b^2,c^2, d^2)
    betti (F=res i)
    M = image F.dd_3
    f=F.dd_3
    j=bruns M;
    betti res j -- the ideal has 3 generators
  Text
    Here is a more complicated example, also involving a complete
    intersection.  You can see that columns three and four in the two
    Betti diagrams are the same.
  Example
    kk=ZZ/32003
    S=kk[a..d]
    i=ideal(a^2,b^3,c^4, d^5)
    betti (F=res i)
    M = image F.dd_3
    f=F.dd_3
    j1=bruns f; 
    betti res j1 
    j=bruns M;
    betti res j 
  Text
    In the next example, we perform the "Brunsification" of a rational curve.
  Example
    kk=ZZ/32003
    S=kk[a..e]
    i=monomialCurveIdeal(S, {1,3,4,5})
    betti (F=res i)
    time j=bruns F.dd_3;
    betti res j
SeeAlso
    brunsIdeal
///


---------------------------------------------------------
-- DOCUMENTATION brunsIdeal
---------------------------------------------------------

doc ///
Key
  brunsIdeal
  (brunsIdeal,Ideal)
Headline
  Returns an ideal generated by three elements whose 2nd syzygy module agrees with the given ideal
Usage
  j = brunsIdeal i
Inputs
  i:Ideal
    a homogeneous ideal
Outputs
  j:Ideal
    a homogeneous ideal generated by three elements whose second syzygy module is isomorphic the second syzygy module of the ideal i.
Description
  Text
    This function is a special case of the function @TO bruns @.  Given an ideal, the
    user can find another ideal which is 3-generated, and furthermore, the second syzygy modules
    of both ideals are isomorphic.  Although one can use @TO bruns @ to do this procedure,
    this function cuts out some of the steps.
  Example
    kk=ZZ/32003
    S=kk[a..d]
    i=ideal(a^2,b^2,c^2, d^2)
    betti (F=res i)
    M = image F.dd_3
    j1 = bruns M
    betti res j1    
    j2=brunsIdeal i
    betti res j2
    (betti res j1) == (betti res j2)
SeeAlso
    bruns
///

---------------------------------------------------------
-- DOCUMENTATION elementary
---------------------------------------------------------

doc ///
Key
  elementary
  (elementary,Matrix,ZZ,ZZ)
Headline
  Elementary moves are used to reduce the target of a syzygy matrix
Usage
  g= elementary(f,k,m)
Inputs
  f:Matrix
    whose target degrees are in ascending order
  k:ZZ
    whose value is strictly less than the number of rows of f
  m:ZZ
    positive
Outputs
  g:Matrix
    obtained from f by adding random multiples of the last row by polynomials in the first m variables to the k preceding rows, 
    and then deleting the last row.
Description
  Text
    Factors out a general element, reducing the rank of f.      
    More precisely, the routine adds random multiples of the last row, whose coefficients are polynomials
    in the first m variables,  to the k preceding rows and drops the last row. 
    For this to be effective, the target degrees of f must be in ascending order.
    
    This is a fundamental operation in the theory of basic elements, see D. Eisenbud and E. G. Evans, 
    {\em Basic elements: theorems from algebraic k-theory}, Bulletin of the AMS, {\bf 78}, No.4, 1972, 546-549.
    
    Here is a basic example:
  Example 
    kk=ZZ/32003
    S=kk[a..d]
    M=matrix{{a,0,0,0},{0,b,0,0},{0,0,c,0},{0,0,0,d}}
    elementary(M,0,1)-- since k=0, this command simply eliminates the last row of M.
  Text
    Here is a more involved example. This is also how this function is used within the package.
  Example
    kk=ZZ/32003
    S=kk[a..d]
    I=ideal(a^2,b^3,c^4, d^5)
    F=res I
    M=image F.dd_3
    f=matrix gens M
    fascending=transpose sort(transpose f, DegreeOrder=>Descending) -- this is f with rows sorted so that the degrees are ascending.
    g=elementary(fascending,1,1) --k=1, so add random multiples of the last row to the preceding row
    g1=elementary(fascending,1,3)
  Text
    This method is called by @TO evansGriffith @. 
///

---------------------------------------------------------
-- DOCUMENTATION evansGriffith
---------------------------------------------------------

doc ///
Key
  evansGriffith
  (evansGriffith,Matrix,ZZ)
Headline
  Reduces the rank of a syzygy
Usage
  N = evansGriffith(M,d)
Inputs
  M:Matrix
   over a polynomial ring whose cokernel is an d-th syzygy.
  d:ZZ
   positive
Outputs
  N:Matrix
   with the same source and kernel as M, but such that 
   coker N is a dth syzygy of rank d.
Description
  Text
   The routine reduces the target of M by elementary moves (see
   @TO elementary @) involving just d+1 variables.
   The outcome is probabilistic, but if the routine fails, it 
   gives an error message.  
   
   See the book of Evans and Griffith
   ({\it Syzygies.} London Mathematical Society Lecture Note Series, 106. Cambridge University Press, Cambridge, 1985.)
  Example
   kk=ZZ/32003
   S=kk[a..e]
   i=ideal(a^2,b^3,c^4, d^5)
   F=res i
   f=F.dd_3
   EG = evansGriffith(f,2)  -- notice that we have a matrix with one less row, as described in elementary, and the target module rank is one less.
   isSyzygy(coker EG,2)
  Text
   This is called within @TO bruns @. 
///

---------------------------------------------------------
-- DOCUMENTATION isSyzygy
---------------------------------------------------------

doc ///
Key
  isSyzygy
  (isSyzygy,Module,ZZ)
Headline
  Tests if a module is a d-th syzygy
Usage
  b = isSyzygy(M,d) 
Inputs
  M:Module
   over a polynomial ring.
  d:ZZ
   positive
Outputs
  b:Boolean
   returns {\tt true} if M is a d-th syzygy, and {\tt false} otherwise.
Description
  Text
    This algorithm is based upon the methods described
    in the book of Evans and Griffith ({\em Syzygies}. 
    London Mathematical Society Lecture Note Series, 106. Cambridge University Press, Cambridge, 1985.) 
  Example
    kk=ZZ/32003
    S=kk[a..d]
    F=res (ideal vars S)^2
  Text
    {\bf NOTE:}  We are viewing a syzygy module as a cokernel of an appropriate map.
  Example  
    isSyzygy(coker F.dd_3,3)  -- the cokernel defined by the 3rd map is not a 3rd syzygy
    isSyzygy(coker F.dd_4,3)  -- the cokernel defined by the 4th map is a 3rd syzygy
  Text
    This function is called within @TO evansGriffith @.  
    ///


TEST/// 
  kk=ZZ/32003
  S=kk[a..d]
  F=res (ideal vars S)^2
  assert(isSyzygy(coker F.dd_3,3)==false)
  assert(isSyzygy(coker F.dd_4,3)==true)
///
TEST /// 
   kk=ZZ/32003
   S=kk[a..d]
   i=ideal(a^2,b^3,c^4, d^5)
   F=res i
   f=F.dd_3
   EG = evansGriffith(f,2)  
   assert(isSyzygy(coker EG,2)==true)
///
TEST/// 
    kk=ZZ/32003
    S=kk[a..d]
    M=matrix{{a,0,0,0},{0,b,0,0},{0,0,c,0},{0,0,0,d}}
    E=elementary(M,0,1)
    assert(rank M -1 == rank E)
///
TEST///
    kk=ZZ/32003
    S=kk[a..d]
    i=ideal(a^2,b^2,c^2, d^2)
    betti (F=res i)
    M = image F.dd_3
    f=F.dd_3
    g=bruns f   
    assert((#flatten entries gens g) == 3)
///
TEST///
    kk=ZZ/32003
    S=kk[a..d]
    i=ideal(a^2,b^2,c^2, d^2)
    F=res i
    M = image F.dd_3
    j1 = bruns M
    j2 = brunsIdeal i
    assert((betti res j1) == (betti res j2))
///

end 


restart
installPackage ("Bruns", UserMode=>true)
loadPackage "Bruns"
viewHelp

restart
installPackage ("Bruns")
loadPackage "Bruns"
check Bruns




---------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
--Here is some useful examples not included in documentation, for example
--the times for various operations.

--brunsification of a complete intersection
kk=ZZ/32003
S=kk[a..e]
i=ideal(a^2,b^3,c^4, d^5)
betti (F=res i)
F.dd_4
time (j=bruns coker F.dd_4)
betti res j
time j=bruns image F.dd_3;
betti res j
f=F.dd_3
time j=bruns f; -- 25 sec on macbook pro
betti gens j
betti res j 
time (f1=evansGriffith(f,3);)
time (f1=evansGriffith(f,2);)
evansGriffith(f,2)
isSyzygy(f,2)

kk=ZZ/32003
S=kk[x_0..x_5]
i=ideal (vars S)^[2]
betti (F=res i)
f=F.dd_(codim i-1)
isSyzygy(f, 4) --true
betti res coker f

evansGriffith(f,2)
time j=bruns f;

betti (G=res j) 
r=rank G.dd_3
j3=minors(r,G.dd_3);
codim j3==3

restart
loadPackage "Bruns"
kk=ZZ/32003
S=kk[a..f]
m=matrix"a,b,c,d,e,0;0,a,b,c,d,e"
n=transpose syz m
time j= bruns n; -- 20 sec on macbook pro  --15 seconds on an iMac
betti (G=res j)
r=rank G.dd_3

---

S=QQ[a,b,c,d]
j=ideal"b2d2+d4,b2c2+a2d2+c2d2,b4-b2d2-d4"
betti (G=res j)
codim minors(3, G.dd_3)
bruns G.dd_4

--try a small field
kk=ZZ/2
S=kk[a..e]
i=ideal(a^2,b^3,c^4, d^5)
betti (F=res i)
f=F.dd_3;
time f1=evansGriffith(f,2);
time j=bruns f;

--5 variables
kk=ZZ/101
S=kk[a..e]
i=ideal(a^3,b,c,d,e)
betti (F=res i)
f=F.dd_4;
--time f1=evansGriffith(f,2);
time j=bruns f;
betti res j
--(ass j)/codim
k=top j;
betti res k

--brunsification of a monomial curveideal
kk=ZZ/32003
S=kk[a..e]
i=monomialCurveIdeal(S, {1,3,6,7})
betti (F=res i)
--          0: 1 .  . . .
--          1: . 2  . . .
--          2: . 3  5 1 .
--          3: . 1  6 7 2
time j=bruns F.dd_3
--229 sec v 2.0 does it in 60 sec
betti res j
--          0: 1 . . . .
--          1: . . . . .
--          2: . . . . .
--          3: . . . . .
--          4: . . . . .
--          5: . . . . .
--          6: . . . . .
--          7: . 3 . . .
--          8: . . . . .
--          9: . . . . .
--         10: . . . . .
--         11: . . . . .
--         12: . . . . .
--         13: . . 5 1 .
--         14: . . 3 7 2



--Oddity: setting LengthLimit slows things down instead of speeding them up. 
--this used to run MUCH more slowly!

restart 
loadPackage "Bruns"
S=kk[a..e]
i=ideal(a^2,b^3,c^4, d^5)
betti (F=res i)
f=F.dd_3
f1=elementary(f,3,5);
g=transpose syz transpose f1;
time FF=res (coker transpose g, LengthLimit=>2)
betti FF
time GG=res (coker transpose FF.dd_2, LengthLimit=>2)
--237 sec -- 2008: now 1.7 sec
time GG=res (coker transpose FF.dd_2)
-- 55 seconds -- now 1.09 sec                                                                      

restart
--how to make a matrix whose elements are random elements of a given ideal I
kk=QQ
S=kk[a..e]
L0 = {-6}
L3={-2,-3,-4,-5}
I=(ideal (vars S)_{0,1})^2
g=transpose gens I
random(S^L3, (target g)**S^L0) * (map((target g)**S^L0, (source g)**S^L0, g))
betti oo


----------------------

