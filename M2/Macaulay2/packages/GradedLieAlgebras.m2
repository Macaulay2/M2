
 --------------------------------------------------------------------------------
-- Copyright 2018  Clas L\"ofwall and Samuel Lundqvist
-- 
-- You may redistribute this program under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 2 of the
-- License, or any later version.
--------------------------------------------------------------------------------

--"

newPackage(
	"GradedLieAlgebras",
	Version => "2.0",
	Date => "Nov 2018",
	Authors => {{		  Name => "Clas Löfwall",
		  Email => "clas.lofwall@gmail.com"},
	      {		  Name => "Samuel Lundqvist",
		  Email => "samuel@math.su.se"} 
	      },
	AuxiliaryFiles => true,
        DebuggingMode => false,
	Headline => "graded Lie algebras"
   	) 
 
export {
    "annLie",
    "axiomsLie",
    "basisLie",
    "boundariesBasisLie",
    "boundariesTableLie",
    "centerLie",
    "centerAllLie",
    "characterLie",
    "coeffsLie",
    "compdeg",
    "cyclesBasisLie",
    "cyclesTableLie",
    "decompidealLie",
    "defLie",
    "degLie",
    "DerLie",
    "derLie",
    "diffl",
    "diffLie",
    "diffLieAlgebra",
    "dimsLie",
    "dimTableLie",
    "dimtotLie",
    "divisorLie",
    "eulerLie",
    "extBasisLie",
    "extRepRing",
    "extTableLie",
    "extMultLie",
    "field",
    "genDiffs",
    "genWeights",
    "genSigns",
    "gensLie",
    "holonomyLie",
    "homologyBasisLie",
    "homologyTableLie",
    "idealBasisLie",
    "idealTableLie",
    "idMapLie",
    "imageBasisLie",
    "imageTableLie",
    "imapLie",
    "indexFormLie",
    "innerDerLie",
    "intersectionLie",
    "invImageBasisLie",
    "invImageLie",
    "kernelBasisLie",
    "kernelTableLie",
    "koszulDualLie",
    "LieAlgebra",
    "lieAlgebra",
    "LieElement",
    "lieRing",
    "localLie",
    "MapLie",
    "mapLie",
    "maplie",
    "mbRing",
    "minmodel",
    "minmodelLie",
    "minPresLie",
    "modelmap",
    "monomialsLie",
    "multLie",
    "multListLie",
    "multOnly",
    "normalFormLie",
    "peekLie",
    "permopLie",
    "relsLie",
    "randomLie",
    "sign",
    "signExtLie",
    "signLie",
    "sourceLie",
    "subalgBasisLie",
    "subalgTableLie",
    "symmetryLie",
    "targetLie",
    "useLie", 
    "weight",
    "weightExtLie",
    "weightLie",
    "whichLie",
    "zz"
    }

recursionLimit=10000;


----------------------------------------
--
--TYPES AND CONSTRUCTORS
--
----------------------------------------


LieAlgebra = new Type of MutableHashTable
LieElement = new Type of BasicList
MapLie = new Type of MutableHashTable
DerLie = new Type of MutableHashTable

globalAssignment LieAlgebra
globalAssignment MapLie
globalAssignment DerLie




local LL;

--"

--   Any Lie algebra L is a type and its elements belong to the type LieElement, which
--   is the parent of L. Also L is a member of the type LieAlgebra and is a 
--   MutableHashTable (which is the parent of LieAlgebra). An element in LieElement 
--   is of the form {{c1,c2,..},{{i01,i02,..},{i11,i12,..},...}} where all {} are
--   BasicLists and c1,c2,... belong to the field specified in L 
--   and ipq are natural numbers corresponding to the generators in L and a list
--   of them corresponds to an iterated Lie product of the generators. The real content
--   of a LieElement y may be looked upon by writing y#0 and y#1. The pretty printing
--   of a LieElement is a linear combination of arrays of the generators and input
--   should be written as a linear combination of iterated Lie monomials where "SPACE"
--   is used for Lie multiplication and this symbol is right associative, so to get
--   the Lie monomial [a,[b,c]] it is enough to write a b c and the output is written
--   [a,b,c] (or a normal form equivalent). The symbol SPACE is
--   also used to multiply by coefficients. Hence, the expression -2[a,[b,c]]+3[b,[c,a]]
--   is written -2 a b c + 3 b c a, but the expression is changed to normal form in 
--   the Lie algebra. The same symbols for the generators may be used in different Lie
--   algebras and they may also be used as variables in polynomial rings. The function
--   "useLie L" is used to change the actual Lie algebra to L (or "use R" to change
--  focus to a polynomial ring R). Also indexed variables may be used as generators, 
--   but then the name of the variables must not have been used before as generators. 
--   There are some names (fr, pr, ko,...) for indexed variables used in the program 
--   for constructing new Lie algebras and in fact these names may also be used by
--   the user to define indexed variables.
--   To define a Lie algebra, a free Lie algebra is first constructed as 
--   L=lieAlgebra{a,b,c,...} with possible options to define weights, signs and 
--   the field. After that a new Lie algebra with differential may be defined 
--   (if the option diffl is set to true)  by giving the value of
--   the differential on the generators. This is done by applying the function
--   diffLieAlgebra on the list of values. A quotient (differential) Lie algebra may be formed as 
--   Q=L/{y1,y2,...} where y1,y2,... are homogeneous LieElements in L. 
--   L.gensLie and L.cache.genslie are both defined as the list of symbols obtained
--   by applying "baseName" to the list given in input,
--   but later L.gensLie is changed to the list of LieElements corresponding to the
--   generators and in pretty printing of these elements, 
--   the symbols in L.cache.genslie are used.

--" 

-------------------------------
--   ambient 
-------------------------------
--   this is the extension of the built-in function
--   ambient to cover the case LieAlgebra. With input
--   L, the output is the free Lie algebra M such 
--   that L=M/I for an ideal I in M. This is slightly
--   different from the use of ambient for rings, in
--   that case, M need not be a polynomial ring.


ambient(LieAlgebra):=L->
    (if L.relsLie==={} then out:=L else out=class (L.relsLie)_0;
     useLie out;
     out);
    

-------------------------------
-- baseName
-------------------------------
--  this is the extension of the built-in function
--   baseName to cover the case LieElement, the output
--   is the symbol name of a generator

baseName(LieElement):=x->(
    L:=class x;
    i:=((x#1)#0)#0;
    (L.cache.genslie)_i);    

----------------------------------------------------
-- CONSTRUCTIONS OF LIE ALGEBRAS, MAPS AND DERIVATIONS
----------------------------------------------------
----------------------------------
-- lieAlgebra
----------------------------------

lieAlgebra = method(TypicalValue => LieAlgebra,Options => 
    {genWeights => 1, genSigns => 0, field => QQ, diffl => false} )
lieAlgebra(List) := opts->(gensLie)->(
    L := new MutableHashTable of LieElement; 
    LL=L;
    L.gensLie=apply(gensLie,baseName);
    L.genWeights=opts.genWeights;
    L.genSigns=opts.genSigns;
    L.diffl=opts.diffl;
    L.field=opts.field;
    L.genDiffs={};
    L#(symbol cache)=new CacheTable;
    L.relsLie={};
    L.numGen=length L.gensLie;
    L.compdeg=0;
    L.cache.maxDeg=5;
    L.cache.lieRing=L.field[];
    L.cache.mbRing=L.field[]; 
    L.cache.genslie=apply(gensLie,baseName);
    L.cache#(symbol dims)=new MutableHashTable;
    L.cache#(symbol opL)=new MutableHashTable;
    L.cache#(symbol bas)=new MutableHashTable;
    L.cache#(symbol gr)=new MutableHashTable;
    L.cache#(symbol cyc)=new MutableHashTable;
    L.cache#(symbol bound)=new MutableHashTable;
    L.cache#(symbol deglist)=new MutableHashTable;
    L.cache.bas#0={mb0}; 
    L.cache.dims#0=1;
    if L.cache.genslie==={} then doZeroLie() else ( 
	if L.genSigns===0 then L.genSigns=flatten table(1,L.numGen,x->0);
      	if L.genSigns===1 then L.genSigns=flatten table(1,L.numGen,x->1);
      	if L.genWeights===1 then L.genWeights=flatten table(1,L.numGen,x->1);   
      	if class (L.genWeights)_0===ZZ and L.diffl then (
            error "there is no homological degree defined";
	    );
      	if class (L.genWeights)_0===List and not L.diffl then (
     	    L.genWeights=apply(L.genWeights,x->append(x,0));
	    );
      	if class (L.genWeights)_0===ZZ and not L.diffl then (
	    L.genWeights=apply(L.genWeights,x->{x,0});
	    );   
      	if min(apply(L.genWeights,x->x_0))<1 then (
	    error "the (first) degree of a generator must be at least one";
	    );
      	if not L.numGen==length L.genWeights  then (
            error "the number of weights must be equal to the number of generators";
	    );
     	if length unique apply(L.genWeights,length)>1 then (
            error "all weights must have the same length";
	    );
     	if not L.numGen==length L.genSigns then (
            error "the number of signs must be equal to the number of generators";
	    );
      	if not all(L.genSigns,x->x===0 or x===1) then (
            error "all signs must be 0 or 1";
	    );
      	if L.diffl and any(L.genWeights,x->x_0<=x_(-1)) then (
            error "the homological (last) degree must be less than the (first) degree";
	    );
      	if L.diffl and any(L.genWeights,x->x_(-1)<0) then (
            error "the homological (last) degree must be non-negative";
	    );
      	L.degLength=length (L.genWeights)_0; 
      	L.cache.lieRing=lieR()
    	);
    M:=new LieAlgebra from L;
    LL=M;
    M.zz=new M from new BasicList from {empty,empty}; 
    M.genDiffs=flatten table(1,M.numGen,x->M.zz);
    M.gensLie=apply(M.numGen,i->new M from (
	    new BasicList from {new BasicList from {1_(M.field)},
		new BasicList from {new BasicList from {i}}}));
    net M:= x->(N:=LL;
	        LL=M; 		
		if x#0===empty then out:=toString 0 else (
		    if (x#0)#0==1 and #(x#1)==1  then out=outmon (x#1)#0 else out=outputrec x;
		);
		useLie N;
	        if substring(out,0,1)=== "+" then substring(out,2) else 
		if substring(out,0,2)=== " +" then substring(out,3) else out
		);
    for i from 0 to M.numGen-1 do (M.cache.genslie)_i<-(M.gensLie)_i;
    M
    );

----------------------------------
-- diffLieAlgebra
---------------------------------
--  the input should be a list of LieElements, which are the differentials of 
--  the generators in a free Lie algebra L, L.zz is used for the zero element. It
--  is checked by the program that the differential preserves all weights except 
--  the homological degree which should be lowered by 1
--  and also it should change the sign. The square of the 
--  differential need not be zero, the differential of the elements in the list are added
--  to the relations (this is enough since d^2 is a derivation).  
--  It is also possible to write L.genDiffs={...}, but then there is no checking done. 


diffLieAlgebra=x->(
    L:=LL;
    if not L.relsLie=={} then (
	error "the current Lie algebra is not free";
	);
     if not all(x,y->class y===L) then (
	 error "the elements of input do not belong to the right Lie algebra";
	 );	  
    if not length x==L.numGen then (
        error "the number of differentials must be equal to the number of generators";
	);
    if not all(x,y->signtest(1 y)) then (
        error "the differentials are not sign-homogeneous";
	);
    if not all(x,y->weighttest(1 y)) then (
        error "the differentials are not weight-homogeneous";
	);
    unit:=append(flatten table(1,L.degLength -1,x->0),1);
    apply(L.numGen,
	i->(if not x_i===L.zz then (
		w:=intweightsecond(x_i#1); 
		s:=isign(x_i);
		if not (L.genWeights)_i-w==unit then (
		    print(x_i); 
		    error " has not the right weight"
		    ); 
		if (L.genSigns)_i==s then (
		    print(x_i); 
		    error " has not the right sign"
		    );
		)
	    )
	);
    Q:=ilieAlgebra(L.gensLie,{},
	genWeights=>L.genWeights,genSigns=>L.genSigns,
	field=>L.field,diffl=>true);
    g:=imapLie(Q,L);
    Q.genDiffs=g x;
    d:=diffLie();
    k:=imapLie(L,Q);
    LL=L;
    rel:=skipzz k d g x;
    if not rel==={} then (
	print("warning: relations have been added for the square of the differential to be zero");
       	Q=ilieAlgebra(L.gensLie,rel,
	    genWeights=>L.genWeights,genSigns=>L.genSigns,
	    field=>L.field,diffl=>true);
        g=imapLie(Q,L);
        Q.genDiffs=g x
	);
    useLie Q;
    Q
    );



-----------------------------------
-- Quotient of Lie algebras
-----------------------------------

LieAlgebra/List:=(L,x)->(
     LL=L;
     if not all(x,y->class y===L) then (
	 error "the generators for the ideal do not belong to the right Lie algebra";
	 );
    if not all(x,signtest) then (
	error "the generators for the ideal are not sign-homogeneous";
	);
    if not all(x,weighttest) then (
	error "the generators for the ideal are not weight-homogeneous";
	);
    M:=ambient L;
    g:=imapLie(M,L);
    LL=L;
    xM:=g skipzz x;
    rel:=join(L.relsLie,xM);
    dL:=diffLie();
    Q:=ilieAlgebra(M.gensLie,rel,
	genWeights=>L.genWeights,genSigns=>L.genSigns,
	field=>L.field,diffl=>L.diffl);
    h:=imapLie(M,Q);
    g=imapLie(Q,L);
    newrel:=h skipzz g dL x;
    newrel=join(rel,newrel);
    if not rel===newrel then (
      print("warning: new generators for the ideal have been added to get invariance of the differential");
      Q=ilieAlgebra(M.gensLie,newrel,
	genWeights=>L.genWeights,genSigns=>L.genSigns,
	field=>L.field,diffl=>L.diffl)
      );
    Q.genDiffs=g L.genDiffs; 
    useLie Q;  
    Q);

-----------------------------------
-- Quotient by the image of a map
-----------------------------------
    
LieAlgebra/MapLie:=(L,f)->L/f(f.sourceLie.gensLie);

--------------------------------
-- free product of Lie algebras
--------------------------------
LieAlgebra*LieAlgebra:=(L1,L2)->(
    if not L1.field===L2.field then (
	error "The Lie algebras must be defined over the same field";
	 );
    if not L1.degLength===L2.degLength then (
	error "The degree length of the Lie algebras must be equal";
	);
    n1:=L1.numGen;
    n2:=L2.numGen;
    M1:=ambient L1;
    M2:=ambient L2;
    M0:=lieAlgebra(apply(n1,x->pr_x)|apply(n2,x->pr_(x+n1)),
	   genWeights=>join(L1.genWeights,L2.genWeights),
	   genSigns=>join(L1.genSigns,L2.genSigns),field=>L1.field,diffl=>true);
    ide:=(imapLie(M0,M1,take(M0.gensLie,n1)))(L1.relsLie)|
         (imapLie(M0,M2,take(M0.gensLie,{n1,n1+n2-1})))(L2.relsLie);
    M0.genDiffs=(imapLie(M0,L1,take(M0.gensLie,n1)))(L1.genDiffs)|
         (imapLie(M0,L2,take(M0.gensLie,{n1,n1+n2-1})))(L2.genDiffs);
    M0/ide
    );

------------------------------
-- direct sum of Lie algebras
------------------------------

LieAlgebra**LieAlgebra:=(L1,L2)->(
    if not L1.field===L2.field then (
	error "The Lie algebras must be defined over the same field";
	);
    if not L1.degLength===L2.degLength then ( 
	error "The degree length of the Lie algebras must be equal";
	);
    n1:=L1.numGen;
    n2:=L2.numGen;
    M1:=ambient L1;
    M2:=ambient L2;
    M0:=lieAlgebra(apply(n1,x->pr_x)|apply(n2,x->pr_(x+n1)),
	   genWeights=>join(L1.genWeights,L2.genWeights),
	   genSigns=>join(L1.genSigns,L2.genSigns),field=>L1.field,diffl=>true);
    pair:=flatten table(n1,n2,(i,j)->(-M0.gensLie)#i (M0.gensLie)#(n1+j));
    ide:=(imapLie(M0,M1,take(M0.gensLie,n1)))(L1.relsLie)|
         (imapLie(M0,M2,take(M0.gensLie,{n1,n1+n2-1})))(L2.relsLie);
    M0.genDiffs=(imapLie(M0,L1,take(M0.gensLie,n1)))(L1.genDiffs)|
         (imapLie(M0,L2,take(M0.gensLie,{n1,n1+n2-1})))(L2.genDiffs);
    ide=ide|pair;   
    M0/ide
    );



-------------------------------------
-- mapLie
-------------------------------------
-- construction of Lie homomorphisms,
-- elements in MapLie


mapLie = method(TypicalValue=>MapLie)
mapLie(LieAlgebra,LieAlgebra,List) := (L,M,y)->(
     N:=LL;
     fF:=new MutableHashTable; 
     F:=ambient M; 
     fF.sourceLie=F; 
     fF.targetLie=L; 
     if not M.field===L.field then (
       	 error "The Lie algebras must be defined over the same field";
	 );
     LL=F; 
     n:=max(0,max(apply(join(F.gensLie,F.genDiffs,M.relsLie),ideglie))); 
     LL=L;
     computeLie n;
     yy:=idef ifed y; 
     if not M.numGen==length y then (
	  error "input does not have the right length";
	  );
     if any(y,z->not class z===L) then (
	  error "input does not belong to the target Lie algebra";
	  );
     apply(length y,i->if not yy_i===L.zz and not intweightsecond(yy_i#1)==(M.genWeights)_i then (
	     error "input does not have the right weights"
	     )
	 );
     apply(length y,i->if not yy_i===L.zz and not isign(yy_i)==(M.genSigns)_i then (
	     error "input does not have the right signs"
	     )
	 );
     apply(M.numGen, i->fF#((F.gensLie)_i)=yy_i); 
     g:=new MapLie from fF; 
     ok:=unique apply(M.relsLie,x->g(x));
     if not (ok==={} or ok==={L.zz}) then print("the map is not welldefined") else (
     	 h:=new MutableHashTable;
     	 h.sourceLie=M;
     	 h.targetLie=L;
     	 apply(M.numGen, i->h#((M.gensLie)_i)=yy_i);
     	 f:=new MapLie from h;
     	 dL:=diffLie();
     	 LL=M;
     	 dM:=diffLie();
     	 LL=L;
     	 if not all(M.gensLie,x->dL(f(x))===f(dM(x))) then (
             print("the map does not commute with the differential")
	     ) else (
     	     useLie N;
     	     f
	     )
	 )
     );
 
-----------------------------
-- mapLie
-----------------------------
-- the following version of mapLie is in analogy
-- with map for rings. The generators in M which are
-- not generators in L are sent to zero, the others
-- are sent to the generator with the same name in L 
     
mapLie(LieAlgebra,LieAlgebra) := (L,M)->(
     N:=LL;
     xx:=apply(M.cache.genslie,x->
	 if not member(x,L.cache.genslie) then L.zz else (
	     Li:=position(L.cache.genslie,y->y===x);
	     Mi:=position(M.cache.genslie,y->y===x);
	     Lx:=(L.gensLie)_Li;		  
	     if (not (M.genWeights)_Mi==(L.genWeights)_Li or
		 not (M.genSigns)_Mi==(L.genSigns)_Li) then L.zz else Lx
	     )
	 );
     useLie N;
     mapLie(L,M,xx)
     );
 
   
-----------------------------
-- imapLie
-----------------------------
--   this constructs a homomorphism without
--   any checking. It is useful for defining the section 
--   L/I->L (in analogy with "map" for rings)


 
imapLie = method(TypicalValue=>MapLie)
imapLie(LieAlgebra,LieAlgebra,List) := (L,M,y)->(
     N:=LL;
     f:=new MutableHashTable;  
     f.sourceLie=M; 
     f.targetLie=L; 
     LL=L; 
     computeLie max(0,max(apply(M.genWeights,x->x_0)));
     yy:=idef ifed y;
     apply(M.numGen, i->f#((M.gensLie)_i)=yy_i); 
     g:=new MapLie from f; 
     useLie N;
     g);
imapLie(LieAlgebra,LieAlgebra) := (L,M)->(
     N:=LL;
     xx:=apply(M.cache.genslie,x->
	 if not member(x,L.cache.genslie) then L.zz else (
	     Li:=position(L.cache.genslie,y->y===x);
       	     Mi:=position(M.cache.genslie,y->y===x);
       	     Lx:=(L.gensLie)_Li;		  
	     if (not (M.genWeights)_Mi==(L.genWeights)_Li or
		 not (M.genSigns)_Mi==(L.genSigns)_Li) then L.zz else Lx
	     ) 
         );
     useLie N;
     imapLie(L,M,xx)
     );
 

----------------------------------
-- derLie
----------------------------------
-- construction of a derivation M->L, where
-- L is an M-module via a map f:M->L
 
derLie = method(TypicalValue=>DerLie)
derLie(List) := y->derLie(idMapLie(),y);
derLie(MapLie,List) := (f,y)->(
    N:=LL;
    L:=f.targetLie; 
    M:=f.sourceLie;
    dF:=new MutableHashTable;  
    F:=dF.sourceLie=ambient M; 
    dF.targetLie=L; 
    dF.maplie=f*imapLie(M,F);  
    if not M.numGen==length y then (
	error "input has not the right length";
	);
    if not all(y,z->class z===L) then (
	error "input does not belong to the right Lie algebra";
	);
    LL=L;
    computeLie(max(0,max(flatten apply(y,z->apply(monomialsLie z,ideglie))))); 
    yy:=idef ifed y;
    if all(yy,z->z===L.zz) then ( 
	dF.weight=flatten table(1,M.degLength,x->0);
	dF.sign=0
	) else (
	i:=position(yy,z->not z===L.zz);
	LL=L;
	dF.weight=intweight(yy_i#1#0)-(M.genWeights)_i;
	dF.sign=(isign(yy_i)-(M.genSigns)_i)%2
	);
    LL=L;
    apply(length y,i->(
	    if not yy_i===L.zz and not dF.weight==intweightsecond(yy_i#1)-(M.genWeights)_i then (
	    	error "input does not have the right weight";
		);
            if not yy_i===L.zz and not dF.sign==(isign(yy_i)-(M.genSigns)_i)%2 then (
		 error "input does not have the right sign";
		 );
	     )
	 );
    apply(M.numGen,i->dF#((F.gensLie)_i)=yy_i); 
    e:=new DerLie from dF;
    useLie M;
    n:=relDegMax()+(e.weight)_0;
    useLie L;
    computeLie n;    
    ok:=unique apply(M.relsLie,x->e(x));
    if not (ok==={} or ok==={L.zz}) then print("the derivation is not welldefined") else (
	h:=new MutableHashTable;
    	h.sourceLie=M;
    	h.targetLie=L;
    	h.maplie=f;
    	h.weight=dF.weight;
    	h.sign=dF.sign;
    	apply(M.numGen, i->h#((M.gensLie)_i)=yy_i);
    	d:=new DerLie from h;   
    	useLie N;
    	d
	)
    );

------------------------------------
-- iderLie
------------------------------------
-- the internal version of derLie without
-- any checking

iderlie = method(TypicalValue=>DerLie)
iderlie(List) := y->iderlie(idMapLie(),y);
iderlie(MapLie,List) := (f,y)->(
    N:=LL;
    L:=f.targetLie; 
    M:=f.sourceLie;
    d:=new MutableHashTable;  
    d.sourceLie=M; 
    d.targetLie=L; 
    d.maplie=f; 
    LL=L;
    computeLie(max(0,max(flatten apply(y,z->apply(monomialsLie z,ideglie))))); 
    yy:=idef ifed y;
    if all(yy,z->z===L.zz) then ( 
	d.weight=flatten table(1,L.degLength,x->0);
	d.sign=0
	) else (
	i:=position(yy,z->not z===L.zz);
	LL=L;
	d.weight=intweight(yy_i#1#0)-(M.genWeights)_i;
	d.sign=(isign(yy_i)-(M.genSigns)_i)%2
	);
    apply(M.numGen,i->d#((M.gensLie)_i)=yy_i); 
    e:=new DerLie from d; 
    useLie N;
    e
    );


 ------------------------------------------------
 -- OPERATIONS ON LIE ALGEBRAS, MAPS AND DERIVATIONS
 ------------------------------------------------

-----------------------------------
-- Lie multiplication and 
-- linear operations on LieElement
-----------------------------------
-- the following operations  ++ * / @  produce 
-- LieElement objects which are non-normalized

LieElement++LieElement:=(x,y)->(
    L:=class x;
    new L from new BasicList from {join(x#0,y#0),join(x#1,y#1)});
Number@LieElement:=(x,y)->(
    L:=class y;
    new L from new BasicList from {apply(y#0,z->x*z),y#1});
RingElement@LieElement:=(x,y)->(
    L:=class y;
    new L from new BasicList from {apply(y#0,z->x*z),y#1});
LieElement/LieElement:=(x,y)->x++(-1)@y;
LieElement@LieElement:=(x,y)->(
    LL=class y;
    if x#0===empty then LL.zz else
       summplus(x#0,x#1,(z,u)->z@monmult(u,y)))
monmult=method(TypicalValue=>LieElement)
monmult(ZZ,LieElement):=(x,y)->
    new LL from new BasicList from {y#0,apply(y#1,z->prepend(x,z))};
monmult(BasicList,LieElement):=(x,y)->(    
    x1:=last x;
    x0:=drop(x,-1);
    if #x == 1 then monmult(x1,y) else 
       monmult(x0,monmult(x1,y))/(eps(x0,x1)@monmult(x1,monmult(x0,y)))
    );

 
LieElement LieElement:=(x,y)->multLie(x,y);
LieElement+LieElement:=(x,y)->(
    N:=LL;
    LL=class y;
    computeLie ideglie y; 
    out:=idef(ifed x+ifed y);
    useLie N;
    out
    );
LieElement-LieElement:=(x,y)->x+(-1) y;
-LieElement:=x->(-1) x;
Number LieElement:=(x,y)->(
    N:=LL;
    LL=class y;
    computeLie ideglie y;
    out:=idef(x*(ifed y));
    useLie N;
    out
    );
RingElement LieElement:=(x,y)->(
    N:=LL;
    LL=class y;
    computeLie ideglie y;
    out:=idef(x*(ifed y));
    useLie N;
    out
    );
List List:=(x,y)->flatten apply(x,z->apply(y,u->z u));
               
-------------------------------
-- linear operations on MapLie
-------------------------------

MapLie+MapLie:=(f,g)->imapLie(
    f.targetLie,f.sourceLie,apply(f.sourceLie.gensLie,x->f(x)+g(x))
    );
Number MapLie:=(x,f)->imapLie(
    f.targetLie,f.sourceLie,apply(f.sourceLie.gensLie,y->x f(y))
    );
RingElement MapLie:=(x,f)->imapLie(
    f.targetLie,f.sourceLie,apply(f.sourceLie.gensLie,y->x f(y))
    );
-MapLie:=y->(-1) y;
MapLie-MapLie:=(x,y)->x+(-1) y;
-------------------------------
-- * is used for composition
-------------------------------
MapLie*MapLie:=(f,g)->imapLie(
    f.targetLie,g.sourceLie,apply(g.sourceLie.gensLie,x->f(g(x)))
    );

-------------------------------
-- Lie multiplication of ordinary derivations on L
-------------------------------
DerLie DerLie:=(d,e)->(
    if not (d.sourceLie===d.targetLie and d.targetLie===e.sourceLie and 
	e.sourceLie===e.targetLie) then (
    	error "the derivations may not be multiplied";
	);
    if not ((d.maplie)(d.sourceLie.gensLie)===d.sourceLie.gensLie and
	(e.maplie)(e.sourceLie.gensLie)===e.sourceLie.gensLie) then (
    	error "the maps defining the derivations must be the identity";
	);
    iderlie(apply(d.sourceLie.gensLie,x->d(e(x))-(-1)^(d.sign*e.sign) e(d(x))))
    );


-------------------------------
-- linear operations on DerLie
-------------------------------

DerLie+DerLie:=(d,e)->(
    if not (d.sourceLie===e.sourceLie and d.targetLie===e.targetLie) then (
       	error "the derivations may not be added";
       	);
    if not ((d.maplie)(d.sourceLie.gensLie)===(e.maplie)(e.sourceLie.gensLie)) then (
       	error "the maps defining the derivations must be equal";
       	);
    iderlie(d.maplie,apply(d.sourceLie.gensLie,x->d(x)+e(x)))
    );
DerLie-DerLie:=(x,y)->x+(-1) y;
-DerLie:=y->(-1) y;
Number DerLie:=(c,d)->iderlie(d.maplie,apply(d.sourceLie.gensLie,x->c d(x)));
RingElement DerLie:=(c,d)->iderlie(d.maplie,apply(d.sourceLie.gensLie,x->c d(x)));

--------------------------------
-- operation of MapLie on DerLie
--------------------------------
DerLie*MapLie:=(d,g)->iderlie(d.maplie*g,apply(g.sourceLie.gensLie,x->d(g(x))));
MapLie*DerLie:=(g,d)->iderlie(g*d.maplie,apply(d.sourceLie.gensLie,x->g(d(x))));

--------------------------------
-- the associative multiplication 
-- on the Ext-algebra 
-- (basis elements: ext_i)
--------------------------------
RingElement RingElement:=(a,b)->
    linext((x,y)->extmonmult(x,y),a,b);




----------------------------
-- application of a homomorphism 
-- to a LieElement 
----------------------------
MapLie(LieElement):=(f,x)->(
     N:=LL;
     M:=class x;
     if not f.sourceLie===M then error "the map is not defined on input";
     LL=M;
     n:=max(genDegMax(),ideglie x);
     LL=f.targetLie;
     computeLie n;
     y:=apply(M.numGen,j->ifed f#((M.gensLie)_j));
     out:=if x#0===empty then LL.zz else idef summ(x#0,x#1,(i,j)->i*imultindex(y,j));
     useLie N;
     out
     );
 
------------------------------
-- application of a homomorphism 
-- to a list
------------------------------
MapLie(List) := (f,y)->apply(y,x->f x);


----------------------------
-- application of a derivation 
-- to a LieElement 
----------------------------
DerLie(LieElement) := (d,x)->(
    N:=LL;
    M:=d.sourceLie;
    if not class x===M then error "the derivation is not defined on input";
    LL=M;
    n:=max(genDegMax(),ideglie x+(d.weight)_0);
    LL=d.targetLie;
    computeLie n;
    f:=d.maplie;
    yd:=apply(M.numGen,j->ifed d#((M.gensLie)_j));
    yf:= ifed f M.gensLie;
    g:=z->if #z==1 then yd_(z#0) else (
    	imult(yd_(z#0),imultindex(yf,drop(z,1)))+
    	(-1)^((LL=d.sourceLie;isign(z#0))*(d.sign))*
	imult((LL=d.targetLie;yf_(z#0)),g drop(z,1))
    );
    out:=if x#0===empty then LL.zz else idef summ(x#0,x#1,(i,j)->i*g j); 
    useLie N;
    out
    );

----------------------------
-- application of a derivation 
-- to a list 
----------------------------

DerLie(List) := (d,y)->apply(y,x->d x); 


----------------------------------------
--
-- EXPORTED FUNCTIONS
--
----------------------------------------



----------------------------------------
-- annLie
----------------------------------------
-- a basis for the set of elements of degree d
-- which multiply the set p of elements of degree s
-- to zero

annLie=method(TypicalValue=>List)
annLie(ZZ,ZZ,List) := (d,s,p)->(
    t:=d+s;
    computeLie(t);       
    if p==={} or unique p==={LL.zz} or LL.cache.dims#t==0 then basisLie(d) else (
       	if not all(p,y->class y===LL) or not unique degLie p == {s} then (
	    error "the input is not correct"
	    ) else idef idivisor(t,s,{0_(LL.cache.lieRing)}, apply(p,x->ifed x))
      	)	   
    );  
	



 

 
----------------------------------------
--basisLie   
----------------------------------------
-- a basis for the Lie algebra in degree n
-- or degree n and homological degree d 
   
basisLie=method(TypicalValue=>List)
basisLie(ZZ):=(n)->(
    computeLie n;
    idef ibasis n
    );
basisLie(ZZ,ZZ):=(n,d)->(
    computeLie n;
    select(basisLie(n),x->(intweight(x#1#0))_(-1)==d)
    );
basisLie(List):=(x)->(
    computeLie x_0;
    select(basisLie(x_0),y->intweight(y#1#0)==x)
    ); 

----------------------------------------
--boundariesBasisLie
----------------------------------------
-- a basis for the boundaries in degree n
-- and homological degree d or multi-degree x

boundariesBasisLie = method(TypicalValue=>List)
boundariesBasisLie(ZZ,ZZ) := (n,d)->
    idef iboundaries(n,d);
boundariesBasisLie(List):=(x)->
    select(boundariesBasisLie(first x,last x),y->intweight(y#1#0)==x);
    
     
    
----------------------------------------
--boundariesTableLie
----------------------------------------
boundariesTableLie = n->matrix apply(n,d->apply(n,j->
	length iboundaries(j+1,d)))

----------------------------------------
--centerLie
----------------------------------------
-- gives a basis for the central elements in degree d

centerLie = method()
centerLie(ZZ):=(d)->(
	intersectionLie(d,for i from 1 to genDegMax() list 
    	    	annLie(d,i,select(LL.gensLie,x->degLie x==i))));

-- gives a basis for the central elements up to degree n 

centerAllLie = n->(
    computeLie(n+genDegMax());
    flatten(for i from 1 to n list centerLie(i)));
    



     

----------------------------------------
--characterLie
----------------------------------------
-- x is a set of cycles, y is a basis for a subspace
-- of the Lie algebra which is invariant under x
-- output is the trace of x as a map y->y

characterLie=method(TypicalValue=>RingElement) 
characterLie(List,List):=(x,y)->(
  if not class symmetryLie x===MapLie then (
    error "the first input does not define an automorphism"
    ) else (
      if y==={} then 0_(LL.field) else (
    	d:=degLie y_0;
    	if not all(y,z->class z===LL and degLie z==d) then (
    	    error "the second input is not correct" 
	    ) else (
    	        yy:=subalgBasisLie(d,y);
	    	if not all(flatten x,z->member(z,LL.gensLie)) or not all(x,z->z===unique z) then (		
	    	    error "the first input is not a product of cycles"
	    	    ) else (
		    m1:=basToMat(d,apply(y,z->ifed z));
	            m2:=basToMat(d,apply(y,z->ifed ipermop(x,z)));
	            if rank(m1|m2)>rank m1 then 
	                 error "the second input is not an invariant subspace" else 
	            trace solve(m1,m2)
		    )
	    	)
    	    )
    	)
    );



--------------------------------------
coeffsLie
--------------------------------------
-- this gives the coeffients of a LieElement, which is a 
-- linear combination of basis elements as iterated Lie products

coeffsLie=method(TypicalValue=>List)
coeffsLie(LieElement):=x->toList x#0;




---------------------------------------
-- cyclesBasisLie
---------------------------------------
-- gives a basis for the cycles in first degree n
-- and homological degree d or multi-degree x

cyclesBasisLie = method()
cyclesBasisLie(ZZ,ZZ) := (n,d)->idef cycles(n,d);
cyclesBasisLie(List):=(x)->select(cyclesBasisLie(first x,last x),y->intweight(y#1#0)==x);


----------------------------------------
--cyclesTableLie
----------------------------------------

cyclesTableLie = n->
     dimTableLie n - matrix drop(entries(
	     matrix table(1,n,x->0)||id_(ZZ^n)),-1)*boundariesTableLie n;

----------------------------------------
-- decompidealLie
----------------------------------------
--   for a holonomy Lie algebra L, this is 
--   the kernel of the surjective Lie homomorphism from L'=[L,L]
--   to the direct sum of L_i', where L_i are the local Lie
--   algebras (obtained by localLie(i)). 
--   The ideal is generated by the elements in degree 3,
--   which are linear combinations of [x,y,z], where not all x,y,z
--   belong to the same L_i.


decompidealLie = method(TypicalValue=>List)
decompidealLie(ZZ) := (d)->(
    computeLie d;
    loc:=join (apply(toSequence LL.cache.localone,y->isubalg(3,apply(y,z->genbas(z))))|
              apply(toSequence LL.cache.localtwo,y->isubalg(3,apply(y,z->genbas(z)))));
    genide:=select(ibasis(3),y->not member(y,loc));
    idef iideal(d,genide)
    );


----------------------------------------
-- defLie
----------------------------------------
-- the LieElement corresponding to an element in mbRing

defLie = method()
defLie(List) := (p)->apply(p,x->defLie(x));
defLie(RingElement):=(p)->idef mbToLie p;


----------------------------------------
-- degLie
----------------------------------------
-- the first degree of a homogeneous LieElement or of
-- a generator as a number 0,...,n

degLie = method()
degLie (ZZ):=(g)->((LL.genWeights)_g)_0;	   
degLie(LieElement):=(x)->(weightLie(x))_0;
degLie(DerLie):=d->(d.weight)_0;  
degLie(List):=x->apply(x,degLie);

------------------------------------
-- diffLie
------------------------------------
-- This is the extension of L.genDiffs to a derivation on L 

diffLie=()->derLie(LL.genDiffs);




 ----------------------------------------
--dimsLie   
----------------------------------------
-- gives the list of dimensions of the Lie algebra when the first
-- degree ranges from 1 to d

dimsLie=method(TypicalValue=>List)
dimsLie(ZZ):=d->computeLie d;

----------------------------------------
--dimTableLie   
----------------------------------------
-- the matrix of dimensions in first and last degree

dimTableLie=method()
dimTableLie(ZZ):=(n)->(
    computeLie n;
    dit:=new MutableHashTable; 
    apply(n,j->dit#j=LL.cache.bas#(j+1));
    matrix apply(n,d->apply(n,j->length select(dit#(j),x->(degree x)_(-1)==d)))
    );

-- s=0 gives only dimensions of even elements, s=1 only odd
dimTableLie(ZZ,ZZ):=(n,s)->(
    computeLie n;
    matrix apply(n,d->apply(n,j->length select(ibasis(j+1,d),y->isign y==s)))
    );


----------------------------------------
--dimtotLie   
----------------------------------------
-- the sum of the dimensions up to degree d
	    
dimtotLie=method(TypicalValue=>ZZ)
dimtotLie(ZZ):=(d)->sum dimsLie(d);


----------------------------------------
--divisorLie
----------------------------------------
-- a basis for the subspace of elements of degree t-s which multiply all 
-- elements in the set p of degree s to the space generated
-- by the elements of degree t in m.
 
divisorLie=method(TypicalValue=>List)
divisorLie(ZZ,ZZ,List,List) := (t,s,m,p)->(
    computeLie t; 
    d:=t-s; 
    if m=={} or unique m==={LL.zz} then annLie(t-s,s,p) else
      if not all(m,y->class y===LL) or not unique degLie m == {t} then 
	      error "the third input is not correct" else
	if p==={} or unique p==={LL.zz} then basisLie(d) else 
          if not all(p,y->class y===LL) or not unique degLie p == {s} then 
	      error "the fourth input is not correct" else
            if LL.cache.dims#t==0 then basisLie(d) else 
	        (        
		mm:= apply(m,x->ifed x); 
		pp:=apply(p,x->ifed x);
		idef idivisor(t,s,mm,pp)
		)
	 );
	

----------------------------------------
--eulerLie
----------------------------------------
-- computes the list of eulercharacteristics of 
-- first degree 1 to n. It is
-- assumed that the homological degree is less
-- than the first degree.

eulerLie=method(TypicalValue=>List)
eulerLie(ZZ):=(n)->(
    computeLie n;
    for i from 1 to n list ieuler(i)
    );

----------------------------------------
--extBasisLie   
----------------------------------------
-- a basis for the Ext-algebra up to degree n
-- or degree n and homological degree d or multidegree x

extBasisLie=method(TypicalValue=>List)
extBasisLie(ZZ):=(n)->(
    minmodelLie n;
    gens LL.cache.extRepRing
    );
extBasisLie(ZZ,ZZ):=(n,d)->(
    minmodelLie n;
    select(extBasisLie(n),x->(degree x)_0==n and last degree x==d)
    );
extBasisLie(List):=(x)->(
    computeLie x_0;
    select(extBasisLie(x_0),y->degree y==x)
    ); 


----------------------------------------
--extTableLie
----------------------------------------
-- gives the matrix of dimensions of the Ext-algebra
-- up to degree n

extTableLie = method(TypicalValue=>Matrix)
extTableLie(ZZ) := (n)->(
    M:=minmodelLie(n);
    matrix toList apply(
	1..n,d->toList apply(
	    1..n,j->length select(M.numGen,y->
	      ((M.genWeights_y)_0===j and (M.genWeights_y)_(-1)===d-1))))
    );



----------------------------------------
-- extMultLie
----------------------------------------
-- this is multiplication as prefix notation in the graded
-- commutative associative Ext-algebra. It is also
-- possible to use SPACE as infix notation

extMultLie = method()
extMultLie(RingElement,RingElement) := (x,y)-> x y;
	

----------------------------------------
--holonomyLie
----------------------------------------
-- the holonomy Lie algebra of an arrangement or matroid,
-- the first list is the list of 1-flats of size at least two,
-- the second list is the set of 2-flats of size at least three
	
holonomyLie=method(TypicalValue=>LieAlgebra,Options=>{field=>QQ})
holonomyLie(List,List):= opts->(x,y)->(
    xx:=apply(x,z->apply(z,baseName));
    yy:=apply(y,z->apply(z,baseName));   
    uu:=xx|yy;
    xxx:=flatten xx;
    Generators:=unique (xxx|flatten yy);
    n:=length Generators; 
    allpairs:=flatten apply(uu,y->subsets(y,2));
    if not (unique allpairs===allpairs) then (
	error "two sets in the second input must have at most one element in common";
	); 
    if not (unique xxx===xxx) then (
	error "the sets in the first input must be disjoint";
	);
    M:=ilieAlgebra(Generators,{},
	    genWeights=>flatten table(1,n,x->{1,0}),
	    genSigns=>flatten table(1,n,x->0), field=>opts.field);
    trans:=y->(i:=position(Generators,z->z===y);(M.gensLie)_i);
    liepairs:=apply(select(subsets(M.cache.genslie,2),
		y->all(uu,x->not isSubset(y,x))),z->
	           (trans (z_1)) (trans (z_0)));
    lieideallocal:=y->apply(drop(y,1),
		z->summ(y,u-> (trans z) (trans u)));
    L:=ilieAlgebra(M.gensLie,flatten apply(yy,lieideallocal)|liepairs,
	        genWeights=>flatten table(1,n,x->{1,0}),
	        genSigns=>flatten table(1,n,x->0), field=>opts.field);
    trans=y->(i:=position(Generators,z->z===y);(L.gensLie)_i);
    L.cache.localone=apply(xx,z->apply(z,u->trans u));
    L.cache.localtwo=apply(yy,z->apply(z,u->trans u));
    L
    );

holonomyLie(List):=opts->x->holonomyLie({},x,field=>opts.field);

	  

----------------------------------------
--homologyBasisLie
----------------------------------------
-- a basis for the homology in degree n and
-- homological degree d

homologyBasisLie=method(TypicalValue=>List)
homologyBasisLie(ZZ,ZZ):=(n,d)->idef ihomology(n,d);
homologyBasisLie(List):=(x)->select(homologyBasisLie(first x,last x),y->intweight(y#1#0)==x);



----------------------------------------
--homologyTableLie
----------------------------------------
-- the table of dimensions of the homology 
-- up to degree n

homologyTableLie = method()
homologyTableLie(ZZ) := (n)->cyclesTableLie n - boundariesTableLie n;
       
       
----------------------------------------
--idealBasisLie   
----------------------------------------
-- gives a basis in first degree n (and homological degree d)
-- or multi-degree y for the differential Lie ideal which is 
-- generated by the elements in the list x of LieElements
-- (which might contain elements of different degrees).

idealBasisLie=method(TypicalValue=>List)
idealBasisLie(ZZ,List):=(n,x)->(
    computeLie n;
    N:=LL;
    M:=N/x;
    useLie N;
    d:=diffLie();
    xx:=skipzz join(x,d x);
    idef iideal(n,apply(xx,ifed))
    );
idealBasisLie(ZZ,ZZ,List):=(n,d,x)->select(idealBasisLie(n,x),z->(intweight(z#1#0))_(-1)==d);	
idealBasisLie(List,List):=(y,x)->select(idealBasisLie(y_0,x),z->intweight(z#1#0)==y);
   


----------------------------------------
--idealTableLie   
----------------------------------------
-- this gives a table of dimensions for the differential Lie ideal
-- generated by the input, in first degree 1 to n
-- and homological degrees 0 to n-1.
-- With input (n,s,x), s is either 0 or 1,
-- giving dimensions of even or odd elements. 

idealTableLie=method()
idealTableLie(ZZ,List):=(n,x)->(
    N:=LL;
    y1:=dimTableLie n;
    LL=LL/x;
    y0:=dimTableLie n;
    out:=y1-y0;
    useLie N;
    out
    );
idealTableLie(ZZ,ZZ,List):=(n,s,x)->(
    N:=LL;
    y1:=dimTableLie(n,s);
    LL=LL/x;
    y0:=dimTableLie(n,s);
    out:=y1-y0;
    useLie N;
    out
    );

----------------------------------------    
-- idMapLie
----------------------------------------
-- the identity map on L

idMapLie=()->imapLie(LL,LL);

 

----------------------------------------
--imageBasisLie
---------------------------------------- 
-- a basis in degree n for the image of a map f   

imageBasisLie=method()	    
imageBasisLie(ZZ,MapLie) := (n,f)->(
    N:=LL;
    LL=f.sourceLie;arg:=basisLie(n);
    LL=f.targetLie;
    out:=subalgBasisLie(n,f arg);
    useLie N;
    out
    );
-- a basis in degree n for the image of a derivation f 
imageBasisLie(ZZ,DerLie) := (n,f)->(
    N:=LL;
    LL=f.sourceLie;arg:=basisLie(n-(f.weight)_0);
    LL=f.targetLie;
    out:=subalgBasisLie(n,f arg);
    useLie N;
    out
    );
-- a basis in degree n and homological degree d or multi-degree x
-- for the image of a map or derivation f 
imageBasisLie(ZZ,ZZ,MapLie) :=imageBasisLie(ZZ,ZZ,DerLie) := (n,d,f)->
    select(imageBasisLie(n,f),z->(intweight(z#1#0))_(-1)==d);
imageBasisLie(List,MapLie) :=imageBasisLie(List,DerLie) := (x,f)->
    select(imageBasisLie(x_0,f),z->intweight(z#1#0)==x);

----------------------------------------
--imageTableLie
----------------------------------------
-- the dimensions of the image of a map or derivation f in degrees
-- up to n and also homological degrees

imageTableLie = (n,f)->(
    imb:=new MutableHashTable; 
    apply(n,j->imb#j=imageBasisLie(j+1,f));
    matrix apply(n,d->apply(n,j->length select(imb#j,x->(intweight(x#1#0))_(-1)==d)))
    );




----------------------------------------
--indexFormLie   
----------------------------------------
-- gives the representation of a LieElement in mbRing, 
-- it is left inverse to defLie, that is indexFormLie(defLie(x))=x. 
-- It holds that
-- defLie(indexFormLie(x))≈x in the free Lie algebra on 
-- the generators modulo the relations.
-- indexFormLie är listable


indexFormLie = method()
indexFormLie(LieElement) := (x)->(
    N:=LL;
    LL=class x;
    computeLie degLie x;
    if x#1===empty then out:=0_(LL.cache.mbRing) else (
    	out=summ(x#0,x#1,(i,j)->i*(lieToMb ifed j));
    );
    useLie N;
    out
    );
indexFormLie(List):=x->apply(x,indexFormLie);


----------------------------------------
-- innerDerLie
----------------------------------------
-- this gives for x the inner derivation y->[x,y]

innerDerLie=method(TypicalValue=>DerLie)
innerDerLie(LieElement) := x->iderlie(apply(LL.gensLie,y->x y));



----------------------------------------
--intersectionLie
----------------------------------------
-- if m is {}, the output will be the whole space basisLie(d)

intersectionLie = method(TypicalValue=>List)
intersectionLie(ZZ,List) := (d,m)->(
    if m=={} then basisLie d else (
    	if any(m,x->x=={}) then {} else (
    	    if not all(flatten m,y->class y===LL) or 
	       not (skipz unique degLie flatten m == {d} or
		   skipz unique degLie flatten m == {}) then 
    		error "all input elements are not of degree d or 
		       do not belong to the current Lie algebra" else (
    		computeLie d;  
		if LL.cache.dims#d==0 then {} else
                idef matToBas(d,
                 generators kernel transpose joinhoriz apply(
	         apply(m,y->apply(y,z->ifed z)),
	         x->(generators kernel transpose basToMat(d,x))))
    		)
	    )
	)
    );


----------------------------------------
--invImageBasisLie
---------------------------------------- 
-- a basis for the inverse image under a map f 
-- of a space generated by b

invImageBasisLie = method()  
invImageBasisLie(MapLie,List) := (f,b)->(
	N:=LL; 
	LL=f.targetLie; 
	if any(b,x->not class x===LL) then (
 	    error "the elements in the list do not belong to the target of the map";
		   );
	de:= skipZZ unique apply(b,x->if (weightLie x)_0===0 then 0 else intweight(x#1#0));
	if de=={} then (
	    error "the input generates the zero space, use kernelBasisLie instead";
		   );
	if length de>1 then (
	    error "the input is not multi-homogeneous";
	    );
	n:=(de_0)_0;
	computeLie n; 
        B:=basToMat(n,ifed b);
        LL=f.sourceLie;  
        ba:=basisLie(n);
	if ba==={} then (LL=N; {}) else ( 
        LL=f.targetLie;
        A:=basToMat(n,ifed f ba);
        C:=invimage(A,B); 
        LL=f.sourceLie; 
	out:=idef select(matToBas(n,C),x->last degree x==last (de_0)); 
	LL=N; 
	out)
        ); 
    
-- a basis for the inverse image under a derivation f 
-- of a space generated by b    

invImageBasisLie(DerLie,List) := (f,b)->( 
        N:=LL; 
        LL=f.targetLie;
            if any(b,x->not class x===LL) then
                error "the elements in the list do not belong to 
	               the target of the derivation"; 
	de:= skipZZ unique apply(b,x->if (weightLie x)_0===0 then 0 else intweight(x#1#0));
	    if de=={} then 
	        error "the input generates the zero space, 
	           use kernelBasisLie instead";
	    if length de>1 then 
	        error  "the input is not multi-homogeneous";
	n:=(de_0)_0;
        computeLie n;
        B:=basToMat(n,apply(b,ifed));
        LL=f.sourceLie;   
        ba:=basisLie(n-(f.weight)_0); 
	if ba==={} then (LL=N; {}) else ( 
        LL=f.targetLie;
        A:=basToMat(n,ifed f ba);
        C:=invimage(A,B); 
        LL=f.sourceLie; 
        out:=idef select(matToBas(n-(f.weight)_0,C),x->last(degree x+f.weight)==last (de_0)); 
        LL=N; 
        out)
        );
 

----------------------------------------
--invImageLie
---------------------------------------- 
-- the dimension of the inverse image under a map or
-- derivation f of the space generated by b

invImageLie = method()
invImageLie(MapLie,List) := invImageLie(DerLie,List) := (f,b)-> 
        length invImageBasisLie(f,b);

 


----------------------------------------
--kernelBasisLie
----------------------------------------
-- a basis in degree n for the kernel of a map f

kernelBasisLie = method(TypicalValue=>List)	    	
kernelBasisLie(ZZ, MapLie) := (n,f)->(
    N:=LL;
    LL=f.sourceLie;
    ba:=basisLie(n);
    if ba=={} then (useLie N;{}) else (
	LL=f.targetLie;
	computeLie n;
	if LL.cache.dims#n==0 then (useLie N;ba) else (
	    fval:=ifed f ba;
	    fmat:=basToMat(n,fval); 
	    LL=f.sourceLie;
	    out:=idef matToBas(n,(generators kernel fmat));
	    useLie N;
	    out
	    )
	)
    );


-- a basis in degree n for the kernel of a derivation f
kernelBasisLie(ZZ, DerLie) := (n,f)->(
    N:=LL;
    LL=f.sourceLie;
    ba:=basisLie(n);
    if ba=={} then (useLie N;{}) else (
	LL=f.targetLie;
	nf:=n+(f.weight)_0;
	computeLie(nf);
	if LL.cache.dims#nf==0 then (useLie N;ba) else (
 	    fval:=ifed f ba;
      	    fmat:=basToMat(nf,fval); 
	    LL=f.sourceLie;
            out:=idef matToBas(n,(generators kernel fmat));
	    useLie N;
	    out
	    )
      	)
    );


-- a basis in degree n and homological degree d or multi-degree x
-- for the kernel of a map or derivation f
kernelBasisLie(ZZ,ZZ,MapLie) := kernelBasisLie(ZZ,ZZ,DerLie) := (n,d,f)->
      select(kernelBasisLie(n,f),z->(intweight(z#1#0))_(-1)==d);
kernelBasisLie(List,MapLie) := kernelBasisLie(List,DerLie) := (x,f)->
      select(kernelBasisLie(x_0,f),z->intweight(z#1#0)==x);


----------------------------------------
--kernelTableLie
----------------------------------------    
-- gives the table of dimensions of the kernel in first degree 1 to n
-- and homological degree 0 to n-1
       
kernelTableLie = (n,f)->(keb:=new MutableHashTable; 
    apply(n,j->keb#j=kernelBasisLie(j+1,f));
    matrix apply(n,d->apply(n,j->length select(keb#j,x->(intweight(x#1#0))_(-1)==d)))
    );

	
----------------------------------------	
--koszulDualLie
----------------------------------------
-- the Lie algebra whose envelopping algebra is the Koszul dual
-- of the input which is a quotient of a polynomial algebra
-- modulo a quadratic ideal, some of the generators may be
-- skewcommutative 

koszulDualLie=method(TypicalValue=>LieAlgebra)
koszulDualLie(QuotientRing):=koszulDualLie(PolynomialRing):=(Q)->(
    if Q#?SkewCommutative then S:=Q.SkewCommutative else S={};
    skewco:=apply(S,i->(gens Q)_i);
    signlist:=apply(length gens Q,x->if member(x,S) then 1 else 0);
    fie:=coefficientRing Q;
    R:=fie[gens Q,SkewCommutative=>skewco,Degrees=>apply(gens Q,degree)];    
    I:=flatten entries presentation Q;
    R1:=if I=={} then Q else ring I_0;
    I=apply(I,x->(map(R,R1))(x));
    scalarprod:=(l1,l2)->sum(l1,l2,(i,j)->i*j);
    explist:=(p)->apply(flatten entries monomials p,m->flatten exponents m);
    homogen:=(p)->length unique apply(explist p,y->scalarprod(signlist,y)%2)==1;
    apply(I,p->if not homogen p then (
	    print(p); 
	    error "is not sign-homogeneous"
	    )
	);
    if not isHomogeneous Q then (
	print(I); 
	error "is not multi-homogeneous"
	);
    if I=={} then grI:={} else (
        I=ideal I;    
	grI=flatten entries gens gb(I,DegreeLimit=>flatten table(1,degreeLength R,x->2));
	grI=apply(grI,x->x*(1/(leadCoefficient x)))
	);
    lM:=apply(grI,leadMonomial);
    bM:=select(flatten entries basis(2,R),x->not member(x,lM));
    -- grI_k = lM_k + \sum a_ki bM_i
    M:=ilieAlgebra(apply(numgens R,i->ko_i),{},
	genWeights=>apply(apply(gens R,degree),x->append(x,0)),
	genSigns=>apply(signlist,x->(x+1)%2),field=>fie);
    tolie:=(m)->(
	po:=positions(flatten exponents m,x->x>0); 
       	if length po==1 then (M.gensLie)_(po_0) (M.gensLie)_(po_0) else 
     	   if not member(po_0,S) and member(po_1,S) then 
	   (M.gensLie)_(po_1) (M.gensLie)_(po_0) else
       	   (M.gensLie)_(po_0) (M.gensLie)_(po_1)
	   );
    sqco:=(m)->(
	po:=positions(flatten exponents m,x->x>0); 
       	if length po==1 then (1/2)_fie else 1_fie
	);
    coe:=(k,i)->sqco(lM_k)*(map(fie,R))(((apply(coefficients(
	    grI_k,Monomials=>{bM_i}),x->flatten entries x))_1)_0);
-- 1/2a_ki alt a_ki depending on lM_k being a square or not
    co:=(i)->prepend(-sqco(bM_i),apply(length grI,k->coe(k,i)));  
-- {-1 alt -1/2,a_0i,...,a_(r-1)i}
    relmon:=(i)->prepend(tolie(bM_i),apply(lM,tolie));
    rels:=apply(length bM,i->sum(co(i),relmon(i),(j,k)->j k));
-- tolie(bM_i) - \sum a_ki tolie(lM_k)
    M/rels
  ); 

  

	
	
------------------------------------------------
--localLie
------------------------------------------------
-- the ith local Lie algebra of a holonomy Lie algebra, 
-- corresponding to the ith flat in the input where
-- the 1-flats are counted before the 2-flats
 
localLie=method()
localLie(ZZ) := (i)->(
    N:=LL;
    j:=length LL.cache.localone;
    if i<j then
       out:=holonomyLie({(LL.cache.localone)_i},{},field=>LL.field) else
       out=holonomyLie({(LL.cache.localtwo)_(i-j)},field=>LL.field);
    useLie N;
    out
    );  


----------------------------------------
--minmodelLie
----------------------------------------
-- the minimal model of a Lie algebra up to degree d

minmodelLie=method(TypicalValue=>LieAlgebra)
minmodelLie(ZZ):=(d)->(
    L:=LL;
    computeLie d; 
    if L#?minmodel then (
	M:=L.minmodel; 
	j:=M.compdeg
	) else (
	M=minmodelone(); 
	j=1
	); 
    for n from j+1 to d do M=minmodelstep(n,M);
    LL=M; 
    computeLie d; 
    M.modelmap=imapLie(L,M,M.cache.homdefs); 
    L.minmodel = M;
    unit:=append(flatten table(1,M.degLength -1,x->0),1);
    L.cache.extRepRing = (L.field)[
	ext_0..ext_(length M.gensLie - 1),
	Degrees=>apply(M.genWeights,x->x+unit)];
    useLie L;
    M
    );   


----------------------------------------
--minPresLie
---------------------------------------- 
-- gives a minimal presentation of H_0(L) up to first degree n, 
-- the presentation uses the same names for the generators as in L.

minPresLie=method(TypicalValue=>LieAlgebra)
minPresLie(ZZ):=(n)->(
    N:=LL;
    if LL#?minmodel and LL.minmodel.compdeg>=n then M:=LL.minmodel else M=minmodelLie(n);
    LL=M;
    if M.gensLie=={} then lieAlgebra{} else (
    po:=positions(M.gensLie,x->(intweight(x#1#0))_(-1)==0);
    f:=M.modelmap; 
    Mgens:=apply(po,x->(M.gensLie)_x);
    Lgens:=f Mgens;
    MP:=ilieAlgebra(Lgens,{},
	genSigns=>apply(po,x->(M.genSigns)_x),
	genWeights=>apply(po,x->(M.genWeights)_x),
	field=>M.field);
    LL=M;
    Mrels:=select(select(M.genDiffs,x->not x===M.zz),y->(intweight(y#1#0))_(-1)==0);
    g:=imapLie(MP,N)*f;
    MPrels:=g Mrels;   
    MP/MPrels)
    );

  

------------------------------------------
-- monomialsLie
------------------------------------------
-- the list of iterated Lie products in a Lie Element

monomialsLie=method(TypicalValue=>List)
monomialsLie(LieElement) := x->(
    LL=class x;
    toList apply(x#1,y->new LL from 
       	{new BasicList from {1_(LL.field)},new BasicList from {y}}
       	)
    );



----------------------------------------
--multLie   
----------------------------------------
-- Lie multiplication of LieElements, 
-- it is also possible to use SPACE as an infix symbol  

multLie=method()
multLie(LieElement,LieElement) := (x,y)->(
    N:=LL;
    LL=class x;
    if not class y===LL then (
       error "the arguments do not belong to the same Lie algebra";
       );
    n:=degLie x+degLie y; 
    computeLie n; 
    out:=idef(imult(ifed x,ifed y));
    useLie N;
    out 
    );


----------------------------------------
--multListLie   
----------------------------------------
-- Lie multiplication of lists of LieElements.  
-- There is an option multOnly, which
-- only multiplies those pairs (i,j) such that the condition is
-- true (the condition might for instance be a condition on the degrees).
-- without the option, it is possible to use SPACE as an infix operator 

multListLie=method(Options=>{multOnly=>(i,j)->true})
multListLie(List,List) := o -> (x,y) -> (
    skipZZ flatten apply(x,z->apply(y,u->if o.multOnly(z,u) then multLie(z,u) else 0))
    );

----------------------------------------
-- normalFormLie
----------------------------------------
-- gives the normal form of a "formal" LieElement 

normalFormLie = method()
normalFormLie(LieElement):= x -> (
    N:=LL;
    LL=class x;
    computeLie(max(0,max(apply(monomialsLie x,ideglie))));
    out:=idef ifed x;
    if not weighttest out then error "the element is not weight-homogeneous";
    if not signtest out then error "the element is not sign-homogeneous";
    LL=N;
    out
    );
   



----------------------------------------
-- peekLie
----------------------------------------
-- gives printed information about the HaschTable, 
-- which is somewhat less than what is got from peek 
-- for LieAlgebras 

peekLie = method()
peekLie(LieAlgebra) := L -> (
    "gensLie => "|net(L.cache.genslie)||
    "genWeights => "|net(L.genWeights)||
    "genSigns => "|net(L.genSigns)||
    "relsLie => "|net(L.relsLie)||
    "genDiffs => "|net(L.genDiffs)||
    "field => "|net(L.field)||
    "diffl => "|net(L.diffl)||
    "compdeg => "|net(L.compdeg)||
    if L#?minmodel then 
    "minmodel => "|net(L.minmodel) else
    if L#?modelmap then
    "modelmap => "|peekLie(L.modelmap) else ""              
     );
peekLie(MapLie) := f-> (
       L:=f.targetLie;
       G:=f.sourceLie.gensLie;
       if all(G,z->f(z)===L.zz) then 
          "0" else if all(G,z->f(z)===z) then
                  "id" else peek f
       );
peekLie(DerLie) := d-> (
    L:=d.targetLie;
    G:=d.sourceLie.gensLie;
    if all(G,z->d(z)===L.zz) then "0" else 
      ( outder(d,G)||
       "maplie => "|peekLie d.maplie||
       "sign => "|net(d.sign)||
       "weight => "|net(d.weight)||
       "sourceLie => "|net(d.sourceLie)||
       "targetLie => "|net(d.targetLie)
       )
    );


----------------------------------------
--permopLie
---------------------------------------- 
-- gives the effect of applying a product of cycles 
-- (of the generators) to a Lie element

permopLie=(x,y)->(
   if not class y===LL then 
      error "the second input is not an element in the current Lie algebra" else
	(
	if not all(flatten x,z->member(z,LL.gensLie)) or 
	   not all(x,z->z===unique z) then 
	    error "the first input is not a product of cycles" else
	    (
	    computeLie(degLie y); 
	    ipermop(x,y)
	    )
	)
    );


----------------------------------------
--randomLie
----------------------------------------
-- gives a random element of degree d in the current Lie algebra

randomLie=method()
randomLie(ZZ) := randomLie(List):= d -> (
    m:=dimLie(d);
    coefs := apply(m,i->random(LL.field));
    sum(coefs,basisLie(d),(i,j)->i j)
    );





----------------------------------------
--signExtLie   
---------------------------------------- 
-- this gives the sign of a basis element in the Ext-algebra,
-- the basis elements are ext_0, ext_1,...
   
signExtLie=method()
signExtLie(RingElement):=(x)-> (
	    po:=index(x); 
	    ((LL.minmodel.genSigns)_po + 1)%2
            );  
signExtLie(List) := x-> apply(x,signExtLie);



----------------------------------------
-- signLie
---------------------------------------
-- the sign of a LieElement

signLie=method()
signLie(LieElement) := (x)->(
    N:=LL;
    LL=class x;
    xx:=normalFormLie(x);
    if xx#1===empty then out:=0 else out=isign((xx#1)#0);
    useLie N; 
    out
    );
signLie(List) := x-> apply(x,signLie);


----------------------------------------
--subalgBasisLie   
----------------------------------------
-- a basis in degree n (or degree n and homological degree d or multidegree y) 
-- for the differential Lie subalgebra generated by the set x of Lie elements

subalgBasisLie=method(TypicalValue=>List)
subalgBasisLie(ZZ,List) := (n,x)->(
    computeLie n;
    d:=diffLie();
    xx:=skipzz join(x,d x);
    idef isubalg(n,apply(xx,ifed))
    );
subalgBasisLie(ZZ,ZZ,List):=(n,d,x)->
    select(subalgBasisLie(n,x),z->(intweight(z#1#0))_(-1)==d);
subalgBasisLie(List,List):=(y,x)->
    select(subalgBasisLie(y_0,x),z->intweight(z#1#0)==y);
    


----------------------------------------
--subalgTableLie   
----------------------------------------
-- the table of dimensions up to degree n
-- and homological degree n-1 for the 
-- differential Lie subalgebra generated by x  
    
subalgTableLie=method()
subalgTableLie(ZZ,List) := (n,x)->(
    computeLie n;
    d:=diffLie();
    xx:=skipzz join(x,d x);
    matrix apply(n,d->apply(n,j->
	    length select(isubalg(j+1,apply(xx,ifed)),z->last degree z==d)))
    );
    
    
----------------------------------------
--symmetryLie
---------------------------------------- 
-- x is a list of  cycles or a reordering of L.gensLie. 
-- It is checked that x induces an automorphism on L
-- if it is true, the map is given as output  

symmetryLie=(x)->
    if class x_0===List then
       if not all(flatten x,z->member(z,LL.gensLie)) or not all(x,z->z===unique z) then 		
	    	    error "the input is not a product of cycles" else 
          mapLie(LL,LL,apply(LL.gensLie,y->opperm(x,y))) else   
            if not (all(x,y->member(y,LL.gensLie)) and unique x===x and LL.numGen==length x) then
               error "the input is not a permutation of the generators" else     
    mapLie(LL,LL,x);
    



----------------------------------------
-- useLie
----------------------------------------
-- this changes the current Lie Algebra
	
  useLie=method()
  useLie(LieAlgebra) := (L)->(
     LL=L;
     use L.cache.mbRing;
     if L.cache#?extRepRing then use L.cache.extRepRing;
     setgen();
     L
      );

----------------------------------------
-- weightExtLie
----------------------------------------
--the weight of a homogeneous element in the Ext-algebra

weightExtLie=method()
weightExtLie(RingElement) := x->
     if not length unique apply(flatten entries monomials x,degree)<=1 then
     error "the element is not weight-homogeneous" else
     degree x;
weightExtLie(List) := x->apply(x,weightExtLie);


----------------------------------------
-- weightLie
----------------------------------------
--the weight of a LieElement

weightLie=method(TypicalValue=>List)
weightLie(LieElement) := (x)->(
    N:=LL;
    LL=class x; 
    xx:=normalFormLie(x);
    out:=intweightsecond(xx#1);
    useLie N;
    out
    );
weightLie(List) := x->apply(x,weightLie);



----------------------------------------
-- whichLie
----------------------------------------
-- prints the current Lie Algebra
	
whichLie = ()->LL;


 
----------------------------------------
--
-- INTERNAL SYMBOLS AND FUNCTIONS
--
----------------------------------------


----------------------------------------
-- SYMBOLS
----------------------------------------


aR=getSymbol("aR");
mb0=getSymbol("mb0");
genslie=getSymbol("genslie");
mb=getSymbol("mb");
fr=getSymbol("fr");
ext=getSymbol("ext");
dims=getSymbol("dims");
opL=getSymbol("opL");
bas=getSymbol("bas");
gr=getSymbol("gr");
cyc=getSymbol("cyc");
bound=getSymbol("bound");
deglist=getSymbol("deglist");
degLength=getSymbol("degLength");
localone=getSymbol("localone");
localtwo=getSymbol("localtwo");
maxDeg=getSymbol("maxDeg");
numGen=getSymbol("numGen");
pr=getSymbol("pr");
ko=getSymbol("ko");
homdefs=getSymbol("homdefs");



----------------------------------------    
-- allgens
---------------------------------------- 
-- gives the list of elements in lieRing corresponding
-- to the generators
allgens=()->apply(LL.numGen,x->LL.cache.lieRing_x);



----------------------------------------    
-- basToMat
----------------------------------------
-- a list m of linear combinations of lieRing elements of
-- degree d to a matrix whose columns are the coefficients of the
-- corresponding elements in ibasis(d).
basToMat=method(TypicalValue=>Matrix) 
basToMat(ZZ,List):=(d,m)->(
    dimd:=LL.cache.dims#d;
    if m==={} or dimd==0 then matrix{{0_(LL.field)}} else 
	(
            idm:=entries (basis((LL.field)^(LL.cache.dims#d)));
            transpose matrix apply(m, x-> if x==0 then 
	     	flatten table(1,LL.cache.dims#d,x->0_(LL.field)) 
	     	else sum(
	            apply(flatten entries (coefficients x)_1,
		       	x->(map(LL.field,LL.cache.lieRing))(x)), 
	            apply(flatten entries (coefficients x)_0,
	               	y->idm_(ind(d,y))),(i,j)->i*j
	            )
	    	)
            )
    ); 
-- a list m of linear combinations of lieRing elements of first
-- degree d and last degree j to a matrix whose columns are the coefficients of the
-- corresponding elements in ibasis(d,j).
basToMat(ZZ,ZZ,List):=(d,j,m)->(
    if m==={} or LL.cache.dims#d==0 then matrix{{0_(LL.field)}} else 
          (
       	    idm:=entries (basis((LL.field)^(dimLie(d,j))));
       	    transpose matrix apply(m, x->if x==0 then 
               	flatten table(1,dimLie(d,j),x->0_(LL.field)) 
	       	else sum(
		    apply(flatten entries (coefficients x)_1,
		       	x->(map(LL.field,LL.cache.lieRing))(x)),
	            apply(flatten entries (coefficients x)_0,
	               	y->idm_(ind(d,j,y))),(r,s)->r*s
	            )    
	    	)
    	    )
    ); 





----------------------------------------    
-- boundariesdim
----------------------------------------
-- returns the dimension only

boundariesdim=(n,d)->length iboundaries(n,d); 



----------------------------------------    
-- commrel and commrelgen
----------------------------------------  
-- the commutative law on basis elements, used in computeLie

commrel=(m)->(
    defm:=idefmon(m);
    if #defm==1 then 0_(LL.cache.lieRing) else (
	x:=first defm;
	xx:=take(defm,{0,0});
	m1:=drop(defm,1); 
	m+eps(xx,m1)*op(m1,LL.cache.lieRing_(x))
	)
    ); 

commrelgen=(x,m)->(
    defm:=idefmon(m);
    xx:=new BasicList from {x}; 
    op(xx,m)+eps(xx,defm)*op(defm,LL.cache.lieRing_(x)));

----------------------------------------
--computeLie   
----------------------------------------      

computeLie=method(TypicalValue=>List)
computeLie(ZZ):=(d)->(
    if LL.compdeg>=d then idims(d) else if LL.cache.genslie==={} then (
        if LL.cache.maxDeg<d then  LL.cache.maxDeg=d+5;
        LL.compdeg=d; 
	doZeroLie(); 
	idims(d)
	) else (   
	  if LL.cache.maxDeg<d then (
	      LL.cache.maxDeg=d+5; 
	      j0:=1; 
	      LL.cache.lieRing=lieR();
	      ) else j0=LL.compdeg+1;   		    	     
         for i from j0 to d  do (   
	   LL.cache.gr#(i)=ideal {0_(LL.cache.lieRing)}; 
	   prebasis:={};
	   rellist:={};
           for j from 1 to min(i,genDegMax()) do 
	       prebasis=prebasis|flatten apply(LL.cache.bas#(i-j),y->
		   apply(gendeg(j),x->op(new BasicList from {x},y)));
           for j from 1 to min(i,relDegMax()) do 
	       rellist=rellist|
	           flatten apply(LL.cache.bas#(i-j),m->
		   apply(rel(j),x-> 
		   if x#0===empty then 0_(LL.cache.lieRing)  else 
		      summ(x#0,x#1,(i,j)->i*op(j,m))));
	   newrellist:=flatten apply(rel(i),y->
	       (toList apply(y#1,z->relcomm(z))));
    	   rellist=rellist|newrellist;
	   ch:=char LL.field;
           badgen:=select(LL.numGen,y->(LL.genWeights_y)_0%ch==0 and 
	       (LL.genWeights_y)_0<i);
           newrellist=flatten apply(badgen,x->
	       apply(LL.cache.bas#(i-(LL.genWeights_x)_0),m->commrelgen(x,m)));
           rellist=rellist|newrellist;
    	   if ch==3 and i%3==0 then rellist=rellist|apply(select(LL.cache.bas#(i//3),z->
		   isign z==1),x->imult(x,imult(x,x)));
    	   if ch==2 and i%2==0 then rellist=
	           rellist|apply(LL.cache.bas#(i//2),x->imult(x,x));
	   deglimit:=prepend(i,flatten table(1,LL.degLength-1,x->0));
    	   if (rellist=={} or ideal rellist==ideal{0_(LL.cache.lieRing)}) then (
	       gr0:=ideal {0_(LL.cache.lieRing)};
	       bas0:=prebasis;
	       ) else (  
	       ide:=ideal rellist;
	       gr0=ideal(flatten entries gens gb(ide,DegreeLimit=>deglimit));
	       grlead:=apply(flatten entries gens gr0, leadMonomial);
	       bas0=select(prebasis,x->(not member(x,grlead)));
	       );
    	   if gr0==0 then newrellist= flatten apply(bas0,m->commrel(m)) else 
	       newrellist= toList apply(flatten apply(bas0,m->commrel(m)),x->x%gr0);
    	   if rellist|newrellist == {} then (
	       LL.cache.gr#(i)=ideal {0_(LL.cache.lieRing)};
	       LL.cache.bas#(i)=bas0;
	       LL.cache.deglist#(i)=apply(LL.cache.bas#(i),degree)
	       ) else (  
	       newide:=ideal(rellist|newrellist); 
	       if newide==ideal{0_(LL.cache.lieRing)} then (
		   LL.cache.gr#(i)=ideal {0_(LL.cache.lieRing)};
		   LL.cache.bas#(i)=bas0;
		   LL.cache.deglist#(i)=apply(LL.cache.bas#(i),degree)
		   ) else (  
		   LL.cache.gr#(i)=
		     ideal(flatten entries gens gb(newide,DegreeLimit=>deglimit));
                   grlead=apply(flatten entries gens LL.cache.gr#(i),leadMonomial);
               	   LL.cache.bas#(i)=select(bas0,x->(not member(x,grlead)));
		   LL.cache.deglist#(i)=apply(LL.cache.bas#(i),degree)
	      	   )
	       );
       	   LL.cache.dims#i=length (LL.cache.bas#(i)) -- next i
   	 ); -- end of for loop
      LL.cache.mbRing=modR(d);
      LL.compdeg=d; 
      idims(d)
      )
   );


----------------------------------------    
-- cycles
----------------------------------------
-- gives a basis in lieRing for the cycles of first degree n and
-- homological degree j
 
cycles=(n,j)->(
    if LL.cache.cyc#?(n,j) then  LL.cache.cyc#(n,j) else (
    computeLie n;
    if j==0 then LL.cache.cyc#(n,0)=ibasis(n,0) else (
	--if ibasis(n,j)==={} or ibasis(n,j-1)==={} then ba:=0 else
	ba:=basToMat(n,j-1,apply(ibasis(n,j),x->idiff(idef x)));
        if ba==0 then LL.cache.cyc#(n,j)=ibasis(n,j) else 
	   LL.cache.cyc#(n,j)=skipz matToBas(n,j,matrix gens kernel ba)
	)
    ));
    
   

----------------------------------------    
-- deflist
----------------------------------------  
-- when x is the value of monlist y, where y is in lieRing, 
-- deflist x computes the corresponding iterated 
-- Lie product of generators (as a BasicList)

deflist=(x)->(if x==={} then empty else 
    prepend((last x)%(LL.numGen),deflist(drop (x,-1))));





----------------------------------------    
-- degiterlie
----------------------------------------
-- gives the multidegree of an iterated lie monomial in 
-- digital form ({1,2,0,1})

degiterlie=(x)->summ(x,y->LL.genWeights_y); 


----------------------------------------    
-- degRel
----------------------------------------
-- computes the multidegree of a homogeneous relation y 
 	    

degRel=method()
degRel(LieElement):=(y)-> if y#1===empty then {0} else degiterlie((y#1)#0);

----------------------------------------
--dimLie   
----------------------------------------
-- the dimension of the Lie algebra in a specified degree
 
dimLie=method(TypicalValue=>ZZ)
dimLie(ZZ):=(d)->(
    computeLie d;
    LL.cache.dims#d
    );
dimLie(ZZ,ZZ):=(n,d)->(
    computeLie n;
    length ibasis(n,d)
    );
dimLie(List):=(x)->(
    computeLie x_0;
    length ibasis(x)
    );


----------------------------------------    
-- doZeroLie
----------------------------------------
-- this sets the keys for the zero Lie algebra, with dims=0 up to maxDeg

doZeroLie=()->(
    LL.genWeights=LL.genSigns={}; LL.degLength=0;
    for i from 1 to LL.compdeg do (
	LL.cache.bas#i={};
	LL.cache.dims#i=0;
	LL.cache.gr#i=ideal{0_(LL.field)};
	LL.cache.deglist#i={}
	)
    );

---------------------------------------
-- empty, the empty BasicList
--------------------------------------


empty=new BasicList from {};


----------------------------------------    
-- eps
----------------------------------------
-- the exchange sign when x and y are iterated Lie products (as BasicLists)
-- in digital form
  
eps=(x,y)->((-1)^(isign(x)*isign(y)));  


----------------------------------------    
-- evalperm
----------------------------------------  
-- x is a permutation of elements in the form of a cycle, y is an element

evalperm=(x,y)->(if member(y,x) then (
	i:=position(x,z->z===y); 
	x_((i+1)%length x)
	) else y
    );


----------------------------------------    
-- extmult
----------------------------------------
-- multiplication in the Ext-algebra
     
extmult=(i,j)-> (
    N:=LL;
    M:=LL.minmodel;
    LL=M; 
    fi:=(M.gensLie)_i; 
    fj:=(M.gensLie)_j;
    si:=(M.genSigns)_i; 
    sj:=(M.genSigns)_j;
    co:=apply(M.numGen,k->(
	    coes:=toList ((M.genDiffs)_k)#0; 
	    arrs:=monomialsLie (M.genDiffs)_k;
	    len:=length coes;
	    if not (member(fi fj,arrs) or member(fj fi,arrs)) then 
	    0_(N.cache.extRepRing) else (
		if member(fi fj,arrs) then (
		    po:=(select(len,x->arrs_x===fi fj))_0;
		    coes_po*(if i==j then 2 else (-1)^(sj*(si+1))))
		else (
		    po=(select(len,x->arrs_x===fj fi))_0;
		    coes_po*(-1)^(sj+1)
	  -- this sign is correct! It makes the ExtAlgebra SkewCommutative
		    )
	    	)
	    )
    	);
    out:=sum(co,gens N.cache.extRepRing,(r,s)->r*s);
    LL=N;
    out   
    );

----------------------------------------    
-- extmonmult
----------------------------------------
extmonmult=(a,b)->extmult(index(a),index(b));
  

----------------------------------------    
-- genbas
---------------------------------------- 
-- gives the basis element in lieRing corresponding to the
-- generator z

genbas=(z)->(
    i:=position(LL.gensLie,x->(x===z));
    LL.cache.lieRing_i
    );
    

----------------------------------------    
-- genDegMax
----------------------------------------
-- the maximal first degree of the generators 

genDegMax=()->max(0,max apply(LL.genWeights,x->x_0));
  

----------------------------------------    
-- gendeg
----------------------------------------   

gendeg=method(TypicalValue=>List)
-- gives the generators in digital form of first degree d
gendeg(ZZ):=(d)->select(LL.numGen,y->((LL.genWeights_y)_0===d));
-- gives the generators in digital form of multidegree x
gendeg(List):=(x)->select(LL.numGen,y->(LL.genWeights_y===x));


----------------------------------------
--homologyLie
----------------------------------------
-- the dimension of the homology in degree n
-- and homological degree d

homologyLie = method()
homologyLie(ZZ,ZZ) := (n,d)->(
    dimLie(n,d)-boundariesdim(n,d-1)-boundariesdim(n,d)
    );


----------------------------------------    
-- ibasis
----------------------------------------  
ibasis=method(TypicalValue=>List)
-- gives the list of basis elements in lieRing of first degree n: 
ibasis(ZZ):=(n)->LL.cache.bas#n;
-- gives the list of basis elements in lieRing of first degree n
-- and last degree d: 	 
ibasis(ZZ,ZZ):=(n,d)->select(ibasis(n),x->last degree x==d);
-- gives the list of basis elements in lieRing of multidegree x:   
ibasis(List):=(x)->select(ibasis(x_0),y->degree y===x); 

----------------------------------------    
-- iboundaries
----------------------------------------
-- gives a basis in lieRing for the boundaries of first degree n and
-- homological degree d

iboundaries=(n,d)->(
    	if LL.cache.bound#?(n,d) then  LL.cache.bound#(n,d) else (
        computeLie n;
	idi:=skipz apply(ibasis(n,d+1),x->idiff(idef x)); 
	if idi==={} then LL.cache.bound#(n,d)={} else 
	    LL.cache.bound#(n,d)=flatten entries gens gb(
	    ideal idi,DegreeLimit=>
	    prepend(n,flatten table(1,length((LL.genWeights)_0)-1,x->0)))
        ));
    


----------------------------------------    
-- idefmon
----------------------------------------
-- transforms a monomial in a lieRing to an iterated Lie product
-- in digital form (as a BasicList) 

idefmon=(m)->deflist monlist m;


----------------------------------------    
-- idef
----------------------------------------    
-- computes the LieElement corresponding to the element p in lieRing 
-- idef is listable. 

idef=method()
idef(List):=(p)->apply(p,x->idef(x));
idef(RingElement):=(p)-> ( 
    mons:=new BasicList from flatten entries monomials p; 
    if mons===empty then LL.zz else (  
       if p===mons#0 then new LL from new BasicList from
       {new BasicList from {1_(LL.field)},
	   new BasicList from {idefmon(p)}} else (
	    coef:=apply(mons,x->(p_x));
	    new LL from {coef,apply(mons,idefmon)}
	    )
	));

----------------------------------------    
-- ideg
----------------------------------------
-- the first degree of an element in a ring, the degree is defined as 0 for x=0

ideg=x->if x==0 then 0 else (degree x)_0;

----------------------------------------
ideglie
----------------------------------------
-- the internal version of degLie

ideglie = x-> if x#1===empty then 0 else (intweight((x#1)#0))_0;


---------------------------------------
-- idiff
---------------------------------------
-- the value in lieRing of the differential on a Lie element
-- for internal use, idef idiff x is the same as (diffLie())(x) 


idiff=method(TypicalValue=>RingElement)
idiff(LieElement) := x->(
    yd:=apply(LL.genDiffs,ifed);
    yf:=apply(LL.gensLie,ifed);
    g:=z->if #z==1 then yd_(z#0) else 
    imult(yd_(z#0),imultindex(yf,drop(z,1)))+
    (-1)^(isign(z#0))*imult(yf_(z#0),g drop(z,1));
    if x#0===empty then 0_(LL.cache.lieRing) else 
           summ(x#0,x#1,(i,j)->i*g j));





----------------------------------------    
-- idims
----------------------------------------  
idims=d->for i from 1 to d list LL.cache.dims#i;


----------------------------------------    
-- idimtot
----------------------------------------  

idimtot=d->sum idims(d);


----------------------------------------    
-- idivisor
----------------------------------------
-- m is a list of lieRing elements of degree t,
-- p is a list of lieRing elements of degree s, the result 
-- is a basis for those x of degree d=t-s such that x multiplies
-- p to the space generated by m

idivisor=(t,s,m,p)->(
        d:=t-s;
	if LL.cache.dims#d==0 then {} else (
      	B:=transpose basToMat(t,m);
      	kerB:=transpose generators kernel(B);
      	matToBas(d,generators kernel joinvert apply(p,y->
		(kerB*basToMat(t,apply(ibasis(d),x->imult(y,x))))))
	));    

----------------------------------------    
-- ieuler
---------------------------------------- 
-- computes the eulercharacteristics of first degree n. It is
-- assumed that the homological degree is at most n-1.

ieuler=(n)->sum apply(n,j->(-1)^j*dimLie(n,j));


----------------------------------------    
-- ifed
----------------------------------------
-- transforms a LieElement (or a Lie monomial as a BasicList) of 
-- first degree d to a linear combination
-- of elements in bas#d
-- ifed(idef m)=m if m is a basis element

ifed=method(TypicalValue=>RingElement)
ifed(BasicList):=(x)->(
    le:=#x; 
    if le===1 then ( 
	d:=(degiterlie(x))_0; 
	if gens LL.cache.gr#(d) == 0 then (
	    (LL.cache.lieRing)_(x#0) 
	    ) else (
	    ((LL.cache.lieRing)_(x#0))%LL.cache.gr#d
	    )
	) else  op(take(x,{0,0}),ifed(drop(x,1)))
    );   
ifed(LieElement):=(x)->if x#1===empty then 0_(LL.cache.lieRing) else 
                       summ(x#0,x#1,(i,j)->i*ifed(j));
ifed(List):= x->apply(x,ifed);

		       
-----------------------------------------
--ihomology
-----------------------------------------
-- gives a basis in terms of cycles in lieRing for the
-- homology of first degree n and homological degree j
		       
ihomology=(n,j)->(
	if iboundaries(n,j)=={} then prel:=cycles(n,j) else (
            I:=ideal iboundaries(n,j);
	    prel=skipz apply(cycles(n,j),x->x%I));
	if prel=={} then {} else 
            flatten entries gens gb(ideal prel,DegreeLimit=>
	    prepend(n,flatten table(1,length((LL.genWeights)_0)-1,x->1)))
   );

----------------------------------------    
-- iideal
----------------------------------------
-- gives a minimal generator set of lieRing elements
-- in first degre n for the lie ideal generated by 
-- lieRing elements in the list y (which may contain
-- elements of different degrees)  
 
iideal = method(TypicalValue=>List) 
iideal(ZZ,List):=(n,y)->(cacheide:=new MutableHashTable;
    if cacheide#?(n,y) then cacheide#(n,y) else (
	rs:=listdeg(n,y);
	for i from 1 to n-1 do 
	rs=rs|flatten apply(gendeg(n-i),x->apply(iideal(i,y),z->op({x},z))); 
	rs=skipz rs;
	if rs==={} then cacheide#(n,y)={} else
	cacheide#(n,y)=flatten entries gens gb(
	    ideal rs,DegreeLimit=>
	    prepend(n,flatten table(1,length((LL.genWeights)_0)-1,x->0)))
      	)
    );
-- as above, but with the first degree replaced by the multidegree 
iideal(List,List):=(d,y)->(select(iideal(d_0,y),z->degree z==d));


----------------------------------------
-- ilieAlgebra
----------------------------------------
-- this is an internal version of lieAlgebra which does not make any checking,
-- the relations are given as the second input, which all
-- belong to a free Lie algebra which is ambient of the constructed
-- Lie algebra  

ilieAlgebra = method(TypicalValue => LieAlgebra,Options => 
    {genWeights => 1, genSigns => 0, field => QQ, diffl => false})
ilieAlgebra(List,List) := opts->(gensLie,relsLie)->(
    L := new MutableHashTable of LieElement; 
    LL=L;
    L.gensLie=apply(gensLie,baseName);
    L.genWeights=opts.genWeights;
    L.genSigns=opts.genSigns;
    L.diffl=opts.diffl;
    L.field=opts.field;
    L.genDiffs={};
    L#(symbol cache)=new CacheTable;
    L.relsLie=relsLie;
    L.numGen=length L.gensLie;
    L.compdeg=0;
    L.cache.maxDeg=5;
    L.cache.lieRing=L.field[]; 
    L.cache.mbRing=L.field[];
    L.cache.genslie=apply(gensLie,baseName);
    L.cache#(symbol dims)=new MutableHashTable;
    L.cache#(symbol opL)=new MutableHashTable;
    L.cache#(symbol bas)=new MutableHashTable;
    L.cache#(symbol gr)=new MutableHashTable;
    L.cache#(symbol cyc)=new MutableHashTable;
    L.cache#(symbol bound)=new MutableHashTable;
    L.cache#(symbol deglist)=new MutableHashTable;
    L.cache.bas#0={mb0}; 
    L.cache.dims#0=1; 
    if L.numGen==0 then doZeroLie() else
      L.degLength=length (L.genWeights)_0; 
    L.cache.lieRing=lieR();
    M:=new LieAlgebra from L;
    LL=M;
    M.zz=new M from new BasicList from {empty,empty}; 
    M.genDiffs=flatten table(1,M.numGen,x->M.zz);
    M.gensLie=apply(M.numGen,i->new M from 
	(new BasicList from {new BasicList from {1_(M.field)},
	 new BasicList from {new BasicList from {i}}}));
    net M:= x->(
	N:=LL;
	LL=M; 
	if member(x,LL.gensLie) then y:=1 x else y=x;
	if y#0===empty then out:=toString 0 else 
	    if (y#0)#0==1 and #(y#1)==1  then out=outmon (y#1)#0 else 
              out=outputrec y;
	useLie N;
	if substring(out,0,1)=== "+" then substring(out,2) else 
	       if substring(out,0,2)=== " +" then substring(out,3) else out);	
    for i from 0 to M.numGen-1 do (M.cache.genslie)_i<-(M.gensLie)_i;
    M);




----------------------------------------    
-- imult
----------------------------------------
-- the internal Lie product on lieRing
-- OBS: imult, when defined on lists, removes zeroes, but multListLie does not!

imult=method(TypicalValue=>RingElement)
imult(RingElement,RingElement):=(x,y) -> linext((u,v)->imultmon(u,v),x,y);
imult(List,List):= (x,y)->(skipz flatten apply(x,z->apply(y,u->imult(z,u))));

---------------------------------------
-- imultindex
---------------------------------------
-- it is used in definition of application of maps and derivations
-- and also in idiff 

imultindex = (y,a)->fold(apply(#a,i->y_(a#i)),(u,v)->imult(u,v));


----------------------------------------    
-- imultmon
----------------------------------------  
-- x,y are basis elements in L.cache.lieRing of degree d,e;
-- output is the Lie product, which is a linear combination of 
-- basis elements of degree d+e 
 
imultmon=(x,y)->op(idefmon(x),y);  

----------------------------------------    
-- ind
----------------------------------------
-- gives the index of m in ibasis(d), m is a basis element in lieRing
ind=method(TypicalValue=>ZZ)
ind(ZZ,RingElement):=(d,m)->position(LL.cache.bas#d,x->x===m);
--gives the index of m in ibasis(d,j). m belongs to lieRing
ind(ZZ,ZZ,RingElement):=(d,j,m)->position(ibasis(d,j),x->x===m);

--------------------------------------
-- internal version of indexFormLie
--------------------------------------
indexform = method()
indexform(LieElement) := (x)->(
    LL=class x;
    computeLie ideglie x;
    if x#1===empty then 0_(LL.cache.mbRing) else
    summ(x#0,x#1,(i,j)->i*(lieToMb ifed j))
    );
indexform(List):=x->apply(x,indexform); 

		
---------------------------------------
-- intweight
--------------------------------------
-- the internal weight on an iterated Lie product as a BasicList

intweight=method(TypicalValue=>List)
intweight(ZZ):=(g)->(LL.genWeights)_g;
intweight(BasicList):=(x)->
	  if #x==1 then intweight(x#0) else 
	     intweight(first x) + intweight(drop(x,1));
	     
---------------------------------------
-- intweightsecond
--------------------------------------
-- the internal weight on the second part of a LieElement

intweightsecond=method(TypicalValue=>List)
intweightsecond(BasicList):=(x)->
    if x===empty then 
       flatten table(1,degreeLength LL.cache.lieRing,x->0) else intweight(x#0);
	  
	     
	     
----------------------------------------    
-- invimage
----------------------------------------
-- gives a basis, in terms of columns in a matrix, for the space of
-- those x such that Ax is in the column space of B. A and B are matrices
-- of numbers with the same number of rows. The number of rows in output
-- is equal to the number of columns in A. Used in invImageLie. See also
-- idivisorLie.
invimage=(A,B)->generators kernel((transpose generators kernel(transpose B))*A); 


	     
	     
----------------------------------------    
-- ipermop
----------------------------------------  
--  y is a LieElement, x is a product of cycles of generators, output
--  is a LieElement
ipermop=(x,y)->	(
    xx:=apply(x,z->apply(z,u->position(LL.gensLie,y->y===u)));    
    idef ifed new LL from new BasicList from {y#0,apply(y#1,z->opiter(xx,z))}
    );

    
	     


----------------------------------------    
-- isign
----------------------------------------  
isign=method(TypicalValue=>ZZ)
-- the sign of a generator 
isign(ZZ):=(x)->(LL.genSigns)_x;
-- the sign of an iterated Lie product
isign(BasicList):=(x)->(summ(x,y->isign y))%2;
-- the sign of a LieElement      
isign(LieElement):=(x)->if x#1===empty then 0 else isign((x#1)#0);
-- the sign of a homogeneous element i lieRing
isign(RingElement):=(x)->isign idef x; 


----------------------------------------    
-- isubalg
----------------------------------------  
-- gives a basis in lieRing of first degree n for the Lie subalgebra
-- generated by the elements in the list x (which may contain elements 
-- of different degrees).
isubalg=method(TypicalValue=>List)
isubalg(ZZ,List):=(n,x)->(
    cachesub:=new MutableHashTable;
    if cachesub#?(n,x) then cachesub#(n,x) else (
	rs:=listdeg(n,x);
	for i from 1 to n-1 do rs=rs|imult(listdeg(n-i,x),isubalg(i,x));
	if rs=={} then cachesub#(n,x)={} else (
	    cachesub#(n,x)=
	    flatten entries gens gb(ideal rs,DegreeLimit=>
	 	prepend(n,flatten table(1,length((LL.genWeights)_0)-1,x->0)))
	    )
	)
    );  
-- as above, but with degree replaced by multidegree:
isubalg(List,List):=(y,x)->(
    select(isubalg(y_0,x),z->degree z==y)
    );
  


----------------------------------------    
-- iweight
----------------------------------------
-- the multidegree of an element in lieRing

iweight = x-> if x==0 then flatten table(1,degreeLength ring x,x->0) else degree x;


----------------------------------------    
-- joinhoriz
---------------------------------------- 
-- horizontal concatenation of a list of matrices
joinhoriz=x->(
    if length x==1 then x_0 else fold((u,v)->u|v,x_0,drop(x,1))
    );


----------------------------------------   
-- joinvert
---------------------------------------- 
-- vertical concatenation of a list of matrices
joinvert=x->(
    if length x==1 then x_0 else fold((u,v)->u||v,x_0,drop(x,1))
    );


----------------------------------------    
-- lieR
----------------------------------------
-- construction of lieRing

lieR=()->(LL.field)[(aR)_0..(aR)_(LL.cache.maxDeg*LL.numGen-1),
    Degrees=>flatten table(LL.cache.maxDeg,LL.genWeights,(x,y)->y)];

----------------------------------------    
-- lieToMb
----------------------------------------
-- conversion of a basis element (or a list of basis elements) in lieRing
-- to a linear combination of elements mb_{n,i} (corresponding to the
-- i'th basis element in bas#n) in mbRing.

lieToMb=method(TypicalValue=>RingElement)
lieToMb(RingElement):=(p)->(
    linext(x->lieToMbmon(x),map(LL.cache.mbRing,LL.cache.lieRing),p)
    );
lieToMb(List):=(x)->apply(x,y->lieToMb(y));


----------------------------------------    
-- lieToMbmon
---------------------------------------- 
-- gives to a basis element in lieRing the
-- corresponding basis element in mbRing 
   
lieToMbmon=(m)->(
    d:=ideg m;
    po:=position(LL.cache.bas#d,x->x===m);
    (LL.cache.mbRing)_(idimtot(d-1)+po)
    );


----------------------------------------    
-- linext
----------------------------------------
-- is the linear extension of f, which is defined on monomials
-- in a polynomial ring, with values in the same polynomial ring.

linext=method();
linext(Function,RingElement):=(f,p)->(
    if p==0 then 0_(ring p) else ( 
	co:= coefficients p; 
	mons:=flatten entries first co;
	coefs:=flatten entries last co;
	sum(coefs,mons,(i,j)->i*f(j))
      )
  ); 
-- If the value is in another polynomial ring, the coefficients need
-- to be mapped to this ring as follows:

linext(Function,RingMap,RingElement):=(f,g,p)->(
    if p==0 then g(0_(ring p)) else ( 
	co:= coefficients p; 
	mons:=flatten entries first co; 
	coefs:=flatten entries last co;
	sum(coefs,mons,(i,j)->g(i)*f(j))
	)
    );
-- below is the bilinear extension of f, which is defined on pair of
-- monomials in a polynomial ring

linext(Function,RingElement,RingElement):=(f,p,q)->linext(x->linext(y->f(x,y),q),p);

----------------------------------------    
-- listdeg
----------------------------------------
-- selects the elements in a list x of first degree n
listdeg=(n,x)->(select(x,y->(ideg y==n)));



----------------------------------------    
-- matToBas
----------------------------------------  
matToBas=method(TypicalValue=>List) 
-- the number of rows in A equals dimLie(d),
-- the result is a list of linear combination of
-- basis elements in lieRing of first degree d corresponding
-- to the columns in A
matToBas(ZZ,Matrix):=(d,A)->(
    if A==0 then flatten table(1,numgens source A,x->0_(LL.cache.lieRing)) else (
	apply(entries transpose A,x->sum(x,ibasis(d),(i,j)->i*j))
	)
    );
-- the number of rows in A equals dimLie(d,j),
-- the result is a list of linear combination of
-- basis elements in lieRing of first degree d and last degree j corresponding
-- to the columns in A
matToBas(ZZ,ZZ,Matrix):=(d,j,A)->(
    if A==0 then flatten table(1,numgens source A,x->0_(LL.cache.lieRing)) else (
	apply(entries transpose A,x->sum(x,ibasis(d,j),(r,s)->r*s))
	)
    );



----------------------------------------    
-- mbToLie
----------------------------------------
-- conversion of an element (or a list of elements) in mbRing 
-- to the corresponding linear combination of elements in bas#d,
-- which is an element in lieRing

mbToLie=method(TypicalValue=>RingElement)
mbToLie(RingElement):=(p)->(
    linext(x->mbToLiemon(x),map(LL.cache.lieRing,LL.cache.mbRing),p)
    );
mbToLie(List):=(x)->apply(x,y->mbToLie(y));


----------------------------------------    
-- mbToLiemon
---------------------------------------- 
-- given a basis element in mbRing this gives the 
-- corresponding basis element in lieRing

mbToLiemon=(m)->(
    d:=ideg m; 
    computeLie d;
    po:=position(gens LL.cache.mbRing,x->x===m);
    (LL.cache.bas#d)_(-idimtot(d-1)+po)
    );


----------------------------------------    
-- minmodelone
----------------------------------------
--contruction of the model in first degree 1
minmodelone=()->(
    L:=LL;
    computeLie 1;
    gens1:=basisLie 1; 
    newgens:=apply(length gens1,x->fr_x); 
    newweights:=apply(gens1,x->intweight((x#1)#0));
    newsigns:=apply(gens1,x->isign(x));
    M:=ilieAlgebra(newgens,{},genWeights=>newweights,
	genSigns=>newsigns,diffl=>true,field=>LL.field);
    M.cache.homdefs=gens1;
    useLie L;
    M
    );

----------------------------------------
--minmodelstep
----------------------------------------
-- given that the Lie algebra M is a free minimal model of L up to
-- first degree n-1, the output becomes a free minimal model up to
-- first degree n

minmodelstep=(n,M)->(
    newgens:=M.gensLie;
    newsigns:=M.genSigns; 
    newdiffs:=M.genDiffs; 
    newweights:=M.genWeights; 
    L:=LL;
    -- step 1: H(f) surjective, f: M -> L
    f:=imapLie(L,M,M.cache.homdefs);
    LL=M; 
    computeLie(n); 
    LL=L; 
    indechom:=(i)->(
	dec:=iboundaries(n,i); 
	cyc:=ihomology(n,i);
	LL=M; 
	cycM:= ihomology(n,i); 
	yL:=f idef cycM;
	LL=L;	
	dec=join(dec,ifed yL);
	if dec=={} then cyc else (
	    prel:= skipz apply(cyc,x->x%ideal dec); 
	    if prel=={} then {} else skipz flatten entries gens gb(ideal prel,
		DegreeLimit=>prepend(n,flatten table(1,length(L.genWeights)_0-1,x->1)))
	  )
      );
  newfR:=flatten for i from 0 to n-1 list indechom(i);
  newf:=apply(newfR,x->idef(x));
  newgens=join(apply(newgens,baseName),
      toList(fr_(length newgens)..fr_(length newgens + length newf - 1)));
  newsigns=join(newsigns,apply(newfR,x->isign(x)));
  newweights=join(newweights,apply(newfR,x->iweight(x)));
  M1:=ilieAlgebra(newgens,{},genSigns=>newsigns,
      genWeights=>newweights,diffl=>true,field=>L.field);
  g:=imapLie(M1,M);
  M1.genDiffs=newdiffs=join(g newdiffs,apply(length newf,x->M1.zz));
  M1.cache.homdefs=join(M.cache.homdefs,newf);
   -- step 2: H(f) injective
  f=imapLie(L,M1,M1.cache.homdefs); 
  LL=M1;
  computeLie(n); 
  unit:=append(flatten table(1,M1.degLength -1,x->0),1);
  hoM:=hoL:=ge:=newbo:=baL:=baM:=baD:=newf={};
  for i from 0 to n-1 do (    
      LL=L;
      r:=homologyLie(n,i); 
      LL=M1; 
      s:=homologyLie(n,i);
      if s > r  then  (    
	  LL=L;
	  baD=basToMat(n,i,apply(basisLie(n,i+1),x->idiff(x)));
	  -- the matrix for d_L in bas(n,i+1,L) --> bas(n,i,L)
	  LL=M1;
	  hoM=(ihomology(n,i)); 
	  -- z_1,...,z_s cycles in M1.cache.lieRing, s>0
	  y:=f idef hoM;
	  LL=L; 
	  hoLM:=ifed y;
	  -- f(z_1),...,f(z_s)
	  baM=basToMat(n,i,hoLM);
	  -- the matrix for hoLM in ibasisLie(n,i,L)
	  if r==0 or skipz hoLM=={} then (
	      -- directly to the last step, a simple case when fr 
	      -- kills the basis for hoM and fr --> v_i, where d_L(v_i)=f(z_i): 
	      len:=length newgens;
	      newgens=join(newgens,toList(fr_len..fr_(len+s-1)));  
	      LL=M1;
	      newsigns=join(newsigns,apply(hoM,x->(isign(x)+1)%2));
	      newdiffs=join(newdiffs,idef(hoM));
	      newweights=join(newweights,apply(hoM,x->degree(x)+unit));
	      newf=join(newf,(
		      LL=L; 
		      if baD==0 then flatten table(1,s,x->L.zz) else 
		       apply(flatten entries (
		               matrix{ibasis(n,i+1)}*solve(baD,baM)),
		           x->idef(x)))); 
	       -- see below	 
	       ) else (
	       hoL=ihomology(n,i); 
	       -- u_1,...,u_r are cycles in lieRing
	       baL=basToMat(n,i,hoL);
	       -- the matrix for hoL in ibasisLie(n,i,L)	    
	       ge=generators kernel matrix take(
		   entries solve(if baD==0 then baL else baL|baD,baM),r);
	       -- (a_1,...,a_s) such that z=\sum(a_i f(z_i)) is a boundary
	       newbo=flatten entries (matrix{hoM}*ge);
	       -- list of cycles \sum a_iz_i to be killed
	       newf=join(newf,(
		       LL=L; 
		       if baD==0 then flatten table(1,length newbo,x->L.zz) else 
		       apply(
		           flatten entries (
		               matrix{ibasis(n,i+1)}*solve(baD,baM*ge)),
		           x->idef(x))
		       )); 
	       -- list x which will be f:s values on the new generators
    	       -- summary (commutative diagram): f: fr_i --> x, 
	       -- d_M1: fr_i --> z, f: z --> v, d_L_ x --> v
	       len=length newgens;
	       newgens=join(apply(newgens,baseName),apply(length newbo,x->fr_(len+x)));
	       LL=M1;
	       newsigns=join(newsigns,apply(newbo,x->(isign(x)+1)%2));
	       newdiffs=join(newdiffs,idef(newbo));
	       newweights=join(newweights,apply(newbo,x->degree(x)+unit));
	       ) -- this finishes the main case when there are cycles to kill
	   ) -- next i
       ); -- end of loop
   M2:=ilieAlgebra(newgens,{},genSigns=>newsigns,
       genWeights=>newweights,diffl=>true,field=>L.field);
   useLie M2;
   g=imapLie(M2,M1);
   M2.genDiffs= g newdiffs;
   M2.cache.homdefs=join(M1.cache.homdefs,newf);
   useLie L; 
   M2	
   ); 





----------------------------------------    
-- modR
----------------------------------------
-- creates mbRing when called from computeLie

modR=method(TypicalValue=>Ring)
modR(ZZ):=(d)->(
    mblist:={};
    degli:={};
    for i from 1 to d do for j from 0 to LL.cache.dims#i-1 do 
        mblist= append(mblist,(mb)_{i,j});
    for i from 1 to d do degli=degli|LL.cache.deglist#i;
    if degli=={} then LL.field else LL.field[mblist,Degrees=>degli]
    );


----------------------------------------    
-- monlist
----------------------------------------  
-- transforms a squarefree monomial in any ring to a list of indices 

monlist=m->(
    expm:=flatten exponents m;
    r:={};
    scan(length expm,i->(if expm_i > 0 then r=append(r,i)));
    r
    );


----------------------------------------    
-- op
----------------------------------------
-- computes the operation of an iterated Lie product 
-- (as a Basiclist) in digital form
-- of first degree d on a monomial m in lieRing of degree e.
-- The result is a polynomial in lieRing which is a linear
-- combination of basis elements in bas#(d+e) if the degree d+e is
-- computed. Otherwise, the output is a linear combination of prebasis 
-- elements and this fact is used in computeLie.
 
op=(x,m)->(
    if LL.cache.opL#?(x,m) then LL.cache.opL#(x,m) else ( 
	if m===mb0 then ifed(x) else ( 
	    if m == 0 then 0_(LL.cache.lieRing) else (
		d:=ideg m; 
		le:=#x; 
            	d0:= ((LL.genWeights)_(x#0))_0; 
            	if le==1 then ( 
		    if (gens LL.cache.gr#(d+d0)) == 0 then 
                    LL.cache.lieRing_(d*LL.numGen+x#0)*m else 
                    LL.cache.lieRing_(d*LL.numGen+x#0)*m % LL.cache.gr#(d+d0)
              	    ) else (
		    fir:=first x;
		    firr:=take(x,{0,0}); 
		    secd:=drop(x,1); 
		    ds:=(degiterlie(secd))_0;
                    if  (gens LL.cache.gr#(d+ds+d0)) == 0 then ( 
			(LL.cache.lieRing_((d+ds)*LL.numGen+fir)*op(secd,m)) -
                   	eps(firr,secd)*linext(i-> op(secd,i), 
                  	    LL.cache.lieRing_(d*LL.numGen+fir)*m %LL.cache.gr#(d+d0))
                      	) else ( 
			LL.cache.opL#(x,m)= 
		      ((LL.cache.lieRing_((d+ds)*LL.numGen+fir)*op(secd,m)) -
                            eps(firr,secd)*linext(i-> op(secd,i), 
                            	LL.cache.lieRing_(d*LL.numGen+fir)*
                            	m %LL.cache.gr#(d+d0)))%LL.cache.gr#(d+ds+d0)
		    	)
               	    )
	       	)
            )
      	)
    );


----------------------------------------    
-- opiter
---------------------------------------- 
-- x is a product of cycles of numbered generators
-- y is a BasicList (or List) of numbered generators    
-- output is the permuted BasicList (or List) of numbered generators
opiter=(x,y)->(apply(y,z->opperm(x,z)));


----------------------------------------    
-- opperm    	    
----------------------------------------  
-- opperm({{a,c},{b,d},{c,e},{d,f}},e)=a
-- x is a product of cycles of elements, y is an element
opperm=(x,y)->fold(append(x,y),evalperm);


---------------------------------------
-- outder
---------------------------------------
-- this is used in peekLie

outder = (d,x) -> (
       if length x==0 then "" else if length x==1 then 
       net(x_0)|" => "|net(d(x_0)) else
       net(x_0)|" => "|net(d(x_0))||outder(d,drop(x,1))
       );



---------------------------------------
-- outmon, outmonrec
---------------------------------------
-- outmon and outputrec are used in net, which is
-- given in lieAlgebra

--outmon=y-> outputrec(1 y); 
           
outmon=y->  
            if #y==1 then toString baseName LL.gensLie_(y#0) else 
              "("|outmon take(y,1)|" "|(z:=drop(y,1); if #z==1 then outmon z|")" else 
              substring(outmon z,1));
	  	  
---------------------------------------
-- outputrec
---------------------------------------
-- if the field is QQ, negative coefficients co are written - |co|, else
-- the coefficients are written + (co)

outputrec= r->if r#0===empty then "" else if LL.field===QQ then 
     (if (r#0)#0==1 then " + "|outmon (r#1)#0|outputrec apply(r,u->drop(u,1)) else
     if (r#0)#0==-1 then " - "|outmon (r#1)#0|outputrec apply(r,u->drop(u,1)) else 
     if denominator (r#0)#0===1 then 
     (if (r#0)#0>0 then 
     " + "|toString(r#0)#0|" "|outmon (r#1)#0|outputrec apply(r,u->drop(u,1)) else 
     " - "|toString abs((r#0)#0)|" "|outmon (r#1)#0|outputrec apply(r,u->drop(u,1))) else
     (if (r#0)#0>0 then 
     " + "|"("|toString(r#0)#0|")"|outmon (r#1)#0|outputrec apply(r,u->drop(u,1)) else 
     " - "|"("|toString abs((r#0)#0)|")"|outmon (r#1)#0|outputrec apply(r,u->drop(u,1)))
     ) else
     (if (r#0)#0==1 then " + "|outmon (r#1)#0|outputrec apply(r,u->drop(u,1)) else
     if (r#0)#0==-1 then " - "|outmon (r#1)#0|outputrec apply(r,u->drop(u,1)) else  
     " + "|"("|toString ((r#0)#0)|")"|outmon (r#1)#0|outputrec apply(r,u->drop(u,1)));
 
 

----------------------------------------    
-- rel
----------------------------------------
-- gives the relations of first degree d

rel=(d)->(select(LL.relsLie,y->((degRel(y))_0===d)));


----------------------------------------    
-- relcomm
----------------------------------------
-- the commutative law on Lie elements in a monomial relation, 
-- as a BasicList, used in computeLie

relcomm=(x)->(
    if #x<=1 then 0_(LL.cache.lieRing) else (
	x1:=first x;
	xx1:=take(x,{0,0}); 
	x2:= drop(x,1);
	ifed(x) + eps(xx1,x2)*op(x2,LL.cache.lieRing_(x1))
	)
    );


----------------------------------------    
-- relDegMax
----------------------------------------
-- the maximal degree of the relations 

relDegMax=()->if LL.relsLie==={} then 0 else max apply(LL.relsLie,y->(degRel(y))_0);


----------------------------------------
-- setgen
----------------------------------------
-- this sets the value of the generator symbols,
-- it is used in useLie


setgen=()->for i from 0 to LL.numGen-1 do 
               (LL.cache.genslie)_i<-(LL.gensLie)_i; 


----------------------------------------
-- signtest
----------------------------------------
-- testing sign homogeneity, input is a LieElement

signtest=x->length unique toList apply(x#1,isign)<=1;



----------------------------------------    
-- skipz
----------------------------------------
-- removes any zeroes in a list

skipz=x->if length(x)==0 then {} else (
    if x_0==0 then skipz drop(x,1) else prepend (x_0,skipz drop(x,1))
    );

----------------------------------------    
-- skipzz
----------------------------------------
-- removes zeroes in a list of Lie elements 

skipzz=x->if length(x)==0 then {} else (
    if x_0===LL.zz then skipzz drop(x,1) else prepend (x_0,skipzz drop(x,1))
    );



----------------------------------------    
-- skipZZ
----------------------------------------
-- removes O_ZZ in a list

skipZZ=x->if length(x)==0 then {} else (
    if x_0===0 then skipZZ drop(x,1) else prepend (x_0,skipZZ drop(x,1))
    );

summ=method()
summ(BasicList,Function):= (x,f)->
        if #x==1 then f(x#0) else f(x#0) + summ(drop(x,1),f);

summ(BasicList,BasicList,Function):= (x,y,f)->
   if #x==1 then f((x#0),(y#0)) else f((x#0),(y#0)) + summ(drop(x,1),drop(y,1),f);
   
summplus = (x,y,f)->
   if #x==1 then f((x#0),(y#0)) else f((x#0),(y#0)) ++ summplus(drop(x,1),drop(y,1),f);
     

     

---------------------------------------
-- weighttest
---------------------------------------
weighttest=x->length unique toList apply(x#1,intweight)<=1;




--Documentation
beginDocumentation()
load "./GradedLieAlgebras/doc.m2"
load "./GradedLieAlgebras/doc2.m2"
load "./GradedLieAlgebras/tut.m2"
load "./GradedLieAlgebras/tut2.m2"
load "./GradedLieAlgebras/cons.m2"
load "./GradedLieAlgebras/symm.m2"
load "./GradedLieAlgebras/diff.m2"

--Asserts
load "./GradedLieAlgebras/asserts.m2"



end
    
L=lieAlgebra({a,b})/{a a a b,(a b) a a+a b a b}


L1=lieAlgebra({a,b,c},genWeights => {1,1,2}, genSigns=>{0,0,0})
Q1=L1/{b c-a c,a a c-2 b b b a}


L2=lieAlgebra({a,b,c,d,e},genSigns=>1,field=>ZZ/5)
Q2=L2/{a a,a b,a c,
 b c-a d,c c+2 b d,
c c+2 a e,c d-b e,c e,d e,e e}

L3=lieAlgebra({a,b,c,r3,r4},genWeights => 
    {{1,0},{1,0},{2,0},{3,1},{4,1}},
    genSigns=>{1,1,0,0,1},diffl=>true)

L4=lieAlgebra({a,b,c},
  genWeights => {2,2,4}, genSigns=>{1,1,0})
Q4=L4/{b c-a c,a a c-2 b b b a}
  
L5=lieAlgebra({a,b})
  Q5=L5/{b b b a,a a a b}

f=mapLie(Q5,Q1)

		  
L6=lieAlgebra({a},genSigns=>1)
Q6=L6/{a a}

L7=lieAlgebra({x,y,z,a,b,c},genWeights=>{2,2,2,1,1,1})
Q7=L7/{x+y+z+a b,x-y+z+a c,x+z+b c,a x+b y+c z}

L12=lieAlgebra({a,b,c,r3,r4},
	genWeights => {{1,0},{1,0},{2,0},{3,1},{4,1}},
    genSigns=>{1,1,0,0,1},diffl=>true)
diffLieAlgebra{L12.zz,L12.zz,L12.zz,
	b c-a c,a a c-2 b b b a}
Q12=L12/{b c-a c,a b,b r4-a r4}

L15=lieAlgebra({x,y},genSigns=>1,genWeights=>1)
Q15=L15/{x x,y y}



L16=lieAlgebra({e12,e23,e34,e45,e13,e24,e35,e14,e25,e15},
    genWeights=>{1,1,1,1,2,2,2,3,3,4})
rels={e12 e34,e12 e45,e23 e45,e12 e13,e12 e35,e12 e14,
	e12 e15,e23 e45,e23 e13,e23 e24,e23 e14,e23 e25,
	e23 e15,e34 e24,e34 e35,e34 e14,e34 e25,e34 e15,
	e45 e13,e45 e35,e45 e25,e45 e15,e13 e24,e13 e14,
	e13 e25,e13 e15,e24 e35,e24 e14,e24 e25,e24 e15,
	e35 e14,e35 e25,e35 e15,e14 e25,e14 e15,e25 e15,
	e12 e23-e13,e12 e24-e14,
    e12 e25-e15,e13 e34-e14,
    e13 e35-e15,e14 e45-e15,
    e23 e34-e24,e23 e35-e25,
    e24 e45-e25,e34 e45-e35}
Q16=L16/rels

L122=lieAlgebra({a,b,c,r3,r4,r42},
	genWeights => 
    {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
    genSigns=>{0,0,0,1,1,0},diffl=>true)
diffLieAlgebra{L122.zz,L122.zz,L122.zz,
    b c-2 a c,a a c-2 b b b a,r4+a r3}
Q122=L122/{b c-a c,a b,b r4-a r4}

F=frac(ZZ/7[x])

    
L2 = lieAlgebra({a,b,c},genWeights=>{{1,-1},{1,4},{2,3}},
         genSigns=>{0,1,1})
     
L3=lieAlgebra({a,b,c},
          genWeights=>{1,1,2},genSigns=>{1,0,0})
      relationsLie{-c+a*a,(a*b)*a}
      
L4 = lieAlgebra({a,b,c},genWeights=>{{1,0},{2,1},{3,2}},
          genSigns=>{1,1,1},diffl=>true)
      diffLieAlgebra{L4.zz,L4.zz,a b}
      
L=lieAlgebra({a,b,c},genSigns=>1,
         genWeights=>{{1,0},{1,0},{2,1}},diffl=>true)/{a a,b b}
     diffLieAlgebra{L.zz,L.zz,a b}
     
L1=lieAlgebra({a,b},genSigns=>{0,1},genWeights=>{{2,0},{2,1}},diffl=>true)
          diffLieAlgebra{L1.zz,a}
	  
L2=lieAlgebra({a,b},genSigns=>{0,1},genWeights=>{{2,0},{2,1}},
    field=>ZZ/2,diffl=>true)
          diffLieAlgebra{L2.zz,a}
	  
L3=lieAlgebra({a,b},genSigns=>{0,1},genWeights=>{{2,0},{2,1}},
    field=>ZZ/3,diffl=>true)
          diffLieAlgebra{L3.zz,a}
	  
L = lieAlgebra({a,b,c},genSigns=>{1,0,1},
     genWeights=>{{1,0},{1,0},{1,2}})/{c a}
      
d=defLie(mb_{4,5}+2*mb_{4,6})

L4 = lieAlgebra({a,b,c},genWeights=>{{1,0},{2,1},{3,2}},
          genSigns=>{1,1,1},diffl=>true)
      diffLieAlgebra{L4.zz,a a,a b}
      
Q4=L4/{b b+4 a c}

homologyBasisLie(5,3)

L=lieAlgebra({a,b,c2,c3,c4},genSigns=>{0,0,1,0,1},
    genWeights=>{{1,0},{1,0},{2,1},{3,2},{5,3}},diffl=>true)
diffLieAlgebra{L.zz,L.zz,a b,a c2,a b c3}

L=lieAlgebra({a,b,c,r3,r4,r42},
         genWeights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
         genSigns=>{0,0,0,1,1,0},diffl=>true)
L.genDiffs={L.zz,L.zz,L.zz,a c,a a c,r4 - a r3}
Q=L/{b c - a c,a b,b r4 - a r4}

Q=holonomyLie{{a1,a2,a3},{a1,a4,a5},{a2,a4,a6},{a3,a5,a6}}











    
  








   




 





















