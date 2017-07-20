
--------------------------------------------------------------------------------
-- Copyright 2017  Clas L\"ofwall and Samuel Lundqvist
-- 
-- You may redistribute this program under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 2 of the
-- License, or any later version.
--------------------------------------------------------------------------------

-- "

newPackage(
	"GradedLieAlgebras",
	Version => "1.0",
	Date => "March 2017",
	Authors => {{		  Name => "Clas Löfwall",
		  Email => "clas.lofwall@gmail.com"},
	      {		  Name => "Samuel Lundqvist",
		  Email => "samuel@math.su.se"} 
	      },
	AuxiliaryFiles => true,
        DebuggingMode => false,
	Headline => "computations in graded Lie algebras"
   	)
 
export {
    "annLie",
    "holonomyLie",
    "axiomsLie",          
    "basicExpressionLie",
    "basicMonomialLie",
    "basisExtLie",
    "basisLie", 
    "boundariesBasisLie", 
    "centreLie", 
    "characterLie", 
    "compdeg",    
    "computeLie",     
    "decompidealLie",
    "deglength",     
    "degLie",
    "DerLie", 
    "derLie",
    "defLie", 
    "diffLie",
    "dimLie", 
    "dimsLie", 
    "dimTableLie",
    "dimtotLie",
    "divisorLie",
    "eulerLie", 
    "evalDerLie",     
    "evalDiffLie",
    "evalMapLie",
    "extAlgLie",      
    "extAlgMultLie", 
    "extAlgRing",     
    "field",     
    "genDiffs",     
    "generalExpressionLie",
    "gensLie", 
    "genSigns",
    "genWeights", 
    "homologyBasisLie",
    "homologyLie", 
    "indexFormLie",
    "idealBasisLie",
    "idealLie",
    "imageLie",
    "imageBasisLie", 
    "intersectionLie",     
    "invImageLie", 
    "invImageBasisLie",
    "kernelBasisLie", 
    "kernelLie",
    "koszulDualLie", 
    "LieAlgebra", 
    "lieAlgebra",
    "lieRing",
    "localLie",  
    "MapLie",
    "mapLie",
    "maplie",
    "maxDeg",
    "mbRing",
    "minmodel",
    "minmodelLie", 
    "minPresLie",
    "modelmap",
    "monomialLie",   
    "multDerLie",
    "multLie", 
    "multListLie",
    "multOnly",   
    "normalFormLie",
    "numGen",   
    "permopLie",
    "randomLie", 
    "relsLie", 
    "signDer", 
    "signExtLie",
    "signLie", 
    "sourceLie", 
    "subalgBasisLie", 
    "subalgLie", 
    "symmCyclePermLie", 
    "symmPermLie",     
    "targetLie",
    "toMonomialLie",
    "useLie", 
    "weightDer",  
    "weightLie", 
    "whichLie"
  }

recursionLimit=10000;

----------------------------------------
--
--TYPES AND CONSTRUCTORS
--
----------------------------------------


LieAlgebra = new Type of MutableHashTable
MapLie = new Type of MutableHashTable
DerLie = new Type of MutableHashTable

globalAssignment LieAlgebra
globalAssignment MapLie
globalAssignment DerLie

local LL;

----------------------------------------
-- derLie
----------------------------------------
-- defines a derivation d from M to L, where L is a M-module,
-- by means of f, that is, 
-- d([a,b]) = [d(a),f(b)]+(-1)^(d*a)[f(a),d(b)]. See also
-- mapLie and evalDerLie.

derLie = method(TypicalValue=>DerLie)
derLie(MapLie,List) := (f,y)->(
    N:=LL;
    L:=f.targetLie; 
    M:=f.sourceLie;
    d:=new MutableHashTable;  
    d.sourceLie=M; 
    d.targetLie=L; d.maplie=f; ok:=true; 
    if not M.numGen==length y then (ok=false;
	print(y);print(" has not the right length")) else (
    	if all(y,z->z==[]) then ( 
	    d.weightDer=flatten table(1,L.deglength,x->0);
	    if d.weightDer=={} then d.weightDer={0};
      	    d.signDer=0) 
	else (
     	    i:=position(y,z->not z==[]);
	    LL=L;
     	    d.weightDer=weightLie(y_i)-(M.genWeights)_i;
     	    d.signDer=(signLie(y_i)-(M.genSigns)_i)%2
    	    ));
    LL=M; 
    n:=(d.weightDer)_0+relDegMax(); 
    if L.compdeg<n then (
	LL=L;computeLie n);  
    LL=L;
    apply(y, x->if not generalExpressionLie x then (
		    ok=false;
	      	    print(x);
	      	    print("is not a general Lie expression")));
    apply(length y,i->(if not y_i==[] then ( 
		if not d.weightDer==weightLie(y_i)-(M.genWeights)_i then (
		    ok=false;	 
	      	    print(y_i);    
	      	    print(" has not the right weight"));
          	if not d.signDer==(signLie(y_i)-(M.genSigns)_i)%2 then (
		    ok=false; 
	       	    print(y_i);
	       	    print(" has not the right sign")))));
    if ok then (
	apply(M.numGen,i->d#((M.gensLie)_i)=y_i); 
	e:=new DerLie from d; 
	ok=checkder e
	);
    if ok then (
	LL=N; 
	e)
    );

----------------------------------------
-- lieAlgebra
----------------------------------------
-- it is required that the last part of the weight is the homological
-- degree, which should be >= 0 and less than the first degree. 
-- The differential reduces the homological
-- degree by 1 (it also changes the "sign"). The other weights are 
-- left unchanged. If no differential is given a homological degree = 0
-- is added to the weight list and the differential is defined to be 0

lieAlgebra = method(TypicalValue => LieAlgebra,Options => 
    {genWeights => 1, genSigns => 0, field => QQ, genDiffs => {}})
lieAlgebra(List,List) := opts->(gensLie,relsLie)->(
    L := new MutableHashTable; 
    LL=L;
    L.gensLie=gensLie;
    L.genWeights=opts.genWeights;
    L.genSigns=opts.genSigns;
    L.genDiffs=opts.genDiffs;
    L#(symbol cache)=new CacheTable;
    if L.genDiffs=={} then L.cache.diffl=false else L.cache.diffl=true;
    L.relsLie=relsLie;
    L.field=opts.field;
    L.numGen=length L.gensLie;
    L.compdeg=0;
    L.cache.maxDeg=5;
    L.cache.lieRing=L.field[]; 
    L.cache.mbRing=L.field[];
    L.cache#(symbol dims)=new MutableHashTable;
    L.cache#(symbol opL)=new MutableHashTable;
    L.cache#(symbol bas)=new MutableHashTable;
    L.cache#(symbol gr)=new MutableHashTable;
    L.cache#(symbol deglist)=new MutableHashTable;
    if L.gensLie=={} then doZeroLie();
    L.cache.bas#0={mb0}; 
    L.cache.dims#0=1;
    if L.genSigns===0 then L.genSigns=flatten table(1,L.numGen,x->0);
    if L.genSigns===1 then L.genSigns=flatten table(1,L.numGen,x->1); 
    if not L.cache.diffl or (unique L.genDiffs==={[]} and L.genWeights===1) then ( 
	L.genDiffs=flatten table(1,L.numGen,x->[]);
    	if not L.genWeights==={} then (
	    if L.genWeights===1 then ( 
	    	L.genWeights=flatten table(1,L.numGen,x->{1,0})
		) else (
		if class (L.genWeights)_0===ZZ then ( 
		    L.genWeights=apply(L.genWeights,x->{x,0})
		    ) else (
		    L.genWeights=apply(L.genWeights,x->append(x,0))
		    )
		)
	    )
	); 
    ok:=true;  
    if L.cache.diffl and not unique L.genDiffs==={[]} and L.genWeights===1 then (
    	L.genWeights=flatten table(1,L.numGen,x->{1,0});
    	);
    if L.cache.diffl and any(L.genWeights,x->class x===ZZ) then (
	ok=false;
        print("there is no homological degree defined")
	);
    if ok then ( 
  	if L.genWeights==={} then L.deglength=0 else L.deglength=length (L.genWeights)_0;
  	if min(apply(L.genWeights,x->x_0))<1 then (
	    ok=false;
            print("the (first) degree of a generator must be at least one")
	);
        if L.cache.diffl and any(L.genWeights,x->x_0<=x_(-1)) then (
	    ok=false;
            print("the homological degree must be less than the (first) degree")
	    );
  	if L.cache.diffl and any(L.genWeights,x->x_(-1)<0) then (
	    ok=false;
            print("the homological degree must be non-negative")
	    );
  	if not L.numGen==length L.genWeights  then (
	    ok=false;
            print("the number of weights must be equal to the number of generators")
	    );
  	if not L.numGen==length L.genSigns then (
	    ok=false;
            print("the number of signs must be equal to the number of generators")
	    );
  	if not L.numGen==length L.genDiffs then (
	    ok=false;
            print("the number of differentials must be equal to the number of generators")
	    );
  	if length unique apply(L.genWeights,length)>1 then (
	    ok=false;
            print("all weights must have the same length")
	    );
  	if not all(L.genSigns,x->x===0 or x===1) then (
	    ok=false;
            print("all signs must be 0 or 1")
	    );
 	);
    if ok then (
     	L.cache.lieRing=lieR();
     	apply(
       	    join(relsLie,L.genDiffs), 
            x->if not generalExpressionLie x then (
		ok=false;
		print(x);
		print("is not a general Lie expression"))
	    );
     	apply(L.numGen,
             i->( x:=isubstrel((L.genDiffs)_i); 
	          if not x==[] then ( 
		      if class x===Array then (
			  w:=degiterlie(x); 
			  s:=sign(x)
			  ) else (
			  w=degiterlie((x_1)_0); 
			  s=sign((x_1)_0)
			  ); 
		      unit:=append(flatten table(1,L.deglength -1,x->0),1);
		      if not (L.genWeights)_i-w==unit then ( 
			  ok=false;
	                  print((L.genDiffs)_i);
	                  print(" has not the right weight")
		       	  );
		      if (L.genSigns)_i==s then ( 
			  ok=false;
	                  print((L.genDiffs)_i);
	                  print(" has not the right sign")
  		         )
	             )
	         )
            )
       );
   if ok then ( 
       M:=new LieAlgebra from L;LL=M; if M.cache.diffl then ( 
	   n:=max(apply(join(L.genDiffs,relsLie),degLie));
	   computeLie n; 
	   ok=checkdiff()
	   ) 
        );
    if ok then M
    );


----------------------------------------
-- mapLie
----------------------------------------
-- The value f is a MutableHashTable defined on M.gensLie, y is a list of 
-- elements in L of degree <= n, written in the same way as relations,
-- and these are the values of f on the generators in M. Also [] is allowed.
-- The values are checked to have the same multidegree as the corresponding
-- generators in M, and the same sign.
 
 mapLie = method(TypicalValue=>MapLie)
 mapLie(LieAlgebra,LieAlgebra,List) := (L,M,y)->(
     N:=LL;
     f:=new MutableHashTable;  
     f.sourceLie=M; 
     f.targetLie=L; 
     if not M.field===L.field then (
       	 print("The Lie algebras must be defined over the same field")
       	 ) else ( 
       	 ok:=true; 
       	 LL=M; 
       	 n:=max(0,max(apply(join(M.gensLie,M.genDiffs,M.relsLie),degLie))); 
       	 LL=L;
       	 computeLie n; 
       	 if not M.numGen==length y then (
	     ok=false;
	     print(y);
	     print(" has not the right length")
	     ) else (
    	     apply(y, 
	       	 x-> if not generalExpressionLie x then (
	    	     ok=false;
	    	     print(x);
	    	     print("is not a general Lie expression")
		     )
	       	 );	  
    	     apply(length y,
	       	 i->if not y_i==[] and not weightLie(y_i)==(M.genWeights)_i then (
	    	     ok=false;
	    	     print(y_i);
	    	     print(" has not the right weight")
	    	     )
	       	 );
    	     apply(length y,
	       	 i->if not y_i==[] and not signLie(y_i)==(M.genSigns)_i then (
	    	     ok=false;
	    	     print(y_i);
	    	     print(" has not the right sign")
		     )
	       	 )
    	     ); 
       	 if ok then (
	     apply(M.numGen, i->f#((M.gensLie)_i)=y_i); 
	     g:=new MapLie from f; 
	     ok=checkmap g
	     ); 
       	 if ok then (
	     LL=N;
	     g
	     )
       	 )
     );


----------------------------------------
--
-- EXPORTED FUNCTIONS
--
----------------------------------------

----------------------------------------
--annLie
----------------------------------------

annLie=method(TypicalValue=>List)
annLie(ZZ,ZZ,List) := (s,d,p)->(
    computeLie(d+s);       
    if p=={} or unique p=={[]} then basisLie(d) else (
    	if not all(p,y->generalExpressionLie y) or not unique degLie p == {s} then (
	    print("the input is not correct") 
	    ) else (	   
       	    idefLie idivisorLie(d+s,s,{0_(LL.cache.lieRing)}, apply(p,x->idefinvLie x))
       	    )    
   	) 
    );




----------------------------------------
-- basicExpressionLie
----------------------------------------

basicExpressionLie = x -> (
    x===[] or basicMonomialLie x or (
	class x === List and 
	generalExpressionLie x and 
	all(x_1,basicMonomialLie) and
	all(x_0,y->not y==0) and 
	not (length x_0==1 and (x_0)_0==1_(LL.field))
	)	 
    );


----------------------------------------
-- basicMonomialLie
----------------------------------------	
basicMonomialLie = x -> monomialLie x and not x===[] and member(x,basisLie degLie x);


----------------------------------------
--basisLie   
----------------------------------------
   
basisLie=method(TypicalValue=>List)
basisLie(ZZ):=(n)->(
    computeLie n;
    idefLie ibasisLie n
    );
basisLie(ZZ,ZZ):=(n,d)->(
    computeLie n;
    select(basisLie(n),x->(weightLie x)_(-1)==d)
    );
basisLie(List):=(x)->(
    computeLie x_0;
    select(basisLie(x_0),y->weightLie y==x)
    ); 
 
 
----------------------------------------
--basisExtLie   
----------------------------------------

basisExtLie=method(TypicalValue=>List)
basisExtLie(ZZ):=(n)->(
    minmodelLie n;
	select(gens LL.cache.extAlgRing,x->idegLie(x)==n)
    );
basisExtLie(ZZ,ZZ):=(n,d)->(
    minmodelLie n;
	select(basisExtLie(n),x->last degree x==d)
    );
basisExtLie(List):=(x)->(
   minmodelLie x_0;
	select(basisExtLie(x_0),y->degree y==x)
    ); 


----------------------------------------
--boundariesBasisLie
----------------------------------------

boundariesBasisLie = method(TypicalValue=>List)
boundariesBasisLie(ZZ,ZZ) := (n,d)->(
    computeLie n;
    idefLie iboundariesLie(n,d)
    );


----------------------------------------
--centreLie
----------------------------------------
-- gives all the central elements in degree d
centreLie = method()
centreLie(ZZ) := (d)->(
    computeLie(d+genDegMax());
    if LL.cache.dims#d==0 then {} else (
	intersectionLie(d,(for i from 1 to genDegMax() list 
       		annLie(i,d,idefLie (if LL.cache.gr#i==0 then 
	       		listdeg(i,allgens()) else 
	       		apply(listdeg(i,allgens()),x->x%(LL.cache.gr#i))))))
	)
    );
-- gives all the central elements up to the computed degree minus gendegmax
centreLie(LieAlgebra):=(L)->(
    N:=LL;
    LL=L;
    out:=flatten (n:=1; while LL.cache.dims#?(n+genDegMax()) list centreLie(n) do n=n+1);
    LL=N;	 
    out
    );

     
----------------------------------------
--characterLie
----------------------------------------
characterLie=method(TypicalValue=>RingElement) 
characterLie(ZZ,List,List):=(d,x,y)->(
    if not all(y,z->generalExpressionLie z and degLie z==d) then (
    	print("the third input is not correct")
    	) else (
    	if not length subalgBasisLie(d,y)==length y then print("the third input is not a basis") else (
	    trace solve(basToMat(d,apply(y,z->idefinvLie z)),basToMat(d,apply(y,z->ipermopLie(x,z))))
	    )
    	)
    );
 
      
----------------------------------------
--computeLie   
----------------------------------------      

computeLie=method(TypicalValue=>List)
computeLie(ZZ):=(d)->(
    if LL.compdeg>=d then idimsLie(d) else if LL.gensLie=={} then (
        if LL.cache.maxDeg<d then  LL.cache.maxDeg=d+5;
        LL.compdeg=d; 
	doZeroLie(); 
	idimsLie(d)
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
		   apply(gendeg(j),x->op([x],y)));
           for j from 1 to min(i,relDegMax()) do 
	       rellist=rellist|flatten apply(LL.cache.bas#(i-j),m->
		   apply(rel(j),x->
		       (if class x===Array then op(x,m) else sum(x_0,x_1,(i,j)->i*op(j,m)))));
	   newrellist:=flatten apply(rel(i),y->
	       (if class y===Array then relcomm(y) else apply(y_1,z->relcomm(z))));
    	   rellist=rellist|newrellist;
	   ch:=char LL.field;
           badgen:=select(LL.numGen,y->(LL.genWeights_y)_0%ch==0 and (LL.genWeights_y)_0<i);
           newrellist=flatten apply(badgen,x->
	       apply(LL.cache.bas#(i-(LL.genWeights_x)_0),m->commrelgen(x,m)));
           rellist=rellist|newrellist;
    	   if ch==3 and i%3==0 then rellist=rellist|apply(select(LL.cache.bas#(i//3),z->
		   isignLie z==1),x->imult(x,imult(x,x)));
    	   if ch==2 and i%2==0 then rellist=rellist|apply(LL.cache.bas#(i//2),x->imult(x,x));
	   deglimit:=prepend(i,flatten table(1,LL.deglength-1,x->0));
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
		   LL.cache.gr#(i)=ideal(flatten entries gens gb(newide,DegreeLimit=>deglimit));
                   grlead=apply(flatten entries gens LL.cache.gr#(i),leadMonomial);
               	   LL.cache.bas#(i)=select(bas0,x->(not member(x,grlead)));
		   LL.cache.deglist#(i)=apply(LL.cache.bas#(i),degree)
	      	   )
	       );
       	   LL.cache.dims#i=length (LL.cache.bas#(i)); -- next i
   	   ); -- end of for loop
       LL.cache.mbRing=modR(d);LL.compdeg=d; 
       idimsLie(d)
       )
   );


----------------------------------------
--decompidealLie
----------------------------------------
--the kernel of the surjective Lie homomorphism from L'=[L,L]
--to the direct sum of L_i', where L_i are the local Lie
--algebras. The ideal is generated by the elements in degree 3,
--which are linear combinations of [x,y,z], where not all x,y,z
--belongs to the same L_i.

decompidealLie=method(TypicalValue=>List)
decompidealLie(ZZ) := (d)->(
    computeLie d;
    loc:=join apply(toSequence LL.cache.locals,y->isubalgLie(3,apply(y,z->genbas(z))));
    genide:=select(ibasisLie(3),y->not member(y,loc));
    idefLie iideal(d,genide)
    );


----------------------------------------
-- defLie
----------------------------------------

defLie = method()
defLie(List) := (p)->apply(p,x->defLie(x));
defLie(RingElement):=(p)->( 
      if not ring p===LL.cache.mbRing then (
	  print("the wrong mbRing is currently used")
	  ) else ( 
	  computeLie(idegLie p); 
	  mons:=flatten entries monomials p; 
	  if mons=={} then [] else (  
	      if p===mons_0 then idefLie(mbToLie p) else (
		  coef:=apply(mons,x->(p_x));
		  out:={coef,apply(mons,x->idefLie(mbToLie x))};
    		  if generalExpressionLie out then out
		  )
              )    
      	  )
      );


----------------------------------------
-- degLie
----------------------------------------

degLie = method(TypicalValue=>ZZ)
degLie(Symbol) := degLie (IndexedVariable):= degLie(ZZ):=(g)->(
    if not member(g,LL.gensLie) then (
	print(g);
	print(" is not a generator")
	) else (
	po:=position(LL.gensLie,x->x===g);
	((LL.genWeights)_po)_0)
    );	   
degLie(Array):=(x)->sum(x,degLie);	  
degLie(List):=(x)->if generalExpressionLie x then degLie((x_1)_0) else degList x;


----------------------------------------
--derLie
----------------------------------------

derLie(List):=(y)->derLie(idMapLie(),y);

  
----------------------------------------
--diffLie
----------------------------------------
  
diffLie=()->iderLie(LL.genDiffs);
  

----------------------------------------
--dimLie   
----------------------------------------
 
dimLie=method(TypicalValue=>ZZ)
dimLie(ZZ):=(d)->(
    computeLie d;
    LL.cache.dims#d
    );
dimLie(ZZ,ZZ):=(n,d)->(
    computeLie n;
    length ibasisLie(n,d)
    );
dimLie(List):=(x)->(computeLie x_0;
    length ibasisLie(x)
    );

 
 ----------------------------------------
--dimsLie   
----------------------------------------
-- gives the list of dimensions of the Lie algebra when the first
-- degree ranges from 1 to d

dimsLie=method(TypicalValue=>List)
dimsLie(ZZ):=(d)->(
    computeLie d;
    for i from 1 to d list LL.cache.dims#i
    );


----------------------------------------
--dimTableLie   
----------------------------------------
-- the matrix of dimensions in first and last degree
-- s=0 gives only dimensions of even elements, s=1 only odd

dimTableLie=method()
dimTableLie(ZZ):=(n)->(
    computeLie n;
    matrix apply(n+1,d->apply(n,j->dimLie(j+1,d)))
    );
dimTableLie(ZZ,ZZ):=(n,s)->(
    computeLie n;
    matrix apply(n+1,d->apply(n,j->length select(basisLie(j+1,d),y->signLie y==s)))
    );
        
	
----------------------------------------
--dimtotLie   
----------------------------------------
	    
dimtotLie=method(TypicalValue=>ZZ)
dimtotLie(ZZ):=(d)->(
    computeLie d;
    sum dimsLie(d)
    );


----------------------------------------
--divisorLie
----------------------------------------

divisorLie=method(TypicalValue=>List)
divisorLie(ZZ,ZZ,List,List) := (t,s,m,p)->(
    computeLie t; 
    d:=t-s; 
    if m=={} then m={[]};
    if not all(m,y->generalExpressionLie y) or not unique degLie m == {t} then (
	print("the third input is not correct")
	) else (
	if not all(p,y->generalExpressionLie y) or not unique degLie p == {s} then (
	    print("the fourth input is not correct")
	    ) else (
	    if LL.cache.dims#t==0  or p=={} or unique p=={[]} then basisLie(d) else (        
		mm:= apply(m,x->idefinvLie x); 
		pp:=apply(p,x->idefinvLie x);
		B:=transpose basToMat(t,mm);
		kerB:=transpose generators kernel(B);
		idefLie matToBas(d,generators kernel joinvert apply(pp,y->
			(kerB*basToMat(t,apply(ibasisLie(d),x->imult(y,x))))))
		)
	    )
	)
    );

  
----------------------------------------
--eulerLie
----------------------------------------

eulerLie=method(TypicalValue=>List)
eulerLie(ZZ):=(n)->(
    computeLie n;
    for i from 1 to n list ieuler(i)
    );


----------------------------------------
--evalDerLie
----------------------------------------

evalDerLie=method()
evalDerLie(DerLie,Array):=(d,x)->if d#?x then d#x else (
    N:=LL;
    LL=d.sourceLie; 
    n:=(d.weightDer)_0 + degLie x; 
    LL=d.targetLie; 
    computeLie n;
    out:=idefLie ievalDerLie(d,x); 
    LL=N; 
    d#x=out
    );
evalDerLie(DerLie,List):=(d,x)->(
    N:=LL;
    LL=d.sourceLie;
    n:=(d.weightDer)_0 + degLie x;
    LL=d.targetLie; 
    computeLie n;
    out:=idefLie ievalDerLie(d,x);
    LL=N;
    out
    );



----------------------------------------
--evalDiffLie
----------------------------------------
  
evalDiffLie=method()
evalDiffLie(Array):=evalDiffLie(List):=x->(
    n:=degLie x; 
    computeLie n;
    idefLie idiffLie x
    );
 
 
----------------------------------------
--evalMapLie
----------------------------------------

evalMapLie=method()
evalMapLie(MapLie,Array):=(f,x)->if f#?x then f#x else (
    N:=LL;
    LL=f.sourceLie; 
    n:=degLie x; 
    LL=f.targetLie; 
    computeLie n;
    out:=idefLie ievalMapLie(f,x); 
    LL=N; 
    f#x=out
    );
evalMapLie(MapLie,List):=(f,x)->(
    N:=LL;
    LL=f.sourceLie; 
    n:=degLie x; 
    LL=f.targetLie;
    computeLie n;
    out:=idefLie ievalMapLie(f,x);
    LL=N;
    out
    );


----------------------------------------
--extAlgLie
----------------------------------------

extAlgLie = method(TypicalValue=>Matrix)
extAlgLie(ZZ) := (n)->(
    M:=minmodelLie(n);
    matrix toList apply(
	1..n,d->toList apply(
	    1..n,j->length select(M.numGen,y->
		((M.genWeights_y)_0===j and (M.genWeights_y)_(-1)===d-1))))
    );


----------------------------------------
--extAlgMultLie
----------------------------------------

extAlgMultLie = method(TypicalValue=>RingElement)
extAlgMultLie(RingElement, RingElement) := (a,b)->(
    linext((x,y)->extmonmult(x,y),a,b)
    );
		 

----------------------------------------
-- generalExpressionLie
----------------------------------------
 
 generalExpressionLie = (x)-> (
     monomialLie x or 
     class x===List and 
     length x==2 and 
     class x_0===List and 
     class x_1===List and
     length x_0==length x_1 and
     not x_0=={} and
     all(x_1,y->monomialLie y and not y==[]) and
     all(x_0,y->y==0 or class(1/(1/y))===LL.field or class y===ZZ or 
	 class y===ZZ/char LL.field or (
	     class y===QQ and not mod(denominator y,char LL.field)==0
	     )
	 ) and (
	 ok:=true; 
	 xx:=isubstrel(x); 
	 if class xx===List then (
	     d:=unique apply(xx_1,degiterlie);
	     s:=unique apply(xx_1,sign);
	     if length d>1 then (
		 ok=false;
		 print("the input is not weight-homogeneous")
		 );
	     if length s>1 then (
		 ok=false;
		 print("the input is not sign-homogeneous")
		 )
	     );
	 ok
	 )
     );        


----------------------------------------
--holonomyLie
----------------------------------------	
holonomyLie=method(TypicalValue=>LieAlgebra,Options=>{field=>QQ})
holonomyLie(List):=opts->(x)->( 
    Generators:=unique join toSequence x; 
    pairslocal:=y->subsets(y,2); 
    allpairs:=join apply(toSequence x,pairslocal);
    if not (unique allpairs===allpairs) then (
	print("two 2-flats must have at most one element in common")
	) else (
	liepairs:=apply(select(subsets(Generators,2),y->not member(y,allpairs)),z->new Array from z);
	lieideallocal:=y->(apply(drop(y,1),z-> {apply(length y,u->1),apply(y,v->([z,v]))}));
	L:=ilieAlgebra(5,Generators,join apply(toSequence x,lieideallocal)|liepairs,field=>opts.field);
      	L.cache.locals=x;
      	L
	)
    );

----------------------------------------
--homologyBasisLie
----------------------------------------

homologyBasisLie=method(TypicalValue=>List)
homologyBasisLie(ZZ,ZZ):=(n,j)->(
    computeLie n;
    idefLie ihomologyLie(n,j)
    );


----------------------------------------
--homologyLie
----------------------------------------

homologyLie = method()
homologyLie(ZZ,ZZ) := (n,d)->(
    computeLie n;
    dimLie(n,d)-boundariesDimLie(n,d-1)-boundariesDimLie(n,d)
    );
homologyLie(ZZ) := (n)->(
       computeLie n;
       matrix apply(n,d->apply(n,j->homologyLie(j+1,d)))
       );


----------------------------------------
--idealBasisLie   
----------------------------------------
-- gives a basis in first degree/multidegree n for the lie ideal which is 
-- generated by the elements in the list x of GE-elements
-- (which might contain elements of different degrees).

idealBasisLie=method(TypicalValue=>List)
idealBasisLie(ZZ,List):=(n,x)->(
    computeLie n;
    if all(x,y->generalExpressionLie y) then (
	idefLie iideal(n,apply(x,idefinvLie))
	) else (
	print("the input is not correct")
	)
    );
idealBasisLie(List,List):=(y,x)->(
    select(idealBasisLie(y_0,x),z->weightLie z==y)
    );


----------------------------------------
--idealLie   
----------------------------------------

idealLie=method()
idealLie(ZZ,List):=(n,x)->(
    computeLie n;
    if all(x,y->generalExpressionLie y) then (
	apply(n,i->length iideal(i+1,apply(x,idefinvLie)))
	) else (
	print("the input is not correct")
	)
    );
idealLie(List,List) := (y,x)->(
    computeLie y_0;
    if all(x,y->generalExpressionLie y) then (
	length iideal(y,apply(x,idefinvLie))
	) else (
	print("the input is not correct"))
    );


----------------------------------------
--imageBasisLie
----------------------------------------   

imageBasisLie=method(TypicalValue=>List)	    
imageBasisLie(ZZ,MapLie) := (n,f)->(
    N:=LL;
    LL=f.targetLie;
    out:=subalgBasisLie(n,apply(f.sourceLie.gensLie,x->f#x));
    LL=N;
    out
    );
imageBasisLie(ZZ,ZZ,MapLie) := (n,d,f)->select(imageBasisLie(n,f),x->(weightLie x)_(-1)==d);

----------------------------------------
--imageLie
----------------------------------------

imageLie = (n,f)->matrix apply(n,d->apply(n,j->iimageLie(j+1,d,f)));

----------------------------------------
--invImageBasisLie
---------------------------------------- 

invImageBasisLie = method()  
invImageBasisLie(ZZ,MapLie,List) := (n,f,b)->(
    if b=={} then {} else (
	N:=LL; 
	LL=f.targetLie;  
	if any(b,x->not (weightLie x)_0 == n) then (
	    print("the weights in the list are not correct"); 0)  else (
	    computeLie n; 
	    B:=basToMat(n,apply(b,idefinvLie));
	    LL=f.sourceLie; 
	    computeLie n; 
	    ba:=basisLie(n); 
	    LL=f.targetLie;
	    A:=basToMat(n,apply(apply(ba,x->evalMapLie(f,x)),idefinvLie));
	    C:=invimage(A,B); 
	    LL=f.sourceLie; 
	    out:=idefLie matToBas(n,C); 
	    LL=N; 
	    out
	    )
	)
    );
invImageBasisLie(ZZ,DerLie,List) := (n,f,b)->(
    if b=={} then {} else (  
	N:=LL; 
	LL=f.targetLie;  
	if any(b,x->not (weightLie x)_0 == n) then (
	    print("the weights in the list are not correct"); 0) else (
	    computeLie n;
	    B:=basToMat(n,apply(b,idefinvLie));
	    LL=f.sourceLie;  
	    computeLie n;  
	    ba:=basisLie(n-(f.weightDer)_0); 
	    LL=f.targetLie;
	    A:=basToMat(n,apply(apply(ba,x->evalDerLie(f,x)),idefinvLie));
	    C:=invimage(A,B); 
	    LL=f.sourceLie; 
	    out:=idefLie matToBas(n-(f.weightDer)_0,C); 
	    LL=N; 
	    out
	    )
	)
    );
invImageBasisLie(ZZ,ZZ,MapLie,List) := (n,d,f,b)->(
    if b=={} then {} else ( 
	N:=LL; 
	LL=f.targetLie;  
	if any(b,x->(not (weightLie x)_0 == n or not (weightLie x)_(-1) == d)) then (
	    print("the weights in the list are not correct"); 
	    0
	    ) else (
	    computeLie n;
	    B:=basToMat(n,d,apply(b,idefinvLie));
	    LL=f.sourceLie;  
	    computeLie n; 
	    ba:=basisLie(n,d); 
	    LL=f.targetLie;
	    A:=basToMat(n,d,apply(apply(ba,x->evalMapLie(f,x)),idefinvLie));
	    C:=invimage(A,B); 
	    LL=f.sourceLie; 
	    out:=idefLie matToBas(n,d,C); 
	    LL=N; 
	    out
	    )
	)
    );
invImageBasisLie(ZZ,ZZ,DerLie,List) := (n,d,f,b)->(
    if b=={} then {} else (
	N:=LL; 
	LL=f.targetLie; 
	if any(b,x->(not (weightLie x)_0 == n or not (weightLie x)_(-1) == d)) then (
	    print("the weights in the list are not correct"); 
	    0
	    ) else (
	    computeLie n;
	    B:=basToMat(n,d,apply(b,idefinvLie));
	    LL=f.sourceLie;  
	    computeLie n; 
	    ba:=basisLie(n-(f.weightDer)_0,d-(f.weightDer)_(-1)); 
	    LL=f.targetLie;
	    A:=basToMat(n,d,apply(apply(ba,x->evalDerLie(f,x)),idefinvLie));
	    C:=invimage(A,B); 
	    LL=f.sourceLie; 
	    out:=idefLie matToBas(n-(f.weightDer)_0,d-(f.weightDer)_(-1),C); 
	    LL=N; 
	    out
	    )
	)	
    );


----------------------------------------
--invImageLie
---------------------------------------- 

invImageLie = method()
invImageLie(ZZ,MapLie,List) := invImageLie(ZZ,DerLie,List) := (n,f,b)-> 
        if class invImageBasisLie(n,f,b)===List then length invImageBasisLie(n,f,b);
invImageLie(ZZ,ZZ,MapLie,List) := invImageLie(ZZ,ZZ,DerLie,List) := (n,d,f,b)->
        if class invImageBasisLie(n,d,f,b)===List then length invImageBasisLie(n,d,f,b);


 
----------------------------------------
--indexFormLie   
----------------------------------------
-- is left inverse to "defLie", that is indexFormLie(defLie(x))=x. 
-- It holds that
-- defLie(indexFormLie(x))≈x in the free Lie algebra on 
-- the generators modulo the relations.
-- "indexFormLie" is not "listable", see "indexFormListLie". 
-- Input is a generator or an iterated
-- Lie product, or of the type {{coef}, {lie}}.  

indexFormLie = method(TypicalValue=>RingElement)
indexFormLie(Symbol) := indexFormLie(IndexedVariable) :=
indexFormLie(ZZ) := (x)->(
    if not member(x,LL.gensLie) then (
	print(x);
	print(" is not a generator")
	) else (
	indexFormLie([x])
	)
    );
--elements in ZZ are also valid as generators!                                                    
indexFormLie(Array) := (x)->(
    if x==[] then 0_(LL.cache.mbRing) else (
	computeLie(degLie x);
	lieToMb ifed isubst x
	)
    );
indexFormLie(List) := (x)->(
    if generalExpressionLie x then (
	sum(x_0,x_1,(i,j)->i*indexFormLie(j))
	) else (
	indexFormList x
	)
    );


----------------------------------------
--intersectionLie
----------------------------------------
-- if m is {}, the output will be the whole space basisLie(d)
intersectionLie = method(TypicalValue=>List)
intersectionLie(ZZ,List) := (d,m)->(
    if m=={} then basisLie d else (
    	if any(m,x->x=={}) then {} else (
    	    if not all(flatten m,y->generalExpressionLie y) or 
	       not skipz unique degLie flatten m == {d} then (
    		print("the input is not correct")
    		) else ( computeLie d;
    		idefLie matToBas(d,generators kernel transpose joinhoriz apply(
			apply(m,y->apply(y,z->idefinvLie z)),
			x->(generators kernel transpose basToMat(d,x))))
    		)
	    )
	)
    );


----------------------------------------
--kernelBasisLie
----------------------------------------

kernelBasisLie = method(TypicalValue=>List)	    	
kernelBasisLie(ZZ, MapLie) := (n,f)->(
      N:=LL;
      LL=f.sourceLie;
      ba:=basisLie(n);
      if ba=={} then (LL=N;{}) else (
      	  LL=f.targetLie;
	  computeLie n;
 	  fval:=apply((apply(ba,x->evalMapLie(f,x))),y->idefinvLie y);
      	  fmat:=basToMat(n,fval); 
	  LL=f.sourceLie;
          out:=idefLie matToBas(n,(generators kernel fmat));
	  LL=N;
	  out
	  )
      );
kernelBasisLie(ZZ,ZZ,MapLie) := (n,d,f)->(
      N:=LL;
      LL=f.sourceLie;
      out:=select(kernelBasisLie(n,f),x->(weightLie x)_(-1)==d);
      LL=N;
      out
      );
    

----------------------------------------
--kernelLie
----------------------------------------    
       
kernelLie = (n,f)->matrix apply(n,d->apply(n,j->ikernelLie(j+1,d,f)));


----------------------------------------
--koszulDualLie
----------------------------------------

koszulDualLie=method(TypicalValue=>LieAlgebra)
koszulDualLie(QuotientRing):=koszulDualLie(PolynomialRing):=(Q)->(
    R1:=ambient Q; 
    if Q#?SkewCommutative then S:=Q.SkewCommutative else S={};
    skewco:=apply(S,i->(gens Q)_i);
    signlist:=apply(length gens Q,x->if member(x,S) then 1 else 0);
    R:=coefficientRing Q[gens Q,SkewCommutative=>skewco,Degrees=>apply(gens Q,degree)];
    fie:=coefficientRing R;
    I1:=flatten entries gens ideal Q;
    scalarprod:=(l1,l2)->sum(l1,l2,(i,j)->i*j);
    explist:=(p)->apply(flatten entries monomials p,m->flatten exponents m);
    homogen:=(p)->length unique apply(explist p,y->scalarprod(signlist,y)%2)==1;
    ok:=true;
    apply(I1,p->(if not homogen p then (ok=false;print(p);print("is not sign-homogeneous"));
	         if not isHomogeneous Q then (ok=false;print(p);print("is not multi-homogeneous"))));
    if I1=={} then grI:={} else (
	I:=ideal apply(I1,x->(map(R,R1))(x));    
	grI=flatten entries gens gb(I,DegreeLimit=>flatten table(1,degreeLength R,x->2));
	grI=apply(grI,x->x*(1/(leadCoefficient x)))
	);
    lM:=apply(grI,leadMonomial);
    bM:=select(flatten entries basis(2,R),x->not member(x,lM));
    -- grI_k = lM_k + \sum a_ki bM_i
    tolie:=(m)->(
	po:=positions(flatten exponents m,x->x>0); 
       	if length po==1 then [ko_(po_0),ko_(po_0)] else 
     	if not member(po_0,S) and member(po_1,S) then [ko_(po_1),ko_(po_0)] else
       	[ko_(po_0),ko_(po_1)]);
    sqco:=(m)->(
	po:=positions(flatten exponents m,x->x>0); 
       	if length po==1 then (1/2)_fie else 1_fie);
    coe:=(k,i)->sqco(lM_k)*(map(fie,R))(((apply(coefficients(
	    grI_k,Monomials=>{bM_i}),x->flatten entries x))_1)_0);
-- 1/2a_ki alt a_ki depending on lM_k being a square or not
    co:=(i)->prepend(-sqco(bM_i),apply(length grI,k->coe(k,i)));  
-- {-1 alt -1/2,a_0i,...,a_(r-1)i}
    relmon:=(i)->prepend(tolie(bM_i),apply(lM,tolie));
    redrel:=(x)->(
	out:={skipz x_0,skipZZ apply(1+length grI,
		i->if (x_0)_i==0 then 0 else (x_1)_i)};
	if length (out_0)==1 then (out_1)_0 else out);
    rels:=apply(length bM,i->redrel {co(i),relmon(i)});
-- tolie(bM_i) - \sum a_ki tolie(lM_k)
   if ok then M:=ilieAlgebra(5,apply(numgens R,i->ko_i),rels,genWeights=>apply(gens R,degree),
	genSigns=>apply(signlist,x->(x+1)%2),
	field=>fie);
    if ok then M
  ); 


----------------------------------------
--localLie
----------------------------------------
localLie=method()
localLie(ZZ) := (i)->(
    N:=LL;
    out:=holonomyLie({(LL.cache.locals)_i});
    LL=N;
    out
    );  
localLie(ZZ,ZZ) := (i,n)->(
    N:=LL;
    LL=localLie(i);
    out:=basisLie n;
    LL=N;
    out
    );
    
    
    

         
----------------------------------------
--minmodelLie
----------------------------------------

minmodelLie=method(TypicalValue=>LieAlgebra)
minmodelLie(ZZ):=(d)->(
    L:=LL;
    computeLie d; 
    if L#?minmodel then (M:=L.minmodel; j:=M.compdeg) else
    (M=minmodelone(L.cache.maxDeg); j=1); 
    for n from j+1 to d do M=minmodelstep(n,L.cache.maxDeg,M);
    LL=M; 
    computeLie d; 
    M.modelmap=mapLie(L,M,M.cache.homdefs); 
    M.targetLie = L; 
    L.minmodel = M;
    unit:=append(flatten table(1,M.deglength -1,x->0),1);
    L.cache.extAlgRing = (L.field)[
	ext_0..ext_(length M.gensLie - 1),
	Degrees=>apply(M.genWeights,x->x+unit)];
    LL=L;
    M
    );      


----------------------------------------
--minPresLie
---------------------------------------- 
-- gives a minimal presentation of H_0(L) up to first degree n, 
-- the presentation uses the same names for the generators as in L.

minPresLie=method(TypicalValue=>LieAlgebra)
minPresLie(ZZ):=(n)->(
    L:=LL;
    if LL#?minmodel and LL.minmodel.compdeg>=n then M:=LL.minmodel else M=minmodelLie(n);
    LL=M;
    po:=positions(M.gensLie,x->(weightLie(x))_(-1)==0);
    f:=M.modelmap; 
    g:=(x)->(f#x)_0;
    Mgens:=apply(po,x->(M.gensLie)_x);
    Lgens:=apply(Mgens,g);
    LL=M;
    Mrels:=select(select(M.genDiffs,x->not x==[]),y->(weightLie(y))_(-1)==0);
    Lrels:=apply(Mrels,x->(if class x===Array then apply(x,g) else {x_0,apply(x_1,y->apply(y,g))}));
    MP:=ilieAlgebra(M.cache.maxDeg,Lgens,
	Lrels,
	genSigns=>apply(po,x->(M.genSigns)_x),
	genWeights=>apply(po,x->drop((M.genWeights)_x,-1)),
	field=>M.field);
    LL=L;
    MP
    );


----------------------------------------
-- monomialLie
----------------------------------------	     
monomialLie = x -> (
    class x===Array and all(x,y->member(y,LL.gensLie))
    );


----------------------------------------
--multDerLie
----------------------------------------
-- gives the Lie-multiplication on DerLie (when M=L)

multDerLie=method(TypicalValue=>DerLie)
multDerLie(DerLie,DerLie):=(f,g)->(
    N:=LL;
    LL=f.targetLie; 
    n:=(f.weightDer)_0+(g.weightDer)_0+genDegMax();
    computeLie n;
    out:=iderLie(apply(LL.gensLie,x->idefLie(evalmultDerLie(f,g,[x]))));
    out.weightDer=f.weightDer+g.weightDer;
    out.signDer=(f.signDer+g.signDer)%2;
    LL=N;
    out
    );


----------------------------------------
--multLie   
----------------------------------------

multLie=method()
multLie(Thing,Thing):=(x,y)->(
    n:=degLie x+degLie y; 
    computeLie n;
    ok:=generalExpressionLie x and generalExpressionLie y; 
    if ok then idefLie(imult(idefinvLie x,idefinvLie y)) else 
    print("the input is not correct")
    );

----------------------------------------
--multListLie   
----------------------------------------
-- Lie multiplication of (lists of ) GE-elements.  
-- There is an option "multOnly", which, when it comes to lists,
-- only multiplies those pairs (i,j) such that the condition is
-- true (the condition might for instance be a condition on the degrees).

multListLie=method(Options=>{multOnly=>(i,j)->true})
multListLie(List,List):= o -> (x,y) -> (
    skipZZ flatten apply(x,z->apply(y,u->if o.multOnly(z,u) then multLie(z,u) else 0))
    );


----------------------------------------
-- normalFormLie
----------------------------------------
	
normalFormLie = x-> (
    if generalExpressionLie x then (
	defLie indexFormLie x
	) else (
	print("the input is not a general Lie expression")
	)
    );


----------------------------------------
--permopLie
----------------------------------------

permopLie=(x,y)->(
    if not generalExpressionLie y then (
	print("the second input is not a general Lie expression")
	) else (
	if not all(flatten x,z->member(z,LL.gensLie)) or not all(x,z->z==unique z) then (
	    print("the first input is not a product of cycles") 
	    ) else (
	    computeLie(degLie y); 
	    idefLie(ipermopLie(x,y))
	    )
	)
    );


----------------------------------------
--randomLie
----------------------------------------

randomLie=method(Options => {genWeights => 1, genSigns => 0, field => QQ})
randomLie(ZZ,List) := opts -> (d, g) -> (
    N := LL;
    LL = lieAlgebra(g, {}, genWeights => opts.genWeights, genSigns => opts.genSigns, field => opts.field);
    computeLie(d);
    m := dimLie(d);
    coefs := apply(m,i->random(opts.field));
    out := {coefs,basisLie d};
    LL = N;
    out);
randomLie(List):= opts -> (dl) -> (
    m:=dimLie(dl);
    coefs := apply(m,i->random(LL.field));
    {coefs,basisLie(dl)}
    );


----------------------------------------
--signLie   
----------------------------------------

signLie=method(TypicalValue=>ZZ)
signLie(Symbol):=signLie(IndexedVariable):=
signLie(ZZ):=(x)->(
    if not member(x,LL.gensLie) then (
	print(x);
	print(" is not a generator")
	) else (
	po:=position(LL.gensLie,u->u===x);
	(LL.genSigns)_po
	)
    );
signLie(Array):=(x)->sum(x,signLie)%2;
  
signLie(List):=(x)->if generalExpressionLie x then signLie((x_1)_0) else signList x;
signLie(RingElement):=x->signLie defLie x;
   
   
----------------------------------------
--signExtLie   
---------------------------------------- 
   
signExtLie=method(TypicalValue=>ZZ)
signExtLie(RingElement):=(x)-> (
    if not LL.cache#?extAlgRing then print("compute minmodelLie first") else (
	if not member(x,gens LL.cache.extAlgRing) then (
	    print(x);
	    print(" is not a generator in extAlgRing")
	    ) else (
	    po:=index(x); 
	    ((LL.minmodel.genSigns)_po + 1)%2
	    )
	)
    );  


----------------------------------------
--subalgBasisLie   
----------------------------------------

subalgBasisLie=method(TypicalValue=>List)
subalgBasisLie(ZZ,List) := (n,x)->(
    computeLie n;
    if all(x,y->generalExpressionLie y) then (
	idefLie isubalgLie(n,apply(x,y->idefinvLie y))
	) else (
	print("the input is not correct")
	)
    );
subalgBasisLie(List,List):=(y,x)->(
    select(subalgBasisLie(y_0,x),z->weightLie z==y)
    );


----------------------------------------
--subalgLie   
----------------------------------------  
    
subalgLie=method()
subalgLie(ZZ,List) := (n,x)->(
    computeLie n;
    if all(x,y->generalExpressionLie y) then (
	apply(n,i->length isubalgLie(i+1,apply(x,y->idefinvLie y)))
	) else (
	print("the input is not correct")
	)
    );
subalgLie(List,List) := (y,x)->(
    length subalgBasisLie(y,x)
    );


----------------------------------------
--symmCyclePermLie
---------------------------------------- 
-- x is a list of  cycles. It is checked that x induces an
-- automorphism on LL

symmCyclePermLie=(x)->(
    n:=max(genDegMax(),relDegMax());
    computeLie(n);
    f:=mapLie(LL,LL,apply(LL.gensLie,y->[opperm(x,y)]))
    );


----------------------------------------
--symmPermLie
----------------------------------------
-- x is a reordering of L.gensLie. It is checked that x induces an
-- automorphism on LL
 
symmPermLie=(x)->(
    n:=max(genDegMax(),relDegMax());
    computeLie(n);
    f:=mapLie(LL,LL,apply(x,y->[y]))
    );


----------------------------------------
-- toMonomialLie
----------------------------------------

toMonomialLie = method()
toMonomialLie(Array) := x-> (
    computeLie(idegg x);
    idefLie(ifedd(isubstt(x)))
    );
toMonomialLie(List) := x-> (
    computeLie(max(apply(x_1,y->idegg y)));
    idefLie(sum(x_0,apply(x_1,y->ifedd(isubstt(y))),(i,j)->i*j))
    ); 
toMonomialLie(Array,List,List):=(x,G,S)->(
    if length x==1 then {{1},{x}} else (
	x1:=if class (drop(x,1))_0===Array then (drop(x,1))_0 else drop(x,1); 
	x0:=if class (drop(x,-1))_0===Array then (drop(x,-1))_0 else drop(x,-1);
	leftStandard(x0,toMonomialLie(x1,G,S),G,S)
	)
    );

    
----------------------------------------
-- uselLie
----------------------------------------
	
  useLie=method()
  useLie(LieAlgebra) := (L)->(
      use L.cache.mbRing;
      LL=L
      );



----------------------------------------
-- weightLie
----------------------------------------

weightLie=method(TypicalValue=>List)
weightLie(Symbol):=
weightLie(IndexedVariable):=
weightLie(ZZ):=(g)->(
    if not member(g,LL.gensLie) then (
	print(g);
	print(" is not a generator")
	) else (
	po:=position(LL.gensLie,x->x===g);
	((LL.genWeights)_po)
	)
    );
weightLie(Array):=(x)->(
    if x==[] then (
	flatten table(1,degreeLength LL.cache.lieRing,x->0)
	) else (
	sum(x,weightLie)
	)
    );
weightLie(List):=(x)->(
    if generalExpressionLie x then weightLie((x_1)_0) else weightList x
    );

----------------------------------------
-- whichLie
----------------------------------------
	
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
mb=getSymbol("mb")
fr=getSymbol("fr");
ext=getSymbol("ext");
ko=getSymbol("ko");
dims=getSymbol("dims");
opL=getSymbol("opL");
bas=getSymbol("bas");
gr=getSymbol("gr");
deglist=getSymbol("deglist");
locals=getSymbol("locals");
homdefs=getSymbol("homdefs");
diffl=getSymbol("diffl");


 

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
-- corresponding elements in ibasisLie(d).
basToMat=method(TypicalValue=>Matrix) 
basToMat(ZZ,List):=(d,m)->(
    if m==={}  then matrix {{0_(LL.field)}} else (
       	if LL.cache.dims#d==0 then matrix table(1,length m,x->0_(LL.field)) else (
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
    	)
    ); 
basToMat(ZZ,ZZ,List):=(d,j,m)->(
    if m==={} then matrix {{0_(LL.field)}} else (    
       	if LL.cache.dims#d==0 then matrix table(1,length m,x->0_(LL.field)) else (
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
    	)
    ); 




----------------------------------------    
-- boundariesDimLie
----------------------------------------
-- returns the dimension only
boundariesDimLie=(n,d)->length iboundariesLie(n,d); 





----------------------------------------    
-- checkmap
----------------------------------------
-- f:M->L, M and L should be computed to max of genDegMax() and
-- relDegMax() and max of deg(f(x)), x generator in M.
checkmap=(f)->(
      L0:=LL;
      M:=f.sourceLie; 
      L:=f.targetLie;
      if L===M and all(length M.gensLie,i->f#((M.gensLie)_i)===[(L.gensLie)_i]) then true else (
      	  relOK:=skipz apply(M.relsLie,x->ievalMapLie(f,x))=={};
      	  diffOK:= if M.gensLie=={} then true else 
          skipz apply(length M.gensLie,
	      i->ievalMapLie(f,(M.genDiffs)_i)-idiffLie(
	  	  (LL=L;ievalMapLie(f,[(M.gensLie)_i]))))=={};
      LL=L0;
      if not relOK then print("the map is not welldefined");
      if not diffOK then print("the map does not commute with the differential");
      relOK and diffOK)
  );



----------------------------------------    
-- checkder
----------------------------------------
-- d:M->L is a derivation, checking that d maps the relations on M to 0.
-- L should be computed to max of (d.weightDer)_0 and relDegMax for M
checkder=(d)->(
    che:=unique apply(d.sourceLie.relsLie,x->evalDerLie(d,x)); 
    ok:= che=={} or che=={[]};
    if not ok then print("the derivation is not welldefined");
    ok
    ); 


----------------------------------------    
-- checkdiff
----------------------------------------
-- Controlling that the differential maps the relations to 0 and that
-- the square is 0.
checkdiff=()->(
    dz:=skipz apply(LL.relsLie,x->idiffLie(x));
    d2:=skipz apply(LL.genDiffs,x->idiffLie(x)); 
    ok:=true; 
    if not dz=={} then (
	ok=false; 
	print("the differential is not welldefined")
	);
    if not d2=={} then (
	ok=false; 
	print("the square of the differential is not zero")
	);
    ok
    ); 


  
  
----------------------------------------    
-- commrel and commrelgen
----------------------------------------  
-- the commutative law on basis elements, used in computeLie
commrel=(m)->(
    defm:=idef(m);
    if length defm==1 then 0_(LL.cache.lieRing) else (
	x:=first defm;
	m1:=drop(defm,1); 
	m+eps([x],m1)*op(m1,LL.cache.lieRing_(x))
	)
    ); 

commrelgen=(x,m)->(defm:=idef(m); op([x],m)+eps([x],defm)*op(defm,LL.cache.lieRing_(x)));
   

----------------------------------------    
-- cyclesLie
----------------------------------------
-- gives a basis for the cycles of first degree n and
-- homological degree j 
cyclesLie=(n,j)->(
    if j==0 then ibasisLie(n,0) else (
	ba:=basToMat(n,j-1,apply(ibasisLie(n,j),x->idiffLie(x)));
        if ba==0 then ibasisLie(n,j) else skipz matToBas(n,j,matrix gens kernel ba)
	)
    );


----------------------------------------    
-- deflist
----------------------------------------  
deflist=(x)->(if x==={} then [] else prepend((last x)%(LL.numGen),deflist(drop (x,-1))));


----------------------------------------    
-- degiterLie
----------------------------------------
-- gives the multidegree of an iterated lie monomial in 
-- digital form ([1,2,0,1])

degiterlie=(x)->sum(x,y->LL.genWeights_y); 


----------------------------------------    
-- degList
----------------------------------------
degList = x->apply(x,y->if class y===Array then degLie y else 
    if generalExpressionLie y then degLie((y_1)_0) else (
	print(y); 
	print("is not a general Lie expression")
	)
    );


----------------------------------------    
-- degRel
----------------------------------------
-- computes the multidegree of a homogeneous relation y 
-- in digital form  	    

degRel=(y)->(if class y===Array then degiterlie(y) else degiterlie((y_1)_0));




----------------------------------------    
-- doZeroLie
----------------------------------------
-- this sets the keys for the zero Lie algebra, with dims=0 up to maxDeg

doZeroLie=()->(
    LL.genWeights=LL.genSigns={};
    for i from 1 to LL.compdeg do (
	LL.cache.bas#i={};
	LL.cache.dims#i=0;
	LL.cache.gr#i=ideal{0_(LL.field)};
	LL.cache.deglist#i={}
	)
    );


----------------------------------------    
-- eps
----------------------------------------
-- the "exchange sign" when x and y are iterated Lie product
-- in digital form
  
eps=(x,y)->((-1)^(sign(x)*sign(y)));  


----------------------------------------    
-- eval
----------------------------------------  
-- x is a permutation in the form of a cycle, y is an element
eval=(x,y)->(if member(y,x) then (
	i:=position(x,z->z===y); 
	x_((i+1)%length x)
	) else y
    );



----------------------------------------    
-- evalmultDerLie
----------------------------------------
evalmultDerLie=(f,g,x)->idefinvLie(evalDerLie(f,evalDerLie(g,x)))-
       (-1)^(f.signDer*g.signDer)*idefinvLie(evalDerLie(g,evalDerLie(f,x)));
     
 
----------------------------------------    
-- extmult
----------------------------------------     
extmult=(i,j)-> (
    M:=LL.minmodel; 
    fi:=(M.gensLie)_i; 
    fj:=(M.gensLie)_j;
    si:=(M.genSigns)_i; 
    sj:=(M.genSigns)_j;
    co:=apply(M.numGen,k->(
	    if class (M.genDiffs)_k ===Array then (
		coes:={1}; 
		arrs:={(M.genDiffs)_k}
		) else (
	    	coes=((M.genDiffs)_k)_0; 
		arrs=((M.genDiffs)_k)_1
		);
	    len:=length coes;
	    if not (member([fi,fj],arrs) or member([fj,fi],arrs)) then 
	    0_(LL.cache.extAlgRing) else (
		if member([fi,fj],arrs) then (
		    po:=(select(len,x->arrs_x===[fi,fj]))_0;
		    coes_po*(if i==j then 2 else (-1)^(sj*(si+1)))
		    ) else (
		    po=(select(len,x->arrs_x===[fj,fi]))_0;
		    coes_po*(-1)^(sj+1)
		    )
	    	)
	    )
    	);
    sum(co,gens LL.cache.extAlgRing,(r,s)->r*s)
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
-- ibasisLie
----------------------------------------  
ibasisLie=method(TypicalValue=>List)
-- gives the list of basis elements in lieRing of first degree n 
ibasisLie(ZZ):=(n)->LL.cache.bas#n;
-- gives the list of basis elements in lieRing of first degree n
-- and last degree d 	 
ibasisLie(ZZ,ZZ):=(n,d)->select(ibasisLie(n),x->last degree x==d);
-- gives the list of basis elements in lieRing of multidegree x    
ibasisLie(List):=(x)->select(ibasisLie(x_0),y->degree y===x); 


----------------------------------------    
-- iboundariesLie
----------------------------------------
-- gives a basis for the boundaries of first degree n and
-- homological degree d

iboundariesLie=(n,d)->(
	idi:=skipz apply(ibasisLie(n,d+1),x->idiffLie(x)); 
	if idi==={} then {} else 
	    flatten entries gens gb(
	    ideal idi,DegreeLimit=>
	    prepend(n,flatten table(1,length((LL.genWeights)_0)-1,x->0)))
    );


----------------------------------------    
-- idef
----------------------------------------
-- transforms a monomial in a lieRing to an iterated Lie product
-- in digital form 

idef=(m)->deflist monlist m;




----------------------------------------    
-- idefinvLie
----------------------------------------
-- is left inverse to "idefLie", that is, idefinvLie(idefLie(x))=x,
-- when x is a basis element in lieRing.
-- It holds that idefLie(idefinvLie(x))≈x in the free Lie algebra
-- on the generators modulo the relations.
-- idefinvLie is not "listable". Input is a generator or an iterated
-- Lie product, or of the type {{coef}, {lie}}.  
idefinvLie=method(TypicalValue=>RingElement)
idefinvLie(Symbol):=idefinvLie(IndexedVariable):=(x)->idefinvLie([x]);
idefinvLie(ZZ):=(x)->idefinvLie([x]);
idefinvLie(Array):=(x)->(
    if x==[] then 0_(LL.cache.lieRing) else ifed isubst x
    );
idefinvLie(List):=(x)->sum(x_0,x_1,(i,j)->i*idefinvLie(j));



----------------------------------------    
-- idefLie
----------------------------------------    
-- computes the linear combination of iterated Lie products as 
-- {{coef},{lie}} corresponding to the element p in lieRing, where
-- {lie} is a list of iterated Lie products of generators
-- (generators as given in input, not the digital form). 
-- The value on a basis element is only the iterated Lie product.
-- idefLie is "listable". 
idefLie=method(TypicalValue=>List)
idefLie(List):=(p)->apply(p,x->idefLie(x));
idefLie(RingElement):=(p)-> (  
    mons:=flatten entries monomials p; 
    if mons=={} then [] else (  
	if p===mons_0 then apply(idef(p),y->LL.gensLie_y) else (
	    coef:=apply(mons,x->(p_x));
	    {coef,apply(mons,x->apply(idef(x),y->LL.gensLie_y))}
	    )
	)
    );
    
  
----------------------------------------    
-- idegg
----------------------------------------  
idegg=method()
idegg(Array):=(x)->idegg x_0 + idegg x_1;
idegg(Symbol):=idegg(IndexedVariable):=idegg(ZZ):=(g)-> (
    if not member(g,LL.gensLie) then (
	print(g);
	print(" is not a generator")
	) else (
	po:=position(LL.gensLie,x->x===g);
	((LL.genWeights)_po)_0)
    );

 
----------------------------------------    
-- idegLie
----------------------------------------

idegLie=x->if x==0 then 0 else (degree x)_0;


----------------------------------------    
-- iderLie
---------------------------------------- 
iderLie=y->(
    d:=new MutableHashTable; 
    d.sourceLie=LL; 
    d.targetLie=LL; 
    apply(LL.numGen, i->d#((LL.gensLie)_i)=y_i); 
    if all(y,z->z==[]) then ( 
	d.weightDer=flatten table(1,LL.deglength,x->0);
	d.signDer=0
	) else (
	i:=position(y,z->not z==[]);
	d.weightDer=weightLie(y_i)-(LL.genWeights)_i;
	d.signDer=(signLie(y_i)-(LL.genSigns)_i)%2
	);
    d.maplie=idMapLie();
    new DerLie from d
    );
 
 
----------------------------------------    
-- idiff
----------------------------------------
-- computation of the differential as a polynomial
-- in liering. Input is a generator in digital form or 
-- an iterated Lie product in digital form.
idiff=method(TypicalValue=>RingElement)
idiff(ZZ):=(n)->(
    gd:=(LL.genDiffs)_n;
    if gd===[] then 0_(LL.cache.lieRing) else ifed(isubstrel(gd)));
idiff(Array):=(x)->(
    if x==[] then 0_(LL.cache.lieRing) else if length x==1 then idiff(x_0) else (
	x1:=drop(x,1);
	si:=sign([x_0]);
	-(-1)^((si+1)*sign(x1))*op(x1,idiff(x_0))+(-1)^(si)*op([x_0],idiff(x1)))
    );



----------------------------------------    
-- idiffLie
----------------------------------------  
-- the differential of a generator, GE-element, or an element 
-- in lieRing. The value is in lieRing.
idiffLie=method(TypicalValue=>RingElement)
idiffLie(Symbol):=idiffLie(IndexedVariable):=
idiffLie(ZZ):=(x)->(
    po:=position(LL.gensLie,u->u===x);
    xx:=(LL.genDiffs)_po;if xx===[] then 0_(LL.cache.lieRing) else idefinvLie(xx)
    );
idiffLie(Array):=(x)-> (
    if x==[] then 0_(LL.cache.lieRing) else if length x==1 then idiffLie(x_0) else idiff(isubst x)
    );
idiffLie(List):=(x)->sum(x_0,x_1,(i,j)->i*idiffLie(j));
idiffLie(RingElement):=(x)->(
    if x==0_(LL.cache.lieRing) then 0_(LL.cache.lieRing) else idiffLie(idefLie(x))
    );



----------------------------------------    
-- idimsLie
----------------------------------------  
idimsLie=d->for i from 1 to d list LL.cache.dims#i;


----------------------------------------    
-- idimtotLie
----------------------------------------  

idimtotLie=d->sum idimsLie(d);



----------------------------------------    
-- idivisorLie
----------------------------------------
-- m is a list of lieRing elements of degree t,
-- p is a list of lieRing elements of degree s, the result 
-- is a basis for those x of degree d=t-s such that x multiplies
-- p to the space generated by m
idivisorLie=(t,s,m,p)->(
    d:=t-s; 
    if LL.cache.dims#t==0 then ibasisLie(d) else (
      	B:=transpose basToMat(t,m);
      	kerB:=transpose generators kernel(B);
      	matToBas(d,generators kernel joinvert apply(p,y->
		(kerB*basToMat(t,apply(ibasisLie(d),x->imult(y,x))))))
	)    
);


----------------------------------------    
-- idMapLie
----------------------------------------
-- the identity map on L

idMapLie=()->mapLie(LL,LL,apply(LL.gensLie,x->[x]));



----------------------------------------    
-- ieuler
---------------------------------------- 
-- computes the eulercharacteristics of first degree n. It is
-- assumed that the homological degree is at most n-1.
ieuler=(n)->sum apply(n,j->(-1)^j*dimLie(n,j));

----------------------------------------    
-- ievalDerLie
----------------------------------------
-- gives the value of the derivation in lieRing
-- on an iteraded Lie product on the generators in d.sourceLie.

ievalDerLie=method()
ievalDerLie(DerLie,Array):=(d,x)->( 
    N := LL;
    LL = d.targetLie; 
    if x==[] then out:=0_(LL.cache.lieRing) 
       else if length x==1 then out=idefinvLie d#(x_0) else 
	out=imult(idefinvLie(d#(x_0)),
	    ievalMapLie(d.maplie,drop(x,1)))+
	    (-1)^((LL=d.sourceLie;isignLie(x_0))*(d.signDer))*imult((
		LL=d.targetLie;
		idefinvLie(d.maplie#(x_0))),
	        ievalDerLie(d,drop(x,1)));
    LL=N;
    out
    );
ievalDerLie(DerLie,List):=(d,x)->sum(x_0,x_1,(j,k)->j*ievalDerLie(d,k));
    
----------------------------------------    
-- ievalMapLie
----------------------------------------
-- gives the value of the Lie homomorphism in lieRing
-- on an iteraded Lie product on the generators in f.sourceLie.
ievalMapLie=method(TypicalValue=>RingElement)
ievalMapLie(MapLie,Array):=(f,x)->(
    N:=LL;
    LL=f.targetLie;
    if x==[] then out:=0_(LL.cache.lieRing) 
        else if length x==1 then out=idefinvLie(f#(x_0)) else (
	fli:=apply(x,y->idefinvLie(f#y));
	out=fold(fli,(u,v)->imult(u,v)));
    LL=N;
    out
    );
--gives the value of the Lie homomorphism in lieRing on a GE-element	    
ievalMapLie(MapLie,List):=(f,x)->sum(x_0,x_1,(j,k)->j*ievalMapLie(f,k));

--gives the value of the Lie homomorphism on a linear combination of
--basis elements in f.sourceLie.cache.lieRing. The values are in 
-- f.targetLie.cache.lieRing	    
ievalMapLie(MapLie,RingElement):=(f,x)->(
    N:=LL;
    LL=f.sourceLie;
    if x==0_(LL.cache.lieRing) then 0_(f.targetLie.cache.lieRing) else (
	out:=ievalMapLie(f,idefLie(x));
	LL=N;
	out
	)
    );



----------------------------------------    
-- ifed
----------------------------------------
-- transforms an iterated Lie product in digital form of 
-- first degree d to a linear combination
-- of elements in bas#d
-- ifed(idef m)=m if m is a basis element
ifed=method(TypicalValue=>RingElement)
ifed(Array):=(x)->(
    le:=length x; 
    if le===1 then ( 
	d:=(degiterlie(x))_0; 
	if gens LL.cache.gr#(d) == 0 then (
	    (LL.cache.lieRing)_(x_0) 
	    ) else (
	    ((LL.cache.lieRing)_(x_0))%LL.cache.gr#d
	    )
	) else  op([x_0],ifed(drop(x,1)))
    );
-- input is of the form {{coef},{lie}}, where the elements in 
-- lie are iterated Lie products in digital form.   
ifed(List):=(x)->sum(x_0,x_1,(i,j)->i*ifed(j));



----------------------------------------    
-- ifedd
----------------------------------------
-- The following three functions, ifedd, isubstt and idegg are only used in toMonomialLie 
ifedd=method(TypicalValue=>RingElement)
ifedd(Array):=(x)->(
    le:=length x; 
    if le===1 then ( 
	d:=(degiterlie(x))_0; 
	if gens LL.cache.gr#(d) == 0 then (LL.cache.lieRing)_(x_0) else ((LL.cache.lieRing)_(x_0))%LL.cache.gr#d
	) else (
	x1:=if class (drop(x,1))_0===Array then (drop(x,1))_0 else drop(x,1); 
	x0:= if class x_0===Array then x_0 else [x_0];	
	imult(ifedd(x0),ifedd(x1)))
    );
ifedd(List):=(x)->sum(x_0,x_1,(i,j)->i*ifedd(j));



----------------------------------------    
-- ihomologyLie
----------------------------------------
-- gives a basis in terms of cycles in lieRing for the
-- homology of first degree n and homological degree j

ihomologyLie=(n,j)->(
	if iboundariesLie(n,j)=={} then prel:=cyclesLie(n,j) else (
	    I:=ideal iboundariesLie(n,j);
	    prel=skipz apply(cyclesLie(n,j),x->x%I));
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
	rs=rs|flatten apply(gendeg(n-i),x->apply(iideal(i,y),z->op([x],z))); 
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
-- iimageLie
----------------------------------------
iimageLie=(n,d,f)->(
    L:=LL;
    out:=length select(imageBasisLie(n,f),x->(weightLie(LL=f.targetLie;x))_(-1)==d);
    LL=L;
    out
    );


----------------------------------------    
-- ikernelLie
----------------------------------------  
ikernelLie = (n,d,f)->(
    N:=LL;
    LL=f.sourceLie;
    out:=length select(kernelBasisLie(n,f),x->(weightLie x)_(-1)==d);
    LL=N;
    out
    );

----------------------------------------
-- ilieAlgebra
----------------------------------------
-- this is an internal version of lieAlgebra which does not make any checking and has
-- maxDeg as an extra argument

ilieAlgebra = method(TypicalValue => LieAlgebra,Options => 
    {genWeights => 1, genSigns => 0, field => QQ, genDiffs => {}})
ilieAlgebra(ZZ,List,List) := opts->(imaxDeg,gensLie,relsLie)->(
    L := new MutableHashTable; 
    LL=L;
    L.gensLie=gensLie;
    L.genWeights=opts.genWeights;
    L.genSigns=opts.genSigns;
    L.genDiffs=opts.genDiffs;
    L#(symbol cache)=new CacheTable;
    if L.genDiffs=={} then L.cache.diffl=false else L.cache.diffl=true;
    L.relsLie=relsLie;
    L.field=opts.field;
    L.numGen=length L.gensLie;
    L.compdeg=0;
    L.cache.maxDeg=imaxDeg;
    L.cache.lieRing=L.field[]; 
    L.cache.mbRing=L.field[];
    L.cache#(symbol dims)=new MutableHashTable;
    L.cache#(symbol opL)=new MutableHashTable;
    L.cache#(symbol bas)=new MutableHashTable;
    L.cache#(symbol gr)=new MutableHashTable;
    L.cache#(symbol deglist)=new MutableHashTable;
    if L.gensLie=={} then doZeroLie();
    L.cache.bas#0={mb0}; 
    L.cache.dims#0=1;
    if not L.cache.diffl then (
        L.genDiffs=flatten table(1,L.numGen,x->[]);
	if L.genWeights===1 then L.genWeights=flatten table(1,L.numGen,x->{1,0}) else
	    L.genWeights=apply(L.genWeights,x->append(x,0))
	);
    if L.genSigns===0 then L.genSigns=flatten table(1,L.numGen,x->0);
    if L.genWeights==={} then L.deglength=0 else L.deglength=length (L.genWeights)_0;
    L.cache.lieRing=lieR();
    M:=new LieAlgebra from L;
    LL=M;
    M);



----------------------------------------    
-- imult
----------------------------------------
-- the internal Lie product on lieRing
-- OBS: imult removes zeroes, but multListLie does not!
imult=method(TypicalValue=>RingElement)
imult(RingElement,RingElement):=(x,y) -> linext((u,v)->imultmon(u,v),x,y);
imult(List,List):= (x,y)->(skipz flatten apply(x,z->apply(y,u->imult(z,u)))); 


----------------------------------------    
-- imultmon
----------------------------------------  
-- x,y are basis elements in L.cache.lieRing of degree d,e;
-- output is the Lie product, which is a linear combination of 
-- basis elements of degree d+e  
imultmon=(x,y)->op(idef(x),y);   



----------------------------------------    
-- ind
----------------------------------------
-- gives the index of m in ibasisLie(d). m belongs to lieRing
ind=method(TypicalValue=>ZZ)
ind(ZZ,RingElement):=(d,m)->position(LL.cache.bas#d,x->x===m);
--gives the index of m in ibasisLie(d,j). m belongs to lieRing
ind(ZZ,ZZ,RingElement):=(d,j,m)->position(ibasisLie(d,j),x->x===m);



----------------------------------------    
--indexFormList
----------------------------------------	  
indexFormList = x->apply(x,y->if class y===Array then indexFormLie y else 
    if generalExpressionLie y then sum(y_0,y_1,(i,j)->i*indexFormLie(j)) else (
	print(y); 
	print("is not a general Lie expression")
	)
    );


----------------------------------------    
-- invimage
----------------------------------------
-- gives a basis, in terms of columns in a matrix, for the space of
-- those x such that Ax is in the column space of B. A and B are matrices
-- of numbers such that BA is defined. Used in invImageLie. See also
-- idivisorLie.
invimage=(A,B)->generators kernel((transpose generators kernel(transpose B))*A); 


----------------------------------------    
-- ipermopLie
----------------------------------------  
--  y is a GE-element, x is a product of cycles, output
--  is in lieRing
ipermopLie=(x,y)->if class y===Array then (
    idefinvLie(opiterlie(x,y))
    ) else (
    sum(y_0,y_1,(i,j)->i*idefinvLie(opiterlie(x,j)))
    );


----------------------------------------    
-- iRels
----------------------------------------
-- transforms the relations to digital form

iRels=()->apply(LL.relsLie,isubstrel);



----------------------------------------    
-- isignLie
----------------------------------------  
isignLie=method(TypicalValue=>ZZ)
-- the sign of a generator 
isignLie(Symbol):=isignLie(IndexedVariable):=isignLie(ZZ):=(x)->(
    po:=position(LL.gensLie,u->u===x);
    (LL.genSigns)_po
    );
-- the sign of an iterated Lie product
isignLie(Array):=(x)->sum(x,isignLie)%2;
-- the sign of a general Lie expression      
isignLie(List):=(x)->isignLie((x_1)_0);
-- the sign of a homogeneous element i lieRing
isignLie(RingElement):=(x)->(
    de:=idefLie(x);
    if class de===Array then isignLie(de) else isignLie((de_1)_0)
    );


----------------------------------------    
-- isubalgLie
----------------------------------------  
-- gives a basis in lieRing of first degree n for the sub Lie algebra
-- generated of the elements in the list x (which may contain elements 
-- of different degrees).
isubalgLie=method(TypicalValue=>List)
isubalgLie(ZZ,List):=(n,x)->(cachesub:=new MutableHashTable;
    if cachesub#?(n,x) then cachesub#(n,x) else (
	rs:=listdeg(n,x);
	for i from 1 to n-1 do rs=rs|imult(listdeg(n-i,x),isubalgLie(i,x));
	if rs=={} then cachesub#(n,x)={} else (
	    cachesub#(n,x)=
	    flatten entries gens gb(ideal rs,DegreeLimit=>
	 	prepend(n,flatten table(1,length((LL.genWeights)_0)-1,x->0)))
	    )
	)
    );  
-- as above, but with degree replaced by multidegree:
isubalgLie(List,List):=(y,x)->(
    select(isubalgLie(y_0,x),z->degree z==y)
    );


----------------------------------------    
-- isubst
----------------------------------------
-- transforms a Lie monomial to digital form

isubst=(x)->apply(x,y->position(LL.gensLie,u->u===y));


----------------------------------------    
-- isubstrel
----------------------------------------     
-- transforms a non-zero element in genDiffs or relsLie to 
-- digital form

isubstrel=(x)-> if class x===Array then isubst(x) else {x_0,apply(x_1,isubst)};


----------------------------------------    
-- isubstt
----------------------------------------
isubstt=method(TypicalValue=>RingElement)
isubstt(Array):=(x)->[isubstt x_0,isubstt x_1];
isubstt(Symbol):=isubstt(IndexedVariable):=isubstt(ZZ):= (x)->position(LL.gensLie,u->u===x);


----------------------------------------    
-- iweightLie
----------------------------------------
  iweightLie = x-> if x==0 then flatten table(1,degreeLength ring x,x->0) else degree x;


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
-- leftStandard
----------------------------------------
leftStandard = (x,y,G,S) -> (
    if length x==1 then {y_0,apply(y_1,z->prepend(x_0,z))} else (
	x1:=if class (drop(x,1))_0===Array then (drop(x,1))_0 else drop(x,1); 
	x0:=if class (drop(x,-1))_0===Array then (drop(x,-1))_0 else drop(x,-1);
	y0:=leftStandard(x0,leftStandard(x1,y,G,S),G,S);
	y1:=leftStandard(x1,leftStandard(x0,y,G,S),G,S);
	{join(y0_0, -(-1)^(signGenLie(x0,G,S)*signGenLie(x1,G,S))*y1_0),join(y0_1,y1_1)}
	)
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
    d:=idegLie m;
    po:=position(LL.cache.bas#d,x->x===m);
    (LL.cache.mbRing)_(idimtotLie(d-1)+po)
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
-- below the bilinear extension of f, which is defined on pair of
-- monomials in a polynomial ring
linext(Function,RingElement,RingElement):=(f,p,q)->linext(x->linext(y->f(x,y),q),p);



----------------------------------------    
-- listdeg
----------------------------------------
-- selects the elements in a list x of first degree n
listdeg=(n,x)->(select(x,y->(idegLie y==n)));






----------------------------------------    
-- matToBas
----------------------------------------  
matToBas=method(TypicalValue=>List) 
-- transforms a list m of linear combinations of lieRing elements 
-- in first degree d and last degree j to a matrix
-- whose columns are the coefficients (belong to L.field)
-- of the corresponding lieRing elements in ibasisLie(d,j)
matToBas(ZZ,Matrix):=(d,A)->(
    if A==0 then flatten table(1,numgens source A,x->0_(LL.cache.lieRing)) else (
	apply(entries transpose A,x->sum(x,ibasisLie(d),(i,j)->i*j))
	)
    );
-- the number of rows in A equals dimLie(d),
-- the result is a list of linear combination of
-- basis elements in lieRing of first degree d corresponding
-- to the columns in A
matToBas(ZZ,ZZ,Matrix):=(d,j,A)->(
    if A==0 then flatten table(1,numgens source A,x->0_(LL.cache.lieRing)) else (
	apply(entries transpose A,x->sum(x,ibasisLie(d,j),(r,s)->r*s))
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
-- gives to a basis element in mbRing the 
-- corresponding basis element in lieRing
mbToLiemon=(m)->(
    d:=idegLie m;
    po:=position(gens LL.cache.mbRing,x->x===m);
    (LL.cache.bas#d)_(-idimtotLie(d-1)+po)
    );




----------------------------------------    
-- minmodelone
----------------------------------------
--contruction of the model in first degree 1
minmodelone=(d)->(
    L:=LL;
    gens1:=select(LL.gensLie,x->degLie(x)==1);
    newdiffs:=flatten table(1,length gens1,x->[]); 
    newgens:=apply(length gens1,x->fr_x); 
    newweights:=apply(gens1,x->weightLie(x));
    newsigns:=apply(gens1,x->isignLie(x));
    M:=ilieAlgebra(d,newgens,{},genWeights=>newweights,
	genSigns=>newsigns,genDiffs=>newdiffs,field=>LL.field);
    M.cache.homdefs=apply(gens1,x->[x]);LL=L;M
    );


----------------------------------------    
-- minmodelstep
----------------------------------------
-- given that the Lie algebra M is a free minimal model of L up to
-- first degree n-1, the output becomes a free minimal model up to
-- first degree n
minmodelstep=(n,d,M)->(
    newgens:=M.gensLie;
    newsigns:=M.genSigns; 
    newdiffs:=M.genDiffs; 
    newweights:=M.genWeights; 
    L:=LL;
    -- step 1: H(f) surjective, f: M -> L
    f:=mapLie(L,M,M.cache.homdefs);
    LL=M; 
    computeLie(n); 
    LL=L; 
    indechom:=(i)->(
	dec:=iboundariesLie(n,i); 
	cyc:=cyclesLie(n,i);
	LL=M; 
	cycM:= ihomologyLie(n,i); 
	dec=join(dec,apply(cycM,x->ievalMapLie(f,x)));
	LL=L;
	if dec=={} then cyc else (
	    prel:= skipz apply(cyc,x->x%ideal dec); 
	    if prel=={} then {} else skipz flatten entries gens gb(ideal prel,
		DegreeLimit=>prepend(n,flatten table(1,length(L.genWeights)_0-1,x->1)))
	  )
      );
  newfR:=flatten for i from 0 to n-1 list indechom(i);
  newf:=apply(newfR,x->idefLie(x));
  newgens=join(newgens,toList(fr_(length newgens)..fr_(length newgens + length newf - 1)));
  newsigns=join(newsigns,apply(newfR,x->isignLie(x)));
  newdiffs=join(newdiffs,flatten table(1,length newf,x->[]));
  newweights=join(newweights,apply(newfR,x->iweightLie(x)));
  M1:=ilieAlgebra(d,newgens,{},genSigns=>newsigns,
      genWeights=>newweights,genDiffs=>newdiffs,field=>L.field);
  M1.cache.homdefs=join(M.cache.homdefs,newf);
  -- step 2: H(f) injective
  f=mapLie(L,M1,M1.cache.homdefs); 
  LL=M1;
  computeLie(n); 
  unit:=append(flatten table(1,M1.deglength -1,x->0),1);
  hoM:=hoL:=ge:=newbo:=baL:=baM:=baD:=newf={};
  for i from 0 to n-1 do (    
      LL=L;
      r:=homologyLie(n,i); 
      LL=M1; 
      s:=homologyLie(n,i);
      if s > r  then  (    
	  LL=L;
	  baD=basToMat(n,i,apply(ibasisLie(n,i+1),x->idiffLie(x)));
	  -- the matrix for d_L in bas(n,i+1,L) --> bas(n,i,L)
	  hoM=(LL=M1; ihomologyLie(n,i)); 
	  -- z_1,...,z_s cycles in M1.cache.lieRing, s>0 
	  hoLM:=apply(hoM,x->ievalMapLie(f,x));
	  LL=L;
	  -- f(z_1),...,f(z_s)
	  baM=basToMat(n,i,hoLM);
	  -- the matrix for hoLM in ibasisLie(n,i,L)
	  if r==0 or skipz hoLM=={} then (
	      -- directly to the last step, a simple case when fr 
	      -- kills the basis for hoM and fr --> v_i, where d_L(v_i)=f(z_i): 
	      len:=length newgens;
	      newgens=join(newgens,toList(fr_len..fr_(len+s-1)));  
	      LL=M1;
	      newsigns=join(newsigns,apply(hoM,x->(isignLie(x)+1)%2));
	      newdiffs=join(newdiffs,idefLie(hoM));
	      newweights=join(newweights,apply(hoM,x->degree(x)+unit));
	      newf=join(newf,(
		      LL=L; 
		      if baD==0 then flatten table(1,s,x->[]) else 
		       apply(flatten entries (
		               matrix{ibasisLie(n,i+1)}*solve(baD,baM)),
		           x->idefLie(x)))); 
	       -- see below	 
	       ) else (
	       hoL=(LL=L; ihomologyLie(n,i)); 
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
		       if baD==0 then flatten table(1,length newbo,x->[]) else 
		       apply(
		           flatten entries (
		               matrix{ibasisLie(n,i+1)}*solve(baD,baM*ge)),
		           x->idefLie(x))
		       )); 
	       -- list x which will be f:s values on the new generators
    	       -- summary (commutative diagram): f: fr_i --> x, 
	       -- d_M1: fr_i --> z, f: z --> v, d_L_ x --> v
	       len=length newgens;
	       newgens=join(newgens,apply(length newbo,x->fr_(len+x)));
	       LL=M1;
	       newsigns=join(newsigns,apply(newbo,x->(isignLie(x)+1)%2));
	       newdiffs=join(newdiffs,idefLie(newbo));
	       newweights=join(newweights,apply(newbo,x->degree(x)+unit));
	       ) -- this finishes the main case when there are cycles to kill
	   ) -- next i
       ); -- end of loop
   M2:=ilieAlgebra(d,newgens,{},genSigns=>newsigns,
       genWeights=>newweights,genDiffs=>newdiffs,field=>L.field);
   M2.cache.homdefs=join(M1.cache.homdefs,newf); 
   LL=L;
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
    for i from 1 to d do for j from 0 to LL.cache.dims#i-1 do mblist= append(mblist,(mb)_{i,j});
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
-- computes the operation of an iterated Lie product in digital form
-- of first degree d on a monomial m in lieRing of degree e.
-- The result is a polynomial in lieRing which is a linear
-- combination of basis elements in bas#(d+e) if the degree d+e is
-- computed. Otherwise, the output is a linear combination of "prebasis" 
-- elements and this fact is used in computeLie. 
op=(x,m)->(
    if LL.cache.opL#?(x,m) then LL.cache.opL#(x,m) else ( 
	if m===mb0 then ifed(x) else ( 
	    if m == 0 then 0_(LL.cache.lieRing) else (
		d:=idegLie m; 
		le:=length x; 
            	d0:= ((LL.genWeights)_(x_0))_0; 
            	if le==1 then ( 
		    if (gens LL.cache.gr#(d+d0)) == 0 then 
                    LL.cache.lieRing_(d*LL.numGen+x_0)*m else 
                    LL.cache.lieRing_(d*LL.numGen+x_0)*m % LL.cache.gr#(d+d0)
              	    ) else (
		    fir:=first x; secd:=drop(x,1); 
		    ds:=(degiterlie(secd))_0;
                    if  (gens LL.cache.gr#(d+ds+d0)) == 0 then ( 
			(LL.cache.lieRing_((d+ds)*LL.numGen+fir)*op(secd,m)) -
                   	eps([fir],secd)*linext(i-> op(secd,i), 
                  	    LL.cache.lieRing_(d*LL.numGen+fir)*m %LL.cache.gr#(d+d0))
                      	) else ( 
			LL.cache.opL#(x,m)= ((LL.cache.lieRing_((d+ds)*LL.numGen+fir)*op(secd,m)) -
                            eps([fir],secd)*linext(i-> op(secd,i), 
                            	LL.cache.lieRing_(d*LL.numGen+fir)*
                            	m %LL.cache.gr#(d+d0)))%LL.cache.gr#(d+ds+d0)
		    	)
               	    )
	       	)
            )
      	)
    );


----------------------------------------    
-- opiterlie
----------------------------------------  
-- opiterlie({{3,4},{5,6}},[1,4,2,5])=[1,3,2,6]
-- opiterlie({{1,2,3,4}},[1,2,3,4])=[2,3,4,1]
opiterlie=(x,y)->(apply(y,z->opperm(x,z)));


----------------------------------------    
-- opperm
----------------------------------------  
-- opperm({{1,3},{2,4},{3,5},{4,6}},5)=1
opperm=(x,y)->fold(append(x,y),eval);


----------------------------------------    
-- rel
----------------------------------------
-- gives the relations in digital form of first degree d

rel=(d)->(select(iRels(),y->((degRel(y))_0===d)));


----------------------------------------    
-- relcomm
----------------------------------------
-- the commutative law on Lie elements in a relation, used in computeLie
relcomm=(x)->(
    if length x==1 then 0_(LL.cache.lieRing) else (
	x1:=first x; 
	x2:= drop(x,1);
	ifed(x) + eps([x1],x2)*op(x2,LL.cache.lieRing_(x1))
	)
    );


----------------------------------------    
-- relDegMax
----------------------------------------
-- the maximal degree of the relations 
relDegMax=()->if LL.relsLie==={} then 0 else max apply(iRels(),y->(degRel(y))_0);


----------------------------------------    
-- sign
----------------------------------------
-- gives the sign of an iterated Lie product in 
-- digital form ([1,2,0,1])
sign=(x)->(sum(x,y->LL.genSigns_y))%2;



----------------------------------------    
-- signGenLie
----------------------------------------
signGenLie = (x,G,S)->(
    if length x==1 then (po:=position(G,u->u===x_0);S_po) else (
	x1:=if class (drop(x,1))_0===Array then (drop(x,1))_0 else drop(x,1); 
	x0:=if class (drop(x,-1))_0===Array then (drop(x,-1))_0 else drop(x,-1);
	(signGenLie(x1,G,S) + signGenLie(x0,G,S))%2
	)
    );



----------------------------------------    
-- signList
----------------------------------------
signList = x->apply(x,y->if class y===Array then signLie y else 
    if generalExpressionLie y then signLie((y_1)_0) else (
	print(y); 
	print("is not a general Lie expression")
	)
    );



----------------------------------------    
-- skipz
----------------------------------------
-- removes zeroes in a list
skipz=x->if length(x)==0 then {} else (
    if x_0==0 then skipz drop(x,1) else prepend (x_0,skipz drop(x,1))
    );


----------------------------------------    
-- skipZZ
----------------------------------------
-- removes O_ZZ in a list
skipZZ=x->if length(x)==0 then {} else (
    if x_0===0 then skipZZ drop(x,1) else prepend (x_0,skipZZ drop(x,1))
    );


----------------------------------------    
-- weightList
----------------------------------------	  
weightList = x->apply(x,y->if class y===Array then weightLie y else 
    if generalExpressionLie y then weightLie((y_1)_0) else
    (print(y); print("is not a general Lie expression")));


--Documentation
beginDocumentation()
load "./GradedLieAlgebras/doc.m2"
load "./GradedLieAlgebras/doc2.m2"
load "./GradedLieAlgebras/tut.m2"
load "./GradedLieAlgebras/tut2.m2"
load "./GradedLieAlgebras/symm.m2"
load "./GradedLieAlgebras/how.m2"
load "./GradedLieAlgebras/diff.m2"
load "./GradedLieAlgebras/cons.m2"

--Asserts
load "./GradedLieAlgebras/asserts.m2"

end

uninstallPackage "GradedLieAlgebras"
exit
path=append(path,"~/Dropbox/GradedLieAlgebras/")
installPackage "GradedLieAlgebras"
viewHelp "GradedLieAlgebras"
 

loadPackage"GradedLieAlgebras"



