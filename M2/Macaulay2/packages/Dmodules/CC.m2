------------------------------------------------
-- what is below used to be it.m2
------------------------------------------------

---------------------------------------------------------------------
-- gradient
grad = method()
grad RingElement := f -> (
     R := ring f; -- assume R = QQ[x_1,...,x_n,a_1...,a_n]
     n := numgens R // 2;
     transpose (jacobian ideal f)^{0..n-1}
     ) 
----------------------------------------------------------------------
-- processComponent(I,f) --
-- computes the cotangent bundle of a component Y=V(I) relative to f
----------------------------------------------------------------------
-- output: (the defining ideal J_f of the bundle, 
--          the defining ideal of the exceptional set where f|Y is not submersion)
-- caveat: the correctness is guaranteed ONLY for top-dimensional components 
--         of the projection of V(J_f) 
processComponent = method()
processComponent(Ideal, RingElement)  := (I,f) -> (
     R := ring f; -- assume R = QQ[x_1,...,x_n,a_1...,a_n]
     n := numgens R // 2;
     GRAD := grad f;
     Nspace := GRAD;
     scan(flatten entries gens I, g-> Nspace = Nspace||grad g);
     nRows := numgens target Nspace;
     d := codim I;
     if d == 0 then d = nRows - 1; -- should be the codim of V(I) 
     I0 := minors(d+1, Nspace);
     K := ker map(R^nRows/I, R^n, Nspace);
     J := I + ideal (matrix{{R_n..R_(2*n-1)}}*(gens K));
     satJf := saturate(J, I0);
     out := satJf + ideal f;
     (out,I0)
     );
----------------------------------------------------------------------
-- correctBundle(B,I,f,J) --
-- takes the bundle B computed for V(I) relative to f and 
-- "corrects" the component V(J) of B
-- output: ideal J' such that V(J')=V(J), but the multiplicity of J' is correct
correctBundle = method()
correctBundle List  := --(B,I,f,J) 
inp -> (
     pInfo(3, "Correcting: "|toString inp);
     B := inp#0; I := inp#1; f := inp#2; J := inp#3;
     R := ring f; -- assume R = QQ[x_1,...,x_n,a_1...,a_n]
     n := numgens R // 2;
     GRAD := grad f;
     Nspace := GRAD;
     scan(flatten entries gens I, --project radical J, ???  
	  g-> Nspace = Nspace||grad g);
     nRows := numgens target Nspace;
     Kf := ker map( R^nRows/(J
	       --+ideal f
	       ), R^n, Nspace);
     out  := --B + 
     ideal (matrix{{R_n..R_(2*n-1)}}*(gens Kf))
     );

---------------------------------------------------------------------
-- old cotanbun
cotanbun = method()
cotanbun RingElement := f -> (
     R := ring f; -- assume R = QQ[x_1,...,x_n,a_1...,a_n]
     processComponent(ideal map(R^0,R^0),f)
     )

---------------------------------------------------------------------
-- multiplicity -- 
-- given a Poincare polynomial for a module outputs its multiplicity
multiplicityPoincare := method()
multiplicityPoincare RingElement := f-> (
     R := ring f;
     while (f % (1-R_0) == 0) do f = f//(1-R_0);
     sub(f, {R_0=>1})
     );

---------------------------------------------------------------------
-- projection onto x_1..x_n
project = method()
project Ideal := I -> (
     R := ring I; -- assume R = QQ[x_1,...,x_n,a_1...,a_n]
     n := numgens R // 2;
     v := flatten entries vars R;
     PW :=  QQ(monoid [drop(v,n),take(v,n), MonomialOrder=>Eliminate n]);
     sub(ideal selectInSubring(1, gens gb sub(I,PW)), R)
     )

----------------------------------------------------------------------
-- get all the components
BMM = method()
BMM(Ideal, RingElement) := (I,f) -> (
     if BMMstashedCC#?(I,f) then return BMMstashedCC#(I,f);
     if f % I == 0 then return {};
     comps := {}; toCorrect := {};
     (B,I0) := processComponent(I,f);
     prd := primaryDecomposition B;
     dec := ass B; 
     scan(#dec, i->(
	 compI := project dec#i;      
	 projdec := drop(dec,{i,i})/project;
	 suspectedMultiplicity := multiplicityPoincare@@poincare prd#i // multiplicityPoincare@@poincare dec#i;
	 if suspectedMultiplicity > 1 and isSubset(I0,compI) -- and any(projdec, c->isSubset(c,compI)) 
	 then (
	      pInfo(3,"BMM: Component "|toString compI|" should be corrected");
	      toCorrect = toCorrect | {i};
	      ) 
	 else (  
	      comps = comps|{compI => suspectedMultiplicity};
	      )
	 ));
     while #toCorrect > 0 do (
	  i := first toCorrect; toCorrect = drop(toCorrect,1);
     	  compI := project dec#i;
	  pInfo(3,"Correcting component "|toString compI);
	  if isSubset(project I0, compI) then pInfo(3, "! it is in the exceptional set !");
	  -- it suffices to consider supercomponents... 
	  -- superComp := select(toList(0..#dec-1), j->isSubset(project dec#j,compI));
	  corB := correctBundle{
	       B, --intersect apply(superComp,j->prd#j),--...this is slower, though 
	       I,f,
	       project prd#i};
	  cordec := {dec#i}; -- try localization
	  pInfo(3, ass corB);
	  corprd := { localize(corB,dec#i) };
	  --corprd = primaryDecomposition corB; -- used to compute the whole primary decomposition ... is "localize" stable?
     	  --cordec = ass corB; 
	  j := position(cordec, J->project J==compI);
	  comps = comps|{compI => (multiplicityPoincare@@poincare corprd#j //  
		    multiplicityPoincare@@poincare cordec#j)}; 
	  );
     comps = {I=>1} | comps ;
     if BMMidealREMEMBER then BMMstashedCC#(I,f) = comps;
     return comps	  
     )
BMM(List,RingElement)  := (cc,f) -> (
     out := {};
     scan(cc, c->if f%c#0!=0 then (
	       comps := BMM(c#0,f);
	       scan(comps, comp->(
			 X := select(1,toList(0..#out-1),i->out#i#0==comp#0);
			 if #X==0 then out = out | {comp#0=>c#1*comp#1}
			 else (
			      i := first X;
			      out = take(out,i)|{comp#0=>c#1*comp#1+out#i#1}|drop(out,i+1);
			      )
			 ))  
	       ));
     return out
     )

------------------------------------------------------------------------------
-- By default, BMM procedure stashed the result
BMMidealREMEMBER = true;
BMMstashedCC = new MutableHashTable from {};
--------------------------------------------------------------------------
-- "Prunes" a pair of CCs, 
-- i.e: removes identical components (accounting for multiplicity)
pruneCC = method()
pruneCC(List,List) := (A,B) -> (
     nA := {};
     nB := B; 
     scan(A, c->(
	       icc := position(nB, cc -> cc#0==c#0);
	       if icc === null then nA = nA | {c}
	       else (
		    cc :=  nB#icc;
		    m := min(c#1,cc#1); -- take minimum of the multiplicities
		    if c#1-m > 0 then nA = nA | {c#0=>c#1-m};
		    if cc#1-m > 0 then nB = take(nB,icc)|{c#0=>cc#1-m}|take(nB,icc-#nB+1)
		    else nB = take(nB,icc)|take(nB,icc-#nB+1); -- take cc out
		    )
	       )); 
     return (nA,nB)
     ) 

--------------------------------------------------------------------------------
-- Local cohomology computation by "pruning" the CCs of Cech complex 
-- given by a mutable hashtable B
---------------------------------------
-- output: none (B is modified)
pruneCechComplexCC = method() 
pruneCechComplexCC(MutableHashTable) := B -> (
     ind := keys B;
     n := max(ind/(i->#i));
     scan(n, j->scan(ind, a->(
		    if not (set a)#?j then (
			 aj := sort (a|{j});
			 NEWaj := pruneCC(B#a, B#aj);
			 B#a = NEWaj#0; 
			 B#aj = NEWaj#1;
			 )
		    )) ) 
     )

-------------------------------------------------------------------------------
-- computes CCs of M_{f_{i_1},...,f_{i_k}} -- the parts of the Cech complex --
-- and stores them as entries in a MutableHashTable 
------------------------------------------------------------------------------
populateCechComplexCC = method()
populateCechComplexCC(Ideal,List) := (I,cc) -> ( 
     BMMstashedCC = new MutableHashTable from {};
     n := numgens I;
     ssets := (toList subsets set(0..n-1)) / sort@@toList; -- subsets of {0,...,n-1}
     inds := {}; -- nonempty subsets of {0,1,...,n-1} 
     scan(n, i->inds = inds | select(ssets, s->#s==i+1));
     B := new MutableHashTable from {{}=>cc};
     scan(inds, i -> (
     	       J := B#{}#0#0;	  
     	       f := I_(last i);
     	       if #i>1 then J = B#(drop(i,-1)); 
     	       B#i = BMM(J,f);
     	       pInfo(2, "localization " | toString i | ": " | toString B#i);
     	       )); 
     return B
)



TEST ///
------------------------------------
-- Brianson-Maisonobe-Merle formula
S = QQ[x,y,z,a,b,c]
cc = BMM({ideal S=>1},x^3+y^3+z^3)
assert(cc === {ideal S=>1, ideal(x^3+y^3+z^3)=>1, ideal(z,y,x)=>4})
///

