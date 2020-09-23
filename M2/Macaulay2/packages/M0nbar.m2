newPackage(
     "M0nbar",
     Version => "0.3",
     Date => "April 5, 2014",
     Authors => {
	  {Name => "Han-Bom Moon",
	   Email => "hmoon8@fordham.edu"},
          {Name=> "Dave Swinarski", 
	   Email=> "dswinarski@fordham.edu",
	   HomePage=>"http://faculty.fordham.edu/dswinarski"}
	  },
     Keywords => {"Commutative Algebra"},
     Headline => "calculations for divisors and F-curves on the moduli space of stable n-pointed genus zero curves"
     )
export {"CurveClassRepresentativeM0nbar",
	"DivisorClassRepresentativeM0nbar",
 	"curveClassRepresentativeM0nbar",
	"divisorClassRepresentativeM0nbar", 
	"intersection",
	"negativeSum",
	"isEffectiveExpression",
        "nonadjacentBasis",
	"writeDivisorInNonadjacentBasis",
	"writeCurveInDualNonadjacentBasis",
	"isEquivalent",
	"singletonSpineBasis",
	"writeCurveInSingletonSpineBasis",
	"KeelRelationAmongCurves",
	"seekEffectiveExpression",
	"FcurvesOfGivenShape",
	"Fcurves",
	"permute"
       	} 
     

-*
We define two types that are used by the M0nbar package:
CurveClassRepresentativeM0nbar
DivisorClassRepresentativeM0nbar

Both are hash tables.

A curve class is just a linear combination of F-curves.  
Proposed functions for curve classes include
curveClassRepresentativeM0nbar
+
-
scalar multiplication
writeInDualNonadjacentBasis
writeInSingletonSpineBasis
seekEffectiveExpression
isEquivalent
tex



A divisor class is a linear combination of boundary divisors
Proposed functions for divisor classes include
divisorClassRepresentativeM0nbar
+
-
scalar multiplication
nonadjacentBasis
writeInNonadjacentBasis
isEquivalent
tex
+
-
scalar multiplication

----Left out for now: 
----create the Chow ring of M0nbar
----write expressions equivalent to zero in Rulla's basis of relations
*-



--------------------------------------------
--------------------------------------------
-- Creation, checking, and texing 
--------------------------------------------
--------------------------------------------



CurveClassRepresentativeM0nbar = new Type of HashTable 

DivisorClassRepresentativeM0nbar = new Type of HashTable 




--Not exported
--The following function checks to make sure that all the labels 
--appearing are in {1,...,n} in case you try to do something 
--stupid like create $\delta_{\{1,7\}}$ on $\bar{M}_{0,6}$
isValidDivisorExpression = (n,H)->(
    if n<=3 then error "n should be greater than or equal to 4";
    S:=apply(n, i -> i+1);
    K:=keys H;
    for i from 0 to #K-1 do (
        if isSubset(set(K_i),S) == false then return false        
    );    
    return true
)


--Not exported
--The following function checks to make sure that the input
--is a partition of {1,...,n} into four nonempty subsets
isValidCurveExpression = (n,H)->(
    if n<=3 then error "n should be greater than or equal to 4";
    S:=apply(n, i -> i+1);
    K:=keys H;
    for i from 0 to #K-1 do (
        if #(K_i) != 4 then (
	    print concatenate("The expression ",toString(K_i)," does not have four subsets");
	    return false
	);  
        if sort(flatten(K_i)) != S then (
	    print concatenate("The expression ",toString(K_i)," is not a partition of {1,...,n}");
	    return false
	);
    );
    return true
);


--The following function first checks to make sure that the input expression makes sense
--It sorts the labels
--It searches for like terms and combines them if there are
--It removes zeroes
--It creates an object of type DivisorClassRepresentativeM0nbar
divisorClassRepresentativeM0nbar = method(
    TypicalValue=>DivisorClassRepresentativeM0nbar    
);
divisorClassRepresentativeM0nbar(ZZ,HashTable) := (n,H) -> (
    if isValidDivisorExpression(n,H)== false then error "The divisor expression is invalid";
    p:=pairs(H);
    T:=new MutableHashTable from {};
    k:={};
    for i from 0 to #p-1 do (
	k=sort(p_i_0);
	if #(p_i_0) > n/2 then k=sort(toList(set(apply(n, i -> i+1))-set(k)));
	if even(n) and #(p_i_0) == lift(n/2,ZZ) and isSubset(set({1}),p_i_0)==false then k=sort(toList(set(apply(n, i -> i+1))-set(k)));
	if T#?k then T#k=T#k+p_i_1 else T#k=p_i_1
	);
    T=pairs(T);
    T=apply(#T, i -> if T_i_1 !=0 then T_i);
    T=delete(null,T); 
    T=new HashTable from T;
    return new DivisorClassRepresentativeM0nbar from {"NumberOfMarkedPoints"=>n,"DivisorExpression"=> T}
);

--Users requested that they be able to create divisor classes from a list instead of a hash table
--Internally I always create them from hash tables
divisorClassRepresentativeM0nbar(ZZ,List) := (n,L) -> (
    K:=unique apply(#L, i -> (L_i)#0);
    H:=new MutableHashTable from apply(#K, i -> K_i => 0);
    for i from 0 to #L-1 do (
	H#((L_i)#0) = H#((L_i)#0) + (L_i)#1 
	);
    return divisorClassRepresentativeM0nbar(n,new HashTable from pairs H)
);


--Put extra backslashes in front of the curly braces 
--that go around a set in tex
--Not exported
texSet = (S) -> (
    s:=toString(S);
    s=substring(1,#s-2,s);
    s=concatenate(///\{///,s,///\}///)    
)

texPartition = (P) -> (
    s:=texSet(P_0);
    for i from 1 to #P-1 do (
        s=concatenate(s,", ",texSet(P_i))
    );
    return s    
)


tex DivisorClassRepresentativeM0nbar := D -> (
    H:=D#"DivisorExpression";    
    P:=pairs(H);
    s:="";
    if P_0_1==1 then ( 
	s=concatenate(///\delta_{///,texSet(P_0_0),"}")
    ) else (
    s=concatenate(toString(P_0_1),///\delta_{///,texSet(P_0_0),"}")
    );
    for i from 1 to #P-1 do (
        if P_i_1==1 then ( 
	    s=concatenate(s,/// + \delta_{///,texSet(P_i_0),"}")
        ) else if P_i_1==-1 then ( 
	    s=concatenate(s,/// - \delta_{///,texSet(P_i_0),"}")
        ) else if P_i_1 > 0 then (
            s=concatenate(s," + ",toString(P_i_1),///\delta_{///,texSet(P_i_0),"}")
        ) else if P_i_1 < 0 then (
	    s=concatenate(s," - ",toString(abs(P_i_1)),///\delta_{///,texSet(P_i_0),"}")
	);
    );
    return s
);

tex CurveClassRepresentativeM0nbar := C -> (
    H:=C#"CurveExpression";    
    P:=pairs(H);
    s:="";
    if P_0_1==1 then ( 
	s=concatenate(///F_{///,texPartition(P_0_0),"}")
    ) else (
    s=concatenate(toString(P_0_1),///F_{///,texPartition(P_0_0),"}")
    );
    for i from 1 to #P-1 do (
        if P_i_1==1 then ( 
	    s=concatenate(s,/// + F_{///,texPartition(P_i_0),"}")
        ) else if P_i_1 == -1 then ( 
	    s=concatenate(s,/// - F_{///,texPartition(P_i_0),"}")
        ) else if P_i_1 > 0 then (
            s=concatenate(s," + ",toString(P_i_1),///F_{///,texPartition(P_i_0),"}")
        ) else if P_i_1 < 0 then (
	    s=concatenate(s," - ",toString(abs(P_i_1)),///F_{///,texPartition(P_i_0),"}")
	);
    );
    return s
);

--not exported
toScreen = (C) -> (
    H:=C#"CurveExpression";    
    P:=pairs(H);
    s:="";
    if P_0_1==1 then ( 
	s=concatenate(///F_///,toString(P_0_0))
    ) else (
    s=concatenate(toString(P_0_1),///F_///,toString(P_0_0))
    );
    for i from 1 to #P-1 do (
        if P_i_1==1 then ( 
	    s=concatenate(s,/// + F_///,toString(P_i_0))
        ) else if P_i_1 == -1 then ( 
	    s=concatenate(s,/// - F_///,toString(P_i_0))
        ) else if P_i_1 > 0 then (
            s=concatenate(s," + ",toString(P_i_1),///F_///,toString(P_i_0))
        ) else if P_i_1 < 0 then (
	    s=concatenate(s," - ",toString(abs(P_i_1)),///F_///,toString(P_i_0))
	);
    );
    return s
);

--Not exported
--This function sorts the partitions in an F curve
--yielding a canonical way of writing each F curve
sortOneFCurve = (F) -> (
    sort apply(4, i -> sort(F_i))    
);


--The following function checks to make sure that the expression entered makes sense
--It sorts the labels
--It searches for like terms and combines them if there are
--It removes zeroes
curveClassRepresentativeM0nbar = method(
    TypicalValue=>CurveClassRepresentativeM0nbar    
);
curveClassRepresentativeM0nbar(ZZ,HashTable) := (n,H) -> (
    if isValidCurveExpression(n,H)== false then error "The curve expression is invalid";
        p:=pairs(H);
    T:=new MutableHashTable from {};
    k:={};
    for i from 0 to #p-1 do (
	k=sortOneFCurve(p_i_0);
	if T#?k then T#k=T#k+p_i_1 else T#k=p_i_1
	);
    T=pairs(T);
    T=apply(#T, i -> if T_i_1 !=0 then T_i);
    T=delete(null,T); 
    T=new HashTable from T;
    return new CurveClassRepresentativeM0nbar from {"NumberOfMarkedPoints"=>n,"CurveExpression"=> T}
);

curveClassRepresentativeM0nbar(ZZ,List) := (n,L) -> (
    K:=unique apply(#L, i -> (L_i)#0);
    H:=new MutableHashTable from apply(#K, i -> K_i => 0);
    for i from 0 to #L-1 do (
	H#((L_i)#0) = H#((L_i)#0) + (L_i)#1 
	);
    return curveClassRepresentativeM0nbar(n,new HashTable from pairs H)
);


FcurvesOfGivenShape = method(
    TypicalValue=>List
);
FcurvesOfGivenShape(ZZ,ZZ,ZZ,ZZ) := (a,b,c,d) -> (
    n:=a+b+c+d;
    S:=apply(n, i -> i+1);
    perms:=permutations(S);
    allcurves:=apply(#perms, i -> sort { sort(apply(a, j-> perms_i_j)),   sort(apply(b, j-> perms_i_(j+a))), sort(apply(c, j-> perms_i_(j+a+b))),  sort(apply(d, j-> perms_i_(j+a+b+c))) });
    ans:=unique allcurves;
    return apply(#ans, i -> curveClassRepresentativeM0nbar(n,new HashTable from {ans_i =>1}))
)

--Not exported
partitionsOfWidth4 = (n) -> (
    L:={};
    p:=partitions(n);
    for i from 0 to #p -1 do ( 
	if #(p_i)==4 then L = append(L,p_i) 
    );
    return L
)

Fcurves = method(
    TypicalValue=>List
);
Fcurves(ZZ) := (n) -> (
    pa:=partitionsOfWidth4(n);
    ans:={};
    for i from 0 to #pa-1 do (
        ans = append(ans,FcurvesOfGivenShape(pa_i_0,pa_i_1,pa_i_2,pa_i_3) )
    );
    ans=flatten ans
)


permuteList = (sigma,L) -> (
    return apply(#L, i -> sigma_(L_i -1))   
);


permute = method();
permute(List,CurveClassRepresentativeM0nbar) := (sigma,C) -> (
    n:=C#"NumberOfMarkedPoints";
    if sort(sigma) != apply(n, i -> i+1) then error "The number of elements in the permutation does not match the number of marked points";
    P:=pairs(C#"CurveExpression");    
    P=apply(#P, i -> {apply(#(P_i_0), j -> permuteList(sigma,P_i_0_j)),P_i_1});
    H:=new HashTable from P;
    return curveClassRepresentativeM0nbar(n,H)    
)


permute(List,DivisorClassRepresentativeM0nbar) := (sigma,D) -> (
    n:=D#"NumberOfMarkedPoints";
    if sort(sigma) != apply(n, i -> i+1) then error "The number of elements in the permutation does not match the number of marked points";
    P:=pairs(D#"DivisorExpression");    
    P=apply(#P, i -> {permuteList(sigma,P_i_0),P_i_1});
    H:=new HashTable from P;
    return divisorClassRepresentativeM0nbar(n,H)    
)


--------------------------------------------
--------------------------------------------
-- Vector space structure 
--------------------------------------------
--------------------------------------------


DivisorClassRepresentativeM0nbar + DivisorClassRepresentativeM0nbar := (D1,D2) -> (
    n1:=D1#"NumberOfMarkedPoints";
    n2:=D2#"NumberOfMarkedPoints";    
    if n1 != n2 then error ///Cannot add a divisor on \bar{M}_{0,n_1} to a divisor on \bar{M}_{0,n_2} if n_1 \neq n_2///;
    n:=n1;
    D1exp:=D1#"DivisorExpression";
    D2exp:=D2#"DivisorExpression";
    K1:=keys(D1exp);
    K2:=keys(D2exp);
    K:=unique flatten {K1,K2};
    P:={};
    for i from 0 to #K-1 do (
    	if D1exp#?(K_i) and D2exp#?(K_i) and D1exp#(K_i)+D2exp#(K_i) !=0 then P=append(P,{K_i, D1exp#(K_i)+D2exp#(K_i)});
	if D1exp#?(K_i) and D2exp#?(K_i)==false then P=append(P,{K_i, D1exp#(K_i)});
        if D1exp#?(K_i)==false and D2exp#?(K_i) then P=append(P,{K_i, D2exp#(K_i)});	
    );   
    return divisorClassRepresentativeM0nbar(n,new HashTable from P)
)

DivisorClassRepresentativeM0nbar - DivisorClassRepresentativeM0nbar := (D1,D2) -> (
    n1:=D1#"NumberOfMarkedPoints";
    n2:=D2#"NumberOfMarkedPoints";    
    if n1 != n2 then error ///Cannot add a divisor on \bar{M}_{0,n_1} to a divisor on \bar{M}_{0,n_2} if n_1 \neq n_2///;
    n:=n1;
    D1exp:=D1#"DivisorExpression";
    D2exp:=D2#"DivisorExpression";
    K1:=keys(D1exp);
    K2:=keys(D2exp);
    K:=unique flatten {K1,K2};
    P:={};
    for i from 0 to #K-1 do (
    	if D1exp#?(K_i) and D2exp#?(K_i) and D1exp#(K_i)-D2exp#(K_i) !=0 then P=append(P,{K_i, D1exp#(K_i)-D2exp#(K_i)});
	if D1exp#?(K_i) and D2exp#?(K_i)==false then P=append(P,{K_i, D1exp#(K_i)});
        if D1exp#?(K_i)==false and D2exp#?(K_i) then P=append(P,{K_i, -(D2exp#(K_i))});	
    );   
    return divisorClassRepresentativeM0nbar(n,new HashTable from P)
)

Number * DivisorClassRepresentativeM0nbar := (c,D) -> (
    n:=D#"NumberOfMarkedPoints";
    Dexp:=D#"DivisorExpression";
    P:=pairs(Dexp);
    P=apply(#P, i -> {P_i_0,c*(P_i_1)});
    return divisorClassRepresentativeM0nbar(n,new HashTable from P)
)

CurveClassRepresentativeM0nbar + CurveClassRepresentativeM0nbar := (C1,C2) -> (
    n1:=C1#"NumberOfMarkedPoints";
    n2:=C2#"NumberOfMarkedPoints";    
    if n1 != n2 then error ///Cannot add a curve on \bar{M}_{0,n_1} to a curve on \bar{M}_{0,n_2} if n_1 \neq n_2///;
    n:=n1;
    C1exp:=C1#"CurveExpression";
    C2exp:=C2#"CurveExpression";
    K1:=keys(C1exp);
    K2:=keys(C2exp);
    K:=unique flatten {K1,K2};
    P:={};
    for i from 0 to #K-1 do (
    	if C1exp#?(K_i) and C2exp#?(K_i) and C1exp#(K_i)+C2exp#(K_i) !=0 then P=append(P,{K_i, C1exp#(K_i)+C2exp#(K_i)});
	if C1exp#?(K_i) and C2exp#?(K_i)==false then P=append(P,{K_i, C1exp#(K_i)});
        if C1exp#?(K_i)==false and C2exp#?(K_i) then P=append(P,{K_i, C2exp#(K_i)});	
    );   
    return curveClassRepresentativeM0nbar(n,new HashTable from P)
)

CurveClassRepresentativeM0nbar - CurveClassRepresentativeM0nbar := (C1,C2) -> (
    n1:=C1#"NumberOfMarkedPoints";
    n2:=C2#"NumberOfMarkedPoints";    
    if n1 != n2 then error ///Cannot add a curve on \bar{M}_{0,n_1} to a curve on \bar{M}_{0,n_2} if n_1 \neq n_2///;
    n:=n1;
    C1exp:=C1#"CurveExpression";
    C2exp:=C2#"CurveExpression";
    K1:=keys(C1exp);
    K2:=keys(C2exp);
    K:=unique flatten {K1,K2};
    P:={};
    for i from 0 to #K-1 do (
    	if C1exp#?(K_i) and C2exp#?(K_i) and C1exp#(K_i)-C2exp#(K_i) !=0 then P=append(P,{K_i, C1exp#(K_i)-C2exp#(K_i)});
	if C1exp#?(K_i) and C2exp#?(K_i)==false then P=append(P,{K_i, C1exp#(K_i)});
        if C1exp#?(K_i)==false and C2exp#?(K_i) then P=append(P,{K_i, -(C2exp#(K_i))});	
    );   
    return curveClassRepresentativeM0nbar(n,new HashTable from P)
)

Number * CurveClassRepresentativeM0nbar := (c,C) -> (
    n:=C#"NumberOfMarkedPoints";
    Cexp:=C#"CurveExpression";
    P:=pairs(Cexp);
    P=apply(#P, i -> {P_i_0,c*(P_i_1)});
    return curveClassRepresentativeM0nbar(n,new HashTable from P)
)
--------------------------------------------
--------------------------------------------
-- Basic functions
--------------------------------------------
--------------------------------------------

--This function adds all the negative coefficients in 
--an expression.  It is a measure of how far from being
--effective the expression is
negativeSum = method(
    TypicalValue=>ZZ  
);
negativeSum(CurveClassRepresentativeM0nbar) := (C) -> (
    v:=values(C#"CurveExpression");
    sum apply(#v, i -> if v_i<0 then v_i else 0)         
);


--This function tests whether all the coefficients 
--in an expression are nonnegative
isEffectiveExpression = method(
    TypicalValue=>Boolean  
);
isEffectiveExpression(CurveClassRepresentativeM0nbar) := (C) -> (
    v:=values(C#"CurveExpression");
    return apply(#v, i -> v_i >=0) ==apply(#v, i -> true)     
);

isEffectiveExpression(DivisorClassRepresentativeM0nbar) := (D) -> (
    v:=values(D#"DivisorExpression");
    return apply(#v, i -> v_i >=0) ==apply(#v, i -> true)     
);


--Not exported
--The following function intersects a single F curve with a single boundary divisor
--Source: Keel and McKernan
FdotdeltaI = (C,I) -> (
    S:=flatten C;
    I= set(I);
    a:=set(C_0); b:=set(C_1); c:=set(C_2); d:=set(C_3);
    Ic:=set(S) - I;
    if I === a or I === b or I === c or I === d or Ic === a or Ic === b or Ic === c or Ic === d then return -1;
    if I === a+b or I === a+c or I === a+d or I === b+c or I === b+d or I === c+d then return 1;
    return 0
);




--Overloading intersect method, don't need to export
intersection = method(
    TypicalValue=>QQ 
);
intersection(CurveClassRepresentativeM0nbar,DivisorClassRepresentativeM0nbar):=(C,D) -> (
    PC:=pairs(C#"CurveExpression");
    PD:=pairs(D#"DivisorExpression");
    sum apply(#PD, i -> (PD_i_1)*(sum apply(#PC, j -> (PC_j_1)*FdotdeltaI(PC_j_0,PD_i_0))))  
);

intersection(DivisorClassRepresentativeM0nbar,CurveClassRepresentativeM0nbar):=(D,C) -> (
    intersection(C,D)     
);






--------------------------------------------
--------------------------------------------
-- Create the nonadjacent basis
--------------------------------------------
--------------------------------------------




--Not exported
--Draw an n-gon and label the vertices with the labels {1,...,n}
--Given a subset I in {1,...,n}, decide if the labels of I
--are adjacent 
isAdjacent = (I,n) -> (S:=apply(n, i -> i+1);
    Ic:=toList(set(S) - set(I));
    jumpcount:=0;
    for i from 1 to n-1 do (     
        if isSubset( set({ i }), set(I) ) ==true and isSubset( set({ i+1 }), set(Ic) ) ==true then jumpcount = jumpcount +1;
        if isSubset( set({ i }), set(Ic) ) ==true and isSubset( set({ i+1 }), set(I) ) ==true then jumpcount = jumpcount +1;
        if jumpcount >= 4 then return false;
    );
    if isSubset( set({ n }), set(I) ) ==true and isSubset( set({ 1 }), set(Ic) ) ==true then jumpcount = jumpcount +1;
    if isSubset( set({ n }), set(Ic) ) ==true and isSubset( set({ 1 }), set(I) ) ==true then jumpcount = jumpcount +1;
    if jumpcount >= 4 then return false;
    return true
)

--Not exported
--Give a list of all the subsets I in {1,...,n} 
--with #I==n/2 and 1 \in I.
subshalfn = (n) -> (
      S := apply(n, i -> i+1);
      s := sort subsets(S,lift(n/2,ZZ));
      L :={};
      for i from 0 to #s-1 do (
      if isSubset(set({1}),set(s_i)) == true then L = append(L, s_i)      );
     return L
);

--Create the nonadjacent basis
--This function creates the internal representation 
--i.e. these are just subsets I_j in {1,...,n} 
--not objects of class DivisorClassRepresentativeM0nbar
--Not exported
nonadjbasis = (n) -> (
     f:=floor(n/2);
     S := apply(n, i -> i+1);
     subs := {};
     for i from 2 to f-1 do subs = append(subs, sort subsets(S,i) );
     if odd(n) == true then subs = append(subs, sort subsets(S,f) );
     if even(n) == true then subs = append(subs, subshalfn(n));      
     subs = flatten subs;
     nonadj := {};
     for i from 0 to #subs -1 do (
	  if isAdjacent(subs_i,n) == false then nonadj = append(nonadj, subs_i) 
	  );
     return nonadj
);


--Exported
nonadjacentBasis = method(
    TypicalValue=>List   
);
nonadjacentBasis(ZZ) := memoize( (n) -> (
     nonadj := nonadjbasis(n);
     orderednonadj := {};
     pelements :={};
     for p from 2 to floor(n/2) do (
	 pelements={}; 
         for j from 0 to #nonadj-1 do (
	    if #(nonadj_j) == p then pelements = append(pelements, nonadj_j) 
	      );
	 pelements=sort pelements;
	 orderednonadj = append(orderednonadj,pelements));	  
     B:=flatten orderednonadj;
     B=apply(#B, i -> divisorClassRepresentativeM0nbar(n,new HashTable from {{B_i,1}}));
     return B	 	  
));



--------------------------------------------
--------------------------------------------
-- Write divisors in the nonadjacent basis
--------------------------------------------
--------------------------------------------

--Order a regular n-gon with the labels 1,...,n
--Given a subset I in {1,...,n}, 
--give the decomposition of I into blocks where the 
--labels are adjacent in the n-gon, followed by gaps
--in between blocks
--Not exported
blockGapDecomposition = (I,n) -> (
    S:=apply(n, i -> i+1);
    Ic:=toList(set(S) - set(I));
    L:={};
    Lc:={};
    if isSubset( set({ 1 }), set(I) ) ==true then ( 
        L = I;
        Lc = Ic
    );
    if isSubset( set({ 1 }), set(Ic) ) ==true then ( 
        L = Ic;
        Lc = I);
    blocks:={};
    gaps:={};
    blockszero:={};
    currentset := {1};
    for i from 2 to n do (     
        if isSubset( set({ i }), set(L) ) ==true and isSubset( set({ i-1 }), set(L) ) ==true then currentset = append(currentset, i);
        if isSubset( set({ i }), set(Lc) ) ==true and isSubset( set({ i-1 }), set(L) ) ==true then (
            blocks = append(blocks, currentset);
            currentset = {i}
	);
        if isSubset( set({ i }), set(Lc) ) ==true and isSubset( set({ i-1 }), set(Lc) ) ==true then currentset = append(currentset, i);
        if isSubset( set({ i }), set(L) ) ==true and isSubset( set({ i-1 }), set(Lc) ) ==true then (
            gaps = append(gaps, currentset);
            currentset = {i}
	)
    );
    if isSubset( set({ n }), set(Lc) ) ==true then gaps = append(gaps,currentset);
    if isSubset( set({ n }), set(L) ) ==true then (
        blockszero = blocks_0;
        blocks = drop(blocks,{0,0});
        blockszero = flatten { blockszero, currentset}; 
        blocks = prepend(blockszero, blocks)
    );  
    return {blocks, gaps}
);


--Here we order the nonadjacent basis by the # of blocks
--Not exported
nonadjacentBasisOrderedByNumberOfBlocks = (n) -> (
     nonadj := nonadjbasis(n);
     nonadj = apply(#nonadj, i -> blockGapDecomposition(nonadj_i,n) );
     ordnonadj := {};
     pelements :={};
     for p from 2 to floor(n/2) do (
	 pelements={}; 
         for j from 0 to #nonadj-1 do (
	    if #(nonadj_j_0) == p then pelements = append(pelements, nonadj_j) 
	      );
	 ordnonadj = append(ordnonadj,pelements));	  
     return ordnonadj	 
)


--This function computes the coefficient on a nonadjacent basis element
--if it only has two blocks and two gaps
--Not exported
coefficientOnTwoBlockNonadjacentDivisor = (D,bgDI,n) -> (
     blocks := bgDI_0; 
     gaps := bgDI_1;
     H:=new HashTable from {{blocks_0,gaps_0, blocks_1, gaps_1}=>1};
     C:= curveClassRepresentativeM0nbar(n,H);	 
     return intersection(C,D)
);



--Given a subset I of {1,...,n} return a standard key 
--Useful for detecting if you have equivalent expressions for delta_I
--Not exported
standardKey = (I,n) -> (
    if #I < floor(n/2) then return sort(I);
    S := apply(n, i -> i+1);
    if #I > floor(n/2) then return sort(toList(set(S) - set(I) ));
    if even(n) == true and #I == lift(n/2,ZZ) and isSubset(set({1}), set(I)) == true then return sort(I);
    if even(n) == true and #I == lift(n/2,ZZ) and isSubset(set({1}), set(I)) == false then return sort( toList(set(S) - set(I) ) );
    if odd(n) == true and #I == floor(n/2) then return sort(I)
)     
     
     

--Given the block-gap decomposition bgDI of a subset I \subset \{1,...,n\}, 
--with I nonadjacent, I with p blocks, 
--and given the coefficients N for all H with <= p-1 blocks, 
--return the coefficient on delta_I when D is written in the nonadj basis
--Reference: Carr p. ?
nonadjacentBasisCoefficient = (D, bgDI, n, N, p) -> (
     blocks := bgDI_0;
     gaps := bgDI_1;
     S:=apply(n, i-> i+1);
     truncb := flatten drop(blocks,{p-1,p-1});
     truncg := flatten drop(gaps,{p-1,p-1});
     J:= flatten append(drop(gaps,{p-1,p-1}),blocks_(p-1));
     H := new HashTable from {{truncb, truncg, blocks_(p-1), gaps_(p-1)}=>1};
     C:=curveClassRepresentativeM0nbar(n,H);
     dtruncb := N#(standardKey(truncb,n));
     dtruncg := N#(standardKey(truncg,n));
     dj := N#(standardKey(J,n));
     return intersection(C,D) + dtruncb + dtruncg - dj
);     



--Write a divisor D in the nonadjacent basis
writeDivisorInNonadjacentBasis = method(
    TypicalValue=>DivisorClassRepresentativeM0nbar
);
writeDivisorInNonadjacentBasis(DivisorClassRepresentativeM0nbar) := (D) -> ( 
    n:=D#"NumberOfMarkedPoints";
    L:=nonadjacentBasisOrderedByNumberOfBlocks(n);
    N := new MutableHashTable;
    p:=2;
    scan(#(L_(p-2)), i -> N#(standardKey(flatten(L_(p-2)_i_0),n)) = coefficientOnTwoBlockNonadjacentDivisor(D,L_(p-2)_i,n) );
    for p from 3 to floor(n/2) do (
	scan(#(L_(p-2)), i -> N#(standardKey(flatten(L_(p-2)_i_0),n)) = nonadjacentBasisCoefficient(D,L_(p-2)_i,n,N,p) )
    );
    return divisorClassRepresentativeM0nbar(n,new HashTable from pairs(N))
);


writeCurveInDualNonadjacentBasis = method(
    TypicalValue=>List
);
writeCurveInDualNonadjacentBasis(CurveClassRepresentativeM0nbar) := (C) -> (
    n:=C#"NumberOfMarkedPoints";
    NAB:=nonadjacentBasis(n);
    return apply(#NAB, i -> intersection(C,NAB_i))    
);


isEquivalent = method(
    TypicalValue=>Boolean
);
isEquivalent(CurveClassRepresentativeM0nbar,CurveClassRepresentativeM0nbar) := (C1,C2) -> (
    n1:=C1#"NumberOfMarkedPoints";
    n2:=C2#"NumberOfMarkedPoints";
    if n1 != n2 then error ///The curves are on \bar{M}_{0,n_1} and \bar{M}_{0,n_2} with n_1 \neq n_2///;
    n:=n1;
    v1:=writeCurveInDualNonadjacentBasis(C1);
    v2:=writeCurveInDualNonadjacentBasis(C2);
    return v1==v2
);


isEquivalent(DivisorClassRepresentativeM0nbar,DivisorClassRepresentativeM0nbar) := (D1,D2) -> (
    n1:=D1#"NumberOfMarkedPoints";
    n2:=D2#"NumberOfMarkedPoints";
    if n1 != n2 then error ///The divisors are on \bar{M}_{0,n_1} and \bar{M}_{0,n_2} with n_1 \neq n_2///;
    n:=n1;
    E1:=writeDivisorInNonadjacentBasis(D1);
    E2:=writeDivisorInNonadjacentBasis(D2);
    return E1===E2
);


--------------------------------------------
--------------------------------------------
-- Singleton spine basis of F-curves
--------------------------------------------
--------------------------------------------

--Create all the subsets of {1,...,n} of size k 
--containing 1
--Not exported 
subswith1 = (n,k) -> (
    S:= apply(n-1, i -> i+2);
    s:= subsets(S,k-1);
    return apply(#s,i -> prepend(1,s_i))
);  


--Writes k mod n if -n < k < 2n
--Not exported
mymod = (n,k) -> ( if k < 1 then return n+k;
    if k> n then return k-n;
    return k
)


singletonSpineBasis = method(
    TypicalValue=>List
);
singletonSpineBasis(ZZ) := memoize ((n) -> (
    if n<7 then error "The singleton spine basis is only a basis for n>=7";
    subsk:={};
    A:={};
    g:=0;
    b:=0;
    I2:={};
    I3:={};
    I4:={};
    if even(n) then g=lift((n-2)/2,ZZ) else g=lift((n-3)/2,ZZ);
    S:=apply(n, i ->i+1);
    for i from 1 to n do (
        if even(n) and odd(g) then b=lift((g+1)/2,ZZ);
        if even(n) and even(g) then b=lift(g/2,ZZ);  
        if odd(n) and odd(g) then b=lift((g+1)/2,ZZ);
        if odd(n) and even(g) then b=lift(g/2+1,ZZ);        
        I2=sort apply(b, j -> mymod(n,i-j-1));
        I3=sort apply(b, j -> mymod(n,i-j-1-b));
        I4=sort toList(set(S)-set(I2)-set(I3)-set({i}));
        A=append(A,{{i},I2,I3,I4})     
    );
    s:={};
    sc:={};
    for k from 3 to floor(n/2) do (
        if k==n/2 then subsk=subswith1(n,floor(n/2)) else subsk=subsets(S,k);
        for p from 0 to #subsk-1 do (
        s=sort(subsk_p);     
        sc=sort toList(set(S)-set(s));
        A = append(A, {{s_0}, {s_1}, apply(#s-2,i-> s_(i+2)),sc})
        )
    );
    A=sort unique apply(#A, i -> sortOneFCurve(A_i));
    if #A != (2^(n-1)-binomial(n,2)-1) then error "The code gave the wrong number of elements in a basis";
    return apply(#A, i-> curveClassRepresentativeM0nbar(n,new HashTable from {A_i=>1}))
))





--Given a list of intersection numbers with the nonadjacent basis divisors,
--return the coefficients in the singleton spine basis.
--Exported
writeCurveInSingletonSpineBasis = method(
    TypicalValue=>CurveClassRepresentativeM0nbar
);
writeCurveInSingletonSpineBasis(ZZ,List) := (n,L) -> (
    NAB:=nonadjacentBasis(n);
    SSB:=singletonSpineBasis(n);
    M:=matrix apply(#NAB, i -> apply(#SSB, j -> (1/1)*intersection(NAB_i,SSB_j)));
    v:=transpose matrix {L};
    w:=flatten entries((M^-1)*v); 
    H:=new HashTable from apply(#SSB, i -> {(keys((SSB_i)#"CurveExpression"))_0,w_i});
    return curveClassRepresentativeM0nbar(n,H)
);

writeCurveInSingletonSpineBasis(CurveClassRepresentativeM0nbar) := (C1) -> (
    n:=C1#"NumberOfMarkedPoints";
    L:=writeCurveInDualNonadjacentBasis(C1);
    NAB:=nonadjacentBasis(n);
    SSB:=singletonSpineBasis(n);
    M:=matrix apply(#NAB, i -> apply(#SSB, j -> (1/1)*intersection(NAB_i,SSB_j)));
    v:=transpose matrix {L};
    w:=flatten entries((M^-1)*v); 
    H:=new HashTable from apply(#SSB, i -> {(keys((SSB_i)#"CurveExpression"))_0,w_i});
    return curveClassRepresentativeM0nbar(n,H)
);



--------------------------------------------
--------------------------------------------
-- Make a curve class less negative
--------------------------------------------
--------------------------------------------


--Given two F-curve lists
--get their common refinement
--Not exported
getCommonRefinement = (list1, list2) -> (
    S:= {};
    counter := 0;
    myIntersection:={};
     for i from 0 to 3 do (
        for j from 0 to 3 do (
            if set(list1_i) === set(list2_j) then counter = counter + 1;
            myIntersection = sort toList(set(list1_i) * set(list2_j));
            if myIntersection != {} then S=append(S,myIntersection);
        );
    );
    return {S, counter}
);


--Given two F-curve lists
--decide if they are adjacent
--Not exported
isAdjacentList = (list1,list2) -> (
    P:=getCommonRefinement(list1,list2);
    commonRef:=P_0;
    overlap:=P_1;
    return (#commonRef ==5 and overlap==2) 
);

--Given two F-curve lists
--decide if they appear in the same Keel relation
--Not exported
isInSameRelation = (list1,list2) -> (
    P:=getCommonRefinement(list1,list2);
    commonRef:=P_0;
    overlap:=P_1;
    return (#commonRef ==5 and overlap==1)
)

--Given an F-curve that appears with a negative coefficient
--find all the F-curves with positive coefficients that are adjacent to it
--Not exported
getAdjacentCurves = (negativeCurve, positiveCurves) -> (
    adjacentList:={};
    ll:={};
    for l from 0 to #positiveCurves-1 do (
        if isAdjacentList(negativeCurve,positiveCurves_l) then (
            ll={};
	    for i from 0 to 3 do ll=append(ll,sort (positiveCurves_l_i));
	    adjacentList=append(adjacentList,sort ll)	   
        );	   
    );       
    return adjacentList    
);

--Given two F-curve lists
--find any common parts
--Not exported
getCommonPartitions = (list1, list2) -> (
    S:={};    
    for i from 0 to 3 do (
        for j from 0 to 3 do (
	    if set(list1_i) === set(list2_j) then S=append(S,list1_i);
        );
    );
    return S;    
);

getNewCurve = (list1, list2, commonPartition) -> (
    S:={};
    myIntersection:={};
    for i from 0 to 3 do (
	for j from 0 to 3 do (
	    myIntersection = sort toList(set(list1_i)*set(list2_j));
	    if myIntersection != {} then S=append(S,myIntersection)
	)
    );
    T:={};
    counter:=-1;    
    reserveSet:={};
    for l from 0 to #list2-1 do (
        if set(list2_l) === set(commonPartition) then T=append(T,sort(list2_l)) else (
	    for m from 0 to #S-1 do (
		if set(list2_l) =!= set(S_m) and isSubset(set(S_m),set(list2_l)) then T=append(T,S_m);
		if set(list2_l) === set(S_m) then (
		    if counter == -1 then (counter = 0; reserveSet=S_m) else T=append(T,sort toList(set(list2_l)+set(reserveSet)))
	        )
	    )	
        )
    );	
    return sort T
);



isFinalStage = (listCurve) -> (
    l:=#listCurve;
    for i from 0 to l-1 do (
	for j from i+1 to l-1 do (
	    if isInSameRelation(listCurve_i,listCurve_j) then return true
	)
    );
    return false        
);

getChainOfAdjacentCurves = (negativeCurve,positiveCurves,level) -> (
    history:=set {};
    commonPartition:={};
    newCurve:={};
    flag:={};
    print concatenate("input curve: ", toString(negativeCurve),", ",toString(level)) << endl;
    nextList := getAdjacentCurves(negativeCurve, positiveCurves);
    if isFinalStage(nextList) then (
	print concatenate("This is the final stage", toString(nextList)) << endl;
	return 1;
	);
    for l from 0 to #nextList-1 do (
	print concatenate("Adjacent curve: ",toString(nextList_l),", ",toString(level)) << endl;
	commonPartition=getCommonPartitions(negativeCurve,nextList_l);
	for i from 0 to #commonPartition-1 do (
	    print concatenate("Using common partition ",toString(commonPartition_i)) << endl;
	    newCurve=getNewCurve(negativeCurve,nextList_l,commonPartition_i);
	    if isSubset(set({newCurve}),history) then continue else (
	    history = history + set({newCurve});
	    flag=getChainOfAdjacentCurves(newCurve, positiveCurves,level+1);
	    if flag == 1 then return 1);
	)
    );    
    return 0;	
);
  


--enter C as a hash table
--enter negative curve as a list of four lists
--standardize everything so can track history


listOfPositiveCurves = (C) -> (
    CE:=C#"CurveExpression";
    p:=pairs(CE);    
    positiveCurves:={};
    for i from 0 to #p-1 do (
        if p_i_1 > 0 then positiveCurves = append(positiveCurves,p_i_0)    
    );
    return positiveCurves    
);

listOfNegativeCurves = (C) -> (
    CE:=C#"CurveExpression";
    p:=pairs(CE);    
    negativeCurves:={};
    for i from 0 to #p-1 do (
        if p_i_1 < 0 then negativeCurves = append(negativeCurves,p_i_0)    
    );
    return negativeCurves    
);



getEdges = (negativeCurves,positiveCurves) -> (
edges:={};
listOfAdjacentCurves:={};
for h from 0 to #negativeCurves-1 do (
    listOfAdjacentCurves = getAdjacentCurves(negativeCurves_h,positiveCurves);
    for i from 0 to #listOfAdjacentCurves-1 do (
        commonPartitions:=getCommonPartitions(negativeCurves_h,listOfAdjacentCurves_i);
        for j from 0 to #commonPartitions-1 do (
	    edges=append(edges,{negativeCurves_h,listOfAdjacentCurves_i,commonPartitions_j})    
        );
    );
);
return edges
);


verticesOfLevelLessThanL = (H, l) -> (
    answer:={};
    L:={};
    for i from 0 to l do (
        L=H#i;
        for j from 0 to #L-1 do (
	    answer=append(answer,(L_j)#"CurveClassRepresentative")        
        );        
    );
    return answer
);





--Given a partition of {1,...,n} into 5 nonempty subsets, form the Keel relation:
-- F_{I1,I2,I3,I4 cup I5} + F_{I1 cup I2,I3,I4,I5} - F_{I1,I4,I3,I2 cup I5} - F_{I1 cup I4, I3, I2, I5}
--Exported

KeelRelationAmongCurves = method(
    TypicalValue=>CurveClassRepresentativeM0nbar    
);
KeelRelationAmongCurves(List) := (P) -> (
    n:=#(flatten P);
    I1:=P_0;
    I2:=P_1;
    I3:=P_2;
    I4:=P_3;
    I5:=P_4;
    L:={};
    L=append(L, { {I1,I2,I3,flatten {I4,I5} } , 1} );
    L=append(L, { {flatten {I1,I2} ,I3, I4, I5 } , 1} );
    L=append(L, { {I1,I4,I3,flatten {I2,I5} } , -1} );
    L=append(L, { {flatten {I1,I4} ,I2,I3, I5 } , -1} );
    H:=new HashTable from L;
    return curveClassRepresentativeM0nbar(n,H)
)

--Given input, adjacent, and commonPartition
--output the Keel relation determined by these
--Not exported
edgeToKeelRelation = (edge) -> (
    inputCurve:=edge_0;
    adjacentCurve:=edge_1;    
    commonPartition:=edge_2;
    S:=getCommonRefinement(inputCurve,adjacentCurve);
    S=S_0;
    I3:=edge_2;
    I2:={};
    I4:={};
    I5:={};
    I1:=flatten toList(set(inputCurve)*set(adjacentCurve)-set({commonPartition}));   
    S245:=toList(set(S)-set({I3})-set({I1}));
    for i from 0 to 2 do (
        if isSubset({S245_i},set(inputCurve)) and isSubset({S245_i},set(adjacentCurve)) ==false then I2=S245_i;
        if isSubset({S245_i},set(inputCurve))==false and isSubset({S245_i},set(adjacentCurve))  then I4=S245_i;
        if isSubset({S245_i},set(inputCurve))==false and isSubset({S245_i},set(adjacentCurve)) ==false then I5=S245_i;     
    );
    return KeelRelationAmongCurves({I1,I2,I3,I4,I5})
);



--Given a new expression
--Look to see if it is on the list of known expressions
isKnown = (C,knownVertices) -> (
    for i from 0 to #knownVertices -1 do (
        if C === knownVertices_i then return true    
    );    
    return false    
);


--Given part of the directed acyclic rooted graph of expressions MS-equivalent to E_0
--compute the next level of the graph
--that is, expressions that can be obtained from E_0 by adding l+1 Keel relations but not l Keel relations
--Not exported
computeNextLevel = (H) -> (
    K:=keys(H);
    l:=max(K);
    --Declare variables
    C:={};
    newC:={};
    newPath:={};
    negativeCurves:={};
    positiveCurves:={};   
    edges:={};
    levelLVertices:=H#l;
    levelLPlusOneVertices:={};
    knownVertices:=verticesOfLevelLessThanL(H,l);
    --main loop
    for i from 0 to #levelLVertices-1 do (
        C=(levelLVertices_i)#"CurveClassRepresentative";    
        negativeCurves=listOfNegativeCurves(C);
        positiveCurves=listOfPositiveCurves(C);   
        edges=getEdges(negativeCurves,positiveCurves);    
	--Now follow each new edge and see if you get to a new expression
        for j from 0 to #edges-1 do (
            newC=C + edgeToKeelRelation(edges_j);
	    --ignore if you get something more negative
            if negativeSum(newC) < negativeSum(C) then continue;
	    --ignore if you get something you've seen before
            if isKnown(newC, knownVertices) then continue;
            knownVertices=append(knownVertices,newC);
            newPath=append((levelLVertices_i)#"path",edgeToKeelRelation(edges_j));
            if negativeSum(newC) > negativeSum(((H#0)_0)#"CurveClassRepresentative") then (
	        print concatenate("New sum of negative coefficients: ",toString(negativeSum(newC))) << endl;
		print concatenate("New curve expression: ",toString(newC)) << endl;
		print "Keel relations added: " << endl;
		for j from 0 to #newPath-1 do (
		    print concatenate("    ",toScreen(newPath_j)) << endl;    
		);
		tp:=append( ((H#0)_0)#"TotalPath",newPath);
                H=startingVertex(newC,tp);
		return H
	    );		
            levelLPlusOneVertices=append(levelLPlusOneVertices,new HashTable from {"CurveClassRepresentative"=>newC,"path"=>newPath});   
        );   
    );
    H#(l+1) = levelLPlusOneVertices;
    print concatenate("Computed level ",toString(l+1)) << endl;
    return H        
);


--Given a curve class representative and a previous total path
--start the search anew from this representative, with this total path
startingVertex = (CR,tp) -> (
    return new MutableHashTable from {
        0 => {new HashTable from {"CurveClassRepresentative"=>CR, "path"=>{}, "TotalPath"=>tp}}
    }    
)

seekEffectiveExpression = method(
    TypicalValue=>HashTable  
);
seekEffectiveExpression(CurveClassRepresentativeM0nbar) := (C) -> (
    H:=startingVertex(C,{});
    c:=negativeSum(((H#0)_0)#"CurveClassRepresentative");
    while c<0 do (
        H=computeNextLevel(H);
        c=negativeSum(((H#0)_0)#"CurveClassRepresentative");
    );
    H=(H#0)_0;
    return new HashTable from {"CurveClassRepresentative"=>H#"CurveClassRepresentative", "TotalPath"=>H#"TotalPath"}
);









--******************************************--
-- DOCUMENTATION     	       	    	    -- 
--******************************************--

beginDocumentation()

doc ///
    Key
        M0nbar
    Headline
        calculations for divisors and curves on the moduli space of stable n-pointed genus zero curves
    Description        
        Text
            This package contains two types for working with divisor classes and curve classes on $\bar{M}_{0,n}$, the moduli space of stable n-pointed genus zero curves.  
	    The basic types are DivisorClassRepresentativeM0nbar and CurveClassRepresentativeM0nbar.  
	   
        Example
            L1= { {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
            C=curveClassRepresentativeM0nbar(5,L1);
	    L2=new HashTable from { {{1,3},1}, {{1,4},1}};
	    D=divisorClassRepresentativeM0nbar(5,L2);
	    intersection(C,D)
///;



--------------------------------
-- Bibliography and background
--------------------------------

doc ///
    Key 
        "Bibliography"
    Headline
        Bibliography for the M0nbar package
    Description
        Text	
    
	     [Carr] Carr.   A polygonal presentation of $Pic(\bar{M}_{0,n})$, arXiv:0911.2649.
	     [KM] Keel and McKernan.  Contractible extremal rays. p. 113-128 in {\it Handbook of Moduli, Vol. II.}  Higher Education \& International Press, Beijing-Boston, 2012.	     
	     [MS] Moon and Swinarski.  Effective curves on $\bar{M}_{0,n}$ from group actions.  Manuscripta Math.  147 (2015) 239-268.  
	     [S] Swinarski.  A basis of F-curves on $\bar{M}_{0,n}$, available at  http://faculty.fordham.edu/dswinarski/Fcurves/.
	    
     

///

doc ///
    Key 
        "Boundary divisors"
    Headline
        irreducible components of the boundary of the moduli space of stable n-pointed genus zero curves
    Description
        Text	
            Let $\Delta_I$ be the closure of the locus of curves with two irreducible components meeting at one node such that the marked points with labels in $I$ lie on the first component, and the marked points with labels in $I^c$ lie on the second component.  Then $\Delta_I$ is an irreducible effective divisor.
        Text
	    Let $\delta_I$ be the class of $\Delta_I$.  Then the classes $\{ \delta_I : \#I \geq 2, \#I \leq n/2, 1 \in I if \#I=n/2\}$ span the Picard group of $\bar{M}_{0,n}$.  The relations between these classes are called the Keel relations.
///


doc ///
    Key
        "F curves"
    Headline
        effective curves in the moduli space of stable n-pointed genus zero curves
    Description
        Text
            Let $P={P_0,P_1,P_2,P_3}$ be a partition of $\{1,...,n\}$ into four nonempty subsets.  Fix four (arithmetic) genus zero at worst nodal curves $C_j$ for  $j=0,1,2,3$, and $\#(P_j)$ marked points on each curve.  We call the curves $C_j$ the tails.  Mark one additional point $x_j$ on each tail. Next, consider $\mathbb{P}^1$ with four marked points, $y_0,...,y_3$; we call this the spine.  Glue the four tails to the spine by identifying $x_j$ and $y_j$.  Then, as the cross ratio of $y_0,...,y_3$ varies, we sweep out a curve $F_{P}$ in $\bar{M}_{0,n}$.
	    
	Text
            The homology class of $F_{P}$ only depends on the partition $P$, and not on the choice of the tails $C_j$ or the choices of marked points.  The classes of the F-curves span $H_2(\bar{M}_{0,n},Q)$.
///
--------------------------------
-- Documentation for classes
--------------------------------



doc ///
    Key
       CurveClassRepresentativeM0nbar
    Headline
        class implementing curves on the moduli space of stable n-pointed genus zero curves
    Description
        Text 
    	    This class represents curve classes in $ NS_1(\bar{M}_{0,n}) $.  A curve class is just a linear combination of F-curves.  
        Example
	    L= { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
	    curveClassRepresentativeM0nbar(5,L)               
///


doc ///
    Key
       DivisorClassRepresentativeM0nbar
    Headline
        class implementing divisors on the moduli space of stable n-pointed genus zero curves
    Description
        Text 
    	    This class represents divisor classes in $ NS^1(\bar{M}_{0,n}) $.  A divisor class is just a linear combination of the boundary divisors $ \delta_I $.	     
	Example
	    H=new HashTable from { {{1,3},1}, {{1,4},1} };
	    divisorClassRepresentativeM0nbar(6,H)                        
///


--------------------------------
-- Documentation for functions and methods
--------------------------------


--------------------------------
-- Documentation for creation functions
--------------------------------

doc /// 
    Key
        curveClassRepresentativeM0nbar
        (curveClassRepresentativeM0nbar,ZZ,List) 
    Headline
        creates an object of type CurveClassRepresentativeM0nbar
    Usage
        curveClassRepresentativeM0nbar(n,H)
    Inputs
        n:ZZ 
        H:List
    Outputs
         :CurveClassRepresentativeM0nbar
    Description 
        Text
	    This function creates an object of type CurveClassRepresentativeM0nbar.  Here is a basic example:
	Example   
	    L1= { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
	    curveClassRepresentativeM0nbar(5,L1)     
        Text    
	    The input can be a list or a hash table (see the documentation for (curveClassRepresentativeM0nbar,ZZ,HashTable)) .  The elements of the list should be pairs {{I1,I2,I3,I4},c}.  This will add $c F_{I_1,I_2,I_3,I_4}$ to the curve class expression.  Equivalently, you can type {I1,I2,I3,I4}=> instead of {{I1,I2,I3,I4},c}.
	Example
	    L2= { {{1,2},{3},{4},{5}}=>1, {{1,3},{2},{4},{5}}=>1 };
	    curveClassRepresentativeM0nbar(5,L2)
	Text
	    The function does some minimal testing to make sure the expression makes sense.  If you type "L2=new HashTable from \{ \{\{1,7\},\{2\},\{3\},\{4\}\} =>1 \}" and then run "curveClassRepresentativeM0nbar(6,L2)", you will get an error that "The curve expression is invalid."
	Text    
	    The function sorts the curve class labels.  If sorting creates like terms, they are combined:
	Example    
	    L3={ {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1}, {{{3},{4},{2,1},{5}},1}};
	    curveClassRepresentativeM0nbar(5,L3)
	Text
	    It deletes terms whose coefficient is zero.
        Example
	    L4={ {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1}, {{{3},{4},{2,1},{5}},-1}};
	    curveClassRepresentativeM0nbar(5,L4)
///

TEST /// 
   L= { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
   C=curveClassRepresentativeM0nbar(5,L);
   assert(set(keys(C#"CurveExpression")) === set({ {{1,2},{3},{4},{5}}, {{1,3},{2},{4},{5}} }))
///

TEST /// 
   L= { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
   C=curveClassRepresentativeM0nbar(5,L);
   assert(C#"NumberOfMarkedPoints" === 5)
///

doc /// 
    Key
        (curveClassRepresentativeM0nbar,ZZ,HashTable) 
    Headline
        creates an object of type CurveClassRepresentativeM0nbar
    Usage
        curveClassRepresentativeM0nbar(H)
    Inputs
        n:ZZ 
        H:HashTable
    Outputs
         :CurveClassRepresentativeM0nbar
    Description 
        Text
	    This function creates an object of type CurveClassRepresentativeM0nbar from a hash table.  Here is a basic example:
	Example   
	    H1=new HashTable from { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
	    curveClassRepresentativeM0nbar(5,H1)     
	Text 
	    Warning: when you enter a hash table in Macaulay2, if you use a key more than once, the first instance is discarded.   Here is an example where the behavior may differ from what you want:
        Example
	    H=new HashTable from { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1}, {{{1,2},{3},{4},{5}},2}}
	Text 
	    The user probably wanted $F_{\{1,2\},\{3\},\{4\},\{5\}} + 2F_{\{1,2\},\{3\},\{4\},\{5\}}$ to give $3F_{\{1,2\},\{3\},\{4\},\{5\}}$ instead.  So if your expression has two terms that are written exactly alike, you could either combine them before you create the input hash table, or input a list instead.
	Example
	    L= { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1}, {{{1,2},{3},{4},{5}},2}}
	    curveClassRepresentativeM0nbar(5,L)     
	Text 
	    For this reason, most users will probably prefer to enter curves via lists, rather than hash tables.  
        Text
	    The function curveClassRepresentative does the same minimal testing if you enter a hash table that it does if you enter a list.  See the documentation for curveClassRepresentativeM0nbar(ZZ,List).
///

TEST /// 
   H=new HashTable from { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
   C=curveClassRepresentativeM0nbar(5,H);
   assert(set(keys(C#"CurveExpression")) === set({ {{1,2},{3},{4},{5}}, {{1,3},{2},{4},{5}} }))
///

TEST /// 
   H=new HashTable from { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
   C=curveClassRepresentativeM0nbar(5,H);
   assert(C#"NumberOfMarkedPoints" === 5)
///



doc /// 
    Key
        divisorClassRepresentativeM0nbar
        (divisorClassRepresentativeM0nbar,ZZ,List) 
    Headline
        creates an object of type DivisorClassRepresentativeM0nbar
    Usage
        divisorClassRepresentativeM0nbar(n,H)
    Inputs
        n:ZZ
        L:List
    Outputs
         :DivisorClassRepresentativeM0nbar
    Description 
        Text
	    This function creates an object of type DivisorClassRepresentativeM0nbar.   Here is a basic example:
	Example   
	    L1= { {{1,3},1}, {{1,4},1} }
	    divisorClassRepresentativeM0nbar(6,L1)      
	Text    
	    The input can be a list or a hash table (see the documentation for (divisorClassRepresentativeM0nbar,ZZ,HashTable)) .  The elements of the list should be pairs {I,c}.  This will add $c \delta_{I}$ to the divisor class expression.  Equivalently, you can type I=>c instead of {I,c}.
	Example
	    L2= { {1,3}=>1, {1,4}=>1 }
	    divisorClassRepresentativeM0nbar(6,L2) 
	Text
	    The function divisorClassRepresentative does some minimal testing to make sure the expression makes sense.  For instance, if you type "L=\{ {1,7\}=>1 \}" and then run "divisorClassRepresentativeM0nbar(6,L)" you will get an error that "The divisor expression is invalid."
	Text    
	    The function sorts the divisor class labels.  If sorting creates like terms, they are combined:
	Example    
	    L3={ {{1,3},1}, {{1,4},1}, {{3,1},1} };
	    divisorClassRepresentativeM0nbar(6,L3)
	Text 
	    If $\#I^c < \# I$ the function will replace $\delta_I$ by $\delta_{I^c}$.  
	Example    
	    L4= { {{1,3},1}, {{1,4},1}, {{2,4,5,6},1} };
	    divisorClassRepresentativeM0nbar(6,L4)     
	Text    
	    If $\#I = n/2$ and 1 is not in $I$ it will replace $\delta_I$ by $\delta_{I^c}$.  
        Example
	    L5= { {{1,3},1}, {{1,4},1}, {{4,5,6},1} };
	    divisorClassRepresentativeM0nbar(6,L5)
	Text
	    It deletes terms whose coefficient is zero.
        Example
	    L6= { {{1,3},1}, {{1,4},1}, {{4,5,6},1},{{1,2,3},-1} };
	    divisorClassRepresentativeM0nbar(6,L6)
///

TEST /// 
   L= { {{1,3},1}, {{1,4},1} };
   D=divisorClassRepresentativeM0nbar(6,L);
   assert(set(keys(D#"DivisorExpression")) === set({ {1,3}, {1,4} }))
///

TEST /// 
   L= { {{1,3},1}, {{1,4},1} };
   D=divisorClassRepresentativeM0nbar(6,L);
   assert(D#"NumberOfMarkedPoints" === 6)
///

TEST /// 
    L1= { {{3,1},1}, {{1,4},1} };
    D1=divisorClassRepresentativeM0nbar(6,L1);
    L2={ {{1,3},1}, {{1,4},1} };
    D2=divisorClassRepresentativeM0nbar(6,L2);
    assert(D1===D2)
///

doc /// 
    Key
        (divisorClassRepresentativeM0nbar,ZZ,HashTable) 
    Headline
        creates an object of type DivisorClassRepresentativeM0nbar
    Usage
        divisorClassRepresentativeM0nbar(n,H)
    Inputs
        n:ZZ
        H:HashTable
    Outputs
         :DivisorClassRepresentativeM0nbar
    Description 
        Text
	    This function creates an object of type DivisorClassRepresentativeM0nbar from a hash table.  Here is a basic example:
	Example   
	    H1=new HashTable from { {{1,3},1}, {{1,4},1} };
	    divisorClassRepresentativeM0nbar(6,H1)      
	Text 
	    Warning: when you enter a hash table in Macaulay2, if you use a key more than once, the first instance is discarded.  Here is an example where the behavior may differ from what you want:
        Example
	    H=new HashTable from { {{1,3},1}, {{1,4},1}, {{1,3},2}}
	Text
	    The user probably wanted $\delta_{\{1,3\}} + 2\delta_{\{1,3\}}$ to give $3\delta_{\{1,3\}}$ instead.  The moral of the story: if your expression has two terms that are written exactly alike, you could either combine them before you create the input hash table, or input a list instead:   
        Example
	    L= { {{1,3},1}, {{1,4},1}, {{1,3},2}}
	    divisorClassRepresentativeM0nbar(6,L)
	Text 
	    For this reason, most users will probably prefer to enter divisors via lists, rather than hash tables.  
        Text
	    The function divisorClassRepresentative does the same minimal testing if you enter a hash table that it does if you enter a list.  See the documentation for divisorClassRepresentativeM0nbar(ZZ,List).
///

TEST /// 
   H=new HashTable from { {{1,3},1}, {{1,4},1} };
   D=divisorClassRepresentativeM0nbar(6,H);
   assert(set(keys(D#"DivisorExpression")) === set({ {1,3}, {1,4} }))
///

TEST /// 
   H=new HashTable from { {{1,3},1}, {{1,4},1} };
   D=divisorClassRepresentativeM0nbar(6,H);
   assert(D#"NumberOfMarkedPoints" === 6)
///

TEST /// 
    H1=new HashTable from { {{3,1},1}, {{1,4},1} };
    D1=divisorClassRepresentativeM0nbar(6,H1);
    H2=new HashTable from { {{1,3},1}, {{1,4},1} };
    D2=divisorClassRepresentativeM0nbar(6,H2);
    assert(D1===D2)
///

doc ///
    Key
	(tex, DivisorClassRepresentativeM0nbar)
    Headline
        convert to TeX format
    Usage
        tex D
    Inputs
        D:DivisorClassRepresentativeM0nbar
    Outputs
        :String
    Description
        Example
            L= { {{1,2},7}, {{1,3},2}, {{2,3,5,6},1}};
            D=divisorClassRepresentativeM0nbar(6,L);
            tex D
///


TEST /// 
    H=new HashTable from { {{1,2},7}, {{1,3},2}, {{2,3,5,6},1}};
    D=divisorClassRepresentativeM0nbar(6,H);
    s1=tex D;
    s2="2\\delta_{\\{1, 3\\}} + \\delta_{\\{1, 4\\}} + 7\\delta_{\\{1, 2\\}}";
    assert(s1===s2)
///


doc ///
    Key
	(tex, CurveClassRepresentativeM0nbar)
    Headline
        convert to TeX format
    Usage
        tex C
    Inputs
        C:CurveClassRepresentativeM0nbar
    Outputs
        :String
    Description
        Example
            L= { {{{1},{2},{3},{4,5}},-1}, {{{1},{2,5},{3},{4}},1}, {{{1,4},{2},{3},{5}},1},{{{1,3},{2},{4},{5}},1}}; 
            C=curveClassRepresentativeM0nbar(5,L);
            tex C
///


TEST /// 
    H=new HashTable from { {{{1},{2},{3},{4,5}},-1}, {{{1},{2,5},{3},{4}},1}, {{{1,4},{2},{3},{5}},1},{{{1,3},{2},{4},{5}},1}}; 
    C=curveClassRepresentativeM0nbar(5,H);
    s1=tex C;
    s2="F_{\\{1\\}, \\{2, 5\\}, \\{3\\}, \\{4\\}} + F_{\\{1, 4\\}, \\{2\\}, \\{3\\}, \\{5\\}} + F_{\\{1, 3\\}, \\{2\\}, \\{4\\}, \\{5\\}} - F_{\\{1\\}, \\{2\\}, \\{3\\}, \\{4, 5\\}}";
    assert(s1===s2)
///


doc ///
    Key
        FcurvesOfGivenShape
	(FcurvesOfGivenShape, ZZ,ZZ,ZZ,ZZ)
    Headline
        a list of the F-curves with the given shape
    Usage
        FcurvesOfGivenShape(a,b,c,d)
    Inputs
        a:ZZ
	b:ZZ
	c:ZZ
	d:ZZ
    Outputs
        L:List
    Description
        Text
	    Given four positive integers $a$, $b$, $c$, $d$ with $a+b+c+d=n$, this function lists the F-curves $F_{I_1,I_2,I_3,I_4}$ with $\#I_1=a$,  $\#I_2=b$,  $\#I_3=c$,  $\#I_4=d$.  
        Example
            FcurvesOfGivenShape(2,2,1,1)
///


TEST /// 
    L=FcurvesOfGivenShape(2,2,1,1);
    assert(#L ===  45)
///


doc ///
    Key
        Fcurves
	(Fcurves, ZZ)
    Headline
        lists the F-curves for a given n
    Usage
        Fcurves n
    Inputs
        n:ZZ
    Outputs
        :List
    Description
        Text
	    Given an integer $n$, this function returns a list of the F-curves on $\bar{M}_{0,n}$.  
	   
        Example
            Fcurves(6)
///


TEST /// 
    L=Fcurves(5);
    assert(#L===10)
///


--------------------------------
-- Documentation for vector space structure
--------------------------------

doc ///
    Key
	(symbol +, DivisorClassRepresentativeM0nbar, DivisorClassRepresentativeM0nbar)
    Headline
        add two divisor class representatives on M0nbar
    Usage
        D1+D2
    Inputs
        D1:DivisorClassRepresentativeM0nbar
	D2:DivisorClassRepresentativeM0nbar
    Outputs
        D3:DivisorClassRepresentativeM0nbar
    Description
        Text
	    This function adds two divisor class representatives on M0nbar.
        Example
            L1= { {{1,3},1}, {{1,4},2}};
            D1=divisorClassRepresentativeM0nbar(6,L1);
            L2={ {{1,2},7}, {{1,3},2}, {{2,3,5,6},1}};
            D2=divisorClassRepresentativeM0nbar(6,L2);
            D1+D2
///

TEST /// 
    H1=new HashTable from { {{1,3},1}, {{1,4},2}};
    D1=divisorClassRepresentativeM0nbar(6,H1);
    H2=new HashTable from { {{1,2},7}, {{1,3},2}, {{2,3,5,6},1}};
    D2=divisorClassRepresentativeM0nbar(6,H2);
    H3=new HashTable from {{1, 3} => 3, {1, 4} => 3, {1, 2} => 7};
    D3=divisorClassRepresentativeM0nbar(6,H3);
    assert(D1+D2=== D3)
///

doc ///
    Key
	(symbol -, DivisorClassRepresentativeM0nbar, DivisorClassRepresentativeM0nbar)
    Headline
        subtract two divisor class representatives on M0nbar
    Usage
        D1+D2
    Inputs
        D1:DivisorClassRepresentativeM0nbar
	D2:DivisorClassRepresentativeM0nbar
    Outputs
        D3:DivisorClassRepresentativeM0nbar
    Description
        Text
	    This function subtracts two divisor class representatives on M0nbar.
        Example
            L1= { {{1,3},1}, {{1,4},2}};
            D1=divisorClassRepresentativeM0nbar(6,L1);
            L2= { {{1,2},7}, {{1,3},2}, {{2,3,5,6},1}};
            D2=divisorClassRepresentativeM0nbar(6,L2);
            D1-D2
///

TEST /// 
    H1=new HashTable from { {{1,3},1}, {{1,4},2}};
    D1=divisorClassRepresentativeM0nbar(6,H1);
    H2=new HashTable from { {{1,2},7}, {{1,3},2}, {{2,3,5,6},1}};
    D2=divisorClassRepresentativeM0nbar(6,H2);
    H3=new HashTable from {{1, 3} => -1, {1, 4} => 1, {1, 2} => -7};
    D3=divisorClassRepresentativeM0nbar(6,H3);
    assert(D1-D2=== D3)
///

doc ///
    Key
	(symbol *, Number, DivisorClassRepresentativeM0nbar)
    Headline
        multiply a divisor class representatives on M0nbar by a scalar
    Usage
        c*D
    Inputs
        c:Number
	D:DivisorClassRepresentativeM0nbar
    Outputs
        E:DivisorClassRepresentativeM0nbar
    Description
        Text
	    This function multiplies a divisor class representatives on M0nbar by a scalar
        Example
            L1= { {{1,3},1}, {{1,4},2}};
            D1=divisorClassRepresentativeM0nbar(6,L1);
            3*D1
///

TEST /// 
    H1=new HashTable from { {{1,3},1}, {{1,4},2}};
    D1=divisorClassRepresentativeM0nbar(6,H1);
    H2=new HashTable from { {{1,3},3}, {{1,4},6}};
    D2=divisorClassRepresentativeM0nbar(6,H2);
    assert(3*D1=== D2)
///


doc ///
    Key
	(symbol +, CurveClassRepresentativeM0nbar, CurveClassRepresentativeM0nbar)
    Headline
        add two curve class representatives on M0nbar
    Usage
        C1+C2
    Inputs
        C1:CurveClassRepresentativeM0nbar
	C2:CurveClassRepresentativeM0nbar
    Outputs
        C3:DivisorClassRepresentativeM0nbar
    Description
        Text
	    This function adds two curve class representatives on M0nbar.
        Example
	    L1= { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
            C1=curveClassRepresentativeM0nbar(5,L1);
	    L2= { {{{2,1},{3},{4},{5}},2}, {{{1,4},{2},{3},{5}},1} };
            C2=curveClassRepresentativeM0nbar(5,L2);
            C1+C2
///

TEST /// 
    H1=new HashTable from { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
    C1=curveClassRepresentativeM0nbar(5,H1);
    H2=new HashTable from { {{{2,1},{3},{4},{5}},2}, {{{1,4},{2},{3},{5}},1} };
    C2=curveClassRepresentativeM0nbar(5,H2);
    H3=new HashTable from { {{{1,2},{3},{4},{5}},3}, {{{1,3},{2},{4},{5}},1}, {{{1,4},{2},{3},{5}},1} };
    C3=curveClassRepresentativeM0nbar(5,H3);
    assert(C1+C2=== C3)
///

doc ///
    Key
	(symbol -, CurveClassRepresentativeM0nbar, CurveClassRepresentativeM0nbar)
    Headline
        subtract two curve class representatives on M0nbar
    Usage
        C1-C2
    Inputs
        C1:CurveClassRepresentativeM0nbar
	C2:CurveClassRepresentativeM0nbar
    Outputs
        C3:DivisorClassRepresentativeM0nbar
    Description
        Text
	    This function subtracts two curve class representatives on M0nbar.
        Example
	    L1= { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
            C1=curveClassRepresentativeM0nbar(5,L1);
	    L2= { {{{2,1},{3},{4},{5}},2}, {{{1,4},{2},{3},{5}},1} };
            C2=curveClassRepresentativeM0nbar(5,L2);
            C1-C2
///

TEST /// 
    H1=new HashTable from { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
    C1=curveClassRepresentativeM0nbar(5,H1);
    H2=new HashTable from { {{{2,1},{3},{4},{5}},2}, {{{1,4},{2},{3},{5}},1} };
    C2=curveClassRepresentativeM0nbar(5,H2);
    H3=new HashTable from { {{{1,2},{3},{4},{5}},-1}, {{{1,3},{2},{4},{5}},1}, {{{1,4},{2},{3},{5}},-1} };
    C3=curveClassRepresentativeM0nbar(5,H3);
    assert(C1-C2=== C3)
///

doc ///
    Key
	(symbol *, Number, CurveClassRepresentativeM0nbar)
    Headline
        multiply a curve class representatives on M0nbar by a scalar
    Usage
        c*C1
    Inputs
        c:Number
	C1:CurveClassRepresentativeM0nbar
    Outputs
        C2:CurveClassRepresentativeM0nbar
    Description
        Text
	    This function multiplies a curve class representatives on M0nbar by a scalar
        Example
            L1= { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
            C1=curveClassRepresentativeM0nbar(5,L1);
	    3*C1
///

TEST /// 
    H1=new HashTable from { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1} };
    C1=curveClassRepresentativeM0nbar(5,H1);
    H2=new HashTable from { {{{1,2},{3},{4},{5}},3}, {{{1,3},{2},{4},{5}},3} };
    C2=curveClassRepresentativeM0nbar(5,H2);
    assert(3*C1=== C2)
///


doc /// 
    Key
        negativeSum
        (negativeSum,CurveClassRepresentativeM0nbar) 
    Headline
        sum of the negative coefficients in CurveClassRepresentativeM0nbar
    Usage
        negativeSum C
    Inputs
        C:CurveClassRepresentativeM0nbar
    Outputs
         :ZZ
    Description 
        Text
	    This function sums the negative coefficients in an object of type CurveClassRepresentativeM0nbar.  This is a measure of how far the expression is from being an effective sum of F curves.
        Example
            L= { {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
            C=curveClassRepresentativeM0nbar(5,L);
	    negativeSum C
///

TEST /// 
    H=new HashTable from { {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
    C=curveClassRepresentativeM0nbar(5,H);	    
    assert(negativeSum(C)===-9)
///

doc /// 
    Key
        isEffectiveExpression
        (isEffectiveExpression,CurveClassRepresentativeM0nbar) 
    Headline
        determines whether a curve class expression is effective
    Usage
        isEffectiveExpression(C)
    Inputs
        C:CurveClassRepresentativeM0nbar
    Outputs
         :Boolean
    Description 
        Text
	    This function tests whether the coefficients in an object of type CurveClassRepresentativeM0nbar are all nonnegative.
     	Text 
	    Warning: this function tests only the input representative of a curve class.  That is, it answers whether the input is an effective expression, not whether there exists an effective expression equivalent to the input.
        Example
            L1= { {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
            C1=curveClassRepresentativeM0nbar(5,L1);
	    isEffectiveExpression(C1)
            L2={ {{{2,1},{3},{4},{5}},2}, {{{1,3},{2},{4},{5}},7}, {{{1,4},{2},{3},{5}},1}};
            C2=curveClassRepresentativeM0nbar(5,L2);	    
	    isEffectiveExpression(C2)
///


TEST /// 
    H1=new HashTable from { {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
    C1=curveClassRepresentativeM0nbar(5,H1);
    assert(isEffectiveExpression(C1)===false)
///

TEST /// 
    H2=new HashTable from { {{{2,1},{3},{4},{5}},2}, {{{1,3},{2},{4},{5}},7}, {{{1,4},{2},{3},{5}},1}};
    C2=curveClassRepresentativeM0nbar(5,H2);
    assert(isEffectiveExpression(C2)===true)
///


doc /// 
    Key
	(isEffectiveExpression,DivisorClassRepresentativeM0nbar) 
    Headline
        determines whether a divisor class expression is effective
    Usage
	isEffectiveExpression(D)
    Inputs
        D:DivisorClassRepresentativeM0nbar
    Outputs
         :Boolean
    Description 
        Text
	    This function tests whether the coefficients in an object of type DivisorClassRepresentativeM0nbar are all nonnegative.
	Text 
	    Warning: this function tests only the input representative of a divisor class.  That is, it answers whether the input is an effective expression, not whether there exists an effective expression equivalent to the input.
        Example
	    L1= { {{3,1},-1}, {{1,4},1} };
	    D1=divisorClassRepresentativeM0nbar(6,L1)  
	    isEffectiveExpression(D1)
	    L2= { {{3,1},1}, {{1,4},1} };
	    D2=divisorClassRepresentativeM0nbar(6,L2)  
	    isEffectiveExpression(D2)
///


TEST /// 
    H1=new HashTable from { {{3,1},-1}, {{1,4},1} };
    D1=divisorClassRepresentativeM0nbar(6,H1)  
    isEffectiveExpression(D1)
///



doc /// 
    Key  
        intersection
        (intersection,CurveClassRepresentativeM0nbar,DivisorClassRepresentativeM0nbar) 
    Headline
        intersection number of a curve class and divisor class
    Usage
        intersection(C,D)
    Inputs
        C:CurveClassRepresentativeM0nbar
	D:DivisorClassRepresentativeM0nbar
    Outputs
         :QQ
    Description 
        Text
	    The basic formula for intersecting a boundary divisor with an F-curve is found in Keel and McKernan's paper.  
	Example
            L1={ {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
            C=curveClassRepresentativeM0nbar(5,L1);
	    L2={ {{1,3},1}, {{1,4},1}};
	    D=divisorClassRepresentativeM0nbar(5,L2);
	    intersection(C,D)
///

TEST /// 
    H1=new HashTable from { {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
    C=curveClassRepresentativeM0nbar(5,H1);
    H2=new HashTable from { {{1,3},1}, {{1,4},1}};
    D=divisorClassRepresentativeM0nbar(5,H2);
    assert(intersection(C,D) === 6)
///

doc /// 
    Key  
        (intersection,DivisorClassRepresentativeM0nbar,CurveClassRepresentativeM0nbar) 
    Headline
        intersection number of a divisor class and curve class
    Usage
        intersection(D,C)
    Inputs
	D:DivisorClassRepresentativeM0nbar
	C:CurveClassRepresentativeM0nbar
    Outputs
         :QQ
    Description 
        Text
	    The basic formula for intersecting a boundary divisor with an F-curve is found in Keel and McKernan's paper.  
	Example
            L1={ {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
            C=curveClassRepresentativeM0nbar(5,L1);
	    L2={ {{1,3},1}, {{1,4},1}};
	    D=divisorClassRepresentativeM0nbar(5,L2);
	    intersection(D,C)
///

TEST /// 
    H1=new HashTable from { {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
    C=curveClassRepresentativeM0nbar(5,H1);
    H2=new HashTable from { {{1,3},1}, {{1,4},1}};
    D=divisorClassRepresentativeM0nbar(5,H2);
    assert(intersection(D,C) === 6)
///





doc /// 
    Key
        nonadjacentBasis
        (nonadjacentBasis,ZZ) 
    Headline
        computes the nonadjacent basis of divisors on M0nbar
    Usage
        nonadjacentBasis n
    Inputs
        n:ZZ
    Outputs
         :List
    Description 
        Text
	    The nonadjacent basis is described in Carr, A polygonal presentation of $Pic(\bar{M}_{0,n})$, arXiv:0911.2649.
        Example
            B5=nonadjacentBasis(5)
	    print toString B5
	    B6=nonadjacentBasis(6)	    
	    print toString B6
///

TEST /// 
    B5stored={divisorClassRepresentativeM0nbar(5,new HashTable from {{1, 3} => 1}), divisorClassRepresentativeM0nbar(5,new HashTable from {{1, 4} => 1}), divisorClassRepresentativeM0nbar(5,new HashTable from {{2, 4} => 1}), divisorClassRepresentativeM0nbar(5,new HashTable from {{2, 5} => 1}), divisorClassRepresentativeM0nbar(5,new HashTable from {{3, 5} => 1})};
    B5=nonadjacentBasis(5);
    assert(B5===B5stored);
///

TEST ///
    NA6={{1, 3}, {1, 4}, {1, 5}, {2, 4}, {2, 5}, {2, 6}, {3, 5}, {3, 6}, {4, 6}, {1, 2, 4}, {1, 2, 5}, {1, 3, 4}, {1, 3, 5}, {1, 3, 6}, {1, 4, 5}, {1, 4, 6}};
    B6stored = apply(#NA6, i -> divisorClassRepresentativeM0nbar(6,new HashTable from {NA6_i => 1}));
    B6=nonadjacentBasis(6);
    assert(B6===B6stored);
///


doc /// 
    Key
        writeDivisorInNonadjacentBasis
        (writeDivisorInNonadjacentBasis,DivisorClassRepresentativeM0nbar) 
    Headline
        write a divisor in the nonadjacent basis
    Usage
        writeDivisorInNonadjacentBasis(D)
    Inputs
        D:DivisorClassRepresentativeM0nbar
    Outputs
        E:DivisorClassRepresentativeM0nbar
    Description 
        Text
	    The nonadjacent basis is described in Carr, A polygonal presentation of $Pic(\bar{M}_{0,n})$, arXiv:0911.2649.
	    
	    This function takes a divisor class and writes it in the nonadjacent basis.  
	    
	    Needs more advanced tests!
        Example
            L= {{1,2}=>1};
	    D=divisorClassRepresentativeM0nbar(5,L);
            writeDivisorInNonadjacentBasis(D)
	    L= {{1,5,6}=>1};
	    D=divisorClassRepresentativeM0nbar(6,L);
            writeDivisorInNonadjacentBasis(D)	    
///

TEST /// 
    H1=new HashTable from {{2,3}=>1};
    D1=divisorClassRepresentativeM0nbar(5,H1);
    D1=writeDivisorInNonadjacentBasis(D1);
    H2=new HashTable from {{1,3}=>1,{1,4}=>-1,{2,4}=>1};
    D2=divisorClassRepresentativeM0nbar(5,H2);
    assert(D1===D2)
///

TEST /// 
    H1=new HashTable from {{1,5,6}=>1};
    D1=divisorClassRepresentativeM0nbar(6,H1);
    D1=writeDivisorInNonadjacentBasis(D1);
    H2=new HashTable from {{1, 4, 6} => 1, {1, 4} => 1, {1, 5} => -1, {1, 3, 4} => 1, {1, 3, 5} => -1, {2, 4} => -1, {2, 5} => 1};
    D2=divisorClassRepresentativeM0nbar(6,H2);
    assert(D1===D2)
///    




doc /// 
    Key
        writeCurveInDualNonadjacentBasis
        (writeCurveInDualNonadjacentBasis,CurveClassRepresentativeM0nbar) 
    Headline
        write a curve in the basis dual to the nonadjacent basis
    Usage
        writeCurveInDualNonadjacentBasis C
    Inputs
        C:CurveClassRepresentativeM0nbar
    Outputs
        v:List
    Description 
        Text
	    The nonadjacent basis is described in Carr, A polygonal presentation of $Pic(\bar{M}_{0,n})$, arXiv:0911.2649.
	    
	    This function takes a curve class and returns the vector of its intersection numbers with the nonadjacent basis of divisors.  These intersection numbers are the coefficients in the basis of curves that is dual to the nonadjacent basis.  
        Example
            L1={ {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1}};
            C=curveClassRepresentativeM0nbar(5,L1);
            writeCurveInDualNonadjacentBasis(C)
///


TEST /// 
    Dec19b = new HashTable from {{{{1}, {2, 8}, {3, 4, 6, 7, 9, 10, 11, 12}, {5}}, 1},
{{{1}, {2}, {3, 4, 5, 6, 7, 9, 10, 11, 12}, {8}}, 1},
{{{1, 2, 4, 5, 7, 8, 10, 11}, {3}, {6, 9}, {12}}, 1},
{{{1, 2, 4, 5, 7, 8, 10}, {3}, {6, 9, 12}, {11}}, 1},
{{{1, 2, 4, 5, 7, 8}, {3}, {6, 9, 11, 12}, {10}}, 1},
{{{1, 2, 4, 5, 8}, {3}, {6, 9, 10, 11, 12}, {7}}, 1},
{{{1, 2, 5, 8}, {3}, {4}, {6, 7, 9, 10, 11, 12}}, 1},
{{{1, 2, 8}, {3}, {4, 6, 7, 9, 10, 11, 12}, {5}}, 2},
{{{1, 2, 3, 8}, {4}, {5}, {6, 7, 9, 10, 11, 12}}, 1},
{{{1, 2, 3, 4, 5, 7, 8, 10, 11}, {6}, {9}, {12}}, 1},
{{{1, 2, 3, 4, 5, 7, 8, 10}, {6}, {9, 12}, {11}}, 1},
{{{1, 2, 3, 4, 5, 7, 8}, {6}, {9, 11, 12}, {10}}, 1},
{{{1, 2, 3, 4, 5, 8}, {6}, {7}, {9, 10, 11, 12}}, 1},
{{{1, 3, 5, 6, 7, 8, 10, 11}, {2}, {4, 9}, {12}}, 1},
{{{1, 3, 5, 6, 7, 8, 10}, {2}, {4, 9, 12}, {11}}, 1},
{{{1, 3, 5, 6, 7, 8}, {2}, {4, 9, 11, 12}, {10}}, 1},
{{{1, 3, 5, 6, 8}, {2}, {4, 9, 10, 11, 12}, {7}}, 1},
{{{1, 3, 6, 8}, {2}, {4, 7, 9, 10, 11, 12}, {5}}, 1},
{{{1, 6, 8}, {2}, {3}, {4, 5, 7, 9, 10, 11, 12}}, 1},
{{{1, 8}, {2}, {3, 4, 5, 7, 9, 10, 11, 12}, {6}}, 2},
{{{1, 2, 8}, {3}, {4, 5, 7, 9, 10, 11, 12}, {6}}, 1},
{{{1, 2, 3, 5, 6, 7, 8, 10, 11}, {4}, {9}, {12}}, 1},
{{{1, 2, 3, 5, 6, 7, 8, 10}, {4}, {9, 12}, {11}}, 1},
{{{1, 2, 3, 5, 6, 7, 8}, {4}, {9, 11, 12}, {10}}, 1},
{{{1, 2, 3, 5, 6, 8}, {4}, {7}, {9, 10, 11, 12}}, 1},
{{{1, 2, 3, 6, 8}, {4}, {5}, {7, 9, 10, 11, 12}}, 1},
{{{1, 2, 3, 8}, {4}, {5, 7, 9, 10, 11, 12}, {6}}, 2},
{{{1, 2, 3, 4, 8}, {5}, {6}, {7, 9, 10, 11, 12}}, 2},
{{{1}, {2, 3, 4, 6, 7, 8, 10, 11}, {5, 9}, {12}}, 1},
{{{1}, {2, 3, 4, 6, 7, 8, 10}, {5, 9, 12}, {11}}, 1},
{{{1}, {2, 3, 4, 6, 7, 8}, {5, 9, 11, 12}, {10}}, 1},
{{{1}, {2, 3, 4, 6, 8}, {5, 9, 10, 11, 12}, {7}}, 1},
{{{1}, {2, 3, 4, 8}, {5, 7, 9, 10, 11, 12}, {6}}, 1},
{{{1}, {2}, {3, 4, 8}, {5, 6, 7, 9, 10, 11, 12}}, 1},
{{{1}, {2, 5, 6, 7, 9, 10, 11, 12}, {3, 8}, {4}}, 2},
{{{1}, {2, 4, 5, 6, 7, 9, 10, 11, 12}, {3}, {8}}, 2},
{{{1, 3, 8}, {2}, {4}, {5, 6, 7, 9, 10, 11, 12}}, 1},
{{{1, 8}, {2}, {3}, {4, 5, 6, 7, 9, 10, 11, 12}}, 1},
{{{1, 2, 3, 4, 6, 7, 8, 10, 11}, {5}, {9}, {12}}, 1},
{{{1, 2, 3, 4, 6, 7, 8, 10}, {5}, {9, 12}, {11}}, 1},
{{{1, 2, 3, 4, 6, 7, 8}, {5}, {9, 11, 12}, {10}}, 1},
{{{1, 2, 3, 4, 6, 8}, {5}, {7}, {9, 10, 11, 12}}, 1},
{{{1}, {2}, {3}, {4, 5, 6, 7, 8, 9, 10, 11, 12}}, -2},
{{{1, 2, 3, 7, 8, 9, 10, 11, 12}, {4}, {5}, {6}}, -2},
{{{1, 2, 4, 6, 8, 9, 10, 11, 12}, {3}, {5}, {7}}, -1},
{{{1, 3, 4, 5, 8, 9, 10, 11, 12}, {2}, {6}, {7}}, -1},
{{{1}, {2, 3, 4, 6, 7, 8, 9, 10, 11}, {5}, {12}}, -1},
{{{1, 3, 4, 5, 7, 8, 9, 11, 12}, {2}, {6}, {10}}, -1},
{{{1, 2, 5, 6, 7, 8, 9, 10, 12}, {3}, {4}, {11}}, -1},
{{{1}, {2, 3, 5, 6, 7, 8, 9, 11, 12}, {4}, {10}}, -1},
{{{1, 3, 4, 6, 7, 8, 9, 10, 12}, {2}, {5}, {11}}, -1},
{{{1, 2, 4, 5, 7, 8, 9, 10, 11}, {3}, {6}, {12}}, -1},
{{{1}, {2, 3, 4, 5, 7, 8, 9, 10, 12}, {6}, {11}}, -1},
{{{1, 3, 5, 6, 7, 8, 9, 10, 11}, {2}, {4}, {12}}, -1},
{{{1, 2, 4, 6, 7, 8, 9, 11, 12}, {3}, {5}, {10}}, -1},
{{{1},{4},{7},{2,3,5,6,8,9,10,11,12}},-1}
};
    C=curveClassRepresentativeM0nbar(12,Dec19b);
    w=writeCurveInDualNonadjacentBasis(C);
    v={0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    assert(w===v)
///

doc /// 
    Key
        isEquivalent
        (isEquivalent,CurveClassRepresentativeM0nbar,CurveClassRepresentativeM0nbar) 
    Headline
        check whether two curve class representatives are numerically equivalent
    Usage
        isEquivalent(C1,C2)
    Inputs
        C1:CurveClassRepresentativeM0nbar
	C2:CurveClassRepresentativeM0nbar
    Outputs
        b:Boolean
    Description 
        Text
	    This function checks whether two curve class representatives C1 and C2 are equivalent.  
	    
	    To do so, it checks whether the vector of intersection numbers with the nonadjacent basis of divisors is the same for C1 and C2.
	      
        Example
            L1={ {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1}};
            C1=curveClassRepresentativeM0nbar(5,L1);
	    L2={ {{{1},{2},{3},{4,5}},-1}, {{{1},{2,5},{3},{4}},1}, {{{1,4},{2},{3},{5}},1},{{{1,3},{2},{4},{5}},1}};
            C2=curveClassRepresentativeM0nbar(5,L2);
	    isEquivalent(C1,C2)
///

TEST /// 
    H1=new HashTable from { {{{1,2},{3},{4},{5}},1}, {{{1,3},{2},{4},{5}},1}};
    C1=curveClassRepresentativeM0nbar(5,H1);
    H2=new HashTable from { {{{1},{2},{3},{4,5}},-1}, {{{1},{2,5},{3},{4}},1}, {{{1,4},{2},{3},{5}},1},{{{1,3},{2},{4},{5}},1}}; 
    C2=curveClassRepresentativeM0nbar(5,H2);
    b=isEquivalent(C1,C2);
    assert(b === true)	    
///    


doc /// 
    Key
        singletonSpineBasis
        (singletonSpineBasis,ZZ) 
    Headline
        computes the singleton spine basis of curves on M0nbar
    Usage
        singletonSpineBasis(n)
    Inputs
        n:ZZ
    Outputs
         :List
    Description 
        Text
	    The singleton spine basis of F-curves is described in notes by Swinarski, A basis of F-curves on $\bar{M}_{0,n}$, available at  http://faculty.fordham.edu/dswinarski/Fcurves/.
	    
	    It is defined for n>=7.
        Example
            B7=singletonSpineBasis(7)	   
///

TEST /// 
    B7stored={{{1}, {2}, {3}, {4, 5, 6, 7}}, {{1}, {2}, {3, 4, 5, 6}, {7}}, {{1}, {2}, {3, 4, 5, 7}, {6}}, {{1}, {2}, {3, 4, 6, 7}, {5}}, {{1}, {2}, {3, 5, 6, 7}, {4}}, {{1}, {2, 3}, {4, 5}, {6, 7}}, {{1}, {2, 3, 4, 5}, {6}, {7}}, {{1}, {2, 3, 4, 6}, {5}, {7}}, {{1}, {2, 3, 4, 7}, {5}, {6}}, {{1}, {2, 3, 5, 6}, {4}, {7}}, {{1}, {2, 3, 5, 7}, {4}, {6}}, {{1}, {2, 3, 6, 7}, {4}, {5}}, {{1}, {2, 4, 5, 6}, {3}, {7}}, {{1}, {2, 4, 5, 7}, {3}, {6}}, {{1}, {2, 4, 6, 7}, {3}, {5}}, {{1}, {2, 5, 6, 7}, {3}, {4}}, {{1, 2}, {3}, {4, 5}, {6, 7}}, {{1, 2}, {3, 4}, {5}, {6, 7}}, {{1, 2}, {3, 4}, {5, 6}, {7}}, {{1, 2, 3, 4}, {5}, {6}, {7}}, {{1, 2, 3, 5}, {4}, {6}, {7}}, {{1, 2, 3, 6}, {4}, {5}, {7}}, {{1, 2, 3, 7}, {4}, {5}, {6}}, {{1, 2, 4, 5}, {3}, {6}, {7}}, {{1, 2, 4, 6}, {3}, {5}, {7}}, {{1, 2, 4, 7}, {3}, {5}, {6}}, {{1, 2, 5, 6}, {3}, {4}, {7}}, {{1, 2, 5, 7}, {3}, {4}, {6}}, {{1, 2, 6, 7}, {3}, {4}, {5}}, {{1, 3, 4, 5}, {2}, {6}, {7}}, {{1, 3, 4, 6}, {2}, {5}, {7}}, {{1, 3, 4, 7}, {2}, {5}, {6}}, {{1, 3, 5, 6}, {2}, {4}, {7}}, {{1, 3, 5, 7}, {2}, {4}, {6}}, {{1, 3, 6, 7}, {2}, {4}, {5}}, {{1, 4, 5, 6}, {2}, {3}, {7}}, {{1, 4, 5, 7}, {2}, {3}, {6}}, {{1, 4, 6, 7}, {2}, {3}, {5}}, {{1, 5, 6, 7}, {2}, {3}, {4}}, {{1, 7}, {2}, {3, 4}, {5, 6}}, {{1, 7}, {2, 3}, {4}, {5, 6}}, {{1, 7}, {2, 3}, {4, 5}, {6}}};
    B7stored=apply(#B7stored, i -> curveClassRepresentativeM0nbar(7, new HashTable from {B7stored_i=>1}));
    B7=singletonSpineBasis(7);
    assert(B7===B7stored);
///

 

doc /// 
    Key
        (isEquivalent,DivisorClassRepresentativeM0nbar,DivisorClassRepresentativeM0nbar) 
    Headline
        check whether two divisor class representatives are numerically equivalent
    Usage
        isEquivalent(D1,D2)
    Inputs
        D1:DivisorClassRepresentativeM0nbar
	D2:DivisorClassRepresentativeM0nbar
    Outputs
        b:Boolean
    Description 
        Text
	    This function checks whether two divisor class representatives D1 and D2 are equivalent.  
	    
	    To do so, it checks whether their expressions in the nonadjacent basis of divisors are the same.  
	      
        Example
            L1= { {{1,2},1}};
            D1=divisorClassRepresentativeM0nbar(5,L1);
	    L2={ {{1,3},1}, {{2,4},1}, {{3,4},-1} };
            D2=divisorClassRepresentativeM0nbar(5,L2);
	    isEquivalent(D1,D2)
///

TEST /// 
    H1=new HashTable from { {{1,2},1}};
    D1=divisorClassRepresentativeM0nbar(5,H1);
    H2=new HashTable from { {{1,3},1}, {{2,4},1}, {{3,4},-1} };
    D2=divisorClassRepresentativeM0nbar(5,H2);
    b=isEquivalent(D1,D2);
    assert(b === true)	    
///    


doc /// 
    Key
        writeCurveInSingletonSpineBasis
	(writeCurveInSingletonSpineBasis,CurveClassRepresentativeM0nbar) 
    Headline
        write a curve class in the singleton spine basis of curve
    Usage
        writeCurveInSingletonSpineBasis(C1)
    Inputs
        C1:CurveClassRepresentativeM0nbar
    Outputs
        C2:CurveClassRepresentativeM0nbar
    Description 
        Text
	    This function writes a curve class in the singleton spine basis of curves.

            Recall that the singleton spine basis of curves is only defined on $\bar{M}_{0,n}$ if $n \geq 7$.
        Example
            L= { {{1,2},{3,4},{5,6},{7,8}}=>1 };
            C=curveClassRepresentativeM0nbar(8,L);
	    writeCurveInSingletonSpineBasis(C)
///

TEST /// 
    H1=new HashTable from { {{1,2},{3,4},{5,6},{7,8}}=>1 };
    C1=curveClassRepresentativeM0nbar(8,H1);
    C1=writeCurveInSingletonSpineBasis(C1);
    H2=new HashTable from {{{1, 3, 4, 5, 8}, {2}, {6}, {7}} => 1/1, {{1, 2}, {3, 4}, {5}, {6, 7, 8}} => 1/1, {{1, 3, 4, 5, 6}, {2}, {7}, {8}} => -1/1, {{1}, {2}, {3, 4, 5, 6, 7}, {8}} => 1/1, {{1}, {2}, {3, 4, 7, 8}, {5, 6}} => -1/1, {{1}, {2}, {3, 4, 5, 6}, {7, 8}} => -1/1, {{1}, {2, 3, 4, 7, 8}, {5}, {6}} => -1/1, {{1, 3, 4, 7, 8}, {2}, {5}, {6}} => -1/1, {{1, 8}, {2}, {3, 4, 5}, {6, 7}} => 1/1, {{1}, {2}, {3, 4, 6, 7, 8}, {5}} => 1/1, {{1}, {2, 3, 4, 5}, {6}, {7, 8}} => 1/1};
    C2=curveClassRepresentativeM0nbar(8,H2);    
    assert(C1 === C2)	    
///    

   


doc /// 
    Key
	(writeCurveInSingletonSpineBasis,ZZ,List) 
    Headline
        write a curve class in the singleton spine basis of curve
    Usage
        writeCurveInSingletonSpineBasis(n,L)
    Inputs
        n:ZZ
	L:List
    Outputs
        C2:CurveClassRepresentativeM0nbar
    Description 
        Text
	    This function writes a curve class in the singleton spine basis of curves.  The input is the number of marked points n, and a list of the intersection numbers with the nonadjacent basis of divisors.

            Recall that the singleton spine basis of curves is only defined on $\bar{M}_{0,n}$ if $n \geq 7$.
        Example
            L= { {{1,2},{3,4},{5,6},{7,8}}=>1 };
            C=curveClassRepresentativeM0nbar(8,L);
	    v=writeCurveInDualNonadjacentBasis(C);
            writeCurveInSingletonSpineBasis(8,v)
///

TEST /// 
    H1=new HashTable from { {{1,2},{3,4},{5,6},{7,8}}=>1 };
    C1=curveClassRepresentativeM0nbar(8,H1);
    v=writeCurveInDualNonadjacentBasis(C1);
    C1=writeCurveInSingletonSpineBasis(8,v);
    H2=new HashTable from {{{1, 3, 4, 5, 8}, {2}, {6}, {7}} => 1, {{1, 2}, {3, 4}, {5}, {6, 7, 8}} => 1, {{1, 3, 4, 5, 6}, {2}, {7}, {8}} => -1, {{1}, {2}, {3, 4, 5, 6, 7}, {8}} => 1, {{1}, {2}, {3, 4, 7, 8}, {5, 6}} => -1, {{1}, {2}, {3, 4, 5, 6}, {7, 8}} => -1, {{1}, {2, 3, 4, 7, 8}, {5}, {6}} => -1, {{1, 3, 4, 7, 8}, {2}, {5}, {6}} => -1, {{1, 8}, {2}, {3, 4, 5}, {6, 7}} => 1, {{1}, {2}, {3, 4, 6, 7, 8}, {5}} => 1, {{1}, {2, 3, 4, 5}, {6}, {7, 8}} => 1};
    C2=curveClassRepresentativeM0nbar(8,H2);    
    assert(isEquivalent(C1,C2) === true)	    
///    


doc /// 
    Key
        KeelRelationAmongCurves
	(KeelRelationAmongCurves,List) 
    Headline
        write the Keel relation among F curves based on the input lists
    Usage
        KeelRelationAmongCurves(P)
    Inputs
        P:List
    Outputs
        C:CurveClassRepresentativeM0nbar
    Description 
        Text
	    This function writes a Keel relation among curves.  It is an expression supported on four F-curves that is equivalent to zero.  
	    
	    Specifically, let $P = I_1 \cup \cdots \cup I_5$ be a partition of $\{1,...,n\}$ into five nonempty subsets.  Then this function returns the curve class representative $ F_{I_1,I_2,I_3,I_4 \cup I_5} + F_{I_1 \cup I_2, I_3,I_4,I_5} - F_{I_1, I_4, I_3, I_2 \cup I_5} - F_{I_1 \cup I_4, I_3, I_2, I_5}$.

        Example
	    C1=KeelRelationAmongCurves({{1},{2},{3},{4},{5}})
            L={  };
            C2=curveClassRepresentativeM0nbar(5,L);
            isEquivalent(C1,C2)
///

TEST /// 
    C1=KeelRelationAmongCurves({{1},{2},{3},{4},{5}})
    H=new HashTable from {{{1, 2}, {3}, {4}, {5}} => 1, {{1}, {2, 5}, {3}, {4}} => -1, {{1, 4}, {2}, {3}, {5}} => -1, {{1}, {2}, {3}, {4, 5}} => 1};
    C2=curveClassRepresentativeM0nbar(5,H);
    assert(C1 === C2)    
///    

doc /// 
    Key
        seekEffectiveExpression
	(seekEffectiveExpression,CurveClassRepresentativeM0nbar) 
    Headline
        seeks to write a curve class as an effective sum of F-curves
    Usage
        seekEffectiveExpression(C)
    Inputs
        C:CurveClassRepresentativeM0nbar
    Outputs
        H:HashTable
    Description 
        Text
	    This function seeks to write a curve class representative as an effective sum of F-curves.  See Moon and Swinarski, "Subvarieties of $\bar{M}_{0,n}$ from group actions" for a description of the strategy used.  
	    
	    Important note: the strategy used by this function is not an algorithm because it is not guaranteed to terminate.

        Example
            L1= {{{1, 2}, {3}, {4}, {5}} => 2, {{1}, {2, 5}, {3}, {4}} => -1, {{1, 4}, {2}, {3}, {5}} => -1, {{1}, {2}, {3}, {4, 5}} => 1};
            C1=curveClassRepresentativeM0nbar(5,L1);
            seekEffectiveExpression(C1)
///


TEST /// 
    H1=new HashTable from {{{1, 2}, {3}, {4}, {5}} => 2, {{1}, {2, 5}, {3}, {4}} => -1, {{1, 4}, {2}, {3}, {5}} => -1, {{1}, {2}, {3}, {4, 5}} => 1};
    C1=curveClassRepresentativeM0nbar(5,H1);
    C2=(seekEffectiveExpression(C1))#"CurveClassRepresentative";
    assert(isEquivalent(C1,C2)===true);
    assert(isEffectiveExpression(C2) === true)    
///    

doc /// 
    Key
        permute
	(permute,List,CurveClassRepresentativeM0nbar) 
    Headline
        compute the image of a curve class representative under a permutation of the marked points
    Usage
        permute(sigma,C)
    Inputs
        sigma:List
        C:CurveClassRepresentativeM0nbar
    Outputs
        C2:CurveClassRepresentativeM0nbar
    Description 
        Text
	    The symmetric group $S_n$ acts on $\bar{M}_{0,n}$ by permuting the marked points.
	    
	    This function computes the image of a curve class representative $C$ under a permutation $\sigma$ of the marked points.
	    
	    Enter $\sigma$ as a list $\{ \sigma(1),\sigma(2),\ldots,\sigma(n)\}$.  Cycle class notation is not supported for this function.  
	    
        Example
            L= { {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
            C=curveClassRepresentativeM0nbar(5,L);
            permute({5,2,1,3,4}, C)
///


TEST /// 
    H1=new HashTable from { {{{2,1},{3},{4},{5}},-2}, {{{1,3},{2},{4},{5}},-7}, {{{1,4},{2},{3},{5}},1}};
    C1=curveClassRepresentativeM0nbar(5,H1);
    C2=permute({5,2,1,3,4}, C1);
    H2=C2#"CurveExpression";
    H3=new HashTable from {{{1}, {2, 5}, {3}, {4}} => -2, {{1, 5}, {2}, {3}, {4}} => -7, {{1}, {2}, {3, 5}, {4}} => 1};
    assert(H2 === H3)
///   

doc /// 
    Key
	(permute,List,DivisorClassRepresentativeM0nbar) 
    Headline
        compute the image of a divisor class representative under a permutation of the marked points
    Usage
        permute(sigma,D)
    Inputs
        sigma:List
        D:DivisorClassRepresentativeM0nbar
    Outputs
        D2:DivisorClassRepresentativeM0nbar
    Description 
        Text
	    The symmetric group $S_n$ acts on $\bar{M}_{0,n}$ by permuting the marked points.
	    
	    This function computes the image of a divisor class representative $C$ under a permutation $\sigma$ of the marked points.
	    
	    Enter $\sigma$ as a list $\{ \sigma(1),\sigma(2),\ldots,\sigma(n)\}$.  Cycle class notation is not supported for this function.  
	    
        Example
            L= { {{1,3},1}, {{1,4},-3}};
            D=divisorClassRepresentativeM0nbar(5,L);
            permute({5,2,1,3,4}, D)
///


TEST /// 
    H1=new HashTable from { {{1,3},1}, {{1,4},-3}};
    D1=divisorClassRepresentativeM0nbar(5,H1);
    D2=permute({5,2,1,3,4}, D1);
    H2=D2#"DivisorExpression";
    H3=new HashTable from {{3, 5} => -3, {1, 5} => 1}
    assert(H2 === H3)
///   
--------------------------------------
--------------------------------------
end
--------------------------------------
--------------------------------------





uninstallPackage "M0nbar"
restart
installPackage("M0nbar",UserMode=>true,DebuggingMode => true)

