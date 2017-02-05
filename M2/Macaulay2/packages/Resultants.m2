
newPackage(
       "Resultants",
	Version => "1.0", 
    	Date => "Feb 5, 2017",
    	Authors => {{Name => "Giovanni Staglianò", 
		     Email => "giovannistagliano@gmail.com" 
                    }
                   },
    	Headline => "Resultants, discriminants, and Chow forms",
    	DebuggingMode => false,
	Reload => true
    	)

export{
       "Resultant",
       "Discriminant",
       "genericPolynomials",
       "Poisson",
       "Poisson2", 
       "Macaulay",
       "Macaulay2",
       "fromPluckerToStiefel",
       "Grass", 
       "dualize",
       "tangentialChowForm", 
       "ChowForm",
       "ChowEquations",
       "CayleyTrick", 
       "Duality",
       "Xresultant",
       "AffineChartGrass",
       "AffineChartProj",
       "Dual",
       "AssumeOrdinary",
       "isCoisotropic"
};


----------------------------------------------------------------------------------
----------------------- MultipolynomialResultats ---------------------------------
----------------------------------------------------------------------------------
    
Resultant=method(TypicalValue => RingElement, Options => {Algorithm => Poisson});
    
Resultant Matrix := o -> (F) -> (
    if numgens target F != 1 then error("expected a matrix with one row");
    if not isPolynomialRing ring F then error("the base ring must be a polynomial ring");
    n:=numgens source F -1;
    if n+1 != numgens ring F then error("the number of variables in the ring must be equal to the number of entries of the matrix, but got " | toString(numgens ring F) | " variables and " | toString(numgens source F) | " entries");
    if o.Algorithm =!= Poisson and o.Algorithm =!= Poisson2 and o.Algorithm =!= Macaulay and o.Algorithm =!= Macaulay2 then error "bad value for option Algorithm; possible values are Poisson, Poisson2, Macaulay, and Macaulay2";         
    K:=coefficientRing ring F;
    x:=local x;
    Pn:=K[x_0..x_n];
    F=sub(F,vars Pn); F':=F;
    d:=apply(flatten entries F,ee->first degree ee);
    if not isField K then (K':=frac K; Pn':=K'[x_0..x_n]; F'=sub(F,Pn'));
    if not isHomogeneous ideal F' then error("the matrix entries need to be homogeneous polynomials");
    if o.Algorithm === Macaulay then (if (min d > -1 and sum(d) > n) then return MacaulayResultant F else <<"--warning: option Algorithm=>Macaulay ignored\n");
    if o.Algorithm === Poisson2 then return interpolateRes(F,Poisson);
    if o.Algorithm === Macaulay2 then return interpolateRes(F,Macaulay);
    R:=PoissonFormula F';
    if R != 0 then (
        if isField K then return R;
        if isUnit denominator R then return (denominator R)^(-1) * (numerator R) else return R;
    );
    if dim ideal F' > 0 then return sub(0,K);
    return Resultant(wobble F,Algorithm=>Poisson); 
);

Resultant List := o -> (s) -> (
    return Resultant(matrix{s},Algorithm=>o.Algorithm);
);
    
PoissonFormula = (F) -> (
    -- Theorem 3.4, p. 96 of [David A. Cox and John Little and Donal O'shea - Using Algebraic Geometry - (2005)]
    n:=numgens source F-1;
    K:=coefficientRing ring F;
    x:=local x;
    R:=K[x_0..x_n];
    F=sub(F,vars R);
    d:=apply(flatten entries F,ee->first degree ee);
    if n == 0 then return leadCoefficient F_(0,0);
    if n == 1 then if min d > 0 then return standardRes F;
    if d === {2,2,2} then return Res222 F; 
    xn:=x_n; S:=K[x_0..x_(n-1)];
    f:=sub(sub(F,{xn=>1}),S);
    Fbar:=sub(sub(submatrix'(F,,{n}),{xn=>0}),S);
    Res0:=PoissonFormula Fbar;
       if Res0 == 0 then return Res0;
    A:=S/ideal(submatrix'(f,,{n}));
    bs:=basis(A,Limit=>(product d_{0..(#d-2)}));  
       entriesBs:=flatten entries bs; if not apply(entriesBs,leadMonomial) === entriesBs then error "internal method expected to receive a monomial basis, but received something else";
    mf:=sub(submatrix(f,{n}),A);
    mf=sub(last coefficients(mf*bs,Monomials=>bs),K);
    Res0^(d_n) * det(mf)
);

MacaulayResultant = (F) -> (
    -- Theorem 4.9, p. 108 of [David A. Cox and John Little and Donal O'shea - Using Algebraic Geometry - (2005)]
    K:=coefficientRing ring F;
    n:=numgens source F -1;
    d:=apply(flatten entries F,ee->first degree ee);
    x:=gens ring F;
    mons:=flatten entries gens (ideal x)^(sum(d)-n);
    eqs:={}; nonReducedMons:={}; q:=local q; r:=local r; divs:=local divs;
    for i to #mons -1 do (
          divs={};
          for j to n do (
                (q,r)=quotientRemainder(mons_i,x_j^(d_j));
                if r == 0 then divs=append(divs,j);
                if divs == {j} then eqs=append(eqs,q*F_(0,j));
                if #divs >= 2 then (nonReducedMons=append(nonReducedMons,i); break);
          );
    );
    Mn:=sub(transpose (coefficients(matrix{eqs},Monomials=>mons))_1,K);
    Mn':=submatrix(Mn,nonReducedMons,nonReducedMons);
    Dn:=det Mn;
    Dn':=det Mn';
    if Dn' == 0 then (
          F':=F;
          if not isField K then F'=sub(F',frac(K)[x]);
          if dim ideal F' > 0 then return sub(0,K);
          return MacaulayResultant wobble F;
    );
    resF:=Dn/Dn';
    if ring resF === K then return resF;
    if isUnit denominator resF then return (denominator resF)^(-1) * (numerator resF) else return resF;
);   

interpolateRes = (F,Alg) -> (    
    R:=coefficientRing ring F; 
    if not (isPolynomialRing R and numgens R > 0) then error "expected coefficient ring to be polynomial";
    if not isHomogeneous ideal F then error "the matrix entries need to be homogeneous polynomials";
    K:=coefficientRing R; 
    if not isField K then K=frac K;
    n:=numgens source F -1;
    x:=local x; Rx:=R[x_0..x_n]; Kx:=K[x_0..x_n];
    F=sub(F,vars Rx);
    d:=sum(0..n,i -> (degree F_(0,i))_1 * product first entries submatrix'(matrix{apply(flatten entries F,ee -> first degree ee)},,{i}));
    if d<=0 then error "interpolation not possible";
    points:=sub(matrix apply(flatten entries gens (ideal gens R)^d,G->apply(gens R,t->degree(t,G))),K); 
    M:=points|transpose matrix{apply(entries points,p->Resultant(sub(sub(F,apply(numgens R,i -> R_i=>p_i)),Kx),Algorithm=>Alg))};
    if K === coefficientRing R then return interpolate(M,R,d);
    Res:=interpolate(M,K[gens R],d);
    sub(Res * lcm apply(flatten entries sub(last coefficients Res,coefficientRing ring Res),u->denominator u),vars R)
);

wobble = (F) -> (  
    R:=ring F;
    K:=coefficientRing R;
    n:=numgens R-1;
    A:=matrix 0; d:=0; while not isUnit d do (A=matrix for i to n list for j to n list sub(random(0,1),K); d=det A);
    A=d^(-1) * (matrix A_0) | submatrix'(A,,{0});
    Sub:=map(R,R,transpose(A*(transpose vars R)));
    Sub(F)
);

standardRes = (F) -> (
    -- Determinant of the Sylvester matrix
    K:=coefficientRing ring F;
    (x,y):=toSequence gens ring F;
    (l,m):=(first degree F_(0,0),first degree F_(0,1));
     A:=sub(transpose (coefficients(matrix{{F_(0,0)}}, Monomials => for i to l list x^(l-i) * y^i))_1,K);   
     B:=sub(transpose (coefficients(matrix{{F_(0,1)}}, Monomials => for i to m list x^(m-i) * y^i))_1,K);   
     A':=transpose(A|matrix{toList(m-1:0_K)});
     B':=transpose(B|matrix{toList(l-1:0_K)});
     Sylvester:=A';
     for i from 1 to m-1 do (
          A'=matrix{{0_K}} || submatrix'(A',{l+m-1},);
          Sylvester=Sylvester|A'
     );
     Sylvester=Sylvester|B';
     for i from 1 to l-1 do (
          B'=matrix{{0_K}} || submatrix'(B',{l+m-1},);
          Sylvester=Sylvester|B'
     );
     det Sylvester
);

Res222' = (F) -> ( -- not longer used
    -- Resultant of three ternary quadrics
    -- p. 89 of [David A. Cox and John Little and Donal O'shea - Using Algebraic Geometry (2005)]
    assert(char coefficientRing ring F =!= 2);
    K:=coefficientRing ring F;
    (x,y,z):=toSequence gens ring F;
    M:={x^2,y^2,z^2,x*y,x*z,y*z};
    J:=transpose jacobian matrix{{det jacobian F}};
    A:=sub(transpose (coefficients(F,Monomials=>M))_1,K);
    B:=sub(transpose (coefficients(J,Monomials=>M))_1,K);
    d:=-1/sub(512,K)*det(A||B);
    try lift(d,K) else d
);

Res222 = (F) -> (
   K:=coefficientRing ring F;
   y:=local y;
   ringG25:=K[y_(0,1,2),y_(0,1,3),y_(0,2,3),y_(1,2,3),y_(0,1,4),y_(0,2,4),y_(1,2,4),y_(0,3,4),y_(1,3,4),y_(2,3,4),y_(0,1,5),y_(0,2,5),y_(1,2,5),y_(0,3,5),y_(1,3,5),y_(2,3,5),y_(0,4,5),y_(1,4,5),y_(2,4,5),y_(3,4,5)];
     -- code: "chow"<<toString dualize ChowForm(trim kernel map(QQ[t_0..t_2],QQ[x_0..x_5],gens (ideal(t_0..t_2))^2),Variable=>y)<<close;
   W:=y_(0,3,5)^4+y_(0,2,4)*y_(0,3,5)^2*y_(1,3,5)-2*y_(0,1,5)*y_(0,3,5)^2*y_(1,3,5)+y_(0,1,5)^2*y_(1,3,5)^2+y_(0,2,3)*y_(0,2,5)*y_(1,3,5)^2-y_(0,1,4)*y_(0,2,5)*y_(1,3,5)^2+y_(0,1,2)*y_(1,2,5)*y_(1,3,5)^2-y_(0,2,4)*y_(0,3,4)*y_(0,3,5)*y_(2,3,5)+2*y_(0,2,3)*y_(0,3,5)^2*y_(2,3,5)+y_(0,1,4)*y_(0,3,5)^2*y_(2,3,5)-y_(0,2,3)*y_(0,2,4)*y_(1,3,5)*y_(2,3,5)+y_(0,1,4)*y_(0,2,4)*y_(1,3,5)*y_(2,3,5)-y_(0,1,2)*y_(1,2,4)*y_(1,3,5)*y_(2,3,5)-y_(0,1,4)*y_(0,1,5)*y_(1,3,5)*y_(2,3,5)+3*y_(0,1,2)*y_(0,3,5)*y_(1,3,5)*y_(2,3,5)+y_(0,2,3)^2*y_(2,3,5)^2+y_(0,1,2)*y_(1,2,3)*y_(2,3,5)^2-y_(0,1,3)*y_(0,2,4)*y_(2,3,5)^2-y_(0,1,2)*y_(0,3,4)*y_(2,3,5)^2+y_(0,1,3)*y_(0,1,5)*y_(2,3,5)^2-2*y_(0,3,4)*y_(0,3,5)^2*y_(0,4,5)+y_(0,3,4)^2*y_(0,4,5)^2-y_(0,2,4)*y_(0,3,4)*y_(0,3,5)*y_(1,4,5)-2*y_(0,2,3)*y_(0,3,5)^2*y_(1,4,5)+3*y_(0,1,4)*y_(0,3,5)^2*y_(1,4,5)-y_(0,2,3)*y_(0,2,4)*y_(1,3,5)*y_(1,4,5)+y_(0,1,4)*y_(0,2,4)*y_(1,3,5)*y_(1,4,5)-y_(0,1,2)*y_(1,2,4)*y_(1,3,5)*y_(1,4,5)-y_(0,1,4)*y_(0,1,5)*y_(1,3,5)*y_(1,4,5)-y_(0,1,2)*y_(0,3,5)*y_(1,3,5)*y_(1,4,5)+2*y_(0,2,3)*y_(0,3,4)*y_(0,4,5)*y_(1,4,5)-y_(0,1,4)*y_(0,3,4)*y_(0,4,5)*y_(1,4,5)-2*y_(0,1,3)*y_(0,3,5)*y_(0,4,5)*y_(1,4,5)+y_(0,2,3)^2*y_(1,4,5)^2+y_(0,1,2)*y_(1,2,3)*y_(1,4,5)^2-y_(0,1,3)*y_(0,2,4)*y_(1,4,5)^2+2*y_(0,1,2)*y_(0,3,4)*y_(1,4,5)^2+y_(0,1,3)*y_(0,1,5)*y_(1,4,5)^2+y_(0,2,4)*y_(0,3,4)^2*y_(2,4,5)+y_(0,2,3)*y_(0,2,4)*y_(1,3,4)*y_(2,4,5)-y_(0,1,4)*y_(0,2,4)*y_(1,3,4)*y_(2,4,5)+y_(0,1,2)*y_(1,2,4)*y_(1,3,4)*y_(2,4,5)-y_(0,2,3)^2*y_(2,3,4)*y_(2,4,5)-y_(0,1,2)*y_(1,2,3)*y_(2,3,4)*y_(2,4,5)+y_(0,1,3)*y_(0,2,4)*y_(2,3,4)*y_(2,4,5)+y_(0,1,2)*y_(0,3,4)*y_(2,3,4)*y_(2,4,5)-2*y_(0,2,3)*y_(0,3,4)*y_(0,3,5)*y_(2,4,5)-y_(0,1,4)*y_(0,3,4)*y_(0,3,5)*y_(2,4,5)-2*y_(0,1,3)*y_(0,3,5)^2*y_(2,4,5)+y_(0,1,4)^2*y_(1,3,5)*y_(2,4,5)-3*y_(0,1,2)*y_(0,3,4)*y_(1,3,5)*y_(2,4,5)-y_(0,1,3)*y_(0,1,4)*y_(2,3,5)*y_(2,4,5)+2*y_(0,1,3)*y_(0,3,4)*y_(0,4,5)*y_(2,4,5)-y_(0,1,3)*y_(0,1,4)*y_(1,4,5)*y_(2,4,5)+y_(0,1,3)^2*y_(2,4,5)^2-2*y_(0,2,3)^2*y_(1,2,5)*y_(3,4,5)-2*y_(0,1,2)*y_(1,2,3)*y_(1,2,5)*y_(3,4,5)+2*y_(0,1,3)*y_(0,2,4)*y_(1,2,5)*y_(3,4,5)-2*y_(0,1,3)*y_(0,1,5)*y_(1,2,5)*y_(3,4,5)-y_(0,2,3)*y_(0,2,4)*y_(0,3,5)*y_(3,4,5)-2*y_(0,1,4)*y_(0,1,5)*y_(0,3,5)*y_(3,4,5)+8*y_(0,1,3)*y_(0,2,5)*y_(0,3,5)*y_(3,4,5)-3*y_(0,1,2)*y_(0,3,5)^2*y_(3,4,5)+2*y_(0,1,2)*y_(0,1,5)*y_(1,3,5)*y_(3,4,5)+y_(0,1,2)*y_(0,2,3)*y_(2,3,5)*y_(3,4,5)-y_(0,1,2)*y_(0,1,4)*y_(2,3,5)*y_(3,4,5)+2*y_(0,2,3)^2*y_(0,4,5)*y_(3,4,5)+y_(0,1,4)^2*y_(0,4,5)*y_(3,4,5)-2*y_(0,1,3)*y_(0,2,4)*y_(0,4,5)*y_(3,4,5)-2*y_(0,1,3)*y_(0,1,5)*y_(0,4,5)*y_(3,4,5)+2*y_(0,1,2)*y_(0,2,3)*y_(1,4,5)*y_(3,4,5)-2*y_(0,1,2)*y_(0,1,4)*y_(1,4,5)*y_(3,4,5)+y_(0,1,2)*y_(0,1,3)*y_(2,4,5)*y_(3,4,5)+y_(0,1,2)^2*y_(3,4,5)^2;
   g:=gens ringG25;
   M:=transpose sub(last coefficients(F,Monomials=>(gens (ideal vars ring F)^2)),K);
   mm:=apply(subsets(6,3),m -> det submatrix(M,m));
   sub(W,apply(20,j -> g_j => mm_j))
);

Discriminant=method(TypicalValue => RingElement, Options => {Algorithm => Poisson});
    
Discriminant RingElement := o -> (G) -> (
    if not (isPolynomialRing ring G) then error("expected a homogeneous polynomial");   
--  if not (isHomogeneous G) then error("expected a homogeneous polynomial");   
    n:=numgens ring G;
    d:=first degree G;
    a:=((d-1)^n - (-1)^n)/d;
    resG:=Resultant(transpose jacobian matrix{{G}},Algorithm=>o.Algorithm);
    try lift(resG/(d^a),ring resG) else resG
);

genericPolynomials = method(TypicalValue => List);
genericPolynomials (Ring,List) := (K,d) -> (
   try assert(ring matrix {d} === ZZ) else error "expected list of integers";
   A:=apply(#d,i->K[(vars i)_0..(vars i)_(binomial(#d-1+d_i,d_i)-1)]);
   R:=A_0; for i from 1 to #d-1 do R=R**A_i;
   R=K[gens R];
   x:=local x; P:=R[x_0..x_(#d-1)];
   apply(#d,i -> (sub(vars A_i,R) * transpose gens (ideal vars P)^(d_i))_(0,0) )
); 
genericPolynomials (List) := (d) -> genericPolynomials(ZZ,d);

interpolate = method(TypicalValue => RingElement)
interpolate (Matrix,PolynomialRing,ZZ) := (M,R,d) -> (
    --  input: M=matrix{{a_(0,0),...,a_(n,0),b_0},...,{a_(0,N),...,a_(n,N),b_N}}
    --  output: homogeneous polynomial P of degree d s.t. P(a_(0,i),...,a_(n,i))=b_i
    Det := (i,M) -> sum(0..(numgens source M -1), j -> (-1)^(i+j) * M_(i,j) * det submatrix'(M,{i},{j}));
    n:=numgens source M -2;   
    N:=numgens target M -1;
    K:=coefficientRing R;
    mons:=gens (ideal gens R)^d;
    if not (isField K and K === ring M and n+1 === numgens R and N+1 === numgens source mons) then error "invalid input data for interpolation";
    S:=matrix apply(N+1,i -> first entries ((map(K,R,submatrix(M,{i},0..n))) mons));
    detS:=det S; if detS == 0 then error "interpolation failed";
    B:=flatten entries submatrix(M,,{n+1})/detS;
    sum(0..N,i -> B_i * Det(i,submatrix'(S,{i..N},)||mons||submatrix'(S,{0..i},)))
);

----------------------------------------------------------------------------------
---------------------------- ChowForms -------------------------------------------
----------------------------------------------------------------------------------

GG:=local GG;

Grass = method(TypicalValue => QuotientRing, Options => {Variable => "p"});

Grass (ZZ,ZZ,Ring) := o -> (k,n,KK) -> (
   pp:=getSymbol toString o.Variable;
   ch:=char KK;
   if ( KK =!= QQ and KK =!= ZZ/ch ) then (Gr:=Grassmannian(k,n,CoefficientRing=>KK,Variable=>pp); return((ring Gr)/Gr));
   if (class GG_(k,n,ch,pp) === QuotientRing or class GG_(k,n,ch,pp) === PolynomialRing) then return GG_(k,n,ch,pp); 
   J:=Grassmannian(k,n,CoefficientRing=>KK,Variable=>pp);
   GG_(k,n,ch,pp)=(ring J)/J;
   -- <<"-- created Grassmannian G("<<k<<","<<n<<") over "<<toExternalString(KK)<<" with variable "<<pp<<endl;
   return GG_(k,n,ch,pp)
);

Grass (ZZ,ZZ) := o -> (k,n) -> Grass(k,n,QQ,Variable=>o.Variable);

getVariable = method(TypicalValue => Symbol)
getVariable(PolynomialRing) := (R) -> (
    x := baseName first gens R;
    if class x === IndexedVariable then x = first x;
    getSymbol toString x
);

detectGrassmannian = method(TypicalValue => Sequence);
detectGrassmannian (PolynomialRing) := (G) -> ( -- thanks to Federico Galetto 
    -- input: ambient ring of G(k,n) 
    -- output: (k,n)
    k := (length sequence last baseName first gens G) - 1;
    n := last sequence last baseName last gens G;
    if binomial(n+1,k+1) =!= numgens G then error "unable to detect Grassmannian ring";
    return (k,n,coefficientRing G,getVariable G)
);
detectGrassmannian (QuotientRing) := (G) -> (
   (k,n,KK,p):=detectGrassmannian ambient G;
   if sub(ideal Grass(k,n,KK,Variable=>p),vars ambient G) != ideal G then error "unable to detect Grassmannian ring";
   return (k,n,KK,p)
);

duality = method(TypicalValue => RingMap); -- p. 94 [Gelfand, Kapranov, Zelevinsky - Discriminants, resultants, and multidimensional determinants, 1994]

sign = (permutation) -> sub(product(subsets(0..#permutation-1,2),I->(permutation_(I_1)-permutation_(I_0))/(I_1-I_0)),ZZ); -- thanks to Nivaldo Medeiros 
tosequence = (L) -> if #L != 1 then toSequence L else L_0;

duality(PolynomialRing) := (R) -> (  -- returns the map R:=G(k,P^n) ---> G(n-k-1,P^n*)
   (k,n,KK,p):=detectGrassmannian R; 
   G:=ambient Grass(k,n,KK,Variable=>p);
   G':=ambient Grass(n-k-1,n,KK,Variable=>p);  
   L:=for U in subsets(set(0..n),n-k) list sign( (sort toList(set(0..n)-U)) | sort toList U)  *  (p_(tosequence sort toList(set(0..n)-U)))_G;
   return(map(R,G,vars R) * map(G,G',L));
);

duality(QuotientRing) := (R) -> ( -- returns the map R:=G(k,P^n) ---> G(n-k-1,P^n*)
   (k,n,K,p):=detectGrassmannian R;
   Gkn:=Grass(k,n,K,Variable=>p);
   f:=map(Gkn,Grass(n-k-1,n,K,Variable=>p),matrix duality ambient Gkn); 
   return(map(R,Gkn,vars R) * f);
);

dualize = method()
dualize(RingElement) := (F) -> (
   (inverse duality ring F) F
);
dualize(Matrix) := (F) -> (
   (inverse duality ring F) F
);
dualize(Ideal) := (F) -> (
   (inverse duality ring F) F
);
dualize(RingMap) := (phi) -> (
   phi*duality(source phi)
);
dualize(VisibleList) := (L) -> (
   new class L from apply(L,dualize)
);

tangentialChowForm = method(TypicalValue => RingElement, Options => {Variable => null, Duality => null, AffineChartGrass => true, AssumeOrdinary => null, AffineChartProj => true}); 

tangentialChowForm (Ideal,ZZ) := o -> (I,s) -> (
   -- Input: I ideal of X = V(I) subset P^n of dimension d
   -- Output: form defining the hypersurface Z_s(X) subset GG(d-s,P^n*) = GG(n-d-1+s,P^n) 
   if not isPolynomialRing ring I then error("expected ideal in a polynomial ring");
   if not isHomogeneous I then error("expected a homogeneous ideal");
   if s<0 then error("expected a nonnegative integer");
   p:=if o.Variable === null then getVariable ring I else getSymbol toString o.Variable;
   K:=coefficientRing ring I; 
   n:=numgens ring I -1;
   d:=dim I -1;
   useDuality:=o.Duality;
   if useDuality === null then useDuality = d-s+1 <= n-d+s;
   if class useDuality =!= Boolean then error "expected true or false for option Duality";
   r:=if useDuality then d-s else n-d-1+s; 
   if n-d-1+s >= n or n-d-1+s <=-1 then return 1_(Grass(n-d-1+s,n,K,Variable=>p));
   mnr:=o.AffineChartGrass;
   if mnr === true then mnr = (random toList(0..n))_{0..r};
   if mnr =!= false then (try assert(ring matrix{mnr} === ZZ and min mnr >=0 and max mnr <=n and #set mnr == r+1) else error("bad value for option AffineChartGrass: expected either boolean value or list of "|toString(r+1)|" distinct integers beetween 0 and "|toString(n)|" but got "|toString(mnr))); 
   if mnr =!= false then mnr=sort mnr; 
   if (class o.AssumeOrdinary =!= Boolean and o.AssumeOrdinary =!= null) then error "expected true or false for option AssumeOrdinary";
   limitNumGens:=if (o.AssumeOrdinary === true or (o.AssumeOrdinary === null and s==0)) then 1 else infinity;  
   x:=local x; t:=local t; u:=local u;
   R:=if useDuality then K[x_0..x_n,u_(0,0)..u_(r,n),MonomialOrder=>Eliminate(n+1)] else K[x_0..x_n,t_0..t_r,u_(0,0)..u_(r,n),MonomialOrder=>Eliminate(n+r+2)];
   U:=genericMatrix(R,u_(0,0),n+1,r+1);
   if useDuality then U=transpose U; 
   Ru:=K[u_(0,0)..u_(r,n)]; 
   if mnr =!= false then (
      if useDuality then (
         R=K[x_0..x_n,flatten entries submatrix'(U,mnr),MonomialOrder=>Eliminate(n+1)];
         Ru=K[flatten entries submatrix'(U,mnr)];
         U=sub(sub(U,flatten for i to r list for j to r list U_(i,mnr_j) => (if i == j then 1 else 0)),R)
      ) else (
         R=K[x_0..x_n,t_0..t_r,flatten entries submatrix'(transpose U,mnr),MonomialOrder=>Eliminate(n+r+2)];
         Ru=K[flatten entries submatrix'(transpose U,mnr)];
         U=sub(sub(U,flatten for i to r list for j to r list U_(mnr_i,j) => (if i == j then 1 else 0)),R)
      );
   );
   Sub:=map(R,ring I,{x_0..x_n});
   Sub':=map(Ru,R,matrix{toList((numgens R - numgens Ru):0_K)} | vars Ru); 
   Irr:=ideal(x_0..x_n);
   Inc:=if useDuality then (Sub I)+ideal(U*transpose(gens Irr)) else (Sub I)+ideal(U*transpose(matrix{toList(t_0..t_r)})-transpose(gens Irr));
   if s>0 then (
       J:=transpose jacobian I; 
       singLocus:=trim(I+minors(n-d,J));
       if dim singLocus -1 >= 0 then Irr=Sub singLocus; 
       J=Sub J; 
       Inc=if useDuality then Inc+minors(n-s+1,J||U) else Inc+minors(n-d,J*U)
   ); 
   jj:=o.AffineChartProj;
   if jj === true then jj=random(n+1); 
   if jj =!= false then (try assert(class jj === ZZ and jj >=0 and jj <=n) else error("bad value for option AffineChartProj: expected either boolean value or integer beetween 0 and "|toString(n)|" but got "|toString(jj))); 
   if s>0 and Irr =!= ideal(x_0..x_n) then jj=false;
   if jj === false then Inc=saturate(Inc,Irr) else ( 
       tt:=x_jj;
       VarsR:=flatten entries submatrix'(vars R,{jj});
       R=if useDuality then K[VarsR,MonomialOrder=>Eliminate n] else K[VarsR,MonomialOrder=>Eliminate (n+r+1)]; 
       Inc=sub(sub(Inc,tt=>1),R); 
       U=sub(U,R);
       Sub'=map(Ru,R,submatrix'(matrix Sub',{jj}))
   );
   Z:=trim Sub' ideal selectInSubring(1,gens gb(Inc,SubringLimit=>limitNumGens)); 
   G:=Grass(r,n,K,Variable=>p);
   f:=map(Ru/Z,G,gens minors(r+1,Sub' U));
   if mnr =!= false then limitNumGens=infinity; 
   Zs:=kernel(f,SubringLimit=>limitNumGens);
   if mnr =!= false then Zs=homogenize(Zs,(p_(tosequence mnr))_G); 
   if useDuality then Zs=dualize Zs;
   Zs=trim Zs;
   if Zs == 1 and (o.AffineChartGrass =!= false or o.AffineChartProj =!= false) then (
        -- <<"-- rerun tangentialChowForm() with options AffineChartGrass=>false, AffineChartProj=>false... \n";
        return tangentialChowForm(I,s,Variable=>o.Variable,Duality=>o.Duality,AffineChartGrass=>false,AssumeOrdinary=>o.AssumeOrdinary,AffineChartProj=>false);
   ); 
   if numgens Zs == 1 then Zs_0 else Zs
);

ChowForm = method(TypicalValue => RingElement, Options => {Variable => null, Duality => null, AffineChartGrass => true, AffineChartProj => true}); 

ChowForm (Ideal) := o -> (I) -> (
   return tangentialChowForm(I,0,Variable=>o.Variable,Duality=>o.Duality,AffineChartGrass=>o.AffineChartGrass,AffineChartProj=>o.AffineChartProj);
);

ChowForm (RingMap) := o -> (phi) -> ( -- undocumented
   -- Input: an isomorphism phi:P^r ---> X \subset P^n
   -- Output: form defining the hypersurface Z_0(X) subset GG(r,P^n*) = GG(n-r-1,P^n)
   if not isPolynomialRing target phi then error("the target of the ring map needs to be a polynomial ring");
   if not ((isPolynomialRing source phi or isQuotientRing source phi) and isPolynomialRing ambient source phi and isHomogeneous ideal source phi) then error("the source of the ring map needs to be either a polynomial ring or a quotient of a polynomial ring by a homogeneous ideal");
   K:=coefficientRing ambient source phi; 
   if not (K === coefficientRing target phi) then error("different coefficient rings encountered");
   if not isField K then error("the coefficient ring needs to be a field");
   F:=submatrix(matrix phi,{0..(numgens source phi -1)});
   if not (isHomogeneous ideal F and # unique degrees ideal F == 1) then error("the map needs to be defined by homogeneous polynomials of the same degree");
   p:=if o.Variable === null then getVariable ambient source phi else getSymbol toString o.Variable;
   if (o.AffineChartGrass =!= true or o.AffineChartProj =!= true or o.Duality =!= null) then error "option not available with ChowForm(RingMap); you can use it with ChowForm(Ideal)"; 
   n:=numgens ambient source phi -1;
   r:=numgens target phi -1;
   x:=local x; u:=local u;
   R:=K[x_1..x_r,u_(0,0)..u_(r,n-r-1),MonomialOrder=>Eliminate r];
   U:=(transpose genericMatrix(R,u_(0,0),n-r,r+1)) | sub(matrix for i to r list for j to r list if i == j then 1 else 0,R);
   F=(map(R,target phi,{1,x_1..x_r})) transpose F;
   Ru:=K[u_(0,0)..u_(r,n-r-1)];
   Z:=sub(ideal selectInSubring(1,gens gb(ideal(U*F),SubringLimit=>1)),Ru); 
   G:=Grass(r,n,K,Variable=>p);
   f:=map(Ru/Z,G,gens minors(r+1,sub(U,Ru)));
   Zs:=kernel(f,SubringLimit=>2);
   Zs=trim dualize homogenize(Zs,(p_((n-r)..n))_G); 
   if numgens Zs == 1 then Zs_0 else Zs
);

SegreProduct = method(Options => {Variable => null});
SegreProduct (Ideal,ZZ) := o -> (X,k) -> ( 
   -- Input: X ideal of a subvariety of P^n, k integer
   -- Output: (W,M), W = ideal of X x P^k in PP(Mat(k+1,n+1)), M = generic matrix of coordinates in PP(Mat(k+1,n+1))
   if not isPolynomialRing ring X then error("expected ideal in a polynomial ring");
   if not isHomogeneous X then error("expected a homogeneous ideal");
   if k<0 then error("expected a nonnegative integer");
   x:=if o.Variable === null then getVariable ring X else getSymbol toString o.Variable;
   t:=local t; y:=local y;
   K:=coefficientRing ring X;
   n:=numgens ring X -1;
   Mat:=K[x_(0,0)..x_(k,n)];
   M:=genericMatrix(Mat,n+1,k+1); 
   PnPk:=K[t_0..t_n,y_0..y_k];
   s:=map(PnPk,Mat,flatten for i to k list for j to n list t_j*y_i);
   XxPk:=trim preimage(s,(map(PnPk,ring X,{t_0..t_n})) X);
   (XxPk,M)
);

CayleyTrick = method (Options => {Variable => null});

CayleyTrick (Ideal) := o -> (I) -> ( 
  -- input: ideal I of a subvariety X subset P^n of dimension k
  -- output: (X x P^k),(X x P^k)^*
  k:=dim I -1;
  n:=numgens ring I -1;
  KK:=coefficientRing ring I;
  (W,M):=SegreProduct(I,k,Variable=>o.Variable);
  Z:=ideal dualize tangentialChowForm(I,0,Variable=>o.Variable);
  f:=map(ring M,ring Z,gens minors(k+1,M));
  (W,f Z)
);

CayleyTrick (Ideal,ZZ) := o -> (I,b) -> ( -- undocumented
  -- input: ideal I of a subvariety X subset P^n of dimension k, b integer s.t. def X <= b <= dim X
  -- output: (X x P^b),(X x P^b)^*
  k:=dim I -1;
  if b > k then error "integer exceeds the dimension of the projective scheme";
  n:=numgens ring I -1;
  KK:=coefficientRing ring I;
  (W,M):=SegreProduct(I,b,Variable=>o.Variable);
  Z:=ideal dualize tangentialChowForm(I,k-b,Variable=>o.Variable);
  f:=map(ring M,ring Z,gens minors(b+1,M));
  (W,f Z)
);

Xresultant = method (Options => {Variable => null});
Xresultant (Ideal) := o -> (I) -> (
   fromPluckerToStiefel(dualize ChowForm I,Variable=>o.Variable)
);

ChowEquations = method(TypicalValue => Ideal, Options => {Variable => null});

ChowEquations (RingElement) := o -> (W) -> ( 
   (k,n,KK,x):=detectGrassmannian ambient ring W;
   k=n-k-1;   -- W in G(n-k-1,n), k=dim X 
   if o.Variable =!= null then x = getSymbol toString o.Variable;
   Pn:=Grass(0,n,KK,Variable=>x);
   s:=local s; 
   R:=Pn[s_(0,0)..s_(n-k-2,n)];   
   M:=if n-k-1>0 then transpose(genericMatrix(R,n+1,n-k-1))||sub(vars Pn,R) else sub(vars Pn,R);
   mm:=minors(n-k,M);
   pp:=gens ring W;
   P:=sub(W,for i to #pp -1 list pp_i => mm_i);
   trim sub(ideal last coefficients matrix{{P}},Pn)
);

ChowEquations (RingElement,ZZ) := o -> (W,s) -> ( -- undocumented  
   -- recover the variety X from its tangential Chow form W=Z_s(X)
   W=dualize W;
   (k,n,KK,x):=detectGrassmannian ring W;
   k=k+s;   -- W in G(k-s,n), k=dim X 
   if not (s>=0 and s<=k) then error("wrong integer value for second argument of ChowEquations");
   if o.Variable =!= null then x = getSymbol toString o.Variable;
   Pn:=Grass(0,n,KK,Variable=>x);
   psi:=first projectionMap(ring W,AffineChartGrass=>false);
   Z:=psi W;
   f:=map((ring Z)/Z,ring Z,transpose jacobian matrix Z) * map(ring Z,Pn,submatrix(vars ring Z,0..n));
   trim kernel f
);

Dual = method(TypicalValue => Ideal, Options => {AssumeOrdinary => false}); 

Dual (Ideal) := o -> (I) -> (
   I':=if o.AssumeOrdinary then tangentialChowForm(I,dim I -1,AffineChartGrass=>false,AssumeOrdinary=>o.AssumeOrdinary) else tangentialChowForm(I,dim I -1,AssumeOrdinary=>o.AssumeOrdinary);
   I'=dualize I';
   if not isIdeal I' then I'=ideal I';
   sub(I',vars ring I)
);

Dual (RingMap) := o -> (phi) -> ( -- undocumented 
   -- Input: an isomorphism phi:P^n ---> X \subset P^m 
   -- Output: the dual variety of X 
   if not isPolynomialRing target phi then error("the target of the ring map needs to be a polynomial ring");
   if not ((isPolynomialRing source phi or isQuotientRing source phi) and isPolynomialRing ambient source phi and isHomogeneous ideal source phi) then error("the source of the ring map needs to be either a polynomial ring or a quotient of a polynomial ring by a homogeneous ideal");
   K:=coefficientRing ambient source phi; 
   if not (K === coefficientRing target phi) then error("different coefficient rings encountered");
   if not isField K then error("the coefficient ring needs to be a field");
   F:=submatrix(matrix phi,{0..(numgens source phi -1)});
   if not (isHomogeneous ideal F and # unique degrees ideal F == 1) then error("the map needs to be defined by homogeneous polynomials of the same degree");
   if (class o.AssumeOrdinary =!= Boolean and o.AssumeOrdinary =!= null) then error "expected true or false for option AssumeOrdinary";
   n:=numgens target phi -1;
   m:=numgens ambient source phi -1;
   x:=local x; y:=local y;
   R:=K[x_1..x_n,y_0..y_m,MonomialOrder=>Eliminate n];
   jacF:=(map(R,target phi,{1,x_1..x_n})) transpose jacobian F;
   J:=ideal(matrix{{y_0..y_m}} * jacF);
   gbJ:=if o.AssumeOrdinary === true then gb(J,SubringLimit=>1) else gb J;
   sub(sub(ideal selectInSubring(1,gens gbJ),K[y_0..y_m]),vars ambient source phi)
);

projectionMap = method(Options => {Variable => null, AffineChartGrass => true});

projectionMap (Ring) := o -> (G) -> (
   (k,n,KK,p):=detectGrassmannian G;
   if o.Variable =!= null then p = getSymbol toString o.Variable;
   R:=KK[p_(0,0)..p_(k,n)];
   M:=genericMatrix(R,n+1,k+1);
   psi:=map(R,G,gens minors(k+1,M));
   mnr:=o.AffineChartGrass;
   if mnr === false then return (psi,M);
   if mnr === true then mnr = (random toList(0..n))_{0..k};
   try assert(ring matrix{mnr} === ZZ and min mnr >=0 and max mnr <=n and #set mnr == k+1) else error("bad value for option AffineChartGrass: expected either boolean value or list of "|toString(k+1)|" distinct integers beetween 0 and "|toString(n)|" but got "|toString(mnr)); 
   mnr=sort mnr; 
   R=KK[flatten entries submatrix'(transpose M,mnr)];
   M=sub(sub(M,flatten for i to k list for j to k list M_(mnr_i,j) => (if i == j then 1 else 0)),R);
   psi=map(R,G,gens minors(k+1,M));
   (psi,submatrix'(transpose M,mnr))
);

isCoisotropic = method(TypicalValue => Boolean, Options => {AffineChartGrass => true})

isCoisotropic (RingElement) := o -> (F) -> (
   if o.AffineChartGrass === false then error "value not allowed for option AffineChartGrass";
   (psi,M):=projectionMap(ring F,AffineChartGrass=>o.AffineChartGrass);
   (m,n):=(numgens target M -1,numgens source M -1);
   f:=psi F;
   J:=matrix for i to m list for j to n list diff(M_(i,j),f);
   sub(minors(2,J),(ring J)/f) == 0
);

fromPluckerToStiefel = method(Options => {Variable => null, AffineChartGrass => false})

fromPluckerToStiefel (RingElement) := o -> (I) -> (
   f:=first projectionMap(ring I,Variable=>o.Variable,AffineChartGrass=>o.AffineChartGrass);
   return(f I)
);

fromPluckerToStiefel (Matrix) := o -> (I) -> (
   f:=first projectionMap(ring I,Variable=>o.Variable,AffineChartGrass=>o.AffineChartGrass);
   return(f I)
);

fromPluckerToStiefel (Ideal) := o -> (I) -> (
   f:=first projectionMap(ring I,Variable=>o.Variable,AffineChartGrass=>o.AffineChartGrass);
   return(saturate(f I,ideal matrix f))
);


----------------------------------------------------------------------------------
---------------------------- Documentation ---------------------------------------
----------------------------------------------------------------------------------

     
beginDocumentation() 
document { 
    Key => Resultants, 
    Headline => "package for computation with resultants, discriminants, and Chow forms", 
    PARA{"This package provides methods to deal with resultants and discriminants of multivariate polynomials, and with higher associated subvarieties of irreducible projective varieties. The main methods are: ", TO "Resultant",", ",TO "Discriminant", ", ", TO "ChowForm",", and ",TO "tangentialChowForm",". For the mathematical theory, we refer to the following two books: ", HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"},", by David A. Cox, John Little, Donal O'shea; ", HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky. Other references for the theory of Chow forms are: ", HREF{"https://projecteuclid.org/euclid.dmj/1077305197","The equations defining Chow varieties"}, ", by M. L. Green and I. Morrison; ", HREF{"http://link.springer.com/article/10.1007/BF02567693","Multiplicative properties of projectively dual varieties"},", by J. Weyman and A. Zelevinsky; and the preprint ",HREF{"https://arxiv.org/abs/1607.05932","Coisotropic Hypersurfaces in the Grassmannian"}, ", by K. Kohn."},
    EXAMPLE {
      "-- Resultant of two generic binary quartics
F = genericPolynomials(QQ,{4,4})",
     "time R = Resultant F",
      "-- X-resultant of the rational normal quartic curve
P4 = QQ[a_0..a_4]; C4 = minors(2,matrix{{a_0..a_3},{a_1..a_4}})",
      "R == sub(Xresultant C4,vars ring R)",
      "-- Discriminant of a generic binary quintic
G = first genericPolynomials(QQ,{5,-1})",
      "time D = Discriminant G",
      "-- Dual hypersurface of the rational normal quintic curve
C5 = minors(2,matrix{(gens ring D)_{0..4},(gens ring D)_{1..5}})",
      "D == (Dual C5)_0"
    }
}
undocumented{Poisson,Macaulay,Poisson2,Macaulay2}
document { 
    Key => {[Resultant,Algorithm],[Discriminant,Algorithm]}, 
    "This option determines which algorithm will be used to compute the ",TO Resultant, ". There are currently four algorithms implemented: ",
    PARA{},
    "[",TT "Algorithm => Poisson", "]"," (default) the resultant is computed, recursively, through the Poisson Formula (see [1, Theorem 3.4]);",
    PARA{}, 
    "[", TT "Algorithm => Macaulay" ,"]"," the resultant is computed as a Macaulay resultant, i.e. as a ratio of two determinants (see [1, Theorem 4.9]);",
    PARA{}, 
    "[", TT "Algorithm => Poisson2"," and ",TT "Algorithm => Macaulay2" ,"]"," these are variants of the above ones using interpolation of multivariate polynomials.",
    PARA{},
    "[1] David A. Cox, John Little, Donal O'shea - ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"}, ", Graduate Texts in Mathematics, Volume 185 (2005).", 
    PARA{},
    EXAMPLE {
        "R = QQ[a,b][x,y,z,w]",
        "F = {(7/3)*x+(7/2)*y+z+2*w, ((10/7)*a+b)*x^2+(a+(5/4)*b)*x*y+(2*a+(1/2)*b)*y^2+((7/8)*a+(7/5)*b)*x*z+((3/4)*a+b)*y*z+((7/8)*a+(1/7)*b)*z^2+((5/7)*a+(4/3)*b)*x*w+(9*a+10*b)*y*w+((7/5)*a+(3/4)*b)*z*w+((4/3)*a+5*b)*w^2, ((1/2)*a+(7/5)*b)*x^3+((1/2)*a+10*b)*x^2*y+((8/9)*a+(3/5)*b)*x*y^2+(a+(7/6)*b)*y^3+((3/7)*a+(3/4)*b)*x^2*z+((1/3)*a+(9/10)*b)*x*y*z+((9/4)*a+b)*y^2*z+((1/6)*a+(1/5)*b)*x*z^2+(3*a+(5/2)*b)*y*z^2+((5/3)*a+(3/7)*b)*z^3+(a+b)*x^2*w+((4/5)*a+(5/4)*b)*x*y*w+((5/3)*a+(5/8)*b)*y^2*w+((3/2)*a+(1/6)*b)*x*z*w+((1/3)*a+(4/5)*b)*y*z*w+(9*a+(1/3)*b)*z^2*w+((7/3)*a+(5/4)*b)*x*w^2+(a+(3/4)*b)*y*w^2+((9/8)*a+(7/8)*b)*z*w^2+((9/7)*a+2*b)*w^3, 2*x+(1/4)*y+(8/3)*z+(4/5)*w}",
        "time Resultant(F,Algorithm=>Poisson2)",
        "time Resultant(F,Algorithm=>Macaulay2)",
        "time Resultant(F,Algorithm=>Poisson)",
        "time Resultant(F,Algorithm=>Macaulay)",
         "o3 == o4 and o4 == o5 and o5 == o6"
            }
} 
document { 
    Key => {Xresultant,(Xresultant,Ideal)}, 
    Headline => "X-resultant of a projective variety", 
    Usage => "Xresultant I", 
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///},
              }, 
    Outputs => { {"the X-resultant of ", TEX///$X$///}, 
               },
          PARA{"This is equivalent to both of the following ",TT "fromPluckerToStiefel dualize ChowForm I"," and ", TT "(last CayleyTrick I)_0","."},
    SeeAlso => {Resultant,ChowForm,CayleyTrick}
} 

document { 
    Key => {Resultant,(Resultant,Matrix),(Resultant,List)}, 
    Headline => "multipolynomial resultant", 
    Usage => "Resultant F", 
    Inputs => { "F" => Matrix => {"a row matrix whose entries are ", TEX///$n+1$///," homogeneous polynomials ", TEX///$F_0,\ldots,F_n$///," in ", TEX///$n+1$///," variables (or a ", TO List," to be interpreted as such a matrix)"} 
}, 
    Outputs => { 
    {"the resultant of ",TEX///$F_0,\ldots,F_n$///} 
}, 
    PARA{"Let ",TEX///$F_0,\ldots,F_n$///," be ",TEX///$n+1$///," homogeneous polynomials in ",TEX///$n+1$///," variables ",TEX///$x_0,\ldots,x_n$///," over a commutative ring ",TEX///$K$///,". The resultant ",TEX///$R(F_0,\ldots,F_n)$///," is a certain polynomial in the coefficients of ",TEX///$F_0,\ldots,F_n$///,"; when ",TEX///$K$///," is an algebraically closed field, ",TEX///$R(F_0,\ldots,F_n)$///," vanishes if and only if ",TEX///$F_0,\ldots,F_n$///," have a common nontrivial root.  For the general theory, see one of the following: ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"},", by David A. Cox, John Little, Donal O'shea; ", HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky."},
    EXAMPLE { 
    "ZZ[t,u,v,w][x,y]",
    "F = {x^7+3*x^4*y^3+t*x*y^6+u*y^7,x^8+x^5*y^3+v*x*y^7+w*y^8}",
    "time Resultant F",
    "{random(1,ring oo),random(4,ring oo),random(2,ring oo),random(1,ring oo)}",
    "time Resultant oo"
    },
    PARA{"Below we compute the general expression of the resultant of a linear, a quadratic and a cubic form on ",TEX///$\mathbb{P}^2$///,"."},
    EXAMPLE {
    "F = genericPolynomials {1,2,3}",
    "time Resultant F",
    },
    PARA{"On page 88 of ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"}," is stated that the resultant of three ternary quadrics, written out in its full glory, has 21.894 terms."},
    EXAMPLE {
    "F = genericPolynomials {2,2,2}",
    "time # terms Resultant F"
    },
    SeeAlso => {resultant,ChowForm,Xresultant,Discriminant} 
}

document { 
    Key => {Discriminant,(Discriminant,RingElement)}, 
    Headline => "resultant of the partial derivatives", 
    Usage => "Discriminant F", 
    Inputs => { "F" => RingElement => {"a homogeneous polynomial"} 
}, 
    Outputs => { 
    {"the discriminant of ",TT "F"} 
}, 
    PARA{"The discriminant of a homogeneous polynomial is defined, up to a scalar factor, as the ",TO Resultant," of its partial derivatives. For the general theory, see one of the following: ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"},", by David A. Cox, John Little, Donal O'shea; ", HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky."},
    EXAMPLE { 
    "ZZ[a,b,c][x,y]; F = a*x^2+b*x*y+c*y^2",
    "time Discriminant F",
    "ZZ[a,b,c,d][x,y]; F = a*x^3+b*x^2*y+c*x*y^2+d*y^3",
    "time Discriminant F",
},
    PARA{"The next example illustrates how computing the intersection of a pencil generated by two degree ",TEX///$d$///," forms ",TEX///$F(x_0,\ldots,x_n), G(x_0,\ldots,x_n)$///," with the discriminant hypersurface in the space of forms of degree ",TEX///$d$///," on ",TEX///$\mathbb{P}^n$///},
    EXAMPLE {
    "x=symbol x; R=ZZ/331[x_0..x_3]",
    "F=x_0^4+x_1^4+x_2^4+x_3^4",
    "G=x_0^4-x_0*x_1^3-x_2^4+x_2*x_3^3",
    "R'=ZZ/331[t_0,t_1][x_0..x_3];",
    "pencil=t_0*sub(F,R')+t_1*sub(G,R')",
    "time D=Discriminant pencil",
    "factor D"
},
    SeeAlso => {discriminant,Dual,Resultant} 
}

document { 
    Key => {genericPolynomials,(genericPolynomials,Ring,List),(genericPolynomials,List)}, 
    Headline => "generic homogeneous polynomials", 
    Usage => "genericPolynomials(K,d) 
              genericPolynomials d",
    Inputs => { "K" => Ring => {"optional, with default value ",TO ZZ},
                "d" => List => {TT"n+1"," integers ",TT"d_0,...,d_n"} 
              }, 
    Outputs => { {TT"n+1"," generic homogeneous polynomials of degrees ",TT "d_0,...,d_n", " in the ring ",TT "K[a_0,a_1,...,b_0,b_1,...][x_0,...,x_n]"}
             },
    EXAMPLE {
         "genericPolynomials {1,2,3}",
         "first genericPolynomials(ZZ/101,{4,2,3})",
         "first genericPolynomials(ZZ/101,{4,-1,-1})"
            }
}

document { 
    Key => {tangentialChowForm,(tangentialChowForm,Ideal,ZZ)}, 
    Headline => "higher Chow forms of a projective variety", 
    Usage => "tangentialChowForm(I,s)",
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///,", say of dimension ", TEX///$k$///},
                "s" => ZZ  
              }, 
Outputs => { {"or an ", TO "Ideal", " (if there is more than one generator) in the coordinate ring of the Grassmannian ",TEX///$\mathbb{G}(n-k-1+s,\mathbb{P}^n)$///," in the Plucker embedding, representing the higher associated subvariety ",TEX///$Z_s(X)$///} 
           }, 
    "For a projective variety ",TEX///$X\subset\mathbb{P}^n$///," of dimension ",TEX///$k$///, ", the ",TEX///$s$///,"-th associated subvariety ",TEX///$Z_s(X)\subset\mathbb{G}(n-k-1+s,\mathbb{P}^n)$///, " (also called tangential Chow form) is defined to be the closure of the set of ", TEX///$(n-k-1+s)$///, "-dimensional subspaces ",TEX///$L\subset \mathbb{P}^n$///, " such that ", TEX///$L\cap X\neq\emptyset$///, " and ",TEX///$dim(L\cap T_x(X))\geq s$///, " for some smooth point ",TEX///$x\in L\cap X$///, ", where ",TEX///$T_x(X)$///, " denotes the embedded tangent space to ", TEX///$X$///, " at ",TEX///$x$///,". In particular, ", TEX///$Z_0(X)\subset\mathbb{G}(n-k-1,\mathbb{P}^n)$///, " is defined by the Chow form of ", TEX///$X$///, ", while ",TEX///$Z_k(X)\subset\mathbb{G}(n-1,\mathbb{P}^n)$///, " is identified to the dual variety ", TEX///$X^{*}\subset\mathbb{P}^n^{*}=\mathbb{G}(0,\mathbb{P}^n^{*})$///, " via the duality of Grassmannians ", TEX///$\mathbb{G}(0,\mathbb{P}^n^{*})=\mathbb{G}(n-1,\mathbb{P}^n)$///,". For details we refer to the third chapter of ", HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky. ",
    PARA{
    "The algorithm used are standard, based on projections of suitable incidence varieties. Here are some of the available options that could speed up the computation."
        },    
    PARA{
    TT "Duality"," Taking into account the duality of Grassmannians, one can perform the computation in ", TEX///$\mathbb{G}(k-s,n)$///, " and then passing to ", TEX///$\mathbb{G}(n-k-1+s,n)$///, ". This is done by default when it seems advantageous."
        },   
    PARA{
    TT "AffineChartGrass", " If one of the standard coordinate charts on the Grassmannian is specified, then the internal computation is done on that chart. By default, a random chart is used. Set this to ",TT"false"," to not use any chart."
        },   
    PARA{
    TT "AffineChartProj", " This is quite similar to ",TT "AffineChartGrass",", but it allows to specify one of the standard coordinate charts on the projective space. You should set this to ",TT"false"," for working with reducible or degenerate varieties."
        },   
    PARA{
    TT "AssumeOrdinary", " Set this to ",TT"true"," if you know that ",TEX///$Z_s(X)$///," is a hypersurface (by default is already ",TT"true"," if ",TEX///$s=0$///,")."
        },
    EXAMPLE { 
          "-- cubic rational normal scroll surface in P^4=G(0,4)
use Grass(0,4,Variable=>p); S = minors(2,matrix{{p_0,p_2,p_3},{p_1,p_3,p_4}})",
          "-- 0-th associated hypersurface of S in G(1,4) (Chow form)
time tangentialChowForm(S,0)",
          "-- 1-th associated hypersurface of S in G(2,4)
time tangentialChowForm(S,1)",
          "-- 2-th associated hypersurface of S in G(3,4) (parameterizing tangent hyperplanes to S)
time tangentialChowForm(S,2)",
          "-- we get the dual hypersurface of S in G(0,4) by dualizing
time S' = ideal dualize tangentialChowForm(S,2)",
          "-- we then can recover S
time dualize tangentialChowForm(S',3) == S"
            },
    SeeAlso => {isCoisotropic,ChowForm}
}

document { 
    Key => {ChowForm,(ChowForm,Ideal),(ChowForm,RingMap)}, 
    Headline => "Chow form of a projective variety", 
    Usage => "ChowForm I", 
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///},
              }, 
    Outputs => { {"the Chow form of ",TEX///$X$///," in the coordinate ring of the Grassmannian ",TEX///$\mathbb{G}(n-dim(X)-1,\mathbb{P}^n)$///," in the Plucker embedding"}
                },
    "This is the same as ",TT "tangentialChowForm(I,0)", ", see ",TO "tangentialChowForm","."
}

undocumented {(ChowEquations,RingElement,ZZ)};

document { 
    Key => {ChowEquations,(ChowEquations,RingElement)}, 
    Headline => "Chow equations of a projective variety", 
    Usage => "ChowEquations W", 
    Inputs => { "W" => RingElement => {"the Chow form of an irreducible projective variety ",TEX///$X\subset\mathbb{P}^n$///,}
              }, 
Outputs => { {"generated by the Chow equations of ",TEX///$X$///} 
           },
    "Given the Chow form ",TEX///$Z_0(X)\subset\mathbb{G}(n-k-1,n)$///," of an irreducible projective ",TEX///$k$///,"-dimensional variety ", TEX///$X\subset\mathbb{P}^n$///, ", one can recover a canonical system of equations, called Chow equations, that always define ",TEX///$X$///, " set-theoretically, and also scheme-theoretically whenever ",TEX///$X$///, " is smooth. For details, see chapter 3, section 2C of ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky.",
    PARA{},
    EXAMPLE { 
          "P3 = Grass(0,3,ZZ/11,Variable=>x)",
          "-- an elliptic quartic curve
C = ideal(x_0^2+x_1^2+x_2^2+x_3^2,x_0*x_1+x_1*x_2+x_2*x_3)",
          "-- Chow equations of C
time eqsC = ChowEquations ChowForm C",
          "C == saturate eqsC",
          "-- a singular irreducible curve 
D = ideal(x_1^2-x_0*x_2,x_2^3-x_0*x_1*x_3,x_1*x_2^2-x_0^2*x_3)",
          "-- Chow equations of D
time eqsD = ChowEquations ChowForm D",
          "D == saturate eqsD",
          "D == radical eqsD"
            },
    PARA{},
    "Actually, one can use ", TT "ChowEquations", " to recover a variety ",TEX///$X$///," from some other of its tangential Chow forms as well. This is based on generalizations of the \"Cayley trick\", see ",HREF{"http://link.springer.com/article/10.1007/BF02567693","Multiplicative properties of projectively dual varieties"},", by J. Weyman and A. Zelevinsky; see also the preprint ", HREF{"https://arxiv.org/abs/1607.05932","Coisotropic Hypersurfaces in the Grassmannian"}, ", by K. Kohn. For instance, ",
    PARA{},
    EXAMPLE { 
          " Q = ideal(x_0*x_1+x_2*x_3)",
          "-- tangential Chow forms of Q
time (W0,W1,W2) = (tangentialChowForm(Q,0),tangentialChowForm(Q,1),tangentialChowForm(Q,2))",
          "time (Q,Q,Q) == (ChowEquations(W0,0),ChowEquations(W1,1),ChowEquations(W2,2))"
            },
    PARA{},
    "Note that ",TT "ChowEquations(W,0)", " is not the same as ",TT "ChowEquations W","."
}

undocumented {(CayleyTrick,Ideal,ZZ)};

document { 
    Key => {CayleyTrick,(CayleyTrick,Ideal)}, 
    Headline => "Cayley trick and X-resultant", 
    Usage => "CayleyTrick I", 
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///,", say of dimension ", TEX///$k$///},
              }, 
    Outputs => { {"a pair ",TEX///$(J,J')$///, ", where ",TEX///$J$///, " is the defining ideal of the Segre product ", TEX///$X\times\mathbb{P}^k\subset\mathbb{P}^{(k+1)(n+1)-1}$///, ", while ", TEX///$J'$///, " is the principal ideal defining the dual hypersurface ", TEX///$(X\times\mathbb{P}^k)^{*}\subset{\mathbb{P^{(k+1)(n+1)-1}}}^{*}$///}
               },
    Consequences => { 
          TT "Dual J == J' and J == Dual J'" 
                    }, 
"Let ",TEX///$X\subset\mathbb{P}^n$///," be a ",TEX///$k$///,"-dimensional projective variety. Consider the product ", TEX///$W = X\times\mathbb{P}^k$///," as a subvariety of ",TEX///$\mathbb{P}(Mat(k+1,n+1))$///,", the projectivization of the space of ",TEX///$(k+1)\times (n+1)$///, "-matrices, and consider the projection ",TEX///$p:\mathbb{P}(Mat(k+1,n+1))--->\mathbb{G}(k,n)=\mathbb{G}(n-k-1,n)$///,". Then the \"Cayley trick\" states that the dual variety ",TEX///$W^*$///," of ", TEX///$W$///," equals the closure of ",TEX///$p^{-1}(Z_0(X))$///, ", where ", TEX///$Z_0(X)\subset\mathbb{G}(n-k-1,n)$///," is the Chow hypersurface of ",TEX///$X$///,". ", TEX///$W^*$///," is also called ", TEX///$X$///,"-resultant. For details and proof, see ",HREF{"http://link.springer.com/article/10.1007/BF02567693","Multiplicative properties of projectively dual varieties"},", by J. Weyman and A. Zelevinsky; see also the preprint ", HREF{"https://arxiv.org/abs/1607.05932","Coisotropic Hypersurfaces in the Grassmannian"}, ", by K. Kohn.",
    PARA{},
    EXAMPLE { 
          "-- X-resultant of the quadric surface P1xP1
QQ[t,x,y,z]; P1xP1 = ideal(x*y-t*z)",
          "time (P1xP1xP2,P1xP1xP2') = CayleyTrick(P1xP1,Variable=>c)",
            },
    PARA{"Actually, using other versions of the Cayley trick, we can easily obtain the dual variety of the Segre product ", TEX///$X\times\mathbb{P}^s$///," for other values of ",TEX///$s$///,". For instance, the ideal of ",TEX///$\mathbb{P}^1\times\mathbb{P}^1\times\mathbb{P}^1\subset\mathbb{P}^7$///," and that of its dual can be computed as follows:"},
    EXAMPLE { 
          "time (P1xP1xP1,P1xP1xP1') = CayleyTrick(P1xP1,1,Variable=>c)",
            },
    SeeAlso => {Xresultant}
}

document { 
    Key => {dualize,(dualize,RingElement),(dualize,Matrix),(dualize,Ideal),(dualize,RingMap),(dualize,VisibleList)}, 
    Headline => "apply duality of Grassmannians", 
    Usage => "dualize f", 
    Inputs => { "f" => RingElement => {" or a ",TO "Matrix", ", or an ",TO "Ideal",", in the coordinate ring (resp. ambient ring) of a Grassmannian ",TEX///$\mathbb{G}(k,n)$///}
              }, 
    Outputs => { {"the image of ", TT "f", " in the coordinate ring (resp. ambient ring) of ", TEX///$\mathbb{G}(n-k-1,n)$///," via the duality of Grassmannians"}
               },
    "This method implements the natural identification ", TEX///$\mathbb{G}(k,\mathbb{P}^n)\to\mathbb{G}(n-k-1,{\mathbb{P}^n}^{*})$///,", which takes a subspace ", TEX///$L\in\mathbb{G}(k,\mathbb{P}^n)$///, " to its orthogonal complement ",TEX///$L^*\in\mathbb{G}(n-k-1,{\mathbb{P}^n}^*)$///,".",
    PARA{},
    EXAMPLE { 
          "P9 = ambient Grass(2,4,ZZ/13,Variable=>x)",
          "vars P9",
          "dualize vars P9",
          "F  = random(2,P9)",
          "dualize F",
          "F == dualize dualize F",
            },
}

document { 
    Key => {fromPluckerToStiefel,(fromPluckerToStiefel,RingElement),(fromPluckerToStiefel,Matrix),(fromPluckerToStiefel,Ideal)}, 
    Headline => "convert from Plucker coordinates to Stiefel coordinates", 
    Usage => "fromPluckerToStiefel f", 
    Inputs => { "f" => RingElement => {" or a ",TO "Matrix", ", or an ",TO "Ideal",", in the coordinate ring of a Grassmannian ",TEX///$\mathbb{G}(k,n)$///}
              }, 
    Outputs => { {"the representation of ",TT"f"," in the Stiefel coordinates of ",TEX///$\mathbb{G}(k,n)$///," (or in the affine coordinates if an affine chart is given with ",TO"AffineChartGrass",")"}
               },
    PARA{},
    EXAMPLE { 
          "-- Chow form of the twisted cubic 
use Grass(0,3); w = ChowForm minors(2,matrix{{p_0..p_2},{p_1..p_3}})",
          "time fromPluckerToStiefel w",
          "time fromPluckerToStiefel(w,AffineChartGrass=>{0,1})",
          "fromPluckerToStiefel(w,AffineChartGrass=>{2,3},Variable=>a)"
            },
    PARA{"As an application, we check that the singular locus of the Chow form of the twisted cubic has dimension 2 (on each standard chart)."},
    EXAMPLE { 
          "time U = apply(subsets(4,2),s->ideal fromPluckerToStiefel(w,AffineChartGrass=>s))",
          "time apply(U,u->dim singularLocus u)"
            }
}

document { 
    Key => {Grass,(Grass,ZZ,ZZ),(Grass,ZZ,ZZ,Ring)}, 
    Headline => "coordinate ring of a Grassmannian", 
    Usage => "Grass(k,n)
              Grass(k,n,K)",
    Inputs => {"k" => ZZ ,
               "n" => ZZ ,
               "K" => Ring => {"optional with default value ",TO "QQ",", the coefficient ring to be used"}
              }, 
    Outputs => { {"the coordinate ring of the Grassmannian variety of all projective ",TEX///$k$///,"-planes in ",TEX///$\mathbb{P}^n$///}
               },
    "This method calls the method ", TO "Grassmannian", ", and ", TT "Grass(k,n,K,UseVarible=>p)", " can be considered equivalent to ", TT "quotient Grassmannian(k,n,Variable=>p,CoefficientRing=>K)", ". However, over ", TT "QQ", " and ", TT "ZZ/p", ", the method ",TT "Grass", " creates only an instance of ring for any given ", TT "(k,n,K,p)",".", 
    PARA{},
    EXAMPLE { 
          "Grass(1,3) === Grass(1,3)",
          "Grass(2,4,ZZ/11,Variable=>t) === Grass(2,4,ZZ/11,Variable=>t)"
            },
     PARA{},
     "In order to facilitate comparisons, the outputs of the methods ", TO "ChowForm",", ",TO "tangentialChowForm",", ",TO "ChowEquations",", and ",TO "dualize", " always lie in these rings.",
     EXAMPLE { 
          "R = ZZ/11[x_0..x_3]",
          "L = trim ideal(random(1,R),random(1,R))",
          "w = ChowForm L",
          "ring w === Grass(1,3,ZZ/11,Variable=>x)",
          "L'= ChowEquations w",
          "ring L' === Grass(0,3,ZZ/11,Variable=>x)"
            }
}

document {
    Key => {[Grass,Variable],[tangentialChowForm,Variable],[ChowForm,Variable],[ChowEquations,Variable],[CayleyTrick,Variable],[fromPluckerToStiefel,Variable],[Xresultant,Variable]},
    Headline => "specify a name for a variable", 
    TT "Variable => x"," -- an option used to specify a symbol to be used as a name for the generator of the coordinate ring of the Grassmannian."
} 

document {
    Key => {AssumeOrdinary, [tangentialChowForm,AssumeOrdinary],[Dual,AssumeOrdinary]},
    Headline => "whether the expected codimension is 1", 
    TT "AssumeOrdinary => true/false"," -- an option for ", TO "tangentialChowForm"," and ",TO "Dual"," that controls the option ", TO "SubringLimit",", used internally with ", TO "kernel", " and ", TO "gb",". You can set this to ",TT"true"," when the expected output should represent a hypersurface."
} 

document {
    Key => {Duality, [tangentialChowForm,Duality],[ChowForm,Duality]},
    Headline => "whether to use dual Plucker coordinates", 
    TT "Duality => true/false"," -- an option for ",TO "ChowForm", " and ", TO "tangentialChowForm",", to specify whether to perform internal computations using the dual Plucker coordinates."
} 

document {
    Key => {AffineChartGrass, [tangentialChowForm,AffineChartGrass],[ChowForm,AffineChartGrass], [isCoisotropic,AffineChartGrass],[fromPluckerToStiefel,AffineChartGrass]},
    Headline => "use an affine chart on the Grassmannian", 
    TT "AffineChartGrass => l", " -- here ",TT "l"," is a list of ",TEX///$k+1$///," distinct integers between 0 and ",TEX///$n$///,", corresponding to an affine chart ",TEX///$U$///," on the Grassmannian ",TEX///$\mathbb{G}(k,n)$///,". This indicates to the method that should perform internal computations using the chart ",TEX///$U$///,". Set this to ", TT "false"," (resp. ",TT "true",") to not use any chart (resp. to use a random chart)."
} 

document {
    Key => {AffineChartProj, [tangentialChowForm,AffineChartProj],[ChowForm,AffineChartProj]},
    Headline => "use an affine chart on the projective space", 
    TT "AffineChartProj => l", " -- here ",TT "l"," is an integer between 0 and ",TEX///$n$///,", corresponding to an affine chart ",TEX///$U$///," on the projective space ",TEX///$\mathbb{P}^n$///,". This option, quite similar to ",TO "AffineChartGrass", ", indicates to perform internal computations using the chart ",TEX///$U$///,"."
} 

document { 
    Key => {Dual,(Dual,Ideal),(Dual,RingMap)}, 
    Headline => "projective dual variety", 
    Usage => "Dual I", 
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///},
              }, 
    Outputs => { {"the ideal of the projective dual variety ",TEX///$X^{*}\subset{\mathbb{P}^n}^{*}$///}
                },
    "This is basically a shortcut for ",TT "dualize tangentialChowForm(I,dim I -1)",".", 
    PARA{},
    EXAMPLE { 
          "-- Veronese surface in P^5
V = trim minors(2,genericSymmetricMatrix(QQ[x_0..x_5],3))",
          "time Dual V",
          "-- reflexivity theorem
time V == Dual Dual V"
            },
   SeeAlso => {Discriminant}
}

document { 
    Key => {isCoisotropic,(isCoisotropic,RingElement)}, 
    Headline => "whether a hypersurface of a Grassmannian is a tangential Chow form", 
    Usage => "isCoisotropic w",
    Inputs => { "w" => RingElement => {" representing a hypersurface of a Grassmannian"},
              }, 
    Outputs => { {"whether ",TT"w", " is a tangential Chow form of some projective variety"}
                },
    "The algorithm implemented is based on Proposition 3.12 in Chapter 4 of ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"},", by Israel M. Gelfand, Mikhail M. Kapranov and Andrei V. Zelevinsky.",
    PARA{},
    EXAMPLE { 
          "-- first tangential Chow form of a random quadric in P^3
w = tangentialChowForm(ideal random(2,Grass(0,3)),1)",
          "time isCoisotropic w",
          "-- random quadric in G(1,3)
w' = sub(random(2,ambient Grass(1,3)),Grass(1,3))",
          "time isCoisotropic w'"
            }
}

----------------------------------------------------------------------------------
------------------------------- Tests --------------------------------------------
----------------------------------------------------------------------------------

TEST /// 
genericResultant = {Algorithm => Poisson} >> o -> (d,K) -> ( 
    F:=matrix{genericPolynomials(K,toList d)};
    Resultant(F,Algorithm=>o.Algorithm)
);
a:=local a; ring112=ZZ[a_(0,0),a_(0,1),a_(0,2),a_(1,0),a_(1,1),a_(1,2),a_(2,0),a_(2,1),a_(2,2),a_(2,3),a_(2,4),a_(2,5)];
res112=a_(0,0)^2*a_(1,1)^2*a_(2,5)-a_(0,0)^2*a_(1,1)*a_(1,2)*a_(2,4)+a_(0,0)^2*a_(1,2)^2*a_(2,3)-2*a_(0,0)*a_(0,1)*a_(1,0)*a_(1,1)*a_(2,5)+a_(0,0)*a_(0,1)*a_(1,0)*a_(1,2)*a_(2,4)+a_(0,0)*a_(0,1)*a_(1,1)*a_(1,2)*a_(2,2)-a_(0,0)*a_(0,1)*a_(1,2)^2*a_(2,1)+a_(0,1)^2*a_(1,0)^2*a_(2,5)-a_(0,1)^2*a_(1,0)*a_(1,2)*a_(2,2)+a_(0,1)^2*a_(1,2)^2*a_(2,0)+a_(0,0)*a_(0,2)*a_(1,0)*a_(1,1)*a_(2,4)-a_(0,0)*a_(0,2)*a_(1,1)^2*a_(2,2)-2*a_(0,0)*a_(0,2)*a_(1,0)*a_(1,2)*a_(2,3)+a_(0,0)*a_(0,2)*a_(1,1)*a_(1,2)*a_(2,1)-a_(0,1)*a_(0,2)*a_(1,0)^2*a_(2,4)+a_(0,1)*a_(0,2)*a_(1,0)*a_(1,1)*a_(2,2)+a_(0,1)*a_(0,2)*a_(1,0)*a_(1,2)*a_(2,1)-2*a_(0,1)*a_(0,2)*a_(1,1)*a_(1,2)*a_(2,0)+a_(0,2)^2*a_(1,0)^2*a_(2,3)-a_(0,2)^2*a_(1,0)*a_(1,1)*a_(2,1)+a_(0,2)^2*a_(1,1)^2*a_(2,0);
assert(sub(genericResultant((1,1,2),ZZ,Algorithm=>Poisson),vars ring112) == res112);
ResM=genericResultant((1,3,1),ZZ,Algorithm=>Macaulay); ResP=genericResultant((1,3,1),ZZ,Algorithm=>Poisson); ResP=sub(ResP,vars ring ResM);
assert(ResM - ResP == 0 or ResM + ResP == 0)
ResM=genericResultant((2,1,2),ZZ,Algorithm=>Macaulay); ResP=genericResultant((2,1,2),ZZ,Algorithm=>Poisson); ResP=sub(ResP,vars ring ResM);
assert(ResM - ResP == 0 or ResM + ResP == 0)
///

TEST ///
invariance = (d,K) -> (
    n:=#d -1; x:=local x; R:=K[x_0..x_n];
    F:=matrix({for i to n list sub(random(d_i,ZZ[x_0..x_n]),R)});
    A:=matrix for i to n list for j to n list sub(random(-10,10),K); 
    Sub:=map(R,R,transpose(A*(transpose vars R)));
    F':=Sub(F);
    (Resultant F') == (det A)^(product toList d) * (Resultant F)
);
assert(invariance((4,6),ZZ)) 
assert(invariance((1,2,3),ZZ)) 
assert(invariance((2,3,5),QQ)) 
assert(invariance((1,1,2),ZZ[z])) 
assert(invariance((3,3,1,1,2),ZZ/331))
/// 
    
TEST ///
PoissonVsMacaulay = (d,K) -> (
    n:=#d -1; x:=local x; R:=K[x_0..x_n];
    F:=matrix({for i to n list try random(d_i,R) else sub(random(d_i,ZZ[x_0..x_n]),R)});
    poiRes:=Resultant(F,Algorithm=>Poisson);
    macRes:=Resultant(F,Algorithm=>Macaulay);
    (poiRes - macRes) * (poiRes + macRes) == 0
);
assert(PoissonVsMacaulay((4,6),ZZ)) 
assert(PoissonVsMacaulay((1,2,3),ZZ)) 
assert(PoissonVsMacaulay((2,3,5),QQ)) 
assert(PoissonVsMacaulay((1,1,2),ZZ[z])) 
assert(PoissonVsMacaulay((3,3,1,1,2),ZZ/331))
assert(PoissonVsMacaulay((3,2,2,2),ZZ/33331))
/// 

TEST /// 
genericDiscriminant = {Algorithm => Poisson} >> o -> (d,n,K) -> ( 
    F:=first genericPolynomials(K,prepend(d,toList(n:(-1))));
    Discriminant(F,Algorithm=>o.Algorithm)
);
Veronese = (d,n,K) -> (
    Gg:=Grass(0,n,K,Variable=>h);
    kernel map(Gg,Grass(0,binomial(n+d,d)-1,K),gens (ideal vars Gg)^d)
);
compareDiscriminants = (d,n,K) -> (
    D1:=genericDiscriminant(d,n,K,Algorithm=>Poisson);
    D1':=sub(genericDiscriminant(d,n,K,Algorithm=>Macaulay),vars ring D1);
    D2:=sub((Dual Veronese(d,n,K))_0,vars ring D1);
    ideal(D2) == ideal(D1) and ideal(D1) == ideal(D1')
);
assert compareDiscriminants(2,1,QQ) 
assert compareDiscriminants(3,1,GF 5^5) 
assert compareDiscriminants(4,1,QQ) 
assert compareDiscriminants(5,1,ZZ/33331) 
assert compareDiscriminants(2,2,ZZ/101)
/// 

TEST ///
randomPols = (K,m,e,d) -> ( -- homogeneous polynomials in K[a_0..a_m][x_0..x_n]
  n:=#d-1; assert(#d==#e);
  a:=local a; x:=local x;
  R:=K[a_0..a_m,x_0..x_n,Degrees=>{(m+1):{1,0},(n+1):{0,1}}];   
  R':=K[a_0..a_m][x_0..x_n];
  toList apply(0..n,i->sub(random({e_i,d_i},R),R'))
)
checkInterpolate = (K,m,e,d) -> (
   F:=randomPols(K,m,e,d);
   <<"F="<<F<<endl;
   time R1 := Resultant(F,Algorithm=>Poisson2);
   time R2 := Resultant(F,Algorithm=>Macaulay2);
   time R3 := Resultant(F,Algorithm=>Poisson);
   time R4 := Resultant(F,Algorithm=>Macaulay);
   <<"Res="<<R2<<endl;
   assert(R1 == R3 and R2 == R4 and (R1-R2)*(R1+R2) == 0);
);
checkInterpolate(QQ,1,{1,1},{1,2});
checkInterpolate(GF(331^2),1,{1,1},{1,2});
checkInterpolate(QQ,1,{1,3},{1,2});
checkInterpolate(QQ,1,{1,2},{3,2})
checkInterpolate(ZZ/3331,1,{2,3},{3,4});
checkInterpolate(ZZ,2,{0,1},{3,2});
checkInterpolate(ZZ/3331,1,{2,0},{2,2});
checkInterpolate(ZZ,1,{1,3,0},{1,2,2});
checkInterpolate(ZZ,0,{1,3,0},{1,2,2});
checkInterpolate(ZZ,1,{1,0,0,2},{1,2,1,1});
G=first randomPols(ZZ,1,{1,0,0},{3,0,0})
assert(Discriminant(G,Algorithm=>Poisson2) == Discriminant(G,Algorithm=>Macaulay2))
///

TEST ///
pencil = (d,n,K) -> (
F:=random(d,Grass(0,n,K));
G:=random(d,Grass(0,n,K));
t:=local t;
x:=local x;
R:=(coefficientRing ring F)[t_0,t_1][x_0..x_(numgens ring F -1)];
G=t_0*sub(F,vars R)+t_1*sub(G,vars R);
time T1 = Discriminant(G,Algorithm=>Poisson2);
time T2 = Discriminant(G,Algorithm=>Macaulay2);
time T3 = Discriminant G;
assert(T1 == T3 and (T1+T2)*(T1-T2) == 0)
);
pencil(2,2,ZZ)
pencil(4,1,GF(101^3))
pencil(2,4,ZZ/331)
pencil(3,2,ZZ/331)
///

TEST ///
-- testing dualize
assert (dualize ideal Grass(1,4) == ideal Grass(2,4))
assert (dualize ideal Grass(0,4,ZZ/331) == ideal Grass(3,4,ZZ/331))
assert (dualize ideal Grass(3,5,Variable=>T) == ideal Grass(1,5,Variable=>T))
assert (dualize Grassmannian(2,6,CoefficientRing=>(ZZ/11),Variable=>r) == ideal Grass(3,6,ZZ/11,Variable=>r))
///

TEST ///
-- testing CayleyTrick
testCT = (U) -> (
  time (X,Y):=CayleyTrick U;
  if numgens Y != 1 then return false;
  f:=map((ring Y)/Y,ring Y,transpose jacobian Y);
  f X == 0
);
P3=ZZ/331[x_0..x_3]
assert testCT minors(2,matrix{{x_0..x_2},{x_1..x_3}})
assert testCT ideal random(1,P3)
assert testCT ideal random(2,P3)
P2=QQ[t_0,t_1,t_2]
C=ideal(t_0^3+t_1^3+t_2^3)
(W,W')=CayleyTrick C;
assert(W' == Dual W) 
///

TEST ///
-- testing ChowEquations 
P3=Grass(0,3,Variable=>y)
C=minors(2,matrix{{y_0..y_2},{y_1..y_3}})
assert(C == saturate ChowEquations ChowForm C)
///

TEST /// 
-- testing tangentialChowForm and Dual
checkOptions = (I,s) -> (
   test:=true;
   time Z:=tangentialChowForm(I,s);
   time test =          Z == tangentialChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>false,Duality=>false);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>false,Duality=>true);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>true, Duality=>false);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>true, Duality=>true);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>false,Duality=>false);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>false,Duality=>true);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>true, Duality=>false);
   time test = test and Z == tangentialChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>true, Duality=>true);
   test
);
-- rational normal scroll surface S(1,2) subset P^4
P4 = Grass(0,4,ZZ/3331,Variable=>x)
G14= Grass(1,4,ZZ/3331,Variable=>x)
X=minors(2,matrix{{x_0,x_2,x_3},{x_1,x_3,x_4}})
Z=x_(1,3)^2*x_(2,3)-x_(1,2)*x_(1,3)*x_(2,4)-x_(0,3)*x_(1,3)*x_(2,4)+x_(0,2)*x_(1,4)*x_(2,4)+x_(1,2)^2*x_(3,4)+x_(0,3)^2*x_(3,4)-2*x_(0,1)*x_(2,3)*x_(3,4)-x_(0,2)*x_(0,4)*x_(3,4)
assert (Z == ChowForm X)
assert checkOptions(X,0)
assert (X == Dual Dual X)
assert (X == Dual Dual(X,AssumeOrdinary=>true))
-- cone in P^4 over twisted cubic
X=minors(2,matrix{{x_1,x_2,x_3},{x_2,x_3,x_4}})
Z=x_(2,3)^3-x_(1,3)*x_(2,3)*x_(2,4)+x_(1,2)*x_(2,4)^2+x_(1,3)^2*x_(3,4)-2*x_(1,2)*x_(2,3)*x_(3,4)-x_(1,2)*x_(1,4)*x_(3,4)
assert (Z == ChowForm X)
assert checkOptions(X,0)
assert (X == Dual Dual X)
-- quadratic Fermat hypersurfaces 
isQuadricFermat = (X) -> (R:=ring X; x:=gens R; n:=#x-1; ideal(sum for i to n list x_i^2) == ideal X);
use Grass(0,5,ZZ/3331,Variable=>p)
Q = ideal sum for i to 5 list p_i^2
assert isQuadricFermat tangentialChowForm(Q,0)
assert isQuadricFermat tangentialChowForm(Q,1)
assert isQuadricFermat tangentialChowForm(Q,2)
assert isQuadricFermat tangentialChowForm(Q,3)
assert isQuadricFermat tangentialChowForm(Q,4)
assert checkOptions(X,3)
assert checkOptions(X,4)
///

TEST ///
testGCT = (X) -> (
   -- checking: dualize Z_(dimX-s)(X) == Z_(s-defX)(X*)
   X':=Dual X; dimX:=dim X -1; defX:=codim X' -1; Z:=local Z; cond:=true;
   <<"dim X = "<<dimX<<", def X = "<<defX<<endl;
   for s from defX to dimX do (
      Z = (dualize tangentialChowForm(X,dimX-s),tangentialChowForm(X',s-defX));
      cond = cond and ideal(Z_0) == ideal(Z_1);
      <<"s="<<s<<", (Z_"<<dimX-s<<"(X))' == Z_"<<s-defX<<"(X*) : "<<cond<<endl 
   );
   cond
);
-- X=S(1,2) subset P^4
P4=ZZ/101[x_0..x_4];
X=minors(2,matrix{{x_0,x_2,x_3},{x_1,x_3,x_4}})
assert testGCT X
-- cone over twisted cubic
Y=minors(2,matrix{{x_1,x_2,x_3},{x_2,x_3,x_4}})
assert testGCT Y
-- hyperplane
assert testGCT ideal(random(1,P4))
-- quadric hypersurface 
assert testGCT ideal(random(2,P4))
///

end


