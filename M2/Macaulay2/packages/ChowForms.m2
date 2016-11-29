
newPackage(
	"ChowForms",
	Version => "0.1", 
    	Date => "Nov 29, 2016",
    	Authors => {{Name => "Giovanni Staglianò", 
		     Email => "giovannistagliano@gmail.com" 
                    }
                   },
    	Headline => "Chow forms of projective varieties",
    	DebuggingMode => false,
        Reload => false
)

export{
       "ringGrassmannian", 
       "dualize",
       "ChowForm", 
       "ChowEquations",
       "CayleyTrick", 
       "UseVariable",
       "Duality",
       "AffineChartGrass",
       "AffineChartProj",
       "Dual",
       "AssumeOrdinary"
}

GG:=local GG;

intGrassmannian = method(TypicalValue => QuotientRing);

intGrassmannian (ZZ,ZZ,Ring,Thing) := (k,n,KK,pp) -> (
   if ( KK =!= QQ and KK =!= ZZ/(char KK) ) then (Gr:=Grassmannian(k,n,CoefficientRing=>KK,Variable=>pp); return((ring Gr)/Gr));
   pp=getSymbol toString pp;
   ch:=char KK;
   if (class GG_(k,n,ch,pp) === QuotientRing or class GG_(k,n,ch,pp) === PolynomialRing) then return GG_(k,n,ch,pp); 
   J:=Grassmannian(k,n,CoefficientRing=>KK,Variable=>pp);
   GG_(k,n,ch,pp)=(ring J)/J;
   -- <<"-- created Grassmannian G("<<k<<","<<n<<") over "<<toExternalString(KK)<<" with variable "<<pp<<endl;
   return GG_(k,n,ch,pp)
);

ringGrassmannian = method(TypicalValue => QuotientRing, Options => {UseVariable => "p"});
ringGrassmannian (ZZ,ZZ,Ring) := o -> (k,n,KK) -> intGrassmannian(k,n,KK,o.UseVariable);
ringGrassmannian (ZZ,ZZ) := o -> (k,n) -> ringGrassmannian(k,n,QQ,UseVariable=>o.UseVariable);

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

duality = method(TypicalValue => RingMap); -- p. 94 [Gelfand, Kapranov, Zelevinsky - Discriminants, resultants, and multidimensional determinants, 1994]

sign = (permutation) -> sub(product(subsets(0..#permutation-1,2),I->(permutation_(I_1)-permutation_(I_0))/(I_1-I_0)),ZZ); -- thanks to Nivaldo Medeiros 
tosequence = (L) -> if #L != 1 then toSequence L else L_0;

duality(PolynomialRing) := (R) -> (  -- returns the map R:=G(k,P^n) ---> G(n-k-1,P^n*)
   (k,n,KK,p):=detectGrassmannian R; 
   G:=ambient intGrassmannian(k,n,KK,p);
   G':=ambient intGrassmannian(n-k-1,n,KK,p);  
   L:=for U in subsets(set(0..n),n-k) list sign( (sort toList(set(0..n)-U)) | sort toList U)  *  (p_(tosequence sort toList(set(0..n)-U)))_G;
   return(map(R,G,vars R) * map(G,G',L));
);

duality(QuotientRing) := (R) -> ( -- returns the map R:=G(k,P^n) ---> G(n-k-1,P^n*)
   (k,n,K,p):=detectGrassmannian ambient R;
   Gkn:=intGrassmannian(k,n,K,p);
   if sub(ideal Gkn,vars ambient R) != ideal R then error "unable to detect Grassmannian ring";
   f:=map(Gkn,intGrassmannian(n-k-1,n,K,p),matrix duality ambient Gkn); 
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

ChowForm = method(TypicalValue => RingElement, Options => {UseVariable => null, Duality => null, AffineChartGrass => true, AssumeOrdinary => null, AffineChartProj => true}); 

ChowForm (Ideal,ZZ) := o -> (I,s) -> (
   -- Input: I ideal of X = V(I) subset P^n of dimension d
   -- Output: form defining the hypersurface Z_s(X) subset GG(d-s,P^n*) = GG(n-d-1+s,P^n) 
   if not isPolynomialRing ring I then error("expected ideal in a polynomial ring");
   if not isHomogeneous I then error("expected a homogeneous ideal");
   if s<0 then error("expected a nonnegative integer");
   p:=if o.UseVariable === null then getVariable ring I else getSymbol toString o.UseVariable;
   K:=coefficientRing ring I; 
   n:=numgens ring I -1;
   d:=dim I -1;
   useDuality:=o.Duality;
   if useDuality === null then useDuality = d-s+1 <= n-d+s;
   if class useDuality =!= Boolean then error "expected true or false for option Duality";
   r:=if useDuality then d-s else n-d-1+s; 
   if n-d-1+s >= n or n-d-1+s <=-1 then return 1_(ringGrassmannian(n-d-1+s,n,K,UseVariable=>p));
   mnr:=o.AffineChartGrass;
   if mnr === true then mnr = (random toList(0..n))_{0..r};
   if mnr =!= false then (try assert(ring matrix{mnr} === ZZ and min mnr >=0 and max mnr <=n and #set mnr == r+1) else error("bad value for option AffineChartGrass: expected either boolean value or list of "|toString(r+1)|" distinct integers beetween 0 and "|toString(n)|" but got "|toString(mnr))); 
   if mnr =!= false then mnr=sort mnr; 
   if (class o.AssumeOrdinary =!= Boolean and o.AssumeOrdinary =!= null) then error "expected true or false for option AssumeOrdinary";
   limitNumGens:=if (mnr === false) and (o.AssumeOrdinary === true or (o.AssumeOrdinary === null and s==0)) then 1 else infinity;
   if (o.AssumeOrdinary === true and o.AffineChartGrass =!= false) then (<<"-- warning: option AssumeOrdinary ignored, you should use this with AffineChartGrass=>false"<<endl);      
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
   G:=ringGrassmannian(r,n,K,UseVariable=>p);
   f:=map(Ru/Z,G,gens minors(r+1,Sub' U));
   Zs:=kernel(f,SubringLimit=>limitNumGens);
   if mnr =!= false then Zs=homogenize(Zs,(p_(tosequence mnr))_G); 
   if useDuality then Zs=dualize Zs;
   Zs=trim Zs;
   if Zs == 1 and o.AffineChartGrass =!= false then (
        <<"-- rerun ChowForm() with option AffineChartGrass=>false... \n";
        return ChowForm(I,s,UseVariable=>o.UseVariable,Duality=>o.Duality,AffineChartGrass=>false,AssumeOrdinary=>o.AssumeOrdinary,AffineChartProj=>o.AffineChartProj);
   ); 
   if numgens Zs == 1 then Zs_0 else Zs
);

ChowForm (RingElement,ZZ) := o -> (F,s) -> (
   if not (isPolynomialRing ring F and isHomogeneous F) then error("expected a homogeneous polynomial");
   return ChowForm(ideal F,s,UseVariable=>o.UseVariable,Duality=>o.Duality,AffineChartGrass=>o.AffineChartGrass,AssumeOrdinary=>o.AssumeOrdinary,AffineChartProj=>o.AffineChartProj);
);

ChowForm (Ideal) := o -> (I) -> (
   return ChowForm(I,0,UseVariable=>o.UseVariable,Duality=>o.Duality,AffineChartGrass=>o.AffineChartGrass,AssumeOrdinary=>o.AssumeOrdinary,AffineChartProj=>o.AffineChartProj);
);

ChowForm (RingElement) := o -> (F) -> (
   return ChowForm(F,0,UseVariable=>o.UseVariable,Duality=>o.Duality,AffineChartGrass=>o.AffineChartGrass,AssumeOrdinary=>o.AssumeOrdinary,AffineChartProj=>o.AffineChartProj);
);

SegreProduct = method(Options => {UseVariable => null});
SegreProduct (Ideal,ZZ) := o -> (X,k) -> ( 
   -- Input: X ideal of a subvariety of P^n, k integer
   -- Output: (W,M), W = ideal of X x P^k in PP(Mat(k+1,n+1)), M = generic matrix of coordinates in PP(Mat(k+1,n+1))
   if not isPolynomialRing ring X then error("expected ideal in a polynomial ring");
   if not isHomogeneous X then error("expected a homogeneous ideal");
   if k<0 then error("expected a nonnegative integer");
   x:=if o.UseVariable === null then getVariable ring X else getSymbol toString o.UseVariable;
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

CayleyTrick = method (Options => {UseVariable => null});

CayleyTrick (Ideal) := o -> (I) -> ( 
  -- input: ideal I of a subvariety X subset P^n of dimension k
  -- output: (X x P^k),(X x P^k)^*
  k:=dim I -1;
  n:=numgens ring I -1;
  KK:=coefficientRing ring I;
  (W,M):=SegreProduct(I,k,UseVariable=>o.UseVariable);
  Z:=ideal dualize ChowForm(I,UseVariable=>o.UseVariable);
  f:=map(ring M,ring Z,gens minors(k+1,M));
  (W,f Z)
);

CayleyTrick (Ideal,ZZ) := o -> (I,b) -> ( -- undocumented
  -- input: ideal I of a subvariety X subset P^n of dimension k, b integer s.t. def X <= b <= dim X
  -- output: (X x P^b),(X x P^b)^*
  k:=dim I -1;
  n:=numgens ring I -1;
  KK:=coefficientRing ring I;
  (W,M):=SegreProduct(I,b,UseVariable=>o.UseVariable);
  Z:=ideal dualize ChowForm(I,k-b,UseVariable=>o.UseVariable);
  f:=map(ring M,ring Z,gens minors(b+1,M));
  (W,f Z)
);

ChowEquations = method(TypicalValue => Ideal, Options => {UseVariable => null});

ChowEquations (RingElement) := o -> (W) -> ( 
   (k,n,KK,x):=detectGrassmannian ambient ring W;
   k=n-k-1;   -- W in G(n-k-1,n), k=dim X 
   if o.UseVariable =!= null then x = getSymbol toString o.UseVariable;
   Pn:=intGrassmannian(0,n,KK,x);
   s:=local s; 
   R:=Pn[s_(0,0)..s_(n-k-2,n)];   
   M:=if n-k-1>0 then transpose(genericMatrix(R,n+1,n-k-1))||sub(vars Pn,R) else sub(vars Pn,R);
   mm:=minors(n-k,M);
   pp:=gens ring W;
   P:=sub(W,for i to #pp -1 list pp_i => mm_i);
   trim sub(ideal last coefficients matrix{{P}},Pn)
);

Dual = method(TypicalValue => Ideal, Options => {AssumeOrdinary => false}); 

Dual (Ideal) := o -> (I) -> (
   I':=if o.AssumeOrdinary then ChowForm(I,dim I -1,AffineChartGrass=>false,AssumeOrdinary=>o.AssumeOrdinary) else ChowForm(I,dim I -1,AssumeOrdinary=>o.AssumeOrdinary);
   I'=dualize I';
   if not isIdeal I' then I'=ideal I';
   sub(I',vars ring I)
);

--------------------------------------------------------
--------------------------------------------------------

beginDocumentation() 
document { 
    Key => ChowForms, 
    Headline => "Chow forms of projective varieties",
    "This package provides methods to deal with Chow forms and, more generally, with higher associated subvarieties of irreducible projective varieties; the main method is ", TO "ChowForm",". For the mathematical theory, we refer to the book: Israel M. Gelfand, Mikhail M. Kapranov, Andrei V. Zelevinsky, ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"}, ", Mathematics: Theory & Applications (1994). " 
}

document { 
    Key => {ChowForm,(ChowForm,Ideal,ZZ),(ChowForm,RingElement,ZZ),(ChowForm,Ideal),(ChowForm,RingElement)}, 
    Headline => "Higher Chow forms of a projective variety", 
    Usage => "ChowForm(I,s)  
              ChowForm I",
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///,", say of dimension ", TEX///$k$///},
                "s" => ZZ => {"if not passed, 0 is used"} 
              }, 
Outputs => { {"or an ", TO "Ideal", " (if there is more than one generator) in the coordinate ring of the Grassmannian ",TEX///$\mathbb{G}(n-k-1+s,\mathbb{P}^n)$///," in the Plucker embedding, representing the higher associated subvariety ",TEX///$Z_s(X)$///} 
           }, 
    "For a projective variety ",TEX///$X\subset\mathbb{P}^n$///," of dimension ",TEX///$k$///, ", the ",TEX///$s$///,"-th associated subvariety ",TEX///$Z_s(X)\subset\mathbb{G}(n-k-1+s,\mathbb{P}^n)$///, " is defined to be the closure of the set of ", TEX///$(n-k-1+s)$///, "-dimensional subspaces ",TEX///$L\subset \mathbb{P}^n$///, " such that ", TEX///$L\cap X\neq\emptyset$///, " and ",TEX///$dim(L\cap T_x(X))\geq s$///, " for some smooth point ",TEX///$x\in L\cap X$///, ", where ",TEX///$T_x(X)$///, " denotes the embedded tangent space to ", TEX///$X$///, " at ",TEX///$x$///,". In particular, ", TEX///$Z_0(X)\subset\mathbb{G}(n-k-1,\mathbb{P}^n)$///, " is defined by the Chow form of ", TEX///$X$///, ", while ",TEX///$Z_k(X)\subset\mathbb{G}(n-1,\mathbb{P}^n)$///, " is identified to the dual variety ", TEX///$X^{*}\subset\mathbb{P}^n^{*}=\mathbb{G}(0,\mathbb{P}^n^{*})$///, " via the duality of Grassmannians ", TEX///$\mathbb{G}(0,\mathbb{P}^n^{*})=\mathbb{G}(n-1,\mathbb{P}^n)$///,". For details we refer to the third chapter of Israel M. Gelfand, Mikhail M. Kapranov, Andrei V. Zelevinsky, ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"}, ", Mathematics: Theory & Applications (1994).",
    PARA{
    "The algorithm used are standard, based on projections of suitable incidence varieties. Here are some of the available options that could speed up the computation."
        },    
    PARA{
    TT "Duality"," Taking into account the duality of Grassmannians, one can perform the computation in ", TEX///$\mathbb{G}(k-s,n)$///, " and then passing to ", TEX///$\mathbb{G}(n-k-1+s,n)$///, ". This is done by default when it seems advantageous."
        },   
    PARA{
    TT "AffineChartGrass", " If one of the standard coordinate charts on the Grassmannian is specified, then the internal computation is done on that chart. By default, a random chart is used. Set this to false to not use any chart."
        },   
    PARA{
    TT "AssumeOrdinary", " Set this to ",TT"true"," if you know that ",TEX///$Z_s(X)$///," is a hypersurface (by default is already ",TT"true"," if ",TEX///$s=0$///,")."
        },
    EXAMPLE { 
          "-- cubic rational normal scroll surface in P^4=G(0,4)
use ringGrassmannian(0,4,UseVariable=>p); S = minors(2,matrix{{p_0,p_2,p_3},{p_1,p_3,p_4}})",
          "-- 0-th associated hypersurface of S in G(1,4) (Chow form)
time ChowForm S",
          "-- 1-th associated hypersurface of S in G(2,4)
time ChowForm(S,1)",
          "-- 2-th associated hypersurface of S in G(3,4) (parameterizing tangent hyperplanes to S)
time ChowForm(S,2)",
          "-- we get the dual hypersurface of S in G(0,4) by dualizing
time S' = dualize ChowForm(S,2)",
          "-- we then can recover S
time dualize ChowForm(S',3) == S"
            }
}

document { 
    Key => {ChowEquations,(ChowEquations,RingElement)}, 
    Headline => "Chow equations of a projective variety", 
    Usage => "ChowEquations W", 
    Inputs => { "W" => RingElement => {"the Chow form of an irreducible projective variety ",TEX///$X\subset\mathbb{P}^n$///,}
              }, 
Outputs => { {"generated by the Chow equations of ",TEX///$X$///} 
           },
    "Given the Chow form ",TEX///$Z_0(X)\subset\mathbb{G}(n-k-1,n)$///," (constructed for instance using ",TO "ChowForm",") of an irreducible projective ",TEX///$k$///,"-dimensional variety ", TEX///$X\subset\mathbb{P}^n$///, ", one can recover a canonical system of equations, called Chow equations, that always define ",TEX///$X$///, " set-theoretically, and moreover also scheme-theoretically whenever ",TEX///$X$///, " is smooth. For details, see chapter 3, section 2-C of Israel M. Gelfand, Mikhail M. Kapranov, Andrei V. Zelevinsky, ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"}, ", Mathematics: Theory & Applications (1994).",
    PARA{},
    EXAMPLE { 
          "P3 = ringGrassmannian(0,3,ZZ/11,UseVariable=>x)",
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
"Let ",TEX///$X\subset\mathbb{P}^n$///," be a ",TEX///$k$///,"-dimensional projective variety. Consider the product ", TEX///$W = X\times\mathbb{P}^k$///," as a subvariety of ",TEX///$\mathbb{P}(Mat(k+1,n+1))$///,", the projectivization of the space of ",TEX///$(k+1)\times (n+1)$///, "-matrices, and consider the projection ",TEX///$p:\mathbb{P}(Mat(k+1,n+1))--->\mathbb{G}(k,n)=\mathbb{G}(n-k-1,n)$///,". Then the \"Cayley trick\" states that the dual variety ",TEX///$W^*$///," of ", TEX///$W$///," equals the closure of ",TEX///$p^{-1}(Z_0(X))$///, ", where ", TEX///$Z_0(X)\subset\mathbb{G}(n-k-1,n)$///," is the Chow hypersurface of ",TEX///$X$///,". ", TEX///$W^*$///," is also called ", TEX///$X$///,"-resultant. For details and proof, see: J. Weyman, A. Zelevinsky, ",HREF{"http://link.springer.com/article/10.1007/BF02567693","Multiplicative properties of projectively dual varieties"}, ", Manuscripta Math. 82 (1994), no. 2, 139--148.",
    PARA{},
    EXAMPLE { 
          "-- X-resultant of the quadric surface P1xP1
QQ[t,x,y,z]; P1xP1 = ideal(x*y-t*z)",
          "time (P1xP1xP2,P1xP1xP2') = CayleyTrick(P1xP1,UseVariable=>c)",
            },
    PARA{},
    "Actually, using other versions of the Cayley trick, we can easily obtain the dual variety of the Segre product ", TEX///$X\times\mathbb{P}^s$///," for other values of ",TEX///$s$///,". For instance, the ideal of ",TEX///$\mathbb{P}^1\times\mathbb{P}^1\times\mathbb{P}^1\subset\mathbb{P}^7$///," and that of its dual can be computed as follows:",
    PARA{},
    EXAMPLE { 
          "time (P1xP1xP1,P1xP1xP1') = CayleyTrick(P1xP1,1,UseVariable=>c)",
            }
}

document { 
    Key => {dualize,(dualize,RingElement),(dualize,Matrix),(dualize,Ideal)}, 
    Headline => "apply duality of Grassmannians", 
    Usage => "dualize f", 
    Inputs => { "f" => RingElement => {" or a ",TO "Matrix", ", or an ",TO "Ideal",", in the coordinate ring (resp. ambient ring) of a Grassmannian ",TEX///$\mathbb{G}(k,n)$///}
              }, 
    Outputs => { {"the image of ", TT "f", " in the coordinate ring (resp. ambient ring) of ", TEX///$\mathbb{G}(n-k-1,n)$///," via the duality of Grassmannians"}
               },
    "This method implements the natural identification ", TEX///$\mathbb{G}(k,\mathbb{P}^n)\to\mathbb{G}(n-k-1,{\mathbb{P}^n}^{*})$///,", which takes a subspace ", TEX///$L\in\mathbb{G}(k,\mathbb{P}^n)$///, " to its orthogonal complement ",TEX///$L^*\in\mathbb{G}(n-k-1,{\mathbb{P}^n}^*)$///,".",
    PARA{},
    EXAMPLE { 
          "P9 = ambient ringGrassmannian(2,4,ZZ/13,UseVariable=>x)",
          "vars P9",
          "dualize vars P9",
          "F  = random(2,P9)",
          "dualize F",
          "F == dualize dualize F",
            },
}

document { 
    Key => {ringGrassmannian,(ringGrassmannian,ZZ,ZZ),(ringGrassmannian,ZZ,ZZ,Ring)}, 
    Headline => "coordinate ring of a Grassmannian", 
    Usage => "ringGrassmannian(k,n)
              ringGrassmannian(k,n,K)",
    Inputs => {"k" => ZZ ,
               "n" => ZZ ,
               "K" => Ring => {"optional with default value ",TT "QQ",", the coefficient ring for the polynomial ring to be made"}
              }, 
    Outputs => { {"the coordinate ring of the Grassmannian variety of all projective ",TEX///$k$///,"-planes in ",TEX///$\mathbb{P}^n$///}
               },
    "This method calls the method ", TO "Grassmannian", ", and ", TT "ringGrassmannian(k,n,K,UseVarible=>p)", " can be considered equivalent to ", TT "last(G=Grassmannian(k,n,Variable=>p,CoefficientRing=>K),(ring G)/G)", ". However, over ", TT "QQ", " and ", TT "ZZ/p", ", the method ",TT "ringGrassmannian", " creates only an instance of ring for any given ", TT "(k,n,K,p)",".", 
    PARA{},
    EXAMPLE { 
          "R_0 = ringGrassmannian(1,3)",
          "R_1 = ringGrassmannian(2,4,ZZ/11,UseVariable=>t)",
          "S_0 = ringGrassmannian(1,3)",
          "S_1 = ringGrassmannian(2,4,ZZ/11,UseVariable=>t)",
          "R_0 === S_0 and R_1 === S_1"
            },
     PARA{},
    "In order to facilitate comparisons, the outputs of the methods ",TO "ChowForm",", ",TO "ChowEquations",", and ",TO "dualize", " always lie in these rings."
}

document {
    Key => {UseVariable, [ChowForm,UseVariable],[ringGrassmannian,UseVariable],[ChowEquations,UseVariable],[CayleyTrick,UseVariable]},
    Headline => "specify a name for a variable", 
    TT "UseVariable => x"," -- an option used to specify a symbol to be used as a name for the generator of the coordinate ring of the Grassmannian."
} 

document {
    Key => {AssumeOrdinary, [ChowForm,AssumeOrdinary], [Dual,AssumeOrdinary]},
    Headline => "whether the expected codimension is 1", 
    TT "AssumeOrdinary => true/false"," -- an option for ", TO "ChowForm"," and ",TO "Dual"," that controls the option ", TO "SubringLimit",", used internally with ", TO "kernel", " and ", TO "gb",". You can set this to ",TT"true"," when the expected output should represent a hypersurface."
} 

document {
    Key => {Duality, [ChowForm,Duality]},
    Headline => "whether to use dual Plucker coordinates", 
    TT "Duality => true/false"," -- an option for ",TO "ChowForm",", to specify whether to perform internal computations using the dual Plucker coordinates."
} 

document {
    Key => {AffineChartGrass, [ChowForm,AffineChartGrass]},
    Headline => "use an affine chart on the Grassmannian", 
    TT "AffineChartGrass => l", " -- here ",TT "l"," is a list of ",TEX///$k+1$///," distinct integers between 0 and ",TEX///$n$///,", corresponding to an affine chart ",TEX///$U$///," on the Grassmannian ",TEX///$\mathbb{G}(k,n)$///,". This indicates to ",TO "ChowForm"," that should perform internal computations using the chart ",TEX///$U$///,". Set this to ", TT "false"," (resp. ",TT "true",") to not use any chart (resp. to use a random chart)."
} 

document {
    Key => {AffineChartProj, [ChowForm,AffineChartProj]},
    Headline => "use an affine chart on the projective space", 
    TT "AffineChartProj => l", " -- here ",TT "l"," is an integer between 0 and ",TEX///$n$///,", corresponding to an affine chart ",TEX///$U$///," on the projective space ",TEX///$\mathbb{P}^n$///,". This option, quite similar to ",TO "AffineChartGrass", ", indicates to perform internal computations using the chart ",TEX///$U$///,"."
} 

document { 
    Key => {Dual,(Dual,Ideal)}, 
    Headline => "projective dual variety", 
    Usage => "Dual I", 
    Inputs => { "I" => Ideal => {"a homogeneous ideal defining a projective variety ",TEX///$X=V(I)\subset\mathbb{P}^n$///},
              }, 
    Outputs => { {"the ideal of the projective dual variety ",TEX///$X^{*}\subset{\mathbb{P}^n}^{*}$///}
                },
    "This is basically a shortcut for ",TT "dualize ChowForm(I,dim I -1)",".", 
    PARA{},
    EXAMPLE { 
          "-- Veronese surface in P^5
V = trim minors(2,genericSymmetricMatrix(QQ[x_0..x_5],3))",
          "time Dual V",
          "-- reflexivity theorem
time V == Dual Dual V"
            }
}


--------------------------------------------------------
--------------------------------------------------------

TEST ///
-- testing dualize
assert (dualize ideal ringGrassmannian(1,4) == ideal ringGrassmannian(2,4))
assert (dualize ideal ringGrassmannian(0,4,ZZ/331) == ideal ringGrassmannian(3,4,ZZ/331))
assert (dualize ideal ringGrassmannian(3,5,UseVariable=>T) == ideal ringGrassmannian(1,5,UseVariable=>T))
assert (dualize Grassmannian(2,6,CoefficientRing=>(ZZ/11),Variable=>r) == ideal ringGrassmannian(3,6,ZZ/11,UseVariable=>r))
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
P3=ringGrassmannian(0,3,UseVariable=>y)
C=minors(2,matrix{{y_0..y_2},{y_1..y_3}})
assert(C == saturate ChowEquations ChowForm C)
///

TEST /// 
-- testing ChowForm and Dual
checkOptions = (I,s) -> (
   test:=true;
   time Z:=ChowForm(I,s);
   time test =          Z == ChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>false,Duality=>false);
   time test = test and Z == ChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>false,Duality=>true);
   time test = test and Z == ChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>true, Duality=>false);
   time test = test and Z == ChowForm(I,s,AffineChartGrass=>false,AffineChartProj=>true, Duality=>true);
   time test = test and Z == ChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>false,Duality=>false);
   time test = test and Z == ChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>false,Duality=>true);
   time test = test and Z == ChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>true, Duality=>false);
   time test = test and Z == ChowForm(I,s,AffineChartGrass=>true, AffineChartProj=>true, Duality=>true);
   test
);
-- rational normal scroll surface S(1,2) subset P^4
P4 = ringGrassmannian(0,4,ZZ/3331,UseVariable=>x)
G14= ringGrassmannian(1,4,ZZ/3331,UseVariable=>x)
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
use ringGrassmannian(0,5,ZZ/3331,UseVariable=>p)
Q = sum for i to 5 list p_i^2
assert isQuadricFermat ChowForm(Q,0)
assert isQuadricFermat ChowForm(Q,1)
assert isQuadricFermat ChowForm(Q,2)
assert isQuadricFermat ChowForm(Q,3)
assert isQuadricFermat ChowForm(Q,4)
assert checkOptions(X,3)
assert checkOptions(X,4)
///

TEST ///
testGCT = (X) -> (
   -- checking: dualize Z_(dimX-s)(X) == Z_(s-defX)(X*)
   X':=Dual X; dimX:=dim X -1; defX:=codim X' -1; Z:=local Z; cond:=true;
   <<"dim X = "<<dimX<<", def X = "<<defX<<endl;
   for s from defX to dimX do (
      Z = (dualize ChowForm(X,dimX-s),ChowForm(X',s-defX));
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

