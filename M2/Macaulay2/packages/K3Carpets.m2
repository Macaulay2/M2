///
restart
uninstallPackage"K3Carpets"
restart
installPackage"K3Carpets"
loadPackage("K3Carpets", Reload => true)
check "K3Carpets"
viewHelp K3Carpets
///
newPackage(
	"K3Carpets",
    	Version => "0.5", 
    	Date => "March 24, 2018",
    	Authors => {{Name => "David Eisenbud", 
		  Email => "de@msri.org", 
		  HomePage => "http://www.msri.org/~de"},
	          {Name => "Frank-Olaf Schreyer", 
		  Email => "schreyer@math.uni-sb.de", 
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer"}},
    	Headline => "K3 double structure on scrolls",
	Keywords => {"Commutative Algebra"},
	PackageExports => {"CompleteIntersectionResolutions", "NonminimalComplexes"},
	PackageImports => {"Elimination"}
	)

export {
    "schreyerName",
    -- should be moved to an other package, e.g. NonminimalComplexes
    "analyzeStrand",
    "carpetDet",
    "resonanceDet",
    "carpet",
    "gorensteinDouble",
    "canonicalCarpet",
    "homotopyRanks",
    "canonicalHomotopies",
    "carpetBettiTables",
    "carpetBettiTable",
--  "smithNormalFormValues", -- no export needed
    "hankelMatrix",
    "correspondenceScroll",
    "smallDiagonal",
    "productOfProjectiveSpaces",
    "irrelevantIdeal",
    "schemeInProduct",
    "degenerateK3",
    "degenerateK3BettiTables",
    "allGradings",
    "relativeEquations",
    "coxMatrices",
    "relativeResolution",
    "relativeResolutionTwists",
    "resonanceScroll",
    "computeBound",
--symbols
    "FineGrading",
   
    "Scrolls"    
    }

--needsPackage "NonminimalComplexes"


///
I = carpet(3,1, FineGrading =>true)
isHomogeneous I
betti res I
///
carpet = method(Options =>{Characteristic => 32003,FineGrading=>false,Scrolls =>false})

carpet(ZZ,ZZ,Matrix) := opts -> (a1,a2,m) ->(
    --matrix m should be 2 x (a1+a2) of LINEAR forms
    --both a1,a2>=1
    --we'll always divide m into two parts, with the smaller part of size 2 x a coming "first"
    --check for bad entries:
    if a1+a2>numcols m then error"sum of the integers must be <= numcols of matrix";
    a := max(a1,a2);b := min(a1,a2);
    if a<1 then error("both integers should be >=1");
    
    xmat := m_{0..a-1};
    ymat := m_{a..a+b-1};
    if a == 1 then return ideal ((det m)^2);
    if b==1 then(  -- replace m by a new matrix, where, in effect, b1 = 2
    	ymat = map(target ymat,,matrix{{m_(0,a)^2,m_(0,a)*m_(1,a)},{m_(0,a)*m_(1,a),m_(1,a)^2}});
	b = 2);
    Ix := minors(2,xmat);
    Iy := minors(2,ymat);
    Imixed := ideal flatten apply(b-1, 
	    j-> apply(a-1,
		i->(det (xmat_{i}|ymat_{j+1})-det(xmat_{i+1}|ymat_{j}))
		));
    (-1)*Ix+Imixed+(-1)*Iy)


carpet(ZZ,ZZ) := opts -> (a1,a2) ->(
    --this version makes an appropriate ring and matrix, and then calls carpet(a,b,matrix)
    if opts.Characteristic == 0 then kk := QQ else kk = ZZ/opts.Characteristic;
    x := symbol x; y:=symbol y;
    a := max(a1,a2);
    b := min(a1,a2);
    if opts.FineGrading == false then
    S := kk[x_0..x_a, y_0..y_b] else (
    degreeString := apply(a+1, i->{1,0,i,a-i})|apply(b+1, i->{0,1,i,b-i});
    S = kk[x_0..x_a, y_0..y_b, Degrees=>degreeString]);
    if opts.FineGrading == false then (
	xmat := map(S^2, S^{a:-1}, (i,j) -> x_(i+j));
        ymat := map(S^2, S^{b:-1}, (i,j) -> y_(i+j))
	)
    else(	
        xmat = map(S^{{0,0,0,0},{0,0,1,-1}}, S^(apply(a,j->{ -1,0,-j,-a+j})), (i,j) -> x_(i+j));
        ymat = map(S^{{0,0,0,0},{0,0,1,-1}}, S^(apply(b,j->{ 0,-1,-j,-b+j})), (i,j) -> y_(i+j))
	);

    if a==1 then return ideal ((det(xmat|ymat))^2) -- in this case b == 1 as well
    else if b ==1 then (
	if opts.FineGrading == false then
    	ymat = map(S^2,S^{2:-2},(i,j)->y_i*y_j)
        else
        ymat = map(S^{{0,0,0,0},{0,0,1,-1}}, 
	           S^{{ 0,-1,0,-2},{ 0,-1,-1,-1}}, 
		   matrix{{y_0^2,y_0*y_1},{y_0*y_1,y_1^2}});
    	b = 2);

    	mat := xmat|ymat;
	if opts.Scrolls == false then return carpet(a,b,mat) else (carpet(a,b,mat),xmat,ymat)
	)        

///
restart
loadPackage "K3Carpets"
S = ZZ/101[x_0..x_9]
--mat = genericMatrix(S,x_0,2,5)
mat = hankelMatrix(S,x_2,2,3)|hankelMatrix(S,2,1)
betti res carpet(3,1,mat)
betti res carpet(1,3)
netList flatten apply(3, a-> apply(4,i->(betti(res carpet(a+1,a+1+i,Characteristic => 2,FineGrading=>true), Weights =>{1,1,0,0}))))
carpet(1,1)
betti(res canonicalCarpet(7,3,Characteristic => 2,FineGrading=>true), Weights =>{1,1,0,0})
betti res correspondenceScroll((smallDiagonal pp 2)^2,{3,1})
time carpet(5,5)
time 

viewHelp correspondenceScroll
--
restart
loadPackage "K3Carpets"
I = carpet(1,5,FineGrading=>true)
I = carpet(1,3,FineGrading=>false)
betti (res I, Weights =>{1,1,0,0})
isHomogeneous I
degrees  S
isHomogeneous mat
degrees target mat
degrees source mat
mat
--
///



analyzeStrand = method()
analyzeStrand(ChainComplex,ZZ) := (F,a) -> (
    mainStrand := constantStrand(F,a+1);
    M1 := mainStrand.dd_(a-1);
    M1Z := lift(M1,ZZ);
    sM1Z := syz M1Z;
    M2 := mainStrand.dd_a;
    M2Z := lift(M2,ZZ);
    assert(M1Z*M2Z == 0);
    M1s := smithNormalForm(M1Z,ChangeMatrix=>{false,false});
    L := apply(rank source M1s,i->M1s_(i,i));
    assert(diagonalMatrix L == M1s);
    assert(product L == 1);
    cM1 := chainComplex sM1Z;
    cM2 := chainComplex M2Z;
    compare := extend(cM1,cM2,id_cM1_0);
    M:= compare#1;
    elapsedTime Ms := smithNormalForm(M,ChangeMatrix=>{false,false});
    L = apply(rank source Ms,i->Ms_(i,i));
    assert(diagonalMatrix L == Ms);
    L1:= select(L,d-> d!=1);
    L)


carpetBettiTable=method()
carpetBettiTable(ZZ,ZZ,ZZ) := (a,b,p) -> (
    h:= carpetBettiTables(a,b);
    primes := keys h;
    if member(p,primes) then h#p else h#0)

carpetBettiTable(HashTable,ZZ) := (h,p) -> (
    primes := keys h;
    if member(p,primes) then h#p else h#0)
    

carpetBettiTables=method()
carpetBettiTables(ZZ,ZZ) := (a,b) -> (
    I := carpet(a,b);
    F := res(I,FastNonminimal=>true);
   carpetData1 := {};carpetData2 := {};
    L:= null;primes:= {};
    for c from 3 to length F-2 do (    
	L = analyzeStrand(F,c);
	carpetData2 = append(carpetData2,(c=>#L));
	if (product L != 1 ) then (
	    d := factor product L;
            primes = apply(#d,i->d#i#0)) else primes={};      
        primes = apply(primes,p->(p,#select(L,t->t%p==0)));
       	carpetData1 = append(carpetData1,(c=>{d,primes}));
	); 
    cD1 := hashTable carpetData1;
    cD2 := hashTable carpetData2;
    T:= betti F;
    T1 := new BettiTally from {(0,{0},0)=>1}|apply(
	    toList(1..length F),i->( (i,{i+1},i+1) => if i>=length F-1 then 0 else (
		    (T#(i,{i+1},i+1)- if i>2 and i<length F-1 then cD2#i else 0 ))));
    T2 := T1+((dual T1)[-length F])(-length F-3);
    exceptionalPrimes := unique flatten apply(values cD1,dc->apply(dc_1,j->j_0));
    Ts := new HashTable from apply(exceptionalPrimes,p-> p=> new BettiTally from (
	    apply(keys cD2,c->( cor:=select(cD1#c_1,j->j_0==p); 
		    if #cor>0 then (c,{c+1},c+1) => cor_0_1 ))
	    ));    
    new HashTable from {0 => T2}|apply(exceptionalPrimes,p-> (
	     p => (T1p:=T1+Ts#p;T1p+((dual T1p)[-length F])(-length F-3))))
     )
 
degenerateK3BettiTables=method()
degenerateK3BettiTables(ZZ,ZZ,Sequence) := (a,b,e) -> (
    I := degenerateK3(a,b,e);
    F := res(I,FastNonminimal=>true);
    carpetData1 := {};carpetData2 := {};
    L:= null;primes:= {};
    for c from 3 to length F-2 do (    
	L = analyzeStrand(F,c);
	carpetData2 = append(carpetData2,(c=>#L));
	if (product L != 1 ) then (
	    d := factor product L;
            primes = apply(#d,i->d#i#0)) else primes={};      
        primes = apply(primes,p->(p,#select(L,t->t%p==0)));
       	carpetData1 = append(carpetData1,(c=>{d,primes}));
	); 
    cD1 := hashTable carpetData1;
    cD2 := hashTable carpetData2;
    T:= betti F;
    T1 := new BettiTally from {(0,{0},0)=>1}|apply(
	    toList(1..length F),i->( (i,{i+1},i+1) => if i>=length F-1 then 0 else (
		    (T#(i,{i+1},i+1)- if i>2 and i<length F-1 then cD2#i else 0 ))));
    T2 := T1+((dual T1)[-length F])(-length F-3);
    exceptionalPrimes := unique flatten apply(values cD1,dc->apply(dc_1,j->j_0));
    Ts := new HashTable from apply(exceptionalPrimes,p-> p=> new BettiTally from (
	    apply(keys cD2,c->( cor:=select(cD1#c_1,j->j_0==p); 
		    if #cor>0 then (c,{c+1},c+1) => cor_0_1 ))
	    ));    
    new HashTable from {0 => T2}|apply(exceptionalPrimes,p-> (
	     p => (T1p:=T1+Ts#p;T1p+((dual T1p)[-length F])(-length F-3))))
     )
  

schreyerName = method()
schreyerName(ChainComplex,ZZ,ZZ) := (F,i,n) -> (
    -- recursive definition, might be better to produce all names in the complex by
    -- with a for loop
    lT := leadTerm F.dd_i_n;
    elT := entries lT;
    m := position(elT ,c->not c==0);
    mon := (entries lT)_m;
    if i==1 then return {mon} else (
	L:=schreyerName(F,i-1,m);
	return append(L,mon));
    )

schreyerName(ChainComplex) := F -> (
    r := rank F_1;
    L := new MutableHashTable from apply(length F,i->(i+1)=>{});
    L#1 = for n from 0 to r-1 list (
	 schreyerName(F,1,n));
    for i from 2 to length F do (
	r = rank F_i;
	L#i = for n from 0 to r-1 list (
	    lT := leadTerm F.dd_i_n;
            elT := entries lT;
            m := position(elT ,c->not c==0);
            mon := (entries lT)_m;
	    append((L#(i-1))_m,mon));
	);
    new HashTable from L
    )

schreyerName(ChainComplex,ZZ) :=(F,a) -> (
     r := rank F_1;
    L := new MutableHashTable from apply(length F,i->(i+1)=>{});
    L#1 = for n from 0 to r-1 list (
	 schreyerName(F,1,n));
    for i from 2 to a do (
	r = rank F_i;
	L#i = for n from 0 to r-1 list (
	    lT := leadTerm F.dd_i_n;
            elT := entries lT;
            m := position(elT ,c->not c==0);
            mon := (entries lT)_m;
	    append((L#(i-1))_m,mon));
	);
    new HashTable from L
    )

schreyerName(HashTable,ZZ,ZZ) := (L,i,n) -> (L#i_n)

	    
	



smithNormalFormValues = method()
smithNormalFormValues(Matrix) := M-> (
    Ms := smithNormalForm(M,ChangeMatrix=>{false,false});
    sort apply(rank source Ms,i-> Ms_(i,i))
    )


--A different indexing, by genus and Clifford index (Cliff <= (g-1)//2))
canonicalCarpet = method(Options=>{Characteristic=>32003,FineGrading => false,Scrolls=>false})
canonicalCarpet(ZZ,ZZ) := opts -> (gen,cliff) -> 
     carpet(gen-cliff-1, cliff,Characteristic => opts.Characteristic, FineGrading => opts.FineGrading,Scrolls=>opts.Scrolls)

--Here's a structural approach that instead takes the kernel of the unique map of mainimal degree
--from the ideal of the scroll to the canonical module of the scroll. This code produces
--Gorenstein double structures on ACM varieties more generally. 
--computationally, the bare hands approach of carpet is much faster.

gorensteinDouble = method()
gorensteinDouble Ideal := I -> (
    --the script assumes that the "first" map I --> omega will be a surjection of the right degree
    c := codim I;
    F := res(I, LengthLimit => c);
    omega := coker transpose F.dd_c;
    ideal kernel (homomorphism (Hom(module I, omega))_{0})
    )



canonicalHomotopies = method(Options=>{Characteristic=>32003,FineGrading=>false})
--note: returns the pair: the resolution F of the canonical Carpet
--and the function that used to be called h0 such that h0(i,j) is the j-th homotopy 
--with source F_j that corresponds
--to the i-th quadric.
canonicalHomotopies(ZZ,ZZ):= opts -> (g,cliff) -> (
    F := res canonicalCarpet(g,cliff, 
	Characteristic => opts.Characteristic, 
	FineGrading => opts.FineGrading);
    mins := apply(1+length F, i-> min degrees F_i);
    ff := F.dd_1;
    H := makeHomotopies1(ff,F);
    if opts.FineGrading == false then
    h0:= (i,j) -> submatrixByDegrees(H#{i,j},mins_(j+1),mins_(j+1))
    else
    h0 = (i,j) ->(
	dlist := select(flatten degrees H#{i,j}, de->de_0+de_1 == j+3);
	hashTable apply(dlist, de -> (de,submatrixByDegrees(H#{i,j},de, de)))
	);
    (F,h0)
    )
///
restart
loadPackage "K3Carpets"
homotopyRanks(7,3)
homotopyRanks(7,3, Characteristic =>2)
(F,h0) = canonicalHomotopies(7,3);
h0(0,2)
///


homotopyRanks = method(Options=>{Characteristic=>32003})
homotopyRanks (ZZ,ZZ) := opts-> (g,cliff) ->(
(F,h0) := canonicalHomotopies(g,cliff,
    Characteristic =>opts.Characteristic);
print betti F;
ff := F.dd_1;
netList apply(numcols ff , i->{ff_i, apply(g-2, m->(rank h0(i,m)))})
)

hankelMatrix = method()
hankelMatrix(Matrix,ZZ,ZZ) := (rowmat,nrows,ncols) ->(
   --makes a Hankel matrix using the first ronum + colnum -1 entries of 
   --the (first) row of rowmat,
   --and having rownum rows, colnum columns, padding with zeros as necessary
   --result is homogeneous as long as the entries of the first row of rowmat all have the same
   --degree.
   if not instance(nrows,ZZ) or not instance(ncols,ZZ) or nrows < 0 or ncols < 0
   then error "expected nonnegative integers";
   S := ring rowmat;
   degs := {ncols:-((degree rowmat_0)_0)};
   map(S^nrows,S^degs,(i,j) ->if i+j<numcols rowmat then rowmat_(0,j+i) else 0_S)
   )

hankelMatrix(Ring,ZZ,ZZ) := (R,nrows,ncols) -> hankelMatrix(vars R, nrows, ncols)
hankelMatrix(Ring,RingElement, ZZ,ZZ) := (R,y,nrows,ncols) -> (
     j := position(gens R, i->i==y);
     v := (vars R)_{j..numgens R -1};
     hankelMatrix(v, nrows, ncols)
     )

hankelMatrix(ZZ,ZZ) := (nrows,ncols) -> (
    	  X := symbol X;
	  S := ZZ/32003[X_0..X_(nrows+ncols-2)];
          hankelMatrix(vars S,nrows,ncols)
	  )

resonanceDet=method()

resonanceDet(ZZ) := (a) -> (
    --a=4,b=4
      b := a;
      kk:=ZZ/32003;
      e := symbol e;
      x := symbol x;
      y := symbol y;
      EZ := ZZ[e_1,e_2,Degrees=>{1,2}];      
      S := kk[x_0..x_a,y_0..y_b,e_1,e_2,Degrees=>{a+1:1,1..b+1,1,2}];
      A := matrix apply(2,i->apply(a,j->x_(i+j)));
      B := matrix apply(2,i->apply(b,j->y_(i+j)));
      D := (diagonalMatrix({1,-e_1,e_2})_{2,1,0})^{2,1,0};
      A1 := matrix apply(3,i->apply(a-1,j->x_(i+j)));
      B1 := matrix apply(3,i->apply(b-1,j->y_(i+j)));
      J := minors(2,A)+minors(2,B)+ideal (transpose A1*D*B1);
      --isHomogeneous J;
      elapsedTime betti (fJ := res(J,LengthLimit=>a));                                             
      degs := apply(a+1,i->{1,1,0,i})|apply(b+1,j->{j+1,0,1,j})|{{1,0,0,0},{2,0,0,0}};
      Sall := (coefficientRing S)[x_0..x_a,y_0..y_b,e_1,e_2,Degrees=>degs];
      Fall := allGradings(fJ,Sall);
      --verification that the complex is valid over ZZ[e_1,e_2] should be added
      M := Fall.dd_a; 
      degsSourceM := degrees source M;      
      cols := select(rank source M,i->(d:=degsSourceM_i;d_1+d_2==a+1));
      degsTargetM := degrees target M;      
      rows := select(rank target M,i->(d:=degsTargetM_i;d_1+d_2==a+1));
      M=M^rows_cols;
      degsSourceM = degrees source M;      
      degsTargetM = degrees target M;  
      b1 := unique apply(degrees target M,d->{d_1,d_2,d_3});
      blocks := apply(b1,d-> (
	      --d=first b1
	      cols=select(rank source M,i->{degsSourceM_i_1,degsSourceM_i_2,degsSourceM_i_3}==d);
	      rows=select(rank target M,i->{degsTargetM_i_1,degsTargetM_i_2,degsTargetM_i_3}==d);
	      M^rows_cols));
      blocks=apply(blocks,B->(
	      --B=first blocks
	      degsTargetE := apply(degrees target B,d->-d_0);      
     	      degsSourceE := apply(degrees source B,d->-d_0);
              map(EZ^degsTargetE,EZ^degsSourceE, sub(B,EZ))
	      )); 
      print ("number of blocks= ", #blocks );
      print ("size of the matrices",tally apply(blocks,B->rank source B));
      dets := apply(blocks,B->(
	      print betti B;
	      elapsedTime detB := det B;
	      print factor detB;
	      detB));
      det1:=factor product dets;
      (factor abs sub(first last det1,ZZ),drop(det1,-1)))      
///
a=4,b=4      
resonanceDet(a,b)
///
 
correspondenceScroll = method()
correspondenceScroll (Ideal, List) := (I,scroll) ->(
    --I should be an ideal over a polynomial ring
    --S = kk[x_(i,j)], where j = 0,1, and i = 1..n, homogeneous in each pair
    --x_(i,0),x_(i,1) (say of degree e_i), and scroll should be 
    --a list of positive integers a_i.
    --The script interprets scroll as the degrees of rational normal curves,
    --and forms the ideal of the union of the planes spanned by the n-tuples of
    --points of P^1 defined by I.
    --The ordinary scroll should be the case when I is the (small) diagonal,
    --defined by the {n\choose 2} equations {x_(i,0)x_(i',1)-x_(i,1)x_(i',0).}
    
    --Currently, the script uses elimination to construct the ideal of the correspondence.
    --Once could speed it up by an option that would construct the ideals directly,
    --in the case of a product of projective spaces done here, by
    --multiplying the given equations to make them multi-homogeneous of degree
    --d_i in the i-th set of variables, where d_i is the smallest multiple of scroll_i
    --such that d_i \geq e_i.
S := ring I;
uS := unique (degrees vars S)_1;
SvarIdeals := apply(#scroll, i-> ideal basis(uS_i, S));
dims := apply(SvarIdeals, v -> numgens v -1);
kk := coefficientRing S;
--print dims;
--
y := symbol y;
--var := apply(#scroll, i -> toList(y_(i,0)..y_(i,scroll_i)));
var := apply(#scroll, i -> toList(y_(i,0)..y_(i,binomial(dims_i+scroll_i, scroll_i)-1)));
--print var;
T := kk[flatten var];
varRings := apply(var, v -> kk[v]);
TvarIdeals := apply(varRings, vr -> ideal sub(vars vr, T));
tar := {};
scan(#scroll, i->tar = tar|((SvarIdeals_i)^(scroll_i))_*);
return ker map(S/I,T,tar)    
)


///
restart
loadPackage("K3Carpets",Reload=>true)
scroll = {2,2}
S = productOfProjectiveSpaces{2,2}
vars S
I = ideal random(S^1, S^{{-5,-5}})
m = random(S^5, S^{5:{-1,-1}})
mm = m-transpose m
I = pfaffians(4,mm);
correspondenceScroll(I,scroll);
minimalBetti oo
///

-*
---possible rewrite for speed, currently imcomplete.
Scrolls := apply(#scroll, i->hankelMatrix(T, y_(i,0), 2, scroll_i));
J0 := sum(#scroll, i->minors(2, Scrolls_i));
--
G := I_*;
Ge := flatten apply(G, f -> degree f);
--multiply up to make f have degree the smallest possible multiple of a_i in the i-th set of vars.
Gd := apply(#Ge, i-> (ceiling (Ge_i/scroll_i)*scroll_i));
--the following is the list of the bihomogeneous equations of correct degree in the S variables.
L := apply(#G, i-> product(#scroll, j->(SvarIdeals_j)^(Gd_j-Ge_j))*G_i)
--now need to translate these in terms of the T variables (and add J0)
)
*-




productOfProjectiveSpaces = method(Options=>{Characteristic=>32003})
productOfProjectiveSpaces(List, Symbol, Ring) := opt -> (L,x,kkk) -> (
    degs := flatten apply(#L, i -> toList (1+L_i:apply(#L, j-> if j==i then 1 else 0)));
    varlist := flatten apply(#L, i->apply(1+L_i, j->x_(j,i)));
    kkk[varlist, Degrees =>degs])

productOfProjectiveSpaces(List, Symbol) := opt -> (L,x) -> (
    kkk := ZZ/opt.Characteristic; productOfProjectiveSpaces(L,x,kkk))
productOfProjectiveSpaces(ZZ, Symbol) := opt -> (n,x) -> productOfProjectiveSpaces(toList(n:1),x,Characteristic=>opt.Characteristic)
productOfProjectiveSpaces List := opt -> L -> (x := symbol x; productOfProjectiveSpaces(L, x, Characteristic=>opt.Characteristic))
productOfProjectiveSpaces ZZ := opt -> n -> (x := symbol x; productOfProjectiveSpaces(toList(n:1),Characteristic=>opt.Characteristic))

///
L={2,3}
S=productOfProjectiveSpaces L
vars S
n=2
S=productOfProjectiveSpaces n
vars S
///

pp = productOfProjectiveSpaces


schemeInProduct = method()
schemeInProduct(Ideal, List, Ring) := (I,Maps,PP) ->(
    --I defines a projective scheme in a polynomial ring S
    --Maps is a sequence of 1 x (1+m_i) matrices each consisting of forms of the 
    --same degree inside S/I, interpreted as rational maps from proj(S/I) to 
    --P^(m_i). PP is a multigraded ring corresponding to the product of the P^(m_i),
    --defined over the same field as S.
    --The routine forms the corresponding product of projective spaces, and
    --returns the multi-homogeneous ideal of the image.
    --
    --We should also have a version
    --inputting the product of projective spaces.
    S := ring I;
    dims := apply(Maps, f -> numcols f-1);
    degs := apply(Maps, f -> degree f_0);
    degslist := apply(#degs, i->toList(i:0)|degs_i|toList(1-i+dims_i:0));
    PPS := PP**(S/I);
    p := map(PPS,S);
    q := apply(#Maps,i-> select(gens PPS, u -> (degree u)_i == 1));
    lastdegs := toList (#Maps:0)|{1};
    Mats := apply(#Maps, i->
             map(PPS^{degslist_i,lastdegs}, PPS^(1+dims_i), 
	    (u,v) ->if u == 0 then p (Maps_i_v)_0 else q_i_v)
	);
    J := sum(Mats, m->minors(2,m));
    Jsat := saturate(J, irrelevantIdeal PPS);
    eliminate( (gens S)/p, Jsat)
    )

schemeInProduct(Ideal, List, Symbol) := (I,Maps,X) ->(
    --This version forms the corresponding product of projective spaces, and
    --returns the multi-homogeneous ideal of the image.
    S := ring I;
    kkk := coefficientRing S;
    dims := apply(Maps, f -> numcols f-1);
    PP := productOfProjectiveSpaces (dims, symbol X, kkk);
    schemeInProduct(I,Maps,PP)
)
 ///
 restart
 loadPackage("K3Carpets", Reload=>true)
 S = ZZ/101[a,b]
 I = ideal 0_S
 f0 = matrix"a,b"
 f1 = matrix"a,b"
 maps = {f0,f1}
 schemeInProduct(I, maps, Y) 
///
smallDiagonal = method()
smallDiagonal ZZ  := n ->(
    --returns the ring of (P^1)^n, and in it the ideal of the small diagonal
    S := productOfProjectiveSpaces n;
    smallDiagonal S)

smallDiagonal Ring  := S ->(
    --given an appropriate ring, as returned by 
    --productOfProjectiveSpaces n
    --returns the ideal of the small diagonal
    n := numgens S//2;
    v := genericMatrix(S,S_0,2,n);
    minors(2,v)
    )

degenerateK3=method(Options=>{Characteristic=>32003})
degenerateK3(ZZ,ZZ,Sequence):= opt -> (a,b,e) -> (
    S := productOfProjectiveSpaces(2,Characteristic=>opt.Characteristic);
    J := ideal( S_1^2*S_2^2-e_0*S_0*S_1*S_2*S_3+e_1*S_0^2*S_3^2);
    correspondenceScroll(J,{a,b})
    )

degenerateK3(ZZ,ZZ,List) := opt -> (a,b,L) -> (e:=(sum L,product L);
    degenerateK3(a,b,e,Characteristic=>opt.Characteristic))

relativeEquations=method()
relativeEquations(ZZ,ZZ,ZZ) := (a,b,k) -> (
--    k=3,    (a,b)=(4,4)
    assert(a>=b);
    root := 13; p:=null;
    while (p = first last factor (root^k-1);
	not (p>=b)) do root=root+1;
    kk := ZZ/p;
    e := (1,root+1,root);
    x := symbol x; y := symbol y;
    S := kk[x_0..x_a,y_0..y_b];
    A := matrix apply(2,i->apply(a,j->x_(i+j)));
    B := matrix apply(2,i->apply(b,j->y_(i+j)));
    D := (diagonalMatrix({1,-e_1,e_2}))^{2,1,0};
    A1 := matrix apply(3,i->apply(a-1,j->x_(i+j)));
    B1 := matrix apply(3,i->apply(b-1,j->y_(i+j)));
    J := minors(2,A)+minors(2,B)+(C := ideal (transpose A1*D*B1));
    assert(isHomogeneous J);
    
    as:=apply(k,i->#select(a+1,j->(j-i)%k==0)-1);
    asmax:=max as;
    bs:=apply(k,i->#select(b+1,j->(j-i)%k==0)-1);
    bsmax :=max bs;
    degs:={{0,0,1},{0,0,1}}|apply(as,ai->{1,0,asmax-ai})|apply(bs,bi->{0,1,bsmax-bi});
    u := symbol u;v := symbol v; s:= symbol s; t:= symbol t;
    coxRing:=kk[s,t,u_0..u_(k-1),v_0..v_(k-1),Degrees=>degs];
    phi0 := matrix{
        apply(a+1,i->(j:=i%k;ai:=as_j;l:=lift((i-j)/k,ZZ);s^(ai-l)*t^l*u_j)) |
        apply(b+1,i->(j:=i%k;bi:=bs_j;l:=lift((i-j)/k,ZZ);s^(bi-l)*t^l*v_j))
    };
    assert(isHomogeneous phi0);
    phi := map(coxRing,S,phi0);
    Au := gens sum apply(k,j->saturate(image (phi A)_{j},ideal(s*t)));
    Bv := gens sum apply(k,j->saturate(image (phi B)_{j},ideal(s*t)));
    A1u:= gens sum apply(k-1,j->saturate(image(phi A1)_{j},ideal(s*t)));
    B1v:= gens sum apply(k-1,j->saturate(image(phi B1)_{j},ideal(s*t)));
    Ccox:=transpose A1u*D*B1v;
    Jcox := saturate(phi J,ideal(s*t));    
    m1 := gens Jcox;
    dm1 := degrees source m1;
    cols :=select(#dm1,i->dm1_i_0+dm1_i_1==2);
    Jcox = ideal m1_cols;
    m2:=gens saturate(ideal Ccox,ideal(s*t));
    dm2 := degrees source m2;
    cols =select(#dm2,i->dm2_i_0+dm2_i_1==2);
    IC := ideal(m2_cols);
    assert((minors(2,Au)+minors(2,Bv)+IC)== Jcox);
    Jcox)

coxMatrices=method()
coxMatrices(ZZ,ZZ,ZZ) := (a,b,k) -> (
--    k=3,    (a,b)=(4,4)
    assert(a>=b);
    root := 13; p:=null;
    while (p = first last factor (root^k-1);
	not (p>=b)) do root=root+1;
    kk := ZZ/p;
    e := (1,root+1,root);
    x := symbol x; y := symbol y;
    S := kk[x_0..x_a,y_0..y_b];
    A := matrix apply(2,i->apply(a,j->x_(i+j)));
    B := matrix apply(2,i->apply(b,j->y_(i+j)));
    D := (diagonalMatrix({1,-e_1,e_2}))^{2,1,0};
    A1 := matrix apply(3,i->apply(a-1,j->x_(i+j)));
    B1 := matrix apply(3,i->apply(b-1,j->y_(i+j)));
    J := minors(2,A)+minors(2,B)+(C := ideal (transpose A1*D*B1));
    assert(isHomogeneous J);
    
    as:=apply(k,i->#select(a+1,j->(j-i)%k==0)-1);
    asmax:=max as;
    bs:=apply(k,i->#select(b+1,j->(j-i)%k==0)-1);
    bsmax :=max bs;
    degs:={{0,0,1},{0,0,1}}|apply(as,ai->{1,0,asmax-ai})|apply(bs,bi->{0,1,bsmax-bi});
    u := symbol u;v := symbol v; s:= symbol s; t:= symbol t;
    coxRing:=kk[s,t,u_0..u_(k-1),v_0..v_(k-1),Degrees=>degs];
    phi0 := matrix{
        apply(a+1,i->(j:=i%k;ai:=as_j;l:=lift((i-j)/k,ZZ);s^(ai-l)*t^l*u_j)) |
        apply(b+1,i->(j:=i%k;bi:=bs_j;l:=lift((i-j)/k,ZZ);s^(bi-l)*t^l*v_j))
    };
    assert(isHomogeneous phi0);
    phi := map(coxRing,S,phi0);
    Au := gens sum apply(k,j->saturate(image (phi A)_{j},ideal(s*t)));
    Bv := gens sum apply(k,j->saturate(image (phi B)_{j},ideal(s*t)));
    A1u:= gens sum apply(k-1,j->saturate(image(phi A1)_{j},ideal(s*t)));
    B1v:= gens sum apply(k-1,j->saturate(image(phi B1)_{j},ideal(s*t)));
    (Au,Bv,A1u,B1v)) 


relativeResolution=method()
relativeResolution(ZZ,ZZ,ZZ) := (a,b,k) -> (
    Jcox := relativeEquations(a,b,k);
    Lrel := {(gens Jcox)};s1 := null;m1 := null;
    for i from 3 to 2*k-2 do (
	s1=syz(last Lrel,DegreeLimit=>{i,i,i});
	m1=s1_(positions(toList(0..rank source s1-1),j->(degrees source s1)_j_0+(degrees source s1)_j_1==i));
        Lrel=append(Lrel,m1);
	);
    m2 := syz(last Lrel);
    frel := chainComplex append(Lrel,m2);
    frel)
   
relativeResolutionTwists=method()
relativeResolutionTwists(ZZ,ZZ,ChainComplex) := (amax,bmax,fcox) -> (
    relTwists:={{{0,0}}}|apply(length fcox,i->
	apply(degrees fcox_(i+1),d->{d_0+d_1,d_2-amax*d_0-bmax*d_1}));
    relTwists
    )

resonanceScroll = method()
resonanceScroll(ZZ,ZZ,ZZ) := (a,b,k) -> (
    assert( a>k and b>k);
    as:=apply(k,i->#select(a+1,j->(j-i)%k==0)-1);
    bs:=apply(k,i->#select(b+1,j->(j-i)%k==0)-1);
    (as,bs))

computeBound=method()

computeBound(ZZ,ZZ,ZZ):= (a,b,k) -> (
    (as,bs):=resonanceScroll(a,b,k);
    fcox:=relativeResolution(a,b,k);
    ma:=as_0;
    mb:=bs_0;
    j:=0;relTwists:= null;
    while (
	relTwists=relativeResolutionTwists(ma+j,mb+j,fcox);
	not -relTwists_(2*k-2)_0_1+1 >=
	2*k-3+max apply(relTwists_(2*k-3),d->-d_1)) do (j=j+1);
    (a+k*j,b+k*j))

computeBound(ZZ) := k-> (
    cases := flatten apply(toList(k+1..2*k),a->apply(toList(k+1..a),b->(a,b,k)));
    min apply(cases,abk->elapsedTime min computeBound(abk)))
///
computeBound 3
computeBound 4
///

allGradings=method()
allGradings (ChainComplex,Ring) := (fJ,Sall) -> (
    fJall := new ChainComplex;
    fJall.Ring = Sall;
    fJall_0 = Sall^1;
    for i from 1 to length fJ do (
	m := map(fJall_(i-1),,sub(fJ.dd_i,Sall));
	fJall_i = source m;
	fJall.dd_i=m);
    chainComplex apply(length fJ,i->fJall.dd_(i+1))
    )
--loadPackage("K3Carpets", Reload => true)

upperTriangular=method(TypicalValue=>Boolean)
upperTriangular(Matrix) := B -> (
   if rank target B == 1 then return true;
   if rank target B == 2 then return B_(1,0)==0;
   #(keys tally flatten   apply(rank target B,i->apply(i-1,j->B_(i,j)==0)))==1)

carpetDet=method()
carpetDet(ZZ,ZZ) := (a,b) -> (
  --a=5,b=a
	I := carpet(a,b);
	elapsedTime fI := res(I,FastNonminimal=>true,LengthLimit=>a);
        S :=ring I;
	kk := coefficientRing S;
        degs := apply(a+1,j->{1,0,j})|apply(b+1,j->{0,1,j});
        Sall := kk[gens S,Degrees=>degs];
	elapsedTime F:= allGradings(fI,Sall);
	SZ := ZZ[gens S,Degrees=>degs];
	FZ:= chainComplex apply(length F,i->sub(F.dd_(i+1),SZ));
	assert( keys tally apply(length F-1,i->FZ.dd_(i+1)*FZ.dd_(i+2)==0) == {true});    
        degsM := tally select(degrees F_a,d->d_0+d_1==a+1);
        blocks := keys degsM;
	M := FZ.dd_a;
	degsSource := degrees source M;
        degsTarget := degrees target M;
	print ("number Of blocks",#blocks);
	cols := null; rows := null;B := null;gB:= null;detB:=null;
        dets := apply(blocks,deg ->(
--		deg =first blocks
		cols = select(rank source M,i->degsSource_i==deg);
		rows = select(rank target M,i->degsTarget_i==deg);
                B=transpose sub(M^rows_cols,ZZ);
		elapsedTime gB=gens gb image B;
                if not (upperTriangular gB) then (
		    print betti gB;
	 	    return gB);
		detB=product(rank B,i->gB_(i,i) );
		print factor detB;
		detB)
	    );
	product dets)

///
restart
loadPackage("K3Carpets",Reload=>true)
L ={3,3}
I=degenerateK3(3,3,{1,1})
minimalBetti I
betti(F=res(I,FastNonminimal=>true))
I'=carpet(3,3)
betti(F'=res(I',FastNonminimal=>true))
assert(betti F == betti F')
I=degenerateK3(5,5,(-1,1))
minimalBetti I
///
///
loadPackage( "K3Carpets",Reload=>true)
L={2,3}
n=#L
S= productOfProjectiveSpaces L
vars S
///

--UNDER CONSTRUCTION:
rationalOnP1n = method()
rationalOnP1n List := L ->(
    --constructs the general geometrically rational curve of type L_0,L_1.. on 
    --(P^1)^n, with multigrading, where n = #L
    n:= #L;
    S := productOfProjectiveSpaces n;
    kk := coefficientRing S;
    tar := random(S^1, S^{2:{-L_0,-L_0}});
    scan(n-1, i->tar = tar||random(S^1, S^{2:{-L_i,-L_i}}));
    tar)

-*
restart
uninstallPackage "K3Carpets"
loadPackage ("K3Carpets", Reload =>true)
     I = rationalOnP1n({1,1})
*-



///
restart
loadPackage("K3Carpets", Reload =>true)
loadPackage("StableResidual", Reload =>true)

scroll = {2,2,2}
x = symbol x;
S = productOfProjectiveSpaces(#scroll,x)
J1 = smallDiagonal S
correspondenceScroll(J1,scroll)

T = kk[y_0,y_1,y_2]
threepoints = intersect(ideal(y_0,y_1),ideal(y_0,y_2),ideal(y_1,y_2))
f = rand(threepoints,1,3) -- general cubic through the three points
ST = (flattenRing(T**S))_0
ff = entries sub(transpose matrix {{y_0,y_1},{y_0,y_2},{y_1,y_2}}, ST) -- projections from the three points
irrel = irrelevantIdeal ST

D1 = det matrix{{x_(0,0),ff_1_0},{x_(1,0),ff_0_0}}
D2 = det matrix{{x_(0,1),ff_1_1},{x_(1,1),ff_0_1}}
D3 = det matrix{{x_(0,2),ff_1_2},{x_(1,2),ff_0_2}}
    
J = sub(f, ST)+ideal(D1,D2,D3)
Js = saturate(J, irrel);
I = eliminate({y_0,y_1,y_2}, Js);
IS = (map(S,ST))I;
codim I

g = correspondenceScroll(IS, scroll);
minimalBetti g -- NOT Gorenstein. 
--the curve is an image of an elliptic, but is it actually (arithmetically) genus 1?

n= 3
L = {1,2,3}
phi = map(ZZ/32003[a,b], productOfProjectiveSpaces 2, {a,b,a,b}, DegreeLift => (D -> (
	{sum(#D//2, i ->D_i), sum(#D//2, i->D_(i+1))})))
isHomogeneous phi
viewHelp (map, Ring, Ring, List)
ker phi
I = smallDiagonal 2
scroll = {1,1,1}
J = correspondenceScroll(I^2, scroll)
betti res J
codim J
S = ring J
betti res prune Ext^2(S^1/J, S^1)
S = kk[x_(0,0)..x_(1,#scroll-1), Degrees =>{2:{1,0},2:{0,1}}]
I = ideal sum(2, j-> product(#scroll, i->x_(i,j)))

betti res oo
correspondenceScroll(I^2, scroll)
betti res oo
    degs := flatten apply(n, i-> toList (2:apply(n, j-> if j==i then 1 else 0)));
    x := symbol x;
    kk := ZZ/32003;
    varlist := flatten apply(n, i->apply(2,j->x_(j,i)));
    S := kk[varlist, Degrees =>degs];
    ideal flatten apply(n, i-> 
	              apply(toList(i+1..n-1), j->
	          v_(0,i)*x_(1,j) - x_(1,i)*x_(0,j)))
///

    
irrelevantIdeal = R ->(
u := unique ((gens R)/degree);
intersect(apply(u, d -> ideal basis(d,R))))


beginDocumentation()

document { 
  Key => K3Carpets,
  Headline => "The unique Gorenstein double structure on a surface scroll",
  "This package accompanies our paper ",
   HREF("http://arxiv.org","Equations and syzygies of K3 carpets and union of scrolls") ,
  " for experimental exploration.   There is a unique surjection from the ideal of a 2-dimensional rational normal scroll (other than the cone
   over a rational normal curve) onto the canonical module of the scroll 
   and the kernel
   of the this map is the ideal of a scheme that looks numerically like a K3 surface: a K3 carpet.
   (Theorem 1.3 of Degenerations of K3 surfaces in projective space,
   by Francisco Gallego and B.P. Purnaprajna,
   Trans. Amer. Math. Soc. 349 (1997), no. 6, 2477–2492.)",
   PARA{},
-*   "For $a,b > 1$ he ideal of the carpet and more general a family of degenerate K3 surfaces $X_e(a,b)$ is generated
   by the 2x2 minors of the matrices
   $$
\begin{pmatrix}
x_0 & x_1 & ... & x_{a-1} \\
x_1 & x_2 & ... & x_a \\
\end{pmatrix}
\qquad
\begin{pmatrix}
y_0 & y_1 & ... & y_{b-1} \\
y_1 & y_2 & ... & y_b \\
\end{pmatrix}
$$ 
and the entries of the $(a-1) \times (b-1)$ matrix 
$$
 \begin{pmatrix} 
 x_0 & x_1 & x_2 \\
 x_1 & x_2 & x_3 \\
  : & : & :\cr
 x_{a-2}&x_{a-1}& x_a \\
 \end{pmatrix}
 \begin{pmatrix} 
 0& 0 &  e_2 \\
 0 & -e_1 & 0 \\
 1 & 0 & 0 \\
 \end{pmatrix} 
  \begin{pmatrix} 
 y_0 & y_1 & ... &y_{b-2} \\
 y_1 & y_2 & ... & y_{b-1} \\
 y_2 & y_3 & ... & y_{b} \\
 \end{pmatrix} 
 $$
   "*-
   "The carpet lies on the intersection of the cones over two rational normal curves Ca and Cb
   of degrees a>=b. We write the ideal of Ca as the minors of a 2xa matrix X with entries x_i, i= 0..a,
   and similarly for Cb, with  a 2 x b matrix Y with entries y_j. We write Xi for the ith column of X, and
   similarly for Y.
   In the general case, where a,b are both >=2, the additional generators of the ideal of the Carpet are then given by the differences
   det(Xi,Yj)-det(X(i+1),Y(j-1)), or equivalently, by the minors of (Xi+Yj,X(i+1)+Y(j-1),        
   (In the case a=1=b the ideal is the square of the determinant of X|Y; if a>1, b=1 then for the mixed minors we replace the 1-column matrix Y
   by a symmetric 2x2 matrix with entries y_0^2,y_0y_1,y_1^2 )",
   PARA{},
   "The hyperplane section of a K3 carpet is a canonical ribbon indexed by genus g=a+b+1 and clifford index b.
   ",
    PARA{},
    "The K3 carpets generalize to a family of degenerate K3 surfaces which are unoins of two scrolls,
    whose hyperplane sections are reducible canonical curves consisting of two rational normal curves of degree
    g-1 intersecting in g+1 points. The functions in this package explore the syzygies of these surfaces
    for fields of arbitrary characteristic. Inparticular, the functions in the package allow for g <= 15 a computational proof of the following
    conjecture.",
    PARA{},
     HREF("http://arxiv.org","Conjecture 0.1") ,
    " A general canonical curve of genus g over a field of characteristic p satisfies Green's conjecture,
    if p >= (g-1)/2.",
    
   PARA{},
    SUBSECTION "Constructions",
    UL{
       TO carpet,
       TO canonicalCarpet,
       TO gorensteinDouble,
      },    
    SUBSECTION "Analyzing",
    UL{ 
	TO carpetBettiTables,
        TO carpetBettiTable,
	TO analyzeStrand,
	TO degenerateK3BettiTables,
	TO schreyerName, -- should be moved to an other package, e.g. NonminimalComplexes
        TO allGradings,
	TO carpetDet,
	TO resonanceDet,
	},
    SUBSECTION "Correspondence Scrolls",
    UL{ 
	TO correspondenceScroll,
        TO hankelMatrix,
        TO productOfProjectiveSpaces,
	TO schemeInProduct,
        TO smallDiagonal,
	TO irrelevantIdeal,
	TO degenerateK3
	},
    SUBSECTION "Relative resolutions of X_e(a,b) in case of k resonance",
    UL{
	TO resonanceScroll,
	TO coxMatrices,
	TO relativeEquations,	
	TO relativeResolution,
	TO relativeResolutionTwists,
	TO computeBound
	},
    SUBSECTION "Homotopies",
    UL{ 
	TO homotopyRanks,
        TO canonicalHomotopies,
        },
   }

doc ///
   Key
    degenerateK3
    (degenerateK3, ZZ, ZZ, Sequence)
    (degenerateK3, ZZ, ZZ, List)    
    [degenerateK3, Characteristic]
   Headline
    Ideal of a degenerate K3 surface X_e(a,b)
   Usage
    I = degenerateK3(a,b,e)
    I = degenerateK3(a,b,t)    
   Inputs
    a:ZZ
    b:ZZ
     a and b should be positive
    e:Sequence
     of two integers (e_1,e_2) 
    t:List
     of two integers {t_1,t_2}
    Characteristic => ZZ 
     the characteristic of the ground field
   Outputs
    I:Ideal
   Description
    Text
     The routine computes the homogeneous ideal of the degenerate K3 surface
     in $\mathbb P^{a+b+1}$ associated as in HREF{y}{x} to a polynomial
     $$p(z)=z^2-e_1z+e_2=(z-t_1)(z-t_2)$$
     In case $p(z)=(z-1)^2$ it coincides with carpet(a,b).
    Example
     I=degenerateK3(5,5,{1,1});
     minimalBetti I
     I_10
     I=degenerateK3(5,5,(-1,1));
     I_10
     minimalBetti I
     I=degenerateK3(5,5,{1,1},Characteristic=>3);
     minimalBetti I
   Caveat
    
   SeeAlso
    carpet
    correspondenceScroll
///

doc ///
   Key
    carpet
    (carpet, ZZ, ZZ)
    (carpet, ZZ, ZZ, Matrix)    
    [carpet, Characteristic]
    [carpet, FineGrading]    
    [carpet, Scrolls]
   Headline
    Ideal of the unique Gorenstein double structure on a 2-dimensional scroll
   Usage
    I = carpet(a1,a2)
    I = carpet(a1,a2,m)    
    (I,xmat,ymat) = carpet(a1,a2,Scrolls=>true)
   Inputs
    a1:ZZ
    a2:ZZ
     a1 and a2 should be positive
    m:Matrix
     a 2xn matrix for some $n \ge{} a1+a2$
    Characteristic => ZZ 
     the characteristic of the ground field
    Scrolls =>  Boolean
     if true return in addition the matrices defining the sections
    FineGrading => Boolean
     if true then I is defined over the ring with $\ZZ^4$-grading
   Outputs
    I:Ideal
    xmat: Matrix
    ymat: Matrix
     the matrices of the sections of the scroll
   Consequences
    Item
     If no matrix m is present then the script creates a type a1,a2 K3-carpet over a new ring. If m is given,
     then an ideal made from certain minors and sums of minors of m is produced.
     The characteristic is given by the option, defaulting to 32003.
     If the option FineGrading is set to true, then the ideal is returned with the natural $\ZZ^4$ grading
     (the default is FineGrading => false). This last may not work unless the matrix is of scroll type (or
     not given!) If Scrolls=>true, then a sequence of three items is returned, the second
     and third being the smaller and larger scroll matrices.
   Description
    Text
     The routine carpet(a1,a2,m) sets a = min(a1,a2), b = max(a1,a2), and forms
     two matrices from m:
     X:the 2 x a matrix that is the first a cols of m;
     Y:the 2 x b matrix that is the nex b cols of m--that is, cols a1..a1+a2-1 of m;
     Let Ix, Iy be the ideals of 2 x 2 minors of X and Y. If $a,b\geq 2$,the routine returns
     Ix+Iy+Imixed, where Imixed
     consists of the quadrics "outside minor - inside minor", that is,
     $det(X_{\{i\}},Y_{\{j+1\}})-det(X_{\{i+1\}}|Y_{\{j\}})$,
     for each pair of (i,i+1), (j,j+1) in the ranges a1 and a2.
     
     If m is usual ideal of the scroll of type (a,b), then carpet(a,b,m) produces the same ideal
     (over a different ring) as carpet(a,b). This is the ideal of the 2-dimensional
     rational normal scroll Scroll(a1,a2) is the ideal of 2 x 2 minors of X|Y.
     The ideal I to be constructed is the ideal of the unique (numerically) K3 scheme that is a double
     structure on the scroll S(a1,a2).
     
     When a,b > 1, the carpet ideal I is the sum $Ix+Iy$ plus
     the ideal Imixed 
     
     When a = b = 1, I is the square of the determinant of X|Y.

     When a = 1, b>1 (or symmetrically), I is defined as in the case a,b>1, after replacing
     $$ X = \begin{pmatrix} 
                x_0 \\ 
		x_1 
	    \end{pmatrix}
     $$

     by the 2 x 2 matrix
     $$ \begin{pmatrix} 
            x_0^2    &  x_0*x_1 \\
	    x_0*x_1  &  x_1^2
	\end{pmatrix}
     $$
     and changing $a$ to 2.

    Example
     betti res carpet(2,5)
     S = ZZ/101[a..j]
     m = genericMatrix(S,a,2,5)
     I = carpet(2,3,m)
     L = primaryDecomposition I;
     betti res L_0
     betti res L_1
    Text
     
   Caveat
    We require $a1,a2 \ge 1$. If $a1>a2$ then the blocks are reversed, so that the smaller block always comes first.
    The script generalizeScroll is a more general tool that can do the same things.   
   SeeAlso
    canonicalCarpet
    gorensteinDouble
    correspondenceScroll
///
 

 doc ///
   Key
    canonicalCarpet
    (canonicalCarpet, ZZ, ZZ)
    [canonicalCarpet, Characteristic]
    [canonicalCarpet, FineGrading]    
    [canonicalCarpet, Scrolls]
   Headline
    Carpet of given genus and Clifford index
   Usage
    I = canonicalCarpet(g,cliff)
    (I,xmat,ymat) = canonicalCarpet(g,cliff,Scrolls=>true)
   Inputs
    g:ZZ
     desired genus
    cliff:ZZ
     desired clifford index
    Characteristic => ZZ 
     the characteristic of the ground field
    Scrolls =>  Boolean
     if true return in addition the matrices defining the sections
    FineGrading => Boolean
     if true then I is defined over the ring with $\ZZ^4$-grading
   Outputs
    I:Ideal
     ideal of the K3 Carpet of (sectional) genus g, Clifford index cliff
   Description
    Text
     This is just a re-indexing of the carpet script:
     canonicalCarpet(g,cliff) = carpet(g-cliff-1, cliff).
     Here the natural choices for cliff are $1 \leq{} cliff \leq{} (g-1)//2$.
   SeeAlso
    carpet
///



doc ///
   Key
    analyzeStrand
    (analyzeStrand, ChainComplex, ZZ)
   Headline
    analyze the (a+1)-st constant strand of F over ZZ
   Usage
    L = analyzeStrand(F,a)
   Inputs
    F:ChainComplex
     the nonminimal resolution of a carpet over large prime
    a:ZZ
     a+1 is the degree of the strand
   Outputs
     L:List
      the diagonal entries of the Smith normal form of a the cruital
      matrix
   Description
    Text
     Starting from a nonminimal resolution F of the carpet 
     over a larger finite prime field, we lift the complex to the integers,
     and compute the diagonal entries of the Smith normal form. The critical
     constrand strand for a carpet of type (a,b) with a>=b is the a+1-st
     strand. Green's conjecture for carpet says that the map has maximal 
     rank over QQ.     
    Example
     a=5,b=5
     I = carpet(a,b);
     F = res(I, FastNonminimal => true)
     L = analyzeStrand(F,a); #L
     betti F_a, betti F
     factor product L  
     L3 = select(L,c->c%3==0); #L3
     carpetBettiTable(a,b,3)
   SeeAlso
    carpetBettiTable
///

doc ///
   Key
    allGradings
    (allGradings, ChainComplex, Ring)
   Headline
    add Grading to a chainComplex
   Usage
    Fall = allGradings(F,S)
   Inputs
    F:ChainComplex
     the free resolution of an ideal
    S:Ring
     a ring with a finer garding  
   Outputs
    Fall:ChainComplex
      a complex over S, the same as F but now with homogeneous with respect to all gradings of S   
   Description
    Text
     Given a resolution F of an ideal, with carries additional homogenity with respect
     to the finer graded ring S, we compute the grading.
    Example
     a=3,b=3
     I=carpet(a,b);
     F = res(I,FastNonminimal=>true,LengthLimit=>2);
     betti F
     degs=apply(a+1,i->{1,0,i})|apply(b+1,j->{0,1,j})
     S=coefficientRing ring I[gens ring I,Degrees=>degs]
     Fall = allGradings(F,S)
     netList apply(length Fall+1,i->tally degrees Fall_i)
///

doc ///
   Key
    carpetDet
    (carpetDet, ZZ, ZZ)
   Headline
    compute the determinant of the crucial constant strand of a carpet X(a,b)
   Usage
    d = carpetDet(a,b)
   Inputs
    a:ZZ
     the larger value, i.e a>=b
    b:ZZ
     the desired clifford index    
   Outputs
     d:ZZ
      the crucial determinant  
   Description
    Text
     We compute  nonminimal resolution F of the carpet of type (a,b) over a finite prime field,
     Lift this to a resolution over ZZ, introduce the fine grading, grep the various blocks of
     the crucial map in the a-th strand, compute their determinants and return their product.
    Example
     a=4,b=4
     d=carpetDet(a,b)
     factor d
   SeeAlso
     analyzeStrand
///

doc ///
   Key
    resonanceDet
    (resonanceDet, ZZ)
   Headline
    compute the resonance determinant of the crucial constant strand of a degenerate K3 X_e(a,a)
   Usage
    (d1,d2) = resonanceDet(a)
   Inputs
    a:ZZ
   Outputs
     d1:Product
      the integer factor
     d2:Product
      the resonance factor in ZZ[e_1,e_2] 
   Description
    Text
     We compute the minimal resolution F of degenerate K3 X_e(a,a) over ZZ[e_1,e_2] where deg e_i =i and the variables
     x_0,..x_a,y_0..y_b have degrees deg x_i=i+1 and deg y_i=1. The equations of X_e(a,b) are homogeneous with respect to this grading.
     Viewed as a resolution over QQ(e_1,e_2), this resolution is non-minimal and carries further gradings. We
     decompose the crucial map of the a-th strand into blocks, compute their
     determinants, and factor the product.
    Example
     a=4
     (d1,d2)=resonanceDet(a)
   SeeAlso
     carpetDet
///



doc ///
   Key
    carpetBettiTables
    (carpetBettiTables, ZZ, ZZ)
   Headline
    compute the Betti tables of a carpet of given genus and Clifford index over all prime fields 
   Usage
    h = carpetBettiTables(a,b)
   Inputs
    a:ZZ
     the larger value, i.e a>=b
    b:ZZ
     the desired clifford index    
   Outputs
     h:HashTable
      of carpet Betti tables for various characteristics of the ground field  
   Description
    Text
     We compute the equation and nonminimal resolution F of the carpet of type (a,b)
     where $a \ge b$ over a larger finite prime field, lift the complex to the integers,
     which is possible since the coefficients are small. Finally we study the nonminimal
     strands over ZZ by computing the Smith normal form. The resulting data allow us
     to compute the Betti tables for arbitrary primes.
    Example
     a=5,b=5
     h=carpetBettiTables(a,b)
     T= carpetBettiTable(h,3)
     J=canonicalCarpet(a+b+1,b,Characteristic=>3);
     elapsedTime T'=minimalBetti J
     T-T'
     elapsedTime h=carpetBettiTables(6,6);
     keys h
     carpetBettiTable(h,7)
     carpetBettiTable(h,5)
   SeeAlso
    carpetBettiTable
///

doc ///
   Key
    degenerateK3BettiTables
    (degenerateK3BettiTables, ZZ, ZZ,Sequence)
   Headline
    compute the Betti tables of a degenerate K3 over all prime fields 
   Usage
    h = carpetBettiTables(a,b,e)
   Inputs
    a:ZZ
     the larger value, i.e a>=b
    b:ZZ
     the desired clifford index
    e:Sequence
     of two small integers e=(e_1,e_2)   
   Outputs
     h:HashTable
       Betti tables of the degenerate K3 for various characteristics of the ground field  
   Description
    Text
     We compute the equation and nonminimal resolution F of the degeneate K3 of type (a,b,e)
     where $a \ge b$ over a large finite prime field, lift the complex to the integers,
     which is possible if the coefficients are small. Finally we study the nonminimal
     strands over ZZ by computing the Smith normal form. The resulting data allow us
     to compute the Betti tables for arbitrary primes.
    Example
     a=5,b=5
     e=(-1,5)
     h=degenerateK3BettiTables(a,b,e)
     keys h
     elapsedTime T= minimalBetti degenerateK3(a,b,e,Characteristic=>5)
     T-h#5
    Text
     Already for fairly small values of (e_1,e_2) the result might be incorrect, because the lift to characteristic zero
     fails due to high powers of e_1 and e_2 in the non-minimal resolution. It would be easy to alter the program 
     to catch these mistakes.
    Example
     e=(-1,5^2)
     h=degenerateK3BettiTables(a,b,e)
     keys h
   Caveat
     Already for (e_1,e_2) fairly small, the algorithm might give wrong answers
     since the lift to characteristic zero  might be incorrect. A correction is easy to implement as soon
     res(.,FastNonminimal=>true) allows QQ (or ZZ) as coefficient ring. Another possibily
     would be to use the Chinese remainder for lifting to ZZ.
   SeeAlso
    carpetBettiTable
///




doc ///
   Key
    carpetBettiTable
    (carpetBettiTable, ZZ, ZZ, ZZ)
    (carpetBettiTable, HashTable, ZZ)
   Headline
    compute the Betti tables of a carpet of given genus and Clifford index over a prime field of characteristic p
   Usage
    h = carpetBettiTable(a,b,p)
    carpetBettiTable(h,p)
   Inputs
    a:ZZ
     the larger value, i.e a>=b
    b:ZZ
     the desired clifford index
    h:HashTable
     of carpet Betti tables for various characteristics of the ground field  
    p:ZZ
     the characteristic of the ground field
   Outputs
     :BettiTally
      the betti Table of a carpet of the given ground field
   Description
    Text
     We compute the equation and nonminimal resolution F of the carpet of type (a,b)
     where $a \ge b$ over a larger finite prime field, lift the complex to the integers,
     which is possible since the coefficients are small. Finally we study the nonminimal
     strands over ZZ by computing the Smith normal form. The resulting data allow us
     to compute the Betti tables for arbitrary primes.
    Example
     a=5,b=5
     elapsedTime T=carpetBettiTable(a,b,3)
     J=canonicalCarpet(a+b+1,b,Characteristic=>3);
     elapsedTime T'=minimalBetti J
     T-T'
     elapsedTime h=carpetBettiTables(6,6);
     carpetBettiTable(h,7)
     carpetBettiTable(h,5)          
   SeeAlso
     carpetBettiTables
///


doc ///
   Key
    schreyerName
    (schreyerName,ChainComplex,ZZ,ZZ)
    (schreyerName,ChainComplex)
    (schreyerName,ChainComplex,ZZ)
    (schreyerName,HashTable,ZZ,ZZ)
   Headline
    get the names of generators in the (nonminimal) Schreyer resolution according to Schreyer's convention
   Usage
    L = schreyerName(F,i,n)
    h = schreyerName(F,a)
    L = schreyerName(h,i,n)    
   Inputs
     F:ChainComplex
      the nonminimal resolution from a Groebner basis
     i:ZZ
      the homological degree
     n:ZZ
      the number of the generator
     a:ZZ
      homological degree
     h:HashTable
      of lists of names
   Outputs
    L:List
     list of monomials
    h:HashTable
     of names
   Description
    Text
     We name the generators of the syzygies by the list ofthe  monomial parts 
     of the leadTerm with position m recursively:
     
     schreyerName(F,i,n) = append(schreyerName(F,i-1,m),mon)
     
     where mon denotes the monomial part and m the position in F_(i-1) of 
     leadTerm F.dd_i_n.
    Example
     (a,b)=(5,4)
     I = carpet(a,b);
     F = res(I, FastNonminimal =>true)
     betti F
     i=3,n=10
     schreyerName(F,3,10)
     h=schreyerName F;
     h#8
     h#7_20
     h#7_20 == schreyerName(F,7,20)
///



doc ///
   Key
    canonicalHomotopies
    (canonicalHomotopies,ZZ,ZZ)
    [canonicalHomotopies, FineGrading]
    [canonicalHomotopies, Characteristic]    
   Headline
    Homotopies on the resolution of a K3 carpet
   Usage
    (F,h) = canonicalHomotopies(g, cliff)
   Inputs
    g:ZZ
    cliff:ZZ
    Characteristic => ZZ 
     the characteristic of the ground field
    FineGrading => Boolean
     if true then F is defined over the ring with $\ZZ^4$-grading
   Outputs
    F:ChainComplex
     free resolution of the canonical carpet of genus g, clifford index cliff
    h:Function
     h(i,j) is the homotopy with source F_j for the i-th quadric.
   Description
    Text
     By default the option FineGrading is set to false. With FineGrading=>true
     the script returns the $\ZZ^4$-graded resolution, and the function h returns the homotopies
     one graded component at a time as a HashTable. 
     
     Note that the homotopies are 0
     except in the middle part of the resolution,
     where there is a generator degree common to 
     two consecutive free modules.
    Example
     (F,h0) = canonicalHomotopies(7,3)
     betti F
     netList apply(length F, j-> sum(rank F_1, i->h0(i,j)))
     H = makeHomotopies1(F.dd_1, F);
     (F,h0) = canonicalHomotopies(7,3, FineGrading=>true);
     h0(0,2)
     homotopyRanks(7,3)
   SeeAlso
    canonicalCarpet
///


doc ///
   Key
    homotopyRanks
    (homotopyRanks, ZZ, ZZ)
    [homotopyRanks, Characteristic]
   Headline
    compute the ranks of the quadratic homotopies on a carpet
   Usage
    L = homotopyRanks(g,cliff)
   Inputs
    g:ZZ
     genus of the carpet
    cliff:ZZ
     Clifford index of the carpet
    Characteristic => ZZ 
     the characteristic of the ground field
   Outputs
    L:Net
     netList showing the ranks
   Description
    Text
     Prints the Betti table of the canonical carpet
     and a list of pairs, with first element a quadric and second element
     the sequence of ranks of the homotopies for that quadric on F_i, for i = 1..g-2.
    Example
     homotopyRanks(7,3)
     homotopyRanks(7,3, Characteristic => 2)     
///


doc ///
   Key
    FineGrading
   Headline
    Option for carpet, canonicalCarpet
   Description
    Text
     The default is FineGrading => false. If the option FineGrading=>true is given, then the 
     ideal returned has the natural $\ZZ^4$ grading, where x_i has degree \{1,0,i,a-i\}\ and 
     y_i has degree \{0,1,i,b-i\}. 
     (Note that after the call carpet(a1,a2) we have a = min(a1,a2), b = max(a1,a2).)
///
doc ///
   Key
    Scrolls
   Headline
    Option for carpet, canonicalCarpet
   Description
    Text
     The default is Scrolls => false. If the option Scrolls=>true is given, then 
     a triple is returned, with the ideal first followed by xmat and ymat, the smaller
     and larger of the scrolls containing the carpet.
///

doc ///
   Key
    gorensteinDouble
    (gorensteinDouble,Ideal)
   Headline
    attempts to produce a Gorenstein double structure J subset I
   Usage
    gorensteinDouble I
   Inputs
    I:Ideal
   Outputs
    J:Ideal
   Description
    Text
     Let S = ring I, and that I is an ideal of codimension c.
     Let F be the S-free resolution of S/I.
     Assuming that S is a polynomial ring and S/I is Cohen-Macaulay, 
     the canonical module of S/I is 
     omega = coker transpose F.dd_c. The script returns the ideal J that is the kernel of the first
     element of Hom(I, omega). In case I is the ideal of a scroll there is a unique
     element of minimal degree, and it represents a surjection, so S/J is Gorenstein.
///

doc ///
   Key
    productOfProjectiveSpaces
    (productOfProjectiveSpaces,List,Symbol,Ring)    
    (productOfProjectiveSpaces,List,Symbol)
    (productOfProjectiveSpaces,List)
    (productOfProjectiveSpaces,ZZ,Symbol)
    (productOfProjectiveSpaces,ZZ)
    [productOfProjectiveSpaces, Characteristic]
   Headline
    Constructs the multi-graded ring of a product of copies of P^1 (pp is a synonym)
   Usage
    R = productOfProjectiveSpaces(L,y,kkk)      
    R = productOfProjectiveSpaces(L,y)   
    R = productOfProjectiveSpaces L
    R = productOfProjectiveSpaces (n, y)
    R = productOfProjectiveSpaces n
   Inputs
    L:List
     of positive integers, the dimensions of the projective spaces
    n:ZZ
     positive integer, number of P^1 factors
    y:Symbol
    kkk:Ring
     ground field to use in the construction
     variable name for R (defaults to "x" if none given)
    Characteristic => ZZ 
     the characteristic of the ground field
   Outputs
    R:Ring
     ZZ^n - graded
   Description
    Text
     The variables are in 1+L_i -tuples, x_(0,i).. x_(L_i,i) with degree {0..0,1,0..0}, the 1 being in 
     the i-th place.     
    Example
     R = productOfProjectiveSpaces{1,3}
     v = gens R
     v/degree
     gens productOfProjectiveSpaces({1,1},symbol y)
     gens productOfProjectiveSpaces 2
///


-*
doc ///
   Key
    rationalOnP1n
    (rationalOnP1n, List)
   Headline
    Constructs the ideal of a general rational curve of type L on (P^1)^n

   Usage
    I  = rationalOnP1n L
   Inputs
    L:List
     List of positive integers - the degrees of the curve
   Outputs
    I:Ideal
     in a ring R = productOfProjectiveSpaces #L
   Description
    Text
     UNDER CONSTRUCTION!
     Chooses pairs of general forms of degree L_i for i = 1..n
     and forms the corresponding curve in (P^1)^n with n = #L
    Example
     I = rationalOnP1n({1,1})
     S = ring I;
     isHomogeneous I
///
*-

doc ///
   Key
    correspondenceScroll
    (correspondenceScroll, Ideal, List)
   Headline
    Union of planes joining points of rational normal curves according to a given correspondence
   Usage
    G = correspondenceScroll(I,scroll)
   Inputs
    scroll:List
     list of positive integers, the degrees of disjoint rational normal curves
    I:Ideal
     ideal of a correspondence; an arbitrary subscheme of (P^1)^(#scroll)
   Outputs
    G:Ideal
     ideal of the generalized scroll
   Description
    Text
     Let L = {a_0,..a_{(m-1)}}, and let P = P^{(#L-1+ sum L)}.
     Just as the ordinary scroll S(L) is  the union of planes joining rational normal curves C_i
     of
     degree a_i according to some chosen isomorphism among them (a (1,1,..,1) correspondence), 
     the generalized Scroll is the union of planes joining the points that correspond under
     an arbitrary correspondence, specified by I.

     Thus if I is the ideal of the small diagonal of (P^1)^m, then 
     generalized Scroll(I,L) is equal to S(L). If #L = 2, and I is the square of the ideal
     of the diagonal, we get a K3 carpet:
    Example
     L = {3,4}
     x = symbol x;
     S = productOfProjectiveSpaces(#L,x) --creates the multi-graded ring of (P^1)^(#L)
     Delta = smallDiagonal S -- the ideal of the small diagonal of (P^1)^(#L)
     G = correspondenceScroll(Delta, L)
     minimalBetti G
     G = correspondenceScroll(Delta^2, L)
     minimalBetti G
    Text
     Here is how to make the generalized scroll corresponding to a general elliptic curve
     in (P^1)^3. First, the general elliptic curve, as a plane cubic through three given points:
    Example
     T = ZZ/32003[y_0,y_1,y_2]
     threepoints = gens intersect(ideal(y_0,y_1),ideal(y_0,y_2),ideal(y_1,y_2))
     f = threepoints*random(source threepoints, T^{-3}); -- general cubic through the three points
     L = {2,2,2}
     x = symbol x;
     S = productOfProjectiveSpaces(#L,x) --creates the multi-graded ring of (P^1)^(#L)
     ST = (flattenRing(T**S))_0
     irrel = irrelevantIdeal ST;
    Text
     Here the irrelevant ideal is the intersection of the 4 ideals of coordinats
     (P^2 and the three copies of P^1).
     Next, define the pairs of sections on the curve giving the three projections:
    Example
     ff = entries sub(transpose matrix {{y_0,y_1},{y_0,y_2},{y_1,y_2}}, ST) -- projections from the three points
    Text
     And create the equations of the incidence variety
    Example
     D1 = det matrix{{x_(0,0),ff_1_0},{x_(1,0),ff_0_0}}
     D2 = det matrix{{x_(0,1),ff_1_1},{x_(1,1),ff_0_1}}
     D3 = det matrix{{x_(0,2),ff_1_2},{x_(1,2),ff_0_2}}
     J = sub(ideal f, ST)+ideal(D1,D2,D3)
    Text
     This must be saturated with respect to the irrelevant ideal, and then the y variables are eliminated,
     to get the curve in (P^1)^3.
    Example
     Js = saturate(J, irrel);
     I = eliminate({y_0,y_1,y_2}, Js);
     IS = (map(S,ST))I;
     codim I
    Text 
     Finally, we compute the ideal of the generalized Scroll:
    Example
     g = correspondenceScroll(IS, L);
     minimalBetti g
   Caveat
    The script currently uses an elimination method, but could be speeded up by replacing that
    with the easy direct description of the equations that come from the correspondence I.
   SeeAlso
    irrelevantIdeal
    pp
    smallDiagonal
///

doc ///
   Key
    smallDiagonal
    (smallDiagonal, Ring)
    (smallDiagonal, ZZ)
   Headline
    Ideal of the small diagonal in (P^1)^n
   Usage
    I = smallDiagonal n
    I = smallDiagonal S
   Inputs
    n:ZZ
     number of factors of P^1
    S:Ring
     ring with 2n vars, corresponding to (P^1)^n for some n
   Outputs
    I:Ideal
     ideal of the small diagonal
   Description
    Example
     smallDiagonal 3
     S = productOfProjectiveSpaces 3
     smallDiagonal S
   SeeAlso
    pp
///

doc ///
   Key
    irrelevantIdeal
   Headline
    returns the irrelevant ideal of a multi-graded ring
   Usage
    mm = irrelevantIdeal R
   Inputs
    R:Ring
     (multi)-graded ring
   Outputs
    mm:Ideal
     irrelevant ideal of R
   Description
    Text
     Returns the intersection of the ideals of variables in each single multi-degree.
    Example
     R = productOfProjectiveSpaces 3
     vars R
     (gens R)/degree     
     irrelevantIdeal R
   SeeAlso
    pp
///

doc ///
   Key
    hankelMatrix
    (hankelMatrix, Matrix, ZZ, ZZ)
    (hankelMatrix, Ring, RingElement, ZZ, ZZ)
    (hankelMatrix, Ring, ZZ, ZZ)    
    (hankelMatrix, ZZ, ZZ)
   Headline
    matrix with constant anti-diagonal entries
   Usage
    m = hankelMatrix(r,p,q)
    m = hankelMatrix(R,x,p,q)    
    m = hankelMatrix(R,p,q)    
    m = hankelMatrix(p,q)        
   Inputs
    r:Matrix
     matrix whose first row will be the entries of m
    R:Ring
     in this case, hankelMatrix will use the variables of R
    x:RingElement
     if present, hankelMatrix will use the variables of R starting with x
    p:ZZ
     number of rows
    q:ZZ
     number of columns
   Description
    Text
     A Hankel matrix (or catalecticant matrix) is a matrix with a repeated element on
     each anti-diagonal, that is m_(i,j) depends only on i+j. 
     If the matrix r is given, then we set m_(i,j) = r_(0,i+j) if i+j<numcols r, and 0 otherwise.
     The degrees of the rows of m are set to 0, and the degrees of the columns are set to
     the degree of r_(0,0); thus the Hankel matrix is homogeneous iff all the entries
     of the first row of r have the same degree.
    
     If no ring or matrix is given, hankelMatrix defines a new ring S with p+q-1 variables X_i,
     and then calls hankelMatrix(vars S, p,q).
    Example
     p = 2;q=3;
     S = ZZ/101[x_0..x_(p+q-2)]
     hankelMatrix(vars S, p,q)
     r = vars S ** transpose vars S
     hankelMatrix(r, p,q)
     hankelMatrix(S,p,q)
     hankelMatrix(r, p,q+2)
     hankelMatrix(p,q+2)
///
doc ///
   Key
    schemeInProduct
    (schemeInProduct, Ideal, List, Ring)    
    (schemeInProduct, Ideal, List, Symbol)
   Headline
    multi-graded Ideal of the image of a map to a product of projective spaces
   Usage
    J = schemeInProduct(I,maps,PP)
    J = schemeInProduct(I,maps,X)    
   Inputs
    I:Ideal
     defining the source scheme
    maps:List
     list of sequences of polynomials each of a single degree, in I
    PP:Ring
     multigraded ring of the product of projective spaces
    X:Symbol
     name of variable to use in constructing a ring PP if none is given
   Outputs
    J:Ideal
     multi-graded ideal of the image
   Description
    Example
     S = ZZ/101[a,b]
     I = ideal 0_S
     f0 = matrix"a,b"
     f1 = matrix"a,b"
     maps = {f0,f1}
     schemeInProduct(I, maps, Y) 
   SeeAlso
    productOfProjectiveSpaces
///

doc ///
   Key
    relativeEquations
    (relativeEquations, ZZ,ZZ,ZZ)    
   Headline
    compute the relative quadrics 
   Usage
    I = relativeEquations(a,b,k)
   Inputs
    a:ZZ
    b:ZZ
    k:ZZ
   Outputs
    I:Ideal
     multi-graded ideal in the Cox ring
   Description
    Text
      We compute the relative equations of a resonance degenerate K3 in case of k resonance. The first step consists
      in chooses a prime field which has a k-th root of unity. We then follow section 4 of the paper equations and syzygies of
      K3 carpets and unions of scrolls.
    Example
     I = relativeEquations(4,4,3)
     betti I
   SeeAlso
    coxMatrices
    resonanceScroll
    relativeResolution
///

doc ///
   Key
    relativeResolution
    (relativeResolution, ZZ,ZZ,ZZ)    
   Headline
    compute the relative resolution 
   Usage
    F = relativeResolution(a,b,k)
   Inputs
    a:ZZ
    b:ZZ
    k:ZZ
   Outputs
    F:ChainComplex
     multi-graded chainComplex in the Cox ring
   Description
    Text
      We compute the relative resolution of a resonance degenerate K3 in case of k resonance. The first step consists
      in chooses a prime field which has a k-th root of unity. We then follow section 4 of the paper equations and syzygies of
      K3 carpets and unions of scrolls.
    Example
     F = relativeResolution(5,4,3)
     betti F
   SeeAlso
    relativeResolutionTwists
    relativeEquations
    coxMatrices
///

doc ///
   Key
    coxMatrices
    (coxMatrices, ZZ,ZZ,ZZ)    
   Headline
    compute the Cox matrices 
   Usage
    (A,B,A1,B1) = coxMatrices(a,b,k)
   Inputs
    a:ZZ
    b:ZZ
    k:ZZ
   Outputs
    A:Matrix
    B:Matrix
    A1:Matrix
    B1:Matrix
     over the Cox ring
   Description
    Text 
     We compute the Hankel matrices over the Cox ring, which give
     rize to the relative quadrics of a degenerate K3 X_e(a,b) in case of k resonance
     within the resonance scroll, see ES2018    
    Example
     (A,B,A1,B1)=coxMatrices(6,5,4);
     A,A1
     B,B1
     (A,B,A1,B1)=coxMatrices(7,4,4);
     A,A1
     B,B1     
   SeeAlso
    relativeEquations
///

doc ///
   Key
    relativeResolutionTwists
    (relativeResolutionTwists,ZZ,ZZ,ChainComplex)    
   Headline
    compute the twists in the relative resolution 
   Usage
    L = relativeResolutionTwists(am,bm,F)
   Inputs
    am:ZZ
    bm:ZZ
    F:ChainComplex
   Outputs
    L:List
     of Lists of multidegrees
   Description
    Text 
     We compute the twists of the relative resolution in the resonance scroll of a degenerate K3 
     X_e(a,b) in case of k resonance after re-embedding the resonace scroll with |H+jR|
     for j=am-a_0=bm-b_0 where $\{a_i\}|\{b_j\}$ is the splitting type of the resonance scroll. 
    Example
     F = relativeResolution(5,4,3);
     (as,bs)=resonanceScroll(5,4,3)
     betti F
     L = relativeResolutionTwists(as_0,bs_0,F);
     netList apply(L,c-> tally c)
     L = relativeResolutionTwists(as_0+2,bs_0+1,F);
     netList apply(L,c-> tally c)
   SeeAlso
    relativeResolution
    resonanceScroll
///

doc ///
   Key
    resonanceScroll
    (resonanceScroll,ZZ,ZZ,ZZ)    
   Headline
    compute the splitting type of the resonance scroll  
   Usage
    (as,bs) = resonanceScroll(a,b,k)
   Inputs
    a:ZZ
    b:ZZ
     type of the degenerate K3
    k:ZZ
     the resonace
   Outputs
    as:List
    bs:List
     the splitting type of the resonance scroll
   Description
    Text 
      We compute the splitting type of the resonance scroll of a degenerate K3 X_e(a,b) in case of k resonance. 
    Example
     (as,bs)=resonanceScroll(6,4,3)
   SeeAlso
    relativeEquations
///

doc ///
   Key
    computeBound
    (computeBound,ZZ,ZZ,ZZ)
     (computeBound,ZZ)    
   Headline
    compute the bound for the good types in case of k resonance    
   Usage
    (a,b) = computeBound(a1,b1,k)
    c = computeBound k
   Inputs
    a1:ZZ
    b1:ZZ
     type of the degenerate K3
    k:ZZ
     the resonance
   Outputs
    a:ZZ
    b:ZZ
     minimal good type
    c:ZZ
     lower bound for good a,b
   Description
    Text
      The iterated mapping over the relative resolution of X_e(a,b) in the resonance scroll 
      has betti numbers in a range of a general 2k-gonal canonical curve of genus a+b+1, 
      if a,b are large enough, see ES2018.
      We compute the minimal type $(a,b) \equiv (a1,b1) \mod  k$ where this becomes true.
      
      In the second version c is the minimal value of a,b's for all congruence classes mod k.
      We conjecture that c=k^2-k. 
    Example
     (a,b)=computeBound(6,4,3)
     computeBound 3
   SeeAlso
    relativeEquations
///


TEST ///
 I =carpet(6,6);
 F = res(I, FastNonminimal =>true)
L=analyzeStrand(F,6)
tally L
factor product L
a=6,b=6
-- this test runs out of memory on habanero regularly:
-- h=carpetBettiTables(a,b)
-- carpetBettiTable(h,7)
-- carpetBettiTable(h,5)
///

TEST ///
(a,b)=(5,4)
 I =carpet(6,6);
 F = res(I, FastNonminimal =>true)
betti F
S=ring I
vars S
schreyerName(F,5,10)
///


 

TEST///
assert(false == (betti res canonicalCarpet(7,3,Characteristic =>0) == 
	        betti res canonicalCarpet(7,3,Characteristic =>2)))
assert(isHomogeneous carpet(2,4,FineGrading=>true))
assert(isHomogeneous canonicalCarpet(7,3,FineGrading=>true))
///

TEST ///
(a,b)=(5,5)
e=(-1,11)
h=degenerateK3BettiTables(a,b,e)
zBT=h#0 - minimalBetti degenerateK3(a,b,e)
assert(sum(keys zBT,k->zBT#k)==0)
///



end--




-*    
lineCorrespondence(ZZ,ZZ,ZZ,ZZ) := (a,b,p,q)->(
    if p>a or (b==1 and q !=2) then error"impossible size for Hankel matrix";
    if a <2 then error"a is too small";
    x := symbol x;
    y := symbol y;
    S := ZZ/101[x_0..x_a,y_0..y_b];
    mx := hankelMatrix(S,x_0,p+1,a-p+1);
    if b>1 then
    my := hankelMatrix(S,y_0,q+1,b-q+1) else
    my = matrix{{y_0^2},{y_0*y_1},{y_1^2}};
    if b>1 then
    t := random(S^(p+1),S^(q+1)) else
    t = random(S^(p+1),S^(3));
    J := ideal ((transpose mx)*t*my);
    Ix := minors(2,hankelMatrix(S,x_0,2,a));
    Iy := minors(2,hankelMatrix(S,y_0,2,b));
    I := Ix+Iy+J;
    zI := I+ideal(x_0,y_b,x_a-y_0);
    (I,zI))
*-

-*
lineCorrespondence(List,List) := (scroll,correspondence) ->(
    if any (correspondence,i->i<1) then 
          error "correspondence degrees must be at least 1";
    if any (#correspondence,i->scroll_i<correspondence_i) then 
          error "correspondence degree > scrolltype not implemented yet";
    --what should one do in the case one of the scroll sizes is < the correspondence size?
    --when the scroll size is 1, it's easy -- just take the corresponding secant matrix to
    --be a column using the correspondence-size monomials. But in general??
    n := #scroll; -- dimension of the scroll -- note that in this contruction the line corr is bigger
    p := reverse sort apply(n, i->{scroll_i,correspondence_i}); -- put biggest scrolls first
    sscroll := p/(i->i_0); -- sorted scroll sizes
    scorr :=  p/(i->i_1); -- sorted correspondence degrees
    x := symbol x;
    S := ZZ/32003[x_0..x_(n-1+sum scroll)];
    scrollMatrices := {hankelMatrix(S,x_0,2,sscroll_0)};
    scrollMatrices = scrollMatrices| apply(n-1,i->
	(hankelMatrix(S,x_(sum(sscroll_{0..i})+i+1),2,sscroll_(i+1))));
    curveIdeals := apply(n,i->minors(2,scrollMatrices_i));
    joinIdeal := sum curveIdeals;
    tmats := 
	{id_(S^(1+scorr_0))} | apply(n-1,i->random(S^(1+scorr_(i+1)), S^(1+scorr_0)));
    tinverses := apply(tmats, m ->m^(-1));
    secants := {hankelMatrix(S,x_0,1+scorr_0,1+sscroll_0-scorr_0)};
    secants = secants| apply(n-1,i->
	(hankelMatrix(S,x_(sum(sscroll_{0..i})+i+1),
		      1+scorr_(i+1),1+sscroll_(i+1)-scorr_(i+1))));
    pa := flatten apply(n-1, i-> apply(n-i-1, j->{i,i+j+1}));
--    J := sum toList apply(toList(1..n-1), i -> ideal(
--	    transpose secants_i*tmats_i*secants_0));
    --with all the scrolls in correspondence with the first, as above, the elements
    --we added to make zI were not a regular sequence. Maybe there's there's a 
    --version of the ideal for each tree different on n vertices, and a different
    --regular sequence needed for each tree.
    J := sum apply(toList(1..n-1), i -> ideal(
	    transpose secants_i*random(S^(1+scorr_i), S^(1+scorr_(i-1)))*secants_(i-1)));
    I := joinIdeal+J;
    Iplus := I+ideal(x_0, x_(n-1+sum scroll) + 
	  ideal apply(n-1, i-> x_(sum(sscroll_{0..i})+i) - x_(sum(sscroll_{0..i})+i+1)));
    zI := ideal presentation  minimalPresentation ((ring Iplus)/Iplus); 
    -- zI is the image of Iplus mod the linear forms
    (I,zI))

*-
-*
degree (I=(lineCorrespondence({1,1,1},{1,1,1}))_0)
dim I
codim I
betti res I
*-

-*
lineCorrespondence1=method()
lineCorrespondence1(List,List) := (scroll,correspondence) ->(
    --scroll is a list of rational normal curve degrees
    --correspondence is a list of correspondence degrees
    --uses generic square scalar matrices for the correspondences.
    --in this version we put in correspondence data for every pair. thus the largest term
    --in the correspondence list must be <= the smallest term in the scroll list.
    --What should one do in the case one of the scroll sizes is < the correspondence size?
    --When the scroll size is 1, it's easy -- just take the corresponding secant matrix to
    --be a column using the correspondence-size monomials. But in general??

    n := #scroll; -- dimension of the scroll, and of the line correspondence
    p := reverse sort apply(n, i->{scroll_i,correspondence_i}); -- put biggest scrolls first
    sscroll := p/(i->i_0); -- sorted scroll sizes
    scorr :=  p/(i->i_1); -- sorted correspondence degrees
    if sscroll_(n-1) < min scorr then error "some scroll is < some correspondence size; not implemented";
--
    x := symbol x;
    S := ZZ/32003[x_0..x_(n-1+sum scroll)];
    scrollMatrices := {hankelMatrix(S,x_0,2,sscroll_0)};
    scrollMatrices = scrollMatrices| apply(n-1,i->
	(hankelMatrix(S,x_(sum(sscroll_{0..i})+i+1),2,sscroll_(i+1))));
    curveIdeals := apply(n,i->minors(2,scrollMatrices_i));
    joinIdeal := sum curveIdeals;
    tmats := 
	{id_(S^(1+scorr_0))} | apply(n-1,i->random(S^(1+scorr_(i+1)), S^(1+scorr_0)));
    tinverses := apply(tmats, m ->transpose m);
    secants := {hankelMatrix(S,x_0,1+scorr_0,1+sscroll_0-scorr_0)};
    secants = secants| apply(n-1,i->
	(hankelMatrix(S,x_(sum(sscroll_{0..i})+i+1),
		      1+scorr_(i+1),1+sscroll_(i+1)-scorr_(i+1))));
    Jlistlist := apply(toList(0..n-1), i -> 
	              apply(toList(i+1..n-1), j-> ideal(
	  (transpose secants_i)*tmats_i*tinverses_j*secants_j)));
    J := sum flatten Jlistlist;
    --with all the scrolls in correspondence with the first, as above, the elements
    --we added to make zI were not a regular sequence. Maybe there's there's a 
    --version of the ideal for each tree different on n vertices, and a different
    --regular sequence needed for each tree.
    I := joinIdeal+J;
    Iplus := I+ideal(x_0, x_(n-1+sum scroll) + 
	  ideal apply(n-1, i-> x_(sum(sscroll_{0..i})+i) - x_(sum(sscroll_{0..i})+i+1)));
    zI := ideal presentation  minimalPresentation ((ring Iplus)/Iplus); 
    -- zI is the image of Iplus mod the linear forms
    error();
    (I,zI))
*-
restart
loadPackage("K3Carpets", Reload =>true)
(I,Iz) = lineCorrespondence1({1,1,1},{1,1,1});
Jlistlist
betti res I
degree I
curveIdeals

doc ///
   Key
    lineCorrespondence
    (lineCorrespondence,Ring,ZZ,ZZ,Matrix)
    (lineCorrespondence,Ring,ZZ,ZZ,ZZ)
   Headline
    Forms the union of lines in a correspondence between two rational normal curves
   Usage
    I = lineCorrespondence(kk,a,b,E)
    I = lineCorrespondence(a,b,p,q)    
   Inputs
    kk:Ring
     base field
    a:ZZ
     degree of first rational normal curve
    b:ZZ
     degree of second rational normal curve    
    E:Matrix
     pxq matrix of ZZ or kk representing a p,q correspondence in P^1 x P^1.
   Outputs
    I:Ideal
     ideal of the union of lines in the correspondence
   Description
    Text
    Code
    Pre
    Example
    CannedExample
   Subnodes
   Caveat
   SeeAlso
///
///
restart
loadPackage("K3Carpets", Reload =>true)
kk = ZZ/32003
E = diagonalMatrix{1,-2,1}
E = diagonalMatrix{3,5,17}
E = random(ZZ^3,ZZ^3)
E = diagonalMatrix{0,1,0}
E = diagonalMatrix{0,1,0}
E = random(kk^2,kk^3)
E = diagonalMatrix{1,2,3,4}
E = diagonalMatrix{1,0,0,0}
E = matrix"0,0,1;0,0,0;0,0,0"
E = matrix"0,0,0;0,0,0;0,0,0"
--all nonzero 3x3 matrices seem to give a Gorenstein
I = lineCorrespondence (kk,3,2,E)
betti res I
degree I
#decompose I
-- suppose E is 3x3, corresponding to a 2-2 correspondence, and thus
--to the biquadratic form in (s,t), (u,v) (transpose (s^2,st,t^2))*E*(u^2,uv,v^2).
--is the subset of P1 x P1 defined by the correspondence reducible? == 2 singular points.
--this must for some reason be true of the diagonal ones. But the general ones??
--identifying P1xP1 with the image of 
--matrix{{su,sv},{tu,tv}} = matrix{{z_0,z_1},{z_2,z_3}}
--we can write the biquadratic form as^2u^2+bstuv+ct^2v^2
kk = ZZ/32003
T = kk[z_0..z_3]
Q0 = ideal(z_0*z_3-z_1*z_2)
Q1 = ideal((random kk)*z_0^2+random(kk)*z_1*z_2+z_3^2)
J = Q0+Q1
dim singularLocus(T/J)
degree singularLocus(T/J) -- 2 sing points

///
restart
loadPackage"K3Carpets"
(a,b)=(6,6)
e=(-1,11)
h=degenerateK3BettiTables(a,b,e) -- gives a wrong answer
keys h -- is wrong
///
-*
The following code should replace the code in the package, as soon as
res(.,FastNonminimal=>true) allows QQ as a coefficient field
*-
degenerateK3BettiTables=method()
degenerateK3BettiTables(ZZ,ZZ,Sequence) := (a,b,e) -> (
    I := degenerateK3(a,b,e);
    S := QQ[gens ring I];
    I = sub(I,S);
    F := res(I,FastNonminimal=>true);
    carpetData1 := {};carpetData2 := {};
    L:= null;primes:= {};
    for c from 3 to length F-2 do (    
	L = analyzeStrand(F,c);
	carpetData2 = append(carpetData2,(c=>#L));
	if (product L != 1 ) then (
	    d := factor product L;
            primes = apply(#d,i->d#i#0)) else primes={};      
        primes = apply(primes,p->(p,#select(L,t->t%p==0)));
       	carpetData1 = append(carpetData1,(c=>{d,primes}));
	); 
    cD1 := hashTable carpetData1;
    cD2 := hashTable carpetData2;
    T:= betti F;
    T1 := new BettiTally from {(0,{0},0)=>1}|apply(
	    toList(1..length F),i->( (i,{i+1},i+1) => if i>=length F-1 then 0 else (
		    (T#(i,{i+1},i+1)- if i>2 and i<length F-1 then cD2#i else 0 ))));
    T2 := T1+((dual T1)[-length F])(-length F-3);
    exceptionalPrimes := unique flatten apply(values cD1,dc->apply(dc_1,j->j_0));
    Ts := new HashTable from apply(exceptionalPrimes,p-> p=> new BettiTally from (
	    apply(keys cD2,c->( cor:=select(cD1#c_1,j->j_0==p); 
		    if #cor>0 then (c,{c+1},c+1) => cor_0_1 ))
	    ));    
    new HashTable from {0 => T2}|apply(exceptionalPrimes,p-> (
	     p => (T1p:=T1+Ts#p;T1p+((dual T1p)[-length F])(-length F-3))))
     )

-----------
--hypersurface correspondence scrolls
restart
loadPackage("K3Carpets", Reload=>true)
scroll = {2,2}
deg = {2,2}
deg = {7,7}

hypersurfaceCorrespondence = method()
hypersurfaceCorrespondence (List, List, List) := (proj,embedding,deg) ->(
P := productOfProjectiveSpaces(proj,symbol X,ZZ/32003);
f := ideal(random(P^1,P^{-deg}));
correspondenceScroll(f,embedding)
)

I = hypersurfaceCorrespondence({2},{3},{5})
betti res I


testbetti = (scroll,deg) ->(
P := productOfProjectiveSpaces({1,1},symbol X,ZZ/32003);
f := ideal(random(P^1,P^{-deg}));
I := correspondenceScroll(f,scroll);
minimalBetti I
)

for a from 3 to 6 do(
    for m from 0 to 4 do
        (b = 2+m*a;
	print (a,b,testbetti({a,a},{b,b}))
	)
)
Gorenstein for a,b = 
2,2
2,4
2,6
2,8 

3,2
3,5
3,8
3,11

4,2
4,6
4,10

a,2+m*a

testbetti({2,2},{8,8})
testbetti({3,3},{5,5})
testbetti({3,3},{8,8})
 
----
kk = ZZ/32003
P = kk[a..f]
M1 = genericSymmetricMatrix(P,a,3)
M2 = random(P^4, P^{4:-1})


M = mutableMatrix M2

for i from 0 to 2  do 
    for j from 0 to 2 do 
	M_(i,j) = M1_(i,j)
M_(0,3) = 0_P
M
for j from 0 to 3 do M_(3,j) = M_(j,3)
M_(3,3) = 0_P

m = matrix M
m-transpose m
I = minors(2,m)
codim I
degree I
m
