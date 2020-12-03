
newPackage(
	"Triplets",
   	Version => "0.1", 
   	Date => "January 24, 2013",
	Authors => {
	     {Name => "Gunnar Floystad", Email => "nmagf@uib.no"}
	      },
     	PackageImports => {"BoijSoederberg"},
   	Headline => "triplets of degree sequences and associated Betti diagrams and cohomology tables",
	Keywords => {"Commutative Algebra"},
   	DebuggingMode => false
   	)
export {
     "Triplet",
     "strands",
     "strandsL",
     "conj",
     --
     "triplet",
     "rotForw",
     "rotBack",
     "toHomology",
     "toDegree",
     "dualHomTriplet",
     "type",     
     --
     "isDegreeTriplet",
     "isHomologyTriplet",
     --
     "Betti1",
     "Betti3",
     "BettiDiagram1",
     "BettiDiagram3",
     --     
     "binPol",
     "hilbCoeff", 
     "hilbPol",
     "chiPol",
     --
     "cohMatrix",
     "cohTable"}

----------------------------
--New type:Triplet
---------------------------

Triplet = new Type of List;


-----------------------------
--Elementary routines on degree sequences
-----------------------------

--the number of strands
strands=method(TypicalValue => ZZ)     
strands(List) := (A) -> (
        IntA := toList apply(first A..last A, i->i);
        NdA := IntA - set(A); 
	return #NdA);
 
TEST ///
L = {0,1,3,6,7}
s = strands(L)
assert(s == 3)  
/// 

--the number of strands considered in the interval [0,n]
strandsL=method(TypicalValue => ZZ)
strandsL(ZZ,List) := (n,A) -> (
     Int := apply(n+1, i-> i);
     LNda := Int - set(A);
     return #LNda);
TEST ///
L = {0,1,3,6,7}
n = 9
s = strandsL(9,L)
assert(s == 5)
///

--makes the conjugate of a degree sequence
conj=method(TypicalValue => List)
conj(List,ZZ) := (X,n) -> (
     nX := #X -1;
     Xb := apply(nX+1, i -> n-X_(nX-i));
     return Xb);

TEST///
L = {0,1,3,6,7}
n = 9
Lc = conj(L,9)
assert(Lc == {2,3,6,8,9})
///


------------------------------
--Elementary routines for manipulating triplets
------------------------------

--make a triplet

triplet=method(TypicalValue => Triplet)
triplet(List,List,List) := Triplet => (A,B,C) -> (
     L := {A,B,C};
     new Triplet from L);


--finds the number of squarefree variables associated to a degree triplet
type=method(TypicalValue => ZZ)
type(Triplet) := (L) -> (
     n := first L#2 + last L#0;
     return n);

TEST ///
T = triplet ({1,2,4,6}, {3,4,5}, {0,1,2,3})
t = type T
assert (t == 6)
///

--permutes a triplet cyclically
rotForw=method(TypicalValue => Triplet)
rotForw(Triplet) := (L) -> (
     return rotate(1,L));

TEST ///
T = triplet({1,2,4,6}, {3,4,5}, {0,1,2,3})
Tf = rotForw T
T2 = triplet({3,4,5}, {0,1,2,3}, {1,2,4,6})
assert (Tf == T2)
///

rotBack=method(TypicalValue => Triplet)
rotBack(Triplet) := (L) -> (
     return rotate(2,L));

TEST ///
T = triplet({1,2,4,6}, {3,4,5}, {0,1,2,3})
Tf = rotBack T
T2 = triplet({0,1,2,3}, {1,2,4,6}, {3,4,5})
assert (Tf == T2)
///


--converts a degree triplet to a homology triplet
toHomology=method(TypicalValue => Triplet)
toHomology(Triplet) := (L) -> (
     --if #L != 3 then error "triplet list should contain three degree sequences";
     if not isDegreeTriplet(L) then error "this is not a degree triplet";
     n := type(L);
     A := L#0;
     B := L#1;
     H := conj(B,n);     
     C := L#2;
     Lnew := {A,H,C};
     new Triplet from Lnew);

TEST ///
T = triplet({1,2,4,6}, {3,4,5}, {0,1,2,3})
Th = toHomology T
T2 = triplet({1,2,4,6}, {1,2,3}, {0,1,2,3})
assert (T2 == Th)
///

--converts a homology triplet to a degree triplet
toDegree=method(TypicalValue => Triplet)
toDegree(Triplet) := (L) ->  (
    --if not isHomologyTriplet(L) then error "this is not a homology triplet";
     n := type(L);
     A := L#0;
     B := L#1;
     B = conj(B,n);     
     C := L#2;
     Lnew := {A,B,C};
     new Triplet from Lnew); 

TEST ///
T = triplet({1,2,4,6}, {1,2,3}, {0,1,2,3})
Th = toDegree T
T2 = triplet({1,2,4,6}, {3,4,5}, {0,1,2,3})
assert (T2 == Th)
///


dualHomTriplet=method()
dualHomTriplet(Triplet) := (L) -> (
     n := type(L);
     Lnew := {conj(L#0,n), L#2, L#1};
     new Triplet from Lnew);

TEST ///
T = triplet({1,2,4,6}, {1,2,3}, {0,1,2,3})
Td = dualHomTriplet T
T2 = triplet({0,2,4,5}, {0,1,2,3}, {1,2,3})
assert (T2 == Td)
///


------------------------------
--Check: Do they fulfill the conditions for triplets?
------------------------------

--is this a degree triplet?
isDegreeTriplet=method(TypicalValue=>Boolean)
isDegreeTriplet(Triplet) := (L) ->(
     if #L != 3 then {print "there should be three degree sequences"; return false};
     A := L#0;
     B := L#1;
     C := L#2;
     nA:=#A-1;
     nB:=#B-1;
     nC:=#C-1;
     if (tally apply(nA,i->A_i<A_(i+1)))_(false) >0 then 
                                   {print "first sequence is not strictly increasing"; return false};
     if (tally apply(nB,i->B_i<B_(i+1)))_(false) >0 then 
                                  {print "second sequence is not strictly increasing"; return false};
     if (tally apply(nC,i->C_i<C_(i+1)))_(false) >0 then 
                                  {print "third sequence is not strictly increasing"; return false};
				  
 
     c:=A_0;a:=B_0;b:=C_0;
     nb:=A_nA; nc:=B_nB; na:=C_nC;
     n := type(L);
     

     if not ((nc + c) == n and (na + a) == n and (nb + b) == n) then 
     {print "summing the start degree of a degree sequence and the end degree of the following,
	  they are not all equal"; return false};
	  
	  
     
     
     if c<0 or nb > n then {print "first is not in the range 0 to n"; return false};
     if a<0 or nc > n then {print "second is not in the range 0 to n"; return false};
     if b<0 or na > n then {print "third is  not in the range 0 to n"; return false};
     
    
     eA:= n-b-c-nA; eB:= n-a-c-nB; eC:= n-a-b-nC;
     
     if n > a+b+c + eA + eB + eC then {print "too few interior nondegrees"; return false}; 
     if n < a+b+c + eA + eB + eC then {print "too many interior nondegrees"; return false};
     
     p := c;
     while p <= n  do (
     nAcp := (tally apply(nA+1, i->(A_i <= p)))_true,   
     nBcp := (tally apply(nB+1, i->(B_i >= n-p)))_true;
     if nAcp + nBcp <= p-c + 1 then 
      {print "first and second are underbalanced"; return false};
      p = p+1);

    p = a;
     while p <= n do (
     nBap := (tally apply(nB+1, i-> (B_i <= p)))_true;   
     nCap := (tally apply(nC+1, i-> (C_i >= n-p)))_true;
     if nBap + nCap <= p-a + 1 then {print "second and third are underbalanced"; return false};
     p = p+1);
   
    p = b;
     while p <= n  do (
     nCbp := (tally apply(nC+1, i-> (C_i <= p)))_true;   
     nAbp := (tally apply(nA+1, i-> (A_i >= n-p)))_true;
     if nAbp + nCbp <= p-b + 1 then {print "third and first are underbalanced"; return false};
     p= p+1);

     -- if not C_nC==(n-c) then return "false";
     -- check number non degrees
     -- check balancing condition
     return true
);

TEST ///
T = triplet({0,1,3,5,6}, {1,2,4,5,6,8}, {2,3,4,5,7})
assert(isDegreeTriplet T == true)
D = triplet({0,3,6}, {1,2,3,5,6}, {0,1,2,4,5} )
assert(isDegreeTriplet D == false)
E = triplet({0,1,3,5,7}, {1,2,4,5,6,8}, {2,3,4,5,7})
assert(isDegreeTriplet E == false)
///   
     
--is this a homology triplet?
isHomologyTriplet=method(TypicalValue=>Boolean)
isHomologyTriplet(Triplet) := (L) ->(
     Lnew := toDegree(L);
     if isDegreeTriplet(Lnew) then return true else {print "This is not a homology triplet.
     Convert to a degree triplet and try isDegreeTriplet to get more info."; return false}
     ); 
   
 TEST ///
T = triplet({0,1,3,5,6}, {0,2,3,4,6,7}, {2,3,4,5,7})
assert(isHomologyTriplet T == true)
D = triplet({0,3,6}, {0,1,3,4,5}, {0,1,2,4,5} )
assert(isHomologyTriplet D == false)
E = triplet({0,1,3,5,7}, {0,2,3,4,6,7}, {2,3,4,5,7})
assert(isHomologyTriplet E == false)
///    
    
     
-------------------------------
--Compute the Betti numbers of triplets of pure free squarefree complexes
------------------------------

--computes the Betti numbers of the first degree sequence of the triplet
Betti1=method(TypicalValue=>List)
Betti1(Triplet) := (L) -> (
     A := L#0;
     B := L#1;
     C := L#2;
     R := ZZ;
     nA:=#A-1;
     nB:=#B-1;
     nC:=#C-1;
     a := B_0;
     b := C_0;
     c := A_0;
     n := A_0 + B_nB;
     --make the conugate set of C
     BarC := apply(nC+1, i -> n- C_(nC-i));     
     -- make the interval [a,Barb] containing BarC
     IntBarC := apply(n-b-a+1, i-> i+a);
     --make the iternal  nondegrees of BarC
     SBarC := set(BarC);
     INdBarC := IntBarC - SBarC;
     nINdC := #INdBarC-1;
     
     --make the interval [a,Barc] containing B
     IntB := apply(n-a-c, i -> i + a);
     --make the internal nondegrees of B
     SetB := set(B);
     INdB := IntB - SetB;
     nINdB := #INdB -1;
     
     --make the matrix of relations for the Betti numbers
     r := #A-1; -- print r; print nINdC; print nINdB; print a;
     if nINdC >= 0 
        then (A1mat := map(R^(nINdC+1), R^(r+1), (i,j) -> binomial(A_j,INdBarC_i)))
        else (A1mat = map(R^0, R^(r+1), (i,j) -> 1));          
     if nINdB >= 0 
        then A2mat := map(R^(nINdB+1), R^(r+1), (i,j) -> binomial(n-A_j,INdB_i))
        else (A2mat= map(R^0, R^(r+1), (i,j) -> 1));      
      A3mat := map(R^(a), R^(r+1), (i,j) -> binomial(n-A_j,i));
     --print A3mat;
     Amat := map(R^1, R^(r+1), (i,j) -> 0);
     Amat = Amat || A1mat;
     Amat = Amat || A2mat;
     --print Amat;
     Amat = Amat || A3mat;
     --print Amat;
     Amat = submatrix'(Amat,{0},);
     --print Amat;
     B = ker Amat;
     --print B;
     B = gens B;
     --print "dette er B";
     --print B;
     B = flatten B;
     BL := apply(r+1, i -> (if B_(0,i) >= 0 then B_(0,i) else -B_(0,i)));
     return BL);

TEST ///
T = triplet({0,1,3,5,6}, {1,2,4,5,6,8}, {2,3,4,5,7})
assert(Betti1(T) == {38, 304, 1015, 1638, 889})
Tb = rotBack T
assert(Betti1(Tb) == {889, 3696, 5145, 2415, 115})
///

--Prints the Betti numbers of the three degree sequences
Betti3=method()
Betti3(Triplet):= (L) -> ( 
<< L#0 << "   " << "===>" << "   " << Betti1(L) << endl
<< L#1 << "   " << "===>" << "   " << Betti1(rotForw(L)) << endl
<< L#2 << "   " << "===>" << "   " << Betti1(rotBack(L)) << endl;
);


--Prints the Betti diagram of the first degree sequence
BettiDiagram1 = method()
BettiDiagram1(Triplet) := (L) -> (
     A := L#0;
     BettiA := Betti1(L);
     s := strands(A);
     a := flatten apply(s+1, i -> 
	  apply(#A, j -> (j, {i+j+A#0}, i+j+A#0) => if (i == A#j-A#0-j)  then BettiA#j else null));
     a = select(a, b -> b#1 =!= null);
     new BettiTally from a
     );

--Prints the Betti diagram of all the three degree sequences
BettiDiagram3=method()
BettiDiagram3(Triplet) := (L) -> (
     << BettiDiagram1(L) << "     " << BettiDiagram1(rotForw(L)) << "     " << BettiDiagram1(rotBack(L)) << endl;
     );
-------------------------------------
--Routines used in computation of cohomology table. Mostly computations
--of Hilbert polynomials
------------------------------------

--computes polynomial vanishing on [0,n] except at i, where
--takes value (-1)^i
binPol=method() --TypicalValue => QQ(monoid[d]))
binPol(RingElement,ZZ,ZZ):= (d,n,i) -> (
     if not ((0 <= i) and (i <= n)) then error "second polynomial parameter out of
     range";
     if i > 0 then t1 := product(i, j -> d+j) else t1 = 1;
     if i > 0 then n1 := product(i, j -> j+1) else n1 = 1;
     p1 := t1//n1; --print p1;
     if i <n then t2 := product(n-i, j -> j+i+1+d) else t2 = 1;
     if i <n then n2 := product(n-i, j -> j+1) else n2 = 1;
     p2 := t2//n2; --print p2;
     --print p1; --print p2;
     p := p1*p2;
     --if odd i then pprod = -p1*p2 else pprod = p1*p2;  --print p;
     --q := p - 17*p1;
     --print q;
     return p);

TEST ///
QQ[d]
binPol(d,6,2)
assert(48*binPol(d,6,2) ==  (d^6 + 19*d^5 + 137*d^4 + 461*d^3 + 702*d^2 + 360*d))
///

--computes the coefficients of the presentations of the Hilbert polynomial
hilbCoeff=method(TypicalValue => List)
hilbCoeff(Triplet) := (Lh) -> (
     n := type Lh;
     B := Lh#0; H := Lh#1; C := Lh#2;
     R := ZZ;
     nB := #B - 1;
     nH := #H - 1;
     nC := #C - 1;
     b := H_nH; 
     if not (b == C_nC) then error "homology and cohomology do not have same end";
     IntH := toList apply(H_0..H_nH, i -> i); 
     --print IntH;
     NdH := IntH - set(H);
     nNdH := #NdH-1;
     IntC := toList apply(C_0..C_nC, i -> i); 
     NdC := IntC - set(C);
     nNdC := #NdC-1; 
     --print nNdH; print nB; print B_0;
     if nNdH >= 0 
        then (A1mat := map(R^(nNdH+1), R^(nB+1), 
		  (i,j) -> binomial(NdH_i,B_j)))
        else (A1mat = map(R^0, R^(nB+1), (i,j) -> 1));         
     if nNdC >= 0
        then (A2mat := map(R^(nNdC+1), R^(nB+1), 
		  (i,j) -> binomial(NdC_i,n-B_j)))
        else (A2mat = map(R^0, R^(nB+1), (i,j) -> 1));   
     A3mat := map(R^(n-b), R^(nB+1), (i,j) -> binomial(i+b+1,B_j));
     --print A3mat;
     Amat := map(R^1, R^(nB+1), (i,j) -> 0);
     Amat = Amat || A1mat;
     Amat = Amat || A2mat;
     --print Amat;
     Amat = Amat || A3mat;
     --print Amat;
     Amat = submatrix'(Amat,{0},);
     --print Amat;
     K := ker Amat;
     --print B;
     K = gens K;
     --print (class K);
     if K_(0,0) < 0 then K = (-1)*K;
     --print "dette er B";
     --print B;
     K = flatten K; 
     --print nB; print K;
     L := apply(nB+1, i -> K_(0,i));
     return L);

TEST ///
T = triplet({0,3,4}, {0,1,2,3,5}, {1,3,4,5})
isHomologyTriplet T
hilbCoeff T
assert(hilbCoeff T == {7,-2,1})
T = triplet({0,1,3,5,6}, {0,2,3,4,6,7}, {2,3,4,5,7})
hilbCoeff T
assert(hilbCoeff T == {304, -304, 145, -234, 254})
///

--computes the Hilbert polynomial of associated complex of coherent sheaves
hilbPol=method()--TypicalValue => QQ[d])
hilbPol(RingElement,ZZ,List,List) := (d,n,B,K) -> (
   
     if not (#B == #K) then 
     error "the Bettidegree list and coefficient list should have the same length";
     P := sum(#B, i -> K_i*binPol(d,n,B_i));
     --for i from 0 to #B - 1 list i do (A := (K_i*binPol(n,B_i)); P := P + A);
     return P);

TEST ///
QQ[d]
T = triplet({0,3,4}, {0,2,3,4,5}, {1,3,4,5} )
K = hilbCoeff T
n = type T
assert(24*hilbPol(d,n,T#0,K) == 3*d^5 + 37*d^4 + 155*d^3 + 251*d^2+ 130*d)
/// 

--computes the Hilbert polynomials of the cohomology sheaves of the
--associated complex of coherent sheaves
chiPol=method() --TypicalValue => QQ[d])
chiPol(RingElement,ZZ,List,List) := (d,p,L,K) -> (
     B := L#0;
     H := L#1;
     if p < 0 or p > strands(H) then error "integer should be in the strand range";
     IntH := toList apply((first H)..last H + 1, i->i);
     NdH := IntH - set(H);
     if p < strands(H) then h2 := NdH_(p) -1 else h2 = last H;
   
     Bhp2 := {}; 
    
     for i from 0 to #B-1 do if (B_i <= h2) then Bhp2 = append(Bhp2,B_i) else
     continue; 

     P := sum(#Bhp2, i -> K_i*binPol(d,h2,B_i));
 
     if (p > 0) then h1 := NdH_(p-1) -1 else h1 = first H - 1;
     Bhp1 := {};
     for i from 0 to #B-1 do if B_i <= h1 then Bhp1 = append(Bhp1,B_i); 
     Q := sum(#Bhp1, i -> K_i*binPol(d,h1,B_i));
     if (even p) then sign := 1 else sign = -1;
     return sign*(P - Q));

TEST ///
QQ[d]
T = triplet({0,3,4}, {0,2,3,4,5}, {1,3,4,5} )
Th = toHomology T
K = hilbCoeff Th
assert(6*chiPol(d,0,{Th#0,Th#1},K) == 5*d^3 + 36*d^2 + 73*d + 42) 
assert(15*chiPol(d,1,{Th#0,Th#1},K) == d^5 + 10*d^4 + 35*d^3 + 50*d^2 + 24*d)
///

-----------------------------------------
--Computes the hypercohomology tables of the complex of coherent sheaves
-----------------------------------------

--this is the hypercohomlogy table in matrix form
cohMatrix=method(TypicalValue => MutableMatrix)
cohMatrix(ZZ,ZZ,Triplet) := (lo,hi,Lh) -> (
     B := Lh#0; H := Lh#1; C := Lh#2;
     n := type(Lh);
     sH := strands(H);
     sB := strandsL(n,B);
     sC := strands(C);
     s := sH + sB + sC;
     dec := sB + sC;
     vec := -lo;
     S := QQ[vars{1}];
     d := S_0;
   
     if (hi < 0) then error "upper bound should be nonnegative";
     if (lo > sB-n) then error "lower bound should be <=" << sB-n << endl;
   
     T := mutableMatrix(QQ,s+1,hi-lo+1); 
   
     K := hilbCoeff(Lh);
  
     for h from 0 to sH do (xp := chiPol(d,h,{B,H},K) + 0*d; 
	  for i from 1 to hi+h do
	  (T_(dec+h,i-h+vec) = substitute(xp, {d => i/1})));
  
     hp := hilbPol(d,n,B,K) + 0*d;
     nB := #B;
     for t from 0 to nB-1 do (p := B_t;
	  h := p - t;
	  v := substitute(hp, {d => (-p)/1});
	  if v < 0 then v = -v;
	  T_(-h+dec,-p+h+vec) = v);      

     Bc := conj(B,n);
     Kc := hilbCoeff(dualHomTriplet(Lh));
     m := n+lo-sB;
     for h from 0 to sC do (xp := chiPol(d,h,{Bc,C},Kc) + 0*d;
	  for i from m-h to -1 do (
	       v := substitute(xp, {d => -i/1});
	       T_(dec-sB-h,-n+i+sB+h+vec) = v));
 
     return T);

TEST ///
T = triplet({0,3,4}, {0,2,3,4,5}, {1,3,4,5} )
Th = toHomology T
A = cohMatrix(-5,3,Th)
B = matrix A
C = lift(B,ZZ)
M = matrix ("788, 318, 100, 19, 0,0,0,0,0; 3,2,1,0,0,0,0,0,0;
           0,0,0,1,2,0,0,0,0; 0,0,0,0,0,0,0,0,0;
	   0,0,0,0,0,7,26,62,120; 0,0,0,0,0,8,48,168,448") 
assert(C == M)
///
	   
--computes the hypercohomology table
cohTable=method(TypicalValue => CohomologyTally)
cohTable(ZZ,ZZ,Triplet) := (lo,hi,Lh) -> (
     homStrands := strands(Lh#1);
     M := cohMatrix(lo,hi,Lh);    
     e := entries M;
     n := #e-1; -- row indices are n-1 .. 0
     a := flatten apply(#e, i -> 
	  apply(#e#i, j -> (n-i-homStrands, i+j+lo-n + homStrands) 
	       => if e#i#j != 0 then e#i#j else null));
     a = select(a, b -> b#1 =!= null);
     new CohomologyTally from a
     );

beginDocumentation()

document { Key => Triplets,
     Headline => "Betti diagrams and hypercohomology tables associated to triplets of degree sequences",
     EM "Triplets", " is a package to calculate",
     
     PARA{},
     
     "1) Betti diagrams of triplets of
     pure free squarefree complexes, as introduced in math.AC/1207.2071 \"Triplets of pure
     free squarefree complexes\" ",
     
     PARA{}, 
     
     " 2) hypercohomology tables associated
     to homology triplets, as given in math.AC/1212.3675 \"Zipping Tate resolutions and
     exterior coalgebras\" ",
     
     PARA{}, 
     "by Gunnar Floeystad.",
     
     PARA{},
     SUBSECTION "Degree sequences",
     UL {
	  TO strands,
	  TO strandsL,
	  TO conj	 
          },
     SUBSECTION "Degree triplets and homology triplets",
     UL { TO triplet,
	  TO rotForw,
	  TO rotBack,
	  TO toHomology,
	  TO toDegree,
	  TO dualHomTriplet,
	  TO type
	  },
     SUBSECTION "Checking triplets",
     UL {
	  TO isDegreeTriplet,
	  TO isHomologyTriplet,
	  },
     SUBSECTION "Betti diagrams",
     UL {
	  TO Betti1,
	  TO Betti3,
	  TO BettiDiagram1,
	  TO BettiDiagram3
	  },
     SUBSECTION "Polynomials",
     UL {
	  TO binPol,
	  TO hilbCoeff,
	  TO hilbPol,
	  TO chiPol
	  },
     SUBSECTION "Cohomology tables",
     UL {
	  TO cohMatrix,
	  TO cohTable
	  },
   
   PARA{},
     "We create a Triplet using the ", TO triplet, " function:",
     EXAMPLE lines ///
              T = triplet({1,2,3}, {0,2}, {0,2,3})
	      isDegreeTriplet T
	      ///,
     "We can rotate this degree triplet forwards or backwards:",
     EXAMPLE lines /// 
              rotForw T
              rotBack T
	     ///,
     "We can compute the Betti numbers and Betti diagrams associated to the degree
     sequences of this triplet:", 
     EXAMPLE lines /// 
          Betti3 T
	  BettiDiagram3 T
	  ///,
	 "We convert it to a homology triplet:",
      EXAMPLE lines /// 
           Th = toHomology T
	   isHomologyTriplet Th
	   ///,
       "We compute the hypercohomology table of a complex of coherent sheaves associated
      to this homology triplet:",
       	EXAMPLE lines /// 
	    cohTable (-7, 5,Th)
	    ///,   
       "The dual homology triplet and its hypercohomology table:",
            EXAMPLE lines /// 
	    Thd = dualHomTriplet Th
	    cohTable (-7,5,Thd)
	    ///,   
	     }


document {
     Key => {Triplet},
     Headline => "triplet",
     "A Triplet is a list consisting of three degree sequences, each  of which is a list of
     increasing integers. These three degree sequences fulfill
     certain compatibility conditions. There are two different but equivalent versions:", 
     
     PARA{},
     "1. A degree triplet, see Definition 2.9 in math.AC/1207.2071 \"Triplets of pure free squarefree
     complexes\" ", 

     PARA{}, 
     "2. A homology triplet, see Definition 5.4 in math.AC/1212.3675 \"Zipping Tate resolutions and
     exterior coalgebras\" ",
     
     PARA{}, 
     "The routines ", TO isDegreeTriplet, " and ", TO isHomologyTriplet, " checks if a triplet fulfills
     the compatibility conditions for degree and homlogy triplets, respectively.
     
     The routine ", TO toHomology, " converts from a degree triplet to a homology triplet, and
     the routine ", TO toDegree, " converts from a homology triplet to a degree triplet."
     
     }
     

     
     
document { 
     Key => {(strands,List), strands},
     Headline => "strand span of degree sequence",
     Usage => "strands D",
     Inputs => {
	  "D" => "a sequence of integers, called a degree sequence"
	  },
     Outputs => {
	  ZZ => "strand span"
	  },
     "This is one less than the number of strands of D. Note that some of the strands
     can be empty.",
     EXAMPLE lines ///
     	  D = {2,3,5, 9,10,11}
	  strands(D)
	  ///,    
     SeeAlso => {strandsL}
     }


document { 
     Key => {(strandsL,ZZ,List), strandsL},
     Headline => "strand span as a subset of [0,n]",
     Usage => "strandsL(n,D)",
     Inputs => {
	  "n" => "to give the interval [0,n]",
	  "D" => "a sequence of integers, called a degree sequence"
	  },
     Outputs => {
	  ZZ => "strand span of the subset D of [0,n]"
	  },
     "This is one less than the number of strands of D as a subset of [0,n]. Note that
     some of the strands may be empty.",
     EXAMPLE lines ///
          n = 5
	  D = {1,2,5}
          strandsL(n,D)
	  ///,
     Caveat => {},
     SeeAlso => {strands}
     }

document { 
     Key => {(conj,List,ZZ), conj},
     Headline => "conjugate of degree sequence",
     Usage => "conj(D,n) ",
     Inputs => {
	  "D" => "a sequence of integers",
	  "n" => "an integer"
	  },
     Outputs => {
	  List => "a sequence of integers"
	  },
     "Gives the sequence of integers n-d where d runs through D.",
     EXAMPLE lines ///
         n = 5
	 D = {2, 4,5}
	 conj(D,n)
	  ///,
     Caveat => {},
     SeeAlso => {}
     }

document {
     Key => {(triplet, List, List, List), triplet},
     Headline => "make a triplet",
     Usage => "triplet(A,B,C)",
     Inputs => {
	  "A" => "an increasing sequence of integers",
	  "B" => "an increasing sequence of integers",
	  "C" => "an increasing sequence of integers"
	  },
     Outputs => {
	  Triplet => "the triplet of degree sequences"
	  },
     EXAMPLE lines ///
     T1 = triplet({1,2,3}, {0,2}, {0,2,3})
     T2 = triplet({0,3,4}, {0,2,3,4,5}, {1,3,4,5})
     T3 = triplet({1,3,4}, {0,2,3}, {0,2,3,4})
     ///,
     SeeAlso => {isDegreeTriplet, isHomologyTriplet}
     }
document { 
     Key => {(rotForw, Triplet), rotForw},
     Headline => "forward cyclic permutation",
     Usage => "rotForw T",
     Inputs => {
	  "T" => "a triplet of degree sequences"
	  },
     Outputs => {
	  Triplet => "the cyclically shifted forward triplet.",
	  },
     EXAMPLE lines ///
     T = triplet({1,2,3}, {0,2}, {0,2,3})
     rotForw(T)
	  ///,
     Caveat => {},
     SeeAlso => {rotBack}
     }

document { 
     Key => {(rotBack, Triplet), rotBack},
     Headline => "backward cyclic permutation",
     Usage => "rotBack T",
     Inputs => {
	  "T" => "a triplet of degree sequences"
	  },
     Outputs => {
	  Triplet => "the cyclically shifted backward triplet.",
	  },
     EXAMPLE lines ///
     T = triplet({1,2,3}, {0,2}, {0,2,3})
     rotBack(T)
	  ///,
     Caveat => {},
     SeeAlso => {rotForw}
     }
document { 
     Key => {(toHomology, Triplet), toHomology},
     Headline => "from degree triplet to homology triplet",
     Usage => "toHomology T",
     Inputs => {
	  "T" => "a degree triplet {A,B,C}"
	  },
     Outputs => {
	  Triplet => "a homology triplet {A, conj(B,n), C}"
	  },
     "Takes a degree triplet and returns the corresponding homology triplet.",
     EXAMPLE lines ///
     T = triplet({1,2,3}, {0,2}, {0,2,3})
     toHomology(T)
	  ///,
     Caveat => {},
     SeeAlso => {toDegree,conj}
     }

document { 
     Key => {(toDegree,Triplet), toDegree},
     Headline => "from homology triplet to degree triplet",
     Usage => "toDegree T",
     Inputs => {
	  "T" => "a homology triplet {B,H,C}"
	  },
     Outputs => {
	  Triplet => "a degree triplet {B, conj(H,n),C}.",
	  },
     "Takes a homology triplet and returns the corresponding degree triplet.",
     EXAMPLE lines ///
     T = triplet({1,2,3}, {1,3}, {0,2,3})
     toDegree(T)
	  ///,
     Caveat => {},
     SeeAlso => {toHomology,conj}
     }

document { 
     Key => {(dualHomTriplet,Triplet), dualHomTriplet},
     Headline => "the dual homology triplet",
     Usage => "dualHomTriplet T",
     Inputs => {
	  "T" => "a homology triplet (B,H,C)"
	  },
     Outputs => {
	  Triplet => "the dual homology triplet (conj(B,n),C,H)"
	  },
     EXAMPLE lines ///
      T = triplet({1,2,3}, {1,3}, {0,2,3})
      dualHomTriplet(T)
	  ///,
     Caveat => {},
     SeeAlso => {conj}
     }
document { 
     Key => {(type,Triplet),type},
     Headline => "number of variables",
     Usage => "type T",
     Inputs => {
	  "T" => "degree triplet or homology triplet"
	  },
     Outputs => {
	  ZZ => "the number of variables in the squarefree polynomial ring"
	  },
     EXAMPLE lines ///
        T = triplet({1,2,3}, {0,2}, {0,2,3})
        type(T)
       ///,
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => {(isDegreeTriplet,Triplet), isDegreeTriplet},
     Headline => "checks if it is a degree triplet",
     Usage => "isDegreeTriplet T",
     Inputs => {
	  "T" => "a triplet of degree sequences"
	  },
     Outputs => {
	  {"whether it is a degree triplet or not"}
	  },
     EXAMPLE lines ///
     T1 = triplet({1,2,3}, {0,2}, {0,2,3})
     isDegreeTriplet(T1)
     T2 = triplet({1,2,3}, {0,2}, {0,1,3})
      isDegreeTriplet(T2)
	  ///,
     Caveat => {},
     SeeAlso => {isHomologyTriplet}
     }

document { 
     Key => {(isHomologyTriplet,Triplet), isHomologyTriplet},
     Headline => "checks if it is a homology triplet",
     Usage => "isHomologyTriplet T",
     Inputs => {
	  "T" => "a triplet of degree sequences"
	  },
     Outputs => {
	  {"whether it is a homology triplet or not"}
	  },
     EXAMPLE lines ///
     T = triplet({1,2,3}, {1,3}, {0,2,3})
     isHomologyTriplet(T)
	  ///,
     Caveat => {},
     SeeAlso => {isDegreeTriplet}
     }



document { 
     Key => {(Betti1,Triplet), Betti1},
     Headline => "Betti numbers of first pure complex",
     Usage => "Betti1 T",
     Inputs => {
	 "T" => "a degree triplet"
	  },
     Outputs => {
	  List => "the Betti numbers"
	  },
     "The Betti numbers of the first complex in the triplet of pure complexes with the three
     given degree sequences.", 
     EXAMPLE lines ///
	T = triplet({1,2,3}, {0,2}, {0,2,3})
	Betti1(T)
	  ///,
     Caveat => {},
     SeeAlso => {Betti3, BettiDiagram1}
     }

document { 
     Key => {(Betti3,Triplet), Betti3},
     Headline => "Betti numbers of the three pure complexes",
     Usage => "Betti3 T",
     Inputs => {
	  "T" => "a degree triplet"
	  },
     Outputs => {
	  List => "The three sequences of Betti numbers of the triplet of pure complexes
	  with the three given degree sequences."
	  },
     EXAMPLE lines ///
        T = triplet({1,2,3}, {0,2}, {0,2,3})
	Betti3(T)
	  ///,
     Caveat => {},
     SeeAlso => {Betti1, BettiDiagram3}
     }

document { 
     Key => {(BettiDiagram1,Triplet), BettiDiagram1},
     Headline => "Betti diagram of first pure complex ",
     Usage => "BettiDiagram1 T",
     Inputs => {
	  "T" => "a degree triplet"
	  },
     Outputs => {
	 BettiTally => "The Betti diagram of the first complex in the triplet of pure complexes with the three
     given degree sequences.", 
	  },
     EXAMPLE lines ///
        T = triplet({1,2,3}, {0,2}, {0,2,3})
	BettiDiagram1(T)
	  ///,
     Caveat => {},
     SeeAlso => {Betti1, BettiDiagram3}
     }

document { 
     Key => {(BettiDiagram3,Triplet), BettiDiagram3},
     Headline => "Betti diagrams of the three pure complexes",
     Usage => "BettiDiagram3 T",
     Inputs => {
	  "T" => "a degree triplet"
	  },
     Outputs => {
	 BettiTally => "The Betti diagrams of the three complexes in the triplet of pure complexes with the three
     given degree sequences.", 
	  },
     EXAMPLE lines ///
        T = triplet({1,2,3}, {0,2}, {0,2,3})
	BettiDiagram3(T)
	  ///,
     Caveat => {},
     SeeAlso => {Betti3, BettiDiagram1}
     }

document { 
     Key => {(binPol,RingElement,ZZ,ZZ), binPol},
     Headline => "product of two binomial polynomials",
     Usage => "binPol(d,n,i)",
    Inputs => {
	   "d" => "the variable of the polynomial ring",
	  "n",
	  "i" => "in [0,n]"
	  },
    Outputs => {
	 {"a polynomial taking integers to integers"}
	  },
     "The polynomial of degree <= n, with values 0 for all d in [-n,0]-{-i} and with value (-1)^i when
     d = -i.",
     EXAMPLE lines ///
         QQ[d]
         binPol(d,3,1)
	  ///,
     Caveat => {},
     SeeAlso => {hilbPol}
     }
document { 
     Key => {(hilbCoeff,Triplet), hilbCoeff},
     Headline => "coefficients of Hilbert polynomial",
     Usage => "hilbCoeff T",
     Inputs => {
	  "T" => "a homology triplet"
	  },
     Outputs => {
	 List => "list of coefficents a_i for Hilbert polynomial"
	  },
     "The Hilbert polynomial will be written as a linear combination
     of the polynomials a_i binPol(n,i), where ", TO binPol, " is used to compute  these polynomials.",
     EXAMPLE lines ///
        T = triplet({1,2,3}, {1,3}, {0,2,3})
	hilbCoeff(T)
	  ///,
     Caveat => {},
     SeeAlso => {hilbPol,binPol}
     }

document { 
     Key => {(hilbPol,RingElement, ZZ,List,List),hilbPol},
     Headline => "Hilbert polynomial",
     Usage => "hilbPol(d,n,B,K)",
     Inputs => {
	  "d" => "variable in the polynomial ring",
	  "n" => "type of degree triplet",
	  "B" => "the Betti degrees",
	  "K" => "integer coefficients of Hilbert polynomial"
	  },
     Outputs => {
	  Expression => "Hilbert polynomial of complex of coherent sheaves associated to homology triplet"
	  },
     "The Hilbert polynomial of a complex of coherent sheaves associated to homology triplet.",
     EXAMPLE lines ///
         QQ[d]
         T = triplet({1,2,3}, {1,3}, {0,2,3})  
	 hilbPol(d,3,{1,2,3},hilbCoeff(T))
	  ///,
     Caveat => {},
     SeeAlso => {binPol, chiPol, hilbCoeff}
     }


document { 
     Key => {(chiPol,RingElement,ZZ,List,List), chiPol},
     Headline => "Hilbert polynomial of cohomology sheaves",
     Usage => "chiPol(d,p,L,K)",
     Inputs => {
	  "d" => "variable in polynomial ring",
	  "p" => "for the -p'th cohomology sheaf",
	  "L" => "pair {B,H} of Betti degrees and homology degrees",
	  "K" => "integer coefficients of the Hilbert polynomial"
	  },
     Outputs => {
	  Expression => "Hilbert polynomial of the -p'th cohomology sheaf of the complex
	  of coherent sheaves associated to a homology triplet"
	  },
     "Computes the Hilbert polynomial of the -p'th cohomology sheaf of the complex	 
      of coherent sheaves associated to a homology triplet",
     EXAMPLE lines ///
         QQ[d]
         T = triplet({1,2,3}, {1,3}, {0,2,3})  
	 chiPol(d,0,{T#0,T#1},hilbCoeff(T))
       chiPol(d,1,{T#0,T#1},hilbCoeff(T))
	  ///,
     Caveat => {},
     SeeAlso => {hilbPol}
     }

document { 
     Key => {(cohMatrix,ZZ,ZZ,Triplet), cohMatrix},
     Headline => "cohomology table in matrix form",
     Usage => "cohMatrix(lo,hi,T)",
     Inputs => {
	  "lo" => "the leftmost degree of the table",
	  "hi" => "the rightmost degree of the table", 
	  "T" => "a homology triplet",
	  },
     Outputs => {
	  MutableMatrix => "a hypercohomology table in matrix form"
	  },
     "The hypercohomology table is written in the shifted form with H^i(E(d) in row i and
     column d+i.",
     EXAMPLE lines ///
        T = triplet({1,2,3}, {1,3}, {0,2,3})  
	cohMatrix(-7,4,T)
	  ///,
     Caveat => {},
     SeeAlso => {cohTable}
     }

document { 
     Key => {(cohTable,ZZ,ZZ,Triplet), cohTable},
     Headline => "cohomology table",
     Usage => "cohTable(lo,hi,T)",
     Inputs => {
	  "lo" => "the leftmost degree of the table",
	  "hi" => "the rightmost degree of the table", 
	  "T" => "a homology triplet",
	  },
     Outputs => {
	  CohomologyTally => "a hypercohomology table"
	  },
     "The hypercohomology table is written in the shifted form with H^i(E(d) in row i and
     column d+i.",
     EXAMPLE lines ///
        T = triplet({1,2,3}, {1,3}, {0,2,3})  
	cohTable(-7,4,T)
	  ///,
     Caveat => {},
     SeeAlso => {cohMatrix}
     }



end

T = triplet({1,3,4}, {0,2,3}, {0,2,3,4})
T = triplet({1,2,3}, {0,2}, {0,2,3})
T = triplet({0,3},{1,2,3}, {0,1,2})
T = triplet({1,3,4}, {0,2,3}, {0,2,3,4})
T = triplet({0,2,3,4}, {0,2,4,5}, {1,2,4,5})
T = triplet({0,1,3,5,6}, {1,2,4,5,6,8}, {2,3,4,5,7})
T = triplet({0,3,6}, {1,2,3,4,5,6}, {0,1,2,4,5})
T = triplet({1,2,4,6}, {3,4,5}, {0,1,2,3})
T = triplet({0,3,4}, {0,2,3,4,5}, {1,3,4,5})
  
isDegreeTriplet(T)
type(T)
T1 = rotForw(T)
T2 = rotBack(T)
Betti1(T)
Betti3(T)
BettiDiagram1(T)
BettiDiagram3(T)


Th = toHomology(T)
isHomologyTriplet(Th)
cohTable(-9,6,Th)
Td = dualHomTriplet(Th)
cohTable(-7,6,Td)
Th1 = toHomology(T1)
cohTable(-10,6,Th1)

T = triplet({0,2,3,4}, {0,2,4,5}, {1,2,4,5}}
isDegreeTriplet(T)
type(T)
T1 = rotForw(T)
T2 = rotBack(T)
Betti1(T)
Betti3(T)
BettiDiagram1(T)
BettiDiagram3(T)


Th = toHomology(T)
isHomologyTriplet(Th)
cohTable(-9,6,Th)
Td = dualHomTriplet(Th)
cohTable(-7,6,Td)
Th1 = toHomology(T1)
cohTable(-10,6,Th1)


T = triplet({0,1,3,5,6}, {1,2,4,5,6,8}, {2,3,4,5,7}}
isDegreeTriplet(T)
type(T)
T1 = rotForw(T)
T2 = rotBack(T)
Betti1(T)
Betti3(T)
BettiDiagram1(T)
BettiDiagram3(T)


Th = toHomology(T)
isHomologyTriplet(Th)
cohTable(-9,6,Th)
Td = dualHomTriplet(Th)
cohTable(-7,6,Td)
Th1 = toHomology(T1)
cohTable(-10,6,Th1)


installPackage("Triplets") --, RerunExamples=>true, RemakeAllDocumentation=>true);
viewHelp "Triplets" 
exit
