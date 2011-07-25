needsPackage "Polyhedra"

newPackage(
	"MonomialAlgebras",
    	Version => "1.9", 
    	Date => "July 8, 2011",
    	Authors => {
         {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de/"},	     
         {Name => "Janko Boehm", Email => "boehm@math.uni-sb.de", HomePage => "http://www.math.uni-sb.de/ag/schreyer/jb/"},
         {Name => "Max Nitsche", Email => "nitsche@mis.mpg.de", HomePage => ""}
         },
    	Headline => "Monomial algebras",
	CacheExampleOutput => false,
	AuxiliaryFiles => false,
    	DebuggingMode => true
        )

-- For information see documentation key "MonomialAlgebras" below.

export({decomposeMonomialAlgebra,
          --decomposeMonomialCurve,
	  --decomposeSimplicialHomogeneousMonomialAlgebra,
	  homogenizeSemigroup,
	  adjoinPurePowers,
	  monomialAlgebraIdeal,
	  CoefficientField,verbose,ReturnBA,isSeminormalMA,isNormalMA,
          isCohenMacaulayMA,findGeneratorsOfSubalgebra,testEGsimplicial,isGorensteinMA,isBuchsbaumMA,isSimplicialMA,
          proveEGsimplicial,proveEG,nextSubset,file,codimMA,degreeMA,testEG,regularityMA,
          TerminateAt,MaxNumberTests,decomposeHomogeneousMonomialAlgebra,randomSemigroups})

needsPackage "Polyhedra"


decomposeMonomialCurve=method(Options=>{CoefficientField=>ZZ/101,verbose=>0})
decomposeMonomialCurve(List):= opts ->A -> (
   kk := opts.CoefficientField;
   vb:=opts.verbose;
   x:=symbol x;
   s:=symbol s;
   t:=symbol t;
   if not gcd A ==1 then print "WARNING: exponents not relatively prime";
   n := #A;
   d := max A;
         if vb>0 then (print d);
   deglist := prepend({0,d}, for i from 0 to n-1 list {A_i, d-A_i});
         if vb>0 then (print deglist);
   S := kk[x_0..x_n, Degrees =>deglist];
   P := kk[s,t, Degrees =>{{1,0}, {0,1}}];
   maplist := deglist/(D->s^(D_1)*t^(D_0));
   I := ker map(P,S,maplist);
         if vb>0 then (print I);
   N := S^1/(ideal(x_0,x_n)+I);
   bN := basis N;
         if vb>0 then (
          print("Basis ",bN);
          print("Degress of the basis elements ",last degrees bN);
         );
   L := partition(p -> ((first p)%d), last degrees bN);
         if vb>0 then (print("Partition by the first coordinate modulo ",d," is ",L));
   --replace each value by itself normalized then divided by d, 
   --with a twist to show the amount of normalization.
   L1 := applyValues(L,LL -> (
	  min1 := min(LL/first);
	  min2 := min(LL/last);
                  if vb>0 then (
                     print("Subtract from ",LL);
                     print("Minima ",min1,min2);
                  );
	  LL1 := {LL/(p->{(first p - min1)//d, (last p - min2)//d}),  (min1+min2)//d};
                  if vb>0 then (
                     print("and divide by ",d);
                     print(LL1#0);
                     print("Twist ",LL1#1);
                     print(" ");
                  );
          LL1
     ));
   a:=getSymbol "a";
   b:=getSymbol "b";  
   T := kk(monoid[a,b]);
   --Now make ideals in T by grouping the degrees in genDegs by congruence class.
  applyValues(L1, LL->( 
     (ideal apply(first LL, m->T_0^(first m)*T_1^(last m)))*(T^{-last LL}))
	  )
)

{*
A = {1,4,5};
decomposeMonomialCurve A

A={{1},{4},{5}}
decomposeSimplicialHomogeneousMonomialAlgebra A



B={{4, 0}, {0, 4}, {1, 3}, {3, 1}}
C={{4, 0}, {0, 4}}
S = QQ[x_0..x_(#B-1), Degrees=>B]
P = QQ[x_0..x_(#C-1), Degrees=>C]
f = map(S,P)
decomposeMonomialAlgebra f


B={{5, 0}, {0, 5}, {1, 4}, {4, 1}}
C={{5, 0}, {0, 5}}
S = QQ[x_0..x_(#B-1), Degrees=>B]
P = QQ[x_0..x_(#C-1), Degrees=>C]
f = map(S,P)
decomposeMonomialAlgebra f


B={{1,0, 0}, {0, 2,0}, {0, 0,2}, {1,0, 1},{0,1,1}}
C={{1,0, 0}, {0, 2,0}, {0, 0,2}}
S = QQ[x_0..x_(#B-1), Degrees=>B]
P = QQ[x_0..x_(#C-1), Degrees=>C]
f = map(S,P)
decomposeMonomialAlgebra f


*}



decomposeHomogeneousMonomialAlgebra=method(Options=>{verbose=>0,ReturnBA=>false,CoefficientField=>ZZ/101})
decomposeHomogeneousMonomialAlgebra(RingMap):= opt-> f -> (
l:=findLinearForm degrees source f;
dc:=decomposeMonomialAlgebra(f,opt);
dc=applyValues(dc,j->{j#0,(transpose(l)*j#1)_(0,0)};
RA:=ring first first values dc;
I:=ideal RA;
K:=coefficientRing RA;
PRA0:=K[gens RA];
RA0:=PRA0/sub(I,PRA0);
applyValues(dc,j->{sub(j#0,RA0),j#1}));
)

decomposeHomogeneousMonomialAlgebra(PolynomialRing):= opt-> R -> (
l:=findLinearForm degrees R;
dc:=decomposeMonomialAlgebra(R,opt);
dc=applyValues(dc,j->{j#0,(transpose(l)*j#1)_(0,0)});
RA:=ring first first values dc;
I:=ideal RA;
K:=coefficientRing RA;
PRA0:=K[gens RA];
RA0:=PRA0/sub(I,PRA0);
applyValues(dc,j->{sub(j#0,RA0),j#1}))

decomposeHomogeneousMonomialAlgebra(List):= opt-> B -> (
if class first B ===ZZ then (
  B = adjoinPurePowers homogenizeSemigroup apply(B,j->{j});
);
l:=findLinearForm B;
c:=(transpose l)*(B#0);
dc:=decomposeMonomialAlgebra(B,opt);
dc=applyValues(dc,j->{j#0,(transpose(l)*j#1)_(0,0)});
RA:=ring first first values dc;
I:=ideal RA;
K:=coefficientRing RA;
PRA0:=K[gens RA];
RA0:=PRA0/sub(I,PRA0);
applyValues(dc,j->{sub(j#0,RA0),j#1}))


findLinearForm=method()
findLinearForm(List):=(B)->(
Li:=gens ker matrix apply(B,j->prepend(1,j));
sv:=first entries Li^{0};
nzc:=select(#sv,j->(sv#j)!=0);
if #nzc==0 then error("Input not homogeneous");
si:=first nzc;
l:=(Li_{si})^{1..(-1+rank target Li)};
a:=-Li_(0,si);
(1/a)*sub(l,QQ));



decomposeMonomialAlgebra=method(Options=>{verbose=>0,ReturnBA=>false,CoefficientField=>ZZ/101})
decomposeMonomialAlgebra(RingMap):= opt-> f -> (
vb:=opt.verbose;
S:=target f;
P:=source f;
K:= coefficientRing S;
p := numgens P;
n := numgens S;
B := degrees S;
C := degrees P;
c := sum C;
m := #B_0;
I := monomialAlgebraIdeal S;
    if vb>0 then (
     --print("The ideal cooresponding to the degree monoid of the target");
     print(net(coefficientRing(ring I))|"["|net(B)|"]  =  "|net(K)|"["|net(gens(S))|"]  /  "|net(I));
     print "";
     print("The image of the variables of the source");
     print "";
    );
fP:=f(ideal vars P);
    if vb>0 then (
     print(fP);
    );
t:=symbol t;
T:= K[t_0..t_(m-1)];
targ:=  apply(B, b -> product(apply (#b, i-> t_i^(b_i))));
ff:=map(T,S,targ);
mAKB:=ff(fP);
I1:=ker map(T/mAKB,S,targ);
--print("Test ",mingens I1, mingens(fP+I));
-- quotient ring by the ideal of the degree monoid and
-- the image of P
N := S^1/(fP+I);
    if vb>0 then (
     print "";
     print(net(K)|" - basis of "|net(K)|"["|net(gens(S))|"] / "|net(mingens(fP+I)));
     print "";
     print basis N;
     print "";
     print "Degrees of the basis elements";
     print "";
     print last degrees basis N;
     print "";
    );
bN := last degrees basis N;
if opt.ReturnBA then return({f,bN});
Cm := transpose matrix C;
C1 := gens gb Cm;
    if vb>0 then (
     print "The degrees of the source variables";
     print "";
     print(Cm);
     print "";
     print "Minimal generators of the finitely generated abelian group generated by the degrees of the source";
     print "";
     print C1;
    );
--mingens image Cm;
L := partition(j->(transpose matrix {j})%C1,bN);
    if vb>0 then (
     print "";
     print "Equivalence classes of the degrees of the basis of the quotient modulo the degrees of the source";
     print "";
     print L;
    );
IA:=monomialAlgebraIdeal P;
PIA:=P/IA;
dc:=applyPairs (L, (k,V0)->(  
         if vb>0 then (
          print "------------------------------------------------------------------------------------";
          print "";
          print ("The reduced representative "|net(k)|" in  image "|(net B)|"  /  image "|(net C));
          print "represents the equivalence class of basis vectors";
          print "";
          print V0;
          print "";
          print "---------------------------------------------";
          print "";
         );
     V:=apply(V0, v1->(entries transpose((transpose matrix {v1})-k))#0);
         if vb>0 then (
          print "Differences of the elements of the equivalence class to the chosen representative";
          print "";
          print V;
         );
     vm := transpose matrix V;
         if vb>0 then (
          print "";
          print "Write the differences in terms of the generators of the degree monoid of the source";
         );
     coef := vm//Cm;
     coef = coef%(gens ker Cm);
         if vb>0 then (
          print "";
          print((net vm)|" = "|net(Cm)|" * "|(net coef));
         );
     mins := transpose matrix {for i from 0 to numrows coef -1 list 
            min (entries coef^{i})#0};
         if vb>0 then (
          print "";
          print "Minimum of each row";
          print "";
          print mins;
          print "";
          print "Representative + Generators * Minima = Twist";
          print "";
         );
     twist:=k+Cm*mins;
         if vb>0 then (
          print(net(k)|" + "|net(Cm)|" * "|net(mins)|" = "|net(twist));
         );
     Mins := mins * matrix{{numcols vm:1}};
         if vb>0 then (
          print "";
          print "Subtract minima from each column of the coefficients";
         );
     coef1 := coef-Mins;
         if vb>0 then (
          print "";
          print coef1;
          print "";
          print "Form the corresponding monomial ideal generated by the columns";
         );
     gamma:=ideal(apply(numcols coef1, 
		  v->product(apply(p, j->P_j^(coef1_v_j)))));
         if vb>0 then (
          print "";
          print gamma;
          print "";
          print("The submodule Gamma_"|net(k)|" of "|net(coefficientRing(ring I))|"["|net(B)|"] is (written multiplicatively)");
          t := local t;
          T := K[t_0..t_(m-1)];
          targ :=  apply(C, c -> product(apply (#c, i-> t_i^(c_i))));
          slist:=apply(#(gens P),j->P_j=>targ#j);
          print "";
          print(net(product(apply (m, i-> Power(T_i,twist_(i,0)))))|" * "|net(sub(gamma,slist)));
          print "";
          print("which is isomorphic to the ideal of "|(net PIA));
          print "";
          print(net(sub(gamma,PIA))|" twisted by "|net(twist));
          print "";
         );
     (k,{sub(gamma,PIA), twist})
           )))

{*
installPackage "MonomialAlgebras"
B = {{3, 3, 1}, {3, 4, 0}, {0, 6, 1}, {4, 0, 3}, {2, 3, 2}, {4, 2, 1}, {1, 2, 4}, {5, 0, 2}, {2, 1, 4}, {0, 1, 6}, {1, 5, 1}}
R=QQ[x_0..x_(#B-1),Degrees=>B]
dc=decomposeMonomialAlgebra R
regularityMA(dc,B)


*}

{*
B={{4,0,0,0,0,0,0,0},{0,4,0,0,0,0,0,0},{0,0,4,0,0,0,0,0},{0,0,0,4,0,0,0,0},{0,0,0,0,4,0,0,0},{0,0,0,0,0,4,0,0},{0,0,0,0,0,0,4,0},{0,0,0,0,0,0,0,4},{1,1,1,1,0,0,0,0},{0,0,0,0,1,1,1,1}}
R=QQ[x_0..x_9,Degrees=>B]
decomposeMonomialAlgebra R


-- square
installPackage "MonomialAlgebras"
m=3
B={{3,0,1},{0,3,1},{0,0,1},{3,3,1},{1,1,1},{2,0,1},{3,2,1}}
R=QQ[x_0..x_(#B-1),Degrees=>B]
dc=decomposeMonomialAlgebra(R,verbose=>0)

A={{3,0,1},{0,3,1},{0,0,1},{3,3,1}}
P=QQ[x_0..x_(#A-1),Degrees=>A]
T = QQ[t_0..t_(m-1)];
targ =  apply(B, c -> product(apply (#c, i-> t_i^(c_i))));
slist=apply(#(gens P),j->P_j=>targ#j);
I=first (values dc)#2
It=first entries gens sub(sub(I,P),slist)
At=apply(#(gens P),j->targ#j)
fj=map(T,QQ[z_1,z_2,Degrees=>{{0,0,2},{3,3,2}}]**P,join(It,At))
ker fj

I=first (values dc)#2
W=ring I
P=ring ideal W
map(W^1,P^2, gens I)

*}



decomposeSimplicialHomogeneousMonomialAlgebra=method(Options=>{CoefficientField=>ZZ/101})
decomposeSimplicialHomogeneousMonomialAlgebra(List):= opts -> A -> (
--A should be a list of elements of NN^(m-1), all of total degree <=d, (thought of
--as homogeneous elements of degree d in NN^m.
B := adjoinPurePowers homogenizeSemigroup A;
d := sum B_0;
m := #B_0;
n := #B;
   kk:=opts.CoefficientField;
   x:=symbol x;
   s:=symbol s;
   t:=symbol t;
S := kk[x_0..x_(n-1), Degrees => B];
C :=  apply(m, i-> apply(m, j -> if(j == i) then d else 0));  -- pure powers
P := kk[s_0..s_(m-1), Degrees => C];
I := monomialAlgebraIdeal S;
N := S^1/(ideal(x_0..x_(m-1))+I);
bN := basis N;
L := partition(p -> p/(i -> i%d), last degrees bN);
--print L;
--replace each value by itself normalized then divided by d, 
--with a twist to show the amount of normalization.
L1 := applyValues(L,LL -> (
	  degLL0 := (sum LL_0)//d;
--	  degLL = (min(LL/sum))//d;
	  LL1 := LL / (l -> (apply(l - LL_0,j->j//d)));
	  mins := apply(#LL_0, i -> min apply(LL1, l -> l#i));
	  {LL1 / (l -> l-mins), degLL0 + sum(mins)}
     ));
   a:=getSymbol "a";
   T := kk(monoid[a_0..a_(m-1)]);
--a := local a;
--T := kk[a_0..a_(m-1)];
--Now make ideals in T by grouping the degrees in genDegs by congruence class.
applyValues(L1, LL->( 
	  LLf := first LL;
     (ideal apply(LLf, mm->product(apply(#mm, i-> T_i^(mm_i)))))*(T^{-last LL}))
	  )
)
homogenizeSemigroup=method()
homogenizeSemigroup(List):= A ->(
     d := max (A/sum);
     A/(a->append(a, d-sum a))
     )

adjoinPurePowers=method()
adjoinPurePowers(List) := B -> (
     d :=  sum B_0;
     m := #(B_0);
     unique(apply(m, i-> apply(m, j -> if(j == i) then d else 0)) | B)
     )

monomialAlgebraIdeal=method()
monomialAlgebraIdeal(PolynomialRing) := (R) -> (
     --R should be a multigraded poly ring
     --forms the ideal in R corresponding to the degree monoid algebra
     --generated by monomials x^(B_i), where B is the list of degrees of
     --the variables of R
     B:=degrees R;
     m := #B_0;
     t := local t;
     k:= coefficientRing R;
     T := k[t_0..t_(m-1)];
     targ :=  apply(B, b -> product(apply (#b, i-> t_i^(b_i))));
     ideal mingens ker map(T,R,targ)
     )





isGorensteinMA=method()

isGorensteinMA(List):=(B)->(
K:=QQ;
x:=symbol x;
R:=K[x_0..x_(#B-1),Degrees=>B];
isGorensteinMA(R))


isGorensteinMA(PolynomialRing):=(S)->(
if not isSimplicialMA S then error("Expected simplicial monomial algebra");
dc:=values decomposeMonomialAlgebra S;
Ilist:=first\dc;
R:=ring first Ilist;
hlist:=first\entries\transpose\last\dc;
-- test for CM
if not all(Ilist,j->j==ideal(1_R)) then return(false);
shlist:=sum\hlist;
msum:=max shlist;
if #select(shlist,j->j==msum)>1 then return(false);
i:=maxPosition(shlist);
maxel:=hlist#i;
j0:=0;j1:=0;
while #hlist>0 do (
  for j0 from 0 to -1+#hlist do (
   j1=position(hlist,jj->jj==maxel-hlist#j0);
   if j1=!=null then break;
  );
  if j1===null then return(false);
  hlist=rem(hlist,set {j0,j1});
);
true);

{*
installPackage "MonomialAlgebras"
R=QQ[x_0..x_4,Degrees=>{{1,0,0},{0,2,0},{0,0,2},{1,0,1},{0,1,1}}]
R=QQ[x_0..x_3,Degrees=>{{3,0},{2,1},{1,2},{0,3}}]
isGorensteinMA R
*}


rem=method()
rem(List,Set):=(L,I)->(
while #I>0 do (
  m:=max(toList I);
  L=remove(L,m);
  I=I - set {m};
);
L)
--rem({10,11,12,13,14,15,16,17},set {3,4,1})


isBuchsbaumMA=method()

isBuchsbaumMA(List):=(B)->(
K:=QQ;
x:=symbol x;
R:=K[x_0..x_(#B-1),Degrees=>B];
isBuchsbaumMA(R))


isBuchsbaumMA(PolynomialRing):=(S)->(
if not isSimplicialMA S then error("Expected simplicial monomial algebra");
dc:=values decomposeMonomialAlgebra S;
Ilist:=first\dc;
R:=ring first Ilist;
if not all(Ilist,j->((j==ideal(1_R)) or (j==ideal(vars R)))) then return(false);
hlist:=first\entries\transpose\last\dc;
dcmax:=select(dc,j->ideal(vars R)==first j);
hmax:=first\entries\transpose\last\dcmax;
B:=degrees S;
A:=first\findGeneratorsOfSubalgebra B;
BminusA:=select(B,j->not member(j,A));
hb:=join toSequence apply(hmax,j->apply(BminusA,jj->jj+j));
hbh:=select(hb,j->member(j,hlist));
Ih:=select(dc,j->member(first entries transpose last j,hbh));
all(first\Ih,j->j==ideal(1_R)));

{*
installPackage "MonomialAlgebras"
R=QQ[x_0..x_3,Degrees=>{{6,0},{0,6},{4,2},{1,5}}]
isBuchsbaumMA R
decomposeMonomialAlgebra R
R=QQ[x_0..x_3,Degrees=>{{4,0},{0,4},{3,1},{1,3}}]
isBuchsbaumMA R
decomposeMonomialAlgebra R
R=QQ[x_0..x_3,Degrees=>{{5,0},{0,5},{4,1},{1,4}}]
isBuchsbaumMA R
decomposeMonomialAlgebra R


loadPackage("MonomialAlgebras",Reload=>true)
R=QQ[x_0..x_3,Degrees=>{{6,0},{0,6},{4,2},{1,5}}]
isBuchsbaumMA R
decomposeMonomialAlgebra R

*}


isSimplicialMA=method()
isSimplicialMA(List):=B->(
A:=first\(findGeneratorsOfSubalgebra B);
P:=posHull transpose matrix B;
d:=P#"dimension of the cone";
#A==d)

isSimplicialMA(PolynomialRing):=R->(
isSimplicialMA degrees R)

testHomogeneous=method()
testHomogeneous(PolynomialRing):=R->(
#(unique(sum\(degrees R)))==1)

findGeneratorsOfSubalgebra=method()
findGeneratorsOfSubalgebra(List):=(B)->(
P:=posHull transpose matrix B;
d:=P#"dimension of the cone";
embdim:=P#"ambient dimension";
if P#"dimension of lineality space">0 then error("expected cone without lineality space");
L:=entries transpose rays(P);
apply(L,v->findGenerator(B,v)));

findGenerator=method()
findGenerator(List,List):=(B,v)->(
L:=select(#B,j->1==rank matrix {B#j,v});
--if #L>1 then error("dependent generators");
i:=minPosition apply(L,j->sum(B#j));
i1:=L#i;
{B#i1,i1})

--findGeneratorsOfSubalgebra {{3,0},{0,3}}

--findGeneratorsOfSubalgebra {{3,0,0},{1,1,1},{0,3,0},{0,1,2}}

decomposeMonomialAlgebra(PolynomialRing):=opt->(R)->(
K:= coefficientRing R;
B := degrees R;
L:=findGeneratorsOfSubalgebra B;
A:=apply(L,first);
subV:=apply(L,j-> R_(last j));
S:=K[toSequence subV,Degrees=>A];
f:=map(R,S);
decomposeMonomialAlgebra(f,opt))

{*
R=QQ[x_0..x_3,Degrees=>{{3,0,0},{1,1,1},{0,3,0},{0,1,2}}]
decomposeMonomialAlgebra R
*}


decomposeMonomialAlgebra(List):= opt-> B -> (
if class first B ===ZZ then (
  B = adjoinPurePowers homogenizeSemigroup apply(B,j->{j});
);
x:=symbol x;
K:=opt.CoefficientField;
R:=K[x_0..x_(#B-1),Degrees=>B];
decomposeMonomialAlgebra(R,opt));



isCohenMacaulayMA=method()

isCohenMacaulayMA(List):=(B)->(
K:=QQ;
x:=symbol x;
R:=K[x_0..x_(#B-1),Degrees=>B];
isCohenMacaulayMA(R))

isCohenMacaulayMA(PolynomialRing):=(R)->(
if not isSimplicialMA R then error("Expected simplicial monomial algebra");
dc:=first\(values decomposeMonomialAlgebra R);
S:=ring first dc;
all(dc,j->j==ideal(1_S)));

{*
installPackage "MonomialAlgebras"
a=3
B={{a, 0}, {0, a}, {1, a-1}, {a-1, 1}}
R = QQ[x_0..x_(#B-1), Degrees=>B]
isCohenMacaulayMA R
*}





isSeminormalMA=method()

isSeminormalMA(List):=(B)->(
K:=QQ;
x:=symbol x;
R:=K[x_0..x_(#B-1),Degrees=>B];
isSeminormalMA(R))


isSeminormalMA(PolynomialRing):=(R)->(
if not isSimplicialMA R then error("Expected simplicial monomial algebra");
fBA:=decomposeMonomialAlgebra(R,ReturnBA=>true);
f:=fBA#0;BA:=fBA#1;
A:=transpose matrix degrees source f;
co:=entries((sub(A,QQ))^-1 * transpose matrix BA);
max(max\co)<=1)

isNormalMA=method()

isNormalMA(List):=(B)->(
K:=QQ;
x:=symbol x;
R:=K[x_0..x_(#B-1),Degrees=>B];
isNormalMA(R))


isNormalMA(PolynomialRing):=(R)->(
if not isSimplicialMA R then error("Expected simplicial monomial algebra");
fBA:=decomposeMonomialAlgebra(R,ReturnBA=>true);
f:=fBA#0;BA:=fBA#1;
A:=transpose matrix degrees source f;
co:=entries((sub(A,QQ))^-1 * transpose matrix BA);
max(max\co)<1)

{*
B={{1,0,0},{0,2,0},{0,0,2},{1,0,1},{0,1,1}}
R=QQ[x_0..x_4,Degrees=>B]
isSeminormalMA R
*}

regularityMA=method(Options=>{CoefficientField=>ZZ/101})
regularityMA(List):=opt->B->(
if class first B ===ZZ then (
  B = adjoinPurePowers homogenizeSemigroup apply(B,j->{j});
);
K:=opt.CoefficientField;
x:=symbol x;
R:=K[x_0..x_(#B-1),Degrees=>B];
regularityMA(R))


regularityMA(PolynomialRing):=opt->R->(
B:=degrees R;
--if not isSimplicialMA R then error("Expected simplicial monomial algebra");
--if not testHomogeneous R then error("Expected homogeneous monomial algebra");
dc:=decomposeMonomialAlgebra R;
if #(first B)==2 then return(regularityMonomialCurve(dc,B));
regularityMA(dc,B))

regularityMA(HashTable,List):=opt->(dc,B)->(
L:=values(dc);
I1:=first first L;
Z:=ring I1;
P:=ring ideal Z;
Li:=gens ker matrix apply(B,j->prepend(1,j));
sv:=first entries Li^{0};
nzc:=select(#sv,j->(sv#j)!=0);
if #nzc==0 then error("Input not homogeneous");
si:=first nzc;
l:=(Li_{si})^{1..(-1+rank target Li)};
a:=-Li_(0,si);
K:=coefficientRing P;
P1:=K[gens P];
KA:=coker sub(matrix entries gens sub(ideal Z,P1),P1);
L1:={};
for j from 0 to #L-1 do (  
  I1=first(L#j);
  matg:=matrix entries sub(gens I1,P1);
  g:=map(KA,source matg,matg);
  reg:=regularity image g;
  shift:=(((matrix entries transpose last(L#j))*l)_(0,0))/a;
  --shift:=(sum(first entries transpose last(L#j)))/a;
  L1=append(L1,{reg,shift});
);
L2:=apply(L1,j->j#0+j#1);
m:=max(L2);
L3:=apply(#L,j->(L#j,L2#j));
{m,select(L3,j->j#1==m)})

regularityMonomialCurve=method()
regularityMonomialCurve(HashTable,List):=(dc,B)->(
L:=values(dc);
I1:=first first L;
Z:=ring I1;
P:=ring ideal Z;
Li:=gens ker matrix apply(B,j->prepend(1,j));
if Li==0 then error("Input not homogeneous");
l:=Li^{1..(-1+rank target Li)};
a:=-Li_(0,0);
if a==0 or rank source Li>1 then error("C(A) is not a cone of full dimension");
K:=coefficientRing P;
L1:={};
for j from 0 to #L-1 do (  
  I1=first(L#j);
  R:=K[gens ring I1,MonomialOrder=>Lex];
  --reg:=regularity sub(I1,R);
  shift:=(((matrix entries transpose last(L#j))*l)_(0,0))/a;
  expo:=exponents sum first entries mingens sub(I1,R);
  if #expo==1 then (
     reg:=0;
  ) else (
     reg=-1 + max apply(-1+#expo, j->expo#j#0+expo#(j+1)#1);
     --print(expo,shift);
  );
  L1=append(L1,{reg,shift});
);
L2:=apply(L1,j->j#0+j#1);
m:=max(L2);
L3:=apply(#L,j->(L#j,L2#j));
{m,select(L3,j->j#1==m)})



testEGsimplicial=method(Options=>{verbose=>0,CoefficientField=>ZZ/101})
testEGsimplicial(ZZ,ZZ,ZZ):=opt->(a,d,c)->(
t:=symbol t;
K:=opt.CoefficientField;
R:=K[t_0..t_(d-1)];
IB:=(ideal vars R)^a;
Bmax:=first\exponents\(first entries mingens IB);
-- form A
A:=first\exponents\(toList apply(d,j->t_j^a));
E:=toList(set(Bmax)-set(A));
-- form a random B
B0:={};
while #B0<c do (
  E=random E;
  B0=append(B0,first E);
  E=remove(E,0);
);
B:=join(A,B0);
x:=symbol x;
S:= K[x_0..x_(#A-1), Degrees=>A];
P:= K[x_0..x_(#B-1), Degrees=>B];
f:= map(P,S);
dc:=decomposeMonomialAlgebra f;
deg:=#(dc);
-- the bound
bound:=deg-c;
regL:=regularityMA(dc,B);
-- the regularity
reg:=regL#0;
-- just in case...
if reg>bound then print("Counterexample "|net(B));
{reg,bound,regL#1,B})

testEGsimplicial(List,List,ZZ):=opt->(aL,dL,n)->(
L:={};
for a in aL do (
  for d in dL do (
    --lb:=a^(d-2);
    lb:=1;
    ub:=binomial(a+d-1,d-1) - d;
    if lb>ub then (lb,ub)=(ub,lb);
          if opt.verbose>0 then (
              print("---------------------------------------------");
              print("a = "|a|"    d = "|d|"    c = "|lb|" .. "|ub);
              print("---------------------------------------------")
          );
    for c from lb to ub do (
      mx:=0;
      for j from 0 to n-1 do (
        tst:=testEGsimplicial(a,d,c,opt);
        mx=max(mx,tst#0/tst#1*1.0);
        L=append(L,prepend(mx,tst));
      );
      if opt.verbose>0 then print("c = "|c|"   "|net(mx));
    );
  );
);
L)


proveEGsimplicial=method(Options=>{verbose=>0,CoefficientField=>ZZ/101,file=>""})
proveEGsimplicial(ZZ,ZZ,ZZ):=opt->(a,d,c)->(
allRegDeg:={};
if opt.file!="" then fd := openOut(opt.file);
t:=symbol t;
K:=opt.CoefficientField;
R:=K[t_0..t_(d-1)];
IB:=(ideal vars R)^a;
Bmax:=first\exponents\(first entries mingens IB);
A:=first\exponents\(toList apply(d,j->t_j^a));
E:=toList(set(Bmax)-set(A));
L:=toList(0..c-1);
mx:=0;
maxB:={};
if c>#E then error("Codimension too large");
ntests:=binomial(#E,c);
      if opt.verbose>0 then print("Number of tests "|net(ntests));
ct:=0;
while (class L)=!=Boolean and #L==c do (
  ct=ct+1;
  if opt.verbose>1 and ct%100==0 then print(ct,ntests);
  B0:=apply(L,j->E#j);
  B:=join(A,B0);
  x:=symbol x;
  S:= K[x_0..x_(#A-1), Degrees=>A];
  P:= K[x_0..x_(#B-1), Degrees=>B];
  f:= map(P,S);
  dc:=decomposeMonomialAlgebra f;
  deg:=#(dc);
  bound:=deg-c;
  regL:=regularityMA(dc,B);
  reg:=regL#0;
  allRegDeg=unique append(allRegDeg,{reg,deg});
  if reg>bound then (
     print("Counterexample "|net(B));return B;
     if opt.file!="" then fd<<"Counterexample "|net(B)<<endl;
  );
  q:=reg/bound*1.0;
  if q==mx then (
     maxB=append(maxB,{reg,deg,B});
  );
  if q>mx then (
     mx=q;
     maxB={{reg,deg,B}};
           if opt.verbose>2 then (
             print "";
             print("New maximum "|net(mx)|" with regularity "|net(reg)|" and degree "|net(deg));
             print(B);
             if opt.file!="" then (
               fd<<endl;
               fd<<"New maximum "|net(mx)|" with regularity "|net(reg)|" and degree "|net(deg)<<endl;
               fd<<net(B)<<endl;
             );
     );
  );
  L=nextSubset(L,#E-1);
);
      if opt.verbose>0 then (
         print("Maximum for a = "|net(a)|", d = "|net(d)|" and c = "|net(c)|" is "|net(mx));
         print "";
         print("All possible (regularity,degree) "|net(allRegDeg));
         print "";
      );
      if opt.file!="" then (
         fd<<endl;
         fd<<"Maximum for a = "|net(a)|", d = "|net(d)|" and c = "|net(c)|" is "|net(mx)<<endl;
         fd<<endl;
         fd<<"The maximum is achieved by (regularity,degree,B): "<<endl;
         apply(maxB, j->(fd<<j<<endl));
         fd<<endl;
         fd<<"All possible (regularity,degree): "<<endl;
         fd<<allRegDeg<<endl;
         fd<<"------------------------------------------------------------------"<<endl;
         close fd;
      );
{mx,allRegDeg,maxB});

{*


installPackage "MonomialAlgebras"


d=2
for a from 2 to 10 do (
   ub=binomial(a+d-1,d-1) - d;
   for c from 1 to ub do (
      proveEGsimplicial(a,d,c,verbose =>1)
   );
);

d=3
for a from 2 to 9 do (
   ub=binomial(a+d-1,d-1) - d;
   for c from 1 to ub do (
      proveEGsimplicial(a,d,c,verbose =>2)
   );
   print "";
);



-- run proveEGsimplicial for d=3, a=2..9 and all possible c
-- store the data in files testEGadc.m2
d=3
for a from 2 to 9 do (
   ub=binomial(a+d-1,d-1) - d;
   for c from 1 to ub do (
      proveEGsimplicial(a,d,c,verbose =>2,file=>"testEG"|toString(a)|toString(d)|toString(c)|".m2")
   );
   print "";
);



*}

-- iterator to produce the power set of {1,...,n}

nextSubset=method()
nextSubset(List,ZZ):=(L0,n)->(
if L0=={} then return({0});
if L0==toList apply(n+1,j->j) then return(true);
L:=new MutableList from L0;
d:=#L;
if L#(d-1)<n then (
   L#(d-1)=L#(d-1)+1;
) else (
   L1:=toList apply(#L-1,j->L#j);
   if #(toList L1)==0 then return(toList apply(d+1,j->j));
   L2:=nextSubset(L1,n);
   L=append(L2,1+last L2);
   if last L>n then L=nextSubset(toList L,n);
);
toList L);

{*
L={};
for j from 0 to 10000000 do (
  L=nextSubset(L,7);print L;
  if class(L)===Boolean then break;
);



*}

-- version that also works in the non-simplicial case
testEG=method(Options=>{verbose=>0,CoefficientField=>ZZ/101})
testEG(ZZ,ZZ,ZZ):=opt->(a,d,cc)->(
t:=symbol t;
K:=opt.CoefficientField;
R:=K[t_0..t_(d-1)];
IB:=(ideal vars R)^a;
E:=first\exponents\(first entries mingens IB);
-- form a random B
B0:={};
while #B0<cc do (
  E=random E;
  if #E==0 then error("Codimension too large");
  B0=append(B0,first E);
  E=remove(E,0);
);
testEG(B0,opt));

testEG(List):=opt->(B)->(
x:=symbol x;
K:=opt.CoefficientField;
P:= K[x_0..x_(#B-1), Degrees=>B];
          if opt.verbose>2 then print B;
          if opt.verbose>2 then print(first\(findGeneratorsOfSubalgebra B));
c:=codimMA B;
          if opt.verbose>2 then print(codim monomialAlgebraIdeal P,c);
          if opt.verbose>2 then print("codim K[B] = "|net(c));
dc:=decomposeMonomialAlgebra P;
          if opt.verbose>3 then print("Test the regularity function: M2: "|net(-1+regularity sub(monomialAlgebraIdeal P,K[x_0..x_(#B-1)]))|"  Via decomposition: "|net(regularityMA(dc,B)));
deg:=degreeMA P;
          if opt.verbose>2 then print("Degree K[B] = "|net(deg));
bound:=deg-c;
          if opt.verbose>2 then print("EG bound = "|net(bound));
regL:=regularityMA(dc,B);
reg:=first regL;
          if reg>bound then print("Counterexample "|net(B));
          if opt.verbose>2 then print("Regularity = "|net(reg));
          if opt.verbose>1 then print("Regularity/bound = "|net(reg/bound*1.0));
{reg,bound,regL#1,B})



-- non-simplicial version
proveEG=method(Options=>{verbose=>0,CoefficientField=>ZZ/101,file=>"",TerminateAt=>infinity,MaxNumberTests=>infinity})

proveEG(ZZ,ZZ):=opt->(a,d)->(
if opt.file!="" then (fd := openOut(opt.file)) else (fd="");
L:={};
s:=0;
maxs:=binomial(a+d-1,d-1);
for s from 3 to maxs do (
  L=append(L,proveEG(a,d,s,verbose=>opt.verbose,CoefficientField=>opt.CoefficientField,file=>fd,TerminateAt=>opt.TerminateAt,MaxNumberTests=>opt.MaxNumberTests));
);
if opt.file!="" then close fd;
L)

proveEG(ZZ,ZZ,ZZ):=opt->(a,d,s)->(
if a<1 then error("a too small");
if d<2 then error("d too small");
if s<1 then error("s too small");
allRegDeg:={};
if class(opt.file)===File then (
  fd:=opt.file;
) else (
  if opt.file=!="" then fd = openOut(opt.file);
);
K:=opt.CoefficientField;
A0:=entries(a*id_(ZZ^d));
PA:=convexHull transpose matrix A0;
E:=first\entries\transpose\latticePoints PA;
L:=toList(0..s-1);
mx:=0;
maxB:={};
if s>#E then error("s too large");
ntests:=binomial(#E,s);
      if opt.verbose>0 then print("Number of tests "|net(ntests));
ct:=0;
while (class L)=!=Boolean and #L==s do (
 ct=ct+1;
 if opt.verbose>1 and ct%10==0 then print(ct,ntests);
 B:=apply(L,j->E#j);
 --PB:=posHull transpose matrix B;
 --dPB:=PB#"dimension of the cone";
 --if dPB==d then (
  x:=symbol x;
  P:= K[x_0..x_(#B-1), Degrees=>B];
  dc:=decomposeMonomialAlgebra P;
  cd:=codimMA B;
  deg:=degreeMA P;
  bound:=deg-cd;
  regL:=regularityMA(dc,B);
  reg:=regL#0;
  allRegDeg=unique append(allRegDeg,{cd,reg,deg});
  if reg>bound then (
     print("Counterexample "|net(B));return B;
     if opt.file=!="" then fd<<"Counterexample "|net(B)<<endl;
  );
  q:=reg/bound*1.0;
  if q==mx then (
     maxB=append(maxB,{cd,reg,deg,B});
  );
  if q>mx then (
     mx=q;
     maxB={{cd,reg,deg,B}};
           if opt.verbose>2 then (
             print "";
             print("New maximum "|net(mx)|" with regularity "|net(reg)|" and degree "|net(deg));
             print(B);
             if opt.file=!="" then (
               fd<<endl;
               fd<<"New maximum "|net(mx)|" with regularity "|net(reg)|" and degree "|net(deg)<<endl;
               fd<<net(B)<<endl;
             );
     );
  );
 --) else (
   -- print("not full dimensional cone")
 --);
 if mx>=(opt.TerminateAt) or ct>opt.MaxNumberTests then break;
 L=nextSubset(L,#E-1);
);
      if opt.verbose>0 then (
        if opt.TerminateAt==infinity and opt.MaxNumberTests==infinity then (
         print("a = "|net(a)|", d = "|net(d)|", s = "|net(s));
         print "";
         print("Maximal quotient is "|net(mx));
         print "";
         print("All possible (codimension,regularity,degree): "|net(allRegDeg));
         print "";
        ) else (
         print("a = "|net(a)|", d = "|net(d)|", s = "|net(s));
         print "";
         print("Terminated at quotient "|net(mx)|" after "|net(ct)|" tests.");
         print "";
         print("All (codimension,regularity,degree) found so far: "|net(allRegDeg));
         print "";
        );
      );
      if opt.file=!="" then (
       if opt.TerminateAt==infinity and opt.MaxNumberTests==infinity then (
        if opt.verbose==1 then (
         fd<<endl;
         fd<<"(a,d,s) = ("|net(a)|","|net(d)|","|net(s)|")  "|"q = "|net(mx)<<endl;
         fd<<"{(codim,reg,deg)} = {"|net(allRegDeg)|"}"<<endl;
        ) else (
         fd<<endl;
         fd<<"a = "|net(a)|", d = "|net(d)|", s = "|net(s)<<endl;
         fd<<endl;
         fd<<"Maximal quotient is "|net(mx);
         fd<<endl;
         fd<<"The maximum is achieved by (codimension,regularity,degree,B): "<<endl;
         apply(maxB, j->(fd<<j<<endl));
         fd<<endl;
         fd<<"All possible (codimension,regularity,degree): "<<endl;
         fd<<allRegDeg<<endl;
         fd<<"------------------------------------------------------------------"<<endl;
        );
        if class(opt.file)=!=File then close fd;
       ) else (
        if opt.verbose==1 then (
         fd<<endl;
         fd<<"(a,d,s) = ("|net(a)|","|net(d)|","|net(s)|")  "|" terminated at q = "|net(mx)|" after "|net(ct)|" tests."<<endl;
         fd<<"{(codim,reg,deg)} = {"|net(allRegDeg)|"}"<<endl;
        ) else (
         fd<<endl;
         fd<<"a = "|net(a)|", d = "|net(d)|", s = "|net(s)<<endl;
         fd<<endl;
         fd<<"Terminated with maximal quotient "|net(mx)|" after "|net(ct)|" tests.";
         fd<<endl;
         fd<<"The maximum is achieved by (codimension,regularity,degree,B): "<<endl;
         apply(maxB, j->(fd<<j<<endl));
         fd<<endl;
         fd<<"All (codimension,regularity,degree) found so far: "<<endl;
         fd<<allRegDeg<<endl;
         fd<<"------------------------------------------------------------------"<<endl;
        );
        if class(opt.file)=!=File then close fd;
       );
      );
{mx,allRegDeg,maxB});



proveEG(List,ZZ):=opt->(A,c)->(
allRegDeg:={};
if opt.file!="" then fd := openOut(opt.file);
t:=symbol t;
K:=opt.CoefficientField;
PA:=convexHull transpose matrix A;
d:=dim posHull transpose matrix A;
Bmax:=first\entries\transpose\latticePoints PA;
E:=toList(set(Bmax)-set(A));
L:=toList(0..c-1);
mx:=0;
maxB:={};
if c>#E then error("c too large");
ntests:=binomial(#E,c);
      if opt.verbose>0 then print("Number of tests "|net(ntests));
ct:=0;
while (class L)=!=Boolean and #L==c do (
  ct=ct+1;
  if opt.verbose>1 and ct%100==0 then print(ct,ntests);
  B0:=apply(L,j->E#j);
  B:=join(A,B0);
  x:=symbol x;
  --S:= K[x_0..x_(#A-1), Degrees=>A];
  P:= K[x_0..x_(#B-1), Degrees=>B];
  --f:= map(P,S);
  dc:=decomposeMonomialAlgebra P;
  cd:=codimMA B;
  deg:=degreeMA B;
  bound:=deg-cd;
  regL:=regularityMA(dc,B);
  reg:=regL#0;
  allRegDeg=unique append(allRegDeg,{reg,deg});
  if reg>bound then (
     print("Counterexample "|net(B));return B;
     if opt.file!="" then fd<<"Counterexample "|net(B)<<endl;
  );
  q:=reg/bound*1.0;
  if q==mx then (
     maxB=append(maxB,{reg,deg,B});
  );
  if q>mx then (
     mx=q;
     maxB={{reg,deg,B}};
           if opt.verbose>2 then (
             print "";
             print("New maximum "|net(mx)|" with regularity "|net(reg)|" and degree "|net(deg));
             print(B);
             if opt.file!="" then (
               fd<<endl;
               fd<<"New maximum "|net(mx)|" with regularity "|net(reg)|" and degree "|net(deg)<<endl;
               fd<<net(B)<<endl;
             );
     );
  );
  L=nextSubset(L,#E-1);
);
      if opt.verbose>0 then (
         print("A = "|net(A)|", d = "|net(d)|", c = "|net(cd));
         print "";
         print("Maximal quotient is "|net(mx));
         print "";
         print("All possible (regularity,degree): "|net(allRegDeg));
         print "";
      );
      if opt.file!="" then (
         fd<<endl;
         fd<<"A = "|net(A)|", d = "|net(d)|", c = "|net(cd)<<endl;
         fd<<endl;
         fd<<"Maximal quotient is "|net(mx);
         fd<<endl;
         fd<<"The maximum is achieved by (regularity,degree,B): "<<endl;
         apply(maxB, j->(fd<<j<<endl));
         fd<<endl;
         fd<<"All possible (regularity,degree): "<<endl;
         fd<<allRegDeg<<endl;
         fd<<"------------------------------------------------------------------"<<endl;
         close fd;
      );
{mx,allRegDeg,maxB});




-- codimension of a monomial algebra (also non-simplicial)
codimMA=method()
codimMA(List):=(B)->(
P:=posHull transpose matrix B;
d:=P#"dimension of the cone";
--embdim:=P#"ambient dimension";
#B-d)

codimMA(PolynomialRing):=(R)->(
codimMA degrees R)


-- degree of a monomial algebra (also non-simplicial)
degreeMA=method(Options=>{verbose=>0})

-- independent of K
degreeMA(List):=opt->(B)->(
K:=ZZ/2;
x:=symbol x;
R:=K[x_0..x_(#B-1),Degrees=>B];
degreeMA R)


degreeMA(PolynomialRing):=opt->(R)->(
K:= coefficientRing R;
B := degrees R;
L:=findGeneratorsOfSubalgebra B;
A:=first\L;
subV:=apply(L,j-> R_(last j));
S:=K[toSequence subV,Degrees=>A];
f:=map(R,S);
dc:=decomposeMonomialAlgebra(f);
R1:=K[toSequence gens R];
S1:=K[toSequence subV];
degS:=degree sub(monomialAlgebraIdeal S,S1);
        if opt.verbose>4 then (
             degR:=degree sub(monomialAlgebraIdeal R,R1);
             print("Degree monomial algebra ideal B "|net(degR));
             print("Degree monomial algebra ideal A "|net(degS));
             print("Number of components "|net(#(values dc)));
             if (#(values dc))*degS != degR then error("Degree computation is wrong");
        );
(#(values dc))*degS)


degreeMA(PolynomialRing,ZZ):=opt->(R,ndc)->(
K:= coefficientRing R;
B := degrees R;
L:=findGeneratorsOfSubalgebra B;
A:=first\L;
subV:=apply(L,j-> R_(last j));
S:=K[toSequence subV,Degrees=>A];
S1:=K[toSequence subV];
degS:=degree sub(monomialAlgebraIdeal S,S1);
        if opt.verbose>4 then (
             R1:=K[toSequence gens R];
             degR:=degree sub(monomialAlgebraIdeal R,R1);
             print("Degree monomial algebra ideal B "|net(degR));
             print("Degree monomial algebra ideal A "|net(degS));
             print("Number of components "|net(ndc));
             if ndc*degS != degR then error("Degree computation is wrong");
        );
ndc*degS)

randomSemigroups=method()
randomSemigroups(ZZ,ZZ,ZZ,ZZ):=(a,d,c,n)->(
setRandomSeed();
t:=symbol t;
R:=ZZ/2[t_0..t_(d-1)];
IB:=(ideal vars R)^a;
E0:=first\exponents\(first entries mingens IB);
L:={};
for i from 0 to n-1 do (
 dPB:=-1;
 while dPB!=d do (
   E:=E0;
   B:={};
   while #B<c+d do (
     j:=random(#E);
     if #E==0 then error("Codimension too large");
     B=append(B,E#j);
     E=remove(E,j);
   );
   PB:=posHull transpose matrix B;
   dPB=PB#"dimension of the cone";
 );
 L=append(L,B);
);
L);


------------------------------------------------------------------------------------------



beginDocumentation()


--generateAssertions

{*
TEST/// 

      kk=ZZ/101
      A = {1,3,4}
      H = decomposeMonomialCurve A
      R = ring H#0
      use R
      assert( H === new HashTable from {0 => image map(R^1,R^1,{{1}}), 1 => image map(R^{{-1}},R^{{-1}},{{1}}), 2 => image map(R^{{-1}},R^{{-2},{-2}},{{b, a}}), 3 => image map(R^{{-1}},R^{{-1}},{{1}})} )

///
*}

TEST ///

     kk=ZZ/101
     B = {{1,2},{3,0},{0,4},{0,5}}
     S = kk[x_0..x_3, Degrees=> B]
assert( monomialAlgebraIdeal S === ideal(x_0^3*x_2-x_1*x_3^2, x_0^6-x_1^2*x_2^3, x_1*x_2^4-x_0^3*x_3^2, x_2^5-x_3^4) )

///


TEST///
dc=decomposeMonomialAlgebra({1,4,5});
R=ring first first values dc;
r=new HashTable from {matrix {{-1}, {1}} => {ideal(1_R), matrix {{4}, {1}}},
                      matrix {{-2}, {2}} => {ideal(R_1^2,R_0), matrix {{3}, {2}}}, 
                      matrix {{0},{0}} =>   {ideal(1_R), matrix {{0}, {0}}}, 
                      matrix {{1}, {-1}} => {ideal(1_R), matrix{{1}, {4}}}, 
                      matrix {{2}, {-2}} => {ideal(R_1,R_0^2), matrix {{2}, {3}}}};
assert(dc === r)
///



TEST///
dc=values decomposeHomogeneousMonomialAlgebra({{5,0},{4,1},{1,4},{0,5}});
R=ring first first dc;
r=set {{ideal(1_R), 1_QQ},
                      {ideal(R_0^2,R_1), 1_QQ}, 
                      {ideal(1_R), 0_QQ}, 
                      {ideal(1_R), 1_QQ}, 
                      {ideal(R_0,R_1^2), 1_QQ}};
assert((set dc) === r)
///


{*
TEST///
      kk= ZZ/101
      A = {{1,2},{3,0},{0,4},{0,5}}
      H = decomposeSimplicialHomogeneousMonomialAlgebra A
      R = ring H#{0,0,0}
      use R
      assert( H  === new HashTable from {{4,4,2} => image map(R^{{-3}},R^{{-5},{-4}},{{a_1^2, a_0}}), {1,1,3} => image map(R^{{-2}},R^{{-2}},{{1}}), {0,3,2} => image map(R^{{-2}},R^{{-2}},{{1}}), {4,3,3} => image map(R^{{-4}},R^{{-4}},{{1}}), {1,0,4} => image map(R^{{-1}},R^{{-3},{-2}},{{a_1^2, a_0}}), {0,2,3} => image map(R^{{-1}},R^{{-4},{-3}},{{a_0^2*a_2, a_1^2}}), {3,4,3} => image map(R^{{-2}},R^{{-2}},{{1}}), {4,2,4} => image map(R^{{-2}},R^{{-2}},{{1}}), {4,1,0} => image map(R^{{-3}},R^{{-3}},{{1}}), {0,1,4} => image map(R^{{-2}},R^{{-5},{-4}},{{a_0^2*a_2, a_1^2}}), {0,0,0} => image map(R^1,R^1,{{1}}), {3,3,4} => image map(R^{{-3}},R^{{-3}},{{1}}), {4,0,1} => image map(R^{{-2}},R^{{-4},{-3}},{{a_1^2, a_0}}), {3,2,0} => image map(R^{{-2}},R^{{-5},{-4}},{{a_0^2*a_2, a_1^2}}), {2,4,4} => image map(R^{{-2}},R^{{-2}},{{1}}), {2,3,0} => image map(R^{{-3}},R^{{-3}},{{1}}), {3,1,1} => image map(R^{{-3}},R^{{-3}},{{1}}), {3,0,2} => image map(R^{{-1}},R^{{-1}},{{1}}), {1,4,0} => image map(R^{{-2}},R^{{-4},{-3}},{{a_1^2, a_0}}), {2,2,1} => image map(R^{{-2}},R^{{-4},{-3}},{{a_1^2, a_0}}), {1,3,1} => image map(R^{{-3}},R^{{-5},{-4}},{{a_1^2, a_0}}), {2,1,2} => image map(R^{{-3}},R^{{-5},{-4}},{{a_1^2, a_0}}), {0,4,1} => image map(R^{{-1}},R^{{-1}},{{1}}), {1,2,2} => image map(R^{{-1}},R^{{-1}},{{1}}), {2,0,3} => image map(R^{{-2}},R^{{-6},{-5},{-4}},{{a_1^4, a_0*a_1^2, a_0^2}})} )
///
*}


TEST///
A = {{1,2},{3,0},{0,4},{0,5}}
assert(homogenizeSemigroup A =={{1, 2, 2}, {3, 0, 2}, {0, 4, 1}, {0, 5, 0}})
///

TEST///
A = {{1,4}, {2,3}}
assert(adjoinPurePowers A == {{5, 0}, {0, 5}, {1, 4}, {2, 3}})
///

TEST///
     a=3
     B={{a, 0}, {0, a}, {1, a-1}, {a-1, 1}}
     R=QQ[x_0..x_3,Degrees=>B]
assert(     isCohenMacaulayMA R )
     B={{4, 0}, {0, 4}, {1, 3}, {3, 1}}
     R=QQ[x_0..x_3,Degrees=>B]
assert( not     isCohenMacaulayMA R)
///

TEST///
     R=QQ[x_0..x_2,Degrees=>{{2, 0}, {0, 2}, {1, 1}}]
     assert(isGorensteinMA R)
///

TEST///
     B={{3, 0}, {0, 3}, {1, 2}, {2, 1}}
     R=QQ[x_0..x_3,Degrees=>B]
assert( not     isGorensteinMA R)
     R=QQ[x_0..x_4,Degrees=>{{1,0,0},{0,2,0},{0,0,2},{1,0,1},{0,1,1}}]
assert(     isGorensteinMA R)
///

TEST///
     R=QQ[x_0..x_3,Degrees=>{{6,0},{0,6},{4,2},{1,5}}]
assert( not     isBuchsbaumMA R)
     R=QQ[x_0..x_3,Degrees=>{{4,0},{0,4},{3,1},{1,3}}]
assert(     isBuchsbaumMA R)
///

TEST///
     B={{1,0,0},{0,2,0},{0,0,2},{1,0,1},{0,1,1}}
     R=QQ[x_0..x_4,Degrees=>B]
assert(     isSeminormalMA R)
assert(not     isNormalMA R)

///

TEST///
     B={{1,0,0},{0,2,0},{0,0,2},{1,0,1},{0,1,1}}
     R=QQ[x_0..x_4,Degrees=>B]
assert(     isSimplicialMA R)
assert(     isSimplicialMA B)
///


TEST///
     a=4
     B={{a, 0}, {2, a-2}, {1, a-1}, {a-1, 1}}
     assert(findGeneratorsOfSubalgebra B ==  {{{4, 0}, 0}, {{1, 3}, 2}})
///


TEST///
     a=5
     B={{a, 0}, {0, a}, {1, a-1}, {a-1, 1}}
     assert( first regularityMA(B) == 3)
///

TEST ///
     LL={}
     L={}
     while class(L)=!=Boolean do (L=nextSubset(L,3);LL=append(LL,L))
     assert(LL == {{0}, {1}, {2}, {3}, {0, 1}, {0, 2}, {0, 3}, {1, 2}, {1, 3}, {2, 3}, {0, 1, 2}, {0, 1, 3}, {0, 2, 3}, {1, 2, 3}, {0, 1, 2, 3}, true})
///

TEST ///
     B={{2, 2, 1}, {1, 1, 3}, {1, 2, 2}, {2, 0, 3}, {1, 4, 0}, {2, 3, 0}, {1, 3, 1}}
     R=QQ[x_1..x_(#B),Degrees=>B]
    assert( degreeMA R == 6)
///


TEST ///
     B={{2, 2, 1}, {1, 1, 3}, {1, 2, 2}, {2, 0, 3}, {1, 4, 0}, {2, 3, 0}, {1, 3, 1}}
     assert(codimMA B == 4)
///

TEST ///
      L=randomSemigroups(5,3,7,2)
      assert(L== {{{5, 0, 0}, {1, 3, 1}, {1, 4, 0}, {0, 2, 3}, {3, 0, 2}, {0, 1, 4}, {0, 5, 0}, {4, 1, 0}, {2,
0, 3}, {4, 0, 1}}, {{1, 3, 1}, {1, 4, 0}, {2, 3, 0}, {0, 5, 0}, {1, 2, 2}, {3, 1, 1}, {0, 0,
5}, {1, 1, 3}, {4, 0, 1}, {0, 2, 3}}})
///

doc ///
  Key
    MonomialAlgebras
  Headline
    Decompose a monomial algebra as a module over a subalgebra.
  Description
    Text
      {\bf Overview:}
      
      Consider a monoid B in \mathbb{N}^m and a submonid A \subseteq B
      such that K[B] is a finitely generated K[A]-module (with the module structure
      given by inclusion).

      Note that this is equivalent to the condition that the corresponding cones C(A) and C(B) 
      spanned by the monoids are equal. From this it follows that G(B)/G(A) is finite, where
      G(A) and G(B) are the groups generated by the monoids.
      
      The corresponding monomial algebra K[B] is decomposed as a direct sum of monomial ideals in K[A]. In
      
      Le Tuan Hoa, Juergen Stueckrad: Castelnuovo-Mumford regularity of simplicial toric rings,
      Journal of Algebra, Volume 259, Issue 1, 1 January 2003, pages 127-146,
      
      it is shown that this decomposition exists in the case that B is homogeneous and simplicial
      and A is generated by minimal generators of B on the extremal rays of C(B). In particular 
      then K[A] is a Noether normalization of K[B]. 

      The same is true in the general case, see:

      J.Boehm, D. Eisenbud, M. Nitsche: Decomposition of semigroup rings, 2011, preprint.
      
      {\bf Key user functions:}

      {\it Decomposition:}

      @TO decomposeMonomialAlgebra@ -- Decomposition of one monomial algebra over a subalgebra.

      @TO decomposeHomogeneousMonomialAlgebra@ -- Decomposition of one homogeneous monomial algebra over a subalgebra.

      {\it Special classes:}

      @TO isCohenMacaulayMA@ -- Test whether a simplicial monomial algebra is Cohen-Macaulay.

      @TO isGorensteinMA@ -- Test whether a simplicial monomial algebra is Gorenstein.

      @TO isBuchsbaumMA@ -- Test whether a simplicial monomial algebra is Buchsbaum.

      @TO isNormalMA@ -- Test whether a simplicial monomial algebra is normal.

      @TO isSeminormalMA@ -- Test whether a simplicial monomial algebra is seminormal.
   
      @TO isSimplicialMA@ -- Test whether a monomial algebra is simplicial.
      
      {\it Regularity:}

      @TO regularityMA@ -- Compute the regularity via the decomposition.

      @TO proveEGsimplicial@ -- Prove the Eisenbud-Goto conjecture for given a,d,c for simplicial homogeneous monomial algebras.

      @TO proveEG@ -- Prove the Eisenbud-Goto conjecture for given a,d for homogeneous monomial algebras.

      @TO testEGsimplicial@ -- Randomized test for the Eisenbud-Goto conjecture for simplicial homogeneous monomial algebras.

      @TO testEG@ -- Randomized test for the Eisenbud-Goto conjecture for homogeneous monomial algebras.


      {\bf Setup:}

      This package requires Macaulay2 version 1.4 or newer.

      Install this @TO Package@ by doing

      @TO installPackage@("MonomialAlgebras")

      
///

{*
doc ///
  Key
    decomposeMonomialCurve
    (decomposeMonomialCurve,List)
  Headline
    Decomposition of a monomial algebra given by a monomial curve.
  Usage
    decomposeMonomialCurve(B)
  Inputs
    B:List
        of integers containing (dehomogenized) generators of B
  Outputs
    :HashTable
  Description
   Text
     This is a convenient special case of the general function @TO decomposeMonomialAlgebra@.  

     The list B is expected to contain dehomogenized generators of a semigroup.
     This function transforms A into a homogeneous semigroup containing
     powers of the variables. The corresponding monomial algebra
     is decomposed as a direct sum of monomial ideals in its Noether normalization
     with respect to the standard grading.

     For example the homogeneous coordinate ring of the smooth rational quartic decomposes
     as follows:

   Example
     B = {1,3,4};
     decomposeMonomialCurve B
   Text

     Some more smooth space curves:

   Example
     for d from 4 to 10 do (B = {1,d-1,d};print(B,decomposeMonomialCurve B));
///
*}

doc ///
  Key
    monomialAlgebraIdeal
    (monomialAlgebraIdeal, PolynomialRing)
  Headline
    Compute the ideal of a monomial algebra
  Usage
    monomialAlgebraIdeal P
  Inputs
    P: PolynomialRing
       degree monoid is used
  Outputs
    :Ideal
  Description
   Text
      Returns the toric ideal associated to the degree monoid of the polynomial ring P
      as an ideal of P.

   Example
     kk=ZZ/101
     B = {{1,2},{3,0},{0,4},{0,5}}
     S = kk[x_0..x_3, Degrees=> B]
     monomialAlgebraIdeal S
     C = {{1,2},{0,5}}
     P = kk[y_0,y_1, Degrees=> C]
     monomialAlgebraIdeal P
///     

doc ///
  Key
    decomposeMonomialAlgebra
    (decomposeMonomialAlgebra,RingMap)
    (decomposeMonomialAlgebra,PolynomialRing)
    (decomposeMonomialAlgebra,List)
  Headline
    Decomposition of one monomial algebra over a subalgebra
  Usage
    decomposeMonomialAlgebra f
    decomposeMonomialAlgebra R
    decomposeMonomialAlgebra B
  Inputs
    f : RingMap
        between multigraded polynomial rings
    R  : PolynomialRing 
           multigraded, with B = @TO degrees@ R and K = @TO coefficientRing@ R
    B  : List
           In the case B is specified, K is set via the @TO Option@ @TO CoefficientField@.
           If a list of positive integers is given, the function uses @TO adjoinPurePowers@ and @TO homogenizeSemigroup@
           to convert it into a list of elements of \mathbb{N}^2.
  Outputs
    :HashTable
       with the following data: Let B be the degree monoid of the @TO target@ of f and analogously A for the @TO source@.
       The @TO keys@ are representatives of congruence classes in G(B) / G(A).
       The value associated to a key k is a tuple whose first component is an ideal of
       K[A] isomorphic to the K[A]-submodule of K[B] consisting of elements in the class k,
       and whose second component is an element of G(B) that is the translation
       vector between the weights of the generators of the ideal and those of the submodule of
       K[B].

  Description
   Text

     Let K[B] be the monomial algebra of the degree monoid of the @TO target@ of f and 
     analogously K[A] for @TO source@ of f. Assume that K[B] is finite as a K[A]-module.
     
     The monomial algebra K[B] is decomposed as a direct sum of monomial ideals in K[A].

     If R with degrees B is specified then A is computed via @TO findGeneratorsOfSubalgebra@.
    
     Note that the shift chosen by the function depends on the monomial ordering of K[A] (in the non-simplicial case).
     
   Example
      B = {{4,2},{10,6},{3,7},{3,6}}
      A = {{4,2},{10,6},{3,7}}
      S = ZZ/101[x_0..x_(#B-1), Degrees=>B];
      P = ZZ/101[x_0..x_(#A-1), Degrees=>A];     
      f = map(S,P)
      decomposeMonomialAlgebra f
   Text

      Decomposition over a polynomial ring
   Example
      B = {{4,2},{3,7},{10,6},{3,6}}
      A = {{4,2},{3,7}}
      S = ZZ/101[x_0..x_(#B-1), Degrees=>B];
      P = ZZ/101[x_0..x_(#A-1), Degrees=>A];     
      f = map(S,P)
      decomposeMonomialAlgebra f
   Text
      
      Specifying R:
   Example
      B = {{4,2},{10,6},{3,7},{3,6}}
      S = ZZ/101[x_0..x_(#B-1), Degrees=>B];
      decomposeMonomialAlgebra S
   Text
      
      Specifying a monomial curve by a list of positive integers:
   Example
      decomposeMonomialAlgebra {1,4,8,9,11}
   Text
   
      Some simpler examples:
   Example
      B = adjoinPurePowers homogenizeSemigroup {{1,2},{3,0},{0,4},{0,5}}
      A = adjoinPurePowers homogenizeSemigroup {{0,5}}
      S = ZZ/101[x_0..x_(#B-1), Degrees=>B];
      P = ZZ/101[x_0..x_(#A-1), Degrees=>A];     
      f = map(S,P)
      decomposeMonomialAlgebra f      
   Text
   
    Consider the family of smooth monomial curves in $\mathbb{P}^3$, the one of degree $d$
    having parametrization 
    $$
     (s,t) \rightarrow (s^d, s^{d-1}t, st^{d-1} t^d) \in \mathbb{P}^3.
    $$

   Example
     kk=ZZ/101;
     L= for d from 4 to 10 list (f= map(kk[x_0..x_3,Degrees=>{{d,0},{d-1,1},{1,d-1},{0,d}}], kk[x_0,x_3,Degrees=>{{d,0},{0,d}}]));
     print\decomposeMonomialAlgebra\L
   Text
     
     See also @TO decomposeHomogeneousMonomialAlgebra@:
        
   Example
     decomposeHomogeneousMonomialAlgebra {{2,0,1},{0,2,1},{1,1,1},{2,2,1},{2,1,1},{1,4,1}}
///


doc ///
  Key
    decomposeHomogeneousMonomialAlgebra
    (decomposeHomogeneousMonomialAlgebra,RingMap)
    (decomposeHomogeneousMonomialAlgebra,PolynomialRing)
    (decomposeHomogeneousMonomialAlgebra,List)
  Headline
    Decomposition of one monomial algebra over a subalgebra
  Usage
    decomposeHomogeneousMonomialAlgebra f
    decomposeHomogeneousMonomialAlgebra R
    decomposeHomogeneousMonomialAlgebra B
  Inputs
    f : RingMap
        between multigraded polynomial rings where the degrees are minimal homogeneous generators of the degree monoid.
    R  : PolynomialRing 
           multigraded, with B = @TO degrees@ R homogeneous generators of a semigroup and K = @TO coefficientRing@ R
    B  : List
           In the case B is specified, K is set via the @TO Option@ @TO CoefficientField@.
           If a list of positive integers is given, the function uses @TO adjoinPurePowers@ and @TO homogenizeSemigroup@
           to convert it into a list of elements of \mathbb{N}^2.
  Outputs
    :HashTable
       with the following data: Let B be the degree monoid of the @TO target@ of f and analogously A for the @TO source@.
       The @TO keys@ are representatives of congruence classes in G(B) / G(A).
       The value associated to a key k is a tuple whose first component is a monomial ideal of
       K[A] isomorphic to the K[A]-submodule of K[B] consisting of elements in the class k,
       and whose second component is an element of @TO ZZ@ that is the twist between
       the ideal and the submodule of K[B] with respect to the standard grading.

  Description
   Text

     Let K[B] be the monomial algebra of the degree monoid of the @TO target@ of f and 
     let analogously K[A] for @TO source@ of f. Assume that K[B] is finite as a K[A]-module.
     
     The monomial algebra K[B] is decomposed as a direct sum of monomial ideals in K[A] with respect to the standard grading.

     If B or R with degrees B is specified then A is computed via @TO findGeneratorsOfSubalgebra@.

     Note that the shift chosen by the function depends on the monomial ordering of K[A] (in the non-simplicial case).
     
   Example
      B = {{4,0},{3,1},{1,3},{0,4}}
      S = ZZ/101[x_0..x_(#B-1), Degrees=>B];
      decomposeHomogeneousMonomialAlgebra S
      decomposeHomogeneousMonomialAlgebra B
   Text
   
   Example
       decomposeHomogeneousMonomialAlgebra {{2,0,1},{0,2,1},{1,1,1},{2,2,1},{2,1,1},{1,4,1}}
///



{*
doc ///
  Key
    decomposeSimplicialHomogeneousMonomialAlgebra
    (decomposeSimplicialHomogeneousMonomialAlgebra,List)
  Headline
    Decomposition for a simplicial homogeneous monomial algebra.
  Usage
    decomposeSimplicialHomogeneousMonomialAlgebra(A)
  Inputs
    A:List
        containing generators of A
  Outputs
    :HashTable
       The @TO keys@ are representatives of congruence classes in G(A)
       modulo the subgroup generated by the degrees of the Noether normalization T.
       The value associated to a key k is a submodule of a rank 1 free module over
       T isomorphic to the T-submodule of K[A] consisting of elements in the class k.
  Description
   Text
     The list A contains dehomogenized generators of a semigroup.
     This function transforms A into a homogeneous semigroup containing
     powers of the variables. The corresponding monomial algebra
     is decomposed as a direct sum of ideals in its Noether normalization.
     
   Example
      A = {{1,2},{3,0},{0,4},{0,5}}
      decomposeSimplicialHomogeneousMonomialAlgebra (A)
   Text
     Using @TO decomposeMonomialAlgebra@ we could do the same example as follows:
   Example
      B = adjoinPurePowers homogenizeSemigroup A
      C = adjoinPurePowers homogenizeSemigroup {{0,5}}
      S = ZZ/101[x_0..x_(#B-1), Degrees=>B]
      P = ZZ/101[x_0..x_(#C-1), Degrees=>C]      
      f = map(S,P)
      decomposeMonomialAlgebra f      
   Text
   
    Consider the family of smooth monomial curves in $P^3$, the one of degree $d$
    having parametrization 
    $
     (s,t) \mapsto (s^d, s^{d-1}t, st^{d-1} t^d)\in P^3.
    $
   
   Example
     for d from 4 to 10 do (A = {{d,0},{d-1,1},{1,d-1},{0,d}}; print decomposeSimplicialHomogeneousMonomialAlgebra A)
   Text
     The case of homogeneous monomial curves can be done with simpler notation using 
     the command @TO decomposeMonomialCurve@
///
*}

doc ///
  Key
    homogenizeSemigroup
    (homogenizeSemigroup,List)
  Headline
    Homogenize generators of a semigroup.
  Usage
    homogenizeSemigroup(A)
  Inputs
    A:List
        of lists of integers
  Outputs
    :List
  Description
   Text
      Homogenize the generators of a semigroup adding an additional coordinate.
   Example
      A = {{1,2},{3,0},{0,4},{0,5}}
      homogenizeSemigroup A
///



doc ///
  Key
    adjoinPurePowers
    (adjoinPurePowers, List)    
  Headline
    adjoin semigroup elements corresponding to pure powers of variables
  Usage
    adjoinPurePowers A
  Inputs
    A:List
        of lists of ZZ, containing generators of A, all of the same degree d.
  Outputs
    :List
        of lists of ZZ. Same as A, but with elements of the form (d,0...), (0,d,0...)...
	prepended.
  Description
   Text
      Used to simplify the input of complicated homogeneous monoids.
   Example
      A = {{1,4}, {2,3}}
      adjoinPurePowers A
///


      
doc ///
  Key
    CoefficientField
    --[decomposeMonomialCurve,CoefficientField]
    [decomposeMonomialAlgebra,CoefficientField]
    --[decomposeSimplicialHomogeneousMonomialAlgebra,CoefficientField]
    [decomposeHomogeneousMonomialAlgebra,CoefficientField]
    [testEGsimplicial,CoefficientField]
    [proveEGsimplicial,CoefficientField]
    [proveEG,CoefficientField]
    [testEG,CoefficientField]
    [regularityMA,CoefficientField]
  Headline
    Option to set the coefficient field.
  Description
   Text
    This option can be used to set the coefficient field of the polynomial rings
    created by @TO decomposeMonomialAlgebra@, @TO decomposeHomogeneousMonomialAlgebra@.
    
    The standard option is @TO ZZ@/101.
///


doc ///
  Key
    verbose
    --[decomposeMonomialCurve,verbose]
    [decomposeMonomialAlgebra,verbose]
    [proveEGsimplicial,verbose]
    [proveEG,verbose]
    [testEGsimplicial,verbose]
    [testEG,verbose]
    [degreeMA,verbose]
    [decomposeHomogeneousMonomialAlgebra,verbose]
  Headline
    Option to print intermediate results.
  Description
   Text
    Option to print intermediate results. The standard value is 0 (= do not print).
///


doc ///
  Key
    file
    [proveEGsimplicial,file]
    [proveEG,file]
  Headline
    Option to write the results to a file.
  Description
   Text
    Option to specify a @TO String@ with a file name to save the results.
///

doc ///
  Key
    ReturnBA
    [decomposeMonomialAlgebra,ReturnBA]
    [decomposeHomogeneousMonomialAlgebra,ReturnBA]
  Headline
    Option to return the set B_A.
  Description
   Text
    Option to return the set B_A. Used by @TO isSeminormalMA@ and @TO isNormalMA@.
///


doc ///
  Key
    isCohenMacaulayMA
    (isCohenMacaulayMA,PolynomialRing)
    (isCohenMacaulayMA,List)
  Headline
    Test whether a simplicial monomial algebra is Cohen-Macaulay.
  Usage
    isCohenMacaulayMA R
    isCohenMacaulayMA B
  Inputs
    R : PolynomialRing
          with B = @TO degrees@ R and K = @TO coefficientRing@ R
    B: List
  Outputs
    :Boolean
  Description
   Text
     Test whether the simplicial monomial algebra K[B] is Cohen-Macaulay.

     Note that this condition does not depend on K.

   Example
     a=3
     B={{a, 0}, {0, a}, {1, a-1}, {a-1, 1}}
     R=QQ[x_0..x_3,Degrees=>B]
     isCohenMacaulayMA R
     decomposeMonomialAlgebra R
   Text

   Example
     a=4
     B={{a, 0}, {0, a}, {1, a-1}, {a-1, 1}}
     R=QQ[x_0..x_3,Degrees=>B]
     isCohenMacaulayMA R
     decomposeMonomialAlgebra R
///


doc ///
  Key
    isGorensteinMA
    (isGorensteinMA,PolynomialRing)
    (isGorensteinMA,List)
  Headline
    Test whether a simplicial monomial algebra is Gorenstein.
  Usage
    isGorensteinMA R
    isGorensteinMA B
  Inputs
    R : PolynomialRing
          with B = @TO degrees@ R and K = @TO coefficientRing@ R
  Outputs
    :Boolean
  Description
   Text
     Test whether the simplicial monomial algebra K[B] is Gorenstein.

     Note that this condition does not depend on K.

     Gorenstein:
     
   Example
     R=QQ[x_0..x_2,Degrees=>{{2, 0}, {0, 2}, {1, 1}}]
     isGorensteinMA R
   Text

     Not Gorenstein:
     
   Example
     B={{3, 0}, {0, 3}, {1, 2}, {2, 1}}
     R=QQ[x_0..x_3,Degrees=>B]
     isGorensteinMA R
   Text

     Not even Cohen-Macaulay:

   Example
     B={{4, 0}, {0, 4}, {1, 3}, {3, 1}}
     R=QQ[x_0..x_3,Degrees=>B]
     isGorensteinMA R
     isCohenMacaulayMA R
   Text

     Gorenstein:

   Example
     R=QQ[x_0..x_4,Degrees=>{{1,0,0},{0,2,0},{0,0,2},{1,0,1},{0,1,1}}]
     isGorensteinMA R
     decomposeMonomialAlgebra R
///

doc ///
  Key
    isBuchsbaumMA
    (isBuchsbaumMA,PolynomialRing)
    (isBuchsbaumMA,List)
  Headline
    Test whether a simplicial monomial algebra is Buchsbaum.
  Usage
    isBuchsbaumMA R
    isBuchsbaumMA B
  Inputs
    R : PolynomialRing
          with B = @TO degrees@ R and K = @TO coefficientRing@ R
  Outputs
    :Boolean
  Description
   Text
     Test whether the simplicial monomial algebra K[B] is Buchsbaum.

     Note that this condition does not depend on K.

   Example
     R=QQ[x_0..x_3,Degrees=>{{6,0},{0,6},{4,2},{1,5}}]
     isBuchsbaumMA R
     decomposeMonomialAlgebra R
   Text

   Example
     R=QQ[x_0..x_3,Degrees=>{{4,0},{0,4},{3,1},{1,3}}]
     isBuchsbaumMA R
     decomposeMonomialAlgebra R
   Text

   Example
     R=QQ[x_0..x_3,Degrees=>{{5,0},{0,5},{4,1},{1,4}}]
     isBuchsbaumMA R
     decomposeMonomialAlgebra R
///


doc ///
  Key
    isSeminormalMA
    (isSeminormalMA,PolynomialRing)
    (isSeminormalMA,List)
  Headline
    Test whether a simplicial monomial algebra is seminormal.
  Usage
    isSeminormalMA R
    isSeminormalMA B
  Inputs
    R : PolynomialRing
          with B = @TO degrees@ R and K = @TO coefficientRing@ R
  Outputs
    :Boolean
  Description
   Text
     Test whether the simplicial monomial algebra K[B] is seminormal.

     Note that this condition does not depend on K.

   Example
     B={{1,0,0},{0,2,0},{0,0,2},{1,0,1},{0,1,1}}
     R=QQ[x_0..x_4,Degrees=>B]
     isSeminormalMA R
     isNormalMA R
///


doc ///
  Key
    isNormalMA
    (isNormalMA,PolynomialRing)
    (isNormalMA,List)
  Headline
    Test whether a simplicial monomial algebra is normal.
  Usage
    isNormalMA R
    isNormalMA B
  Inputs
    R : PolynomialRing
          with B = @TO degrees@ R and K = @TO coefficientRing@ R
  Outputs
    :Boolean
  Description
   Text
     Test whether the simplicial monomial algebra K[B] is normal.

     Note that this condition does not depend on K.

   Example
     B={{1,0,0},{0,2,0},{0,0,2},{1,0,1},{0,1,1}}
     R=QQ[x_0..x_4,Degrees=>B]
     isNormalMA R
     isSeminormalMA R
///



doc ///
  Key
    isSimplicialMA
    (isSimplicialMA,List)
    (isSimplicialMA,PolynomialRing)
  Headline
    Test whether a monomial algebra is simplicial.
  Usage
    isSimplicialMA R
    isSimplicialMA B
  Inputs
    R : PolynomialRing
          with B = @TO degrees@ R and K = @TO coefficientRing@ R
  Outputs
    :Boolean
  Description
   Text
     Test whether the monomial algebra K[B] is simplicial, that is, the cone C(B) is spanned by
     linearly independent vectors.

     Note that this condition does not depend on K.

   Example
     B={{1,0,0},{0,2,0},{0,0,2},{1,0,1},{0,1,1}}
     R=QQ[x_0..x_4,Degrees=>B]
     isSimplicialMA R
     isSimplicialMA B
   Text

   Example
     B={{1,0,1},{0,1,1},{1,1,1},{0,0,1}}
     R=QQ[x_0..x_3,Degrees=>B]
     isSimplicialMA R
     isSimplicialMA B
///



doc ///
  Key
    findGeneratorsOfSubalgebra
    (findGeneratorsOfSubalgebra,List)
  Headline
    Find submonoid corresponding to the convex hull.
  Usage
    findGeneratorsOfSubalgebra B
  Inputs
    B : List
  Outputs
    :List
  Description
   Text
     Compute on each ray of C(B) one element in B with minimal coordinate sum.

   Example
     a=3
     B={{a, 0}, {0, a}, {1, a-1}, {a-1, 1}}
     findGeneratorsOfSubalgebra B
   Text

   Example
     a=4
     B={{a, 0}, {2, a-2}, {1, a-1}, {a-1, 1}}
     findGeneratorsOfSubalgebra B
   Text

   Example
     B={{3, 0}, {2, 0}, {1, 1}, {0, 2}}
     findGeneratorsOfSubalgebra B
///


doc ///
  Key
    regularityMA
    (regularityMA,PolynomialRing)
    (regularityMA,List)
    (regularityMA,HashTable,List)
  Headline
    Compute regularity from decomposition
  Usage
    regularityMA R
    regularityMA B
    regularityMA(dc,B)
  Inputs
    R  : PolynomialRing 
           with B = @TO degrees@ R and K = @TO coefficientRing@ R
    B  : List
           In the case B is specified, K is set via the @TO Option@ @TO CoefficientField@.
           If a list of positive integers is given, the function uses @TO adjoinPurePowers@ and @TO homogenizeSemigroup@
           to convert it into a list of elements of \mathbb{N}^2.
    dc : HashTable
           with the decomposition computed by @TO decomposeMonomialAlgebra@ R
  Outputs
    :ZZ
       list of the regularity and list of the summands achieving the maximum
  Description
   Text

     Compute the regularity of K[B] from the decomposition of the homogeneous monomial algebra K[B].

     We assume that B=<b_{1},...,b_{r}> is homogeneous and minimally generated by b_{1},...,b_{r}, that is, there is a group homomorphism \phi : G(B) \to \mathbb{Z} such that \phi(b_{i}) = 1 for all i.

     In the case of a monomial curve an ad hoc formula for the regularity of the components is used (if R or B is given).

     Specifying R:

   Example
     a=5
     B={{a, 0}, {0, a}, {1, a-1}, {a-1, 1}}
     R=QQ[x_0..x_3,Degrees=>B]
     dc=decomposeMonomialAlgebra R
     regularityMA(dc,B)
   Text

     Specifying the decomposition dc:

   Example
     a=5
     B={{a, 0}, {0, a}, {1, a-1}, {a-1, 1}}
     R=QQ[x_0..x_3,Degrees=>B]
     dc=decomposeMonomialAlgebra R
     regularityMA(dc,B)
   Text

     Specifying B:

   Example
     a=5
     B={{a, 0}, {0, a}, {1, a-1}, {a-1, 1}}
     regularityMA B
   Text

     Compare to

   Example
     I=ker map(QQ[s,t],QQ[x_0..x_3],matrix {{s^a,t^a,s*t^(a-1),s^(a-1)*t}})
     -1+regularity I
///



doc ///
  Key
    testEGsimplicial
    (testEGsimplicial,ZZ,ZZ,ZZ)
    (testEGsimplicial,List,List,ZZ)
  Headline
    Test the Eisenbud-Goto conjecture for simplicial homogeneous monomial algebras
  Usage
    testEGsimplicial(a,d,c)
    testEGsimplicial(aL,dL,n)
  Inputs
    a : ZZ
         the coordinate sum of each generator
    d : ZZ
         the dimension
    c : ZZ
         the codimension with c <= binomal(a+d-1,d-1) - d. Note that the Eisenbud-Goto conjecture holds for c<=deg K[B]/a since reg K[B]<=c*(a-1).
    aL : List
         of a to check
    dL : List
         of d to check
    n:ZZ
       number of runs
  Outputs
    :List
       with the regularity, the Eisenbud-Goto bound, a list with summands achieving the maximum, and the generators of the monoid B.
  Description
   Text
     Test the Eisenbud-Goto conjecture for simplicial homogeneous monomial algebras.
     We consider a random simplicial homogeneous monomial algebra of codimension c and dimension d
     such that the standard vectors a*e_i are among the Hilbert basis.

     If lists aL and dL are specified the test is run for all c = 1,...,binomal(a+d-1,d-1) - d.

     The option @TO verbose@ can be used to print the quotient of the maximum regularity achieved by the Eisenbud-Goto bound (i.e., counterexample would be >1).

   Example
     a=5
     d=3
     c=11
     print\apply(10,j->testEGsimplicial(a,d,c));
   Text

   Example
     testEGsimplicial({3,4},{3},3,verbose=>1);
///


doc ///
  Key
    testEG
    (testEG,ZZ,ZZ,ZZ)
    (testEG,List)
  Headline
    Test the Eisenbud-Goto conjecture for a homogeneous monomial algebras
  Usage
    testEG(a,d,g)
    testEG(B)
  Inputs
    a : ZZ
         the coordinate sum of each generator in \mathbb{N}^d
    d : ZZ
         positive
    s : ZZ
         the number of generators
    B : List
           In this case the test is run just for K[B] and K is set via the @TO Option@ @TO CoefficientField@.
  Outputs
    :List
       with the regularity, the Eisenbud-Goto bound, a list with summands achieving the maximum, and the generators of the monoid B.
  Description
   Text
     Test the Eisenbud-Goto conjecture for homogeneous monomial algebras.
     We consider a random homogeneous monomial algebra K[B] with B \subseteq \mathbb{N}^d where the elements of B have coordinate sum a 
     and B has s generators.

     The regularity is computed via @TO (regularityMA,PolynomialRing)@ from the decomposition of K[B] over K[A], where A is obtained via @TO findGeneratorsOfSubalgebra@

     Degree and codimension are computed via @TO degreeMA@ and @TO codimMA@.

     The option @TO verbose@ can be used to print intermediate results.


   Example
     a=4
     d=3
     c=7
     print testEG(a,d,c);
   Text

   Example
     print testEG(a,d,c);
   Text

   Example
     print testEG(a,d,c);
   Text
     
   Example
     B = {{2, 2, 1}, {1, 1, 3}, {1, 2, 2}, {2, 0, 3}, {1, 4, 0}, {2, 3, 0}, {1, 3, 1}}
     A = first\findGeneratorsOfSubalgebra B
     testEG B
///


doc ///
  Key
    nextSubset
    (nextSubset,List,ZZ)
  Headline
    Iterator to produce subsets.
  Usage
    nextSubset(L,n)
  Inputs
    L : List
          of strictly increasing integers <=n
    n : ZZ
         positive
         If all subsets have been produced the function returns the @TO Boolean@ @TO true@.
  Outputs
    :List
  Description
   Text
     Iterator to produce inductively all subsets of {0,...,n} starting with L=\{\}.

   Example
     L={}
     while class(L)=!=Boolean do (L=nextSubset(L,3);print L)
///



doc ///
  Key
    proveEGsimplicial
    (proveEGsimplicial,ZZ,ZZ,ZZ)
  Headline
    Prove the Eisenbud-Goto conjecture for a class of simplicial homogeneous monomial algebras
  Usage
    proveEGsimplicial(a,d,c)
  Inputs
    a : ZZ
         the coordinate sum of each generator
    d : ZZ
         the dimension
    c : ZZ
         the codimension with c <= binomal(a+d-1,d-1) - d. Note that the Eisenbud-Goto conjecture holds for c<=deg K[B]/a since reg K[B]<=c*(a-1).
  Outputs
    :List
       with the maximum quotient of the regularity by the Eisenbud-Goto bound,
       the set of all (regularity,degree) appearing, and
       a list of all B such that K[B] achieves the maximum quotient.
  Description
   Text
     Prove the Eisenbud-Goto conjecture for all simplicial homogeneous monomial algebras 
     (such that the standard vectors a*e_i are among the Hilbert basis) for given a,d and c.
 
     The option @TO verbose@=>0,1,2,3 can be used to print intermediate results.

     The option @TO file@ can be used to specify a file name to save the results.

     Monomial curve example:

   Example
     proveEGsimplicial(6,2,2,verbose =>1)
   Text
   
     Simplicial homogeneous monomial algebras with d=3 and a=3, any c:

   Example
     d=3
     a=3
     maxc=binomial(a+d-1,d-1) - d;
     apply(maxc,j->proveEGsimplicial(a,d,j+1,verbose =>1));
///



doc ///
  Key
    proveEG
    (proveEG,ZZ,ZZ)
    (proveEG,ZZ,ZZ,ZZ)
    (proveEG,List,ZZ)
  Headline
    Prove the Eisenbud-Goto conjecture for a class of homogeneous monomial algebras
  Usage
    proveEG(a,d)
    proveEG(a,d,s)
    proveEG(A,r)
  Inputs
    a : ZZ
        positive, the coordinate sum of each generator in \mathbb{N}^d
    d : ZZ
        positive
    s : ZZ
        positive, the number of generators of B
    A : List
         homogeneous generators of a semigroup
    r : ZZ
         the number of additional generators of B to be choosen in the convex hull of A.
  Outputs
    :List
       with the maximum quotient of the regularity by the Eisenbud-Goto bound,
       the set of all (codimension, regularity, degree) appearing, and
       a list of all B such that K[B] achieves the maximum quotient.
       If s is not specified, then a list of this returned, the entries corresponding to the 
       possible values of s.
  Description
   Text
     Prove the Eisenbud-Goto conjecture for all homogeneous monomial algebras K[B]
     where the generators of B lie in \mathbb{N}^d and have coordiante sum a.

     It is also possible to specify in addition to this the number s of generators of B.

     The option @TO verbose@=>0,1,2,3 can be used to print intermediate results.

     The option @TO file@ can be used to specify a file name to save the results.

     Another way to use the function is to fix a cone:

     Prove the Eisenbud-Goto conjecture for all homogeneous monomial algebras K[B] with B
     consisting of the vertices of the convex hull of A and r additional generators in the convex hull of A.
 
     Note: In the non-simplicial setting r may not be the codimension (see @TO codimMA@).

     Prove Eisenbud-Goto for a=2 and d=3:

   Example
     proveEG(2,3,verbose =>1)
   Text

     Monomial curve example:

   Example
     proveEG({{5,0},{0,5}},2,verbose =>1)
   Text

     Prove Eisenbud-Goto for a=3 and d=3 (simplicial):

   Example
     d=3
     a=3
     maxc=binomial(a+d-1,d-1) - d;
     apply(maxc,j->proveEG({{a,0,0},{0,a,0},{0,0,a}},j+1,verbose =>1));
   Text
   
     Non-Simplicial example specifying a cone:

   Example
     first proveEG({{2, 3, 0}, {1, 4, 0}, {2, 0, 3}, {1, 1, 3}},2 )
     first proveEG({{2, 3, 0}, {1, 4, 0}, {2, 0, 3}, {1, 1, 3}},3 )
     first proveEG({{2, 3, 0}, {1, 4, 0}, {2, 0, 3}, {1, 1, 3}},4 )
     proveEG({{2, 3, 0}, {1, 4, 0}, {2, 0, 3}, {1, 1, 3}},3 )
///



doc ///
  Key
    degreeMA
    (degreeMA,PolynomialRing)
    (degreeMA,PolynomialRing,ZZ)
    (degreeMA,List)
  Headline
    Degree of a monomial algebra.
  Usage
    degreeMA(R)
    degreeMA(R,r)
    degreeMA(B)
    degreeMA(B,r)
  Inputs
    R : PolynomialRing
          with B = @TO degrees@ R and K = @TO coefficientRing@ R.
    B : List
    r : ZZ
          number of components in the decomposition obtained from @TO decomposeMonomialAlgebra@
  Outputs
    :ZZ
  Description
   Text
     Compute the degree of the homogeneous monomial algebra K[B].

     As the result is independent of K it is possible to specify just B.

     If already known one can also specify the number of components r, otherwise r is computed.

   Example
     B={{2, 2, 1}, {1, 1, 3}, {1, 2, 2}, {2, 0, 3}, {1, 4, 0}, {2, 3, 0}, {1, 3, 1}}
     R=QQ[x_1..x_(#B),Degrees=>B]
     degreeMA R
///

doc ///
  Key
    codimMA
    (codimMA,List)
    (codimMA,PolynomialRing)
  Headline
    Codimension of a monomial algebra.
  Usage
    codimMA(R)
    codimMA(B)
  Inputs
    R:PolynomialRing
          with B = @TO degrees@ R and K = @TO coefficientRing@ R.
    B:List
  Outputs
    :ZZ
  Description
   Text
     Compute the codimension of the homogeneous monomial algebra K[B].

     As the result is independent of K it is possible to specify just B.

   Example
     B={{2, 2, 1}, {1, 1, 3}, {1, 2, 2}, {2, 0, 3}, {1, 4, 0}, {2, 3, 0}, {1, 3, 1}}
     codimMA B
///


doc ///
  Key
    TerminateAt
    [proveEG,TerminateAt]
  Headline
    Option to terminate when reaching the Eisenbud-Goto bound.
  Description
   Text
    This option can be set to a value b in @TO RR@. Then the @TO proveEG@
    terminates, when finding an example with regularity/(degree-codim) bigger or equal to a.

    So for example setting the value to 1 the program will terminate when finding an example
    which shows that the Eisenbud-Goto bound is sharp.
///

doc ///
  Key
    MaxNumberTests
    [proveEG,MaxNumberTests]
  Headline
    Option to terminate after a certain number of tests.
  Description
   Text
    Setting this option to a value b in @TO ZZ@ will terminate @TO proveEG@ after b tests.
///




doc ///
  Key
    randomSemigroups
    (randomSemigroups,ZZ,ZZ,ZZ,ZZ)
  Headline
    Generate random semigroups.
  Usage
    randomSemigroups(a,d,c,n)
  Inputs
    a:ZZ
       positive
    d:ZZ
       positive
    c:ZZ
       positive
    n:ZZ
       positive
  Outputs
    L:List
  Description
   Text
     Generate a list L of n random generating sets of semigroups B in \mathbb{N}^d of full
     dimension with coordinate sum a and codimension c.

     The function uses @TO setRandomSeed@ to produce always the same L.
     
   Example
     randomSemigroups(5,3,7,2)
///


{*

loadPackage("MonomialAlgebras");
d=3
a=5
ub=binomial(a+d-1,d-1) - d;print ub;
for c from 1 to 7 do (
   proveEGsimplicial(a,d,c,verbose =>2,file=>"testEG"|toString(a)|toString(d)|toString(c)|".m2")
);


loadPackage("MonomialAlgebras");
d=3
a=5
ub=binomial(a+d-1,d-1) - d;print ub;
for c from 15 to ub do (
   proveEGsimplicial(a,d,c,verbose =>2,file=>"testEG"|toString(a)|toString(d)|toString(c)|".m2")
);

loadPackage("MonomialAlgebras");
d=3
a=5
ub=binomial(a+d-1,d-1) - d;print ub;
for c from 12 to 14 do (
   proveEGsimplicial(a,d,c,verbose =>2,file=>"testEG"|toString(a)|toString(d)|toString(c)|".m2")
);


loadPackage("MonomialAlgebras");
d=3
a=5
ub=binomial(a+d-1,d-1) - d;print ub;
for c from 10 to 11 do (
   proveEGsimplicial(a,d,c,verbose =>2,file=>"testEG"|toString(a)|toString(d)|toString(c)|".m2")
);


loadPackage("MonomialAlgebras");
d=3
a=5
ub=binomial(a+d-1,d-1) - d;print ub;
for c from 8 to 9 do (
   proveEGsimplicial(a,d,c,verbose =>2,file=>"testEG"|toString(a)|toString(d)|toString(c)|".m2")
);



*}




{*
restart
uninstallPackage("MonomialAlgebras")
installPackage("MonomialAlgebras",RerunExamples=>true);
installPackage("MonomialAlgebras");
check MonomialAlgebras
viewHelp MonomialAlgebras
*}
