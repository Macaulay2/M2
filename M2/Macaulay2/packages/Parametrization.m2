-- -*- coding: utf-8 -*- this has to be on the first line
newPackage(
	"Parametrization",
    	Version => "0.6", 
    	Date => "August 25, 2010",
    	Authors => {{Name => "Janko Boehm", 
		  Email => "boehm@mathematik.uni-kl.de", 
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer/jb/"}},
    	Headline => "rational parametrization of rational plane curves and related computations",
	Keywords => {"Commutative Algebra"},
    	DebuggingMode => false,
	CacheExampleOutput => true,
	AuxiliaryFiles => true,
	PackageExports => {"AdjointIdeal"},
	PackageImports => {"MapleInterface"}
    	)

-- For information see documentation key "Parametrization" below.


export {"parametrize","mapToRNC","isomorphicProjectionOfRNC","rParametrizeRNC","rParametrizePlaneCurve","rationalPointOnConic",
"rParametrizeConic","invertBirationalMap","modularSquareRoot","legendreSymbol","chineseRemainder","modularPower","modularInverse",
"testParametrization","parametrizeConic","vb"}

-------------------------------------------------------------



-- map rational plane curve C birationally to rational normal curve
-- 1st argument: ideal of C
-- 2nd argument: adjoint ideal of C
--
mapToRNC=method()

mapToRNC(Ideal,Ideal):=(I0,J)->(
Rx:=ring I0;
K:=coefficientRing Rx;
m:=degree I0-2;
linsys:=mingens(intersect(J,(ideal vars ring J)^m));
if rank(source linsys)>m+1 then (
   m=degree I0-3;
   linsys=mingens(intersect(J,(ideal vars ring J)^m));
);
m=-1+rank source linsys;
KR:=Rx/I0;
linsysC:=substitute(linsys,KR);
x:=symbol x;
Rrnc:=K[x_0..x_m];
phi:=map(KR,Rrnc,linsysC);
ideal mingens ker phi)

mapToRNC(Ideal):=(I0)->(
J:=adjointIdeal I0;
mapToRNC(I0,J));


mapToRNC(Ideal,Matrix):=(I0,linsys)->(
Rx:=ring I0;
m:=-1+rank source linsys;
K:=coefficientRing Rx;
KR:=Rx/I0;
linsysC:=substitute(linsys,KR);
x:=symbol x;
Rrnc:=K[x_0..x_m];
phi:=map(KR,Rrnc,linsysC);
ideal mingens ker phi)


-- computes from a rational normal curve in PP^n; n>=3
-- an isomorphism
--
--          f
-- PP^n < C -> C' < PP^(n-2)
--
-- 1st argument: ideal of a rational normal curve
--
-- Output: A list of
-- ideal of rational normal curve C'
-- isomorphism f

rncmap=method()
rncmap(Ideal):=(i1)->(
n:=(rank source vars ring i1)-1;
cc:=res coker gens i1;
j:=length cc;
m:=cc.dd_j;
Ra:=ring i1;
K:=coefficientRing Ra;
S:=Ra/i1;
trm:=gens(ker(substitute(m,S)));
lsy:=matrix entries transpose trm_{0};
x:=symbol x;
Rb:=K[x_0..x_(n-2)];
f:=map(S,Rb,lsy);
ngl:=ideal mingens ker f;
{ngl,substitute(lsy,Ra)})


-- instead of the following function
-- we use rncmap (see below)
--
-- computes from a rational normal curve in PP^n
-- a sequence of isomorphisms between rational normal curves
--
--            f_j
-- PP^j < C_j -> C_(j-2) < PP^(j-2), ...
-- terminating with PP^1 or a conic in PP^2
--
-- 1st argument: ideal of a rational normal curve
--
-- Output: A list of
-- List of ideals of the rational normal curves
-- List of isomorphisms f_j
 
rnclist=method()
rnclist(Ideal):=(I)->(
n:=(rank source vars ring I)-1;
I1:=I;
Ilist:={};
maplist:={};
L:={};
while (n>=3) do (
L=rncmap(I1);
I1=L#0;
Ilist=append(Ilist,I1);
n=(rank source vars ring I1)-1;
maplist=append(maplist,L#1);
);
{Ilist,maplist});


-- computes from a rational normal curve in PP^n (n>=3)
-- a sequence of isomorphisms between rational normal curves
--
--            f_j
-- PP^j < C_j -> C_(j-2) < PP^(j-2), ...
-- terminating with PP^1 or a conic in PP^2
--
-- 1st argument: ideal of a rational normal curve
--
-- Output:
-- List of isomorphisms f_j


rncmaps=method()
rncmaps(Ideal):=(I)->(
n:=(rank source vars ring I)-1;
I1:=I;
Ilist:={};
maplist:={};
L:={};
while (n>=3) do (
L=rncmap(I1);
I1=L#0;
Ilist=append(Ilist,I1);
n=(rank source vars ring I1)-1;
maplist=append(maplist,L#1);
);
maplist);



-- composes the maps in the list produced by rncmaps
composemaps=method()
composemaps(List):=(L)->(
f:=L#(#L-1);
q:=#L-2;
j:=0;
slist:={};
while q>=0 do (
  vRa:=(entries vars ring f)#0;
  slist=apply(toList(0..#(vRa)-1),j->(vRa#j=>(L#q)_(0,j)));
  --print(f);
  f=substitute(f,slist);
q=q-1);
f);


-- parametrizing linear system of a rational normal curve
-- over PP^1 or a conic
-- 1st argument: Ideal of a rational normal curve
-- Output: row matrix with the linear system
isomorphicProjectionOfRNC=method()
isomorphicProjectionOfRNC(Ideal):=(I)->(
if degree(I)>=3 then (
  L:=rncmaps(I);
  substitute(composemaps(L),ring I))
else
(
vars ring I
));



-- compute the inverse of a birational map 
-- 1st argument: ideal of the source variety
-- 2nd argument: row matrix with linear system defining the map
--
-- Output: List of
-- birational map given as row matrix
-- ideal of image

invertBirationalMap=method()
invertBirationalMap(Ideal,Matrix):=(I,linsys)->(
R:=ring I;
--S:=R/I;
K:=coefficientRing R;
t:=symbol t;
m:=(rank source linsys)-1;
T:=K[t_0..t_m];
RT:=R**T;
vT:=sub(vars T,RT);
sl:=sub(linsys,RT);
Igr:=sub(I,RT)+minors(2,vT||sl);
Igr=saturate(Igr,ideal sl);
--print(Igr);
rel:=sub(diff(substitute(transpose vars R,RT),mingens Igr),T);
--print(rel);
vRa:=(entries sub(vars R,RT))#0;
slist:=apply(toList(0..#(vRa)-1),j->(vRa#j=>0));
IT:=ideal mingens sub(sub(Igr,slist),T);
Tc:=T/IT;
{sub((gens ker transpose sub(rel,Tc))_{0},T),IT});
--invertBirationalMap(I,linsys)
--invertBirationalMap(Irnc,ls)


-- this is used only for the rational normal curves:
-- compute the inverse of a birational map 
-- 1st argument: ideal of the source variety
-- 2nd argument: row matrix with linear system defining the map
--
-- Output: List of
-- birational map given as row matrix
-- ideal of image

invertBirationalMap0=method()
invertBirationalMap0(Ideal,Matrix):=(I,linsys)->(
R:=ring I;
S:=R/I;
syzls:=syz(substitute(linsys,S));
--syzls:=syz(substitute(linsys,S),DegreeLimit=>1+(degree(linsys_(0,0))#0)#0);
--syzls:=syz linsys;
--print(substitute(linsys,S),syz substitute(linsys,S),degrees source syz(substitute(linsys,S)));
--print(syzls);
K:=coefficientRing R;
t:=symbol t;
m:=(rank source linsys)-1;
T:=K[t_0..t_m];
RT:=R**T;
--print(sub(syzls,R),vars R);
rel:=diff(substitute(transpose vars R,RT),sub(vars(T),RT)*sub(sub(syzls,R),RT));
--print(rel);
f:=map(S,T,sub(linsys,S));
IT:=ker f;
Tc:=T/IT;
--print(sub(rel,Tc),(gens ker transpose sub(rel,Tc)));
{sub((gens ker transpose sub(rel,Tc))_{0},T),IT});
--invertBirationalMap(I,linsys)
--invertBirationalMap(Irnc,ls)


-- parametrize rational normal curve C
-- 1st argument: Ideal of C
-- Output: List of
-- birational map to C from PP^1 or from conic given as a column matrix
-- 0-ideal or ideal of conic 
--
rParametrizeRNC=method()
rParametrizeRNC(Ideal):=(Irnc)->(
linsys:=isomorphicProjectionOfRNC(Irnc);
invertBirationalMap0(Irnc,linsys));
--par=rParametrizeRNC(Irnc)


-- test parametrization of curve C
-- 1st argument: Ideal of C
-- 2nd argument: Parametrization of C given as row matrix
-- Output: true/false

testParametrization=method()
testParametrization(Ideal,Matrix):=(I,par)->(
vRa:=(entries vars ring I)#0;
slist:=apply(toList(0..#(vRa)-1),j->(vRa#j=>(par)_(j,0)));
if par==matrix({apply(toList(0..#(vRa)-1),j->0_(ring par))}) then return(false);
(ideal 0_(ring par))==ideal mingens sub(I,slist));
--testParametrization(Irnc,par)



rParametrizePlaneCurve=method(Options=>{parametrizeConic=>null})


-- the general parametrization function rParametrizePlaneCurve, also parametrizing the conic
-- if option parametrizeConic=>true is given

rParametrizePlaneCurve(Ideal,Ideal):=opts->(I,linsys)->(
parconic:=false;
if class(opts.parametrizeConic)===Boolean and opts.parametrizeConic==true then parconic=true;
if parconic==false then return(rParametrizePlaneCurveOC(I,linsys));
par:=rParametrizePlaneCurve(I,linsys);
R:=ring par;
Ic:=ideal R;
if degree Ic==1 then return(par);
conicpar:=rParametrizeConic(Ic);
if conicpar==0 then (print("No rational point on conic");return(par));
v:=(entries vars R)#0;
par2:=sub(par,{v#0=>conicpar_(0,0),v#1=>conicpar_(1,0),v#2=>conicpar_(2,0)});
par2)
--p=rParametrizePlaneCurve(I,linsys,parametrizeConic=>true)



-- parametrize plane curve C
-- 1st argument: ideal of C
-- 2nd argument: adjoint ideal of C
-- Output: Parametrization of C in a 3x1 row matrix
--         with entries in the homogeneous coordinate ring of PP^1 or a conic
--         so to get the conic apply "ideal ring" to the parametrization

-- new code composes birational maps C->RNC->(PP^1 or C2) and inverts then

rParametrizePlaneCurveOC=method()
rParametrizePlaneCurveOC(Ideal,Ideal):=(I,J)->(
linsys:=mingens(intersect(J,(ideal vars ring J)^(degree(I)-2)));
Irnc:=mapToRNC(I,linsys);
rncls:=isomorphicProjectionOfRNC(Irnc);
vRa:=(entries vars ring Irnc)#0;
slist:=apply(toList(0..#(vRa)-1),j->(vRa#j=>(linsys)_(0,j)));
ls:=sub(rncls,slist);
par1:=invertBirationalMap(I,ls);
Iconic:=par1#1;
par:=par1#0;
-- remove:
teiler:=gcd(gcd(par_(0,0),par_(1,0)),par_(2,0));
Rt:=ring par;
par=matrix {{substitute(par_(0,0)/teiler,Rt)},{substitute(par_(1,0)/teiler,Rt)},{substitute(par_(2,0)/teiler,Rt)}};
if even(degree I)==false then par=optpar par;
T:=Rt/Iconic;
sub(par,T)
);






-*

-- old code
-- inverts the birational maps C->RNC and RNC->(PP^1 or C2) separately

rParametrizePlaneCurve0=method()
rParametrizePlaneCurve0(Ideal,Matrix):=(I,linsys)->(
paramoverrnc:=invertBirationalMap(I,linsys);
paramofrnc:=rParametrizeRNC(paramoverrnc#1);
Iconic:=paramofrnc#1;
vRa:=(entries vars ring (paramoverrnc#0))#0;
slist:=apply(toList(0..#(vRa)-1),j->(vRa#j=>(paramofrnc#0)_(j,0)));
par:=sub(paramoverrnc#0,slist);
teiler:=gcd(gcd(par_(0,0),par_(1,0)),par_(2,0));
Rt:=ring paramofrnc#0;
par=matrix {{substitute(par_(0,0)/teiler,Rt)},{substitute(par_(1,0)/teiler,Rt)},{substitute(par_(2,0)/teiler,Rt)}};
if even(degree I)==false then par=optpar par;
T:=Rt/(paramofrnc#1);
sub(par,T));

--pI:=rParametrizePlaneCurve(I,linsys)
--testParametrization(I,pI)
--ideal ring pI

*-


-- optimizes the height of the parametrization (for odd degree)
-- Argument: column matrix with the parametrization
-- Output: parametrization in the same ring with equal or smaller coefficients
optpar=method()
optpar(Matrix):=(par)->(
yv:=(entries(vars ring par))#0;
n:=(degree (par)_(0,0))#0;
coeffl:={};
qq:=0;
q:=n;
while q>-1 do (coeffl=append(coeffl,(entries transpose matrix((1/(q!*(n-q)!))*(entries substitute(diff(substitute(yv#0^q*yv#1^(n-q),ring par),par),QQ))))#0);q=q-1);
coffmat:=transpose matrix coeffl;
q=0;
nenner:=1;
--lcm(List):=(ll)->(numerator(ll#0*ll#1/gcd(ll#0,ll#1)));
while q<3 do (qq=0;while qq<n+1 do (nenner=lcm({nenner,denominator(coffmat_(q,qq))});qq=qq+1);q=q+1);
coffmat=substitute(nenner*coffmat,ZZ);
transfor2:=par*nenner;
q=0;
teiler:=coffmat_(0,0);
while q<3 do (qq=0;while qq<n+1 do (teiler=gcd(teiler,coffmat_(q,qq));qq=qq+1);q=q+1);
coffmat=substitute(matrix(1/teiler*entries(coffmat)),ZZ);
transfor3:=matrix(1/teiler*entries(transfor2));
gt0:=gcd(coffmat_(0,0),coffmat_(0,n-1));
gt1:=gcd(coffmat_(1,0),coffmat_(1,n-1));
gt2:=gcd(coffmat_(2,0),coffmat_(2,n-1));
gt:=gcd(gcd(gt0,gt1),gt2);
gta:=gt;
teile:=false;
if ((gcd(coffmat_(0,0),gt^n)==gt^n) and (gcd(coffmat_(1,0),gt^n)==gt^n) and (gcd(coffmat_(2,0),gt^n)==gt^n)) then teile=true;
transfor4:=transfor3;
if teile=true then transfor4=substitute(transfor3,{yv#0=>substitute(yv#0,ring par)/gt});
transfor4=substitute(transfor4,ring par);
coeffl={};
q=n;
while q>-1 do (coeffl=append(coeffl,(entries transpose matrix((1/(q!*(n-q)!))*(entries substitute(diff(substitute(yv#0^q*yv#1^(n-q),ring par),transfor4),QQ))))#0);q=q-1);
coffmat=transpose matrix coeffl;
nenner=1;
q=0;
while q<3 do (qq=0;while qq<n+1 do (nenner=lcm({nenner,denominator(coffmat_(q,qq))});qq=qq+1);q=q+1);
coffmat=substitute(nenner*coffmat,ZZ);
transfor4=transfor4*nenner;
gt0=gcd(coffmat_(0,1),coffmat_(0,n));
gt1=gcd(coffmat_(1,1),coffmat_(1,n));
gt2=gcd(coffmat_(2,1),coffmat_(2,n));
gt=gcd(gcd(gt0,gt1),gt2);
teile=false;
if ((gcd(coffmat_(0,n),gt^n)==gt^n) and (gcd(coffmat_(1,n),gt^n)==gt^n) and (gcd(coffmat_(2,n),gt^n)==gt^n)) then teile=true;
transfor5:=transfor4;
if teile=true then transfor5=substitute(transfor4,{yv#1=>substitute(yv#1,ring par)*(1/gt)});
coeffl={};
q=n;
while q>-1 do (coeffl=append(coeffl,(entries transpose matrix((1/(q!*(n-q)!))*(entries substitute(diff(substitute(yv#0^q*yv#1^(n-q),ring par),transfor5),QQ))))#0);q=q-1);
coffmat=transpose matrix coeffl;
nenner=1;
q=0;
while q<3 do (qq=0;while qq<n+1 do (nenner=lcm({nenner,denominator(coffmat_(q,qq))});qq=qq+1);q=q+1);
transfor5=transfor5*nenner;
--print "Verbesserung:";
--print (yv#0,gta,"^",n);
--print (yv#1,gt,"^",n);
transfor5)
--optpar pI




-- rParametrizeConic(Ideal)
--
-- computes a rational parametrization of a conic
-- if the conic has a rational point

rParametrizeConic=method()
rParametrizeConic(Ideal):=(I)->(
y:=symbol y;
R:=ring I;
--S:=QQ[t_0,t_1];
p:=rationalPointOnConic(I);
if p!=matrix(QQ,{{0,0,0}}) then (
A:=(vars R)||sub(p,R);
M:=mingens minors(2,A);
L:=M_{0,1};
par:=invertBirationalMap(I,L);
return(par#0);
);
p);
--I=ideal(y_0*y_2-y_1^2+y_2^2)
--rParametrizeConic(I)



-- rationalPointOnConic(Ideal)
--
-- deciding whether a conic has a rational point and if so computing one
-- Argument: ideal of an irreducible conic defined over QQ
-- Output: row matrix with homogeneous coordinates of a rational point on 
--         the conic or 0 if there is none

-- uses
coeff=method()
coeff:=(m,f)->(
R:=ring f;
K:=coefficientRing R;
sub(contract(m,f),K))

rationalPointOnConic=method(Options=>{vb=>0})
rationalPointOnConic(Ideal):=opt->(I)->(
ueber:=(gens I)_(0,0);
L:=(entries vars ring I)#0;
L1:=(entries mingens (ideal vars ring I)^2)#0;
a:=coeff(L1#5,ueber);
b:=coeff(L1#4,ueber);
c:=coeff(L1#3,ueber);
d:=coeff(L1#2,ueber);
e:=coeff(L1#1,ueber);
f:=coeff(L1#0,ueber);
teilerc:=lcm(lcm(lcm(lcm(lcm(denominator(f),denominator(e)),denominator(d)),denominator(c)),denominator(b)),denominator(a));
uebers:=ueber*teilerc;
a=a*teilerc;
b=b*teilerc;
c=c*teilerc;
d=d*teilerc;
e=e*teilerc;
f=f*teilerc;
nr:=-1;
uebers2:=uebers;
rtrans:=vars ring I;
ds:=0;
tst:=1;
if (b^2==4*a*c) then (
 nr=0;
 uebers2=uebers;
 if c==0 then (
  rtrans=matrix {{-d/2/a*e,-(4*a*f-d^2)/4/a,e}};
  tst=sub(ueber,{L#0=>rtrans_(0,0),L#1=>rtrans_(0,1),L#2=>rtrans_(0,2)});
  if tst!=0 then print("ERROR "|toString tst);
  return(nrm(rtrans))
 );
 if c!=0 then (
  fs:=4*c*f-e^2;
  ds=4*c*d-2*b*e;
  rtrans=matrix {{-fs,-e*ds/2/c+b/2/c*fs,ds}};
  tst=sub(ueber,{L#0=>rtrans_(0,0),L#1=>rtrans_(0,1),L#2=>rtrans_(0,2)});
  if tst!=0 then print("ERROR "|toString tst);
  return(nrm(rtrans))
 );
);
if (d^2==4*a*f) then (
 nr=1;
 uebers2=uebers;
 if a==0 then (
  rtrans=matrix {{-(4*f*c-e^2)/4/f,b,-b/2/f*e}};
  tst=sub(ueber,{L#0=>rtrans_(0,0),L#1=>rtrans_(0,1),L#2=>rtrans_(0,2)});
  if tst!=0 then print("ERROR "|toString tst);
  return(nrm(rtrans))
 );
 if a!=0 then (
  cs:=4*a*c-b^2;
  es:=4*a*e-2*d*b;
  rtrans=matrix {{-es*b/2/a+d/2/a*cs,es,-cs}};
  tst=sub(ueber,{L#0=>rtrans_(0,0),L#1=>rtrans_(0,1),L#2=>rtrans_(0,2)});
  if tst!=0 then print("ERROR "|toString tst);
  return(nrm(rtrans))
 );
);
if (e^2==4*f*c) then (
 nr=2;
 uebers2=uebers;
 if f==0 then (
  rtrans=matrix {{d,-d/2/c*b,-(4*a*c-b^2)/4/c}};
  tst=sub(ueber,{L#0=>rtrans_(0,0),L#1=>rtrans_(0,1),L#2=>rtrans_(0,2)});
  if tst!=0 then print("ERROR "|toString tst);
  return(nrm(rtrans))
 );
 if f!=0 then (
  as:=4*f*a-d^2;
  bs:=4*f*b-2*d*e;
  rtrans=matrix {{bs,-as,-bs*d/2/f+e/2/f*as}};
  tst=sub(ueber,{L#0=>rtrans_(0,0),L#1=>rtrans_(0,1),L#2=>rtrans_(0,2)});
  if tst!=0 then print("ERROR "|toString tst);
  return(nrm(rtrans))
 );
);
N:=4*d*e-4*b*f;
di:=4*a*c-b^2;
jj:=4*c^2*d^2-4*b*c*d*e+4*a*c*e^2+4*b^2*c*f-16*a*c^2*f;
aa:=0;bb:=0;cc:=0;
if a==0 and c==0 then (
  rtrans=sub(rtrans,{L#0=>(L#0+L#1-2*e*L#2)/2/b,L#1=>(L#0-L#1-2*d*L#2)/2/b});
  aa=1;
  bb=-1;
  cc=-N;
);
if a!=0 and c==0 then (
 rtrans={rtrans#1,rtrans#0,rtrans#2};
 uebers2=sub(uebers,{L#0=>L#1,L#1=>L#0});
 ak:=a;
 ck:=c;
 dk:=d;
 ek:=e;
 a=ck;
 c=ak;
 e=dk;
 d=ek;
);
if c!=0 then (
 ueberdh1:=sub(uebers2,{L#1=>(1/(2*c))*L#1-(1/(2*c))*e*L#2-(1/(2*c))*b*L#0});
 ueberdh2:=sub(4*c*di*ueberdh1,{L#0=>((1/di)*L#0-2*d*c/di*L#2+b*e/di*L#2)});
 rtrans=sub(rtrans,{L#1=>(1/(2*c))*L#1-(1/(2*c))*e*L#2-(1/(2*c))*b*L#0});
 rtrans=sub(rtrans,{L#0=>((1/di)*L#0-2*d*c/di*L#2+b*e/di*L#2)});
 aa=1;
 bb=di;
 cc=-jj;
);
if sign(aa)==sign(bb) and sign(bb)==sign(cc) then return(sub(matrix{{0,0,0}},QQ));
pkt:=rtpt(sub(bb,ZZ),sub(cc,ZZ),opt);
if pkt==sub(matrix {{0,0,0}},QQ) then return(pkt);
tpkt:=sub(rtrans,{L#0=>pkt_(0,0),L#1=>pkt_(0,1),L#2=>pkt_(0,2)});
tst=sub(ueber,{L#0=>tpkt_(0,0),L#1=>tpkt_(0,1),L#2=>tpkt_(0,2)});
if tst!=0 then print("ERROR "|toString tst);
nrm(tpkt));

--R=QQ[y_0..y_2]
--I=ideal(y_0^2+y_1^2+2*y_0*y_1+y_2^2)
--I=ideal(y_0^2+y_2^2+2*y_0*y_2+y_1^2)
--I=ideal(7*y_0^2+11*y_2^2+13*y_0*y_2+17*y_1^2+19*y_1*y_2)
--I=ideal(y_0^2+y_2^2+2*y_0*y_2+2*y_1^2+2*y_1*y_2+4*y_0*y_1)
--I=ideal(y_0*y_1-y_2^2)
--I=ideal(y_2*y_1-y_0^2)
--I=ideal(y_0*y_2-y_1^2)
--p=rationalPointOnConic I
--sub(I,{y_0=>p_(0,0),y_1=>p_(0,1),y_2=>p_(0,2)})



-- rtpt(ZZ,ZZ)
--
-- does the same as rationalPointOnConic but for the conic y_0^2 + bb*y_1^2 + cc*y_2^2
-- with bb and cc non-zero of different sign

rtpt=method(Options=>{vb=>0})
rtpt(ZZ,ZZ):=opt->(bb,cc)->(
if opt.vb>0 then print("Input: "|bb,","|cc);
y:=symbol y;
R:=QQ[y_0,y_1,y_2];
tst:=1;
ueberq:=y_0^2+bb*y_1^2+cc*y_2^2;
bbsq:=rootofsquarepart(bb);
rtrans:=matrix {{y_0,y_1,y_2}};
ccsq:=rootofsquarepart(cc);
ueberq2:=sub(ueberq,{y_1=>y_1/bbsq,y_2=>y_2/ccsq});
rtrans=matrix {{rtrans_(0,0),rtrans_(0,1)/bbsq,rtrans_(0,2)/ccsq}};
sqfpb:=sub(bb/bbsq^2,ZZ);
sqfpc:=sub(cc/ccsq^2,ZZ);
gt:=gcd(sqfpb,sqfpc);
rslt:=0;
aaa:=gt;bbb:=sub(sqfpb/gt,ZZ);ccc:=sub(sqfpc/gt,ZZ);
if opt.vb>0 then print("(a,b,c) = ("|aaa|","|bbb|","|ccc|")");
ueberq3:=1/gt*sub(ueberq2,{y_0=>y_0*gt});
rtrans=matrix {{rtrans_(0,0),rtrans_(0,1)/gt,rtrans_(0,2)/gt}};
bk:=0;
while Jval(abs(aaa*bbb),abs(aaa*ccc),abs(bbb*ccc))>1 do (
 while abs(aaa)>abs(bbb) or abs(bbb)>abs(ccc) do (
  if abs(aaa)>abs(bbb) then (
   bk=aaa;
   aaa=bbb;
   bbb=bk;
   rtrans=sub(rtrans,{y_0=>y_1,y_1=>y_0});
  );
  if abs(bbb)>abs(ccc) then (
   bk=bbb;
   bbb=ccc;
   ccc=bk;
   rtrans=sub(rtrans,{y_1=>y_2,y_2=>y_1});
  );
 );
if opt.vb>0 then print("(a,b,c) = ("|aaa|","|bbb|","|ccc|")");
R1:=modularSquareRoot(-aaa*bbb,abs(ccc));
R2:=modularSquareRoot(-aaa*ccc,abs(bbb));
R3:=modularSquareRoot(-bbb*ccc,abs(aaa));
if opt.vb>0 then print("(R1,R2,R2) = ("|R1|","|R2|","|R3|")");
if (R1==-1 or R2==-1 or R3==-1) then return(sub(matrix({{0,0,0}}),QQ));
r1:=modularQuotient(R1/aaa,ccc);
if r1>0 and abs(2*r1)>abs(ccc) then (
   r1=r1-(round(abs(r1/ccc)))*abs(ccc)
);
if r1<0 and abs(2*r1)>abs(ccc) then (
   r1=r1+(round(abs(r1/ccc)))*abs(ccc)
);
r1=sub(r1,ZZ);
Q:=sub((aaa*r1^2+bbb)/ccc,ZZ);
if Q==0 then (
rtrans=sub(rtrans,{y_0=>1_QQ,y_1=>1_QQ,y_2=>0});
hn:=lcm(lcm(denominator(rtrans_(0,0)),denominator(rtrans_(0,1))),denominator(rtrans_(0,2)));
rtrans=rtrans*hn;
tlr:=gcd(gcd(rtrans_(0,0),rtrans_(0,1)),rtrans_(0,2));
rslt=matrix({{(1/tlr)*rtrans_(0,0),(1/tlr)*rtrans_(0,1),(1/tlr)*rtrans_(0,2)}});
tst=sub(ueberq,{y_0=>rslt_(0,0),y_1=>rslt_(0,1),y_2=>rslt_(0,2)});
if tst!=0 then print("ERROR "|toString tst);
if sub(ueberq,{y_0=>rslt_(0,0),y_1=>rslt_(0,1),y_2=>rslt_(0,2)})!=0 then return("ERROR");
return(rslt)
);
A:=gcd(gcd(aaa*r1^2,bbb),ccc*Q);
alpha:=sub(r1/A,ZZ);
beta:=sub(bbb/A,ZZ);
B:=aaa*beta;
C:=squarefreepart(sub(Q/A,ZZ));
gammaa:=rootofsquarepart(sub(Q/A,ZZ));
rtrans=sub(rtrans,{y_0=>A*alpha*y_0-beta*y_1,y_1=>y_0+aaa*alpha*y_1,y_2=>C*gammaa*y_2});
ueberq4:=A*y_0^2+B*y_1^2+C*y_2^2;
if opt.vb>0 then print("(alpha,beta,gamma) = ("|alpha|","|beta|","|gammaa|")");
aaa=A;bbb=B;ccc=C;
);
rtt:=0;
if sign(aaa)==-sign(bbb) then rtt=sub(rtrans,{y_0=>1_QQ,y_1=>1_QQ,y_2=>0});
if sign(aaa)==-sign(ccc) then rtt=sub(rtrans,{y_0=>1_QQ,y_1=>0,y_2=>1_QQ});
if sign(bbb)==-sign(ccc) then rtt=sub(rtrans,{y_0=>0,y_1=>1_QQ,y_2=>1_QQ});
hn=lcm(lcm(denominator(rtt_(0,0)),denominator(rtt_(0,1))),denominator(rtt_(0,2)));
rtt=rtt*hn;
tlr=gcd(gcd(rtt_(0,0),rtt_(0,1)),rtt_(0,2));
rslt=matrix({{rtt_(0,0)/tlr,rtt_(0,1)/tlr,rtt_(0,2)/tlr}});
tst=sub(ueberq,{y_0=>rslt_(0,0),y_1=>rslt_(0,1),y_2=>rslt_(0,2)});
if tst!=0 then print("ERROR "|tst);
if sub(ueberq,{y_0=>rslt_(0,0),y_1=>rslt_(0,1),y_2=>rslt_(0,2)})!=0 then return("ERROR");
return(rslt);
);
--p=rtpt(-117,-116);
--p=rtpt(1180943,-14640196896);
--p_(0,0)^2+1180943*p_(0,1)^2-14640196896*p_(0,2)^2

sign=method()
sign(ZZ):=(n)->(
if n>0 then return(1);
if n<0 then return(-1);
0)
sign(QQ):=(n)->(
if n>0 then return(1);
if n<0 then return(-1);
0)

-- Jval(ZZ,ZZ,ZZ)
-- computes the index of the argument

Jval=method()
Jval(ZZ,ZZ,ZZ):=(a,b,c)->(
     rt:=0;
     if (a<=b and b<=c) then rt=b;
     if (a<=c and c<=b) then rt=c;
     if (b<=a and a<=c) then rt=a;
     if (b<=c and c<=a) then rt=c;
     if (c<=a and a<=b) then rt=a;
     if (c<=b and b<=a) then rt=b;
rt);



-- nrm(Matrix)
-- normalize the coordinates of a point in PP^2(QQ)
-- to relatively coprime coordinates over ZZ
-- in a matrix over QQ
--
nrm=method()
nrm(Matrix):=(tpkt0)->(
  tpkt:=(entries tpkt0)#0;
  hn:=lcm(lcm(denominator(sub(tpkt#0,QQ)),denominator(sub(tpkt#1,QQ))),denominator(sub(tpkt#2,QQ)));
  tpkt1:=matrix {{tpkt#0*hn,tpkt#1*hn,tpkt#2*hn}};
  tlr:=gcd(gcd(tpkt1_(0,0),tpkt1_(0,1)),tpkt1_(0,2));
1/tlr*sub(tpkt1,QQ));
--nrm matrix {{2,2,12}}

-- modularSquareRoot(ZZ,ZZ)
-- modular square root
-- if exists then returns the modular square root of the first argument in ZZ/(second argument)*
-- if not then returns false
--
-- old code, does not work for large primes m
--
modularSquareRoot0=method()
modularSquareRoot0(ZZ,ZZ):=(n,m)->(
r:=0;
t:=symbol t;
R:=(ZZ/m)[t];
f:=t^2-n;
L:=toList factor(f);
if #L==1 then return(false);
sub(sub(L#0#0,t=>0),ZZ))
--modularSquareRoot0(2,5)



-- squarefreepart(ZZ)
-- computing the squarefree part of an integer
--
-- should use squarefree factorization
--
squarefreepart=method()
squarefreepart(ZZ):=(n)->(
L:=toList factor n;
L1:={};
q:=0;
m:=1;
while q<#L do (
 L1=toList(L#q);
 m=m*(L1#0)^(sub(mod(L1#1,2),ZZ));
q=q+1);
m);
--squarefreepart(1234*5^2)


-- rootofsquarepart(ZZ)
-- computing the squareroot of the square part of an integer
--
-- should use squarefree factorization
--
rootofsquarepart=method()
rootofsquarepart(ZZ):=(n)->(
--print("---",n);
L:=toList factor abs n;
L1:={};
q:=0;
m:=1;
while q<#L do (
 L1=toList(L#q);
 m=m*(L1#0)^(sub((L1#1-(sub(mod(L1#1,2),ZZ)))/2,ZZ));
q=q+1);
--print m;
m);
--rootofsquarepart(-1234*5^2)


-- legendreSymbol(ZZ,ZZ)
-- compute the legendreSymbol
-- 1st argument a an integer
-- 2nd argument p an odd prime

legendreSymbol=method()
legendreSymbol(ZZ,ZZ):=(a,p)->(
if denominator(a/p)==1 then return(0);
l:=modularPower(a,sub((p-1)/2,ZZ),p);
if l==p-1 then return(-1);
l);
--legendreSymbol(4,7)
--legendreSymbol(5,7)
--legendreSymbol(14,7)
--legendreSymbol(626,1180943)


-- modularPower(ZZ,ZZ,ZZ)
-- compute the modular power
-- 1st argument a an integer
-- 2nd argument b a positive integer
-- 3rd argument p a prime

modularPower=method()
modularPower(ZZ,ZZ,ZZ):=(a,n,p)->(
r:=1;
j:=1;
while j<=n do (
  r=modp(r*a,p);
j=j+1);
r);
--modularPower(3,10000,101)


-- modp(ZZ,ZZ)
-- 1st argument a an integer
-- 2nd argument p an integer
-- compute a mod p represented by an integer in 0..(p-1)

modp=method()
modp(ZZ,ZZ):=(a,p)->(
m:=floor(a/p);
a-m*p);
--modp(12,7)



-- modularSquareRoot(ZZ,ZZ)
-- 1st argument integer a
-- 2nd argument prime p
-- computes a modular square root of a mod p
-- if not exist returns false
--
-- uses Shanks-Tonelli algorithm


-- uses the following function
-- which does the same as modularSquareRoot for p prime

pmodularSquareRoot=method()
pmodularSquareRoot(ZZ,ZZ):=(a,p)->(
r:=0;
if modp(a,p) == 0 then return(0);
if p==2 then return(1);
if legendreSymbol(a, p)!=1 then return(-1);
if modp(p,4) == 3 then (
  r=modularPower(a, sub((p + 1) / 4,ZZ), p);
  return(r);
);
q:=p-1;
e:=0;
while modp(q,2) == 0 do (
   q=sub(q/2,ZZ);
   e=e+1;
);
w:=2;
while legendreSymbol(w, p)!=-1 do (
  w=w+1;
);
R:=modularPower(a, sub((q + 1) / 2,ZZ), p);
b:=modularPower(a, q, p);
g:=modularPower(w, q, p);
r=e;
t:=0;
m:=0;
gs:=0;
while true do (
   t=b;
   m=0;
   while m<=r do (
      if t == 1 then (
        break
      );
      t=modularPower(t, 2, p);
   m=m+1);
   if m == 0 then (
       return(R)
   );
   gs=modularPower(g, 2^(r - m - 1), p);
   g=modp((gs * gs),p);
   R=modp((R * gs),p);
   b=modp((b * g),p);
   r=m;
);
);
--pmodularSquareRoot(2,17)


--n=19;
--q=0;
--while q<n do (
--print("-",q,modularSquareRoot(q,n));
--q=q+1);



-- modularSquareRoot(ZZ,ZZ)
-- 1st argument integer a
-- 2nd argument squarefree p
-- computes a modular square root of a mod p
-- if not exist returns -1
--

modularSquareRoot=method()
modularSquareRoot(ZZ,ZZ):=(a,n)->(
r:=msqrt(a,n);
--print(r);
return(r);
--print("Input",a,n);
if n==1 then (
  return(0)
);
F:=toList factor(n);
q:=0;
L1:={};
L2:={};
t:=0;
while q<#F do (
  t=pmodularSquareRoot(a,(toList F#q)#0);
  --print t;
  if t==-1 then return(-1);
  L1=append(L1,t);
  L2=append(L2,(toList F#q)#0);
q=q+1);
s:=chineseRemainder(L1,L2);
if s<0 then s=-1*s;
if abs(s)>n/2 then s=abs(n-s);
--print("Result ",s);
s);
--modularSquareRoot(3,13)

--modularSquareRoot(626,1180943)
--modularSquareRoot(111,2*3*5)
--modularSquareRoot(10,1)



-- chineseRemainder(List,List)
--        L   M
-- chinese remainder theorem
-- solves x=L#i mod M#i
-- for x with M#i relatively prime
--

chineseRemainder=method()
chineseRemainder(List,List):=(L1,L2)->(
q:=1;
a:=L1#0;
n:=L2#0;
L:={};
while q<#L1 do (
  a=chineseRemainder0(a,L1#q,n,L2#q);
  n=n*L2#q;
q=q+1);
a);
--chineseRemainder({1,2,3},{3,5,7})



-- chineseRemainder0(ZZ,ZZ,ZZ,ZZ)
--        a, b, p, q
-- chinese remainder theorem
-- solves x=a mod p
--        x=b mod q
-- for x with p,q relatively prime
--

chineseRemainder0=method()
chineseRemainder0(ZZ,ZZ,ZZ,ZZ):=(a,b,n,m)->(
k:=a-b;
L:=igcdx(n,m);
u:=L#0;
v:=L#1;
modp(a-k*u*n,n*m));
--chineseRemainder0(2,3,7,11)


div=method()
div(ZZ,ZZ):=(a,b)->(
r:=modp(a,b);
sub((a-r)/b,ZZ));
--div(100,3)


modularInverse=method()
modularInverse(ZZ,ZZ):=(a,p)->(
if gcd(a,p)!=1 then return(false);
L:=igcdx(a,p);
ia:=L#0;
if ia<0 then ia=ia+p;
if p<0 then return(-ia);
ia);

--apply(1..100,q->modularInverse(q,51335)),



-- modularQuotient(QQ,ZZ)
-- 1st argument q a rational number
-- 2nd argument p an integer
-- compute the modular quotient q mod p
-- (false if not exists)

modularQuotient=method()
modularQuotient(QQ,ZZ):=(q,p)->(
a:=numerator(q);
b:=denominator(q);
if gcd(b,p)!=1 then return(false);
ib:=modularInverse(b,p);
modp(a*ib,p));
--apply(1..100,q->modularQuotient(5/q,51335))
--modularQuotient(103/2,101)


-- igcdx(ZZ,ZZ)
--        a  b
--
-- Extended Euclidean algorithm
-- computes the x,y with x*a+y*b=gcd(a,b)

igcdx=method()
igcdx(ZZ,ZZ):=(a, b)->(
if modp(a,b)== 0 then 
(
  return {0, 1};
)
else 
(
  L:= igcdx(b, modp(a,b));
  return {L#1, L#0-L#1*div(a,b)};
);
);
--igcdx(121,23)



parametrize=method(Options=>{parametrizeConic=>null})

parametrize(Ideal,Ideal):=opts->(I,adj)->(
rParametrizePlaneCurve(I,adj,opts))

parametrize(Ideal):=opts->(I)->(
parconic:=false;
if class(opts.parametrizeConic)===Boolean and opts.parametrizeConic==true then parconic=true;
if parconic==false then return(parametrize0(I));
if degree I==2 then return(rParametrizeConic(I));
if (rank source vars ring I)==(1+degree I) then (
   par:=rParametrizeRNC(I);
   R:=ring(par#0);
   Ic:=par#1;
   if degree Ic==1 or parconic==false then (
      S:=R/Ic;
      pI:=substitute(par#0,S);
      return(pI);
   );
   conicpar:=rParametrizeConic(Ic);
   v:=(entries vars R)#0;
   par2:=sub(par#0,{v#0=>conicpar_(0,0),v#1=>conicpar_(1,0),v#2=>conicpar_(2,0)});
   return(par2);
);
if (rank source vars ring I)==3 then (
   adj:=adjointIdeal I;
   return(parametrize(I,adj,opts));
);
"not yet implemented")

parametrize0=method()
parametrize0(Ideal):=(I)->(
if degree I==2 then return(rParametrizeConic(I));
if (rank source vars ring I)==(1+degree I) then (
   par:=rParametrizeRNC(I);
   S:=(ring par#0)/par#1;
   pI:=substitute(par#0,S);
   return(pI);
);
if (rank source vars ring I)==3 then (
   adj:=adjointIdeal I;
   return(parametrize(I,adj));
);
"not yet implemented")

---------------------------------------------------------------------------------------------

-*
Copyright (C) [2009] [Janko Boehm]

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>
*-



beginDocumentation()

doc ///
  Key
    "Parametrization"
  Headline
    Rational parametrization of rational curves and related computations
  Description
   Text
    {\bf Overview:}

    {\it Parametrization} is a package to compute rational parametrizations of rational curves defined over \mathbb{Q}.

    Suppose C is a rational plane curve C of degree n defined over \mathbb{Q}.

    We use the package {\it AdjointIdeal} to compute the adjoint ideal of C.
    (The package exports also all functions available in {\it AdjointIdeal}, e.g., geometricGenus.)
    
    The corresponding linear system maps the curve birationally to a rational normal curve in \mathbb{P}^{n-2}.

    Iterating the anticanonical map we give a projection of the rational normal curve to \mathbb{P}^{1} for n odd
    or to a conic C_2 in \mathbb{P}^{2} for n even.

    In the case that n is even we test for the existence of a rational point on the conic and if so give a rational parametrization of the conic.

    By inverting the birational map of C to \mathbb{P}^{1} or the conic we obtain a rational parametrization of C.
    If n is odd or C_2 has a rational point C is parametrized by \mathbb{P}^{1} otherwise by C_2.

    The main focus of the algorithm is to avoid unnecessary choices to obtain a parametrization of small height.

    For more theoretical details see 
    J. Boehm: Rational parametrization of rational curves,
    @HREF"http://www.math.uni-sb.de/ag/schreyer/jb/diplom%20janko%20boehm.pdf"@.

    The package is work in progress, so there will be future improvements and more testing is necessary.


    {\bf Key user functions:}

    @TO parametrize@  --  This is the universal rational parametrization function, it works for plane rational curves, 
                     in particular conics, and rational normal curves.

    @TO testParametrization@  --  Test a parametrization.

    @TO rationalPointOnConic@  -- Test for a rational point on a conic and find it if it exists.

    @TO mapToRNC@  --  Map a plane rational curve to a rational normal curve.


    {\bf Setup:}

    This package uses the package {\it AdjointIdeal}, so set up this first.

    Place the file Parametrization.m2 somewhere into the M2 search path and install the package by doing

    installPackage("Parametrization")
///


doc ///
  Key
    parametrize
    (parametrize,Ideal)
    (parametrize,Ideal,Ideal)
  Headline
    Rational parametrization of rational curves.
  Usage
    parametrize(I)
    parametrize(I,J)
  Inputs
    I:Ideal
        defining a rational plane curve C or rational normal curve
    J:Ideal 
        the adjoint ideal of the plane curve C.
  Outputs
    pI:Matrix
        a column matrix with the rational parametrization of C.
  Description
   Text
        Computes a rational parametrization pI of C.  

        If the degree of C odd, pI is over \mathbb{P}^{1}.

        If the degree of C even, pI is over a conic. So to get the conic apply @TO ideal@ @TO ring@ to the parametrization pI.
        If the @TO Option@ parametrizeConic=>true is given and C has a rational point then the conic is parametrized
        hence pI is over \mathbb{P}^{1}.

        If the second argument J is not specified and degree of C is bigger than 2 then J is being computed via the package AdjointIdeal.

        If the function is applied to a rational normal curve it calls @TO rParametrizeRNC@.

        If it is applied to a plane conic it calls @TO rParametrizeConic@.

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^8-u^3*(z+u)^5);
     p=parametrize(I)
     parametrize(I,parametrizeConic=>true)
     Irnc=mapToRNC(I);
     parametrize(Irnc)
     parametrize(Irnc,parametrizeConic=>true)
     Iconic=ideal ring p
     parametrize(Iconic)

   Text

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(u^5+2*u^2*v*z^2+2*u^3*v*z+u*v^2*z^2-4*u*v^3*z+2*v^5);
     parametrize(I)
     Irnc=mapToRNC(I);
     parametrize(Irnc)
   Text

     Specifying J:

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^8-u^3*(z+u)^5);
     J=ideal(u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6);
     p=parametrize(I,J)
     parametrize(I,J,parametrizeConic=>true)
     Irnc=mapToRNC(I,J);
     parametrize(Irnc)
     parametrize(Irnc,parametrizeConic=>true)
     Iconic=ideal ring p
     parametrize(Iconic)
   Text

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(u^5+2*u^2*v*z^2+2*u^3*v*z+u*v^2*z^2-4*u*v^3*z+2*v^5);
     J=ideal(u^3+v*u*z,v*u^2+v^2*z,v^2*u-u^2*z,v^3-v*u*z);
     parametrize(I,J)
     Irnc=mapToRNC(I,J);
     parametrize(Irnc)
  SeeAlso
     rParametrizePlaneCurve
     rParametrizeRNC
     rParametrizeConic
///



doc ///
  Key
    modularInverse
    (modularInverse,ZZ,ZZ)
  Headline
    Compute the inverse in Z/nZ.
  Usage
    modularInverse(a,n)
  Inputs
    a:ZZ
        an integer.
    n:ZZ
        a positive integer.
  Outputs
    :ZZ
        an integer between 0 and n-1 representing the inverse of a, or 
    :Boolean
        false if the inverse does not exist.
  Description
   Text
     Computes the inverse of a in $\mathbb{Z}/n\mathbb{Z}$.

   Example
    modularInverse(5,7)
    modularInverse(5,16)
    modularInverse(4,16)
    modularInverse(1234567,12345678)
///


doc ///
  Key
    chineseRemainder
    (chineseRemainder,List,List)
  Headline
    Solve simultaneous congruences.
  Usage
    chineseRemainder(L,M)
  Inputs
    L:List
        of integers.
    M:List
        of positive integers, which are relatively prime.
  Outputs
    x:ZZ
        an integer x between 0 and the product of the entries of M minus 1 representing the solution of the simultaneous congruences
        x=L#i mod M#i.
  Description
   Text
     Solves the simultaneous congruences x=L#i mod M#i.

   Example
     chineseRemainder({1,2,3},{3,5,7})
  Caveat
    This should be extended slightly to non prime integers.
///


doc ///
  Key
    modularSquareRoot
    (modularSquareRoot,ZZ,ZZ)
  Headline
    Compute the modular square root.
  Usage
    modularSquareRoot(a,n)
  Inputs
    a:ZZ
    n:ZZ 
        positive, squarefree.
  Outputs
    :ZZ
        in 0,...,n-1 representing the square root of a in $\mathbb{Z}/n\mathbb{Z}$, or -1 if it does not exist.
  Description
   Text
     Computes the square root of a in $\mathbb{Z}/n\mathbb{Z}$

   Example
     modularSquareRoot(626,1180943)
     modularSquareRoot(3,13)
     modularSquareRoot(7,14)
     modularSquareRoot(3,14)
  Caveat
    This should be extended slightly to non squarefree n.
    We use this function as we have to handle very large numbers.
    This function is no longer used, as we call Maple to compute the modular square root.
  SeeAlso
    legendreSymbol
    modularPower
    chineseRemainder
///


doc ///
  Key
    invertBirationalMap
    (invertBirationalMap,Ideal,Matrix)
  Headline
    Computes the inverse of a birational map.
  Usage
    invertBirationalMap(I,phi)
  Inputs
    I:Ideal
        defining the source variety
    phi:Matrix 
        a row matrix over the ring of I with linear system defining the map.
  Outputs
    :List
        of a matrix with the inverse of the birational map and the ideal of its image.
  Description
   Text
     Computes the inverse of a birational map.
   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^8-u^3*(z+u)^5);
     linsys=matrix {{u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6}};
     invertBirationalMap(I,linsys)
  Caveat
    Speed this up. Perhaps give as argument just phi in R/I. Does not check whether phi is birational.
///


doc ///
  Key
    legendreSymbol
    (legendreSymbol,ZZ,ZZ)
  Headline
    Compute the Legendresymbol.
  Usage
    legendreSymbol(a,p)
  Inputs
    a:ZZ
    p:ZZ
        an odd prime.
  Outputs
    :ZZ
        0 if a is divisible by p, 1 if a is a quadratic residue mod p and -1 if a is not a quadratic residue mod p.
  Description
   Text
     Computes Legendre-symbol a over p.

   Example
     legendreSymbol(4,7)
     legendreSymbol(5,7)
     legendreSymbol(14,7)
  SeeAlso
     modularPower
///




doc ///
  Key
    testParametrization
    (testParametrization,Ideal,Matrix)
  Headline
    Test if parametrization.
  Usage
    testParametrization(a,p)
  Inputs
    I:Ideal
       defining a rational curve C
    phi:Matrix
       with a parametrization of C, defined over \mathbb{P}^{1} or in the coordinate ring of a conic.
  Outputs
    :Boolean
       true if phi is a parametrization or false if not.
  Description
   Text
     Test whether phi is a parametrization of C.

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^8-u^3*(z+u)^5);
     T=K[t_0,t_1]
     phi=matrix {{t_0^3*t_1^5}, {-t_0^8}, {t_0^8-t_1^8}}
     testParametrization(I,phi)
  SeeAlso
     parametrize
     rParametrizeConic
     rParametrizePlaneCurve
///




doc ///
  Key
    modularPower
    (modularPower,ZZ,ZZ,ZZ)
  Headline
    Compute the modular power.
  Usage
    modularPower(a,b,p)
  Inputs
    a:ZZ
    b:ZZ 
        positive.
    p:ZZ
        prime.
  Outputs
    :ZZ
        in 0,...,p-1 representing a^b in $\mathbb{Z}/p\mathbb{Z}$.
  Description
   Text
     Computes the b-th power of a in $\mathbb{Z}/p\mathbb{Z}$. This function is here to also deal with large p.

   Example
     modularPower(8,10003,101)
///


doc ///
  Key
   rationalPointOnConic
   (rationalPointOnConic,Ideal)
  Headline
   Rational point on a conic.
  Usage
   rationalPointOnConic(I)
  Inputs
   I:Ideal
        the ideal of an irreducible conic.
  Outputs
    :Matrix
        a row matrix over \mathbb{Q} with homogeneous integer coordinates of a rational point on 
        the conic or 0-matrix if there is none.
  Description
   Text
     Decides whether a conic has a rational point and if so computes one.

   Example
     R=QQ[y_0..y_2];
     I=ideal(7*y_0^2+11*y_2^2+13*y_0*y_2+17*y_1^2+19*y_1*y_2);
     p=rationalPointOnConic I
     sub(I,{y_0=>p_(0,0),y_1=>p_(0,1),y_2=>p_(0,2)})
     I=ideal(y_0^2+y_1^2+2*y_0*y_1+y_2^2);
     p=rationalPointOnConic I
     sub(I,{y_0=>p_(0,0),y_1=>p_(0,1),y_2=>p_(0,2)})
     I=ideal(y_0^2+y_2^2+2*y_0*y_2+2*y_1^2+2*y_1*y_2+4*y_0*y_1);
     p=rationalPointOnConic I
     sub(I,{y_0=>p_(0,0),y_1=>p_(0,1),y_2=>p_(0,2)})
     I=ideal(y_0^2+y_2^2+y_1^2);
     p=rationalPointOnConic I
  Caveat
     Returns the 0-matrix if there is no rational point.
  SeeAlso
     modularSquareRoot
     modularInverse
///


doc ///
  Key
    isomorphicProjectionOfRNC
    (isomorphicProjectionOfRNC,Ideal)
  Headline
    Parametrizing linear system of a rational normal curve.
  Usage
    isomorphicProjectionOfRNC(I)
  Inputs
    I:Ideal
        of a rational normal curve
  Outputs
    :Matrix
        over the ring if I containing the linear system.
  Description
   Text
     Compute the parametrizing linear system of a rational normal curve over \mathbb{P}^{1} if the degree is odd or a conic if the degree even.

   Example
     K=QQ;
     R=K[v,u,z];
     I0=ideal(v^8-u^3*(z+u)^5);
     J=ideal matrix {{u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6}};
     I=mapToRNC(I0,J)
     isomorphicProjectionOfRNC(I)
///




doc ///
  Key
    rParametrizeConic
    (rParametrizeConic,Ideal)
  Headline
    Compute a rational parametrization of a conic.
  Usage
    rParametrizeConic(I)
  Inputs
    I:Ideal
        of an irreducible conic.
  Outputs
    :Matrix
        with the rational parametrization, or the 0-matrix if the conic does not have a rational point.
  Description
   Text
     Computes a rational parametrization of a conic or returns the 0-matrix if the conic does not have a rational point.

   Example
     K=QQ;
     R=K[y_0..y_2];
     I=ideal(y_0*y_2-y_1^2+y_2^2);
     rParametrizeConic(I)
  Caveat
     If the conic does not have a rational point the field extension of degree 2 should be implemented. 
///



doc ///
  Key
    rParametrizeRNC
    (rParametrizeRNC,Ideal)
  Headline
    Compute a rational parametrization of a rational normal curve.
  Usage
    rParametrizeRNC(I)
  Inputs
    I:Ideal
        of a rational normal curve.
  Outputs
    :List
        of a matrix phi and an ideal Ic.
  Description
   Text
        Compute a rational parametrization of a rational normal curve C defined by I. 
        
        phi contains the rational parametrization of C
        over \mathbb{P}^{1} if the degree of C is odd or over a conic if the degree of C is even.
        
        Ic is the 0-ideal for odd degree or the ideal of the conic for even degree.

   Example
     K=QQ;
     R=K[v,u,z];
     I0=ideal(v^8-u^3*(z+u)^5);
     J=ideal matrix {{u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6}};
     I=mapToRNC(I0,J)
     rParametrizeRNC(I)
  Caveat
     Perhaps better return just phi in the quotient ring by Ic. 
///


doc ///
  Key
    mapToRNC
    (mapToRNC,Ideal)
    (mapToRNC,Ideal,Ideal)
    (mapToRNC,Ideal,Matrix)
  Headline
    Map plane rational curve to rational normal curve.
  Usage
    mapToRNC(I)
    mapToRNC(I,J)
  Inputs
    I:Ideal
        defining the plane curve
    J:Ideal 
        the adjoint ideal of I.
  Outputs
    :Ideal
        of the rational normal curve.
  Description
   Text
     Maps a plane rational curve birationally to a rational normal curve by the adjoints of degree=degree(C)-2
     Applied to a curve of genus >1 it uses the adjoints of degree=degree(C)-3.

     If the second argument J is not specified and degree of C is bigger than 2 then J is being computed via the package AdjointIdeal.

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^8-u^3*(z+u)^5);
     betti mapToRNC(I)
   Text

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^8-u^3*(z+u)^5);
     J=ideal(u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6);
     betti mapToRNC(I,J)
///

doc ///
  Key
    parametrizeConic
    [rParametrizePlaneCurve,parametrizeConic]
    [parametrize,parametrizeConic]
  Headline
    Option whether to rationally parametrize conics.
  Description
   Text
       If this option is set true and the computation of @TO rParametrizePlaneCurve@ or @TO parametrize@
       leads to a conic (even degree case) and this conic has a rational point then this conic is also parametrized.
       So the final result will be a parametrization over \mathbb{P}^{1}. If the conic does not have a rational point
       a warning is displayed and the parametrization over the conic is returned. 
        
   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^8-u^3*(z+u)^5);
     J=ideal(u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6);
     rParametrizePlaneCurve(I,J,parametrizeConic=>true)
  SeeAlso
     parametrize
     rParametrizePlaneCurve
     rParametrizeConic
  Caveat
     If rParametrizeConic is changed such that it passes to a degree 2 field extension if the degree of C is even and the conic does not have a rational point,
     then the result will have entries in the homogeneous coordinate ring of \mathbb{P}^{1} over this extension.
///

doc ///
  Key
    vb
    [rationalPointOnConic,vb]
  Headline
    Option whether to print intermediate results.
  Description
   Text
       If this option is set an ZZ bigger than 1
       the function will print intermediate results.
        
   Example
    R=QQ[y_0..y_2];
    I=ideal(y_0^2 + 3*3*43*y_1^2 -2*2*2*2*11*41*y_2^2)
    p=rationalPointOnConic(I,vb=>1)
  SeeAlso
     rationalPointOnConic
///


doc ///
  Key
    rParametrizePlaneCurve
    (rParametrizePlaneCurve,Ideal,Ideal)
  Headline
    Rational parametrization of rational plane curves.
  Usage
    rParametrizePlaneCurve(I,J)
  Inputs
    I:Ideal
        defining the plane curve C
    J:Ideal 
        the adjoint ideal of C.
  Outputs
    pI:Matrix
        a column matrix with the rational parametrization of C.
  Description
   Text
        Computes a rational parametrization pI of C.  

        If the degree of C odd:

        pI is over \mathbb{P}^{1}.

        If the degree of C even:

        pI is over a conic. So to get the conic apply "ideal ring" to the parametrization pI.

        If the @TO Option@ parametrizeConic=>true is given and C has a rational point then the conic is parametrized
        so pI is over \mathbb{P}^{1}.

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^8-u^3*(z+u)^5);
     J=ideal(u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6);
     rParametrizePlaneCurve(I,J)
     rParametrizePlaneCurve(I,J,parametrizeConic=>true)
   Text

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(u^5+2*u^2*v*z^2+2*u^3*v*z+u*v^2*z^2-4*u*v^3*z+2*v^5);
     J=ideal(u^3+v*u*z,v*u^2+v^2*z,v^2*u-u^2*z,v^3-v*u*z);
     rParametrizePlaneCurve(I,J)
  Caveat
     If rParametrizeConic is changed such that it passes to a degree 2 field extension if the degree of C is even and the conic does not have a rational point,
     then pI will have entries in the homogeneous coordinate ring of \mathbb{P}^{1} over this extension.
  SeeAlso
     mapToRNC
     isomorphicProjectionOfRNC
     rParametrizeRNC
     rationalPointOnConic
     rParametrizeConic
///





TEST ///
K=QQ;
R=K[v,u,z];
I0=ideal(v^8-u^3*(z+u)^5);
linsys=matrix {{u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6}};
I=mapToRNC(I0,linsys);
par=rParametrizeRNC(I);
S=(ring par#0)/par#1;
pI=substitute(par#0,S);
assert(testParametrization(I,pI)==true);
///



TEST ///
K=QQ;
R=K[y_0..y_2];
I=ideal(y_0*y_2-y_1^2+y_2^2);
par=rParametrizeConic(I);
assert(testParametrization(I,par)==true);
///


TEST ///
assert(legendreSymbol(4,7)==1);
assert(legendreSymbol(5,7)==-1);
assert(legendreSymbol(14,7)==0);
///


-- this test commented out because it requires the presence of maple
-- TEST ///
-- R=QQ[y_0..y_2];
-- I=ideal(7*y_0^2+11*y_2^2+13*y_0*y_2+17*y_1^2+19*y_1*y_2);
-- p=rationalPointOnConic I;
-- assert(sub(I,{y_0=>p_(0,0),y_1=>p_(0,1),y_2=>p_(0,2)})==0);
-- ///


TEST ///
modularPower(8,10003,101)
///



TEST ///
K=QQ;
R=K[v,u,z];
I=ideal(v^8-u^3*(z+u)^5);
J=ideal(u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6);
pI=rParametrizePlaneCurve(I,J,parametrizeConic=>true);
assert(testParametrization(I,pI)==true);
///

-- this test commented out because it requires the presence of maple
-- TEST ///
-- a=modularSquareRoot(626,1180943);
-- assert(a==348206 or a==832737);
-- a=modularSquareRoot(3,13);
-- assert(a==4 or a==9);
-- a=modularSquareRoot(7,14);
-- assert(a==7);
-- a=modularSquareRoot(3,14);
-- assert(a==FAIL);
-- ///

TEST ///
assert(chineseRemainder({1,2,3},{3,5,7})==52);
///

TEST ///
K=QQ;
R=K[v,u,z];
I=ideal(v^8-u^3*(z+u)^5);
linsys=matrix {{u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6}};
L=invertBirationalMap(I,linsys);
assert(degree L#1==6);
///


end

