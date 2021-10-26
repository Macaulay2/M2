restart
restart
loadPackage "SchurFunctors"
help schur
debug SchurFunctors
peek loadedFiles
addColumn
debug SchurFunctors
-- <<<<<<< .mine

E=QQ^4
mu={2,1}

M=schurModule(mu,E)

F=new Filling from {{0,1},{2}}

M = schurModule({1,1,1}, QQ^4);

v = straighten(new Filling from {{3,2,1}}, M)


straighten(F,M)
-- =======
-- Koszul complex check
n = 4;
R = QQ[x_1..x_n];
g = map(R^n,R^n,(i,j)->(i+1)^j)
sg = apply(n,i->schur(toList(i+1:1), g));
Kx = T -> apply(numgens R, j -> (R_j, addColumn(T,0,j)))
Ktop = apply(n-1, i->schurModulesMap(source sg#(i+1), source sg#i,Kx));
y = flatten entries(g * transpose vars R);
Ky = T -> apply(numgens R, j -> (y_j, addColumn(T,0,j)))
Kbottom = apply(n-1, i->schurModulesMap(source sg#(i+1), source sg#i,Ky));
--check commutativity
all(toList(0..n-2), i->sg#(i+1)*Ktop#i==Kbottom#i*sg#i)

SMM = apply(3, i->(
	  M = schurModule(toList(i+1:1),R^4);
	  N = schurModule(toList(i+2:1),R^4);
	  schurModulesMap(N,M,F) ))
SMM#1*SMM#0

-- >>>>>>> .r7062

L={{1,0}}
normalize L
---examples
kk=QQ
M=kk^2
S4M=schurModule({2},M)
(f,finv,AT,ST)=toSequence(S4M.cache.Schur)

AT
ST
keys(AT)

new Filling {{2,0}}

---Simple examples: 
-----Symmetric powers:
kk=QQ
M=kk^3
S1M=schurModule({1},M)
S2M=schurModule({2},M)
S3M=schurModule({3},M)

R=QQ[x_1,x_2,x_3]
F=map(R^1,R^3,vars R)
L=schur({3},F)
------------------n-th veronese embedding

-----Exterior powers:
kk=QQ
M=kk^3
E0M=schurModule({},M)---
E1M=schurModule({1},M)
E2M=schurModule({1,1},M)
E3M=schurModule({1,1,1},M)
-------Koszul Complex
kk=QQ
d=4
R=kk[x_1..x_d]
Partitions=apply(d,j->apply(j+1,s->1))
Koszul=apply(d,j->schurModule(Partitions_j,R^d))
SEQ=apply(Koszul,j->toSequence(j.cache.Schur))
-----Koszul map form 
Koszul_1
Koszul_2
------Map from Koszul 1 to Koszul 2
-----basis for the module
ST=keys((SEQ_1)_2)
TAB=ST_0
L2=apply(d,s->(AddColumn(TAB,0,s),x_s))

Limage=apply(L2,j->(
     N:=normalize(j_0);
     if N_0!=0 then (j_1*N_0,N_1)
     ))          
Limage=select(Limage,x->x=!=null)
F=(x)->(x)
G=(x)->(x-2)

F(10)
Nueva=(F)->(
     F(34))
Nueva(G)

MODU=Koszul_2
TAB=(SEQ_2)_3
sum apply(Limage,j->j_0*MODU_(TAB#(j_1)))

apply(



apply(IMAGE,j->
apply(ST,j->
map(Koszul_2,Koszul_1,



ST=keys ((Koszul_2).cache.Schur)_2
ST_0
L=apply(d,j->AddColumn(ST_0,0,j))
(normalize(L_1))_0
L_0
Koszul_3.
N=(normalize(L_3))_1
AT=((Koszul_3).cache.Schur)_3
N
(Koszul_3)_(AT#N)
AT

(Koszul_3)_((normalize(L_3))_1)
apply(L,j->if (normalize(j))_0!=0 then (Koszul_2)#(normalize(j))_1)


apply(
apply(ST,j->(
	  apply(d,s->


---- Functoriality
kk=QQ
M1=kk^3
M2=kk^3
M=matrix{{1,2,4},{3,9,27},{4,16,64}}
F=map(M2,M1,substitute(M,kk))
det(M)

schur({1,1},F)
minors(2,M)
schur({1,1,1},F)
-----------



res coker syz L
S=kk^1

F=map(M,N,substitute(matrix{{1,1,1},{1,1,1},{1,1,1}},kk))
G=map(S,M,substitute(matrix{{1,1,-2}},kk))
G*F

--------
restart
kk=QQ
R=kk[x_1,x_2,x_3,x_4]
I=ideal(x_1,x_2,x_3,x_4)
J=I^2
res coker gens J

----------------Koszul complex
kk=QQ
d=4
R=kk[x_1..x_d]
Partitions=apply(d,j->apply(j+1,s->1))
apply(Partitions,j->schurModule(j,R^d))
Kmap=(T)->(
     apply(d,j->(addColumn(T,0,j),x_j))
)
T=new Filling from {{1,2,3}}
Kmap(T)

AddColumn(0

F=(T)->(

     )

 
 
toSequence(F2.cache.Schur)

F1=schurModule({1,1,1},R^4) 
F2=schurModule({1,1},R^4)

new Filling from {{3,2,1}}
