scalarProduct=method()
     scalarProduct(Matrix,Matrix):=(X,Y)->sum(3,i->X^{i}*Y^{i})
     scalarProduct(Matrix):=(X)->sum(3,i->X^{i}^2)

setupNRevolute=(kk,N)->(     
     R=kk[T,A_(0,0)..A_(N-1,2),B_(0,0)..B_(N-1,2),MonomialSize=>8];
     RT=kk[T,A_(0,0)..A_(N-1,2),B_(0,0)..B_(N-1,2),
	  a_(0,0)..a_(N-1,4),Degrees=>{6*N+1:1,5*N:2},MonomialSize=>8];
     Aa=genericMatrix(RT,A_(0,0),3,N);
     Bb=genericMatrix(RT,B_(0,0),3,N);
     Aa=Aa|Aa;
     Bb=Bb|Bb;
     aa=genericMatrix(RT,a_(0,0),5,N);
     J=sum(N,i->ideal(scalarProduct(Aa_{i+1}-Aa_{i})-matrix{{a_(i,0)}})+
                ideal(scalarProduct(Bb_{i}-Aa_{i})-matrix{{a_(i,1)}})+
                ideal(scalarProduct(Bb_{i+1}-Aa_{i})-matrix{{a_(i,2)}})+
                ideal(scalarProduct(Aa_{i+1}-Bb_{i})-matrix{{a_(i,3)}})+
       	        ideal(scalarProduct(Bb_{i+1}-Aa_{i+1},Bb_{i}-Aa_{i})-matrix{{a_(i,4)}}));
     phi0=sum(N,i->ideal(scalarProduct(Aa_{i+1}-Aa_{i}))+
                   ideal(scalarProduct(Bb_{i}-Aa_{i}))+
     	           ideal(scalarProduct(Bb_{i+1}-Aa_{i}))+
     	           ideal(scalarProduct(Aa_{i+1}-Bb_{i}))+
     	           ideal(scalarProduct(Bb_{i+1}-Aa_{i+1},Bb_{i}-Aa_{i})));
     phi1=substitute(gens phi0,R);
     s1=substitute(ideal Aa_{0}+ideal (Bb_{0}-transpose matrix{{0,0,1_RT}})+
     	           ideal (Aa_{1}-transpose matrix{{1_RT,0,0}}),R);
     phi=phi1%s1;
     Rs=vars R%s1;
     use RT;
     s=ideal Aa_{0}+ideal (Bb_{0}-transpose matrix{{0,0,1_RT}})+
       ideal (Aa_{1}-transpose matrix{{1_RT,0,0}})+ideal(a_(0,0)-1,a_(0,1)-1,a_(0,3)-2);
     RTs=vars RT%s;
     aa=substitute(aa,RTs);
     betti J)

pickAssemble=(N)->(ar:=substitute(Rs,random(R^1,R^(6*N+1)));
	  RTr:=vars R|substitute(phi,ar);
	  aar:=substitute(aa,RTr);
     	  Jr:=substitute(J+s,RTr);
	  (Jr,aar))
    
search=(N,n)->(I:=ideal 0_R;ap:= matrix{{0_RT}};pick:=(I,ap);
     tally apply(n,c->(pick:=pickAssemble(N);
     	       I=pick_0;
	       if dim I>1 then L=append(L,pick);(dim I,degree I))))    

search0=(N,n)->(I:=ideal 0_R;ap:= matrix{{0_RT}};pick:=(I,ap);
     tally apply(n,c->(pick:=pickAssemble(N);
     	       I=pick_0;
	       )));

search1=(N,n)->(I:=ideal 0_R;ap:= matrix{{0_RT}};pick:=(I,ap);
     tally apply(n,c->(pick:=pickAssemble(N);
     	       I=pick_0;gb I;
	       )));

search2=(N,n)->(I:=ideal 0_R;ap:= matrix{{0_RT}};pick:=(I,ap);
     tally apply(n,c->(pick:=pickAssemble(N);
     	       I=pick_0;(dim I,degree I)
	       )))

search3=(N,n)->(I:=ideal 0_R;ap:= matrix{{0_RT}};pick:=(I,ap);
     tally apply(n,c->(if c%500==0 then print c;pick:=pickAssemble(N);
     	       I=pick_0;(dim I,degree I);
	       )))

search4=(N,n)->(I:=ideal 0_R;ap:= matrix{{0_RT}};pick:=(I,ap);
     tally apply(n,c->(if c%500==0 then print c;pick:=pickAssemble(N);
     	       I=pick_0;dim I;))
	       )

loop=(N,n)->(I:=ideal 0_R;ap:= matrix{{0_RT}};pick:=(I,ap);
     tally apply(n,c->(pick:=pickAssemble(N);
     	       I=pick_0; print toString I; (dim I,degree I)
	       )))


end

restart
load "1-2schreyer-memleak.m2"
kk=ZZ/5
setupNRevolute(kk,4)
--pick=pickAssemble(4);I=pick_0;pick_1;dim I
L={}
setRandomSeed("alpha5")
time search(4,5^5)
time search0(4,5^5) -- doesn't go up much...
time search1(4,5^5) -- doesn't go up (goes to VSIZE 144 M)
time search2(4,5^5) -- did go up (I didn't wait for entire run though).
time search3(4,5^5) -- up to 244 M (227 M with MonomialSize=>8)
time search4(4,5^5) -- 177 M with MonomialSize=>8

restart
kk=ZZ/5
R = kk [T, A_(0,0), A_(0,1), A_(0,2), A_(1,0), A_(1,1), A_(1,2), A_(2,0), A_(2,1), A_(2,2), A_(3,0), A_(3,1), A_(3,2), B_(0,0), B_(0,1), B_(0,2), B_(1,0), B_(1,1),
     B_(1,2), B_(2,0), B_(2,1), B_(2,2), B_(3,0), B_(3,1), B_(3,2), MonomialSize => 8]
loopJ = ideal (A_(0,0)^2+A_(0,1)^2+A_(0,2)^2-2*A_(0,0)*A_(1,0)+A_(1,0)^2-2*A_(0,1)*A_(1,1)+A_(1,1)^2-2*A_(0,2)*A_(1,2)+A_(1,2)^2-1,
     A_(0,0)^2+A_(0,1)^2+A_(0,2)^2-2*A_(0,0)*B_(0,0)+B_(0,0)^2-2*A_(0,1)*B_(0,1)+B_(0,1)^2-2*A_(0,2)*B_(0,2)+B_(0,2)^2-1,
     A_(0,0)^2+A_(0,1)^2+A_(0,2)^2-2*A_(0,0)*B_(1,0)+B_(1,0)^2-2*A_(0,1)*B_(1,1)+B_(1,1)^2-2*A_(0,2)*B_(1,2)+B_(1,2)^2-2,
     A_(1,0)^2+A_(1,1)^2+A_(1,2)^2-2*A_(1,0)*B_(0,0)+B_(0,0)^2-2*A_(1,1)*B_(0,1)+B_(0,1)^2-2*A_(1,2)*B_(0,2)+B_(0,2)^2-2,
     A_(0,0)*A_(1,0)+A_(0,1)*A_(1,1)+A_(0,2)*A_(1,2)-A_(1,0)*B_(0,0)-A_(1,1)*B_(0,1)-A_(1,2)*B_(0,2)-A_(0,0)*B_(1,0)+
       B_(0,0)*B_(1,0)-A_(0,1)*B_(1,1)+B_(0,1)*B_(1,1)-A_(0,2)*B_(1,2)+B_(0,2)*B_(1,2)+2,A_(1,0)^2+A_(1,1)^2+A_(1,2)^2-
       2*A_(1,0)*A_(2,0)+A_(2,0)^2-2*A_(1,1)*A_(2,1)+A_(2,1)^2-2*A_(1,2)*A_(2,2)+A_(2,2)^2-1,A_(1,0)^2+A_(1,1)^2+
       A_(1,2)^2-2*A_(1,0)*B_(1,0)+B_(1,0)^2-2*A_(1,1)*B_(1,1)+B_(1,1)^2-2*A_(1,2)*B_(1,2)+B_(1,2)^2-2,A_(1,0)^2+
       A_(1,1)^2+A_(1,2)^2-2*A_(1,0)*B_(2,0)+B_(2,0)^2-2*A_(1,1)*B_(2,1)+B_(2,1)^2-2*A_(1,2)*B_(2,2)+B_(2,2)^2-1,
     A_(2,0)^2+A_(2,1)^2+A_(2,2)^2-2*A_(2,0)*B_(1,0)+B_(1,0)^2-2*A_(2,1)*B_(1,1)+B_(1,1)^2-2*A_(2,2)*B_(1,2)+B_(1,2)^2+2,
     A_(1,0)*A_(2,0)+A_(1,1)*A_(2,1)+A_(1,2)*A_(2,2)-A_(2,0)*B_(1,0)-A_(2,1)*B_(1,1)-A_(2,2)*B_(1,2)-A_(1,0)*B_(2,0)+B_(1,0)*B_(2,0)-A_(1,1)*B_(2,1)+B_(1,1)*B_(2,1)-A_(1,2)*B_(2,2)+B_(1,2)*B_(2,2)-2,A_(2,0)^2+A_(2,1)^2+A_(2,2)^2-2*A_(2,0)*A_(3,0)+A_(3,0)^2-2*A_(2,1)*A_(3,1)+A_(3,1)^2-2*A_(2,2)*A_(3,2)+A_(3,2)^2+1,A_(2,0)^2+A_(2,1)^2+A_(2,2)^2-2*A_(2,0)*B_(2,0)+B_(2,0)^2-2*A_(2,1)*B_(2,1)+B_(2,1)^2-2*A_(2,2)*B_(2,2)+B_(2,2)^2+1,A_(2,0)^2+A_(2,1)^2+A_(2,2)^2-2*A_(2,0)*B_(3,0)+B_(3,0)^2-2*A_(2,1)*B_(3,1)+B_(3,1)^2-2*A_(2,2)*B_(3,2)+B_(3,2)^2+1,A_(3,0)^2+A_(3,1)^2+A_(3,2)^2-2*A_(3,0)*B_(2,0)+B_(2,0)^2-2*A_(3,1)*B_(2,1)+B_(2,1)^2-2*A_(3,2)*B_(2,2)+B_(2,2)^2+2,A_(2,0)*A_(3,0)+A_(2,1)*A_(3,1)+A_(2,2)*A_(3,2)-A_(3,0)*B_(2,0)-A_(3,1)*B_(2,1)-A_(3,2)*B_(2,2)-A_(2,0)*B_(3,0)+B_(2,0)*B_(3,0)-A_(2,1)*B_(3,1)+B_(2,1)*B_(3,1)-A_(2,2)*B_(3,2)+B_(2,2)*B_(3,2)+2,A_(0,0)^2+A_(0,1)^2+A_(0,2)^2-2*A_(0,0)*A_(3,0)+A_(3,0)^2-2*A_(0,1)*A_(3,1)+A_(3,1)^2-2*A_(0,2)*A_(3,2)+A_(3,2)^2-2,A_(3,0)^2+A_(3,1)^2+A_(3,2)^2-2*A_(3,0)*B_(3,0)+B_(3,0)^2-2*A_(3,1)*B_(3,1)+B_(3,1)^2-2*A_(3,2)*B_(3,2)+B_(3,2)^2-1,A_(3,0)^2+A_(3,1)^2+A_(3,2)^2-2*A_(3,0)*B_(0,0)+B_(0,0)^2-2*A_(3,1)*B_(0,1)+B_(0,1)^2-2*A_(3,2)*B_(0,2)+B_(0,2)^2-1,A_(0,0)^2+A_(0,1)^2+A_(0,2)^2-2*A_(0,0)*B_(3,0)+B_(3,0)^2-2*A_(0,1)*B_(3,1)+B_(3,1)^2-2*A_(0,2)*B_(3,2)+B_(3,2)^2-1,A_(0,0)*A_(3,0)+A_(0,1)*A_(3,1)+A_(0,2)*A_(3,2)-A_(3,0)*B_(0,0)-A_(3,1)*B_(0,1)-A_(3,2)*B_(0,2)-A_(0,0)*B_(3,0)+B_(0,0)*B_(3,0)-A_(0,1)*B_(3,1)+B_(0,1)*B_(3,1)-A_(0,2)*B_(3,2)+B_(0,2)*B_(3,2)+2,A_(0,0),A_(0,1),A_(0,2),B_(0,0),B_(0,1),B_(0,2)-1,A_(1,0)-1,A_(1,1),A_(1,2),0,0,0)
-- in gdb, M2 starts with 125.48 VM, 83.27 MB real memory
for i from 0 to 1000 do (I := ideal flatten entries gens loopJ; (dim I, degree I)) -- leaks lots of monideals, minode stashes
for i from 0 to 1000 do (I := ideal flatten entries gens loopJ; dim I) -- leaks lots of monideals, minode stashes
time for i from 0 to 1000 do (I := ideal flatten entries gens loopJ; degree I) -- HF links
time for i from 0 to 100 do (I := ideal flatten entries gens loopJ; degree I) -- no leak...
time for i from 0 to 100 do (I := ideal flatten entries gens loopJ; codim I) -- leaks one monideal per loop
time for i from 0 to 1000 do (I := ideal flatten entries gens loopJ; gens gb I;) -- VM 142.73 MB
time for i from 0 to 500 do (I := ideal flatten entries gens loopJ; gens gb I;) -- VM immediately jumps to 142.73 MB
time for i from 0 to 1000 do (I := ideal flatten entries gens loopJ; poincare I) -- HF links
time for i from 0 to 100 do (I := ideal flatten entries gens loopJ; poincare I) -- HF links
-- 6 Feb 2008
time for i from 0 to 199 do (stderr << "." << flush; I := ideal flatten entries gens loopJ; (dim I, degree I)) -- no more leaks
time for i from 0 to 199 do (stderr << "." << flush; I := ideal flatten entries gens loopJ; gb I) -- no more leaks
inJ = (I := ideal flatten entries gens loopJ; leadTerm gens gb I); collectGarbage();
time for i from 0 to 999 do (J := ideal flatten entries inJ; poincare J) -- no leak anymore
inJ = (I := ideal flatten entries gens loopJ; leadTerm gb I); collectGarbage();
debug Core
time for i from 0 to 4000 do (J := ideal flatten entries inJ; rawHilbert raw inJ) -- no leaks here
time for i from 0 to 4000 do (J := ideal flatten entries inJ; leadTerm gb presentation cokernel inJ) -- no more leak
time for i from 0 to 4000 do (J := ideal flatten entries inJ; leadTerm gb inJ) -- mo more leak
time for i from 0 to 4000 do (J := ideal flatten entries inJ; gb J) -- no leak here
gJ = gb loopJ
time for i from 0 to 4000 do (leadTerm gJ) -- 





for i from 0 to 1000 do (I := ideal flatten entries gens loopJ; dim I) -- leaks lots of monideals, minode stashes
time for i from 0 to 1000 do (I := ideal flatten entries gens loopJ; degree I) -- HF links
time for i from 0 to 100 do (I := ideal flatten entries gens loopJ; degree I) -- no leak...
time for i from 0 to 100 do (I := ideal flatten entries gens loopJ; codim I) -- leaks one monideal per loop
time for i from 0 to 1000 do (I := ideal flatten entries gens loopJ; gens gb I;) -- VM 142.73 MB
time for i from 0 to 500 do (I := ideal flatten entries gens loopJ; gens gb I;) -- VM immediately jumps to 142.73 MB
time for i from 0 to 1000 do (I := ideal flatten entries gens loopJ; poincare I) -- HF links
time for i from 0 to 100 do (I := ideal flatten entries gens loopJ; poincare I) -- HF links


----------------------------
-- 'poincare' is leaking 2 monideals per loop in the above loop.
-- the following also leaks 2 monomial ideals:
R = ZZ/101[a..e]
I = ideal"c2,cd,ce,d2,de,e2"
poincare I

debug Core
monomialIdealOfRow = (i,m) -> newMonomialIdeal(ring m,rawMonomialIdeal(raw m, i))
time for i from 0 to 100 do (monomialIdealOfRow(0,gens I))
J = monomialIdeal gens I
time for i from 0 to 100 do rawCodimension raw J
----------------------------
restart
load "NRevolute.m2" -- This used to be the name of this file...

kk=ZZ/3
setupNRevolute(kk,4)
pick=pickAssemble(4);I=pick_0;pick_1;dim I
L={}
setRandomSeed("alpha5")
time search(4,3^6)


#L/3^3+0.0==2.25926
apply(L,c->(dim c_0,degree c_0))
L20=select(L,c->dim c_0==2 and degree c_0==20)

restart

load "NRevolute.m2"
kk=ZZ/5
setupNRevolute(kk,4)
pick=pickAssemble(4);I=pick_0;pick_1;dim I
L={}
setRandomSeed("alpha5")
time search(4,5^5)

--23.15*5^3/60
#L/5^2+0.0

 "4RevoluteChar5" << toString L <<endl <<close
apply(L,c->(dim c_0,degree c_0))
L20=select(L,c->dim c_0==2 and degree c_0==20)
