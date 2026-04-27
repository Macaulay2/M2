-- Frank Screyer sent this in to us 7/9/2006.  It is supposed to
-- use a lot of memory.  But I don't see that effect.

scalarProduct=method()
     scalarProduct(Matrix,Matrix):=(X,Y)->sum(3,i->X^{i}*Y^{i})
     scalarProduct(Matrix):=(X)->sum(3,i->X^{i}^2)

setupNRevolute=(kk,N)->(     
     R=kk[T,A_(0,0)..A_(N-1,2),B_(0,0)..B_(N-1,2)];
     RT=kk[T,A_(0,0)..A_(N-1,2),B_(0,0)..B_(N-1,2),
	  a_(0,0)..a_(N-1,4),Degrees=>{6*N+1:1,5*N:2}];
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
     
end

restart

load "NRevolute.m2"

load "1-schreyer-memleak.m2"
kk=ZZ/5
setupNRevolute(kk,4)
pick=pickAssemble(4);I=pick_0;pick_1;dim I

L={}

time search(4,5^4)
#L
apply(L,c->dim c_0)

