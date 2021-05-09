-- old status message: unexpected segmentation fault on rhodium, but it happens only occasionally, and not in the debugger, so it is hard to fix
-- this test has random numbers in it, and the execution time (at least) depends on that.
-- I've added a random seed that makes it particularly slow, so we can try to debug it later
end -- deferred:
setRandomSeed 123423413
twiddle = (A,B,e,p) ->(
R:=ring(A);
F:=coefficientRing(R);
n:=rank source vars(R);
v=first entries vars(R);
R1:=F[v, Y_1..Y_n, MonomialOrder=>ProductOrder{n,n},MonomialSize=>16];
J0:=apply(1..n, i->Y_i-substitute(v#(i-1)^(p^e),R1));
S:=toList apply(1..n, i->Y_i=>substitute(v#(i-1),R1));
G:=first entries compress( (gens substitute(A,R1))%gens(ideal(J0)) );
L:=ideal 0_R1;
apply(G, t->
{
    L=L+ideal((coefficients({0..n-1},t))#1);
});
L1:=L+substitute(B,R1);
L2:=mingens L1;
L3:=first entries L2;
L4:=apply(L3, t->substitute(t,S));
substitute(ideal L4,R)
)



star = (I,u,e,p) ->(
f:=true;
Ne:=sum toList(apply(0..(e-1), i->p^i));
lastI:=I;
while (f) do
{
	f=false;
	I1:=twiddle((u^Ne)*lastI,lastI,e,p);
	t1:=compress (gens(I1))%(gens(lastI));
	if (t1!=0) then
	{
		f=true;
		lastI=I1;
	};
};
I1
)

IpI = (I,p) ->(
I1:=first entries I;
I2:=apply(I1, u->u^p);
I3=ideal(I2):ideal(I1);
S:=ring(I)/ideal(I2);
rank mingens substitute(I3,S)
)


p=2;
n=5; D=3; m=2*n;
R00=ZZ/p[x_1..x_n];

	E={};
	while (#E<m) do
	{
		d=2+(random (D-1));
		M= toList apply(1..d, i->x_(1+random(n)));
		M=product M;
		E=append(E,M);
	};
	E=unique (E);
	E=sort(E);

---n=6; E={{1,2},{2,3},{3,4},{1,4},{1,5},{5,6},{6,2}};
---n=5; E={{1, 4}, {2, 4}, {2, 3}, {1, 3}, {1, 5}, {2, 5}, {4, 5}};
---n=5; E= {{3, 4}, {1, 4}, {1, 3}, {2, 5}, {2, 3}, {1, 2}, {1, 5}, {2, 4}};
---n=5; E= {{1, 2},  {1, 3}, {1, 4}, {1, 5}, {2, 3},   {2, 4}, {2, 5}, {3,4}   };
e=#E;

R0=ZZ/p[x_1..x_n, y_1..y_e, MonomialOrder=>ProductOrder{n,e}];
I1=apply(1..e, i-> y_i - substitute(E#(i-1),R0) );
I2=ideal(I1);
G=gb I2;
I=selectInSubring(1,gens G); --!?

R=ZZ/p[ y_1..y_e,MonomialSize=>15];

I=substitute(I,R);
I1=first entries I;
I2=apply(I1, u->u^p);
I3=ideal(I2):ideal(I1);
I4=(gens I3)%(gens ideal I2);
I4=mingens ideal I4;
---print(I4);
I5=subquotient((gens I3),(gens ideal I2));
I6=prune(I5);

delta=e-dim(coker I)
Omega=prune Ext^delta(coker I, R^1);

S=R/ideal(I);
s1=syz transpose substitute(relations Omega,S);
print(s1);
s2=entries transpose s1;
use R;

---------------------
---------------------
---???
Omega=ideal(apply(s2#0, t->substitute(t,R)));
---------------------
---------------------


pd=pdim (coker I);
d=e-pd;
M=coker vars(R);
---Ed=Ext^d(M,coker I);
---CMtype=rank target relations Ed;
B=res coker I;
CMtype=rank (B_pd);
print(dim(coker I)-d,CMtype);

M1=coker I;
M2=prune subquotient(gens Omega,I);
p=2;
E1=Ext^delta(M2,R^1);
assert(p === 2)

end

-- Here is the problem which used to cause a problem above:

dobug = () -> (
  A'   := ZZ/5[Variables => 3];
  )
p = 2
dobug()
p
p_0

end


From: "Moty Katzman" <M.Katzman@Sheffield.ac.uk>
Date: December 7, 2006 11:45:35 AM EST
To: <Macaulay2@math.uiuc.edu>
Subject: M2 0.9.95  bug

Hello,

Here is some funny behaviour, the variable p becomes undefined:
i357 : p=2

o357 = 2

i358 : Omega=prune Ext^delta(coker I, R^1);

i359 : p

o359 = p

o359 : Symbol

Here is the version information:
i360 : version

o360 = HashTable{architecture => i686
                 compile node name => grayson-vmware
                 compile time => Nov  6 2006, 11:03:04
                 compiler => gcc 3.4.4
                 configure arguments =>  'LDFLAGS=-L/o/local/lib'
                 executable extension => .exe
                 factory version => 3.0.2
                 gc version => 6.7
                 gmp version => 4.2.1
                 libfac version => 3.0.1
                 M2 name => M2.exe
                 M2 suffix =>
                 ntl version => 5.4
                 operating system => CYGWIN_NT-5.1
                 operating system release => 1.5.20(0.156-4-2)
                 readline version => 5.1
                 VERSION => 0.9.95
       ---------------------------------------------------------------------
----

Here is the entire M2 code:
clearAll;


