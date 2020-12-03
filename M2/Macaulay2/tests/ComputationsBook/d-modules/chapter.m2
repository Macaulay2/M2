load "D-modules.m2"
D = QQ[x,y,z,Dx,Dy,Dz, WeylAlgebra => {x=>Dx, y=>Dy, z=>Dz}]
Delta = ideal(Dx,Dy,Dz)
(Dx * x)^2
options D
DeltaBern = inw(Delta,{1,1,1,1,1,1}) 
dim DeltaBern 
D = QQ[x,y,z,w,Dx,Dy,Dz,Dw, 
       WeylAlgebra => {x=>Dx, y=>Dy, z=>Dz, w=>Dw}];
f = x^2+y^2+z^2+w^2
AnnFs(f)
L=ideal(x,y,Dz,Dw)
AnnIFs(L,f)
f
globalBFunction(f)
g=x^3+y^3+z^3+w^3
factorBFunction globalBFunction(g)
D1 = QQ[x,Dx,WeylAlgebra => {x=>Dx}];
I1 = ideal((x*Dx)^2+1)
f1 = x;
b=globalB(I1, f1)
use D
R = (D^1/ideal(Dx,Dy,Dz,Dw))
ann2 = relations Dlocalize(R,f)
F = matrix{{f}}
ann1 = gb modulo(F,ann2)
gb((ideal ann2) + (ideal F))
D = QQ[x,y,z,Dx,Dy,Dz, WeylAlgebra => {x=>Dx, y=>Dy, z=>Dz}];
Delta = ideal(Dx,Dy,Dz);
f=x^3+y^3+z^3;
I1=DlocalizeAll(D^1/Delta,f,Strategy=>Oaku)
I2=DlocalizeAll(D^1/Delta,f)
I1.LocModule
D= QQ[x,y,z,u,v,w,Dx,Dy,Dz,Du,Dv,Dw, WeylAlgebra =>
          {x=>Dx, y=>Dy, z=>Dz, u=>Du, v=>Dv, w=>Dw}];
Delta=ideal(Dx,Dy,Dz,Du,Dv,Dw);
R=D^1/Delta;
f=x*v-u*y;
g=x*w-u*z;
h=y*w-v*z;
Rf=DlocalizeAll(R,f,Strategy => Oaku)
Rfg=DlocalizeAll(Rf.LocModule,g, Strategy => Oaku)
Rfgh=DlocalizeAll(Rfg.LocModule,h, Strategy => Oaku)
Rf.GeneratorPower
Jfgh=ideal relations Rfgh.LocModule;
JH3=Jfgh+ideal(f^2,g,h);
JH3gb=gb JH3
testmTorsion = method();
testmTorsion Ideal := (L) -> (
     LL = ideal generators gb L;
     n = numgens (ring (LL)) // 2;
     LLL = ideal select(first entries gens LL, f->(
               l = apply(listForm f, t->drop(t#0,n));
               all(l, t->t==toList(n:0))       
               ));
     if dim inw(LLL,toList(apply(1..2*n,t -> 1))) == n
     then true
     else false);
testmTorsion(JH3)
D=QQ[x,y,z,w,Dx,Dy,Dz,Dw,WeylAlgebra => {x=>Dx, y=>Dy, z=>Dz,
w=>Dw}];
f=y^2-x*z;
g=z^2-y*w;
h=x*w-y*z;
Delta=ideal(Dx,Dy,Dz,Dw);
R=D^1/Delta;
Rf=DlocalizeAll(R,f,Strategy => Oaku)  
Rfg=DlocalizeAll(Rf.LocModule,g, Strategy => Oaku);
Rfgh=DlocalizeAll(Rfg.LocModule,h, Strategy => Oaku);
Ifgh=ideal relations Rfgh.LocModule;
IH3=Ifgh+ideal(f,g,h);
IH3gb=gb IH3
findSocle = method();
findSocle(Ideal, RingElement):= (L,P) -> (
     createDpairs(ring(L));
     v=(ring L).dpairVars#0;
     myflag = true;
     while myflag do (
          w = apply(v,temp -> temp*P % L);
          if all(w,temp -> temp == 0) then myflag = false
          else (
               p = position(w, temp -> temp != 0);
               P = v#p * P;)
          );
     P);
D = ring JH3
findSocle(JH3,1_D)
findLength = method();
findLength Ideal := (I) -> (   
     l = 0;
     while I != ideal 1_(ring I) do (
          l = l + 1;
          s = findSocle(I,1_(ring I));
          I = I + ideal s;);
     l);
findLength JH3
erase symbol x; erase symbol Dx;
D = QQ[x_1..x_5, Dx_1..Dx_5, WeylAlgebra =>
     apply(toList(1..5), i -> x_i => Dx_i)];
f = x_1^2 + x_2^2 + x_3^2 + x_4^2 +x_5^2;
g = x_1;
R = D^1/ideal(Dx_1,Dx_2,Dx_3,Dx_4,Dx_5);
Rf =DlocalizeAll(R,f,Strategy => Oaku);
Bf = Rf.Bfunction
Rfg = DlocalizeAll(Rf.LocModule,g,Strategy => Oaku);
Bfg = Rfg.Bfunction
Rg = DlocalizeAll(R,g,Strategy => Oaku);
Bg = Rg.Bfunction
Rgf = DlocalizeAll(Rg.LocModule,f,Strategy => Oaku);
Bgf = Rgf.Bfunction
erase symbol x;
R = QQ[x,y,z];
f=x^3+y^3+z^3;
H=deRhamAll(f);
H.CohomologyGroups
H.LocalizeMap
H.TransferCycles
I = gkz(matrix{{1,2}}, {5})
PolySols I
