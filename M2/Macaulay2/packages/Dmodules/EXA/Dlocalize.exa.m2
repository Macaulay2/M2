restart
needsPackage "Dmodules"
------------------------- EXAMPLES for Dlocalization --------------------------------

-- Example 1: Simple example
W = QQ[x,dx,WeylAlgebra=>{x=>dx}]
M = cokernel matrix{{dx}}
Dlocalize(M, x)
DlocalizeMap(M, x)
DlocalizeAll(M, x)
Dlocalize(M, x, Strategy => Oaku)
DlocalizeMap(M, x, Strategy => Oaku)
DlocalizeAll(M, x, Strategy => Oaku)


-- Example 2: Cusp
W = QQ[x,y,dx,dy, WeylAlgebra=>{x=>dx,y=>dy}]
M = cokernel matrix{{dx,dy}}
f = x^2-y^3
Dlocalize(M, f)
DlocalizeAll(M, f)
Dlocalize(M, f, Strategy => Oaku)
DlocalizeAll(M, f, Strategy => Oaku)


-- Example 3: from Coutinho "Primer..."
n = 4
W = QQ[u_1..u_n, Du_1..Du_n, WeylAlgebra => 
     apply(toList(1..n), i -> u_i => Du_i)]
M = W^1/ideal(Du_1..Du_n)
f = sum(toList(1..n), i -> u_i^2)
Dlocalize(M, f)
DlocalizeAll(M, f)
Dlocalize(M, f, Strategy => Oaku)
DlocalizeAll(M, f, Strategy => Oaku)


-- Example 4: from OTW, Example 8
W = QQ[x,y,z,Dx,Dy,Dz,WeylAlgebra => {x=>Dx,y=>Dy,z=>Dz}]
I = ideal((x^3-y^2*z^2)^2*Dx+3*x^2,
     (x^3-y^2*z^2)^2*Dy-2*y*z^2,
     (x^3-y^2*z^2)^2*Dz-2*y^2*z) -- some annihilators of e^(1/x3-y2z2)
nh = x  -- non-holonomic locus
M = W^1/I
Dlocalize(M,nh)
DlocalizeAll(M,nh)
Dlocalize(M, nh, Strategy => Oaku) -- NOTE THAT I IS NOT SATURATED
DlocalizeAll(M, nh, Strategy => Oaku) -- LEADS TO EXTRA FACTOR IN BFUNCTION?


-- Example 5a: Simple example -- pure torsion module
W = QQ[x,dx,WeylAlgebra=>{x=>dx}]
M = cokernel matrix{{x}}
Dlocalize(M, x)
DlocalizeMap(M, x)
DlocalizeAll(M, x)
Dlocalize(M, x, Strategy => Oaku)
DlocalizeMap(M, x, Strategy => Oaku)
DlocalizeAll(M, x, Strategy => Oaku)

-- Example 5b: Simple example -- already localized
W = QQ[x,dx,WeylAlgebra=>{x=>dx}]
M = cokernel matrix{{x*dx+7}}
Dlocalize(M, x)
DlocalizeMap(M, x)
DlocalizeAll(M, x)
Dlocalize(M, x, Strategy => Oaku)
DlocalizeMap(M, x, Strategy => Oaku)
DlocalizeAll(M, x, Strategy => Oaku)

N = cokernel matrix{{x*dx+1/2}}
Dlocalize(N, x)
Dlocalize(N, x, Strategy => Oaku)

-- Example 6: Boundary cases
W = QQ[x,dx,WeylAlgebra=>{x=>dx}]
M = cokernel matrix{{dx}}
Dlocalize(M, 0_W)
Dlocalize(M, 0_W, Strategy => Oaku)
Dlocalize(M, 1_W)
Dlocalize(M, 1_W, Strategy => Oaku)

-- Example 7: Compare OTW and OTWcyclic
W = makeWA (QQ[x,y,z])
I = PolyAnn(x*y-z^2+2)
fs = {x*y+z^2-4,x*y*z,x*y+z^3,x*y*z+z^2} 
for f in fs do (
    elapsedTime resOTW := Dlocalize(I,f,Strategy => OTW);
    elapsedTime resOTWcyclic := Dlocalize(I,f,Strategy => OTW);
    assert(resOTW == resOTWcyclic)
    )

-- Example 8:
W = makeWA(QQ[x,y]);
M = coker matrix{{dx,dy,0,0},{0,0,x,y}}
f=x^2+y^2;
Mf=Dlocalize(M,f); 
assert(Mf == Dlocalize(W^1/ideal(dx,dy),f))
