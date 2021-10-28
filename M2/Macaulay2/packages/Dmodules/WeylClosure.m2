-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This routine computes the Weyl closure of a D-ideal if
-- the module is specializable to its singular locus.
-- General algorithm still needs to be implemented.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
WeylClosure = method()
WeylClosure(Ideal, RingElement) := (I, f) -> (
     W := ring I;
     if W.monoid.Options.WeylAlgebra === {} then
     error "Expected a Weyl algebra" ;

     M := cokernel gens I;
     --<< "Computing localization" << endl;
     locMap := DlocalizeMap(M, f);
     J := I + ideal gens kernel locMap;
     --if (I == J) then << "Ideal is closed with respect to f" << endl
     --else << "Ideal is not closed with respect to f" << endl;
     ideal gens gb J
     )
     
WeylClosure Ideal := I -> (
     W := ring I;
     outputList :={};
     if W.monoid.Options.WeylAlgebra === {} then
     error "Expected a Weyl algebra" ;

     if holonomicRank I === infinity then
     error "Weyl closure only currently implemented for finite rank ideals.";
     
     --<< "Computing singular locus" << endl;
     SL := singLocus I;
     f := (gens SL)_(0,0);
     --<< f << endl;
     
     M := cokernel gens I;
     --<< "Computing localization" << endl;
     locMap := DlocalizeMap(M, f);
     J := I + ideal gens kernel locMap;
     --if (I == J) then << "Ideal is closed " << endl
     --else << "Ideal is not closed " << endl;
     
     --ideal gens gb J
     J
     )

TEST ///
-- test 1: annihilator 1/t-t^2-x
x = symbol x; Dx = symbol Dx; 
t = symbol t; Dt = symbol Dt; 
W = QQ[x,t,Dx,Dt, WeylAlgebra => {x=>Dx, t=>Dt}];
f = t-t^2-x;
I = ideal(Dx*f, Dt*f);
assert ( WeylClosure(I) == 
     I + ideal (-2*t*Dx^2*Dt^3 + Dx^2*Dt^3 + Dx*Dt^4 - 6*Dx^2*Dt^2));

-- test 2: annihilator of e^(1/x^3-y^2*z^2)
x = symbol x; Dx = symbol Dx; 
y = symbol y; Dy = symbol Dy; 
z = symbol z; Dz = symbol Dz; 
W = QQ[x,y,z,Dx,Dy,Dz, WeylAlgebra => {x=>Dx, y=>Dy, z=>Dz}];
f = (x^3-y^2*z^2);
I = ideal(f^2*Dx+3*x^2, f^2*Dy-2*y*z^2, f^2*Dz-2*y^2*z);
assert ( WeylClosure(I) == 
     I + ideal(y*Dy-z*Dz, y^2*z^3*Dz-(2/3)*x^4*Dx-2*x^3*z*Dz-2));
///