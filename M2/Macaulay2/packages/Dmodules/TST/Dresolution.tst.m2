-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needs "Dmodules.m2"

------------------------- TESTS for Drestriction ----------------------------

-- Boundary cases
x = symbol x; Dx = symbol Dx; 
W = QQ[x, Dx, WeylAlgebra => {x=>Dx}];
I = ideal 0_W;
J = ideal 1_W;
w = {-1,1}
assert( Dres(I) == Dres(I, w) );
assert( Dres(W^1/I) == Dres(W^1/I, w) );
assert( Dres(module I) == Dres(module I, w) );
assert( Dres(J) == Dres(J, w) );
assert( Dres(W^1/J) == Dres(W^1/J, w) );
assert( Dres (module J) == Dres(module J, w) );

-- Resolutions in the same res Grobner cone
A = matrix{{1,1,1},{1,3,6}};
b = {3,2};
I = gkz(A,b, Vars=>Local);
F1 = Dres(I, {-1,-2,-21,1,2,21});
F2 = Dres(I, {-1,-2,-20,1,2,20});
assert all(toList(0..length F1), i -> zeroize F1.dd#i == zeroize F2.dd#i);

F3 = Dres(I, {-1,-2,-21,1,2,21}, Strategy => Vhomogenize);
F4a = Dres(I, {-1,-2,-20,1,2,20}, Strategy => Vhomogenize);
assert all(toList(0..length F3), i -> zeroize F3.dd#i == zeroize F4a.dd#i);

F5 = Dres(I, {-3,-1,-3,3,1,3});
assert(zeroize F5.dd#1 == zeroize gbw(gens I, {-3,-1,-3,3,1,3}));
