-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needsPackage "Dmodules";

--Boundary cases
x = symbol x; y = symbol y;
R = QQ[x,y]
default := hashTable {0=>QQ^1, 1=>QQ^0, 2=>QQ^0};
F1 = deRham(0_R); -- empty set
F2 = deRham(1_R); -- affine space
assert all (keys default, i -> (F1#i == default#i));
assert all (keys default, i -> (F2#i == default#i));

--Small change doesn't affect deRham groups
F1 = deRham(x^2-30*y^3)
F2 = deRham(x^2-31*y^3)
assert all (keys F1, i -> (F1#i == F2#i));

--These lead to problems in factor at the moment
F1 = deRham(x^2-y^10)
F2 = deRham(2*x^2 - 3*y^10)
assert all (keys F1, i -> (F1#i == F2#i));

