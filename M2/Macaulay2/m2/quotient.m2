--		Copyright 1994 by Daniel R. Grayson

gcdCoefficients(ZZ,ZZ) := (a,b) -> (
     m := {a,1,0};
     n := {b,0,1};
     if a<0 then m=-m;
     if b<0 then n=-n;
     if a>b then (k :=m;m=n;n=k);
     while m#0 > 0 do (
	  t := n#0 // m#0;
	  n = n - apply(m,y -> t * y);
	  (k=m;m=n;n=k);
	  );
     {n#1,n#2});

document { quote gcdCoefficients,
     TT "gcdCoefficients(a,b)", " -- returns ", TT "{r,s}", " so that
     ", TT"a*r + b*s", " is the greatest common divisor of ", TT "a", "
     and ", TT "b", ".",
     PARA,
     "Works for integers or elements of polynomial rings.",
     SEEALSO "gcd"
     }

mod = (i,n) -> i * 1_(ZZ/n)

document { quote mod,
     TT "mod(i,n)", " -- reduce the integer i modulo n, producing an
     element of ZZ/n."
     }

