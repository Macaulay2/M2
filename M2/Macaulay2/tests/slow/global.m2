-- sheaves on P^0
R = QQ[x]
X = Proj R
scan(-4 .. 4, i -> assert( HH^0 OO_X(i) == QQ^1 ))
scan(-4 .. 4, i -> assert( HH^1 OO_X(i) == QQ^0 ))

scan(0 .. 4, i -> assert( HH^0 (OO_X)(>=i) == R^{0} ))
scan(-5 .. -1, i -> assert( HH^0 (OO_X)(>=i) == R^{-i} ))

-- sheaves on P^1
R = QQ[x,y]
X = Proj R

scan(-6 .. -1, i -> assert( HH^0 OO_X(i) == QQ^0))
scan(-1 .. 5, i -> assert( HH^0 OO_X(i) == QQ^(i+1)))

scan(-6 .. -1, i -> assert( HH^1 OO_X(i) == QQ^-(i+1)))
scan(-1 .. 5, i -> assert( HH^1 OO_X(i) == QQ^0))

scan(-4 .. 4, i -> assert( HH^0 (OO_X(i))(*) == R^{i}))
scan(-4 .. 4, i -> assert( HH^0 (OO_X(i))(>-5) == R^{i}))
scan(-4 .. 4, i -> assert( HH^0 (OO_X(i))(>5) == R^{i}))

-- sheaves on P^2
R = QQ[x,y,z]
X = Proj R
k = coker vars R

scan(-6 .. -1, i -> assert( HH^0 OO_X(i) == QQ^0))
scan(-1 .. 5, i -> assert( HH^0 OO_X(i) == QQ^(binomial(i+2,2))))

scan(-5 .. 5, i -> assert( HH^1 OO_X(i) == QQ^0))

scan(-2 .. 4, i -> assert( HH^2 OO_X(i) == QQ^0))
scan(-7 .. -3, i -> assert( HH^2 OO_X(i) == QQ^(binomial(-i-1,2))))

scan(-5 .. 5, i -> assert( HH^3 OO_X(i) == QQ^0))

scan(-4 .. 4, i -> assert( HH^0 (OO_X(i)) (*) == R^{i}))
scan(-4 .. 4, i -> assert( HH^1 (OO_X(i)) (>=0) == R^0))

scan( -2 .. 4 , i -> assert( HH^2 (OO_X) (>=i) == R^0 ))
assert( HH^2 (OO_X) (>=-3) == R^{3} ** k)
scan(-7 .. -3, i -> assert( degree HH^2 (OO_X) (>=i) == binomial(-i,3) ))

-- sheaves on an elliptic curve
R = QQ[x,y,z]
S = R/(x^3+y^3+z^3)
X = Proj S
I = sheaf_X module ideal(x,y+z)
assert( degree ((OO_X)^1/I) == 1 )
L = dual I
L^**2
L^**3
M = prune L^**3
assert( module M == module OO_X(1) ) -- eventually remove the 'module's here after we install the right algorithm for CoherentSheaf == CoherentSheaf !
scan(1 .. 4, i -> HH^0 L^**i == QQ^i)
scan(1 .. 4, i -> HH^0 I^**i == QQ^0)

-- check that HH^0 is compatible with direct sum of sheaves with different dimension of support

S = QQ[x..z]
X = Proj S
m = ideal vars S
scan( { (4,4), (6,6), (4,6), (6,4), (2,6), (6,2), (-2,3), (3,-2), (0,0) },
  (a,b) -> (
    scan(3, 
      i -> (
        F = sheaf_X (m^i * S^{-a});
        HF = HH^0 F(>=0);
        scan(3, 
          j -> (
            G = sheaf_X (m^j * S^{-b}/(y,z));
            HG = HH^0 G(>=0);
            HFG = HH^0 (F++G)(>=0);
            scan(0 .. 10,
              n -> (
                r = rank source basis(n, HF);
                s = rank source basis(n, HG);
                t = rank source basis(n, HFG);
		if r+s != t then (
		     stderr
		     << "---------------------------------" << endl
		     << "a=" << a << "; b=" << b << "; i=" << i << "; j=" << j << "; n=" << n << ";" << endl
		     << "---------------------------------" << endl;
		     );
                assert( r+s == t );
                -- stderr << r << " + " << s << " = " << t << endl;
                ))))))))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test/slow global.out"
-- End:
