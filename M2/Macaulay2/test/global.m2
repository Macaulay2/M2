-- check that HH^0 is compatible with direct sum of sheaves with different dimension of support

S = QQ[x..z]
X = Proj S
m = ideal vars S
scan( { (4,4), (6,6), (4,6), (6,4), (2,6), (6,2), (-2,3), (3,-2), (0,0) },
  (a,b) -> (
    scan(3, 
      i -> (
        F = sheaf_X (m^i * S^{-a});
        HF = HH^0 F;
        scan(3, 
          j -> (
            G = sheaf_X (m^j * S^{-b}/(y,z));
            HG = HH^0 G;
            HFG = HH^0 (F++G);
            scan(0 .. 10,
              n -> (
                r = rank source basis(n, HF);
                s = rank source basis(n, HG);
                t = rank source basis(n, HFG);
                assert( r+s == t );
                -- stderr << r << " + " << s << " = " << t << endl;
                ))))))))
