
makeT = method()
makeT(Matrix, ChainComplex,ZZ) := (F,G,i) ->(
     {*
     If F is an m x 1 matrix and
     G is a resolution of a module at least up to the i-th step,
     over R = S/(ideal F), 
     of codim c this returns a list of the c ci-operators
     G_i \to G_{i-2}
     corresponding to the entries of F.
     *}
     c := numcols F;
     degsF := flatten((degrees F)_1);
     R := ring G;
     S := ring F;
     d0 := sub(G.dd_i, S);
     d1 := sub(G.dd_(i-1), S);
     Gtar := target d1;
     Gsour := source d0;
     d2 := d1*d0;
     utemp := local utemp;
     u := apply(c,i ->(
	     utemp = map(S^{-degsF_i}**Gtar, Gsour, d2//((target d2)**F_{i}));
	     d2 = d2 - utemp**F_{i};
	     utemp));
     --check: is d1*d0 = sum F_{i}*u_i 
     if d1*d0 != map(Gtar, Gsour, sum(c, i-> u_i**F_{i})) then 
                  error{"doesn't add up"};
     ret := map(R,S);
     apply(u, u1 -> ret u1)
     )


moduleAsExt = method()
moduleAsExt(Module,Ring) := (MM,R) ->(
    Ops := ring MM;
    reg := regularity MM;
    MMr := truncate(reg, MM);
    F := res MMr;
    K := res(coker vars R, LengthLimit => reg+numgens R);
    K2 := res(coker K.dd_3, LengthLimit=>5);
    T := makeT(presentation R, K, 2);
    Tmat := T_0;
    scan(drop(T,1), t->Tmat = Tmat||t);
    --Two subroutines
    insertT := phi -> (
	--replace each entry of phi by the 
	--appropriate linear combination of the rows of Tmat.
	--Note that the entries of phi must be linear forms of Ops
	--and the output is a matrix of scalars over thing ring R.
	v := vars ring phi;
	L := entries phi; -- list of lists of lin forms in Ops
        matrix apply(#L, i -> 
	    apply(#L_i, 
		j-> sub(diff(v, L_i_j),R)*Tmat))
	);
    dsum := (p,F)-> directSum apply(p, i->F);
    --End subroutines
    phi := F.dd_1;
    
    print betti (Ks := dsum(rank source phi, K));
    print betti (Kt := dsum(rank target phi,R^{2}**K2));
    phiT := map(Ks_0,Kt_0,insertT phi);
    print betti phiT;
    print "next command causes the crash";
    error "debug me";
--    error();
    extend(dsum(rank source phi, K), 
	dsum(rank target phi,R^{2}**K2), 
	phiT)    
        )
end

restart
load "~/src/M2-git/bugs/mike/00-eisenbud-bug-extend.m2"
  kk = ZZ/101;
  S = kk[a,b,c];
  ff = matrix{{a^2, b^2}};
  R = S/ideal ff;
    dsum := (p,F)-> directSum apply(p, i->F);
    K = res(coker vars R, LengthLimit => 5);
C1 = dsum(3, K)
gb C1.dd_2


  Ops = kk[x_1,x_2]
  MM = Ops^1/ideal(x_1^2*x_2)  


    Ops = ring MM;
    reg = regularity MM;
    MMr = truncate(reg, MM);
    F = res MMr;
    K = res(coker vars R, LengthLimit => reg+numgens R);
    dsum := (p,F)-> directSum apply(p, i->F);

    K = res(coker vars R, LengthLimit => 5);
C1 = dsum(3, K)
gb C1.dd_2


    K2 = res(coker K.dd_3, LengthLimit=>5);
    T = makeT(presentation R, K, 2);
    Tmat := T_0;
    scan(drop(T,1), t->Tmat = Tmat||t);
    --Two subroutines
    insertT := phi -> (
	--replace each entry of phi by the 
	--appropriate linear combination of the rows of Tmat.
	--Note that the entries of phi must be linear forms of Ops
	--and the output is a matrix of scalars over thing ring R.
	v := vars ring phi;
	L := entries phi; -- list of lists of lin forms in Ops
        matrix apply(#L, i -> 
	    apply(#L_i, 
		j-> sub(diff(v, L_i_j),R)*Tmat))
	);
    dsum := (p,F)-> directSum apply(p, i->F);


    --End subroutines
    phi := F.dd_1;



restart
--load "bug-extend.m2"
load "~/src/M2-git/bugs/mike/00-eisenbud-bug-extend.m2"
  kk = ZZ/101;
  S = kk[a,b,c];
  ff = matrix{{a^2, b^2}};
  R = S/ideal ff;
  Ops = kk[x_1,x_2]
  MM = Ops^1/ideal(x_1^2*x_2)  
--  moduleAsExt(MM,R)


    Ops = ring MM;
    reg = regularity MM;
    MMr = truncate(reg, MM);
    F = res MMr;
    K = res(coker vars R, LengthLimit => reg+numgens R);
    K2 = res(coker K.dd_3, LengthLimit=>5);
    T = makeT(presentation R, K, 2);
    Tmat := T_0;
    scan(drop(T,1), t->Tmat = Tmat||t);
    --Two subroutines
    insertT := phi -> (
	--replace each entry of phi by the 
	--appropriate linear combination of the rows of Tmat.
	--Note that the entries of phi must be linear forms of Ops
	--and the output is a matrix of scalars over thing ring R.
	v := vars ring phi;
	L := entries phi; -- list of lists of lin forms in Ops
        matrix apply(#L, i -> 
	    apply(#L_i, 
		j-> sub(diff(v, L_i_j),R)*Tmat))
	);
    dsum := (p,F)-> directSum apply(p, i->F);


    --End subroutines
    phi := F.dd_1;

C1 = dsum(rank source phi, K)
gb C1.dd_2

    print betti (Ks := dsum(rank source phi, K));
    print betti (Kt := dsum(rank target phi,R^{2}**K2));
    phiT := map(Ks_0,Kt_0,insertT phi);
    print betti phiT;
    print "next command causes the crash";
--    error "debug me";
--    error();

C1 = dsum(rank source phi, K)
C2 = dsum(rank target phi,R^{2}**K2)
--extend(C1,C2,phiT) -- crash


--     extend(ChainComplex,ChainComplex,Matrix) := ChainComplexMap => opts -> (D,C,fi)-> (
          
D = C1
C = C2
fi = phiT          
gb(D.dd_2, ChangeMatrix=>true)

          i := 0;
          j := 0;
          f := new ChainComplexMap;
          f.cache = new CacheTable;
          f.source = C;
          f.target = D;
          complete C;
          s := f.degree = j-i;
          f#i = fi;
          n := i+1;

C#?n
          while C#?n do (
               p := f_(n-1) * C.dd_n;
               q := D.dd_(n+s);
               if opts.Verify then (
                    (quot,rem) := quotientRemainder(p,q);
                    if rem != 0 then error "map cannot be extended";
                    f#n = quot;
                    )
               else (
                    f#n = p // q;
                    );
               n = n+1;
               );
          f)


    extend(dsum(rank source phi, K), 
	dsum(rank target phi,R^{2}**K2), 
	phiT)    


n=2
               p = f_(n-1) * C.dd_n;
               q = D.dd_(n+s);

f = p
g = q
gb(g, ChangeMatrix => true) -- this crashes

     quotientRemainder(Matrix,Matrix) := Matrix => (f,g) -> (
          if ring g =!= ring f then error "expected maps over the same ring";
          M := target f;
          if M != target g then error "expected maps with the same target";
          L := source f;
          N := source g;
          f = matrix f;
          g = matrix g;
          G := (
               if M.?relations 
               then gb(g | presentation M, ChangeMatrix => true, SyzygyRows => rank source g)
               else gb(g,                  ChangeMatrix => true)
               );
          (rem,quo,cplt) := rawGBMatrixLift(raw G, raw f);
          (
               map(N, L, quo, Degree => degree f - degree g),
               map(M, L, rem)
          ))

f = map(R^{{-1},{-1},{-1},{-1},{-1},{-1},{-1},{-1},{-1}},R^{{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2}},{{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -c, 0, 0, 0, -b, -a, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, c, 0, 0, 0, b, a, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -b, a, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, c, 0, 0, 0, b, a, 0, c, 0, 0, 0, b, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, -c, 0, 0, 0, -b, -a, 0, -c, 0, 0, 0, -b, -a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, b, -a, 0, 0, 0, 0, 0, b, -a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, c, 0, 0, 0, b, a, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -c, 0, 0, 0, -b, -a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, b, -a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}})
g = map(R^{{-1},{-1},{-1},{-1},{-1},{-1},{-1},{-1},{-1}},R^{{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2},{-2}},{{0, -c, 0, -b, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {-c, 0, b, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {b, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, -c, 0, -b, a, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, -c, 0, b, a, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, b, a, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -c, 0, -b, a}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -c, 0, b, a, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, b, a, 0, 0, 0}})          
f == p
g == q
gb(g, ChangeMatrix => true) -- this crashes
