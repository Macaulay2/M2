graphIdealZZ = method()
   graphIdealZZ RingMap := f-> (
            S := source f;
            R := target f;
            m := numgens R;
            n := numgens S;
            k := coefficientRing R;
            if not isCommutative S then error "expected source of ring map to be a commutative ring";
            if S === k then return ideal map(R^1, R^0, 0);
            if not ( k === coefficientRing S ) then error "expected polynomial rings over the same ring";
            gensk := generators(k, CoefficientRing => ZZ);
            if not all(gensk, x -> promote(x, R) == f promote(x, S)) then error "expected ring map to be identity on coefficient ring";
            RS := tensor(R, S,
                 MonomialOrder => Eliminate m,
                 DegreeGroup => degreeGroup R,
                 Degrees => join(degrees R, apply(degrees S, f.cache.DegreeMap)),
                 Heft => heft R);
            v := vars RS;
            yv := v_{m .. m + n-1};
            xv := v_{0 .. m-1};
            h := f.matrix_{0 .. n - 1};
            I := ideal(yv - substitute(h, xv));
            assert(not isHomogeneous f or isHomogeneous I);
            I)



kernelZZ = method()
kernelZZ(RingMap) := f->(
              (F, R) := (target f, source f);
              if
              not instance(ambient R, PolynomialRing)
              or not instance(ambient F, PolynomialRing)
              or not coefficientRing R === coefficientRing F
              then return null;
                     graph := generators graphIdealZZ f;
                     assert( not isHomogeneous f or isHomogeneous graph );
                     SS := ring graph;
chh := false;
                     if chh then (
                         hf := poincare module target f;
                         T := degreesRing SS;
                         hf = hf * product(degrees source graph, d -> 1 - T_d);
                         poincare cokernel graph = hf;
                         );
              n1 := numgens F;
                     mapback := map(R, ring graph, map(R^1, R^n1, 0) | vars R);
                     G := gb(graph);
                     assert (not chh or G#?"rawGBSetHilbertFunction log"); -- ensure the Hilbert function hint was actually used in gb.m2
                     ideal mapback selectInSubring(1, generators G))

protect symbol kernelZZ;


    preimageZZ = method()
    preimageZZ(RingMap, Ideal) := Ideal => (f, J) -> (
            R := ring J;
            kernelZZ( map(R/J, R) * f ))



