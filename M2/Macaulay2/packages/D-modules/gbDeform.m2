-- a local function used by gbW and inW  
homGB := (I, w) -> (
     if ((ring I).?HomWeylAlgebra == false) then
     createHomWeylAlgebra (ring I);

     W := ring I;
     HW := W.HomWeylAlgebra;
     dpairs := W.monoid.Options.WeylAlgebra;
     -- Do some sanity checking
     if dpairs === {} 
     then error "expected a Weyl algebra";
     if any(dpairs, v -> class v =!= Option)
     then error "expected non-homogenized Weyl algebra";
     if #w =!= numgens W
     then error ("expected weight vector of length " | numgens W);
     -- Make the new weight vector
     wts = prepend(-1,w);

     -- Homogenize I
     I1 := W.WAtoHWA I;
     homogenize(gens I1, HW_0, wts)
     );

-- computes the GB of ideal "I" with respect to weight vector "w"  
gbW = method()
gbW(Ideal, List) := (I, w) -> (
     I2 := homGB(I, w);
     -- Do the computation
     ideal compress (ring I).HWAtoWA gens gb I2
     );-- end gbW

-- computes in_w(I). The result is a WA ideal (as opposed to 
-- an ideal in the associated commutative ring) 
inW = method()
inW(Ideal,List) := (I, w) -> (
     I2 := homGB(I, w);
     I3 := leadTerm(1, gens gb I2);
     ideal compress (ring I).HWAtoWA I3
     );

