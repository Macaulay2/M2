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
