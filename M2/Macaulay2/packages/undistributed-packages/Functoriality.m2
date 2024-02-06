-- Functoriality Macaulay 2 package
-- Copyright (C) 2009 Jason G McCullough 

--This program is free software; you can redistribute it and/or
--modify it under the terms of the GNU General Public License version 2
--as published by the Free Software Foundation.

--This program is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--GNU General Public License for more details.

newPackage(
     "Functoriality",
     Version => "0.2", 
     Date => "August 20, 2009",
     Authors => {
	  {Name => "Jason McCullough", Email => "jmccullo@math.ucr.edu", HomePage => "http://www.math.ucr.edu/~jmccullo"}
	  },
     Headline => "Adds functoriality to the Tor and Ext functions within M2 as well as functionality for the connecting homomorphisms.
                  Some functions to access the canonical maps associated with the mapping cone of a chain complex map are also included.",
     DebuggingMode => false
     )

--=========================================================================--
     
export{"coneProjectTarget","coneProjectSource","coneInjectTarget","coneInjectSource","TorConnectingMap"}

--=========================================================================--

--needsPackage "ChainComplexExtras"


-- Computes the map of Tor modules Tor_i(f,N)
-- Returns the map (Matrix) Tor_i(source f,N) --> Tor_i(target f,N)

Tor(ZZ, Matrix, Module) := Matrix => opts -> (i,f,N) -> (
	if ring source f != ring N then error "expected the same ring";
	R := ring N;
	if not isCommutative R then error "'Tor' not implemented yet for noncommutative rings";
	if i < 0 then map(R^0,R^0,0)
	else if i === 0 then  f ** N
	else (
	     F := resolution(f,LengthLimit=>i+1);
	     C := source F;
	     D := target F;
	     tC := C ** N;
	     tD := D ** N;	     
	     tf := F_i ** N;
	     inducedMap(HH_i(tD), HH_i(tC), tf)
	     )
	)
   
-- Computes the map of Tor modules Tor_i(N,f)
-- Returns the map (Matrix) Tor_i(N, source f) --> Tor_i(N, target f)

Tor(ZZ, Module, Matrix) := Matrix => opts -> (i,N,f) -> (
	if ring source f != ring N then error "expected the same ring";
	R := ring N;
	if not isCommutative R then error "'Tor' not implemented yet for noncommutative rings";
	if i < 0 then map(R^0,R^0,0)
	else if i === 0 then  N ** f
	else (
	     F := resolution(f,LengthLimit=>i+1);
	     C := source F;
	     D := target F;
	     tC := N ** C;
	     tD := N ** D;	     
	     tf := N ** F_i;
	     inducedMap(HH_i(tD), HH_i(tC), tf)
	     )
	)

-- Computes the map of Hom modules Hom_i(f,N)
-- Returns the map (Matrix) Hom_i(source f,N) --> Hom_i(target f,N)

Ext(ZZ, Matrix, Module) := Matrix => opts -> (i,f,N) -> (
	if ring source f != ring N then error "expected the same ring";
	R := ring N;
	if not isCommutative R then error "'Hom' not implemented yet for noncommutative rings";
	if i < 0 then map(R^0,R^0,0)
	else if i === 0 then Hom(f,N)
	else (
	     F := resolution(f,LengthLimit=>i+1);
	     C := source F;
	     D := target F;
	     tC := Hom(C,N);
	     tD := Hom(D,N);
	     tf := Hom(F_i,N);
	     inducedMap(HH_i(tD), HH_i(tC), tf)
	     )
	)
   
-- Computes the map of Hom modules Hom_i(N,f)
-- Returns the map (Matrix) Hom_i(N, source f) --> Hom_i(N, target f)

Ext(ZZ, Module, Matrix) := Matrix => opts -> (i,N,f) -> (
	if ring source f != ring N then error "expected the same ring";
	R := ring N;
	if not isCommutative R then error "'Hom' not implemented yet for noncommutative rings";
	if i < 0 then map(R^0,R^0,0)
	else if i === 0 then  N ** f
	else (
	     F := resolution(f,LengthLimit=>i+1);
	     C := source F;
	     D := target F;
	     tC := Hom(N,C);
	     tD := Hom(N,D);	     
	     tf := Hom(N,F_i);
	     inducedMap(HH_i(tD), HH_i(tC), tf)
	     )
	)


-- TorConnectingMap(ZZ, Matrix, Module) := Matrix => (i,f,P)
-- Returns the map (Matrix) Tor_i(target f, P) --> Tor_(i-1)(ker f, P)
-- Assumes f is surjective

TorConnectingMap = method()
TorConnectingMap(ZZ, Module, Matrix) := Matrix => (i,P,f) -> (
     if ring source f != ring P then error "expected the same ring";
     R := ring P;
     if not isCommutative R then error "'Tor' not implemented yet for noncommutative rings";
     if isInjective f then (
	  if isSurjective f then error "Map is an isomorphism.  Expected a surjective or injective map"
	  else (
	       g := inducedMap(coker f,source f);
	       f = g;
	       );
	  );
     if i < 0 then map(R^0,R^0,0)
     else if i === 0 then map(P ** (target f), R^0, 0)
     else (
	  M := source f;
	  N := target f;
	  --F := resolution(f,LengthLimit=>i+1);
	  B := resolution(M,LengthLimit=>i+1);
	  C := resolution(N,LengthLimit=>i+1);
	  F := extend(C,B,matrix f);
	  D := cone(F);
	  G := coneInjectTarget F;
	  GP := P ** G;
	  inducedMap(HH_i (P ** D), HH_i (P ** C), GP_i)
	  )
     	  
     )



coneInjectTarget = method()
coneInjectTarget(ChainComplexMap) := ChainComplexMap => (f) -> (
      C := source f;
      D := target f;
      E := cone f;
      map(E,D,k->E_k_[0])
      )

coneInjectSource = method()
coneInjectSource(ChainComplexMap) := ChainComplexMap => (f) -> (
      C := source f;
      D := target f;
      E := cone f;
      map(E,C[-1],k->E_k_[1])
      )
 
coneProjectTarget = method()
coneProjectTarget(ChainComplexMap) := ChainComplexMap => (f) -> (
      C := source f;
      D := target f;
      E := cone f;
      map(D,E,k->E_k^[0])
      )
 
 
coneProjectSource = method()
coneProjectSource(ChainComplexMap) := ChainComplexMap => (f) -> (
      C := source f;
      D := target f;
      E := cone f;
      map(C[-1],E,k->E_k^[1])
      )


ChainComplexMap Array := (F,A) -> (
     if length A != 1 then error "Expect array of length 1 containing an integer.";
     C := source F;
     D := target F;
     shift := A_0;
     map(D[shift],C[shift],k->F_(k+shift))
     )
	  

beginDocumentation() -- the start of the documentation

-----------------------------------------------------------------------------

document { 
     Key => Functoriality, 
     Headline => "Adds functoriality to Tor and Ext functions as well as functions
                  to compute the connecting homomorphisms",      
     EM "Functoriality", " is a package which adds functorial functionality to the
         Tor and Ext methods.  In particular we add functions", TO (Tor, ZZ, Matrix, Module), ", ", TO (Tor, ZZ, Module, Matrix), ", ", TO (Ext, ZZ, Matrix, Module), ", ", TO (Ext, ZZ, Module, Matrix), ", ", TO (TorConnectingMap, ZZ, Module, Matrix), ", ", TO (coneInjectTarget, ChainComplexMap), ", and ", TO (coneInjectSource, ChainComplexMap), "."
      }
  
-----------------------------------------------------------------------------
