-- Copyright 1997 by Michael Stillman and David Eisenbud
--
-- The code here should stay the SAME as the code
-- in the tutorial 'Fano.m2', if that is possible.
--

needs "matrix1.m2"
needs "genmat.m2"

Fano = method()
-- Grassmannian = method()

Fano(ZZ,Ideal,Ring) := Ideal => (k,X,GR) -> (
  -- Get info about the base ring of X:
  -- The coefficient ring (to make new rings of
  -- the same characteristic, for example)
  -- and the number of variables
  KK:=coefficientRing ring X;
  r := (numgens ring X) - 1;
  -- Next make private variables for our 
  -- intermediate rings, to avoid interfering
  -- with something outside:
  t:=symbol t;
  p:=symbol p;
  -- And rings
  S1 := KK[t_0..t_k];
  S2 := KK[p_0..p_(k*r+k+r)];
  S := tensor(S1,S2);
  -- Over S we have a generic point of a generic
  -- line, represented by a row vector, which
  -- we use to define a map from the base ring
  -- of X
  F := map(S,ring X,
          genericMatrix(S,S_0,1,k+1)*
          genericMatrix(S,S_(k+1),k+1,r+1)
          );
  -- We now apply F to the ideal of X
  FX := F X;
  -- and the condition we want becomes the condition
  -- that FX vanishes identically in the t_i.
  -- The following line produces the matrix of
  -- coefficients of the monomials in the 
  -- variables labelled 0..k:
  cFX := last coefficients (generators FX, Variables => 0..k);
  -- We can get rid of the variables t_i
  -- to ease the computation:
  cFX = substitute(cFX, S2);
  -- The ring we want is the quotient
  S2bar := S2/ideal cFX;
  -- Now we want to move to the Grassmannian,
  -- represented by the ring GR
  -- We define a map sending the variables of GR
  -- to the minors of the generic matrix in the
  -- p_i regarded as elements of S1bar
  gr := map(S2bar,GR,
            exteriorPower(k+1, 
            genericMatrix(S2bar,S2bar_0,k+1,r+1)
            )
           );
  -- and the defining ideal of the Fano variety is
  kernel gr
)

Fano(ZZ, Ideal) := Ideal => (k,X) -> (
  KK:=coefficientRing ring X;
  r := (numgens ring X) - 1;
  -- We can specify a private ring with binomial(r+1,k+1)
  -- variables as follows
  M := monoid [Variables => binomial(r+1,k+1)];
  GR := KK M;
  -- the work is done by
  Fano(k,X,GR)
)


-- Grassmannian(ZZ,ZZ,Ring) := Ideal => (k,r,R) ->( 
--         KK := coefficientRing R;
--         RPr := KK[Variables => r+1];
--         Pr := ideal(0_RPr);
--         substitute( Fano(k,Pr) , vars R )
--      )
-- 
-- Grassmannian(ZZ,ZZ) := Ideal => (k,r) -> (
--         R := ZZ/31991[
--                vars(0..(binomial(r+1,k+1)-1))
--                     ];
--         Grassmannian(k,r,R)
--                      )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
