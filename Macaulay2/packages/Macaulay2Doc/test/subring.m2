-- Copyright 1995. Michael E. Stillman
-- local variables in the variables added October 27, 1995 (David Eisenbud)

sat1 = m -> (
    -- this method is used to find (m : lastvar^*), if the monomial
    -- order is the reverse lex order
    R := ring m;
    (msat,topv) = divideByVariable(generators gb m,R_(numgens R - 1));
    msat)

--document {
--     Key => symbol sat1,
--     TT "sat1(m)", " -- returns (m : lastvar^*), if the monomial
--     order is the reverse lex order."
--     }

     -- for I : f^*, when f isn't last variable (check first)
     -- iterate I = I:f until equality
     -- or, add a new variable z as last variable, append equation z-f, and
     -- apply the fi


subrg = (f, I, S) -> (
  -- find the image of the map m from V(I) to Proj S
  R := ring f;
  F := source f;
  n1 := #( generators R );
  n2 := numgens F;
  x := symbol x;
  y := symbol y;
  RS = (coefficientRing R)[y_0 .. y_(n2-1), x_0 .. x_(n1-1)];
  xvars := matrix table(1, n1, (j,i)->x_i);
  yvars := matrix table(1, n2, (j,i)->y_i);
  mapback := vars S | map(S^1, S^n1, 0);
  -- now compute the syzygies on f mod I
  syzM = modulo(f, I);  -- These should really be computed in R/I
  syzM = substitute(syzM,xvars);
  J = (substitute(I,xvars)) | (yvars*syzM);
  mingens image substitute((sat1 generators gb J),mapback)
  )

--document {
--     Key => symbol subrg,
--     TT "subrg(f,I,S)", " -- returns the image of V(I) in Proj S
--     under the rational map defined by f."
--     }
 
suber = (f, S) -> subrg(f, map(R^1, R^0, 0), S)

--document {
--     Key => symbol suber,
--     "suber(f,S)", " -- returns the image of the projective space
--     under the rational map defined by f."
--     }

eg1 = () -> (
  -- The first example: the rational quartic in P3
  R = ZZ/101[s,t];
  S = ZZ/101[a..d];
  phi = matrix {{s^4, s^3*t, s*t^3, t^4}};
  suber(phi, S)
  )

eg2 = () -> (
  -- The image of an elliptic curve in P2
  R = ZZ/101[s,t,u];
  S = ZZ/101[a..f];
  phi = symmetricPower(2, vars R);
  I = matrix{{s^3 - t^2* u}};
  subrg(phi, I, S)
  )

assert ( image eg1() == image matrix {{b*c-a*d, b^3-a^2*c, c^3-b*d^2, a*c^2-b^2*d}})
assert ( image eg2() == image matrix {{e^2-d*f, a*c-d*f, a*b-d*e, c*e-b*f, c*d-b*e, 
	       c^2-a*f, a^2-b*e, b*c-a*e, b^2-a*d}})
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test subring.out"
-- End:
