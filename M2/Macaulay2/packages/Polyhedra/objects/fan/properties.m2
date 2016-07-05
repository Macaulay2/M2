compute#Fan#smooth = method()
compute#Fan#smooth Fan := F -> (
   R := rays F;
   L := transpose linealitySpace F;
   MC := maxCones F;
   MC = apply(MC,
      m -> R_m
   );
   all(MC, r -> spanSmoothCone(transpose r, L))
)

