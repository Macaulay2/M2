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

compute#Fan#computedDimension = method()
compute#Fan#computedDimension Fan := F -> (
   R := rays F;
   MC := maxCones F;
   L := linealitySpace F;
   MC = apply(MC, m -> R_m);
   MC = apply(MC, r -> rank (r | L));
   max MC
)
