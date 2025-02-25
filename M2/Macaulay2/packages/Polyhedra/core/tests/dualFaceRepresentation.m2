TEST ///
P = hypercube 2
M = dualFaceRepresentationMap P
C = cone P
R = rays C
F = facets C
FP = faces C
for k in keys FP do (
   for face in FP#k do (
      for i in face do (
         for j in M#face do (
            ray := R_{i};
            facet := F^{j};
            eval := facet*ray;
            eval = flatten entries eval;
            eval = eval#0;
            assert(eval == 0)
         )
      )
   )
)
///

TEST ///
P = convexHull matrix {{-3, 2, -3, 1, 1, -3, -3, -1, -2, 3, 3, 1, 1, 2, -1}, {0, -3, 3, -1, -1, 3, 2, 3, 2, 2, 2, 0, -3, 2, -2}, {-3, 2, 0, -3, 2, 3, -1, -2, -3, 3, -2, -1, 0, -3, 1}, {-2, 3, -2, -1, 2, 3, 2, -3, 2, -2, -1, -2, 2, 0, 0}}
M = dualFaceRepresentationMap P
C = cone P
R = rays C
F = facets C
FP = faces C
for k in keys FP do (
   for face in FP#k do (
      for i in face do (
         for j in M#face do (
            ray := R_{i};
            facet := F^{j};
            eval := facet*ray;
            eval = flatten entries eval;
            eval = eval#0;
            assert(eval == 0)
         )
      )
   )
)
///
