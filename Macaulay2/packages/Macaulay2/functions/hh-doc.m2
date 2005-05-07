--- status: draft
--- author(s): Sorin Popescu
--- notes: 
document {
     Key => (hh,Sequence,ProjectiveVariety),
     Headline => "Hodge numbers of a smooth projective variety",
     Usage =>"hh^(p,q)(X)",
     Inputs => {"(p,q)" => Sequence,"X" => ProjectiveVariety},
     Outputs => {ZZ=>""},     
     "The command computes the Hodge numbers h^{p,q} of the smooth
     projective variety X. They are calculated as ", 
     TT "HH^q(cotangentSheaf(p,X))",
     PARA,
     "As an example we compute h^11 of a K3 surface (Fermat quartic
     in projective threespace:",
     EXAMPLE {
          "X = Proj(QQ[x_0..x_3]/ideal(x_0^4+x_1^4+x_2^4+x_3^4))",
          "hh^(1,1)(X)"
          },
     Caveat => {"There is no check made if the variety X is smooth or no."}
     SeeAlso => {(cohomology,ZZ,SumOfTwists),(cohomology,ZZ,SheafOfRings),(euler,ProjectiveVariety)}
     }




