--- status: DRAFT
--- author(s): M. Stillman
--- notes: only added example

document {
     Key => {ceiling},
     Headline => "ceiling function",
     SeeAlso => {floor}
     }
     
document { 
     Key => {(ceiling,Number)},
     Headline => "ceiling function",
     Usage => "ceiling x",
     Inputs => { "x" },
     Outputs => { { "the least integer greater than or equal to the number ", TT "x",
          ". The imaginary part of a complex number is ignored." } },
     EXAMPLE {
     "ceiling(-3.4)"
     },
     SeeAlso => {(floor, Number), (truncate, Number)}
     }
