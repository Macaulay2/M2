--- status: DRAFT
--- author(s): MES, taken from Dan's
--- notes: 

doc ///
  Key
    comodule
    (comodule, Ideal)
    (comodule, Module)
    (quotient, Ideal)
    (quotient, Module)
  Headline
    submodule to quotient module
  Usage
    comodule M
    quotient M
  Inputs
    M:Module
      or an @TO2(Ideal, "ideal")@
  Outputs
    :Module
      the quotient module @TT "N/M"@, if @TT "M"@ is given as a submodule $M
      \subset N$, where @TT "N"@ is either a free module or a quotient of a
      free module.
  Description
    Example
      R = QQ[a..d];
      I = ideal(a,b,c,d^3);
      comodule I
  SeeAlso
    image
    cokernel
    kernel
///
