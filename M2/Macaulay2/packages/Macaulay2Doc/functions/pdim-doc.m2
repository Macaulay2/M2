--- status: DRAFT
--- author(s): L.Gold, Josephine, Jonah
--- notes: 

doc ///
  Key
    pdim
  Headline
    compute the projective dimension
  Usage
    pdim M
  Caveat
    For now, the method is to measure the length of a projective resolution.
///

doc ///
  Key
    (pdim, Module)
  Headline
    compute the projective dimension of a module
  Usage
    pdim M
  Inputs
    M:Module
  Outputs
    :ZZ
      the projective dimension
  Description
    Example
      R = QQ[x,y,z];
      I = ideal(x^2, x*y, y*z);
      M = R^1/I
      res M
      pdim M
    Text
      Notice this is one more than the projective dimension of I as an
      R-module.
    Example
      res(module I)
      pdim(module I)
///
