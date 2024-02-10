doc ///
Node
  Key
    preimage
   (preimage, RingMap, Ideal)
   (preimage, Matrix, Module)
  Headline
    preimage of a map
  Synopsis
    Heading
      preimage of an ideal under a ring map
    Usage
      preimage(f, I)
    Inputs
      f:RingMap
      M:Ideal -- in the target of $f$
    Outputs
      :Ideal -- the preimage of $I$ under the map $f$
    Description
      Example
        R = QQ[x,y,z]
        S = QQ[t,u]
        f = map(R, S, {x*y, y*z})
        preimage_f ideal(x^2,y^2)
  Synopsis
    Heading
      preimage of a submodule under a module map
    Usage
      preimage(f, M)
    Inputs
      f:Matrix
      M:Module -- a submodule in the target of $f$
    Outputs
      :Module -- the preimage of $M$ under the map $f$
    Description
      Text
        $M$ and @TO target@ of $f$ should have the same @TO ambient@ module.
      -- TODO: Example
  SeeAlso
    coimage
    image
    kernel
    source
    target
///
