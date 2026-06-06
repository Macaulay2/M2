--- status: Draft
--- author(s): Giulio 
--- notes: updated November 2021

-- TODO: this is currently a duplicate one, remove one of them
doc ///
Node
  Key
    (regularity, ChainComplex)
  Headline
    compute the Castelnuovo-Mumford regularity
  Usage
    r = regularity C
    r = regularity(C, Weights => w)
  Inputs
    C:ChainComplex
    Weights=>List -- a weight vector @TT "w"@, see @TO [betti, Weights]@
  Outputs
    r:ZZ
  Description
    Text
      For a free chain complex @TT "C"@, the regularity @TT "r"@ is the smallest number so that each
      basis element of @TT "C_i"@ has degree at most @TT "i+r"@. For an ideal @TT "I"@, regularity is
      one plus the regularity of the minimal free resolution of the quotient of the ambient ring by @TT "I"@.
      For a module @TT "M"@, regularity is the regularity of a minimal free resolution of @TT "M"@.
    Example
      R = ZZ/32003[a..d];
      I = ideal(a^20, b^20, a*c^19-b*d^19);
      C = resolution I
      regularity C
      regularity comodule I
      regularity I
      regularity module I
    Text
      The regularity is the label of the last row in the @TO2 {betti, "Betti diagram"}@ of a chain complex.
      However, this depends on the total degree weights in the Betti tally, which are computed based on the
      @TO2 {"heft vectors", "heft vector"}@ of the underlying ring. To adjust this vector, a vector @TT "w"@
      whose length is the same as the @TO2 {degreeLength, "degree length"}@ of the ring can be provided using
      the option @TT "Weights"@. The dot products of @TT "w"@ with the multidegrees in the tally will be used
      in the resulting computation.
    Example
      C = resolution ideal(a^3, a^2*b, a*b^6, a^2*c);
      betti C
      regularity C
      betti(C, Weights => {2})
      regularity(C, Weights => {2})
  SeeAlso
    resolution
    betti
    comodule
    "VirtualResolutions :: multigradedRegularity"
///
