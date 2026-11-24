--- status: DRAFT
--- author(s): 
--- notes: 

doc ///
  Key
    entries
    (entries,Matrix)
    (entries, MutableMatrix)
  Headline
    get the entries of a matrix
  Usage
    entries M
  Inputs
    M:
      a @TO Matrix@ or a @TO MutableMatrix@
  Outputs
    :
      a doubly nested @TO List@ of the entries of M
  Description
      --     Consequences => {
      --	  },
      --     "description",
    Example
      R = ZZ[x,y,z];
      M = matrix{{x,z,x*y},{x^2+z,y*z,1}}
      entries M
      N = mutableMatrix{{x,z,x*y},{x^2+z,y*z,1}}
      entries N
///

doc ///
  Key
    (entries,Vector)
  Headline
    get the entries of a vector
  Usage
    entries v
  Inputs
    v:
  Outputs
    :
      a @TO List@ of the coordinates of v
  Description
      --     Consequences => {
      --	  },
      --     "description",
    Example
      R = (ZZ[x,y])^3;
      v = vector {1,x,x*y};
      entries v
///

