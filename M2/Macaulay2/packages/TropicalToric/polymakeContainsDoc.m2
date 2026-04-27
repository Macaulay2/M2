doc ///
  Key
    polymakeConeContains
    (polymakeConeContains,List, List)
  Headline
    Check if a vector is contained in a cone using polymake
  Usage
    polymakeConeContains(v,C)
  Inputs
    v: List
    C: List
  Outputs
    b: Boolean
  Description
    Text
        This method checks if a vector, given as a list, is contained in a cone, where the cone is given
        as a list containing lists representing the vectors spanning the cone.
    Example
        v = {1,0,0};
        C = {{1,0,0},{0,1,0},{0,0,1}};
        polymakeConeContains(v,C)
    Example
        v = {1,0,0};
        C = {{2,1,0},{0,1,0},{0,0,1}};
        polymakeConeContains(v,C)
///
