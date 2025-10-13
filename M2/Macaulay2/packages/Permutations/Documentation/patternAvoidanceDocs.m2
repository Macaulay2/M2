-- avoidsPattern
doc ///
  Key
    avoidsPattern
    (avoidsPattern, Permutation, List)
  Headline
    whether a permutation avoids a pattern
  Usage
    avoidsPattern(w, pattern)
  Inputs
    w:Permutation
    pattern:List
  Outputs
    isAvoiding:Boolean
  Description
    Text
      Pattern avoidance is more easily understood through an example.
      A permutation $p$ is $2143$-avoiding if there do not exist indices 
      $i < j < k < l$ such that $w_j < w_i < w_l < w_k$.
    Example
      p = permutation {3,1,2,5,4}
      avoidsPattern(p, {2,1,4,3})
  Acknowledgement
    This method was ported over from the @TO "MatrixSchubert"@ package.
  SeeAlso
    avoidsPatterns
    isCartwrightSturmfels
    isCDG
    isSeparable
    isVexillary
///

-- avoidsPatterns
doc ///
  Key
    avoidsPatterns
    (avoidsPatterns, Permutation, List)
  Headline
    whether a permutation simultaneously avoids a list of patterns
  Usage
    avoidsPatterns(w, patterns)
  Inputs
    w:Permutation
    patterns:List
  Outputs
    :Boolean
  Description
    Text
      See @TO avoidsPattern@ for more information on pattern avoidance.
    Example
      p = permutation {3,1,2,5,4}
      avoidsPatterns(p, {{2,1,4,3}, {1,4,3,2}})
  Acknowledgement
    This method was ported over from the @TO "MatrixSchubert"@ package.
  SeeAlso
    avoidsPattern
    isCartwrightSturmfels
    isCDG
    isSeparable
    isVexillary
///

-- isCartwrightSturmfels
doc ///
  Key
    isCartwrightSturmfels
    (isCartwrightSturmfels, Permutation)
  Headline
    whether a permutation is Cartwright-Sturmfels
  Usage
    isVexillary w
  Inputs
    w:Permutation
  Outputs
    :Boolean
  Description
    Text
      A permutation $p$ is {\em Cartwright-Sturmfels} if it avoids all of the 
      following patterns:
      $12543$, $13254$, $13524$, $13542$, $21543$, $125364$, $125634$, $215364$, $215634$, $315264$, $315624$, and $315642$.
    Example
      p = permutation {3,1,2,5,4}
      isCartwrightSturmfels p
  Acknowledgement
    This method was ported over from the @TO "MatrixSchubert"@ package.
  SeeAlso
    avoidsPattern
    avoidsPatterns
    isCDG
    isSeparable
    isVexillary
///

-- isCDG
doc ///
  Key
    isCDG
    (isCDG, Permutation)
  Headline
    whether a permutation is CDG.
  Usage
    isCDG w
  Inputs
    w:Permutation
  Outputs
    :Boolean
  Description
    Text
      A permutation $p$ is {\em Conca-De Negri-Gorla} ({\em CDG}) if it avoids 
      all of the following patterns:
      $13254$, $21543$, $214635$, $215364$, $215634$, $241635$, $315264$, and $4261735$.
    Example
      p = permutation {3,1,2,5,4}
      isCDG p
  Acknowledgement
    This method was ported over from the @TO "MatrixSchubert"@ package.
  SeeAlso
    avoidsPattern
    avoidsPatterns
    isCartwrightSturmfels
    isSeparable
    isVexillary
///

-- isSeparable
doc ///
  Key
    isSeparable
    (isSeparable, Permutation)
  Headline
    whether a permutation is separable.
  Usage
    isSeparable w
  Inputs
    w:Permutation
  Outputs
    :Boolean
  Description
    Text
      A permutation $p$ is {\em separable} if it avoids $2413$ and $3142$.
    Example
      p = permutation {1,4,3,2,5,9,7,8,6}
      isSeparable p
  SeeAlso
    avoidsPattern
    avoidsPatterns
    isCartwrightSturmfels
    isCDG
    isVexillary
///

-- isVexillary
doc ///
  Key
    isVexillary
    (isVexillary, Permutation)
  Headline
    whether a permutation is vexillary
  Usage
    isVexillary w
  Inputs
    w:Permutation
  Outputs
    :Boolean
  Description
    Text
      A permutation $p$ is {\em vexillary} if it is $2143$-avoiding.
    Example
      p = permutation {3,1,2,5,4}
      isVexillary p
  Acknowledgement
    This method was ported over from the @TO "MatrixSchubert"@ package.
  SeeAlso
    avoidsPattern
    avoidsPatterns
    isCartwrightSturmfels
    isCDG
    isSeparable
///