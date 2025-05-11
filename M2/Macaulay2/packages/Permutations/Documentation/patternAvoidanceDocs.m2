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
  SeeAlso
    avoidsPatterns
    isCartwrightSturmfels
    isCDG
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
  SeeAlso
    avoidsPattern
    isCartwrightSturmfels
    isCDG
    isVexillary
///

-- isCartwrightSturmfels
-- TODO: add list of patterns to description
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
      A permutation $p$ is {\em Cartwright-Sturmfels} if it avoids.
    Example
      p = permutation {3,1,2,5,4}
      isCartwrightSturmfels p
  SeeAlso
    avoidsPattern
    avoidsPatterns
    isCDG
    isVexillary
///

-- isCDG
-- TODO: add CDG spelled out to headline and description
-- TODO add list of patterns to description
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
      A permutation $p$ is {\em CDG} if it avoids.
    Example
      p = permutation {3,1,2,5,4}
      isCDG p
  SeeAlso
    avoidsPattern
    avoidsPatterns
    isCartwrightSturmfels
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
  SeeAlso
    avoidsPattern
    avoidsPatterns
    isCartwrightSturmfels
    isCDG
///