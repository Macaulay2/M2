doc ///
Node
  Key
    setRandomSeed
   (setRandomSeed, Sequence)
   (setRandomSeed, String)
   (setRandomSeed, ZZ)
  Headline
    set starting point for random number generator
  Usage
    setRandomSeed()
    setRandomSeed(seed)
  Consequences
    Item
      If {\tt seed} is {\tt ()}, initializes the random number generator to a fixed state,
      identical to the initial state (upon program start) in version 1.2 and earlier of Macaulay2.
      Otherwise sets {\tt randomSeed} to a value based on {\tt seed}.
  Description
    Text
      Since version 1.2, when Macaulay2 starts the random number seed is initially set to
      a number that depends on the current date, the time (in seconds), and the process id,
      except for when running examples and tests in packages, where it is always initialized
      to 0 by passing the command line option @TT "--no-randomize"@.

      The sequence of future pseudo-random results is determined by the random number seed.
    Example
      setRandomSeed()
      random 2^100
      random 2^100
      setRandomSeed()
      random 2^100
      random 2^100
    Text
      If an integer seed is given, the random number seed is set to the low-order 32 bits of the seed.
    Example
      setRandomSeed 123456
      for i to 10 list random 100
      setRandomSeed 123456
      for i to 10 list random 100
    Text
      If a string seed is given, sets the random number seed to an integer computed from it.
      Every character of the string contributes to the seed, but only 32 bits of data are used.
      The sequence of future pseudo-random results is determined by the seed.
    Example
      setRandomSeed "thrkwjsxz"
      for i to 10 list random 100
      setRandomSeed "thrkwjsxz"
      for i to 10 list random 100
  SourceCode
    (setRandomSeed, ZZ)
    (setRandomSeed, String)
    (setRandomSeed, Sequence)
///
