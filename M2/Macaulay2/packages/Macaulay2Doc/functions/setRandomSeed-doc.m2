
document {
     Key => setRandomSeed,
     Headline => "set starting point for random number generator"
     }

document {
     Key => 1 : setRandomSeed,
     Usage => "setRandomSeed()",
     Consequences => {
	  {"Initializes the random number generator to a fixed state, identical to the
	       initial state (upon program start) in version 1.2 and earlier of Macaulay2.  (After version 1.2,
	       the random number seed is initially set (when Macaulay2 starts) to a number that depends on the current date, 
	       the time (in seconds), and the process id, except for when running examples and tests
	       in packages (as signalled by use of the command line option ", TT "--no-randomize", "), where it is always initialized to 0.)"}
	  },
     EXAMPLE lines ///
     setRandomSeed()
     random 2^100
     setRandomSeed()
     random 2^100
     ///,
     SeeAlso => { (setRandomSeed,ZZ), (setRandomSeed,String) }
     }

document {
     Key => (setRandomSeed, ZZ),
     Usage => "setRandomSeed i",
     Inputs => {"i"},
     Consequences => {
     	  {"Sets the random number seed to the low-order 32 bits of the integer ", TT "i", ".
     	  The sequence of future pseudo-random results is determined by the seed."}
	  },
     EXAMPLE {
	  "setRandomSeed 123456",
	  "for i to 10 list random 100",
	  "setRandomSeed 123456",
	  "for i to 10 list random 100"
	  },
     SeeAlso => { 1:setRandomSeed, (setRandomSeed,String) }
     }

document {
     Key => (setRandomSeed, String),
     Usage => ///setRandomSeed s///,
     Inputs => {"s"},
     Consequences => {
	  {"Sets the random number seed to an integer computed from ", TT "s", ".  Every character 
	  of the string contributes to the seed, but only 32 bits of data are used.
	  The sequence of future pseudo-random results is determined by the seed."}
	  },
     EXAMPLE {
	  ///setRandomSeed "thrkwjsxz"///,
	  ///for i to 10 list random 100///,
	  ///setRandomSeed "thrkwjsxz"///,
	  ///for i to 10 list random 100///
	  },
     SeeAlso => { 1:setRandomSeed, (setRandomSeed,ZZ) }
     }
