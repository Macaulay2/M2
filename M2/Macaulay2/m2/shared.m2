-- in this file we write stub definitions (no content) for use by packages, so multiple packages can use the same thing

needs "methods.m2"

-- methods

chi = method()
euler  = method()
eulers = method()
genera = method()
genus  = method()

complete = method()
minimize = method()

cone = method()
rays = method(Options => true)

decompose = method(Options => true)

-- TODO: change options to true?
extend = method(Options => {Verify => true})

union = method(Binary => true)
intersect = method(Options => true, Binary => true) -- an associative binary method

tensor    = method(Options => true, Binary => true) -- tensor is left-associative

directProduct = method(Binary => true)

truncate = method(Options => true)

isEmpty = method(TypicalValue => Boolean)
isEmpty Thing := x -> #x == 0

isExact = method(TypicalValue => Boolean, Options => true)

isSmooth = method(TypicalValue => Boolean, Options => true)
isVeryAmple = method(TypicalValue => Boolean, Options => true)

isNormal = method()

normalCone = method(Options => true)

isMorphism = method(TypicalValue => Boolean)
isAbelianCategory = method(TypicalValue => Boolean)

-- symbols

protect Base

protect Jacobian

protect Iterate

protect Strict

protect Threads

protect Tries
