-- in this file we write stub definitions (no content) for use by packages, so multiple packages can use the same thing

needs "methods.m2"

-- methods

chi = method()

decompose = method(Options => true)

intersect = method(Dispatch => Thing, Options => true)
intersection = method(Options => true)

truncate = method()

isEmpty = method(TypicalValue => Boolean)

-- symbols

protect Jacobian

protect Iterate
