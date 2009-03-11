newPackage "Python"

export \\ (s -> currentPackage#"private dictionary"#s = Core#"private dictionary"#s) \ {
     "runSimpleString"
     }

export { "loadSage" }

loadSage = () -> runSimpleString "from sage.all import *"
