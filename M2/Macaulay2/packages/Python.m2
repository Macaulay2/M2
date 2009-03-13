newPackage "Python"

export \\ (s -> currentPackage#"private dictionary"#s = Core#"private dictionary"#s) \ {
     "runSimpleString", "PythonObject", "runString"
     }

export { "pythonHelp" }

pythonHelp = Command (() -> runString ///help()///)

end

needsPackage "Python"

runSimpleString "x=2"
runSimpleString "print(x)"
runString "dir()"
runString "dict"
runString "__builtins__"
runString "help()"
runString "eval(compile( 'd = {}','','single' ),__builtins__) "
runString "d"
runString "__builtins__['d']"
r = s -> runString concatenate("eval(compile(",format s,",'','single' ),d)")
r "x=2"
g = s -> runString concatenate("d[", format s, "]")
runString "d"					    -- now has all the builtin symbols in it
g "x"
r "x"
r "from math import *"
r "tmp = sin(4.5)"
g "tmp"
runString "range(2,33)"
runString "srange(2,33)"
