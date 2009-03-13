newPackage "Python"

export \\ (s -> currentPackage#"private dictionary"#s = Core#"private dictionary"#s) \ {
     "runSimpleString", "PythonObject", "runString"
     }

export { "pythonHelp" }
pythonHelp = Command (() -> runString ///help()///)

end

needsPackage "Python"
runString "eval(compile( 'd = {}','','single' ),__builtins__) " -- this one includes all the builtins in d
runString "eval(compile( 'd = {}','','single' ),{}) "		-- this one starts over somehow
r = s -> runString concatenate("eval(compile(",format s,",'','single' ),d)")
g = s -> runString concatenate("d[", format s, "]")
r "from math import *"
runString "d.keys()"

runSimpleString "x=2"
runSimpleString "print(x)"
runString "dir()"
runString "dict"
-- runString "__builtins__"
runString "__builtins__.keys()"
runString "help()"
quit
runString "d"
runString "__builtins__['d']"
r "x=2"
g "x"
r "x"
r "tmp = sin(4.5)"
g "tmp"
runString "range(2,100)"
