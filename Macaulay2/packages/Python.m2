newPackage "Python"

export \\ (s -> currentPackage#"private dictionary"#s = Core#"private dictionary"#s) \ {
     "runSimpleString", "PythonObject", "runString", "sysGetObject", "objectType"
     }

export { "pythonHelp" }
pythonHelp = Command (() -> runString ///help()///)

end

needsPackage "Python"
runString "eval(compile( 'd = {}','','single' ),__builtins__) " -- this one includes all the builtins in d
-- runString "eval(compile( 'd = {}','','single' ),{}) "		-- this one starts over somehow
r = s -> runString concatenate("eval(compile(",format s,",'','single' ),d)")
g = s -> runString concatenate("d[", format s, "]")
rg = s -> ( r ("tmp = "|s); g "tmp" )




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

rg "range(2,100)"
runString "range(2,100)"

-- math
r "from math import *"
rg "sin(4.5)"
runString "d.keys()"

-- module sys
-- http://docs.python.org/library/sys.html#module-sys
sysGetObject "subversion"
sysGetObject "builtin_module_names"
sysGetObject "copyright"
r "import sys"
rg "sys.version"
rg "sys.modules"
rg "sys.copyright"
rg "sys.prefix"
rg "sys.executable"

