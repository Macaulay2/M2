newPackage "Python"

export \\ (s -> currentPackage#"private dictionary"#s = Core#"private dictionary"#s) \ {
     "runSimpleString", "PythonObject", "runString", "sysGetObject", "objectType"
     }

export { "pythonHelp" }
pythonHelp = Command (() -> runString ///help()///)

PythonObject#{Standard,AfterPrint} = x -> (
     << endl;
     << concatenate(interpreterDepth:"o") << lineNumber << " : PythonObject of type " << replace("<type '(.*)'>","\\1",toString objectType x) << endl;
     )

end

needsPackage "Python"
rs = s -> (
     if debugLevel > 0 then stderr << "python command: " << s << endl;
     runString s)     
rs "eval(compile( 'd = {}','','single' ),__builtins__) " -- this one includes all the builtins in d
-- rs "eval(compile( 'd = {}','','single' ),{}) "		-- this one starts over somehow
access = (d,s) -> concatenate(d,"[", format s, "]")
val = s -> rs access("d",s)
e = s -> rs concatenate("eval(compile(",s,",'','single' ),d)")
r = s -> e format s
v = s -> e access("d",s)
rg = s -> ( r("tmp = "|s); val "tmp")
sage = s -> ( r("tmp = preparse("|format s|")"); v "tmp")
dir = s -> rg concatenate("dir(", s, ")")

runSimpleString "x=2"
runSimpleString "print(x)"
rs "dir()"
rs "dict"
rs "__builtins__.keys()"
rs "help()"
quit
rs "d"
rs "__builtins__['d']"
r "x=2"
val "x"
rg "range(2,100)"
rs "range(2,100)"
-- math
r "from math import *"
rg "sin(4.5)"
rs "d.keys()"
-- module sys
-- http://docs.python.org/library/sys.html#module-sys
sysGetObject "subversion"
sysGetObject "builtin_module_names"
sysGetObject "copyright"
r "import sys"
rg "sys.version"
r "from sys import *"
rg "version"
rg "modules.keys()"
rg "copyright"
rg "prefix"
rg "executable"
-- sage
r "from sage.all import *"
rg "sage"
rg "dir(sage)"
rg "sage.version"
rg "version()"
rg "dir(sage.version)"
rg "sage.version.version"
rg "plot"
rg "preparse"
rg "preparse('x=1')"
sage "x=2^100"
val "x"
objectType oo
sage "R.<x,y,z> = QQ[]"
rg "R"
rg "var('x')"
rg "plot(sin(x))"
sage "plot(sin(x))"
rg "show(plot(sin(x)))"
sage "I = ideal(x^2,y*z)"
rg "I"
val "I"
sage "G = I.groebner_basis()"
rg "G"
dir "I"
