newPackage "Python"

export \\ (s -> currentPackage#"private dictionary"#s = Core#"private dictionary"#s) \ {
     "runSimpleString", "PythonObject", "runString", "sysGetObject", "objectType"
     }

export { "pythonHelp" }
pythonHelp = Command (() -> runString ///help()///)

end

needsPackage "Python"
rs = s -> (
     if debugLevel > 0 then stderr << "python command: " << s << endl;
     runString s)     
rs "eval(compile( 'd = {}','','single' ),__builtins__) " -- this one includes all the builtins in d
-- rs "eval(compile( 'd = {}','','single' ),{}) "		-- this one starts over somehow
r = s -> rs concatenate("eval(compile(",format s,",'','single' ),d)")
v = var -> rs concatenate("eval(compile(d[",format var,"],'','single' ),d)")
g = s -> rs concatenate("d[", format s, "]")
rg = s -> ( 
     r("tmp = "|s); 
     g "tmp")
rv = s -> (
     r("tmp = preparse("|format s|")"); 
     v "tmp")

runSimpleString "x=2"
runSimpleString "print(x)"
rs "dir()"
rs "dict"
-- rs "__builtins__"
rs "__builtins__.keys()"
rs "help()"
quit
rs "d"
rs "__builtins__['d']"
r "x=2"
g "x"
r "x"

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
rg "sys.modules"
rg "sys.copyright"
rg "sys.prefix"
rg "sys.executable"

-- sage
r "from sage.all import *"
rg "sage"
rg "dir(sage)"
rg "sage.version"
rg "dir(sage.version)"
rg "sage.version.version"
rg "plot"
rg "preparse"
rg "preparse('x=1')"
rv "x=2^100"
g "x"
objectType oo
rv "R.<x,y,z> = QQ[]"
g "R"
