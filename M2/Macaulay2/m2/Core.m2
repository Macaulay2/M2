--		Copyright 1993-2003 by Daniel R. Grayson
-- The source code of Macaulay2 is contained in multiple files,
-- all contained in the subdirectory "Macaulay2/".

if notify then printerr("reading ", minimizeFilename currentFileName)

if class Core =!= Symbol
or class oooo =!= Symbol then error "Core cannot be reloaded"
if class RawMutableMatrix =!= Type then error "where is RawMutableMatrix?"

-----------------------------------------------------------------------------
-- Temporary definitions to get error messages working early
-----------------------------------------------------------------------------

assert'( class between === Symbol )
between = (m, v) -> mingle(v, (#v - 1):m)

assert'( class toString === Symbol )
toString = x -> (
    if hasAttribute(x, ReverseDictionary) then simpleToString getAttribute(x, ReverseDictionary) else
    if class x === Net then concatenate between("\n", unstack x) else simpleToString x)

silentRobustNetWithClass =
silentRobustNet    = (wid, ht, sec, y) -> simpleToString y
silentRobustString = (wid,     sec, y) -> simpleToString y

-----------------------------------------------------------------------------
-- Core utilities
-----------------------------------------------------------------------------

-- TODO: move to the interpreter, can it be made copy free?
nonnull = x -> select(x, i -> i =!= null)
nonempty = x -> select(x, i -> i =!= "")

-- TODO: deprecate these
undocumentedkeys = new MutableHashTable
undocumented' = key -> undocumentedkeys#key = true

-- a first-in last-out list of symbol values
-- TODO: move to the interpreter and make thread-safe
varstack = new MutableHashTable
pushvar = (sym,newval) -> (
     varstack#sym = if varstack#?sym then (value' sym,varstack#sym) else (value' sym, null);
     sym <- newval;
     )
popvar = (sym) -> if varstack#?sym then (
     c := varstack#sym;
     if c === null then error "internal error: empty stack";
     sym <- c#0;
     varstack#sym = c#1;
     )

-----------------------------------------------------------------------------
-- Dictionaries and Symbols
-----------------------------------------------------------------------------

OutputDictionary = new Dictionary
dictionaryPath = append(dictionaryPath,OutputDictionary)

-- references to the symbol 'User' occur before the corresponding package has been created...
getGlobalSymbol(PackageDictionary,"User")

-- the former behavior
-- getSymbol = nm -> (
--      if isGlobalSymbol nm then return getGlobalSymbol nm;
--      for d in dictionaryPath do if mutable d then return getGlobalSymbol(d,nm);
--      error "no mutable dictionary on path";
--      )

getSymbol = s -> (
     if instance(User,Symbol) then error "getSymbol used before package User created";
     getGlobalSymbol(User#"private dictionary", s))

-----------------------------------------------------------------------------
-- Standard AfterEval and oo rotation
-----------------------------------------------------------------------------

rotateOutputLines := x -> (
     if ooo =!= null then global oooo <- ooo;
     if oo =!= null then global ooo <- oo;
     if x =!= null then global oo <- x;
     )

Thing#AfterEval = x -> (
     if x =!= null then (
     	  s := getGlobalSymbol(OutputDictionary,concatenate(interpreterDepth:"o",simpleToString lineNumber));
     	  s <- x;
	  );
     rotateOutputLines x;
     x)

Nothing#{Standard,Print} = identity

-----------------------------------------------------------------------------
-- load, input, needs
-----------------------------------------------------------------------------

searchPath' = (path, name) -> select(path, dir -> fileExists concatPath(dir, name))
loadPath := (path, filename, loadfun, notify) -> (
    if class filename =!= String then error "expected a string";
    ret := if isAbsolutePath filename then ( if first tryLoad(filename, filename, loadfun, notify) then "" )
    else scan(if isStablePath filename then {currentFileDirectory} else path, dir -> (
	    if class dir =!= String then error "member of 'path' not a string";
	    filepath := concatPath(dir, filename);
	    if debugLevel === 1011 then printerr("attempting to load ", filepath);
	    if first tryLoad(filename, filepath, loadfun, notify) then break dir));
    if ret =!= null then return ret;
    error splice("file not found",
	if isAbsolutePath filename then "" else
	if isStablePath   filename then (" in ", format currentFileDirectory)
	else " on path", ": ", format filename))

load  = filename -> (loadPath(path, filename, simpleLoad, notify);)
input = filename -> (loadPath(path, filename, simpleInput, false);)
needs = filename -> if not filesLoaded#?filename then load filename else (
     (filepath, filetime) := filesLoaded#filename;
     if filetime < fileTime filepath then load filepath)

-----------------------------------------------------------------------------
-- Load the rest of Core
-----------------------------------------------------------------------------

-- if nocore then end

loads := minimizeFilename concatenate(currentFileDirectory, "loadsequence")
if notify then printerr("about to read ", loads)
scan(select("^\\w+\\.m2", "$&", get loads), needs)
if notify then printerr("read ", loads)

corepath' := corepath
userpath' := userpath
noinitfile' := noinitfile
-- after this point, private global symbols, such as noinitfile,
-- are no longer visible, and public symbols have been exported
endPackage "Core"

path = select(if not noinitfile' then join(userpath', path) else path, dir -> not member(dir, corepath'))
if #OutputDictionary > 0 then error("symbols entered into OutputDictionary from Core: ", toString keys OutputDictionary)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
