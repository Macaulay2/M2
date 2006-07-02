--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

recursionLimit = 300

addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))
addEndFunction(() -> path = {})

wr := (sep,x) -> wrap(printWidth - promptWidth(), sep, net x)
Tally.Wrap = RawMatrix.Wrap = Matrix.Wrap = Ideal.Wrap = RingElement.Wrap = VisibleList.Wrap = Sequence.Wrap = x -> wr("-",x)
String.Wrap = x -> ( x = net x; if height x + depth x <= 3 then wr("",x) else x )
Net.Wrap = x -> if height x + depth x <= 3 then wr("-",x) else x

addStartFunction( 
     () -> (
	  if class value getGlobalSymbol "User" =!= Package then (
     	       dismiss "User";
	       newPackage("User", DebuggingMode => true);
	       );
	  needsPackage \ Core#"pre-installed packages";
	  if not member("--silent",commandLine) then stderr << "with packages: " << wrap concatenate between_", " sort Core#"pre-installed packages" << endl;
	  )
     )

addStartFunction( () -> if sourceHomeDirectory =!= null then Core#"source directory" = sourceHomeDirectory|"m2/" )

addStartFunction( () -> if not noinitfile and prefixDirectory =!= null then makePackageIndex() )

unexportedSymbols = () -> hashTable apply(pairs Core#"private dictionary", (n,s) -> if not Core.Dictionary#?n then (s => class value s => value s))

scan(values Core#"private dictionary" - set values Core.Dictionary,
     s -> if mutable s and value s === s then stderr << symbolLocation s << ": warning: mutable unexported unset symbol in Core: " << s << endl)

-- make sure this is after all public global symbols are defined or erased
endPackage "Core"
-- after this point, private global symbols, such as noinitfile, are no longer visible

load "installedpackages.m2"

scan(Core#"pre-installed packages",	-- initialized in the file installedpackages.m2, which is made from the file installedpackages
     pkg -> loadPackage(pkg, DebuggingMode => not stopIfError))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
