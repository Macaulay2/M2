newPackage(
	"TestSimpleDoc",
    	Version => "0.1", 
    	Date => "Aug 31, 2010",
    	Authors => {
	     {Name => "Dan Grayson", 
		  Email => "dan@math.uiuc.edu", 
		  HomePage => "http://www.math.uiuc.edu/~grayson/"},
	     {Name => "Mike Stillman", 
		  Email => "mike@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/~mike/"}},
    	Headline => "test module for SimpleDoc",
    	DebuggingMode => false
    	)

export {f10, f11, f12, f13, f14}

debug needsPackage "SimpleDoc"

f10 = method()
f10 Matrix := (m) -> m

f11 = method()
f11 Matrix := (m) -> m

f12 = method()
f12 Matrix := (m) -> m

f13 = method()
f13 Matrix := (m) -> m

f14 = method()
f14 Matrix := (m) -> m

------------------------------------------
str1 = ///
Node
  Key
    TestSimpleDoc
  Headline
    A package for testing SimpleDoc
  Description
    Text
    Example
  Caveat
  SeeAlso
///
-- doc1 = toDoc(NodeFunctions, str1)
------------------------------------------
-- This one fails, with a reasonable error message
-- TestSimpleDoc.m2:48:6:(2):[10]: error: line 5 of string: expected single indented line after Headline
str2 = ///
Node
  Key
    [undefinedFunction, Matrix]
  Headline
  Description
    Text
    Example
  Caveat
  SeeAlso
///
-- doc2 = toDoc(NodeFunctions, str2) -- this line gives the error
------------------------------------------
str3 = ///
Node
  Key
    bad key
  Headline
  Description
    Text
    Example
  Caveat
  SeeAlso
///
-- doc3 = toDoc(NodeFunctions, str3)
------------------------------------------
------------------------------------------
-- TestSimpleDoc.m2:84:1:(3):[7]: error: expected first element of document key for optional argument to be a function or sequence
-- this is so so error message...
-- this error message is coming from 'document'.  We should check results first?
str4 = ///
Node
  Key
    [undefinedFunction, Matrix]
  Headline
    Now we have a head line
  Description
    Text
    Example
  Caveat
  SeeAlso
///
-- doc4 = toDoc(NodeFunctions, str4)
------------------------------------------
--TestSimpleDoc.m2:117:1:(3):[7]: error: line 17 of string: expected at least one indented line after Usage
-- good error message
str5 =  ///
Node
  Key
    "node3"
  Headline
    boo
  Description
    Text
    Example
  Caveat
  SeeAlso
Node
  Key
    "node4"
  Headline
    boo2
  Usage
  Inputs
  Outputs
  Consequences
  Description
    Text
    Example
    Code
    Pre
  Caveat
  SeeAlso
///
------------------------------------------
-- this one is more subtle: no error message
-- but usage is not displayed either
-- in fact, only Caveat is displayed on the page
str6 =  ///
Node
  Key
    "node4"
  Headline
    boo2
  Usage
    f = g b
  Inputs
  Outputs
  Consequences
  Description
    Text
    Example
    Code
    Pre
  Caveat
  SeeAlso
///
------------------------------------------
-- this one is more subtle: no error message
-- but usage is not displayed either
-- in fact, only Caveat is displayed on the page
str7 =  ///
Node
  Key
    "node7"
  Headline
    boo7
  Usage
    b = f a
  Inputs
    a:Matrix
  Outputs
    b:Matrix
  Description
    Text
    Example
    Code
    Pre
  Caveat
  SeeAlso
///
------------------------------------------
-- usage is still not displayed here
str8 =  ///
Node
  Key
    "node8"
  Headline
    boo8
  Usage
    b = f a
  Inputs
    a:Matrix
  Outputs
    b:Matrix
  Description
    Text
      Some text
  Caveat
  SeeAlso
///
------------------------------------------
-- usage is not displayed here.  Why not?
str9 =  ///
Node
  Key
    "node9"
  Headline
    boo9
  Usage
    b = f a
  Inputs
    a:Matrix
      what a is
  Outputs
    b:Matrix
      what b is
  Description
    Text
      Some text
  Caveat
  SeeAlso
///
------------------------------------------
-- Usage is displayed here.  So it depends on whether it is a function being documented
str10 =  ///
Node
  Key
    f10
  Headline
    boo10
  Usage
    b = f10 a
  Inputs
    a:Matrix
      what a is
  Outputs
    b:Matrix
      what b is
  Description
    Text
      Some text
  Caveat
  SeeAlso
///
------------------------------------------
-- note: Usage might need at least one line, but Inputs, Outputs, Consequences do not
-- also, indenting on inputs is not so strict
str11 =  ///
Node
  Key
    f11
  Headline
    boo11
  Usage
    b = f11 a
  Inputs
      a:Matrix
    b:    Matrix
  Outputs
  Consequences
  Description
    Text
      Some text
  Caveat
  SeeAlso
///
------------------------------------------
str12 =  ///
Node
  Key
    f12
    (f12,Matrix)
  Headline
    boo12
  Usage
    b = f12 a
  Inputs
    a:Matrix
    b:Matrix
  Outputs
    c:Matrix
  Consequences
    Item
      Cool  stuff gets deleted
  Description
    Text
      Blah
    Example
      f12 id_(ZZ^3)
  Caveat
  SeeAlso
///
------------------------------------------
--  This one does not give an error, but probably should
--  the input line is displayed as a => Matrix, an optional argument, I guess
str13 =  ///
Node
  Key
    f13
  Headline
    boo13
  Usage
    b = f13 a
  Inputs
    Matrix:a
  Description
    Text
      Use this function
  Caveat
  SeeAlso
///
------------------------------------------
-- Bad error message, if only because it doesn't say where it came from
-- also: does user need to know about local, global invisible, external string?
-- stdio:4:1:(3): error: can't convert local symbol or invisible global symbol 'g14' to external string
-- currentString:1:1-1:3: here is the first use of 'g14'
str14 =  ///
Node
  Key
    f14
  Headline
    boo14
  Usage
    b = f14 a
  Inputs
    Matrix:a
  Description
    Text
      Use this function
  Caveat
  SeeAlso
    g14
///
------------------------------------------
str15 =  ///
Node
  Key
    "test15"
  Headline
    boo15
  Description
    Text
      Valid keywords include:
    Code
      UL {
	   {TEX "\\bf Key"},
	   TEX "\\bf Headline",
	   TEX "\\bf Usage",
	   TEX "\\bf Inputs",
	   TEX "\\bf Outputs",
	   TEX "\\bf Consequences",
	   TEX "\\bf Description",
	   TEX "\\bf SeeAlso",
	   TEX "\\bf Subnodes",
	   TEX "\\bf Caveat"
	   }
  Caveat
  SeeAlso
///
------------------------------------------

beginDocumentation()

multidoc str1
-- multidoc str2 -- gives correct error
-- multidoc str3 -- gives bad error: very hard to see where error occurs
-- multidoc str4 -- gives so so error
-- multidoc str5 -- gives correct error
multidoc str6
multidoc str7
multidoc str8
multidoc str9
multidoc str10
multidoc str11
multidoc str12
multidoc str13
-- multidoc str14 -- gives bad error message
multidoc str15

-- document \ doc1
-- document \ doc2
-- document \ doc3

end

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

end
restart
--debug loadPackage("SimpleDoc", DebuggingMode=>true)
loadPackage("SimpleDoc")
debug loadPackage "TestSimpleDoc"
installPackage "TestSimpleDoc"
viewHelp TestSimpleDoc
uninstallPackage "TestSimpleDoc"


