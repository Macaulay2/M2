newPackage ("RandomObjects",
      	Version => "0.2",
    	Date => "June 19, 2011",
    	Authors => {
	     {Name     => "Hans-Christian Graf v. Bothmer",
	      Email    => "bothmer@uni-math.gwdg.de",
	      HomePage => "http://www.crcg.de/wiki/User:Bothmer"},

	     {Name     => "Florian Geiss",
	      Email    => "fg@math.uni-sb.de",
	      HomePage => "http://www.math.uni-sb.de/ag/schreyer/"},

	     {Name     => "Daniel R. Grayson",
	      Email    => "dan@math.uiuc.edu",
	      HomePage => "http://www.math.uiuc.edu/~dan/"},

	     {Name     => "Frank-Olaf Schreyer",
	      Email    => "schreyer@math.uni-sb.de",
	      HomePage => "http://www.math.uni-sb.de/ag/schreyer/"}},
        Headline => "a framework for making random objects in algebraic geometry",
	Keywords => {"Examples and Random Objects"},
	PackageImports => {"SimpleDoc"},
    	DebuggingMode => false
        )



-- export of functions, options and keys:

export {
     "Certify",  -- here we have a minor problem: "Certify" already exists as
                 -- a reserved word and in the documentation we need to get
		 -- a link to the docu-site of our "Certify"
     "RandomObject",
     "Attempts",
     -- We can't export Certification, because it conflicts with an optional argument to newPackage, so prevents some
     -- other packages from being loaded after this one.
     -- "Certification",
     "Construction",
     "randomObjectTemplate"}


-- the central new type of this package is the
-- randomObject which derives from the type
-- MutableHashTable. It is expected that a RandomObject
-- has the following keys:
-- Construction
-- Certification

RandomObject = new Type of MutableHashTable

-- the following line ensures that if an
-- object of type RandomObject is assigned to
-- a global variable and the object will print
-- out its type.

globalAssignment RandomObject

-- (random randomObjectClass) returns a function that
-- constructs a random object of the class using the
-- method "randomObjectClass.Construction". The returned
-- function takes arguments specified in "randomObjectClass.Construction"
-- (e.g. an ambient coordinate ring) and -in addition -
-- the options "Attempts" and "Verify". If "Verify" is set
-- to "true", the constructed object is tested using
-- the method "randomObjectClass.Verification" that
-- returns "true" or "false". "Attempts" specifies
-- the maximum number "randomObjectClass.Construction"
-- is carried out unless it returns a value other than
-- "null". If "Verify" is set to "true" then "Attempts"
-- specifies the maximum number "randomObjectClass.Construction"
-- is carried out unless it returns a value other than null
-- and that returns "true" under "randomClass.Verification".

-- The method function "random" which is overloaded has some
-- build in options which are caught in "randomopts".
-- All arguments of (random randomObjectClass) are caught
-- in "args"

random RandomObject := randomopts -> Object -> args -> (
     -- default values for certify and attempts
     cert := false;
     att := infinity;

     -- if the args consist of a single element make it into a
     -- sequence:
     if not instance(args, Sequence) then args = 1:args;

     -- argsConstr is a list that keeps all entries
     -- of args except for the Attempts-entry, if given.
     -- Its value is written to "att"
     argsConstr := toSequence (for x in args list (
	  if instance(x,Option) then (
	       if x#0 === Attempts then (att = x#1 ; continue )
	       else x)
	  else x));

     -- same as above but with Certify.
     argsCert:= toSequence (for x in argsConstr list (
	  if instance(x,Option) then (
	       if x#0 === Certify then (cert = x#1 ; continue )
	       else x
	       )
	  else x));

     -- try to construct the object until either the maximum number of
     -- attempts is reached or a (certified) object is constructed
     -- if no (certified) object is constructed the null is returned.
     for i from 1 do (
	  if i > att then return null;
	  object := Object.Construction argsConstr;
	  if object === null then continue;
	  if not cert then return object;
	  if Object.Certification prepend(object, argsCert) then return object;
	  ))


-- the following function returns a template
-- for a package that implements the randomObject
--

randomObjectTemplate=method()
randomObjectTemplate(String):=(Object)->(
     lowerObject:=toLower(Object_0)|Object_(1,#Object-1);
     upperObject:=toUpper(Object_0)|Object_(1,#Object-1);
     match("Outputs",docTemplate);
     docuString:=(docTemplate)_(lastMatch_0_0,#docTemplate-1);
     ("export {\n"|
      "  construct"|upperObject|",\n"|
      "  certify"|upperObject|",\n"|
      "  "|lowerObject|"\n"|
      "  }\n\n"|
      "construct"|upperObject|"=method(Options=>{Certify=>false})\n"|
      "construct"|upperObject|"(Thing):=opt->(args)->(\n   )\n\n"|
      "undocumented construct"|upperObject|"\n\n"|
      "certify"|upperObject|"=method()\n"|
      "certify"|upperObject|"(Thing,Thing)->(object,args)->(\n   )\n\n"|
      "undocumented certify"|upperObject|"\n\n"|
       lowerObject|" = new RandomObject from {\n
               Construction  => construct"|upperObject|",\n
	       Certification => certify"|upperObject|"}\n\n"|
      "doc /// \n"|
      "   Key\n    "|lowerObject|"\n"|
      "   Headline\n"|
      "   Usage\n"|
      "    (random "|lowerObject|")(args)\n"|
      "   Inputs\n"|
      "    args : Thing\n   "|docuString))

beginDocumentation()

doc ///
Key
 randomObjectTemplate
Headline
 a template for the implementation of a random object
Usage
 t=randomObjectTemplate(name)
Inputs
 name: String
          name of the object
Outputs
 t: String
      template for the implementation of "(random name)"
Description
  Text
    A template for the implementation of a new random object.
    Remember to document what Certify => true actually certifies.
  Example
    randomObjectTemplate("foo")
///

doc ///
Key
 RandomObjects
Headline
 a Framework for the construction of random points of unirational moduli spaces
Description
 Text
   This package provides the framework for the implementation of unirationality
   constructions.

   A moduli space $M$ of objects is unirational if there exists a dominant rational
   map $\phi:\mathbb{P}^n --> M$. If the function $\phi$ is explicitly given it can
   be translated into a construction function that computes $\phi(P)$ for a given
   $P \in \mathbb{P}^n$.
   If $P$ is chosen randomly (over a finite field $F_q$ or over a subset of $\mathbb{Q}$
   limited by a given height) it may not lie in the open subset of $\mathbb{P}^n$
   where $\phi$ is defined. This can be remedied by calling the function several times,
   i.e. allowing a certain number of Attempts. One is also interested in certifying the
   constructed object meaning that it satisfies certain reasonable properties.
///

doc ///
Key
 RandomObject
Headline
 framework for creation of random objects
Description
 Text
    RandomObject bundles the necessary functions for the construction of certified
    random objects parametrized by a unirational moduli space $M$.

    RandomObject is a MutableHashTable as ancestor and needs to have the following keys:

    * Construction: the method function assigned to this key contains a
      unirationality construction.

    * Certification: the method function assigned to this key checks whether the
      constructed object fulfills certain conditions.

    In the following example we construct plane curves of degree $d$. The Certification
    checks whether they are irreducible over the coefficient field.

 Example
    constructPlaneCurve = method(TypicalValue=>Ideal, Options=>{Certify=>false})
    constructPlaneCurve(ZZ,PolynomialRing):=opt->(d,R) -> ideal random(R^1,R^{1:-d})

    certifyPlaneCurve = method(TypicalValue=>Boolean)
    certifyPlaneCurve(Ideal,ZZ,PolynomialRing):=(I,d,R)-> #decompose I==1

    planeCurve = new RandomObject from {
     	 Construction => constructPlaneCurve,
     	 Certification => certifyPlaneCurve
     	 }

 Text
    We construct a curve of degree $2$ as follows

 Example
    setRandomSeed("alpha");
    R=ZZ/3[x_0..x_2];
    (random planeCurve)(2,R)

 Text
    We can certify the curve by using the option Certify

 Example
    (random planeCurve)(2,R,Certify=>true)

 Text
    We have chosen a small finite field as ground field. Hence,
    there is a certain chance that the curve is not irreducible.
    We can check this if we limit the number of attempts to $1$.
    If the curve is then reducible, {\tt null} is returned.

 Example
    tally apply(3^4,i->(random planeCurve)(2,R,Certify=>true,Attempts=>1) === null)
///

doc ///
  Key
   Attempts
  Headline
   number of attempts in the construction of a random object
  Usage
    (random randomObjectClass)(args,Attempts=>...)
  Description
   Text
     {\tt Attempts} can be assigned a nonnegative integer or {\tt infinity}
///

doc ///
  Key
   Certify
  Headline
   whether to certify the object
  Usage
    (random randomObjectClass)(args,Certify=>...)
///

doc ///
  Key
    "random(RandomObject)"
  Headline
    returns a function that constructs a random object
  Usage
    f=random(randomObjectClass)
  Inputs
    randomObjectClass : RandomObject
  Outputs
    f : Function
    	  that constructs a random object
  Description
    Text
      The returned function f takes the following options:

        * {\tt Attempts => ... } a nonnegative integer or {\tt infinity} (default)
	  that limits the maximal number of attempts for the construction of an object

        * {\tt Certify => ... } {\tt true} or {\tt false} (default) whether the output
	  is certified. The certifying properties are encoded in the Certification
	  function of the randomObjectClass
  SeeAlso
    Attempts
    Certify
///

doc ///
  Key
   Construction
  Headline
   key of randomObject that contains the construction function
  Usage
   randomObject.Construction(args)
  Outputs
   :Thing
///

-- doc ///
--   Key
--    Certification
--   Headline
--    key of randomObject that contains the Certification function
--   Usage
--    randomObject.Certification(args)
--   Outputs
--    :Boolean
-- ///

end

restart;
uninstallPackage"RandomObjects";
installPackage"RandomObjects";
viewHelp RandomObjects
viewHelp random
