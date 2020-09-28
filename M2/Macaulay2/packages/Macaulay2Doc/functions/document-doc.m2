--- status: Rewritten August 2020
--- author(s): Mahrud
--- notes: functions below are all defined in document.m2, roughly in this order

-- documented in SimpleDoc
undocumented { Description, (document, String) }

doc ///
Node
  Key
     document
    (document, List)
  Usage
    document { Key => key, ... }
  Headline
    create a documentation node
  Inputs
    :List
      with @TO Hypertext@, @TO String@, and special documentation entries as
      optional arguments such as @TT "Key => \"...\""@ (required)
    Key=>List
    Headline=>List
    BaseFunction=>List
    Usage=>List
    Inputs=>List
    Outputs=>List
    Consequences=>List
    Acknowledgement=>List
    Contributors=>List
    References=>List
    Caveat=>List
    SeeAlso=>List
    Subnodes=>List
    SourceCode=>List
    ExampleFiles=>List
  Consequences
    Item
      formatted documentation is created and stored in the @TO2 {"currentPackage", "current package"}@
  Description
    Text
      There are two basic types of documentation:

      @OL {
          {"Documenting a function or a method, as in ", TO (resolution, Module), "."},
          {"documenting an overview or functions with methods, as in ", TO "chain complexes",
              " or ", TO resolution, " respectively."}}@

      The headings @TO Usage@, @TO BaseFunction@, @TO Inputs@, @TO Outputs@, and @TO Consequences@,
      are useful only for documentation of the first type, as well as in a call to @TO SYNOPSIS@.

    Tree
      :Templates for using the @TO (document, List)@ method
        "package documentation template"
        "function documentation template"
        "optional argument documentation template"
        "overview documentation template"

    Text
      Here is a basic template:
    Code
      EXAMPLE { PRE ////
          document {
              Key          => {myFunc, (myFunc, MyType), [myFunc, MyOption], ...},
              Headline     => "one line description of myFunc", -- not needed for overviews
              BaseFunction => myFunc, -- usually not needed
              Usage        => "myFunc x",
              Inputs       => { inputs, ... },
              Outputs      => { outputs, ... },
              SourceCode   => { METHOD1, ... }, -- usually not needed
              Consequences => { side-effects, ... },
              "A list of strings containings names of files in the auxiliary
              directory of the package can go here; the files will be visible
              in the current directory while example code is run.",
              ExampleFiles => {"datafile1", "datafile2"},
              "There can be explanatory prose here in the form of a hypertext list.",
              EXAMPLE lines //////
                m2code
                m2code
                m2code
              //////,
              "There can be explanatory prose here in the form of a hypertext list.",
              Caveat       => {"warning"},
              SeeAlso      => {"other things"},
              Subnodes     => {
                  "subheading a",
                  TO "node 1",
                  TO "node 2",
                  "subheading b",
                  TO "node 3",
                  ...
                  },
              }////}
    Tree
      :Other Functions generating @TO "hypertext list format"@ that you may use:
        SYNOPSIS
        EXAMPLE
  SeeAlso
    "writing documentation"
    hypertext
  Subnodes
    :Documentation templates based on the hypertext list format
    "package documentation template"
    "function documentation template"
    "optional argument documentation template"
    "overview documentation template"
    :Special documentation keywords
    [document, Key]
    [document, Headline]
    [document, BaseFunction]
    [document, Usage]
    [document, Inputs]
    [document, Outputs]
    [document, Consequences]
    [document, Acknowledgement]
    [document, Contributors]
    [document, References]
    [document, Caveat]
    [document, SeeAlso]
    [document, Subnodes]
    [document, SourceCode]
    [document, ExampleFiles]

Node
  Key
    Key
    [document, Key]
  Headline
    key of a documentation node
  Usage
    document { Key => key, ... }
  Consequences
    Item
      specifies the name under which the documentation is stored
  Description
    Text
      The document key is the name of the node, specifically the one that allows users to find the
      documentation for the desired command. Some node names, rather than being strings, are sequences
      of Macaulay2 objects that refer to specific method functions. If one is documenting a function of
      Macaulay2, the key will just be the function's name, as in the documentation for @TO resolution@.
    Code
      EXAMPLE { PRE ////Key => {resolution}//// }

    Text
      However, if one is documenting a method for a function, then the key will have
      a different form, as in the documentation for @TO (resolution, Module)@.
    Code
      EXAMPLE { PRE ////Key => {(resolution, Module)}//// }

    Text
      If one is documenting an optional argument to a function, then the key
      has a different form still, as in @TO [resolution, SyzygyLimit]@.
    Code
      EXAMPLE { PRE ////Key => {[resolution, SyzygyLimit]}//// }

    Text
      Finally, if one is documenting an overview of a group of functions or a package,
      as in @TO "chain complexes"@ then one would set up a document key of the form:
    Code
      EXAMPLE { PRE ////Key => {"chain complexes"}//// }
  Caveat
    No two documentation nodes may have the same key or even a key that is a synonym for a key which is already used.
    However, sometimes there is a need for several documentation nodes to have the same key. This happens when a
    command that needs to be documented is a synonym for another already documented command. In this case the
    synonym's key should be stated in quotation marks.
    As an example, look at the documentation for @TO "Text :: SUBSECTION"@ and @TO "Text :: HEADER2"@ in
    @HREF { currentLayout#"packages" | "Text.m2", "Text.m2" }@.
  SeeAlso
    TO

Node
  Key
    Headline
    [document, Headline]
  Headline
    headline of a documentation node
  Usage
    document { Key => key, Headline => "one line description", ... }
  Consequences
    Item
      the headline string will be used to annotate itemized lists of cross-references to the documentation @TO "Key"@
  Description
    Text
      The headline of a documentation node, gives a brief, half line, description of the thing being documented.
      As an example, the headline for this documentation node was obtained with the code:
    Code
      EXAMPLE { PRE ////Headline => "make a headline for a documentation node"//// }
  SeeAlso
    TOH

Node
  Key
    BaseFunction
    [document, BaseFunction]
  Headline
    the basename of a function in a documentation node
  Description
    Text
      The @TT "BaseFunction"@ entry gives the function that uses the feature being documented.

      Here is a sample usage of this entry:
    Code
      EXAMPLE { PRE ////BaseFunction => myFunc//// }

Node
  Key
    Usage
    [document, Usage]
  Headline
    ways to use a function in a documentation node
  Usage
    document { ... , Usage => "usage", ... }
  Description
    Text
      The @TT "Usage"@ entry should give a formal example showing the usage of the function.
      The variables used in this formal example should be the ones used in the @TO "Inputs"@ and @TO "Outputs"@
      sections of the documentation. Here is the code for the @TT "Usage"@ entry of the method @TO (matrix, List)@:
    Code
      EXAMPLE { PRE ////Usage => "matrix v"//// }
    Text
      Here is the code for the @TT "Usage"@ entry of the method @TO (resolution, Module)@:
    Code
      EXAMPLE { PRE ////Usage => "resolution M\nres M"//// }
    Text
      This option also can be used within a @TO SYNOPSIS@ section.
  SeeAlso
    Inputs
    Outputs

Node
  Key
    Inputs
    [document, Inputs]
  Headline
    inputs for a function in a documentation node
  Usage
    document { ... , Inputs => { inputs, ... }, ... }
  Description
    Text
      The entries should consist of items in one of the following forms.

      @UL {
          TT "hypertext",
          TT "class",
          TT "symbolname",
          TT "class => hypertext",
          TT "symbolname => class",
          TT "symbolname => hypertext",
          TT "symbolname => class => hypertext",
          }@

      As an example, here is the @TT "Inputs"@ entry of the method @TO (resolution, Ideal)@:
    Code
      EXAMPLE { PRE ////Inputs => { "I" => { "an ideal in a ring ", TT "R", ", say" } }//// }
    Text
      Here is an example of the @TT "Inputs"@ entry of the function @TO sin@:
    Code
      EXAMPLE { PRE ////Inputs => { "x" => RR },//// }
  SeeAlso
    Outputs
    Usage

Node
  Key
    Outputs
    [document, Outputs]
  Headline
    outputs for a function in a documentation node
  Usage
    document { ... , Outputs => { outputs, ... }, ... }
  Description
    Text
      The entries should consist of items in one of the following forms.

      @UL {
          TT "hypertext",
          TT "class => hypertext (or null)",
          TT "symbolname => hypertext (or null)",
          TT "symbolname => class => hypertext (or null)",
          }@

      As an example, here is the @TT "Outputs"@ entry of the method @TO ( resolution,Ideal)@:
    Code
      EXAMPLE { PRE ////Outputs => { {"a resolution of ", TT "R/I", " by projective ", TT "R", "-modules" } }//// }
    Text
      Note that the hypertext list needs to be bounded by @TT "{"@ and @TT "}"@ as there is only one output for
      @TO (resolution, Ideal)@. Without the braces, multiple outputs are defined. Note also that the @TO Type@ of
      the output is automatically added in this case.

      Here is an example of the @TT "Outputs"@ entry of the function @TO sin@:
    Code
      EXAMPLE { PRE ////Outputs => { { "the sine of ", TT "x", "" } }//// }
  SeeAlso
    Inputs
    Usage

Node
  Key
    Consequences
    [document, Consequences]
  Headline
    side-effects of a function in a documentation node
  Usage
    document { ... , Consequences => { "side-effects" }, ... }
  Description
    Text
      Here is where one documents effects of a function that are not return values.
      As an example here is the @TT "Consequences"@ entry for the documentation node @TO Headline@:
    Code
      EXAMPLE { PRE ////Consequences => {
              { "the headline string will be used to annotate itemized
                  lists of crossreferences to the documentation ", TO "Key" }}//// }

Node
  Key
    Acknowledgement
    [document, Acknowledgement]
  Headline
    acknowledgements listed in a documentation node
  Usage
    document { ... , Acknowledgements => {"NSF grant number"}, ... }
  Description
    Text
      This part of the documentation can be used to acknowledge funding sources and collaborators.
  SeeAlso
    Contributors

Node
  Key
    Contributors
    [document, Contributors]
  Headline
    non-author contributors listed in a documentation node
  Usage
    document { ... , Contributors => {"previous authors"}, ... }
  Description
    Text
      This part of the documentation can be used to list contributors and previous authors to the
      package who are no longer maintainers for it.
  SeeAlso
    Acknowledgement

Node
  Key
    References
    [document, References]
  Headline
    references listed in a documentation node
  Usage
    document { ... , References => {"bibliography"}, ... }
  Description
    Text
      This part of the documentation can be used to list references for the package.

Node
  Key
    Caveat
    [document, Caveat]
  Headline
    warnings or limitations listed in a documentation node
  Usage
    document { ... , Caveat => {"warning"}, ... }
  Description
    Text
      This part of the documentation serves to highlight pitfalls for the user.

Node
  Key
    SeeAlso
    [document, SeeAlso]
  Headline
    cross-references to other documentation nodes
  Usage
    document { ... , SeeAlso => { ... }, ... }
  Description
    Text
      This option inserts into a documentation page a sentence instructing the reader to see some other topics.

      The entries may have the special forms used with @TO "TO"@.
      As an example, here is the code for the @TT "SeeAlso"@ part of a documentation node referencing this node.
    Code
      EXAMPLE { PRE ////SeeAlso => {[document, SeeAlso]}//// }
  SeeAlso
    TOH
    Subnodes

Node
  Key
    Subnodes
    [document, Subnodes]
  Headline
    a menu of documentation nodes
  Usage
    document { ... , Subnodes => { ... }, ... }
  Consequences
    Item
      the documentation subnodes listed will appear as decendents of this node in the documentation tree
  Description
    Text
      This option inserts into a documentation page a menu of subnodes and defines how the tree structure
      of the documentation is constructed. This is relevant when printing the documentation.

      @SUBSECTION {"Example of usage as an optional argument to ", TO (document, List)}@
    Code
      EXAMPLE { PRE ////Subnodes => {
              "An example of subnodes",
              TO "a link name",
              TO "another link name"
              },//// }

    Text
      @SUBSECTION {"Example of usage as a keyword in ", TO "SimpleDoc :: doc(String)"}@

      When used as a main section of a documentation node, @TT "Subnodes"@ can be used as follows:
    Code
      EXAMPLE { PRE ////Subnodes
          :Heading 1
            myFunction
            "a link name"
            :A subheading about @TO "mySecondFunction"@
              mySecondFunction
          :Heading 2
            "another link name"//// }
  SeeAlso
    TOH
    "SimpleDoc :: doc(String)"

Node
  Key
    SourceCode
    [document, SourceCode]
  Headline
    sources displayed in a documentation node
  Description
    Text
      This option inserts the source code of the listed functions into a documentation page.

      As an example, here is the code for the @TT "SourceCode"@ part
      of the documentation node for @TO (variety, SheafOfRings)@.
    Code
      EXAMPLE { PRE ////SourceCode => {(variety, SheafOfRings)}//// }
  SeeAlso
    code

Node
  Key
    ExampleFiles
    [document, ExampleFiles]
  Headline
    data files corresponding to a documentation node
  Description
    Text
      This option lists filenames of data files in the auxiliary directory which
      should be made available in the directory where the example code is run.

      Here is a sample usage of this entry:
    Code
      EXAMPLE { PRE ////ExampleFiles => {"datafile1", "datafile2"}//// }
  SeeAlso
    [newPackage, AuxiliaryFiles]
///

doc ///
Node
  Key
    "function documentation template"
    "function name documentation template"
  Description
    Code
      EXAMPLE { PRE ////document {
              Key => {
                   functionName,
                  (functionName, argumentClass1, argumentClass2, ...)},
              Headline     => "one line description",
              Usage        => "usage",
              Inputs       => {}, -- each input is a hypertext list
              Outputs      => {}, -- each output is a hypertext list
              Consequences => {}, -- each effect is a hypertext list
              "There can be explanatory prose here in the form of a hypertext list.",
              EXAMPLE {
                  "m2code",
                  "m2code",
                  "m2code"},
              "There can be explanatory prose here in the form of a hypertext list.",
              Caveat => {"warning"}
              }//// }
  SeeAlso
    "writing documentation"
    "hypertext list format"
Node
  Key
    "optional argument documentation template"
    "optional argument name documentation template"
  Description
    Code
      EXAMPLE { PRE ////document {
              Key => {
                   OptionName,
                  [functionName, OptionName]},
              Headline     => "one line description",
              Usage        => "usage",
              Inputs       => {}, -- each input is a hypertext list
              Consequences => {}, -- each effect is a hypertext list
              "There can be explanatory prose here in the form of a hypertext list.",
              EXAMPLE {
                  "m2code",
                  "m2code",
                  "m2code"},
              "There can be explanatory prose here in the form of a hypertext list.",
              Caveat => {"warning"}
              }//// }
  SeeAlso
    "writing documentation"
    "hypertext list format"
Node
  Key
    "overview documentation template"
  Description
    Code
      EXAMPLE { PRE ////document {
              Key      => "description of this page",
              Headline => "one line description",
              "There can be explanatory prose here in the form of a hypertext list.",
              EXAMPLE {
                  "m2code",
                  "m2code",
                  "m2code"},
              "There can be explanatory prose here in the form of a hypertext list.",
              Caveat => {"warning"}
              }//// }
  SeeAlso
    "writing documentation"
    "hypertext list format"
Node
  Key
    "package documentation template"
  Description
    Code
      EXAMPLE { PRE ////document {
              Key      => PackageName,
              Headline => "one line description",
              "There can be explanatory prose here in the form of a hypertext list.",
              EXAMPLE {
                  "m2code",
                  "m2code",
                  "m2code"},
              "There can be explanatory prose here in the form of a hypertext list.",
              Acknowledgement => {} -- a hypertext list
              Contributors => {} -- a hypertext list
              References => {} -- a hypertext list
              Caveat => {"warning"}
              Subnodes => {
                  TO functionName1,
                  TO functionName2 -- and so on
                  }
              }//// }
  SeeAlso
    "writing documentation"
    "hypertext list format"
///

doc ///
Node
  Key
     SYNOPSIS
    (SYNOPSIS, List)
    (SYNOPSIS, Thing)
    (SYNOPSIS, Sequence)
    [SYNOPSIS, BaseFunction]
    [SYNOPSIS, Heading]
    [SYNOPSIS, Usage]
    [SYNOPSIS, Inputs]
    [SYNOPSIS, Outputs]
    [SYNOPSIS, Consequences]
    Heading
  Headline
    a standardized synopsis for use in documentation nodes
  Usage
    SYNOPSIS ( ..., Usage => "...", ... )
  Inputs
    :List
      with @TO Hypertext@, @TO String@, and special documentation entries as
      optional arguments such as @TT "Usage => \"...\""@ (required)
    BaseFunction=>Function
      see @TOH [document, BaseFunction]@
    Heading=>List
      see @TO  [document, Headline]@ -- specifies a subheading for the synopsis
    Usage=>List
      see @TOH [document, Usage]@
    Inputs=>List
      see @TOH [document, Inputs]@
    Outputs=>List
      see @TOH [document, Outputs]@
    Consequences=>List
      see @TOH [document, Consequences]@
  Outputs
    :Hypertext
      usable as an input of @TO (document, List)@
  Description
    Text
      This function prepares a standardized synopsis in hypertext for use in documentation nodes.
      Here is an empty template for use with @TT "SYNOPSIS"@.
    Code
      EXAMPLE { PRE ////SYNOPSIS (
              BaseFunction => fn,
              Heading      => "",
              Usage        => "",
              Inputs       => {},
              Outputs      => {},
              Consequences => {},
              PARA { ... },
              EXAMPLE lines " ... ")//// }
    Text
      The options are used just as with @TO document@. Here is an example of its use.
    Code
      EXAMPLE { PRE ////SYNOPSIS {
              Heading => "using binary methods for method functions",
              Usage   => "f(x,y)",
              Inputs  => {
                  "f" => { "a method function" },
                  "x" => { "an object of type ", TT "X" },
                  "y" => { "an object of type ", TT "Y" }},
              Outputs => {
                  { "the previously installed method for ", TT "f(X, Y)", " is called with arguments ", TT "(x, y)",
                      ", and the return value is returned. If no such method has been installed, then Macaulay2
                      searches for a method for ", TT "f(X',Y')", ", where ", TT "X'", " is an ancestor of ", TT "X",
                      " and ", TT "Y'", " is an ancestor of ", TT "Y", "(see ", TO "inheritance", " for details)."}},
              "The second line of the following example illustrates the syntax above,
              using ", TO "source", ", which happens to be a method function.",
              EXAMPLE lines //////
                source(String, String) := peek;
                source("foo", "bar")//////,
              PARA "The same syntax works for 3 or 4 arguments."}//// }
    Text
      See the @TT "Synopsis"@ keyword in @TO "SimpleDoc :: doc(String)"@ for usage as a keyword in that format.
///

doc ///
Node
  Key
     undocumented
    (undocumented, Thing)
    (undocumented, List)
  Headline
    declare that something need not be documented
  Usage
    undocumented key
  Inputs
    key:Thing
      a documentation key, or a list of keys
  Consequences
    Item
      the documentation key(s) are designated as keys not needing documentation,
      thus avoiding warning messages when a package is installed.
  SeeAlso
    installPackage
    "documentation keys"
  Description
    Example
      f = method()
      f List := x -> 1
      f VisibleList := x -> 2
      f BasicList := x -> 3
      undocumented { f, (f,List) }
///
