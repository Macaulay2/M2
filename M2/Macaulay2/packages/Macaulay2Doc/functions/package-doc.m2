--- status: Rewritten August 2020
--- author(s): Mahrud
--- notes: functions below are all defined in packages.m2

doc ///
Node
  Key
    Package
  Headline
    the class of all packages
  Description
    Text
      See @TO "packages"@ for an overview about using and writing Macaulay2 packages.

    Tree
      :Available packages:
        "packages provided with Macaulay2"
        :The directory containing the packages is @HREF { currentLayout#"packages", currentLayout#"packages" }@

      :Functions useful when @TO "using packages"@:
        needsPackage
        loadPackage
        installPackage
        check
        debug
        importFrom

      :Functions useful when @TO "creating a package"@:
        newPackage
        export
        exportMutable
        beginDocumentation
        document
        TEST

      :Functions useful when debugging a package:
        "debugging"
        debug
        check

      :Functions that are only rarely used:
        use
        dismiss
        exportFrom
        endPackage
        readPackage
        uninstallPackage
        makePackageIndex
        "loadedPackages"
  SeeAlso
    "packages"
    "creating a package"
    "SimpleDoc :: packageTemplate"

Node
  Key
    "PackageDictionary"
  Headline
    the dictionary for names of packages
  Description
    Text
      This dictionary is used just for names of packages.
    Example
      dictionaryPath
      values PackageDictionary
  SeeAlso
    "dictionaryPath"

Node
  Key
     package
    (package, Array)
    (package, Dictionary)
    (package, DocumentTag)
    (package, Function)
    (package, HashTable)
    (package, Nothing)
    (package, Option)
    (package, Package)
    (package, Sequence)
    (package, String)
    (package, Symbol)
    (package, Thing)
  Headline
    get containing package
  Usage
    package x
  Inputs
    x:Thing
  Outputs
    :Package
      the package in which the documentation key @TT "x"@ was defined
  Description
    Example
      package sin
      package poly

Node
  Key
    "currentPackage"
  Headline
    the current package
  Description
    Example
      newPackage "Foo"
      currentPackage
      endPackage "Foo"

Node
  Key
     readPackage
    (readPackage, String)
    (readPackage, Package)
    [readPackage, FileName]
  Headline
    read the package preamble
  Usage
    readPackage PackageName
  Inputs
    PackageName:String
      the name of the package, or a @TO Package@ object
    FileName=>String
      the name of the file containing the source code of the package, from which it should be read
  Outputs
    :OptionTable
      containing the options given to @TO newPackage@ in the preamble of the package
  Description
    Text
      The file @TT "PackageName.m2"@ should be on the load @TO "path"@ and should contain a package named @TT "PackageName"@.
      This function is mostly used for getting information about a package quickly and without processing the entire package.
    Example
      opts = readPackage "NormalToricVarieties"
      "1.9" <= opts.Version
  SeeAlso
    Package
    newPackage
    loadPackage

Node
  Key
     loadPackage
    (loadPackage, String)
    (loadPackage, Package)
    [loadPackage, Configuration]
    [loadPackage, DebuggingMode]
    [loadPackage, FileName]
    [loadPackage, LoadDocumentation]
    [loadPackage, Reload]
  Headline
    load a package
  Usage
    loadPackage PackageName
  Inputs
    PackageName:String
      the name of the package, or a @TO Package@ object
    FileName=>String
      the name of the file containing the source code of the package, from which it should be loaded
    LoadDocumentation=>Boolean
      whether to load the documentation of the package, too; see @TO beginDocumentation@
    DebuggingMode=>Boolean
      the value of @TO "debuggingMode"@ during loading the package; specifying it here overrides the
      value specified as an option to @TO newPackage@ by the package itself; however, if @TO "debuggingMode"@
      is already @TT "false"@ it will remain so.
    Configuration=>List
      the entries are options @TT "KEY => VALUE"@ overriding the defaults specified in the source code of the package
      and the (possibly updated) values in the file in the user's application directory.
    Reload=>Boolean
      whether to reload the package, if it has been loaded before.
      If the input was an already loaded @TO Package@, this is automatically set to true.
  Outputs
    :Package
      the package just loaded; if the input was an already loaded @TO Package@, this is the reloaded package
  Consequences
    Item
      Loads (or reloads) the package @TT "PackageName"@ that is in the file @TT "PackageName.m2"@.
  Description
    Text
      The file @TT "PackageName.m2"@ should be on the load @TO "path"@ and should contain a package named @TT "PackageName"@.

      If the variable @TO "notify"@ is set to true, then an informational message is displayed after the file is loaded.
    Example
      notify = true
      loadPackage "FirstPackage"
    Text
      If the input is a loaded package, then this command will reload the package instead:
    Example
      loadPackage FirstPackage
    Text
      In fact this version of the command is simply a convenient shortcut for @TT "loadPackage"@ with the option
      @TT "Reload"@ overriden to true.
  SeeAlso
    "packages"
    readPackage
    dismiss
    needsPackage
    "packages provided with Macaulay2"

Node
  Key
     needsPackage
    (needsPackage, String)
    [needsPackage, Configuration]
    [needsPackage, DebuggingMode]
    [needsPackage, FileName]
    [needsPackage, LoadDocumentation]
    [needsPackage, Reload]
  Headline
    load a package if not already loaded
  Usage
    needsPackage PackageName
  Inputs
    PackageName:String
      the name of the package
    FileName=>String
      the name of the file containing the source code of the package, from which it should be loaded
    LoadDocumentation=>Boolean
      whether to load the documentation of the package, too; see @TO beginDocumentation@
    DebuggingMode=>Boolean
      the value of the variable @TO "debuggingMode"@ during loading the package; specifying it here overrides the
      value specified as an option to @TO newPackage@ by the package itself
    Configuration=>List
      the entries are options @TT "KEY => VALUE"@ overriding the defaults specified in the source code of
      the package and the (possibly updated) values in the file in the user's application directory.
    Reload=>Boolean
      whether to reload the package, if it has been loaded from a different source before
  Outputs
    :Package
      the package requested
  Consequences
    Item
      loads the package @TT "PackageName"@ by loading the file @TT "PackageName.m2"@ which should appear
      in one of the directories occuring in the list @TO "path"@, unless it has already been loaded, in
      which case it ensures that the package's dictionary of exported symbols occurs in @TO "dictionaryPath"@
      and are thus available to the user. In addition, the function @TO needsPackage@ is applied to each
      of the packages whose names are specified by the @TO [newPackage, PackageExports]@ option for the
      requested package. If the variable @TO "notify"@ is set to true, then an informational message is
      displayed after the file is loaded.
  Description
    Text
      For example, to load the sample package @TT "FirstPackage"@:
    Example
      notify = true
      needsPackage "FirstPackage"
      needsPackage "FirstPackage"
    Text
      The second time the package was not reloaded.
  SeeAlso
    "packages"
    dismiss
    loadPackage
    "packages provided with Macaulay2"

Node
  Key
     newPackage
    (newPackage, String)
    [newPackage, Authors]
    [newPackage, AuxiliaryFiles]
    [newPackage, CacheExampleOutput]
    [newPackage, Certification]
    [newPackage, Configuration]
    [newPackage, Date]
    [newPackage, DebuggingMode]
    [newPackage, Headline]
    [newPackage, HomePage]
    [newPackage, InfoDirSection]
    [newPackage, Keywords]
    [newPackage, OptionalComponentsPresent]
    [newPackage, PackageExports]
    [newPackage, PackageImports]
    [newPackage, Reload]
    [newPackage, UseCachedExampleOutput]
    [newPackage, Version]
    Name
    Email
    HomePage
  Headline
    the preamble of a package
  Usage
    newPackage ( PackageName, ... )
  Inputs
    PackageName:String
      the name of the new package
    Version=>String
      the version number of the package.
      A version number less than 1.0 indicates that the package is under development, and the user interface may change.
    Date=>String
      the date of this version of the package
    InfoDirSection=>String
      the title of the section in the info page directory where the menu entry for this package should be made
    Headline=>String
      a brief (5-10 words) description of the package
    Authors=>List
      each entry is a list of options the form
      @TT "Name => x"@, @TT "Email => x"@, or @TT "HomePage => x"@, where @TT "x"@ is a string.
    Keywords=>List
      the entries are keywords describing the package, used to classify the package in the list of @TO "packages provided with Macaulay2"@
    HomePage=>String
      the URI pointing to the home page of the package, if any
    DebuggingMode=>Boolean
      whether @TO "debuggingMode"@ should be true during package loading.  However, if @TO "debuggingMode"@
      is already @TT "false"@, it will remain so.
    AuxiliaryFiles=>Boolean
      whether the package source to be distributed includes a directory for
      auxiliary files, with the same name as the package
    PackageExports=>List
      the entries are names of other packages to load, both for the user and for the code of the new package
    PackageImports=>List
      the entries of names of other packages to load, just for the code of the new package
    CacheExampleOutput=>Boolean
      whether @TO installPackage@ should cache (newer) example output in a subdirectory of the auxiliary file directory
      named @TT "examples"@, for use in a future installation. This value can be overridden by a value explicitly specified
      when @TO installPackage@ is called. After the directory is created, it will necessary for the user also to specify
      @TT "AuxiliaryFiles => true"@.
    OptionalComponentsPresent=>Boolean
      whether all optional external components of the package are present on the system. Unless the user sets this
      option or @TT "CacheExampleOutput"@ to @TT "true"@, this option will be initialized to @TT "true"@.
    UseCachedExampleOutput=>Boolean
      whether @TO installPackage@ should copy previously cached example output, if it is present and
      corresponds to the current example input for a node, rather than rerunning the examples, which might
      be important if optional external software is not present in the system. This is relevant only when
      @TT "CacheExampleOutput"@ and @TT "AuxiliaryFiles"@ are set to @TT "true"@. Unless set by the user,
      it is set to the negation of the value of @TT "OptionalComponentsPresent"@.
    Certification=>List
      the certification block inserted by the maintainers of @EM "Macaulay2"@ after the package has been accepted
      for publication by a journal, such as The Journal of Software for Algebra and Geometry: @EM "Macaulay2"@.
      Authors should not undertake to create such a certification block themselves.
    Configuration=>List
      the entries are configuration options for the package. The keys and values should be constant expressions,
      such as strings and integers, not incorporating symbols to be exported by the package (and not yet defined).
      The first time the package is loaded by the user, unless the @TT "-q"@ option is specified on the @TT "M2"@ command
      line, these options will be stored in a file in the user's application directory (see @TO applicationDirectory@).
      The user can change the configuration by editing the file. The user can override the configuration settings when
      loading the package; see @TO [loadPackage, Configuration]@ and @TO [needsPackage, Configuration]@.
      The file will be overwritten when a newer version of the package with different configuration options is loaded,
      but a backup will be made and the user's settings for the surviving options will be retained.
    Reload=>Boolean
      whether to reload the package, if it has been loaded before
  Outputs
    :Package
      the new package
  Consequences
    Item
      a package is created
  Description
    Text
      The dictionaries for the symbols in the packages loaded by the user are moved out of the way to avoid conflicts, so
      just the standard pre-loaded packages are visible to the source code of the package.  In addition, the package
      @TO "SimpleDoc :: SimpleDoc"@ is made available. If functions from additional packages are needed by the code in
      the new package, then @TO needsPackage@ can be used (after the use of @TT "newPackage"@) to provide them.
      If functions from additional packages are needed by the user who will load the new package, then @TO needsPackage@
      can be used (before the use of @TT "newPackage"@) to provide them.
    Example
      newPackage("Foo",
          Version => "1.1",
          Headline => "making Foo",
          Configuration => { "foo" => 42, "bar" => "x" }
          )
      endPackage "Foo"
    Text
      The options can be recovered with @TO options@ as follows.
    Example
      opts = options Foo
      opts.Headline
    Text
      Here is a template for a typical @TT "newPackage"@ entry in a package.
    Code
      EXAMPLE { PRE ////newPackage("PackageName",
              Headline => "one line description",
              Version => "0.1",
              Date => "month XX, 20XX",
              Authors => {
                  {Name => "author1", Email => "email1", HomePage => "url1"},
                  {Name => "author2", Email => "email2", HomePage => "url2"}},
              DebuggingMode => false,
              HomePage => "http://univ.edu/~user/PackageName/",
              Configuration => {}
              )////}
  SeeAlso
    "packages"
    "creating a package"
    readPackage
    loadPackage
    needsPackage

Node
  Key
     endPackage
    (endPackage, String)
  Headline
    end a new package
  Usage
    endPackage pkgname
  Inputs
    pkgname:String
      the name of the package
  Outputs
    :Package
      the new package
  Consequences
    Item
      The package named should have been opened with @TO "newPackage"@.
      The package is closed, and the former value of @TO "dictionaryPath"@ is restored, with the global
      dictionary (containing the exported symbols) of the package prepended.
  Description
    Text
      A package writer need not call this function explicitly, since the end of file hook for the file
      containing the @TO "newPackage"@ command that opened the package will call @TO "endPackage"@.
    Example
      newPackage "Foo"
      export "abc"
      abc = 3
      dictionaryPath
      endPackage "Foo"
      peek oo
      dictionaryPath
      abc
    Text
      This function is called automatically at the end of a file, so package authors don't need to explicitly
      call @TT "endPackage"@.

Node
  Key
     installPackage
    (installPackage, String)
    (installPackage, Package)
    [installPackage, CacheExampleOutput]
    [installPackage, CheckDocumentation]
    [installPackage, DebuggingMode]
    [installPackage, FileName]
    [installPackage, IgnoreExampleErrors]
    [installPackage, InstallPrefix]
    [installPackage, MakeDocumentation]
    [installPackage, MakeHTML]
    [installPackage, MakeInfo]
    [installPackage, MakeLinks]
    [installPackage, RemakeAllDocumentation]
    [installPackage, RerunExamples]
    [installPackage, RunExamples]
    [installPackage, SeparateExec]
    [installPackage, UserMode]
    [installPackage, Verbose]
  Headline
    load and install a package and its documentation
  Usage
    installPackage PackageName
  Inputs
    PackageName:String
      or @TO Package@, the package to install
    FileName=>String
      the name of the file containing the source code of the package, from which it should be loaded
    UserMode=>Boolean
      if true, then do not give the @TT "-q"@ option to the Macaulay2 executable when running examples, thereby
      allowing it to load the user's @TO "initialization file"@, allowing it to load packages previously installed
      in the user's @TO2 {"applicationDirectory", "application directory"}@, and allowing packages it loads to read
      their configuration files from the the user's @TO2{"applicationDirectory", "application directory"}@.
      If false, then do give the option. If @TO "null"@, then propagate the option from the current @TO "commandLine"@,
      if one occurs there.
    DebuggingMode=>Boolean
      whether to enter @TO "the debugger"@ if an error occurs during installation;
      however, if @TO "debuggingMode"@ is already false, it will remain so.
    RerunExamples=>Boolean
      whether to rerun all the examples during installation
    RunExamples=>Boolean
      whether to run the examples during installation
    IgnoreExampleErrors=>Boolean
      whether to ignore errors that occur during the running of examples during installation
    CheckDocumentation=>Boolean
      whether to check the package's documentation for consistency and completeness
    MakeDocumentation=>Boolean
      whether to make the documentation for the package during installation
    MakeHTML=>Boolean
      whether to make HTML documentation. This is a form of the documentation that can be viewed using a browser.
    MakeInfo=>Boolean
      whether to make the info pages. This is a form of the documentation that can be viewed using the
      Unix command @TT "info"@ or using @TT "emacs"@
    InstallPrefix=>String
      the installation prefix for installation of the files of the package, in case encapsulation is not enabled,
      or for installation of the links to the files, in case encapsulation is enabled. The default value is the
      subdirectory named @TT "local"@ of the user's @TO "application directory"@.
    MakeLinks=>Boolean
      whether to make links to the files after installing them, in case encapsulation is enabled
    RemakeAllDocumentation=>Boolean
      whether to regenerate all of the help pages for this package. The default action is to rebuild only the html
      pages of the documentation entries that have been changed since the last time the package was installed.
      However, some changes to an entry, such as to its headline, will change the html of other pages that cross-reference it.
    CacheExampleOutput=>Boolean
      whether to cache (newer) example output in a subdirectory of the @TO2 {[newPackage, AuxiliaryFiles], "auxiliary file directory"}@
      named @TT "examples"@ for use in a future installation. This value, if set to true or false, will
      override any value explicitly specified when @TO newPackage@ is called. After the directory is created, it will
      be necessary for the user to specify @TT "AuxiliaryFiles => true"@ with the @TO newPackage@ command.
    SeparateExec=>Boolean
      whether to install the files of the package in two separate directory trees, one for the architecture
      independent files, and one for the architecture dependent files
    Verbose=>Boolean
      whether to display some details of the installation procedure. For even more information set @TO "debugLevel"@
      to a number greater than 0 or a number greater than 5.
  Outputs
    :Package
      the package that was installed
  Consequences
    Item
      The package is installed in a local directory, so that in the future, one may simply use @TO loadPackage@.
      Documentation for the package is also produced, running any Macaulay2 examples that are requested in the
      package documentation, with the random number seed initialized to 0.
  Description
    Text
      The main action of this routine is to generate the documentation of the given package and install the
      Macaulay2 package and documentation.

      The actual file loaded is @TT "PackageName.m2"@, which should be on the load @TO "path"@ and should contain a
      package named @TT "PackageName"@.

      In order to accomplish this, several steps are performed (or bypassed, depending on the values of the optional arguments).

      @UL {
          {"load the package, if not already loaded (see ", TO loadPackage, ")"},
          {"determine which help pages have changed since last install"},
          {"run any new or previously failed examples, or all examples, as specified by the ", TT "RerunExamples", " option"},
          {"generate the html pages, or not, as specified by the ", TT "MakeHTML", " option, for the modified help pages,
	      or all html pages if ", TT "RemakeAllDocumentation", " is set to true"},
          {"generate the info pages, or not, as specified by the ", TT "MakeInfo", " option"},
          {"install the documentation and package in the location specified by the ", TT "InstallPrefix", " option"},
          {"place a link to this html documentation in the
               file ", TT "index.html", " in the user's ", TO "application directory", "; see ", TO makePackageIndex}
          }@

      The current value of @TO "prefixPath"@ is used to determine how to direct documentation hyperlinks; the link
      will be directed to the appropriate file if one is found by searching the trees referred to by @TO "prefixPath"@.
      Otherwise, all documentation hyperlinks are relative to positions within a single tree of directories, as describe by @TO "Layout"@.

      It might be necessary to run @TO installPackage@ twice if a package with the same name is already installed:
      the second installation will redirect the hyperlinks to the freshly installed documentation, because the files will
      have been installed by the first installation.
      This applies, for example, to those authors who are developing updates to packages already included with Macaulay2.

      The files of the package are placed in subdirectories of the appropriate prefix directory as specified by @TO "Layout"@,
      depending on the value of the @TT "SeparateExec"@ option:

    Tree
      :Install paths given @TT "SeparateExec => false"@:
        @TT (Layout#1#"packages"|"PackageName.m2")@,
        @TT (Layout#1#"info"|"PackageName.info")@,
        @TT replace("PKG", "PackageName", Layout#1#"packagelib")@,
        @TT replace("PKG", "PackageName", Layout#1#"package")@, and
        @TT replace("PKG", "PackageName", Layout#1#"packagedoc")@;
      :Install paths given @TT "SeparateExec => true"@:
        @TT (Layout#2#"packages"|"PackageName.m2")@,
        @TT (Layout#2#"info"|"PackageName.info")@,
        @TT replace("PKG", "PackageName", Layout#2#"packagelib")@,
        @TT replace("PKG", "PackageName", Layout#2#"package")@, and
        @TT replace("PKG", "PackageName", Layout#2#"packagedoc")@.

    Text
      Note that in the latter case, the paths reflect the type of your machine.
      In addition, if no errors occurred during running the examples, then an empty file whose name is
      @TT (replace("PKG", "PackageName", Layout#1#"packagelib")|".installed")@ or
      @TT (replace("PKG", "PackageName", Layout#2#"packagelib")|".installed")@ is created,
      to signify that installation was completed.

  Caveat
    Links from html files containing documentation to documentation in another package not yet installed may go
    to the wrong place, because it is assumed that the package not yet installed will be installed under the same prefix.
    By contrast, if the other package has already been installed under some prefix occurring in the value of @TO "prefixPath"@,
    then the correct path will be used.  To get two packages installed under different prefixes which refer to each other's
    documentation correctly, it may be necessary to install one of them twice.
  SeeAlso
    "Layout"
    "packages"
    "prefixPath"
    installedPackages
    uninstallAllPackages
    uninstallPackage

Node
  Key
    installedPackages
  Usage
    installedPackages()
  Outputs
    :List
      the entries are strings containing the names of the packages that have been installed in the user's @TO "application directory"@.
  SeeAlso
    installPackage
    "loadedPackages"

Node
  Key
     uninstallPackage
    (uninstallPackage, String)
    (uninstallPackage, Package)
    [uninstallPackage, InstallPrefix]
  Headline
    uninstall a package and remove its documentation
  Usage
    uninstallPackage PackageName
  Inputs
    PackageName:Package
      or @TO String@, the package to uninstall
    InstallPrefix=>String
      see @TOH [installPackage, InstallPrefix]@
  Consequences
    Item
      the links to the files and the files of the specified package created by @TO installPackage@,
      in case encapsulation is enabled, are removed, for every version of the package.
  SeeAlso
    "packages"
    "prefixPath"
    installPackage
    installedPackages
    uninstallAllPackages

Node
  Key
    uninstallAllPackages
  Usage
    uninstallAllPackages()
  Consequences
    Item
      the packages that have been installed in the user's @TO "application directory"@ are uninstalled.
  SeeAlso
    installPackage
    installedPackages
    uninstallPackage

Node
  Key
     dismiss
    (dismiss, String)
    (dismiss, Package)
  Headline
    dismiss a package
  Usage
    dismiss pkg
  Inputs
    pkg:Package
  Consequences
    Item
      the dictionary of the package @TT "pkg"@ is removed from @TO "dictionaryPath"@, so the symbols
      exported by @TT "pkg"@ are no longer available for use.
  Description
    Text
      The package itself is still accessible under its name in the @TO "PackageDictionary"@,
      and its exported symbols can be made available again with @TO "use"@.
    Example
      newPackage "PKG"
      export "x1"
      x1=3
      endPackage "PKG"
      dictionaryPath
      x1
      dismiss PKG
      dictionaryPath
      x1
      values PackageDictionary
      use PKG
      x1
  SeeAlso
    loadPackage
    "dictionaryPath"
    "PackageDictionary"
    (use, Package)

Node
  Key
     makePackageIndex
    (makePackageIndex, List)
    (makePackageIndex, Sequence)
  Headline
    create an index of installed packages
  Usage
    makePackageIndex()
  Consequences
    Item
      Creates a file @TT "index.html"@ in the @TT "~/.Macaulay2"@ directory, containing links
      to the documentation for Macaulay2 and all installed packages.
  Description
    Text
      This command may need to be run after installing a package via @TO installPackage@.

      This command is run each time the program is started, updating the index file.
      Thus, one can simply restart Macaulay2 to obtain the same consequence.
  SeeAlso
    "packages"
///

document { Key => (options, Package),
    EXAMPLE lines ///
    options Core
    ///}
