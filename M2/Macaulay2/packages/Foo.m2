newPackage ("Foo",
     Headline => "foo",
     AuxiliaryFiles => true,
     Reload => true,
     DebuggingMode => true
     )

beginDocumentation()

document {
     Key => Foo,
     ExampleFiles => {"data"},
     EXAMPLE lines ///
     	currentDirectory()
        get "data"
     ///
     }

f = () -> error "hi"
f()

doc ///
  Key
   "foo"
  ExampleFiles
   data
  Description
   Example
    get "data"
///