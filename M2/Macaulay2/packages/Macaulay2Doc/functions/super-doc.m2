document {
    Key => {
	super,
       (super, Matrix),
       (super, Module),
       (super,Vector)
    },
    Headline => "get the ambient module",
    TT "super M", " -- yields the module that the module ", TT "M", " is a submodule of.",
    BR{},
    TT "super f", " -- if ", TT "f", " is a map whose target is a submodule 
    of ", TT "M", ", yields the composite of ", TT "f", " with the inclusion into ", TT "M", ".",
    PARA{},
    SeeAlso => { "cover", "ambient" }}
