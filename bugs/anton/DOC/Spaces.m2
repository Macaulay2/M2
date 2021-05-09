-- relates to #975
newPackage("Spaces", Headline=>"spaces")
export "Space"
Space = new Type of List
hilbertFunction Space := S -> 1
hilbertFunction (Space,ZZ) := (S,n) -> 1

beginDocumentation()
doc ///
Key 
  (hilbertFunction,Space,ZZ)
  (hilbertFunction,Space)
Headline
  viewHelp (hilbertFunction,Space) fails  
Usage 
  A = hilbertFunction S
  A = hilbertFunction (S,n)
///
