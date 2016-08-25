-- Methods for installing alternative interfaces provided in the alternatives folder.

export{
   "loadAlternative",
   "dropAlternatives"
}


loadAlternative = method()
loadAlternative String := name -> (
   if name == "lrs" then insertAlternatives(lrs)
)

dropAlternatives = method()
installMethod(dropAlternatives, o -> (alternative = new MutableHashTable))

insertAlternatives = method()
insertAlternatives MutableHashTable := newAlternatives -> (
   mergeMutableHashTables(alternative, newAlternatives)
)


mergeMutableHashTables = method()
mergeMutableHashTables(MutableHashTable, MutableHashTable) := (old, given) -> (
   for key in keys given do (
      if not old#?key then (
         old#key = given#key
      ) else if instance(old#key, MutableHashTable) and instance(given#key, MutableHashTable) then (
         mergeMutableHashTables(old#key, given#key);
      ) else if instance(old#key, MethodFunction) and instance(given#key, MethodFunction) then (
         -- Warning?
         old#key = given#key
      )
   )
)

