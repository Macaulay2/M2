protect targetProperty
protect sourceProperties

rules = new CacheTable

findFittingRule = method()
findFittingRule(HashTable, Set) := (HT, targets) -> (
   allRules := keys rules;
   << allRules << endl;
   givenProperties := set flatten {keys HT, keys HT.cache};
   goodRules := select(allRules, rule -> targets <= rule#1);
   bestRules := select(goodRules, rule -> rule#0 <= givenProperties);
   if #bestRules == 0 then error "No fitting rule found."
   else bestRules#0
)

applyFittingRule = method()
applyFittingRule(HashTable, Set) := (HT, targets) -> (
   << "Hello." << instance(targets, Set) <<  endl;
   rule := findFittingRule(HT, targets);
   rules#rule HT
)

applyFittingRule(HashTable, Symbol) := (HT, target) -> (
   applyFittingRule(HT, set {target});
   << "Computation done." << endl;
   HT.cache.target
)
