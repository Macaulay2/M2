protect targetProperty
protect sourceProperties

rules = new CacheTable

findFittingRule = method()
findFittingRule(HashTable, Symbol) := (HT, target) -> (
   allRules := keys rules;
   givenProperties := set flatten {keys HT, keys HT.cache};
   << allRules << endl;
   goodRules := select(allRules, rule -> rule#1 == target);
   bestRules := select(goodRules, rule -> rule#0 <= givenProperties);
   if #bestRules == 0 then error "No fitting rule found."
   else bestRules#0
)

applyFittingRule = method()
applyFittingRule(HashTable, Symbol) := (HT, target) -> (
   rule := findFittingRule(HT, target);
   rules#rule HT
)
