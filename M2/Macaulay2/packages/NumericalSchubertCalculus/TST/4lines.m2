needsPackage "NumericalSchubertCalculus"
root = playCheckers({1},{1},2,4)
resolveNode(root, {({1},random(FFF^4,FFF^4)), ({1},random(FFF^4,FFF^4))})
end
restart
load "NumericalSchubertCalculus/TST/4lines.m2"
debug NumericalSchubertCalculus
checkIncidenceSolution(node.FlagM*node.Solutions#0,{remaining'conditions'flags#0})
checkIncidenceSolution(node.FlagM*node.Solutions#0,{remaining'conditions'flags#1})
checkIncidenceSolution(node.FlagM*node.Solutions#0,{({1},node.FlagM),({1},ID)})
