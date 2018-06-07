-- these functions were already in ReactionNetworks
viewHelp stoichiometricMatrix
viewHelp stoichiometricSubspace
viewHelp negativeUndirectedLaplacian

-- these functions were implemented by us
viewHelp stoichSubspaceKer
viewHelp stoichiometricConeKer
viewHelp reducedStoichiometricConeKer
viewHelp negativeLaplacian
viewHelp negativeWeightedLaplacian 
viewHelp superDoublingSets
viewHelp preClusters
viewHelp clusters
viewHelp hasIsolation

--examples
N1 = reactionNetwork "A <--> B, B <--> C, C <--> A"
N2 = oneSiteModificationA()
N3 = twoSiteModificationG()

stoichiometricMatrix N3
stoichiometricConeKer N3

negativeWeightedLaplacian N2
negativeLaplacian N2

hasIsolation N2
clusters N2

hasIsolation N3
clusters N3

hasIsolation N1
clusters N1
