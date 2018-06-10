installPackage "ReactionNetworks"
viewHelp ReactionNetworks

-- these functions were already in ReactionNetworks;
viewHelp stoichiometricMatrix
viewHelp stoichiometricSubspace
viewHelp negativeUndirectedLaplacian --renamed
viewHelp reactantMatrix
viewHelp stoichSubspaceKer

viewHelp negativeLaplacian
viewHelp negativeWeightedLaplacian 
viewHelp reactionNetwork --added optional imputs

--ReactionNetworks/motifs-Michael.m2
viewHelp nSiteProcessiveModification
viewHelp nSiteDistributiveModification
viewHelp nSiteImmuneReaction
viewHelp nSiteSequestration
viewHelp nSiteDiffusion
viewHelp nSitePoreForming
viewHelp nSiteAutocatalytic

-- these functions were implemented by us
viewHelp stoichiometricConeKer
viewHelp reducedStoichiometricConeKer
viewHelp superDoublingSets
viewHelp preClusters
viewHelp clusters
viewHelp hasIsolation

--examples

N3 = twoSiteModificationG()
stoichiometricMatrix N3
stoichiometricConeKer N3
clusters N3
hasIsolation N3

-- N2 = oneSiteModificationA()
-- stoichiometricMatrix N2
-- stoichiometricConeKer N2
-- clusters N2
-- hasIsolation N2

N1 = reactionNetwork "A <--> B, B <--> C, C <--> A"
stoichiometricMatrix N1
stoichiometricConeKer N1
hasIsolation N1
clusters N1