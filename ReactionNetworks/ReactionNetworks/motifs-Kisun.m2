export {"oneSiteModificationA", "oneSiteModificationB", "oneSiteModificationC", "oneSiteModificationD", "twoSiteModificationE", "twoSiteModificationF", "twoLayerCascadeL", "crossLinkingModelCelldeath"}
-- oneSiteModificationA

oneSiteModificationA = method()
installMethod(oneSiteModificationA,
    () -> reactionNetwork {"S_0 + E <-->X","X -->S_1+E","S_1+F<-->Y","Y-->S_0+F"})

-- oneSiteModificationB

oneSiteModificationB = method()
installMethod(oneSiteModificationB, () -> reactionNetwork {"S_0 + E <--> X","X --> S_1+E", "S_1+E <--> Y", "Y --> S_0+E"})

-- oneSiteModificationC

oneSiteModificationC = method()
installMethod(oneSiteModificationC, () -> reactionNetwork {"S_0 + E_k <--> X_k", "X_k --> S_1+E_k", "S_1+F <--> Y", "Y --> S_0+F"})

-- oneSiteModificationD

oneSiteModificationD = method()
installMethod(oneSiteModificationD, () -> reactionNetwork {"S_0 + E_1 <--> X_1", "X_1 --> S_1+E_1", "S_1 + F_1 <--> Y_1", "Y_1 --> S_0 + F_1", "S_0 + E_2 <--> X_2", "X_2 --> S_1 + E_2", "S_1 + F_2 <--> Y_2", "Y_2 --> S_0 + F_2"})


-- twoSiteModificationE

twoSiteModificationE = method()
installMethod(twoSiteModificationE, () -> reactionNetwork {"S_0 + E_1 <--> X_1", "X_1 --> S_1+E_1", "S_1 + E_2 <--> X_2", "X_2 --> S_2 + E_2", "S_1 + F_1 <--> Y_1", "Y_1 --> S_0 + F_1", "S_2 + F_2 <--> Y_2", "Y_2 --> S_1 + F_2"})

-- twoSiteModificationF

twoSiteModificationF = method()
installMethod(twoSiteModificationF, () -> reactionNetwork {"S_0 + E <--> X_1", "X_1 --> S_1 +E", "S_1 + E <--> X_2", "X_2 --> S_2 + E", "S_1 + F_1 <--> Y_1", "Y_1 --> S_0 + F_1", "S_2 + F_2 <--> Y_2", "Y_2 --> S_1 + F_2"})

-- twoLayerCascadeL

twoLayerCascadeL = method()
installMethod(twoLayerCascadeL, () -> reactionNetwork {"S_0 + E <--> X_1", "X_1 --> S_1 + E", "P_0 + S_1 <--> X_2", "X_2 --> P_1 + S_1", "P_0 + E <--> X_3", "X_3 --> P_1 + E", "S_1 + F_1 <--> Y_1", "Y_1 --> S_0 + F_1", "P_1 + F_2 <--> Y_2", "Y_2 --> P_0 + F_2"})

-- crossLinkingModelCelldeath


crossLinkingModelCelldeath = method()
installMethod(crossLinkingModelCelldeath, () -> reactionNetwork {"L + R <--> C_1","C_1 + R <--> C_2", "C_2 + R <--> C_3"})
