-- oneSiteModificationA

oneSiteModificationA = method()
installMethod(oneSiteModificationA, () -> reactionNetwork {"S_0 + E <-->X","X -->S_1+E","S_1+F<-->Y","Y-->S_0+F"})

oneSiteModificationA List := A -> (
    Re1 :=  {"A1 + A2 <-->A3","A3 -->A4 + A2","A4 + A5<-->A6","A6 -->A1+A5"};
    for i from 1 to 6 do Re1 = apply(Re1, s -> replace(concatenate("A",toString(i)),A #(i-1),s));
    RA := reactionNetwork Re1;
    RA
    )

-- oneSiteModificationB

oneSiteModificationB = method()
installMethod(oneSiteModificationB, () -> reactionNetwork {"S_0 + E <--> X","X --> S_1+E", "S_1+E <--> Y", "Y --> S_0+E"})

oneSiteModificationB List := A -> (
    Re2 :=  {"A1 + A2 <--> A3","A3 --> A4+A2", "A4+A2 <--> A5", "A5 --> A1+A2"};
    for i from 1 to 5 do Re2 = apply(Re2, s -> replace(concatenate("A",toString(i)),A #(i-1),s));
    RB := reactionNetwork Re2;
    RB
    )

-- oneSiteModificationC

oneSiteModificationC = method()
installMethod(oneSiteModificationC, () -> reactionNetwork {"S_0 + E_k <--> X_k", "X_k --> S_1+E_k", "S_1+F <--> Y", "Y --> S_0+F"})
 
oneSiteModificationC List := A -> (
    Re3 := {"A1 + A2 <--> A3","A3 --> A4+A2", "A4+A5 <--> A6", "A6 --> A1+A5"};
    for i from 1 to 6 do Re3 = apply(Re3, s -> replace(concatenate("A",toString(i)),A #(i-1),s));
    R3 := reactionNetwork Re3;
    R3
    )

-- oneSiteModificationD

oneSiteModificationD = method()
installMethod(oneSiteModificationD, () -> reactionNetwork {"S_0 + E_1 <--> X_1", "X_1 --> S_1+E_1", "S_1 + F_1 <--> Y_1", "Y_1 --> S_0 + F_1", "S_0 + E_2 <--> X_2", "X_2 --> S_1 + E_2", "S_1 + F_2 <--> Y_2", "Y_2 --> S_0 + F_2"})
 
oneSiteModificationD List := A -> (
    Re3 := {"A0 + A1 <--> A2", "A2 --> A3+A1", "A3+A4 <--> A5", "A5 --> A0+A4", "A0 + A6 <--> A7", "A7 --> A3 + A6", "A3 + A8 <--> A9", "A9 --> A0 + A8" };
    for i from 0 to 9 do Re3 = apply(Re3, s -> replace(concatenate("A",toString(i)),A #(i),s));
    R3 := reactionNetwork Re3;
    R3
    )


-- twoSiteModificationE

twoSiteModificationE = method()
installMethod(twoSiteModificationE, () -> reactionNetwork {"S_0 + E_1 <--> X_1", "X_1 --> S_1+E_1", "S_1 + E_2 <--> X_2", "X_2 --> S_2 + E_2", "S_1 + F_1 <--> Y_1", "Y_1 --> S_0 + F_1", "S_2 + F_2 <--> Y_2", "Y_2 --> S_1 + F_2"})
    
twoSiteModificationE List := A -> (
    Re3 := {"A0 + A1 <--> A2", "A2 --> A3+A1", "A3+A4 <--> A5", "A5 --> A6+A4", "A3 + A7 <--> A8", "A8 --> A0 + A7", "A6 + A9 <--> A10", "A10 --> A3 + A9" };
    for i from 0 to 10 do Re3 = apply(Re3, s -> replace(concatenate("A",toString(10-i)),A #(10-i),s));
    R3 := reactionNetwork Re3;
    R3
    )

-- twoSiteModificationF

twoSiteModificationF = method()
installMethod(twoSiteModificationF, () -> reactionNetwork {"S_0 + E <--> X_1", "X_1 --> S_1 +E", "S_1 + E <--> X_2", "X_2 --> S_2 + E", "S_1 + F_1 <--> Y_1", "Y_1 --> S_0 + F_1", "S_2 + F_2 <--> Y_2", "Y_2 --> S_1 + F_2"})
    
twoSiteModificationF List := A -> (
    Re3 := {"A0 + A1 <--> A2", "A2 --> A3+A1", "A3+A1 <--> A4", "A4 --> A5+A1", "A3 + A6 <--> A7", "A7 --> A0 + A6", "A5 + A8 <--> A9", "A9 --> A3 + A8" };
    for i from 0 to 9 do Re3 = apply(Re3, s -> replace(concatenate("A",toString(9-i)),A #(9-i),s));
    R3 := reactionNetwork Re3;
    R3
    )

-- twoLayerCascadeL

twoLayerCascadeL = method()
installMethod(twoLayerCascadeL, () -> reactionNetwork {"S_0 + E <--> X_1", "X_1 --> S_1 + E", "P_0 + S_1 <--> X_2", "X_2 --> P_1 + S_1", "P_0 + E <--> X_3", "X_3 --> P_1 + E", "S_1 + F_1 <--> Y_1", "Y_1 --> S_0 + F_1", "P_1 + F_2 <--> Y_2", "Y_2 --> P_0 + F_2"})
    
twoLayerCascadeL List := A -> (
    Re3 := {"A0 + A1 <--> A2", "A2 --> A3+A1", "A4+A3 <--> A5", "A5 --> A6+A3", "A4 + A1 <--> A7", "A7 --> A6 + A1", "A3 + A8 <--> A9", "A9 --> A0 + A8", "A6 + A10 <--> A11", "A11 --> A4 + A10"};
    for i from 0 to 11 do Re3 = apply(Re3, s -> replace(concatenate("A",toString(11-i)),A #(11-i),s));
    R3 := reactionNetwork Re3;
    R3
    )

-- crossLinkingModelCelldeath


crossLinkingModelCelldeath = method()
installMethod(crossLinkingModelCelldeath, () -> reactionNetwork {"L + R <--> C_1","C_1 + R <--> C_2", "C_2 + R <--> C_3"})
    
twoLayerCascadeL List := A -> (
    Re3 := {"A0 + A1 <--> A2", "A2 + A1 <--> A3", "A3 + A1 <--> A4"};
    for i from 0 to 4 do Re3 = apply(Re3, s -> replace(concatenate("A",toString(4-i)),A #(4-i),s));
    R3 := reactionNetwork Re3;
    R3
    )
