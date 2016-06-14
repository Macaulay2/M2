oneSiteModificationA = method()
installMethod(oneSiteModificationA, () -> reactionNetwork {"S_0 + E <-->X","X -->S_1+E","S_1+F<-->Y","Y-->S_0+F"})

oneSiteModificationA List := A -> (
    -- put species from A in place of default ones
    -- reactionNetwork {"S_0 + E <-->X","X -->S_1+E","S_1+F<-->Y","Y-->S_0+F"}
    )

-- oneSiteModificationB ...