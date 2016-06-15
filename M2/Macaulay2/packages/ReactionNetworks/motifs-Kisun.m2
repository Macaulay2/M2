oneSiteModificationA = method()
installMethod(oneSiteModificationA, () -> reactionNetwork {"S_0 + E <-->X","X -->S_1+E","S_1+F<-->Y","Y-->S_0+F"})

oneSiteModificationA List := A -> (
    Re :=  {"A1 + A2 <-->A3","A3 -->A4 + A2","A4 + A5<-->A6","A6 -->A1+A5"};
    for i from 1 to 6 do Re = apply(Re, s -> replace(concatenate("A",toString(i)),A #(i-1),s));
    RS := reactionNetwork Re;
    RS
    )

-- oneSiteModificationB ...

A'= {"A", "B", "C", "D", "E", "F"} 
oneSiteModificationA(A')
