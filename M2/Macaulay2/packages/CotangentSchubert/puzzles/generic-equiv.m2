-- *all* rhombi
allRhombi = d -> try myget ("generic-equiv-"|toString d|".m2") else error "generic equivariant tiles not implemented for this value of d"
