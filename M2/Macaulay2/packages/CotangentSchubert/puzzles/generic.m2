-- *all* triangles
allTriangles = d -> try myget ("generic-"|toString d|".m2") else error "generic tiles not implemented for this value of d"
