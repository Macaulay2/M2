R = ZZ[x,y, Degrees => {-1,-2}, 
          Repair => d -> -d,
          Adjust => d -> -d];
degree \ gens R
transpose vars R
