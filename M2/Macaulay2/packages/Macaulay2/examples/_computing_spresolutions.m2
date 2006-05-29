R = QQ[x..z];
M = cokernel vars R
C = res M
R = ZZ/2[a..d];
M = coker random(R^4, R^{5:-3,6:-4});
(<< "-- computation started: " << endl;
 while true do try (
     alarm 3;
     time res M;
     alarm 0;
     << "-- computation complete" << endl;
     status M.cache.resolution;
     << res M << endl << endl;
     break;
     ) else (
     << "-- computation interrupted" << endl;
     status M.cache.resolution;
     << "-- continuing the computation" << endl;
     ))
