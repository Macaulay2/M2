#include "brp.h"

map<int,BRP> testLong8Example() {
   map<int,BRP> G;
   G[0] = BRP(393216) + BRP(131072) + BRP(65536) + BRP(49152);
   G[1] = BRP(1011713) + BRP(2111) + BRP(4) + BRP(2);
   G[2] = BRP(393510) + BRP(262146) + BRP(2048) + BRP(200);
   G[3] = BRP(690184) + BRP(65704) + BRP(24576);
   G[4] = BRP(2048) + BRP(448) + BRP(12) + BRP(8);
   return G;
}
