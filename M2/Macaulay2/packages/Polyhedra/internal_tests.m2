export {
   "test1"
}

test1 = method()
test1 ZZ := a ->(
   CT := new CacheTable;
   CT.rays = matrix {{1},{2}};
   CT.computedLinealityBasis = matrix{{0},{-1}};
   H := new HashTable from {symbol cache => CT};
   C := new Cone from H;
   dim C
)
