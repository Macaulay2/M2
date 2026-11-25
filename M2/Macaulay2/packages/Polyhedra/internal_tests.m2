-- FIXME: this file is not loaded by Polyhedra/loadFile.m2, so the
-- `export {"test1"}` below and the test1 method are dead code -- test1 is not
-- actually part of the package.  Either add `load "./internal_tests.m2"` to
-- loadFile.m2 (together with a real TEST for test1) or delete this orphan file.
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
