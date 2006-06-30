t = 1..4
newClass(Array,t)
x = new HashTable from { a => 1, b => 2 }
z = newClass(ImmutableType,Vector,x)
parent z
new Thing of Thing from Thing := (A,B,c) -> (
       << "-- new " << A << " of " << B 
       << " from " << toString c << endl;
       c);
new ImmutableType of Vector from x
newClass(ImmutableType,Vector,x)
