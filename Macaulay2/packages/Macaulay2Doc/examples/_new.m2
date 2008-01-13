new Type of BasicList from Function := (A,B,f) -> hashTable { net => f };
f = s -> "--list of type X--"
X = new Type of List from f
class X
parent X
peek X
x = new X from {1,3,11,12}
class x
parent x
peek x

               new Type of BasicList := (type,array) -> (
                    stderr << "--new " << type << " of " 
                           << array << " being made" << endl;
                    new MutableHashTable)
          
M = new Type of BasicList
m = new M from {3,4,5}
class m
m#1
- M := reverse
- m
new M from ZZ := (M',i) -> 0 .. i
n = new M from 13
- n
new M := (M') -> {a,b,c}
new M
