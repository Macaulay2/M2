book = new HashTable from {
     "Joe" => "344-5567",
     "Sarah" => "567-4223",
     "John" => "322-1456"}
book#"Sarah"
book#?"Mary"
x = set {a,b,c,r,t}
peek x
x#?a
x#?4
x = new MutableHashTable;
x#"Joe" = "344-5567";
x#3 = {a,b,c};
x#{1,2} = 44;
x#3
x#?4
x
peek x
p=4;
x.p = 444;
x.p
x#?4
