Qu = new Type of List
w = new Qu from {1,-2,0,4}
expression Qu := z -> (
               expression z#0 +
               expression z#1 * expression "I" +
               expression z#2 * expression "J" +
               expression z#3 * expression "K");
net Qu := z -> net expression z;
toString Qu := z -> toString expression z;
tex Qu := z -> tex expression z;
html Qu := z -> html expression z;
w
toString w
tex w
html w
I = new Qu from {0,1,0,0}
J = new Qu from {0,0,1,0}
K = new Qu from {0,0,0,1}
2*I + 5*J
peek oo
