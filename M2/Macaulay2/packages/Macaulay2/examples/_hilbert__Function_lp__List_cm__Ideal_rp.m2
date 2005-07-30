R = QQ[a..f, Degrees=>{6:{1,1}}];
I = ideal (a*b, c*d, e*f);
hilbertFunction({2,2}, I)
S = R/I;
basis({2,2},S)
R = QQ[a..f];
I = ideal (a*b, c*d, e*f);
hilbertFunction(2, I)
S = R/I;
basis(2,S)
