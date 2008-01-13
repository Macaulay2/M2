A = QQ[a..d];
B = A/(a^2,b^3);
C = B/(a*b*c,b*c*d, b^2);
presentation A
presentation B
presentation C
presentation(B,C)
presentation(A,C)
minimalPresentation C
