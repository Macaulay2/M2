     assert( (pdim image matrix {{1,1}}) === 0 );
     assert( (pdim image matrix {{0}}) === 0 );
     assert( (pdim image matrix {{1,1},{0,0}}) === 0 );
     assert( (pdim image matrix {{4,4},{4,4}}) === 0 );
     assert( (pdim image matrix {{4,4},{4,4}}) === 0 );
     assert( (pdim image matrix {{4,3,3},{5,4,4}}) === 0 );
R = QQ[x,y]
     assert( (pdim image matrix {{1_R,1}}) === 0 );
     assert( (pdim image matrix {{0_R}}) === 0 );
     assert( (pdim image matrix {{1_R,1},{0,0}}) === 0 );

S = ZZ/101[x]
     assert( pdim coker map(S^1,S^1,1) === 0 )
