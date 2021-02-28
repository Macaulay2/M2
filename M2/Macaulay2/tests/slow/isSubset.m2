debuggingMode = false;
R=ZZ[a,b,c,d];
I=ideal "
d8-7cd6+15c2d4-bd6-10c3d2+10bcd4+c4-18bc2d2-6ad6-4b2d4+4bc3+27acd4+3b2cd2-33ac2d2+3b2c2+11abd4+7ac3+5b3d2-30abcd2-2b3c-2d6+15abc2+13cd4+9a2d4-3ab2d2-b4-21c2d2-27a2cd2+3ab2c-3bd4+7c3+13a2c2-6bcd2-13a2bd2-ab3+11bc2+14a2bc+14ad4+13b2d2-38acd2-5b2c-3a3d2+3a2b2+22ac2+7a3c-16abd2-5b3+18abc-d4+3a3b-4cd2-17a2d2+6c2+19a2c+10bd2-2bc+11a2b-7ad2-8b2+11ac+4a3+3ab+3d2-c+6a2-5b-1,
cd6-5c2d4-bd6+6c3d2+3bcd4-c4+3bc2d2+ad6+2b2d4-3bc3-8acd4-9b2cd2+15ac2d2-6ac3+6abcd2+3b3c-d6-8abc2+2cd4-2a2d4-ab2d2+3c2d2+11a2cd2+2ab2c+6bd4-5c3-9a2c2-12bcd2-bc2-7a2bc-3ad4-8b2d2+11acd2+10b2c+a3d2-11ac2-4a3c+abd2+2b3-4abc+4d4-7cd2+8a2d2+2ab2+c2-10a2c-12bd2+9bc-4a2b-ad2+7b2-2ac-3a3+2ab-4d2+4c-3a2+6b+2a+1,
c2d4-bd6-3c3d2+3bcd4+c4-3bc2d2+ad6+2b2d4+3bc3-2acd4-3b2cd2-3ac2d2+b2c2+5ac3-b3d2-6abcd2-b3c-d6+7abc2+2cd4-2a2d4-2c2d2+3a2cd2+2ab2c+7bd4+4c3+5a2c2-11bcd2+a2bd2+3bc2+5a2bc-ad4-6b2d2-2acd2+6ac2-6abd2+10abc+3d4-3cd2+3a2d2+2ab2+2a2c-9bd2+3bc+3a2b+b2+2ac+4ab-3d2-a2+2b+1,
c3d2-2bcd4-c4+3bc2d2+ad6+2b2d4-3bc3-3acd4+b2cd2+5ac2d2-b2c2-5abd4-5ac3-4b3d2+12abcd2+2b3c-d6-9abc2+2cd4-3a2d4+2ab2d2+b4+c2d2+7a2cd2-3ab2c+4bd4-3c3-7a2c2-3bcd2+10a2bd2+ab3-2bc2-10a2bc+ad4-9b2d2+2acd2+5b2c+2a3d2-3a2b2-7ac2-3a3c+2abd2+5b3-8abc+2d4-3a3b-5cd2+a2d2+ab2+c2-6a2c-6bd2+6bc-7a2b-2ad2+7b2+ac-a3+ab-d2+3c+3b+a1,
c4-3bc2d2+b2d4+3bc3+2acd4-6ac2d2+b2c2-2abd4+5ac3-b3d2-2b3c-d6+9abc2+5cd4+a2d4+ab2d2-8c2d2-8a2cd2+bd4+3c3+8a2c2-2bcd2+a2bd2+4bc2+6a2bc+4ad4-b2d2-13acd2-4b2c-a3d2+11ac2+4a3c-abd2-b3+6abc+d4-5cd2-7a2d2+3c2+11a2c+2bd2-2bc+4a2b-5ad2-3b2+6ac+3a3+2d2+4a2-3b-1,
bd6-5bcd4+6bc2d2-ad6-b2d4-bc3+4acd4+6b2cd2-3ac2d2-3b2c2-2abd4-2b3d2+4abcd2-b3c+d6-4abc2-4cd4+3a2d4+5ab2d2+b4+4c2d2-7a2cd2-4ab2c-5bd4-c3+a2c2+15bcd2+a2bd2-8bc2-a2bc+ad4+3b2d2-6b2c-3a3d2-3a2b2-2ac2+2a3c+4abd2+2b3-8abc-4d4+9cd2-4a2d2-4ab2-4c2+2a2c+9bd2+a4-9bc-a2b+3ad2-2b2-4ac+2a3-4ab+4d2-4c-4b-2a-1,
bcd4-3bc2d2-ad6-b2d4+bc3+4acd4+b2cd2-4ac2d2+2b2c2+3abd4+ac3+2b3d2-9abcd2-b3c+d6+6abc2-3cd4+3a2d4-3ab2d2-b4+c2d2-9a2cd2+2ab2c-4bd4+c3+4a2c2+5bcd2-4a2bd2+4bc2+7a2bc+ad4+7b2d2-3acd2-4b2c-2a3d2+3a2b2+5ac2+4a3c-abd2-4b3+4abc-3d4+a3b+5cd2-7a2d2+7a2c+8bd2-5bc+4a2b+2ad2-6b2+3a3-2ab+3d2-2c+3a2-4b-2a-1,
bc2d2-b2d4-bc3-acd4+b2cd2+2ac2d2-2b2c2+3abd4-ac3+2b3d2-abcd2+d6-4abc2-4cd4-2a2d4-4ab2d2+5c2d2+5a2cd2-4bd4-2c3-3a2c2+9bcd2-2a2bd2-6bc2-2a2bc-2ad4+5b2d2+9acd2-2b2c+4a3d2+a2b2-7ac2-3a3c+abd2-b3-6abc-2d4+6cd2+4a2d2-4c2-7a2c+5bd2-a4-6bc-a2b+2ad2-2b2-6ac-3a3-2ab+d2-2c-2a2-b1,
bc3-2b2cd2-ac2d2+2b2c2+2abd4+ac3+b3d2-4abcd2+4abc2+cd4-a2d4-2ab2d2-b4-3c2d2+a2cd2+3ab2c-bd4+2c3+2a2c2-bcd2-4a2bd2+4bc2+4a2bc+ad4+3b2d2-4acd2+2a3d2+3a2b2+5ac2-4abd2-3b3+6abc+a3b-cd2+a2d2+ab2+2c2+2a2c+2bd2-a4+4a2b-3ad2-3b2+3ac-a3+2ab+a2-b+a1,
b2d4-acd4-3b2cd2+3ac2d2+b2c2-abd4-ac3-b3d2+3abcd2+2b3c-2abc2+cd4-2c2d2+3a2cd2+2ab2c+bd4-4a2c2-5bcd2+a2bd2+3bc2-4a2bc-5b2d2+8b2c-2ac2-3a3c+2abd2+2b3+2abc-2cd2+2a2d2+2ab2+3c2-4a2c-4bd2+8bc-3a2b-2ad2+6b2+2ac-2a3+2ab+2c-a2+4b+2a1,
b2cd2-ac2d2-b2c2-abd4+ac3-b3d2+3abcd2-b3c+cd4+a2d4+3ab2d2+b4-2c2d2-3a2cd2-4ab2c+c3+3a2c2-bcd2-bc2-ad4-acd2-3b2c-2a3d2-3a2b2+3ac2+3a3c+3abd2+2b3-2abc-3ab2+4a2c+bd2+a4-bc+ad2-b2+2a3-2ab-c-2b-a1,
b2c2-ac3-b3d2+b3c+ab2d2+c2d2+a2cd2+ab2c+bd4-c3-2a2c2-5bcd2+3bc2-2a2bc-ad4-b2d2+2acd2+5b2c-ac2-a3c-2abd2+b3+2abc+d4-3cd2+2a2d2+ab2+2c2-2a2c-3bd2+7bc-a2b+ad2+3b2+ac-a3+2ab-3d2+3c-a2+3b+a+1,
b3d2-2abcd2-b3c+2abc2+a2d4-ab2d2-b4+c2d2-a2cd2+2ab2c-bd4-c3+a2c2+3bcd2-2a2bd2-2bc2+4a2bc-ad4+3b2d2-5b2c-2a3d2+3a2b2-ac2+a3c+4abd2-4b3-2abc+a3b+cd2-ab2-2c2+a2c+3bd2-7bc+a2b+2ad2-5b2-2ac+a3-4ab-c-2b-a1,
b3c-2abc2-ab2d2+2a2cd2+ab2c+c3-2a2c2+a2bd2-2a2bc-ad4-b2d2+acd2+3b2c-ac2-3a3c+abd2+b3+2abc+d4-2cd2+2a2d2+ab2-4a2c-3bd2+3bc-2a2b+ad2+3b2+ac-2a3+2ab-2d2+c-2a2+3b+a+1,
b4-3ab2c+a2c2+2a2bd2+2bc2-2a2bc-2b2d2-2acd2+2b2c-a3d2-3a2b2+2ac2+2a3c+2abd2+3b3+d4-a3b-3cd2-a2d2+2c2+3a2c-3bd2+a4+4bc-2a2b-ad2+3b2+3ac+2a3-d2+2c+a2+b1,
ad7-6acd5+10ac2d3-abd5-4ac3d+8abcd3-d7-9abc2d+5cd5-5a2d5-3ab2d3-6c2d3+18a2cd3+2bd5+c3d-15a2c2d-10bcd3+8a2bd3+3ab3d+6bc2d-14a2bcd+2ad5+b2d3-2acd3+5b2cd+6a3d3-a2b2d-4ac2d-12a3cd-8abd3-2b3d+8abcd+3d5-7a3bd-11cd3+4a2d3+6ab2d+9c2d-9a2cd-5bd3-a4d+12bcd-2a2bd-7ad3-b2d+10acd-4a3d+11abd-3d3+5cd+4bd+4ad+d1,
acd5-4ac2d3-abd5+3ac3d+2abcd3-d7+3abc2d+5cd5+a2d5+2ab2d3-7c2d3-6a2cd3-5ab2cd+2bd5+3c3d+8a2c2d-8bcd3-a2bd3-ab3d+5bc2d+2a2bcd+3ad5-13acd3+b2cd-a3d3+a2b2d+12ac2d+5a3cd+abd3+4abcd+3d5+a3bd-9cd3-7a2d3-5ab2d+6c2d+13a2cd-5bd3-a4d+8bcd+2a2bd-6ad3+11acd+5a3d+abd-2d3+3cd+7a2d+2bd+ad1
";
J=ideal "
a3c2d-a3bd3-2abc2d+2ab2d3-a2cd3+c3d+a4d3+a3b2d+a2c2d-a4cd-2ab3d-bc2d+a2bcd-2b2d3+a4bd+2acd3+2b2cd-a3d3-4a2b2d-2ac2d+a3cd+2b3d-2a5d+5a3bd-2ab2d-2c2d-a2cd+2bcd-a2bd+4b2d-2acd+2a3d-2abd1,
a3bd3-2a3bcd-2ab2d3-a2cd3+4ab2cd-a4d3-a3b2d+2a2c2d+2bcd3+a4cd+2a2bd3+2ab3d-4bc2d-a2bcd+ad5+a4bd-4acd3-2b2cd+a3d3-2a2b2d+3ac2d+a3cd-4abd3+a5d+4abcd-d5-5a3bd+4cd3-2a2d3+7ab2d-3c2d+4a2cd+2bd3-a4d-6bcd+2a2bd-b2d+a3d+2abd+2d3-4cd+3a2d-2bd-3ad-d1,
a3bcd-2ab2cd-a4d3-a3b2d-a2c2d+a4cd+2a2bd3+2ab3d+2bc2d-a2bcd+a4bd-2b2cd+2a3d3-2a2b2d-2a3cd-4abd3+2a5d+4abcd-6a3bd-a2d3+4ab2d+a2cd+2bd3-2a4d-2bcd+5a2bd-2b2d-2a3d+4abd+2a2d-4bd1,
a3b2d-a4cd-2ab3d+a2bcd-a4bd+2b2cd+a3d3+2a2b2d-ac2d+a3cd-4abcd-a3bd-3a2d3+c2d+3a2cd+a2bd+2ad3+a3d-2cd+a2d-2ad1,
a4d4-3a4cd2-3a2bd4+a4c2+9a2bcd2+b2d4-a4bd2-3a2bc2+2acd4-3b2cd2+2a4bc-a3d4+3a2b2d2-6ac2d2+b2c2+2a3cd2-6a2b2c+2abd4+2ac3-b3d2-2a5d2-6abcd2+2b3c-d6+3a5c+8a3bd2+4abc2+3cd4-10a3bc-6ab2d2-2c2d2+a5b-3a2cd2+5ab2c+2bd4+c3-a4d2-4a3b2+5a2c2-5bcd2+2a4c+4a2bd2+3ab3+3bc2+a6-7a2bc+3ad4-3b2d2-3a4b-11acd2+6b2c+2a3d2+9ac2-2a3c-9abd2+b3+2a5+14abc+3d4-8a3b-6cd2-2a2d2+8ab2+2c2+7a2c-5bd2+6bc+a2b-5ad2+3b2+9ac-a3+7ab-3d2+2c+a2+3b+2a+1,
a4cd2-a4c2-3a2bcd2-a4bd2+3a2bc2+b2cd2-a4bc-a3d4+3a2b2d2+2ac2d2-b2c2+2a3cd2+3a2b2c+2abd4-2ac3-b3d2+a5d2+a4b2-a3c2-6abcd2-b3c-2a5c-a3bd2-3a2b3-2cd4+5a3bc+a2d4-3ab2d2+b4+4c2d2+2ab2c-2c3+2a4d2-3a2c2+5bcd2-3a4c-6a2bd2-4bc2-a6+9a2bc-ad4+2a4b+5acd2-3b2c-a3d2-6ac2-a3c+2b3-2a5-2abc+5a3b+3cd2+ab2-3c2-3a2c+bd2-5bc+3a2b+2ad2-b2-4ac+a3-2ab-c-a2-2b-a1,
a4c2-a4bd2-3a2bc2+a4bc+3a2b2d2+b2c2-a3cd2-3a2b2c+2ac3-b3d2+a3c2+b3c+a5c+a3bd2-3a3bc-2ab2d2-2c2d2+a5b+a2cd2+ab2c+bd4+2c3-4a3b2+a2c2-2bcd2+a4c+3ab3+3bc2-2a2bc-b2d2+a4b-acd2+2b2c-3a2b2+2ac2-a3c-2abd2+b3+6abc-a3b+4ab2+c2-2bd2+3bc+2b2+ac+ab+b1,
a4bd2-a4bc-3a2b2d2-a3cd2+3a2b2c+b3d2-a5d2-a4b2+a3c2+4abcd2-b3c+3a3bd2+3a2b3-4abc2+a3bc+a2d4-ab2d2-b4-c2d2-4a2cd2-4ab2c-2bd4+c3+a4d2+a2c2+4bcd2+a4c-5a2bd2-bc2+a6+a2bc+5b2d2-4a4b+3acd2-5b2c-2a3d2+6a2b2-ac2+5a3c+4abd2-4b3-10abc-d4+a3b+3cd2-2a2d2-3ab2-c2+a2c+6bd2-a4-7bc+4a2b+2ad2-6b2-6ac+3a3-6ab+2d2-3c+a2-4b-3a-1,
a4bc-3a2b2c-a5d2-a3c2+b3c+a5c+3a3bd2+4abc2-3a3bc-ab2d2-a2cd2+ab2c-c3+a4d2+a2c2-2bcd2-2a2bd2+2bc2+a6-2a2bc+ad4-4a4b-2acd2+3b2c+3a2b2+3ac2-abd2+a5+4abc-3a3b-cd2-2a2d2+ab2+2c2+4a2c-a4+3bc+4a2b-2ad2+3ac+2ab+c+a2+a1,
a4b2-a5c-3a2b3+2a3bc+b4+3ab2c+a4d2-2a2c2-a4c-a2bd2-bc2-a6+a2bc-2b2d2+3a4b+2acd2+2b2c-a3d2-3a2b2-2ac2-a3c+3b3-a5+2a3b+a2d2+2ab2-2a2c-bd2+a4+2bc-2a2b+3b2-ac-a2+b1,
a5d3-2a5cd-4a3bd3+8a3bcd+3ab2d3-a5bd+3a2cd3-6ab2cd-a4d3+4a3b2d-6a2c2d-2bcd3+a4cd+3a2bd3-3ab3d+4bc2d-a6d-6a2bcd-2ad5-b2d3+6a4bd+4acd3+3b2cd-9a2b2d-2ac2d-2a3cd+4abd3+2b3d-2abcd+2d5+a3bd-5cd3+4a2d3-4ab2d+4c2d-7a2cd-5bd3+8bcd-6a2bd+5b2d-2a3d+abd-4d3+5cd+4bd+2ad+d1,
a5cd-4a3bcd-a5bd+3ab2cd-a4d3+4a3b2d+3a2c2d+a4cd+3a2bd3-3ab3d-2bc2d-6a2bcd-b2d3+a4bd-4acd3+3b2cd+a3d3-3a2b2d+4ac2d-a3cd+b3d+2a5d+4abcd+d5-7a3bd+2ab2d-c2d+5a2cd-3bd3+2bcd-3a2bd-3ad3+3b2d+5acd-2a3d+7abd-2d3-cd+2a2d+3bd+2ad+d1,
a5bd-4a3b2d-a4cd+3ab3d-a6d+6a2bcd+4a4bd-3b2cd+a3d3-3a2b2d-2ac2d-4a3cd-4abd3+6abcd-a3bd+2cd3+a2d3+4ab2d-2c2d-a2cd+2bd3-4bcd-ad3-b2d+2acd-2a3d+3abd-d3-cd-2bd+2ad+d1,
a6d2-a6c-5a4bd2+5a4bc+6a2b2d2-a6b+4a3cd2-6a2b2c-b3d2-a5d2+5a4b2-4a3c2-6abcd2+b3c+4a3bd2-6a2b3+6abc2-a7-4a3bc-3a2d4-3ab2d2+b4+c2d2+6a5b+3a2cd2+6ab2c+2bd4-c3-10a3b2-3a2c2-2bcd2-3a4c+6a2bd2+4ab3+bc2+4ad4-4b2d2+2a4b-6acd2+3b2c+6a3d2-9a2b2+3ac2-7a3c-12abd2+4b3+a5+16abc-d4-11a3b-cd2-2a2d2+13ab2+c2+3a2c-3bd2-4a4+3bc+6a2b-6ad2+4b2+6ac-a3+10ab+2d2+c+5a2+a-1,
a6c-5a4bc+6a2b2c-a5d2+4a3c2-b3c+a5c+4a3bd2-6abc2-4a3bc-3ab2d2-6a2cd2+3ab2c+c3+a4d2+6a2c2+4bcd2-a4c-3a2bd2-4bc2+a6+6a2bc+2ad4+b2d2-5a4b-3b2c+6a2b2-2ac2+5a3c-2abd2-b3+a5-4abc-2d4-4a3b+4cd2-4a2d2+3ab2-4c2+8a2c+3bd2-a4-7bc+6a2b-ad2-3b2-3ac+a3+2ab+4d2-5c+2a2-3b-a-1,
a6b-5a4b2-a5c+6a2b3+8a3bc-b4-9ab2c+a4d2-3a2c2-a4c-6a2bd2+3bc2-a6+6a2bc+3b2d2+3a4b+4acd2-3b2c-a3d2+3a2b2-4ac2-2a3c+4abd2-3b3-a5-6abc-d4+7a3b+2a2d2-7ab2+c2-5a2c+2bd2+a4-2bc-3a2b+ad2-3b2-ac-a3-6ab+d2+c-2a2-b+a1,
a7d-6a5bd+10a3b2d+5a4cd-4ab3d-a6d-12a2bcd+5a4bd+3b2cd-4a3d3-6a2b2d+3ac2d+6abd3+b3d+4a3bd-2cd3+6a2d3-6ab2d+c2d-6a2cd-4bd3+4a4d+6bcd-12a2bd-2ad3+4b2d+2acd-2a3d+3cd-4a2d+5bd+2ad1,
a8-7a6b+15a4b2+6a5c-10a2b3-20a3bc+b4+12ab2c-5a4d2+6a2c2+5a4c+12a2bd2-3bc2-a6-12a2bc-3b2d2+10a4b-6acd2+3b2c+4a3d2-18a2b2+6ac2-8a3c-6abd2+4b3+5a5+18abc+d4-16a3b+3a2d2+9ab2-2c2-4bd2+a4+4bc-9a2b-4ad2+6b2+8ac-7a3+14ab-2d2+c+4b+3a+1
";
K = I+J;
f = J_(numgens J - 1);
m = a*b*c^6
assert( m % leadTerm K == 0 )
assert( m % K != m )
s = select( flatten entries leadTerm K, n -> m % leadMonomial n == 0 )
assert( m % ideal s == 0 )
assert( f % K == 0 )
assert isSubset(J,K)
assert ( gens gb gens gb K == gens gb K )
