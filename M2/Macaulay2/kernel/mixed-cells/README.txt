To compile, go to src/ and run
g++ -O3 matrix.cpp

Then run
time ./a.out ../demicsExamples/cyclic10.dat
to test the program.

To profile
g++ -O3 matrix.cpp -pg
./a.out ../demicsExamples/cyclyc11.dat
gprof >gprof.output 