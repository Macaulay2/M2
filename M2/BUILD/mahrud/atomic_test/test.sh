gcc -c test.c -o c11.o --std=c11
g++ -c test.cpp -o cxx14.o --std=c++14
g++ cxx14.o c11.o -pthread
./a.out
