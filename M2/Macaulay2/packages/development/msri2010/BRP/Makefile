#
# example working make file for use of the class Account
# files: source files:  account.cpp accountmain.cpp
#        header file:  account.h
# executable file: account
#

# first define target file : account 
#              dependencies are the object files that build the program

Buchberger: buchberger.o brp.o longTest1.o longTest2.o longTest3.o longTest4.o longTest5.o
	g++ -O3 -g  -o Buchberger buchberger.o brp.o longTest1.o longTest2.o longTest3.o longTest4.o longTest5.o 

# now define how each object file is a target and list dependencies and how
#  to build that object file if any dependencies change

brp.o: brp.cpp brp.h
	g++ -O3  -g  -c brp.cpp

longTest1.o: longTest1.cpp longTest1.h
	g++ -O3  -g  -c longTest1.cpp

longTest2.o: longTest2.cpp longTest2.h
	g++ -O3  -g  -c longTest2.cpp

longTest3.o: longTest3.cpp longTest3.h
	g++ -O3  -g  -c longTest3.cpp

longTest4.o: longTest4.cpp longTest4.h
	g++ -O3  -g  -c longTest4.cpp

longTest5.o: longTest5.cpp longTest5.h
	g++ -O3  -g  -c longTest5.cpp

buchberger.o: runner.cpp 
	g++ -O3  -g  -c runner.cpp -o buchberger.o

clean:
	rm Buchberger *.o



