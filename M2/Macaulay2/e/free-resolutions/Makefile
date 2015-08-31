OBJ = betti.o moninfo.o f4-monlookup.o res-f4-mem.o monhashtable.o res-gausser.o res-poly-ring.o res-f4.o res-schreyer-frame.o rank.o main.o 

CXXFLAGS +=  --std=c++11 -g -O0 -Wall

freeres : $(OBJ)
	$(CXX) $(OBJ) -lstdc++ -o freeres

clean:
	rm -rf *.o freeres
