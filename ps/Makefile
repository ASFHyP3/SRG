CC=g++ -std=c++11 -Wall
#PYTHON_LIB=`python3-config --includes` 
#PYTHON_EXT=`python3-config --extension-suffix`

default: All

sario.o: sario.hpp sario.cpp
	$(CC) -c sario.cpp

All: sario.o psinterp.cpp
	$(CC) -fopenmp -O3 -fPIC -c psinterp.cpp
	$(CC) -fopenmp -O3 -fPIC psinterp.o sario.o -o psinterp
clean:
	rm -f *.o psinterp
