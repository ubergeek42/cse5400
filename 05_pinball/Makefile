# Various files used by the compilation process
TESTFILE=input_30.txt
#TESTFILE=test_25r.txt
MAIN=Main.hs

# Build the components
all:
	ghc --make -o main $(MAIN)

# Test them
test: all
	time ./main < $(TESTFILE)

clean:
	rm -f *.o *.hi *.out *~ main
