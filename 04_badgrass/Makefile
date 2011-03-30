# Various files used by the compilation process
TESTFILE=testdata.txt
MAIN=Main.hs

# Build the components
all:
	ghc -o main $(MAIN)

# Test them
test: all
	time ./main < $(TESTFILE)

clean:
	rm -f *.o *.hi *.out *~ main
