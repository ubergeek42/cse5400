# Various files used by the compilation process
TESTFILE=testdata.txt
#TESTFILE=badgras-209.inp
MAIN=Main.hs

# Build the components
all:
	ghc --make -o main $(MAIN)

# Test them
test: all
	time ./main < $(TESTFILE)

clean:
	rm -f *.o *.hi *.out *~ main
