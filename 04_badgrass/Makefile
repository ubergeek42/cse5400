# Various files used by the compilation process
#TESTFILE=testdata.txt
#TESTFILE=test50.txt
#TESTFILE=test75.txt
TESTFILE=badgras-209.inp
MAIN=Main.hs

# Build the components
all:
	ghc --make -prof -auto-all -o main $(MAIN)

# Test them
test: all
	time ./main +RTS -p -RTS < $(TESTFILE)

clean:
	rm -f *.o *.hi *.out *~ main
