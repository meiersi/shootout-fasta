GHC = ghc
CXXFLAGS = -std=c++0x -O3
# CXXFLAGS = -O3
GHCFLAGS = -O3 -fllvm -Wall -rtsopts
CFLAGS = -O3

all: hs cpp c

cpp: fastacpp

c: fastac

fastacpp: fasta.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

fastac: fasta.c
	$(CC) $(CFLAGS) -o $@ $^

hs: fastahs oldfastahs fastabuilderhs fastaword8hs fastastate

fastahs: Fasta.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

fastabuilderhs: FastaBuilder.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

fastaword8hs: FastaWord8.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

fastastate: FastaState.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

oldfastahs: OldFasta.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

clean:
	-rm -f fastahs fastacpp oldfastahs fastac
