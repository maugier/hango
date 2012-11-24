all:
	ghc -O3 --make hango

clean:
	rm -f hango *.hi *.o
