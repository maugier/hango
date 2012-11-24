all: hango
	ghc -O3 --make $<

clean:
	rm -f hango *.hi
