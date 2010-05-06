
snaprotate.o: snaprotate.hs
	ghc --make snaprotate.hs

clean:
	rm -f snaprotate.o snaprotate.hi
