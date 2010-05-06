
SnapRotate.o: SnapRotate.hs
	ghc --make SnapRotate.hs

clean:
	rm -f SnapRotate.o SnapRotate.hi
