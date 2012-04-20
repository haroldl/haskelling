all: Main doc/index.html

Main: *.hs
	ghc --make Main

doc/index.html: LogFormat.hs
	haddock -h -o doc $<

clean:
	-rm -rf doc Main *.hi *.o