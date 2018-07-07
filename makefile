
HC = ghc -static -outputdir=bin
SRC = *.hs makefile

backend: $(SRC)
	mkdir -p bin
#	$(HC) -main-is $@ $@ -o bin/$@
	$(HC) -main-is Main backend.hs -o bin/$@

clean :
	rm -frv bin
