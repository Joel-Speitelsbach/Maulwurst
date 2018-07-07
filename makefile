
HC = ghc -static -outputdir=bin -threaded
SRC_Hs = *.hs
SRC_Elm = frontend/*.elm

all: bin/backend frontend/index.html

bin/backend: $(SRC_Hs)
	mkdir -p bin
	$(HC) -main-is Main backend.hs -o $@

frontend/index.html: $(SRC_Elm)
	cd frontend; elm-make Übersicht.elm

clean :
	rm -frv bin
