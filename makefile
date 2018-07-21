
HC = ghc -static -outputdir=bin -threaded
SRC_Hs = *.hs
SRC_Elm = frontend/*.elm

move : move/backend move/index

all: bin/backend frontend/index.html

move/backend: $(SRC_Hs)
	scp backend.hs makefile otto@37.221.194.181:/home/otto/Server
	mkdir -p move/
	touch $@

move/index: frontend/index.html
	scp -C frontend/index.html otto@37.221.194.181:/home/otto/Server/frontend
	mkdir -p move/
	touch $@

bin/backend: $(SRC_Hs)
	mkdir -p bin
	$(HC) -main-is Main backend.hs -o $@

frontend/index.html: $(SRC_Elm)
	cd frontend; elm-make frontend.elm

clean :
	rm -frv bin
