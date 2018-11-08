
HC = cd backend && ghc -O2 -outputdir=bin -dynamic -threaded -W -fno-warn-missing-signatures
SRC_Hs_All = backend/*/*.hs
SRC_Hs = $(filter-out backend/Data/Local.hs, $(SRC_Hs_All))
SRC_Elm = frontend/*.elm

build: backend/bin/backend frontend/index.html

move: move/backend move/index move/makefile

move/backend: $(SRC_Hs)
	scp $(SRC_Hs) otto@37.221.194.181:/home/otto/Server/backend
	mkdir -p move/
	touch $@

move/makefile: $(SRC_Hs)
	scp makefile otto@37.221.194.181:/home/otto/Server
	mkdir -p move/
	touch $@

move/index: frontend/index.html
	scp -C frontend/index.html otto@37.221.194.181:/home/otto/Server/frontend
	mkdir -p move/
	touch $@

backend/bin/backend: $(SRC_Hs)
	mkdir -p backend/bin
	$(HC) -main-is Main backend.hs -o ../$@

frontend/index.html: $(SRC_Elm)
	cd frontend; elm-make frontend.elm

clean :
	rm -frv bin
