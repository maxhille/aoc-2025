.PHONY: all

all: main.js elm-worker.js

main.js: app/*.elm 
	elm make app/Main.elm --output=main.js

elm-worker.js: app/*.elm 
	elm make app/Worker.elm --output=elm-worker.js
