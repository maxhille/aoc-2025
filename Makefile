.PHONY: all

all: main.js elm-worker.js

main.js: app/*.elm days/*.elm
	elm make app/Main.elm --output=main.js

elm-worker.js: app/*.elm days/*.elm
	elm make app/Worker.elm --output=elm-worker.js
