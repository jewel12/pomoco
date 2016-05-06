.PHONY: run build install

run:
	electron .

build:
	elm-make elm/Pomoco.elm --output elm.js

install:
	npm install
	elm-package install -y
