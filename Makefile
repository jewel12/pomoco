.PHONY: run build install

run:
	electron .

build:
	elm-make elm/Pomoco.elm

install:
	npm install
	elm-package install -y
