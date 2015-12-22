.PHONY: run build install

run:
	electron .

build:
	elm-make elm/Pomoco.elm

install:
	elm-package install -y
