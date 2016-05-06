.PHONY: run build dev-install package

run:
	electron .

build:
	elm-make elm/Pomoco.elm --output elm.js

dev-install:
	npm install
	elm-package install -y

package:
	./node_modules/.bin/electron-packager . Pomoco --platform=darwin --arch=x64 --version=0.33.0
