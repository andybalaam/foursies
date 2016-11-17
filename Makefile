
all: test

compile:
	elm-make --output=foursies.js src/Main.elm

run:
	@echo 'http://localhost:8000/index.html'
	elm-reactor

test: compile
	elm-test

setup:
	sudo apt-get install nodejs-legacy
	sudo npm install -g elm@0.18
	sudo npm install -g elm-test@0.18
	elm-package install -y
