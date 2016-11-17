
all: test

compile:
	elm-make --output=foursies.js src/Main.elm

run:
	@echo 'http://localhost:8000/index.html'
	elm-reactor

test: compile
	elm-test

setup:
	# sudo apt-get install nodejs-legacy
	# sudo npm install elm
	elm-package install -y
