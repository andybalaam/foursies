
all: test

test:
	elm-test

setup:
	# sudo apt-get install nodejs-legacy
	# sudo npm install elm
	elm-package install
