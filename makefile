build:
	elm make src/Main.elm --output polar.js

deploy:
	rsync -a $$PWD/ charles@skyforest.xyz:code/polar/
