live:
	elm-live --port 8001 src/Main.elm -- --output=index.js

clean:
	rm -f index.js

index.js: clean
	elm make --output=index.js src/Main.elm
