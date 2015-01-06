all: index.html

index.html: EscapeFromRlyeh.elm
	elm-make EscapeFromRlyeh.elm --output=index.html
