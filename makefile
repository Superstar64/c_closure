
sample.o: sample.c
	$(CC) -o$@ $< -Iinclude

sample.c: sample.lambda build
	cat $< | cabal -v0 run > $@

build:
	cabal build
.PHONY: build

