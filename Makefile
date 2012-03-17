all: build install

build:
	./Setup.hs build

install:
	./Setup.hs install

clean:
	./Setup.hs clean