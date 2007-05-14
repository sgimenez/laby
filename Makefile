.PHONY: default all clean

default: all

all:
	@./build --all

clean:
	@./build --clean

byte native byte-debug native-profile:
	@./build --$@

