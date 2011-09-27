.PHONY: default all clean

include project.conf
PROJECT_ARCHIVE=$(PROJECT_NAME)-$(PROJECT_VERSION)

default: all

all:
	@./build --all

clean:
	@./build --clean

byte native byte-debug native-profile:
	@./build --$@

dist:
	@mkdir _dist
	@git archive --prefix="$(PROJECT_ARCHIVE)/" HEAD \
		 | gzip >_dist/"$(PROJECT_ARCHIVE)".tar.gz
	@echo archive stored in "_dist/$(PROJECT_ARCHIVE).tar.gz"
