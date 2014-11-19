.PHONY: default all clean

include project.conf
PROJECT_ARCHIVE=$(PROJECT_NAME)-$(PROJECT_VERSION)

DESTDIR ?=
PREFIX ?= /usr

default: all

all:
	@./build --all

clean:
	@./build --clean

byte native byte-debug native-profile:
	@./build --$@

install:
	install -Dpm 0755 laby ${DESTDIR}${PREFIX}/bin/laby
	install -d   0644 ${DESTDIR}${PREFIX}/share/laby/
	cp -pr data/* ${DESTDIR}${PREFIX}/share/laby/
	install -Dpm 0644 data/tiles/ant-e.svg ${DESTDIR}${PREFIX}/share/icons/hicolor/scalable/apps/laby.svg
	install -Dpm 0644 laby.xpm ${DESTDIR}${PREFIX}/share/pixmaps/laby.xpm
	desktop-file-install laby.desktop --dir=${DESTDIR}${PREFIX}/share/applications
	install -Dpm 0644 laby.appdata.xml ${DESTDIR}${PREFIX}/share/appdata/laby.appdata.xml

dist:
	@mkdir _dist
	@git archive --prefix="$(PROJECT_ARCHIVE)/" HEAD \
		 | gzip >_dist/"$(PROJECT_ARCHIVE)".tar.gz
	@echo archive stored in "_dist/$(PROJECT_ARCHIVE).tar.gz"
