.PHONY: default all clean

include project.conf
PROJECT_ARCHIVE=$(PROJECT_NAME)-$(PROJECT_VERSION)

DESTDIR ?=
PREFIX ?= /usr
BINDIR ?= ${PREFIX}/bin
DATADIR ?= ${PREFIX}/share

default: all

all:
	@./build --all

clean:
	@./build --clean

byte native byte-debug native-profile:
	@./build --$@

install:
	install -Dp --mode=0755 laby \
		"${DESTDIR}${BINDIR}/laby"
	install -d "${DESTDIR}${DATADIR}/laby/"
	cp -pr data/* "${DESTDIR}${DATADIR}/laby/"
	install -Dp --mode=0644 data/tiles/ant-e.svg \
		"${DESTDIR}${DATADIR}/icons/hicolor/scalable/apps/laby.svg"
	desktop-file-install packaging/laby.desktop \
		--dir="${DESTDIR}${DATADIR}/applications"
	install -Dp --mode=0644 packaging/laby.appdata.xml \
		"${DESTDIR}${DATADIR}/appdata/laby.appdata.xml"

dist:
	@mkdir _dist
	@git archive --prefix="$(PROJECT_ARCHIVE)/" HEAD \
		 | gzip >"_dist/$(PROJECT_ARCHIVE).tar.gz"
	@echo archive stored in "_dist/$(PROJECT_ARCHIVE).tar.gz"
