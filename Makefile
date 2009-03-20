.PHONY: default all clean

PROJECT_ARCHIVE=\
$(shell source project.conf; echo $${PROJECT_NAME}-$${PROJECT_VERSION})

default: all

all:
	@./build --all

clean:
	@./build --clean

byte native byte-debug native-profile:
	@./build --$@

dist:
	@rm -rf "_dist/$(PROJECT_ARCHIVE)"
	@mkdir -p "_dist/$(PROJECT_ARCHIVE)"
	@mtn list known | while read i; do \
	  [ -f "$$i" ] && \
	    cp -f --parents "$$i" "_dist/$(PROJECT_ARCHIVE)/"; \
	done
	@cp build.status "_dist/$(PROJECT_ARCHIVE)/"
	@cd _dist; tar czf $(PROJECT_ARCHIVE).tar.gz $(PROJECT_ARCHIVE)
	@echo archive stored in "_dist/$(PROJECT_ARCHIVE).tar.gz"
