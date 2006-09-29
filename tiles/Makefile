%.xpm: %.png
	convert $< $@

%.png: %.svg
	inkscape -e $@ -C -w 50 -h 50 $<