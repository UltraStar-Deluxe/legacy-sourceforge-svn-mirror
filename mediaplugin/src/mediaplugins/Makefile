SUBDIRS = ffmpeg
PLUGIN_DIR = ../../../game/plugins/media
#LIB_EXT = .so

.PHONY: all clean distclean

all:
	@for dir in $(SUBDIRS); do \
	  $(MAKE) -C "$$dir" compile; \
	done

clean:
	@for dir in $(SUBDIRS); do \
	  $(MAKE) -C "$$dir" clean; \
	done

distclean:
	@for dir in $(SUBDIRS); do \
	  $(MAKE) -C "$$dir" distclean; \
	done

