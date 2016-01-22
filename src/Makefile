include ../Make.options

.PHONY: default
default:
	$(MMC) --make libmercury_inttypes

.PHONY: rebuild
rebuild:
	$(MMC) --rebuild libmercury_inttypes

.PHONY: install
install:
	$(MMC) --make libmercury_inttypes.install

tags: $(wildcard *.m)
	mtags $^

.PHONY: clean
clean:
	$(MMC) --make mercury_inttypes.clean

.PHONY: realclean
realclean:
	$(MMC) --make mercury_inttypes.realclean
	/bin/rm -rf Mercury
	/bin/rm -f $(wildcard *.err) $(wildcard *.mh)
	/bin/rm -f tags
