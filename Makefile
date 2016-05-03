# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: R. Schöne
RACR_RACKET_BIN   = $$HOME/git/2bt/racr/racr/racket-bin
RACR_LARCENY_BIN  = $$HOME/git/2bt/racr/racr/larceny-bin
RACKET_BUILD_DIR  = racket-bin
LARCENY_BUILD_DIR = larceny-bin
RDEPS := $(shell sed -e '1,/@sources:/d' -e '/^\#/d' dependencies.txt | while read l; do echo $(RACKET_BUILD_DIR)/mquat/$$l.ss; done)
LDEPS := $(shell sed -e '1,/@sources:/d' -e '/^\#/d' dependencies.txt | while read l; do echo $(LARCENY_BUILD_DIR)/mquat/$$l.sls; done)

.PHONY: larceny racket run sockets clean
# larceny builds everytime, so not included in default target
all: racket

racket: $(RDEPS)
	@rm -f $(RACKET_BUILD_DIR)/mquat/ilp.ss

larceny: $(LSRC)
	@mkdir -p $(LARCENY_BUILD_DIR)
	@cp compile-stale $(LARCENY_BUILD_DIR)/mquat
	@cd $(LARCENY_BUILD_DIR)/mquat && larceny --r6rs --path ..:$(RACR_LARCENY_BIN) --program compile-stale

$(RACKET_BUILD_DIR)/mquat/%.ss: %.scm
	@mkdir -p $(RACKET_BUILD_DIR)
	@rm -f $@
	plt-r6rs ++path $(RACR_RACKET_BIN) --install --collections $(RACKET_BUILD_DIR) $<

larceny-bin/mquat/%.sls: %.scm
	cp -a $< $@

sockets: SocketSend.class SocketReceive.class

%.class: %.java
	javac $<

ilp-noncached.scm: ilp.scm
	sed 's/(lambda (n/#f (lambda (n/g' $< > $@

clean:
	rm -rf $(RACKET_BUILD_DIR)/*
	rm -rf $(LARCENY_BUILD_DIR)/*

run: racket
	fab call_racket:cli.scm,ag | tee ast-output.txt | less
