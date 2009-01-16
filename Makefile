#
# Makefile (to byte-compile)
#
EMACS=emacs
#EMACS=/Applications/Emacs.app/Contents/MacOS/bin/emacs

.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -q -no-site-file -f batch-byte-compile $<

all: nt-macros.elc nt-readval.elc nt-string.elc nt-utf8.elc nt-bocu.elc nt-file.elc nt-english.elc \
	pdicv-core.elc pdicv-search.elc pdicv-eijiro.elc pdicv-mode.elc pdicviewer.elc

clean:
	rm -f *.elc *~

nt-macros.elc: nt-macros.el

nt-readval.elc: nt-readval.el

nt-string.elc: nt-string.el # requires nt-macros
	$(EMACS) -batch -q -no-site-file -l nt-macros.elc  -f batch-byte-compile $<

nt-utf8.elc: nt-utf8.el

nt-bocu.elc: nt-bocu.el # requires nt-utf8
	$(EMACS) -batch -q -no-site-file -l nt-utf8.elc  -f batch-byte-compile $<

nt-file.elc: nt-file.el

nt-english.elc: nt-english.el

pdicv-core.elc: pdicv-core.el # nt-macros.elc nt-readval.elc nt-string.elc nt-bocu.elc nt-file.elc nt-english.elc
	$(EMACS) -batch -q -no-site-file -l nt-macros.elc -l nt-readval.elc -l nt-string.elc -l nt-utf8.elc -l nt-bocu.elc -l nt-file.elc -l nt-english.elc  -f batch-byte-compile $<

## defface may cause warnings on several environment; ignore them.

pdicv-search.elc: pdicv-search.el # requires nt-macros, nt-bocu, pdicv-core
	$(EMACS) -batch -q -no-site-file -l nt-macros.elc -l nt-readval.elc -l nt-string.elc -l nt-utf8.elc -l nt-bocu.elc -l nt-file.elc -l nt-english.elc -l pdicv-core.elc  -f batch-byte-compile $<

pdicv-eijiro.elc: pdicv-eijiro.el # requires pdicv-search
	$(EMACS) -batch -q -no-site-file -l nt-macros.elc -l nt-readval.elc -l nt-string.elc -l nt-utf8.elc -l nt-bocu.elc -l nt-file.elc -l nt-english.elc -l pdicv-core.elc -l pdicv-search.elc  -f batch-byte-compile $<

pdicv-mode.elc: pdicv-mode.el  pdicv-core.elc pdicv-search.elc pdicv-eijiro.elc
	$(EMACS) -batch -q -no-site-file -l nt-macros.elc -l nt-readval.elc -l nt-string.elc -l nt-utf8.elc -l nt-bocu.elc -l nt-file.elc -l nt-english.elc -l pdicv-core.elc -l pdicv-search.elc -l pdicv-eijiro.elc  -f batch-byte-compile $<

pdicviewer.elc: pdicviewer.el  pdicv-core.elc pdicv-search.elc pdicv-eijiro.elc
	$(EMACS) -batch -q -no-site-file -l nt-macros.elc -l nt-readval.elc -l nt-string.elc -l nt-utf8.elc -l nt-bocu.elc -l nt-file.elc -l nt-english.elc -l pdicv-core.elc -l pdicv-search.elc -l pdicv-eijiro.elc -l pdicv-mode.elc  -f batch-byte-compile $<

