SRCDIR = src
BINDIR = bin
INSTALLDIR = $(HOME)/bin

$(BINDIR)/fnorm: $(SRCDIR)/main.hs
	ghc --make $^ -o $@

.PHONY: install

install:
	install $(BINDIR)/fnorm $(INSTALLDIR)
