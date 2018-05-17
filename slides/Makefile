PANDOC		= pandoc
PREAMBLE_BEAMER = common/preamble-beamer.tex

PANDOC_BEAMER_FLAGS =				\
		-t beamer			\
		-H $(PREAMBLE_BEAMER)		\
		--latex-engine=xelatex		\
		--slide-level=2			\
		--variable=fontsize:14pt	\
		--listings			\
#		--filter ../common/columnfilter.py	\


SRC =						\
	tutorial.md			\

PDF      = $(SRC:.md=.pdf)

all: $(PDF)

.SUFFIXES:

%.pdf: src/%.md
	@/bin/echo -e "\e[1mPANDOC\e[21m $(basename $@)"
	@$(PANDOC) $(PANDOC_BEAMER_FLAGS) -o $@ $<


.PHONY: clean
clean:
	@rm -f $(PDF)
