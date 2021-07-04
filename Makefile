cpif   ?= | cpif
tangle  = notangle -R'$@' $< ${cpif} $@
weave   = noweave -autodefs lisp -n -delay -index $< ${cpif} $@

latexmk_flags  = -file-line-error -outdir=tex -pdf -xelatex -shell-escape -synctex=1
ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags += -gg
endif

.SUFFIXES: .tex .pdf
.tex.pdf:
	latexmk $(latexmk_flags) $<

all: \
	paip.asd \
	init.lisp \
	$(wildcard src/*.lisp) \
	bin/runtests \
	tex/paip.pdf

check: bin/runtests all
	@ echo
	@ for subpkg in intro gps; do \
		echo "#:paip.$$subpkg" ; \
		$< $$subpkg ; \
	done

paip.asd init.lisp src/*.lisp: paip.nw
	${tangle}

bin/runtests: paip.nw
	${tangle}
	chmod +x $@

tex/paip.tex: export FINDUSES_LISP=1
tex/paip.tex: paip.nw
	${weave}

quicklisp:
	@ sbcl --noinform --non-interactive --no-userinit \
		--load lib/quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "quicklisp/")'
	@ sbcl --noinform --non-interactive --no-userinit \
		--load quicklisp/setup.lisp \
		--eval '(ql:quickload "lisp-unit")'
