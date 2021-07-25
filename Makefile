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
	@ for subpkg in intro simple gps; do \
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

quicklisp: quicklisp/setup.lisp

quicklisp/setup.lisp: lib/quicklisp.lisp
	@ mkdir -p $(@D)
	@ clisp -ansi -i $< -norc \
		-x '(quicklisp-quickstart:install :path "quicklisp/")'

install-deps: quicklisp/setup.lisp
	@ clisp -ansi -i $< -norc \
		-x '(ql:quickload "alexandria")' \
		-x '(ql:quickload "check-it")' \
		-x '(ql:quickload "clunit")'
