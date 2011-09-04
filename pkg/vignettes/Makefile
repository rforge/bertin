# $Header: /u/math/j40/cvsroot/lectures/src/SIntro/bertinR/Makefile,v 1.20 2010/01/03 14:26:02 j40 Exp $
# bertinR
#
.PHONY: clean
clean:
	-rm -f *.aux *.dvi *.log *.bbl *.blg *.exa *.glo \
	*.gls *.idx *.ind *.ilg *.inf *.out *.toc *.tpt *.pdfsync \
	*.rfd *.rfx *.rtd *.rtx
	@for d in * ; do \
	 if test -d  "$${d}" ; then \
	 echo directory "$${d}"; \
	  if (test "$${d}" != CVS); then	  (cd "$${d}" && $(MAKE) $@;) fi;\
	  fi \
	done

pdf:
	pdflatex bertinR || echo "## presumably a font error"
	pdflatex bertinR || echo "## presumably a font error"
	bibtex bertinR  || echo "## a bib error"
#	makeindex Sintro.nlo -s nomencl.ist -o Sintro.nls
	pdflatex bertinR || echo "## presumably a font error"
	pdflatex bertinR || echo "## presumably a font error"
	makeindex bertinR.idx -o bertinR.ind
	cp bertinR.ilg bertinR_ind.ilg
	makeindex bertinR.rfx -o bertinR.rfd
	cp bertinR.ilg bertinR_rfd.ilg
	makeindex bertinR.rtx -o bertinR.rtd
	cp bertinR.ilg bertinR_rtd.ilg
	pdflatex bertinR || echo "## presumably a font error"
	thumbpdf bertinR || echo "## failure in thumbpdf. Maybe cover page needs flattening."
	pdflatex bertinR || echo "## presumably a font error"
	grep  arning bertinR.log
	wc *.ilg
