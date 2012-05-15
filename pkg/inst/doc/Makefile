#$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/bertin/pkg/R/imagem.R $
#$Id: imagem.R 39 2011-09-19 19:39:42Z gsawitzki $
#$Revision: 39 $
#$Date: 2011-09-19 21:39:42 +0200 (Mon, 19 Sep 2011) $
#$Author: gsawitzki $

.PHONY: pdf clean

pdf:
	pdflatex bertinR || echo "## presumably a font error"
	pdflatex bertinR || echo "## presumably a font error"
	bibtex bertinR  || echo "## a bib error"
#	makeindex Sintro.nlo -s nomencl.ist -o Sintro.nls
	pdflatex bertinR || echo "## presumably a font error"
	pdflatex bertinR || echo "## presumably a font error"
	makeindex bertinR.idx -o bertinR.ind
	cp bertinR.ilg bertinR_ind.ilg
#	makeindex bertinR.rfx -o bertinR.rfd
#	cp bertinR.ilg bertinR_rfd.ilg
#	makeindex bertinR.rtx -o bertinR.rtd
#	cp bertinR.ilg bertinR_rtd.ilg
	pdflatex bertinR || echo "## presumably a font error"
	thumbpdf bertinR || echo "## failure in thumbpdf. Maybe cover page needs flattening."
	pdflatex bertinR || echo "## presumably a font error"
	grep  arning bertinR.log
	wc *.ilg
	echo "### vignette Makefile done"
	-rm -f *.aux *.dvi *.log *.bbl *.blg bertinR-*.pdf *.idx *.ilg *.ind *.toc *.tpt
	exit 0

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
