% -*- coding: utf-8 -*-
\newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}
%\newcommand{\Rlogo}{\if{html}{\figure{../../html/logo.jpg}}\if{latex}{\figure{logo.jpg}{options: width=0.5in}}}
\newcommand{\Rlogo}{\if{html}{\figure{logo.jpg}}\if{latex}{\figure{logo.jpg}{options: width=0.5in}}}
\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{Bertin}
\title{Bertin}
\encoding{UTF-8}

\section{CHANGES IN VERSION 1.1-2}{

\subsection{ CHANGES IN Bertin NEWS in inst}{
\itemize{
\item under construction in int pkg
}
}
}
\section{CHANGES IN VERSION 1.1-2}{
  \subsection{BUG FIX}{
    \itemize{
      \item \code{discretize()} would return wrong results when argument
      \code{step} was omitted in favor of \code{by} \emph{and} the
      discretization method \code{unbiased} was used. (Thanks to
      Marie-Pier Côté \email{marie-pier.cote.11@ulaval.ca} for the
      catch.)
    }
  }
}

\section{CHANGES IN VERSION 1.1-1}{
  \subsection{USER-VISIBLE CHANGES}{
    \itemize{
      \item CITATION file updated.
    }
  }

  \subsection{BUG FIX}{
    \itemize{
      \item \code{summary.cm()} could skip records in the output
      thinking they were duplicates.
    }
  }
}

\section{CHANGES IN VERSION 1.1-0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New argument \code{convolve} in \code{aggregateDist()} to
      convolve the distribution obtained with the recursive method a
      number of times with itself. This is used for large portfolios
      where the expected number of claims is so large that recursions
      cannot start. Dividing the frequency parameter by \eqn{2^n} and
      convolving \eqn{n} times can solve the problem.

      \item New method of \code{diff()} for \code{"aggregateDist"}
      objects to return the probability mass function at the knots of
      the aggregate distribution. Valid (and defined) for
      \code{"recursive"}, \code{"exact"} and \code{"simulation"} methods
      only.

      \item Since the terminology Tail Value-at-Risk is often used
      instead of Conditional Tail Expectation, \code{TVaR()} is now an
      alias for \code{CTE()}.
    }
  }