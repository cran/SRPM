\name{codeObject-class}
\docType{class}
\alias{codeObject-class}
\alias{show,codeObject-method}
\alias{viewplot,codeObject-method}
\alias{view,codeObject-method}

\title{Class "codeObject"}
\description{
  This class essentially represents a code chunk in a Sweave document.
  If the results of the code are cached, the cache database is stored,
  as are any figures that are created.
}
\section{Slots}{
  \describe{
    \item{\code{text}:}{Object of class \code{"character"} ~~ }
    \item{\code{db}:}{Object of class \code{"filehashOrNULL"} ~~ }
    \item{\code{fig}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{show}:}{prints the code corresponding to the code chunk,
      including comments, if any}
    \item{\code{view}:}{loads code in the pager}
    \item{\code{viewplot}:}{if there is a figure associated with this
      code chunk, the figure is loaded in the PDF viewer (only works for
      PDF figures)}
  }
}

\keyword{classes}
