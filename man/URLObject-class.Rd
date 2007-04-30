\name{URLObject-class}
\docType{class}
\alias{URLObject-class}
\alias{show,URLObject-method}

\title{Class "URLObject"}
\description{
  A simple class for URLs
}
\section{Slots}{
	 \describe{
	   \item{\code{url}:}{character, a URL}
  }
}
\section{Methods}{
  \describe{
    \item{\code{show}:}{loads the URL into a web browser via the
      \code{browseURL} function}
  }
}

\examples{
showClass("URLObject")
}
\keyword{classes}
