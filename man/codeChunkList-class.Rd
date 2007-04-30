\name{codeChunkList-class}
\docType{class}
\alias{codeChunkList-class}
\alias{show,codeChunkList-method}

\title{Class "codeChunkList"}
\description{
  A vector of code chunk names
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"character"}}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{character}"}, from data part.
  Class \code{"\linkS4class{vector}"}, by class "character", distance 2.
}
\section{Methods}{
  \describe{
    \item{\code{show}:}{prints the code chunk names along with their
      sequence numbers and indicators as to whether there are figures
      [F] or cache databases [C] associated with the chunk}
  }
}

\keyword{classes}
