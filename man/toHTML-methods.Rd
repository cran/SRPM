\name{toHTML}
\docType{methods}
\alias{toHTML}
\alias{toHTML-methods}
\alias{toHTML,localDB-method}
\alias{toHTML,ANY-method}
\alias{toHTML,function-method}
\alias{toHTML,data.frame-method}

\title{Methods for Function toHTML}

\description{
  \code{toHTML} is a generic function which takes R objects as input and
  writes HTML representations to a connection or file.
}

\section{Methods}{
  \describe{
    
    \item{x = "localDB"}{Writes a simple "index.html" file listing the
      keys in the database and for each key, a summary of the associated
      object is provided.}
    
    \item{x = "ANY"}{The default method, essentially writes out
      \code{str(x)} to a file.}
    
    \item{x = "function"}{This method prints out the entire function
      body.}
    
    \item{x = "data.frame"}{The data frame is written out using
      \code{write.table}.}
  }
}
\keyword{methods}
