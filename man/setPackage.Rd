\name{setPackage}
\alias{setPackage}
\alias{currentPackage}

\title{Set and retrieve the currently registered package}
\description{
  Set and retrieve the currently registered package
}
\usage{
setPackage(name)
currentPackage()
}

\arguments{
  \item{name}{the name of the reproducible research package to be
    registered or a URL to a remote zip file}
}
\details{
  \code{setPackage} sets the currently registered package so that
  functions such as \code{code}, \code{figure}, and \code{cache} know
  where to look for code chunks, figures, and cache databases.
  \code{currentPackage} returns the currently registered database.

  If \code{name} is a URL to a remote zip file, the zip file is
  downloaded and an attempt is made to extract it.  Upon successful
  extraction, the package is registered as if it were a local package.
}
\value{
  For \code{currentPackage} the name of the current package is
  returned.
}

\keyword{utilities}

