\name{code}
\alias{code}
\alias{figure}
\alias{cache}
\alias{runcode}
\alias{article}
\alias{loadcache}
\alias{edit}
\alias{edit,codeObject-method}

\title{Retrieve code, figures, and cache databases}
\description{
  List available code chunks, figures, and cache databases and retrieve
  them
}
\usage{
code(name = NULL)
figure(name)
cache(name)
runcode(namevec, env = parent.frame(), useCache = TRUE)
loadcache(namevec, env = parent.frame())
article()
edit(name, ...)

\S4method{edit}{codeObject}(name, ...)
}

\arguments{
  \item{name}{the name of a code chunk/figure/cache database (character)
    or its sequence number}
  \item{namevec}{a vector of names of code chunks, or an object of class
  \code{"codeObject"}}
  \item{env}{environment in which to eevaluate code or load a cache
    database}
  \item{useCache}{logical, indicating whether a cache database should be
    used if available}
  \item{...}{other arguments passed to methods}
}
\details{
  When \code{code} is called without arguments, the list of available
  objects is printed.  Otherwise, the corresponding object is returned
  by the function.

  \code{figure} views a figure associated with a code chunk (if there is
  one).  Currently, this just loads the figure in the PDF viewer.  In
  particular, the code which produced the figure is not run.

  \code{cache} returns the database object associated with a code chunk
  if a cache database exists for that chunk.

  \code{runcode} takes a code chunk name or a sequence of code chunk
  names and evaluates the code associated with that code chunk.  If a
  code chunk has a cache database associated with it, then the code for
  that code chunk is \emph{not} run and the objects from the cache
  database are lazy-loaded instead. If \code{namevec} is an object of
  class \code{"codeObject"}, then \code{runcode} executes the code in
  that object.

  \code{article} simply loads the article text in its finished form
  (currently, just a PDF file).

  \code{edit} allows the user to edit the code associated with a code
  chunk in an external editor.  Once editing is finished, \code{edit}
  returns a modified \code{"codeObject"} object.
}
\value{
  \code{code} and \code{figure} both return an object of class
  \code{"codeObject"}.  \code{cache} returns an object inheriting from
  class \code{"filehash"}.  \code{edit} returns an object of class
  \code{"codeObject"}.
}

\author{Roger D. Peng \email{rpeng@jhsph.edu}}

\examples{
pkg <- system.file("SRP-ex", "srp_simple", package = "SRPM")
setPackage(pkg)

## Show annotated list of code chunks
code()

## Load cache from code chunk 2
loadcache(2)
ls()

## Make table from code chunk 3
runcode(3)

## Show Figure 1
\dontrun{
figure(1)
}
}


\keyword{utilities}


