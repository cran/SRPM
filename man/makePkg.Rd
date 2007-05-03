\name{makeSRP}
\alias{makeSRP}
\alias{makeWebpage}

\title{Make a Reproducible Research Package}
\description{
  Create a reproducible research package from a Sweave file
}
\usage{
makeSRP(pkg, SweaveFile = NULL, clean = TRUE)
makeWebpage(clear = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{name of the package to be created}
  \item{SweaveFile}{name of the Sweave file}
  \item{clean}{logical, delete any files created in the package
    generation process?}
  \item{clear}{logical, should the directory in which the webpages will
    be written be removed if it already exists?}
}
\details{
  \code{makeSRP} reads the data map file created by the
  \pkg{cacheSweave} package and uses the information to generate a
  "reproducible research package" (RRP).  Any cached computation
  databases are copied into the package as well as any figures and the
  article itself.  The figures and the article should be in PDF format.

  After creating a reproducible research package with \code{makeSRP},
  one can register the package with \code{setPackage} and use the
  \code{makeWebpage} function to construct a rudimentary web interface
  to the package.  \code{makeWebpage} places the HTML files in a
  directory called "html" under the main directory of the reproducible
  research package.

  If \code{SweaveFile} is \code{NULL} then it defaults to using
  \code{<pkg>.Rnw}.
}

\value{
  A list of objects of class \code{codeObject} corresponding to the code
  chunks in the Sweave file is returned (invisibly).
}
\author{Roger D. Peng \email{rpeng@jhsph.edu}}

\keyword{package}
