\name{view}
\alias{view}
\alias{viewplot}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{View code objects}
\description{
  View code objects and their associated plots
}
\usage{
view(object, ...)
viewplot(object, ...)
}

\arguments{
  \item{object}{an object to be viewed}
  \item{\dots}{other arguments passed to methods}
}
\details{
  Currently, there are methods for objects of class
  \code{codeObject}. \code{view} loads the code associated with a code
  object in the pager and \code{viewplot} launches a PDF viewer to view
  the associated plot.
}
\value{
  \code{viewplot} returns its argument invisibly.
}

\keyword{methods}
