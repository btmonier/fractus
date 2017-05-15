\name{fractus}
\alias{fractus}

\title{Generate fractals based on the algorithm described by Barnsely (1988)}

\description{
  This function will generate fractals by randomly assigning a point (i.e. a 
  seed) within a regular \eqn{n}-gon. Next, a point (\eqn{r}) is placed at
  a fractional distance from a randomly assigned polygonal vertex. This process
  is continued \eqn{i} times at the user's request. 
}

\usage{
fractus(n, r, shape, point, title = TRUE)
}
\arguments{
  \item{n}{The total number of points that will be placed on the image.}
  \item{r}{The fractional distance from either the seed or prior point in which 
    the next point in the sequence is placed.}
  \item{shape}{Defines the polygon shape (triangle, square, or pentagon).}
  \item{point}{Size of the points placed on the graph.}
  \item{title}{Add descriptive title to graph. Defaults to \code{TRUE}.}
}

\author{Brandon Monier, \email{brandon.monier@sdstate.edu} }

\examples{
  \code{fractus(15000, 0.5, "triangle", point = 0.5, title = TRUE)}
}

\references{
  Barnsley, Michael F. "Fractal modeling of real world images." 
  The science of fractal images. Springer New York, 1988. 219-242.
}

\keyword{fractals}