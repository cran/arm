\name{triangleplot}
\alias{triangleplot}

\title{Triangle Plot}

\description{
Function for making a triangle plot from a square matrix
}

\usage{
triangleplot (x, y=NULL, details=TRUE, 
            n.col.legend=5, cex.col=0.7, 
            cex.var=0.9, digits=1, color=FALSE)
}

\arguments{
  \item{x}{a square matrix.}
  \item{y}{a vector of names that corresponds to each element of the square matrix x.}
  \item{details}{show more than one digits correlaton values. Default 
              is TRUE.  FALSE is suggested to get readable output.}
  \item{n.col.legend}{number of legend for the color thermometer}
  \item{cex.col}{font size of the color thermometer.}
  \item{cex.var}{font size of the variable names.}
  \item{digits}{number of digits shown in the text of the color theromoeter.}
  \item{color}{color of the plot, default is FALSE, which uses gray scale.}
}

\details{
The function makes a triangle plot from a square matrix, e.g., the correlation plot, see 
\code{\link{corrplot}}.
}


\author{Yu-Sung Su \email{ys463@columbia.edu}
        }
        
\seealso{\code{\link{corrplot}}
}

\examples{
 x <- matrix(runif(1600, 0, 1), 40, 40)
 triangleplot(x)
}

\keyword{dplot}
