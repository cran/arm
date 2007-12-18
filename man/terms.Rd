\name{terms.bayes}
\docType{genericFunction}
\alias{terms.bayes}
\alias{terms.bayes,formula-method}

\title{Construct a terms Object from a Formula}

\description{
  This function takes a formula and some optional arguments and
  constructs a terms object. The terms object can then be used to
  construct a \code{\link{model.matrix.bayes}}.
}
\usage{
\S4method{terms.bayes}{formula}(x, specials = NULL, abb = NULL, data = NULL, 
    neg.out = TRUE, keep.order = FALSE, simplify = FALSE, 
    allowDotAsName = FALSE, \dots)
}

\arguments{
  \item{x}{a formula.}
  \item{specials}{which functions in the formula should be marked as
    special in the \code{terms} object.}
  \item{abb}{Not implemented in \R.}
  \item{data}{a data frame from which the meaning of the special symbol
    \code{.} can be inferred. It is unused if there is no \code{.} in
    the formula.}
  \item{neg.out}{Not implemented in \R.}
  \item{keep.order}{a logical value indicating whether the terms should
    keep their positions. If \code{FALSE} the terms are reordered so
    that main effects come first, followed by the interactions,
    all second-order, all third-order and so on.  Effects of a given
    order are kept in the order specified. Default is \code{FALSE}.}
  \item{simplify}{should the formula be expanded and simplified, the
    pre-1.7.0 behaviour?}
  \item{\dots}{further arguments passed to or from other methods.}
  \item{allowDotAsName}{normally \code{.} in a formula refers to the
    remaining variables contained in \code{data}.  Exceptionally,
    \code{.} can be treated as a name for non-standard uses of formulae.}
}

\details{
 This function is a revised version of \code{\link[stats]{terms.formula}} in 
 \code{stats} package.  It activates the option \code{keep.order}.  
 It is designed for the use of \code{\link{bayesglm}} 
 and \code{bayesglm.hierachical} where terms might need to keep unordered.
}

\author{Yu-Sung Su \email{ys463@columbia.edu}}

\seealso{
  \code{\link[stats]{terms}}, \code{\link[stats]{terms.object}}, 
  \code{\link{model.matrix.bayes}},
  \code{\link{bayesglm}}.
}

\keyword{models}
