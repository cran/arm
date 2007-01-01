\name{bayesglm}

\alias{bayesglm}
\alias{bayesglm.fit}

\title{Bayesian generalized linear models.}

\description{Bayesian functions for generalized linear modeling 
        with independent normal, t, or Cauchy prior distribution 
        for the coefficients.}
        
\usage{
bayesglm (formula, family = gaussian, data, 
    weights, subset, na.action, 
    start = NULL, etastart, mustart, 
    offset, control = glm.control(...), 
    model = TRUE, method = "glm.fit", 
    x = FALSE, y = TRUE, contrasts = NULL, 
    prior.mean = 0, prior.scale = 2.5, 
    prior.scale.for.intercept = 10, prior.df = 1, 
    scaled = TRUE, n.iter = 50, ...)
    
bayesglm.fit (x, y, weights = rep(1, nobs), 
    start = NULL, etastart = NULL, 
    mustart = NULL, offset = rep(0, nobs), family = gaussian(), 
    control = glm.control(), intercept = TRUE,
    prior.mean=0, prior.scale=2.5, prior.scale.for.intercept=10,
    prior.df=1, scaled=TRUE)
}

\arguments{
  \item{formula}{a symbolic description of the model to be fit.
    The details of model specification are given below.}

  \item{family}{a description of the error distribution and link
    function to be used in the model. This can be a character string
    naming a family function, a family function or the result of a call
    to a family function.  (See \code{\link{family}} for details of
    family functions.)}

  \item{data}{an optional data frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing
    the variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)},
    typically the environment from which \code{glm} is called.}

  \item{weights}{an optional vector of weights to be used in the fitting
    process.  Should be \code{NULL} or a numeric vector.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \dQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Another possible value is
    \code{NULL}, no action.  Value \code{\link{na.exclude}} can be useful.}

  \item{start}{starting values for the parameters in the linear predictor.}

  \item{etastart}{starting values for the linear predictor.}

  \item{mustart}{starting values for the vector of means.}

  \item{offset}{this can be used to specify an \emph{a priori}
    known component to be included in the linear predictor
    during fitting.  This should be \code{NULL} or a numeric vector of
    length either one or equal to the number of cases.
    One or more \code{\link{offset}} terms can be included in the
    formula instead or as well, and if both are specified their sum is
    used.  See \code{\link{model.offset}}.}

  \item{control}{a list of parameters for controlling the fitting
    process.  See the documentation for \code{\link{glm.control}}
    for details.}

  \item{model}{a logical value indicating whether \emph{model frame}
    should be included as a component of the returned value.}

  \item{method}{the method to be used in fitting the model.
    The default method \code{"glm.fit"} uses iteratively reweighted
    least squares (IWLS).  The only current alternative is
    \code{"model.frame"} which returns the model frame and does no fitting.}

  \item{x, y}{For \code{glm}:
    logical values indicating whether the response vector and model
    matrix used in the fitting process should be returned as components
    of the returned value.
    
    For \code{glm.fit}: \code{x} is a design matrix of dimension \code{n
      * p}, and \code{y} is a vector of observations of length \code{n}.
  }

  \item{contrasts}{an optional list. See the \code{contrasts.arg}
    of \code{model.matrix.default}.}
    
  \item{intercept}{logical. Should an intercept be included in the
    \emph{null} model?}
     
  \item{prior.mean}{prior mean for the coefficients:default is 0. Can be a vector 
    of length equal to the number of predictors 
    (including the intercept, if any). If it is a scalar, it is 
    expanded to the length of this vector.}
  
  \item{prior.scale}{prior scale for the coefficients: default is 2.5. 
    Can be a vector of length equal to the number of predictors 
    (including the intercept, if any). If it is a scalar, it is 
    expanded to the length of this vector.}
  
  \item{prior.scale.for.intercept}{prior scale for the intercept: default is 10.}
  
  \item{prior.df}{for t distribution: default is 1 (Cauchy). Set to Inf to 
    get normal prior distributions. Can be a vector of length equal to 
    the number of predictors (including the intercept, if any). 
    If it is a scalar, it is expanded to the length of this vector.}
  
  \item{scaled}{if scaled = TRUE, then the prior distribution is rescaled: 
    default is TRUE}
  
  \item{n.iter}{default is 50.}
  
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
The program is a simple alteration of \code{glm()} that uses an approximate EM 
algorithm to update the betas at each step using an augmented regression 
to represent the prior information.

We use Student-t prior distributions for the coefficients. The prior 
distribution for the constant term is set so it applies to the value 
when all predictors are set to their mean values.

If scaled=TRUE, the scales for the prior distributions of the 
coefficients are determined as follows: For a predictor with only one 
value, we just use prior.scale. For a predictor with two values, we use 
prior.scale/range(x). For a predictor with more than two values, we use 
prior.scale/(2*sd(x)).

We include all the \code{glm()} arguments but we haven't tested that all the 
options (e.g., \code{offests}, \code{contrasts}, 
\code{deviance} for the null model) all work.

The new arguments here are: \code{prior.mean}, \code{prior.scale}, 
\code{prior.scale.for.intercept}, \code{prior.df}, and 
\code{scaled}.
}

\value{See \code{glm} for details.}

\references{Andrew Gelman, Aleks Jakulin, Maria Grazia Pittau and Yu-Sung Su, 
            A default prior distribution for logistic and other regression models,  
unpublished paper available at \url{http://www.stat.columbia.edu/~gelman/standardize/}
}

\author{Andrew Gelman \email{gelman@stat.columbia.edu};
        Yu-Sung Su \email{ys463@columbia.edu};
        Maria Grazia Pittau \email{grazia@stat.columbia.edu};
        Aleks Jakulin \email{Jackulin@stat.columbia.edu}}

\seealso{\code{\link{glm}},
         \code{\link{bayespolr}}
        }

\examples{
  n <- 100
  x1 <- rnorm (n)
  x2 <- rbinom (n, 1, .5)
  b0 <- 1
  b1 <- 1.5
  b2 <- 2
  y <- rbinom (n, 1, invlogit(b0+b1*x1+b2*x2))

  M1 <- glm (y ~ x1 + x2, family=binomial(link="logit"))
  display (M1)  # (using the display() function from regression.R)

  M2 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"), 
    prior.scale=Inf, prior.df=Inf)
  display (M2)  # just a test:  this should be identical to classical logit

  M3 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"))  
    # default Cauchy prior with scale 2.5
  display (M3)

  M4 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"), 
    prior.scale=2.5, prior.df=1)  
    # Same as M3, explicitly specifying Cauchy prior with scale 2.5
  display (M4)

  M5 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"), 
    prior.scale=2.5, prior.df=7)   # t_7 prior with scale 2.5
  display (M5)

  M6 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"), 
    prior.scale=2.5, prior.df=Inf)  # normal prior with scale 2.5
  display (M6)

# Create separation:  set y=1 whenever x2=1
# Now it should blow up without the prior!

  y <- ifelse (x2==1, 1, y)

  M1 <- glm (y ~ x1 + x2, family=binomial(link="logit"))
  display (M1)

  M2 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"), 
    prior.scale=Inf, prior.df=Inf) # Same as M1
  display (M2)

  M3 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"))
  display (M3)

  M4 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"), 
    prior.scale=2.5, prior.df=1)  # Same as M3
  display (M4)

  M5 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"), 
    prior.scale=2.5, prior.df=7)
  display (M5)

  M6 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"), 
    prior.scale=2.5, prior.df=Inf)
  display (M6)
}

\keyword{models}
\keyword{methods}
