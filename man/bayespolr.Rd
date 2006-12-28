\name{bayespolr}
\alias{bayespolr}


\title{Bayesian Ordered Logistic or Probit Regression}

\description{Bayesian functions for ordered logistic or probit modeling 
        with independent normal, t, or Cauchy prior distribution 
        for the coefficients.
}
\usage{
bayespolr(formula, data, weights, start, ..., 
    subset, na.action, contrasts = NULL, 
    Hess = TRUE, model = TRUE, 
    method = c("logistic", "probit", "cloglog", "cauchit"), 
    prior.mean = 0, prior.scale = 2.5, prior.df = 1, 
    scaled = TRUE, prior.mean.for.cutpoints = 0, 
    prior.scale.for.cutpoints = 10, prior.df.for.cutpoints = 1)
}

\arguments{
  \item{formula}{a formula expression as for regression models, of the form
          'response ~ predictors'. The response should be a factor
          (preferably an ordered factor), which will be interpreted as
          an ordinal response, with levels ordered as in the factor.  A
          proportional odds model will be fitted.  The model must have
          an intercept: attempts to remove one will lead to a warning
          and be ignored. An offset may be used.  See the documentation
          of 'formula' for other details.}
          
  \item{data}{an optional data frame in which to interpret the variables
          occurring in 'formula'.}
          
  \item{weights}{optional case weights in fitting.  Default to 1.}
  
  \item{start}{initial values for the parameters.  This is in the format
          'c(coefficients, zeta)'}
          
  \item{\dots}{additional arguments to be passed to 'optim', most often a
          'control' argument.}
          
  \item{subset}{expression saying which subset of the rows of the data should
           be used in the fit.  All observations are included by
          default.}
  
  \item{na.action}{a function to filter missing data.}
  
  \item{contrasts}{a list of contrasts to be used for some or all of the
          factors appearing as variables in the model formula.}
          
  \item{Hess}{logical for whether the Hessian (the observed information
          matrix) should be returned.}
          
  \item{model}{logical for whether the model matrix should be returned.}
  
  \item{method}{logistic or probit or complementary log-log or cauchit
          (corresponding to a Cauchy latent variable and only available
          in R >= 2.1.0).}
  
  \item{prior.mean}{prior mean for the coefficients: default is 0.}
  
  \item{prior.scale}{prior scale for the coefficients: default is 2.5.}
  
  \item{prior.df}{for t distribution: default is 1 (Cauchy).}
   
  \item{scaled}{if scaled = TRUE, then the prior distribution is rescaled.}
   
  \item{prior.mean.for.cutpoints}{prior mean for cutpoints: default is 0.}
  
  \item{prior.scale.for.cutpoints}{prior scale for cutpoints: default is 10.}
  
  \item{prior.df.for.cutpoints}{for t distribution: default is 1 (Cauchy).}
}
\details{The program is a simple alteration of polr() that uses an approximate EM
    algorithm to update the betas at each step using an augmented regression
    to represent the prior information. If scaled=TRUE, the scales for 
    the prior distributions of the coefficients are determined as follows:
    
    For a predictor with only one value, we just use prior.scale. 
    For a predictor with two values, we use prior.scale/range(x).
    For a predictor with more than two values, we use prior.scale/(2*sd(x)).
    
    we use weaker a prior distribution to the cutpoints 
    (constant terms)-a Cauchy with center 0 and scale 10.
    
    The new arguments here are: \code{prior.mean}, \code{prior.scale},
    \code{prior.df}, \code{scaled}, \code{prior.mean.for.cutpoints},
    \code{prior.scale.for.cutpoints}, \code{prior.df.for.cutpoints}}

\value{See \code{polr} for details.}

\author{Andrew Gelman \email{gelman@stat.columbia.edu};
        Yu-Sung Su \email{ys463@columbia.edu};
        Maria Grazia Pittau \email{grazia@stat.columbia.edu}
}
\seealso{\code{\link{bayesglm}},
        \code{\link[MASS]{polr}}
}

\examples{
    M1 <- bayespolr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
    display(M1)

}

\keyword{models}
\keyword{methods}
