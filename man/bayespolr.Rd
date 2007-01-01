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
  
  \item{prior.mean}{prior mean for the coefficients: default is 0. 
    Can be a vector of length equal to the number of predictors 
    (not counting the intercepts). If it is a scalar, it is 
    expanded to the length of this vector.}
  
  \item{prior.scale}{prior scale for the coefficients: default is 2.5.
    Can be a vector of length equal to the number of predictors 
    (not counting the intercepts). If it is a scalar, it is expanded 
    to the length of this vector.}
  
  \item{prior.df}{for t distribution: default is 1 (Cauchy). 
    Set to Inf to get normal prior distributions. Can 
    be a vector of length equal to the number of predictors (not counting 
    the intercepts). If it is a scalar, it is expanded to the length of this 
    vector.}
   
  \item{scaled}{if scaled = TRUE, then the prior distribution is rescaled.
    Can be a vector of length equal to the number of cutpoints 
    (intercepts). If it is a scalar, it is expanded to the length of 
    this vector.}
   
  \item{prior.mean.for.cutpoints}{prior mean for cutpoints: default is 0.
    Can be a vector of length equal to the number of cutpoints 
    (intercepts). If it is a scalar, it is expanded to the length of 
    this vector.}
  
  \item{prior.scale.for.cutpoints}{prior scale for cutpoints: default is 10.
    Can be a vector of length equal to the number of cutpoints 
    (intercepts). If it is a scalar, it is expanded to the length 
    of this vector.}
  
  \item{prior.df.for.cutpoints}{for t distribution: default is 1 (Cauchy).
    Can be a vector of length equal to the number of cutpoints 
    (intercepts). If it is a scalar, it is expanded to the length 
    of this vector.}
}
\details{
The program is a simple alteration of \code{polr()} that augments the 
loglikelihood with the log of the t prior distributions for the 
coefficients.

We use Student-t prior distributions for the coefficients. The prior 
distributions for the intercepts (the cutpoints) are set so they apply 
to the value when all predictors are set to their mean values.

If scaled=TRUE, the scales for the prior distributions of the 
coefficients are determined as follows: For a predictor with only one 
value, we just use \code{prior.scale}. For a predictor with two 
values, we use prior.scale/range(x). 
For a predictor with more than two values, we use prior.scale/(2*sd(x)).
}

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
