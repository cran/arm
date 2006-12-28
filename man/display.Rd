\name{display}
\docType{genericFunction}
\alias{display}
\alias{display,lm-method}
\alias{display,glm-method}
\alias{display,mer-method}
\alias{display,polr-method}
\alias{display.lm}
\alias{display.glm}
\alias{display.mer}
\alias{display.polr}

\title{Functions for Processing lm, glm, mer and polr Output}

\description{This generic function gives a
    clean printout of lm, glm, mer and polr objects.}

\usage{
    display(object, digits = 2)
}

\arguments{
  \item{object}{The output of a call to lm, glm, mer, polr, or related regressions 
    function with n data points and k predictors.}
  \item{digits}{number of significant digits to display.}
  }

\details{This generic function gives a
    clean printout of lm, glm, mer and polr objects, focusing 
    on the most pertinent pieces of information: 
    the coefficients and their standard errors, the sample size, 
    number of predictors, residual standard deviation, and R-squared.
    Note: R-squared is automatically displayed to 2 digits, and deviances are
    automatically displayed to 1 digit, no matter what.
}

\value{Coefficients
    and their standard errors, the sample size, number of predictors, 
    residual standard
    deviation, and R-squared}
    
\references{Andrew Gelman and Jennifer Hill, 
            Data Analysis Using Regression and Multilevel/Hierarchical Models, 
            Cambridge University Press, 2006.}

\author{Andrew Gelman \email{gelman@stat.columbia.edu}; 
        Yu-Sung Su \email{ys463@columbia.edu};
        Maria Grazia Pittau \email{grazia@stat.columbia.edu}
        }
        
\note{Output are the model, the regression coefficients and standard errors,
 and the residual sd and R-squared (for a linear model),
 or the null deviance and residual deviance (for a generalized linear model).
}

\seealso{\code{\link{summary}},
         \code{\link{lm}},
         \code{\link{glm}},
         \code{\link[lme4]{lmer}},
         \code{\link[MASS]{polr}}
         }

\examples{                          
# Here's a simple example of a model of the form, y = a + bx + error, 
# with 10 observations in each of 10 groups, and with both the 
# intercept and the slope varying by group.  First we set up the model and data.

   group <- rep(1:10, rep(10,10))
   mu.a <- 0
   sigma.a <- 2
   mu.b <- 3
   sigma.b <- 4
   rho <- 0
   Sigma.ab <- array (c(sigma.a^2, rho*sigma.a*sigma.b, 
                    rho*sigma.a*sigma.b, sigma.b^2), c(2,2))
   sigma.y <- 1
   ab <- mvrnorm (10, c(mu.a,mu.b), Sigma.ab)
   a <- ab[,1]
   b <- ab[,2]
#   
   x <- rnorm (100)
   y1 <- rnorm (100, a[group] + b[group]*x, sigma.y)
   y2 <- rbinom(100, 1, prob=invlogit(a[group] + b*x))
# 
# Then fit and display a simple varying-intercept model:
 
   M1 <- lmer (y1 ~ x + (1|group))
   display (M1)
   M1.sim <- mcsamp (M1)
   print (M1.sim)
   plot (M1.sim)
# 
# Then the full varying-intercept, varying-slope model:
# 
   M2 <- lmer (y1 ~ x + (1 + x |group))
   display (M2)
   M2.sim <- mcsamp (M2)
   print (M2.sim)
   plot (M2.sim)
# 
# Then the full varying-intercept, logit model:
# 
   M3 <- lmer (y2 ~ x + (1|group), family=binomial(link="logit"))
   display (M3)
   M3.sim <- mcsamp (M3)
   print (M3.sim)
   plot (M3.sim)
# 
# Then the full varying-intercept, varying-slope logit model:
# 
   M4 <- lmer (y2 ~ x + (1 + x |group), family=binomial(link="logit"))
   display (M4)
   M4.sim <- mcsamp (M4)
   print (M4.sim)
   plot (M4.sim)

# 
# Then the ordered logit model from polr
#
   M5 <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
   display(M5)
   
   M6 <- bayespolr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
   display(M6)

   
}

\keyword{methods}
