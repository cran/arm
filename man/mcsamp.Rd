\name{mcsamp}
\alias{mcsamp}

\title{Quick Function to Run mcmcsamp() in lme4}

\description{
    The function for MCMC sampling for
    lmer and glmer objects and convert to Bugs objects for easy display
}
\usage{
mcsamp (object, n.chains=3, n.iter=1000, 
    n.burnin=floor(n.iter/2), n.thin=1, 
    saveb=TRUE, make.bugs.object=TRUE)
}

\arguments{
  \item{object}{lmer or glmer objects}
  \item{n.chains}{number of MCMC chains}
  \item{n.iter}{number of iteration for each MCMC chain}
  \item{n.burnin}{number of burnin for each MCMC chain}
  \item{n.thin}{number of thin for each MCMC chain}
  \item{saveb}{if 'TRUE', causes the values
          of the random effects in each sample to be saved.}
  \item{make.bugs.object}{tranform the output into bugs object, default is TRUE}
}

\details{
     This generic generates a sample from the posterior
     distribution of the parameters of a fitted model using Markov
     Chain Monte Carlo methods.
}

\value{
An object of (S3) class '"mcmc"' suitable for use with the
functions in the "coda" package.
}

\references{Andrew Gelman and Jennifer Hill, 
            Data Analysis Using Regression and Multilevel/Hierarchical Models, 
            Cambridge University Press, 2006.}

\author{Andrew Gelman \email{gelman@stat.columbia.edu}; 
        Yu-Sung Su \email{ys463@columbia.edu}
        }

\seealso{\code{\link{display}},
        \code{\link{sim}}
        }

\examples{
# Here's a simple example of a model of the form, y = a + bx + error, 
# with 10 observations in each of 10 groups, and with both the intercept 
# and the slope varying by group.  First we set up the model and data.
#   
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
 }

\keyword{models}
\keyword{methods}
