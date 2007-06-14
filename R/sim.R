sim.lm <- function(object, n.sims=100){
    object.class <- class(object)[[1]]
    summ <- summary (object)
    coef <- summ$coef[,1:2,drop=FALSE]
    dimnames(coef)[[2]] <- c("coef.est","coef.sd")
    sigma.hat <- summ$sigma
    beta.hat <- coef[,1]
    V.beta <- summ$cov.unscaled
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    sigma <- rep (NA, n.sims)
    beta <- array (NA, c(n.sims,k))
    dimnames(beta) <- list (NULL, names(beta.hat))
    for (s in 1:n.sims){
      sigma[s] <- sigma.hat*sqrt((n-k)/rchisq(1,n-k))
      beta[s,] <- mvrnorm (1, beta.hat, V.beta*sigma[s]^2)
    }
    return (list (beta=beta, sigma=sigma))
}


sim.glm <- function(object, n.sims=100){
    object.class <- class(object)[[1]]
    summ <- summary (object, correlation=TRUE)
    coef <- summ$coef[,1:2,drop=FALSE]
    dimnames(coef)[[2]] <- c("coef.est","coef.sd")
    beta.hat <- coef[,1]
    sd.beta <- coef[,2]
    corr.beta <- summ$corr
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    V.beta <- corr.beta * array(sd.beta,c(k,k)) * t(array(sd.beta,c(k,k)))
    beta <- array (NA, c(n.sims,k))
    dimnames(beta) <- list (NULL, names(beta.hat))
    for (s in 1:n.sims){
      beta[s,] <- mvrnorm (1, beta.hat, V.beta)
    }
    sigma <- rep (sqrt(summ$dispersion), n.sims)
    return (list(beta=beta, sigma=sigma))
}




sim.mer <- function(object, n.sims=100){
#    object <- summary(object)
#    if (lapply(object@bVar,sum)<=0|sum(unlist(lapply(object@bVar, is.na)))>0){
#        object@call$control <- list(usePQL=TRUE)
#        object <- lmer(object@call$formula)
#    }
    sc <- attr (VarCorr (object), "sc")
    # simulate unmodeled coefficients
    fcoef <- .Call("mer_fixef", object, PACKAGE = "lme4")
    corF <- vcov(object)@factors$correlation
    se.unmodeled <- corF@sd
    V.beta <- (se.unmodeled %o% se.unmodeled) * as.matrix(corF)
    beta.unmodeled <- NULL
    if (length (fcoef) > 0){
      beta.unmodeled[[1]] <- mvrnorm (n.sims, fcoef, V.beta)
      names (beta.unmodeled) <- "unmodeled"
    }
    # simulate coefficients within groups
    coef <- ranef (object)
    sc <- attr (VarCorr (object, useScale=useScale), "sc")
    vars <- object@bVar
    beta.bygroup <- vars
    n.groupings <- length (vars)
    for (m in 1:n.groupings){
      vars.m <- vars[[m]]
      K <- dim(vars.m)[1]
      J <- dim(vars.m)[3]
      beta.bygroup[[m]] <- array (NA, c(n.sims, J, K))
      bhat <- ranef(object)[[m]]
      for (j in 1:J){
        V.beta <- untriangle(vars.m[,,j])*sc^2
        beta.bygroup[[m]][,j,] <- mvrnorm (n.sims, bhat[j,], V.beta)
      }   
      dimnames (beta.bygroup[[m]]) <- c (list(NULL), dimnames(bhat))
    }
    betas <- c (beta.unmodeled, beta.bygroup)
    return (betas)
}

sim.mer2 <- function(object, n.sims=100){

    # simulate unmodeled coefficients
    fcoef <- fixef(object)
    corF <- vcov(object)@factors$correlation
    se.unmodeled <- corF@sd
    V.beta <- (se.unmodeled %o% se.unmodeled) * as.matrix(corF)
    beta.unmodeled <- NULL
    if (length (fcoef) > 0){
      beta.unmodeled[[1]] <- mvrnorm (n.sims, fcoef, V.beta)
      names (beta.unmodeled) <- "unmodeled"
    }
    # simulate coefficients within groups
    sc <- attr (VarCorr (object), "sc")  # scale
    coef <- ranef (object)
    vars <- object@bVar      #???? str(ranef(object, postVar=TRUE))????
    beta.bygroup <- vars
    n.groupings <- length (vars)
    for (m in 1:n.groupings){
      vars.m <- vars[[m]]
      K <- dim(vars.m)[1]
      J <- dim(vars.m)[3]
      beta.bygroup[[m]] <- array (NA, c(n.sims, J, K))
      bhat <- ranef(object)[[m]]
      for (j in 1:J){
        V.beta <- untriangle(vars.m[,,j])*sc^2
        beta.bygroup[[m]][,j,] <- mvrnorm (n.sims, bhat[j,], V.beta)
      }   
      dimnames (beta.bygroup[[m]]) <- c (list(NULL), dimnames(bhat))
    }
    betas <- c (beta.unmodeled, beta.bygroup)
    return (betas)
}
