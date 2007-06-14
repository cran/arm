display.lm <- function(object, digits=2){
    call <- object$call
    summ <- summary (object)
    coef <- summ$coef[,1:2,drop=FALSE]
    dimnames(coef)[[2]] <- c("coef.est","coef.se")
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    print (call)
    pfround (coef, digits)
    cat (paste ("  n = ", n, ", k = ", k,
    "\n  residual sd = ", fround (summ$sigma, digits),
    ", R-Squared = ", fround (summ$r.squared, 2), "\n", sep=""))
}

display.glm <- function (object, digits = 2){
    call <- object$call
    summ <- summary(object, dispersion = object$dispersion)
    coef <- matrix( NA, length( object$coefficients ),2 )
    rownames(coef) <- names( object$coefficients )          ## M
    dimnames(coef)[[2]] <- c( "coef.est", "coef.se" )
    coef[ rownames( coef ) %in% rownames( summ$coef[, 1:2, drop = FALSE]) , ] <- summ$coef[ , 1:2, drop = FALSE ]  ## M
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    print(call)
    pfround(coef, digits)
    cat(paste("  n = ", n, ", k = ", k, "\n  residual deviance = ", 
        fround(summ$deviance, 1), ", null deviance = ", fround(summ$null.deviance, 
            1), " (difference = ", fround(summ$null.deviance - 
            summ$deviance, 1), ")", "\n", sep = ""))
    dispersion <- if (is.null(object$dispersion)) 
        summ$dispersion
    else object$dispersion
    if (dispersion != 1) {
        cat(paste("overdispersion parameter = ", fround(dispersion, 
            1), "\n", sep = ""))
        if (family(object)$family == "gaussian") {
            cat(paste("  residual sd is sqrt(overdispersion) = ", 
                fround(sqrt(dispersion), digits), "\n", sep = ""))
        }
    }
}


display.mer <- function(object, digits=2){
    call <- object@call
    print (call)
    #object <- summary(object)
    fcoef <- .Call("mer_fixef", object, PACKAGE = "lme4")
    useScale <- object@devComp[8]
    corF <- vcov(object)@factors$correlation
    coefs <- cbind(fcoef, corF@sd)
    if (length (fcoef) > 0){
      dimnames(coefs) <- list(names(fcoef), c("coef.est", "coef.se"))
      pfround (coefs, digits)
    }
    cat("Error terms:\n")
    vc <- as.matrix.VarCorr (VarCorr (object), useScale=useScale, digits)
    print (vc[,c(1:2,4:ncol(vc))], quote=FALSE)
    ngrps <- lapply(object@flist, function(x) length(levels(x)))
    REML <- object@status["REML"]
    llik <- logLik(object, REML)
    AIC <- AIC(llik)
    dev <- object@deviance[1]     # Dbar
    devc <- object@devComp
    Dhat <- -2*(llik) # Dhat
    pD <- dev - Dhat              # pD
    DIC <- dev + pD               # DIC=Dbar+pD=Dhat+2pD
    cat(sprintf("number of obs: %d, groups: ", devc[1]))
    cat(paste(paste(names(ngrps), ngrps, sep = ", "), collapse = "; "))
    cat(sprintf("\nAIC = %g, DIC = ", fround(AIC, 1)))
    cat(fround(DIC, 1))
    cat("\ndeviance =", fround (dev, 1), "\n")
    if (useScale < 0){
      cat("overdispersion parameter =", fround (.Call("mer_sigma", 
        object, FALSE, PACKAGE = "lme4"), 1), "\n")
    }
}


display.mer2 <- function(object, digits=2){
    call <- object@call
    print (call)
    #object <- summary(object)
    fcoef <- fixef(object)
    useScale <- attr (VarCorr (object), "sc")
    corF <- vcov(object)@factors$correlation
    coefs <- cbind(fcoef, corF@sd)
    if (length (fcoef) > 0){
      dimnames(coefs) <- list(names(fcoef), c("coef.est", "coef.se"))
      pfround (coefs, digits)
    }
    cat("Error terms:\n")
    vc <- as.matrix.VarCorr (VarCorr (object), useScale=useScale, digits)
    print (vc[,c(1:2,4:ncol(vc))], quote=FALSE)
    ngrps <- lapply(object@flist, function(x) length(levels(x)))
    llik <- logLik(object)
    AIC <- AIC(llik)
    dev <- object@deviance[1]     # Dbar
    devc <- object@dims
    Dhat <- -2*(llik) # Dhat
    pD <- dev - Dhat              # pD
    DIC <- dev + pD               # DIC=Dbar+pD=Dhat+2pD
    cat(sprintf("number of obs: %d, groups: ", devc[2]))
    cat(paste(paste(names(ngrps), ngrps, sep = ", "), collapse = "; "))
    cat(sprintf("\nAIC = %g, DIC = ", fround(AIC,1)))
    cat(fround(DIC, 1))
    cat("\ndeviance =", fround (dev, 1), "\n")
    if (useScale < 0){
      cat("overdispersion parameter =", fround (.Call("lmer2_sigma", 
        object, FALSE, PACKAGE = "lme4"), 1), "\n")
    }
}




display.polr <- function(object, digits=2){
    call <- object$call
    summ <- summary(object)
    coef <- summ$coef[, 1:2, drop = FALSE]
    dimnames(coef)[[2]] <- c("coef.est", "coef.se")
    n <- summ$n  # or maybe should be "nobs", I don't know for sure
    k <- nrow (coef)
    k.intercepts <- length (summ$zeta)
    print(call)
    pfround(coef, digits)
    cat(paste("  n = ", n, ", k = ", k, " (including ", k.intercepts,
        " intercepts)\n  residual deviance = ",
        fround(summ$deviance, 1), 
        ", null deviance is not computed by polr",
        "\n", sep = ""))
}
