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

display.glm <- function(object, digits=2){
    call <- object$call
    summ <- summary (object)
    coef <- summ$coef[,1:2,drop=FALSE]
    dimnames(coef)[[2]] <- c("coef.est","coef.se")
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    print (call)
    pfround (coef, digits)
    cat (paste ("  n = ", n, ", k = ", k,
        "\n  residual deviance = ", fround (summ$deviance, 1),
        ", null deviance = ", fround (summ$null.deviance, 1),
        " (difference = ", fround (summ$null.deviance - summ$deviance, 1), ")",
        "\n", sep=""))
      if (summ$dispersion!=1) 
        cat (paste ("  overdispersion parameter = ",
          fround (summ$dispersion, 1), "\n", sep=""))
}

display.mer <- function(object, digits=2){
    object <- summary(object)
    call <- object@call
    print (call)
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
    dev <- object@deviance
    devc <- object@devComp
    cat(sprintf("number of obs: %d, groups: ", devc[1]))
    cat(paste(paste(names(ngrps), ngrps, sep = ", "), collapse = "; "))
    cat("\ndeviance =", fround (dev[1], 1), "\n")
    if (useScale < 0){
      cat("  overdispersion parameter =", fround (.Call("mer_sigma", 
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
