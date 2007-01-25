

sigma.hat.lm <- function(object){
    object.class <- class(object)[[1]]
    sigma <- summary(object)$sigma
    return (sigma)
}


sigma.hat.glm <- function(object){
    object.class <- class(object)[[1]]
    if(object$family$family=="gaussian"){
        sigma <- summary.lm(object)$sigma
    }
    else {
        sigma <- summary(object, correlation=TRUE)$sigma
    }
    return (sigma)
}


sigma.hat.mer <- function(object){
    object <- summary (object)
    fcoef <- .Call("mer_fixef", object, PACKAGE = "lme4")
    useScale <- object@devComp[8]
    ngrps <- lapply(object@flist, function(x) length(levels(x)))
    n.groupings <- length (ngrps)
    varc <- VarCorr (object, useScale=useScale)
    sc <- attr(varc, "sc")
    recorr <- lapply(varc, function(el) el@factors$correlation)
    reStdDev <- c(lapply(recorr, slot, "sd"), list(Residual = sc))
    sigmas <- as.list (rep (NA, n.groupings+1))
    sigmas[1] <- ifelse (useScale, sc, NA)
    cors <- as.list (rep (NA, n.groupings+1))
    names (sigmas) <- names (cors) <- c ("data", names (varc))
    for (k in 1:n.groupings){
      sigmas[[k+1]] <- reStdDev[[k]]
      cors[[k+1]] <- as.matrix (recorr[[k]])
      if (length (cors[[k+1]]) == 1) cors[[k+1]] <- NA
    }
    return (list (sigma=sigmas, cors=cors))
}
