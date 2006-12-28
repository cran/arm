se.coef.lm <- function(object){
    object.class <- class(object)[[1]]
    sqrt (diag(vcov(object)))
}

se.coef.glm <- function(object){
    object.class <- class(object)[[1]]
    sqrt (diag(vcov(object)))
}

se.coef.mer <- function(object){
    if (sum(unlist(lapply(object@bVar, is.na)))>0){
        object@call$control <- list(usePQL=TRUE)
        object <- lmer(object@call$formula)
    }
    fcoef <- .Call("mer_fixef", object, PACKAGE = "lme4")
    useScale <- object@devComp[8]
    corF <- vcov(object)@factors$correlation
    se.unmodeled <- NULL
    se.unmodeled[[1]] <- corF@sd
    names (se.unmodeled) <- "unmodeled"

    sc <- attr (VarCorr (object, useScale=useScale), "sc")
    vars <- object@bVar
    se.bygroup <- vars
    n.groupings <- length (vars)
    for (m in 1:n.groupings){
      vars.m <- vars[[m]]
      K <- dim(vars.m)[1]
      J <- dim(vars.m)[3]
      se.bygroup[[m]] <- array (NA, c(J,K))
      for (j in 1:J){
        se.bygroup[[m]][j,] <- sqrt(diag(as.matrix(vars.m[,,j])))
      }
      se.bygroup[[m]] <- se.bygroup[[m]]*sc
      names.full <- dimnames (ranef(object)[[m]])
      dimnames (se.bygroup[[m]]) <- list (names.full[[1]],
        names.full[[2]])
    }
    ses <- c (se.unmodeled, se.bygroup)
    return (ses)
}


se.fixef <- function (object){
  #object <- summary (object)
  fcoef <- .Call("mer_fixef", object, PACKAGE = "lme4")
  useScale <- object@devComp[8]
  corF <- vcov(object)@factors$correlation
  return (corF@sd)
}

se.ranef <- function (object){
  if (sum(unlist(lapply(object@bVar, is.na)))>0){
        object@call$control <- list(usePQL=TRUE)
        object <- lmer(object@call$formula)
  }
  useScale <- object@devComp[8]
  sc <- attr (VarCorr (object, useScale=useScale), "sc")
  vars <- object@bVar
  se.bygroup <- vars
  n.groupings <- length (vars)
  for (m in 1:n.groupings){
    vars.m <- vars[[m]]
    K <- dim(vars.m)[1]
    J <- dim(vars.m)[3]
    se.bygroup[[m]] <- array (NA, c(J,K))
    for (j in 1:J){
      se.bygroup[[m]][j,] <- sqrt(diag(as.matrix(vars.m[,,j])))
    }
    se.bygroup[[m]] <- se.bygroup[[m]]*sc
    names.full <- dimnames (ranef(object)[[m]])
    dimnames (se.bygroup[[m]]) <- list (names.full[[1]],
      names.full[[2]])
  }
  return (se.bygroup)
}
