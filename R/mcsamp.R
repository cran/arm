# mcsamp function (wrapper for mcmcsamp in lmer())

mcsamp <- function (object, n.chains=3, n.iter=1000, n.burnin=floor(n.iter/2), n.thin=1, saveb=TRUE, make.bugs.object=TRUE){
  # Quick function to run mcmcsamp() [the function for MCMC sampling for
  # lmer objects) and convert to Bugs objects for easy display

  require ("R2WinBUGS")
  if (n.chains<2) stop ("n.chains must be at least 2")
  n.keep <- n.iter - n.burnin
  first.chain <- mcmcsamp (object, n.iter, saveb=saveb, trans=TRUE)[(n.burnin+1):n.iter,]
  n.parameters <- ncol(first.chain)
  if (n.thin!=1){
    cat ("Sorry--thinning hasn't been implemented yet!\n")
    n.thin <- 1
  }
  sims <- array (NA, c(n.keep, n.chains, n.parameters))
  par.names <- dimnames(first.chain)[[2]]
  par.names <- gsub("b.*", "", par.names, ignore.case = FALSE, # Su: rename "b.*" to ""
                    extended = TRUE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE)      
  par.names <- par.names[is.na(match(par.names,""))] 
  if (saveb){
    b.hat <- se.coef (object)                   # Su: use se.coef() 
    n.groupings <- length(b.hat) - 1
    for (m in 1:n.groupings){
      J <- dim(b.hat[[m+1]])[1]
      K <- dim(b.hat[[m+1]])[2]
      var.names <- paste (names(b.hat)[m+1],
                          unlist (dimnames(b.hat[[m+1]])[2]), sep=".")
      par.names <- c (par.names,
        paste ("eta.", rep(var.names,J), "[", rep(1:J,each=K), "]", sep=""))
    }
  }
  sims[,1,] <- first.chain
  for (k in 2:n.chains){
    sims[,k,] <- mcmcsamp (object, n.iter, saveb=saveb, trans=TRUE)[(n.burnin+1):n.iter,]
  }
  for (j in 1:n.parameters){
    if (pmatch("log(sigma^2)", par.names[j], nomatch=0)){#=="log(sigma^2)"){
      par.names[j] <- "sigma.y"
      sims[,,j] <- exp (sims[,,j]/2)
    }
    else if (pmatch("log(", par.names[j], nomatch=0)){#(substr(par.names[j],1,4)=="log("){
      par.names[j] <- paste ("sigma.", substr(par.names[j], 5, nchar(par.names[j])-1), sep="")
      sims[,,j] <- exp (sims[,,j]/2)
    }
    else if (pmatch("atanh(", par.names[j], nomatch=0)){#(substr(par.names[j],1,6)=="atanh("){
      par.names[j] <- paste ("rho.", substr(par.names[j], 7, nchar(par.names[j])-1), sep="")
      sims[,,j] <- tanh (sims[,,j])
    }
    else if (pmatch("eta.", par.names[j], nomatch=0)){#(substr(par.names[j],1,4)=="eta."){
      #par.names[j] <- paste ("", substr(par.names[j], 5, nchar(par.names[j])), sep="")
      par.names[j] <- par.names[j]
    }
    else if (pmatch("deviance", par.names[j], nomatch=0)){#(par.names[j]=="deviance"){          # Su: keep par.names for "deviance"
        sims <- sims[,,-j]                          # Su: delete deviance value from sims
    } 
    else {
      par.names[j] <- paste ("beta.", par.names[j], sep="")
    }
  }
  par.names <- par.names[is.na(match(par.names,"deviance"))] # Su: delete par.names for "deviance"
  dimnames(sims) <- list (NULL, NULL, par.names)
  if (make.bugs.object){
    return (as.bugs.array (sims, program="lmer", n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin))
  }
  else {
    return (sims)
  }
}
