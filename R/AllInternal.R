# some useful little functions

  sd.scalar <- function (x, ...) {sqrt(var(as.vector(x), ...))}
  wmean <- function (x, w, ...) {mean(x*w, ...)/mean(w, ...)}
  logit <- function (x) {log(x/(1-x))}
  untriangle <- function (x) {x + t(x) - x*diag(nrow(as.matrix(x)))}

# new functions!


as.matrix.VarCorr <- function (varc, useScale, digits){
# VarCorr function for lmer objects, altered as follows:
#   1.  specify rounding
#   2.  print statement at end is removed
#   3.  reMat is returned
#   4.  last line kept in reMat even when there's no error term
#
                  sc <- attr(varc, "sc")[[1]]    
                  recorr <- lapply(varc, function(el) el@factors$correlation)
                  reStdDev <- c(lapply(recorr, slot, "sd"), list(Residual = sc))
                  reLens <- unlist(c(lapply(reStdDev, length)))
                  reMat <- array('', c(sum(reLens), 4),
                                 list(rep('', sum(reLens)),
                                      c("Groups", "Name", "Variance", "Std.Dev.")))
                  reMat[1+cumsum(reLens)-reLens, 1] <- names(reLens)
                  reMat[,2] <- c(unlist(lapply(reStdDev, names)), "")
#                  reMat[,3] <- format(unlist(reStdDev)^2, digits = digits)
#                  reMat[,4] <- format(unlist(reStdDev), digits = digits)
                  reMat[,3] <- fround(unlist(reStdDev)^2, digits)
                  reMat[,4] <- fround(unlist(reStdDev), digits)
                  if (any(reLens > 1)) {
                      maxlen <- max(reLens)
                      corr <-
                          do.call("rbind",
                                  lapply(recorr,
                                         function(x, maxlen) {
                                             x <- as(x, "matrix")
#                                             cc <- format(round(x, 3), nsmall = 3)
                                             cc <- fround (x, digits)
                                             cc[!lower.tri(cc)] <- ""
                                             nr <- dim(cc)[1]
                                             if (nr >= maxlen) return(cc)
                                             cbind(cc, matrix("", nr, maxlen-nr))
                                         }, maxlen))
                      colnames(corr) <- c("Corr", rep("", maxlen - 1))
                      reMat <- cbind(reMat, rbind(corr, rep("", ncol(corr))))
                  }
#                  if (!useScale) reMat <- reMat[-nrow(reMat),]
          if (useScale<0) reMat[nrow(reMat),] <- c ("No residual sd", rep("",ncol(reMat)-1))
          return (reMat)
      }


# rwish and dwish functions stolen from Martin and Quinn's MCMCpack

rwish <- function (v, S){
  if (!is.matrix(S)) 
        S <- matrix(S)
    if (nrow(S) != ncol(S)) {
        stop(message = "S not square in rwish().\n")
    }
    if (v < nrow(S)) {
        stop(message = "v is less than the dimension of S in rwish().\n")
    }
    p <- nrow(S)
    CC <- chol(S)
    Z <- matrix(0, p, p)
    diag(Z) <- sqrt(rchisq(p, v:(v - p + 1)))
    if (p > 1) {
        pseq <- 1:(p - 1)
        Z[rep(p * pseq, pseq) + unlist(lapply(pseq, seq))] <- rnorm(p * 
            (p - 1)/2)
    }
    return(crossprod(Z %*% CC))
}

dwish <- function (W, v, S) {
    if (!is.matrix(S)) 
        S <- matrix(S)
    if (nrow(S) != ncol(S)) {
        stop(message = "W not square in dwish()\n\n")
    }
    if (!is.matrix(W)) 
        S <- matrix(W)
    if (nrow(W) != ncol(W)) {
        stop(message = "W not square in dwish()\n\n")
    }
    if (nrow(S) != ncol(W)) {
        stop(message = "W and X of different dimensionality in dwish()\n\n")
    }
    if (v < nrow(S)) {
        stop(message = "v is less than the dimension of S in  dwish()\n\n")
    }
    k <- nrow(S)
    gammapart <- 1
    for (i in 1:k) {
        gammapart <- gammapart * gamma((v + 1 - i)/2)
    }
    denom <- gammapart * 2^(v * k/2) * pi^(k * (k - 1)/4)
    detS <- det(S)
    detW <- det(W)
    hold <- solve(S) %*% W
    tracehold <- sum(hold[row(hold) == col(hold)])
    num <- detS^(-v/2) * detW^((v - k - 1)/2) * exp(-1/2 * tracehold)
    return(num/denom)
}

model.matrix.bayes <- function( object, data = environment( object ),
                                contrasts.arg = NULL, xlev = NULL, keep.order=keep.order, ...)
{
    t <- if( missing( data ) ) { terms.object( object ) } else { terms.bayes( object, data=data, keep.order=keep.order ) }
    attr(t, "intercept") <- attr(object, "intercept")
    if ( is.null( attr( data, "terms" ) ) ){ data <- model.frame( object, data, xlev=xlev ) }
    else {
        reorder <- match(sapply(attr(t,"variables"),deparse, width.cutoff=500)[-1], names(data))
        if ( any( is.na( reorder ) ) ) {
            stop( "model frame and formula mismatch in model.matrix()" ) 
        }
        if( !identical( reorder, seq_len( ncol( data ) ) ) ) {
            data <- data[,reorder, drop=FALSE] 
        }
    }
    int <- attr( t, "response")
    if( length( data ) ) {      # otherwise no rhs terms, so skip all this
        contr.funs <- as.character(list("contr.bayes.unordered", "contr.bayes.ordered"))
        namD <- names(data)
        ## turn any character columns into factors
        for(i in namD)
            if(is.character( data[[i]] ) ) {
                data[[i]] <- factor(data[[i]])
                warning( gettextf( "variable '%s' converted to a factor", i ), domain = NA)
            }
        isF <- sapply( data, function( x ) is.factor( x ) || is.logical( x ) )
        isF[int] <- FALSE
        isOF <- sapply( data, is.ordered )
        for( nn in namD[isF] )            # drop response
            if( is.null( attr( data[[nn]], "contrasts" ) ) ) {
                contrasts( data[[nn]] ) <- contr.funs 
            }
        ## it might be safer to have numerical contrasts:
        ##    get(contr.funs[1 + isOF[nn]])(nlevels(data[[nn]]))
        if ( !is.null( contrasts.arg ) && is.list( contrasts.arg ) ) {
            if ( is.null( namC <- names( contrasts.arg ) ) ) {
                stop( "invalid 'contrasts.arg' argument" )
            }
            for (nn in namC) {
                if ( is.na( ni <- match( nn, namD ) ) ) {
                    warning( gettextf( "variable '%s' is absent, its contrast will be ignored", nn ), domain = NA )
                }
                else {
                    ca <- contrasts.arg[[nn]]
                    if( is.matrix( ca ) ) {
                        contrasts( data[[ni]], ncol( ca ) ) <- ca
                    }
                    else { 
                        contrasts( data[[ni]] ) <- contrasts.arg[[nn]]
                    }
                }
            }
        }
    } else {               # internal model.matrix needs some variable
        isF  <-  FALSE
        data <- list( x=rep( 0, nrow( data ) ) )
    }
    ans  <- .Internal( model.matrix( t, data ) )
    cons <- if(any(isF)){
    lapply( data[isF], function( x ) attr( x,  "contrasts") ) }
    else { NULL }
    attr( ans, "contrasts" ) <- cons
    ans
}

contr.bayes.ordered <- function ( n, scores = 1:n, contrasts = TRUE )
{
    make.poly <- function( n, scores ) {
        y   <- scores - mean( scores )
        X   <- outer( y, seq_len( n ) - 1, "^" )
        QR  <- qr( X )
        z   <- QR$qr
        z   <- z *( row( z ) == col( z ) )
        raw <- qr.qy( QR, z )
        Z   <- sweep( raw, 2, apply( raw, 2, function( x ) sqrt( sum( x^2 ) ) ), "/" )
        colnames( Z ) <- paste( "^", 1:n - 1, sep="" )
        Z
    }
    if ( is.numeric( n ) && length( n ) == 1 ) { levs <- 1:n }
    else {
        levs <- n
        n <- length( levs )
    }
    if ( n < 2 ) {
        stop( gettextf( "contrasts not defined for %d degrees of freedom", n - 1 ), domain = NA ) 
    }
    if ( n > 95 ) {
        stop( gettextf( "orthogonal polynomials cannot be represented accurately enough for %d degrees of freedom", n-1 ), domain = NA ) 
    }
    if ( length( scores ) != n ) {
        stop( "'scores' argument is of the wrong length" )
    }
    if ( !is.numeric( scores ) || any( duplicated( scores ) ) ) {
        stop("'scores' must all be different numbers")
    }
    contr <- make.poly( n, scores )
    if ( contrasts ) {
        dn <- colnames( contr )
        dn[2:min( 4, n )] <- c( ".L", ".Q", ".C" )[1:min( 3, n-1 )]
        colnames( contr ) <- dn
        contr[, , drop = FALSE]
    }
    else {
        contr[, 1] <- 1
        contr
    }
}

contr.bayes.unordered <- function(n, base = 1, contrasts = TRUE) {
    if( is.numeric( n ) && length( n ) == 1) {
        if( n > 1 ) { levs <- 1:n }
        else stop( "not enough degrees of freedom to define contrasts" )
    } 
    else {
        levs <- n
        n <- length( n )
    }
    contr <- array( 0, c(n, n), list( levs, levs ) )
    diag( contr ) <- 1
    if( contrasts ) {
        if( n < 2 ) { stop( gettextf( "contrasts not defined for %d degrees of freedom", n - 1 ), domain = NA ) }
        if( base < 1 | base > n ){ stop( "baseline group number out of range" ) }
        contr <- contr[, , drop = FALSE]
    }
    contr
}

terms.bayes <- function( x, specials = NULL, abb = NULL, data = NULL, neg.out = TRUE, 
                         keep.order = FALSE, simplify = FALSE, allowDotAsName = FALSE, ... )
{
    fixFormulaObject <- function(object, keep.order) {
        Terms <- terms( object, keep.order )
        tmp   <- attr( Terms, "term.labels" )
        ## fix up terms involving | : PR#8462
        ind <- grep( "|", tmp, fixed = TRUE )
        if( length( ind ) ) tmp[ind] <- paste( "(", tmp[ind], ")" )
        ## need to add back any offsets
        if( length( ind <- attr( Terms, "offset" ) ) ) {
            ## can't look at rownames of factors, as not there y ~ offset(x)
            tmp2 <- as.character(attr(Terms, "variables"))[-1]
            tmp <- c(tmp, tmp2[ind])
        }
        form <- formula( object )
        lhs  <- if( length( form ) == 2 ) { NULL } else { paste( deparse( form[[2]] ), collapse="" ) }
        rhs  <- if( length( tmp ) ) { paste( tmp, collapse = " + ") } else { "1" }
        if( !attr( terms( object ), "intercept" ) ) rhs <- paste(rhs, "- 1")
            formula( paste( lhs, "~", rhs ) )
    }
    
    if(!is.null( data ) && !is.environment( data ) && !is.data.frame( data ) ) { 
        data <- as.data.frame( data, optional=TRUE )
    }

    terms <- .Internal( terms.formula( x, specials, data, keep.order, allowDotAsName ) )
    
    if( simplify ) {
        a     <- attributes( terms )
        terms <- fixFormulaObject( terms )
        attributes( terms ) <- a
    }
    environment( terms ) <- environment( x )
    if( !inherits( terms, "formula" ) ) { class( terms ) <- c( oldClass( terms ), "formula" ) }
    terms
}
