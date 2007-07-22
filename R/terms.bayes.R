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
