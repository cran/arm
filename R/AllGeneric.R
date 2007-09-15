if (!isGeneric("coefplot")) {
    setGeneric("coefplot",
               function(object, ...)
               standardGeneric("coefplot"))
}


if (!isGeneric("display")) {
    setGeneric("display",
               function(object, ...)
               standardGeneric("display"))
}


if (!isGeneric("sim")) {
    setGeneric("sim",
               function(object, ...)
               standardGeneric("sim"))
}


if (!isGeneric("sigma.hat")) {
    setGeneric("sigma.hat",
               function(object)
               standardGeneric("sigma.hat"))
}

if (!isGeneric("se.coef")) {
    setGeneric("se.coef",
               function(object)
               standardGeneric("se.coef"))
}


if (!isGeneric("terms")) {
    setGeneric("terms",
               function(x,...)
               standardGeneric("terms"))
}

if (!isGeneric("mcsamp")) {
    setGeneric("mcsamp",
               function(object, ...)
               standardGeneric("mcsamp"))
}


   
