if (!isGeneric("display")) {
    setGeneric("display",
               function(object, digits=2)
               standardGeneric("display"))
}


if (!isGeneric("sim")) {
    setGeneric("sim",
               function(object, n.sims=100)
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



   
