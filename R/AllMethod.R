# coefplot methods
setMethod("coefplot", signature(object = "numeric"), 
    function(object, ...)
    {
    coefplot.default (coefs=object, ...)
    }
)

setMethod("coefplot", signature(object = "lm"), 
    function(object, ...)
    {
    coefplot.lm (object, ...)
    }
)

setMethod("coefplot", signature(object = "glm"),
    function(object, ...)
    {
    coefplot.glm (object, ...)
    }
)

setMethod("coefplot", signature(object = "bugs"),
    function(object, ...)
    {
    coefplot.bugs (object, ...)
    }
)

setMethod("coefplot", signature(object = "polr"),
    function(object, ...)
    {
    coefplot.polr (object, ...)
    }
)

# display methods

setMethod("display", signature(object = "lm"),
    function(object, ...)
    {
    display.lm (object, ...)
    }
)

setMethod("display", signature(object = "glm"),
    function(object, ...)
    {
    display.glm (object, ...)
    }
)

setMethod("display", signature(object = "mer"),
    function(object, ...)
    {
    display.mer (object, ...)
    }
)

setMethod("display", signature(object = "polr"),
    function(object, ...)
    {
    display.polr (object, ...)
    }
)

# sim methods

setMethod("sim", signature(object = "lm"),
    function(object, ...)
    {
    sim.lm (object, ...)
    }
)

setMethod("sim", signature(object = "glm"),
    function(object, ...)
    {
    sim.glm (object, ...)
    }
)

setMethod("sim", signature(object = "mer"),
    function(object, ...)
    {
    sim.mer (object, ...)
    }
)


# sigma.hat methods

setMethod("sigma.hat", signature(object = "lm"),
    function(object)
    {
    sigma.hat.lm (object)
    }
)


setMethod("sigma.hat", signature(object = "glm"),
    function(object)
    {
    sigma.hat.glm (object)
    }
)


setMethod("sigma.hat", signature(object = "mer"),
    function(object)
    {
    sigma.hat.mer (object)
    }
)  

# se.coef methods

setMethod("se.coef", signature(object = "lm"),
    function(object)
    {
    se.coef.lm (object)
    }
)


setMethod("se.coef", signature(object = "glm"),
    function(object)
    {
    se.coef.glm (object)
    }
)

setMethod("se.coef", signature(object = "mer"),
    function(object)
    {
    se.coef.mer (object)
    }
)
