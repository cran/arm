# coefplot methods

#setMethod("coefplot", signature(object = "lm"),
#    function(fit, ...)
#   coefplot.lm (fit, ...)
#    }
#)


# display methods

setMethod("display", signature(object = "lm"),
    function(object, digits=2)
    {
    display.lm (object, digits=2)
    }
)

setMethod("display", signature(object = "glm"),
    function(object, digits=2)
    {
    display.glm (object, digits=2)
    }
)

setMethod("display", signature(object = "mer"),
    function(object, digits=2)
    {
    display.mer (object, digits=2)
    }
)

setMethod("display", signature(object = "polr"),
    function(object, digits=2)
    {
    display.polr (object, digits=2)
    }
)

# sim methods

setMethod("sim", signature(object = "lm"),
    function(object, n.sims=100)
    {
    sim.lm (object, n.sims=100)
    }
)

setMethod("sim", signature(object = "glm"),
    function(object, n.sims=100)
    {
    sim.glm (object, n.sims=100)
    }
)

setMethod("sim", signature(object = "mer"),
    function(object, n.sims=100)
    {
    sim.mer (object, n.sims=100)
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
