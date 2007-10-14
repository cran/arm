setOldClass("family")
setOldClass("polr")
setOldClass("bugs")
setOldClass("terms")


setClass("bayesglm",
     representation(
            formula = "formula",
            family = "family",
            prior.mean = "numeric", 
            prior.scale = "numeric", 
            prior.df = "numeric"),
    contains = "glm"
)


setClass("bayesglm.h",
     representation(
            formula = "formula",
            family = "family",
            prior.mean = "numeric", 
            prior.scale = "numeric", 
            prior.df = "numeric",
            batch = "numeric"),
    contains = c("bayesglm","glm")
)
