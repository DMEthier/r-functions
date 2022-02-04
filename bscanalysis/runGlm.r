# creates an inner function that will run the glm model, based on different model distributions

# df: dataframe containing the data
# fmla: formula object containing the model. You can also use bscanalysis.getFormula to create the formula.
# model.dist: model distribution from the value possible values:
#          log.trans (log-transformed), logist (logistic), nb (neg. binomial)
#          pois (poisson)
# weight.var: name of the variable used for weight in the model

# returns a glm object 

bscanalysis.runGlm <- function(tmp, fmla, model.dist, weight.var) {

    if (model.dist == "log.trans") {
      tmp.glm <- glm(fmla, weight = tmp[[weight.var]], data=tmp)
    }  # end of if model.dist str statement

    else if (model.dist == "pois") {
      tmp.glm <- glm(fmla, weight = tmp[[weight.var]], data=tmp, family = poisson)
    }  # end of if model.dist str statement

    else if (model.dist == "nb") {
      tmp.glm <- glm.nb(fmla, weight = tmp[[weight.var]], data=tmp, init.theta = 0.5)
    }  # end of if model.dist str statement

    else if (model.dist == "logist") {
      tmp.glm <- glm(fmla, weight = tmp[[weight.var]], data=tmp, family = binomial)
    }  # end of if model.dist str statement
    
    else {
      warning(paste("'", model.dist, "' is not a recognized model type"))
    }
    if (exists("tmp.glm")) return(tmp.glm)

} # end of runGlm
