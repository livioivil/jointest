#' Join resampling based tests  
#'
#' @description runs resampling-based tests jointly (e.i. sign-flip score tests (Hemerik, Goeman and Finos (2020) <doi:10.1111/rssb.12369>) to allow for multivariate testing -- e.g. weak and strong control of the Familywise Error Rate or True Discovery Proportion.
#' @import flipscores
# @importFrom car Anova
# @importFrom MASS glm.nb
# @importFrom plyr laply
# @importFrom methods is
# @importFrom stats D as.formula model.matrix sd summary.glm update
#' @examples
#' n=20
#' set.seed(123)
#' D=data.frame(X=rnorm(n),Z1=rnorm(n),Z2=rnorm(n))
#' D$Y=D$Z1+D$X+rnorm(n)
#' mod1=glm(Y~X+Z1+Z2,data=D)
#' mod2=glm(Y~X+poly(Z1,2)+Z2,data=D)
#' mod3=glm(Y~X+poly(Z1,2)+poly(Z2,2),data=D)
#' mod4=glm(Y~X+Z1+poly(Z2,2),data=D)
#' mods=list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4)
#' for(i in 1:length(mods))
#' mods[[i]]$call$data=eval(D)
#' library(jointest)
#' res=join_flipscores(mods,n_flips = 5000,
#'                     seed = 1, tested_coeffs = "X")
#' summary(res)
#' summary(combine(res))
#' res=p.adjust.fwer(res)
#' summary(res)
#' 
#' @docType package
#'
#' @author Livio Finos
#' @name jointest-package
NULL