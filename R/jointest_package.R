#' Robust Score Testing in GLMs, by Sign-Flip Contributions 
#'
#' @description It provides robust tests for testing in GLMs, by sign-flipping score contributions. The tests are often robust against overdispersion, heteroscedasticity and, in some cases, ignored nuisance variables.
#' @import flip
#' @importFrom car Anova
#' @importFrom MASS glm.nb
#' @importFrom plyr laply
#' @importFrom methods is
#' @importFrom stats D as.formula model.matrix sd summary.glm update
#' @examples
#' @docType package
#'
#' @author Livio Finos, Jelle Goeman and Jesse Hemerik, with contribution of Vittorio Giatti.
#' @name jointest-package
NULL