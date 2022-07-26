#'jointest for flipscores objects
#'@param tested_coeff is a list of the same length of \code{mods}, each element of the list being a vector of  
#'of names of tested coefficients. Alternatively, it can be a vector of names of tested coefficients, in this case, the tested coefficients are attributed to all models (when present). 
#'As a last option, it can be \code{NULL}, if so, all coefficients are tested.
#'@export
#'
#'@examples
#'n=20
#'set.seed(123)
#'D=data.frame(X=rnorm(n),Z1=rnorm(n),Z2=rnorm(n))
#'D$Y=D$Z1+D$X+rnorm(n)
#'mod1=glm(Y~X+Z1+Z2,data=D)
#'mod2=glm(Y~X+poly(Z1,2)+Z2,data=D)
#'mod3=glm(Y~X+poly(Z1,2)+poly(Z2,2),data=D)
#'mod4=glm(Y~X+Z1+poly(Z2,2),data=D)
#'mods=list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4)
#'for(i in 1:length(mods))
#' mods[[i]]$call$data=eval(D)
#'library(jointest)
#'res=join_flipscores(mods,n_flips = 5000, score_type = "standardized" ,
#'       seed = 1, tested_coeffs = "X")
#'summary(res)
#'summary(combine(res))
#'res=p.adjust.fwer(res)
#'summary(res)
join_flipscores <- function(mods,
                       tested_coeffs=NULL,
                       n_flips=5000,
                       score_type="standardized",
                       seed=NULL,
                       statistics="t"){
  
  names(mods)=.set_mods_names(mods) 
  
  if(is.null(tested_coeffs)){
    tested_coeffs=.get_all_coeff_names_list(mods)
  }
  
  if(!is.list(tested_coeffs)){
    temp=.get_all_coeff_names_list(mods)
    tested_coeffs=lapply(temp,function(nms) intersect(tested_coeffs,nms))
  }

  modflips=lapply(1:length(mods), function(i)
    { 
    temp=flipscores::flipscores(model=mods[[i]],score_type = score_type,n_flips = n_flips,
               to_be_tested=tested_coeffs[[i]],seed=eval(seed,1))
<<<<<<< HEAD
    if(statistics%in%c("t")) 
      if(score_type=="effective"||score_type=="orthogonalized") {
        sumY2s=colSums(temp$scores^2)
        n=nrow(temp$scores)
        tt=sapply(1:length(sumY2s),
                  function(i) flipscores:::.sum2t(temp$Tspace[,i],sumY2s[i],n))
        colnames(tt)=colnames(temp$Tspace)
        temp$Tspace = tt
      } 
=======
    if(statistics%in%c("t")){
      sumY2s=colSums(temp$scores^2)
      n=nrow(temp$scores)
      tt=sapply(1:length(sumY2s),
                function(i) .sum2t(temp$Tspace[,i],sumY2s[i],n))
      if(any(is.na(tt))) browser()
      colnames(tt)=colnames(temp$Tspace)
      temp$Tspace = tt
    } 
>>>>>>> master
    temp
  })
  names(modflips)=names(mods)
  class(modflips) <- c("jointest", class(modflips))
  
  modflips
}
