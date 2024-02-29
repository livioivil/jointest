# # TODO
# # # jointest is a class which inherit fom list. each element of the list is a list with 2 elements: Tspace (mandatory) and summary_table (optional). further elements are allowed.
# https://www.datamentor.io/r-programming/s3-class
# #
# # test1 <- setClass("test1", slots = c(Tspace="matrix", summary_table="data.frame"))
# # ## an object from the class
# # t1 <- test1(Tspace = matrix(1:10))
# # 
# # ## A class extending the previous, adding one more slot
# # jointest <- setClass("jointest",
# #                        slots = c("list"))
# # 
# # jointest(mods)
# # https://stackoverflow.com/questions/41520472/r-define-class-as-list-of-classes
# setClass("onetest", slots = c(Tspace="matrix", summary_table="data.frame"),
#          prototype = prototype(Tspace= matrix(NA), summary_table=data.frame()))
# # setClass('onetest',
# # #         representation = representation(obj = "character", amount = "numeric"),
# #          prototype = prototype(obj = 'hi', amount = 0))
# 
# getClass('onetest')
# test1=new("onetest")
# 
# # init multiset#
# # setMethod(f = 'initialize', 
# #           signature = 'multiset', 
# #           definition =  function(.Object, ..., amount = numeric(0)){
# #             print('you just initialized the class - multiset')
# #             callNextMethod(.Object, ..., amount = amount)
# #           })
# # new('multiset', amount = 2)
# # # [1] "you just initialized the class - multiset"
# # # An object of class "multiset"
# # # Slot "obj":
# # #   [1] "hi"
# # # 
# # # Slot "amount":
# # #   [1] 2
# # 
# 
# setMethod('jointest', 
#          representation = representation(objects = 'list'),
#           prototype = prototype(objects = list(test1)))
# 
# jt=new('jointest', objects = list(test1,test1))
# str(jt)
# jt[1]

#' @title Methods for jointest objects
#'
#' @description Methods for \code{jointest} objects. 
#' The following are methods to extract and manipulate relevant information from
#' a \code{jointest} object.
#' 
#' @name jointest-method
#' @docType methods

NULL
# 

# #' print.jointest print method for a jointest object.
# #' @param x a jointest object
# #' @method print jointest
# #' @docType methods
# #' @rdname jointest-method
# #' @export
# 
# 

#' summary.jointest summary method for a jointest object.
#' @rdname jointest-method
#' @param object an object of class \code{jointest}.
#' @param ... additional arguments to be passed
#' @method  summary jointest
#' @docType methods
#' @export

summary.jointest <- function (object, ...) 
{
#object$summary_table
  do.call(rbind,lapply(object, function(ob) ob$summary_table))
}

.get_summary_table_from_flipscores <- function(object){
  tab = as.data.frame(summary(object)$coefficients)
  tab = tab[!is.na(tab[, "Score"]), ]
  colnames(tab)[ncol(tab)]="p"
  tab = cbind( Coeff = rownames(tab), tab)
}
 

is_signif=NULL
###########################
#' as.jointest method for a jointest object.
#' @rdname jointest-method
#' @param object an object of class \code{jointest}.
#' @param names_obj a vector of names, its length must be equal to the length of \code{object}
#' @param ... additional arguments to be passed
#' @method as jointest
#' @docType methods
#' @export

as.jointest <- function (object, names_obj=NULL, ...)
{
  # TODO: calcolare summary_table in ogni elemento di object. se flipscores usa
  #     .get_summary_table_from_flipscores()
  if(!is.null(names_obj)) names(object)=names_obj
  if (is.null(names(object))) names(object)=paste0("mod",1:length(object))
  class(object) <- unique(c("jointest", class(object)))
  object
}
#############################################
#' plot.jointest summary method for a jointest object.
#' @rdname jointest-method
#' @param object an object of class \code{jointest}.
#' @param p.values  use "raw" or "adjusted" (i.e. corrected for selective inference) 
#' @param mark_signif a value between 0 and 1. The plot will mark the p-values smaller than \code{mark_signif} (0.05 by default). If equal to 0 or 1 nothing will be marked. 
#' @param ... additional arguments to be passed
#' @method  plot jointest
#' @docType methods
#' @export
#' @import ggplot2

plot.jointest <- function(object,
                    p.values=c("raw","adjusted"),
                    mark_signif=.05){
  
  p.values=p.values[1]
  y="-log10(p.vals)"
  x="Estimate"
  group="Coeff"
  D=summary.jointest(object)
  if(p.values =="raw"){
    if(!is.null(D$`Pr(>|z|)`)) D$p.vals=D$`Pr(>|z|)` else
      D$p.vals=D$`Pr(>|t|)`
  } else
    if(p.values=="adjusted")
      D$p.vals=D$p.adj
  D$is_signif=(D$p.vals<=mark_signif)
  
  if(p.values=="raw") title="(Raw) p-values" else title="Adjusted p-values"
  
  p <- ggplot(D, aes_string(y=y, 
                            x=x, 
                            group=group,color=group)) +
    geom_point(aes(shape=is_signif),size=2)+ 
    ggtitle(title) + theme_minimal() 
  if(!(mark_signif%in%c(0,1))){
    p <- p +
      scale_shape_manual(values=c(3,19),name = paste(p.values,
                                                     "p-value"), 
                         labels = paste0(c("p >  ", "p <= "),
                                         mark_signif))
  }
  
  p
}
