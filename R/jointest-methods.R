#' @title Methods for \code{jointest} objects
#' @description Methods to extract and manipulate relevant information from
#' a \code{jointest} object.
#' @name jointest-methods
#' @docType methods


#' @description \code{print} method for class "\code{jointest}".
#' @param x an object of class \code{jointest}.
#' @param ... additional arguments to be passed
#' @method  print jointest
#' @docType methods
#' @rdname jointest-methods
#' @export

 print.jointest <- function(x, ...) 
 {
   summary.jointest(x, ...)
  # do.call(rbind,lapply(object, function(ob) ob$summary_table))
 }
 

#' @description \code{summary} method for class "\code{jointest}"
#' @param object an object of class \code{jointest}.
#' @param ... additional arguments to be passed
#' @method  summary jointest
#' @docType methods
#' @rdname jointest-methods
#' @export


summary.jointest <- function (object, ...) 
{
  object$summary_table$.assign=NULL
  object$summary_table
 # do.call(rbind,lapply(object, function(ob) ob$summary_table))
}

is_signif = NULL
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


#' @description \code{p.adjust} method for class "\code{jointest}". 
#' Add adjusted p-values into the \code{jointest} object.
#' @rdname jointest-methods
#' @param x an object of class \code{jointest}.
#' @param method any method implemented in \code{flip::flip.adjust} or 
#' a custom function. In the last case it must be a function that uses a matrix 
#' as input and returns a vector of adjusted p-values equal to the number of columns of the inputed matrix.
#' @param ... additional arguments to be passed, e.g., the argument \code{tail}. See details. 
#' @details
#' \code{tail} argument: expresses the tail direction of the alternative hypothesis. 
#' It can be "two.sided" (or 0, the default), "less" (or -1) or "greater" (or +1).
#' @method  p.adjust jointest
#' @docType methods
#' @importFrom flip flip.adjust
#' @importFrom stats p.adjust
#' @export
#' @seealso  \code{\link[flip]{flip.adjust}}

p.adjust.jointest <- function (x, method = "maxT", ...) 
{
  if(is.null(tail)){tail = 0}
  if(is.character(method)){
    if(method=="maxT"){
      #      if("alphas"%in%names(as.list(match.call())))
      p.adj=maxT.light(abs(x$Tspace),...)
    } else 
      if(method%in%c("minp","Tippet") ) {
        p.adj=maxT.light(-abs(x$Tspace),...)
      } else
        p.adj = flip.adjust(.set_tail(x$Tspace, tail = tail), 
                            method = method) 
  } else if(is.function(method)){
    p.adj = method(.set_tail(x$Tspace, tail = tail))
  }
  x$summary_table$p.adj = p.adj
  x
}

#' @rdname jointest-methods
#' @description \code{plot} method for class "\code{jointest}"
#' This \code{plot} function visualizes p-values from multiverse models, with
#' different markers to indicate statistical significance levels as defined by
#' the \code{mark_signif} argument (default is 0.05). Points are plotted with 
#' varying shapes based on whether the p-value is below the significance threshold,
#' and colors are used to distinguish between different coefficients.
#' @param x an object of class \code{jointest}.
#' @param ... additional arguments to be passed, i.e., \code{mark_signif} and
#' \code{p.values=c("raw","adjusted")}. See details.
#' @details
#' \code{mark_signif} argument: numeric value representing the significance threshold 
#' for marking p-values. Any p-value below this threshold will be marked 
#' with a dot. The default is \code{0.05}.
#' \code{p.values} argument: a character vector specifying which p-values to display.
#' It can be either \code{"raw"} for raw p-values or \code{"adjusted"} for 
#' adjusted p-values. The default is \code{"raw"}.
#' @export
#' @method plot jointest
#' @docType methods
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 scale_shape_manual

plot.jointest <- function(x, ...){
  if(is.null(p.values)){p.values = c("raw","adjusted")}
  if(is.null(mark_signif)){mark_signif = .05}
  
  p.values=p.values[1]
  Y="-log10(p.vals)"
  X="Estimate"
  group="Coeff"
  D=summary(x)
  if(p.values =="raw"){
    if(!is.null(D$`Pr(>|z|)`)) D$p.vals=D$`Pr(>|z|)` else
      D$p.vals=D$`Pr(>|t|)`
    if(is.null(D$p.vals)) D$p.vals=D$p
  } else
    if(p.values=="adjusted")
      D$p.vals=D$p.adj
  D$is_signif=(D$p.vals<=mark_signif)
  
  if(p.values=="raw") title="(Raw) p-values" else title="Adjusted p-values"
  
  p <- ggplot(D, aes_string(y=Y, 
                            x=X, 
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

