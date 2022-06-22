#' @title Methods for jointest objects
#'
#' @description Methods for \code{jointest} objects. 
#' The following are methods to extract and manipulate relevant information from
#' a \code{jointest} object.
#' 
#' @name jointest-method
#' @docType methods

NULL



# #' print.jointest print method for a jointest object.
# #' @param x a jointest object
# #' @method print jointest
# #' @docType methods
# #' @rdname jointest-method
# #' @export
# 
# 
# print.jointest <- function(x, ...) {
#   cat(get_head_flip_out(x))
#   cat("Call: ")
#   print(x$call)
#   cat("\nCoefficients:\n")
#   print(x$coefficients)
#   # print.default(x)
# }

#' summary.jointest summary method for a jointest object.
#' @rdname jointest-method
#' @param object a jointest object
#' @param ... additional arguments to be passed
#' @method  summary jointest
#' @docType methods
#' @export

summary.jointest <- function (object, ...) {
  #  if summary_table is already an object of each element of the list:  
  # (checking only the first for the moment)
  if(!is.null(object[[1]]$summary_table)){
    if(length(object)>1){
      summary_table =  lapply(object,function(cmb) cmb$summary_table)
      summary_table = dplyr::bind_rows(summary_table)
    } else {
      summary_table=  object[[1]]$summary_table
    }
  } else { # ELSE: get it from summary.glm 
    
    tab=as.data.frame(summary(object[[1]])$coefficients)
    tab=tab[!is.na(tab[,"Score"]),]
    tab=cbind(Model= names(object[1]), Coeff=rownames(tab),   tab)
    
    summary_table=tab
    if(length(object)>1){
      for (i in 2: length(object)){
        tab=as.data.frame(summary(object[[i]])$coefficients)
        tab=tab[!is.na(tab[,"Score"]),]
        tab=cbind(Model= names(object[i]), Coeff=rownames(tab),   tab)
        summary_table=rbind(summary_table,tab)
      }
    }
    nms=names(summary_table)
    summary_table=data.frame(lapply(summary_table,unlist))
    colnames(summary_table)=nms
  }
  rownames(summary_table)=NULL
  summary_table
}


#############################################
#' plot.jointest summary method for a jointest object.
#' @rdname jointest-method
#' @param object a jointest object
#' @param ... additional arguments to be passed
#' @method  plot jointest
#' @docType methods
#' @export
#' @import ggplot2

plot.jointest <- function(mods,
                    p.values=c("raw","adjusted"),
                    mark_signif=.05){
  
  library(wesanderson)
  cbPalette = wes_palette("Zissou1", 5, type = "continuous")[c(1,4,5)]
  
  p.values=p.values[1]
  y="-log10(p.vals)"
  x="Estimate"
  group="Coeff"
  D=summary.jointest(mods)
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
    scale_colour_manual(values=cbPalette) +
    geom_point(aes(shape=is_signif))+ 
    ggtitle(title)
  if(!(mark_signif%in%c(0,1))){
    p <- p +
      scale_shape_manual(values=c(3,19),name = paste(p.values,
                                                     "p-value"), 
                         labels = paste0(c("p >  ", "p <= "),
                                         mark_signif))
  }
  
  p
}
