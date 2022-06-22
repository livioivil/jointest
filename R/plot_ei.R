plot_ei <- function(mods,
                    p.values=c("raw","adjusted"),
                    mark_signif=.05){
  
  library(wesanderson)
  cbPalette = wes_palette("Zissou1", 5, type = "continuous")[c(1,4,5)]
  
  p.values=p.values[1]
  y="-log10(p.vals)"
  x="Estimate"
  group="Coeff"
  D=summary_ei(mods)
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
