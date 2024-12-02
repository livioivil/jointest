# (stepdown) Westfall and Young max-t procedure 
maxT.light <- function (permT, alphas = c(.001,(1:9)/100,
                                          (1:9)/10), weights = NULL, m = ncol(permT)) 
{
  if (is.null(colnames(permT))) 
    colnames(permT) = 1:m
  
  #  print(alphas-ceiling(alphas*nrow(permT))/nrow(permT))
  alphas=floor(alphas*nrow(permT))/nrow(permT)
  alphas=sort(alphas)
  
  
  if (!is.null(weights)) 
    permT = t(weights * t(permT))
  
  
  #names(notrejs) = colnames(permT)
  
  Padjs = rep(1, m)
  names(Padjs) = colnames(permT)
  
  alpha=alphas[1]
  nalphas=length(alphas)
  
  alphaid=1
  
  #  ths=compute_thresholds(permT,alphas[1])
  #Padjs=rep(1,m) #[permT[1,]>ths]=alphas[alphaid]
  notrejs=1:m #which(Padjs==1)
  
  while ((alphaid <= nalphas)&(length(notrejs)>0))# & ifelse(i > 1, Padjs[steps[i - 1]] <= maxalpha, 
  {
    #  if(alphaid==1) browser()
    #  print(alphaid)                        
    #  print(notrejs)
    #    included_in_max=1:m
    #print(alphas[alphaid])
    th=compute_thresholds(permT[,notrejs,drop=FALSE],alphas[alphaid])
    tmp=which(permT[1,notrejs]>th)
    #ths=th
    #   print(tmp)
    while(length(tmp)>0){
      #    browser()
      Padjs[notrejs[tmp]]=alphas[alphaid]
      notrejs=notrejs[-tmp]
      included_in_max=notrejs
      if(length(notrejs)==0)return(Padjs)
      th=compute_thresholds(permT[,notrejs,drop=FALSE],alphas[alphaid])
      tmp=which(permT[1,notrejs]>th)
    } 
    # 
    # while(th<ths[alphaid]) {   
    #   tmp=which(permT[1,notrejs]>=th)
    #   ths[alphaid]=th
    #   if(length(tmp)>0){
    #     Padjs[notrejs[tmp]]=alphas[alphaid]
    #     notrejs=notrejs[-tmp]
    #     if(length(notrejs)==0)return(Padjs)
    #     th=compute_thresholds(permT[,notrejs,drop=FALSE],alphas[alphaid])
    #   } 
    # }
    alphaid=alphaid+1
  }
  
  return(Padjs)
}

######################################
compute_thresholds <- function(permT,alphas){
  #out=sort(apply(permT,1,max))[floor(round((1-alphas)*nrow(permT),12))]
  quantile(apply(permT,1,max),round(1-alphas,10),type=1,names=FALSE)
}

