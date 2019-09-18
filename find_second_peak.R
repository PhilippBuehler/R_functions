find_second_peak<- function(Q,first_peak,n_it=400,plot_it=NULL) {
  speicher=rep(NaN,n_it)
  loc=rep(NaN,n_it)
  start=max(Q)
  x1=Q
  
  if (abs(first_peak-which.max(Q))<2){
    for (j in 1:n_it){
      x2=rep((start-(1/n_it)*j*max(Q)),length(Q))
      
      above<-x1>x2
      
      intersect.points<-which(diff(above)!=0)
      
      x1.slopes<-x1[intersect.points+1]-x1[intersect.points]
      x2.slopes<-x2[intersect.points+1]-x2[intersect.points]
      
      x.points<-intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes-x2.slopes))
      y.points<-x1[intersect.points] + (x1.slopes*(x.points-intersect.points))
      
      if(!is.null(plot_it)){
        png(filename=paste(plot_it,j,".png",sep=""))
        plot(x1,type='l',ylab = "Discharge [m³/s]")
        lines(x2,type='l',col='red')
        points(x.points,y.points,col='blue')
        dev.off()
      }
      
      
      if (length(x.points) >3){
        #browser()
        teste1_not_peak=any(unique(round(x.points[1:2]))!=first_peak)
        teste2_not_peak=any(unique(round(x.points[(length(x.points)-1):length(x.points)]))!=first_peak)
        # teste1_log=all(abs(x.points[1:2]-round(x.points[1:2]))<tol)
        # teste2_log=all(abs(x.points[(length(x.points)-1):length(x.points)]
        #                    -round(x.points[(length(x.points)-1):length(x.points)]))<tol)
        
        if(diff(round(x.points[1:2]))==1&&
           diff(Q[round(x.points[1:2])])==0){
          tol_diff=1
        }else if(diff(round(x.points[1:2]))==2&&
                 all(diff(Q[seq(round(x.points[1]),round(x.points[2]))])==0)){
          tol_diff=2
        }else{
          tol_diff=0
        }
        teste1_log_unique=diff(round(x.points[1:2]))==tol_diff&
          all(Q[round(x.points[1:2])] >= y.points[1]) 
        
        
        if(diff(round(x.points[(length(x.points)-1):length(x.points)]))==1&&
           diff(Q[round(x.points[(length(x.points)-1):length(x.points)])])==0){
          tol_diff=1
        }else if(diff(round(x.points[(length(x.points)-1):length(x.points)]))==2&&
                 diff(Q[seq(round(x.points[(length(x.points)-1)]),round(x.points[length(x.points)]))])==0){
          tol_diff=2
        }else{
          tol_diff=0
        }
        teste2_log_unique=diff(round(x.points[(length(x.points)-1):length(x.points)]))==tol_diff &
          all(Q[round(x.points[(length(x.points)-1):length(x.points)])] >= y.points[1]) 
        
        
        
        
        comb1=sum(c(teste1_not_peak,teste1_log_unique))
        comb2=sum(c(teste2_not_peak,teste2_log_unique))
        choose_side=which.max(c(comb1,comb2,2))
        
        if(choose_side==1){
          #browser()
          #Hq rechts, 2.Peak links
          u=unique(round(x.points))
          uu=u-first_peak
          uu[uu>0]=NaN
          uu=u[which.max(uu)]
          linker_punkt=u[1]
          rechter_punkt=uu
          s1=x2[1]*((rechter_punkt-linker_punkt)+1)
          if(length(linker_punkt)==0){browser()}
          s2=sum(Q[linker_punkt:rechter_punkt])
          speicher[j]=s1-s2
          loc[j]=linker_punkt
        }
        else if(choose_side==2){
          #HQ links, 2.Peak reachts
          #browser()
          u=unique(round(x.points))
          uu=u-first_peak
          uu[uu<0]=NaN
          uu=u[which.min(uu)]
          linker_punkt=uu
          if(length(linker_punkt)==0){browser()}
          rechter_punkt=u[length(u)]
          s1=x2[1]*((rechter_punkt-linker_punkt)+1)
          s2=sum(Q[linker_punkt:rechter_punkt])
          speicher[j]=s1-s2
          loc[j]=rechter_punkt
        }
      }
    }
    #browser()
    which_close_to_peak=which(abs(loc-first_peak)<=1)
    speicher[which_close_to_peak]=NA
    dd=loc[which.max(speicher)]
  }else{
    dd=which.max(Q)
  }
  return(dd)
}
