 library(tidyverse)
        library(OptimalCutpoints)
        library(pROC)
        
        ?optimal.cutpoints
        
        hem2 = as.data.frame(hem)
        
        
       x= optimal.cutpoints(PNED.SCORE~DEATH,tag.healthy = 1,data=hem2,methods = "Youden")
     x1= summary(x)
      x1$Youden$Global$optimal.cutoff$cutoff
      
      
      
      score= c("PNED.SCORE","AIMS65","GBS.SCORE")
      outcome= c("REBLEED.EPISODE","DEATH")
      so = expand.grid(score,outcome)
      # execute
      allcomb_var <- expand.grid(var1 = c("hb","TLC"), var2 = c("TLC"),
                                 stringsAsFactors = FALSE)
      
      allcomb_var %>% 
        purrr::pmap(.f = sum_wrapper, tbl=master)
      
      
      test_wrapper <- function (tbl=NULL, var1=NULL, var2=NULL) {
        tbl_pair <- tbl %>%
          select_(var1, var2)
        x <- tbl_pair %>% pull(var1)
        y <- tbl_pair %>% pull(var2)
        f=t.test(x~y) # there was fault if no formula mode
        g= colnames(tbl_pair) # get col_names
        h=list(g,f)}
        
       
      length(hem2$DEATH[hem2$PNED.SCORE<6])/length
      
      rocwrapper(x="PNED.SCORE",y="REBLEED.EPISODE")
      
      so
      
      library(tidyverse)
      library(OptimalCutpoints)
      so %>% 
        purrr::pmap(.f = rocwrapper, data=hem2)
      
    
    score= c("ROCKALL.SCORE","PNED.SCORE","AIMS65","GBS.SCORE")
    outcome= c("REBLEED.EPISODE","DEATH")
    so = expand.grid(x=score,y=outcome, stringsAsFactors = FALSE)
so
    rocwrapper = function(x=NULL,y=NULL){
      leg = paste(x,y) # helps to get names
      pair = hem2 %>% select_(x,y)
      x <- pair %>% pull(x)
      y <- pair %>% pull(y)
      pair=data.frame(x,y)
      
      k=optimal.cutpoints(x~y,tag.healthy = 1,data=pair,methods = "Youden")
     l= summary(k)
     m= colnames(pair)
     n= list(leg,l)
     n
    }
    
    so %>% 
      purrr::pmap(rocwrapper)
    
    str(so)
    
    map2(so$x,so$y,rocwrapper)
  # gives error that fcator have to be changed to character
    so$x=as.character(so$x)
    so$y=as.character(so$y)
    
    
   
   
    map2(so$x,so$y,rocwrapper)
    
   # even without doing this change
    so %>% 
      purrr::pmap(rocwrapper)
    
    
