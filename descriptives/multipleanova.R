# multiple anovas

outcome= c("diagnoses")
anovas = 
  
  
  
  m1 = c("master$feNa","master$NGAL")
m2= c("master$TLC")
m3=expand.grid(m1,m2,stringsAsFactors = FALSE)

m3$Var1[2]

m

library(tidyverse)

# Multiple ANOVAS with single map argument 

# write a custom function

av= function(x){
  
  f=aov(x~master$diagnoses)
  
  
  f1= summary(f)
  
  f2=TukeyHSD(f)
  
  f3=list(f2,f1)
  
  f3
}

# apply map argument


master%>% select_if(is.numeric) %>% map(~av(.))


